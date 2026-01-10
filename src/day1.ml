(* I have tried to use range-finder example code shape the answer for this day. *)
open! Core
open! Hardcaml
open! Signal

(* Set the standard bit width to 16. Could be changed to 12 for more mem efficiency. *)
let num_bits = 16

(* Needed for reciprocal multiplication division. *)
let divisor = 100
let recip_multiplier = (1 lsl num_bits) / divisor

(* Not changed from range-finder. *)
module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; data_in : 'a [@bits num_bits]
    ; data_in_valid : 'a
    }
  [@@deriving hardcaml]
end

(* Needed to store the answers to both parts and had a valid. *)
module O = struct
  type 'a t =
    { part1 : 'a [@bits num_bits]
    ; part2 : 'a [@bits num_bits]
    ; valid : 'a
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Accepting_inputs
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope ({ clock; clear; start; finish; data_in; data_in_valid } : _ I.t) : _ O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  (* Instantiated the registers needed so ptr to keep track of hand.
     Both parts accumulators instead of getting back from inputs. *)
  let%hw_var ptr = Variable.reg spec ~width:num_bits in
  let%hw_var part1 = Variable.reg spec ~width:num_bits in
  let%hw_var part2 = Variable.reg spec ~width:num_bits in
  (* Next signals needed for the fast mod and div. *)
  let recip_sig = of_unsigned_int ~width:num_bits recip_multiplier in
  let hundred_sig = of_unsigned_int ~width:num_bits divisor in
  let valid = Variable.wire ~default:gnd () in
  compile
    [ sm.switch
        [ ( Idle
          , [ when_
                (* Setting up the state machine with default value. *)
                start
                [ part1 <-- zero num_bits
                ; part2 <-- zero num_bits
                ; ptr <-- of_unsigned_int ~width:16 50
                ; sm.set_next Accepting_inputs
                ]
            ] )
        ; ( Accepting_inputs
          , [ when_
                data_in_valid
                [ if_
                    (msb data_in)
                    (* To avoid pain with dealing with signed input, I have converted it 
                       to be unsigned so I can correctly work out the quot and rem. 
                       By doing (100 - ptr.value + abs data_in) % 100,
                       I can do 100 - on the rem to find the correct position. *)
                    [ ((* Find below explanation of fast quotRem. *)
                       let ptr' = hundred_sig -: ptr.value -: data_in in
                       let prod = ptr' *: recip_sig in
                       let quot = sel_top prod ~width:num_bits in
                       let rem = ptr' -: uresize ~width:num_bits (quot *: hundred_sig) in
                       let value = hundred_sig -: rem in
                       (* Needed to evaluate edge cases as when value <> 0. *)
                       let edge = value <>:. 0 in
                       (* Not sure how to still use the lets in continuous statements 
                          without using when_ vdd. *)
                       when_
                         vdd
                         [ ptr <-- value
                         ; (* There was a problem when doing a left on 0.
                              It would increment part2 due to how my division requires 
                              100 - ptr.value + data_in which makes quot larger
                              but there was also an issue where when value was 0,
                              part2 did not increment so created if to sort logic. *)
                           if_
                             (ptr.value ==:. 0)
                             [ if_
                                 edge
                                 [ part2 <-- part2.value +: quot -:. 1 ]
                                 [ part2 <-- part2.value +: quot ]
                             ]
                             [ if_
                                 edge
                                 [ part2 <-- part2.value +: quot ]
                                 [ part2 <-- part2.value +: quot +:. 1 ]
                             ]
                         ; when_ (value ==:. 0) [ part1 <-- part1.value +:. 1 ]
                         ])
                    ]
                    [ ((* Find below explanation of fast quotRem. *)
                       let ptr' = ptr.value +: data_in in
                       let prod = ptr' *: recip_sig in
                       let quot = sel_top prod ~width:num_bits in
                       let rem = ptr' -: uresize ~width:num_bits (quot *: hundred_sig) in
                       (* Seems to be an edge case where the division will allow r to be 100. *)
                       if_
                         (rem ==:. 100)
                         [ ptr <-- zero num_bits
                         ; part2 <-- part2.value +: quot +:. 1
                         ; part1 <-- part1.value +:. 1
                         ]
                         [ ptr <-- rem; part2 <-- part2.value +: quot ])
                    ]
                ]
            ; when_ finish [ sm.set_next Done ]
            ] )
        ; Done, [ valid <-- vdd; when_ finish [ sm.set_next Accepting_inputs ] ]
        ]
    ];
  { part1 = part1.value; part2 = part2.value; valid = valid.value }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day1" create
;;

(* Motivation for quotRem. 
   We can only use this method, if we have constraints and knowledge of which divisor
   we want. Let x be our input, d be our divisor, n be size, q be quot, and r be rem. Then
   q = x / d. Then we can multiply by 2^n / 2^n so we get q = (x * 2^n) / (d * 2^n).
   Same as q = (x / 2^n) * (2^n / d) so as we know d and n. We can precompute 2^n / d
   so as dividing by a power of 2 is the same as right shifting we can know q to be
   approximately (x * (2^n / d)) >> n which can be done in 1 cycle as we know
   (2^n / d) as recip_multiplier. Then we can easily work out r by x - (q * d) 
   as that is what the remainder is. Instead of just shifting I can use sel_top
   n bits as the multiplication has 32 bits so right shifting is the same. *)
