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
                       to be unsigned while keeping the modulus working *)
                    [ (let ptr' = hundred_sig -: ptr.value -: data_in in
                       let prod = ptr' *: recip_sig in
                       let q = sel_top prod ~width:num_bits in
                       let r = ptr' -: uresize ~width:num_bits (q *: hundred_sig) in
                       let value = hundred_sig -: r in 
                       when_
                         vdd
                         [ ptr <-- value
                         ; part2 <-- part2.value +: q
                         ; when_ (value ==:. 0) [ part1 <-- part1.value +:. 1 ]
                         ])
                    ]
                    [ (let ptr' = ptr.value +: data_in in
                       let prod = ptr' *: recip_sig in
                       let q = sel_top prod ~width:num_bits in
                       let r = ptr' -: uresize ~width:num_bits (q *: hundred_sig) in
                       (* Seems to be an edge case where the division will allow r to be 100. *)
                       if_
                         (r ==:. 100)
                         [ ptr <-- zero num_bits
                         ; part2 <-- part2.value +: q +:. 1
                         ; part1 <-- part1.value +:. 1
                         ]
                         [ ptr <-- r; part2 <-- part2.value +: q ])
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
