(* I have tried to use range-finder example code shape the answer for this day. *)
open! Core
open! Hardcaml
open! Signal

(* Set the standard bit width to 16. Could be changed to 12 for more mem efficiency. *)
let num_bits = 16

(* Needed for reciprocal division. *)
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
  (* Reciprocal division. *)
  let recip_sig = of_unsigned_int ~width:num_bits recip_multiplier in
  let hundred_sig = of_unsigned_int ~width:num_bits divisor in
  let quotRem100 num =
    let prod = num *: recip_sig in
    let quot = sel_top prod ~width:num_bits in
    let rem = num -: uresize ~width:num_bits (quot *: hundred_sig) in
    quot, rem
  in
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
                    (let quot, rem = quotRem100 (hundred_sig -: ptr.value -: data_in) in
                     let value = hundred_sig -: rem in
                     let edge = value ==:. 0 in
                     [ ptr <-- value
                     ; if_
                         ((ptr.value ==:. 0) ^: edge)
                         [ if_
                             edge
                             [ part2 <-- part2.value +: quot +:. 1 ]
                             [ part2 <-- part2.value +: quot -:. 1 ]
                         ]
                         [ part2 <-- part2.value +: quot ]
                     ; when_ edge [ part1 <-- part1.value +:. 1 ]
                     ])
                    (let quot, rem = quotRem100 (ptr.value +: data_in) in
                     [ if_
                         (rem ==: hundred_sig)
                         [ ptr <-- zero num_bits
                         ; part2 <-- part2.value +: quot +:. 1
                         ; part1 <-- part1.value +:. 1
                         ]
                         [ ptr <-- rem; part2 <-- part2.value +: quot ]
                     ])
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
