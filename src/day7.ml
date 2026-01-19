open! Core
open! Hardcaml
open! Signal

(* Needed 128 bits to get part 2 correct for AOC input. *)
let num_bits = 128

(* Set size of each line. *)
let max_line_size = 150
let max_line_bits = num_bits_to_represent max_line_size

module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; data_in : 'a
    ; data_in_valid : 'a
    ; line_done : 'a
    }
  [@@deriving hardcaml]
end

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
    | First_line
    | Accepting_inputs
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create
  scope
  ({ clock; clear; start; finish; data_in; data_in_valid; line_done } : _ I.t)
  : _ O.t
  =
  let _ = scope in
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  (* Array to store a single line of input. *)
  let regs = Array.init max_line_size ~f:(fun _ -> Variable.reg spec ~width:num_bits) in
  (* Creates a counter to keep track of which part of the line I am on. *)
  let counter = Variable.reg spec ~width:max_line_bits in
  let part1 = Variable.reg spec ~width:num_bits in
  let valid = Variable.wire ~default:gnd () in
  compile
    [ sm.switch
        [ ( Idle
          , [ when_
                start
                [ (* Default set up. *)
                  part1 <-- zero num_bits
                ; counter <-- zero max_line_bits
                ; sm.set_next First_line
                ]
            ] )
        ; ( First_line
          , [ (* Processing the first line seperate for start up. *)
              when_
                data_in_valid
                [ (* Proc used so that the counter is not part of the mapping. *)
                  proc
                    (Array.to_list
                       (* Had trouble with getting the nth register at runtime. *)
                       (Array.mapi regs ~f:(fun i reg ->
                          when_
                            (counter.value ==:. i)
                            [ reg <-- uresize ~width:num_bits data_in ])))
                ; counter <-- counter.value +:. 1
                ]
            ; when_ line_done [ counter <--. 0; sm.set_next Accepting_inputs ]
            ] )
        ; ( Accepting_inputs
          , [ when_
                data_in_valid
                [ proc
                    (Array.to_list
                       (Array.mapi regs ~f:(fun i reg ->
                          when_
                            (counter.value ==:. i)
                            [ when_
                                (reg.value >:. 0)
                                [ when_
                                    data_in
                                    (if (* Boundary checking. *)
                                        i > 0 && i < max_line_size - 1
                                     then (
                                       let prev = regs.(i - 1) in
                                       let next = regs.(i + 1) in
                                       [ prev <-- prev.value +: reg.value
                                       ; reg <-- zero num_bits
                                         (* Can change next as I leverage the fact that 2
                                          splitters cannot be next to each other. *)
                                       ; next <-- next.value +: reg.value
                                         (* Part 1 counts the amount of splitters encountered. *)
                                       ; part1 <-- part1.value +:. 1
                                       ])
                                     else [])
                                ]
                            ])))
                ; counter <-- counter.value +:. 1
                ]
            ; when_ line_done [ counter <--. 0 ]
            ; when_ finish [ sm.set_next Done ]
            ] )
        ; Done, [ valid <-- vdd ]
        ]
    ];
  { part1 = part1.value
  ; (* Part 2 is the amount of combinations so just add all the bottom line values. *)
    part2 = Array.fold regs ~init:(zero num_bits) ~f:(fun acc x -> acc +: x.value)
  ; valid = valid.value
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day7" create
;;
