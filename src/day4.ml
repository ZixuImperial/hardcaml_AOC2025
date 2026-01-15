(* For this day, I plan on reusing the sliding window from last year's FPGA challenge.
   Modified as I do not have the UART. *)
open! Core
open! Hardcaml
open! Signal

(* Issues with full input if left at 8, most likely due to input size being larger than 128. *)
let max_width_bits = 10
let max_width = 1 lsl max_width_bits
let num_bits = 16

(* Not changed from range-finder. *)
module I = struct
  type 'a t =
    { clock : 'a
    ; clear : 'a
    ; start : 'a
    ; finish : 'a
    ; line_done : 'a
    ; data_in : 'a
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
    | In_Row
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let rec make_shreg ~n ~spec ~enable signal =
  match n with
  | 0 -> []
  | _ -> signal :: make_shreg ~n:(n - 1) ~spec ~enable (reg spec ~enable signal)
;;

(* Mux with a register between each level of the mux tree, this allows us to
   support arbitrary widths while still remaining within the ECP5's limited 
   logic and routing resources. *)
let rec recursive_mux ~spec ~sel list =
  let _ = spec in
  assert (List.length list = 1 lsl width sel);
  match list with
  | [ a; b ] -> mux2 sel b a, 0
  | _ ->
    (match List.chunks_of ~length:(List.length list / 2) list with
     | [ first; second ] ->
       let a, depth_a = recursive_mux ~spec ~sel:(lsbs sel) first in
       let b, depth_b = recursive_mux ~spec ~sel:(lsbs sel) second in
       assert (depth_a = depth_b);
       reg spec (mux2 (msb sel) b a), 1 + depth_a
     | _ -> failwith "unreachable")
;;

(* Higher-order function for implementing a sliding window convolution. Takes
   the input value and associated counters, along with a function which takes a
   window and checks for a match. This then builds the sliding window,
   pipelines it, and then runs it through the provided function to check for a
   match. *)
let make_sliding_window
  scope
  (* Width and height are static values *)
  ~clock
  ~clear
  ~width
  ~height
  (* The function used to determine if a given window
     matches the expected image *)
  ~(check_fn : Signal.t list list -> Signal.t)
  (* Grid width is based on the dimension of the inputted grid/image *)
  ~row_counter
  ~col_counter
  ~grid_width
  (* Input the image one pixel at a time *)
    (value_in : _ With_valid.t)
  =
  let spec = Reg_spec.create ~clock ~clear () in
  (* We rely on the synthesizer to not duplicate the shift register between calls to this function *)
  let shift_reg =
    make_shreg ~n:(max_width * height) ~spec ~enable:value_in.valid value_in.value
  in
  let depth = ref 0 in
  let window =
    List.init height ~f:(fun i ->
      List.init width ~f:(fun j ->
        (* For each pixel in the sliding window, use the measured grid width
           and a wide mux to find that pixel in the shift register *)
        let signal, depth' =
          recursive_mux
            ~spec
            ~sel:(uresize ~width:max_width_bits grid_width)
            (List.init max_width ~f:(fun x -> List.nth_exn shift_reg ((x * i) + j)))
        in
        depth := depth';
        Scope.naming scope signal [%string "window_%{i#Int}_%{j#Int}x"]))
  in
  let%hw window_valid =
    value_in.valid &: (row_counter >=:. height - 1) &: (col_counter >=:. width - 1)
  in
  (* Apply the provided check function to the window, and combine its result
     with whether the window is valid (i.e. fully within the area of the
     image), and match the pipeline depths of all of the components. *)
  pipeline spec ~n:(!depth + 2) window_valid
  &: check_fn (window |> List.map ~f:(List.map ~f:(pipeline spec ~n:2)))
;;

(* Counting the adjacent spaces. *)
let adjacent_check list =
  let total =
    list
    |> List.concat (* Flatten the list. *)
    |> List.fold ~init:(of_unsigned_int ~width:4 0) ~f:(fun acc value ->
      acc +: uresize ~width:4 value)
    (* Add all the 1s inside the window. *)
  in
  (* Getting the center and if there are less than 5 @ (includes itself) in the square *)
  match list with
  | [ [ _; _; _ ]; [ _; center; _ ]; [ _; _; _ ] ] -> center &: (total <:. 5)
  | _ -> failwith "unreachable case"
;;

let create
  scope
  ({ clock; clear; start; finish; line_done; data_in; data_in_valid } : _ I.t)
  : _ O.t
  =
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  let is_end_of_line = line_done in
  (* Measure the width of the grid by counting how long the first line is *)
  let width_known = reg_fb spec ~width:1 ~f:(fun x -> x |: is_end_of_line) in
  let grid_width =
    reg_fb
      spec
      ~width:(num_bits_to_represent max_width)
      ~enable:(data_in_valid &: ~:is_end_of_line &: ~:width_known)
      ~f:(fun i -> i +:. 1)
  in
  let col_counter =
    reg_fb
      spec
      ~width:(num_bits_to_represent max_width)
      ~enable:data_in_valid
      ~f:(fun i -> mux2 is_end_of_line (zero (width i)) (i +:. 1))
  in
  let row_counter =
    reg_fb
      spec
      ~width:(num_bits_to_represent max_width)
      ~enable:is_end_of_line
      ~f:(fun i -> i +:. 1)
  in
  let make_sliding_window =
    make_sliding_window
      scope
      ~clock
      ~clear
      ~row_counter
      ~col_counter
      ~grid_width
      { With_valid.valid = data_in_valid; value = data_in }
  in
  let check = make_sliding_window ~width:3 ~height:3 ~check_fn:adjacent_check in
  let%hw_var part1 = Variable.reg ~width:num_bits spec in
  let%hw_var part2 = Variable.reg ~width:num_bits spec in
  let valid = Variable.wire ~default:gnd () in
  compile
    [ sm.switch
        [ ( Idle
          , [ when_
                start
                [ part1 <-- of_unsigned_int ~width:num_bits 0
                ; part2 <-- of_unsigned_int ~width:num_bits 0
                ; sm.set_next In_Row
                ]
            ] )
        ; ( In_Row
          , [ part1 <-- part1.value +: uresize ~width:num_bits check
            ; when_ finish [ sm.set_next Done ]
            ] )
        ; Done, [ valid <-- vdd; when_ finish [ sm.set_next In_Row ] ]
        ]
    ];
  { part1 = part1.value; part2 = part2.value; valid = valid.value }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day4" create
;;
