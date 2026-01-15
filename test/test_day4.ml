open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Day4 = Hardcaml_AOC2025.Day4
module Harness = Cyclesim_harness.Make (Day4.I) (Day4.O)

let data =
  "..@@.@@@@.\n\
   @@@.@.@.@@\n\
   @@@@@.@.@@\n\
   @.@@@@..@.\n\
   @@.@@@@.@@\n\
   .@@@@@@@.@\n\
   .@.@.@.@@@\n\
   @.@@@.@@@@\n\
   .@@@@@@@@.\n\
   @.@.@@@.@.\n"
;;

(* To test your own input, comment out the above definition for data and uncomment line 
   below and replace PUT PATH HERE for the full path of the input file. *)
(* let data = In_channel.read_all "PUT PATH HERE" *)

(* Converts a string into an int list list with each list being a single line separated
   by '\n', '@' being a 1. and everything else being a 0. *)
let convert string =
  let acc, line =
    Base.String.fold string ~init:([], []) ~f:(fun (acc, line) ch ->
      match ch with
      | '\n' -> ((0 :: List.rev line) @ [ 0 ]) :: acc, []
      | '@' -> acc, 1 :: line
      | _ -> acc, 0 :: line)
  in
  (* Cases where the last line does not have a new line separator. *)
  match line with
  | [] -> acc
  | _ -> ((0 :: List.rev line) @ [ 0 ]) :: acc
;;

(* Adds a blank border on bottom and top so sliding window works out of bounds. *)
let pad string =
  let lists = convert string |> List.rev in
  match lists with
  | hd :: _ ->
    let len = List.length hd in
    let border = Base.List.init len ~f:(fun _ -> 0) in
    (border :: lists) @ [ border ]
  | _ -> invalid_arg "Empty input or convert did not work."
;;

let sample = pad data
let ( <--. ) = Bits.( <--. )

(* Essentially the same as the range_finder testbench. *)
let sample_testbench (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in
  (* Helper functions for accepting inputs. *)
  let feed_input n =
    inputs.data_in <--. n;
    inputs.data_in_valid := Bits.vdd;
    cycle ();
    inputs.data_in_valid := Bits.gnd;
    cycle ()
  in
  let feed_lines list =
    List.iter list ~f:feed_input;
    inputs.line_done := Bits.vdd;
    cycle ();
    inputs.line_done := Bits.gnd;
    cycle ()
  in
  (* Reset the design *)
  inputs.clear := Bits.vdd;
  cycle ();
  inputs.clear := Bits.gnd;
  cycle ();
  (* Pulse the start signal *)
  inputs.start := Bits.vdd;
  cycle ();
  inputs.start := Bits.gnd;
  (* Input some data *)
  List.iter sample ~f:feed_lines;
  (* Few more cycles to let sliding window pipeline fully finish. *)
  cycle ~n:10 ();
  inputs.finish := Bits.vdd;
  cycle ();
  inputs.finish := Bits.gnd;
  cycle ();
  (* Wait for result to become valid *)
  while not (Bits.to_bool !(outputs.valid)) do
    cycle ()
  done;
  (* Getting out both of the results and outputting the message. *)
  let part1 = Bits.to_unsigned_int !(outputs.part1) in
  let part2 = Bits.to_unsigned_int !(outputs.part2) in
  print_s [%message "Result" (part1 : int) (part2 : int)];
  (* Show in the waveform that [valid] stays high. *)
  cycle ~n:2 ()
;;

(* Below here is my expect test following the same format as range_finder. *)
let waves_config = Waves_config.no_waves

(* let waves_config = *)
(*   Waves_config.to_directory "/tmp/" *)
(*   |> Waves_config.as_wavefile_format ~format:Hardcamlwaveform *)
(* ;; *)

(* let waves_config = *)
(*   Waves_config.to_directory "/tmp/" *)
(*   |> Waves_config.as_wavefile_format ~format:Vcd *)
(* ;; *)

let%expect_test "Simple test, optionally saving waveforms to disk" =
  Harness.run_advanced ~waves_config ~create:Day4.hierarchical sample_testbench;
  [%expect {| (Result (part1 13) (part2 0)) |}]
;;

let%expect_test "Simple test with printing waveforms directly" =
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.Glob.glob "day4$i*" |> Re.compile)
    ; Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.Glob.glob "day4$o*" |> Re.compile)
    ]
  in
  Harness.run_advanced
    ~create:Day4.hierarchical
    ~trace:`All_named
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
          (* [display_rules] is optional, if not specified, it will print all named
             signals in the design. *)
        ~signals_width:20
        ~display_width:250
        ~wave_width:(-1)
        (* [wave_width] configures how many chars wide each clock cycle is *)
        waves)
    sample_testbench;
  [%expect {|
    (Result (part1 13) (part2 0))
    ┌Signals───────────┐┌Waves───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │day4$i$clear      ││─┐                                                                                                                                                                                                                                  │
    │                  ││ └──────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │day4$i$clock      ││╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥╥│
    │                  ││╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨╨│
    │day4$i$data_in    ││                                   ┌───┐ ┌───────┐       ┌─────┐ ┌─┐ ┌─┐ ┌───┐     ┌─────────┐ ┌─┐ ┌───┐     ┌─┐ ┌───────┐   ┌─┐       ┌───┐ ┌───────┐ ┌───┐       ┌─────────────┐ ┌─┐       ┌─┐ ┌─┐ ┌─┐ ┌─────┐     ┌─┐ ┌─────┐ ┌──│
    │                  ││───────────────────────────────────┘   └─┘       └───────┘     └─┘ └─┘ └─┘   └─────┘         └─┘ └─┘   └─────┘ └─┘       └───┘ └───────┘   └─┘       └─┘   └───────┘             └─┘ └───────┘ └─┘ └─┘ └─┘     └─────┘ └─┘     └─┘  │
    │day4$i$data_in_val││   ┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐  ┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐  ┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐  ┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐  ┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐  ┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐  ┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐  ┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐  ┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌│
    │                  ││───┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└──┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└──┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└──┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└──┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└──┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└──┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└──┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└──┘└┘└┘└┘└┘└┘└┘└┘└┘│
    │day4$i$finish     ││                                                                                                                                                                                                                                    │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │day4$i$line_done  ││                           ┌┐                        ┌┐                        ┌┐                        ┌┐                        ┌┐                        ┌┐                        ┌┐                        ┌┐                 │
    │                  ││───────────────────────────┘└────────────────────────┘└────────────────────────┘└────────────────────────┘└────────────────────────┘└────────────────────────┘└────────────────────────┘└────────────────────────┘└─────────────────│
    │day4$i$start      ││  ┌┐                                                                                                                                                                                                                                │
    │                  ││──┘└────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                  ││───────────────────────────────────────────────────────────────────────────┬─┬───┬─┬───┬─────────┬─────────────────────────────────────┬───────────────────────────────────────┬─────────────────┬──────────────────────────────────│
    │day4$o$part1      ││ 0                                                                         │1│2  │3│4  │5        │6                                    │7                                      │8                │9                                 │
    │                  ││───────────────────────────────────────────────────────────────────────────┴─┴───┴─┴───┴─────────┴─────────────────────────────────────┴───────────────────────────────────────┴─────────────────┴──────────────────────────────────│
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │day4$o$part2      ││ 0                                                                                                                                                                                                                                  │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │day4$o$valid      ││                                                                                                                                                                                                                                    │
    │                  ││────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    └──────────────────┘└────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;
