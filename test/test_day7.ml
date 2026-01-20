open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Day7 = Hardcaml_AOC2025.Day7
module Harness = Cyclesim_harness.Make (Day7.I) (Day7.O)

(* Currently using OCaml to parse the data input so we can iterate it later. *)
let convert string =
  let acc, line =
    (* Parsing everything apart from '.' and '\n' as 1 so we reduce bit width and 
       S is only used once so only used in First_line state. *)
    Base.String.fold string ~init:([], []) ~f:(fun (acc, line) ch ->
      match ch with
      | '\n' -> List.rev line :: acc, []
      | '\r' -> List.rev line :: acc, []
      | '.' -> acc, 0 :: line
      | _ -> acc, 1 :: line)
  in
  (* Cases where the last line does not have a new line separator. *)
  (match line with
   | [] -> acc
   | _ -> List.rev line :: acc)
  |> List.rev
;;

let data =
  ".......S.......\n\
   ...............\n\
   .......^.......\n\
   ...............\n\
   ......^.^......\n\
   ...............\n\
   .....^.^.^.....\n\
   ...............\n\
   ....^.^...^....\n\
   ...............\n\
   ...^.^...^.^...\n\
   ...............\n\
   ..^...^.....^..\n\
   ...............\n\
   .^.^.^.^.^...^.\n\
   ...............\n"
;;

(* To test your own input, comment out the above definition for data and uncomment line 
   below and replace PUT PATH HERE for the full path of the input file. *)
(* let data = In_channel.read_all "PUT PATH HERE" *)

let sample = convert data
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
  Harness.run_advanced ~waves_config ~create:Day7.hierarchical sample_testbench;
  [%expect {| (Result (part1 21) (part2 40)) |}]
;;

let%expect_test "Simple test with printing waveforms directly" =
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.Glob.glob "day7*" |> Re.compile)
    ]
  in
  Harness.run_advanced
    ~create:Day7.hierarchical
    ~trace:`All_named
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
          (* [display_rules] is optional, if not specified, it will print all named
             signals in the design. *)
        ~signals_width:30
        ~display_width:140
        ~wave_width:0
        (* [wave_width] configures how many chars wide each clock cycle is *)
        waves)
    sample_testbench;
  [%expect
    {|
    (Result (part1 21) (part2 40))
    ┌Signals─────────────────────┐┌Waves───────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │day7$i$clear                ││──┐                                                                                                         │
    │                            ││  └─────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │day7$i$clock                ││┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐│
    │                            ││ └┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└│
    │day7$i$data_in              ││                                  ┌───┐                                                                     │
    │                            ││──────────────────────────────────┘   └─────────────────────────────────────────────────────────────────────│
    │day7$i$data_in_valid        ││      ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐     ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─│
    │                            ││──────┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─────┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ │
    │day7$i$finish               ││                                                                                                            │
    │                            ││────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │day7$i$line_done            ││                                                                  ┌─┐                                       │
    │                            ││──────────────────────────────────────────────────────────────────┘ └───────────────────────────────────────│
    │day7$i$start                ││    ┌─┐                                                                                                     │
    │                            ││────┘ └─────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                            ││────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │day7$o$part1                ││ 0                                                                                                          │
    │                            ││────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    │                            ││────────────────────────────────────┬───────────────────────────────────────────────────────────────────────│
    │day7$o$part2                ││ 0                                  │1                                                                      │
    │                            ││────────────────────────────────────┴───────────────────────────────────────────────────────────────────────│
    │day7$o$valid                ││                                                                                                            │
    │                            ││────────────────────────────────────────────────────────────────────────────────────────────────────────────│
    └────────────────────────────┘└────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;
