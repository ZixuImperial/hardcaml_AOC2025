open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Day1 = Hardcaml_AOC2025.Day1
module Harness = Cyclesim_harness.Make (Day1.I) (Day1.O)

(* Currently using OCaml to parse the data input so we can iterate it later. *)
let dir cmd =
  let nums = Int.of_string @@ String.drop_prefix cmd 1 in
  match String.get cmd 0 with
  | 'L' -> -nums
  | _ -> nums
;;

let parse (cmds : string list) = List.map cmds ~f:dir
(* I cannot get dune to abbreviate the file path to not expose my system so 
   please if you want to run this with your own input, put the complete file 
   path in and uncomment the 2 lines after. *)

(* let data file = parse @@ String.split_lines @@ In_channel.read_all file *)
(* let sample = data "PUT PATH HERE" *)

(* I have put in the sample1 data here so you can run the test with the expect 
   tests. Comment out if using your own input. *)
let sample = parse [ "L68"; "L30"; "R48"; "L5"; "R60"; "L55"; "L1"; "L99"; "R14"; "L82"]
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
  List.iter sample ~f:(fun x -> feed_input x);
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
  Harness.run_advanced ~waves_config ~create:Day1.hierarchical sample_testbench;
  [%expect {| (Result (part1 3) (part2 6)) |}]
;;

let%expect_test "Simple test with printing waveforms directly" =
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.Glob.glob "day1*" |> Re.compile)
    ]
  in
  Harness.run_advanced
    ~create:Day1.hierarchical
    ~trace:`All_named
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
          (* [display_rules] is optional, if not specified, it will print all named
             signals in the design. *)
        ~signals_width:30
        ~display_width:140
        ~wave_width:1
        (* [wave_width] configures how many chars wide each clock cycle is *)
        waves)
    sample_testbench;
  [%expect
    {|
    (Result (part1 3) (part2 6))
    ┌Signals─────────────────────┐┌Waves───────────────────────────────────────────────────────────────────────────────────────────────────────┐
    │day1$i$clear                ││────┐                                                                                                       │
    │                            ││    └───────────────────────────────────────────────────────────────────────────────────────────────────────│
    │day1$i$clock                ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
    │                            ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
    │                            ││────────────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────────────────────│
    │day1$i$data_in              ││ 0          │65468  │65506  │48     │65531  │60     │65481  │65535  │65437  │14     │65454                  │
    │                            ││────────────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────────────────────│
    │day1$i$data_in_valid        ││            ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐   ┌───┐                   │
    │                            ││────────────┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───┘   └───────────────────│
    │day1$i$finish               ││                                                                                            ┌───┐           │
    │                            ││────────────────────────────────────────────────────────────────────────────────────────────┘   └───────────│
    │day1$i$start                ││        ┌───┐                                                                                               │
    │                            ││────────┘   └───────────────────────────────────────────────────────────────────────────────────────────────│
    │                            ││────────────────────────────────┬───────────────────────┬───────────────┬───────────────────────────────────│
    │day1$o$part1                ││ 0                              │1                      │2              │3                                  │
    │                            ││────────────────────────────────┴───────────────────────┴───────────────┴───────────────────────────────────│
    │                            ││────────────────┬───────────────┬───────┬───────┬───────────────┬───────────────────────┬───────────────────│
    │day1$o$part2                ││ 0              │1              │2      │3      │4              │5                      │6                  │
    │                            ││────────────────┴───────────────┴───────┴───────┴───────────────┴───────────────────────┴───────────────────│
    │day1$o$valid                ││                                                                                                ┌───────────│
    │                            ││────────────────────────────────────────────────────────────────────────────────────────────────┘           │
    │                            ││────────────────────────────────┬───────────────────────┬───────────────┬───────────────────────────────────│
    │day1$part1                  ││ 0                              │1                      │2              │3                                  │
    │                            ││────────────────────────────────┴───────────────────────┴───────────────┴───────────────────────────────────│
    │                            ││────────────────┬───────────────┬───────┬───────┬───────────────┬───────────────────────┬───────────────────│
    │day1$part2                  ││ 0              │1              │2      │3      │4              │5                      │6                  │
    │                            ││────────────────┴───────────────┴───────┴───────┴───────────────┴───────────────────────┴───────────────────│
    │                            ││────────────┬───┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────┬───────────────────│
    │day1$ptr                    ││ 0          │50 │82     │52     │0      │95     │55     │0      │99     │0      │14     │32                 │
    │                            ││────────────┴───┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────┴───────────────────│
    └────────────────────────────┘└────────────────────────────────────────────────────────────────────────────────────────────────────────────┘
    |}]
;;
