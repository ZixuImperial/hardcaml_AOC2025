Day 4
====

For this day, I did not implement an OCaml representation. So I will only explain the hardcaml implementation.

Sliding window
----

This day works very well with a sliding window which we can check for the condition if the total of the 3x3 square has less than 5 with the middle being an @.

For this day, I have used the sliding window template from the advent of hardcaml 2024 day 4. More can be found out at from this [Jane Street blog](https://blog.janestreet.com/advent-of-hardcaml-2024/). I will go through my understanding of the code.

```OCaml
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
```

These are all the relevant functions in making this window work with a check_fn.

From my understanding of this code, the shreg (shift register)  essentially will make a registers every clock cycle and produces a list of signals which when necessary can be read as signal could be updated during runtime.  
Then the recursive mux will allow us to get all the signal that we need for our window and will find the maximum cycles we need to wait to ensure that the shreg's register have sufficient clock cycles so that the data can be filled in.  
So all we have to do is pipeline to wait that amount of cycles and a bit extra to ensure our window is fully correct and then we can use the check_fn to use this window we created.

Part 1
---

The hardest bit of this part is converting the UART code that the template gave into code that fits in the same format as the rest of my days which is essentially passing in the inputs through the testbench. Also, the time it took to understand how the code worked.

As of writing, I have not implemented part 2.

```OCaml
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
```

This was the create function, I have used.

I had to change the signature of the 'I' module so that it did not depend on the UART so, it was tweaking the code so that it depended on the data in and data in valid for correct input logic. Also, the code needed to know when lines ended so, in the 'I' module, I added a line_done field. Along with this, I only had a minor error in my state machine where the sliding window only works when the sliding window is ran every clock cycle.

```OCaml
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
```

The function passed into the sliding window is quite generic. First, make sure the center is actually on so, we do not inflate the count. Then, add up all the list of lists and make sure that it is less than 5 as we included ourselves.

This is pretty much all for the implementation of part 1.

Part 2
---

Currently this part is TODO.

My brainstorm for this part is:

- Store the result of count inside a block of RAM during the first iteration, then during further iterations, we can read the RAM one-by-one using the same way. Only problem is that I am not that familiar with how RAM is implemented in hardcaml.
- Return the result of each count back to the testbench and then what we can do let the testbench re-input all the data in again until part 2 does not change. The main problem I see with this approach is that it is a bit hacky and that I need to figure out how to retrieve the correct values at the correct time as the sliding window does not complete in a single cycle.

Testing
--

Nearly the same as day 1. Differences between the parsing, feeding, and extra cycles to wait for the pipeline to clean as the circuit is still processing results.

```OCaml
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
```

All the parsing and file handling.

The parsing was a bit extra this time, due to the fact that the sliding window did not account for borders so for a temporary fix, I just padded a border of 0s around the input as now the sliding window does every needed 3x3 square.

I found a bit of trouble converting the string into int list list to pass in. This maybe due to the fact that, I though strings are a char list by default in OCaml like Haskell. In the end, I made use of the String functions found in Base to pattern match on certain characters and made use of pairs of list to keep an overall accumulator and a line accumulator. I have tried to avoid concatenation as I assume OCaml is similar to Haskell where concat is O(n) so had to use a lot more reverses.

If you want to test your own input, follow the comment above the second let.

Unless there are more edge cases, the output should match AOC answer as mine managed to.

```OCaml
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
```

To feed in the inputs, I fed each list one by one and from each list, I fed each int, updating the correct input and cycling. The waveform for this testbench will be hard to read in the expect test due to the amount of cycling needed.

Conclusion
--

There are limitations on my program which I found during testing. So as the input for this day exceeds 128, I had to increase max width bits and I saw the correct answer however, due to how this is used by shreg, this makes the code run really slowly as each clock cycle there could be 2^(max width bits) * height register that need updating. There is an upper limit of the size of data we can use during this program. For decent range and speed, I have it set to 10 bits.

Overall, this day allowed me to see how powerful hardcaml can be as I thought everything would be more verbose but using powerful OCaml features to help synthesise hardware is brilliant. So this has been a great learning experience and been interesting to understand.
