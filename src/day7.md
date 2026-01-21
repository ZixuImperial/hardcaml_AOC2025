Day 7
===

This will be the next day, I will attempt as I think this day is very well suited to hardcaml.

My initial idea is to make use of RAM which will store a line of the state of the previous line with the beams in the correct position. With this, I can feed the next line and we can check if the previous line has a beam so a 1 and then do the appropriate action depending if there is a splitter or not. Doing this till we reach the finish and then just read all the 1s on the RAM and that is part 1 done.

For part 2, my idea is for the RAM to hold numbers which relate to how many times the beam can get onto this path so this will help tell us the amount of possibles timelines once we compute to the end.

To visualise

```()
Let RAM be           0, 1, 1, 1, 0
and the next line be ., ^, ., ., ^

I want to process each index separately
so that each state of RAM for this line would look like this

0, 1, 1, 1, 0 no change for .
1, 0, 2, 1, 0 gone through split, changed self to be 0, add to adjacent by self
1, 0, 1, 1, 0 no change as a .
1, 0, 1, 1, 0 no change as a .
1, 0, 1, 1, 0 no change as currently 0
```

Repeating this until all inputs are fed so then for part 1 it is just the count of splits hit and part 2 is just the sum.

Current implementation
---

Pretty much like the explanation above but I found a problem in which that RAM can only have a limited amount of read and write ports so when we do the splitting we need access to the previous, current, and next values in the line so instead I just used a fixed size register array as it is more simplistic to manage.

A current problem that I have is making too much memory as I do not know how to make a the array dynamic size with respect to the input line size as it currently set to 150 so I create 150 register but I may only use like 40 of them. Also, the fact that for the answer to display for part2 correctly, I set num_bits to a high number like 128 but I think this could be changed to 64 depending on the input so the memory usage is too high.

Another thing which I am not sure if there is a better way to implement the array accessing as I need the reference to the register which mux cannot provide me and to unsigned int requires the value to constant during compilation. So for my solution, I use mapi to keep the current index and when it is the same as counter, I do the checking I need.

```OCaml
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
```

This is the state machine code.

As you can see, I process the first line differently as I need to input the 'S' from the input in the correct spot and once this line is done, I can go into the main part of the code. I also use proc so that I can do all the array stuff inside without making the counter part of it.

So as explained above, the only case we care about is when the current register at counter index is a 1 then we check if the current data is a '^' or a '1' from the parsing so then we can split it. By split, I mean add the current value of the register to the prev and next register as this accounts for part2 as now there is 2 times the current value timelines (ways to get there), otherwise if it is a '.' or '0' we ignore it.

During this process, I took some liberties as I realised that the input will never have a chain of '^' next to each other so I can freely add to the next without affecting the correctness of program, otherwise I could be creating too many timelines and especially for the question, it would make no sense as where would the beams go. Another liberty I took, is that the '^' are never at the ends of the inputs so I just did some boundary checking to avoid these cases.

Part 2 is gained by adding all the values at the bottom so I decided to use fold for this, I could potentially used reduce instead but I am more experienced with fold as I know Haskell. Part 1 is just a register that stores how many splits there were during the program as you can see it get incremented when the prev, reg, and next get set.

Testing
--

Pretty much the same as Day 4 with minor tweaking.

```OCaml
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
```

Parsing is more simplistic as we do not have to add extra borders around the data this time. I do not think, I explained why I wrote the parsing like this last time. So, I wrote the parsing like this as I need an int list list and OCaml concatenation is O(n) for any amount of items as lists are backed by linked lists and will need to traverse to the end to add it on. Therefore, I used used reverse at the end as this is O(n) once instead of n many times of O(n) operations.

Feeding the inputs is the same as day4 so feed line by line until all data is processed. Like before as well, you can test it on your own input by changing the "PUT PATH HERE" string for the whole file path and uncommenting this and commenting the prior definition.

Conclusion
---

Improvement will need to be made for this program to be within reasonable limits, maybe future designs could feature RAM with at least 2 write ports and 1 read port (Getting the output for part 2 will however be slower) as we need to change prev and the current at least, then we could have a register for carry which we can manipulate in the next cycle alongside next's value.

Overall, this day seemed easier than the other days, maybe due to this being the third day I properly attempted or ease of the question but solving these problems is rewarding.
