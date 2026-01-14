Day 1
======

This file will talk through my implementation of day 1 of advent of code.

OCaml implementation
---

```OCaml
let dir cmd =
  let nums = Int.of_string @@ String.drop_prefix cmd 1 in
  match String.get cmd 0 with
  | 'L' -> -nums
  | _ -> nums
;;

let parse (cmds : string list) = List.map cmds ~f:dir
let data file = parse @@ String.split_lines @@ In_channel.read_all file

let dial ptr turn res =
  let ptr' = (ptr + turn) % 100 in
  let res' = if ptr' = 0 then res + 1 else res in
  ptr', res'
;;

let ticks ptr turn res =
  let ptr' = (ptr + turn) % 100 in
  let diff = if ptr = 0 then 100 else if turn >= 0 then 100 - ptr else ptr in
  if diff <= abs turn then ptr', res + 1 + ((abs turn - diff) / 100) else ptr', res
;;

let part1 file =
  snd @@ List.fold (data file) ~init:(50, 0) ~f:(fun (ptr, res) turn -> dial ptr turn res)
;;

let part2 file =
  snd
  @@ List.fold (data file) ~init:(50, 0) ~f:(fun (ptr, res) turn -> ticks ptr turn res)
;;
```

First, I solved the problem in OCaml by:

- First, parsing the file was just checking the first letter which will sign the rest of the string.
- Dial and ticks handled the ptr movement and updated res with respect to which part.
- Finally, we just had to combine these into the part1 and part2 function.
- To do this, we can repeatedly apply dial or tick with the parsed data with a fold.

Hardcaml conversion
-----

Using the demo project as a template, I decided to have the testbench provide the parsed data instead of using UART which is used in advent of hardcaml 2024. Also, due to my limited knowledge, I used the range-finder to template my day 1 solution.

So, the first problem I faced when trying to plan this day was thinking how to get quotient and remainder from a random input in base Hardcaml.

Obviously, I could probably use the provided division operator ( /: ). However, the main problem I found with this is slowdown of the FPGA due to division taking multiple as one of the methods I know is restoring division and this takes multiple clock cycles to figure out. Also, the fact the FPGA will have to provide this would be worse overall.

One thing, I did notice was the fact that if the divisor was a power of 2, then the question would be more trivial as we could just select upper bits for quotient and lower bits for remainder.

After some research, I learned that if the divisor was a fixed constant and never changing, we could calculate the quotient and the remainder in one clock cycle through a combinational circuit.

Proof for this method
Let x be our input, d be our divisor, n be bit size, q be quotient, and r be remainder. Then we would like to get q and r.

```python
q = x / d                  # By definition
q = (x * 2^n) / (d * 2^n)  # By multiplying by 2^n / 2^n
q = (x * (2^n / d)) / 2^n  # By rearranging
q = (x * (2^n / d)) >> n   # By definition of right shift
```

This is faster as we can precompute 2^n / d at the start of the program and multiplication is much faster than division. Then r = x - (q * d) by definition and now we have achieved the quotient and remainder in a single combinatorial circuit.

Limitations of this method is the fact that it needs a constant divisor which is fine for this problem and that the result is an approximation has some edge cases. Also, this method does not work on signed numbers which will be solved later.

Reciprocal division implementation
--

```OCaml
(* Some predefined constants *)
let num_bits = 16
let divisor = 100
let recip_multiplier = (1 lsl num_bits) / divisor

(* Chunk of the main code. *)
let recip_sig = of_unsigned_int ~width:num_bits recip_multiplier in
let hundred_sig = of_unsigned_int ~width:num_bits divisor in
let quotRem100 num = 
  let prod = num *: recip_sig in
  let quot = sel_top prod ~width:num_bits in
  let rem = num -: uresize ~width:num_bits (quot *: hundred_sig) in
  quot, rem
in
```

Instead of shifting right by num_bits on prod, I ended up just using sel_top as this will give me a value with a width of num_bits. Apart from that, it essentially follows explanation above.

Structure of code
---

The I, O, and States modules were from range-finder with modification on O.

```OCaml
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
```

For the main part of my code, I used Always block to help design the circuit. So the entire solution can be found in the state machine.

Fixing signed numbers
---

Problem that arose during the reciprocal division is signed number and this greatly affected the solution as I had to debug to ensure correct position of the dial pointer. I did not want to just take the absolute as I thought it will be harder for part 2 as I will need exactly how many times it went through 0 but I think in the end, there were edge cases regardless. To filter for the signed cases, all I had to do was check the msb of the data in.

Then to resolve this, I managed to figure out is that if I did 100 - dial pointer, this would give me the positive equivalent for the pointer and then I just subtracted the data in. Which was then fed into the quotRem100 to find the quotient and remainder as if I was doing a right move. The only thing, I needed to do after was set the dial pointer to 100 - remainder as this would correct for the fact I did a right move but there were edge cases I will discuss afterwards.

```OCaml
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
```

This is just the left part of Accepting_input state.

There was only edge cases for the part2. The problem was just the 100 - that I did to force right moving properties. What I noticed during testing was this:

| ptr.value == 0 | edge | change |
|----------------|------|--------|
| True           | True | 0      |
| True           | False| -1     |
| False          | True | +1     |
| False          | False| 0      |

So to code this, I had originally used to 2 if statements to first check ptr.value == 0 then edge. But after drawing the truth table, I realised that I can use XOR instead. I am not entirely sure if this is better, I had assumed so as the more common case which is when both are false only has to do 1 if statement compared to the 2 before. This does not help the special case as it sill uses the same amount of if_ statements but probably more work as XOR is now apply this time.

The right move was much simpler than the left side to handle.

```OCaml
(let quot, rem = quotRem100 (ptr.value +: data_in) in
 [ if_
     (rem ==: hundred_sig)
     [ ptr <-- zero num_bits
     ; part2 <-- part2.value +: quot +:. 1
     ; part1 <-- part1.value +:. 1
     ]
     [ ptr <-- rem; part2 <-- part2.value +: quot ]
 ])
```

There was only 1 edge case, which was when (ptr + data in) is a multiple of 100, this is due to the truncation made for the reciprocal division. So, the reciprocal is 2^16 / 100 which is 655 truncates the .36 so, when we have a multiple of 100, we get the quotient to be 1 less as (65500 >> 16) is (65500 / 2^16) = 0.9995 which will get truncated to 0 in my code. The fix is to simply check if the rem is 100 and then set ptr to 0 and increment both parts accordingly.

Testing
---

The testbench is of the same format as the range-finder testbench. So, we iteratively feed the inputs one-by-one. As mentioned earlier, I parse the input in OCaml to feed through. I have not messed with the waves config as of writing but, it is included in the file.

```linux
dune build bin/generate.exe @runtest
bin/generate.exe day1
```

You can run the first command to run all the test, changing the expect test on the testbench, will output diff if you want to verify the running.

The 2nd command is to generate an RTL for day1 in Verilog source into the terminal.

```OCaml
(* I cannot get dune to abbreviate the file path to not expose my system so 
   please if you want to run this with your own input, put the complete file 
   path in and uncomment the 2 lines after. *)

(* let data file = parse @@ String.split_lines @@ In_channel.read_all file
let sample = data "PUT PATH HERE" *)

(* I have put in the sample1 data here so you can run the test with the expect 
   tests. Comment out if using your own input. *)
let sample = parse [ "L68"; "L30"; "R48"; "L5"; "R60"; "L55"; "L1"; "L99"; "R14"; "L82"]
let ( <--. ) = Bits.( <--. )
```

The above sample is from the testbench. If you want to solve your actual input for AOC, get the input as a .txt and uncomment the 2 lines below the top comment and replace the "PUT PATH HERE" to the full path as I cannot get dune to abbreviate path.

Unless there is unaccounted edge cases, the output should match the AOC answer like it did for me.

Conclusion
-----

I had a great time working out the syntax for both OCaml and Hardcaml, there were instances where I had trouble figuring out what the compiler wants or debugging. However, I did find completing this question quite interesting and especially thinking more about hardware like clock cycles or synthesising cost a little as I feel more software-orientated during my course at university. This was definitely an experience to remember.
