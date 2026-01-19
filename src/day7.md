Day 7
===

This will be the next day, I will attempt as I think this day is very well suited to hardcaml.

My idea is to make use of RAM which will store a line of the state of the previous line with the beams in the correct position. With this, I can feed the next line and we can check if the previous line has a beam so a 1 and then do the appropriate action depending if there is a splitter or not. Doing this till we reach the finish and then just read all the 1s on the RAM and that is part 1 done.

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

Repeating this until all inputs are fed so then for part 1 it is just the sum of non-zero positions and part 2 is just the sum.
