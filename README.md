# aoc2021

Advent Of Code 2021

This is my attempt att learning Common Lisp (having previous experience with Scheme), so far it's been mostly "learn the loop macro"...

Some "rules" I made up along the way:

* No external libraries, just the standard library
* Don't fix ugly/buggy code once the right answer is produced, show it "Warts and all"

In particular the code for part 1 of day 10 is wrong! it produces the right answer "by accident".

I had trouble with day 14, `aoc14_2.lisp` is basically starting from scratch.

## Running examples

All examples should be possible to load with eg `(load "aoc13.lisp")` and then run with `(run)` and `(run2)` (for the second part for that day).

The code expects the data to be in `aocxx.input` in the same directory.
