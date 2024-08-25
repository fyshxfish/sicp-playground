# Chapter 1 (Dirty Note)

## about Exercise 1.6

`new-if` defined in this exercise is a `function` instead of a special form like `if`, therefore the parameters of this function must be evaluated before the procedure application, which causes the endless recursion of the third param `sqrt-iter`.

-> the semantics of function(`new-if`) and special form (`if`) differ?


