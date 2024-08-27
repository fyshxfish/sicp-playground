# Chapter 1 (Dirty Note)

## about Exercise 1.6

`new-if` defined in this exercise is a **function** instead of a special form like `if`, therefore the parameters of this function must be evaluated before the procedure application, which causes the endless recursion of the third param `sqrt-iter`.

[more](https://sicp-solutions.net/post/sicp-solution-exercise-1-6/)

-> the semantics of functions(like `new-if`) and special forms (like `if`) differ?


## about **recursive process** and **iterative process**

- resume computation

- implementation on register machine without auxiliary memory

## **recursive procedure** v.s. **recursive process** 

Not the same thing.

## trouble shooting 

1. Error: `The object 10 is not applicable.`

    Check whether a numerical argument is placed in the position of a function (abstraction).

    e.g.

    ```scheme
    (cond 
        ((close-enough? mid) (mid))         ; fix: remove the parenthesis around the second `mid`
        ; --snip--
    )
    ```

    Parenthesis around the second `mid` make the interpreter view `mid` as an abstraction instead of a numerical value.

    Conclusion: BE CAREFUL WITH PARENTHESES. 

## about/define **first class**

> Elements with the fewest restrictions are said to have *first-class* status. (sicp(2nd edition) page 102)


