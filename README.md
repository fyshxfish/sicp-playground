# sicp-playground

## Scheme Enviroment Set-up

MacOS:

```bash
brew install mit-scheme
```

Ubuntu:

```bash
sudo apt-get update 
sudo apt-get install mit-scheme
```

## REPL Basics

1. Enter the scheme repl:

    ```bash
    mit-scheme
    ```

2. Load a scheme file in repl:

    ```
    1 ] => (load "path/to/file")
    ```

3. Clear the screen: 

    cmd+k (MacOS)

<!-- 4. You don't have to RESTART if enter an illegal statement in REPL.  
    
    You can keep going despite the error. -->

4. If your REPL hangs after you enter a line of code: 
    
    Check whether your parentheses are properly matched.

5. Scheme is case-insensitive.

6. History variable (procedure): Reuse the returned procedure in REPL:

    ```scheme
    1 ]=> (average-dump square)

    ;Value: #[compound-procedure 12]    ; can be reused in subsequent expressions, like history variable `$1` in gdb

    1 ]=> (#[compound-procedure 12] 10)

    ;Value: 55

    1 ]=> (define newfunc #[compound-procedure 12])     

    ;Value: newfunc

    1 ]=> (newfunc 10)

    ;Value: 55
    ```

    (So what exactly is this compound-procedure? machine code, code evaluated by means of substitute model, or something?)


## Resources 

- Structure and Interpretation of Computer Programs (Second Edition)

- Lambda-calculus and combinators, an introduction



