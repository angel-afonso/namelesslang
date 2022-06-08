![Rust](https://github.com/angel-afonso/namelesslang/workflows/Rust/badge.svg)

# Nameless-lang

An unnamed programming language that ironically is its name

Nameless-lang is heavily inspired by rust (and is also written in rust)

I wrote this language as a practice for reading `Writing an interpreter in go` and `Writing a compiler in go`. Extending the implementation explained in the books and adding more features

## Why the name "nameless"?

Good question, everything goes back to me, thinking for a long time names and seeing that there is already a project with that name on github, until it occurred to me that the best name is no name

### A quick look to Nameless

```
 fn main() {
    println(factorial(5));
 }

 fn factorial(number) {
    if number == 1 {
        return 1;
    }

    return number * factorial(number - 1);
}

```
