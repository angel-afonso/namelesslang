![Rust](https://github.com/angel-afonso/namelesslang/workflows/Rust/badge.svg)

# Nameless-lang

A nameless programing language which is ironically it's name

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

# Nameless Virtual Machine preview

The interpreter implementation is being moved to a virtual machine (specifically, a stack machine).

This can boost the performance of the language and allow to introduce more features

