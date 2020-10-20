# Nameless-Lang (Draft design)

## A nameless programming language which is ironically its name

1. [Data types](#data-types)
2. [Variables](#variables)
3. [Control sentences](#control-sentences)

## Data types

### Numeric values
Proporsal: numeric data types with different size.

- int (64 bit signed integer)
- uint (64 bit unsigned integer)
- float (a 64 bit floating point number)

### Char
A single character value 

### String
Many chars grouped

### Bool
Represents a boolean value (true or false)

## Variables
Proporsal: variable inmutablitity.

Variables are declared in this form:

```
    [data type] <identifier> = [expression]
```

### Example 
 ```
    int x = 1;

    string str = "hello world";

    float decimal = 123.321;

    bool boolean = true;
 ```

 Note: Semicolon is required on each expression end.

 Each data type is initialized with a default non-null value.
 
 Example:
 ```
    int x; // x is initialized by 0

    string str; // str is initalized with ""
 ```

 #### Inference (Idea)
 Data type can be infered by using the ```let``` keyword.
```
    let x = 23 // infered int type
    let y = "123" // infered string type
```


## Control Sencences
### If statement

