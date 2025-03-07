# Interpreter Project  

## Project Overview  
The goal of this project was to develop an interpreter. The provided grammar files define the syntax of the language. The program first performs data validation, and if the tests pass successfully, the interpreter executes the code.  

## Key Features Considered  
The implementation takes into account the following aspects:  

1. **Data Types** – Three different types  
2. **Literals, Arithmetic, and Comparisons**  
3. **Variables and Assignments**  
4. **Print Function**  
5. **Control Structures** – `while` loops and `if` statements  
6. **Functions and Procedures** – Including recursion  
7. **Parameter Passing** – By variable, by value, and in/out  
8. **Read-Only Variables and `for` Loops**  
9. **Variable Shadowing and Static Binding**  
10. **Runtime Error Handling**  
11. **Functions Returning Values**  
12. **Static Typing**  
13. **Nested Functions with Static Binding**  
14. **Records, Lists, Arrays, and Multidimensional Arrays**  
15. **Tuples with Assignment**  
16. **Loop Control Statements** – `break` and `continue`  
17. **Higher-Order Functions** – Including anonymous functions and closures  
18. **Generators**  

## Testing  
A comprehensive suite of tests has been included to verify the correctness of the interpreter. These tests cover:  

- **Syntax validation** – Ensuring that invalid programs are correctly rejected.  
- **Type checking** – Verifying that types are enforced properly.  
- **Error handling** – Testing how the interpreter reacts to incorrect input and exceptions.  
- **Expected outputs** – Running valid programs and checking if they produce the expected results.  

## Additional Information  
Test cases are included to validate the functionality. Additionally, a slightly modified version of the project, incorporating feedback from the course instructor, can be found in the `michal_sobczak` folder.  
