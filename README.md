# Pascal Compiler extension to allow formal parameters to be arrays of arbitrary size.

This is my final submission for the 2024 compilers assignment - the PDF "Matthew_Zahra_Compilers_Assignment.pdf" details the work that I completed.

The compiler is written in OCaml and is for a version of Pascal. 

The intial compiler does not allow for arrays of arbitrary size to be used as a function's argument. I extended the compiler to allow for this, allow for pointers to such arrays and manage other areas such as side effects.

This involved me editing the context-free grammar, adding new types of nodes in the Abstract Syntax Trees, managing code generation amongst other things.
