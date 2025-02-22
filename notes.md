# Cool Typechecking and Runtime Organization
## SELF_TYPE
- We want SELF_TYPE so that we can type check inherited methods.
- SELF_TYPE extends the type system (makes it more expressive) as the cost of added type-checking complexity
- Subtyping T1 <= T2
- Least Upper Bound: lub(T1,T2)
- Extend operations to handle SELF_TYPE

### Type Rules
- R, M, C |--  e: T where C is the *enclosing class* 
- An expression e occuring in the body of C has static type T given a variable type environment R and a method signature M
- Need to change method dispatch rules that check if method return type is SELF_TYPE

### Where is SELF_TYPE permitted
- in "m(x :  T) : T' {...}" only T' (not T) can be SELF_TYPE

# Run Time Environments
## Virtual Memory
- Address Space: Partial mapping from addresses to values. Likely a Big array.
