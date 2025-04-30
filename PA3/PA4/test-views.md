# 25
Consists of using functions as arguments to another function. Could also be an issue with the fact that every param has the same name:

out_int(f(g(1),h(1)));
f(x,y) = x + y
h(x) = x + 7
g(x) = x + 5

# 45
Literally just a massive lambda calculus compiler, no useful information to be gained. Rosetta.cl is our equivalent test file.

# 73
String liters must be translated intostatic string instance, even if the literal also happens to be used internally bu the compiler for other purposes. This is a malicous little test program designed to catch peopole who hard-wired things like "no_class" as a superflous strings that should not be emitted with the rest of the string table.

Examples:
- Out_string("no_class")
- Out_string("_no_class")
- Out_string("__no_class")
- Out_string("prim_slot")
- Out_string("_prim_slot")
- Out_string("__prim_slot")
- Out_string("SELF_TYPE")
- Out_string("_SELF_TYPE")
- Out_string("__SELF_TYPE")

# 81
Divide by zero test
