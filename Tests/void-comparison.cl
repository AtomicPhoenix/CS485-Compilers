class Main inherits IO {
  main() : Object {
    {
    if (new Int)<5 then
      out_string("(new Int)<5\n")
    else
      out_string("(new Int)>=5\n")
    fi;
    let a : A, b : B in
        if a = b then
            out_string("a is equal to b\n")
        else
            out_string("a is not equal to b!\n")
        fi;
    let a : A, b : B in
        if a < b then
            out_string("a is less than b\n")
        else
            out_string("a is not less than b!\n")
        fi;
    let a : A, b : B in
        if a <= b then
            out_string("a is less/equal than b\n")
        else
            out_string("a is not less/equal than b!\n")
        fi;
    let a : A <- new A, b : B in
        if a = b then
            out_string("a is equal to b\n")
        else
            out_string("a is not equal to b!\n")
        fi;
    let a : A <- new A, b : B in
        if a < b then
            out_string("a is less than b\n")
        else
            out_string("a is not less than b!\n")
        fi;
    let a : A <- new A, b : B in
        if a <= b then
            out_string("a is less/equal than b\n")
        else
            out_string("a is not less/equal than b!\n")
        fi;
    let a : A <- new A, b : A <- new A in
        if a < b then
            out_string("a is less than b\n")
        else
            out_string("a is not less than b!\n")
        fi;

    }
  };
};


class A inherits IO {
a : Int;
};
class B inherits IO {
a : Int;
};
