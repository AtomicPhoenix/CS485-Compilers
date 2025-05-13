class Main inherits IO {
    main() : Object {

        let a : A <- (new A), b : A <- (new A) in
        if a <= b then
            out_string("a is less than b\n")
        else
            out_string("a is greater than b!\n")
        fi
    };

};

class A inherits IO {
a : Int;
};
