class Main inherits IO {
    main() : Object {

        let a : A, b : B in
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
class B inherits IO {
a : Int;
};
