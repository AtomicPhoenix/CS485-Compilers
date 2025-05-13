class Main inherits IO {
    main() : Object {

        let a : A, b : B in
        if a = b then
            out_string("a is equal to b\n")
        else
            out_string("a is not equal to b!\n")
        fi
    };

};

class A inherits IO {
a : Int;
};
class B inherits IO {
a : Int;
};
