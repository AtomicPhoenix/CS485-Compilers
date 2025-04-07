class Main inherits IO {
    main() : Object {{
        let a : A <- (new A), b : B <- (new B) in
            if a = b then
                out_string("a == b!\n")
            else
                out_string("a != b!\n")
            fi;
        let a : A, b : B in
            if a = b then
                out_string("void a == void b!\n")
            else
                out_string("void a != void b!\n")
            fi;

    }};


};

class A inherits IO {
    print_letter() : Object { out_string("A") };
};
class B inherits IO {
    print_letter() : Object { out_string("B") };
};
