class Main inherits IO {
    main() : Object {
        let a : A <- new A in {
            out_int(a.a());
            out_string("\n");
            a.seta(73);
            out_int(a.a());
            out_string("\n");
            out_int(a.seta(45));
            out_string("\n");
        }

    };
};

class A inherits IO {
    a : Int <- 5;
    a() : Int { a };
    seta(b : Int) : Int { a <- b };
};

class B inherits IO {
    a : Int <- 5;

    a() : Int {a};

    seta(b : Int) : Int { a <- b };

    b() : Object {{
            out_int(a());
            out_string("\n");
            a <- 73;
            out_int(a());
            out_string("\n");
            out_int(a <- 45);
            out_string("\n");
            a <- 64;
            out_int(a);
            out_string("\n");
        }

    };
};
