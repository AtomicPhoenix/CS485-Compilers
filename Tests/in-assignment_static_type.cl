class Main inherits IO {
    main() : Object {{
        out_string("Hello, world!\n");
        let s : A <- (new B).init(2, 3) in
            out_int(s.get_a());
        out_string("\n");
    }};

};

class A {
    a : Int;
    init(b : Int, c : Int) : SELF_TYPE {{
        a <- b;
        self;
    }};
    get_a() : Int { a };

};
class B inherits A {
    b : Int;
    init(c : Int, d :Int) : SELF_TYPE {{
        a <- c;
        b <- d;
        self;
    }};
};
