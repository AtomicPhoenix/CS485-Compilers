class Main inherits IO {
    main() : Object {
        let a : ClassA <- (new ClassB).init(1, 2) in
        out_string(a.type_name())
    };

};

class ClassA {
    a : Int;
    b : Int;
    init(c: Int, d : Int) : SELF_TYPE {{
        a <- c;
        b <- d;
        self;
    }};
};


class ClassB inherits ClassA {
    init(c: Int, d : Int) : ClassB {{
        a <- c;
        b <- d;
        self;
    }};
};
