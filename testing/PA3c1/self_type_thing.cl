class Main inherits IO {
    main() : Object {
        out_string("Hello, world!\n")
    };

};

class A inherits IO {
    thing : A;
    thing2 : SELF_TYPE;
    init() : SELF_TYPE {{
        thing <- (new A);
        thing2 <- (new SELF_TYPE);
        self;
    }};

};
