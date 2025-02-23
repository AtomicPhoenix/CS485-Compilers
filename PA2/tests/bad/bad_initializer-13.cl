class Main inherits IO {
    main() : Object {{
        out_string("a");
    }};

};

class NewClass {
    a : Int <- "cat";
    init(b: Int) : NewClass {{ a <- b; self; }};
    print_letter() : Object { out_string("a") };
};

