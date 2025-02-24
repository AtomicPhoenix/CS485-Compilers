class Main inherits IO {
    main() : Object {{
        out_string("a");
        not 1;
    }};

};

class NewClass {
    a : Int;
    init(b: Int, b : Int) : NewClass {{ a <- b; self; }};
    print_letter() : Object { out_string("a") };
};

