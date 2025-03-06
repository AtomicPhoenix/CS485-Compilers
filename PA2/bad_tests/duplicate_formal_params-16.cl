class Main inherits IO {
    main() : Object {{
        out_string("a");
    }};

};

class NewClass inherits IO {
    a : Int;
    init(b: Int, 
            b : Int) : NewClass {{ a <- b; self; }};
    print_letter() : Object { out_string("a") };
};

