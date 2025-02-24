class Main inherits IO {
    main() : Object {
        if 1 then
            out_string("oh no")
        else
            out_string("oh no but else")
        fi
    };

};

class NewClass {
    a : Int;
    init(b: Int) : NewClass {{ a <- b; self; }};
    print_letter() : Object { out_string("a") };
};

