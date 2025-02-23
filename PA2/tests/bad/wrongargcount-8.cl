class Main inherits IO {
    main() : Object {
        let a : NewClass <- (new NewClass).init(1, 2) in
            a.print_letter()
    };

};

class NewClass {
    a : Int;
    init(b: Int) : NewClass {{ a <- b; self; }};
    print_letter() : Object { out_string("a") };
};

