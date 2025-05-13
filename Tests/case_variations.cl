class Main inherits IO {
    a : SELF_TYPE;
    b : Main;
    c : Int;
    stupidcase() : Object { abort() };

    init(d : Int) : SELF_TYPE { { c <- d; self;} };
    mainwrapper() : Int {{
    let i : MainInh <- (new MainInh).init(7) in
        i.casetest(i.get_self());
    1;
    }};
    main() : Object {
        mainwrapper()
    };
};
class MainInh inherits Main {
    e: MainInh ;   
   casetest(z : MainInh) : Object {
        case z of
            f : Main => out_string("Main\n");
            g : Object => out_string("Object\n");
            h : Whatever => out_string("Whatever\n");
        esac
   };
   e() : MainInh { e };
   get_self() : SELF_TYPE { self };
};
class Whatever inherits IO {};
