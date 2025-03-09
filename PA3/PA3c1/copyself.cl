class Main inherits IO {
    main() : Object {
        let a : A <- (new A).init(3) in
            let a2 : A <- a.copyself() in {
            out_int(a2.getb().getc());
            out_string("\n");
            a2.getb().setc(7);
            out_int(a.getb().getc());
            out_string("\n");
            out_int(a.b());
            out_string("\n");
            a.seta(27);
            out_int(a2.b());
            out_string("\n");
            out_int(a.b());
            out_string("\n");

            }};
};
class A {
a : Int;
b : B;
    b() : Int { a };
    init(num : Int) : SELF_TYPE {{
        a <- num;
        b <- (new B).init(num);
        self;
    }};
    getb() : B { b };
    copyself() : SELF_TYPE { copy() };
seta(num : Int) : Object { a <- num };
};

class B {
c : Int;
    init(num : Int) : SELF_TYPE {{ c <- num; self; }};
    getc() : Int { c };
setc(num : Int) : Object { c <- num };
};
