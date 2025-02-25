class A {
self : SELF_TYPE;
};
class Main inherits IO {
m : Main <- new Main;
    main() : Object {
        out_string(m@SELF_TYPE.type_name())
    };
};
