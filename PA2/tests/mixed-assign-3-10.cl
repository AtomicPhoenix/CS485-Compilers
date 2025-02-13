class Foo {
    int : Int;
};


class Main inherits IO {
    main() : Object {
	let foo : Int <- new Foo in {
	    out_string("hi");
	}
    };

};
