class Foo {
    int : Int;
};


class Main inherits IO {
    main() : Object {
	let a : Int <- 5 in {
	    let f : Foo <- new Foo in {
		a <- f;
	    };
	}
    };

};
