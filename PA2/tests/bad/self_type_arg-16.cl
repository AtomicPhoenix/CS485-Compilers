class Foo {
    getInt(y: SELF_TYPE) : Int {
	new Int
    };
};

class Bar inherits Foo {
};


class Main inherits IO {
    main() : Object {
	let x : Foo <- new Foo in {
	    x.copy();
	}
    };

};
