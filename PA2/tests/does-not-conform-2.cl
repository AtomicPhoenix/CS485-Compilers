class Foo {
    int : Int;

    get() : SELF_TYPE {
	int <- 5
    };
};

class Bar inherits Foo {
   
};


class Main inherits IO {
    main() : Object {
	let a : Foo <- new Object in {
	    out_string("");
	}
    };


};
