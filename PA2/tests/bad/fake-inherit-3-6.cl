class Foo {
    int : Int;
};

class Bar inherits Foo {
    

    getInt() : Int {
	int
    };
};


class Main inherits IO {
    main() : Object {
      let x : Bar <- new Bar in {
	    x@Main.getInt();
	}
	
    };

};
