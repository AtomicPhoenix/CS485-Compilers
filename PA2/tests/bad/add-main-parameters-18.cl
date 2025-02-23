class Foo {
    int : Int;

    getInt() : Int {
	5
    };
};

class Bar inherits Foo { fizz : Int ; };



class Main inherits IO {
    main(x : Int) : Object {
      let x : Foo <- new Foo in {
	    let y : Foo <- new Bar in {
		if (x <- y) then 5 else 6 fi;
	    };
	}
    };
};
