class Main inherits IO {
  my_attribute : Int <- 5 ;
  main() : Object {
    {
      recurse();
    }
  } ;

    recurse() : Object {
    {
      let obj : Object <- new Object in
        recurse();
    }
    };
} ;

class A {
  a: Int <- 5;
  
  getInt() : Int {
    a
  };
};
