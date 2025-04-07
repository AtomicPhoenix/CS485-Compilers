class Main inherits IO {
  my_attribute : Int <- 5 ;
  main() : Object {
    {
    let a : A in 
      a.getInt();
    out_int(my_attribute+5);
    out_string("Hello, world.\n");
    }
  } ;
} ;

class A {
  a: Int <- 5;
  
  getInt() : Int {
    a
  };
};
