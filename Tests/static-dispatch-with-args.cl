class Main inherits IO {
  my_attribute : Int <- 5 ;
  main() : Object {
    let x : X <- New X in
      x@Z.exec(1)
  } ;
} ;

class X inherits Z {
  exec(a : Int) : Object {
    {
      out_string("Hello, world from X.\n");
      out_string("The passed argument was: ");
      out_int(a);
      out_string("\n");
    }
  } ;
};

class Z inherits IO {
  exec(a : Int) : Object {
      {
      out_string("Hello, world from Z.\n");
      out_string("The passed argument was: ");
      out_int(a);
      out_string("\n");
      }
  } ;
};
