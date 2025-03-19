class Main inherits IO {
  my_attribute : Int <- 5 ;
  main() : Object {
    let x : X <- New X in
      x@Z.exec()
  } ;
} ;



class X inherits Z {
  exec() : Object {
    out_string("Hello, world from X.\n")
  } ;
};

class Z inherits IO {
  exec() : Object {
    out_string("Hello, world from Z.\n")
  } ;
};
