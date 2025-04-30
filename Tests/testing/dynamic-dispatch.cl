class Main inherits IO {
  my_attribute : Int <- 5 ;
  main() : Object {
    let x : X <- New X in
      x.exec()
  } ;
} ;



class X inherits Y {
};

class Y inherits Z {
  exec() : Object {
    out_string("Hello, world from Y.\n")
  } ;
};

class Z inherits IO {
  exec() : Object {
    out_string("Hello, world from Z.\n")
  } ;
};
