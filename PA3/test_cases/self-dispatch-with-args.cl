class Main inherits IO {
  my_attribute : Int <- 5 ;
  main() : Object {
    let x : X <- New X in
      x.exec("hi", "bye")
  } ;
} ;



class X inherits IO {
  exec(x : String, y : String) : Object {
    self.exec2(x,y)
  } ;
  
  exec2(x : String, y : String) : Object {
    out_string("Hello, world from X.\n")
  } ;
};

