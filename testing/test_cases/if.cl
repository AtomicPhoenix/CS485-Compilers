class Main inherits IO {
  my_attribute : Int <- 5 ;
  main() : Object {
    if (true) then
      let x : Int <- 1 in
      let y : Int <- 1 in
        x + y
    else
      let x : Int <- 0 in
      let y : Int <- 0 in
        x + y
    fi
  } ;
} ;

class A {
  a: Int <- 5;
};
