class Main inherits IO {
  my_attribute : Int <- 5 ;
  main() : Object {
    let a : Int, b : Int in {
    a <- in_int();
    b <- in_int();
    if (true) then
      out_int(a)
      --1
    else
      out_int(~a)
      --2
    fi;
    if (a < b) then
      out_int(a)
      --1
    else
      out_int(b)
      --2
    fi;
    let c : Int in {
    c <- if a < b then a else b fi;
    out_int(c);
    };
  }} ;
} ;

class A {
  a: Int <- 5;
};
