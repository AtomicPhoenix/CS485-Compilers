class Main inherits IO {
  main() : Object {
    {
      out_int(f(g(1),h(1)));
    }
  } ;

  f(x : Int, y : Int) : Int {
    {
      x + y;
    }
  } ;

  g(x : Int) : Int {
    {
      x + 5;
    }
  } ;

  h(x : Int) : Int {
    {
      x + 7;
    }
  } ;

} ;


