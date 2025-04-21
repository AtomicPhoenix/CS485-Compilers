class Main inherits IO {
  main() : Object {
    {
      foo(1,2,3,4);
    }
  } ;

  foo(a: Int, b:Int, c:Int, d: Int) : Object {
    {
      out_int(a + b + c + d);
    }
  } ;

} ;

