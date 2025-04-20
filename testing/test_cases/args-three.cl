class Main inherits IO {
  main() : Object {
    {
      foo(1,2,3);
    }
  } ;

  foo(a: Int, b:Int, c:Int) : Object {
    {
      out_int(a + b + c);
    }
  } ;

} ;

