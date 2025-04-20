class Main inherits IO {
  main() : Object {
    {
      foo(1,2,3,4,5);
    }
  } ;

  foo(a: Int, b:Int, c:Int, d: Int, e:Int) : Object {
    {
      out_int(a + b + c + d + e);
    }
  } ;

} ;

