class Main inherits IO {
  main() : Object {
    {
      a(1);
      b(1,2);
      c(1,2,3);
      d(1,2,3,4);
      e(1,2,3,4,5);
      f(1,2,3,4,5,6);
      g(1,2,3,4,5,6,7);
      h(1,2,3,4,5,6,7,8);
      i(1,2,3,4,5,6,7,8,9);
      j(1,2,3,4,5,6,7,8,9,10);
      k(1,2,3,4,5,6,7,8,9,10,11);
      l(1,2,3,4,5,6,7,8,9,10,11,12);
    }
  } ;
  a(a: Int) : Object {
    {
      out_int(a);
      out_string("\n");
    }
  } ;

  b(a: Int, b : Int) : Object {
    {
      out_int(a + b);
      out_string("\n");
    }
  } ;


  c(a: Int, b : Int, c : Int) : Object {
    {
      out_int(a + b + c);
      out_string("\n");
    }
  } ;


  d(a: Int, b : Int, c : Int, d : Int) : Object {
    {
      out_int(a + b + c + d);
      out_string("\n");
    }
  } ;

  e(a: Int, b : Int, c : Int, d : Int, e : Int) : Object {
    {
      out_int(a + b + c + d + e);
      out_string("\n");
    }
  } ;

  f(a: Int, b : Int, c : Int, d : Int, e : Int, f : Int) : Object {
    {
      out_int(a + b + c + d + e + f);
      out_string("\n");
    }
  } ;


  g(a: Int, b : Int, c : Int, d : Int, e : Int, f : Int, g : Int) : Object {
    {
      out_int(a + b + c + d + e + f + g);
      out_string("\n");
    }
  } ;

  h(a: Int, b : Int, c : Int, d : Int, e : Int, f : Int, g : Int, h : Int) : Object {
    {
      out_int(a + b + c + d + e + f + g + h);
      out_string("\n");
    }
  } ;


  i(a: Int, b : Int, c : Int, d : Int, e : Int, f : Int, g : Int, h : Int, i : Int) : Object {
    {
      out_int(a + b + c + d + e + f + g + h + i);
      out_string("\n");
    }
  } ;

  j(a: Int, b : Int, c : Int, d : Int, e : Int, f : Int, g : Int, h : Int, i : Int, j : Int) : Object {
    {
      out_int(a + b + c + d + e + f + g + h + i + j);
      out_string("\n");
    }
  } ;

  k(a: Int, b : Int, c : Int, d : Int, e : Int, f : Int, g : Int, h : Int, i : Int, j : Int, k : Int) : Object {
    {
      out_int(a + b + c + d + e + f + g + h + i + j + k);
      out_string("\n");
    }
  } ;

  l(a: Int, b : Int, c : Int, d : Int, e : Int, f : Int, g : Int, h : Int, i : Int, j : Int, k : Int, l : Int) : Object {
    {
      out_int(a + b + c + d + e + f + g + h + i + j + k + l);
      out_string("\n");
    }
  } ;
} ;

