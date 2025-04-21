class Main inherits IO {
  s : String <- "Hello world!a";

  main() : Object {
    {
      let s2 : String <- s.substr(0,12) in
        out_string(s2);
    }
  } ;
} ;

