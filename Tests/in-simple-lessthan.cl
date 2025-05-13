class Main inherits IO {
  main() : Object {
    {
    let x : Int <- in_int() in
      let y : Int <- in_int() in
        if ( y < x ) then
          out_int(x)
        else
          out_int(y)
        fi;
    }
  } ;
} ;

