class Main inherits IO {
  main() : Object {
    {
    let x : Int <- in_int() in
      let y : Int <- in_int() in
        if ( y = x ) then
          out_int(1)
        else
          out_int(0)
        fi;
    }
  } ;
} ;

