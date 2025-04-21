class Main inherits IO {
  main() : Object {
    {
    let x : Int <- in_int() in
      let y : Int <- in_int() in
        let z : Int <- in_int() in
        if ( y < x ) then
          if ( x < z ) then
            out_int(z)
          else
            out_int(x)
          fi
        else
          if ( y < z ) then
            out_int(z)
          else
            out_int(y)
          fi
        fi;
    }
  } ;
} ;

