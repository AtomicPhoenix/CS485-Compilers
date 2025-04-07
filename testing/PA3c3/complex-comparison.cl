class Main inherits IO {
  main() : Object {
    {
    let x : Int <- in_int() in
      let y : Int <- in_int() in
        let z : Int <- in_int() in
          let t1 : Bool in 
            let t2 : Bool in 
              let t3 : Bool in {
                t1 <- y < x;
                t2 <- y < z;
                t3 <- x < z;
                if ( t1 ) then
                  if ( t3 ) then
                    out_int(z)
                  else
                    out_int(x)
                  fi
                else
                  if ( t2 ) then
                    out_int(z)
                  else
                    out_int(y)
                  fi
                fi;
      };
    }
  } ;
} ;

