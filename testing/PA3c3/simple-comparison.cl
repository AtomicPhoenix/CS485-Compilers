class Main inherits IO {
  main() : Object {
    {
          let t1 : Bool in 
            let t2 : Bool in 
              let t3 : Bool in {
                t1 <- true;
                t2 <- false;
                t3 <- true;
                if ( t1 ) then
                    out_int(1)
                else
                    out_int(0)
                fi;
                if ( t2 ) then
                    out_int(1)
                else
                    out_int(0)
                fi;
                if ( t3 ) then
                    out_int(1)
                else
                    out_int(0)
                fi;


      };
    }
  } ;
} ;

