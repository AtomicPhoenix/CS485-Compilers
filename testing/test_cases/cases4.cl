class Main inherits IO {
 my_attribute : C <- new C ;
  
  main() : Object { 
      {
        case my_attribute of
         c1 : A => out_string("This is wrong\n");
         c2 : Object => out_string("This is correct!!\n");
         c3 : D => out_string("This is wrongD\n");
        esac;
      }
  } ;
} ;

class A {};
class B {};
class C inherits B {};
class D inherits B {};
