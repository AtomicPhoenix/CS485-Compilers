class Main inherits IO {
 my_attribute : C <- new C ;
  
  main() : Object { 
      {
        case my_attribute of
         c1 : A => out_string("This is wrong\n");
         c1 : C => out_string("This is Correct\n");
         c3 : D => out_string("This is wrongD\n");
        esac;
      }
  } ;
} ;

class A {};
class B {};
class C inherits B {};
class D inherits B {};
class A1 {};
class B1 {};
class C1 inherits A {};
class D1 inherits B {};
class A2 {};
class B2 {};
class C2 inherits C {};
class D2 inherits B {};
class A21 {};
class B21 {};
class C21 inherits D {};
class D21 inherits B {};
