class ObjArray
{
  public static void main( String[] args )
  {
    System.out.println( new A().Test() );
  }
}

class A
{
  public int Test()
  {
    B[] args;

    args = new B[3];

    args[0] = new B();
    args[1] = new B();
    args[2] = new B();

    return (args[2]).Foo();
  }
}

class B
{
  public int Foo()
  {
    return 3;
  }
}
