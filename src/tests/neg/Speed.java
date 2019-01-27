class Speed {
  public static void main( String[] args ) {
    System.out.println( new Test().Go() );
  }
}

class object
{
  public int value() { return 0; }
}

class myint extends object
{
  int i;

  public myint init( int _i ) {
    i = _i;
    return this;
  }

  public int value() { return i; }
}

class Test
{
  public int Go() {
    myint[] args;
    args = new myint[3];
    args[1] = new myint().init( 0 );
    args[2] = new myint().init( 500 /*100000*/ );
    return this.Loop( args );
  }

  public int Loop( myint[] args )
  {
    myint[] newargs;
    myint a;
    myint b;
    int i;

    a = args[1];
    b = args[2];
    i = a.value();

    if( a.value() < b.value() )
    {
      newargs = new myint[3];
      newargs[1] = new myint().init( i.value() + 1 );
      //newargs[1] = new myint().init( a.value() + 1 );
      newargs[2] = args[2];

      return this.Loop( newargs );
    }
    else
    {
      return a.value();
    }
  }
}
