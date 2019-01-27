class Speed2 {
  public static void main( String[] args ) {
    System.out.println( new Test().Go() );
  }
}

// how many times am I hitting the GC code?  Is the slowness in malloc() or gc()?
// based on current tests, even my slow tail call is *free*.  validate that?
// what transforms is GCJ doing that make it so damn fast?

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
    args[2] = new myint().init( /*5*/ 100000 );
    return this.LoopLessSlow2( args );
  }

  public int LoopLessSlow2( myint[] args )
  {
    myint a;
    myint b;

    a = args[1];
    b = args[2];

    while( a.value() < b.value() )
    {
      args = new myint[3];
      args[1] = new myint().init( a.value() + 1 );
      args[2] = b;
      a = args[1];
      b = args[2];
    }

    return a.value();
  }

  public int LoopLessSlow( myint[] args )
  {
    myint a;
    myint b;

    a = args[1];
    b = args[2];

    while( a.value() < b.value() )
    {
      a = new myint().init( a.value() + 1 );
    }

    return a.value();
  }

  public int LoopFast( myint[] args )
  {
    myint[] newargs;
    myint a;
    myint b;
    int i;
    int j;

    a = args[1];
    b = args[2];
    i = a.value();
    j = b.value();

    while( i < j )
    {
      i = i + 1;
    }

    return i;
  }
}
