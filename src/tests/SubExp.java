class SubExp {
  public static void main( String[] args )
  {
    System.out.println( new Test().Func() );
  }
}

class Test
{
  public int Func()
  {
    return this.Func1() + this.Func2( 9, 10 );
  }

  public int Func1()
  {
    int a;
    int b;

    a = 1;
    b = 3;

    return this.Add( a + b, 2 * ( a + b ) );
  }

  public int Func2( int a, int b )
  {
    return this.Add( a + b, 2 * ( a + b ) );
  }

  public int Add( int i, int j )
  {
    return i + j;
  }
}

class Test2
{
  int x;
  int y;
  int[] a;

  public int Init()
  {
    x = 1;
    y = 2;
    a = new int[10];
    return x;
  }

  public int Go()
  {
    return a[x+2*y];
  }

  public int Go2( int x, int y )
  {
    return a[x+2*y];
  }
}
