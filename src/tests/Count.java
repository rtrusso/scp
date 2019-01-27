class Count
{
  public static void main( String[] args )
  {
    System.out.println( new Test().Go() );
  }
}

class Test
{
  public int Go()
  {
    int i;

    i = 0;
    while( i < 100 )
    {
      i = i + 1;
    }

    return i;
  }
}
