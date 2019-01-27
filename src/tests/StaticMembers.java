class StaticMembers
{
  public static void main( String[] args )
  {
    System.out.println( A.Test() );
  }
}

class A
{
  static int i;

  public static int Init()
  {
    i = 0;
    return 0;
  }

  public static int Test()
  {
    int tmp;
    tmp = A.Init();
    i = i + 1;
    i = i + i;
    return i;
  }
}
