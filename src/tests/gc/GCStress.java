class GCStress {
  public static void main( String[] args ) {
    System.out.println( new A().Test() );
  }
}

class A {
  public int Test()
  {
    int max;
    int iter;
    int dontcare;
    int idx;
    B[] waste;
    B temp;

    max = 128;
    iter = 0;
    idx = 0;

    waste = new B[32];

    while( iter < max )
    {
      System.out.println(989898);
      System.out.println(iter);
      System.out.println(idx);
      System.out.println(waste.length);
      temp = new B();
      waste[idx] = temp;

      if( waste.length < 1 )
      {
        waste[10000000] = new B();
      }
      else
      {
      }

      dontcare = temp.Init();

      if( waste.length < 1 )
      {
        waste[30000000] = new B();
      }
      else
      {
      }


      idx = idx + 1;
      iter = iter + 1;

      if ( ! ( idx < waste.length  ) )
      {
        idx = 0;
      }
      else
      {
      }
    }

    return waste.length;
  }
}

class B {
  int[] stuff;

  public int Init() {
    stuff = new int[1024];
    return stuff.length;
  }
}
