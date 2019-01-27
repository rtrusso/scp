class GCStress2 {
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
      System.out.println(iter);
      System.out.println(idx);
      temp = new B();
      waste[idx] = temp;
      dontcare = temp.Init( iter );
      dontcare = temp.Print();

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

  public int Init( int iter ) {
    int count;

    stuff = new int[1024];

    count = 0;
    while( count < 16 ) {
      stuff[count] = count * iter;
      count = count + 1;
    }

    return stuff.length;
  }

  public int Print() {
    int count;

    count = 0;
    while( count < 16 ) {
      System.out.println(stuff[count]);
      count = count + 1;
    }

    return 0;
  }
}
