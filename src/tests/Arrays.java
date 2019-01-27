class Arrays {
  public static void main(String[] args) {
    System.out.println( new A().Test() );
  }
}

class A {
  public int Test() {
    int[] a;
    int iter;

    a = new int[32];
    iter = 0;

    while( iter < 32 ) {
      a[iter] = iter;
      iter = iter + 1;
    }

    System.out.println(a.length);
    iter = 0;
    while( iter < 32 ) {
      System.out.println(a[iter]);
      iter = iter + 1;
    }
    return 9999;
  }
}
