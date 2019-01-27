class MyFactorial // -*-Java-*-
{
  public static void main(String[]args) {
    System.out.println(new Factorial().Go());
  }
}

class Factorial
{
  public int Go()
  {
    return this.Fact(5);
  }

  public int Fact(int n)
  {
    int res;

    if(1<n)
    {
      res = this.Fact(n-1)*n;
    }
    else
    {
      res = 1;
    }

    return res;
  }

}
