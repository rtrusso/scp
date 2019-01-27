class test2 {
  public static void Main( string[] args ) {
    System.Console.WriteLine( new Test().Go() );
  }
}

class Test
{
  virtual public int Go() {
    string s;
    char[] c;

    s = "asdf";
    c = new char[1];

    s = new string(c);

    return 1;
  }
}
