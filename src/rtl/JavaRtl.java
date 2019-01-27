class JavaRtl
{
  public static void main( String[] args )
  {
    {}
  }
}

class String
{
  char[] c;

  public String( char[] chars )
  {
    c = chars;
  }

  public int length()
  {
    return c.length;
  }

  public char charAt( int i )
  {
    return c[i];
  }

  public char[] toCharArray() {
    return c;
  }

  public String append( String s )
  {
    int len;
    int idx;
    char[] newchars;

    len = c.length + s.length();
    newchars = new char[len];

    idx = 0;
    while( idx < c.length )
    {
      newchars[idx] = c[idx];
      idx = idx + 1;
    }

    idx = 0;
    while( idx < s.length() )
    {
      newchars[idx+c.length] = s.charAt( idx );
      idx = idx + 1;
    }

    return new String( newchars );
  }
}
