class Rectangles { // -*-Java-*-
  public static void main(String[]args)
  {
    System.out.println(new Foo().Main());
  }
}

class Foo
{
  public int Main()
  {
    int nt;

    nt = this.Test( 0, 0, 100, 100, 
                    10, 10, 50, 50 );
    nt = this.Test( 10, 10, 50, 50,
                    0, 0, 100, 100 );

    nt = this.Test( 0, 0, 2, 2,
                    1, 1, 0, 0 );
    nt = this.Test( 0, 0, 2, 2,
                    0, 0, 1, 1 );

    nt = this.Test( 0, 0, 21, 21,
                    10, 10, 10, 10 );
    nt = this.Test( 10, 10, 10, 10,
                    20, 20, 10, 10 );

    return 9999;
  }

  public int Test( int x1, int y1, int cx1, int cy1, int x2, int y2, int cx2, int cy2 ) {
    boolean b;
    int res;
    Rect r1;
    Rect r2;
    int zero;

    zero = 0;
    r1 = new Rect().SetUL(new Point().SetX(x1).SetY(y1)).SetSZ(new Size().SetCX(cx1).SetCY(cy1));
    System.out.println( x1 );
    System.out.println( r1.UL().X() );

    System.out.println( y1 );
    System.out.println( r1.UL().Y() );

    System.out.println( cx1 );
    System.out.println( r1.SZ().CX() );

    System.out.println( cy1 );
    System.out.println( r1.SZ().CY() );

    r2 = new Rect().SetUL(new Point().SetX(x2).SetY(y2)).SetSZ(new Size().SetCX(cx2).SetCY(cy2));

    System.out.println( x2 );
    System.out.println( r2.UL().X() );

    System.out.println( y2 );
    System.out.println( r2.UL().Y() );

    System.out.println( cx2 );
    System.out.println( r2.SZ().CX() );

    System.out.println( cy2 );
    System.out.println( r2.SZ().CY() );

    b = r1.Contains(r2);

    if( b )
    {
      res = 1;
    }
    else
    {
      res = 0;
    }

    System.out.println(zero-9999);
    System.out.println(res);
    return res;
  }
}

class Point
{
  int x;
  int y;

  public int X() { return x; }
  public int Y() { return y; }

  public Point SetX( int _x )
  {
    x = _x;
    return this;
  }

  public Point SetY( int _y )
  {
    y = _y;
    return this;
  }
}

class Size
{
  int cx;
  int cy;

  public int CX() { return cx; }
  public int CY() { return cy; }

  public Size SetCX( int _cx )
  {
    cx = _cx;
    return this;
  }

  public Size SetCY( int _cy )
  {
    cy = _cy;
    return this;
  }
}

class Rect
{
  Point ul;
  Size  size;

  public Point UL() { return ul; }
  public Size  SZ() { return size; }

  public Rect SetUL( Point _ul )
  {
    ul = _ul;
    return this;
  }

  public Rect SetSZ( Size _sz )
  {
    size = _sz;
    return this;
  }

  public Point LR()
  {
    Point p;
    Point t;

    p = new Point();

    t = p.SetX( this.UL().X() + this.SZ().CX() );
    t = p.SetY( this.UL().Y() + this.SZ().CY() );

    return p;
  }

  public boolean Contains( Rect r )
  {
    boolean result;

    if( ( this.UL().X() < r.UL().X() ) &&
        ( this.UL().Y() < r.UL().Y() ) &&
        ( r.LR().X() < this.LR().X() ) &&
        ( r.LR().Y() < this.LR().Y() ) )
    {
      result = true;
    }
    else
    {
      result = false;
    }

    return result;
  }
}
