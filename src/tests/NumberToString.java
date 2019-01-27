class NumberToString {

  public static void main(String[] args) {
    System.out.println(new A().go());
  }

}

class A {

  public int go() {
    char[] str;
    int i;

    str = A.number_to_string(120, 10);
    System.out.println(str.length);
    i = 0;
    while (i < str.length) {
      System.out.println(0+str[i]);
      i = i + 1;
    }
    return str.length;
  }

  public static char[] number_to_string(int val, int radix) {
    String digits_string;
    char[] digits;
    char[] result;
    int digits_count;
    int iter;
    int quotient;
    int remainder;
    int idx;
    boolean negative;
    int zero;

    zero = 0;
    digits_count = 0;
    digits_string = "0123456789abcdef";
    digits = digits_string.toCharArray();
    System.out.println(1);
    if (val < 0) {
      negative = true;
      val = zero-val;
      System.out.println(2);
    } else {
      negative = false;
      System.out.println(3);
    }  /* if */
    if (val == 0) {
      result = new char[1];
      result[0] = '0';
      System.out.println(3);
    } else {
      if (negative) {
        digits_count = 1;
        System.out.println(4);
      } else {
        digits_count = 0;
        System.out.println(5);
      }  /* if */
      iter = val;
      while (!(iter == 0)) {
        System.out.println(6);
        quotient = 1;
        while (quotient * radix < iter) { quotient = quotient + 1; }
        if (iter < quotient * radix) {
          quotient = quotient - 1;
          System.out.println(7);
        } else {
          System.out.println(8);
        }  /* if */
        iter = quotient;
        digits_count = digits_count + 1;
        System.out.println(9);
      }  /* while */
      result = new char[digits_count];
      System.out.println(10);
      if (negative) {
        result[0] = '-';
        System.out.println(11);
      } else {
        System.out.println(12);
      }  /* if */
      iter = val;
      idx = result.length - 1;
      while (!(iter == 0)) {
        System.out.println(13);
        quotient = 1;
        while (quotient * radix < iter) { quotient = quotient + 1; }
        if (iter < quotient * radix) {
          quotient = quotient - 1;
          System.out.println(14);
        } else {
          System.out.println(15);
        }  /* if */
        remainder = iter - quotient * radix;
        iter = quotient;
        result[idx] = digits[remainder];
        idx = idx - 1;
        System.out.println(16);
      }  /* while */
    }  /* if */
    return result;
  }  /* number_to_string */
}
