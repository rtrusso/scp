class GCTest4 {
    public static void main( String[] args ) {
        System.out.println( new Test().Go() );
    }
}

class Test
{
    public int initArray(int[] array) {
        int l;
        int i;
        l = array.length;
        i = l-1;
        while (!(i < 0))
        {
            array[i] = l-i;
            i = i-1;
        }
        return i;
    }

    public int validateArray(int[] array) {
        int l;
        int i;
        int r;
        r = 0;
        l = array.length;
        i = l-1;
        while (!(i < 0)) {
            if (!(array[i] == l-i)) {
                r = 133133133;
            } else {
            }

            i = i - 1;
        }

        return r;
    }

    public int Go() {
        int[] a;
        int[] prev_a;
        int index;
        int u;

        prev_a = new int[1];
        a = new int[1];
        u = this.initArray(prev_a);
        index = 0;
        while (index < 50000*16)
        {
            a = new int[2];
            System.out.println( a.length );
            u = this.initArray(a);
            u = this.validateArray(prev_a);
            System.out.println(u);

            prev_a = a;
            a = new int[1];
            System.out.println( a.length );

            index = index + 1;
        }

        return a.length;
    }
}
