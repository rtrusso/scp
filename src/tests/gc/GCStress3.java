class GCStress3 {
    public static void main(String[] args) {
        System.out.println(new Test().Go());
    }
}

class Node {
    Node next;
    int i;
    Node alwaysNull;
    public Node(Node n) {
        next = n;
        return;
    }
}

class Test {
    Node my_null;

    public int Go() {
        int i;
        int j;
        i = 0;
        while (i < 10000) {
            j = this.buildChain(1000);
            i = i + 1;
        }
        return i;
    }

    public int buildChain(int n) {
        Node current;
        current = my_null;
        while (!(n < 0)) {
            current = new Node(current);
            n = n -1;
        }
        return n;
    }
}
