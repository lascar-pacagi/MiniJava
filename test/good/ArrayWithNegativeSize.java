class ArrayWithNegativeSize {
    public static void main(String[] args) {
        System.out.println(new A().run());
    }
}

class A {
    public int run() {
        int[] a;
        int size;
        System.out.println(1);
        System.out.println(2);
        size = 10 - 12345;
        a = new int[size];
        System.out.println(3);
        System.out.println(4);
        return 0;
    }
}