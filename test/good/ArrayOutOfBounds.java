class ArrayOutOfBounds {
    public static void main(String[] args) {
        System.out.println(new Out().run());
    }
}

class Out {
    public int run() {
        int[] a;
        int i;
        a = new int[512];
        i = 0;
        while (i < a.length) {
            a[i] = i;
            i = i + 1;
        }
        System.out.println(a[0]);
        System.out.println(a[511]);
        System.out.println(a[512]);
        System.out.println(a[1]);
        System.out.println(a[2]);
        System.out.println(a[3]);
        return 1;
    }
}