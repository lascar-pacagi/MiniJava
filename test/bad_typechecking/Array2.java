class Array2 {
    public static void main(String[] args) {
        System.out.println(new A().init());
    }
}

class A {
    public int init() {
        int[] a;
        a = new int[42];
        // Index of array must be an integer
        return a[new A()];
    }
}