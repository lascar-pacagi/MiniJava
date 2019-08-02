class Array1 {
    public static void main(String[] args) {
        System.out.println(new A());
    }
}

class A {
    public int[] m() {
        int[] a;
        // Size is missing in array creation
        a = new int[];
        return a;
    }
}
