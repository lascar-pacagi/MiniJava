class Call2 {
    public static void main(String[] args) {
        System.out.println(new C().init());
    }
}

class C {
    public int init() {
        // Bad return type
        return new C().m1(4, 5, 6);
    }
    public boolean m1(int a, int b, int c) {
        return a + b < c;
    }
}
