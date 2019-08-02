class Call3 {
    public static void main(String[] args) {
        System.out.println(new C().init());
    }
}

class C {
    public int init() {
        // Bad parameter type
        return this.m2(new C().m1(1, 2, 3), 5);
    }
    public boolean m1(int a, int b, int c) {
        return a + b < c;
    }
    public int m2(int a, int b) {
        return a * b;
    }
}
