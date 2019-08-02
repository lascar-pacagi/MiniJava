class Call1 {
    public static void main(String[] args) {
        System.out.println(new C().init());
    }
}

class C {
    public int init() {
        // Wrong number of arguments
        return new C().m1(4, 5);
    }
    public int m1(int a, int b, int c) {
        return a + b + c;
    }
}
