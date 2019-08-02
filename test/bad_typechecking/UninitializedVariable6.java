class UninitializedVariable6 {
    public static void main(String[] args) {
        System.out.println(new U().run());
    }
}

class U {
    public int run() {
        int a;
        return this.m(42 + a * 2);
    }
    public int m(int n) {
        return n;
    }
}