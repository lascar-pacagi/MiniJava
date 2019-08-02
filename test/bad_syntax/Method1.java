class Method1 {
    public static void main(String[] args) {
        System.out.println(new M());
    }
}

class M {
    // Missing parameter after comma
    public int m(int a, int b,) {
        return 42;
    }
}