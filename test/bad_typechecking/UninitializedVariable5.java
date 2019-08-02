class UninitializedVariable5 {
    public static void main(String[] args) {
        System.out.println(new U().run(42));
    }
}

class U {
    public int run(int n) {
        int a;
        int b;
        b = 0;
        if (n < 0) {
            a = 1;
        } else {
            b = 1;
        }
        return a;
    }
}