class Overloading1 {
    public static void main(String[] args) {
        System.out.println(new O().init());
    }
}

class O {
    public int init() {
        return 0;
    }
    public int m1(int a) {
        return 0;
    }
    // In MiniJava overloading is not allowed
    public int m1() {
        return 0;
    }
}