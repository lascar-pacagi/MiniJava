class Arith2 {
    public static void main(String[] args) {
        System.out.println(new A().init());
    }
}

class A {
    public int init() {
        int a;
        // Missing left parenthesis
        a = 1 + 2 * (3 - ((5));
                     return a;
    }
}