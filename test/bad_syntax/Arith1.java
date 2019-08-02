class Arith1 {
    public static void main(String[] args) {
        System.out.println(new A().init());
    }
}

class A {
    public int init() {
        int a;
        // Missing an expression after the '*'
        a = 1 + 2 *;
        return a;
    }
}