class And {
    public static void main(String[] args) {
        System.out.println(new A().init());
    }
}

class A {
    public int init() {
        boolean b;
        // '&&' arguments must be of boolean type
        b = true && 1;
        return 0;
    }
}