class Assignment3 {
    public static void main(String[] args) {
        System.out.println(new A().init());
    }
}

class A {
    public int init() {
        B b;
        // A is not a subclass of B
        b = new A();
        return 0;
    }
}

class B extends A {
}
