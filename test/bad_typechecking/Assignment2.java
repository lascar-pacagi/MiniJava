class Assignment2 {
    public static void main(String[] args) {
        System.out.println(new A().init());
    }
}

class A {
    public int init() {
        B b;
        A a;
        // A is not a subclass of B
        b = a;
        return 0;
    }
}

class B extends A {
}
