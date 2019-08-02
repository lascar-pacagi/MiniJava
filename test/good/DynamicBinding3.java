class DynamicBinding3 {
    public static void main(String[] args) {
        System.out.println(new Main().start());
    }
}

class Main {
    public int start() {
        A a;
        A b;
        A c;
        boolean tmp1;
        int tmp2;
        a = new A();
        tmp1 = a.init();
        b = new B();
        tmp1 = b.init();
        c = new C();
        tmp1 = c.init();
        tmp2 = a.draw();
        tmp2 = b.draw();
        tmp2 = c.draw();
        return 0;
    }
}

class A {
    int a;

    public boolean init() {
        a = 65;
        return true;
    }

    public int draw() {
        System.out.println(a);
        return a;
    }
}

class B extends A {
    int a;

    public boolean init() {
        a = 66;
        return true;
    }

    public int draw() {
        System.out.println(a);
        return a;
    }
}

class C extends B {
    int a;

    public boolean init() {
        a = 67;
        return true;
    }

    public int draw() {
        System.out.println(a);
        return a;
    }
}
