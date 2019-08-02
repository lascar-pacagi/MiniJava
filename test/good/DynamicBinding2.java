class DynamicBinding2 {
    public static void main(String[] args) {
        System.out.println(new D().start());
    }
}

class D {
    int z;
    public int m1() {
        int ignore;
        z = 1;
        System.out.println(1);
        ignore = this.m2();
        return 0;
    }
    public int m2() {
        System.out.println(2);
        System.out.println(z);
        return 0;
    }
    public int start() {
        D d;
        D d1;
        D d2;
        D d3;
        D3 d4;
        int ignore;
        d = new D();
        d1 = new D1();
        d2 = new D2();
        d3 = new D3();
        d4 = new D4();
        ignore = d.m1();
        ignore = d1.m1();
        ignore = d2.m1();
        ignore = d3.m1();
        ignore = d4.m1();
        return 0;
    }
}

class D1 extends D {
    int b;
    public int m1() {
        int ignore;
        b = 2;
        System.out.println(11);
        ignore = this.m2();
        return 0;
    }
}

class D2 extends D1 {
    int a;
    public int m1() {
        int ignore;
        a = 3;
        System.out.println(21);
        ignore = this.m2();
        return 0;
    }
    public int m2() {
        System.out.println(22);
        System.out.println(a);
        return 0;
    }
}

class D3 extends D2 {
    int z;
    public int m1() {
        int ignore;
        z = 4;
        System.out.println(31);
        ignore = this.m2();
        return 0;
    }
    public int m2() {
        System.out.println(32);
        System.out.println(z);
        return 0;
    }
}

class D4 extends D3 {
    int a;
    public int m1() {
        int ignore;
        a = 5;
        System.out.println(31);
        ignore = this.m2();
        return 0;
    }
    public int m2() {
        System.out.println(32);
        System.out.println(a);
        System.out.println(z);
        System.out.println(b);
        return 0;
    }
}