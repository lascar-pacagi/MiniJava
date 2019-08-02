class DynamicBinding1 {
    public static void main(String[] args) {
        System.out.println(new D().start());
    }
}

class D {
    public int m1() {
        int ignore;
        System.out.println(1);
        ignore = this.m2();
        return 0;
    }
    public int m2() {
        System.out.println(2);
        return 0;
    }
    public int start() {
        D d;
        D d1;
        D d2;
        D d3;
        int ignore;
        d = new D();
        d1 = new D1();
        d2 = new D2();
        d3 = new D3();
        ignore = d.m1();
        ignore = d1.m1();
        ignore = d2.m1();
        ignore = d3.m1();
        return 0;
    }
}

class D1 extends D {
    public int m1() {
        int ignore;
        System.out.println(11);
        ignore = this.m2();
        return 0;
    }
}

class D2 extends D1 {
    public int m1() {
        int ignore;
        System.out.println(21);
        ignore = this.m2();
        return 0;
    }
    public int m2() {
        System.out.println(22);
        return 0;
    }
}

class D3 extends D2 {
    public int m1() {
        int ignore;
        System.out.println(31);
        ignore = this.m2();
        return 0;
    }
    public int m2() {
        System.out.println(32);
        return 0;
    }
}