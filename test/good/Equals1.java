class Equals1 {
    public static void main(String[] args) {
        System.out.println(new E().start());
    }
}

class E {
    int att1;
    public int init(int a) {
        att1 = a;
        return 0;
    }
    public int getAtt1() {
        return att1;
    }
    public boolean equals(E e) {
        return !(att1 < e.getAtt1()) && !(e.getAtt1() < att1);
    }
    public int start() {
        int ignore;
        E ea;
        E eb;
        E1 e1;
        E1 e2;
        e1 = new E1();
        ignore = e1.init1(42, 1975);
        ea = e1;
        e2 = new E1();
        ignore = e2.init1(42, 2020);
        eb = e1;
        if (ea.equals(eb)) {
            System.out.println(1);
        } else {
            System.out.println(0);
        }
        if (e1.equals(e2)) {
            System.out.println(1);
        } else {
            System.out.println(0);
        }
        if (e1.equals1(e2)) {
            System.out.println(1);
        } else {
            System.out.println(0);
        }
        return 0;
    }
}

class E1 extends E {
    int att2;
    public int init1(int a, int b) {
        att1 = a;
        att2 = b;
        return 0;
    }
    public int getAtt2() {
        return att2;
    }
    public boolean equals1(E1 e1) {
        return !(att1 < e1.getAtt1()) && !(e1.getAtt1() < att1) &&
               !(att2 < e1.getAtt2()) && !(e1.getAtt2() < att2);
    }
}