class Not {
    public static void main(String[] args) {
        System.out.println(new N().init());
    }
}

class N {
    public int init() {
        boolean b;
        b = !new N1().f();
        return 0;
    }
}

class N1 {
    public int f() {
        return 0;
    }
}
