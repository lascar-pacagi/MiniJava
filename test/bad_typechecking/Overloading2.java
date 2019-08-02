class Overloading1 {
    public static void main(String[] args) {
        System.out.println(new O().init());
    }
}

class O {
    public int init() {
        return 0;
    }
    public int m1(O o) {
        return 0;
    }
}

class O1 extends O {
    // In MiniJava overloading is not allowed
    public int m1(O1 o1) {
        return 0;
    }
}