class Overriding2 {
    public static void main(String[] args) {
        System.out.println(new O().init());
    }
}

class O {
    public int init() {
        return 0;
    }
    public O1 m1(O o) {
        return 0;
    }
}

class O1 extends O {
    // Return type must be compatible
    public O m1(O o) {
        return true;
    }
}