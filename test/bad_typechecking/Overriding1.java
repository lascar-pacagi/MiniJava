class Overriding1 {
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
    // Return type must be compatible
    public boolean m1(O o) {
        return true;
    }
}