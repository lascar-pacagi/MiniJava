class Attribute1 {
    public static void main(String[] args) {
        System.out.println(new A().init());
    }
}

class C {
    int c;

    public int init() {
        boolean c;
        // Attribute is of type int but local is of type boolean
        return c + 5;
    }
}
