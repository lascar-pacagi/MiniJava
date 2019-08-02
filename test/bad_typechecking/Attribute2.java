class Attribute2 {
    public static void main(String[] args) {
        System.out.println(new A().init());
    }
}

class A {
    // Attributes must have different names
    int a;
    A a;

    public int init() {
        return 0;
    }
}
