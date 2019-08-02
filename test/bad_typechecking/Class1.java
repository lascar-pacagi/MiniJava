class Class1 {
    public static void main(String[] args) {
        System.out.println(new C().init());
    }
}

// Classes must have different names
class C {
    public int init() {
        return 0;
    }
}

class C {
    public int init() {
        return 1;
    }
}
