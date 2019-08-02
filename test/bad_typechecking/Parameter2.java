class Parameter2 {
    public static void main(String[] args) {
        System.out.println(new P().init(1, 2));
    }
}

class P {
    // Parameters must have different names
    public int init(int a, boolean a) {
        return a;
    }
}
