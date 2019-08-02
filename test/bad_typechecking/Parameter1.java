class Parameter1 {
    public static void main(String[] args) {
        System.out.println(new P().init(1, 2));
    }
}

class P {
    public int init(int a, int b) {
        // Parameters and locals must have different names
        int a;
        return a + b;
    }
}
