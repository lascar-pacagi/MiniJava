class Parameter3 {
    public static void main(String[] args) {
        System.out.println(new P().init(1, 2));
    }
}

class P {
    public int init(int a, boolean b) {
        // 'c' is not defined
        return c;
    }
}
