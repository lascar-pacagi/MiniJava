class Method6 {
    public static void main(String[] args) {
        System.out.println(new M());
    }
}

class M {
    public int m(int a) {
        // The following affectation should appear after the declaration of 'res'
        a = 42;
        int res;
        res = 0;
        return res;
    }
}