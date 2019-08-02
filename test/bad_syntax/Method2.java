class Method2 {
    public static void main(String[] args) {
        System.out.println(new M());
    }
}

class M {
    public int m(int a, int b) {
        // Missing expression after 'return'
        return;
    }
}