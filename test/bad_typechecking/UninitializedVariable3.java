class UninitializedVariable3 {
    public static void main(String[] args) {
        System.out.println(new U().run());
    }
}

class U {
    public int run() {
        boolean a;
        int res;
        if (a) res = 1;
        else res = 0;
        return res;
    }
}