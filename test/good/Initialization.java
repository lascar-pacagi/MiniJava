class Initialization {
    public static void main(String[] args) {
        System.out.println(new I().run(42));
    }
}

class I {
    public int run(int n) {
        int res;
        if (n < 0) {
            res = 1;
        } else {
            res = 0;
        }
        return res;
    }
}