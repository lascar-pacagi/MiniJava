class Call1 {
    public static void main(String[] args) {
        System.out.println(new C());
    }
}

class C {
    public int factorial(int n) {
        int res;
        if (n < 1) res = 1;
        // 'this.' is missing in the call to factorial
        else res = n * factorial(n - 1);
        return res;
    }
}