class Ackermann {
    public static void main(String[] args) {
        System.out.println(new A().ackermann(3, 4));
    }
}

class A {
    public int ackermann(int m, int n) {
        int res;
        if (!(m < 0) && !(0 < m)) {
            res = n + 1;
        } else {
            if (!(n < 0) && !(0 < n)) {
                res = this.ackermann(m - 1, 1);
            } else {
                res = this.ackermann(m - 1, this.ackermann(m, n - 1));
            }
        }
        return res;
    }
}
