class Fibonacci {
    public static void main(String[] args) {
        System.out.println(new Fibo().fibo(10));
    }
}

class Fibo {
    public int fibo(int n) {
        int res;
        if (n < 2) {
          res = n;
        } else {
            res = this.fibo(n - 2) + this.fibo(n - 1);
        }
        return res;
    }
}