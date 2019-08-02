class Hanoi {
    public static void main(String[] args) {
        System.out.println(new H().solve(10, 1, 2, 3));
    }
}

class H {
    public int solve(int n, int a, int b, int c) {
        int moves;
        if (0 < n) {
            moves = this.solve(n - 1, a, c, b);
            System.out.println(a);
            System.out.println(c);
            System.out.println(0);
            moves = moves + 1 + this.solve(n - 1, b, c, a);
        } else {
            moves = 0;
        }
        return moves;
    }
}