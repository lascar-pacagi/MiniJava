class Change {
    public static void main(String[] args) {
        System.out.println(new C().solve(28965));
    }
}

class C {
    int[] nbChange;
    int[] coins;

    public int init(int v) {
        int i;
        coins = new int[8];
        coins[0] = 1;
        coins[1] = 2;
        coins[2] = 5;
        coins[3] = 7;
        coins[4] = 10;
        coins[5] = 25;
        coins[6] = 50;
        coins[7] = 100;
        nbChange = new int[v + 1];
        i = 0;
        while (i < v + 1) {
            nbChange[i] = 10000000;
            i = i + 1;
        }
        return 0;
    }

    public int solve(int value) {
        int v;
        int i;
        int ignore;
        ignore = this.init(value);
        nbChange[0] = 0;
        v = 1;
        while (!(value < v)) {
            i = 0;
            while (i < coins.length) {
                if (!(v - coins[i] < 0)) {
                    if (nbChange[v - coins[i]] + 1 < nbChange[v]) {
                        nbChange[v] = nbChange[v - coins[i]] + 1;
                    } else {
                    }
                } else {
                }
                i = i + 1;
            }
            v = v + 1;
        }
        return nbChange[value];
    }
}