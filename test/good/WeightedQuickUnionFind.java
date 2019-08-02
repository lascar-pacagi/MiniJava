class WeightedQuickUnionFind {
    public static void main(String[] args) {
        System.out.println(new Main().start());
    }
}

class Main {
    public int start() {
        int ignore;
        UF uf;
        uf = new UF();
        ignore = uf.init(1000);
        System.out.println(uf.union(1, 2));
        System.out.println(uf.union(3, 4));
        if (uf.connected(1, 2)) {
            System.out.println(1);
        } else {
            System.out.println(0);
        }
        System.out.println(uf.union(5, 9));
        System.out.println(uf.union(9, 1));
        if (uf.connected(9, 4)) {
            System.out.println(1);
        } else {
            System.out.println(0);
        }
        System.out.println(uf.count());
        System.out.println(uf.union(3, 4));
        if (uf.connected(100, 200)) {
            System.out.println(1);
        } else {
            System.out.println(0);
        }
        System.out.println(uf.union(200, 4));
        System.out.println(uf.count());
        System.out.println(uf.union(200, 1));
        if (uf.connected(4, 300)) {
            System.out.println(1);
        } else {
            System.out.println(0);
        }
        return 0;
    }
}

class UF {
    int[] id;
    int[] sz;
    int count;

    public int init(int N)
    {
        int i;
        count = N;
        id = new int[N];
        sz = new int[N];
        i = 0;
        while (i < N) {
            id[i] = i;
            sz[i] = 1;
            i = i + 1;
        }
        return 0;
    }
    public int count() {
        return count;
    }
    public boolean connected(int p, int q) {
        int fp;
        int fq;
        fp = this.find(p);
        fq = this.find(q);
        return !(fp < fq) && !(fq < fp);
    }
    public int find(int p) {
        while (!(!(p < id[p]) && !(id[p] < p))) {
            p = id[p];
        }
        return p;
    }
    public int union(int p, int q) {
        int res;
        int i;
        int j;
        i = this.find(p);
        j = this.find(q);
        if (!(i < j) && !(j < i)) {
            res = 0;
        } else {
            if (sz[i] < sz[j]) {
                id[i] = j;
                sz[j] = sz[j] + sz[i];
            } else {
                id[j] = i;
                sz[i] = sz[i] + sz[j];
            }
            count = count - 1;
            res = 1;
        }
        return res;
    }
}