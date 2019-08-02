class MemoryBomb {
    public static void main(String[] args) {
        System.out.println(new Bomb().explode(1000000));
    }
}

class Bomb {
    int[] a;
    public int init(int n) {
        a = new int[n];
        return 0;
    }
    public int explode(int n) {
        int i;
        int size;
        int ignore;
        Bomb b;
        size = 100;
        i = 0;
        while (i < n) {
            b = new Bomb();
            ignore = b.init(size);
            i = i + 1;
        }
        return n * size;
    }
}