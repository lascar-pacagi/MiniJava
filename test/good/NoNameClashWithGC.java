class NoNameClashWithGC {
    public static void main(String[] gc) {
        System.out.println(new gc().gc(42));
    }
}

class gc {
    int gc;
    public int gc(int n) {
        int gc;
        int[] a;
        gc = n;
        a = new int[gc];
        return gc;
    }
}