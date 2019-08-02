class Burk {
    public static void main(String[] args) {
        System.out.println(new One().start());
    }
}

class One {
    int tag;
    int it;
    public int setTag() {
        tag = 1;
        return 0;
    }
    public int getTag() {
        return tag;
    }
    public int setIt(int i) {
        it = i;
        return 0;
    }
    public int getIt() {
        return it;
    }
    public int start() {
        int ignore;
        Two two;
        One one;
        two = new Two();
        one = two;
        ignore = one.setTag();
        System.out.println(one.getTag());
        ignore = one.setIt(17);
        ignore = two.setTag();
        System.out.println(two.getIt());
        System.out.println(two.getThat());
        ignore = two.resetIt();
        System.out.println(two.getIt());
        System.out.println(two.getThat());
        return 0;
    }
}

class Two extends One {
    int it;
    public int setTag() {
        tag = 2;
        it = 3;
        return 0;
    }
    public int getThat() {
        return it;
    }
    public int resetIt() {
        return this.setIt(42);
    }
}
