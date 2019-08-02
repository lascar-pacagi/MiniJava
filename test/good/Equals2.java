class Equals2 {
    public static void main(String[] args) {
        System.out.println(new E().start());
    }
}

class E {
    int att1;
    public int init(int a) {
        att1 = a;
        return 0;
    }
    public int[] getAtts() {
        int[] res;
        res = new int[1];
        res[0] = att1;
        return res;
    }
    public boolean equals(E other) {
        boolean res;
        int[] this_atts;
        int[] other_atts;
        int i;
        this_atts = this.getAtts();
        other_atts = other.getAtts();
        i = 0;
        if (!(this_atts.length < other_atts.length) &&
            !(other_atts.length < this_atts.length)) {
            res = true;
            while (res && i < this_atts.length) {
                if (this_atts[i] < other_atts[i]) {
                    res = false;
                } else {
                    if (other_atts[i] < this_atts[i]) {
                        res = false;
                    } else {
                    }
                }
                i = i + 1;
            }
        } else {
            res = false;
        }
        return res;
    }
    public int start() {
        int ignore;
        E ea;
        E eb;
        E1 e1;
        E1 e2;
        e1 = new E1();
        ignore = e1.init1(42, 1975);
        ea = e1;
        e2 = new E1();
        ignore = e2.init1(42, 2020);
        eb = e1;
        if (ea.equals(eb)) {
            System.out.println(1);
        } else {
            System.out.println(0);
        }
        if (e1.equals(e2)) {
            System.out.println(1);
        } else {
            System.out.println(0);
        }
        if (ea.equals(e2)) {
            System.out.println(1);
        } else {
            System.out.println(0);
        }
        if (e1.equals(eb)) {
            System.out.println(1);
        } else {
            System.out.println(0);
        }
        return 0;
    }
}

class E1 extends E {
    int att2;
    public int init1(int a, int b) {
        att1 = a;
        att2 = b;
        return 0;
    }
    public int[] getAtts() {
        int[] res;
        res = new int[2];
        res[0] = att1;
        res[1] = att2;
        return res;
    }
}