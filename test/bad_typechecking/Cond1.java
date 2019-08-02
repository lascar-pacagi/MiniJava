class Cond1 {
    public static void main(String[] args) {
        // Integer compared to boolean
        if (1 + 2 < (3 * 4 < 10)) {
            System.out.println(1);
        } else {
            System.out.println(0);
        }
    }
}
