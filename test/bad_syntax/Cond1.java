class Cond1 {
    public static void main(String[] args) {
        // Should write: 1 < 2 && 2 < 3
        if (1 < 2 < 3) {
            System.out.println(1);
        } else {
            System.out.println(0);
        }
    }
}
