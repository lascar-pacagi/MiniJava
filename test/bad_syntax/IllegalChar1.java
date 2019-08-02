class IllegalChar1 {
    public static void main(String[] args) {
        // Illegal lexem '&'
        if (true & true) {
            System.out.println(1);
        } else {
            System.out.println(0);
        }
    }
}