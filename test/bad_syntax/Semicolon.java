class Semicolon {
    public static void main(String[] args) {
        System.out.println(new S());
    }
}

class S {
    public int semicolon() {
        int a;
        int b;
        // Missing semicolon
        a = a + 1
            b = a;
        return b;
    }
}