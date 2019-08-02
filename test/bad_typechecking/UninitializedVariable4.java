class UninitializedVariable4 {
    public static void main(String[] args) {
        System.out.println(new U().run());
    }
}

class U {
    int a;
    public int run() {
        int a;
        return a;
    }
}