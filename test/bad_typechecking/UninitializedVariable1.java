class UninitializedVariable1 {
    public static void main(String[] args) {
        System.out.println(new U().run());
    }
}

class U {
    public int run() {
        int a;
        return a;
    }
}