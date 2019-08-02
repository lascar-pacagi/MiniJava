class Block {
    public static void main(String[] args) {
        System.out.println(new B());
    }
}

class B {
    // We cannot declare variable inside a block
    public boolean m() {
        {
            int a;
            a = 0;
        }
        return 0;
    }
}