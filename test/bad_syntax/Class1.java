class Class1 {
    public static void main(String[] args) {
        System.out.println(new C());
    }
}

// Multiple inheritance is forbidden
class C extends A extends B {
}