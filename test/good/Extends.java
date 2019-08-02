class Extends {
  public static void main(String[] args) {
    System.out.println(new A().init());
  }
}
class A {
  int a;
  public A get() {
    return new A();
  }
  public int init() {
    A a;
    A b;
    A c;
    int tmp;
    a = new A().get();
    b = new B().get();
    c = new C().get();
    tmp = a.print();
    tmp = b.print();
    tmp = c.print();
    return 0;
  }
  public int print() {
    System.out.println(65);
    return 65;
  }
}
class B extends A {
  int b;
  public B get() {
    return new B();
  }
  public int print() {
    System.out.println(66);
    return 66;
  }
}
class C extends B {
  public C get() {
    return new C();
  }
  public int print() {
    System.out.println(67);
    return 67;
  }
}
