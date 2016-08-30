class A extends Object {
    Object x;
    A(Object x) {
        super();
        this.x = x;
    }
    Object getX() {
        return this.x;
    }
    A setX(Object newx) {
        return new A(newx);
    }
}

class B extends A {
    B(Object x) {
        super(x);
    }
    Object setX(Object newx) {
        return newx;
    }
}
