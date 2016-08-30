class A extends Object {
    Object x;
    Object y;
    A(Object x, Object y) {
        super();
        this.x = x;
        this.y = y;
    }
    Object getX() {
        return this.x;
    }
    Object getY() {
        return this.y;
    }
    A setX(Object newx) {
        return new A(newx, this.y);
    }
    A setY(Object newy) {
        return new A(this.x, newy);
    }
}

class B extends A {
    B(Object x, Object y) {
        super(x);
    }
}
