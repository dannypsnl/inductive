package datastructure;

public class Pair<F, T> {
    Pair(F f, T t) {
        first = f;
        tail = t;
    }

    public F first;
    public T tail;
}
