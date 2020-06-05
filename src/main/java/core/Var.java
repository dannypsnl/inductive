package core;

public class Var implements Term, Value {
    public Var(String name) {
        this.name = name;
    }

    public String name;
}
