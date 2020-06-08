package core;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

public class Constructor implements Term, Value {
    public String name;
    public List<Term> constructors;

    public Constructor(String name, Term... constructors) {
        this.name = name;
        this.constructors = Arrays.asList(constructors);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof Constructor that)) {
            return false;
        } else if (!Objects.equals(name, that.name)) {
            return false;
        } else {
            return true;
        }
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, constructors);
    }
}
