package core;

import java.util.Arrays;
import java.util.List;
import java.util.Objects;

public class Inductive implements Term, Value {
    public String name;
    public List<Constructor> constructors;

    public Inductive(String name, Constructor... constructors) {
        this.name = name;
        this.constructors = Arrays.asList(constructors);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (!(o instanceof Inductive inductive)) {
            return false;
        } else {
            // promise inductive name is unique is the premise of this check
            return Objects.equals(name, inductive.name);
        }
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, constructors);
    }
}
