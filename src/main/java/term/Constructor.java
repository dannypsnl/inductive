package term;

import java.util.Arrays;
import java.util.List;

public class Constructor implements Term {
    public Constructor(String name, Term... constructors) {
        this.name = name;
        this.constructors = Arrays.asList(constructors);
    }

    public String name;
    public List<Term> constructors;
}
