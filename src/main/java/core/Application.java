package core;

import java.util.Arrays;
import java.util.List;

public class Application implements Term {
    public Term function;
    public List<Term> arguments;

    public Application(Term function, Term... arguments) {
        this.function = function;
        this.arguments = Arrays.asList(arguments);
    }
}
