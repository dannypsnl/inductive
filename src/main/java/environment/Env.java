package environment;

import semanctic.NoVariable;
import semanctic.RedefinedException;
import term.Term;

import java.util.HashMap;
import java.util.Optional;

public class Env {
    private final Optional<Env> parent;
    private final HashMap<String, Term> nameToTerm;

    public Term lookup(String name) throws NoVariable {
        if (nameToTerm.containsKey(name)) {
            return nameToTerm.get(name);
        } else if (parent.isPresent()) {
            return parent.get().lookup(name);
        } else {
            throw new NoVariable(name);
        }
    }

    public void bind(String name, Term t) throws RedefinedException {
        if (nameToTerm.containsKey(name)) {
            throw new RedefinedException(name);
        } else {
            nameToTerm.put(name, t);
        }
    }

    public Env() {
        this(Optional.empty());
    }

    public Env(Optional<Env> p) {
        parent = p;
        nameToTerm = new HashMap<>();
    }
}

