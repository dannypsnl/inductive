import core.*;
import datastructure.Pair;
import environment.Env;
import pattern.Deconstruct;
import pattern.Name;
import pattern.Pattern;
import semanctic.SemanticException;

public class Evaluator {
    public Value eval(Env env, Term term) throws SemanticException, UnreachableException {
        if (term instanceof PatternMatching p) {
            for (Pair<Pattern, Term> pair : p.patternToResult) {
                var pattern = pair.first;
                var result = pair.tail;
                if (match(env, pattern, p.target)) {
                    return eval(env, result);
                }
            }
        } else if (term instanceof Constructor c) {
            return c;
        } else if (term instanceof Var v) {
            return env.lookup(v.name);
        }
        throw new UnreachableException();
    }

    boolean match(Env env, Pattern pattern, Term target) throws SemanticException, UnreachableException {
        if (pattern instanceof Deconstruct d &&
                target instanceof Constructor c) {
            if ((c.name.equals(d.name))
                    && (c.constructors.size() == d.patternList.size())) {
                for (var i = 0; i < d.patternList.size(); i++) {
                    // if any sub-pattern mismatched, return false
                    if (!match(env, d.patternList.get(i), c.constructors.get(i))) {
                        return false;
                    }
                }
                // all sub-pattern works
                return true;
            }
        } else if (pattern instanceof Name n) {
            env.bind(n.name, eval(env, target));
            return true;
        }
        return false;
    }
}
