import datastructure.Pair;
import environment.Env;
import semanctic.RedefinedException;
import pattern.Deconstruct;
import pattern.Name;
import pattern.Pattern;
import semanctic.SemanticException;
import term.Constructor;
import term.PatternMatching;
import term.Term;
import term.Var;

public class Evaluator {
    public Term eval(Env env, Term term) throws SemanticException {
        if (term instanceof PatternMatching p) {
            for (Pair<Pattern, Term> pair : p.patternToResult) {
                var pattern = pair.first;
                var result = pair.tail;
                if (match(env, pattern, p.target)) {
                    return eval(env, result);
                }
            }
        } else if (term instanceof Constructor) {
            return term;
        } else if (term instanceof Var v) {
            return env.lookup(v.name);
        }
        // do nothing for now
        return term;
    }

    boolean match(Env env, Pattern pattern, Term target) throws RedefinedException {
        if (pattern instanceof Deconstruct d &&
                target instanceof Constructor c) {
            return c.name.equals(d.name);
        } else if (pattern instanceof Name n) {
            env.bind(n.name, target);
        }
        return false;
    }
}
