import core.*;
import datastructure.Pair;
import environment.Env;
import pattern.Deconstruct;
import pattern.Name;
import pattern.Pattern;
import semanctic.*;

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
            throw new NonExhaustException();
        } else if (term instanceof Constructor c) {
            return c;
        } else if (term instanceof Var v) {
            return env.lookup(v.name);
        } else if (term instanceof Application app) {
            if (app.function instanceof Lambda f) {
                if (f.parameters.size() != app.arguments.size()) {
                    throw new ArityException();
                }
                for (var i = 0; i < f.parameters.size(); i++) {
                    var param = f.parameters.get(i);
                    var arg = app.arguments.get(i);
                    check(env, arg, param.type);
                    env.bind(param.name, eval(env, arg));
                }
                return eval(env, f.body);
            } else {
                throw new NotLambdaException();
            }
        } else if (term instanceof Inductive inductive) {
            return inductive;
        } else {
            throw new UnreachableException();
        }
    }

    boolean match(Env env, Pattern pattern, Term target) throws SemanticException, UnreachableException {
        if (pattern instanceof Deconstruct d &&
                eval(env, target) instanceof Constructor c) {
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

    void check(Env env, Term term, Term type) throws SemanticException, UnreachableException {
        var tm = eval(env, term);
        var typ = eval(env, type);
        if (typ instanceof Inductive inductive) {
            for (Constructor c : inductive.constructors) {
                if (c.equals(tm)) {
                    return;
                }
            }
        }
        throw new TermIsNotValueOfType();
    }
}
