package term;

import datastructure.Pair;
import pattern.Pattern;

import java.util.List;

public class PatternMatching implements Term {
    PatternMatching(Term t, List<Pair<Pattern, Term>> p) {
        target = t;
        patternToResult = p;
    }

    public Term target;
    public List<Pair<Pattern, Term>> patternToResult;
}
