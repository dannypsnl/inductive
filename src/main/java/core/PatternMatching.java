package core;

import datastructure.Pair;
import pattern.Pattern;

import java.util.Arrays;
import java.util.List;

public class PatternMatching implements Term {
    @SafeVarargs
    public PatternMatching(Term t, Pair<Pattern, Term>... p) {
        target = t;
        patternToResult = Arrays.asList(p);
    }

    public Term target;
    public List<Pair<Pattern, Term>> patternToResult;
}
