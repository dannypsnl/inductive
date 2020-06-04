package pattern;

import java.util.Arrays;
import java.util.List;

public class Deconstruct implements Pattern {
    public Deconstruct(String name, List<Pattern> patternList) {
        this.name = name;
        this.patternList = patternList;
    }

    public String name;
    public List<Pattern> patternList;

    public Deconstruct(String name, Pattern... patterns) {
        this.name = name;
        patternList = Arrays.asList(patterns);
    }
}
