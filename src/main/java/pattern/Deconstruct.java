package pattern;

import java.util.List;

public class Deconstruct implements Pattern {
    public Deconstruct(String name, List<Pattern> patternList) {
        this.name = name;
        this.patternList = patternList;
    }

    public String name;
    public List<Pattern> patternList;
}
