package semanctic;

public class NoVariable extends SemanticException {
    private final String variableName;

    public NoVariable(String name) {
        variableName = name;
    }

    @Override
    public String getMessage() {
        return "no variable name: " + variableName;
    }
}
