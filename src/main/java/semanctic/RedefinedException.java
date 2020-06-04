package semanctic;

public class RedefinedException extends SemanticException {
    public RedefinedException(String name) {
        variableName = name;
    }

    @Override
    public String getMessage() {
        return "redefined variable: " + variableName;
    }

    private final String variableName;
}
