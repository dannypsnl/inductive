package semanctic;

public class NonExhaustException extends SemanticException {
    @Override
    public String getMessage() {
        return "non-exhaust";
    }
}
