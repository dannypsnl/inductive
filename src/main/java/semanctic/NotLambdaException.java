package semanctic;

public class NotLambdaException extends SemanticException {
    @Override
    public String getMessage() {
        return "not a lambda";
    }
}
