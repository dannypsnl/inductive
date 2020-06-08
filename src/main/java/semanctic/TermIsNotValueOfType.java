package semanctic;

public class TermIsNotValueOfType extends SemanticException {
    @Override
    public String getMessage() {
        return "term is not value of type";
    }
}
