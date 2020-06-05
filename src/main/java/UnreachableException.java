public class UnreachableException extends Exception {
    @Override
    public String getMessage() {
        return "unreachable";
    }
}
