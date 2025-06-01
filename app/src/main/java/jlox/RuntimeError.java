package jlox;

/**
 * Custom error for runtime errors in the interpreter
 */
class RuntimeError extends RuntimeException {
    final Token token;

    RuntimeError(Token token, String message) {
        super(message);
        this.token = token;
    }
}
