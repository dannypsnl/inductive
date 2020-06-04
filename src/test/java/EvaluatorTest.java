import environment.Env;
import org.junit.Assert;
import org.junit.Test;
import semanctic.SemanticException;
import term.Constructor;

public class EvaluatorTest {
    Evaluator evaluator = new Evaluator();

    @Test
    public void evalConstructor() {
        Env env = new Env();
        try {
            var r = evaluator.eval(env, new Constructor("S", new Constructor("Z")));
            Assert.assertEquals(
                    r,
                    new Constructor("S", new Constructor("Z"))
            );
        } catch (SemanticException e) {
            e.printStackTrace();
        }
    }
}
