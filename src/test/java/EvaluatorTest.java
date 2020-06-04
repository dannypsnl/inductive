import environment.Env;
import org.junit.Test;
import semanctic.SemanticException;
import term.Constructor;
import term.Var;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class EvaluatorTest {
    Evaluator evaluator = new Evaluator();

    @Test
    public void evalOnVariable() throws SemanticException {
        Env env = new Env();
        env.bind("x", new Constructor("Z"));
        var r = evaluator.eval(env, new Var("x"));
        if (r instanceof Constructor c) {
            assertEquals(
                    c.name,
                    "Z"
            );
        } else {
            fail("not a constructor");
        }
    }

    @Test
    public void evalOnConstructor() throws SemanticException {
        Env env = new Env();
        var r = evaluator.eval(env, new Constructor("S", new Constructor("Z")));
        if (r instanceof Constructor c) {
            assertEquals(
                    c.name,
                    "S"
            );
        } else {
            fail("not a constructor");
        }
    }
}
