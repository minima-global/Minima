package org.minima.tests.cli.tokens;

import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;

public class TokensTest extends MinimaCliTest {

    @Test
    public void testTokensWithNoArgs() throws Exception {
        String output = minimaTestNode.runCommand("tokens");
        runBaseTests(output);
    }

    public void runBaseTests(String output) throws Exception {
        super.runBaseTests(output);
    }

}