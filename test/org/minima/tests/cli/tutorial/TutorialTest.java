package org.minima.tests.cli.tutorial;

import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;

public class TutorialTest extends MinimaCliTest {

    @Test
    public void testTutorialWithNoArgs() throws Exception {
        String output = minimaTestNode.runCommand("tutorial");
        runBaseTests(output);
    }

    public void runBaseTests(String output) throws Exception {
        super.runBaseTests(output);
    }

}