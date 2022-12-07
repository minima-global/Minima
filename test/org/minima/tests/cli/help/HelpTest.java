package org.minima.tests.cli.help;

import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;
import org.minima.tests.cli.MinimaTestNode;

public class HelpTest extends MinimaCliTest {

    public MinimaTestNode test = new MinimaTestNode();

    @Test
    public void testHelpWithNoArgs() throws Exception {
        String output = minimaTestNode.runCommand("help");
        runBaseTests(output);
    }

    public void runBaseTests(String output) throws Exception {
        super.runBaseTests(output);
    }

}