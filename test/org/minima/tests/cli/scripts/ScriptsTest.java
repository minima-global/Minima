package org.minima.tests.cli.scripts;

import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;

public class ScriptsTest extends MinimaCliTest {

    @Test
    public void testScriptsWithNoArgs() throws Exception {
        String output = minimaTestNode.runCommand("scripts");
        runBaseTests(output);
    }

    public void runBaseTests(String output) throws Exception {
        //The cmd response should be valid JSON
        super.runBaseTests(output);
    }

}