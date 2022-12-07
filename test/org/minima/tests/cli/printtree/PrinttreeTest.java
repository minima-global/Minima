package org.minima.tests.cli.printtree;

import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;

public class PrinttreeTest extends MinimaCliTest {

    @Test
    public void testPrinttreeWithNoArgs() throws Exception {
        String output = minimaTestNode.runCommand("printtree");
        runBaseTests(output);
    }

    public void runBaseTests(String output) throws Exception {
        //The cmd response should be valid JSON
        super.runBaseTests(output);
    }

}