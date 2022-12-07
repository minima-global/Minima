package org.minima.tests.cli.mds;

import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;

public class MdsTest extends MinimaCliTest {

    @Test
    public void testMdsWithNoArgs() throws Exception {
        String output = minimaTestNode.runCommand("mds");
        runBaseTests(output);
    }

    public void runBaseTests(String output) throws Exception {
        //The cmd response should be valid JSON
        super.runBaseTests(output);
    }

}