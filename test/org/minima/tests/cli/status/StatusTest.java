package org.minima.tests.cli.status;

import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;

public class StatusTest extends MinimaCliTest {

    @Test
    public void testStatusWithNoArgs() throws Exception {
        String output = minimaTestNode.runCommand("status");
        runBaseTests(output);
    }

    public void runBaseTests(String output) throws Exception {
        //The cmd response should be valid JSON
        super.runBaseTests(output);
    }

}