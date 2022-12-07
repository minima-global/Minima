package org.minima.tests.cli.getaddress;

import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;

public class GetaddressTest extends MinimaCliTest {

    @Test
    public void testGetaddressWithNoArgs() throws Exception {
        String output = minimaTestNode.runCommand("getaddress");
        runBaseTests(output);
    }

    public void runBaseTests(String output) throws Exception {
        //The cmd response should be valid JSON
        super.runBaseTests(output);
    }

}