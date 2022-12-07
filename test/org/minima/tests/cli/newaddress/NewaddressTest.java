package org.minima.tests.cli.newaddress;

import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;

public class NewaddressTest extends MinimaCliTest {

    @Test
    public void testNewaddressWithNoArgs() throws Exception {
        String output = minimaTestNode.runCommand("newaddress");
        runBaseTests(output);
    }

    public void runBaseTests(String output) throws Exception {
        super.runBaseTests(output);
    }

}