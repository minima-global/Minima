package org.minima.tests.cli.network;

import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;

public class NetworkTest extends MinimaCliTest {

    @Test
    public void testNetworkWithNoArgs() throws Exception {
        String output = minimaTestNode.runCommand("network");
        runBaseTests(output);
    }

    public void runBaseTests(String output) throws Exception {
        super.runBaseTests(output);
    }

}