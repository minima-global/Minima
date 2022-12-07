package org.minima.tests.cli.incentivecash;

import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;

public class IncentivecashTest extends MinimaCliTest {

    @Test
    public void testIncentivecashWithNoArgs() throws Exception {
        String output = minimaTestNode.runCommand("incentivecash");
        runBaseTests(output);
    }

    public void runBaseTests(String output) throws Exception {
        super.runBaseTests(output);
    }

}