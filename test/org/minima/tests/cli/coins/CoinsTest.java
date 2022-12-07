package org.minima.tests.cli.coins;

import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;

public class CoinsTest extends MinimaCliTest {

    @Test
    public void runBaseTests() throws Exception {
        boolean confirmed = minimaTestNode.waitForMinimaBlockConfirmation();

        //run coins
        String coinsOutput = minimaTestNode.runCommand("coins");
        super.runBaseTests(coinsOutput);
    }

}