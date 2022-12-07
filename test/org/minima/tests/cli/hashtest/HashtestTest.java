package org.minima.tests.cli.hashtest;

import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;

public class HashtestTest extends MinimaCliTest {

    @Test
    public void testHashtestWithNoArgs() throws Exception {
        String output = minimaTestNode.runCommand("hashtest");
        runBaseTests(output);
    }

    public void runBaseTests(String output) throws Exception {
        super.runBaseTests(output);
    }

}