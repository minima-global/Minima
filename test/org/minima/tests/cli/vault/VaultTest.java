package org.minima.tests.cli.vault;

import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;

public class VaultTest extends MinimaCliTest {

    @Test
    public void testVaultWithNoArgs() throws Exception {
        String output = minimaTestNode.runCommand("vault");
        runBaseTests(output);
    }

    public void runBaseTests(String output) throws Exception {
        super.runBaseTests(output);
    }

}