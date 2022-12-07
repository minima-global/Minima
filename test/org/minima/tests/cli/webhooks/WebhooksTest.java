package org.minima.tests.cli.webhooks;

import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;

public class WebhooksTest extends MinimaCliTest {

    @Test
    public void testWebhooksWithNoArgs() throws Exception {
        String output = minimaTestNode.runCommand("webhooks");
        runBaseTests(output);
    }

    public void runBaseTests(String output) throws Exception {
        super.runBaseTests(output);
    }

}