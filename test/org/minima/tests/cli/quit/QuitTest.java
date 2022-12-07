package org.minima.tests.cli.quit;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.minima.tests.cli.MinimaCliTest;

import java.io.IOException;
import java.net.Socket;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

public class QuitTest extends MinimaCliTest {

    @Disabled
    @Test
    public void testQuitWithNoArgs() throws Exception {
        boolean portOpen = available(9001);

        assertFalse(portOpen);

        String output = minimaTestNode.runCommand("quit");

        //output = minimaTestNode.runCommand("status");

        Thread.sleep(5000);

        output = minimaTestNode.runCommand("status");
        portOpen = available(9001);

        assertTrue(portOpen);

        runBaseTests(output);
    }

    private static boolean available(int port) {
        System.out.println("--------------Testing port " + port);
        Socket s = null;
        try {
            s = new Socket("localhost", port);

            // If the code makes it this far without an exception it means
            // something is using the port and has responded.
            System.out.println("--------------Port " + port + " is not available");
            return false;
        } catch (IOException e) {
            System.out.println("--------------Port " + port + " is available");
            return true;
        } finally {
            if (s != null) {
                try {
                    s.close();
                } catch (IOException e) {
                    throw new RuntimeException("Something went wrong closing the port", e);
                }
            }
        }
    }

}