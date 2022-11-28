package org.minima.tests.cli.quit;

import org.junit.Test;
import org.junit.Before;
import org.junit.After;
import static org.junit.Assert.*;

import org.minima.system.Main;
import org.minima.tests.cli.MinimaTestNode;
import org.minima.tests.cli.MinimaCliTest;

import java.net.*;
import java.io.*;

public class QuitTest extends MinimaCliTest {

    @Test
    public void testQuitWithNoArgs () throws Exception
    {
        boolean portOpen = available(9001);

        assertFalse(portOpen);

        String output = super.minimaTestNode.runCommand("quit");

        //output = super.minimaTestNode.runCommand("status");

        Thread.sleep(5000);

        output = super.minimaTestNode.runCommand("status");
        portOpen = available(9001);

        assertTrue(portOpen);

        //runBaseTests(output);        
    }
    
    public void runBaseTests (String output) throws Exception
    {
        //The cmd response should be valid JSON
        super.runBaseTests(output);
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
            if( s != null){
                try {
                    s.close();
                } catch (IOException e) {
                    throw new RuntimeException("Something went wrong closing the port" , e);
                }
            }
        }
    }

}