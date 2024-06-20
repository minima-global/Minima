package org.minima.tests.cli.hash;

import org.junit.Test;
import org.junit.Before;
import org.junit.After;
import static org.junit.Assert.*;

import org.minima.system.commands.CommandException;

import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;

import org.minima.system.Main;
import org.minima.tests.cli.MinimaTestNode;
import org.minima.tests.cli.MinimaCliTest;

public class HashTest extends MinimaCliTest {

    public MinimaTestNode test = new MinimaTestNode();
    
    public void runBaseTests (String output) throws Exception
    {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        System.out.println("status must be false: " + json.get("status"));
        assertFalse((boolean)json.get("status"));

        //cmd response pending should be false
        System.out.println("pending must be false:" + json.get("pending").toString());
        assertFalse((boolean)json.get("pending"));
    }

    @Test
    public void testConnectWithNoArgs () throws Exception
    {
        String output = test.runCommand("hash");

        runBaseTests(output);        
    }

    @Test
    public void runKeccakTest() throws Exception {
        String output = test.runCommand("hash data:0xB291E3A6D546E1D5E61A3EF08D01474386749D267774D718E8E07280F678A628 type:keccak");
        System.out.println("keccak output: ");
        System.out.println(output);
    }

    @Test
    public void runSha2Test() throws Exception {
        String output = test.runCommand("hash data:0xB291E3A6D546E1D5E61A3EF08D01474386749D267774D718E8E07280F678A628 type:sha2");
        System.out.println("sha2 output: ");
        System.out.println(output);
    }

    @Test
    public void runSha3Test() throws Exception {
        String output = test.runCommand("hash data:0xB291E3A6D546E1D5E61A3EF08D01474386749D267774D718E8E07280F678A628 type:sha3");
        System.out.println("sha3 output: ");
        System.out.println(output);
    }
}