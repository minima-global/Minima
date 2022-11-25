package org.minima.tests.cli.mmrcreate;

import org.junit.Test;
import org.junit.Before;
import org.junit.After;
import static org.junit.Assert.*;

import org.minima.system.commands.CommandException;

import java.util.Arrays;

import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;

import org.minima.system.Main;
import org.minima.tests.cli.MinimaTestNode;
import org.minima.tests.cli.MinimaCliTest;

public class MmrCreate extends MinimaCliTest {

    //public MinimaTestNode test = new MinimaTestNode();

    @Test
    public void testMmrcreateWithNoArgs () throws Exception
    {
        String output = super.minimaTestNode.runCommand("mmrcreate");
        runBaseTests(output);        
    }

    /*
        ERROR HERE:

        mmrcreate nodes:[] 

        java.lang.IndexOutOfBoundsException: Index 0 out of bounds for length 0
    */ 

    @Test
    public void createEmptyTree() throws Exception
    {
        String output = super.minimaTestNode.runCommand("mmrcreate nodes:"+noData);
        runBaseTests(output);        
        super.minimaTestNode.killMinima();
    }

    @Test
    public void createTreeWithGarbage() throws Exception
    {
        String output = super.minimaTestNode.runCommand("mmrcreate nodes:GARBAGESTRING");
        runBaseTests(output);        
        super.minimaTestNode.killMinima();
    }

    @Test
    public void createSmallTree() throws Exception
    {
        createTree(smallData);
    }

    @Test
    public void createMediumTree() throws Exception
    {
        createTree(mediumData);
    }

    @Test
    public void createBigTree() throws Exception
    {
        createTree(bigData);
    }

    public void runBaseTests (String output) throws Exception
    {
        //The cmd response should be valid JSON
        JSONObject json = (JSONObject) new JSONParser().parse(output);

        //status of the cmd request must be true
        assertFalse("status must be false: ", (boolean)json.get("status"));

        //cmd response pending should be false
        assertFalse("pending must be false: ", (boolean)json.get("pending"));
    }

    public void createTree(String treedata) throws Exception
    {
        String output = super.minimaTestNode.runCommand("mmrcreate nodes:"+treedata); 

        var jsonObject =  (JSONObject) new JSONParser().parse(output.toString());
        JSONObject innerJson = (JSONObject) jsonObject.get("response");
        JSONArray innerJsonObj = (JSONArray) innerJson.get("nodes");
        JSONObject firstNode = (JSONObject) innerJsonObj.get(0);
       

        String data = firstNode.get("data").toString();
        String root = innerJson.get("root").toString();
        String proof = firstNode.get("proof").toString();

        //run mmr proof to check it successfully concludes that a node is in the mmrtree
        output = super.minimaTestNode.runCommand("mmrproof data:0x2941de4921503b127164fad64b418604beb5fee6fac5628a4d47647d4f84cd7a proof:"+proof+" root:"+root);

        jsonObject =  (JSONObject) new JSONParser().parse(output.toString());
        innerJson = (JSONObject) jsonObject.get("response");
        boolean valid = (boolean)innerJson.get("valid");
        
        assertTrue(valid);

    }

    String noData = "[``]".replace('`', '"');
    String smallData = "[`0x2941de4921503b127164fad64b418604beb5fee6fac5628a4d47647d4f84cd7a`, `0x456ad4a6b3ffb2d3bd29368f5b1a0bbebc64b6d4f1de28468bf44ad8a53e84dc`, `0x996a744b9d59663d68782636db45f9c9f1193971e02bac6be2acb013472c5fa3`]".replace('`', '"');
    String mediumData = "[`0x2941de4921503b127164fad64b418604beb5fee6fac5628a4d47647d4f84cd7a`,`0x456ad4a6b3ffb2d3bd29368f5b1a0bbebc64b6d4f1de28468bf44ad8a53e84dc`,`0x996a744b9d59663d68782636db45f9c9f1193971e02bac6be2acb013472c5fa3`,`0x7db62e4bb68492c5d9054782473757cd1ee39aba2576f9fa8155b15b7507a201`,`0xf6d0905ff448e2b5b7ea321e6ce01d157b2a1bcb2c905b00bc3ed94eef2b5f09`,`0x5036a40c760958b942cb37a178f6d53ea18e58985134c8ff2c20527f7155db79`,`0xa5ddd1ed3870aca0a85fc3ee1dc846fd665e04c1fd4e5711a9928637cc105f73`,`0xa1e540f297b6095975c85e52915a7bbfad16465b0dfa851df4932ebd2d6ac0bb`,`0x9c198b87da2c8c622bc554685c706d3fa7a9b0efeeab184438075d6002757aa6`,`0xd2aced31a497c17d01d73e7d57339ebe731a2a937d784b61b45b26c1a3bd697d`]".replace('`', '"');
    String bigData = "[`0x2941de4921503b127164fad64b418604beb5fee6fac5628a4d47647d4f84cd7a`,`0x456ad4a6b3ffb2d3bd29368f5b1a0bbebc64b6d4f1de28468bf44ad8a53e84dc`,`0x996a744b9d59663d68782636db45f9c9f1193971e02bac6be2acb013472c5fa3`,`0x7db62e4bb68492c5d9054782473757cd1ee39aba2576f9fa8155b15b7507a201`,`0xf6d0905ff448e2b5b7ea321e6ce01d157b2a1bcb2c905b00bc3ed94eef2b5f09`,`0x5036a40c760958b942cb37a178f6d53ea18e58985134c8ff2c20527f7155db79`,`0xa5ddd1ed3870aca0a85fc3ee1dc846fd665e04c1fd4e5711a9928637cc105f73`,`0xa1e540f297b6095975c85e52915a7bbfad16465b0dfa851df4932ebd2d6ac0bb`,`0x9c198b87da2c8c622bc554685c706d3fa7a9b0efeeab184438075d6002757aa6`,`0xd2aced31a497c17d01d73e7d57339ebe731a2a937d784b61b45b26c1a3bd697d`,`0xb9b177f32aecf029271720c468610274ffd0a384914a06a4e46d34c98f484906`,`0xf5050bab94e8b8e49ada302c6b003e044a514c753b006466e5bf2b3194bba042`,`0x1dc74573d18b06021c0a30f17def9c6c03f8862a93a6277b7a41b02c82db2026`,`0x8f44bfdb40f26539b40478a3e1dce0f91b75c46668fd1de2346f434fb5cb8e7e`,`0x5387f64a74b10d7f46676e1a2493de452943461e732567eca77ba4b3fb0fffc8`,`0xf96140242bb611dd06a19c8db62a9afae729ceaf89240974c55fa6eed513a13f`,`0x61690eff87cc86ff18bc8eed54167b931b21c476e70b506833ed8335ac698985`,`0x629fa7b6d876f3832c52a88d2828c4b4fcc75525cc0f09e27145de4498e20805`,`0x520bc8156de3f1a8093135e0409a6c436d30e38f5d87ead11edd16aa16c42c50`,`0xea76cd1b872b46345ff584931246b212821a2ea45a837fa55597e46c23ba4bf0`,`0xc1efc1a3cefdfee7321195598d1e373b2ddb014d09bdd7944d9f40f5abc11555`,`0xd826db6ad65401cf569e50d1fb8372f14fa71639cf7df78435d8dfbb1139c488`,`0x6056240cf0ed9ef4656964e0cb69653cbe59baaac98b1a3f95b3d4cc2c72fa5b`,`0xec4c48ff8bba1c43e1c01ac20563ac5363a34dc5a8c0619c024e26d99361cb22`,`0x0c15e6abbe42d2ca078a62e58e23c7fe41619a5c3b105149d1459f98ecadd26f`,`0xd3e4284cb909fbd255b331ae4c1f983a71a48206627e5d3d652d8cebf16d0c91`,`0xebe962fb651c0e6bd76246d4943e14afcd37288488d5cb2965599cd9fb419905`,`0x6f9834f51e94eb343bc225c9ce2627c5bee25fa7bd3dfad1bd594c0bd1f43cd2`,`0x06b9c5460354ac4c2ac3bae1f12539e06778186e110954f9ffbd89bb0d976be6`,`0x26b38577f7882dfbcc144125dd3b6a8ad08fb2a134dd4c9ff2803d4c4d6f9f14`,`0xde56cf517b93d9fb2ec355f3892b362c9ee8a12141ccf3c521e9fec0034ae0f8`,`0xa4a6dfa749d1192b8575fb3c4a7adc2a8dfe4908712806cc40f2b025e2969546`,`0x4343b8d08912567bf8845824318c3abc617ae510df2c4f3cb34c0c76dded620d`,`0x2790edd0499baa6a2f1c2eb203133f343875f8410df0554ea4fa941838fbd36e`,`0x0fd4a8c14b0a084c0432d5ac73d30730e0ad945fa53886ff5aac2999f55a436f`,`0x347e5cdb1f9fca756fe62e5e8757341c08248537967b76e7e73263cfa85340d3`,`0x241a4e8e0e8c19e45000ce511360b9b6cab40e0061be32157422fac01eb4dcbb`,`0x87df96ff0cc0df67fa574534178914296509b75aedbd6a632b5635c41e1f56a2`,`0xfb837d62058281f9e291ca01269ef7a6832d8305499703354dbe5e4e2fa89cdb`,`0xf10d549fd94a826d606ab3f00fd7049777fbc3b4e1793685e14cae0d67ea2b6f`,`0xfddf8884cb7a264d77c05c7b36f0f8fa146bbe9132d85d3f09dca6a3eef32559`,`0x74785c8961488a3b4f1161db79dec50e1dcaddca3886e1bffd953d0b5e2a8310`,`0xe072b1f29c57bbcbdb485eb9230448e9d34457d714e8e792418b4543b356788f`,`0x8d483cf8140d3a572bd6234e33a419fa8afaae3615df2b869d84839b7f207168`,`0xaad7a4b1177cc6db0662dccacc0e5e49e7c1c0b3e71b8103648b9b9baa22dcf5`,`0x0303949e3589b67740c5e24efcdd408dfb92509d50386b815984cc327fd76623`,`0x8fbc43ad175b2a7b3cd25e4aace41220e0054558e4188fb9550c1d5284758b5d`,`0xf81ccde88c459590cd7fc6dc9a8ed4f6f158ee0581f1047c7116946baa770710`,`0x03dc22b9200b1631c7ccaf4ca8b4ae7004553cf8c9d116bc5dd838d6bd46eb02`,`0xbd69655d17bc95d2493b274999c68fff7746478e0a050ed95149e407d5851dca`]".replace('`', '"');

}