package org.minima.tests.database.mmr;

import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;

import org.junit.Test;
import org.minima.database.mmr.MMRData;
import org.minima.database.mmr.MMREntry;
import org.minima.database.mmr.MMRProof;
import org.minima.database.mmr.MMRSet;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.StateVariable;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.keys.PubPrivKey;
import org.minima.utils.Crypto;
import org.minima.utils.json.JSONObject;

public class MMRSetTest {

    @Test
    public void testWriteAndReadDataStream() {
        //try {
        //    MMRSet mmrs1 = new MMRSet();
        //
        //    ByteArrayOutputStream bos = new ByteArrayOutputStream();
        //    DataOutputStream dos = new DataOutputStream(bos);
        //
        //    mmrs1.writeDataStream(dos);
        //
        //    InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
        //    DataInputStream dis = new DataInputStream(inputStream);
        //
        //    MMRSet mmrs2 = new MMRSet();
        //    mmrs2.readDataStream(dis);
        //
        //    assertEquals("should be equal ", mmrs1., mmrs2.;
        //} catch (final IOException e) {
        //    System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
        //    assertTrue(" there should not be an IOException", false);
        //}

        //try {
        //    MMRSet mmrs1 = new MMRSet(512);
        //
        //    ByteArrayOutputStream bos = new ByteArrayOutputStream();
        //    DataOutputStream dos = new DataOutputStream(bos);
        //
        //    mmrs1.writeDataStream(dos);
        //
        //    InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
        //    DataInputStream dis = new DataInputStream(inputStream);
        //
        //    MMRSet mmrs2 = new MMRSet();
        //    mmrs2.readDataStream(dis);
        //
        //    //assertEquals("should be equal ", mmrs1., mmrs2.;
        //} catch (final IOException e) {
        //    System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
        //    assertTrue(" there should not be an IOException", false);
        //}
        //try {
        //    MMRSet mmrs1 = new MMRSet(512);
        //
        //    ByteArrayOutputStream bos = new ByteArrayOutputStream();
        //    DataOutputStream dos = new DataOutputStream(bos);
        //
        //    mmrs1.writeDataStream(dos);
        //
        //    InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
        //    DataInputStream dis = new DataInputStream(inputStream);
        //
        //    MMRSet mmrs2 = new MMRSet();
        //    mmrs2.readDataStream(dis);
        //
        //    //assertEquals("should be equal ", mmrs1., mmrs2.;
        //} catch (final IOException e) {
        //    System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
        //    assertTrue(" there should not be an IOException", false);
        //}
        try {
            MMRSet mmrs1 = new MMRSet();

            MMRProof mmrp1 = new MMRProof(new MiniNumber(1234567890), new MMRData(new MiniData("0x01"), new MiniNumber(new MiniNumber(1234567890))), new MiniNumber(987654321));
            mmrs1.addExternalUnspentCoin(mmrp1);
            MMRProof mmrp2 = new MMRProof(new MiniNumber(123456789), new MMRData(new MiniData("0x03"), new MiniNumber(new MiniNumber(123456789))), new MiniNumber(98765432));
            mmrs1.addExternalUnspentCoin(mmrp2);
            //MMRProof mmrp3 = new MMRProof(new MiniNumber(1234567891), new MMRData(new MiniData("0x02"), new MiniNumber(new MiniNumber(1234567891))), new MiniNumber(987654322));
            //mmrs1.addExternalUnspentCoin(mmrp3);

            MMRSet mmrs2 = new MMRSet(mmrs1);
            //mmrs2.setParent(mmrs1);

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);

            mmrs2.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            MMRSet mmrs3 = new MMRSet();
            mmrs3.readDataStream(dis);

            //assertEquals("should be equal ", mmrs1., mmrs2.;
        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(" there should not be an IOException", false);
        }

        try {
            MMRSet mmrs1 = new MMRSet();

            MMRData mmrd1 = new MMRData(new MiniData(), new MiniNumber(new MiniNumber(1234567890)));

            PubPrivKey pk = new PubPrivKey(512);
            String script = "RETURN SIGNEDBY ( " + pk.getPublicKey() + " )";
            Address addr = new Address(script, pk.getBitLength());
            ArrayList<StateVariable> states = new ArrayList<StateVariable>();
            states.clear();
            states.add(new StateVariable(0, "dummy"));

            MMRData mmrd2 = new MMRData(new MiniByte(0), new Coin(new MiniData("0x00"), addr.getAddressData(), MiniNumber.TEN, new MiniData("0x00")), new MiniNumber(1234567890), states);

            mmrs1.addUnspentCoin(mmrd1);
            mmrs1.addUnspentCoin(mmrd2);

            MMRProof mmrp1 = new MMRProof(new MiniNumber(1234567890), new MMRData(new MiniData("0x01"), new MiniNumber(new MiniNumber(1234567890))), new MiniNumber(987654321));
            mmrs1.addExternalUnspentCoin(mmrp1);
            MMRProof mmrp2 = new MMRProof(new MiniNumber(123456789), new MMRData(new MiniData("0x03"), new MiniNumber(new MiniNumber(123456789))), new MiniNumber(98765432));
            mmrs1.addExternalUnspentCoin(mmrp2);

            MMRSet mmrs2 = new MMRSet(mmrs1);

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);

            mmrs2.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            MMRSet mmrs3 = new MMRSet();
            mmrs3.readDataStream(dis);

            //assertEquals("should be equal ", mmrs1., mmrs2.;
        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(" there should not be an IOException", false);
        }

    }

    @Test
    public void testJSONConversion() {
        {
            MMRSet mmrs = new MMRSet();
            JSONObject json = mmrs.toJSON();
            assertTrue("JSON object should contain block key", json.containsKey("block"));
            assertTrue("JSON object should contain entrynumber key", json.containsKey("entrynumber"));
            assertTrue("JSON object should contain entries key", json.containsKey("entries"));
            assertTrue("JSON object should contain maxentries key", json.containsKey("maxentries"));
            assertTrue("JSON object should contain keepers key", json.containsKey("keepers"));
        }

        {
            MMRSet mmrs = new MMRSet(512);
            JSONObject json = mmrs.toJSON();
            assertTrue("JSON object should contain block key", json.containsKey("block"));
            assertTrue("JSON object should contain entrynumber key", json.containsKey("entrynumber"));
            assertTrue("JSON object should contain entries key", json.containsKey("entries"));
            assertTrue("JSON object should contain maxentries key", json.containsKey("maxentries"));
            assertTrue("JSON object should contain keepers key", json.containsKey("keepers"));
        }

    }

    @Test
    public void testMMRSetInsertions() {
        // Empty MMR
        MMRSet base = new MMRSet();
        assertTrue("Empty MMR has no entries.", base.getEntryNumber().isEqual(new MiniNumber(0)));
        assertTrue("Empty MMR", base.getMMRPeaks() != null && base.getMMRPeaks().size() == 0);

		//Add a single zero entry to create the first peak.
		Coin gencoin    = new Coin(new MiniData("0x00"), Address.TRUE_ADDRESS.getAddressData(), MiniNumber.ZERO, Coin.MINIMA_TOKENID);
        MMRData gendata = new MMRData(MiniByte.FALSE, gencoin, MiniNumber.ZERO, new ArrayList<StateVariable>());
        MiniData gendataHash = gendata.getFinalHash();
		base.addUnspentCoin(gendata);
        assertTrue("Genesis MMR set has one entry.", base.getEntryNumber().isEqual(new MiniNumber(1)));
        assertTrue("Genesis MMR only entry is also its only peak.", base.getMMRPeaks() != null && base.getMMRPeaks().size() == 1 && base.getMMRPeaks().get(0).getHashValue().isEqual(gendataHash));
        MMREntry e0 = base.getMMRPeaks().get(0);
        assertTrue("Genesis MMR unique element has entry number 0", e0.getEntryNumber().isEqual(new MiniNumber(0)));
        assertTrue("Genesis MMR unique element is at row 0", e0.getRow() == 0);
        assertTrue("MMR highest peak is at level 0", base.getMMRPeaks().get(0).getRow() == 0);
        //assertTrue("MMR max row is at level 0", base.getMaxRow() == 0);
        assertTrue("MGenesis MR root coin total is 0", base.getMMRRoot().getValueSum().isEqual(new MiniNumber(new MiniNumber(0))));
        // Print MMR tree: 0=root

        // create one node
        // tx input
        //Coin in = new Coin(gimme50.COINID_INPUT,Address.TRUE_ADDRESS.getAddressData(),new MiniNumber("50"), Coin.MINIMA_TOKENID);
        // tx outputs (we send twice to true address instead of new addresses, should fail as same data)
        Coin gimme50_a = new Coin(Coin.COINID_OUTPUT, Address.TRUE_ADDRESS.getAddressData(), new MiniNumber("25"), Coin.MINIMA_TOKENID);
        Coin gimme50_b = new Coin(Coin.COINID_OUTPUT, Address.TRUE_ADDRESS.getAddressData(), new MiniNumber("25"), Coin.MINIMA_TOKENID);
        MMRData data_a = new MMRData(MiniByte.FALSE, gimme50_a, MiniNumber.ZERO, new ArrayList<StateVariable>());
        MMRData data_b = new MMRData(MiniByte.FALSE, gimme50_b, MiniNumber.ZERO, new ArrayList<StateVariable>());
        MiniData hash_a = data_a.getFinalHash();
        MiniData hash_b = data_b.getFinalHash();
        MiniData hash_0a = Crypto.getInstance().hashAllObjects(512,gendataHash, hash_a, new MiniNumber(25)); // MMRSet.MMR_HASH_BITS);
        MiniData hash_0ab = Crypto.getInstance().hashAllObjects(512,hash_0a, hash_b, new MiniNumber(50)); // MMRSet.MMR_HASH_BITS);
        
        MiniNumber sumNum = new MiniNumber(new MiniNumber("25")); // 0+25
        base.addUnspentCoin(data_a);
        assertTrue("MMR set has two entries after adding one node.", base.getEntryNumber().isEqual(new MiniNumber(2)));
        assertTrue("MMR peaks count verification.", base.getMMRPeaks() != null && base.getMMRPeaks().size() == 1);
        assertTrue("MMR peak hash is not equal to previous peak hash.", base.getMMRPeaks() != null && !base.getMMRPeaks().get(0).getHashValue().isEqual(gendataHash));
        assertTrue("MMR peak hash is not equal to new node hash.", base.getMMRPeaks() != null && !base.getMMRPeaks().get(0).getHashValue().isEqual(hash_a));
        assertTrue("MMR peak hash is equal to constructed hash.", base.getMMRPeaks() != null && base.getMMRPeaks().get(0).getHashValue().isEqual(hash_0a));
        assertTrue("MMR peak hash is not equal to zero.", base.getMMRPeaks() != null && !base.getMMRPeaks().get(0).getData().getValueSum().isEqual(new MiniNumber(new MiniNumber("0"))));
        assertTrue("MMR peak hash is equal to sum of child node values.", base.getMMRPeaks() != null && base.getMMRPeaks().get(0).getData().getValueSum().isEqual(sumNum));
        assertTrue("MMR highest peak is at level 1", base.getMMRPeaks().get(0).getRow() == 1);
        assertTrue("MMR highest peak is at level 1", base.getMMRPeaks().get(0).getRow() == 1); assertTrue("MMR root coin total is 25", base.getMMRRoot().getValueSum().isEqual(sumNum));
        //assertTrue("MMR max row is at level 1", base.getMaxRow() == 1);
        //                  (2=root)
        // Print MMR tree: 0 1   (MMR canonical entries numbering)
        // leaf nodes: 0 a

        base.addUnspentCoin(data_b);
        assertTrue("MMR set has three entries after adding second node.", base.getEntryNumber().isEqual(new MiniNumber(3)));  // entryNumber only counts leaf nodes
        assertTrue("MMR has two peaks.", base.getMMRPeaks() != null && base.getMMRPeaks().size() == 2);
        assertTrue("MMR first peak hash is equal to constructed hash 0a.", base.getMMRPeaks() != null && base.getMMRPeaks().get(0).getHashValue().isEqual(hash_0a));
        assertTrue("MMR second peak hash is equal to hash b.", base.getMMRPeaks() != null && base.getMMRPeaks().get(1).getHashValue().isEqual(hash_b));
        assertTrue("MMR second peak row is lower than first peak row", base.getMMRPeaks().get(1).getRow() < base.getMMRPeaks().get(0).getRow());
        assertTrue("MMR highest peak is at level 1", base.getMMRPeaks().get(0).getRow() == 1);
        //assertTrue("MMR max row is at level 1", base.getMaxRow() == 1);
        assertTrue("MMR root hash is equal to hash of two peaks", base.getMMRRoot().getFinalHash().isEqual(hash_0ab));
        assertTrue("MMR root coin total is 50", base.getMMRRoot().getValueSum().isEqual(new MiniNumber(new MiniNumber(50))));
        //                    (4=root)
        //                  2
        // Print MMR tree: 0 1  3 (MMR canonical entries numbering: minima leaf nodes numbering would be: 0 1 2)
        // leaf nodes: 0 a b

        Coin coins_c = new Coin(Coin.COINID_OUTPUT, Address.TRUE_ADDRESS.getAddressData(), new MiniNumber("25"), Coin.MINIMA_TOKENID);
        MMRData data_c = new MMRData(MiniByte.FALSE, coins_c, MiniNumber.ZERO, new ArrayList<StateVariable>());
        MiniData hash_c = data_c.getFinalHash();
        MiniData hash_bc   = Crypto.getInstance().hashAllObjects(512, hash_b, hash_c, new MiniNumber(50)); // MMRSet.MMR_HASH_BITS);
        MiniData hash_0abc = Crypto.getInstance().hashAllObjects(512,hash_0a, hash_bc, new MiniNumber(75)); // MMRSet.MMR_HASH_BITS);
        
        base.addUnspentCoin(data_c);
        assertTrue("MMR set has four entries after adding third node.", base.getEntryNumber().isEqual(new MiniNumber(4)));  // entryNumber only counts leaf nodes
        assertTrue("MMR has one peak.", base.getMMRPeaks() != null && base.getMMRPeaks().size() == 1);
        assertTrue("MMR highest peak is at level 2", base.getMMRPeaks().get(0).getRow() == 2);
        assertTrue("MMR peak hash is equal to constructed hash 0abc.", base.getMMRPeaks() != null && base.getMMRPeaks().get(0).getHashValue().isEqual(hash_0abc));
        assertTrue("MMR root hash is equal to unique peak hash", base.getMMRRoot().getFinalHash().isEqual(hash_0abc));
        assertTrue("MMR root coin total is 75", base.getMMRRoot().getValueSum().isEqual(new MiniNumber(new MiniNumber(75))));
        //                    (6=root)
        //                  2    5 
        // Print MMR tree: 0 1  3 4 (MMR canonical entries numbering: minima leaf nodes numbering would be: 0 1 2)
        // leaf nodes: 0 a b c

        Coin coins_d = new Coin(Coin.COINID_OUTPUT, Address.TRUE_ADDRESS.getAddressData(), new MiniNumber("25"), Coin.MINIMA_TOKENID);
        MMRData data_d = new MMRData(MiniByte.FALSE, coins_d, MiniNumber.ZERO, new ArrayList<StateVariable>());
        MiniData hash_d = data_d.getFinalHash();
        MiniData hash_0abcd = Crypto.getInstance().hashAllObjects(512, hash_0abc, hash_d, new MiniNumber(100)); // MMRSet.MMR_HASH_BITS);

        base.addUnspentCoin(data_d);
        assertTrue("MMR set has five entries after adding fourth node.", base.getEntryNumber().isEqual(new MiniNumber(5)));  // entryNumber only counts leaf nodes
        assertTrue("MMR has two peaks.", base.getMMRPeaks() != null && base.getMMRPeaks().size() == 2);
        assertTrue("MMR highest peak is at level 2", base.getMMRPeaks().get(0).getRow() == 2);
        assertTrue("MMR first peak hash is equal to constructed hash 0abc.", base.getMMRPeaks() != null && base.getMMRPeaks().get(0).getHashValue().isEqual(hash_0abc));
        assertTrue("MMR second peak hash is equal to hash d.", base.getMMRPeaks() != null && base.getMMRPeaks().get(1).getHashValue().isEqual(hash_d));
        assertTrue("MMR second peak row is lower than first peak row", base.getMMRPeaks().get(1).getRow() < base.getMMRPeaks().get(0).getRow());
        assertTrue("MMR root hash is equal to hash of two peaks", base.getMMRRoot().getFinalHash().isEqual(hash_0abcd));
        assertTrue("MMR root coin total is 100", base.getMMRRoot().getValueSum().isEqual(new MiniNumber(new MiniNumber(100))));
        //                    (6=root)
        //                  2    5 
        // Print MMR tree: 0 1  3 4 7 (MMR canonical entries numbering: minima leaf nodes numbering would be: 0 1 2)
        // leaf nodes:     0 a  b c d

        // Now we spend the first coin (a, 25 coins, block zero)
        MMRData data_a_spent = new MMRData(MiniByte.TRUE, gimme50_a, MiniNumber.ZERO, new ArrayList<StateVariable>());
        MMRProof mmrProofa = new MMRProof(new MiniNumber(1), data_a, MiniNumber.ONE);
        // updateSpentCoin will replace a by a_spent -> new hash
        MiniData hash_a_spent = data_a_spent.getFinalHash();
        MiniData hash_0a_spent = Crypto.getInstance().hashAllObjects(512, gendataHash, hash_a_spent, new MiniNumber(0));
        MiniData hash_0a_spent_bc = Crypto.getInstance().hashAllObjects(512, hash_0a_spent, hash_bc,  new MiniNumber(50)); // MMRSet.MMR_HASH_BITS);
        MiniData hash_0a_spent_bcd =  Crypto.getInstance().hashAllObjects(512, hash_0a_spent_bc, hash_d,  new MiniNumber(75)); // MMRSet.MMR_HASH_BITS);
        base.updateSpentCoin(mmrProofa);
        
        assertTrue("MMR set has five entries after adding fourth node.", base.getEntryNumber().isEqual(new MiniNumber(5)));  // entryNumber only counts leaf nodes
        assertTrue("MMR has two peaks.", base.getMMRPeaks() != null && base.getMMRPeaks().size() == 2);
        assertTrue("MMR highest peak is at level 2", base.getMMRPeaks().get(0).getRow() == 2);
        assertTrue("MMR first peak hash is equal to recomputed hash 0a_spent_bc.", base.getMMRPeaks() != null && base.getMMRPeaks().get(0).getHashValue().isEqual(hash_0a_spent_bc));
        assertTrue("MMR second peak hash is equal to hash d.", base.getMMRPeaks() != null && base.getMMRPeaks().get(1).getHashValue().isEqual(hash_d));
        assertTrue("MMR second peak row is lower than first peak row", base.getMMRPeaks().get(1).getRow() < base.getMMRPeaks().get(0).getRow());
        assertTrue("MMR root hash is equal to hash of two peaks (recomputed)", base.getMMRRoot().getFinalHash().isEqual(hash_0a_spent_bcd));
        assertTrue("MMR root coin total is 75", base.getMMRRoot().getValueSum().isEqual(new MiniNumber(new MiniNumber(75))));
    }
}
