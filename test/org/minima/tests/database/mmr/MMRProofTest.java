package org.minima.tests.database.mmr;

import org.minima.database.mmr.MMRProof;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;

import org.junit.Test;

public class MMRProofTest {

    @Test
    public void testWriteAndReadDataStream() {
        //try {
            MMRProof mmrp1 = new MMRProof();

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);

            //mmrp1.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            MMRProof mmrp2 = new MMRProof();
            //mmrp2.readDataStream(dis);

            //assertEquals("should be equal ", db1.getKeys().size(), db2.getKeys().size());
            //assertEquals("should be equal ", db1.getSimpleAddresses().size(), db2.getSimpleAddresses().size());
            //assertEquals("should be equal ", db1.getAllKnownTokens().size(), db2.getAllKnownTokens().size());
            //assertEquals("should be equal ", db1.getAllRows().size(), db2.getAllRows().size());
            //assertEquals("should be equal ", db1.getHistory().size(), db2.getHistory().size());
            //assertEquals("should be equal ", db1.getAllAddresses().size(), db2.getAllAddresses().size());
        //} catch (final IOException e) {
        //    System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
        //    assertTrue(" there should not be an IOException", false);
        //}
    }

}
