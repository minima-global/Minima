package org.minima.tests.database.mmr;

import org.minima.database.mmr.MMREntry;

import org.minima.objects.base.MiniInteger;

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

public class MMREntryTest {

    @Test
    public void testWriteAndReadDataStream() {
        //try {
            MMREntry mmre1 = new MMREntry(123, new MiniInteger(1234567890));

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);

            //mmre1.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            MMREntry mmre2 = new MMREntry(0, new MiniInteger(0));
            //mmre2.readDataStream(dis);

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
