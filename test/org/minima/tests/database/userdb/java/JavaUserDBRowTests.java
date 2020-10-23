package org.minima.tests.database.userdb.java;

import org.minima.database.userdb.java.JavaUserDBRow;

import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.utils.json.JSONObject;

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
import org.junit.internal.ArrayComparisonFailure;

public class JavaUserDBRowTests {

    @Test
    public void testConstructors() {
        JavaUserDBRow r1 = new JavaUserDBRow();
        assertEquals("should be equal ", 0, r1.getID());
        assertEquals("should be equal ", null, r1.getTransaction());
        assertEquals("should be equal ", null, r1.getWitness());

        JavaUserDBRow r2 = new JavaUserDBRow(123);
        assertEquals("should be equal ", 123, r2.getID());
        assertNotNull("should not be null", r2.getTransaction());
        assertNotNull("should not be null ", r2.getWitness());
    }

    @Test
    public void testGettersAndSetters() {
        JavaUserDBRow r1 = new JavaUserDBRow();
        assertEquals("should be equal ", 0, r1.getID());
        assertEquals("should be equal ", null, r1.getTransaction());
        assertEquals("should be equal ", null, r1.getWitness());

        Transaction t = new Transaction();
        Witness w = new Witness();

        r1.setTransaction(t);
        r1.setWitness(w);
        assertEquals("should be equal ", t, r1.getTransaction());
        assertEquals("should be equal ", w, r1.getWitness());
    }

    @Test
    public void testWriteAndReadDataStream() {
        try {
            JavaUserDBRow r1 = new JavaUserDBRow(123);

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);

            r1.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            JavaUserDBRow r2 = new JavaUserDBRow();
            r2.readDataStream(dis);

            assertEquals("should be equal ", r1.getID(), r2.getID());
            //assertEquals("should be equal ", r1.getTransaction(), r2.getTransaction());
            //assertEquals("should be equal ", r1.getWitness(), r2.getWitness());

        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(" there should not be an IOException", false);
        }
    }

    @Test
    public void testJSONConversion() {
        JavaUserDBRow r1 = new JavaUserDBRow(123);
        JSONObject json = r1.toJSON();
        assertTrue("JSON object should contain id key", json.containsKey("id"));
        assertTrue("JSON object should contain transaction key", json.containsKey("transaction"));
        assertTrue("JSON object should contain witness key", json.containsKey("witness"));
    }

    @Test
    public void testToString() {
        JavaUserDBRow r1 = new JavaUserDBRow(123);
        String exp_s = "ID:123 Witness:" + r1.getWitness() + " Txn:" + r1.getTransaction();
        String obj_s = r1.toString();
        assertEquals("should be equal ", exp_s, obj_s);
    }
}
