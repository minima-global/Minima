package org.minima.tests.database.userdb.java;

import org.minima.database.userdb.UserDBRow;

import org.minima.database.userdb.java.JavaUserDB;

import org.minima.objects.Address;
import org.minima.objects.PubPrivKey;

import org.minima.objects.base.MiniData;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;

import java.util.ArrayList;

import org.junit.Test;
import org.junit.internal.ArrayComparisonFailure;

public class JavaUserDBTests {

    @Test
    public void testConstructors() {
        JavaUserDB db1 = new JavaUserDB();

        assertNotNull("should not be null ", db1.getKeys());
        assertEquals("should be empty ", 0, db1.getKeys().size());

        assertNotNull("should not be null ", db1.getSimpleAddresses());
        assertEquals("should be empty ", 0, db1.getSimpleAddresses().size());

        assertNotNull("should not be null ", db1.getAllKnownTokens());
        assertEquals("should be empty ", 0, db1.getAllKnownTokens().size());

        assertNotNull("should not be null ", db1.getAllRows());
        assertEquals("should be empty ", 0, db1.getAllRows().size());

        assertNotNull("should not be null ", db1.getHistory());
        assertEquals("should be empty ", 0, db1.getHistory().size());
    }

    @Test
    public void testKeyHandling() {
        JavaUserDB db1 = new JavaUserDB();

        assertNotNull("should not be null ", db1.getKeys());
        assertEquals("should be empty ", 0, db1.getKeys().size());

        PubPrivKey ppk1 = db1.newPublicKey(512);
        assertEquals("should contain 1 key ", 1, db1.getKeys().size());

        PubPrivKey ppk2 = db1.newPublicKey(512);
        assertEquals("should contain 2 keys ", 2, db1.getKeys().size());

        PubPrivKey ppk3 = db1.newPublicKey(512);
        assertEquals("should contain 3 keys ", 3, db1.getKeys().size());

        MiniData md_ppk1 = ppk1.getPublicKey();
        PubPrivKey ppk11 = db1.getPubPrivKey(md_ppk1);
        assertEquals("should be equal ", ppk1, ppk11);

        MiniData md_ppk2 = ppk2.getPublicKey();
        PubPrivKey ppk21 = db1.getPubPrivKey(md_ppk2);
        assertEquals("should be equal ", ppk2, ppk21);

        MiniData md_ppk3 = ppk3.getPublicKey();
        PubPrivKey ppk31 = db1.getPubPrivKey(md_ppk3);
        assertEquals("should be equal ", ppk3, ppk31);

        MiniData md_dummy = new MiniData();
        PubPrivKey ppk_dummy = db1.getPubPrivKey(md_dummy);
        assertNull("should be null", ppk_dummy);
    }

    @Test
    public void testAddressHandling() {
        JavaUserDB db = new JavaUserDB();

        assertEquals("should contain 0 keys ", 0, db.getKeys().size());
        assertEquals("should contain 0 simple addresses ", 0, db.getSimpleAddresses().size());
        assertEquals("should contain 0 addresses in total ", 0, db.getAllAddresses().size());

        PubPrivKey[] pk = {
            db.newPublicKey(512),
            db.newPublicKey(512),
            db.newPublicKey(512),
            db.newPublicKey(512),
            db.newPublicKey(512)
        };
        assertEquals("should contain 5 keys ", 5, db.getKeys().size());

        Address[] sa = {
            db.newSimpleAddress(),
            db.newSimpleAddress(),
            db.newSimpleAddress(),
            db.newSimpleAddress(),
            db.newSimpleAddress()
        };
        assertEquals("should contain 5 simple addresses ", 5, db.getSimpleAddresses().size());
        assertEquals("should contain 5 addresses in total ", 5, db.getAllAddresses().size());

        Address[] sa_with_bitlength = {
            db.newSimpleAddress(512),
            db.newSimpleAddress(512),
            db.newSimpleAddress(512),
            db.newSimpleAddress(512),
            db.newSimpleAddress(512)
        };
        assertEquals("should contain 10 simple addresses ", 10, db.getSimpleAddresses().size());
        assertEquals("should contain 10 addresses in total ", 10, db.getAllAddresses().size());

        Address[] sa_from_pk = {
            db.newSimpleAddress(pk[0]),
            db.newSimpleAddress(pk[1]),
            db.newSimpleAddress(pk[2]),
            db.newSimpleAddress(pk[3]),
            db.newSimpleAddress(pk[4])
        };
        assertEquals("should contain 15 simple addresses ", 15, db.getSimpleAddresses().size());
        assertEquals("should contain 15 addresses in total ", 15, db.getAllAddresses().size());

        for (int i = 0; i < 5; i++) {
            assertTrue("should be simple address ", db.isSimpleAddress(sa[i].getAddressData()));
            assertTrue("should be simple address ", db.isSimpleAddress(sa_with_bitlength[i].getAddressData()));
            assertTrue("should be simple address ", db.isSimpleAddress(sa_from_pk[i].getAddressData()));
            assertEquals("should be equal ", pk[i].getPublicKey(), db.getPublicKeyForSimpleAddress(sa_from_pk[i].getAddressData()));
        }

        String script = "RETURN TRUE"; // same script returns same address!!!
        Address[] script_a = {
            db.newScriptAddress(script),
            db.newScriptAddress(script),
            db.newScriptAddress(script),
            db.newScriptAddress(script),
            db.newScriptAddress(script)
        };
        assertEquals("should contain 15 simple addresses ", 15, db.getSimpleAddresses().size());
        assertEquals("should contain 16 addresses in total ", 16, db.getAllAddresses().size());

        String script_extra = "RETURN TRUE RETURN TRUE"; // same script returns same address!!!
        Address[] extra_a = {
            db.newExtraAddress(script_extra),
            db.newExtraAddress(script_extra),
            db.newExtraAddress(script_extra),
            db.newExtraAddress(script_extra),
            db.newExtraAddress(script_extra)
        };
        assertEquals("should contain 15 simple addresses ", 15, db.getSimpleAddresses().size());
        assertEquals("should contain 17 total addresses ", 17, db.getAllAddresses().size());

        for (int i = 0; i < 5; i++) {
            assertEquals("should be equal ", script, db.getScript(script_a[i].getAddressData()));
            assertEquals("should be equal ", script_extra, db.getScript(extra_a[i].getAddressData()));
        }
    }

    @Test
    public void testUserRowHandling() {
        JavaUserDB db = new JavaUserDB();

        assertEquals("should contain 0 rows ", 0, db.getAllRows().size());

        UserDBRow[] user_rows = {
            db.addUserRow(0),
            db.addUserRow(1),
            db.addUserRow(2),
            db.addUserRow(3),
            db.addUserRow(4)
        };
        assertEquals("should contain 5 rows ", 5, db.getAllRows().size());

        for (int i = 0; i < user_rows.length; i++) {
            UserDBRow udbr = db.getUserRow(i);

            assertEquals("should be equal ", i, udbr.getID());
            assertNotNull("should not be null", udbr.getTransaction());
            assertNotNull("should not be null ", udbr.getWitness());

            assertEquals("should be equal ", user_rows[i].getTransaction(), udbr.getTransaction());
            assertEquals("should be equal ", user_rows[i].getWitness(), udbr.getWitness());
        }
        UserDBRow udbr = db.getUserRow(5);
        assertNull("should be null", udbr);

        assertEquals("should contain 5 rows ", 5, db.getAllRows().size());
        db.deleteUserRow(5);
        assertEquals("should contain 5 rows ", 5, db.getAllRows().size());
        db.deleteUserRow(4);
        assertEquals("should contain 4 rows ", 4, db.getAllRows().size());
        db.deleteUserRow(2);
        assertEquals("should contain 3 rows ", 3, db.getAllRows().size());
        db.deleteUserRow(3);
        assertEquals("should contain 2 rows ", 2, db.getAllRows().size());
        db.deleteUserRow(0);
        assertEquals("should contain 1 rows ", 1, db.getAllRows().size());
        db.deleteUserRow(1);
        assertEquals("should contain 0 rows ", 0, db.getAllRows().size());
    }

    @Test
    public void testWriteAndReadDataStream() {
        try {
            JavaUserDB db1 = createAndPopulateDB();

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);

            db1.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            JavaUserDB db2 = new JavaUserDB();
            db2.readDataStream(dis);

            assertEquals("should be equal ", db1.getKeys().size(), db2.getKeys().size());
            assertEquals("should be equal ", db1.getSimpleAddresses().size(), db2.getSimpleAddresses().size());
            assertEquals("should be equal ", db1.getAllKnownTokens().size(), db2.getAllKnownTokens().size());
            assertEquals("should be equal ", db1.getAllRows().size(), db2.getAllRows().size());
            assertEquals("should be equal ", db1.getHistory().size(), db2.getHistory().size());
            assertEquals("should be equal ", db1.getAllAddresses().size(), db2.getAllAddresses().size());

        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(" there should not be an IOException", false);
        }
    }

    public JavaUserDB createAndPopulateDB() {
        JavaUserDB db = new JavaUserDB();

        PubPrivKey[] pk = {
            db.newPublicKey(512),
            db.newPublicKey(512),
            db.newPublicKey(512),
            db.newPublicKey(512),
            db.newPublicKey(512)
        };

        Address[] sa = {
            db.newSimpleAddress(),
            db.newSimpleAddress(),
            db.newSimpleAddress(),
            db.newSimpleAddress(),
            db.newSimpleAddress()
        };

        Address[] sa_with_bitlength = {
            db.newSimpleAddress(512),
            db.newSimpleAddress(512),
            db.newSimpleAddress(512),
            db.newSimpleAddress(512),
            db.newSimpleAddress(512)
        };

        Address[] sa_from_pk = {
            db.newSimpleAddress(pk[0]),
            db.newSimpleAddress(pk[1]),
            db.newSimpleAddress(pk[2]),
            db.newSimpleAddress(pk[3]),
            db.newSimpleAddress(pk[4])
        };

        String script = "RETURN TRUE";
        Address[] script_a = {
            db.newScriptAddress(script),
            db.newScriptAddress(script),
            db.newScriptAddress(script),
            db.newScriptAddress(script),
            db.newScriptAddress(script)
        };

        String script_extra = "RETURN TRUE";
        Address[] extra_a = {
            db.newExtraAddress(script_extra),
            db.newExtraAddress(script_extra),
            db.newExtraAddress(script_extra),
            db.newExtraAddress(script_extra),
            db.newExtraAddress(script_extra)
        };

        UserDBRow[] user_rows = {
            db.addUserRow(0),
            db.addUserRow(1),
            db.addUserRow(2),
            db.addUserRow(3),
            db.addUserRow(4)
        };

        return db;
    }

}
