package org.minima.tests.objects;

import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;

import org.junit.Test;
import org.minima.objects.Transaction;
import org.minima.objects.TxBody;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.utils.json.JSONObject;

public class TxBodyTests {

    @Test
    public void testTxBody() {
        TxBody body = new TxBody();
        body.mTxnDifficulty = new MiniData("0xffff");
        body.mTransaction = new Transaction();
        body.mWitness = new Witness();
        assertTrue("txbody not null", body != null);
        JSONObject json = body.toJSON();
        assertTrue("JSON not null", json != null);

        try {

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);
            body.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            TxBody bodyRead = new TxBody();
            bodyRead.readDataStream(dis);

        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(" there should not be an IOException", false);
        }

    }
}
