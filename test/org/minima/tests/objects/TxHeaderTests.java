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
import org.minima.objects.TxHeader;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniInteger;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Crypto;
import org.minima.utils.json.JSONObject;

public class TxHeaderTests {

    @Test

    public void TxHeaderTests() {
        // in real use case Txheader is configured / built by TxPoW
        TxHeader mHeader = new TxHeader();
        mHeader.mTxBodyHash = Crypto.getInstance().hashObject(new MiniData("Hello World"));
        mHeader.mBlockDifficulty = new MiniData("0xffff");
        mHeader.mTimeSecs = new MiniNumber(99999999).divRoundDown(MiniNumber.THOUSAND);
        mHeader.mBlockNumber = new MiniNumber(123450);
        mHeader.mNonce = new MiniInteger(900000);
        assertTrue("tx header should have a body hash", mHeader.getBodyHash() != null);
        assertTrue("body hash should not be empty", mHeader.getBodyHash().isMore(new MiniData("0x0")));
        JSONObject json = mHeader.toJSON();
        assertTrue("json object should not be null", json != null);
        try {
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);
            mHeader.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            TxHeader mHeaderRead = new TxHeader();
            mHeaderRead.readDataStream(dis);

            assertTrue("mHeaderRead should not be null", mHeaderRead != null);

        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(" there should not be an IOException", false);
        }

    }
}
