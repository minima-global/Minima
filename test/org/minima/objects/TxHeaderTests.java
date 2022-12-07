package org.minima.objects;

import org.junit.jupiter.api.Test;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Crypto;
import org.minima.utils.json.JSONObject;

import java.io.*;

import static org.junit.jupiter.api.Assertions.assertTrue;

public class TxHeaderTests {


    @Test
    public void TxHeaderTests() {
        // in real use case Txheader is configured / built by TxPoW
        TxHeader mHeader = new TxHeader();
        mHeader.mTxBodyHash = Crypto.getInstance().hashObject(new MiniData("0x1234"));
        mHeader.mBlockDifficulty = new MiniData("0xffff");
        mHeader.mTimeMilli = new MiniNumber(99999999);
        mHeader.mBlockNumber = new MiniNumber(123450);
        mHeader.mNonce = new MiniNumber(900000);
        assertTrue(mHeader.getBodyHash() != null, "tx header should have a body hash");
        assertTrue(mHeader.getBodyHash().isMore(new MiniData("0x0")), "body hash should not be empty");
        JSONObject json = mHeader.toJSON();
        assertTrue(json != null, "json object should not be null");
        try {
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);
            mHeader.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            TxHeader mHeaderRead = new TxHeader();
            mHeaderRead.readDataStream(dis);

            assertTrue(mHeaderRead != null, "mHeaderRead should not be null");

        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(false, " there should not be an IOException");
        }

    }
}
