package org.minima.tests.objects;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;

import org.junit.Test;
import org.minima.objects.base.MiniData;
import org.minima.objects.proofs.SignatureProof;

public class SignatureProofTests {

    @Test
    public void testSignatureProof() {
        // MiniData c = new MiniData();
        MiniData j = new MiniData("0xFFFF");
        MiniData n = new MiniData("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF");
        SignatureProof sp = new SignatureProof(n, j);
        assertTrue("should not be equal ", sp.getSignature().isEqual(j));
        System.out.println("json  value " + sp.toJSON());

    }

    @Test
    public void testStaticReadAndWriteDataStream() {
        try {
            MiniData j = new MiniData("0xFFFF");
            MiniData n = new MiniData("0xFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF");
            SignatureProof sp = new SignatureProof(n, j);

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);
            sp.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);
            SignatureProof.ReadFromStream(dis);
            System.out.println("Proof json values in read and write stream - " + sp.toJSON());

            assertNotNull(sp);
        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(" there should not be an IOException", false);
        }
    }
}
