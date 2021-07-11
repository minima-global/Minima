package org.minima.tests.objects.proofs;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;

import org.junit.Test;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.proofs.Proof;
import org.minima.objects.proofs.Proof.ProofChunk;

import static org.junit.Assert.*;

public class ProofTests {

    @Test
    public void testProof() throws Exception {
        MiniData proofValue = new MiniData("0xffffffff");
        Proof p = new Proof();
        System.out.println("Proof created values - " + p.getData());
        p.setData(proofValue);
        System.out.println("Proof created values - " + p.getData());
        assertEquals(p.getData(), proofValue);
        System.out.println("Proof created values - " + p.getChainSHAProof());
        MiniData chainsha = p.getChainSHAProof();
        System.out.println("Proof length values - " + p.getProofLen());
        p.setProof(chainsha);
        System.out.println("Proof json values - " + p.toJSON());
        System.out.println("Proof length values - " + p.getProofLen());
        MiniNumber k = new MiniNumber(1);
        MiniNumber l = new MiniNumber(k);
        MiniByte j = new MiniByte(1);
        MiniByte m = new MiniByte(8);
        Proof.ProofChunk pc = p.new ProofChunk(j, proofValue, l);
        assertEquals("should be equal", proofValue, pc.getHash());
        assertEquals("should be equal", j, pc.getLeft());
        assertEquals("should be equal", l, pc.getValue());
        System.out.println("Proof class values - " + pc.getClass());
        p.addProofChunk(j, proofValue);
        System.out.println("Proof length values - " + p.getProofLen());
        p.setHashBitLength(6);
        p.getChainSHAProof();
        System.out.println("Sha Chain Proof length values - " + p.getChainSHAProof());
        MiniData proofValue2 = new MiniData("0xffffffffffffffffffffffffff");
        p.setData(proofValue2);
        MiniData chainsha2 = p.getChainSHAProof();
        p.setProof(chainsha2);
        p.addProofChunk(m, proofValue2);
        p.setHashBitLength(8);
        System.out.println("Proof length values - " + p.getProofLen());
        System.out.println("Sha Chain Proof length values - " + p.getChainSHAProof());
        System.out.println("Final hash values - " + p.getFinalHash());
        System.out.println("Proof json values - " + p.toJSON());

        assertEquals(160, Proof.getChainSHABits("0x05"));
        assertEquals(192, Proof.getChainSHABits("0x06"));
        assertEquals(224, Proof.getChainSHABits("0x07"));
        assertEquals(256, Proof.getChainSHABits("0x08"));
        assertEquals(288, Proof.getChainSHABits("0x09"));
        assertEquals(320, Proof.getChainSHABits("0x0A"));
        assertEquals(384, Proof.getChainSHABits("0x0C"));
        assertEquals(416, Proof.getChainSHABits("0x0D"));
        assertEquals(448, Proof.getChainSHABits("0x0E"));
        assertEquals(480, Proof.getChainSHABits("0x0F"));
        assertEquals(512, Proof.getChainSHABits("0x10"));

        assertThrows(Exception.class, () -> {
            Proof.getChainSHABits("0x04");
        });
        assertThrows(Exception.class, () -> {
            Proof.getChainSHABits("0x0B");
        });
        assertThrows(Exception.class, () -> {
            Proof.getChainSHABits("0x11");
        });
    }

    @Test
    public void testStaticReadAndWriteDataStream() {
        try {

            MiniData proofValue = new MiniData("0xffffffff");
            MiniByte j = new MiniByte(8);

            Proof p = new Proof();
            p.setData(proofValue);
            p.addProofChunk(j, proofValue);

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);
            p.writeDataStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);
            Proof.ReadFromStream(dis);
            System.out.println("Proof json values in read and write stream - " + p.toJSON());
            MiniData md = p.getChainSHAProof();
            String shastr = md.toString();
            try {
                assertNotNull(Proof.getChainSHABits(shastr));
                System.out.println("Proof get sha bits read and write stream - " + Proof.getChainSHABits(shastr));

            } catch (Exception e) {
                System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
                assertTrue(" there should not be an IOException", false);
            }

            assertNotNull(p);
            p.getProofChunk(0);
        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(" there should not be an IOException", false);
        }
    }

    @Test
    public void testReadAndWriteDataStreamProofChunk() {
        {
            try {
                Proof p = new Proof();
                Proof.ProofChunk pc = p.new ProofChunk(new MiniByte(8), MiniData.getRandomData(32), MiniNumber.ONE);

                ByteArrayOutputStream bos = new ByteArrayOutputStream();
                DataOutputStream dos = new DataOutputStream(bos);

                pc.writeDataStream(dos);

                InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
                DataInputStream dis = new DataInputStream(inputStream);

                Proof p1 = new Proof();
                Proof.ProofChunk pc1 = p.new ProofChunk();

                pc1.readDataStream(dis);

            } catch (Exception e) {
                fail();
            }
        }
        {
            try {
                Proof p = new Proof();
                Proof.ProofChunk pc = p.new ProofChunk(new MiniByte(8), MiniData.getRandomData(32), MiniNumber.ZERO);

                ByteArrayOutputStream bos = new ByteArrayOutputStream();
                DataOutputStream dos = new DataOutputStream(bos);

                pc.writeDataStream(dos);

                InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
                DataInputStream dis = new DataInputStream(inputStream);

                Proof p1 = new Proof();
                Proof.ProofChunk pc1 = p.new ProofChunk();

                pc1.readDataStream(dis);

            } catch (Exception e) {
                fail();
            }
        }
    }
}
