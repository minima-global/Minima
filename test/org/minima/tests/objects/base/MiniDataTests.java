package org.minima.tests.objects.base;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigInteger;
import java.util.Arrays;

import org.junit.Test;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.objects.base.MiniData;

import static org.junit.Assert.*;

public class MiniDataTests {

    @Test
    public void testValueBytes() {
        MiniData i = new MiniData("0x1388");
        MiniData j = new MiniData("0xFFFF");
        MiniData k = new MiniData("5475746f7269616c73706f69674");
        MiniData[] mData = {j, k};
        MiniData[] mDataCompare = {i, j, k};
        System.out.println("i value is " + i);
        System.out.println("j value is " + j);
        System.out.println("k value is " + k);
        System.out.println("mData value is " + Arrays.toString(mData));
        System.out.println("mDataCompare value is " + Arrays.toString(mDataCompare));
        assertNotNull("should not be null", i.getLength());
        System.out.println("i.getLength()" + i + " resolves as " + i.getLength());
        assertNotNull("should not be null", j.getLength());
        System.out.println("i.getLength()" + j + " resolves as " + j.getLength());
        assertNotNull("should not be null", k.getLength());
        System.out.println("i.getLength()" + k + " resolves as " + k.getLength());
        assertNotNull("should not be null", i.getData());
        System.out.println("i.getData()" + i + " resolves as " + i.getData());
        assertNotNull("should not be null", i.getDataValue());
        System.out.println("i.getDataValue()" + i + " resolves as " + i.getDataValue());
        assertNotNull("should not be null", i.getDataValueDecimal());
        System.out.println("i.getDataValueDecimal()" + i + " resolves as " + i.getDataValueDecimal());
        assertNotNull("should not be null", i.to0xString());
        System.out.println("i.to0xString()" + i + " resolves as " + i.to0xString());
        assertNotNull("should not be null", i.toString());
        System.out.println("i.toString()" + i + " resolves as " + i.toString());
        assertNotNull("should not be null", mData[0].compare(k));
        System.out.println("mData[0].compare(k) resolves as " + mData[0].compare(k));
        assertNotNull("should not be null", i.getRandomData(1));
        System.out.println("i.getRandomData(1)" + i + " resolves as " + i.getRandomData(1));
        assertNotNull("should not be null", i.shiftr(64));
        System.out.println("i.shiftr(64) i:" + i + " resolves as " + i.shiftr(64));
        assertNotNull("should not be null", i.shiftl(64));
        System.out.println("i.shiftl(64) i:" + i + " resolves as " + i.shiftl(64));
        assertNotNull("should not be null", i.concat(j));
        System.out.println("i.concat(j) i:" + i + " j:" + j + " resolves as " + i.concat(j));
        assertNotNull("should not be null", i.to0xString(1));
        System.out.println("i.to0xString(1)" + i + " resolves as " + i.to0xString(1));
        assertNotNull("should not be null", i.to0xString(10000));
        System.out.println("i.to0xString(10000)" + i + " resolves as " + i.to0xString(10000));
        assertFalse("should be equal to false", i.equals(j));
        System.out.println("i.equals(j) i:" + i + " j:" + j + " resolves as " + i.equals(j));
        assertTrue("should be equal to true", j.equals(j));
        System.out.println("j.equals(j) j:" + j + " j:" + j + " resolves as " + j.equals(j));
        assertFalse("should be equal to false", i.isEqual(j));
        System.out.println("i.isEqual(j)) i:" + i + " j:" + j + " resolves as " + i.isEqual(j));
        assertFalse("should be equal to false", mData[1].isEqual(mDataCompare[1]));
        System.out.println("mData[1].isEqual(mDataCompare[1]) mData[1]:" + mData[1] + " mDataCompare[1]:"
                + mDataCompare[1] + " resolves as " + mData[1].isEqual(mDataCompare[1]));
        assertTrue("should be equal to true", i.isLess(j));
        System.out.println("i.isLess(j) i:" + i + " j:" + j + " resolves as " + i.isLess(j));
        assertFalse("should be equal to false", j.isLess(i));
        System.out.println("j.isLess(i) i:" + i + " j:" + j + " resolves as " + j.isLess(i));
        assertFalse("should be less that or equal with value false", j.isLessEqual(i));
        System.out.println("j.isLessEqual(i) i:" + i + " j:" + j + " resolves as " + j.isLessEqual(i));
        assertTrue("should be less that or equal with value true", j.isLessEqual(j));
        System.out.println("j.isLess(i) i:" + i + " j:" + j + " resolves as " + j.isLess(i));
        assertTrue("should be more than with value true", j.isMore(i));
        System.out.println("j.isMore(i) i:" + i + " j:" + j + " resolves as " + j.isMore(i));
        assertFalse("should be more than with value false", i.isMore(j));
        System.out.println("i.isMore(j) i:" + i + " j:" + j + " resolves as " + i.isMore(j));
        assertFalse("should be more that or equal with value false", i.isMoreEqual(j));
        System.out.println("i.isMoreEqual(j) i:" + i + " j:" + j + " resolves as " + i.isMoreEqual(j));
        assertTrue("should be more that or equal with value true", j.isMoreEqual(j));
        System.out.println("j.isMoreEqual(j) j:" + j + " j:" + j + " resolves as " + j.isMoreEqual(j));

        assertThrows(IllegalArgumentException.class, () -> {
            MiniData md = new MiniData(BigInteger.valueOf(-1));
        });
        assertThrows(IllegalArgumentException.class, () -> {
            MiniData md = new MiniData(BigInteger.valueOf(-100));
        });
        assertThrows(IllegalArgumentException.class, () -> {
            MiniData md = new MiniData(BigInteger.valueOf(-123456789));
        });
    }

    @Test
    public void testReadFromStreamData() {
        try {
            String value = "0xFFFF";
            MiniData j = new MiniData(value);
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);
            j.writeDataStream(dos);

            for (int l = 0; l < 10000; l++) {
                bos.write(32); // large possible value, MSB is +/- sign
            }

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            j.readDataStream(dis);
            assertNotNull(j);
            System.out.println("j.toString = " + j.toString());
            assertTrue("j matches original value", j.toString().compareToIgnoreCase(value) == 0);
            System.out.println(" j is now equal to " + j);

            bos.reset();

            j.readDataStream(dis, 27);

        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue("test", true);
        }

    }

    @Test
    public void testWriteAndReadDataStream() {
        {
            try {
                MiniData md = new MiniData("");

                ByteArrayOutputStream bos = new ByteArrayOutputStream();
                DataOutputStream dos = new DataOutputStream(bos);

                md.writeDataStream(dos);

                InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
                DataInputStream dis = new DataInputStream(inputStream);

                MiniData md1 = new MiniData("01234567");
                md1.readDataStream(dis);

                assertEquals(md.getLength(), md1.getLength());
                assertTrue(md.isEqual(md1));
            } catch (Exception e) {
                fail();
            }
        }

        {
            try {
                MiniData md = new MiniData("1234567890");

                ByteArrayOutputStream bos = new ByteArrayOutputStream();
                DataOutputStream dos = new DataOutputStream(bos);

                md.writeDataStream(dos);

                InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
                DataInputStream dis = new DataInputStream(inputStream);

                MiniData md1 = new MiniData("01234567");
                md1.readDataStream(dis, md.getLength());

                assertEquals(md.getLength(), md1.getLength());
                assertTrue(md.isEqual(md1));
            } catch (Exception e) {
                fail();
            }
        }

        {
            try {
                MiniData md = new MiniData("1234567890");

                ByteArrayOutputStream bos = new ByteArrayOutputStream();
                DataOutputStream dos = new DataOutputStream(bos);

                md.writeDataStream(dos);

                InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
                DataInputStream dis = new DataInputStream(inputStream);

                MiniData md1 = new MiniData("01234567");
                assertThrows(IOException.class, () -> {
                    md1.readDataStream(dis, md.getLength() + 16);
                });
            } catch (Exception e) {
                fail();
            }
        }
    }

    @Test
    public void testWriteAndReadHashDataStream() {
        {
            try {
                MiniData md = MiniData.getRandomData(65);

                ByteArrayOutputStream bos = new ByteArrayOutputStream();
                DataOutputStream dos = new DataOutputStream(bos);

                assertThrows(IOException.class, () -> {
                    md.writeHashToStream(dos);
                });
            } catch (Exception e) {
                fail();
            }
        }
        {
            try {
                MiniData md = MiniData.getRandomData(65);

                ByteArrayOutputStream bos = new ByteArrayOutputStream();
                DataOutputStream dos = new DataOutputStream(bos);

                md.writeDataStream(dos);

                InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
                DataInputStream dis = new DataInputStream(inputStream);

                MiniData md1 = new MiniData("01234567");
                assertThrows(IOException.class, () -> {
                    md1.readHashFromStream(dis);
                });
            } catch (Exception e) {
                fail();
            }
        }
        {
            try {
                MiniData md = MiniData.getRandomData(65);

                ByteArrayOutputStream bos = new ByteArrayOutputStream();
                DataOutputStream dos = new DataOutputStream(bos);

                dos.writeInt(-5);
                dos.write(md.getData());

                InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
                DataInputStream dis = new DataInputStream(inputStream);

                MiniData md1 = new MiniData("01234567");
                assertThrows(IOException.class, () -> {
                    md1.readHashFromStream(dis);
                });
            } catch (Exception e) {
                fail();
            }
        }
    }

    @Test
    public void testReadFromStreamDataTooBig() {
        try {
            MiniData j = new MiniData("0xFFFFFFFFFFFFFFFFFFFFFF"
                    + "FFFFFFFFFFFFFFFFFFFF"
                    + "FFFFFFFFFFFFFFFFFFFF"
                    + "FFFF"
                    + "FFFFFFFFFFFFFFFFFFFF"
                    + "FFFFFFFFFFFFFFFFFFFF"
                    + "FFFFFFFFFFFFFFFFFFFF"
                    + "FFFFFFFFFFFFFFFFFFFF");

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);
            j.writeDataStream(dos);
            System.out.println(" j is now equal to " + j);
            for (int l = 0; l < 10000000; l++) {
                bos.write(128); // large possible value, MSB is +/- sign
            }

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);
            // bos.flush();
            j.readDataStream(dis, -1);
            dis.reset();
            j.ReadFromStream(dis);
            // assertNotNull(j);
            System.out.println(" j is now equal to " + j);
        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(" there should be an IOException with message input too large ",
                    e.getMessage().contains(new String("input too large")));
        }

    }

    // @Test
    // public void testWriteHashToStreamDataTooBig() {
    //     try {
    //         MiniData i = new MiniData("0x11");
    //         MiniData j = new MiniData("0xFFFF");
    //         MiniData k = new MiniData("FFFFFFFFFFFFFFF");
    //         ByteArrayOutputStream bos = new ByteArrayOutputStream();
    //         DataOutputStream dos = new DataOutputStream(bos);
    //         j.writeHashToStream(dos);
    //         for(int l=0; l < 10000000; l++) {
    //             bos.write(999999999); // large possible value, MSB is +/- sign
    //             }
    //         InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
    //         DataInputStream dis = new DataInputStream(inputStream);
    //         // k.readHashFromStream(dis);
    //         // dis.reset();
    //         // System.out.println(" dos is now equal to " + dis.readInt());
    //         j.readHashFromStream(dis);
    //         // j.readDataStream(dis, -1);
    //         dis.reset();
    //         // dis.reset();
    //         // j.readDataStream(dis, 11);
    //         assertNotNull(j);
    //         System.out.println(" j is now equal to " + j);
    //     } catch (final IOException e) {
    //         System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
    //         assertTrue(" there should be an IOException with message input too large ",
    //                 e.getMessage().contains(new String("input too large")));
    //     }
    // }
    @Test
    public void testWriteHashStreamData() {
        try {

            MiniData k = new MiniData("0xFFFFF");

            // MiniData k = new MiniData("0xFFFF");
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);
            k.writeHashToStream(dos);

            for (int l = 0; l < 1000; l++) {
                bos.write(68); // large possible value, MSB is +/- sign
            }
            bos.flush();
            //     bos.flush();
            // InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            // DataInputStream dis = new DataInputStream(inputStream);
            // k.readHashFromStream(dis);
            // dis.reset();

            assertNotNull(k);
        } catch (final IOException e) {
            assertTrue(" there should be an IOException with message input too large ",
                    e.getMessage().contains(new String("input too large")));
        }

    }

    @Test
    public void testReadHashFromStream() {

        MiniData j = new MiniData("0xFFFFFFFFFFFFFFFFFFFFF"
                + "FFFFFFFFFFFFFFFFFFFF"
                + "FFFFFFFFFFFFFFFFFFFF"
                + "FFFF"
                + "FFFFFFFFFFFFFFFFFFFF"
                + "FFFFFFFFFFFFFFFFFFFF"
                + "FFFFFFFFFFFFFFFFFFFF"
                + "F");
        MiniData k;

        try {
            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);
            j.writeHashToStream(dos);

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);

            k = MiniData.ReadHashFromStream(dis);
            assertNotNull(k);
        } catch (IOException e) {
            assertFalse("We should not reach this line", true);
        }
    }

    @Test
    public void testReadHashStreamDataLarge() {
        try {

            MiniData j = new MiniData("0xFFFFFFFFFFFFFFFFFFFFF"
                    + "FFFFFFFFFFFFFFFFFFFF"
                    + "FFFFFFFFFFFFFFFFFFFF"
                    + "FFFF"
                    + "FFFFFFFFFFFFFFFFFFFF"
                    + "FFFFFFFFFFFFFFFFFFFF"
                    + "FFFFFFFFFFFFFFFFFFFF"
                    + "F");
            MiniData k = new MiniData("0xFFFFF");

            ByteArrayOutputStream bos = new ByteArrayOutputStream();
            DataOutputStream dos = new DataOutputStream(bos);

            for (int l = 0; l < 1000; l++) {
                bos.write(127); // large possible value, MSB is +/- sign
            }

            InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
            DataInputStream dis = new DataInputStream(inputStream);
            k = j.ReadHashFromStream(dis);
            assertNotNull(k);

            assertFalse("We should not reach this line", true);
        } catch (final IOException e) {
            assertTrue(" there should be an IOException with message HASH Length too large ",
                    e.getMessage().contains(new String(" HASH Length greater then 64")));
        }

    }

    @Test
    public void testgetMiniDataVersion() {
        {
            try {
                MiniData md = MiniData.getRandomData(128);
                MiniData md1 = MiniData.getMiniDataVersion(md);
            } catch (Exception e) {
                fail();
            }
        }
    }
}
