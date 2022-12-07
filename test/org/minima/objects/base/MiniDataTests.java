package org.minima.objects.base;

import org.junit.jupiter.api.Test;

import java.io.*;
import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.*;

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
        assertNotNull(i.getLength(), "should not be null");
        System.out.println("i.getLength()" + i + " resolves as " + i.getLength());
        assertNotNull(j.getLength(), "should not be null");
        System.out.println("i.getLength()" + j + " resolves as " + j.getLength());
        assertNotNull(k.getLength(), "should not be null");
        System.out.println("i.getLength()" + k + " resolves as " + k.getLength());
        assertNotNull(i.getBytes(), "should not be null");
        System.out.println("i.getData()" + i + " resolves as " + i.getBytes());
        assertNotNull(i.getDataValue(), "should not be null");
        System.out.println("i.getDataValue()" + i + " resolves as " + i.getDataValue());
        assertNotNull(i.getDataValueDecimal(), "should not be null");
        System.out.println("i.getDataValueDecimal()" + i + " resolves as " + i.getDataValueDecimal());
        assertNotNull(i.to0xString(), "should not be null");
        System.out.println("i.to0xString()" + i + " resolves as " + i.to0xString());
        assertNotNull(i.toString(), "should not be null");
        System.out.println("i.toString()" + i + " resolves as " + i.toString());
        assertNotNull(mData[0].compare(k), "should not be null");
        System.out.println("mData[0].compare(k) resolves as " + mData[0].compare(k));
        assertNotNull(i.getRandomData(1), "should not be null");
        System.out.println("i.getRandomData(1)" + i + " resolves as " + i.getRandomData(1));
        assertNotNull(i.shiftr(64), "should not be null");
        System.out.println("i.shiftr(64) i:" + i + " resolves as " + i.shiftr(64));
        assertNotNull(i.shiftl(64), "should not be null");
        System.out.println("i.shiftl(64) i:" + i + " resolves as " + i.shiftl(64));
        assertNotNull(i.concat(j), "should not be null");
        System.out.println("i.concat(j) i:" + i + " j:" + j + " resolves as " + i.concat(j));
        assertNotNull(i.to0xString(1), "should not be null");
        System.out.println("i.to0xString(1)" + i + " resolves as " + i.to0xString(1));
        assertNotNull(i.to0xString(10000), "should not be null");
        System.out.println("i.to0xString(10000)" + i + " resolves as " + i.to0xString(10000));
        assertFalse(i.equals(j), "should be equal to false");
        System.out.println("i.equals(j) i:" + i + " j:" + j + " resolves as " + i.equals(j));
        assertTrue(j.equals(j), "should be equal to true");
        System.out.println("j.equals(j) j:" + j + " j:" + j + " resolves as " + j.equals(j));
        assertFalse(i.isEqual(j), "should be equal to false");
        System.out.println("i.isEqual(j)) i:" + i + " j:" + j + " resolves as " + i.isEqual(j));
        assertFalse(mData[1].isEqual(mDataCompare[1]), "should be equal to false");
        System.out.println("mData[1].isEqual(mDataCompare[1]) mData[1]:" + mData[1] + " mDataCompare[1]:"
                + mDataCompare[1] + " resolves as " + mData[1].isEqual(mDataCompare[1]));
        assertTrue(i.isLess(j), "should be equal to true");
        System.out.println("i.isLess(j) i:" + i + " j:" + j + " resolves as " + i.isLess(j));
        assertFalse(j.isLess(i), "should be equal to false");
        System.out.println("j.isLess(i) i:" + i + " j:" + j + " resolves as " + j.isLess(i));
        assertFalse(j.isLessEqual(i), "should be less that or equal with value false");
        System.out.println("j.isLessEqual(i) i:" + i + " j:" + j + " resolves as " + j.isLessEqual(i));
        assertTrue(j.isLessEqual(j), "should be less that or equal with value true");
        System.out.println("j.isLess(i) i:" + i + " j:" + j + " resolves as " + j.isLess(i));
        assertTrue(j.isMore(i), "should be more than with value true");
        System.out.println("j.isMore(i) i:" + i + " j:" + j + " resolves as " + j.isMore(i));
        assertFalse(i.isMore(j), "should be more than with value false");
        System.out.println("i.isMore(j) i:" + i + " j:" + j + " resolves as " + i.isMore(j));
        assertFalse(i.isMoreEqual(j), "should be more that or equal with value false");
        System.out.println("i.isMoreEqual(j) i:" + i + " j:" + j + " resolves as " + i.isMoreEqual(j));
        assertTrue(j.isMoreEqual(j), "should be more that or equal with value true");
        System.out.println("j.isMoreEqual(j) j:" + j + " j:" + j + " resolves as " + j.isMoreEqual(j));
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
            assertTrue(j.toString().compareToIgnoreCase(value) == 0, "j matches original value");
            System.out.println(" j is now equal to " + j);

            bos.reset();

            j.readDataStream(dis, 27);

        } catch (final IOException e) {
            System.out.println("IOException: " + e.toString() + " msg=" + e.getMessage());
            assertTrue(true, "test");
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
            assertTrue(e.getMessage().contains(new String("input too large")), " there should be an IOException with message input too large ");
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
            assertTrue(e.getMessage().contains(new String("input too large")), " there should be an IOException with message input too large ");
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
            assertFalse(true, "We should not reach this line");
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

            assertFalse(true, "We should not reach this line");
        } catch (final IOException e) {
            assertTrue(e.getMessage().contains(new String(" HASH Length greater than 64")), " there should be an IOException with message HASH Length too large ");
        }

    }

}
