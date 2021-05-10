package org.minima.tests.objects;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import org.junit.Assert;

import org.junit.Test;
import org.junit.internal.ArrayComparisonFailure;
import org.minima.objects.Address;
import org.minima.objects.base.MiniData;
import org.minima.utils.json.JSONObject;

public class AddressTests {

    @Test
    public void testConstructors() {
        {
            Address addr = Address.TRUE_ADDRESS;
            assertNotNull(addr);
            assertEquals("RETURN TRUE", addr.getScript());
        }
        {
            Address addr = new Address();
            assertNotNull(addr);
            //assertNull(addr.getScript()); // default constructor does not initialize MiniString
        }
        {
            Address addr = new Address("RETURN TRUE");
            assertNotNull(addr);
            assertEquals("RETURN TRUE", addr.getScript());
        }
        {
            MiniData md = new MiniData();
            Address addr = new Address(md);
            assertNotNull(addr);
            assertEquals("", addr.getScript());
        }
        {
            for (int i = 16; i <= 68; i = i + 4) {
                if (i == 44) {
                    continue;
                }
                MiniData md1 = MiniData.getRandomData(i);
                Address addr1 = new Address(md1);
                assertNotNull(addr1);
                assertEquals("", addr1.getScript());
                assertEquals(md1, addr1.getAddressData());
                assertEquals(Address.makeMinimaAddress(md1), addr1.getMinimaAddress());

                MiniData md2 = MiniData.getRandomData(i);
                Address addr2 = new Address(md2);
                assertNotNull(addr2);
                assertEquals("", addr2.getScript());
                assertEquals(md2, addr2.getAddressData());
                assertEquals(Address.makeMinimaAddress(md2), addr2.getMinimaAddress());

                assertTrue(addr1.isEqual(addr1));
                assertFalse(addr1.isEqual(addr2));

            }
        }
    }

    @Test
    public void testWriteAndReadDataStream() {
        try {
            for (int i = 16; i <= 64; i = i + 4) {
                if (i == 44) {
                    continue;
                }
                MiniData md1 = MiniData.getRandomData(i);
                Address addr1 = new Address(md1);
                assertNotNull(addr1);
                assertEquals("", addr1.getScript());
                assertEquals(md1, addr1.getAddressData());
                assertEquals(Address.makeMinimaAddress(md1), addr1.getMinimaAddress());

                ByteArrayOutputStream bos = new ByteArrayOutputStream();
                DataOutputStream dos = new DataOutputStream(bos);

                addr1.writeDataStream(dos);

                InputStream inputStream = new ByteArrayInputStream(bos.toByteArray());
                DataInputStream dis = new DataInputStream(inputStream);

                Address addr2 = new Address();
                addr2.readDataStream(dis);

                assertTrue(addr1.isEqual(addr2));
            }
        } catch (final IOException e) {
            fail();
        }
    }

    @Test
    public void testJSONConversion() {
        for (int i = 16; i <= 68; i = i + 4) {
            if (i == 44) {
                continue;
            }
            MiniData md1 = MiniData.getRandomData(i);
            Address addr1 = new Address(md1);
            assertNotNull(addr1);
            assertEquals("", addr1.getScript());
            assertEquals(md1, addr1.getAddressData());
            assertEquals(Address.makeMinimaAddress(md1), addr1.getMinimaAddress());

            JSONObject json = addr1.toJSON();

            assertTrue("JSON object should contain script key", json.containsKey("script"));
            assertTrue("JSON object should contain hexaddress key", json.containsKey("hexaddress"));
            assertTrue("JSON object should contain miniaddress key", json.containsKey("miniaddress"));
        }
    }

    @Test
    public void testToString() {
        for (int i = 16; i <= 68; i = i + 4) {
            if (i == 44) {
                continue;
            }
            MiniData md1 = MiniData.getRandomData(i);
            Address addr1 = new Address(md1);
            assertNotNull(addr1);
            assertEquals("", addr1.getScript());
            assertEquals(md1, addr1.getAddressData());
            assertEquals(Address.makeMinimaAddress(md1), addr1.getMinimaAddress());

            String exp_s = addr1.getAddressData().toString();
            String obj_s = addr1.toString();
            assertEquals("should be equal ", exp_s, obj_s);
        }
    }

    @Test
    public void testconvertMinimaAddress() {
        {
            for (int i = 16; i <= 68; i = i + 4) {
                if (i == 44) {
                    continue;
                }
                MiniData md1 = MiniData.getRandomData(i);
                Address addr1 = new Address(md1);
                assertNotNull(addr1);
                assertEquals("", addr1.getScript());
                assertEquals(md1, addr1.getAddressData());
                assertEquals(Address.makeMinimaAddress(md1), addr1.getMinimaAddress());

                if (i == 16) {
                    // makeMinimaAddress does not create mx address for length less than 20
                    assertThrows(ArithmeticException.class, () -> {
                        Address.convertMinimaAddress(addr1.getMinimaAddress());
                    });
                    continue;
                }
                if (i == 68) {
                    // makeMinimaAddress does not create mx address for length greater than 64
                    assertThrows(ArithmeticException.class, () -> {
                        Address.convertMinimaAddress(addr1.getMinimaAddress());
                    });
                    continue;
                }

                MiniData md2 = Address.convertMinimaAddress(addr1.getMinimaAddress());
                assertEquals(md1, md2);
            }
        }
    }
}
