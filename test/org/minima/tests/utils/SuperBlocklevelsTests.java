package org.minima.tests.utils;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.minima.objects.base.MiniData;
import org.minima.utils.SuperBlockLevels;

public class SuperBlocklevelsTests {

    @Test
    public void testSuperBlockLevels() {
        SuperBlockLevels sp = new SuperBlockLevels();
        assertNotNull("should not be null", sp);
        System.out.println("New SuperBlock Object " + sp);
        MiniData genHash = SuperBlockLevels.GENESIS_HASH;
        assertNotNull("should not be null", genHash);
        System.out.println("genesis hash value " + genHash);

        MiniData diff = new MiniData("0xFFFF");
        MiniData act = new MiniData("0x7");

        int sup = SuperBlockLevels.getSuperLevel(diff, act);
        assertTrue("should equal 13 ", SuperBlockLevels.getSuperLevel(diff, act) == (13));
        System.out.println(diff + " " + act + " " + sup);

    }

    @Test
    public void testSuperBlockLevelsTwo() {

        MiniData diff = new MiniData("0xFFFFFFFFFFFFFFFFFFFFF");
        MiniData act = new MiniData("0x7F7F");

        int sup = SuperBlockLevels.getSuperLevel(diff, act);
        assertTrue("should equal 20 ", SuperBlockLevels.getSuperLevel(diff, act) == (31));
        System.out.println(diff + " " + act + " " + sup);

    }

    @Test
    public void testSuperBlockLevelsThree() {
        MiniData genHash = SuperBlockLevels.GENESIS_HASH;
        assertNotNull("should not be null", genHash);
        System.out.println("genesis hash value " + genHash);

        MiniData diff = new MiniData("0x0");
        MiniData act = new MiniData("0x7F7F");

        int sup = SuperBlockLevels.getSuperLevel(diff, act);
        assertTrue("should equal 0 ", SuperBlockLevels.getSuperLevel(diff, act) == (0));
        System.out.println(diff + " " + act + " " + sup);

    }

}
