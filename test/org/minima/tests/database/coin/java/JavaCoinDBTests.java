package org.minima.tests.database.coin.java;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;

import org.junit.Test;
import org.minima.database.coindb.CoinDBRow;
import org.minima.database.coindb.java.JavaCoinDB;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.PubPrivKey;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public class JavaCoinDBTests {

    @Test
    public void testConstructors() {
        JavaCoinDB jcdb = new JavaCoinDB();
    }

    @Test
    public void testGettersAndSetters() {
        PubPrivKey pk = new PubPrivKey(512);
        String script = "RETURN SIGNEDBY ( " + pk.getPublicKey() + " )";
        Address addr = new Address(script, pk.getBitLength());
        Coin c0 = new Coin(new MiniData("0x00"), addr.getAddressData(), MiniNumber.ZERO, new MiniData("0x00"));
        Coin c1 = new Coin(new MiniData("0x01"), addr.getAddressData(), MiniNumber.ONE, new MiniData("0x01"));
        Coin c2 = new Coin(new MiniData("0x02"), addr.getAddressData(), MiniNumber.TWO, new MiniData("0x02"));
        Coin c3 = new Coin(new MiniData("0x03"), addr.getAddressData(), MiniNumber.EIGHT, new MiniData("0x03"));
        Coin c4 = new Coin(new MiniData("0x04"), addr.getAddressData(), MiniNumber.SIXTEEN, new MiniData("0x04"));
        Coin c5 = new Coin(new MiniData("0x05"), addr.getAddressData(), MiniNumber.THIRTYTWO, new MiniData("0x05"));
        Coin c6 = new Coin(new MiniData("0x06"), addr.getAddressData(), MiniNumber.SIXTYFOUR, new MiniData("0x06"));
        Coin c7 = new Coin(new MiniData("0x07"), addr.getAddressData(), MiniNumber.HUNDRED, new MiniData("0x07"));
        Coin c8 = new Coin(new MiniData("0x08"), addr.getAddressData(), MiniNumber.THOUSAND, new MiniData("0x08"));
        Coin c9 = new Coin(new MiniData("0x09"), addr.getAddressData(), MiniNumber.MILLION, new MiniData("0x09"));

        JavaCoinDB jcdb = new JavaCoinDB();

        ArrayList<CoinDBRow> cdbr = jcdb.getComplete();
        assertEquals("should be equal ", 0, cdbr.size());

        jcdb.addCoinRow(c0);
        jcdb.addCoinRow(c0);
        jcdb.addCoinRow(c1);
        jcdb.addCoinRow(c2);
        jcdb.addCoinRow(c3);
        jcdb.addCoinRow(c4);
        jcdb.addCoinRow(c5);
        jcdb.addCoinRow(c6);
        jcdb.addCoinRow(c7);
        jcdb.addCoinRow(c8);

        assertTrue("should be true ", jcdb.removeCoin(c0.getCoinID()));
        assertFalse("should be false ", jcdb.removeCoin(c0.getCoinID()));

        jcdb.getCoinRow(c1.getCoinID()).setInBlockNumber(MiniNumber.TWOFIVESIX);
        jcdb.getCoinRow(c1.getCoinID()).setRelevant(true);
        jcdb.getCoinRow(c1.getCoinID()).setKeeper(true);
        jcdb.getCoinRow(c1.getCoinID()).setIsSpent(false);

        jcdb.getCoinRow(c2.getCoinID()).setInBlockNumber(MiniNumber.TWOFIVESIX);
        jcdb.getCoinRow(c2.getCoinID()).setRelevant(true);
        jcdb.getCoinRow(c2.getCoinID()).setKeeper(false);
        jcdb.getCoinRow(c2.getCoinID()).setIsSpent(false);

        jcdb.getCoinRow(c3.getCoinID()).setInBlockNumber(MiniNumber.TWOFIVESIX);
        jcdb.getCoinRow(c3.getCoinID()).setRelevant(false);
        jcdb.getCoinRow(c3.getCoinID()).setKeeper(true);
        jcdb.getCoinRow(c3.getCoinID()).setIsSpent(false);

        jcdb.getCoinRow(c4.getCoinID()).setInBlockNumber(MiniNumber.TWOFIVESIX);
        jcdb.getCoinRow(c4.getCoinID()).setRelevant(false);
        jcdb.getCoinRow(c4.getCoinID()).setKeeper(false);
        jcdb.getCoinRow(c4.getCoinID()).setIsSpent(false);

        jcdb.getCoinRow(c5.getCoinID()).setInBlockNumber(MiniNumber.TWOFIVESIX);
        jcdb.getCoinRow(c5.getCoinID()).setRelevant(true);
        jcdb.getCoinRow(c5.getCoinID()).setKeeper(true);
        jcdb.getCoinRow(c5.getCoinID()).setIsSpent(true);

        jcdb.getCoinRow(c6.getCoinID()).setInBlockNumber(MiniNumber.TWOFIVESIX);
        jcdb.getCoinRow(c6.getCoinID()).setRelevant(true);
        jcdb.getCoinRow(c6.getCoinID()).setKeeper(false);
        jcdb.getCoinRow(c6.getCoinID()).setIsSpent(true);

        jcdb.getCoinRow(c7.getCoinID()).setInBlockNumber(MiniNumber.TWOFIVESIX);
        jcdb.getCoinRow(c7.getCoinID()).setRelevant(false);
        jcdb.getCoinRow(c7.getCoinID()).setKeeper(true);
        jcdb.getCoinRow(c7.getCoinID()).setIsSpent(true);

        jcdb.getCoinRow(c8.getCoinID()).setInBlockNumber(MiniNumber.TWOFIVESIX);
        jcdb.getCoinRow(c8.getCoinID()).setRelevant(false);
        jcdb.getCoinRow(c8.getCoinID()).setKeeper(false);
        jcdb.getCoinRow(c8.getCoinID()).setIsSpent(true);

        ArrayList<CoinDBRow> cdbrr = jcdb.getCompleteRelevant();
        assertEquals("should be equal ", 6, cdbrr.size());

        jcdb.removeOldSpentCoins(MiniNumber.TWOFIVESIX);
        ArrayList<CoinDBRow> cdbrr1 = jcdb.getComplete();
        assertEquals("should be equal ", 8, cdbrr1.size());

        jcdb.getCoinRow(c5.getCoinID()).setInBlockNumber(MiniNumber.HUNDRED);
        jcdb.getCoinRow(c6.getCoinID()).setInBlockNumber(MiniNumber.HUNDRED);
        jcdb.getCoinRow(c7.getCoinID()).setInBlockNumber(MiniNumber.HUNDRED);
        jcdb.getCoinRow(c8.getCoinID()).setInBlockNumber(MiniNumber.HUNDRED);
        jcdb.removeOldSpentCoins(MiniNumber.TWOFIVESIX);
        ArrayList<CoinDBRow> cdbrr2 = jcdb.getComplete();
        assertEquals("should be equal ", 4, cdbrr2.size());

        jcdb.getCoinRow(c1.getCoinID()).setInBlockNumber(MiniNumber.HUNDRED);
        jcdb.getCoinRow(c2.getCoinID()).setInBlockNumber(MiniNumber.HUNDRED);
        jcdb.resetCoinsFomOnwards(MiniNumber.TWOFIVESIX);
        ArrayList<CoinDBRow> cdbrr3 = jcdb.getComplete();
        assertEquals("should be equal ", 2, cdbrr3.size());
    }
}
