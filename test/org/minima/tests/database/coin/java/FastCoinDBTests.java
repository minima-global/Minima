package org.minima.tests.database.coin.java;

import java.util.ArrayList;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.junit.Test;

import org.minima.database.coindb.CoinDBRow;
import org.minima.database.coindb.java.FastCoinDB;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.PubPrivKey;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public class FastCoinDBTests {

    @Test
    public void testConstructors() {
        FastCoinDB fcdb = new FastCoinDB();
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

        FastCoinDB fcdb = new FastCoinDB();

        ArrayList<CoinDBRow> cdbr = fcdb.getComplete();
        assertEquals("should be equal ", 0, cdbr.size());

        fcdb.addCoinRow(c0);
        fcdb.addCoinRow(c0);
        fcdb.addCoinRow(c1);
        fcdb.addCoinRow(c2);
        fcdb.addCoinRow(c3);
        fcdb.addCoinRow(c4);
        fcdb.addCoinRow(c5);
        fcdb.addCoinRow(c6);
        fcdb.addCoinRow(c7);
        fcdb.addCoinRow(c8);

        //assertTrue("should be true ", fcdb.removeCoin(c0.getCoinID()));
        assertFalse("should be false ", fcdb.removeCoin(c0.getCoinID()));

        fcdb.getCoinRow(c1.getCoinID()).setInBlockNumber(MiniNumber.TWOFIVESIX);
        fcdb.getCoinRow(c1.getCoinID()).setRelevant(true);
        fcdb.getCoinRow(c1.getCoinID()).setKeeper(true);
        fcdb.getCoinRow(c1.getCoinID()).setIsSpent(false);

        fcdb.getCoinRow(c2.getCoinID()).setInBlockNumber(MiniNumber.TWOFIVESIX);
        fcdb.getCoinRow(c2.getCoinID()).setRelevant(true);
        fcdb.getCoinRow(c2.getCoinID()).setKeeper(false);
        fcdb.getCoinRow(c2.getCoinID()).setIsSpent(false);

        fcdb.getCoinRow(c3.getCoinID()).setInBlockNumber(MiniNumber.TWOFIVESIX);
        fcdb.getCoinRow(c3.getCoinID()).setRelevant(false);
        fcdb.getCoinRow(c3.getCoinID()).setKeeper(true);
        fcdb.getCoinRow(c3.getCoinID()).setIsSpent(false);

        fcdb.getCoinRow(c4.getCoinID()).setInBlockNumber(MiniNumber.TWOFIVESIX);
        fcdb.getCoinRow(c4.getCoinID()).setRelevant(false);
        fcdb.getCoinRow(c4.getCoinID()).setKeeper(false);
        fcdb.getCoinRow(c4.getCoinID()).setIsSpent(false);

        fcdb.getCoinRow(c5.getCoinID()).setInBlockNumber(MiniNumber.TWOFIVESIX);
        fcdb.getCoinRow(c5.getCoinID()).setRelevant(true);
        fcdb.getCoinRow(c5.getCoinID()).setKeeper(true);
        fcdb.getCoinRow(c5.getCoinID()).setIsSpent(true);

        fcdb.getCoinRow(c6.getCoinID()).setInBlockNumber(MiniNumber.TWOFIVESIX);
        fcdb.getCoinRow(c6.getCoinID()).setRelevant(true);
        fcdb.getCoinRow(c6.getCoinID()).setKeeper(false);
        fcdb.getCoinRow(c6.getCoinID()).setIsSpent(true);

        fcdb.getCoinRow(c7.getCoinID()).setInBlockNumber(MiniNumber.TWOFIVESIX);
        fcdb.getCoinRow(c7.getCoinID()).setRelevant(false);
        fcdb.getCoinRow(c7.getCoinID()).setKeeper(true);
        fcdb.getCoinRow(c7.getCoinID()).setIsSpent(true);

        fcdb.getCoinRow(c8.getCoinID()).setInBlockNumber(MiniNumber.TWOFIVESIX);
        fcdb.getCoinRow(c8.getCoinID()).setRelevant(false);
        fcdb.getCoinRow(c8.getCoinID()).setKeeper(false);
        fcdb.getCoinRow(c8.getCoinID()).setIsSpent(true);

        ArrayList<CoinDBRow> cdbrr = fcdb.getCompleteRelevant();
        assertEquals("should be equal ", 6, cdbrr.size());

        fcdb.removeOldSpentCoins(MiniNumber.TWOFIVESIX);
        ArrayList<CoinDBRow> cdbrr1 = fcdb.getComplete();
        assertEquals("should be equal ", 8, cdbrr1.size());

        fcdb.getCoinRow(c5.getCoinID()).setInBlockNumber(MiniNumber.HUNDRED);
        fcdb.getCoinRow(c6.getCoinID()).setInBlockNumber(MiniNumber.HUNDRED);
        fcdb.getCoinRow(c7.getCoinID()).setInBlockNumber(MiniNumber.HUNDRED);
        fcdb.getCoinRow(c8.getCoinID()).setInBlockNumber(MiniNumber.HUNDRED);
        fcdb.removeOldSpentCoins(MiniNumber.TWOFIVESIX);
        ArrayList<CoinDBRow> cdbrr2 = fcdb.getComplete();
        assertEquals("should be equal ", 4, cdbrr2.size());

        fcdb.getCoinRow(c1.getCoinID()).setInBlockNumber(MiniNumber.HUNDRED);
        fcdb.getCoinRow(c2.getCoinID()).setInBlockNumber(MiniNumber.HUNDRED);
        fcdb.resetCoinsFomOnwards(MiniNumber.TWOFIVESIX);
        ArrayList<CoinDBRow> cdbrr3 = fcdb.getComplete();
        assertEquals("should be equal ", 2, cdbrr3.size());
    }
}
