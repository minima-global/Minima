package org.minima.tests.database;

import java.math.BigInteger;
import java.util.Hashtable;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.database.MinimaDB;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniInteger;
import org.minima.objects.base.MiniNumber;
import org.minima.system.brains.BackupManager;
import org.minima.system.brains.ConsensusHandler;
import org.minima.system.input.InputHandler;
import org.minima.system.input.functions.gimme50;
import org.minima.system.network.MinimaReader;
import org.minima.system.txpow.TxPoWChecker;
import org.minima.utils.json.JSONArray;
import org.minima.utils.messages.Message;

public class MinimaDBTests {

    @Test
    public void testConstructors() {
        MinimaDB mdb = new MinimaDB();

        assertEquals(0, mdb.getCoinDB().getComplete().size());
        assertEquals(0, mdb.getCoinDB().getCompleteRelevant().size());
        assertEquals(0, mdb.getMainTree().getAsList().size());
        assertEquals(0, mdb.getMempoolCoins().size());
        assertNull(mdb.getMainTree().getChainTip());
        assertEquals(0, mdb.getTotalSimpleSpendableCoins(new MiniData("0x00")).size());
        assertEquals(0, mdb.getTotalUnusedAmount().size());
        assertEquals(0, mdb.getTxPowDB().getSize());
        assertEquals(0, mdb.getUserDB().getAllRows().size());
    }

    @Test
    public void testGenesis() {
        BackupManager bm = new BackupManager("/tmp/minima");
        MinimaDB mdb = new MinimaDB();
        mdb.setBackupManager(bm);

        mdb.DoGenesis();

        assertEquals(0, mdb.getCoinDB().getComplete().size());
        assertEquals(0, mdb.getCoinDB().getCompleteRelevant().size());
        assertEquals(1, mdb.getMainTree().getAsList().size()); // single peak
        assertEquals(0, mdb.getMempoolCoins().size());
        assertNotNull(mdb.getMainTree().getChainTip());
        assertEquals(0, mdb.getTopBlock().getAsInt());
        assertEquals(0, mdb.getTotalSimpleSpendableCoins(new MiniData("0x00")).size()); // single peak
        assertEquals(0, mdb.getTotalUnusedAmount().size());
        assertEquals(0, mdb.getTransactionTokenAmounts(mdb.getTopTxPoW()).size());
        assertEquals(1, mdb.getTxPowDB().getSize()); // single transaction
        assertEquals(0, mdb.getUserDB().getAllRows().size());

        TxPoW txp = new TxPoW();
        txp.setNonce(MiniInteger.TWO);
        txp.calculateTXPOWID();
        assertNull(mdb.getTxPOW(txp.getTxPowID()));
        assertNotNull(mdb.getTxPOW(mdb.getTopTxPoW().getTxPowID()));
        assertNull(mdb.getTxPOWRow(txp.getTxPowID()));
        assertNotNull(mdb.getTxPOWRow(mdb.getTopTxPoW().getTxPowID()));
    }

    @Test
    public void testGimme50() {
        BackupManager bm = new BackupManager("/tmp/minima");
        MinimaDB mdb = new MinimaDB();
        mdb.setBackupManager(bm);

        mdb.DoGenesis();

        // CONSENSUS_GIMME50 begin
        Address addr1 = mdb.getUserDB().newSimpleAddress();
        Address addr2 = mdb.getUserDB().newSimpleAddress();

        Transaction trx = new Transaction();
        Witness w = new Witness();

        Coin in = new Coin(gimme50.COINID_INPUT, Address.TRUE_ADDRESS.getAddressData(), new MiniNumber("50"), Coin.MINIMA_TOKENID);
        trx.addInput(in);

        try {
            w.addScript(Address.TRUE_ADDRESS.getScript(), in.getAddress().getLength() * 8);
        } catch (Exception ex) {
            Logger.getLogger(MinimaDBTests.class.getName()).log(Level.SEVERE, null, ex);
        }

        Coin out1 = new Coin(Coin.COINID_OUTPUT, addr1.getAddressData(), new MiniNumber("25"), Coin.MINIMA_TOKENID);
        Coin out2 = new Coin(Coin.COINID_OUTPUT, addr2.getAddressData(), new MiniNumber("25"), Coin.MINIMA_TOKENID);

        trx.addOutput(out1);
        trx.addOutput(out2);
        // CONSENSUS_GIMME50 end

        // CONSENSUS_SENDTRANS begin
        TxPoW txp1 = mdb.getCurrentTxPow(trx, w, new JSONArray());
        txp1.setHeaderBodyHash();
        txp1.calculateTXPOWID();

        assertTrue(TxPoWChecker.checkSigs(txp1));
        assertTrue(TxPoWChecker.checkTransactionMMR(txp1, mdb));

        mdb.addMiningTransaction(txp1.getTransaction());
        // CONSENSUS_SENDTRANS end

        // TXMINER_MINETXPOW begin
        txp1.setHeaderBodyHash();
        txp1.setNonce(new MiniInteger(33));
        txp1.setTimeMilli(new MiniNumber(System.currentTimeMillis()));
        txp1.calculateTXPOWID();
        // TXMINER_MINETXPOW end

        // CONSENSUS_FINISHED_MINE begin
        mdb.remeoveMiningTransaction(txp1.getTransaction());
        // CONSENSUS_FINISHED_MINE end

        // CONSENSUS_NET_CHECKSIZE_TXPOW begin
        assertTrue(txp1.getSizeinBytes() <= MinimaReader.MAX_TXPOW);
        // CONSENSUS_NET_CHECKSIZE_TXPOW end

        // CONSENSUS_NET_TXPOW begin
        assertTrue(TxPoWChecker.checkSigs(txp1));
        assertTrue(TxPoWChecker.checkTransactionMMR(txp1, mdb));
        mdb.addNewTxPow(txp1);
        // CONSENSUS_NET_TXPOW end

        // CONSENSUS_PROCESSTXPOW begin
        TxPoW prev_tip = mdb.getMainTree().getChainTip().getTxPow();
        mdb.processTxPOW(txp1);
        TxPoW next_tip = mdb.getMainTree().getChainTip().getTxPow();

        //boolean relevant = mdb.getUserDB().isTransactionRelevant(txp1.getTransaction());
        //if (relevant) {
            Hashtable<String, MiniNumber> tta = mdb.getTransactionTokenAmounts(txp1);
            mdb.getUserDB().addToHistory(txp1, tta);
        //}
        // CONSENSUS_PROCESSTXPOW end

        // Checks
        assertEquals(0, mdb.getCoinDB().getComplete().size()); // expecting 2
        assertEquals(0, mdb.getCoinDB().getCompleteRelevant().size()); // expecting 2
        assertEquals(1, mdb.getMainTree().getAsList().size()); // single peak
        assertEquals(0, mdb.getMempoolCoins().size());
        assertNotNull(mdb.getMainTree().getChainTip());
        assertEquals(0, mdb.getTopBlock().getAsInt());
        assertEquals(0, mdb.getTotalSimpleSpendableCoins(new MiniData("0x00")).size()); // expecting 2 or 50
        assertEquals(0, mdb.getTotalUnusedAmount().size()); // expecting 50
        assertEquals(0, mdb.getTransactionTokenAmounts(mdb.getTopTxPoW()).size()); // expecting 50
        assertEquals(2, mdb.getTxPowDB().getSize());
        assertEquals(0, mdb.getUserDB().getAllRows().size()); // expecting 2, for two addresses created

    }
}
