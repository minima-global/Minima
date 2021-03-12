package org.minima.tests.database;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.util.ArrayList;
import java.util.Hashtable;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.junit.Test;
import org.minima.database.MinimaDB;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.Transaction;
import org.minima.objects.TxPoW;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.brains.BackupManager;
import org.minima.system.input.functions.gimme50;
import org.minima.system.network.base.MinimaReader;
import org.minima.system.txpow.TxPoWChecker;
import org.minima.utils.Crypto;
import org.minima.utils.json.JSONArray;

public class MinimaDBTests {

    @Test
    public void testConstructors() {
        MinimaDB mdb = new MinimaDB();

//        assertEquals(0, mdb.getCoinDB().getComplete().size());
//        assertEquals(0, mdb.getCoinDB().getCompleteRelevant().size());
        
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

//        assertEquals(0, mdb.getCoinDB().getComplete().size());
//        assertEquals(0, mdb.getCoinDB().getCompleteRelevant().size());
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
        txp.setNonce(MiniNumber.TWO);
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
        MiniNumber nonce = new MiniNumber(0);
        while (true) {
            txp1.setNonce(nonce);
            txp1.setTimeMilli(new MiniNumber(System.currentTimeMillis()));
            MiniData hash = Crypto.getInstance().hashObject(txp1.getTxHeader());
            if (hash.isLess(txp1.getTxnDifficulty())) {
                break;
            }
            nonce = nonce.increment();
        }
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
//        assertEquals(3, mdb.getCoinDB().getComplete().size());
//        assertEquals(2, mdb.getCoinDB().getCompleteRelevant().size());
        assertEquals(3, mdb.getMMRTip().searchAllCoins().size());
        assertEquals(2, mdb.getMMRTip().searchAllRelevantCoins().size());
        
        assertEquals(2, mdb.getMainTree().getAsList().size());
        assertEquals(0, mdb.getMempoolCoins().size());
        assertNotNull(mdb.getMainTree().getChainTip());
        assertEquals(1, mdb.getTopBlock().getAsInt());
        assertEquals(0, mdb.getTotalSimpleSpendableCoins(Coin.MINIMA_TOKENID).size());
        assertEquals(0, mdb.getTotalUnusedAmount().size());
        Hashtable<String, MiniNumber> amounts = mdb.getTransactionTokenAmounts(mdb.getTopTxPoW());
        assertEquals(1, amounts.size());
        assertEquals(50, amounts.get(Coin.MINIMA_TOKENID.to0xString()).getAsInt());
        assertEquals(2, mdb.getTxPowDB().getSize());
        assertEquals(0, mdb.getUserDB().getAllRows().size());

        TxPoW blocktx = mineblock(mdb);
        mdb.addNewTxPow(blocktx);
        mdb.processTxPOW(blocktx);
        blocktx = mineblock(mdb);
        mdb.addNewTxPow(blocktx);
        mdb.processTxPOW(blocktx);
        blocktx = mineblock(mdb);
        mdb.addNewTxPow(blocktx);
        mdb.processTxPOW(blocktx);

        assertEquals(3, mdb.getMMRTip().searchAllCoins().size());
        assertEquals(2, mdb.getMMRTip().searchAllRelevantCoins().size());
//        assertEquals(3, mdb.getCoinDB().getComplete().size());
//        assertEquals(2, mdb.getCoinDB().getCompleteRelevant().size());
        assertEquals(5, mdb.getMainTree().getAsList().size());
        assertEquals(0, mdb.getMempoolCoins().size());
        assertNotNull(mdb.getMainTree().getChainTip());
        assertEquals(4, mdb.getTopBlock().getAsInt());
        ArrayList<Coin> coins = mdb.getTotalSimpleSpendableCoins(Coin.MINIMA_TOKENID);
        assertEquals(2, coins.size());
        assertEquals(25, coins.get(0).getAmount().getAsInt());
        assertEquals(Coin.MINIMA_TOKENID, coins.get(0).getTokenID());
        assertEquals(25, coins.get(1).getAmount().getAsInt());
        assertEquals(Coin.MINIMA_TOKENID, coins.get(1).getTokenID());
        assertEquals(0, mdb.getTotalUnusedAmount().size());
        assertEquals(5, mdb.getTxPowDB().getSize());
        assertEquals(0, mdb.getUserDB().getAllRows().size());
    }

    private TxPoW mineblock(MinimaDB mdb) {
        TxPoW txpow = mdb.getCurrentTxPow(new Transaction(), new Witness(), new JSONArray());
        txpow.setHeaderBodyHash();
        MiniNumber nonce = new MiniNumber(0);
        while (true) {
            txpow.setNonce(nonce);
            txpow.setTimeMilli(new MiniNumber(System.currentTimeMillis()));
            MiniData hash = Crypto.getInstance().hashObject(txpow.getTxHeader());
            if (hash.isLess(txpow.getTxnDifficulty())) {
                break;
            }
            nonce = nonce.increment();
        }
        txpow.calculateTXPOWID();
        return txpow;
    }
}
