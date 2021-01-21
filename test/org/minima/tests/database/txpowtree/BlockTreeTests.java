package org.minima.tests.database.txpowtree;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import org.junit.Test;
import org.minima.database.txpowtree.BlockTree;
import org.minima.database.txpowtree.BlockTreeNode;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public class BlockTreeTests {

    @Test
    public void testConstructors() {
        BlockTree bt = new BlockTree();

        assertNull("should be null ", bt.getChainRoot());
        assertNull("should be null ", bt.getChainTip());
        assertNull("should be null ", bt.getCascadeNode());

        TxPoW txp_child_untracked = BlockTree.createRandomTxPow();
        //BlockTreeNode btn_child_untracked = new BlockTreeNode(txp_child_untracked);

        BlockTreeNode btn_found;

        btn_found = null;
        btn_found = bt.findNode(txp_child_untracked.getTxPowID(), true);
        assertNull("should be null", btn_found);

        assertEquals("should be equal ", 0, bt.getAsList().size());
        assertEquals("should be equal ", 0, bt.getAsList(true).size());
        //assertEquals("should be equal ", 0, bt.getAvgChainDifficulty());
    }

    @Test
    public void testNodeOperations() {
        BlockTree bt = new BlockTree();

        TxPoW txp_root = BlockTree.createRandomTxPow();
        txp_root.setBlockNumber(new MiniNumber(1));
        txp_root.setBlockDifficulty(new MiniData("0A"));
        txp_root.setTimeSecs(new MiniNumber(1));
        BlockTreeNode btn_root = new BlockTreeNode(txp_root);
        bt.setTreeRoot(btn_root);

        TxPoW txp_child_1 = BlockTree.createRandomTxPow();
        txp_child_1.setBlockNumber(new MiniNumber(2));
        txp_child_1.setBlockDifficulty(new MiniData("0A"));
        txp_child_1.setTimeSecs(new MiniNumber(2));
        BlockTreeNode btn_child_1 = new BlockTreeNode(txp_child_1);
        btn_root.addChild(btn_child_1);

        TxPoW txp_child_2 = BlockTree.createRandomTxPow();
        txp_child_2.setBlockNumber(new MiniNumber(3));
        txp_child_2.setBlockDifficulty(new MiniData("0A"));
        txp_child_2.setTimeSecs(new MiniNumber(3));
        BlockTreeNode btn_child_2 = new BlockTreeNode(txp_child_2);
        btn_root.addChild(btn_child_2);

        TxPoW txp_child_1_1 = BlockTree.createRandomTxPow();
        txp_child_1_1.setBlockNumber(new MiniNumber(4));
        txp_child_1_1.setBlockDifficulty(new MiniData("0A"));
        txp_child_1_1.setTimeSecs(new MiniNumber(4));
        BlockTreeNode btn_child_1_1 = new BlockTreeNode(txp_child_1_1);
        btn_child_1.addChild(btn_child_1_1);

        TxPoW txp_child_1_2 = BlockTree.createRandomTxPow();
        txp_child_1_2.setBlockNumber(new MiniNumber(5));
        txp_child_1_2.setBlockDifficulty(new MiniData("0A"));
        txp_child_1_2.setTimeSecs(new MiniNumber(5));
        BlockTreeNode btn_child_1_2 = new BlockTreeNode(txp_child_1_2);
        btn_child_1.addChild(btn_child_1_2);

        TxPoW txp_child_2_1 = BlockTree.createRandomTxPow();
        txp_child_2_1.setBlockNumber(new MiniNumber(6));
        txp_child_2_1.setBlockDifficulty(new MiniData("0A"));
        txp_child_2_1.setTimeSecs(new MiniNumber(6));
        BlockTreeNode btn_child_2_1 = new BlockTreeNode(txp_child_2_1);
        btn_child_2.addChild(btn_child_2_1);

        TxPoW txp_child_2_2 = BlockTree.createRandomTxPow();
        txp_child_2_2.setBlockNumber(new MiniNumber(7));
        txp_child_2_2.setBlockDifficulty(new MiniData("0A"));
        txp_child_2_2.setTimeSecs(new MiniNumber(7));
        BlockTreeNode btn_child_2_2 = new BlockTreeNode(txp_child_2_2);
        btn_child_2.addChild(btn_child_2_2);

        TxPoW txp_child_untracked = BlockTree.createRandomTxPow();
        BlockTreeNode btn_child_untracked = new BlockTreeNode(txp_child_untracked);
        //bt.addNode(btn_child_untracked);

        BlockTreeNode btn_found;

        btn_found = null;
        btn_found = bt.findNode(txp_root.getTxPowID());
        assertEquals("should be equal ", btn_found, btn_root);

        btn_found = null;
        btn_found = bt.findNode(txp_child_1.getTxPowID());
        assertNull("should be null", btn_found);

        btn_found = null;
        btn_found = bt.findNode(txp_child_untracked.getTxPowID());
        assertNull("should be null", btn_found);

        btn_found = null;
        btn_found = bt.findNode(txp_child_1.getTxPowID(), true);
        assertEquals("should be equal ", btn_found, btn_child_1);

        btn_found = null;
        btn_found = bt.findNode(txp_child_untracked.getTxPowID(), true);
        assertNull("should be null", btn_found);

        assertEquals("should be equal ", 1, bt.getAsList().size());
        assertEquals("should be equal ", 1, bt.getAsList(true).size());

        assertEquals("should be equal ", 10, bt.getAvgChainDifficulty().intValue());
        assertEquals("should be equal ", 10, bt.getAvgChainDifficulty(btn_root).intValue());
        assertEquals("should be equal ", 10, bt.getAvgChainDifficulty(btn_child_1).intValue());
        assertEquals("should be equal ", 10, bt.getAvgChainDifficulty(btn_child_1_1).intValue());
        assertEquals("should be equal ", 10, bt.getAvgChainDifficulty(btn_child_1_2).intValue());
        assertEquals("should be equal ", 10, bt.getAvgChainDifficulty(btn_child_2).intValue());
        assertEquals("should be equal ", 10, bt.getAvgChainDifficulty(btn_child_2_1).intValue());
        assertEquals("should be equal ", 10, bt.getAvgChainDifficulty(btn_child_2_2).intValue());
        assertEquals("should be equal ", -1, bt.getAvgChainDifficulty(btn_child_untracked).intValue());

        assertEquals("should be equal ", 1, bt.getChainSpeed().getAsInt());
        assertEquals("should be equal ", 1, bt.getChainSpeed(btn_root).getAsInt());
        assertEquals("should be equal ", 1, bt.getChainSpeed(btn_child_1).getAsInt());
        assertEquals("should be equal ", 1, bt.getChainSpeed(btn_child_1_1).getAsInt());
        assertEquals("should be equal ", 1, bt.getChainSpeed(btn_child_1_2).getAsInt());
        assertEquals("should be equal ", 1, bt.getChainSpeed(btn_child_2).getAsInt());
        assertEquals("should be equal ", 1, bt.getChainSpeed(btn_child_2_1).getAsInt());
        assertEquals("should be equal ", 1, bt.getChainSpeed(btn_child_2_2).getAsInt());
        //assertEquals("should be equal ", 1, bt.getChainSpeed(btn_child_untracked).getAsInt());

        BlockTreeNode btn_root_copy = BlockTree.copyTreeNode(btn_root);
        BlockTree bt_copy = new BlockTree();
        bt_copy.setTreeRoot(btn_root_copy);

        assertEquals("should be equal ", 1, bt_copy.getAsList().size());
        assertEquals("should be equal ", 1, bt_copy.getAsList(true).size());

        assertEquals("should be equal ", 10, bt_copy.getAvgChainDifficulty().intValue());
        assertEquals("should be equal ", 10, bt_copy.getAvgChainDifficulty(btn_root).intValue());
        assertEquals("should be equal ", 10, bt_copy.getAvgChainDifficulty(btn_child_1).intValue());
        assertEquals("should be equal ", 10, bt_copy.getAvgChainDifficulty(btn_child_1_1).intValue());
        assertEquals("should be equal ", 10, bt_copy.getAvgChainDifficulty(btn_child_1_2).intValue());
        assertEquals("should be equal ", 10, bt_copy.getAvgChainDifficulty(btn_child_2).intValue());
        assertEquals("should be equal ", 10, bt_copy.getAvgChainDifficulty(btn_child_2_1).intValue());
        assertEquals("should be equal ", 10, bt_copy.getAvgChainDifficulty(btn_child_2_2).intValue());
        assertEquals("should be equal ", -1, bt_copy.getAvgChainDifficulty(btn_child_untracked).intValue());

        assertEquals("should be equal ", 1, bt_copy.getChainSpeed().getAsInt());
        assertEquals("should be equal ", 1, bt_copy.getChainSpeed(btn_root).getAsInt());
        assertEquals("should be equal ", 1, bt_copy.getChainSpeed(btn_child_1).getAsInt());
        assertEquals("should be equal ", 1, bt_copy.getChainSpeed(btn_child_1_1).getAsInt());
        assertEquals("should be equal ", 1, bt_copy.getChainSpeed(btn_child_1_2).getAsInt());
        assertEquals("should be equal ", 1, bt_copy.getChainSpeed(btn_child_2).getAsInt());
        assertEquals("should be equal ", 1, bt_copy.getChainSpeed(btn_child_2_1).getAsInt());
        assertEquals("should be equal ", 1, bt_copy.getChainSpeed(btn_child_2_2).getAsInt());
        //assertEquals("should be equal ", 1, bt_copy.getChainSpeed(btn_child_untracked).getAsInt());

        bt.resetWeights();
        //bt.sortBlockTreeNodeStates(new MinimaDB());

        //assertEquals("should be equal ", btn_root, bt.getChainRoot());
        //assertEquals("should be equal ", btn_root, bt.getChainTip());
        //assertEquals("should be equal ", btn_root, bt.getCascadeNode());
    }
}
