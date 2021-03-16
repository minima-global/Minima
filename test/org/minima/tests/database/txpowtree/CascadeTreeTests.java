package org.minima.tests.database.txpowtree;

import org.junit.Test;
import org.minima.database.txpowtree.BlockTree;
import org.minima.database.txpowtree.BlockTreeNode;
import org.minima.database.txpowtree.CascadeTree;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;

public class CascadeTreeTests {

    @Test
    public void testConstructors() {
        BlockTree bt = new BlockTree();

        TxPoW txp_root = BlockTree.createRandomTxPow();
        txp_root.setBlockNumber(new MiniNumber(1));
        txp_root.setBlockDifficulty(new MiniData("0A"));
        txp_root.setTimeMilli(new MiniNumber(1));
        BlockTreeNode btn_root = new BlockTreeNode(txp_root);
        bt.setTreeRoot(btn_root);

        TxPoW txp_child_1 = BlockTree.createRandomTxPow();
        txp_child_1.setBlockNumber(new MiniNumber(2));
        txp_child_1.setBlockDifficulty(new MiniData("0A"));
        txp_child_1.setTimeMilli(new MiniNumber(2));
        BlockTreeNode btn_child_1 = new BlockTreeNode(txp_child_1);
        btn_root.addChild(btn_child_1);

        TxPoW txp_child_2 = BlockTree.createRandomTxPow();
        txp_child_2.setBlockNumber(new MiniNumber(3));
        txp_child_2.setBlockDifficulty(new MiniData("0A"));
        txp_child_2.setTimeMilli(new MiniNumber(3));
        BlockTreeNode btn_child_2 = new BlockTreeNode(txp_child_2);
        btn_root.addChild(btn_child_2);

        TxPoW txp_child_1_1 = BlockTree.createRandomTxPow();
        txp_child_1_1.setBlockNumber(new MiniNumber(4));
        txp_child_1_1.setBlockDifficulty(new MiniData("0A"));
        txp_child_1_1.setTimeMilli(new MiniNumber(4));
        BlockTreeNode btn_child_1_1 = new BlockTreeNode(txp_child_1_1);
        btn_child_1.addChild(btn_child_1_1);

        TxPoW txp_child_1_2 = BlockTree.createRandomTxPow();
        txp_child_1_2.setBlockNumber(new MiniNumber(5));
        txp_child_1_2.setBlockDifficulty(new MiniData("0A"));
        txp_child_1_2.setTimeMilli(new MiniNumber(5));
        BlockTreeNode btn_child_1_2 = new BlockTreeNode(txp_child_1_2);
        btn_child_1.addChild(btn_child_1_2);

        TxPoW txp_child_2_1 = BlockTree.createRandomTxPow();
        txp_child_2_1.setBlockNumber(new MiniNumber(6));
        txp_child_2_1.setBlockDifficulty(new MiniData("0A"));
        txp_child_2_1.setTimeMilli(new MiniNumber(6));
        BlockTreeNode btn_child_2_1 = new BlockTreeNode(txp_child_2_1);
        btn_child_2.addChild(btn_child_2_1);

        TxPoW txp_child_2_2 = BlockTree.createRandomTxPow();
        txp_child_2_2.setBlockNumber(new MiniNumber(7));
        txp_child_2_2.setBlockDifficulty(new MiniData("0A"));
        txp_child_2_2.setTimeMilli(new MiniNumber(7));
        BlockTreeNode btn_child_2_2 = new BlockTreeNode(txp_child_2_2);
        btn_child_2.addChild(btn_child_2_2);

        TxPoW txp_child_untracked = BlockTree.createRandomTxPow();
        BlockTreeNode btn_child_untracked = new BlockTreeNode(txp_child_untracked);
        //bt.addNode(btn_child_untracked);

        CascadeTree ct = new CascadeTree(bt);
        ct.cascadedTree();

    }
}
