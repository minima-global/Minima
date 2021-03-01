package org.minima.tests.database.txpowtree;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.BlockTreeNode;
import org.minima.database.txpowtree.NodeAction;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;

public class NodeActionTests {

    @Test
    public void testFull() {

        NodeAction na1 = new NodeAction() {
            @Override
            public void runAction(BlockTreeNode zNode) {

            }
        };

        NodeAction na2 = new NodeAction(new MiniData()) {
            @Override
            public void runAction(BlockTreeNode zNode) {

            }
        };

        NodeAction na3 = new NodeAction(new MinimaDB()) {
            @Override
            public void runAction(BlockTreeNode zNode) {

            }
        };

        assertFalse("should be false ", na1.returnObject());
        assertNull("should be null ", na1.getObject());
        assertNull("should be null ", na1.getExtraData());
        assertNull("should be null ", na1.getDB());

        assertFalse("should be false ", na2.returnObject());
        assertNull("should be null ", na2.getObject());
        assertNotNull("should be not null ", na2.getExtraData());
        assertNull("should be null ", na2.getDB());

        assertFalse("should be false ", na3.returnObject());
        assertNull("should be null ", na3.getObject());
        assertNull("should be null ", na3.getExtraData());
        assertNotNull("should be not null ", na3.getDB());

        TxPoW txp0 = new TxPoW();
        BlockTreeNode btn = new BlockTreeNode(txp0);
        na1.setReturnObject(btn);
        assertTrue("should be true ", na1.returnObject());
        assertNotNull("should be not null ", na1.getObject());

    }
}
