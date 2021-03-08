package org.minima.tests.database.txpowtree;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.minima.database.txpowtree.BlockTreeNode;
import org.minima.database.txpowtree.NodeStack;
import org.minima.objects.TxPoW;

public class NodeStackTests {

    @Test
    public void testFull() {
        NodeStack ns = new NodeStack();

        assertTrue("should be true ", ns.isEmpty());
        assertNull("should be null ", ns.pop());
        //assertNull("should be null ", ns.peek());

        TxPoW txp0 = new TxPoW();
        BlockTreeNode btn = new BlockTreeNode(txp0);
        ns.push(btn);
        assertFalse("should be false ", ns.isEmpty());
        assertNotNull("should be not null ", ns.peek());
        assertNotNull("should be not null ", ns.pop());
        assertTrue("should be true ", ns.isEmpty());

    }
}
