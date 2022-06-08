package org.minima.database.txpowtree;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;

import java.util.ArrayList;

import org.junit.Test;
import org.minima.objects.TxPoW;

public class TxPowTreeTest {
	
	@Test
	public void testTxPowTreeTip() {
		//Tree and Nodes..
		TxPowTree tree;
		TxPoWTreeNode root,tip,child, child2, child3 = null;
		
		//New Tree
		tree = new TxPowTree();
		root = new TxPoWTreeNode(new TxPoW("0x00", 0, 1));
			root.addChildNode(new TxPoWTreeNode(new TxPoW("0x01", 1, 1)));
			root.addChildNode(new TxPoWTreeNode(new TxPoW("0x02", 1, 1)));
			child = new TxPoWTreeNode(new TxPoW("0x03", 1, 1));
			root.addChildNode(child);
				child.addChildNode(new TxPoWTreeNode(new TxPoW("0x04", 2, 1)));
		tree.setRoot(root);
		tip = tree.getTip();
		assertEquals(tip.getTxPoW().getTxPoWID(), "0x04");
		assertEquals(root.getTotalWeight().intValue(), 5);
		assertEquals(tree.getSize(), 5);
		
		//New Tree
		tree = new TxPowTree();
		root = new TxPoWTreeNode(new TxPoW("0x00", 0, 1));
			root.addChildNode(new TxPoWTreeNode(new TxPoW("0x01", 1, 4)));
			root.addChildNode(new TxPoWTreeNode(new TxPoW("0x02", 1, 1)));
			child = new TxPoWTreeNode(new TxPoW("0x03", 1, 1));
			root.addChildNode(child);
				child.addChildNode(new TxPoWTreeNode(new TxPoW("0x04", 2, 1)));
		tree.setRoot(root);
		tip = tree.getTip();
		assertEquals(tip.getTxPoW().getTxPoWID(), "0x01");
		assertEquals(root.getTotalWeight().intValue(), 8);
		assertEquals(tree.getSize(), 5);
		
		//New Tree
		tree = new TxPowTree();
		root = new TxPoWTreeNode(new TxPoW("0x00", 0, 1));
			root.addChildNode(new TxPoWTreeNode(new TxPoW("0x01", 1, 1)));
			root.addChildNode(new TxPoWTreeNode(new TxPoW("0x02", 1, 3)));
			child = new TxPoWTreeNode(new TxPoW("0x03", 1, 1));
			root.addChildNode(child);
				child.addChildNode(new TxPoWTreeNode(new TxPoW("0x04", 2, 1)));
		tree.setRoot(root);
		tip = tree.getTip();
		assertEquals(tip.getTxPoW().getTxPoWID(), "0x02");
		assertEquals(root.getTotalWeight().intValue(), 7);
		assertEquals(tree.getSize(), 5);
		
		//New Tree
		tree = new TxPowTree();
		root = new TxPoWTreeNode(new TxPoW("0x00", 0, 1));
			root.addChildNode(new TxPoWTreeNode(new TxPoW("0x01", 1, 2)));
			root.addChildNode(new TxPoWTreeNode(new TxPoW("0x02", 1, 2)));
			child = new TxPoWTreeNode(new TxPoW("0x03", 1, 1));
			root.addChildNode(child);
				child.addChildNode(new TxPoWTreeNode(new TxPoW("0x04", 2, 1)));
		tree.setRoot(root);
		tip = tree.getTip();
		assertEquals(tip.getTxPoW().getTxPoWID(), "0x01");
		assertEquals(root.getTotalWeight().intValue(), 7);
		
		//New Tree
		tree = new TxPowTree();
		root = new TxPoWTreeNode(new TxPoW("0x00", 0, 1));
			root.addChildNode(new TxPoWTreeNode(new TxPoW("0x01", 1, 2)));
			root.addChildNode(new TxPoWTreeNode(new TxPoW("0x02", 1, 2)));
			child = new TxPoWTreeNode(new TxPoW("0x03", 1, 1));
			root.addChildNode(child);
				child.addChildNode(new TxPoWTreeNode(new TxPoW("0x04", 2, 1)));
				child.addChildNode(new TxPoWTreeNode(new TxPoW("0x05", 2, 1)));
				child2 = new TxPoWTreeNode(new TxPoW("0x06", 2, 1));
				child.addChildNode(child2);
					child2.addChildNode(new TxPoWTreeNode(new TxPoW("0x07", 3, 1)));
				
		tree.setRoot(root);
		tip = tree.getTip();
		assertEquals(tip.getTxPoW().getTxPoWID(), "0x07");
		assertEquals(root.getTotalWeight().intValue(), 10);
		assertEquals(tree.getSize(), 8);
	}

	@Test
	public void testFindNode() {
		//Tree and Nodes..
		TxPowTree tree;
		TxPoWTreeNode root, treenode, child = null;
		
		//New Tree
		tree = new TxPowTree();
		root = new TxPoWTreeNode(new TxPoW("0x00", 0, 1));
			root.addChildNode(new TxPoWTreeNode(new TxPoW("0x01", 1, 1)));
			root.addChildNode(new TxPoWTreeNode(new TxPoW("0x02", 1, 1)));
			child = new TxPoWTreeNode(new TxPoW("0x03", 1, 1));
			root.addChildNode(child);
				child.addChildNode(new TxPoWTreeNode(new TxPoW("0x04", 2, 1)));
		tree.setRoot(root);
		
		treenode = tree.findNode("0x00");
		assertNotNull(treenode);
		assertEquals(treenode.getTxPoW().getTxPoWID(), "0x00");
		
		treenode = tree.findNode("0x01");
		assertNotNull(treenode);
		assertEquals(treenode.getTxPoW().getTxPoWID(), "0x01");
		
		treenode = tree.findNode("0x02");
		assertNotNull(treenode);
		assertEquals(treenode.getTxPoW().getTxPoWID(), "0x02");
		
		treenode = tree.findNode("0x03");
		assertNotNull(treenode);
		assertEquals(treenode.getTxPoW().getTxPoWID(), "0x03");
		
		treenode = tree.findNode("0x04");
		assertNotNull(treenode);
		assertEquals(treenode.getTxPoW().getTxPoWID(), "0x04");
	
		treenode = tree.findNode("0x05");
		assertNull(treenode);
		
		treenode = tree.findNode("0x11");
		assertNull(treenode);
	}
	
	@Test
	public void testGetHeaviestBranch(){
		//Tree and Nodes..
		TxPowTree tree;
		TxPoWTreeNode root, child = null;
		
		//New Tree
		tree = new TxPowTree();
		root = new TxPoWTreeNode(new TxPoW("0x00", 0, 1));
			root.addChildNode(new TxPoWTreeNode(new TxPoW("0x01", 1, 1)));
			root.addChildNode(new TxPoWTreeNode(new TxPoW("0x02", 1, 1)));
			child = new TxPoWTreeNode(new TxPoW("0x03", 1, 1));
			root.addChildNode(child);
				child.addChildNode(new TxPoWTreeNode(new TxPoW("0x04", 2, 1)));
		tree.setRoot(root);
		
		ArrayList<TxPoWTreeNode> hbranch = tree.getHeaviestBranch();
		assertEquals(hbranch.size(), 3);
		assertEquals(hbranch.get(0).getTxPoW().getTxPoWID(), "0x04");
		assertEquals(hbranch.get(1).getTxPoW().getTxPoWID(), "0x03");
		assertEquals(hbranch.get(2).getTxPoW().getTxPoWID(), "0x00");
	}
	
//	@Test
//	public void testTxPowPrintTree() {
//		//Tree and Nodes..
//		TxPowTree tree;
//		TxPoWTreeNode root,tip,child, child2, child3 = null;
//		
//		//New Tree
//		tree = new TxPowTree();
//		root = new TxPoWTreeNode(new TxPoW("0x00", 0, 1));
//			root.addChildNode(new TxPoWTreeNode(new TxPoW("0x01", 1, 2)));
//			root.addChildNode(new TxPoWTreeNode(new TxPoW("0x02", 1, 2)));
//			
//			child = new TxPoWTreeNode(new TxPoW("0x03", 1, 1));
//			root.addChildNode(child);
//				child.addChildNode(new TxPoWTreeNode(new TxPoW("0x04", 2, 1)));
//				child.addChildNode(new TxPoWTreeNode(new TxPoW("0x05", 2, 1)));
//				
//				child2 = new TxPoWTreeNode(new TxPoW("0x06", 2, 1));
//				child.addChildNode(child2);
//					child2.addChildNode(new TxPoWTreeNode(new TxPoW("0x07", 3, 1)));
//					child2.addChildNode(new TxPoWTreeNode(new TxPoW("0x08", 3, 1)));
//					
//					child3 = new TxPoWTreeNode(new TxPoW("0x09", 2, 1));
//					child2.addChildNode(child3);
//						child3.addChildNode(new TxPoWTreeNode(new TxPoW("0x10", 3, 1)));
//					
//					child2.addChildNode(new TxPoWTreeNode(new TxPoW("0x11", 3, 1)));
//				
//		tree.setRoot(root);
//		tip = tree.getTip();
//		
//		tree.printTree();
//		
//	}
	
	@Test
	public void testTxPowTrimTree() {
		//Tree and Nodes..
		TxPowTree tree;
		TxPoWTreeNode root,tip,child, child2, child3 = null;
		
		//New Tree
		tree = new TxPowTree();
		
		root = new TxPoWTreeNode(new TxPoW("0x00", 0, 1));
		tree.setRoot(root);
		
			root.addChildNode(new TxPoWTreeNode(new TxPoW("0x01", 1, 2)));
			root.addChildNode(new TxPoWTreeNode(new TxPoW("0x02", 1, 2)));
			
			child = new TxPoWTreeNode(new TxPoW("0x03", 1, 1));
			root.addChildNode(child);
//				child.addChildNode(new TxPoWTreeNode(new TxPoW("0x04", 2, 1)));
//				child.addChildNode(new TxPoWTreeNode(new TxPoW("0x05", 2, 1)));
				
				child2 = new TxPoWTreeNode(new TxPoW("0x06", 2, 1));
				child.addChildNode(child2);
					child2.addChildNode(new TxPoWTreeNode(new TxPoW("0x07", 3, 1)));
					child2.addChildNode(new TxPoWTreeNode(new TxPoW("0x08", 3, 1)));
					
					child3 = new TxPoWTreeNode(new TxPoW("0x09", 2, 1));
					child2.addChildNode(child3);
						child3.addChildNode(new TxPoWTreeNode(new TxPoW("0x10", 3, 1)));
					
					child2.addChildNode(new TxPoWTreeNode(new TxPoW("0x11", 3, 1)));
		
		tree.recalculateTree();
		tree.printTree(32);
		
		root = tree.getRoot();
		tip  = tree.getTip();
		System.out.println("ROOT:"+root.getTxPoW());
		System.out.println("TIP:"+tip.getTxPoW());
		System.out.println();
		
		tree.setLength(4);
		tree.recalculateTree();
		tree.printTree(32);
		
		root = tree.getRoot();
		tip  = tree.getTip();
		System.out.println("ROOT:"+root.getTxPoW());
		System.out.println("TIP:"+tip.getTxPoW());
		
		
		
	}

}
