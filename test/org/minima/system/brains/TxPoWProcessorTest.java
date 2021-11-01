package org.minima.system.brains;

import static org.junit.Assert.*;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;
import org.minima.database.MinimaDB;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.txpowtree.TxPowTree;
import org.minima.objects.TxPoW;

public class TxPoWProcessorTest {

	@Test
	public void testTxPoWProcessor() {
//		//Create DB
//		MinimaDB.clearDB();
//		
//		//Now start firing messages..
//		TxPoWProcessor txproc = new TxPoWProcessor();
//		txproc.setFullLogging(true);
//		
//		//Fast access DB
//		TxPoWDB txpdb 		= MinimaDB.getDB().getTxPoWDB();
//		TxPowTree txptree 	= MinimaDB.getDB().getTxPoWTree();
//		
//		//Add a root node!
//		TxPoW root = new TxPoW("0x00", 0, 1, true, "", false);
//		TxPoWTreeNode rootnode = new TxPoWTreeNode(root);
//		txptree.setRoot(rootnode);
//		
//		//Some new blocks
//		TxPoW child1 	= new TxPoW("0x01", 1, 1, true, "0x00", false);
//		TxPoW child2 	= new TxPoW("0x02", 2, 1, true, "0x01", false);
//		
//		TxPoW child3 	= new TxPoW("0x03", 3, 1, true, "0x02", false);
//		TxPoW child31 	= new TxPoW("0x031", 3, 1, true, "0x02", false);
//		TxPoW child32 	= new TxPoW("0x032", 3, 1, true, "0x02", false);
//		
//		TxPoW child4 	= new TxPoW("0x04", 4, 1, true, "0x03", false);
//		TxPoW child41 	= new TxPoW("0x041", 4, 1, true, "0x031", false);
//		
//		TxPoW child5 	= new TxPoW("0x05", 5, 1, true, "0x04", false);
//		
//		txproc.postProcessTxPoW(child4);
//		txproc.postProcessTxPoW(child31);
//		txproc.postProcessTxPoW(child32);
//		txproc.postProcessTxPoW(child3);
//		txproc.postProcessTxPoW(child2);
//		txproc.postProcessTxPoW(child1);
//		txproc.postProcessTxPoW(child5);
//		txproc.postProcessTxPoW(child41);
//		
//		//Small delay..
//		delay(250);
//		
//		//Check tip..
//		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
//		System.out.println("TIP: "+tip.getTxPoW().getTxPoWID());
//		assertEquals("0x05", tip.getTxPoW().getTxPoWID());
//		
//		//Stop processor
//		txproc.stopMessageProcessor();
	}

	@Test
	public void testTxPoWProcessor2() {
//		//Create DB
//		MinimaDB.clearDB();
//		
//		//Now start firing messages..
//		TxPoWProcessor txproc = new TxPoWProcessor();
//		txproc.setFullLogging(true);
//		
//		//Fast access DB
//		TxPoWDB txpdb 		= MinimaDB.getDB().getTxPoWDB();
//		TxPowTree txptree 	= MinimaDB.getDB().getTxPoWTree();
//		
//		//Add a root node!
//		TxPoW root = new TxPoW("0x00", 0, 1, true, "", false);
//		TxPoWTreeNode rootnode = new TxPoWTreeNode(root);
//		txptree.setRoot(rootnode);
//		
//		//Some transactions
//		TxPoW trans1 	= new TxPoW("0xT01", 1, 1, false, "0x00", true);
//		TxPoW trans2 	= new TxPoW("0xT02", 1, 1, false, "0x01", true);
//		TxPoW trans3 	= new TxPoW("0xT03", 1, 1, false, "0x02", true);
//		
//		//Some new blocks
//		TxPoW child1 	= new TxPoW("0x01", 1, 1, true, "0x00", false);
//		child1.addTestTransaction(trans1.getTxPoWID());
//		
//		TxPoW child2 	= new TxPoW("0x02", 2, 1, true, "0x01", false);
//		child2.addTestTransaction(trans2.getTxPoWID());
//		child2.addTestTransaction(trans3.getTxPoWID());
//		
//		TxPoW child3 	= new TxPoW("0x03", 3, 2, true, "0x02", false);
//		TxPoW child7 	= new TxPoW("0x07", 7, 2, true, "0x06", false);
//		TxPoW child6 	= new TxPoW("0x06", 6, 2, true, "0x05", false);
//		TxPoW child5 	= new TxPoW("0x05", 5, 2, true, "0x04", false);
//		TxPoW child4 	= new TxPoW("0x04", 4, 2, true, "0x03", false);
//		
//		//Now post..
//		txproc.postProcessTxPoW(trans1);
//		txproc.postProcessTxPoW(trans2);
//		txproc.postProcessTxPoW(child2);
//		txproc.postProcessTxPoW(trans3);
//		txproc.postProcessTxPoW(child3);
//		txproc.postProcessTxPoW(child4);
//		txproc.postProcessTxPoW(child5);
//		txproc.postProcessTxPoW(child6);
//		txproc.postProcessTxPoW(child7);
//		txproc.postProcessTxPoW(child1);
//		
//		//Small delay to let it finish..
//		delay(250);
//		
//		//Check tip..
//		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
//		System.out.println("TIP: "+tip.getTxPoW().getTxPoWID());
//		assertEquals("0x07", tip.getTxPoW().getTxPoWID());
//		
//		//Stop processor
//		txproc.stopMessageProcessor();
	}
	
	public static void delay() {
		delay(100);
	}
	
	public static void delay(long zMilli) {
		try {Thread.sleep(zMilli);} catch (InterruptedException e) {};
	}
}
