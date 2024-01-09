package org.minima.utils.megammr;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;

import org.minima.database.mmr.MMR;
import org.minima.database.mmr.MMREntry;
import org.minima.database.mmr.MMREntryNumber;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.Coin;
import org.minima.objects.TxBlock;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.MinimaLogger;
import org.minima.utils.mysql.MySQLConnect;

public class MegaMMR_old {

	public static String getHashTableEntry(int zRow, MMREntryNumber zEntry) {
		return zRow+":"+zEntry.toString();
	}
	
	public static void main(String[] zArgs) throws SQLException {
		
		//get the params..
		String host 	= zArgs[0];
		String db 		= zArgs[1];
		String user 	= zArgs[2];
		String password = zArgs[3];
		
		//Are we pruning
		boolean prune = false;
		String prunestr = zArgs[4];
		if(prunestr.equals("true")) {
			prune = true;
		}
		MinimaLogger.log("Prune : "+prune);
		
		//The Massive MMR
		Hashtable<String, MMREntry> megammr 	= new Hashtable<>();
		
		//the Coins
		Hashtable<String, Coin> megacoins 		= new Hashtable<>();
		
		//Get the login details..
		MySQLConnect mysql = new MySQLConnect(host, db, user, password);
		mysql.init();
		
		//Load all the data..
		MiniNumber startblock = MiniNumber.ZERO;
		
		boolean finished = false;
		int counter=0;
		while(!finished) {
			
			if(counter>100) {
				break;
			}
			counter++;
			
			//Get some blocks..
			ArrayList<TxBlock> blocks = mysql.loadBlockRange(startblock);
			
			MinimaLogger.log("Load blocks @ "+startblock);
			
			//If no blocks stop..
			if(blocks.size()<1) {
				finished = true;
				break;
			}
			
			for(TxBlock block : blocks) {
				
				//Start from next block
				startblock = block.getTxPoW().getBlockNumber().increment();
				
				//Create a Tree Node..
				TxPoWTreeNode txpnode 	 = new TxPoWTreeNode(block,false);
				
				//Get the coins..
				ArrayList<Coin> allcoins = txpnode.getAllCoins();
				
				//Output
				for(Coin cc : allcoins) {
					
					//The Key
					String id = cc.getMMREntryNumber().toString();
					
					if(prune) {
						//Is it spent or unspent
						if(cc.getSpent()) {
							megacoins.remove(id);
						}else {
							megacoins.put(id, cc);
						}
					}else{
						megacoins.put(id, cc);
					}
				}
				
				//Add to the Mega MMR..
				MMR tmmr = txpnode.getMMR();
				
				//Get all the entries
				Hashtable<String, MMREntry> allentries = tmmr.getAllEntries();
				
				//The prune subtrees
				Hashtable<String, MMREntry> megaprune 	= new Hashtable<>();
				
				//Squash this onto the mega mmr
				Enumeration<MMREntry> entries = allentries.elements();
				while(entries.hasMoreElements()) {
					
					MMREntry entry = entries.nextElement();
					
					//What row and number
					int row 			= entry.getRow();
					MMREntryNumber num 	= entry.getEntryNumber();
					
					//Get the string rep
					String strent = getHashTableEntry(row, num);
					
					//Add to our list - should be the MySQL DB
					megammr.put(strent, entry);
					
					//Is this a pruner..
					if(entry.getMMRData().getValue().isEqual(MiniNumber.ZERO)) {
						
						//This subtree can be pruned..
						megaprune.put(strent, entry);
					}
				}
				
				//And now prune..
				if(prune) {
					pruneMMR(megaprune, megammr);
				}
			}
		}
		
		System.out.println();
		System.out.println("FINAL RESULTS");
		System.out.println("-------------");
		
		System.out.println();
		System.out.println("MMR SIZE : "+megammr.size());
		
		//Now print out the lot..
		Enumeration<MMREntry> entries = megammr.elements();
		while(entries.hasMoreElements()) {
			MMREntry entry = entries.nextElement();
			MinimaLogger.log("MEGA_MMR @ "+entry.getEntryNumber().toString()+"  "+entry.toJSON());
		}
		
		System.out.println();
		System.out.println("COINS SIZE : "+megacoins.size());
		Enumeration<Coin> coinentries = megacoins.elements();
		while(coinentries.hasMoreElements()) {
			Coin cc = coinentries.nextElement();
			MinimaLogger.log("MEGA_COIN @ "+cc.getMMREntryNumber().toString()+"  "+cc.toJSON());
		}
		
		//Shut down
		mysql.shutdown();
	}
	
	
	public static void pruneMMR(Hashtable<String, MMREntry> zPrune, Hashtable<String, MMREntry> zMMR) {
	
		//Cycle through all the prune data
		Enumeration<MMREntry> pruneentries = zPrune.elements();
		while(pruneentries.hasMoreElements()) {
			
			//This entry is ZERO -you can remove the sub tree
			MMREntry entry = pruneentries.nextElement();
			
			//And recursively prune..
			pruneRecurse(entry, zMMR);
		}
	}
	
	public static void pruneRecurse(MMREntry zStartNode, Hashtable<String, MMREntry> zMMR) {
		
		//Check non null
		if(zStartNode == null) {
			return;
		}
		
		//Is this a valid MMRENtry
		if(zStartNode.isEmpty()) {
			//Already pruned..
			return;
		}
		
		//Which row are the children on..
		int childrow = zStartNode.getChildRow();
		if(childrow<0) {
			//We are at the base leaf nodes.. leave it..
			return;
		}
		
		//The children..
		MMREntryNumber leftchildnumber 	= zStartNode.getLeftChildEntry();
		MMREntryNumber rightchildnumber = zStartNode.getRightChildEntry();
		
		//Now get the hash links
		String leftchildlink 	= getHashTableEntry(childrow, leftchildnumber);
		String rightchildlink 	= getHashTableEntry(childrow, rightchildnumber);
		
		//Get the MMREntry for each child..
		MMREntry leftchild  = zMMR.get(leftchildlink);
		MMREntry rightchild = zMMR.get(rightchildlink);
		
		//Prune the children if they exist
		pruneRecurse(leftchild, zMMR);
		pruneRecurse(rightchild, zMMR);
		
		//And finally.. remove the children..
		zMMR.remove(leftchildlink);
		zMMR.remove(rightchildlink);
	}	
	
	public static void printMMR(Hashtable<String, MMREntry> zMMR) {
		
		//Get the top row
		int toprow=0;
		Enumeration<MMREntry> topentries = zMMR.elements();
		while(topentries.hasMoreElements()) {
			
			//This entry is ZERO -you can remove the sub tree
			MMREntry entry = topentries.nextElement();
			
			//And recursively prune..
			if(entry.getRow()>toprow) {
				toprow = entry.getRow();
			}
		}
		
		//Start from the max row..
		for(int i=toprow;i>=0;i--) {
		
			//The start gap
			int startgap 	= (int) (Math.pow(2, i) -1) * 2;
			int gap 		= (int) (Math.pow(2, i+1)) * 2;
			
			//Get the row..
			ArrayList<MMREntry> row = new ArrayList<>(); 
			Enumeration<MMREntry> entries = zMMR.elements();
			while(entries.hasMoreElements()) {
				MMREntry entry = entries.nextElement();
				if(entry.getRow() == i) {
					row.add(entry);
				}
			}
			
			//The final char buffer for the row
			char[] str = new char[256];
			for(int c=0;c<256;c++) {
				str[c] = ' ';
			}
			
			StringBuffer strow = new StringBuffer("                                                                       ");
			for(MMREntry entry : row) {
				
				//Add the entry to the correct spot..
				int xpos 	 = entry.getEntryNumber().getBigDecimal().intValue();
				int finalpos = startgap+(xpos*gap);
				int value    = entry.getMMRData().getValue().getAsInt();
				String valstr = ""+value;
				char[] cc = valstr.toCharArray();
				
				System.arraycopy(cc, 0, str, finalpos, cc.length);
			}
			
			System.out.println(str);
		}
		
		//Print the peak value..
		System.out.println("Total Entries       : "+zMMR.size());
	}
}
