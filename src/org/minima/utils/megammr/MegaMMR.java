package org.minima.utils.megammr;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Random;

import org.minima.database.mmr.MMR;
import org.minima.database.mmr.MMRData;
import org.minima.database.mmr.MMREntry;
import org.minima.database.mmr.MMREntryNumber;
import org.minima.database.mmr.MMRProof;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.Coin;
import org.minima.objects.TxBlock;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.MinimaLogger;
import org.minima.utils.mysql.MySQLConnect;

public class MegaMMR {

	public static String getHashTableEntry(int zRow, MMREntryNumber zEntry) {
		return zRow+":"+zEntry.toString();
	}
	
	public static void main(String[] zArgs) throws SQLException {
		
		//get the params..
		String host 	= zArgs[0];
		String db 		= zArgs[1];
		String user 	= zArgs[2];
		String password = zArgs[3];
		
		//The Massive MMR
		Hashtable<String, MMREntry> megammr 	= new Hashtable<>();
		
		//the Coins
		Hashtable<String, Coin> megacoins 	= new Hashtable<>();
		
		//Get the login details..
		MySQLConnect mysql = new MySQLConnect(host, db, user, password);
		mysql.init();
		
		//Load some data..
		ArrayList<TxBlock> blocks = mysql.loadBlockRange(MiniNumber.ZERO);
		MinimaLogger.log("Blockss found : "+blocks.size());
		int counter = 0;
		for(TxBlock block : blocks) {
			
			MinimaLogger.log("Block : "+block.getTxPoW().getBlockNumber());
			
			if(counter>3) {
				break;
			}
			
			//Create a Tree Node..
			TxPoWTreeNode txpnode 	 = new TxPoWTreeNode(block,false);
			
			//Get the coins..
			ArrayList<Coin> allcoins = txpnode.getAllCoins();
			for(Coin cc : allcoins) {
				MinimaLogger.log("COIN : "+cc.toJSON());
				megacoins.put(cc.getMMREntryNumber().toString(), cc);
			}
			
			//Add to the Mega MMR..
			MMR tmmr = txpnode.getMMR();
			
			//Get all the entries
			Hashtable<String, MMREntry> allentries = tmmr.getAllEntries();
			
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
				
				//Which position is it..
				MinimaLogger.log("Entry Found @ "+row+" / "+num.toString());
			}
			
			counter++;
		}
		
		System.out.println();
		
		//Now print out the lot..
		Enumeration<MMREntry> entries = megammr.elements();
		while(entries.hasMoreElements()) {
			MMREntry entry = entries.nextElement();
			MinimaLogger.log("MEGA_MMR @ "+entry.getEntryNumber().toString()+"  "+entry.toJSON());
		}
		
		Enumeration<Coin> coinentries = megacoins.elements();
		while(coinentries.hasMoreElements()) {
			Coin cc = coinentries.nextElement();
			MinimaLogger.log("MEGA_COIN @ "+cc.getMMREntryNumber().toString()+"  "+cc.toJSON());
		}
		
		//Shut down
		mysql.shutdown();
	}
	
	public static void pruneDemo() {
		
		System.out.println("** MMR Tree Prune POC **");
		
		MMR mmr = new MMR();
		
		//First bit of data
		MMRData zero 	= new MMRData(new MiniData("0x00"), new MiniNumber(0));
		MMRData one 	= new MMRData(new MiniData("0x01"), new MiniNumber(1));
		
		//Add 16 entries..
		int treesize = 32;
		for(int loop=0;loop<treesize;loop++) {
			mmr.addEntry(one);
		}
		MMR.printmmrtree(mmr);
		
		//Set random values to Zero..
		for(int zz=0;zz<(treesize*2);zz++) {
			int rand 				= new Random().nextInt(treesize);
			MMREntryNumber entry 	= new MMREntryNumber(rand);
			MMREntry ent = mmr.getEntry(0, entry);
			if(ent.isEmpty() || ent.getMMRData().getValue().isEqual(MiniNumber.ZERO)) {
				continue;
			}
			
			System.out.println();
			System.out.println("\nSet entry "+rand+" to 0");
			MMRProof proof 	= mmr.getProofToPeak(entry);
			mmr.updateEntry(entry, proof, zero);
			mmr.pruneTree();
			MMR.printmmrtree(mmr);
		}
	}
	
}
