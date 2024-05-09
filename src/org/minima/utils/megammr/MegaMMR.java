package org.minima.utils.megammr;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Hashtable;

import org.minima.database.mmr.MMR;
import org.minima.database.mmr.MMRData;
import org.minima.database.mmr.MMREntry;
import org.minima.database.mmr.MMREntryNumber;
import org.minima.objects.Coin;
import org.minima.objects.CoinProof;
import org.minima.objects.TxBlock;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.MinimaLogger;
import org.minima.utils.mysql.MySQLConnect;

public class MegaMMR {

	public static Hashtable<String,Coin> mAllCoins;
	
	public static void main(String[] zArgs) throws SQLException {
		
		//get the params..
		String host 	= zArgs[0];
		String db 		= zArgs[1];
		String user 	= zArgs[2];
		String password = zArgs[3];
		
		//Get the login details..
		MySQLConnect mysql = new MySQLConnect(host, db, user, password);
		mysql.init();
		
		//Load all the data..
		MiniNumber startblock = MiniNumber.ZERO;
		
		mAllCoins = new Hashtable<>();
		
		//The MMR
		MMR megammr = new MMR();
		megammr.setBlockTime(MiniNumber.ZERO);
		
		boolean finished = false;
		int counter=0;
		while(!finished) {
			
			if(counter>200) {
			//	break;
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
			
				//Add to the Mega MMR
				constructMMR(megammr, block);
			}
			
			System.gc();
		}
		
		//Before prune
		MMR.printinfo(megammr);
		
		megammr.pruneTree();
		
		MMR.printinfo(megammr);
		
		megammr.finalizeSet();
		
		MinimaLogger.log("All unspent coins.. "+mAllCoins.size());
		
		//Shut down
		mysql.shutdown();
	}
	
	/**
	 * Convert the TxBlock 
	 */
	public static  void constructMMR(MMR zMMR, TxBlock zBlock) {
		
		//What Block Time Are we..
		MiniNumber block = zBlock.getTxPoW().getBlockNumber();
		zMMR.setBlockTime(block);
		
		//Add all the peaks..
		ArrayList<MMREntry> peaks = zBlock.getPreviousPeaks();
		for(MMREntry peak : peaks) {
			zMMR.setEntry(peak.getRow(), peak.getEntryNumber(), peak.getMMRData());
		}
		
		//Calculate the Entry NUmber
		zMMR.calculateEntryNumberFromPeaks();
		
		//Now you have all the previous peaks.. update the spent coins..
		ArrayList<CoinProof> spentcoins = zBlock.getInputCoinProofs();
		for(CoinProof input : spentcoins) {
			
			//Which entry is this in the MMR
			MMREntryNumber entrynumber = input.getCoin().getMMREntryNumber();
			
			//A NEW MMRData of the spent coin
			Coin spentcoin = input.getCoin().deepCopy();
			spentcoin.setSpent(true);

			//Create the MMRData
			MMRData mmrdata = MMRData.CreateMMRDataLeafNode(spentcoin, MiniNumber.ZERO);
						
			//Update the MMR
			zMMR.updateEntry(entrynumber, input.getMMRProof(), mmrdata);
			
			//Add to the total List of coins for this block
			//mCoins.add(spentcoin);
			
			//Remove from all coins..
			String coinid = input.getCoin().getCoinID().to0xString();
			mAllCoins.remove(coinid);
		}
		
		//And ADD all the newly created coins
		ArrayList<Coin> outputs = zBlock.getOutputCoins();
		for(Coin output : outputs) {
			
			//Where are we in the MMR
			MMREntryNumber entrynumber = zMMR.getEntryNumber();
			
			//Create a new CoinMMR structure - unspent..
			Coin newcoin = output.deepCopy();
			newcoin.setMMREntryNumber(entrynumber);
			newcoin.setBlockCreated(block);
			newcoin.setSpent(false);
			
			//Create the MMRData
			MMRData mmrdata = MMRData.CreateMMRDataLeafNode(newcoin, output.getAmount());
						
			//And add to the MMR
			zMMR.addEntry(mmrdata);	
			
			//Add to the total List of coins for this block
			String coinid = output.getCoinID().to0xString();
			mAllCoins.put(coinid, output);
		}
	}
}