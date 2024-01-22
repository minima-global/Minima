package org.minima.database.mmr;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;

import org.minima.objects.Coin;
import org.minima.objects.CoinProof;
import org.minima.objects.TxBlock;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.MiniFile;
import org.minima.utils.MiniFormat;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;

public class MegaMMR implements Streamable {

	//The MMR
	MMR mMMR;
	
	//All the UNSPENT Coins
	Hashtable<String,Coin> mAllUnspentCoins;
	
	public MegaMMR() {
		
		//The actual MMR
		mMMR = new MMR();
		
		//The Unspent Coins
		mAllUnspentCoins = new Hashtable<>();
	}
	
	public MMR getMMR() {
		return mMMR;
	}
	
	public Hashtable<String, Coin> getAllCoins(){
		return mAllUnspentCoins;
	}
	
	/**
	 * Convert the TxBlock 
	 */
	public void addBlock(TxBlock zBlock) {
		
		//What Block Time Are we..
		MiniNumber block = zBlock.getTxPoW().getBlockNumber();
		mMMR.setBlockTime(block);
		
		//MinimaLogger.log("MEGAMMR : Addblock "+block);
		
		//Add all the peaks..
		ArrayList<MMREntry> peaks = zBlock.getPreviousPeaks();
		for(MMREntry peak : peaks) {
			mMMR.setEntry(peak.getRow(), peak.getEntryNumber(), peak.getMMRData());
		}
		
		//Calculate the Entry NUmber
		mMMR.calculateEntryNumberFromPeaks();
		
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
			mMMR.updateEntry(entrynumber, input.getMMRProof(), mmrdata);
			
			//Remove from all coins..
			mAllUnspentCoins.remove(input.getCoin().getCoinID().to0xString());
		}
		
		//And ADD all the newly created coins
		ArrayList<Coin> outputs = zBlock.getOutputCoins();
		for(Coin output : outputs) {
			
			//Where are we in the MMR
			MMREntryNumber entrynumber = mMMR.getEntryNumber();
			
			//Create a new CoinMMR structure - unspent..
			Coin newcoin = output.deepCopy();
			newcoin.setMMREntryNumber(entrynumber);
			newcoin.setBlockCreated(block);
			newcoin.setSpent(false);
			
			//Create the MMRData
			MMRData mmrdata = MMRData.CreateMMRDataLeafNode(newcoin, output.getAmount());
						
			//And add to the MMR
			mMMR.addEntry(mmrdata);	
			
			//Add to the total List of coins for this block
			mAllUnspentCoins.put(output.getCoinID().to0xString(), newcoin);
		}
		
		//Finish up..
		mMMR.finalizeSet();
		mMMR.setFinalized(false);
	}
	
	/**
	 * Wipe the data
	 */
	public void clear() {
		mMMR 				= new MMR();
		mAllUnspentCoins 	= new Hashtable<>();
	}
	
	public void loadMMR(File zFile) {
		MinimaLogger.log("Loading MegaMMR size : "+MiniFormat.formatSize(zFile.length()));
		//MiniFile.loadObjectSlow(zFile, this);
		MiniFile.loadObject(zFile, this);
	}
	
	public void saveMMR(File zFile) {
		//MiniFile.saveObjectDirect(zFile, this);
		MiniFile.saveObject(zFile, this);
		//MinimaLogger.log("Saving MegaMMR size : "+MiniFormat.formatSize(zFile.length()));
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		//First write out the VERSION
		MiniNumber.WriteToStream(zOut, 1);
		
		//Now the MMR
		mMMR.writeDataStream(zOut);
		
		//And now all the coins..
		int size = mAllUnspentCoins.size();
		MiniNumber.WriteToStream(zOut, size);
		
		Enumeration<Coin> coins = mAllUnspentCoins.elements();
		while(coins.hasMoreElements()) {
			Coin cc = coins.nextElement();
			cc.writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		int version = MiniNumber.ReadFromStream(zIn).getAsInt();
		
		//Read in the MMR..
		mMMR = new MMR();
		mMMR.readDataStream(zIn);
		mMMR.setFinalized(false);
		
		//And now all the coins
		mAllUnspentCoins = new Hashtable<>();
		int size = MiniNumber.ReadFromStream(zIn).getAsInt();
		for(int i=0;i<size;i++) {
			Coin cc = Coin.ReadFromStream(zIn);
			mAllUnspentCoins.put(cc.getCoinID().to0xString(), cc);
		}
	}
	
	
	public static void main(String[] zArgs) {
		
		MMR mmr = new MMR();
		
		mmr.addEntry(getCoinData(MiniNumber.BILLION.add(MiniNumber.ONE)));
		mmr.addEntry(getCoinData(MiniNumber.ONE));
		mmr.addEntry(getCoinData(MiniNumber.ONE));
		mmr.addEntry(getCoinData(MiniNumber.ONE));
		mmr.addEntry(getCoinData(MiniNumber.ONE));
		mmr.addEntry(getCoinData(MiniNumber.ONE));
		mmr.addEntry(getCoinData(MiniNumber.ONE));
		mmr.addEntry(getCoinData(MiniNumber.ONE));
		
		MMR.printmmrtree(mmr);
	}
	
	public static MMRData getCoinData() {
		return getCoinData(MiniNumber.ZERO);
	}
	
	public static MMRData getCoinData(MiniNumber zNumber) {
		
		//Create the coin
		Coin test = new Coin(MiniData.ZERO_TXPOWID,zNumber, MiniData.ZERO_TXPOWID);
		
		//Create the MMRData
		return MMRData.CreateMMRDataLeafNode(test, zNumber); 
	}
}
