package org.minima.system.genesis;

import org.minima.database.mmr.MMR;
import org.minima.database.mmr.MMRData;
import org.minima.objects.Coin;
import org.minima.objects.base.MiniNumber;

public class GenesisMMR extends MMR {

	public GenesisMMR() {
		super();
		
		//This is begore the big bang..
		setBlockTime(MiniNumber.ZERO);
		
		//Add 1 entry.. the genesis coin..
		Coin gencoin = new GenesisCoin();
		
		//Create the MMRdata
		MMRData mmrdata = MMRData.CreateMMRDataLeafNode(gencoin, gencoin.getAmount());
		
//		//Get the Hash of this 
//		MiniData hashunspent = Crypto.getInstance().hashObject(new GenesisCoin());
//		
//		//And create a new MMRData with the correct amount
//		MMRData mmrdata = new MMRData(hashunspent, gencoin.getAmount());
		
		//And add to the MMR
		addEntry(mmrdata);
		
		//It's done..
		finalizeSet();
	}
}
