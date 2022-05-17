package org.minima.system.commands.search;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.Coin;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class coins extends Command {

	public coins() {
		super("coins","(relevant:true) (sendable:true) (coinid:) (amount:) (address:) (tokenid:) - Search for coins");
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		//Check a parameter specified
		boolean hardsetrel = false;
		if(!existsParam("relevant") && !existsParam("coinid") && !existsParam("address") && !existsParam("tokenid")) {
			hardsetrel = true;
		}
		
		//Get the txpowid
		boolean relevant	= existsParam("relevant");
		if(hardsetrel) {
			relevant = true;
		}
		
		boolean simple		= getBooleanParam("sendable",false);
		
		boolean scoinid		= existsParam("coinid");
		MiniData coinid		= MiniData.ZERO_TXPOWID;
		if(scoinid) {
			coinid = new MiniData(getParam("coinid", "0x01"));
		}
		
		boolean samount		= existsParam("amount");
		MiniNumber amount	= MiniNumber.ZERO;
		if(samount) {
			amount = getNumberParam("amount");
		}
		
		boolean saddress	= existsParam("address");
		MiniData address	= MiniData.ZERO_TXPOWID;
		if(saddress) {
			address = new MiniData(getAddressParam("address"));
		}
		
		boolean stokenid	= existsParam("tokenid");
		MiniData tokenid	= MiniData.ZERO_TXPOWID;
		if(stokenid) {
			tokenid = new MiniData(getParam("tokenid", "0x01"));
		}
		
		//Get the tree tip..
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
		
		//Run the query
		ArrayList<Coin> coins = TxPoWSearcher.searchCoins(	tip, relevant, 
															scoinid, coinid,
															samount,amount,
															saddress, address, 
															stokenid, tokenid, simple);
		
		//Put it all in an array
		JSONArray coinarr = new JSONArray();
		for(Coin cc : coins) {
			coinarr.add(cc.toJSON());
		}
		
		ret.put("response", coinarr);
	
		return ret;
	}

	@Override
	public Command getFunction() {
		return new coins();
	}

}
