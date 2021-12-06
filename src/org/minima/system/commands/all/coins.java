package org.minima.system.commands.all;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.Coin;
import org.minima.objects.base.MiniData;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class coins extends Command {

	public coins() {
		super("coins","(relevant:true) (coinid:) (address:) (tokenid:) - Search for specific coins");
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		//Check a parameter specified
		if(!existsParam("relevant") && !existsParam("coinid") && !existsParam("address") && !existsParam("tokenid")) {
			throw new Exception("No parameters specified");
		}
		
		//Get the txpowid
		boolean relevant	= existsParam("relevant");
		
		boolean scoinid		= existsParam("coinid");
		MiniData coinid		= MiniData.ZERO_TXPOWID;
		if(scoinid) {
			coinid = new MiniData(getParam("coinid", "0x01"));
		}
		
		boolean saddress	= existsParam("address");
		MiniData address	= MiniData.ZERO_TXPOWID;
		if(saddress) {
			address = new MiniData(getParam("address", "0x01"));
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
															saddress, address, 
															stokenid, tokenid);
		
		//Put it all in an array
		JSONArray coinarr = new JSONArray();
		for(Coin cc : coins) {
			coinarr.add(cc.toJSON());
		}
		
		ret.put("status", true);
		ret.put("response", coinarr);
	
		return ret;
	}

	@Override
	public Command getFunction() {
		return new coins();
	}

}
