package org.minima.system.commands.search;

import java.util.ArrayList;
import java.util.Arrays;

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
	public String getFullHelp() {
		return "\ncoins\n"
				+ "\n"
				+ "Search for coins that are relevant to you or in the unpruned chain.\n"
				+ "\n"
				+ "relevant: (optional)\n"
				+ "    true or false, true will only return coins you are tracking.\n"
				+ "    false will search all coins in the unpruned chain.\n"
				+ "    Default is false unless no other parameters are provided.\n"
				+ "\n"
				+ "sendable: (optional)\n"
				+ "    true only, filter out coins that are not sendable, they might be locked in a contract.\n"
				+ "    Default is to return sendable and unsendable coins.\n"
				+ "\n"
				+ "coinid: (optional)\n"
				+ "    A coinid, to search for a single coin.\n"
				+ "\n"
				+ "amount: (optional)\n"
				+ "    The coin value to search for.\n"
				+ "\n"
				+ "address: (optional)\n"
				+ "    Address of a coin to search for, could be a script address.\n"
				+ "    Can be a 0x or Mx address.\n"
				+ "\n"
				+ "tokenid: (optional)\n"
				+ "    A tokenid, to search for coins of a specific token. Minima is 0x00.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "coins\n"
				+ "\n"
				+ "coins relevant:true sendable:true\n"
				+ "\n"
				+ "coins relevant:true amount:10\n"
				+ "\n"
				+ "coins coinid:0xEECD7..\n"
				+ "\n"
				+ "coins relevant:true tokenid:0xFED5..\n"
				+ "\n"
				+ "coins relevant:true address:0xCEF6.. tokenid:0x00\n"
				+ "\n"
				+ "coins relevant:true address:MxABC9..\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"relevant","sendable","coinid","amount","address","tokenid"}));
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
