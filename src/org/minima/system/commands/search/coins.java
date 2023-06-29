package org.minima.system.commands.search;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;

import org.minima.database.MinimaDB;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.Coin;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.Main;
import org.minima.system.brains.TxPoWMiner;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.system.commands.send.send;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class coins extends Command {

	public coins() {
		super("coins","(relevant:true) (sendable:true) (coinid:) (amount:) (address:) (tokenid:) (coinage:) (checkmempool:) (order:) - Search for coins");
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
				+ "checkmempool: (optional)\n"
				+ "    Check if the coin is in the mempool.\n"
				+ "\n"
				+ "coinage: (optional)\n"
				+ "    How old does the coin have to be.\n"
				+ "\n"
				+ "order: (optional)\n"
				+ "    Order asc or desc (Ascending or Decending).\n"
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
		return new ArrayList<>(Arrays.asList(new String[]{"relevant","sendable","coinid","amount",
				"address","tokenid","checkmempool","order","coinage","simplestate","totalamount"}));
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
		
		//Are we using ther simplestate output
		boolean simplestate = getBooleanParam("simplestate", false);
		
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
		
		
		//How old do the coins need to be.. used by consolidate
		MiniNumber coinage = getNumberParam("coinage", MiniNumber.ZERO);
		
		//Get the tree tip..
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();

		//Do we even have a tip
		if(tip == null) {
			ret.put("response", new JSONArray());
			return ret;
		}
		
		//Run the query
		ArrayList<Coin> coins = TxPoWSearcher.searchCoins(	tip, relevant, 
															scoinid, coinid,
															samount,amount,
															saddress, address, 
															stokenid, tokenid, simple);
		
		//Make sure coins old enough..
		ArrayList<Coin> agecoins = new ArrayList<>();
		
		//Now make sure they are old enough
		MiniNumber mincoinblock = tip.getBlockNumber().sub(coinage);
		for(Coin relc : coins) {
			if(relc.getBlockCreated().isLessEqual(mincoinblock)) {
				agecoins.add(relc);
			}
		}
		
		//Re-assign
		coins = agecoins;
		
		//Are we checking if they are in the mempool
		boolean checkmempool = getBooleanParam("checkmempool", false);
		ArrayList<Coin> finalcoins = null;
		if(checkmempool) {
			
			//Get the TxPoWDB
			TxPoWDB txpdb 		= MinimaDB.getDB().getTxPoWDB();
			TxPoWMiner txminer 	= Main.getInstance().getTxPoWMiner();
			
			finalcoins = new ArrayList<>();
			for(Coin coin : coins) {
				
				//Check if we are already using thewm in another Transaction that is being mined
				if(txminer.checkForMiningCoin(coin.getCoinID().to0xString())) {
					continue;
				}
				
				//Check if in mempool..
				if(txpdb.checkMempoolCoins(coin.getCoinID())) {
					continue;
				}
				
				finalcoins.add(coin);
			}
			
		}else {
			finalcoins = coins;
		}
		
		//Sort the coins..
		String order = getParam("order", "desc");
		if(order.equals("asc")) {
			Collections.sort(finalcoins, new Comparator<Coin>() {
				@Override
				public int compare(Coin o1, Coin o2) {
					return o1.getBlockCreated().compareTo(o2.getBlockCreated());
				}
			});
		}
		
		//Are we listing a total amount of coins..
		if(existsParam("totalamount")) {
			//How much do we need..
			MiniNumber totalamount = getNumberParam("totalamount");
			
			//Get just this number..
			finalcoins = send.selectCoins(finalcoins, totalamount);
		}
		
		//Put it all in an array
		JSONArray coinarr = new JSONArray();
		for(Coin cc : finalcoins) {
			coinarr.add(cc.toJSON(simplestate));
		}
		
		ret.put("response", coinarr);
	
		return ret;
	}

	@Override
	public Command getFunction() {
		return new coins();
	}

}
