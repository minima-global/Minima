package org.minima.system.brains;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;

import org.minima.GlobalParams;
import org.minima.database.MinimaDB;
import org.minima.database.coindb.CoinDBPrinter;
import org.minima.database.coindb.CoinDBRow;
import org.minima.database.mmr.MMRPrint;
import org.minima.database.mmr.MMRSet;
import org.minima.database.txpowdb.TxPowDBPrinter;
import org.minima.database.txpowtree.BlockTreeNode;
import org.minima.database.txpowtree.BlockTreePrinter;
import org.minima.database.userdb.UserDB;
import org.minima.database.userdb.java.reltxpow;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.PubPrivKey;
import org.minima.objects.TokenDetails;
import org.minima.objects.TxPOW;
import org.minima.objects.base.MiniHash;
import org.minima.objects.base.MiniNumber;
import org.minima.system.Main;
import org.minima.system.input.InputHandler;
import org.minima.system.network.NetClient;
import org.minima.utils.Maths;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class ConsensusPrint {


	public static final String CONSENSUS_PREFIX 			= "CONSENSUSPRINT_";
	
	public static final String CONSENSUS_BALANCE 			= CONSENSUS_PREFIX+"BALANCE";
	public static final String CONSENSUS_COINS 				= CONSENSUS_PREFIX+"COINS";
	public static final String CONSENSUS_TXPOW 				= CONSENSUS_PREFIX+"TXPOW";
	public static final String CONSENSUS_KEYS 				= CONSENSUS_PREFIX+"KEYS";
	
	public static final String CONSENSUS_HISTORY 		    = CONSENSUS_PREFIX+"HISTORY";
	
	public static final String CONSENSUS_STATUS 			= CONSENSUS_PREFIX+"STATUS";
	public static final String CONSENSUS_PRINTCHAIN 		= CONSENSUS_PREFIX+"PRINTCHAIN";
	
	public static final String CONSENSUS_PRINTCHAIN_TREE 	= CONSENSUS_PREFIX+"PRINTCHAIN_TREE";
	
    MinimaDB mDB;
	
	ConsensusHandler mHandler;
	
	public ConsensusPrint(MinimaDB zDB, ConsensusHandler zHandler) {
		mDB = zDB;
		mHandler = zHandler;
	}
	
	private MinimaDB getMainDB() {
		return mDB;
	}
	
	private ConsensusHandler getHandler() {
		return mHandler;
	}
	
	public void processMessage(Message zMessage) throws Exception {
	
		if(zMessage.isMessageType(CONSENSUS_PRINTCHAIN)) {
			//Print the Tree
			BlockTreePrinter treeprint = new BlockTreePrinter(getMainDB().getMainTree(), false);
			treeprint.printtree();
			
			//Print the TxPowDB
			TxPowDBPrinter.PrintDB(getMainDB().getTxPowDB());
			
			//Print the COinDB
			CoinDBPrinter.Print(getMainDB().getCoinDB());
			
			if(getMainDB().getMainTree().getChainRoot() == null) {
				MinimaLogger.log("NO BLOCKS!");
				return;
			}
			
			//MMR
			MinimaLogger.log("---");
			MinimaLogger.log("MMR");
			MinimaLogger.log("---");
			MMRSet set = getMainDB().getMainTree().getChainTip().getMMRSet();
			MMRPrint.Print(set);
		
		}else if(zMessage.isMessageType(CONSENSUS_PRINTCHAIN_TREE)){
			//Print the Tree
			BlockTreePrinter treeprint = new BlockTreePrinter(getMainDB().getMainTree(), true);
			treeprint.printtree();
		
		}else if(zMessage.isMessageType(CONSENSUS_BALANCE)){
			//Current top block
			MiniNumber top = getMainDB().getTopBlock();
			
			//A complete details of the TokenID..
			Hashtable<String, JSONObject> full_details = new Hashtable<>();
			
			//Add zero for Minima
			JSONObject basejobj = new JSONObject();
			basejobj.put("tokenid", Coin.MINIMA_TOKENID.to0xString());
			basejobj.put("token", "Minima");
			basejobj.put("total", "1000000000");
			basejobj.put("confirmed", MiniNumber.ZERO);
			basejobj.put("unconfirmed", MiniNumber.ZERO);
			full_details.put(Coin.MINIMA_TOKENID.to0xString(), basejobj);
			
			//Now get the balance..
			Hashtable<String, MiniNumber> totals_confirmed   = new Hashtable<>();
			Hashtable<String, MiniNumber> totals_unconfirmed = new Hashtable<>();
			
			UserDB userdb = getMainDB().getUserDB();
			ArrayList<CoinDBRow> coins = getMainDB().getCoinDB().getComplete();
			for(CoinDBRow coin : coins) {
				
				//Is this one of ours ? Could be an import of someone elses 
				boolean rel = userdb.isAddressRelevant(coin.getCoin().getAddress());
				
				if(coin.isInBlock() && rel) {
					//What Token..
					String     tokid 	= coin.getCoin().getTokenID().to0xString();
					MiniHash   tokhash 	= new MiniHash(tokid);
					MiniNumber depth 	= top.sub(coin.getInBlockNumber());
					
					//Get the Token Details.
					TokenDetails td = getMainDB().getUserDB().getTokenDetail(tokhash);
					
					//Get the JSON object for this Token..
					JSONObject jobj = null;
					if(full_details.containsKey(tokid)) {
						jobj = full_details.get(tokid);
					}else {
						jobj = new JSONObject();
						jobj.put("tokenid", tokid);
						if(tokid.equals(Coin.MINIMA_TOKENID.to0xString())) {
							jobj.put("token", "Minima");
						}else {
							jobj.put("token", td.getName().toString());
						}
						
						//Default Values
						jobj.put("confirmed", MiniNumber.ZERO);
						jobj.put("unconfirmed", MiniNumber.ZERO);
						
						//Add it..
						full_details.put(tokid, jobj);
					}
					
					if(!coin.isSpent()) {
						if(depth.isMoreEqual(GlobalParams.MINIMA_CONFIRM_DEPTH)) {
							//Get the Current total..
							MiniNumber curr = totals_confirmed.get(tokid);
							
							if(curr == null) {
								curr = MiniNumber.ZERO;
							}
							
							//Add it..
							curr = curr.add(coin.getCoin().getAmount());
							
							//Re-add..
							totals_confirmed.put(tokid, curr);
							
							//Add to the JSON object
							jobj.put("confirmed", curr);
							
						}else {
							//Get the Current total..
							MiniNumber curr = totals_unconfirmed.get(tokid);
							if(curr == null) {curr = MiniNumber.ZERO;}
							
							//Add it..
							curr = curr.add(coin.getCoin().getAmount());
							
							//Re-add..
							totals_unconfirmed.put(tokid, curr);
							
							//Add to the JSON object
							jobj.put("unconfirmed", curr);
						}
					}
				}
			}
			
//			//All the balances..
//			JSONObject allbal = InputHandler.getResponseJSON(zMessage);
//			JSONArray totbal = new JSONArray();
//			
//			//Now create a JSON Object
//			Enumeration<String> keys = totals_confirmed.keys();
//			while(keys.hasMoreElements())  {
//				String key     = keys.nextElement();
//				MiniNumber tot = totals_confirmed.get(key);
//				
//				//Store to the JSON Object
//				JSONObject minbal = new JSONObject();
//				minbal.put("tokenid", key);
//				
//				//Is this a token amount!
//				if(!key.equals(Coin.MINIMA_TOKENID.to0xString())) {
//					//Create the Token Hash
//					MiniHash tokenid = new MiniHash(key);
//					
//					//Get the token details
//					TokenDetails td = getMainDB().getUserDB().getTokenDetail(tokenid);
//					if(td == null) {
//						//ERROR you own tokens you have no details about
//						minbal.put("token", "ERROR_UNKNOWN");
//						minbal.put("tokenamount", "-1");
//					}else{
//						MiniNumber tottok = tot.mult(td.getScaleFactor());
//						minbal.put("token", td.getName());
//						minbal.put("tokenamount", tottok.toString());	
//					}
//					
//					MiniNumber tottok = totals_confirmed.get(key);
//				}else {
//					minbal.put("token", "Minima");
//					minbal.put("tokenamount", tot.toString());
//				}
//				
//				//And the Amount in Minima
//				minbal.put("amount", tot.toString());
//				
//				//Add to the Total balance sheet
//				totbal.add(minbal);
//			}	
//			
//			//Confirmed
//			allbal.put("confirmed", totbal);
//			totbal = new JSONArray();
//			
//			//Now create a JSON Object
//			keys = totals_unconfirmed.keys();
//			while(keys.hasMoreElements())  {
//				String key = keys.nextElement();
//				MiniNumber tot = totals_unconfirmed.get(key);
//				
//				JSONObject minbal = new JSONObject();
//				minbal.put("tokenid", key);
//				
//				//Is this a token amount!
//				if(!key.equals(Coin.MINIMA_TOKENID.to0xString())) {
//					//Create the Token Hash
//					MiniHash tokenid = new MiniHash(key);
//					
//					//Get the token details
//					TokenDetails td = getMainDB().getUserDB().getTokenDetail(tokenid);
//					if(td == null) {
//						//ERROR you own tokens you have no details about
//						minbal.put("token", "ERROR_UNKNOWN");
//						minbal.put("tokenamount", "-1");
//					}else{
//						MiniNumber tottok = tot.mult(td.getScaleFactor());
//						minbal.put("token", td.getName());
//						minbal.put("tokenamount", tottok.toString());	
//					}
//					
//					MiniNumber tottok = totals_confirmed.get(key);
//				
//				}else {
//					minbal.put("token", "Minima");
//					minbal.put("tokenamount", tot.toString());
//				}
//				
//				//The Amount in Minima
//				minbal.put("amount", tot.toString());
//				
//				totbal.add(minbal);
//			}
//			allbal.put("unconfirmed", totbal);

			//All the balances..
			JSONObject allbal = InputHandler.getResponseJSON(zMessage);
			JSONArray totbal = new JSONArray();
			
			//Tester..
			Enumeration<String> fulls = full_details.keys();
			while(fulls.hasMoreElements())  {
				String full = fulls.nextElement();
				
				//Get the JSON object
				JSONObject jobj = full_details.get(full);
				
				//Get the Token ID
				String tokenid 	= (String) jobj.get("tokenid");
				MiniHash tok 	= new MiniHash(tokenid);
				if(tok.isExactlyEqual(Coin.MINIMA_TOKENID)) {
					//Now work out the actual amounts..
					MiniNumber tot_conf     = (MiniNumber) jobj.get("confirmed");
					MiniNumber tot_unconf   = (MiniNumber) jobj.get("unconfirmed");
					
					//And re-add
					jobj.put("confirmed", tot_conf.toString());
					jobj.put("unconfirmed", tot_unconf.toString());
					jobj.put("total", "1000000000");
				}else {
					TokenDetails td = getMainDB().getUserDB().getTokenDetail(tok);
					
					//Now work out the actual amounts..
					MiniNumber tot_conf     = (MiniNumber) jobj.get("confirmed");
					MiniNumber tot_scconf   = tot_conf.mult(td.getScaleFactor());
					MiniNumber tot_unconf   = (MiniNumber) jobj.get("unconfirmed");
					MiniNumber tot_scunconf = tot_unconf.mult(td.getScaleFactor());
					MiniNumber tot_toks 	= td.getAmount().mult(td.getScaleFactor());
					
					//And re-add
					jobj.put("confirmed", tot_scconf.toString());
					jobj.put("unconfirmed", tot_scunconf.toString());
					jobj.put("total", tot_toks.toString());
				}
				
				//add it to the mix
				totbal.add(jobj);
			}
			
			//Add it to all ball
			allbal.put("balance",totbal);
			
			//All good
			InputHandler.endResponse(zMessage, true, "");
	
		}else if(zMessage.isMessageType(CONSENSUS_COINS)){
			//Return all or some
			String address = "";
			if(zMessage.exists("address")) {
				address = zMessage.getString("address");
			}
			
			//get the MMR
			BlockTreeNode tip  		= getMainDB().getMainTree().getChainTip();
			MMRSet baseset 			= tip.getMMRSet();
			
			JSONObject allcoins = InputHandler.getResponseJSON(zMessage);
			JSONArray totcoins = new JSONArray();
			
			ArrayList<CoinDBRow> coins = getMainDB().getCoinDB().getComplete();
			for(CoinDBRow coin : coins) {
				if(!coin.isSpent()) {
					if(address.equals("")) {
						totcoins.add(baseset.getProof(coin.getMMREntry()).toJSON());	
					}else if(address.equals(coin.getCoin().getAddress().to0xString())) {
						totcoins.add(baseset.getProof(coin.getMMREntry()).toJSON());
					}
				}
			}
			
			//Add to the main JSON
			allcoins.put("coins", totcoins);
			
			//Add it to the output
			InputHandler.endResponse(zMessage, true, "");
		
		}else if(zMessage.isMessageType(CONSENSUS_HISTORY)){
			//Is it a clear..
			if(zMessage.exists("clear")) {
				getMainDB().getUserDB().clearHistory();
			}
			
			//Get the HIstory
			ArrayList<reltxpow> history = getMainDB().getUserDB().getHistory();
			
			//All the relevant transactions..
			JSONObject allbal = InputHandler.getResponseJSON(zMessage);
			JSONArray totbal = new JSONArray();
			
			for(reltxpow rpow : history) {
				totbal.add(rpow.toJSON());
			}
			
			//And add to the final response
			allbal.put("history",totbal);
			
			//All good
			InputHandler.endResponse(zMessage, true, "");
		
		}else if(zMessage.isMessageType(CONSENSUS_TXPOW)){
			String txpow = zMessage.getString("txpow");
			MiniHash txp = new MiniHash(txpow);
			
			TxPOW pow = getMainDB().getTxPOW(txp);
			
			if(pow == null) {
				InputHandler.endResponse(zMessage, false, "No TxPOW found for "+txpow);
			}else {
				InputHandler.getResponseJSON(zMessage).put("txpow", pow.toJSON());
				InputHandler.endResponse(zMessage, true, "");
			}
		
		}else if(zMessage.isMessageType(CONSENSUS_KEYS)){
			//Public Keys
			ArrayList<PubPrivKey> keys = getMainDB().getUserDB().getKeys();
			JSONArray arrpub = new JSONArray();
			for(PubPrivKey key : keys) {
				arrpub.add(key.toString());
			}
			InputHandler.getResponseJSON(zMessage).put("publickeys", arrpub);
			
			//Addresses
			ArrayList<Address> addresses = getMainDB().getUserDB().getAllAddresses();
			JSONArray arraddr = new JSONArray();
			for(Address addr : addresses) {
				arraddr.add(addr.toJSON());
			}
			InputHandler.getResponseJSON(zMessage).put("addresses", arraddr);
			
			InputHandler.endResponse(zMessage, true, "");
			
		}else if(zMessage.isMessageType(CONSENSUS_STATUS)){
			//Main Handler
			Main main = getHandler().getMainHandler();
			
			if(getMainDB().getMainTree().getChainRoot() == null) {
				//Add it to the output
				InputHandler.endResponse(zMessage, false, "No blocks!");
				return;
			}
			
			//What block are we on
			BlockTreeNode tip  		= getMainDB().getMainTree().getChainTip();
			BlockTreeNode root 		= getMainDB().getMainTree().getChainRoot();
			MiniNumber lastblock 	= tip.getTxPow().getBlockNumber();
			
			//Get the response JSON
			JSONObject status = InputHandler.getResponseJSON(zMessage);
			
			//Version
			status.put("version", 0.4);
			
			//Up time..
			long timediff     = System.currentTimeMillis() - getHandler().getMainHandler().getNodeStartTime();
			String uptime     = Maths.ConvertMilliToTime(timediff);	
			status.put("milliuptime", timediff);
			status.put("stringuptime", uptime);
			status.put("conf", main.getBackupManager().getRootFolder());
			status.put("host", main.getNetworkHandler().getRPCServer().getHost());
			status.put("port", main.getNetworkHandler().getServer().getPort());
			status.put("rpcport", main.getNetworkHandler().getRPCServer().getPort());
			
			status.put("root", root.getTxPow().toJSON());
			status.put("tip", tip.getTxPow().toJSON());
			status.put("chainspeed", getMainDB().getMainTree().getChainSpeed());
			
			status.put("lastblock", lastblock.toString());
			status.put("totalpow", root.getTotalWeight().toString());
			
			status.put("IBD ", +getMainDB().getIntroSyncSize());
			
			//Add the network connections
			ArrayList<NetClient> nets = main.getNetworkHandler().getNetClients();
			JSONArray netarr = new JSONArray();
			if(nets.size()>0) {
				for(NetClient net : nets) {
					netarr.add(net.toJSON());
				}
				
			}
			status.put("network", netarr);
			
			//Add it to the output
			InputHandler.endResponse(zMessage, true, "");
			
			if(true) {
				return;	
			}

			MinimaLogger.log("");
			MinimaLogger.log("-----");
			MinimaLogger.log("Wallet");
			MinimaLogger.log("-----");
			
			//Public Keys
			ArrayList<PubPrivKey> keys = getMainDB().getUserDB().getKeys();
			for(PubPrivKey key : keys) {
				MinimaLogger.log("Public Key : "+key.toString());
			}
			if(keys.size()>0) {MinimaLogger.log("------------");}
			
			//Addresses
			ArrayList<Address> addresses = getMainDB().getUserDB().getAllAddresses();
			for(Address addr : addresses) {
				MinimaLogger.log("Address    : "+addr.toFullString());
			}
			if(addresses.size()>0) {MinimaLogger.log("------------");}
			
			MiniNumber unconfirmed_total 		= new MiniNumber();
			MiniNumber unconfirmed_total_spent 	= new MiniNumber();
			MiniNumber confirmed_total 			= new MiniNumber();
			MiniNumber confirmed_total_spent 	= new MiniNumber();
			
		}
	}
	
	private MiniNumber getIfExists(Hashtable<MiniHash, MiniNumber> zHashTable, MiniHash zToken) {
		Enumeration<MiniHash> keys = zHashTable.keys();
		
		while(keys.hasMoreElements()) {
			MiniHash key = keys.nextElement();
			if(key.isExactlyEqual(zToken)) {
				return zHashTable.get(key);	
			}
		}
		
		return null;
	}
	
}
