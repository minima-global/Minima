package org.minima.system.brains;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Iterator;

import org.minima.GlobalParams;
import org.minima.database.MinimaDB;
import org.minima.database.coindb.CoinDBPrinter;
import org.minima.database.coindb.CoinDBRow;
import org.minima.database.mmr.MMRPrint;
import org.minima.database.mmr.MMRProof;
import org.minima.database.mmr.MMRSet;
import org.minima.database.txpowdb.TxPowDBPrinter;
import org.minima.database.txpowtree.BlockTreeNode;
import org.minima.database.txpowtree.BlockTreePrinter;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.PubPrivKey;
import org.minima.objects.TxPOW;
import org.minima.objects.base.MiniData32;
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
			
			//Now get the balance..
			Hashtable<String, MiniNumber> totals_confirmed   = new Hashtable<>();
			Hashtable<String, MiniNumber> totals_unconfirmed = new Hashtable<>();
			
			ArrayList<CoinDBRow> coins = getMainDB().getCoinDB().getComplete();
			for(CoinDBRow coin : coins) {
				if(coin.isInBlock()) {
					//What Token..
					String     tokid = coin.getCoin().getTokenID().to0xString();
					MiniNumber depth = top.sub(coin.getInBlockNumber());
					
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
						}else {
							//Get the Current total..
							MiniNumber curr = totals_unconfirmed.get(tokid);
							if(curr == null) {curr = MiniNumber.ZERO;}
							
							//Add it..
							curr = curr.add(coin.getCoin().getAmount());
							
							//Re-add..
							totals_unconfirmed.put(tokid, curr);
						}
					}
				}
			}
			
			//All the balances..
			JSONObject allbal = InputHandler.getResponseJSON(zMessage);
			JSONArray totbal = new JSONArray();
			
			//Now create a JSON Object
			Enumeration<String> keys = totals_confirmed.keys();
			while(keys.hasMoreElements())  {
				String key     = keys.nextElement();
				MiniNumber tot = totals_confirmed.get(key);
				
				JSONObject minbal = new JSONObject();
				minbal.put("tokenid", key);
				minbal.put("amount", tot.toString());
				
				totbal.add(minbal);
			}	
			
			//Confirmed
			allbal.put("confirmed", totbal);
			totbal = new JSONArray();
			
			//Now create a JSON Object
			keys = totals_unconfirmed.keys();
			while(keys.hasMoreElements())  {
				String key = keys.nextElement();
				MiniNumber tot = totals_unconfirmed.get(key);
				
				JSONObject minbal = new JSONObject();
				minbal.put("tokenid", key);
				minbal.put("amount", tot.toString());
				
				totbal.add(minbal);
			}
			allbal.put("unconfirmed", totbal);
			
			InputHandler.endResponse(zMessage, true, "");
			
//			MiniNumber unconfirmed_total 		= new MiniNumber();
//			MiniNumber confirmed_total 			= new MiniNumber();
//			MiniNumber top = getMainDB().getTopBlock();
//			ArrayList<CoinDBRow> coins = getMainDB().getCoinDB().getComplete();
//			for(CoinDBRow coin : coins) {
//				if(coin.isInBlock()) {
//					MiniNumber depth = top.sub(coin.getInBlockNumber());
//					if(!coin.isSpent()) {
//						if(depth.isMoreEqual(GlobalParams.MINIMA_CONFIRM_DEPTH)) {
//							confirmed_total = confirmed_total.add(coin.getCoin().getAmount());
//						}else {
//							unconfirmed_total = unconfirmed_total.add(coin.getCoin().getAmount());
//						}
//					}
//				}
//			}
//			
//			//The Object
//			JSONObject minbal = InputHandler.getResponseJSON(zMessage);
//			minbal.put("confirmed", confirmed_total.toString());
//			minbal.put("unconfirmed", unconfirmed_total.toString());
			
		}else if(zMessage.isMessageType(CONSENSUS_COINS)){
			//get the MMR
			BlockTreeNode tip  		= getMainDB().getMainTree().getChainTip();
			MMRSet baseset 			= tip.getMMRSet();
			
			MiniNumber top = getMainDB().getTopBlock();
			ArrayList<CoinDBRow> coins = getMainDB().getCoinDB().getComplete();
			int counter=0;
			for(CoinDBRow coin : coins) {
				if(!coin.isSpent()) {
					InputHandler.getResponseJSON(zMessage).put(counter++, baseset.getProof(coin.getMMREntry()) );
				}
			}
			
			//Add it to the output
			InputHandler.endResponse(zMessage, true, "");
		
		}else if(zMessage.isMessageType(CONSENSUS_TXPOW)){
			String txpow = zMessage.getString("txpow");
			MiniData32 txp = new MiniData32(txpow);
			
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
			status.put("host", main.getNetworkHandler().getServer().getHost());
			status.put("port", main.getNetworkHandler().getServer().getPort());
			status.put("rpcport", main.getNetworkHandler().getRPCServer().getPort());
			status.put("pulse", main.getsimulator().mMiningON);
			
			status.put("root", root.getTxPow().toJSON());
			status.put("tip", tip.getTxPow().toJSON());
			status.put("chainspeed", getMainDB().getMainTree().getChainSpeed());
			
			status.put("lastblock", lastblock.toString());
			status.put("totalpow", root.getTotalWeight().toString());
			
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
			
//			SimpleLogger.log("------------");
//			SimpleLogger.log("Node Details");
//			SimpleLogger.log("------------");
//			SimpleLogger.log("Version   : 0.4");
//			SimpleLogger.log("Uptime    : "+uptime);
//			SimpleLogger.log("Conf      : "+main.getBackupManager().getRootFolder());
//			SimpleLogger.log("Host      : "+main.getNetworkHandler().getServer().getHost());
//			SimpleLogger.log("Port      : "+main.getNetworkHandler().getServer().getPort());
//			SimpleLogger.log("Pulse     : ["+main.getsimulator().mMiningON+"] "+main.getsimulator().mCounter);
//			SimpleLogger.log("ROOT      : "+root);
//			SimpleLogger.log("TIP       : "+tip);
//			SimpleLogger.log("Top Block : "+lastblock);
//			SimpleLogger.log("Total POW : "+root.getTotalWeight());
//			SimpleLogger.log("IBD bytes : "+getMainDB().getIntroSyncSize());
//			
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
			
//			//get the MMR
//			MMRSet baseset = tip.getMMRSet();
//			
//			MiniNumber top = getMainDB().getTopBlock();
//			ArrayList<CoinDBRow> coins = getMainDB().getCoinDB().getComplete();
//			for(CoinDBRow coin : coins) {
//				if(!coin.isSpent()) {
//					//Print it..
//					SimpleLogger.log("Coin       : "+coin.toString());
//					MMRProof proof = baseset.getProof(coin.getMMREntry());
//					SimpleLogger.log("Proof      : "+proof.toString()+" valid:"+baseset.checkProof(proof));
//					if(!coin.getCoin().getTokenID().isExactlyEqual(Coin.MINIMA_TOKENID)) {
//						SimpleLogger.log("Token      : "+coin.getCoin().getTokenID());
//					}
//					SimpleLogger.log("Amount     : "+coin.getCoin().getAmount());
//					SimpleLogger.log("");
//				}
//				
//				if(coin.isInBlock()) {
//					MiniNumber depth = top.sub(coin.getInBlockNumber());
//					if(coin.isSpent()) {
//						if(depth.isMoreEqual(GlobalParams.MINIMA_CONFIRM_DEPTH)) {
//							confirmed_total_spent = confirmed_total_spent.add(coin.getCoin().getAmount());
//						}else {
//							unconfirmed_total_spent = unconfirmed_total_spent.add(coin.getCoin().getAmount());
//						}
//					}else {
//						if(depth.isMoreEqual(GlobalParams.MINIMA_CONFIRM_DEPTH)) {
//							confirmed_total = confirmed_total.add(coin.getCoin().getAmount());
//						}else {
//							unconfirmed_total = unconfirmed_total.add(coin.getCoin().getAmount());
//						}
//					}
//				}
//			}
//			
////			SimpleLogger.log("  UNCONFIRMED SPENT : "+unconfirmed_total_spent);
////			SimpleLogger.log("    CONFIRMED SPENT : "+confirmed_total_spent);
//			SimpleLogger.log("UNCONFIRMED : "+unconfirmed_total);
//			SimpleLogger.log("  CONFIRMED : "+confirmed_total);
//			
//			//Get main
//			ArrayList<NetClient> nets = main.getNetworkHandler().getNetClients();
//			if(nets.size()>0) {
//				SimpleLogger.log("---------------");
//				SimpleLogger.log("Network Clients");
//				SimpleLogger.log("---------------");
//				for(NetClient net : nets) {
//					SimpleLogger.log(""+net);
//				}
//			}
		}
	}
	
	private MiniNumber getIfExists(Hashtable<MiniData32, MiniNumber> zHashTable, MiniData32 zToken) {
		Enumeration<MiniData32> keys = zHashTable.keys();
		
		while(keys.hasMoreElements()) {
			MiniData32 key = keys.nextElement();
			if(key.isExactlyEqual(zToken)) {
				return zHashTable.get(key);	
			}
		}
		
		return null;
	}
	
}
