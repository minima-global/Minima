package org.minima.system.brains;

import java.io.File;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.Enumeration;
import java.util.Hashtable;

import org.minima.GlobalParams;
import org.minima.database.MinimaDB;
import org.minima.database.coindb.CoinDBPrinter;
import org.minima.database.coindb.CoinDBRow;
import org.minima.database.mmr.MMREntry;
import org.minima.database.mmr.MMRPrint;
import org.minima.database.mmr.MMRSet;
import org.minima.database.txpowdb.TxPOWDBRow;
import org.minima.database.txpowdb.TxPowDBPrinter;
import org.minima.database.txpowtree.BlockTree;
import org.minima.database.txpowtree.BlockTreeNode;
import org.minima.database.txpowtree.SimpleBlockTreePrinter;
import org.minima.database.userdb.UserDB;
import org.minima.database.userdb.java.reltxpow;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.PubPrivKey;
import org.minima.objects.TxPOW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniInteger;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.proofs.TokenProof;
import org.minima.system.Main;
import org.minima.system.input.InputHandler;
import org.minima.system.network.NetClient;
import org.minima.utils.Maths;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class ConsensusPrint {

	public class chartpoint{
		public long mBlock;
		public long mTotalWeight;
		
		public chartpoint(long zBlock, long zWeight) {
			mBlock = zBlock;
			mTotalWeight = zWeight;
		}
	}
	ArrayList<chartpoint> mChart = new ArrayList<>();

	
	
	public static final String CONSENSUS_PREFIX 			= "CONSENSUSPRINT_";
	
	public static final String CONSENSUS_BALANCE 			= CONSENSUS_PREFIX+"BALANCE";
	
	public static final String CONSENSUS_COINS 				= CONSENSUS_PREFIX+"COINS";
	public static final String CONSENSUS_COINSIMPLE 		= CONSENSUS_PREFIX+"COINSIMPLE";
	
	public static final String CONSENSUS_TXPOW 				= CONSENSUS_PREFIX+"TXPOW";
	public static final String CONSENSUS_KEYS 				= CONSENSUS_PREFIX+"KEYS";
	public static final String CONSENSUS_ADDRESSES 			= CONSENSUS_PREFIX+"ADDRESSES";
	public static final String CONSENSUS_SEARCH 			= CONSENSUS_PREFIX+"SEARCH";
	public static final String CONSENSUS_TXPOWSEARCH 		= CONSENSUS_PREFIX+"TXPOWSEARCH";
	
	public static final String CONSENSUS_HISTORY 		    = CONSENSUS_PREFIX+"HISTORY";
	public static final String CONSENSUS_TOKENS 			= CONSENSUS_PREFIX+"TOKENS";
	
	public static final String CONSENSUS_RANDOM 			= CONSENSUS_PREFIX+"RANDOM";
	
	public static final String CONSENSUS_STATUS 			= CONSENSUS_PREFIX+"STATUS";
	public static final String CONSENSUS_PRINTCHAIN 		= CONSENSUS_PREFIX+"PRINTCHAIN";
	
	public static final String CONSENSUS_NETWORK 			= CONSENSUS_PREFIX+"NETWORK";
	
	public static final String CONSENSUS_PRINTCHAIN_TREE 	= CONSENSUS_PREFIX+"PRINTCHAIN_TREE";
	
	public static final String CONSENSUS_ADDCHARTPOINT 		= CONSENSUS_PREFIX+"ADDCHARTPOINT";
	public static final String CONSENSUS_OUTPUTCHART 		= CONSENSUS_PREFIX+"OUTPUTCHART";
	public static final String CONSENSUS_CLEARCHART 		= CONSENSUS_PREFIX+"CLEARCHART";
	
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
		
		}else if(zMessage.isMessageType(CONSENSUS_ADDCHARTPOINT)){
			long block  = Long.parseLong(zMessage.getString("block"));
			long weight = Long.parseLong(zMessage.getString("weight"));
			mChart.add(new chartpoint(block, weight));
			
		}else if(zMessage.isMessageType(CONSENSUS_OUTPUTCHART)){
			String filename = "chart.csv";//zMessage.getString("filename");
			File chart = new File(System.getProperty("user.home"),filename);
			if(chart.exists()) {
				chart.delete();
			}
			
			PrintWriter pw = new PrintWriter(chart);
			pw.println("Block,Weight,");
			for(chartpoint point : mChart) {
				pw.println(point.mBlock+","+point.mTotalWeight+",");
			}
			
			pw.flush();
			pw.close();
			
			InputHandler.endResponse(zMessage, true, "Chart output to "+chart.getAbsolutePath());
			
		}else if(zMessage.isMessageType(CONSENSUS_CLEARCHART)){
			mChart.clear();
		
		}else if(zMessage.isMessageType(CONSENSUS_PRINTCHAIN_TREE)){
			if(zMessage.exists("auto")) {
				getHandler().getMainHandler().getConsensusHandler().mPrintChain = zMessage.getBoolean("auto");
			}

			SimpleBlockTreePrinter treeprint = new SimpleBlockTreePrinter(getMainDB().getMainTree());
			String treeinfo = treeprint.printtree();
	
			BlockTree tree = getMainDB().getMainTree();
			
			//DEBUGGING
			if(zMessage.exists("systemout")) {
//				BlockTreePrinter2.clearScreen();
				
				treeinfo += "\n\nSpeed              : "+tree.getChainSpeed()+" blocks / sec";
				treeinfo += "\nCurrent Difficulty : "+tree.getChainTip().getTxPow().getBlockDifficulty().to0xString();
				treeinfo += "\nTotal Weight       : "+tree.getChainRoot().getTotalWeight();

				MinimaLogger.log(treeinfo);
			}else {
				//Now check whether they are unspent..
				JSONObject dets = InputHandler.getResponseJSON(zMessage);
				dets.put("tree", treeinfo);
				dets.put("length", tree.getAsList().size());
				dets.put("speed", tree.getChainSpeed());
				dets.put("difficulty", tree.getChainTip().getTxPow().getBlockDifficulty().to0xString());
				dets.put("weight", tree.getChainRoot().getTotalWeight());
				InputHandler.endResponse(zMessage, true, "");	
			}
		
		}else if(zMessage.isMessageType(CONSENSUS_TXPOWSEARCH)){
			String inputaddr   = zMessage.getString("input");
			String outputaddr  = zMessage.getString("output");
			String tokenid     = zMessage.getString("tokenid");
			
			if(inputaddr.startsWith("Mx")) {
				//It's a Minima Address!
				inputaddr = Address.convertMinimaAddress(inputaddr).to0xString();
			}
			if(outputaddr.startsWith("Mx")) {
				//It's a Minima Address!
				outputaddr = Address.convertMinimaAddress(outputaddr).to0xString();
			}
			
			MiniData inaddr   = new MiniData(inputaddr);
			MiniData outaddr  = new MiniData(outputaddr);
			MiniData tokendat = new MiniData(tokenid);
			
			//What gets checked..
			boolean checkinput  = !inputaddr.equals("");
			boolean checkoutput = !outputaddr.equals("");
			boolean checktoken  = !tokenid.equals("");
			
			//The ones we find..
			JSONArray txpowlist = new JSONArray();
			
			//Get all the TXPOWDB
			UserDB udb = getMainDB().getUserDB();
			ArrayList<TxPOWDBRow> alltxpow = getMainDB().getTxPowDB().getAllTxPOWDBRow();
			for(TxPOWDBRow txpowrow : alltxpow) {
				boolean found = false;
				
				//Do we check the inputs..
				if(!found && (checkinput || checktoken)) {
					ArrayList<Coin> inputs = txpowrow.getTxPOW().getTransaction().getAllInputs();
					for(Coin input : inputs) {
						
						if(checkinput && input.getAddress().isEqual(inaddr)) {
							found = true;
							break;
						}
						
						if(checktoken) {
							if(input.getTokenID().isEqual(tokendat)) {
								found = true;
								break;
							}
						}
					}
				}
				
				//Do we check the outputs
				if(!found && (checkoutput || checktoken)) {
					ArrayList<Coin> outputs = txpowrow.getTxPOW().getTransaction().getAllOutputs();
					for(Coin output : outputs) {
						
						if(checkoutput && output.getAddress().isEqual(outaddr)) {
							found = true;
							break;
						}
						
						if(checktoken) {
							if(output.getTokenID().isEqual(tokendat)) {
								found = true;
								break;
							}
						}
					}
				}
				
				//Do we keep and check it..
				if(found) {
					//Create a JSON Object
					JSONObject txp = new JSONObject();
					
					//Is it relevant to us..?
					boolean relevant = udb.isTransactionRelevant(txpowrow.getTxPOW().getTransaction());
					txp.put("relevant", relevant);
					JSONArray values = new JSONArray();
					if(relevant) {
						//Add the Value Transfer Amounts..
						Hashtable<String, MiniNumber> tokamt = getMainDB().getTransactionTokenAmounts(txpowrow.getTxPOW());
						
						//And add.. 
						Enumeration<String> tokens = tokamt.keys();
						while(tokens.hasMoreElements()) {
							String tok     = tokens.nextElement();
							MiniNumber amt = tokamt.get(tok);
							
							JSONObject value = new JSONObject();
							value.put("token",tok);
							value.put("value",amt);
							
							values.add(value);
						}
					}
					txp.put("values", values);
					
					//Details
					boolean isin = txpowrow.isInBlock();
					txp.put("isinblock", isin);
					if(isin) {
						txp.put("inblock", txpowrow.getInBlockNumber().toString());	
					}else {
						txp.put("inblock", "-1");
					}
					txp.put("txpow", txpowrow.getTxPOW().toJSON());
					
					txpowlist.add(txp);
				}
			}
			
			JSONObject finds = InputHandler.getResponseJSON(zMessage);
			finds.put("txpowlist", txpowlist);
			InputHandler.endResponse(zMessage, true, "");
			
		}else if(zMessage.isMessageType(CONSENSUS_SEARCH)){
			String address = zMessage.getString("address");
			if(address.startsWith("Mx")) {
				//It's a Minima Address!
				address = Address.convertMinimaAddress(address).to0xString();
			}
			MiniData addr     = new MiniData(address);
			
			boolean wantspent = zMessage.getBoolean("spent");
			
			//Now search for that address..
			BlockTreeNode topblk = getMainDB().getMainTree().getChainTip();
			JSONArray allcoins = new JSONArray();
			
			MMRSet topmmr = topblk.getMMRSet();
//			MMRSet topmmr = topblk.getMMRSet().
//					getParentAtTime(topblk.getTxPow().getBlockNumber().sub(GlobalParams.MINIMA_CONFIRM_DEPTH));
			
			MMRSet mmrset = topmmr;
			ArrayList<String> addedcoins = new ArrayList<String>();
			while(mmrset != null) {
				//Search for the address..
				ArrayList<MMREntry> zero = mmrset.getZeroRow();
				
				for(MMREntry coinmmr : zero) {
					if(!coinmmr.getData().isHashOnly()) {
						boolean okaddress = coinmmr.getData().getCoin().getAddress().isEqual(addr);
						if(okaddress) {
							//OK - is it spent.. 	
							boolean spent     = coinmmr.getData().isSpent();
							
							//Add this entry..
							String entry = coinmmr.getEntry().toString();
							if(!addedcoins.contains(entry)) {
								addedcoins.add(entry);
								if(spent == wantspent) {
									allcoins.add(topmmr.getProof(coinmmr.getEntry()));	
								}
							}
						}
					}
				}
				
				//Get the parent..
				mmrset = mmrset.getParent();
			}
			
			//Now check whether they are unspent..
			JSONObject dets = InputHandler.getResponseJSON(zMessage);
			dets.put("coins", allcoins);
			InputHandler.endResponse(zMessage, true, "");
		
		}else if(zMessage.isMessageType(CONSENSUS_RANDOM)){
			int len = zMessage.getInteger("length");
			
			MiniData rand = MiniData.getRandomData(len);
			
			JSONObject dets = InputHandler.getResponseJSON(zMessage);
			dets.put("random", rand.to0xString());
			InputHandler.endResponse(zMessage, true, "");
			
		}else if(zMessage.isMessageType(CONSENSUS_TOKENS)){
			//Get all the tokens..
			ArrayList<TokenProof> tokens = getMainDB().getUserDB().getAllKnownTokens();
			
			JSONArray tokarray = new JSONArray();
			
			JSONObject baseobj = new JSONObject();
			baseobj.put("tokenid", Coin.MINIMA_TOKENID.to0xString());
			baseobj.put("token", "Minima");
			baseobj.put("total", "1000000000");
			tokarray.add(baseobj);
			
			for(TokenProof tok : tokens) {
				tokarray.add(tok.toJSON());	
			}
			
			JSONObject dets = InputHandler.getResponseJSON(zMessage);
			dets.put("tokens", tokarray);
			InputHandler.endResponse(zMessage, true, "");
			
		}else if(zMessage.isMessageType(CONSENSUS_BALANCE)){
			//Is this for a single address
			String onlyaddress = "";
			if(zMessage.exists("address")) {
				onlyaddress = new MiniData(zMessage.getString("address")).to0xString() ;
			}
			
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
			basejobj.put("mempool", MiniNumber.ZERO.toString());
			basejobj.put("sendable", MiniNumber.ZERO.toString());
			
			full_details.put(Coin.MINIMA_TOKENID.to0xString(), basejobj);
			
			//Now get the balance..
			Hashtable<String, MiniNumber> totals_confirmed   = new Hashtable<>();
			Hashtable<String, MiniNumber> totals_unconfirmed = new Hashtable<>();
			
			UserDB userdb = getMainDB().getUserDB();
			ArrayList<CoinDBRow> coins = getMainDB().getCoinDB().getComplete();
			for(CoinDBRow coin : coins) {
				//Is this one of ours ? Could be an import or someone elses 
				boolean rel = userdb.isAddressRelevant(coin.getCoin().getAddress());
				
				//Are we only checking one address
				if(!onlyaddress.equals("")) {
					rel = rel && ( coin.getCoin().getAddress().to0xString().equals(onlyaddress) );
				}
				
				if(coin.isInBlock() && rel) {
					//What Token..
					String     tokid 	= coin.getCoin().getTokenID().to0xString();
					MiniData   tokhash 	= new MiniData(tokid);
					MiniNumber blknum   = coin.getInBlockNumber();
					MiniNumber depth 	= top.sub(blknum);
					
					//Get the Token Details.
					TokenProof td = getMainDB().getUserDB().getTokenDetail(tokhash);
					
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
			
			//Get all the mempool amounts..
			Hashtable<String, MiniNumber> mempool = getMainDB().getTotalUnusedAmount();
			
			//All the balances..
			JSONObject allbal = InputHandler.getResponseJSON(zMessage);
			JSONArray totbal = new JSONArray();
			
			Enumeration<String> fulls = full_details.keys();
			while(fulls.hasMoreElements())  {
				String full = fulls.nextElement();
				
				//Get the JSON object
				JSONObject jobj = full_details.get(full);
				
				//Get the Token ID
				String tokenid 	= (String) jobj.get("tokenid");
				MiniData tok 	= new MiniData(tokenid);
				if(tok.isEqual(Coin.MINIMA_TOKENID)) {
					//Now work out the actual amounts..
					MiniNumber tot_conf     = (MiniNumber) jobj.get("confirmed");
					MiniNumber tot_unconf   = (MiniNumber) jobj.get("unconfirmed");
					
					//And re-add
					jobj.put("confirmed", tot_conf.toString());
					jobj.put("unconfirmed", tot_unconf.toString());
					jobj.put("total", "1000000000");
					
					//MEMPOOL
					MiniNumber memp = mempool.get(Coin.MINIMA_TOKENID.to0xString());
					if(memp == null) {
						memp = MiniNumber.ZERO;
					}
					jobj.put("mempool", memp.toString());
					
					//SIMPLE SENDS
					MiniNumber tot_simple = MiniNumber.ZERO;
					ArrayList<Coin> confirmed = getMainDB().getTotalSimpleSpendableCoins(Coin.MINIMA_TOKENID);
					for(Coin confc : confirmed) {
						tot_simple = tot_simple.add(confc.getAmount());
					}
					jobj.put("sendable", tot_simple.toString());
				}else {
					TokenProof td = getMainDB().getUserDB().getTokenDetail(tok);
					
					//Now work out the actual amounts..
					MiniNumber tot_conf     = (MiniNumber) jobj.get("confirmed");
					MiniNumber tot_scconf   = tot_conf.mult(td.getScaleFactor());
					MiniNumber tot_unconf   = (MiniNumber) jobj.get("unconfirmed");
					MiniNumber tot_scunconf = tot_unconf.mult(td.getScaleFactor());
					MiniNumber tot_toks 	= td.getAmount().mult(td.getScaleFactor());
					
					//And re-add
					jobj.put("confirmed", tot_scconf.toString());
					jobj.put("unconfirmed", tot_scunconf.toString());
					jobj.put("script", td.getTokenScript().toString());
					jobj.put("total", tot_toks.toString());
					
					//MEMPOOL
					MiniNumber memp = mempool.get(tok.to0xString());
					if(memp == null) {
						memp = MiniNumber.ZERO;
					}
					jobj.put("mempool", memp.mult(td.getScaleFactor()).toString());
					
					//SIMPLE SENDS
					MiniNumber tot_simple = MiniNumber.ZERO;
					ArrayList<Coin> confirmed = getMainDB().getTotalSimpleSpendableCoins(tok);
					for(Coin confc : confirmed) {
						tot_simple = tot_simple.add(confc.getAmount());
					}
					jobj.put("sendable", tot_simple.mult(td.getScaleFactor()).toString());
				}
				
				//add it to the mix
				totbal.add(jobj);
			}
			
			//Sort by name
			Collections.sort(totbal,new Comparator<JSONObject>() {
				@Override
				public int compare(JSONObject o1, JSONObject o2) {
					String tok1id = (String) o1.get("tokenid");
					String tok2id = (String) o2.get("tokenid");
					if(tok1id.equals("0x00")) {
						return -1;
					}else if(tok2id.equals("0x00")) {
						return 1;
					}
					
					String tok1 = (String) o1.get("token");
					String tok2 = (String) o2.get("token");
					return tok1.compareTo(tok2);
				} 
			});
			
			
			//Add it to all ball
			allbal.put("balance",totbal);
			
			//All good
			InputHandler.endResponse(zMessage, true, "");
	
		}else if(zMessage.isMessageType(CONSENSUS_COINSIMPLE)){
			//Which token..
			String tokenid = zMessage.getString("tokenid");
			
			//get the MMR
			BlockTreeNode tip  		= getMainDB().getMainTree().getChainTip();
			MMRSet baseset 			= tip.getMMRSet();
			
			JSONObject allcoins = InputHandler.getResponseJSON(zMessage);
			JSONArray totcoins = new JSONArray();
			
			ArrayList<Coin> coins = getMainDB().getTotalSimpleSpendableCoins(new MiniData(tokenid));
			for(Coin coin : coins) {
				//Get the Public Key..
				MiniData pubk = getMainDB().getUserDB().getPublicKeyForSimpleAddress(coin.getAddress());
				
				JSONObject simplecoin = new JSONObject();
				simplecoin.put("coin",coin.toJSON());
				simplecoin.put("key",pubk.to0xString());
				
				totcoins.add(simplecoin);
			}
			
			//Add to the main JSON
			allcoins.put("coins", totcoins);
			
			//Add it to the output
			InputHandler.endResponse(zMessage, true, "");
		
		}else if(zMessage.isMessageType(CONSENSUS_COINS)){
			//Return all or some
			String address = "";
			if(zMessage.exists("address")) {
				address = zMessage.getString("address");
				if(address.startsWith("0x")) {
					//It's a regular HASH address
					address = new MiniData(address).to0xString();
				}else if(address.startsWith("Mx")) {
					//It's a Minima Address!
					address = Address.convertMinimaAddress(address).to0xString();
				}
			}
			
			//What type..
			String type = "unspent";
			if(zMessage.exists("type")) {
				type = zMessage.getString("type");
			}
			
			//get the MMR
			BlockTreeNode tip  		= getMainDB().getMainTree().getChainTip();
			MMRSet baseset 			= tip.getMMRSet();
			
			JSONObject allcoins = InputHandler.getResponseJSON(zMessage);
			JSONArray totcoins = new JSONArray();
			
			ArrayList<CoinDBRow> coins = getMainDB().getCoinDB().getComplete();
			for(CoinDBRow coin : coins) {
				boolean docheck = false;
				if(type.equals("unspent") && !coin.isSpent()) {
					docheck = true;
				}
				
				if(type.equals("spent") && coin.isSpent()) {
					docheck = true;
				}
				
				if(type.equals("all")) {
					docheck = true;
				}
				
				//Do we even check it..
				if(docheck) {
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
			
			boolean useaddress = false;
			MiniData addr = null;
			if(zMessage.exists("address")) {
				useaddress = true;
				String address = zMessage.getString("address");
				if(address.startsWith("0x")) {
					//It's a regular HASH address
					addr = new MiniData(address);
				}else if(address.startsWith("Mx")) {
					//It's a Minima Address!
					addr = Address.convertMinimaAddress(address);
				}
			}
			
			//Get the History
			ArrayList<reltxpow> history = getMainDB().getUserDB().getHistory();
			
			//All the relevant transactions..
			JSONObject allbal = InputHandler.getResponseJSON(zMessage);
			JSONArray totbal = new JSONArray();
			
			for(reltxpow rpow : history) {
				if(useaddress) {
					boolean found = false;
					TxPOW txpow   = rpow.getTxPOW();
					
					ArrayList<Coin> inputs = txpow.getTransaction().getAllInputs();
					for(Coin in : inputs) {
						if(in.getAddress().isEqual(addr)) {
							found = true;
							break;
						}
					}
					
//					if(!found) {
//						ArrayList<Coin> outputs = txpow.getTransaction().getAllOutputs();
//						for(Coin out : outputs) {
//							if(in.getAddress().isEqual(addr)) {
//								found = true;
//								break;
//							}
//						}	
//					}
					
					if(found) {
						totbal.add(rpow.toJSON(getMainDB()));
					}
					
				}else {
					totbal.add(rpow.toJSON(getMainDB()));	
				}
			}
			
			//And add to the final response
			allbal.put("history",totbal);
			
			//All good
			InputHandler.endResponse(zMessage, true, "");
		
		}else if(zMessage.isMessageType(CONSENSUS_TXPOW)){
			String txpow = zMessage.getString("txpow");
			MiniData txp = new MiniData(txpow);
			
			TxPOW pow = getMainDB().getTxPOW(txp);
			
			if(pow == null) {
				InputHandler.endResponse(zMessage, false, "No TxPOW found for "+txpow);
			}else {
				InputHandler.getResponseJSON(zMessage).put("txpow", pow.toJSON());
				InputHandler.endResponse(zMessage, true, "");
			}
		
		}else if(zMessage.isMessageType(CONSENSUS_ADDRESSES)){
			//Addresses
			ArrayList<Address> addresses = getMainDB().getUserDB().getAllAddresses();
			JSONArray arraddr = new JSONArray();
			for(Address addr : addresses) {
				arraddr.add(addr.toJSON());
			}
			InputHandler.getResponseJSON(zMessage).put("addresses", arraddr);
			InputHandler.endResponse(zMessage, true, "");
			
		}else if(zMessage.isMessageType(CONSENSUS_KEYS)){
			//Public Keys
			ArrayList<PubPrivKey> keys = getMainDB().getUserDB().getKeys();
			JSONArray arrpub = new JSONArray();
			for(PubPrivKey key : keys) {
				arrpub.add(key.toJSON());
			}
			InputHandler.getResponseJSON(zMessage).put("publickeys", arrpub);
			
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
			status.put("version", GlobalParams.MINIMA_VERSION);
			status.put("time", new Date().toString());
			
			//Up time..
			long timediff     = System.currentTimeMillis() - getHandler().getMainHandler().getNodeStartTime();
			String uptime     = Maths.ConvertMilliToTime(timediff);	

			status.put("uptime", uptime);
			status.put("conf", main.getBackupManager().getRootFolder());
			status.put("host", main.getNetworkHandler().getRPCServer().getHost());
			status.put("port", main.getNetworkHandler().getServer().getPort());
			status.put("rpcport", main.getNetworkHandler().getRPCServer().getPort());
			
			status.put("automine", main.getMiner().isAutoMining());
			
			status.put("root", root.getTxPowID().to0xString());
			status.put("tip", tip.getTxPowID().to0xString());
			status.put("total", tip.getTxPow().getMMRTotal().toString());
			
			status.put("lastblock", tip.getTxPow().getBlockNumber());
			status.put("lasttime", new Date(tip.getTxPow().getTimeSecs().getAsLong()*1000).toString());
			status.put("difficulty", tip.getTxPow().getBlockDifficulty().to0xString());
			
			status.put("txpowdb", getMainDB().getTxPowDB().getCompleteSize());
			
			status.put("chainlength", getMainDB().getMainTree().getAsList().size());
			status.put("chainspeed", getMainDB().getMainTree().getChainSpeed());
			status.put("chainweight", root.getTotalWeight().toString());
			
			int ibd = getMainDB().getIntroSyncSize();
//			status.put("IBD", ibd);
			status.put("IBD", formatSize(ibd));
			
			//Add the network connections
			ArrayList<NetClient> nets = main.getNetworkHandler().getNetClients();
			status.put("connections", nets.size());
			
			//Add it to the output
			InputHandler.endResponse(zMessage, true, "");
		
		}else if(zMessage.isMessageType(CONSENSUS_NETWORK)){
			//Get the response JSON
			JSONObject network = InputHandler.getResponseJSON(zMessage);
			
			//Add the network connections
			ArrayList<NetClient> nets = getHandler().getMainHandler().getNetworkHandler().getNetClients();
			network.put("connections", nets.size());
			
			JSONArray netarr = new JSONArray();
			if(nets.size()>0) {
				for(NetClient net : nets) {
					netarr.add(net.toJSON());
				}
				
			}
			network.put("network", netarr);
			
			//Add it to the output
			InputHandler.endResponse(zMessage, true, "");
		}
	}
	
	public static String formatSize(long v) {
	    if (v < 1024) return v + " bytes";
	    int z = (63 - Long.numberOfLeadingZeros(v)) / 10;
	    return String.format("%.1f %sB", (double)v / (1L << (z*10)), " KMGTPE".charAt(z));
	}
	
//	private MiniNumber getIfExists(Hashtable<MiniData, MiniNumber> zHashTable, MiniData zToken) {
//		Enumeration<MiniData> keys = zHashTable.keys();
//		
//		while(keys.hasMoreElements()) {
//			MiniData key = keys.nextElement();
//			if(key.isExactlyEqual(zToken)) {
//				return zHashTable.get(key);	
//			}
//		}
//		
//		return null;
//	}
	
}
