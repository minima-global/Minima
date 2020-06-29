package org.minima.system.brains;

import java.io.File;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.Enumeration;
import java.util.Hashtable;

import org.minima.GlobalParams;
import org.minima.database.MinimaDB;
import org.minima.database.coindb.CoinDBRow;
import org.minima.database.mmr.MMREntry;
import org.minima.database.mmr.MMRSet;
import org.minima.database.txpowdb.TxPOWDBRow;
import org.minima.database.txpowtree.BlockTree;
import org.minima.database.txpowtree.BlockTreeNode;
import org.minima.database.txpowtree.BlockTreePrinter;
import org.minima.database.userdb.UserDB;
import org.minima.database.userdb.java.reltxpow;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.PubPrivKey;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.proofs.TokenProof;
import org.minima.system.Main;
import org.minima.system.input.InputHandler;
import org.minima.system.network.NetClient;
import org.minima.system.network.NetworkHandler;
import org.minima.system.network.minidapps.DAPPManager;
import org.minima.system.network.rpc.RPCClient;
import org.minima.utils.Maths;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class ConsensusPrint extends ConsensusProcessor {
	
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
	public static final String CONSENSUS_TOKENVALIDATE 		= CONSENSUS_PREFIX+"TOKENVALIDATE";
	
	public static final String CONSENSUS_RANDOM 			= CONSENSUS_PREFIX+"RANDOM";
	
	public static final String CONSENSUS_STATUS 			= CONSENSUS_PREFIX+"STATUS";
	public static final String CONSENSUS_PRINTCHAIN 		= CONSENSUS_PREFIX+"PRINTCHAIN";
	
	public static final String CONSENSUS_NETWORK 			= CONSENSUS_PREFIX+"NETWORK";
	
	public static final String CONSENSUS_PRINTCHAIN_TREE 	= CONSENSUS_PREFIX+"PRINTCHAIN_TREE";
	
	public static final String CONSENSUS_MINIDAPPS 			= CONSENSUS_PREFIX+"MINIDAPPS";
	
	/**
	 * The Old Balance that was sent to the listeners..
	 */
	String mOldWebSocketBalance = "";
	
	/**
	 * The Old Status that was sent to the listeners..
	 */
	String mOldWebSocketStatus = "";
	String mOldIBD = "";
	
	public ConsensusPrint(MinimaDB zDB, ConsensusHandler zHandler) {
		super(zDB, zHandler);
	}
	
	public void processMessage(Message zMessage) throws Exception {
	
		if(zMessage.isMessageType(CONSENSUS_PRINTCHAIN)) {
			boolean coins  = zMessage.getBoolean("coins");
			boolean txpow  = zMessage.getBoolean("txpow");
			boolean mmr    = zMessage.getBoolean("mmr");
			boolean tree   = zMessage.getBoolean("tree");
			
			JSONObject dets = InputHandler.getResponseJSON(zMessage);
			
			if(coins) {
				JSONArray coinjson = new JSONArray();
				ArrayList<CoinDBRow> coindb = getMainDB().getCoinDB().getComplete();
				for(CoinDBRow row : coindb) {
					coinjson.add(row.toJSON());
				}
				
				dets.put("coindbsize" , coindb.size()); 
				dets.put("coindb", coinjson);
			}
			
			if(txpow) {
				JSONArray txpowjson = new JSONArray();
				ArrayList<TxPOWDBRow> txpowdb = getMainDB().getTxPowDB().getAllTxPOWDBRow();
				for(TxPOWDBRow row : txpowdb) {
					txpowjson.add(row.toJSON());
				}
				
				dets.put("txpowdbsize" , txpowdb.size()); 
				dets.put("txpowdb", txpowjson);
			}
			
			if(tree) {
//				SimpleBlockTreePrinter treeprint = new SimpleBlockTreePrinter(getMainDB().getMainTree());
				BlockTreePrinter treeprint = new BlockTreePrinter(getMainDB().getMainTree());
				String treeinfo    = treeprint.printtree();
				BlockTree maintree = getMainDB().getMainTree();
		
				//Now check whether they are unspent..
				JSONObject treejson = new JSONObject();
				treejson.put("tree", treeinfo);
				treejson.put("length", maintree.getAsList().size());
				treejson.put("speed", maintree.getChainSpeed());
				treejson.put("difficulty", maintree.getChainTip().getTxPow().getBlockDifficulty().to0xString());
				treejson.put("weight", maintree.getChainRoot().getTotalWeight());
				
				dets.put("tree", treejson);
			}
			
			if(mmr) {
				JSONArray mmrarray = new JSONArray();
				MMRSet set = getMainDB().getMainTree().getChainTip().getMMRSet();
				while(set != null) {
					mmrarray.add(set.toJSON());
					set = set.getParent();	
				}
				dets.put("mmr", mmrarray);
			}
			
			InputHandler.endResponse(zMessage, true, "");
			
		}else if(zMessage.isMessageType(CONSENSUS_PRINTCHAIN_TREE)){
			if(zMessage.exists("auto")) {
				getConsensusHandler().mPrintChain = zMessage.getBoolean("auto");
			}

			BlockTreePrinter treeprint = new BlockTreePrinter(getMainDB().getMainTree());
			String treeinfo = treeprint.printtree();
	
			BlockTree tree = getMainDB().getMainTree();
			
			//DEBUGGING
			if(zMessage.exists("systemout")) {
				treeinfo += "\n\nroot               : ( "+tree.getChainRoot().getTxPow().getBlockNumber()+" ) "+tree.getChainRoot().getTxPow().getTxPowID().to0xString();
				treeinfo += "\ncascade            : ( "+tree.getCascadeNode().getTxPow().getBlockNumber()+" ) "+tree.getCascadeNode().getTxPow().getTxPowID().to0xString();
				treeinfo += "\ntip                : ( "+tree.getChainTip().getTxPow().getBlockNumber()+" ) "+tree.getChainTip().getTxPow().getTxPowID().to0xString();
				treeinfo += "\nlength             : "+tree.getAsList().size();
				treeinfo += "\nSpeed              : "+tree.getChainSpeed()+" blocks / sec";
				treeinfo += "\nCurrent Difficulty : "+tree.getChainTip().getTxPow().getBlockDifficulty().to0xString();
				treeinfo += "\nTotal Weight       : "+tree.getChainRoot().getTotalWeight();

				MinimaLogger.log(treeinfo+"\n");
			
			}else {
				//Now check whether they are unspent..
				JSONObject dets = InputHandler.getResponseJSON(zMessage);
				dets.put("tree", treeinfo);
				dets.put("root", "( "+tree.getChainRoot().getTxPow().getBlockNumber()+" ) "+tree.getChainRoot().getTxPow().getTxPowID().to0xString());
				dets.put("cascade", "( "+tree.getCascadeNode().getTxPow().getBlockNumber()+" ) "+tree.getCascadeNode().getTxPow().getTxPowID().to0xString());
				dets.put("tip", "( "+tree.getChainTip().getTxPow().getBlockNumber()+" ) "+tree.getChainTip().getTxPow().getTxPowID().to0xString());
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
				if(txpowrow.getTxPOW().hasBody()) {
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
			//MMRSet topmmr = topblk.getMMRSet().getParentAtTime(topblk.getTxPow().getBlockNumber().sub(GlobalParams.MINIMA_CONFIRM_DEPTH));
			
			//Get the MEMPOOL COINS
			ArrayList<Coin> mempool = getMainDB().getMempoolCoins();
			
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
							String entry = coinmmr.getEntryNumber().toString();
							if(!addedcoins.contains(entry)) {
								addedcoins.add(entry);
								if(spent == wantspent) {
									
									//If you want them unspent - check against the mempool too..
									if(!wantspent) {
										boolean found = false;
										for(Coin mem : mempool) {
											if(coinmmr.getData().getCoin().getCoinID().isEqual(mem.getCoinID()) ) {
												found = true;
												break;
											}
										}
										
										if(!found) {
											allcoins.add(topmmr.getProof(coinmmr.getEntryNumber()));
										}
									}else {
										allcoins.add(topmmr.getProof(coinmmr.getEntryNumber()));	
									}	
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
		
		}else if(zMessage.isMessageType(CONSENSUS_TOKENVALIDATE)){
			//Check that a Token is valid..
			String tokenid = zMessage.getString("tokenid");
			
			JSONObject dets = InputHandler.getResponseJSON(zMessage);
			dets.put("valid", false);
			
			TokenProof td = getMainDB().getUserDB().getTokenDetail(new MiniData(tokenid));
			if(td == null) {
				InputHandler.endResponse(zMessage, false, "TokenID "+tokenid+" not found");	
				return;
			}
			
			//Get the details..
			String name = td.getName().toString();
			if(!name.startsWith("{")) {
				InputHandler.endResponse(zMessage, false, "No Proof URL attached to token");	
				return;
			}
			
			JSONObject tokjson = td.getNameJSON();
			if(!tokjson.containsKey("proof")) {
				InputHandler.endResponse(zMessage, false, "No Proof URL attached to token");	
				return;
			}
			
			//Get the proof..
			String proof = (String) tokjson.get("proof");
			URL proofurl = new URL(proof);
			
			//Now GET that URL..
			String prooffile = RPCClient.sendGET(proof).trim();
			
			dets.put("proofurl", proof);
			dets.put("host", proofurl.getHost());
			dets.put("returned", prooffile);
			
			boolean valid = prooffile.equals(tokenid);
			dets.put("valid", valid);
			
			//And check that this is equal to the Token ID..
			if(!valid) {
				InputHandler.endResponse(zMessage, true, "Invalid - proof mismatch");	
			}else {
				InputHandler.endResponse(zMessage, true, "Valid - proof matches");
			}
			
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
				onlyaddress = new MiniData(zMessage.getString("address")).to0xString();
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
			basejobj.put("unspent", "true");
			
			full_details.put(Coin.MINIMA_TOKENID.to0xString(), basejobj);
			
			//Now get the balance..
			Hashtable<String, MiniNumber> totals_confirmed   = new Hashtable<>();
			Hashtable<String, MiniNumber> totals_unconfirmed = new Hashtable<>();
			
			UserDB userdb = getMainDB().getUserDB();
			ArrayList<CoinDBRow> coins = getMainDB().getCoinDB().getCompleteRelevant();
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
						if(tokid.equals(Coin.MINIMA_TOKENID.to0xString())) {
							jobj.put("tokenid", tokid);
							jobj.put("token", "Minima");
						}else {
							jobj = td.toJSON();
						}
						
						//Default Values
						jobj.put("unspent", "false");
						jobj.put("confirmed", MiniNumber.ZERO);
						jobj.put("unconfirmed", MiniNumber.ZERO);
						
						//Add it..
						full_details.put(tokid, jobj);
					}
					
					if(!coin.isSpent()) {
						//At least one coin is unspent..
						jobj.put("unspent", "true");
						
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
			JSONArray totbal  = new JSONArray();
			MiniData onlyaddrdata = new MiniData(onlyaddress);
			
			Enumeration<String> fulls = full_details.keys();
			while(fulls.hasMoreElements())  {
				String full = fulls.nextElement();
				
				//Get the JSON object
				JSONObject jobj = full_details.get(full);
				String unspentexist = jobj.get("unspent").toString();
				jobj.remove("unspent");
				
				//Do we add.. only if there are unspent coins..
				if(!unspentexist.equals("true")) {
					continue;
				}
				
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
						if(!onlyaddress.equals("")) {
							if(confc.getAddress().isEqual(onlyaddrdata)) {
								tot_simple = tot_simple.add(confc.getAmount());
							}
						}else {
							tot_simple = tot_simple.add(confc.getAmount());	
						}
					}
					jobj.put("sendable", tot_simple.toString());
				}else {
					TokenProof td = getMainDB().getUserDB().getTokenDetail(tok);
					
					//Now work out the actual amounts..
					MiniNumber tot_conf     = (MiniNumber) jobj.get("confirmed");
					MiniNumber tot_scconf   = tot_conf.mult(td.getScaleFactor());
					MiniNumber tot_unconf   = (MiniNumber) jobj.get("unconfirmed");
					MiniNumber tot_scunconf = tot_unconf.mult(td.getScaleFactor());
					
					//And re-add
					jobj.put("confirmed", tot_scconf.toString());
					jobj.put("unconfirmed", tot_scunconf.toString());
//					jobj.put("script", td.getTokenScript().toString());
//					jobj.put("total", td.getTotalTokens().toString());
					
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
						if(!onlyaddress.equals("")) {
							if(confc.getAddress().isEqual(onlyaddrdata)) {
								tot_simple = tot_simple.add(confc.getAmount());
							}
						}else {
							tot_simple = tot_simple.add(confc.getAmount());	
						}
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
	
			//Do we notify..
			String balancestring = totbal.toString();
			
			//Is this a notification message for the listeners.. only if is the total Balance..
			if(onlyaddress.equals("")) {
				//Same as the old ?
				if(!balancestring.equals(mOldWebSocketBalance)) {
					//Store for later.. 
					mOldWebSocketBalance = balancestring;
					
					MinimaLogger.log("NEW BALANCE : "+mOldWebSocketBalance);
					
					//Send this to the WebSocket..
					JSONObject newbalance = new JSONObject();
					newbalance.put("event","newbalance");
					newbalance.put("balance",totbal);
					
					Message msg = new Message(NetworkHandler.NETWORK_WS_NOTIFY);
					msg.addString("message", newbalance.toString());
					getConsensusHandler().getMainHandler().getNetworkHandler().PostMessage(msg);
				}
			}
			
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
			boolean relevant   = zMessage.getBoolean("relevant");
			
			String address     = zMessage.getString("address");
			String tokenid     = zMessage.getString("tokenid");
			String amount      = zMessage.getString("amount");
			String type        = zMessage.getString("type");
			
			//What gets checked..
			boolean checkaddress  = !address.equals("");
			boolean checktokenid  = !tokenid.equals("");
			boolean checkamount   = !amount.equals("");
			
			if(address.startsWith("Mx")) {
				//It's a Minima Address!
				address = Address.convertMinimaAddress(address).to0xString();
			}
			
			MiniData addr   = new MiniData(address);
			MiniData tok    = new MiniData(tokenid);
			MiniNumber amt  = MiniNumber.ZERO;
			if(checkamount) {
				amt = new MiniNumber(amount);
			}
			
			//Current TIP
			BlockTreeNode tip  	 = getMainDB().getMainTree().getChainTip();
//			MiniNumber minblock  = tip.getTxPow().getBlockNumber().sub(GlobalParams.MINIMA_CONFIRM_DEPTH);
			MMRSet baseset 	     = tip.getMMRSet();//.getParentAtTime(minblock);
			
			//A list of all the found coins..
			JSONArray totcoins  = new JSONArray();
			
			//Cycle..
			ArrayList<CoinDBRow> coins = getMainDB().getCoinDB().getComplete();
			for(CoinDBRow coin : coins) {
				//RELEVANT..
				if(relevant && !coin.isRelevant()) {continue;}
				
				//SPEND TYPE
				if(type.equals("spent") && !coin.isSpent()) {continue;}
				if(type.equals("unspent") && coin.isSpent()) {continue;}
				
				//ADDRESS
				if(checkaddress && !coin.getCoin().getAddress().isEqual(addr)) {
					continue;
				}
				
				//TOKEN
				if(checktokenid && !coin.getCoin().getTokenID().isEqual(tok)) {
					continue;
				}
				
				//AMOUNT
				if(checkamount && !coin.getCoin().getAmount().isEqual(amt)) {
					continue;
				}
				
				//DEEP ENOUGH - for now add any found
//				if(coin.getInBlockNumber().isLessEqual(minblock)) {
					//OK - FOUND ONE!
					totcoins.add(baseset.getProof(coin.getMMREntry()).toJSON());	
//				}
			}
			
			//Add to the main JSON
			JSONObject allcoins = InputHandler.getResponseJSON(zMessage);
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
					TxPoW txpow   = rpow.getTxPOW();
					
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
			
			//Get the row in the database..
			TxPOWDBRow row = getMainDB().getTxPOWRow(txp);
			
			if(row == null) {
				InputHandler.endResponse(zMessage, false, "No TxPOW found for "+txpow);
			}else {
				JSONObject resp = InputHandler.getResponseJSON(zMessage);
				
				//Add details..
				resp.put("txpow", row.getTxPOW().toJSON());
				resp.put("ischainblock", row.isOnChainBlock());
				resp.put("isinblock", row.isInBlock());
				resp.put("inblock", row.getInBlockNumber().toString());
				
				InputHandler.endResponse(zMessage, true, "");
			}
			
//			TxPoW pow = getMainDB().getTxPOW(txp);
//			if(pow == null) {
//				InputHandler.endResponse(zMessage, false, "No TxPOW found for "+txpow);
//			}else {
//				InputHandler.getResponseJSON(zMessage).put("txpow", pow.toJSON());
//				InputHandler.endResponse(zMessage, true, "");
//			}
		
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
			Main main = getConsensusHandler().getMainHandler();
			
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
			long timediff     = System.currentTimeMillis() - getConsensusHandler().getMainHandler().getNodeStartTime();
			String uptime     = Maths.ConvertMilliToTime(timediff);	

			status.put("uptime", uptime);
			status.put("conf", main.getBackupManager().getRootFolder().getAbsolutePath());
			status.put("host", main.getNetworkHandler().getDAPPManager().getHostIP());
			status.put("port", main.getNetworkHandler().getServer().getPort());
			status.put("rpcport", main.getNetworkHandler().getRPCServer().getPort());
			
			status.put("automine", main.getMiner().isAutoMining());
			
			status.put("root", root.getTxPowID().to0xString());
			status.put("tip", tip.getTxPowID().to0xString());
			status.put("total", tip.getTxPow().getMMRTotal().toString());
			
			status.put("lastblock", tip.getTxPow().getBlockNumber().toString());
			status.put("lasttime", new Date(new Long(tip.getTxPow().getTimeMilli()+"")).toString());
			
			status.put("cascade", getMainDB().getMainTree().getCascadeNode().getTxPow().getBlockNumber().toString());
			
			status.put("difficulty", tip.getTxPow().getBlockDifficulty().to0xString());
			
			//COINDB
			status.put("coindb", getMainDB().getCoinDB().getComplete().size());
			
			//TxPOWDB
			status.put("txpowdb", getMainDB().getTxPowDB().getSize());
			
			//Size of the TXPOW DB folder..
			File[] txpows = getConsensusHandler().getMainHandler().getBackupManager().getTxPOWFolder().listFiles();
			long totallen = 0;
			int totnum    = 0;
			if(txpows!=null) {
				for(File txf : txpows) {
					totallen += txf.length();
				}
				totnum = txpows.length;
			}
			status.put("txpowfiles", totnum);
			status.put("txpowfolder", formatSize(totallen));
			
			//MemPool
			ArrayList<TxPOWDBRow> unused = getMainDB().getTxPowDB().getAllUnusedTxPOW();
			status.put("mempooltxn", unused.size());
			status.put("mempoolcoins", getMainDB().getMempoolCoins().size());
			
			//Chain details..
			status.put("chainlength", getMainDB().getMainTree().getAsList().size());
			status.put("chainspeed", getMainDB().getMainTree().getChainSpeed());
			status.put("chainweight", root.getTotalWeight().toString());
			
			//Use the cached Version  - this is a slow operation
			if(!mOldIBD.equals("") && !zMessage.exists("hard")) {
				status.put("IBD", mOldIBD);
			}else {
				int ibd = getMainDB().getIntroSyncSize();
				mOldIBD = formatSize(ibd);
				status.put("IBD", mOldIBD);	
			}
			
			//Add the network connections
			ArrayList<NetClient> nets = main.getNetworkHandler().getNetClients();
			status.put("connections", nets.size());
			
			//Add it to the output
			InputHandler.endResponse(zMessage, true, "");
		
			//Do we notify..
			String statusstring = status.toString();
			
			//Is this a notification message for the listeners..
			if(!statusstring.equals(mOldWebSocketStatus)) {
				//Store for later.. 
				mOldWebSocketStatus = statusstring;
				
				//Send this to the WebSocket..
				JSONObject newblock = new JSONObject();
				newblock.put("event","newblock");
				newblock.put("status",status);
				newblock.put("txpow",tip.getTxPow());
				
				Message msg = new Message(NetworkHandler.NETWORK_WS_NOTIFY);
				msg.addString("message", newblock.toString());
				getConsensusHandler().getMainHandler().getNetworkHandler().PostMessage(msg);
			}
			
		}else if(zMessage.isMessageType(CONSENSUS_NETWORK)){
			//Get the response JSON
			JSONObject network = InputHandler.getResponseJSON(zMessage);
			
			//Add the network connections
			ArrayList<NetClient> nets = getConsensusHandler().getMainHandler().getNetworkHandler().getNetClients();
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
		
		}else if(zMessage.isMessageType(CONSENSUS_MINIDAPPS)){
			String name = ""; 
			if(zMessage.exists("name")) {
				name = zMessage.getString("name");
			}
			
			//Search..
			DAPPManager dapps = getConsensusHandler().getMainHandler().getNetworkHandler().getDAPPManager();
			JSONArray minis   = dapps.getMiniDAPPS();
			
			//Get the response JSON
			JSONObject mdapps = InputHandler.getResponseJSON(zMessage);
			
			if(!name.equals("")) {
				for(Object mdapp : minis) {
					JSONObject jobj = (JSONObject)mdapp;
					if(jobj.get("name").toString().equalsIgnoreCase(name)) {
						mdapps.put("minidapp", jobj);
						InputHandler.endResponse(zMessage, true, "MiniDAPP "+name+" found");
						return;
					}
				}
				
				InputHandler.endResponse(zMessage, false, "MiniDAPP "+name+" not found");
				
			}else {
				mdapps.put("count", minis.size());
				mdapps.put("minidapps", minis);
				
				InputHandler.endResponse(zMessage, true, "");	
			}
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
