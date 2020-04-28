package org.minima.system.brains;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.StringTokenizer;

import org.minima.GlobalParams;
import org.minima.database.MinimaDB;
import org.minima.database.coindb.CoinDBRow;
import org.minima.database.mmr.MMRData;
import org.minima.database.mmr.MMREntry;
import org.minima.database.mmr.MMRProof;
import org.minima.database.mmr.MMRSet;
import org.minima.database.txpowdb.TxPOWDBRow;
import org.minima.database.txpowdb.TxPowDB;
import org.minima.kissvm.Contract;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.ScriptValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.PubPrivKey;
import org.minima.objects.StateVariable;
import org.minima.objects.Transaction;
import org.minima.objects.TxPOW;
import org.minima.objects.Witness;
import org.minima.objects.base.MMRSumNumber;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniInteger;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniScript;
import org.minima.objects.proofs.ScriptProof;
import org.minima.system.input.InputHandler;
import org.minima.system.network.NetClient;
import org.minima.system.network.NetClientReader;
import org.minima.system.network.NetworkHandler;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class ConsensusUser {


	public static final String CONSENSUS_PREFIX 			= "CONSENSUSUSER_";
	
	public static final String CONSENSUS_NEWKEY 			= CONSENSUS_PREFIX+"NEWKEY";
	
	public static final String CONSENSUS_SIGN 			    = CONSENSUS_PREFIX+"SIGN";
	
	public static final String CONSENSUS_NEWSIMPLE 			= CONSENSUS_PREFIX+"NEWSIMPLE";
	public static final String CONSENSUS_NEWSCRIPT 			= CONSENSUS_PREFIX+"NEWSCRIPT";
	public static final String CONSENSUS_EXTRASCRIPT 		= CONSENSUS_PREFIX+"EXTRASCRIPT";
	public static final String CONSENSUS_RUNSCRIPT 			= CONSENSUS_PREFIX+"RUNSCRIPT";
	public static final String CONSENSUS_CLEANSCRIPT 		= CONSENSUS_PREFIX+"CLEANSCRIPT";
	
	public static final String CONSENSUS_KEEPCOIN 			= CONSENSUS_PREFIX+"KEEPCOIN";
	public static final String CONSENSUS_UNKEEPCOIN 		= CONSENSUS_PREFIX+"UNKEEPCOIN";
	
	public static final String CONSENSUS_CHECK 		        = CONSENSUS_PREFIX+"CHECK";
	
	public static final String CONSENSUS_FLUSHMEMPOOL 		= CONSENSUS_PREFIX+"FLUSHMEMPOOL";
	
	public static final String CONSENSUS_EXPORTKEY 			= CONSENSUS_PREFIX+"EXPORTKEY";
	public static final String CONSENSUS_IMPORTKEY 			= CONSENSUS_PREFIX+"IMPORTKEY";
	public static final String CONSENSUS_EXPORTCOIN 		= CONSENSUS_PREFIX+"EXPORTCOIN";
	public static final String CONSENSUS_IMPORTCOIN 		= CONSENSUS_PREFIX+"IMPORTCOIN";
	
	public static final String CONSENSUS_MMRTREE 		    = CONSENSUS_PREFIX+"MMRTREE";
	
    MinimaDB mDB;
	
	ConsensusHandler mHandler;
	
	public ConsensusUser(MinimaDB zDB, ConsensusHandler zHandler) {
		mDB = zDB;
		mHandler = zHandler;
	}
	
	private MinimaDB getMainDB() {
		return mDB;
	}
	 
	public void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.isMessageType(CONSENSUS_NEWSIMPLE)) {
			int bitlength = GlobalParams.MINIMA_DEFAULT_HASH_STRENGTH;
			if(zMessage.exists("bitlength")) {
				bitlength = zMessage.getInteger("bitlength");
			}
			
			//Create a new simple address
			Address addr = getMainDB().getUserDB().newSimpleAddress(bitlength);
			
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			resp.put("address", addr.toJSON());
			InputHandler.endResponse(zMessage, true, "");
		
			
		}else if(zMessage.isMessageType(CONSENSUS_SIGN)) {
			String data   = zMessage.getString("data");
			String pubkey = zMessage.getString("publickey");
			
			//Convert
			MiniData hexdat  = new MiniData(data);
			MiniData hexpubk = new MiniData(pubkey);
			
			//Get the public key..
			PubPrivKey key = getMainDB().getUserDB().getPubPrivKey(hexpubk);
			
			if(key == null) {
				InputHandler.endResponse(zMessage, false, "Public key not found");
				return;
			}
			
			//Now do it..
			MiniData result = key.sign(hexdat);
		
			//Output
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			resp.put("data", hexdat.to0xString());
			resp.put("publickey", hexpubk.getLength());
			resp.put("signature", result.to0xString());
			resp.put("length", result.getLength());
			InputHandler.endResponse(zMessage, true, "");
			
		}else if(zMessage.isMessageType(CONSENSUS_EXTRASCRIPT)) {
			//Get the script
			String script = zMessage.getString("script");
			
			//Check we don't already have it..
			Address addrchk = new Address(script);
			String scriptcheck = getMainDB().getUserDB().getScript(addrchk.getAddressData());
			if(scriptcheck.equals("")) {
				getMainDB().getUserDB().newExtraAddress(script);
			}
			
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			resp.put("address", addrchk.toJSON());
			InputHandler.endResponse(zMessage, true, "");
		
		}else if(zMessage.isMessageType(CONSENSUS_NEWSCRIPT)) {
			//Get the script
			String script = zMessage.getString("script");
			
			//Check we don't already have it..
			Address addrchk = new Address(script);
			String scriptcheck = getMainDB().getUserDB().getScript(addrchk.getAddressData());
			if(scriptcheck.equals("")) {
				getMainDB().getUserDB().newScriptAddress(script);
			}
			
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			resp.put("address", addrchk.toJSON());
			InputHandler.endResponse(zMessage, true, "");
		
		}else if(zMessage.isMessageType(CONSENSUS_NEWKEY)) {
			//Get the bitlength
			int bitl = zMessage.getInteger("bitlength");
			
			//Create a new key pair..
			PubPrivKey key = getMainDB().getUserDB().newPublicKey(bitl);
			
			//return to sender!
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			resp.put("key", key.toJSON());
			InputHandler.endResponse(zMessage, true, "");
			
		
		}else if(zMessage.isMessageType(CONSENSUS_CHECK)) {
			String data = zMessage.getString("data");
			
			//How much to who ?
			MiniData check = null;
			if(data.startsWith("0x")) {
				//It's a regular HASH address
				check  = new MiniData(data);
			}else if(data.startsWith("Mx")) {
				//It's a Minima Address!
				check = Address.convertMinimaAddress(data);
			
			}else {
				InputHandler.endResponse(zMessage, false, "INVALID KEY - "+data);	
				return;
			}
			
			//Now check..
			String type = "none";
			boolean found=false;
			if(getMainDB().getUserDB().isAddressRelevant(check)) {
				found=true;
				type = "address";
			}else if(getMainDB().getUserDB().getPubPrivKey(check)!=null) {
				found=true;
				type = "publickey";
			}
					
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			resp.put("relevant", found);
			resp.put("type", type);
			InputHandler.endResponse(zMessage, true, "");
			
		}else if(zMessage.isMessageType(CONSENSUS_MMRTREE)) {
			//What type SCRIPT or HASHES
			int bitlength = zMessage.getInteger("bitlength");
			
			//Create an MMR TREE from the array of inputs..
			ArrayList<MiniScript> leaves = (ArrayList<MiniScript>) zMessage.getObject("leaves");
		
			//First create an MMR Tree..
			MMRSet mmr = new MMRSet(bitlength);
			
			//Now add each 
			JSONArray nodearray = new JSONArray();
			for(MiniScript leaf : leaves) {
				String leafstr = leaf.toString();
				JSONObject mmrnode = new JSONObject();
				MiniData finaldata = null;
				
				//What type of data..
				int valtype = Value.getValueType(leafstr);
				if(valtype == HEXValue.VALUE_HEX ) {
					finaldata = new MiniData(leafstr);
					mmrnode.put("data",finaldata.toString());
					
				}else if(valtype == BooleanValue.VALUE_BOOLEAN ) {
					MiniNumber num = MiniNumber.ZERO;
					if(leaf.toString().equals("TRUE")) {
						num = MiniNumber.ONE;	
					}
					finaldata = MiniData.getMiniDataVersion(num);
					mmrnode.put("data",num.toString());
					
				}else if(valtype == NumberValue.VALUE_NUMBER) {
					MiniNumber num = new MiniNumber(leaf.toString());
					finaldata = MiniData.getMiniDataVersion(num);
					mmrnode.put("data",num.toString());
					
					
				}else{
					//DEFAULT IS SCRIPT
					finaldata = new MiniData(leaf.getData());
					mmrnode.put("data",leafstr);
				}
				
				
				//Now HASH what we have..
				byte[] hash = Crypto.getInstance().hashData(finaldata.getData(), bitlength);
				MiniData finalhash = new MiniData(hash);
				
				//That hash is the actual leaf node of the tree
				mmrnode.put("leaf", finalhash.to0xString());
				
				//Add to the complete array
				nodearray.add(mmrnode);
				
				//Add to the MMR
				mmr.addUnspentCoin(new MMRData(finalhash,MMRSumNumber.ZERO));
			}

			//Now finalize..
			mmr.finalizeSet();
			
			//Now add the proofs..
			int size=nodearray.size();
			for(int i=0;i<size;i++) {
				JSONObject node = (JSONObject) nodearray.get(i);
				
				//Get the proof..
				MMRProof proof = mmr.getFullProofToRoot(new MiniInteger(i));
				
				//Set the Bits
				proof.setHashBitLength(bitlength);
				
				//Calculate the CHAINSHA proof..
				node.put("chainsha", proof.getChainSHAProof().to0xString());
			}
			
			//return to sender!
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			resp.put("nodes", nodearray);
			resp.put("root", mmr.getMMRRoot().getFinalHash().to0xString());
			InputHandler.endResponse(zMessage, true, "");
			
		}else if(zMessage.isMessageType(CONSENSUS_CLEANSCRIPT)) {
			//Get the Script
			String script = zMessage.getString("script");
			
			//Clean it..
			String clean = Contract.cleanScript(script);
			
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			resp.put("script", script);
			resp.put("clean", clean);
			InputHandler.endResponse(zMessage, true, "");
		
		}else if(zMessage.isMessageType(CONSENSUS_RUNSCRIPT)) {
			String script    = zMessage.getString("script").trim();
			if(script.equals("")) {
				InputHandler.endResponse(zMessage, false, "Cannot have a blank script!");
				return;
			}
			
			String sigs      = Contract.cleanScript(zMessage.getString("sigs").trim());
			String state     = Contract.cleanScript(zMessage.getString("state").trim());
			String prevstate = Contract.cleanScript(zMessage.getString("prevstate").trim());
			String globals   = Contract.cleanScript(zMessage.getString("globals").trim());
			String outputs   = Contract.cleanScript(zMessage.getString("outputs").trim());
			String scripts   = Contract.cleanScript(zMessage.getString("scripts").trim());
			
			//Create the transaction..
			Transaction trans = new Transaction();
			Witness wit       = new Witness();
			
			//OUTPUTS
			if(!outputs.equals("")) {
				//Add the outputs to the Transaction..
				StringTokenizer strtok = new StringTokenizer(outputs,"#");
				while(strtok.hasMoreElements()){
					String tok = strtok.nextToken().trim();
					
					//Now split this token..
					if(!tok.equals("")) {
						//Address
						int index = tok.indexOf(":");
						String address = tok.substring(0,index).trim();
						
						//Amount
						int oldindex = index;
						index = tok.indexOf(":", index+1);
						String amount = tok.substring(oldindex+1,index).trim();
						
						//Tokenid
						String tokenid = tok.substring(index+1).trim();
						
						//Create this coin
						Coin outcoin = new Coin(new MiniData("0x00"), 
												new MiniData(address), 
												new MiniNumber(amount), 
												new MiniData(tokenid));
						
						//Add this output to the transaction..
						trans.addOutput(outcoin);
					}
				}
			}
			
			//STATE
			if(!state.equals("")) {
				//Add all the state variables..
				StringTokenizer strtok = new StringTokenizer(state,"#");
				while(strtok.hasMoreElements()){
					String tok = strtok.nextToken().trim();
					
					//Now split this token..
					if(!tok.equals("")) {
						int split = tok.indexOf(":");
						String statenum = tok.substring(0,split).trim();
						String value = tok.substring(split+1).trim();
						
						//Set it..
						trans.addStateVariable(new StateVariable(Integer.parseInt(statenum), value));
					}
				}
			}
			
			//PREVSTATE
			ArrayList<StateVariable> pstate = new ArrayList<>();
			if(!prevstate.equals("")) {
				//Add all the state variables..
				StringTokenizer strtok = new StringTokenizer(prevstate,"#");
				while(strtok.hasMoreElements()){
					String tok = strtok.nextToken().trim();
					
					//Now split this token..
					if(!tok.equals("")) {
						int split = tok.indexOf(":");
						String statenum = tok.substring(0,split).trim();
						String value = tok.substring(split+1).trim();
						
						//Set it..
						pstate.add(new StateVariable(Integer.parseInt(statenum), value));
					}
				}
			}
			
			//SCRIPTS
			if(!scripts.equals("")) {
				//Add all the state variables..
				StringTokenizer strtok = new StringTokenizer(scripts,"#");
				while(strtok.hasMoreElements()){
					String tok = strtok.nextToken().trim();
					
					//Now split this token..
					if(!tok.equals("")) {
						int split = tok.indexOf(":");
						
						String mastscript = tok.substring(0,split).trim();
						String chainsha   = tok.substring(split+1).trim();
						
						//Set it..
						wit.addScript(new ScriptProof(mastscript, chainsha));
					}
				}
			}
			
			//Create a contract
			Contract cc = new Contract(script, sigs, wit, trans, pstate);
			
			//Create an address
			Address ccaddress = new Address(cc.getMiniScript());
			
			//Set the environment
			MiniNumber blocknum  = getMainDB().getTopBlock();
			
			//These 2 are set automatically..
			cc.setGlobalVariable("@ADDRESS", new HEXValue(ccaddress.getAddressData()));
			cc.setGlobalVariable("@SCRIPT", new ScriptValue(script));
			
			//These can be played with..
			cc.setGlobalVariable("@BLKNUM", new NumberValue(blocknum));
			cc.setGlobalVariable("@INPUT", new NumberValue(0));
			cc.setGlobalVariable("@INBLKNUM", new NumberValue(0));
			cc.setGlobalVariable("@AMOUNT", new NumberValue(0));
			cc.setGlobalVariable("@TOKENID", new HEXValue("0x00"));
			cc.setGlobalVariable("@COINID", new HEXValue("0x00"));
			cc.setGlobalVariable("@TOTIN", new NumberValue(1));
			cc.setGlobalVariable("@TOTOUT", new NumberValue(trans.getAllOutputs().size()));
			
			//GLOBALS.. Overide if set..
			if(!globals.equals("")) {
				//Add all the state variables..
				StringTokenizer strtok = new StringTokenizer(globals,"#");
				while(strtok.hasMoreElements()){
					String tok = strtok.nextToken().trim();
					
					//Now split this token..
					if(!tok.equals("")) {
						int split = tok.indexOf(":");
						String global = tok.substring(0,split).trim().toUpperCase();
						String value = tok.substring(split+1).trim();
						
						//Set it..
						cc.setGlobalVariable(global, Value.getValue(value));
					}
				}
			}
			
			//Set the BLKDIFF
			MiniNumber blk   = cc.getGlobal("@BLKNUM").getNumber();
			MiniNumber blkin = cc.getGlobal("@INBLKNUM").getNumber();
			cc.setGlobalVariable("@BLKDIFF", new NumberValue(blk.sub(blkin)));
			
			//Run it!
			cc.run();
		
			//Detailed results..
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			resp.put("script", script);
			resp.put("clean", cc.getMiniScript());
			resp.put("size", cc.getMiniScript().length());
			resp.put("instructions", cc.getNumberOfInstructions());
			resp.put("address", ccaddress.getAddressData().to0xString());
			resp.put("parseok", cc.isParseOK());
			resp.put("variables",cc.getAllVariables());
			resp.put("parse", cc.getCompleteTraceLog());
			resp.put("exception", cc.isException());
			resp.put("result", cc.isSuccess());
			InputHandler.endResponse(zMessage, true, "");
		
		}else if(zMessage.isMessageType(CONSENSUS_FLUSHMEMPOOL)) {
			boolean hard = zMessage.getBoolean("hard");
			
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			resp.put("hard", hard);
			
			//TxPOW DB
			TxPowDB tdb = getMainDB().getTxPowDB();
			
			//Check the MEMPOOL transactions..
			ArrayList<TxPOWDBRow> unused = tdb.getAllUnusedTxPOW();
			ArrayList<MiniData> remove = new ArrayList<>();
			JSONArray requested = new JSONArray();
			
			//Check them all..
//			MinimaLogger.log("FLUSHING MEMPOOL! HARD:"+hard);
			for(TxPOWDBRow txrow : unused) {
				TxPOW txpow    = txrow.getTxPOW();
				
				//Check All..
				if(txpow.isBlock()) {
					MiniData parent = txpow.getParentID();
					if(tdb.findTxPOWDBRow(parent) == null) {
						//Request it from ALL your peers..
						Message msg  = new Message(NetClient.NETCLIENT_SENDOBJECT)
								.addObject("type", NetClientReader.NETMESSAGE_TXPOW_REQUEST)
								.addObject("object", parent);
						Message netw = new Message(NetworkHandler.NETWORK_SENDALL)
								.addObject("message", msg);
						
						//Post it..
						mHandler.getMainHandler().getNetworkHandler().PostMessage(netw);
						
						//Add to out list
						requested.add(parent.to0xString());
					}
					
					//Get all the messages in the block..
					ArrayList<MiniData> txns = txpow.getBlockTxns();
					for(MiniData txn : txns) {
						if(tdb.findTxPOWDBRow(txn) == null) {
							//Request it from ALL your peers..
							Message msg  = new Message(NetClient.NETCLIENT_SENDOBJECT)
									.addObject("type", NetClientReader.NETMESSAGE_TXPOW_REQUEST)
									.addObject("object", txn);
							Message netw = new Message(NetworkHandler.NETWORK_SENDALL)
									.addObject("message", msg);
							
							//Post it..
							mHandler.getMainHandler().getNetworkHandler().PostMessage(netw);
							
							//Add to out list
							requested.add(txn.to0xString());
						}
					}
				}
								
				//Do we just remove them all.. ?
				if(hard) {
					//Remove all..
					remove.add(txpow.getTxPowID());
				}else{
					//Check it..
					boolean sigsok = TxPOWChecker.checkSigs(txpow);
					boolean trxok  = TxPOWChecker.checkTransactionMMR(txpow, getMainDB());
						
					if(!sigsok || !trxok) {
						remove.add(txpow.getTxPowID());
					}	
				}
			}
			
			//Now remove these..
			JSONArray rem = new JSONArray();
			for(MiniData remtxp : remove) {
				rem.add(remtxp.to0xString());
				getMainDB().getTxPowDB().removeTxPOW(remtxp);
			}
			
			//Now you have the proof..
			resp.put("removed", rem);
			resp.put("requested", requested);
			InputHandler.endResponse(zMessage, true, "Mempool Flushed");
			
		}else if(zMessage.isMessageType(CONSENSUS_UNKEEPCOIN)) {
			//Once a coin has been used - say in a DEX.. you can remove it from your coinDB
			String cid = zMessage.getString("coinid");
			
			//Remove the coin..
			boolean found = getMainDB().getCoinDB().removeCoin(new MiniData(cid));
			
			//Now you have the proof..
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			resp.put("found", found);
			resp.put("coinid", cid);
			InputHandler.endResponse(zMessage, true, "Coin removed");
			
		}else if(zMessage.isMessageType(CONSENSUS_KEEPCOIN)) {
			String cid = zMessage.getString("coinid");
			
			//Get the MMRSet
			MMRSet basemmr = getMainDB().getMainTree().getChainTip().getMMRSet();
			
			//Search for the coin..
			MiniData coinid = new MiniData(cid);
			MMREntry entry =  basemmr.findEntry(coinid);
			
			//Now ask to keep it..
			MMRSet coinset = basemmr.getParentAtTime(entry.getBlockTime());
			coinset.addKeeper(entry.getEntry());
			coinset.finalizeSet();
			
			//Get the coin
			Coin cc = entry.getData().getCoin();
			
			//add it to the database
			CoinDBRow crow = getMainDB().getCoinDB().addCoinRow(cc);
			crow.setIsSpent(entry.getData().isSpent());
			crow.setIsInBlock(true);
			crow.setInBlockNumber(entry.getData().getInBlock());
			crow.setMMREntry(entry.getEntry());
			
			//Now you have the proof..
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			resp.put("coin", basemmr.getProof(entry.getEntry()));
			InputHandler.endResponse(zMessage, true, "");
			
		}else if(zMessage.isMessageType(CONSENSUS_IMPORTCOIN)) {
			MiniData data = (MiniData)zMessage.getObject("proof");
			
			ByteArrayInputStream bais = new ByteArrayInputStream(data.getData());
			DataInputStream dis = new DataInputStream(bais);
			
			//Now make the proof..
			MMRProof proof = MMRProof.ReadFromStream(dis);
			
			if(proof.getMMRData().isSpent()) {
				//ONLY UNSPENT COINS..
				InputHandler.endResponse(zMessage, false, "Coin already SPENT!");
				return;
			}
			
			//Get the MMRSet
			MMRSet basemmr = getMainDB().getMainTree().getChainTip().getMMRSet();
			
			//Check it..
			boolean valid  = basemmr.checkProof(proof);
			
			//Stop if invalid.. 
			if(!valid) {
				//Now you have the proof..
				InputHandler.endResponse(zMessage, false, "INVALID PROOF");
				return;
			}
			
			//Get the MMRSet where this proof was made..
			MMRSet proofmmr = basemmr.getParentAtTime(proof.getBlockTime());
			if(proofmmr == null) {
				//Now you have the proof..
				InputHandler.endResponse(zMessage, false, "Proof too old - no MMRSet found @ "+proof.getBlockTime());
				return;
			}
			
			//Now add this proof to the set.. if not already added
			MMREntry entry =  proofmmr.addExternalUnspentCoin(proof);
			
			//Error.
			if(entry == null) {
				InputHandler.endResponse(zMessage, false, "Consensus error addding proof !");
				return;
			}
			
			//And now refinalize..
			proofmmr.finalizeSet();
			
			//Get the coin
			Coin cc = entry.getData().getCoin();
			
			//add it to the database
			CoinDBRow crow = getMainDB().getCoinDB().addCoinRow(cc);
			crow.setIsSpent(entry.getData().isSpent());
			crow.setIsInBlock(true);
			crow.setInBlockNumber(entry.getData().getInBlock());
			crow.setMMREntry(entry.getEntry());
			
			//Now you have the proof..
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			resp.put("proof", proof.toJSON());
			InputHandler.endResponse(zMessage, true, "");
			
		}else if(zMessage.isMessageType(CONSENSUS_EXPORTCOIN)) {
			MiniData coinid = (MiniData)zMessage.getObject("coinid");
			
			//The Base current MMRSet
			MMRSet basemmr  = getMainDB().getMainTree().getChainTip().getMMRSet();
			
			//Get proofs from a while back so reorgs don't invalidate them..
			MMRSet proofmmr = basemmr.getParentAtTime(getMainDB().getTopBlock().sub(GlobalParams.MINIMA_CONFIRM_DEPTH));
			
			//Find this coin..
			CoinDBRow row  = getMainDB().getCoinDB().getCoinRow(coinid);
			
			//Get a proof from a while back.. more than confirmed depth, less than cascade
//			MMRProof proof = getMainTree().getChainTip().getMMRSet().getProof(row.getMMREntry());
			MMRProof proof = proofmmr.getProof(row.getMMREntry());
			
			//Now write this out to  MiniData Block
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			DataOutputStream dos = new DataOutputStream(baos);
			proof.writeDataStream(dos);
			dos.flush();
			
			//Now get the data..
			MiniData pd = new MiniData(baos.toByteArray());
			
			//Now you have the proof..
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			resp.put("coinid", coinid.to0xString());
			resp.put("proof", proof.toJSON());
			resp.put("data", pd.to0xString());
			InputHandler.endResponse(zMessage, true, "");
			
			dos.close();
			baos.close();
			
		}else if(zMessage.isMessageType(CONSENSUS_EXPORTKEY)) {
			MiniData pubk = (MiniData)zMessage.getObject("publickey");
			
			//Get it..
			MiniData priv = getMainDB().getUserDB().getPubPrivKey(pubk).getPrivateSeed();
			
			MinimaLogger.log(priv.toString());
			
		}else if(zMessage.isMessageType(CONSENSUS_IMPORTKEY)) {
			MiniData priv = (MiniData)zMessage.getObject("privatekey");

			PubPrivKey newkey = new PubPrivKey(priv);
			
			if(getMainDB().getUserDB().getPubPrivKey(newkey.getPublicKey())!=null) {
				MinimaLogger.log("Key allready in DB!");
			}else {
				getMainDB().getUserDB().newSimpleAddress(newkey);
			}
		}
	}
	
	public static boolean importCoin(MinimaDB zDB, MMRProof zProof) throws IOException{
		//Get the MMRSet
		MMRSet basemmr = zDB.getMainTree().getChainTip().getMMRSet();
		
		//Check it..
		boolean valid  = basemmr.checkProof(zProof);
		
		//Stop if invalid.. 
		if(!valid) {
			return false;
		}
		
		//Get the MMRSet where this proof was made..
		MMRSet proofmmr = basemmr.getParentAtTime(zProof.getBlockTime());
		if(proofmmr == null) {
			return false;
		}
		
		//Now add this proof to the set.. if not already added
		MMREntry entry =  proofmmr.addExternalUnspentCoin(zProof);
		
		//Error..
		if(entry == null) {
			return false;
		}
		
		//And now refinalize..
		proofmmr.finalizeSet();
		
		//Get the coin
		Coin cc = entry.getData().getCoin();
		
		//add it to the database
		CoinDBRow crow = zDB.getCoinDB().addCoinRow(cc);
		crow.setIsSpent(entry.getData().isSpent());
		crow.setIsInBlock(true);
		crow.setInBlockNumber(entry.getData().getInBlock());
		crow.setMMREntry(entry.getEntry());
		
		return true;
	}
	
	public static MiniData exportCoin(MinimaDB zDB, MiniData zCoinID) throws IOException {
		//The Base current MMRSet
		MMRSet basemmr  = zDB.getMainTree().getChainTip().getMMRSet();
		
		//Get proofs from a while back so reorgs don't invalidate them..
		MMRSet proofmmr = basemmr.getParentAtTime(zDB.getTopBlock().sub(GlobalParams.MINIMA_CONFIRM_DEPTH));
		
		//Find this coin..
		CoinDBRow row  = zDB.getCoinDB().getCoinRow(zCoinID);
		
		//Get a proof from a while back.. more than confirmed depth, less than cascade
		MMRProof proof = proofmmr.getProof(row.getMMREntry());
		
		//Now write this out to  MiniData Block
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		DataOutputStream dos = new DataOutputStream(baos);
		proof.writeDataStream(dos);
		dos.flush();
		
		return new MiniData(baos.toByteArray());
	}
}
