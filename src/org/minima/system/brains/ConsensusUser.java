package org.minima.system.brains;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.util.ArrayList;
import java.util.StringTokenizer;

import org.minima.GlobalParams;
import org.minima.database.MinimaDB;
import org.minima.database.coindb.CoinDBRow;
import org.minima.database.mmr.MMREntry;
import org.minima.database.mmr.MMRProof;
import org.minima.database.mmr.MMRSet;
import org.minima.miniscript.Contract;
import org.minima.miniscript.values.HEXValue;
import org.minima.miniscript.values.NumberValue;
import org.minima.miniscript.values.ScriptValue;
import org.minima.miniscript.values.Value;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.PubPrivKey;
import org.minima.objects.StateVariable;
import org.minima.objects.Transaction;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniHash;
import org.minima.objects.base.MiniNumber;
import org.minima.system.input.InputHandler;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class ConsensusUser {


	public static final String CONSENSUS_PREFIX 			= "CONSENSUSUSER_";
	
	public static final String CONSENSUS_NEWKEY 			= CONSENSUS_PREFIX+"NEWKEY";
	
	public static final String CONSENSUS_NEWSIMPLE 			= CONSENSUS_PREFIX+"NEWSIMPLE";
	public static final String CONSENSUS_NEWSCRIPT 			= CONSENSUS_PREFIX+"NEWSCRIPT";
	public static final String CONSENSUS_RUNSCRIPT 			= CONSENSUS_PREFIX+"RUNSCRIPT";
	public static final String CONSENSUS_CLEANSCRIPT 		= CONSENSUS_PREFIX+"CLEANSCRIPT";
	
	public static final String CONSENSUS_EXPORTKEY 			= CONSENSUS_PREFIX+"EXPORTKEY";
	public static final String CONSENSUS_IMPORTKEY 			= CONSENSUS_PREFIX+"IMPORTKEY";
	public static final String CONSENSUS_EXPORTCOIN 		= CONSENSUS_PREFIX+"EXPORTCOIN";
	public static final String CONSENSUS_IMPORTCOIN 		= CONSENSUS_PREFIX+"IMPORTCOIN";
	
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
			//Create a new simple address
			Address addr = getMainDB().getUserDB().newSimpleAddress();
			
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			resp.put("address", addr.getAddressData().toString());
			resp.put("script", addr.getScript().toString());
			InputHandler.endResponse(zMessage, true, "");
			
		}else if(zMessage.isMessageType(CONSENSUS_NEWSCRIPT)) {
			//Get the script
			String script = zMessage.getString("script");
			
			//Check we don't already have it..
			Address addrchk = new Address(script);
			String scriptcheck = getMainDB().getUserDB().getScript(addrchk.getAddressData());
			if(!scriptcheck.equals("")) {
				InputHandler.endResponse(zMessage, false, "Address already exists..");
				return;	
			}
			
			Address addr = getMainDB().getUserDB().newScriptAddress(script);
			
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			resp.put("address", addr.getAddressData().toString());
			resp.put("script", addr.getScript().toString());
			InputHandler.endResponse(zMessage, true, "");
		
		}else if(zMessage.isMessageType(CONSENSUS_NEWKEY)) {
			//Create a new key pair..
			PubPrivKey key = getMainDB().getUserDB().newPublicKey();
			
			//return to sender!
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			resp.put("key", key.toString());
			InputHandler.endResponse(zMessage, true, "");
			
		}else if(zMessage.isMessageType(CONSENSUS_CLEANSCRIPT)) {
			String script = zMessage.getString("script");
			
//			//Create a contract
//			Contract cc = new Contract(script, "",new Transaction(),false);
//			
//			//Create an address
//			Address ccaddress = new Address(cc.getRamScript());
//			
//			JSONObject resp = InputHandler.getResponseJSON(zMessage);
//			resp.put("script", script);
//			resp.put("clean", cc.getRamScript());
//			resp.put("address", ccaddress.getAddressData().to0xString());
//			resp.put("parseok", cc.isParseOK());
//			resp.put("parse", cc.getCompleteTraceLog());
//			InputHandler.endResponse(zMessage, true, "");
		
		}else if(zMessage.isMessageType(CONSENSUS_RUNSCRIPT)) {
			String script    = zMessage.getString("script").trim();
			String sigs      = Contract.cleanScript(zMessage.getString("sigs").trim());
			String state     = Contract.cleanScript(zMessage.getString("state").trim());
			String prevstate = Contract.cleanScript(zMessage.getString("prevstate").trim());
			String globals   = Contract.cleanScript(zMessage.getString("globals").trim());
			String outputs   = Contract.cleanScript(zMessage.getString("outputs").trim());
			
			//Create the transaction..
			Transaction trans = new Transaction();
			
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
						Coin outcoin = new Coin(MiniHash.ZERO32, 
												new MiniHash(address), 
												new MiniNumber(amount), 
												new MiniHash(tokenid));
						
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
						trans.addStateVariable(new StateVariable(new MiniNumber(statenum), value));
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
						pstate.add(new StateVariable(new MiniNumber(statenum), value));
					}
				}
			}
			
			//Create a contract
			Contract cc = new Contract(script, sigs, trans, pstate);
			
			//Create an address
			Address ccaddress = new Address(cc.getRamScript());
			
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
			cc.setGlobalVariable("@TOKENID", new HEXValue(MiniHash.ZERO32));
			cc.setGlobalVariable("@COINID", new HEXValue(MiniHash.ZERO32));
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
			
			//Run it!
			cc.run();
		
			//Detailed results..
			JSONObject resp = InputHandler.getResponseJSON(zMessage);
			resp.put("script", script);
			resp.put("clean", cc.getRamScript());
			resp.put("address", ccaddress.getAddressData().to0xString());
			resp.put("parseok", cc.isParseOK());
			resp.put("exception", cc.isException());
			resp.put("result", cc.isSuccess());
			resp.put("parse", cc.getCompleteTraceLog());
			
			InputHandler.endResponse(zMessage, true, "");
			
		}else if(zMessage.isMessageType(CONSENSUS_IMPORTCOIN)) {
			MiniData data = (MiniData)zMessage.getObject("proof");
			
			ByteArrayInputStream bais = new ByteArrayInputStream(data.getData());
			DataInputStream dis = new DataInputStream(bais);
			
			//Now make the proof..
			MMRProof proof = MMRProof.ReadFromStream(dis);
			
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
			
//			//Do we already have it..
//			MiniNumber entry = proof.getEntryNumber();
//			MMRProof checker = basemmr.getProof(entry);
//			if(checker != null) {
//				//is It Complete..
//				if(!checker.getMMRData().isHashOnly()) {
//					//We have a complete copy already..
//					JSONObject resp = InputHandler.getResponseJSON(zMessage);
//					resp.put("proof", proof.toJSON());
//					resp.put("valid", true);
//					InputHandler.endResponse(zMessage, true, "");	
//				}
//			}
			
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
			MiniHash coinid = (MiniHash)zMessage.getObject("coinid");
			
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
}
