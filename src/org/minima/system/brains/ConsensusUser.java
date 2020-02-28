package org.minima.system.brains;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;

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
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.PubPrivKey;
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
			
		}else if(zMessage.isMessageType(CONSENSUS_RUNSCRIPT)) {
			String script = zMessage.getString("script");
			String sigs   = zMessage.getString("sigs");
			
			//Set up a contract
			script = Contract.cleanScript(script);
			sigs   = Contract.cleanScript(sigs);
			
			//Create a contract
			Contract cc = new Contract(script, sigs,new Transaction(),true);
			
			//set the environment
			MiniNumber blocknum  = getMainDB().getTopBlock();
			String address = "0x00";
			
			cc.setGlobalVariable("@BLKNUM", new NumberValue(blocknum));
			cc.setGlobalVariable("@ADDRESS", new HEXValue(address));
			cc.setGlobalVariable("@AMOUNT", new NumberValue(MiniNumber.ZERO));
			cc.setGlobalVariable("@SCRIPT", new ScriptValue(script));
			
			//Run it!
			cc.run();
		
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
