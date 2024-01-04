package org.minima.system.commands.search;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRProof;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.wallet.KeyRow;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.base.MiniData;
import org.minima.objects.keys.TreeKey;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.BIP39;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class coinproof extends Command {

	public coinproof() {
		super("coinproof","");
	}
	
	@Override
	public String getFullHelp() {
		return "\ncoinproof\n"
				+ "\n"
				+ "Get a coin MMR proof\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","coinid"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		MiniData coinid = getDataParam("coinid");
		
		//Find coin..
		Coin cc = TxPoWSearcher.searchCoin(coinid);
		ret.put("coin", cc.toJSON());
		
		//Get the MMR proof..
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
		
		//Get the proof..
		MMRProof proof = tip.getMMR().getProofToPeak(cc.getMMREntryNumber());
		
		//Convert to MiniData
		MiniData proofdata = MiniData.getMiniDataVersion(proof);
		
		ret.put("proof", proofdata.to0xString());
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new coinproof();
	}

}
