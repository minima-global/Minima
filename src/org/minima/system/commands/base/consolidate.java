package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRProof;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.wallet.ScriptRow;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Coin;
import org.minima.objects.CoinProof;
import org.minima.objects.ScriptProof;
import org.minima.objects.Token;
import org.minima.objects.Transaction;
import org.minima.objects.TxPoW;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.keys.Signature;
import org.minima.system.Main;
import org.minima.system.brains.TxPoWGenerator;
import org.minima.system.brains.TxPoWMiner;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.params.GlobalParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class consolidate extends Command {

	public consolidate() {
		super("consolidate","[tokenid:] (coinage:) (maxcoins:) (maxsigs:) (burn:) (debug:) (dryrun:) - Consolidate coins by sending them back to yourself");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//The tokenid
		String tokenid = getParam("tokenid");
		
		//How old must the coins
		MiniNumber coinage = getNumberParam("coinage", GlobalParams.MINIMA_CONFIRM_DEPTH);
		
		//Is there a burn
		MiniNumber burn = getNumberParam("burn", MiniNumber.ZERO);
		
		//Is this a dry run
		boolean debug 	= getBooleanParam("debug", false);
		boolean dryrun 	= getBooleanParam("dryrun", false);
		if(dryrun) {
			debug = true;
		}
		
		//Get the tip of the tree
		TxPoWTreeNode tip 	= MinimaDB.getDB().getTxPoWTree().getTip();
		
		//Lets build a transaction..
		ArrayList<Coin> relcoins 	= TxPoWSearcher.getRelevantUnspentCoins(tip,tokenid,true);
		
		//Sort coins via same address - since they require the same signature
		relcoins = send.orderCoins(relcoins);
		
		//How many coins are there
		int COIN_SIZE = relcoins.size();
		if(COIN_SIZE<2) {
			throw new CommandException("Not enough coins ("+COIN_SIZE+") to consolidate");
		}
		
		//Maximum number of coins and signatures
		int MAX_SIGS 	= getNumberParam("maxsigs", new MiniNumber(5)).getAsInt();
		int MAX_COINS 	= getNumberParam("maxcoins", new MiniNumber(20)).getAsInt();
		
		String 		currentaddress 	= "";
		MiniNumber 	totalamount 	= MiniNumber.ZERO;
		int 		totalsigs 		= 0;
		int 		totalcoins 		= 0;
		for(Coin cc : relcoins) {
			
			//This coins address
			String coinaddress = cc.getAddress().to0xString();
			
			//The Amount
			MiniNumber coinamount = cc.getAmount();
			if(!cc.getTokenID().to0xString().equals("0x00")) {
				coinamount = cc.getToken().getScaledTokenAmount(cc.getAmount());
			}
			
			//Is it a new address
			if(!currentaddress.equals(coinaddress)) {
				
				//Are we at the limit
				if(totalsigs+1>MAX_SIGS) {
					MinimaLogger.log("Consolidate - max sigs reached "+totalsigs);
					break;
				}
				
				//New address = new signature
				currentaddress = coinaddress;
				totalsigs++;
			}
			
			//Add to the total..
			totalamount = totalamount.add(coinamount);
			
			//One more coin
			totalcoins++;
			
			if(debug) {
				MinimaLogger.log("Consolidate - add coin "+coinamount+" totalcoins:"+totalcoins+"  totalsigs:"+totalsigs);
			}
			
			//Do checks..
			if(totalcoins>=MAX_COINS) {
				MinimaLogger.log("Consolidate - max coins reached "+totalcoins);
				break;
			}
		}
		
		//Get one of your addresses
		ScriptRow newwalletaddress 	= MinimaDB.getDB().getWallet().getDefaultAddress();
		MiniData myaddress 			= new MiniData(newwalletaddress.getAddress());
		
		//Construct the command
		String command = "send split:2 dryrun:"+dryrun+" debug:"+debug+" burn:"+burn.toString()
				+" amount:"+totalamount.toString()+" address:"+myaddress.to0xString()+" tokenid:"+tokenid;
		
		if(debug) {
			MinimaLogger.log("Consolidate command : "+command);
		}
		
		JSONArray result 		= Command.runMultiCommand(command);
		JSONObject sendresult 	= (JSONObject) result.get(0); 
		if((boolean) sendresult.get("status")) {
			ret.put("response", sendresult.get("response"));
		}else {
			if(sendresult.get("message") != null) {
				ret.put("response", sendresult.get("message"));
			}else if(sendresult.get("error") != null) {
				ret.put("response", sendresult.get("error"));
			}else {
				ret.put("response", "Error occurred..");
			}
		}
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new consolidate();
	}
}
