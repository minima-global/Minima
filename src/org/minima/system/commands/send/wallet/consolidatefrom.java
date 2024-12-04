package org.minima.system.commands.send.wallet;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.Coin;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.commands.CommandRunner;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class consolidatefrom extends Command {
	
	public consolidatefrom() {
		super("consolidatefrom","[fromaddress:] [address:] [amount:] (tokenid:) [script:] [privatekey:] [keyuses:] (burn:) (mine:) - Send Minima or Tokens from a certain address");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"fromaddress",
				"tokenid","script","privatekey","keyuses","mine","burn","maxcoins"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
	
		//From which address
		String fromaddress 	= getAddressParam("fromaddress");
		String tokenid 		= getAddressParam("tokenid", "0x00");
		
		//How many coins to add
		int maxcoins 		= getNumberParam("maxcoins",new MiniNumber(50)).getAsInt(); 
		
		//Get the BURN
		MiniNumber burn 	= getNumberParam("burn",MiniNumber.ZERO);
		if(burn.isMore(MiniNumber.ZERO) && !tokenid.equals("0x00")) {
			throw new CommandException("Currently BURN only works for Minima.. tokenid:0x00.. not tokens.");
		}
		
		//Thew script of the address
		String script 		= getParam("script");
		
		//The private key we need to sign with
		String privatekey	= getAddressParam("privatekey");
		MiniNumber keyuses  = getNumberParam("keyuses");
		
		//ID of the custom transaction
		String randomid 	= MiniData.getRandomData(32).to0xString();
		
		//Now construct the transaction..
		JSONObject result 	= runCommand("txncreate id:"+randomid);
				
		//Are we mining
		boolean mine 		= getBooleanParam("mine", true);
		
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
		
		//Get all the coins for this addres / token
		ArrayList<Coin> coins = TxPoWSearcher.searchCoins(	tip, false, 
								false, MiniData.ZERO_TXPOWID,
								false, MiniNumber.ZERO,
								true, new MiniData(fromaddress), 
								true, new MiniData(tokenid), 
								false, "", true,
								false, Integer.MAX_VALUE,GeneralParams.IS_MEGAMMR);
		
		//Now order the coins..
		Collections.sort(coins, new Comparator<Coin>() {
			@Override
			public int compare(Coin zCoin1, Coin zCoin2) {
				MiniNumber amt1 = zCoin1.getAmount();
				MiniNumber amt2 = zCoin2.getAmount();
				return amt2.compareTo(amt1);
			}
		});
		
		//Now add the top 20 coins
		int coincount	 = 1;
		MiniNumber total = MiniNumber.ZERO;
		
		//Cycle through the top coins
		for(Coin cc : coins){
			total = total.add(cc.getTokenAmount()); 
			coincount++;
			
			//Add this coin to the transaction
			String command 	= "txninput id:"+randomid+" coinid:"+cc.getCoinID().to0xString();
			result 			= runCommand(command);
			
			//How many coins have been added
			if(coincount>maxcoins) {
				break;
			}
		}
		
		//Now do the burn..
		if(tokenid.equals("0x00") && burn.isMore(MiniNumber.ZERO)) {
			if(burn.isLess(total)) {
				total = total.sub(burn);
			}else {
				throw new CommandException("Burn greater than total amount added "+burn+" / "+total);
			}
		}
		
		//And now add the output
		String command 	= "txnoutput id:"+randomid+" amount:"+total+" address:"+fromaddress+" tokenid:"+tokenid;
		result 			= runCommand(command);
		
		//Add the scripts..
		runCommand("txnscript id:"+randomid+" scripts:{\""+script+"\":\"\"}");
		
		//Sort the MMR
		runCommand("txnmmr id:"+randomid);
		
		//Now SIGN
		runCommand("txnsign id:"+randomid+" publickey:custom privatekey:"+privatekey+" keyuses:"+keyuses);
		
		//And POST!
		result = runCommand("txnpost id:"+randomid+" mine:"+mine);
		
		//And delete..
		runCommand("txndelete id:"+randomid);
		
		//And return..
		ret.put("response", result.get("response"));
		
		return ret;
	}
	
	public JSONObject runCommand(String zCommand) {
		JSONArray res 		= CommandRunner.getRunner().runMultiCommand(zCommand);
		JSONObject result 	= (JSONObject) res.get(0);
		return result;
	}

	@Override
	public Command getFunction() {
		return new consolidatefrom();
	}	
}