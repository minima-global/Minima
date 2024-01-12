package org.minima.system.commands.send;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.StringTokenizer;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRProof;
import org.minima.database.txpowdb.TxPoWDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.database.wallet.ScriptRow;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.CoinProof;
import org.minima.objects.ScriptProof;
import org.minima.objects.StateVariable;
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
import org.minima.system.commands.backup.vault;
import org.minima.system.commands.search.keys;
import org.minima.system.commands.txn.txnutils;
import org.minima.system.params.GeneralParams;
import org.minima.system.params.GlobalParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class sendfrom extends Command {

	public class AddressAmount {
		
		MiniData 	mAddress;
		MiniNumber 	mAmount;
		
		public AddressAmount(MiniData zAddress, MiniNumber zAmount) {
			mAddress 	= zAddress;
			mAmount		= zAmount;
		}
		
		public MiniData getAddress(){
			return mAddress;
		}
		
		public MiniNumber getAmount() {
			return mAmount;
		}
	}
	
	public sendfrom() {
		super("send","[fromaddress:] (address:Mx..|0x..) (amount:) (multi:[address:amount,..]) (tokenid:) (state:{}) (password:) (burn:) (split:) (coinage:) (mine:) (debug:) (dryrun:) - Send Minima or Tokens to an address");
	}
	
	@Override
	public String getFullHelp() {
		return "\nsend\n"
				+ "\n"
				+ "Send Minima or custom tokens from a SPECIFIC ADDRESS to a wallet or custom script address.\n"
				+ "\n"
				+ "Optionally, send to multiple addresses in one transaction; split UTxOs; add state variables or include a burn.\n"
				+ "\n"
				+ "address: (optional)\n"
				+ "    A Minima 0x or Mx wallet address or custom script address. Must also specify amount.\n"
				+ "\n"
				+ "amount: (optional)\n"
				+ "    The amount of Minima or custom tokens to send to the specified address.\n"
				+ "\n"
				+ "multi: (optional)\n"
				+ "    JSON Array listing addresses and amounts to send in one transaction.\n"
				+ "    Takes the format [address:amount,address2:amount2,..], with each set in double quotes.\n"
				+ "\n"
				+ "tokenid: (optional)\n"
				+ "    If sending a custom token, you must specify its tokenid. Defaults to Minima (0x00).\n"
				+ "\n"
				+ "state: (optional)\n"
				+ "    List of state variables, if sending coins to a script. A JSON object in the format {\"port\":\"value\",..}\n"
				+ "\n"
				+ "burn: (optional)\n"
				+ "    The amount of Minima to burn with this transaction.\n"
				+ "\n"
				+ "password: (optional)\n"
				+ "    If your Wallet is password locked you can unlock it for this one transaction - then relock it.\n"
				+ "\n"
				+ "split: (optional)\n"
				+ "    You can set the number of coins the recipient will receive, between 1 and 20. Default is 1.\n"
				+ "    The amount being sent will be split into multiple coins of equal value.\n"
				+ "    You can split your own coins by sending to your own address.\n"
				+ "    Useful if you want to send multiple transactions without waiting for change to be confirmed.\n"
				+ "\n"
				+ "coinage: (optional)\n"
				+ "    How old must the coins be in blocks.\n"
				+ "\n"
				+ "debug: (optional)\n"
				+ "    true or false, true will print more detailed logs.\n"
				+ "\n"
				+ "dryrun: (optional)\n"
				+ "    true or false, true will simulate the send transaction but not execute it.\n"
				+ "\n"
				+ "mine: (optional)\n"
				+ "    true or false - should you mine the transaction immediately.\n"
				+ "\n"
				+ "storestate: (optional)\n"
				+ "    true or false - defaults to true. Should the output coins store the state (will still appear in NOTIFYCOIN messages).\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "send address:0xFF.. amount:10\n"
				+ "\n"
				+ "send address:0xFF.. amount:10 tokenid:0xFED5.. burn:0.1\n"
				+ "\n"
				+ "send address:0xFF.. amount:10 split:5 burn:0.1\n"
				+ "\n"
				+ "send multi:[\"0xFF..:10\",\"0xEE..:10\",\"0xDD..:10\"] split:20\n"
				+ "\n"
				+ "send amount:1 address:0xFF.. state:{\"0\":\"0xEE..\",\"1\":\"0xDD..\"}\n";
					
	}

	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"fromaddress","address",
				"amount","tokenid","script","privatekey","keyuses"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
	
		//From which address
		String fromaddress 	= getAddressParam("fromaddress");
		String toaddress 	= getAddressParam("address");
		MiniNumber amount 	= getNumberParam("amount");
		String tokenid 		= getAddressParam("tokenid", "0x00");
		
		//Thew script of the address
		String script 		= getParam("script");
		
		//The private key we need to sign with
		String privatekey	= getAddressParam("privatekey");
		MiniNumber keyuses  = getNumberParam("keyuses");
		
		//ID of the custom transaction
		String randomid 	= MiniData.getRandomData(32).to0xString();
		
		//Now construct the transaction..
		JSONObject result 	= runCommand("txncreate id:"+randomid);
		
		//Add the mounts..
		String command 		= "txnaddamount id:"+randomid+" address:"+toaddress+" amount:"+amount+" tokenid:"+tokenid;
		result = runCommand(command);
		if(!(boolean)result.get("status")) {
			
			//Delete transaction
			runCommand("txndelete id:"+randomid);
			
			//Not enough funds!
			throw new CommandException(result.getString("error"));
		}
		
		//Add the scripts..
		runCommand("txnscript id:"+randomid+" scripts:{\""+script+"\":\"\"}");
		
		//Sort the MMR
		runCommand("txnmmr id:"+randomid);
		
		//Now SIGN
		runCommand("txnsign id:"+randomid+" publickey:custom privatekey:"+privatekey+" keyuses:"+keyuses);
		
		//And POST!
		result = runCommand("txnpost id:"+randomid);
		
		//And delete..
		runCommand("txndelete id:"+randomid);
		
		//And return..
		ret.put("response", result.get("response"));
		
		return ret;
	}
	
	public JSONObject runCommand(String zCommand) {
		JSONArray res 		= Command.runMultiCommand(zCommand);
		JSONObject result 	= (JSONObject) res.get(0);
		return result;
	}

	@Override
	public Command getFunction() {
		return new sendfrom();
	}	
}