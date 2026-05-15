package org.minima.system.commands.send.wallet;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.objects.Coin;
import org.minima.objects.StateVariable;
import org.minima.objects.Token;
import org.minima.objects.Transaction;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.commands.CommandRunner;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class createtokenfrom extends Command {
	
	public createtokenfrom() {
		super("createtokenfrom","[fromaddress:] [script:] [privatekey:] [keyuses:] [name:] [amount:] (decimals:) (mine:)- Create Tokens from a certain address");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"fromaddress","name",
				"amount","privatekey","keyuses","script","decimals","mine"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
	
		//From which address
		String fromaddress 	= getAddressParam("fromaddress");
		MiniNumber amount 	= getNumberParam("amount");
		
		//The script of the address
		String script 		= getParam("script");
		
		//The private key we need to sign with
		String privatekey	= getAddressParam("privatekey");
		MiniNumber keyuses  = getNumberParam("keyuses");
		
		//Are we mining
		boolean mine 		= getBooleanParam("mine", true);
				
		//Is name a JSON
		JSONObject jsonname = null;
		if(isParamJSONObject("name")) {
			
			//Get the JSON
			jsonname = getJSONObjectParam("name");
			
			//make sure there is a name object
			if(!jsonname.containsKey("name")) {
				throw new CommandException("MUST specify a 'name' for the token in the JSON");
			}
			
		}else {
			
			//It's a String.. create a JSON
			jsonname = new JSONObject();
			jsonname.put("name", getParam("name"));
		}
		
		int decimals = 8;
		if(getParams().containsKey("decimals")) {
			decimals = Integer.parseInt((String)getParams().get("decimals"));
			
			//Safety check.. not consensus set - could be more.
			if(decimals>16) {
				throw new Exception("MAX 16 decimal places");
			}
		}
		
		//The actual amount of tokens..
		MiniNumber totaltoks = new MiniNumber(amount).floor(); 
		
		//Safety check Amount is within tolerant levels.. could use ALL their Minima otherwise..
		if(totaltoks.isMore(MiniNumber.TRILLION)) {
			throw new CommandException("MAX 1 Trillion coins for a token");
		}
		
		if(totaltoks.isLessEqual(MiniNumber.ZERO)) {
			throw new CommandException("Cannot create less than 1 token");
		}
		
		//Decimals as a number
		MiniNumber totaldecs = MiniNumber.TEN.pow(decimals); 
		
		//How much Minima will it take to colour.. 
		MiniNumber minimamount = MiniNumber.MINI_UNIT.mult(totaldecs).mult(totaltoks);
		
		//What is the scale..
		int scale = MiniNumber.MAX_DECIMAL_PLACES - decimals;
		
		//ID of the custom transaction
		String randomid 	= MiniData.getRandomData(32).to0xString();
		
		//Now construct the transaction..
		JSONObject result 	= runCommand("txncreate id:"+randomid);
		
		//Add the mounts..
		String command 		= "txnaddamount id:"+randomid+" fromaddress: "+fromaddress+" address:"+fromaddress+" amount:"+minimamount;
		result = runCommand(command);
		if(!(boolean)result.get("status")) {
			
			//Delete transaction
			runCommand("txndelete id:"+randomid);
			
			//Not enough funds!
			throw new CommandException(result.getString("error"));
		}
		
		//Now replace the first Coin with the creation coin
		Coin tokenoutput = new Coin(Coin.COINID_OUTPUT, new MiniData(fromaddress), minimamount, Token.TOKENID_CREATE, true);
		
		//Let's create the token..
		Token createtoken = new Token(Coin.COINID_OUTPUT, 
										new MiniNumber(scale), 
										minimamount,
										new MiniString(jsonname.toString()),
										new MiniString("RETURN TRUE"));
		
		//Set the Create Token Details..
		tokenoutput.setToken(createtoken);
		
		//Get the TXN DB - and replace the first coin
		TxnDB db 			= MinimaDB.getDB().getCustomTxnDB();
		TxnRow txnrow 		= db.getTransactionRow(randomid);
		Transaction trans 	= txnrow.getTransaction();
		
		ArrayList<Coin> outputs = trans.getAllOutputs();
		outputs.set(0, tokenoutput);
		
		//Add the scripts..
		runCommand("txnscript id:"+randomid+" scripts:{\""+script+"\":\"\"}");
			
		//Sort the MMR
		runCommand("txnmmr id:"+randomid);
		
		//Now SIGN
		runCommand("txnsign id:"+randomid+" publickey:custom privatekey:"+privatekey+" keyuses:"+keyuses);
		
		//And POST!
		//result = runCommand("txnlist transactiononly:true id:"+randomid);
		result = runCommand("txnpost id:"+randomid+" mine:"+mine);
		
		//And delete..
		runCommand("txndelete id:"+randomid);
		
		//Check succeeded..
		if(!(boolean)result.get("status")) {
			//Didn't work..
			throw new CommandException((String)result.get("error"));
		}
		
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
		return new createtokenfrom();
	}	
}