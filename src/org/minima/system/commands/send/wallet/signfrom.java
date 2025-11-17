package org.minima.system.commands.send.wallet;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandRunner;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class signfrom extends Command {

	public signfrom() {
		super("signfrom","[data:] [privatekey:] [keyuses:] (post:) - Sign a creatfrom txn");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"id","data","privatekey","keyuses","post"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
	
		TxnDB db = MinimaDB.getDB().getCustomTxnDB();
		
		//Get the HEX data
		MiniData dv = getDataParam("data");
		
		//Convert to a TxnRow
		TxnRow tx 	= TxnRow.convertMiniDataVersion(dv);
		if(existsParam("id")) {
			tx.setID(getParam("id"));
		}
		
		String randomid = tx.getID();
		
		//Add to the DB
		db.addCompleteTransaction(tx);
		
		//The private key we need to sign with
		String privatekey	= getAddressParam("privatekey");
		MiniNumber keyuses  = getNumberParam("keyuses");
		
		//Are we posting..
		boolean post = getBooleanParam("post", false);
		
		//Now SIGN
		JSONObject signres = null; 
		if(!post) {
			signres = runCommand("txnsign id:"+randomid+" publickey:custom privatekey:"+privatekey+" keyuses:"+keyuses);
		}else {
			signres = runCommand("txnsign id:"+randomid+" publickey:custom privatekey:"+privatekey+" keyuses:"+keyuses+" txnpostauto:true txnpostmine:true");
		}
		
		//Now export the txn..
		JSONObject result = runCommand("txnexport id:"+randomid);
				
		//And delete..
		runCommand("txndelete id:"+randomid);
		
		//Get the result
		JSONObject resresp = (JSONObject) result.get("response");
		resresp.put("post", post);
		if(post) {
			try {
				JSONObject ss = (JSONObject) signres.get("response");
				resresp.put("txpow", ss.get("txpow"));
			}catch(Exception exc) {
				MinimaLogger.log(exc);
			}
		}
		
		//And return..
		ret.put("response", resresp);
			
		return ret;
	}
	
	public JSONObject runCommand(String zCommand) {
		JSONArray res 		= CommandRunner.getRunner().runMultiCommand(zCommand);
		JSONObject result 	= (JSONObject) res.get(0);
		return result;
	}

	@Override
	public Command getFunction() {
		return new signfrom();
	}	
}