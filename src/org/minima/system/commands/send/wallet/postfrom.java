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
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class postfrom extends Command {

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
	
	public postfrom() {
		super("postfrom","[data:] (mine:) - Post a signfrom txn ");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"data","mine"}));
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
		
		//Are we mining
		boolean mine 		= getBooleanParam("mine", false);
		
		//And POST!
		JSONObject result = runCommand("txnpost id:"+randomid+" mine:"+mine);
			
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
		return new postfrom();
	}	
}