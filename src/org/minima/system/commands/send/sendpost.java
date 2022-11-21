package org.minima.system.commands.send;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.utils.MiniFile;
import org.minima.utils.json.JSONObject;

public class sendpost extends Command {

	public sendpost() {
		super("sendpost","[file:] - Post a signed txn");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"file"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
	
		String txnfile = getParam("file");
		
		//Load the txn
		byte[] data = MiniFile.readCompleteFile(MiniFile.createBaseFile(txnfile));
		
		//Create the MininData
		MiniData txndata = new MiniData(data);
		
		//Now convert back into a TxPoW
		TxPoW txp = TxPoW.convertMiniDataVersion(txndata);
		
		//Calculate the TxPOWID
		txp.calculateTXPOWID();
				
		JSONObject sigtran = new JSONObject();
		sigtran.put("txpow", txp.toJSON());
		
		JSONObject resp = new JSONObject();
		ret.put("response", sigtran);
		
		//Post It..!
		Main.getInstance().getTxPoWMiner().mineTxPoWAsync(txp);
		
		return ret;
	}
	
	@Override
	public Command getFunction() {
		return new sendpost();
	}
}