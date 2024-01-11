package org.minima.system.commands.search;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMR;
import org.minima.database.mmr.MegaMMR;
import org.minima.database.wallet.KeyRow;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Address;
import org.minima.objects.base.MiniData;
import org.minima.objects.keys.TreeKey;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.params.GeneralParams;
import org.minima.utils.BIP39;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class megammr extends Command {

	public megammr() {
		super("megammr","Scan the Mega MMR");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","publickey","phrase"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();

		MegaMMR megammr = MinimaDB.getDB().getMegaMMR();
		
		//MMR.printmmrtree(megammr.getMMR());
		
		JSONObject resp = new JSONObject();
		resp.put("enabled", GeneralParams.IS_MEGAMMR);
		resp.put("mmr", megammr.getMMR().toJSON(false));
		resp.put("coins", megammr.getAllCoins().size());
		
		//Put the details in the response..
		ret.put("response", resp);
		
		return ret;
	}
	
	@Override
	public Command getFunction() {
		return new megammr();
	}

}
