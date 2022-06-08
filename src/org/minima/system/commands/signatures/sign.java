package org.minima.system.commands.signatures;

import org.minima.database.MinimaDB;
import org.minima.database.wallet.KeyRow;
import org.minima.database.wallet.Wallet;
import org.minima.objects.base.MiniData;
import org.minima.objects.keys.Signature;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class sign extends Command {

	public sign() {
		super("sign","[publickey:] [data:] - Sign the data with the publickey");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		MiniData data = getDataParam("data");
		MiniData pubk = getDataParam("publickey");
		
		//Get the Key row..
		Wallet wallet = MinimaDB.getDB().getWallet();
		KeyRow kr = wallet.getKeysRowFromPublicKey(pubk.to0xString());
		
		//Use the wallet..
		Signature signature = wallet.sign(kr.getPrivateKey(), data);
		
		ret.put("response", MiniData.getMiniDataVersion(signature).to0xString());
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new sign();
	}

}
