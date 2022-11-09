package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.objects.base.MiniData;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.BIP39;
import org.minima.utils.json.JSONObject;

public class vault extends Command {

	public vault() {
		super("vault","[action:seed|lock|unlock] (seed:) (phrase:) - BE CAREFUL. Wipe / Restore your private keys");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","seed","phrase"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String action = getParam("action", "seed");
			
		//Display the base seed..
		MiniData baseseed 	= MinimaDB.getDB().getWallet().getBaseSeed();
		String phrase 		= MinimaDB.getDB().getUserDB().getBasePrivatePhrase();
		
		if(action.equals("seed")) {
			
			JSONObject json = new JSONObject();
			json.put("phrase", phrase);
			json.put("seed", baseseed.to0xString());
			json.put("locked", baseseed.isEqual(MiniData.ZERO_TXPOWID));
			
			ret.put("response", json);
		
		}else if(action.equals("lock")) {
			
			//Get the seed.. TO SHOW THEY KNOW IT..
			MiniData seed = getDataParam("seed");
			if(!seed.isEqual(baseseed)) {
				throw new CommandException("Incorrect seed for lock");
			}
			
			//Wipe the private keys (but keep the public keys, modifiers etc)
			MinimaDB.getDB().getWallet().wipeBaseSeed();
			
			//And reset in the UserDB
			MinimaDB.getDB().getUserDB().setBasePrivatePhrase("");
			MinimaDB.getDB().getUserDB().setBasePrivateSeed(MiniData.ZERO_TXPOWID.to0xString());
			
			ret.put("response", "All private keys wiped!");
		
		}else if(action.equals("unlock")) {
			
			//Check for one or the other..
			String initphrase = getParam("phrase");
			
			//Convert to a data hash
			MiniData seed = BIP39.convertStringToSeed(initphrase);
			
			//And now recreate the private keys
			boolean ok = MinimaDB.getDB().getWallet().resetBaseSeed(seed);

			if(!ok) {
				throw new CommandException("Error updating Private keys.. please try again");
			}

			//And set the key in the UserDB
			MinimaDB.getDB().getUserDB().setBasePrivatePhrase(initphrase);
			MinimaDB.getDB().getUserDB().setBasePrivateSeed(seed.to0xString());
			
			ret.put("response", "All private keys restored!");
		}
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new vault();
	}
}
