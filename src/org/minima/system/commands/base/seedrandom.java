package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.wallet.SeedRow;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.BaseConverter;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;

public class seedrandom extends Command {

	public seedrandom() {
		super("seedrandom","[modifier:] - Generate a random value, based on your SEED and a modifier");
	}
	
	@Override
	public String getFullHelp() {
		return "\nseedrandom\n"
				+ "\n"
				+ "Generate a random value, based on your SEED and a modifier.\n"
				+ "\n"
				+ "modifier: \n"
				+ "    The modifier - added to seed before hash.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "seedrandom modifier:\"Hello you\"\n"
				+ "\n";	
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"modifier"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//Check not locked..
		if(!MinimaDB.getDB().getWallet().isBaseSeedAvailable()) {
			throw new CommandException("DB locked!");
		}
		
		//Get the modifier..
		String modifier = getParam("modifier");
		
		//Get the minidata version..
		MiniData moddata = MiniData.getMiniDataVersion(new MiniString(modifier));
		
		//Now get the base seed..
		SeedRow sr = MinimaDB.getDB().getWallet().getBaseSeed();
		
		//Hash them together..
		MiniData hash = Crypto.getInstance().hashObjects(moddata, new MiniData(sr.getSeed()));
		
		JSONObject resp = new JSONObject();
		resp.put("modifier", modifier);
		resp.put("hashed", hash.to0xString());
			
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new seedrandom();
	}

}
