package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.txpowdb.TxPoWDB;
import org.minima.database.txpowdb.sql.TxPoWSqlDB;
import org.minima.system.commands.Command;
import org.minima.system.params.GeneralParams;
import org.minima.utils.json.JSONObject;

public class logs extends Command {

	public logs() {
		super("logs","(scripts:) (mining:) (maxima:) (networking:) (blocks:) (ibd:) (txpowdb:) - Enable full logs for various parts of Minima");
	}
	
	@Override
	public String getFullHelp() {
		return "\nlogs\n"
				+ "\n"
				+ "Enable detailed logs for script errors or mining activity.\n"
				+ "\n"
				+ "scripts: (optional)\n"
				+ "    true or false, true turns on detailed logs for script errors.\n"
				+ "\n"
				+ "mining: (optional)\n"
				+ "    true or false, true turns on detailed logs for mining start/end activity.\n"
				+ "\n"
				+ "maxima: (optional)\n"
				+ "    true or false, true turns on detailed logs for Maxima.\n"
				+ "\n"
				+ "networking: (optional)\n"
				+ "    true or false, true turns on detailed logs for Network Messages.\n"
				+ "\n"
				+ "blocks: (optional)\n"
				+ "    true or false, true turns on detailed logs for Blocks.\n"
				+ "\n"
				+ "ibd: (optional)\n"
				+ "    true or false, true turns on detailed logs for IBD processing.\n"
				+ "\n"
				+ "peerschecker: (optional)\n"
				+ "    true or false, true turns on detailed logs for IBD processing.\n"
				+ "\n"
				+ "txpowdb: (optional)\n"
				+ "    true or false, true turns on logs for adding TxPoW to TxPoWDB.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "logs scripts:true\n"
				+ "\n"
				+ "logs scripts:false mining:true\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"scripts","mining","maxima",
				"blocks","networking","ibd","peerschecker","txpowdb"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();

		//Are we logging script errors
		if(existsParam("scripts")) {
			String scripts = getParam("scripts", "false");
			if(scripts.equals("true")) {
				GeneralParams.SCRIPTLOGS = true;
			}else {
				GeneralParams.SCRIPTLOGS= false;
			}
		}
		
		//Are we logging all mining
		if(existsParam("mining")) {
			String mining = getParam("mining", "false");
			if(mining.equals("true")) {
				GeneralParams.MINING_LOGS = true;
			}else {
				GeneralParams.MINING_LOGS= false;
			}
		}
		
		//Are we logging all maxima
		if(existsParam("maxima")) {
			String mining = getParam("maxima", "false");
			if(mining.equals("true")) {
				GeneralParams.MAXIMA_LOGS = true;
			}else {
				GeneralParams.MAXIMA_LOGS= false;
			}
		}
		
		//Are we logging blocks
		if(existsParam("blocks")) {
			String mining = getParam("blocks", "false");
			if(mining.equals("true")) {
				GeneralParams.BLOCK_LOGS = true;
			}else {
				GeneralParams.BLOCK_LOGS = false;
			}
		}
		
		//Are we logging networking
		if(existsParam("networking")) {
			String mining = getParam("networking", "false");
			if(mining.equals("true")) {
				GeneralParams.NETWORKING_LOGS = true;
			}else {
				GeneralParams.NETWORKING_LOGS = false;
			}
		}
		
		//Are we logging IBD data
		if(existsParam("ibd")) {
			String mining = getParam("ibd", "false");
			if(mining.equals("true")) {
				GeneralParams.IBDSYNC_LOGS = true;
			}else {
				GeneralParams.IBDSYNC_LOGS = false;
			}
		}
		
		//Are we logging IBD data
		if(existsParam("peerschecker")) {
			String mining = getParam("peerschecker", "false");
			if(mining.equals("true")) {
				GeneralParams.PEERSCHECKER_lOG = true;
			}else {
				GeneralParams.PEERSCHECKER_lOG = false;
			}
		}
		
		//Are we logging IBD data
		if(existsParam("txpowdb")) {
			String mining = getParam("txpowdb", "false");
			if(mining.equals("true")) {
				TxPoWSqlDB.LOG_ADD_BLOCKS 		= true;
				TxPoWSqlDB.LOG_ADD_TRANSACTION 	= true;
			}else {
				TxPoWSqlDB.LOG_ADD_BLOCKS 		= false;
				TxPoWSqlDB.LOG_ADD_TRANSACTION 	= false;
			}
		}
		
		JSONObject resp = new JSONObject();
		resp.put("scripts", GeneralParams.SCRIPTLOGS);
		resp.put("mining", GeneralParams.MINING_LOGS);
		resp.put("maxima", GeneralParams.MAXIMA_LOGS);
		resp.put("blocks", GeneralParams.BLOCK_LOGS);
		resp.put("txpowdb", TxPoWSqlDB.LOG_ADD_BLOCKS);
		resp.put("networking", GeneralParams.NETWORKING_LOGS);
		resp.put("ibd", GeneralParams.IBDSYNC_LOGS);
		resp.put("peerschecker", GeneralParams.PEERSCHECKER_lOG);
		
		//Add balance..
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new logs();
	}

}
