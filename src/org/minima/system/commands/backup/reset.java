package org.minima.system.commands.backup;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.zip.GZIPInputStream;

import javax.crypto.Cipher;
import javax.crypto.CipherInputStream;

import org.minima.database.MinimaDB;
import org.minima.database.txpowdb.sql.TxPoWList;
import org.minima.database.txpowdb.sql.TxPoWSqlDB;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MiniFile;
import org.minima.utils.encrypt.GenerateKey;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.ssl.SSLManager;

public class reset extends Command {

	public reset() {
		super("reset","[archivefile:] [action:] (file:) (password:) (keys:) (keyuses:) - Reset the entire system using an Archive Backup file...");
	}
	
	@Override
	public String getFullHelp() {
		return "\nreset\n"
				+ "\n"
				+ "Reset your node in various ways. You MUST wait until all your original keys are created before this is allowed.\n"
				+ "\n"
				+ "archivefile:\n"
				+ "    Specify the the archive backup file\n"
				+ "\n"
				+ "action:\n"
				+ "    chainsync:\n"
				+ "    seedsync:\n"
				+ "    restore:\n"
				+ "\n"
				+ "file:\n"
				+ "    Specify the filename or local path of the backup to restore\n"
				+ "\n"
				+ "password: (optional)\n"
				+ "    Enter the password of the backup \n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "reset archivefile:archive-backup-162373462763.gz file:my-full-backup-01-Jan-22 password:Longsecurepassword456\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","archivefile","file","password","phrase","keys","keyuses"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//Can only do this if all keys created..
		vault.checkAllKeysCreated();
		
		//Get the archive file..
		String archivefile = getParam("archivefile");
		
		//Which action
		String action = getParam("action"); 
		
		if(action.equals("chainsync")) {
			
			//Refactor the command
			String command 		= "archive action:import file:"+archivefile;
			JSONArray res 		= Command.runMultiCommand(command);
			JSONObject result 	= (JSONObject) res.get(0);
			ret.put("response", result);
			
		}else if(action.equals("seedsync")) {
			
			//Get the phrase
			String phrase = getParam("phrase");
			
			//Refactor the command
			String command 	= "archive action:import file:"+archivefile+" phrase:\""+phrase+"\"";

			//Add extra params
			if(existsParam("keys")) {
				command = command+" keys:"+getParam("keys");
			}
			
			if(existsParam("keyuses")) {
				command = command+" keyuses:"+getParam("keyuses");
			}
			
			JSONArray res 		= Command.runMultiCommand(command);
			JSONObject result 	= (JSONObject) res.get(0);
			ret.put("response", result);
			
		
		}else if(action.equals("restore")) {
			
			//Get the backup file
			String backupfile = getParam("file");
			
			//Now do a restore..
			String command 	= "restore shutdown:false file:"+backupfile;
			
			//Password ?
			if(existsParam("password")) {
				command = command+" password:"+getParam("password");
			}
			
			JSONArray res 		= Command.runMultiCommand(command);
			JSONObject result 	= (JSONObject) res.get(0);
			
			//Check worked..
			if(!(boolean)result.get("status")) {
				throw new CommandException("Error restoring.. "+result.toJSONString());
			}
			
			JSONObject allres = new JSONObject();
			allres.put("restore", result);
			
			//Now reopen the required SQL Dbs..
			Main.getInstance().restoreReadyForSync();
			
			//And NOW do a chain resync..
			command  = "archive action:import file:"+archivefile;
			res 	 = Command.runMultiCommand(command);
			result 	 = (JSONObject) res.get(0);
			allres.put("chainsync", result);
			
			//The final results..
			ret.put("response", allres);
			
		}else {
			throw new CommandException("Invalid action : "+action);
		}
		
		return ret;
	}
	
	private long readNextBackup(File zOutput, DataInputStream zDis) throws IOException {
		MiniData data = MiniData.ReadFromStream(zDis);
		MiniFile.writeDataToFile(zOutput, data.getBytes());
		return zOutput.length();
	}
	
	private TxPoWList readNextTxPoWList(DataInputStream zDis) throws IOException {
		MiniData data 		= MiniData.ReadFromStream(zDis);
		TxPoWList txplist 	= TxPoWList.convertMiniDataVersion(data);
		return txplist;
	}

	@Override
	public Command getFunction() {
		return new reset();
	}

}
