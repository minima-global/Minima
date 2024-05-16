package org.minima.system.commands.backup.mmrsync;

import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MegaMMR;
import org.minima.objects.IBD;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MiniFile;
import org.minima.utils.MiniFormat;
import org.minima.utils.MiniUtil;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;

public class megammr extends Command {

	public megammr() {
		super("megammr","(action:) (file:) - Get Info on or Import / Export the MegaMMR data");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","file"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();

		String action = getParam("action","info");
		
		MegaMMR megammr = MinimaDB.getDB().getMegaMMR();
		
		if(action.equals("info")) {
		
			JSONObject resp = new JSONObject();
			resp.put("enabled", GeneralParams.IS_MEGAMMR);
			resp.put("mmr", megammr.getMMR().toJSON(false));
			resp.put("coins", megammr.getAllCoins().size());
			
			//Put the details in the response..
			ret.put("response", resp);
		
		}else if(action.equals("export")) {
			
			if(!GeneralParams.IS_MEGAMMR) {
				throw new CommandException("MegaMMR not enabled");
			}
			
			//Get the file
			String file = getParam("file","");
			if(file.equals("")) {
				//file = "megammr-backup-"+System.currentTimeMillis()+".bak";
				file = "minima_backup_"+MiniUtil.DATEFORMAT.format(new Date())+".megammr";
			}
			
			//Create the file
			File backupfile = MiniFile.createBaseFile(file);
			
			//get the MMR and IBD..
			if(backupfile.exists()) {
				backupfile.delete();
			}
			backupfile.createNewFile();
			
			IBD ibd = new IBD();
			
			//Lock the DB for read access..
			MinimaDB.getDB().readLock(true);
			
			try {
				
				//Create an IBD
				ibd.createCompleteIBD();
				
				MegaMMRBackup mmrbackup = new MegaMMRBackup(megammr, ibd);
				
				//Now write to it..
				FileOutputStream fos 		= new FileOutputStream(backupfile);
				BufferedOutputStream bos 	= new BufferedOutputStream(fos, 65536);
				DataOutputStream fdos 		= new DataOutputStream(bos);
				
				//And write it..
				mmrbackup.writeDataStream(fdos);
				
				//flush
				fdos.flush();
				bos.flush();
				fos.flush();
				
				fdos.close();
				bos.close();
				fos.close();
				
			}catch(Exception exc) {
				
				//Unlock DB
				MinimaDB.getDB().readLock(false);
				
				throw new CommandException(exc.toString());
			}
			
			//Unlock DB
			MinimaDB.getDB().readLock(false);
			
			JSONObject resp = new JSONObject();
			resp.put("megammrtip", megammr.getMMR().getBlockTime());
			resp.put("ibdtip", ibd.getTreeTip());
			resp.put("backup", backupfile.getAbsolutePath());
			resp.put("size", MiniFormat.formatSize(backupfile.length()));
			
			//Put the details in the response..
			ret.put("response", resp);
		}
		
		return ret;
	}
	
	@Override
	public Command getFunction() {
		return new megammr();
	}

}
