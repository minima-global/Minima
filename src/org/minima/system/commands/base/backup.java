package org.minima.system.commands.base;

import java.io.DataOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.util.zip.GZIPOutputStream;

import org.minima.database.MinimaDB;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MiniFile;
import org.minima.utils.MiniFormat;
import org.minima.utils.json.JSONObject;

public class backup extends Command {

	public backup() {
		super("backup","(file:) (complete:false|true) - Backup the system. Uses a timestamped name by default");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String file = getParam("file","");
		if(file.equals("")) {
			file = "minima-backup-"+System.currentTimeMillis()+".bak.gz";
		}

		boolean complete = getBooleanParam("complete", false);
		
		//Does it exist..
		File backupfile = new File(file);
		if(backupfile.exists()) {
			backupfile.delete();
		}
		
		///Base folder
		File backupfolder = new File(GeneralParams.DATA_FOLDER,"backup");
		backupfolder.mkdirs();
		
		//Lock the DB
		MinimaDB.getDB().readLock(true);
		
		try {
		
			//Save the current state..
			MinimaDB.getDB().saveState();
			
			//Write the SQL Dbs
			File walletfile = new File(backupfolder,"wallet.sql");
			MinimaDB.getDB().getWallet().backupToFile(walletfile);
			MiniData walletata 	= new MiniData(MiniFile.readCompleteFile(walletfile));
			
			File cascade = new File(backupfolder,"cascade.bak");
			MinimaDB.getDB().getCascade().saveDB(cascade);
			MiniData cascadedata = new MiniData(MiniFile.readCompleteFile(cascade));
			
			File chain = new File(backupfolder,"chaintree.bak");
			MinimaDB.getDB().getTxPoWTree().saveDB(chain);
			MiniData chaindata = new MiniData(MiniFile.readCompleteFile(chain));
			
			File userdb = new File(backupfolder,"userdb.bak");
			MinimaDB.getDB().getUserDB().saveDB(userdb);
			MiniData userdata = new MiniData(MiniFile.readCompleteFile(userdb));
			
			File p2pdb = new File(backupfolder,"p2p.bak");
			MinimaDB.getDB().getP2PDB().saveDB(p2pdb);
			MiniData p2pdata = new MiniData(MiniFile.readCompleteFile(p2pdb));
			
			//For Complete..
			File txpowdb 	= new File(backupfolder,"txpowdb.sql");
			File archivedb 	= new File(backupfolder,"archive.sql");
			
			MiniData txpowdata 		= null;
			MiniData archivedata 	= null;
			
			if(complete) {
				MinimaDB.getDB().getTxPoWDB().getSQLDB().backupToFile(txpowdb);
				txpowdata	= new MiniData(MiniFile.readCompleteFile(txpowdb));
				
				MinimaDB.getDB().getArchive().backupToFile(archivedb);
				archivedata = new MiniData(MiniFile.readCompleteFile(archivedb));
			}
			
			//Now create the streams to save these
			FileOutputStream fos 	= new FileOutputStream(backupfile);
			GZIPOutputStream gzos	= new GZIPOutputStream(fos);
			DataOutputStream dos 	= new DataOutputStream(gzos);
			
			//Is it Complete
			MiniByte.WriteToStream(dos, complete);
			
			//And now put ALL of those files into a single file..
			walletata.writeDataStream(dos);
			cascadedata.writeDataStream(dos);
			chaindata.writeDataStream(dos);
			userdata.writeDataStream(dos);
			p2pdata.writeDataStream(dos);
			
			if(complete) {
				txpowdata.writeDataStream(dos);
				archivedata.writeDataStream(dos);
			}
			
			//All done..
			dos.close();
			gzos.close();
			fos.close();
			
			//The total uncompressed size..
			long total = 	walletfile.length()+
							cascade.length()+
							chain.length()+
							userdb.length()+
							p2pdb.length();
			
			if(complete) {
				total += 	txpowdb.length()+
							cascade.length();
			}
			
			//Get all the individual File sizes..
			JSONObject files = new JSONObject();
			files.put("wallet", MiniFormat.formatSize(walletfile.length()));
			
			if(complete) {
				files.put("txpowdb", MiniFormat.formatSize(txpowdb.length()));
				files.put("archive", MiniFormat.formatSize(archivedb.length()));
			}
			
			files.put("cascade", MiniFormat.formatSize(cascade.length()));
			files.put("chain", MiniFormat.formatSize(chain.length()));
			files.put("user", MiniFormat.formatSize(userdb.length()));
			files.put("p2p", MiniFormat.formatSize(p2pdb.length()));
			
			//And send data
			JSONObject resp = new JSONObject();
			resp.put("block", MinimaDB.getDB().getTxPoWTree().getTip().getTxPoW().getBlockNumber());
			resp.put("files", files);
			resp.put("uncompressed", MiniFormat.formatSize(total));
			resp.put("file", backupfile.getAbsolutePath());
			resp.put("size", MiniFormat.formatSize(backupfile.length()));
			ret.put("backup", resp);
			
			//And now clean up..
			MiniFile.deleteFileOrFolder(GeneralParams.DATA_FOLDER, backupfolder);
			
		}catch(Exception exc) {
			
			//Unlock DB..
			MinimaDB.getDB().readLock(false);
			
			//Delete backup folder
			MiniFile.deleteFileOrFolder(GeneralParams.DATA_FOLDER, backupfolder);
			
			//Throw an error to notify user..
			throw new CommandException(exc.toString());
		}
		
		//Unlock..
		MinimaDB.getDB().readLock(false);
				
		//Delete backup folder
		MiniFile.deleteFileOrFolder(GeneralParams.DATA_FOLDER, backupfolder);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new backup();
	}

}
