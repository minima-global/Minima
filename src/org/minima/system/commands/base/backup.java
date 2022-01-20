package org.minima.system.commands.base;

import java.io.DataOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.util.zip.GZIPOutputStream;

import org.minima.database.MinimaDB;
import org.minima.objects.base.MiniData;
import org.minima.system.commands.Command;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MiniFile;
import org.minima.utils.MiniFormat;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;

public class backup extends Command {

	public backup() {
		super("backup","(file:) - Backup the entire system. Uses a timestamped name by default");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String file = getParam("file","");
		if(file.equals("")) {
			file = "minima-backup-"+System.currentTimeMillis()+".bak.gz";
		}
		
		//Does it exist..
		File backupfile = new File(file);
		if(backupfile.exists()) {
			backupfile.delete();
		}
		
		//Lock the DB
		MinimaDB.getDB().readLock(true);
		
		try {
		
			//Save the current state..
			MinimaDB.getDB().saveState();
			
			///Base folder
			File backupfolder = new File(GeneralParams.DATA_FOLDER,"backup");
			backupfolder.mkdirs();
			
			//Write the SQL Dbs
			File walletfile = new File(backupfolder,"wallet.sql");
			MinimaDB.getDB().getWallet().backupToFile(walletfile);
			MiniData walletata 	= new MiniData(MiniFile.readCompleteFile(walletfile));
			
			File txpowdb = new File(backupfolder,"txpowdb.sql");
			MinimaDB.getDB().getTxPoWDB().getSQLDB().backupToFile(txpowdb);
			MiniData txpowdata	= new MiniData(MiniFile.readCompleteFile(txpowdb));
			
			File archivedb = new File(backupfolder,"archive.sql");
			MinimaDB.getDB().getArchive().backupToFile(archivedb);
			MiniData archivedata = new MiniData(MiniFile.readCompleteFile(archivedb));
			
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
			MinimaDB.getDB().getUserDB().saveDB(p2pdb);
			MiniData p2pdata = new MiniData(MiniFile.readCompleteFile(p2pdb));
		
			//Now create the streams to save these
			FileOutputStream fos 	= new FileOutputStream(backupfile);
			GZIPOutputStream gzos	= new GZIPOutputStream(fos);
			DataOutputStream dos 	= new DataOutputStream(gzos);
					
			//And now put ALL of those files into a single file..
			walletata.writeDataStream(dos);
			txpowdata.writeDataStream(dos);
			archivedata.writeDataStream(dos);
			cascadedata.writeDataStream(dos);
			chaindata.writeDataStream(dos);
			userdata.writeDataStream(dos);
			p2pdata.writeDataStream(dos);
			
			//All done..
			dos.close();
			gzos.close();
			fos.close();
			
			//The total uncompressed size..
			long total = 	walletfile.length()+
							txpowdb.length()+
							archivedb.length()+
							cascade.length()+
							chain.length()+
							userdb.length()+
							p2pdb.length();
			
			//Get all the individual File sizes..
			JSONObject files = new JSONObject();
			files.put("wallet", MiniFormat.formatSize(walletfile.length()));
			files.put("txpowdb", MiniFormat.formatSize(txpowdb.length()));
			files.put("archive", MiniFormat.formatSize(archivedb.length()));
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
			MinimaLogger.log(exc);
		}
		
		//Unlock..
		MinimaDB.getDB().readLock(false);
				
		return ret;
	}

	@Override
	public Command getFunction() {
		return new backup();
	}

}
