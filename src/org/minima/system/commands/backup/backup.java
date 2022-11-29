package org.minima.system.commands.backup;

import java.io.DataOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.security.SecureRandom;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Locale;
import java.util.zip.GZIPOutputStream;

import javax.crypto.Cipher;
import javax.crypto.CipherOutputStream;

import org.minima.database.MinimaDB;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MiniFile;
import org.minima.utils.MiniFormat;
import org.minima.utils.MinimaLogger;
import org.minima.utils.encrypt.GenerateKey;
import org.minima.utils.json.JSONObject;

public class backup extends Command {

	public static final SimpleDateFormat DATEFORMAT = new SimpleDateFormat("dd_MM_yyyy_HHmmss", Locale.ENGLISH );
	
	public backup() {
		super("backup","(password:) (file:) (auto:) (complete:false|true) - Backup the system. Uses a timestamped name by default");
	}
	
	@Override
	public String getFullHelp() {
		return "\nbackup\n"
				+ "\n"
				+ "Backup your node. Uses a timestamped name by default.\n"
				+ "\n"
				+ "password: (optional)\n"
				+ "    Set a password using letters and numbers only.\n"
				+ "\n"
				+ "file: (optional)\n"
				+ "    Specify a filename ending in .bak, optionally include a local path for the backup.\n"
				+ "    Default location for a backup is the Minima data folder.\n"
				+ "\n"
				+ "auto: (optional)\n"
				+ "    true or false, true will schedule a non-password protected backup every 24 hours.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "backup password:Longsecurepassword456\n"
				+ "\n"
				+ "backup password:Longsecurepassword456 file:my-backup-01-Jan-22.bak\n"
				+ "\n"
				+ "backup auto:true\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"password","file","auto","complete"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//Is this an AUTO backup initiate..
		if(existsParam("auto")) {
			boolean setauto = getBooleanParam("auto");
			if(setauto) {
				//Start an auto backup feature
				MinimaDB.getDB().getUserDB().setAutoBackup(true);
				
			}else {
				//Stop the auto feature
				MinimaDB.getDB().getUserDB().setAutoBackup(false);
			}
			
			if(!setauto) {
				JSONObject resp = new JSONObject();
				resp.put("autobackup", setauto);
				ret.put("backup", resp);
				
				return ret;
			}
		}
		
		//Get the file
		String file = getParam("file","");
		if(file.equals("")) {
			file = "minima-backup-"+System.currentTimeMillis()+".bak";
			//file = "minima_backup_"+DATEFORMAT.format(new Date())+".bak";
		}

		//Get a password if there is one..
		String password = getParam("password","minima"); 
		if(password.equals("")) {
			throw new CommandException("Cannot have a blank password");
		}
		
		boolean complete = getBooleanParam("complete", false);

		boolean debug = getBooleanParam("debug", false);
		
		//Create the file
		File backupfile = MiniFile.createBaseFile(file);
		
		if(debug) {
			MinimaLogger.log("Backup file : "+backupfile.getAbsolutePath());
		}
		
		//Wipe if exists..
		if(backupfile.exists()) {
			backupfile.delete();
		}
		
		///Base folder
		File backupfolder = new File(GeneralParams.DATA_FOLDER,"backup");
		backupfolder.mkdirs();
		
		if(debug) {
			MinimaLogger.log("Backup folder : "+backupfolder.getAbsolutePath());
		}
		
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
			DataOutputStream dos 	= new DataOutputStream(fos);
			
			//Now create a CipherStream.. first need an IVParam
			MiniData ivparam = new MiniData(GenerateKey.IvParam());
			
			//The SALT - for the password
	    	byte[] bsalt 	= new byte[8];
	    	new SecureRandom().nextBytes(bsalt);
	    	MiniData salt = new MiniData(bsalt);
	    	
			//Now write these 2 bits of info to the stream..
			salt.writeDataStream(dos);
			ivparam.writeDataStream(dos);
			
			//Create an AES SecretKey with Password and Salt
			byte[] secret = GenerateKey.secretKey(password,bsalt).getEncoded();
			
			//Create the cipher..
			Cipher ciph = GenerateKey.getCipherSYM(Cipher.ENCRYPT_MODE, ivparam.getBytes(), secret);
			CipherOutputStream cos 		= new CipherOutputStream(dos, ciph);
			GZIPOutputStream gzos		= new GZIPOutputStream(cos);
			DataOutputStream ciphdos 	= new DataOutputStream(gzos);
			
			//Is it Complete
			MiniByte.WriteToStream(ciphdos, complete);
			
			//And now put ALL of those files into a single file..
			walletata.writeDataStream(ciphdos);
			cascadedata.writeDataStream(ciphdos);
			chaindata.writeDataStream(ciphdos);
			userdata.writeDataStream(ciphdos);
			p2pdata.writeDataStream(ciphdos);
			
			if(complete) {
				txpowdata.writeDataStream(ciphdos);
				archivedata.writeDataStream(ciphdos);
			}
			
			//All done..
			ciphdos.close();
			cos.close();
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
			resp.put("auto", MinimaDB.getDB().getUserDB().isAutoBackup());
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
