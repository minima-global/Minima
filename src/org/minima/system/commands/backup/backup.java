package org.minima.system.commands.backup;

import java.io.DataOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.security.SecureRandom;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
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
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"password","file","auto","complete"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		System.out.println("4.1");
		//Is this an AUTO backup initiate..
		if(existsParam("auto")) {
			System.out.println("4.2");
			boolean setauto = getBooleanParam("auto");
			if(setauto) {
				//Start an auto backup feature
				System.out.println("4.31");
				MinimaDB.getDB().getUserDB().setAutoBackup(true);
				System.out.println("4.32");
				
			}else {
				System.out.println("4.41");
				//Stop the auto feature
				MinimaDB.getDB().getUserDB().setAutoBackup(false);
				System.out.println("4.42");
			}
			
			if(!setauto) {
				System.out.println("4.5");
				JSONObject resp = new JSONObject();
				System.out.println("4.6");
				resp.put("autobackup", setauto);
				ret.put("backup", resp);
				System.out.println("4.7");
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
		
			System.out.println("4.8");
			//Save the current state..
			MinimaDB.getDB().saveState();
			System.out.println("4.9");
			//Write the SQL Dbs
			File walletfile = new File(backupfolder,"wallet.sql");
			MinimaDB.getDB().getWallet().backupToFile(walletfile);
			MiniData walletata 	= new MiniData(MiniFile.readCompleteFile(walletfile));
			System.out.println("4.19");
			File cascade = new File(backupfolder,"cascade.bak");
			MinimaDB.getDB().getCascade().saveDB(cascade);
			MiniData cascadedata = new MiniData(MiniFile.readCompleteFile(cascade));
			System.out.println("42.9");
			File chain = new File(backupfolder,"chaintree.bak");
			MinimaDB.getDB().getTxPoWTree().saveDB(chain);
			MiniData chaindata = new MiniData(MiniFile.readCompleteFile(chain));
			System.out.println("43.9");
			File userdb = new File(backupfolder,"userdb.bak");
			MinimaDB.getDB().getUserDB().saveDB(userdb);
			MiniData userdata = new MiniData(MiniFile.readCompleteFile(userdb));
			System.out.println("44.9");
			File p2pdb = new File(backupfolder,"p2p.bak");
			MinimaDB.getDB().getP2PDB().saveDB(p2pdb);
			MiniData p2pdata = new MiniData(MiniFile.readCompleteFile(p2pdb));
			System.out.println("45.9");
			//For Complete..
			File txpowdb 	= new File(backupfolder,"txpowdb.sql");
			File archivedb 	= new File(backupfolder,"archive.sql");
			System.out.println("46.9");
			MiniData txpowdata 		= null;
			MiniData archivedata 	= null;
			
			if(complete) {
				System.out.println("47.9");
				MinimaDB.getDB().getTxPoWDB().getSQLDB().backupToFile(txpowdb);
				txpowdata	= new MiniData(MiniFile.readCompleteFile(txpowdb));
				
				MinimaDB.getDB().getArchive().backupToFile(archivedb);
				archivedata = new MiniData(MiniFile.readCompleteFile(archivedb));
			System.out.println("48.9");
			}
			System.out.println("49.9");
			//Now create the streams to save these
			FileOutputStream fos 	= new FileOutputStream(backupfile);
			DataOutputStream dos 	= new DataOutputStream(fos);
			System.out.println("40.9");
			//Now create a CipherStream.. first need an IVParam
			MiniData ivparam = new MiniData(GenerateKey.IvParam());
			System.out.println("411.9");
			//The SALT - for the password
	    	byte[] bsalt 	= new byte[8];
	    	new SecureRandom().nextBytes(bsalt);
	    	MiniData salt = new MiniData(bsalt);
	    	System.out.println("412.9");
			//Now write these 2 bits of info to the stream..
			salt.writeDataStream(dos);
			ivparam.writeDataStream(dos);
			System.out.println("413.9");
			//Create an AES SecretKey with Password and Salt
			byte[] secret = GenerateKey.secretKey(password,bsalt).getEncoded();
			
			//Create the cipher..
			Cipher ciph = GenerateKey.getCipherSYM(Cipher.ENCRYPT_MODE, ivparam.getBytes(), secret);
			CipherOutputStream cos 		= new CipherOutputStream(dos, ciph);
			GZIPOutputStream gzos		= new GZIPOutputStream(cos);
			DataOutputStream ciphdos 	= new DataOutputStream(gzos);
			System.out.println("413.9");
			//Is it Complete
			MiniByte.WriteToStream(ciphdos, complete);
			System.out.println("414.9");
			//And now put ALL of those files into a single file..
			walletata.writeDataStream(ciphdos);
			cascadedata.writeDataStream(ciphdos);
			chaindata.writeDataStream(ciphdos);
			userdata.writeDataStream(ciphdos);
			p2pdata.writeDataStream(ciphdos);
			System.out.println("415.9");
			if(complete) {
				txpowdata.writeDataStream(ciphdos);
				archivedata.writeDataStream(ciphdos);
			}
			System.out.println("416.9");
			//All done..
			ciphdos.close();
			cos.close();
			dos.close();
			gzos.close();
			fos.close();
			System.out.println("417.9");
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
			System.out.println("418.9");
			//Get all the individual File sizes..
			JSONObject files = new JSONObject();
			files.put("wallet", MiniFormat.formatSize(walletfile.length()));
			System.out.println("419.9");
			if(complete) {
				files.put("txpowdb", MiniFormat.formatSize(txpowdb.length()));
				files.put("archive", MiniFormat.formatSize(archivedb.length()));
			}
			System.out.println("4100.9");
			files.put("cascade", MiniFormat.formatSize(cascade.length()));
			files.put("chain", MiniFormat.formatSize(chain.length()));
			files.put("user", MiniFormat.formatSize(userdb.length()));
			files.put("p2p", MiniFormat.formatSize(p2pdb.length()));
			System.out.println("4101.9");
			//And send data
			JSONObject resp = new JSONObject();
			System.out.println("aaaaaaaa.1");

			System.out.println("TxPowTree: ");
			System.out.println(MinimaDB.getDB().getTxPoWTree().toString());
			
			System.out.println("getTip: ");
			System.out.println(MinimaDB.getDB().getTxPoWTree().getTip().toString());
			
			System.out.println("getTxPoW: ");
			System.out.println(MinimaDB.getDB().getTxPoWTree().getTip().getTxPoW().toString());
			
			System.out.println("getBlockNumber: ");
			System.out.println(MinimaDB.getDB().getTxPoWTree().getTip().getTxPoW().getBlockNumber().toString());
			

			resp.put("block", MinimaDB.getDB().getTxPoWTree().getTip().getTxPoW().getBlockNumber());
			System.out.println("aaaaaaaa.2");

			resp.put("files", files);
			System.out.println("aaaaaaaa.3");

			resp.put("uncompressed", MiniFormat.formatSize(total));
			System.out.println("aaaaaaaa.4");
			
			resp.put("file", backupfile.getAbsolutePath());
			System.out.println("aaaaaaaa.5");

			resp.put("size", MiniFormat.formatSize(backupfile.length()));
			System.out.println("aaaaaaaa.6");

			resp.put("auto", MinimaDB.getDB().getUserDB().isAutoBackup());
			System.out.println("aaaaaaaa.7");

			ret.put("backup", resp);
			System.out.println("4102.9");
			//And now clean up..
			MiniFile.deleteFileOrFolder(GeneralParams.DATA_FOLDER, backupfolder);
			System.out.println("4103.9");
		}catch(Exception exc) {
			
			System.out.println("commandException");
			System.out.println(exc.toString());
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
