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
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MiniFile;
import org.minima.utils.encrypt.GenerateKey;
import org.minima.utils.json.JSONObject;
import org.minima.utils.ssl.SSLManager;

public class restore extends Command {

	public restore() {
		super("restore","[file:] (password:) - Restore the entire system.");
	}
	
	@Override
	public String getFullHelp() {
		return "\nrestore\n"
				+ "\n"
				+ "Restore your node from a backup.\n"
				+ "\n"
				+ "file:\n"
				+ "    Specify the filename or local path of the backup to restore\n"
				+ "\n"
				+ "password: (optional)\n"
				+ "    Enter the password of the backup \n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "restore file:my-full-backup-01-Jan-22 password:Longsecurepassword456\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"file","password"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String file = getParam("file","");
		if(file.equals("")) {
			throw new Exception("MUST specify a file to restore from");
		}
		
		//Get a password if there is one..
		String password = getParam("password","minima");
		if(password.equals("")) {
			throw new CommandException("Cannot have a blank password");
		}
		
		//Does it exist..
		File restorefile = MiniFile.createBaseFile(file);
		if(!restorefile.exists()) {
			throw new Exception("Restore file doesn't exist : "+restorefile.getAbsolutePath());
		}
		
		///Base folder
		File restorefolder = new File(GeneralParams.DATA_FOLDER, "restore");
		restorefolder.mkdirs();
		
		//First stop everything.. and get ready to restore the files..
		Main.getInstance().restoreReady();
		
		//Open the file..
		byte[] restoredata = MiniFile.readCompleteFile(restorefile);
		
		//Now start reading in the sections..
		ByteArrayInputStream bais 	= new ByteArrayInputStream(restoredata);
		DataInputStream dis 		= new DataInputStream(bais);
		
		//Read in the SALT and IVParam
		MiniData salt 		= MiniData.ReadFromStream(dis);
		MiniData ivparam 	= MiniData.ReadFromStream(dis);
		
		//Create an AES SecretKey with Password and Salt
		byte[] secret = GenerateKey.secretKey(password,salt.getBytes()).getEncoded();
		
		//Create the cipher..
		Cipher ciph = GenerateKey.getCipherSYM(Cipher.DECRYPT_MODE, ivparam.getBytes(), secret);
		CipherInputStream cis 	= new CipherInputStream(dis, ciph);
		GZIPInputStream gzin 	= new GZIPInputStream(cis);
		DataInputStream disciph = new DataInputStream(gzin);
		
		//Is this a complete backup..
		boolean complete = MiniByte.ReadFromStream(disciph).isTrue();
		
		//The total size of files..
		long total = 1;
		
		//Read in each section..
		total += readNextBackup(new File(restorefolder,"wallet.sql"), disciph);
		
		//The rest write directly 
		File basedb = MinimaDB.getDB().getBaseDBFolder();
		total += readNextBackup(new File(basedb,"cascade.db"), disciph);
		total += readNextBackup(new File(basedb,"chaintree.db"), disciph);
		total += readNextBackup(new File(basedb,"userprefs.db"), disciph);
		total += readNextBackup(new File(basedb,"p2p.db"), disciph);
		
		//Now load the sql
		MinimaDB.getDB().getWallet().restoreFromFile(new File(restorefolder,"wallet.sql"));
				
		//Complete
		if(complete) {
			total += readNextBackup(new File(restorefolder,"txpowdb.sql"), disciph);
			total += readNextBackup(new File(restorefolder,"archive.sql"), disciph);
		
			MinimaDB.getDB().getTxPoWDB().getSQLDB().restoreFromFile(new File(restorefolder,"txpowdb.sql"));
			MinimaDB.getDB().getArchive().restoreFromFile(new File(restorefolder,"archive.sql"));
		}else {
	
			//Close and Wipe those..
			MinimaDB.getDB().getTxPoWDB().getSQLDB().saveDB();
			MinimaDB.getDB().getTxPoWDB().getSQLDB().getSQLFile().delete();
			
			MinimaDB.getDB().getArchive().saveDB();
			MinimaDB.getDB().getArchive().getSQLFile().delete();
		}
		
		//Close up shop..
		disciph.close();
		cis.close();
		dis.close();
		gzin.close();
		bais.close();
		
		//And now clean up..
		MiniFile.deleteFileOrFolder(GeneralParams.DATA_FOLDER, restorefolder);
		
		//And will need to recreate the SSL
		MiniFile.deleteFileOrFolder(GeneralParams.DATA_FOLDER, SSLManager.getSSLFolder());
		
		//And send data
		JSONObject resp = new JSONObject();
		resp.put("file", restorefile.getAbsolutePath());
		ret.put("restore", resp);
		ret.put("message", "Restart Minima for restore to take effect!");
		
		//Now save the Databases..
		if(complete) {
			MinimaDB.getDB().saveSQL();
		}else {
			MinimaDB.getDB().saveWalletSQL();
		}
		
		//Don't do the usual shutdown hook
		Main.getInstance().setHasShutDown();
		
		//And NOW shut down..
		Main.getInstance().stopMessageProcessor();
		
		return ret;
	}
	
	private long readNextBackup(File zOutput, DataInputStream zDis) throws IOException {
		MiniData data = MiniData.ReadFromStream(zDis);
		MiniFile.writeDataToFile(zOutput, data.getBytes());
		return zOutput.length();
	}

	@Override
	public Command getFunction() {
		return new restore();
	}

}
