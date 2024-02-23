package org.minima.system.commands.backup;

import java.io.BufferedOutputStream;
import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
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
import org.minima.utils.MiniFormat;
import org.minima.utils.MinimaLogger;
import org.minima.utils.encrypt.GenerateKey;
import org.minima.utils.json.JSONObject;
import org.minima.utils.ssl.SSLManager;

public class decryptbackup extends Command {

	public decryptbackup() {
		super("decryptbackup","[file:] (password:) (output:)- Decrypt an encrypted backup.");
	}
	
	@Override
	public String getFullHelp() {
		return "\ndecryptbackup \n"
				+ "\n"
				+ "Decrypt an encrypted backup.\n"
				+ "\n"
				+ "file:\n"
				+ "    Specify the filename or local path of the backup to restore\n"
				+ "\n"
				+ "password: (optional)\n"
				+ "    Enter the password of the backup \n"
				+ "\n"
				+ "output: (optional)\n"
				+ "    Specify the output file \n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "decryptbackup file:my-full-backup-01-Jan-22 password:Longsecurepassword456\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"file","password","output"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String file = getParam("file");
		String password = getParam("password","minima");
		if(password.equals("")) {
			throw new CommandException("Cannot have a blank password");
		}
		
		//Does it exist..
		File restorefile = MiniFile.createBaseFile(file);
		if(!restorefile.exists()) {
			throw new Exception("Restore file doesn't exist : "+restorefile.getAbsolutePath());
		}
		
		//Get the Output file
		String outfile = getParam("output","");
		if(outfile.equals("")) {
			outfile = "decrypted-"+restorefile.getName();
		}
		File outputfile = MiniFile.createBaseFile(outfile);
		//MinimaLogger.log("Output : "+outputfile.getAbsolutePath());
		if(outputfile.exists()) {
			outputfile.delete();
		}
		
		FileOutputStream fos 		= new FileOutputStream(outputfile);
		BufferedOutputStream bos 	= new BufferedOutputStream(fos);
		
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
		Cipher ciph 			= GenerateKey.getCipherSYM(Cipher.DECRYPT_MODE, ivparam.getBytes(), secret);
		CipherInputStream cis 	= new CipherInputStream(dis, ciph);
		
		GZIPInputStream gzin 	= null;
		try {
			gzin 	= new GZIPInputStream(cis);
		}catch(Exception exc) {
			//Incorrect password ?
			throw new CommandException("Incorrect Password!");
		}
		
		//Now read all the data.. and output straight to the file..
		try {
	        byte[] buffer = new byte[16384];
	        int length;
	        while ((length = gzin.read(buffer)) > 0) {
	            bos.write(buffer, 0, length);
	        }
	    }catch (Exception e) {
			// TODO: handle exception
		} 
		
		bos.flush();
		
		//Close up shop..
		bos.close();
		fos.close();
		
		cis.close();
		dis.close();
		gzin.close();
		bais.close();
		
		//And send data
		JSONObject resp = new JSONObject();
		resp.put("input", restorefile.getAbsolutePath());
		resp.put("inputsize", MiniFormat.formatSize(restorefile.length()));
		resp.put("output", outputfile.getAbsolutePath());
		resp.put("outputsize", MiniFormat.formatSize(outputfile.length()));
		resp.put("message", "You can now open the output file in a HEX editor to get the seed if your backup was not locked");
		
		ret.put("response", resp);
		
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
		return new decryptbackup();
	}

}
