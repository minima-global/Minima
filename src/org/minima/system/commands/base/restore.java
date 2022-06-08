package org.minima.system.commands.base;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.File;
import java.io.IOException;
import java.util.zip.GZIPInputStream;

import org.minima.database.MinimaDB;
import org.minima.objects.base.MiniData;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MiniFile;
import org.minima.utils.json.JSONObject;

public class restore extends Command {

	public restore() {
		super("restore","[file:] - Restore the entire system.");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String file = getParam("file","");
		if(file.equals("")) {
			throw new Exception("MUST specify a file to restore from");
		}
		
		//Does it exist..
		File restorefile = new File(file);
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
		GZIPInputStream gzin 		= new GZIPInputStream(bais);
		DataInputStream dis 		= new DataInputStream(gzin);
		
		//The total size of files..
		long total = 0;
		
		//Read in each section..
		total += readNextBackup(new File(restorefolder,"wallet.sql"), dis);
		total += readNextBackup(new File(restorefolder,"txpowdb.sql"), dis);
		total += readNextBackup(new File(restorefolder,"archive.sql"), dis);
		
		//The rest write directly 
		File basedb = MinimaDB.getDB().getBaseDBFolder();
		total += readNextBackup(new File(basedb,"cascade.db"), dis);
		total += readNextBackup(new File(basedb,"chaintree.db"), dis);
		total += readNextBackup(new File(basedb,"userprefs.db"), dis);
		total += readNextBackup(new File(basedb,"p2p.db"), dis);
		
		//Now load the sql
		MinimaDB.getDB().getWallet().restoreFromFile(new File(restorefolder,"wallet.sql"));
		MinimaDB.getDB().getTxPoWDB().getSQLDB().restoreFromFile(new File(restorefolder,"txpowdb.sql"));
		MinimaDB.getDB().getArchive().restoreFromFile(new File(restorefolder,"archive.sql"));
		
		//Close up shop..
		dis.close();
		gzin.close();
		bais.close();
		
		//And now clean up..
		MiniFile.deleteFileOrFolder(GeneralParams.DATA_FOLDER, restorefolder);
		
		//And send data
		JSONObject resp = new JSONObject();
		resp.put("file", restorefile.getAbsolutePath());
		ret.put("restore", resp);
		ret.put("message", "Restart Minima for restore to take effect!");
		
		//Now save the Databases..
		MinimaDB.getDB().saveSQL();
		
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
