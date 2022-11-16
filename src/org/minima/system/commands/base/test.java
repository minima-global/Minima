package org.minima.system.commands.base;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Date;

import org.minima.database.MinimaDB;
import org.minima.database.archive.ArchiveManager;
import org.minima.objects.TxBlock;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.commands.Command;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;

public class test extends Command {

	public test() {
		super("test","test Funxtion");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
	
		//Get the Archive DB
		ArchiveManager arch = MinimaDB.getDB().getArchive();
		
		//Get the current lowest block
		TxBlock txbbefore = arch.loadLastBlock();
		
		JSONObject resp = new JSONObject();
		
		TxBlock firstblock = arch.loadFirstBlock();
		TxBlock lastblock = arch.loadLastBlock();
		resp.put("xfirstblock", firstblock.getTxPoW().getBlockNumber());
		resp.put("xlastblock", lastblock.getTxPoW().getBlockNumber());
		
		resp.put("archivesizebefore", arch.getSize());
		resp.put("lastblock", txbbefore.getTxPoW().getBlockNumber().toString());
		resp.put("lasttime", new Date(txbbefore.getTxPoW().getTimeMilli().getAsLong()));
		resp.put("lastjson", arch.loadLastBlockJSON());
		
		//Now do a cleanDB
		int del = arch.cleanDB();
		MinimaLogger.log("DELETED : "+del);
		
		txbbefore = arch.loadLastBlock();
		
		resp.put("archiveafter", arch.getSize());
		resp.put("aftlastblock", txbbefore.getTxPoW().getBlockNumber().toString());
		resp.put("aftlasttime", new Date(txbbefore.getTxPoW().getTimeMilli().getAsLong()));
		resp.put("afterjson", arch.loadLastBlockJSON());
		
		ArrayList<TxBlock> blocks = arch.loadBlockRange(new MiniNumber(2000), new MiniNumber(2010));
		for(TxBlock block : blocks) {
			MinimaLogger.log(block.getTxPoW().getBlockNumber().toString());
		}
		
		ret.put("response", resp);
	
		return ret;
	}
	
	@Override
	public Command getFunction() {
		return new test();
	}

	public static void main(String[] zArgs) {
		
		for(int i=0;i<512;i++) {
			
			MiniData data = new MiniData(new BigInteger(Integer.toString(i)));
			
			System.out.println(data.to0xString());
			
		}
		
		
		
	}
}
