package org.minima.system.commands.base;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.archive.ArchiveManager;
import org.minima.objects.TxBlock;
import org.minima.objects.base.MiniData;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class test extends Command {

	public test() {
		super("test","test Funxtion");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"show","action"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
	
		String action = getParam("action");
		
		//Get the archive db
		ArchiveManager arch = MinimaDB.getDB().getArchive();
		
		
		if(action.equals("last")) {
			
			//What is my last block
			TxBlock lastblock 	= arch.loadLastBlock();
			
			ret.put("response", lastblock.getTxPoW().getBlockNumber());
		
		}else if(action.equals("first")) {
			
			//What is my last block
			TxBlock block 	= arch.loadFirstBlock();
			
			ret.put("response", block.getTxPoW().getBlockNumber());
		
		}
			
		
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