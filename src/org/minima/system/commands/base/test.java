package org.minima.system.commands.base;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.archive.ArchiveManager;
import org.minima.objects.TxBlock;
import org.minima.objects.TxPoW;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.network.minima.NIOClient;
import org.minima.system.network.minima.NIOClientInfo;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.minima.NIOMessage;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;

public class test extends Command {

	public test() {
		super("test","test Funxtion");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		
		//Get the archive db
		ArchiveManager arch = MinimaDB.getDB().getArchive();
		
		//What is my last block
		TxBlock lastblock 	= arch.loadLastBlock();
		TxPoW lastpow 		= null;
		if(lastblock == null) {
			MinimaLogger.log("No TxBlock using txpowtree..");
			lastpow = MinimaDB.getDB().getTxPoWTree().getRoot().getTxPoW();
		}else {
			MinimaLogger.log("TxBlock found..");
			lastpow = lastblock.getTxPoW();
		}
		
		MinimaLogger.log("Last TxPoW .."+lastpow.getBlockNumber()+" "+lastpow.getTxPoWID());
		
		//Get a random NIOClient
		ArrayList<NIOClient> allclients =  Main.getInstance().getNIOManager().getAllValidConnectedClients();
		MinimaLogger.log("Found "+allclients.size()+" clients");
		
		//Pick a random one..
		NIOClient client = allclients.get(0);
		
		//Send him a message
		Main.getInstance().getNIOManager().sendNetworkMessage(client.getUID(), NIOMessage.MSG_TXBLOCK_REQ, lastpow);
		
		ret.put("response", true);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new test();
	}

}
