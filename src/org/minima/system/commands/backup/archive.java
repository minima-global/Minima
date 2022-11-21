package org.minima.system.commands.backup;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;

import org.minima.database.MinimaDB;
import org.minima.database.archive.MySQLConnect;
import org.minima.database.cascade.Cascade;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.wallet.Wallet;
import org.minima.objects.IBD;
import org.minima.objects.TxBlock;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.commands.network.connect;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.minima.NIOMessage;
import org.minima.system.network.webhooks.NotifyManager;
import org.minima.utils.BIP39;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageListener;

public class archive extends Command {

	public archive() {
		super("archive","[action:] (host:) (phrase:) (keys:) (keyuses:) - Resync your chain with seed phrase if necessary (otherwise wallet remains the same)");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","host","phrase","keys","keyuses"}));
	}
	
	@Override
	public String getFullHelp() {
		return "\narchive\n"
				+ "\n"
				+ "Resync your node using an archive node. You need to set the host.\n"
				+ "\n"
				+ "Optionally you can set the seed phrase and this will wipe your wallet and reset it with the data.\n"
				+ "\n"
				+ "action:\n"
				+ "    resync : do a resync\n"
				+ "    integrity : check the integrity of your Archive DB if you are running an archive node yourself. No host required.\n"
				+ "\n"
				+ "host:\n"
				+ "    ip:port of the archive node\n"
				+ "\n"
				+ "phrase:\n"
				+ "    Your seed phrase. This will wipe your wallet. You do NOT have to do this. Just resync and you get on the correct chain.\n"
				+ "\n"
				+ "keys:\n"
				+ "    Number of keys to create, if you seed phrase sync. Defaults to the 64 you normally have + 16 extra incase you used newaddress\n"
				+ "\n"
				+ "keyuses:\n"
				+ "    How many times at most did you use your keys.. Every time you resync with seed phrase this needs to be higher as Minima Signatures are stateful. Defaults to 1000 - the max is 262144 for normal keys\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "archive action:resync host:89.98.89.98:8888\n";
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
	
		String action = getParam("action");
		
		
		if(action.equals("integrity")) {
			
			if(!MinimaDB.getDB().getArchive().isStoreMySQL()) {
				throw new CommandException("You are not running an Archive noide.. ");
			}
			
			//Scan through the entire DB.. checking.. 
			MinimaLogger.log("Checking Archive DB.. this will take some time..");
			
			//Get the MySQL Connect DB
			MySQLConnect mysql = MinimaDB.getDB().getArchive().getMySQLCOnnect();
			
			//Get the cascade
			Cascade dbcasc = mysql.loadCascade(); 
			
			//Get t the initial 1000
			MiniNumber lastlog 		= MiniNumber.ZERO;
			MiniNumber start 		= MiniNumber.ZERO;
			MiniData parenthash 	= null;
			MiniNumber parentnum 	= null;
			int errorsfound 		= 0;
			int total = 0;
			while(true) {
				
				//Do we log a message
				if(lastlog.isLess(start.sub(new MiniNumber(2000)))) {
					MinimaLogger.log("Now checking from  "+start);
					lastlog = start;
				}
				
				//Get some blocks
				ArrayList<TxBlock> blocks = mysql.loadBlockRange(start); 
				for(TxBlock block : blocks) {
					total++;
					
					//Start Checking..
					if(parenthash == null) {
						parenthash 	= block.getTxPoW().getTxPoWIDData();
						parentnum  	= block.getTxPoW().getBlockNumber();
						lastlog 	= parentnum;
						
						MinimaLogger.log("ArchiveDB blocks start at block "+parentnum+" @ "+new Date(block.getTxPoW().getTimeMilli().getAsLong()));
						if(dbcasc != null) {
							MiniNumber tip = dbcasc.getTip().getTxPoW().getBlockNumber();
							MinimaLogger.log("ArchiveDB Cascade tip at block "+tip);
							
							if(!parentnum.isEqual(tip.increment())) {
								MinimaLogger.log("ArchiveDB start does not match Cascade!");
								errorsfound++;
							}	
						}
						
					}else {
						
						//Check correct number
						if(!block.getTxPoW().getBlockNumber().isEqual(parentnum.increment())) {
							MinimaLogger.log("Incorrect child block @ "+block.getTxPoW().getBlockNumber()+" parent:"+parentnum);
							errorsfound++;
						}else if(!block.getTxPoW().getParentID().isEqual(parenthash)) {
							MinimaLogger.log("Parent hash incorrect @ "+block.getTxPoW().getBlockNumber());
							errorsfound++;
						}
						
						parenthash 	= block.getTxPoW().getTxPoWIDData();
						parentnum 	= block.getTxPoW().getBlockNumber();
					}
				}
				
				//Have we checked them all..
				if(blocks.size() < 2) {
					break;
				}
				
				//Now recycle..
				start = parentnum.increment();
			}
			
			JSONObject resp = new JSONObject();
			resp.put("message", "Archive integrity check completed");
			resp.put("blocks", total);
			resp.put("cascade", (dbcasc!=null));
			resp.put("errors", errorsfound);
			
			if(errorsfound>0) {
				resp.put("recommend", "There are errors in your Archive DB - you should wipe then resync with a valid host");
			}else {
				resp.put("recommend", "Your ArchiveDB is correct and has no errors.");
			}
			
			ret.put("response", resp);
			
		}else if(action.equals("resync")) {
			
			//Get the Minima Listener..
			MessageListener minimalistener = Main.getInstance().getMinimaListener();
			
			//Get the host
			String fullhost = getParam("host");
			Message connectdata = connect.createConnectMessage(fullhost);
			
			String host = connectdata.getString("host");
			int port 	= connectdata.getInteger("port");
			
			//How many Keys do we need to generate
			int keys = getNumberParam("keys", new MiniNumber(Wallet.NUMBER_GETADDRESS_KEYS + 16)).getAsInt();
			
			//Set the key uses to this..
			int keyuses = getNumberParam("keyuses", new MiniNumber(1000)).getAsInt();
			
			//Before we start deleting - check connection..
			IBD ibdtest = sendArchiveReq(host, port, MiniNumber.MINUSONE);
			if(ibdtest == null) {
				throw new CommandException("Could Not connect to Archive host!");
			}
			
			//Are we resetting the wallet too ?
			boolean seedphrase 	= false;
			MiniData seed 		= null;
			String phrase = getParam("phrase","");
			if(!phrase.equals("")) {
			
				//reset ALL the default data
				Main.getInstance().archiveResetReady(true);
				
				//This can take soem time..
				MinimaLogger.log("Resetting all wallet private keys..");
				
				//Convert that into a seed..
				seed = BIP39.convertStringToSeed(phrase);
				
				//Set it..
				MinimaDB.getDB().getUserDB().setBasePrivatePhrase(phrase);
				MinimaDB.getDB().getUserDB().setBasePrivateSeed(seed.to0xString());
				MinimaDB.getDB().getWallet().initBaseSeed(seed);
				
				//Get the Wallet
				Wallet wallet = MinimaDB.getDB().getWallet();
				
				//Now cycle through all the default wallet keys..
				MinimaLogger.log("Creating a total of "+keys+" keys / addresses..");
				for(int i=0;i<keys;i++) {
					NotifyListener(minimalistener,"Creating key "+i);
					MinimaLogger.log("Creating key "+i);
					
					//Create a new key..
					wallet.createNewSimpleAddress(true);
				}
				MinimaLogger.log("All keys created..");
				
				//Now Update the USES - since they may have been used before - we don;t know.. 
				wallet.updateAllKeyUses(keyuses);
				
			}else {
				//reset ALL the default data
				Main.getInstance().archiveResetReady(false);
			}
			
			//Now cycle through the chain..
			MiniNumber startblock 	= MiniNumber.ZERO;
			MiniNumber endblock 	= MiniNumber.ZERO;
			boolean foundsome 		= false;
			
			//Are we wiping previous archive
			if(MinimaDB.getDB().getArchive().isStoreMySQL()) {
				MySQLConnect mysql = MinimaDB.getDB().getArchive().getMySQLCOnnect();
				mysql.wipeAll();
				mysql.shutdown();
				mysql.init();
			}
			
			while(true) {
				
				//Send him a message..
				IBD ibd = sendArchiveReq(host, port, startblock);
			
				//Is there a cascade..
				if(startblock.isEqual(MiniNumber.ZERO) && ibd.hasCascade()) {
					MinimaLogger.log("Cascade Received.. "+ibd.getCascade().getTip().getTxPoW().getBlockNumber());
					MinimaDB.getDB().setIBDCascade(ibd.getCascade());
					
					//Do we need to sdave this..
					MinimaDB.getDB().getArchive().checkCascadeRequired(ibd.getCascade());
				}
				
				int size = ibd.getTxBlocks().size();
				
				if(size > 0) {
					foundsome 		= true;
					TxBlock start 	= ibd.getTxBlocks().get(0);
					TxBlock last 	= ibd.getTxBlocks().get(size-1);
					endblock		= last.getTxPoW().getBlockNumber();
					startblock 		= endblock.increment();
					
					MinimaLogger.log("Archive IBD received start : "+start.getTxPoW().getBlockNumber()+" end : "+endblock);
				
					//Notify the Android Listener
					NotifyListener(minimalistener,"Loading "+start.getTxPoW().getBlockNumber()+" @ "+new Date(start.getTxPoW().getTimeMilli().getAsLong()).toString());
				}else {
					MinimaLogger.log("No Archive TxBlocks left..");
				}
			
				//Post it..
				Main.getInstance().getTxPoWProcessor().postProcessArchiveIBD(ibd, "0x00");
			
				//Now wait until processed
				TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
				while(foundsome && tip == null) {
					Thread.sleep(250);
					tip = MinimaDB.getDB().getTxPoWTree().getTip();
				}
				
				//Now wait to catch up..
				MinimaLogger.log("Waiting for chain to catch up.. please wait");
				while(foundsome) {
					if(!tip.getBlockNumber().isEqual(endblock)) {
						Thread.sleep(100);
					}else {
						break;
					}
					
					tip = MinimaDB.getDB().getTxPoWTree().getTip();
				}
				
				//Do we have enough to ask again.. 
				if(size<2) {
					break;
				}
			}
			
			//Notify the Android Listener
			NotifyListener(minimalistener,"All blocks loaded.. pls wait");
			MinimaLogger.log("All Archive data received and processed.. shutting down.."); 
			
			//DOUBLE CHECK
			TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
			while(foundsome && tip == null) {
				Thread.sleep(1000);
				tip = MinimaDB.getDB().getTxPoWTree().getTip();
			}
			
			//Now wait to catch up..
			while(foundsome) {
				if(!tip.getBlockNumber().isEqual(endblock)) {
					MinimaLogger.log("Waiting for chain to catch up.. please wait");
					Thread.sleep(5000);
				}else {
					break;
				}
				
				tip = MinimaDB.getDB().getTxPoWTree().getTip();
			}
			
			//And NOW shut down..
			Main.getInstance().getTxPoWProcessor().stopMessageProcessor();
			
			//Don't do the usual shutdown hook
			Main.getInstance().setHasShutDown();
			
			//And NOW shut down..
			Main.getInstance().stopMessageProcessor();
			
			//Now shutdown and save everything
			MinimaDB.getDB().saveAllDB();
			
			JSONObject resp = new JSONObject();
			resp.put("message", "Archive sync completed.. shutting down now.. pls restart after");
			resp.put("start", "0");
			resp.put("end", endblock.toString());
			ret.put("response", resp);
		
		}else {
			throw new CommandException("Invalid action : "+action);
		}
		
		return ret;
	}
	
	public void NotifyListener(MessageListener zListener, String zMessage) {
		//Notify
		if(zListener != null) {
			
			//Details..
			JSONObject data = new JSONObject();
			data.put("message", zMessage);
			
			//Create the JSON Message
			JSONObject notify = new JSONObject();
			notify.put("event", "ARCHIVEUPDATE");
			notify.put("data", data);
			
			Message msg = new Message(NotifyManager.NOTIFY_POST);
			msg.addObject("notify", notify);
			
			//Notify them that something is happening..
			zListener.processMessage(msg);
		}
	}
	
	@Override
	public Command getFunction() {
		return new archive();
	}

	/**
	 * A special PING message to  check a valid connection..
	 */
	public IBD sendArchiveReq(String zHost, int zPort, MiniNumber zStartBlock) {
		
		IBD ibd= null;
		
		try {
			
			//Create the Network Message
			MiniData msg = NIOManager.createNIOMessage(NIOMessage.MSG_ARCHIVE_REQ, zStartBlock);
			
			//Open the socket..
			Socket sock = new Socket();

			//3 seconds to connect
			sock.connect(new InetSocketAddress(zHost, zPort), 10000);
			
			//10 seconds to read
			sock.setSoTimeout(10000);
			
			//Create the streams..
			OutputStream out 		= sock.getOutputStream();
			DataOutputStream dos 	= new DataOutputStream(out);
			
			InputStream in			= sock.getInputStream();
			DataInputStream dis 	= new DataInputStream(in);
			
			//Write the data
			msg.writeDataStream(dos);
			dos.flush();
			
			//Tell the NIO
			Main.getInstance().getNIOManager().getTrafficListener().addWriteBytes(msg.getLength());
			
			//Load the message
			MiniData resp = MiniData.ReadFromStream(dis);
			
			//Tell the NIO
			Main.getInstance().getNIOManager().getTrafficListener().addReadBytes(resp.getLength());
			
			//Close the streams..
			dis.close();
			in.close();
			dos.close();
			out.close();
			
			//Convert
			ByteArrayInputStream bais 	= new ByteArrayInputStream(resp.getBytes());
			DataInputStream bdis 		= new DataInputStream(bais);

			//What Type..
			MiniByte type = MiniByte.ReadFromStream(bdis);
			
			//Load the IBD
			ibd = IBD.ReadFromStream(bdis);
			
			bdis.close();
			bais.close();
		
		}catch(Exception exc){
			MinimaLogger.log(exc);
			
			ibd= null;
		}
		
		return ibd;
	}
}
