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
import java.util.Random;

import org.minima.database.MinimaDB;
import org.minima.database.archive.ArchiveManager;
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
import org.minima.system.network.p2p.params.P2PParams;
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
		
		//Get the ArchiveManager
		ArchiveManager arch = MinimaDB.getDB().getArchive();
		
		if(action.equals("integrity")) {
			
			//Scan through the entire DB.. checking.. 
			MinimaLogger.log("Checking Archive DB.. this may take some time..");
			
			//What is the first block in the DB
			TxBlock starterblock = arch.loadLastBlock();
			
			//What is the first entry
			boolean startcheck 	= true;
			boolean startatroot = false;
			MiniNumber lastlog 	= MiniNumber.ZERO;
			MiniNumber start 	= MiniNumber.ZERO;
			if(starterblock == null) {
				MinimaLogger.log("You have no Archive blocks..");
				startcheck = false;
			}else {
				lastlog = starterblock.getTxPoW().getBlockNumber();
				start 	= lastlog;
				
				//Is it from root..
				MiniNumber startblocknumber = starterblock.getTxPoW().getBlockNumber();
				if(startblocknumber.isEqual(MiniNumber.ONE)) {
					MinimaLogger.log("ArchiveDB starts at root");
					startatroot = true;
				}
			}
			
			//Get the cascade
			Cascade dbcasc = arch.loadCascade(); 
			if(dbcasc != null) {
				//Get the tip..
				MiniNumber tip = dbcasc.getTip().getTxPoW().getBlockNumber();
				
				//Can start from cascade
				MinimaLogger.log("ArchiveDB cascade start : "+tip);
				
				//Start the test from then onwards
				if(!startatroot) {
					lastlog = tip.increment();
					start 	= lastlog;
				}
				
			}else {
				
				//Can start from cascade
				MinimaLogger.log("ArchiveDB has no cascade ");
			}
			
			//Get t the initial 1000
			MiniData parenthash 	= null;
			MiniNumber parentnum 	= null;
			int errorsfound 		= 0;
			int total = 0;
			MiniNumber archstart = start;
			
			while(startcheck) {
				
				//Do we log a message
				if(lastlog.isLess(start.sub(new MiniNumber(2000)))) {
					MinimaLogger.log("Now checking from  "+start);
					lastlog = start;
				}
				
				//Use batches of 256
				MiniNumber end = start.add(MiniNumber.TWOFIVESIX);
				
				//Get some blocks
				ArrayList<TxBlock> blocks = arch.loadBlockRange(start.decrement(),end,false); 
				
				for(TxBlock block : blocks) {
					total++;
					
					//Start Checking..
					if(parenthash == null) {
						parenthash 	= block.getTxPoW().getTxPoWIDData();
						parentnum  	= block.getTxPoW().getBlockNumber();
						lastlog 	= parentnum;
						
						archstart 	= parentnum;
						
						MinimaLogger.log("ArchiveDB blocks resync start at block "+parentnum+" @ "+new Date(block.getTxPoW().getTimeMilli().getAsLong()));
						
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
				if(blocks.size()==0) {
					break;
				}
				
				//Now recycle..
				start = parentnum.increment();
			}
			
			JSONObject resp = new JSONObject();
			resp.put("message", "Archive integrity check completed");
			resp.put("start", archstart);
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
			
			//Is it auto
			if(fullhost.equals("auto")) {
				
				//Choose one from our default list
				int size  	= P2PParams.DEFAULT_ARCHIVENODE_LIST.size();
				int rand  	= new Random().nextInt(size);
				
				InetSocketAddress archaddr = P2PParams.DEFAULT_ARCHIVENODE_LIST.get(rand);
				String ip 	= archaddr.getHostString();
				int port    = archaddr.getPort();
				fullhost	= ip+":"+port;
				
				MinimaLogger.log("RANDOM ARCHIVE HOST : "+rand+" host:"+fullhost);
			}
			
			Message connectdata = connect.createConnectMessage(fullhost);
			
			String host = connectdata.getString("host");
			int port 	= connectdata.getInteger("port");
			
			//How many Keys do we need to generate
			int keys = getNumberParam("keys", new MiniNumber(Wallet.NUMBER_GETADDRESS_KEYS)).getAsInt();
			
			//Set the key uses to this..
			int keyuses = getNumberParam("keyuses", new MiniNumber(1000)).getAsInt();
			
			//Before we start deleting - check connection..
			IBD ibdtest = sendArchiveReq(host, port, MiniNumber.MINUSONE);
			if(ibdtest == null) {
				throw new CommandException("Could not connect to Archive host! @ "+host+":"+port);
			}
			
			//Are we resetting the wallet too ?
			MiniData seed 		= null;
			String phrase = getParam("phrase","");
			if(!phrase.equals("")) {
			
				//Clean it up..
				String cleanphrase = BIP39.cleanSeedPhrase(phrase);
				
				//reset ALL the default data
				Main.getInstance().archiveResetReady(true);
				
				//This can take soem time..
				MinimaLogger.log("Resetting all wallet private keys..");
				
				//Convert that into a seed..
				seed = BIP39.convertStringToSeed(cleanphrase);
				
				//Get the Wallet
				Wallet wallet = MinimaDB.getDB().getWallet();
				
				//Set it..
				wallet.updateSeedRow(cleanphrase, seed.to0xString());
				
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
			boolean firstrun 		= true;
			MiniNumber firstStart   = MiniNumber.ZERO;
			
			while(true) {
				
				//Send him a message..
				IBD ibd = sendArchiveReq(host, port, startblock);
				if(ibd == null) {
					throw new CommandException("Connection error @ "+host+":"+port);
				}
				
				//Is there a cascade..
				if(startblock.isEqual(MiniNumber.ZERO) && ibd.hasCascade()) {
					MinimaLogger.log("Cascade Received.. "+ibd.getCascade().getTip().getTxPoW().getBlockNumber());
					
					//Set it as our cascade
					MinimaDB.getDB().setIBDCascade(ibd.getCascade());
					
					//Do we need to save this..
					MinimaDB.getDB().getArchive().checkCascadeRequired(ibd.getCascade());
				}
				
				int size = ibd.getTxBlocks().size();
				
				if(size > 0) {
					foundsome 		= true;
					TxBlock start 	= ibd.getTxBlocks().get(0);
					if(firstrun) {
						firstrun 	= false;
						firstStart 	= start.getTxPoW().getBlockNumber();
					}
					
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
			
				//Now wait for something to happen
				boolean error = false;
				TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
				int attempts = 0;
				while(foundsome && tip == null) {
					Thread.sleep(250);
					tip = MinimaDB.getDB().getTxPoWTree().getTip();
					attempts++;
					if(attempts>16) {
						error = true;
						break;
					}
				}
				
				if(error) {
					MinimaLogger.log("ERROR : There was an error processing that IBD");
					break;
				}
				
				//Now wait to catch up..
				MinimaLogger.log("Waiting for chain to catch up.. please wait");
				attempts = 0;
				while(foundsome) {
					if(!tip.getBlockNumber().isEqual(endblock)) {
						Thread.sleep(100);
					}else {
						break;
					}
					
					tip = MinimaDB.getDB().getTxPoWTree().getTip();
					
					attempts++;
					if(attempts>100) {
						error = true;
						break;
					}
				}
				
				if(error) {
					MinimaLogger.log("ERROR : There was an error processing that IBD");
					break;
				}
				
				//Do we have enough to ask again.. 
				if(size==0) {
					break;
				}
			}
			
			//Notify the Android Listener
			NotifyListener(minimalistener,"All blocks loaded.. pls wait");
			MinimaLogger.log("All Archive data received and processed.. shutting down.."); 
			
			JSONObject resp = new JSONObject();
			resp.put("message", "Archive sync completed.. shutting down now.. please restart after");
			resp.put("start", firstStart.toString());
			resp.put("end", endblock.toString());
			ret.put("response", resp);
			
			//And NOW shut down..
			Main.getInstance().getTxPoWProcessor().stopMessageProcessor();
			
			//Now shutdown and save everything
			MinimaDB.getDB().saveAllDB();
			
			//Don't do the usual shutdown hook
			Main.getInstance().setHasShutDown();
			
			//And NOW shut down..
			Main.getInstance().stopMessageProcessor();
			
		}else {
			throw new CommandException("Invalid action : "+action);
		}
		
		return ret;
	}
	
	public void NotifyListener(MessageListener zListener, String zMessage) throws Exception {
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
			MinimaLogger.log("Archive connection : "+exc+" @ "+zHost+":"+zPort);
			
			ibd= null;
		}
		
		return ibd;
	}
}
