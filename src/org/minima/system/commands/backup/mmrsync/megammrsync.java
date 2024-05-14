package org.minima.system.commands.backup.mmrsync;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.wallet.ScriptRow;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Coin;
import org.minima.objects.CoinProof;
import org.minima.objects.IBD;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.commands.backup.archive;
import org.minima.system.commands.backup.vault;
import org.minima.system.commands.network.connect;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.minima.NIOMessage;
import org.minima.system.params.GeneralParams;
import org.minima.utils.BIP39;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class megammrsync extends Command {

	public megammrsync() {
		super("megammrsync","[action:] [host:] (phrase:) (keys:) (keyuses:) - Restore from a MegaMMR node");
	}
	
	@Override
	public String getFullHelp() {
		return "\nmegammrsync\n"
				+ "\n"
				+ "Perform a chain or seed re-sync from a MegaMMR node. Fast.\n"
				+ "\n"
				+ "If you are on the wrong chain - all you need to provide is the 'host' to connect to.\n"
				+ "\n"
				+ "If you are on a fresh node, with different seed phrase, provide 'host' and 'phrase' to resync with that Wallet.\n"
				+ "\n"
				+ "The host you connect to MUST be running with -megammr.\n"
				+ "\n"
				+ "action:\n"
				+ "    mydetails : Shows which addresses and public keys you are searching for.\n"
				+ "    resync : Perform the actual MegaMMR resync.\n"
				+ "\n"
				+ "host:\n"
				+ "    ip:port of the node to sync from. Use with action:resync.\n"
				+ "\n"
				+ "phrase: (optional)\n"
				+ "    To seed re-sync, enter your seed phrase in double quotes. Use with action:resync.\n"
				+ "    This will replace the current seed phrase of this node. You do NOT have to do this if you still have access to your wallet.\n"
				+ "\n"
				+ "anyphrase: (optional)\n"
				+ "    true or false. If you set a custom seed phrase on startup, you can set this to true. Default is false.\n"
				+ "\n"
				+ "keys: (optional) \n"
				+ "    Number of keys to create if you need to do a seed re-sync. Default is 64.\n"
				+ "\n"
				+ "keyuses: (optional) \n"
				+ "    How many times at most you used your keys..\n"
				+ "    Every time you re-sync with seed phrase this needs to be higher as Minima Signatures are stateful.\n"
				+ "    Defaults to 1000 - the max is 262144 for normal keys.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "megammrsync action:mydetails\n"
				+ "\n"
				+ "megammrsync action:resync host:98.65.45.34:9001\n"
				+ "\n"
				+ "megammrsync action:resync host:98.65.45.34:9001 phrase:\"YOUR 24 WORD SEED PHRASE\" keyuses:2000\n"
				;
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","host","phrase","anyphrase","keys","keyuses"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String action = getParam("action");

		if(action.equals("mydetails")) {
			
			//Get all your public keys and addresses
			Wallet wal = MinimaDB.getDB().getWallet();
			
			ArrayList<ScriptRow> scrows = wal.getAllAddresses();
			
			JSONArray alldets = new JSONArray();
			for(ScriptRow row : scrows) {
				if(!row.getPublicKey().equals("0x00")) {
					
					JSONObject singledet = new JSONObject();
					singledet.put("publickey", row.getPublicKey());
					singledet.put("address", row.getAddress());
					
					alldets.add(singledet);
				}
			}
			
			JSONObject resp = new JSONObject();
			resp.put("details", alldets);
			resp.put("size", alldets.size());
			
			ret.put("response", resp);
		
		}else if(action.equals("resync")) {
		
			//Can only do this if all keys created..
			vault.checkAllKeysCreated();
			
			//Get the host
			String fullhost = getParam("host");
			
			Message connectdata = connect.createConnectMessage(fullhost);
			
			String host = null;
			int port 	= 0;
			
			//Check a valid host
			if(connectdata == null) {
				throw new CommandException("Invalid HOST format for resync : "+fullhost);
			}
				
			host = connectdata.getString("host");
			port = connectdata.getInteger("port");
			
			//Let's test the connection - before proceeding
			IBD ibdtest = archive.sendArchiveReq(host, port, MiniNumber.MINUSONE);
			if(ibdtest == null) {
				throw new CommandException("Could not connect to Archive host! @ "+host+":"+port);
			}
			
			//How many Keys do we need to generate
			int keys = getNumberParam("keys", new MiniNumber(Wallet.NUMBER_GETADDRESS_KEYS)).getAsInt();
			
			//Set the key uses to this..
			int keyuses = getNumberParam("keyuses", new MiniNumber(1000)).getAsInt();
			
			//Is there a Seed phrase..
			String phrase = getParam("phrase","");
			if(!phrase.equals("")) {
			
				//Are we allowing ANY phrase..
				boolean anyphrase = getBooleanParam("anyphrase", false);
				
				//Clean it up..
				String cleanphrase = phrase;
				if(!anyphrase) {
					cleanphrase = BIP39.cleanSeedPhrase(phrase);
				}
				
				//reset ALL the default data
				Main.getInstance().archiveResetReady(true);
				
				//This can take soem time..
				MinimaLogger.log("Resetting all wallet private keys..");
				
				//Convert that into a seed..
				MiniData seed = BIP39.convertStringToSeed(cleanphrase);
				
				//Get the Wallet
				Wallet wallet = MinimaDB.getDB().getWallet();
				
				//Set it..
				wallet.updateSeedRow(cleanphrase, seed.to0xString());
				
				//Now cycle through all the default wallet keys..
				MinimaLogger.log("Creating a total of "+keys+" keys / addresses..");
				for(int i=0;i<keys;i++) {
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
			
			//Create the public data..
			MegaMMRSyncData syncdata = getMyDetails();
			
			//Send this data to them..
			MegaMMRIBD mibd = sendMegaMMRSyncReq(host, port, syncdata);
			if(mibd == null) {
				throw new CommandException("Error getting MegaMMR data from host");
			}
			
			//Tell the MiniDAPPs..
			Main.getInstance().PostNotifyEvent("MDS_RESYNC_START",new JSONObject());
			
			//Are we MEGA MMR
			if(GeneralParams.IS_MEGAMMR) {
				MinimaDB.getDB().getMegaMMR().clear();
			}
			
			//Now process the IBD.. Override the restore setting
			Main.getInstance().getTxPoWProcessor().postProcessIBD(mibd.getIBD(), "0x00", true);
			
			//Small Pause..
			while(true) {
				Thread.sleep(250);
				
				//Check
				if(Main.getInstance().getTxPoWProcessor().isIBDProcessFinished()) {
					break;
				}
			}
			
			//Import all the coin proofs..
			int csize = mibd.getAllCoinProofs().size();
			MinimaLogger.log("Import CoinProofs.. "+mibd.getAllCoinProofs().size());
			for(CoinProof cp : mibd.getAllCoinProofs()) {
				
				//Convert to MiniData..
				MiniData cpdata = MiniData.getMiniDataVersion(cp);
				
				//Coin Import..
				JSONObject coinproofresp = Command.runSingleCommand("coinimport track:true data:"+cpdata.to0xString());
			}
			
			JSONObject resp = new JSONObject();
			resp.put("message", "MegaMMR sync fininshed.. please restart");
			resp.put("coins", csize);
			ret.put("response", resp);
			
			//Don't do the usual shutdown hook
			Main.getInstance().setHasShutDown();
			
			//And NOW shut down..
			Main.getInstance().shutdownFinalProcs();
			
			//Now shutdown and save everything
			MinimaDB.getDB().saveAllDB();
			
			//And NOW shut down..
			Main.getInstance().stopMessageProcessor();
			
			//Tell listener..
			Main.getInstance().NotifyMainListenerOfShutDown();
		}
			
		return ret;
	}
	
	public static MegaMMRSyncData getMyDetails() {
		
		//Get all your public keys and addresses
		Wallet wal = MinimaDB.getDB().getWallet();
		
		ArrayList<ScriptRow> scrows = wal.getAllAddresses();
		
		ArrayList<MiniData> allAddresses 	= new ArrayList<>();
		ArrayList<MiniData> allPublicKeys	= new ArrayList<>();
		
		for(ScriptRow row : scrows) {
			if(!row.getPublicKey().equals("0x00")) {
				allAddresses.add(new MiniData(row.getAddress()));
				allPublicKeys.add(new MiniData(row.getPublicKey()));
			}
		}
		
		MegaMMRSyncData syncdata = new MegaMMRSyncData(allAddresses, allPublicKeys);
		
		return syncdata;
	}
	
	public static ArrayList<CoinProof> getAllCoinProofs(MegaMMRSyncData zSynData){
		
		ArrayList<CoinProof> proofs = new ArrayList<>();
		
		//First get all the coins..
		ArrayList<Coin> allcoins = searchMegaCoins(zSynData.getAllAddresses(), zSynData.getAllPublicKeys());
		
		for(Coin cc : allcoins) {
			
			//Now get all the coin proofs for these coins..
			JSONObject coinproofresp = Command.runSingleCommand("coinexport coinid:"+cc.getCoinID().to0xString());
			
			//Get the proof data..
			JSONObject resp = (JSONObject) coinproofresp.get("response");
			MiniData cpdata = new MiniData(resp.getString("data"));
			
			//Convert to a coin proof..
			CoinProof newcoinproof 	= CoinProof.convertMiniDataVersion(cpdata);
			proofs.add(newcoinproof);
		}
		
		return proofs;
	}
	
	public static ArrayList<Coin> searchMegaCoins(
			ArrayList<MiniData> zAddresses,
			ArrayList<MiniData> zPublicKeys) {

		//The list of Coins
		ArrayList<Coin> coinentry = new ArrayList<>();
		
		//Start node position
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
		
		//A list of spent CoinID..
		HashSet<String> spentcoins = new HashSet<>();
		
		//Are we MEGAMMR
		boolean MEGACHECK = false; 
		
		//Cycle through
		while(tip!=null || MEGACHECK) {
		
			ArrayList<Coin> coins = null;
			
			if(!MEGACHECK) {
				
				//Get the Relevant coins..
				coins = tip.getAllCoins();
				
			}else {
				
				//Need to LOCK DB
				MinimaDB.getDB().readLock(true);
				
				//Get the MEGAMMR COINS..
				coins = new ArrayList<Coin>(MinimaDB.getDB().getMegaMMR().getAllCoins().values());
			}
			
			//Get the details..
			for(Coin coin : coins) {
			
				//Check this coin against all the relevant Addresses and pub keys
				boolean found = false;
				for(MiniData address : zAddresses) {
					if(coin.getAddress().isEqual(address) ||
					   coin.checkForStateVariable(address.to0xString()) ) {
						found = true;
						break;
					}
				}
				
				if(!found) {
					//Check for pub key && adresses in state
					for(MiniData pubkey : zPublicKeys) {
						if(coin.checkForStateVariable(pubkey.to0xString()) ) {
							found = true;
							break;
						}
					}
				}
				
				if(!found) {
					continue;
				}
				
				//Get the CoinID
				String coinid = coin.getCoinID().to0xString();
				
				//is it spent..
				boolean spent = coin.getSpent();
				
				//Add it to our list of spent coins..
				if(spent) {
					spentcoins.add(coinid);
				}else {
					//Check if this has been spent in a previous block..
					if(!spentcoins.contains(coinid)) {
					
						//Make a copy..
						Coin copycoin = coin.deepCopy();
						
						//OK - fresh unspent coin
						coinentry.add(copycoin);
						
						//And no more from now..
						spentcoins.add(coinid);
					}
				}
			}
		
			if(!MEGACHECK) {
				//And move back up the tree
				tip = tip.getParent();
				
				//Are we at the end..
				if(tip == null && GeneralParams.IS_MEGAMMR) {
					MEGACHECK = true;
				}
			}else {
				//Need to LOCK DB
				MinimaDB.getDB().readLock(false);
				
				//we just did a MEGAMMR check.. that's it..
				break;
			}
		}
		
		//Are we only showing simple Coins..
		ArrayList<Coin> finalcoins = coinentry;
			
		return finalcoins;
	}
	
	/**
	 * Send your public data and receive MEGA MMR Sync IBD + CoinProofs
	 */
	public static MegaMMRIBD sendMegaMMRSyncReq(String zHost, int zPort, MegaMMRSyncData zSyncData) {
		
		MegaMMRIBD megaibd= null;
		
		try {
			
			//Create the Network Message
			MiniData msg = NIOManager.createNIOMessage(NIOMessage.MSG_MEGAMMRSYNC_REQ, zSyncData);
			
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
			Main.getInstance().getNIOManager().getTrafficListener().addWriteBytes("sendMegaMMRSyncReq",msg.getLength());
			
			//Load the message
			MiniData resp = MiniData.ReadFromStream(dis);
			
			//Tell the NIO
			Main.getInstance().getNIOManager().getTrafficListener().addReadBytes("sendMegaMMRSyncReq",resp.getLength());
			
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
			
			//Load the MegaMMR IBD
			megaibd = new MegaMMRIBD();
			megaibd.readDataStream(bdis);
			
			bdis.close();
			bais.close();
			
		}catch(Exception exc){
			MinimaLogger.log("MegaMMR Sync connection : "+exc+" @ "+zHost+":"+zPort);
			
			//Null the IBD
			megaibd= null;
		}
	
		
		return megaibd;
	}
	
	@Override
	public Command getFunction() {
		return new megammrsync();
	}
}
