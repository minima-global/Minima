package org.minima.system.commands.backup.mmrsync;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.math.BigInteger;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.wallet.ScriptRow;
import org.minima.database.wallet.SeedRow;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.IBD;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.objects.keys.TreeKey;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.commands.network.connect;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.minima.NIOMessage;
import org.minima.system.params.GeneralParams;
import org.minima.utils.BIP39;
import org.minima.utils.Crypto;
import org.minima.utils.MiniFile;
import org.minima.utils.MinimaLogger;
import org.minima.utils.encrypt.PasswordCrypto;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.ssl.SSLManager;

public class megasync extends Command {

	public megasync() {
		super("megasync","Restore / Sync from a MegaMMR node");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","address","data","host"}));
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
			
			ret.put("response", resp);
		
		}else if(action.equals("findcoins")) {
			
			MiniData address = getDataParam("address");
			
			ArrayList<MiniData> alladdr = new ArrayList<>();
			alladdr.add(address);
			
			ArrayList<Coin> allcoins = searchMegaCoins(alladdr, new ArrayList<>());
			
			for(Coin cc : allcoins) {
				
				JSONObject coinproofresp = Command.runSingleCommand("coinexport coinid:"+cc.getCoinID().to0xString());
				
				MinimaLogger.log(coinproofresp.toJSONString());
			}
		
		}else if(action.equals("resync")) {
			
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
			
			//Create the public data..
			//..
			
			MegaMMRSyncData syncdata = new MegaMMRSyncData(new ArrayList<>(), new ArrayList<>());
			
			//Send this data to them..
			 MegaMMRIBD mibd = sendMegaMMRSyncReq(host, port, syncdata);
			
			 if(mibd == null) {
				 throw new CommandException("Error connecting to host");
			 }
			 
			 //Output som data..
			 MinimaLogger.log("Sync Received valid:"+mibd.getIBD().checkValidData());

		}
			
		return ret;
	}
	
	public static synchronized ArrayList<Coin> searchMegaCoins(
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
		return new megasync();
	}
}
