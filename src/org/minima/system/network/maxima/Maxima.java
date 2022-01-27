package org.minima.system.network.maxima;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.security.KeyPair;
import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.UserDB;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.commands.network.connect;
import org.minima.system.network.minima.NIOClient;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.minima.NIOMessage;
import org.minima.system.params.GeneralParams;
import org.minima.utils.BaseConverter;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.encrypt.CryptoPackage;
import org.minima.utils.encrypt.GenerateKey;
import org.minima.utils.encrypt.SignVerify;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;

public class Maxima extends MessageProcessor {

	/**
	 * Maxima Messages
	 */
	public static final String MAXIMA_INIT 			= "MAXIMA_INIT";
	public static final String MAXIMA_RECMESSAGE 	= "MAXIMA_RECMESSAGE";
	public static final String MAXIMA_SENDMESSAGE 	= "MAXIMA_SENDDMESSAGE";
	public static final String MAXIMA_HOSTCONNECT 	= "MAXIMA_HOSTCONNECT";
	
	/**
	 * UserDB data
	 */
	private static final String MAXIMA_PUBKEY 	= "maxima_publickey";
	private static final String MAXIMA_PRIVKEY 	= "maxima_privatekey";
	
	private static final String MAXIMA_CLIENTS 	= "maxima_clients";
	
	private static final String MAXIMA_HOSTSET 	= "maxima_hostset";
	private static final String MAXIMA_HOST 	= "maxima_host";
	
	/**
	 * The Respnse message for a Maxima Message
	 */
	public static final MiniData MAXIMA_RESPONSE = new MiniData("0x080000000101");
	
	/**
	 * RSA Keys
	 */
	MiniData mPublic;
	MiniData mPrivate;
	
	private boolean mInited 	= false;
	public boolean mMaximaLogs 	= true;
	
	/**
	 * Who are we forwarding messages to
	 */
	JSONArray mMaximaClients = new JSONArray();

	/**
	 * Who is Hosting
	 */
	boolean mIsMaxHostSet = false;
	String mHost;
	
	public Maxima() {
		super("MAXIMA");
		
		PostMessage(MAXIMA_INIT);
	}
	
	public boolean isInited() {
		return mInited;
	}
	
	public String getPublicKey() {
		return BaseConverter.encode32(mPublic.getBytes());
	}
	
	public boolean isHostSet() {
		return mIsMaxHostSet;
	}
	
	public String getMaximaHost() {
		if(mIsMaxHostSet) {
			return mHost;
		}
		return GeneralParams.MINIMA_HOST+":"+GeneralParams.MINIMA_PORT;
	}

	public void setMaximaHost(String zHost) {
		if(zHost.equals("")) {
			mIsMaxHostSet = false;
			
			//Disconnect if need be
			NIOClient nioc = Main.getInstance().getNIOManager().checkConnected(mHost, false);
			if(nioc != null) {
				Main.getInstance().getNIOManager().disconnect(nioc.getUID());
			}
			
			//Reset
			mHost = GeneralParams.MINIMA_HOST+":"+GeneralParams.MINIMA_PORT;
			
		}else {
			mIsMaxHostSet 	= true;
			mHost 			= zHost;
		}
		
		MinimaDB.getDB().getUserDB().setBoolean(MAXIMA_HOSTSET, mIsMaxHostSet);
		MinimaDB.getDB().getUserDB().setString(MAXIMA_HOST, mHost);
		MinimaDB.getDB().saveUserDB();
	}
	
	public String getFullIdentity() {
		return getPublicKey()+"@"+getMaximaHost();
	}
	
	public MaximaMessage createMaximaMessage(String zFullTo, String zApplication, MiniData zData) {
		MaximaMessage maxima 	= new MaximaMessage();
		maxima.mFrom 			= new MiniString(getFullIdentity());
		maxima.mTo 				= new MiniString(zFullTo);
		maxima.mApplication 	= new MiniString(zApplication);
		maxima.mData 			= zData;
		
		return maxima;
	}
	
	public void addValidMaximaClient(String zClient) {
		
		//Remove the @ section
		String client = zClient;
		if(zClient.contains("@")) {
			int index 	= zClient.indexOf("@");
			client 		= zClient.substring(0,index);
		}
		
		//Add
		mMaximaClients.add(client);
		
		MinimaDB.getDB().getUserDB().setJSONArray(MAXIMA_CLIENTS, mMaximaClients);
		MinimaDB.getDB().saveUserDB();
	}
	
	public void removeValidMaximaClient(String zClient) {
		mMaximaClients.remove(zClient);
		
		MinimaDB.getDB().getUserDB().setJSONArray(MAXIMA_CLIENTS, mMaximaClients);
		MinimaDB.getDB().saveUserDB();
	}
	
	public JSONArray getMaximaClients() {
		
		JSONArray ret = new JSONArray();
		
		//Get all the connected clientgs..
		ArrayList<NIOClient> allclients = Main.getInstance().getNIOManager().getNIOServer().getAllNIOClients();
		for(NIOClient nioc : allclients) {
			if(nioc.isMaximaClient()) {
				ret.add(nioc.getMaximaIdent());
			}
		}
		
		return ret;
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		if(zMessage.getMessageType().equals(MAXIMA_INIT)) {
			
			//Get the UserDB
			UserDB udb = MinimaDB.getDB().getUserDB();
			
			//Do we have an account already..
			if(!udb.exists(MAXIMA_PUBKEY)) {
				createMaximaKeys();
			
			}else {
				//Get the data..
				mPublic  = udb.getData(MAXIMA_PUBKEY, MiniData.ZERO_TXPOWID);
				mPrivate = udb.getData(MAXIMA_PRIVKEY, MiniData.ZERO_TXPOWID);
			}
			
			//Is the Host HARD set
			mIsMaxHostSet = udb.getBoolean(MAXIMA_HOSTSET, false); 
			
			//The Host
			mHost = udb.getString(MAXIMA_HOST, GeneralParams.MINIMA_HOST+":"+GeneralParams.MINIMA_PORT);
			
			//Get the valid client list
			mMaximaClients = udb.getJSONArray(MAXIMA_CLIENTS);
			
			mInited = true;
			
			//Save the DB
			MinimaDB.getDB().saveUserDB();
		
			//Now try and connect to pout host..
			PostTimerMessage(new TimerMessage(10000, MAXIMA_HOSTCONNECT));
			
		}else if(zMessage.getMessageType().equals(MAXIMA_HOSTCONNECT)) {
			
			//Connect to the Host
			if(mIsMaxHostSet) {
				
				//Are we already connected
				if(Main.getInstance().getNIOManager().checkConnected(mHost, false) == null) {
					
					MinimaLogger.log("Connecting to our Maxima Host "+mHost);
					
					//Connecting to Maxima Host
					Message connectmsg = connect.createConnectMessage(mHost);
					Main.getInstance().getNIOManager().PostMessage(connectmsg);
				}
			}
			
			//Check every minute..
			PostTimerMessage(new TimerMessage(60000, MAXIMA_HOSTCONNECT));
			
		}else if(zMessage.getMessageType().equals(MAXIMA_SENDMESSAGE)) {
			
			//Message details
			String publickey	= zMessage.getString("publickey");
			MiniData topubk 	= new MiniData(publickey);
			
			String tohost 		= zMessage.getString("tohost");
			int toport			= zMessage.getInteger("toport");
			
			//Get the Maxima Message
			MaximaMessage maxima 	= (MaximaMessage) zMessage.getObject("maxima");
			
			//Next Sign the Message and create the MaximaInternal message
			MiniData maxdata		= MiniData.getMiniDataVersion(maxima);
			byte[] sigBytes  		= SignVerify.sign(mPrivate.getBytes(), maxdata.getBytes());
			
			MaximaInternal msign 	= new MaximaInternal();
			msign.mFrom				= mPublic;
			msign.mData				= maxdata;
			msign.mSignature		= new MiniData(sigBytes);
			
			//And finally create the encrypted MaximaPackage
			MiniData maxpkg			= MiniData.getMiniDataVersion(msign);
			
			//Now Encrypt the Whole Thing..
			CryptoPackage cp = new CryptoPackage();
			cp.encrypt(maxpkg.getBytes(), topubk.getBytes());
			
			//Now Construct a MaximaPackage
			MaximaPackage mp = new MaximaPackage( topubk , cp.getCompleteEncryptedData());
			
			//Create the Network Message
			MiniData maxmsg = NIOManager.createNIOMessage(NIOMessage.MSG_MAXIMA, mp);
			
			//And send it..
			sendMaximaMessage(tohost, toport, maxmsg);
			
		}else if(zMessage.getMessageType().equals(MAXIMA_RECMESSAGE)) {
			
			//received a Message!
			MaximaPackage mpkg = (MaximaPackage) zMessage.getObject("maxpackage");
			
			//Is it for us?
			if(!mpkg.mTo.isEqual(mPublic)) {
				
				//The pubkey Mx version
				String pubk = BaseConverter.encode32(mpkg.mTo.getBytes());
				
				//Check if it one of our allowed clients! - For now allow all..
//				if(mMaximaClients.contains(pubk)) {
					
					//Forward it to them!
					NIOClient client =  Main.getInstance().getNIOManager().getMaximaUID(pubk);
					
					//Do we have it
					if(client != null) {
						if(mMaximaLogs) {
							MinimaLogger.log("MAXIMA message forwarded to client : "+pubk);
						}
						
						//Send to the client we are connected to..
						NIOManager.sendNetworkMessage(client.getUID(), NIOMessage.MSG_MAXIMA, mpkg);
						
					}else{
						MinimaLogger.log("MAXIMA message received for Client we are not connected to : "+pubk);
					}
					
//				}else {
//					MinimaLogger.log("MAXIMA message received to unknown PublicKey : "+mpkg.mTo.to0xString());
//				}
				
				return;
			}
			
			//Decrypt the data
			CryptoPackage cp = new CryptoPackage();
			cp.ConvertMiniDataVersion(mpkg.mData);
			byte[] data = cp.decrypt(mPrivate.getBytes());
			
			//Now get the Decrypted data..
			MaximaInternal mm = MaximaInternal.ConvertMiniDataVersion(new MiniData(data));
			
			//Check the Signature..
			boolean valid = SignVerify.verify(mm.mFrom.getBytes(), mm.mData.getBytes(), mm.mSignature.getBytes());
			if(!valid) {
				MinimaLogger.log("MAXIMA Invalid Signature on message : "+mpkg.mTo.to0xString());
				return;
			}
			
			//Now convert the data to a Maxima Message
			MaximaMessage maxmsg 	= MaximaMessage.ConvertMiniDataVersion(mm.mData);
			
			//Check the message is from the person who signed it!
			String from 	= maxmsg.mFrom.toString();
			int index 		= from.indexOf("@");
			String pubkey 	= from.substring(0,index);
			MiniData frompubk = new MiniData(pubkey);
			if(!frompubk.isEqual(mm.mFrom)) {
				MinimaLogger.log("MAXIMA Message From field signed by incorrect pubkey  from:"
											+frompubk.to0xString()+" signed:"+mm.mFrom.to0xString());
				return;
			}
			
			//Hash the complete message..
			MiniData hash = Crypto.getInstance().hashObject(mm.mData);
			
			//Now create the final JSON..
			JSONObject maxjson = maxmsg.toJSON();
			maxjson.put("msgid", hash.to0xString());
			
			//Do we log
			if(mMaximaLogs) {
				MinimaLogger.log("MAXIMA : "+maxjson.toString());
			}
			
			//Notify The Listeners
			Main.getInstance().PostNotifyEvent("MAXIMA",maxjson);
		}
	}

	private void sendMaximaMessage(String zHost, int zPort, MiniData zMaxMessage) {
		
		Runnable sender = new Runnable() {
			
			@Override
			public void run() {
				try {
					//Open the socket..
					Socket sock 			= new Socket(zHost, zPort);
					sock.setSoTimeout(10000);
					
					//Create the streams..
					OutputStream out 		= sock.getOutputStream();
					DataOutputStream dos 	= new DataOutputStream(out);
					
					InputStream in			= sock.getInputStream();
					DataInputStream dis 	= new DataInputStream(in);
					
					//Write the data
					zMaxMessage.writeDataStream(dos);
					dos.flush();
					
					//Now get a response.. should be ONE_ID.. give it 1 second max.. ( might get a block..)
					boolean valid = false;
					long maxtime = System.currentTimeMillis() + 1000;
					while(System.currentTimeMillis() < maxtime) {
						MiniData resp = MiniData.ReadFromStream(dis);
						if(resp.isEqual(MAXIMA_RESPONSE)) {
							valid = true;
							break;
						}
					}
					
					if(!valid) {
						MinimaLogger.log("Warning : Maxima message incorrect reply");
					}
					
					//Close the streams..
					dis.close();
					in.close();
					dos.close();
					out.close();
				
				}catch(Exception exc){
					MinimaLogger.log("Error sending Maxima message : "+exc.toString());
				}
			}
		};
		
		Thread tt = new Thread(sender);
		tt.setDaemon(true);
		tt.start();
	}
	
	public void createMaximaKeys() throws Exception {
		
		//Get the UserDB
		UserDB udb = MinimaDB.getDB().getUserDB();
		
		//Create a new new maxima ident..
		KeyPair generateKeyPair = GenerateKey.generateKeyPair();
		
		byte[] publicKey 		= generateKeyPair.getPublic().getEncoded();
		mPublic 				= new MiniData(publicKey);
		
		byte[] privateKey	 	= generateKeyPair.getPrivate().getEncoded();
		mPrivate 				= new MiniData(privateKey);
		
		//Put in the DB..
		udb.setData(MAXIMA_PUBKEY, mPublic);
		udb.setData(MAXIMA_PRIVKEY, mPrivate);
	}
}
