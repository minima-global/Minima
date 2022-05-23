package org.minima.system.network.maxima;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.security.KeyPair;
import java.util.ArrayList;
import java.util.Random;

import org.minima.database.MinimaDB;
import org.minima.database.maxima.MaximaContact;
import org.minima.database.maxima.MaximaDB;
import org.minima.database.maxima.MaximaHost;
import org.minima.database.userprefs.UserDB;
import org.minima.objects.Address;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.network.maxima.message.MaxTxPoW;
import org.minima.system.network.maxima.message.MaximaInternal;
import org.minima.system.network.maxima.message.MaximaMessage;
import org.minima.system.network.maxima.message.MaximaPackage;
import org.minima.system.network.minima.NIOClient;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.minima.NIOMessage;
import org.minima.system.params.GeneralParams;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.encrypt.CryptoPackage;
import org.minima.utils.encrypt.GenerateKey;
import org.minima.utils.encrypt.SignVerify;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;
import org.minima.utils.messages.TimerMessage;

public class MaximaManager extends MessageProcessor {

	/**
	 * Maxima Messages
	 */
	public static final String MAXIMA_INIT 			= "MAXIMA_INIT";
	
	/**
	 * Network Messages
	 */
	public static final String MAXIMA_CONNECTED 	= "MAXIMA_CONNECTED";
	public static final String MAXIMA_DISCONNECTED 	= "MAXIMA_DISCONNECTED";
	
	/**
	 * Checker loop function - every 10 mins
	 */
	public static final String MAXIMA_LOOP 			= "MAXIMA_LOOP";
	long MAXIMA_LOOP_DELAY = 1000 * 60 * 10;
	
	/**
	 * Messages
	 */
	public static final String MAXIMA_CTRLMESSAGE 	= "MAXIMA_CTRLMESSAGE";
	public static final String MAXIMA_RECMESSAGE 	= "MAXIMA_RECMESSAGE";
	public static final String MAXIMA_SENDMESSAGE 	= "MAXIMA_SENDDMESSAGE";
	public static final String MAXIMA_REFRESH 		= "MAXIMA_REFRESH";
	
	/**
	 * UserDB data
	 */
	private static final String MAXIMA_PUBKEY 	= "maxima_publickey";
	private static final String MAXIMA_PRIVKEY 	= "maxima_privatekey";
	
	/**
	 * The Response message for a Maxima Message
	 */
	public static final MiniData MAXIMA_FAIL 		= new MiniData("0x00");
	public static final MiniData MAXIMA_OK 			= new MiniData("0x01");
	public static final MiniData MAXIMA_UNKNOWN 	= new MiniData("0x02");
	public static final MiniData MAXIMA_TOOBIG 		= new MiniData("0x03");
	public static final MiniData MAXIMA_WRONGHASH 	= new MiniData("0x04");
	
	public static final MiniData MAXIMA_RESPONSE_FAIL 		= new MiniData("0x080000000100");
	public static final MiniData MAXIMA_RESPONSE_OK 		= new MiniData("0x080000000101");
	public static final MiniData MAXIMA_RESPONSE_UNKNOWN 	= new MiniData("0x080000000102");
	public static final MiniData MAXIMA_RESPONSE_TOOBIG 	= new MiniData("0x080000000103");
	public static final MiniData MAXIMA_RESPONSE_WRONGHASH 	= new MiniData("0x080000000104");
	
	/**
	 * RSA Keys
	 */
	MiniData mPublic;
	MiniData mPrivate;
	
	String mMaximaAddress;
	
	private boolean mInited 	= false;
	public boolean mMaximaLogs 	= false;

	/**
	 * The Contacts Manager
	 */
	MaximaContactManager mMaxContacts;
	
	public MaximaManager() {
		super("MAXIMA");
		
		mMaxContacts = new MaximaContactManager(this);
		
		PostMessage(MAXIMA_INIT);
	}
	
	public void shutdown() {
		mMaxContacts.stopMessageProcessor();
		stopMessageProcessor();
	}
	
	public boolean isInited() {
		return mInited;
	}
	
	public String getMaximaIdentity() {
		return mMaximaAddress;
	}
	
	public MiniData getPublicKey() {
		return mPublic;
	}
	
	public MiniData getPrivateKey() {
		return mPrivate;
	}
	
	public MaximaContactManager getContactsManager() {
		return mMaxContacts;
	}
	
	public String getLocalMaximaAddress() {
		return getMaximaIdentity()+"@"+GeneralParams.MINIMA_HOST+":"+GeneralParams.MINIMA_PORT;
	}
	
	public String getRandomMaximaAddress() {
		//Get all the current hosts
		ArrayList<MaximaHost> hosts = MinimaDB.getDB().getMaximaDB().getAllHosts();
		
		//Only the connected ones..
		ArrayList<MaximaHost> connctedhosts = new ArrayList<>();
		for(MaximaHost host : hosts) {
			if(host.isConnected()) {
				connctedhosts.add(host);
			}
		}
		
		//Are there any..
		if(connctedhosts.size() == 0) {
			return getLocalMaximaAddress();
		}
		
		return connctedhosts.get(new Random().nextInt(connctedhosts.size())).getMaximaAddress();
	}

	
	public MaximaMessage createMaximaMessage(String zTo, String zApplication, MiniData zData) {
		MaximaMessage maxima 	= new MaximaMessage();
		
		maxima.mFrom 			= getPublicKey();
		maxima.mTo 				= new MiniData(zTo);
		maxima.mApplication 	= new MiniString(zApplication);
		maxima.mData 			= zData;
		
		return maxima;
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		//Get the MaximaDB
		MaximaDB maxdb = MinimaDB.getDB().getMaximaDB();
		
		if(zMessage.getMessageType().equals(MAXIMA_INIT)) {
			
			//Get the UserDB
			UserDB udb = MinimaDB.getDB().getUserDB();
			
			//Do we have an account already..
			if(!udb.exists(MAXIMA_PUBKEY)) {
				
				MinimaLogger.log("Creating Maxima Keys..");
				createMaximaKeys();
			
			}else {
				mPublic  = udb.getData(MAXIMA_PUBKEY, MiniData.ZERO_TXPOWID);
				mPrivate = udb.getData(MAXIMA_PRIVKEY, MiniData.ZERO_TXPOWID);
			
				//Convert to a Maxima Address
				mMaximaAddress = Address.makeMinimaAddress(mPublic);
			}
			
			//We are inited
			mInited = true;
			
			//Save the DB
			MinimaDB.getDB().saveUserDB();
			
			//Post a LOOP message that updates all my contacts just in case..
			PostTimerMessage(new TimerMessage(1000 * 60 * 2, MAXIMA_LOOP));
		
		}else if(zMessage.getMessageType().equals(MAXIMA_LOOP)) {
			
			//Resend all your details to your contacts
			PostMessage(MAXIMA_REFRESH);
			
			//Delete really old MaxHosts - not seen for 7 days
			maxdb.deleteOldHosts();
			
			//Post a LOOP message that updates all my contacts just in case..
			PostTimerMessage(new TimerMessage(MAXIMA_LOOP_DELAY, MAXIMA_LOOP));
		
		}else if(zMessage.getMessageType().equals(MAXIMA_REFRESH)) {
			
			//Get all your contacts
			ArrayList<MaximaContact> allcontacts = maxdb.getAllContacts();
			for(MaximaContact contact : allcontacts) {
				
				//Update them with a new address..
				String publickey = contact.getPublicKey();
				String address	 = contact.getCurrentAddress();
				
				//Now send a message updating them
				Message update = new Message(MaximaContactManager.MAXCONTACTS_UPDATEINFO);
				update.addString("publickey", publickey);
				update.addString("address", address);
				
				getContactsManager().PostMessage(update);
			}
			
		}else if(zMessage.getMessageType().equals(MAXIMA_CONNECTED)) {
		
			//Get the client
			NIOClient nioc = (NIOClient) zMessage.getObject("nioclient");
			
			//is it an outgoing.. ONLY outgoing can be used for MAXIMA
			if(nioc.isOutgoing()) {
				
				//OK.. Do we have this node in our list..
				MaximaHost mxhost = maxdb.loadHost(nioc.getFullAddress());
				
				//Do we have something..
				if(mxhost == null) {
					MinimaLogger.log("MAXIMA NEW connection : "+nioc.getFullAddress());
					
					//Create a new Host
					mxhost = new MaximaHost(nioc.getFullAddress());
					mxhost.createKeys();
					
					//Now insert this into the DB
					maxdb.newHost(mxhost);
				}else{
					MinimaLogger.log("MAXIMA EXISTING connection : "+nioc.getFullAddress());
					
					//Update our details..
					mxhost.setConnected(1);
					maxdb.updateHost(mxhost);
				}
				
				//So we know the details.. Post them to him.. so he knows who we are..
				MaximaCTRLMessage maxmess = new MaximaCTRLMessage(MaximaCTRLMessage.MAXIMACTRL_TYPE_ID);
				maxmess.setData(mxhost.getPublicKey());
				NIOManager.sendNetworkMessage(nioc.getUID(), NIOMessage.MSG_MAXIMA_CTRL, maxmess);
			}
			
		}else if(zMessage.getMessageType().equals(MAXIMA_DISCONNECTED)) {
			
			//Get the client
			NIOClient nioc = (NIOClient) zMessage.getObject("nioclient");
			
			//Is there a reconnect
			boolean reconnect = zMessage.getBoolean("reconnect");
			
			//OK.. Do we have this node in our list..
			MaximaHost mxhost = maxdb.loadHost(nioc.getFullAddress());
			if(mxhost != null) {
				mxhost.setConnected(0);
				maxdb.updateHost(mxhost);
			}
			
			//is it an outgoing.. ONLY outgoing can be used for MAXIMA
			if(nioc.isOutgoing()) {
				
				MinimaLogger.log("MAXIMA outgoing disconnection : "+nioc.getFullAddress()+" "+reconnect);
				
				//Do we need to update Users who contact us through them..
				if(!reconnect) {
					
					//Ok - lets reset contacts that use this host
					String host = nioc.getFullAddress();
					
					//Which contacts used that host - reassign them
					ArrayList<MaximaContact> allcontacts = maxdb.getAllContacts();
					for(MaximaContact contact : allcontacts) {
						
						//Only reset those that use this address
						if(contact.getMyAddress().contains(host)) {
						
							//Update them with a new address..
							String publickey = contact.getPublicKey();
							String address	 = contact.getCurrentAddress();
							
							//Now send a message updating them
							Message update = new Message(MaximaContactManager.MAXCONTACTS_UPDATEINFO);
							update.addString("publickey", publickey);
							update.addString("address", address);
							
							getContactsManager().PostMessage(update);
						}
					}
					
					//Delete from Hosts DB
					maxdb.deleteHost(nioc.getFullAddress());
				}
			}
		
		}else if(zMessage.getMessageType().equals(MAXIMA_CTRLMESSAGE)) {
			
			//Received a control message from a client
			MaximaCTRLMessage msg = (MaximaCTRLMessage) zMessage.getObject("maximactrl");
			
			//Get the NIOClient
			NIOClient nioc = (NIOClient) zMessage.getObject("nioclient");
			
			if(msg.getType().isEqual(MaximaCTRLMessage.MAXIMACTRL_TYPE_ID)) {
				
				//Set the ID for this Connection
				MiniData pubkey = msg.getData();

				//And Set..
				nioc.setMaximaIdent(pubkey.to0xString());
			}
			
		}else if(zMessage.getMessageType().equals(MAXIMA_SENDMESSAGE)) {
			
			//Create the Network Message
			MiniData maxmsg = constructMaximaData(zMessage);
			
			//And send it.. in a separate thread
			sendMaximaMessage(zMessage.getString("tohost"), zMessage.getInteger("toport"), maxmsg);
			
		}else if(zMessage.getMessageType().equals(MAXIMA_RECMESSAGE)) {
			
			//Get the MaxTxPoW
			MaxTxPoW mxtxpow 	= (MaxTxPoW) zMessage.getObject("maxtxpow");
			
			//received a Message!
			MaximaPackage mpkg 	= mxtxpow.getMaximaPackage();
			
			//Get the NIOClient
			NIOClient nioc = (NIOClient) zMessage.getObject("nioclient");
			
			//Private key tpo decode the message
			MiniData privatekey = null;
			
			//The pubkey it is encrypted with
			String tomaxima = mpkg.mTo.to0xString();
			
			//Is it straight to us..
			if(mpkg.mTo.equals(mPublic)) {
				//It's directly sent to us..
				privatekey = mPrivate;
			}
			
			//Is it for us - check the Maxhosts..
			if(privatekey == null) {
				//Get the maxima Host
				MaximaHost host = maxdb.loadHostFromPublicKey(tomaxima);
				if(host != null) {
					privatekey = host.getPrivateKey();
				}
			}
			
			//If we don't find it..
			if(privatekey == null) {
				
				//Forward it to them
				NIOClient client =  Main.getInstance().getNIOManager().getMaximaUID(tomaxima);
				
				//Do we have it
				if(client != null) {
					if(mMaximaLogs) {
						MinimaLogger.log("MAXIMA message forwarded to client : "+tomaxima);
					}
					
					//Send to the client we are connected to..
					NIOManager.sendNetworkMessage(client.getUID(), NIOMessage.MSG_MAXIMA_TXPOW, mxtxpow);
					
					//Notify that Client that we received the message.. this makes external client disconnect ( internal just a ping )
					maximaMessageStatus(nioc,MAXIMA_OK);
					
				}else{
					MinimaLogger.log("MAXIMA message received for Client we are not connected to : "+tomaxima);
				
					//Notify that Client of the fail.. this makes external client disconnect ( internal just a ping )
					maximaMessageStatus(nioc,MAXIMA_UNKNOWN);
				}
				
				return;
			}
			
			//Decrypt the data
			CryptoPackage cp = new CryptoPackage();
			cp.ConvertMiniDataVersion(mpkg.mData);
			byte[] data = cp.decrypt(privatekey.getBytes());
			
			//Now get the Decrypted data..
			MaximaInternal mm = MaximaInternal.ConvertMiniDataVersion(new MiniData(data));
			
			//Check the Signature..
			boolean valid = SignVerify.verify(mm.mFrom.getBytes(), mm.mData.getBytes(), mm.mSignature.getBytes());
			if(!valid) {
				MinimaLogger.log("MAXIMA Invalid Signature on message : "+mpkg.mTo.to0xString());
				
				//Notify that Client of the fail.. this makes external client disconnect ( internal just a ping )
				maximaMessageStatus(nioc,MAXIMA_FAIL);
				
				return;
			}
			
			//Now convert the data to a Maxima Message
			MaximaMessage maxmsg 	= MaximaMessage.ConvertMiniDataVersion(mm.mData);
			
			//Check the message is from the person who signed it!
			if(!maxmsg.mFrom.isEqual(mm.mFrom)) {
				MinimaLogger.log("MAXIMA Message From field signed by incorrect pubkey  from:"+maxmsg.mFrom.to0xString()+" signed:"+mm.mFrom.to0xString());
				
				//Notify that Client of the fail.. this makes external client disconnect ( internal just a ping )
				maximaMessageStatus(nioc,MAXIMA_FAIL);
				
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
			
			//Notify that Client that we received the message.. this makes external client disconnect ( internal just a ping )
			maximaMessageStatus(nioc,MAXIMA_OK);
			
			//Is it a special contact message
			String application = (String) maxjson.get("application");
			if(application.equals(MaximaContactManager.CONTACT_APPLICATION)) {
				
				//Process this internally..
				Message contactmessage = new Message(MaximaContactManager.MAXCONTACTS_RECMESSAGE);
				contactmessage.addObject("maxmessage", maxjson);
				getContactsManager().PostMessage(contactmessage);
				
				//Update DB - this host is being used..
				MaximaHost host = maxdb.loadHost(nioc.getFullAddress());
				if(host != null) {
					host.updateLastSeen();
					maxdb.updateHost(host);
				}
				
			}else {
				//Notify The Listeners
				Main.getInstance().PostNotifyEvent("MAXIMA",maxjson);
			}
		}
	}
	
	private void maximaMessageStatus(NIOClient zClient, MiniData zStatus) throws IOException {
		//Send this Maxima response
		NIOManager.sendNetworkMessage(zClient.getUID(), NIOMessage.MSG_PING, zStatus);
	}
	
	public static MiniData constructMaximaData(Message zMessage) throws Exception {
		//Message details
		String publickey	= zMessage.getString("publickey");
		MiniData topubk 	= new MiniData(publickey);
		
		String tohost 		= zMessage.getString("tohost");
		int toport			= zMessage.getInteger("toport");
		
		//Get the Maxima Message
		MaximaMessage maxima 	= (MaximaMessage) zMessage.getObject("maxima");
		
		//Next Sign the Message and create the MaximaInternal message
		MiniData maxdata		= MiniData.getMiniDataVersion(maxima);
		MiniData privatekey		= (MiniData) zMessage.getObject("myprivatekey");
		byte[] sigBytes  		= SignVerify.sign(privatekey.getBytes(), maxdata.getBytes());
		
		MaximaInternal msign 	= new MaximaInternal();
		msign.mFrom				= (MiniData) zMessage.getObject("mypublickey");
		msign.mData				= maxdata;
		msign.mSignature		= new MiniData(sigBytes);
		
		//And finally create the encrypted MaximaPackage
		MiniData maxpkg			= MiniData.getMiniDataVersion(msign);
		
		//Now Encrypt the Whole Thing..
		CryptoPackage cp = new CryptoPackage();
		cp.encrypt(maxpkg.getBytes(), topubk.getBytes());
		
		//Now Construct a MaximaPackage
		MaximaPackage mp = new MaximaPackage( topubk , cp.getCompleteEncryptedData());
		
		//Now create a MaxTxPow
		MaxTxPoW mxtxpow = MaxTxPoW.createMaxTxPoW(mp);
		
		//Create the Network Message
		return NIOManager.createNIOMessage(NIOMessage.MSG_MAXIMA_TXPOW, mxtxpow);
	}
	
	public static MiniData sendMaxPacket(String zHost, int zPort, MiniData zMaxMessage) throws IOException {
		
		//Open the socket..
		Socket sock = new Socket(zHost, zPort);
		sock.setSoTimeout(10000);
		
		//Create the streams..
		OutputStream out 		= sock.getOutputStream();
		DataOutputStream dos 	= new DataOutputStream(out);
		
		InputStream in			= sock.getInputStream();
		DataInputStream dis 	= new DataInputStream(in);
		
		//Write the data
		zMaxMessage.writeDataStream(dos);
		dos.flush();
		
		//Now get a response.. should be ONE_ID.. give it 10 second max.. ( might get a block..)
		MiniData valid = MAXIMA_RESPONSE_FAIL;
		long maxtime = System.currentTimeMillis() + 10000;
		while(System.currentTimeMillis() < maxtime) {
			MiniData resp = MiniData.ReadFromStream(dis);
			if(resp.isEqual(MAXIMA_RESPONSE_OK)) {
				valid = resp;
				break;
			}else if(resp.isEqual(MAXIMA_RESPONSE_FAIL)) {
				valid = resp;
				break;
			}else if(resp.isEqual(MAXIMA_RESPONSE_TOOBIG)) {
				valid = resp;
				break;
			}else if(resp.isEqual(MAXIMA_RESPONSE_UNKNOWN)) {
				valid = resp;
				break;
			}else if(resp.isEqual(MAXIMA_RESPONSE_WRONGHASH)) {
				valid = resp;
				break;
			}
		}
		
		//Close the streams..
		dis.close();
		in.close();
		dos.close();
		out.close();
		
		return valid;
	}
	
	private void sendMaximaMessage(String zHost, int zPort, MiniData zMaxMessage) {
		
		Runnable sender = new Runnable() {
			
			@Override
			public void run() {
				try {
					
					//Send the message
					MiniData validresp = sendMaxPacket(zHost,zPort,zMaxMessage);
					
					if(!validresp.isEqual(MAXIMA_RESPONSE_OK)) {
						
						if(validresp.isEqual(MaximaManager.MAXIMA_RESPONSE_TOOBIG)) {
							MinimaLogger.log("Warning : Maxima message too big not delivered to.. "+zHost+"@"+zPort);
						}else if(validresp.isEqual(MaximaManager.MAXIMA_RESPONSE_UNKNOWN)) {
							MinimaLogger.log("Warning : Maxima message Unkonw Address not delivered to.. "+zHost+"@"+zPort);
						}else if(validresp.isEqual(MaximaManager.MAXIMA_RESPONSE_WRONGHASH)) {
							MinimaLogger.log("Warning : Maxima message TxPoW Hash wrong not delivered to.. "+zHost+"@"+zPort);
						}else {
							MinimaLogger.log("Warning : Maxima message not delivered to.. "+zHost+"@"+zPort);
						}
						
					}
					
				}catch(Exception exc){
					MinimaLogger.log("Error sending Maxima message : "+exc.toString());
				}
			}
		};
		
		Thread tt = new Thread(sender);
		tt.setDaemon(true);
		tt.start();
	}
	
	private void createMaximaKeys() throws Exception {
		
		//Get the UserDB
		UserDB udb = MinimaDB.getDB().getUserDB();
		
		//Create a new new maxima ident..
		KeyPair generateKeyPair = GenerateKey.generateKeyPair();
		
		byte[] publicKey 		= generateKeyPair.getPublic().getEncoded();
		mPublic 				= new MiniData(publicKey);
		
		byte[] privateKey	 	= generateKeyPair.getPrivate().getEncoded();
		mPrivate 				= new MiniData(privateKey);
	
		//Convert to a Maxima Address
		mMaximaAddress = Address.makeMinimaAddress(mPublic);
		
		//Put in the DB..
		udb.setData(MAXIMA_PUBKEY, mPublic);
		udb.setData(MAXIMA_PRIVKEY, mPrivate);
	}
}
