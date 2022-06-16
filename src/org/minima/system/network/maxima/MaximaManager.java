package org.minima.system.network.maxima;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.security.KeyPair;
import java.util.ArrayList;
import java.util.Random;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.minima.database.MinimaDB;
import org.minima.database.maxima.MaximaContact;
import org.minima.database.maxima.MaximaDB;
import org.minima.database.maxima.MaximaHost;
import org.minima.database.userprefs.UserDB;
import org.minima.objects.Address;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.commands.maxima.maxima;
import org.minima.system.network.maxima.message.MaxTxPoW;
import org.minima.system.network.maxima.message.MaximaInternal;
import org.minima.system.network.maxima.message.MaximaMessage;
import org.minima.system.network.maxima.message.MaximaPackage;
import org.minima.system.network.maxima.mls.MLSPacketGETReq;
import org.minima.system.network.maxima.mls.MLSPacketGETResp;
import org.minima.system.network.maxima.mls.MLSPacketSET;
import org.minima.system.network.maxima.mls.MLSService;
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
	 * Checker loop function - every 20 mins
	 */
	public static final String MAXIMA_LOOP 			= "MAXIMA_LOOP";
	long MAXIMA_LOOP_DELAY = 1000 * 60 * 20;
	
	/**
	 * Messages
	 */
	public static final String MAXIMA_CTRLMESSAGE 	= "MAXIMA_CTRLMESSAGE";
	public static final String MAXIMA_RECMESSAGE 	= "MAXIMA_RECMESSAGE";
	public static final String MAXIMA_SENDMESSAGE 	= "MAXIMA_SENDDMESSAGE";
	public static final String MAXIMA_REFRESH 		= "MAXIMA_REFRESH";
	public static final String MAXIMA_MLSGET_RESP 		= "MAXIMA_GETREQ";
	
	/**
	 * Send a message to CHECK Maxima is working on connect
	 */
	public static final String MAXIMA_SENDCHKCONNECT 	= "MAXIMA_SENDCHKCONNECT";
	public static final String MAXIMA_CHKCONECT_APP 	= "**maxima_check_connect**";
	
	/**
	 * Maxima Location Service Message
	 */
	public static final String MAXIMA_MLS_SETAPP 	= "**maxima_mls_set**";
	public static final String MAXIMA_MLS_GETAPP 	= "**maxima_mls_get**";
	public static String MLS_RANDOM_UID 			= "";
	
	/**
	 * MLS Checker loop function - every 1 hr
	 */
	public static final String MAXIMA_LOOP_MLS 		= "MAXIMA_LOOP_MLS";
	long MAXIMA_LOOP_MLS_DELAY = 1000 * 60 * 60;
	
	/**
	 * UserDB data
	 */
	private static final String MAXIMA_PUBKEY 		= "maxima_publickey";
	private static final String MAXIMA_PRIVKEY 		= "maxima_privatekey";
	
	private static final String MAXIMA_MLSPUBKEY 	= "maxima_mlspublickey";
	private static final String MAXIMA_MLSPRIVKEY 	= "maxima_mlsprivatekey";
	private static final String MAXIMA_MLSHOST 		= "maxima_mlshost";
	private static final String MAXIMA_MLSTIME 		= "maxima_mlstime";
	private static final String MAXIMA_OLDMLSHOST 	= "maxima_oldmlshost";
	
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
	
	/**
	 * MLS Keys
	 */
	MLSService mMLSService = new MLSService();
	MiniData mMLSPublic;
	MiniData mMLSPrivate;
	String mMaximaMLSAddress;
	
	private boolean mInited 	= false;
	public boolean mMaximaLogs 	= false;

	/**
	 * The Contacts Manager
	 */
	MaximaContactManager mMaxContacts;
	
	/**
	 * Message Sender
	 */
	MaxMsgHandler mMaxSender;
	
	public MaximaManager() {
		super("MAXIMA");
		
		mMaxSender = new MaxMsgHandler();
		
		mMaxContacts = new MaximaContactManager(this);
		
		PostMessage(MAXIMA_INIT);
	}
	
	public void shutdown() {
		mMaxContacts.stopMessageProcessor();
		
		mMaxSender.stopMessageProcessor();
		
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
	
	public String getMLSHost() {
		String mls = mMLSService.getMLSServer();
		if(mls.equals("")) {
			return mMaximaMLSAddress+"@"+GeneralParams.MINIMA_HOST+":"+GeneralParams.MINIMA_PORT;
		}
		
		return mls;
	}
	
	public String getOldMLSHost() {
		String mls = mMLSService.getOldMLSServer();
		if(mls.equals("")) {
			return mMaximaMLSAddress+"@"+GeneralParams.MINIMA_HOST+":"+GeneralParams.MINIMA_PORT;
		}
		
		return mls;
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
				createMaximaKeys();
			
			}else {
				mPublic  = udb.getData(MAXIMA_PUBKEY, MiniData.ZERO_TXPOWID);
				mPrivate = udb.getData(MAXIMA_PRIVKEY, MiniData.ZERO_TXPOWID);
			
				//Convert to a Maxima Address
				mMaximaAddress = Address.makeMinimaAddress(mPublic);
			}
			
			//Check MLS Keys
			if(!udb.exists(MAXIMA_MLSPUBKEY)) {
				createMaximaMLSKeys();
			
			}else {
				mMLSPublic  = udb.getData(MAXIMA_MLSPUBKEY, MiniData.ZERO_TXPOWID);
				mMLSPrivate = udb.getData(MAXIMA_MLSPRIVKEY, MiniData.ZERO_TXPOWID);
			
				//Convert to a Maxima Address
				mMaximaMLSAddress = Address.makeMinimaAddress(mMLSPublic);
			}
			
			//Hard set the MLSService
			String oldserver 	= udb.getString(MAXIMA_OLDMLSHOST, "");
			String server 		= udb.getString(MAXIMA_MLSHOST, "");
			long mlstime		= udb.getNumber(MAXIMA_MLSTIME, MiniNumber.ZERO).getAsLong();
			mMLSService.hardSetMLSNode(oldserver, server, mlstime);
			
			//New Random UID for MLS GET Messages
			MLS_RANDOM_UID = MiniData.getRandomData(32).to0xString();
			
			//We are inited
			mInited = true;
			
			//Save the DB
			MinimaDB.getDB().saveUserDB();
			
			//Post a LOOP message that updates all my contacts just in case..
			PostTimerMessage(new TimerMessage(1000 * 60 * 2, MAXIMA_LOOP));
			
			//MLS Checker
			PostTimerMessage(new TimerMessage(1000 * 60 * 3, MAXIMA_LOOP_MLS));
			
		}else if(zMessage.getMessageType().equals(MAXIMA_LOOP_MLS)) {
			
			//Flush the MLS
			mMLSService.flushList();
			
			//The Min Time before we do an MLS lookup - 1 hr..
			long mintime = System.currentTimeMillis() - (1000 * 60 * 60);
			
			//Get all your contacts
			ArrayList<MaximaContact> allcontacts = maxdb.getAllContacts();
			for(MaximaContact contact : allcontacts) {
				
				//Have we heard from them lately
				if(contact.getLastSeen() < mintime) {
					
					//Send an MLS GET req..
					String mls = contact.getMLS();
					if(!mls.equals("")) {
						
						//Create a Get req
						MLSPacketGETReq req = new MLSPacketGETReq(contact.getPublicKey(), MLS_RANDOM_UID);
						
						//Get the data version
						MiniData reqdata = MiniData.getMiniDataVersion(req);
						
						Message getreq = maxima.createSendMessage(mls,MAXIMA_MLS_GETAPP,reqdata);
						PostMessage(getreq);
					}
				}
			}
		
			//Post a LOOP message that updates all my contacts just in case..
			PostTimerMessage(new TimerMessage(MAXIMA_LOOP_MLS_DELAY, MAXIMA_LOOP_MLS));
			
		}else if(zMessage.getMessageType().equals(MAXIMA_LOOP)) {
			
			//Resend all your details to your contacts
			PostMessage(MAXIMA_REFRESH);
			
			//Delete really old MaxHosts - not seen for 7 days
			maxdb.deleteOldHosts();
			
			//Post a LOOP message that updates all my contacts just in case..
			PostTimerMessage(new TimerMessage(MAXIMA_LOOP_DELAY, MAXIMA_LOOP));
		
		}else if(zMessage.getMessageType().equals(MAXIMA_REFRESH)) {
			
			//A list of all your contacts public keys
			ArrayList<String> validpubkeys = new ArrayList<>();
			
			//Get all your contacts
			ArrayList<MaximaContact> allcontacts = maxdb.getAllContacts();
			for(MaximaContact contact : allcontacts) {
				
				//Now send a message updating them
				Message update = new Message(MaximaContactManager.MAXCONTACTS_UPDATEINFO);
				update.addString("publickey", contact.getPublicKey());
				update.addString("address", contact.getCurrentAddress());
				getContactsManager().PostMessage(update);
				
				//Keep this for MLS
				validpubkeys.add(contact.getPublicKey());
			}
			
			//Create an MLSPacket
			MLSPacketSET mlspack = new MLSPacketSET(getRandomMaximaAddress());
			for(String pubkey : validpubkeys) {
				mlspack.addValidPublicKey(pubkey);
			}
			
			//Send the message - to BOTH hosts.. old and new
			PostMessage(maxima.createSendMessage(getMLSHost(),MAXIMA_MLS_SETAPP,MiniData.getMiniDataVersion(mlspack)));
			PostMessage(maxima.createSendMessage(getOldMLSHost(),MAXIMA_MLS_SETAPP,MiniData.getMiniDataVersion(mlspack)));
			
		}else if(zMessage.getMessageType().equals(MAXIMA_CONNECTED)) {
		
			//Get the client
			NIOClient nioc = (NIOClient) zMessage.getObject("nioclient");
			
			//Is this an internal IP
			String fullhost 	= nioc.getFullAddress(); 
			boolean invalidip	 = false;
			
			if(!GeneralParams.ALLOW_ALL_IP) {
				invalidip 	= 	fullhost.startsWith("127.") || 
								fullhost.startsWith("10.")  || 
								fullhost.startsWith("100.") ||
								fullhost.startsWith("169.") ||
								fullhost.startsWith("172.") ||
								fullhost.startsWith("198.") ||
								fullhost.startsWith("192.");
			}
			
			//Warn..
			if(invalidip && nioc.isOutgoing()) {
				MinimaLogger.log("Invalid IP for MAXIMA host ( is internal ) "+nioc.getFullAddress()+" ..re-enable with -allowallip");
				return;
			}
			
			//Send him our MLS details..
			if(nioc.isIncoming()) {
				MaximaCTRLMessage maxmls = new MaximaCTRLMessage(MaximaCTRLMessage.MAXIMACTRL_TYPE_MLS);
				maxmls.setData(new MiniData(mMaximaMLSAddress.getBytes()));
				NIOManager.sendNetworkMessage(nioc.getUID(), NIOMessage.MSG_MAXIMA_CTRL, maxmls);
			}
			
			//is it an outgoing.. ONLY outgoing can be used for MAXIMA
			if(!invalidip && nioc.isOutgoing()) {
				
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
				}else {
					MinimaLogger.log("MAXIMA EXISTING connection : "+nioc.getFullAddress());
				}
				
				//So we know the details.. Post them to him.. so he knows who we are..
				MaximaCTRLMessage maxmess = new MaximaCTRLMessage(MaximaCTRLMessage.MAXIMACTRL_TYPE_ID);
				maxmess.setData(mxhost.getPublicKey());
				NIOManager.sendNetworkMessage(nioc.getUID(), NIOMessage.MSG_MAXIMA_CTRL, maxmess);
				
				//And now post a check message..
				String to 			= mxhost.getMaximaAddress();
				String uid			= nioc.getUID();
				
				//Are we ready to mine a message
				if(MinimaDB.getDB().getTxPoWTree().getTip() != null){
					//Send Immediately..
					Message check = new Message(MAXIMA_SENDCHKCONNECT);
					check.addString("to", to);
					check.addString("uid", uid);
					PostMessage(check);
					
				}else {
					
					MinimaLogger.log("TIMED Maxima connect as no chain yet.. : "+nioc.getFullAddress());
					
					//With Delay
					TimerMessage check = new TimerMessage(10000,MAXIMA_SENDCHKCONNECT);
					check.addString("to", to);
					check.addString("uid", uid);
					PostTimerMessage(check);
				}
			}
			
		}else if(zMessage.getMessageType().equals(MAXIMA_SENDCHKCONNECT)) {
			
			//Send a check Connect message
			String to 			= zMessage.getString("to");
			String application 	= MAXIMA_CHKCONECT_APP;
			MiniData data 		= new MiniData(zMessage.getString("uid").getBytes());
			
			//Create a HELLO message
			Message chkconnect 	= maxima.createSendMessage(to,application,data);
			
			//Send it..
			PostMessage(chkconnect);
			
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
				
				if(mxhost != null) {
					MinimaLogger.log("MAXIMA outgoing disconnection : "+nioc.getFullAddress()+" "+reconnect);
				}
				
				//Do we need to update Users who contact us through them..
				if(!reconnect) {
					
					//Ok - lets reset contacts that use this host
					String host = nioc.getFullAddress();
					
					//Which contacts used that host - reassign them
					ArrayList<MaximaContact> allcontacts = maxdb.getAllContacts();
					for(MaximaContact contact : allcontacts) {
						
						//Only reset those that use this address
						if(contact.getMyAddress().contains(host)) {
						
							MinimaLogger.log("MAXIMA Updating contact on disconnected host : "+contact.getName());
							
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
			
			}else if(msg.getType().isEqual(MaximaCTRLMessage.MAXIMACTRL_TYPE_MLS)) {
				
				//Set the ID for this Connection
				String mlspubkey = new String(msg.getData().getBytes());
				
				//Set this as his MLS address
				nioc.setMaximaMLS(mlspubkey+"@"+nioc.getFullAddress());
			}
			
		}else if(zMessage.getMessageType().equals(MAXIMA_SENDMESSAGE)) {
		
			Message msg = new Message(MaxMsgHandler.MAX_SEND_MESSAGE);
			msg.addObject("msg", zMessage);
			mMaxSender.PostMessage(msg);
			
		}else if(zMessage.getMessageType().equals(MAXIMA_MLSGET_RESP)) {
			//Get the MLS Packet
			MLSPacketGETResp mls = (MLSPacketGETResp) zMessage.getObject("mlsget");
			
			//Set these details..
			MaximaContact contact = maxdb.loadContactFromPublicKey(mls.getPublicKey());
			
			//Set the new Maxima Address
			contact.setCurrentAddress(mls.getAddress());
			
			//And update the DB
			maxdb.updateContact(contact);
			
			//Now send a message updating them
			Message update = new Message(MaximaContactManager.MAXCONTACTS_UPDATEINFO);
			update.addString("publickey", contact.getPublicKey());
			update.addString("address", contact.getCurrentAddress());
			getContactsManager().PostMessage(update);
			
			if(mMaximaLogs) {
				MinimaLogger.log("MLSGET Contact updated : "+contact.toJSON().toString());
			}
			
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
			}else if(mpkg.mTo.equals(mMLSPublic)) {
				//It's an MLS message
				privatekey = mMLSPrivate;
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
			
			//Where is it headed
			String application = (String) maxjson.get("application");
			
			//Notify that Client that we received the message.. this makes external client disconnect ( internal just a ping )
			if(!application.equals(MAXIMA_MLS_GETAPP)) {
				maximaMessageStatus(nioc,MAXIMA_OK);
			}
			
			//Is it a special contact message
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
				
			}else if(application.equals(MAXIMA_CHKCONECT_APP)) {
				
				//Get the Data
				MiniData maxdata = new MiniData(maxjson.getString("data"));
				String uid = new String(maxdata.getBytes());
				
				//Check Valid..
				if(!uid.equals(nioc.getUID())) {
					MinimaLogger.log("INVALID MAXCHECK REC:"+uid+" FROM:"+nioc.getUID());
					return;
				}
				
				MinimaLogger.log("MAXIMA HOST accepted : "+nioc.getFullAddress());
				
				//Get the HOST
				MaximaHost mxhost = maxdb.loadHost(nioc.getFullAddress());
				
				//Now we can use this as one of Our Addresses
				mxhost.setConnected(1);
				maxdb.updateHost(mxhost);
				
				//OK.. add to our list
				if(nioc.isMaximaMLS()) {
					if(mMLSService.newMLSNode(nioc.getMaximaMLS())) {
						//Changed.. set new in DB
						UserDB udb = MinimaDB.getDB().getUserDB();
						udb.setString(MAXIMA_OLDMLSHOST, mMLSService.getOldMLSServer());
						udb.setString(MAXIMA_MLSHOST, mMLSService.getMLSServer());
						udb.setNumber(MAXIMA_MLSTIME, new MiniNumber(mMLSService.getMLSTime()));	
					}
				}
				
			}else if(application.equals(MAXIMA_MLS_SETAPP)) {
				
				//Get the package
				MiniData mlssetdata = new MiniData(maxjson.getString("data"));
				
				//Convert it..
				MLSPacketSET mls = MLSPacketSET.convertMiniDataVersion(mlssetdata);
				
				//Add to the MLSService
				mMLSService.addMLSData(maxmsg.mFrom.to0xString(), mls);
				
			}else if(application.equals(MAXIMA_MLS_GETAPP)) {
				
				//Get the data
				MiniData reqdata 	= new MiniData(maxjson.getString("data"));
				MLSPacketGETReq req = MLSPacketGETReq.convertMiniDataVersion(reqdata);
				
				//Check the MLS service for this 
				MLSPacketSET mlspack = mMLSService.getData(req.getPublicKey());
				
				//Do we have data
				if(mlspack == null) {
					MinimaLogger.log("Unknown publickey in MLSService "+req.getPublicKey());
					maximaMessageStatus(nioc,MAXIMA_UNKNOWN);
					return;
				}
				
				//Is THIS user allowed to see this data
				if(!mlspack.isValidPublicKey(maxmsg.mFrom.to0xString())) {
					MinimaLogger.log("Invalid MLS request for "+req.getPublicKey()+" by "+maxmsg.mFrom.to0xString());
					maximaMessageStatus(nioc,MAXIMA_UNKNOWN);
					return;
				}
				
				//Create a response..
				MLSPacketGETResp mlsget = new MLSPacketGETResp(req.getPublicKey(),mlspack.getMaximaAddress(),req.getRandomUID());
				
				//Convert to a MiniData structure
				MiniData mlsdata = MiniData.getMiniDataVersion(mlsget);
				
				//Send that
				maximaMessageStatus(nioc,mlsdata);
				
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
		
		//Time it..
		long timenow = System.currentTimeMillis();
		
		//Now create a MaxTxPow
		MaxTxPoW mxtxpow = MaxTxPoW.createMaxTxPoW(mp);
		
		//Message if took a long time
		long timediff = System.currentTimeMillis() - timenow;
		if(timediff>5000) {
			MinimaLogger.log("Maxima Construct took more than 5 seconds.. "+timediff);
		}
		
		//Create the Network Message
		return NIOManager.createNIOMessage(NIOMessage.MSG_MAXIMA_TXPOW, mxtxpow);
	}
	
	public static MiniData sendMaxPacket(String zHost, int zPort, MiniData zMaxMessage) throws IOException {
		
		//Open the socket..
		Socket sock = new Socket();

		//3 seconds to connect
		sock.connect(new InetSocketAddress(zHost, zPort), 5000);
		
		//10 seconds to read
		sock.setSoTimeout(30000);
		
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
			}else {
				
				//Is it an MLS Get Package..
				try {
					
					ByteArrayInputStream bais 	= new ByteArrayInputStream(resp.getBytes());
					DataInputStream respdis 	= new DataInputStream(bais);
					
					//What Type..
					MiniByte type = MiniByte.ReadFromStream(respdis);
					MiniData data = MiniData.ReadFromStream(respdis);
					
					//Convert
					MLSPacketGETResp mls = MLSPacketGETResp.convertMiniDataVersion(data);
					
					//Ok - it's read
					valid = MAXIMA_RESPONSE_OK;
					
					//Check ther Random UID - security
					if(!mls.getRandomUID().equals(MLS_RANDOM_UID)) {
						MinimaLogger.log("Invalid MLS GET RandomUID! from "+sock.getInetAddress().toString());
						valid = MAXIMA_RESPONSE_FAIL;
					
					}else{
						//Post this on Maxima..
						Message max = new Message(MAXIMA_MLSGET_RESP);
						max.addObject("mlsget", mls);
						Main.getInstance().getMaxima().PostMessage(max);
					}
					
					respdis.close();
					bais.close();
					
					break;
					
				}catch(Exception exc){
					MinimaLogger.log("Could not convert MLS GET Package : "+exc);
				}
			}
		}
		
		//Close the streams..
		dis.close();
		in.close();
		dos.close();
		out.close();
		
		return valid;
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
	
	/**
	 * MLS Functions
	 */
	private void createMaximaMLSKeys() throws Exception {
		
		//Get the UserDB
		UserDB udb = MinimaDB.getDB().getUserDB();
		
		//Create a new new maxima ident..
		KeyPair generateKeyPair = GenerateKey.generateKeyPair();
		
		byte[] publicKey 		= generateKeyPair.getPublic().getEncoded();
		mMLSPublic 				= new MiniData(publicKey);
		
		byte[] privateKey	 	= generateKeyPair.getPrivate().getEncoded();
		mMLSPrivate 			= new MiniData(privateKey);
	
		//Convert to a Maxima Address
		mMaximaMLSAddress 		= Address.makeMinimaAddress(mMLSPublic);
		
		//Put in the DB..
		udb.setData(MAXIMA_MLSPUBKEY, mMLSPublic);
		udb.setData(MAXIMA_MLSPRIVKEY, mMLSPrivate);
	}
}
