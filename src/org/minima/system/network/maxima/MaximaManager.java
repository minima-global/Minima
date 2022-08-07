package org.minima.system.network.maxima;

import java.io.IOException;
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
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.commands.maxima.maxima;
import org.minima.system.network.maxima.message.MaxTxPoW;
import org.minima.system.network.maxima.message.MaximaErrorMsg;
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
import org.minima.system.network.p2p.P2PManager;
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
	public static final String MAXIMA_CTRLMESSAGE 		= "MAXIMA_CTRLMESSAGE";
	public static final String MAXIMA_RECMESSAGE 		= "MAXIMA_RECMESSAGE";
	public static final String MAXIMA_SENDMESSAGE 		= "MAXIMA_SENDDMESSAGE";
	public static final String MAXIMA_REFRESH 			= "MAXIMA_REFRESH";
	public static final String MAXIMA_MLSGET_RESP 		= "MAXIMA_GETREQ";
	public static final String MAXIMA_CHECK_CONNECTED 	= "MAXIMA_CHECK_CONNECTED";
	public static final String MAXIMA_CHECK_MLS 		= "MAXIMA_CHECK_MLS";
	public static final String MAXIMA_REFRESH_TIMER 	= "MAXIMA_REFRESH_TIMER";
	
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
	boolean mHaveContacts = false;
	
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
		
		mMaxSender 		= new MaxMsgHandler(this);
		mMaxContacts 	= new MaximaContactManager(this);
		
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
	
	public String getMaximaMLSIdentity() {
		return mMaximaMLSAddress;
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
		return getMaximaMLSIdentity()+"@"+GeneralParams.MINIMA_HOST+":"+GeneralParams.MINIMA_PORT;
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
	
	public ArrayList<MaximaHost> getAllConnectedHosts(){
		
		//Get all the current hosts
		ArrayList<MaximaHost> hosts = MinimaDB.getDB().getMaximaDB().getAllHosts();
		
		//Only the connected ones..
		ArrayList<MaximaHost> connctedhosts = new ArrayList<>();
		for(MaximaHost host : hosts) {
			if(host.isConnected()) {
				connctedhosts.add(host);
			}
		}
		
		return connctedhosts;
	}
	
	public String getRandomMaximaAddress() {

		//Who are we connected to
		ArrayList<MaximaHost> connctedhosts = getAllConnectedHosts();
		
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
			PostTimerMessage(new TimerMessage(1000 * 60 * 3, MAXIMA_LOOP));
			
		}else if(zMessage.getMessageType().equals(MAXIMA_LOOP)) {
			
			//Check the MLS servers - for Users we have not seen in a while..
			PostMessage(MAXIMA_CHECK_MLS);
			
			//Resend all your details to your contacts
			PostMessage(MAXIMA_REFRESH_TIMER);
			
			//Delete really old MaxHosts - not seen for 7 days
			maxdb.deleteOldHosts();
			
			//Post a LOOP message that updates all my contacts just in case..
			PostTimerMessage(new TimerMessage(MAXIMA_LOOP_DELAY, MAXIMA_LOOP));
		
		}else if(zMessage.getMessageType().equals(MAXIMA_REFRESH_TIMER)) {
			
			//Wait 60 seconds.. then refresh Users
			PostTimerMessage(new TimerMessage(60000, MAXIMA_REFRESH));
			
		}else if(zMessage.getMessageType().equals(MAXIMA_CHECK_MLS)) {
			
			//Are we forcing
			boolean force = zMessage.getBoolean("force",false);
			
			//Flush the MLS
			mMLSService.flushList();
			
			//The Min Time before we do an MLS lookup - 30 mins.. 1 loop
			long mintime = System.currentTimeMillis() - (1000 * 60 * 30);
			
			//Get all your contacts
			ArrayList<MaximaContact> allcontacts = maxdb.getAllContacts();
			for(MaximaContact contact : allcontacts) {
				
				//Have we heard from them lately
				if(force || contact.getLastSeen() < mintime) {
					
					//Send an MLS GET req..
					String mls = contact.getMLS();
					if(!mls.equals("")) {
						
						//Log it.. 
						MinimaLogger.log("MLS check "+contact.getName()+" @ "+contact.getCurrentAddress()+" mls:"+mls);	
						
						//Create a Get req
						MLSPacketGETReq req = new MLSPacketGETReq(contact.getPublicKey(), MLS_RANDOM_UID);
						
						//Get the data version
						MiniData reqdata = MiniData.getMiniDataVersion(req);
						
						Message getreq = maxima.createSendMessage(mls,MAXIMA_MLS_GETAPP,reqdata);
						PostMessage(getreq);
					}
				}
			}
		
		}else if(zMessage.getMessageType().equals(MAXIMA_REFRESH)) {
			
			//Update the MLS Servers
			updateMLSServers();
			
			//Get all your contacts
			ArrayList<MaximaContact> allcontacts = maxdb.getAllContacts();
			for(MaximaContact contact : allcontacts) {
				
				//Now send a message updating them
				Message update = new Message(MaximaContactManager.MAXCONTACTS_UPDATEINFO);
				update.addString("publickey", contact.getPublicKey());
				update.addString("address", contact.getCurrentAddress());
				getContactsManager().PostMessage(update);
			}
			
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
			
			//Get the NIO Client uid
			String uid = zMessage.getString("uid");
			
			//Send a check Connect message
			String to 			= zMessage.getString("to");
			String application 	= MAXIMA_CHKCONECT_APP;
			MiniData data 		= new MiniData(uid.getBytes());
			
			//Create a HELLO message
			Message chkconnect 	= maxima.createSendMessage(to,application,data);
			
			//Send it..
			PostMessage(chkconnect);
			
			//AND - send a message that 
			TimerMessage checkconnected = new TimerMessage(30000, MAXIMA_CHECK_CONNECTED);
			checkconnected.addString("uid", uid);
			PostTimerMessage(checkconnected);
			
		}else if(zMessage.getMessageType().equals(MAXIMA_CHECK_CONNECTED)) {
			
			//Check that IF this host is connected to us - it is a valid Maxima host
			String uid = zMessage.getString("uid");
			
			//Get the NIO Client..
			NIOClient nioc =  Main.getInstance().getNIOManager().getNIOClientFromUID(uid);
			
			//Is it valid..
			if(nioc != null) {
				
				//Ok - we should be connected..
				MaximaHost mxhost = maxdb.loadHost(nioc.getFullAddress());
				
				//Are we connected..
				MinimaLogger.log("MAXIMA Check if connected : "+nioc.getFullAddress()+" "+mxhost.getConnected());
				
				//If not connected..
				if(mxhost.getConnected() == 0) {
					
					//How many valid hosts are we connected to.. if enough leave it..
					int conns = getAllConnectedHosts().size();
					if(conns < 2) {
					
						MinimaLogger.log("MAXIMA disconnecting from "+nioc.getFullAddress()+" reconnecting to random host");
						
						//Disconnect
						Main.getInstance().getNIOManager().disconnect(uid);
						
						//Disconnect this and reconnect to a random peer..
						Main.getInstance().getNetworkManager().getP2PManager().PostMessage(P2PManager.P2P_RANDOM_CONNECT);
					
					}else {
						MinimaLogger.log("MAXIMA Connected to "+conns+" Hosts.. not disconnecting..");
					}
				}
				
			}else {
				MinimaLogger.log("MAXIMA check if connected : HOST NOT AVAILABLE.. "+uid);
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
				
				if(mxhost != null) {
					MinimaLogger.log("MAXIMA outgoing disconnection : "+nioc.getFullAddress()+" "+reconnect);
				}
				
				//Update the MLS Servers
				updateMLSServers();
				
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
				if(!reconnect) {
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
			
			//Log it..
			MinimaLogger.log("MLSGET address updated for "+contact.getName()+" "+contact.getCurrentAddress());
			
		}else if(zMessage.getMessageType().equals(MAXIMA_RECMESSAGE)) {
			
			//Get the MaxTxPoW
			MaxTxPoW mxtxpow 	= (MaxTxPoW) zMessage.getObject("maxtxpow");
			
			//Get the NIOClient
			NIOClient nioc 		= (NIOClient) zMessage.getObject("nioclient");
			
			//Put the WHOLE thing in a try catch incase there is an ERROR.. remote debugging..
			try {
				
				//received a Message!
				MaximaPackage mpkg 	= mxtxpow.getMaximaPackage();
			
				//Private key tpo decode the message
				MiniData privatekey = null;
				
				//The pubkey it is encrypted with
				String tomaxima = mpkg.mTo.to0xString();
				
				//Debug Remote Function
				if(tomaxima.equals("0x30819F300D06092A864886F70D010101050003818D00308189028181008366F357B1054F76FF4B7B352D1464D7EACBCB7FFA5B2E638D5E358D1314AD3184743364CFD40C8A15FFF10E1EA49E5825B6ACCE3391E1D4B23650BDA27C42EBA5BA389CE2FA89C62BBBC6B62F6076FB6E8385FCC261815FB5D4B0BB9603FAB1BEF9D12F694003C2CF3D9461429BA78F30343A0422371041BDFBC4C7D189102B0203010001")) {
					maximaMessageErrorStatus(nioc, "DEBUG_1");
					return;
				}
				
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
				
				//Debug Remote Function
				if(tomaxima.equals("0x30819F300D06092A864886F70D010101050003818D0030818902818100A595C607021AA5B0B7E5919CA428E3F0B45B6D129A2CBD815569BBA4160890391E71FD5270586FB0AC77BE617449BC299C0FC44CE72D5D45E6F3E5BA00CA16012CCD16A82AAE753DB8581C267C9F1D3C2B1FC4EE331B825A2583F50240BCFDA51269A7D75566CD665781092D3634F3E3B516ECC250A562D933A347AA57780B0F0203010001")) {
					maximaMessageErrorStatus(nioc, "DEBUG_2");
					return;
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
				
				//Debug Remote Function
				if(tomaxima.equals("0x30819F300D06092A864886F70D010101050003818D0030818902818100B4A664963EE2787C38EFCDCE21849E336F574B02659D55530FBA7F87382780DEA989CDE368F374C7A17515320F4A0DF9529F4F6958539C0A9D1D06096722E298D08C543FE6E2D8FC847F9193BAFCF849BAE36EF9936A24531FA794B48A103316F936BA6261B92A70E0D0F23D308E26EE0C05DE568756C9DDD65D2AA5BA33241B0203010001")) {
					maximaMessageErrorStatus(nioc, "DEBUG_3");
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
									
							//Save this..
							MinimaDB.getDB().saveUserDB();
						}
					}
					
					//Update the MLS
					updateMLSServers();
					
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
					MinimaLogger.log("MLS Req received : replying "+mlsget.toJSON());
					
					maximaMessageStatus(nioc,mlsdata);
					
				}else {
					//Notify The Listeners
					Main.getInstance().PostNotifyEvent("MAXIMA",maxjson);
				}
			
			}catch(Exception Exc) {
				MinimaLogger.log(Exc);
				
				//Get the complete StackTrace..
				String trace = Exc.toString()+"--";
				for(StackTraceElement stack : Exc.getStackTrace()) {
					//Print it..
					trace += stack.toString()+"--";
				}
				
				//And send this..
				maximaMessageErrorStatus(nioc, trace);
			}
		}
	}
	
	private void maximaMessageErrorStatus(NIOClient zClient, String zMessage) throws IOException {
		//And send this..
		MaximaErrorMsg error 	= new MaximaErrorMsg(zMessage);
		MiniData errdata 		= MiniData.getMiniDataVersion(error);
		maximaMessageStatus(zClient,errdata);
	}
	
	private void maximaMessageStatus(NIOClient zClient, MiniData zStatus) throws IOException {
		//Send this Maxima response
		NIOManager.sendNetworkMessage(zClient.getUID(), NIOMessage.MSG_PING, zStatus);
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
	
	/**
	 * Update the MLS servers
	 */
	public void updateMLSServers(){
		//A list of all your contacts public keys
		ArrayList<String> validpubkeys = new ArrayList<>();
		
		//Which contacts used that host - reassign them
		ArrayList<MaximaContact> allcontacts = MinimaDB.getDB().getMaximaDB().getAllContacts();
		for(MaximaContact contact : allcontacts) {
			//Store for the MLS
			validpubkeys.add(contact.getPublicKey());
		}
		
		//Create an MLSPacket
		MLSPacketSET mlspack = new MLSPacketSET(getRandomMaximaAddress());
		for(String pubkey : validpubkeys) {
			mlspack.addValidPublicKey(pubkey);
		}
		
		//Get the MiniData version
		MiniData mlspackdata = MiniData.getMiniDataVersion(mlspack);
		
		//Refresh My MLS hosts..
		if(allcontacts.size() > 0) {
			//Send the message - to BOTH hosts.. old and new
			PostMessage(maxima.createSendMessage(getMLSHost(),MAXIMA_MLS_SETAPP,mlspackdata));
			PostMessage(maxima.createSendMessage(getOldMLSHost(),MAXIMA_MLS_SETAPP,mlspackdata));
			mHaveContacts = true;
		}else {
			if(mHaveContacts) {
				PostMessage(maxima.createSendMessage(getMLSHost(),MAXIMA_MLS_SETAPP,mlspackdata));
				PostMessage(maxima.createSendMessage(getOldMLSHost(),MAXIMA_MLS_SETAPP,mlspackdata));
			}
			mHaveContacts = false;
		}
	}
}
