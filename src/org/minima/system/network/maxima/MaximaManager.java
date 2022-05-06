package org.minima.system.network.maxima;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.security.KeyPair;

import org.minima.database.MinimaDB;
import org.minima.database.maxima.MaximaDB;
import org.minima.database.maxima.MaximaHost;
import org.minima.database.userprefs.UserDB;
import org.minima.objects.Address;
import org.minima.objects.base.MiniData;
import org.minima.system.Main;
import org.minima.system.network.maxima.message.MaximaInternal;
import org.minima.system.network.maxima.message.MaximaMessage;
import org.minima.system.network.maxima.message.MaximaPackage;
import org.minima.system.network.minima.NIOClient;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.minima.NIOMessage;
import org.minima.utils.BaseConverter;
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
	 * Checker loop function - every 5 mins
	 */
	public static final String MAXIMA_LOOP 			= "MAXIMA_LOOP";
	long MAXIMA_LOOP_DELAY = 1000 * 60 * 5;
	
	/**
	 * Messages
	 */
	public static final String MAXIMA_CTRLMESSAGE 	= "MAXIMA_CTRLMESSAGE";
	public static final String MAXIMA_RECMESSAGE 	= "MAXIMA_RECMESSAGE";
	public static final String MAXIMA_SENDMESSAGE 	= "MAXIMA_SENDDMESSAGE";
	
	/**
	 * UserDB data
	 */
	private static final String MAXIMA_PUBKEY 	= "maxima_publickey";
	private static final String MAXIMA_PRIVKEY 	= "maxima_privatekey";
	
	/**
	 * The Response message for a Maxima Message
	 */
	public static final MiniData MAXIMA_RESPONSE = new MiniData("0x080000000101");
	
	/**
	 * RSA Keys
	 */
	MiniData mPublic;
	MiniData mPrivate;
	
	private boolean mInited 	= false;
	public boolean mMaximaLogs 	= true;

	public MaximaManager() {
		super("MAXIMA");
		
		PostMessage(MAXIMA_INIT);
	}
	
	public boolean isInited() {
		return mInited;
	}
	
	public String getPublicKey() {
		Address addr = new Address(mPublic);
		return addr.getMinimaAddress();
	}
	
//	public MaximaMessage createMaximaMessage(String zFullTo, String zApplication, MiniData zData) {
//		MaximaMessage maxima 	= new MaximaMessage();
//		maxima.mFrom 			= new MiniString(getFullIdentity());
//		maxima.mTo 				= new MiniString(zFullTo);
//		maxima.mApplication 	= new MiniString(zApplication);
//		maxima.mData 			= zData;
//		
//		return maxima;
//	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
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
			}
	
			//We are inited
			mInited = true;
			
			//Save the DB
			MinimaDB.getDB().saveUserDB();
			
			//Post a LOOP message that updates all my contacts just in case..
			PostTimerMessage(new TimerMessage(MAXIMA_LOOP_DELAY, MAXIMA_LOOP));
		
		}else if(zMessage.getMessageType().equals(MAXIMA_LOOP)) {
			
			//Tell all contacts how to get in touch with you..
			//..
			
			//Post a LOOP message that updates all my contacts just in case..
			PostTimerMessage(new TimerMessage(MAXIMA_LOOP_DELAY, MAXIMA_LOOP));
			
			
		}else if(zMessage.getMessageType().equals(MAXIMA_CONNECTED)) {
		
			//Get the client
			NIOClient nioc = (NIOClient) zMessage.getObject("nioclient");
			
			//is it an outgoing.. ONLY outgoing can be used for MAXIMA
			if(!nioc.isIncoming()) {
				//Get the MaximaDB
				MaximaDB mxdb = MinimaDB.getDB().getMaximaDB();
				
				//OK.. Do we hav this node in our list..
				MaximaHost mxhost = mxdb.loadHost(nioc.getFullAddress());
				
				//Do we have something..
				if(mxhost == null) {
					MinimaLogger.log("MAXIMA NEW connection : "+nioc.getFullAddress());
					
					//Create a new Host
					mxhost = new MaximaHost(nioc.getFullAddress());
					mxhost.createKeys();
					
					//Now insert this into the DB
					mxdb.newHost(mxhost);
				}else{
					MinimaLogger.log("MAXIMA EXISTING connection : "+nioc.getFullAddress());
					
					//Update our details..
					mxhost.updateLastSeen();
					mxdb.updateHost(mxhost);
				}
				
				//So we know the details.. Post them to him.. so he knows who we are..
				MaximaCTRLMessage maxmess = new MaximaCTRLMessage(MaximaCTRLMessage.MAXIMACTRL_TYPE_ID);
				maxmess.setData(mxhost.getPublicKey());
				NIOManager.sendNetworkMessage(nioc.getUID(), NIOMessage.MSG_MAXIMA_CTRL, maxmess);
			}
			
		}else if(zMessage.getMessageType().equals(MAXIMA_DISCONNECTED)) {
			
			//Get the client
			NIOClient nioc = (NIOClient) zMessage.getObject("nioclient");
			
			//is it an outgoing.. ONLY outgoing can be used for MAXIMA
			if(!nioc.isIncoming()) {
				MinimaLogger.log("MAXIMA outgoing disconnection : "+nioc.getFullAddress());
				
				//Do we need to update Users who contact us through them..
				//?
			}
		
		}else if(zMessage.getMessageType().equals(MAXIMA_CTRLMESSAGE)) {
			
			//Received a control message from a client
			MaximaCTRLMessage msg = (MaximaCTRLMessage) zMessage.getObject("maximactrl");
			
			if(msg.getType().isEqual(MaximaCTRLMessage.MAXIMACTRL_TYPE_ID)) {
				
				//Get the NIOClient
				NIOClient nioc = (NIOClient) zMessage.getObject("nioclient");
				
				//Set the ID for this Connection
				MiniData pubkey = msg.getData();

				//And Set..
				nioc.setMaximaIdent(pubkey.to0xString());
				
				MinimaLogger.log("MAXIMA address : "+pubkey.to0xString()+"@"+nioc.getFullAddress());
			}
			
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
