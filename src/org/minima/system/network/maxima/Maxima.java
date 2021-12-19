package org.minima.system.network.maxima;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.security.KeyPair;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.UserDB;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.minima.NIOMessage;
import org.minima.system.params.GeneralParams;
import org.minima.system.params.GlobalParams;
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

public class Maxima extends MessageProcessor {

	/**
	 * Maxima Messages
	 */
	public static final String MAXIMA_INIT 			= "MAXIMA_INIT";
	public static final String MAXIMA_RECMESSAGE 	= "MAXIMA_RECMESSAGE";
	public static final String MAXIMA_SENDMESSAGE 	= "MAXIMA_SENDDMESSAGE";
	
	public static final String MAXIMA_TEST_MSG 		= "MAXIMA_TEST_MSG";
	public long MAXIMA_TEST_TIMER					= 1000 * 60 * 10;
	
	/**
	 * UserDB data
	 */
	private static final String MAXIMA_PUBKEY 	= "maxima_publickey";
	private static final String MAXIMA_PRIVKEY 	= "maxima_privatekey";
	
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
	
	public Maxima() {
		super("MAXIMA");
		PostMessage(MAXIMA_INIT);
		
		//Start the test message..
//		PostTimerMessage(new TimerMessage(5000, MAXIMA_TEST_MSG));
	}
	
	public boolean isInited() {
		return mInited;
	}
	
	public String getIdentity() {
		
		//What is your Host..
		String host = GeneralParams.MINIMA_HOST;
		int port 	= GeneralParams.MINIMA_PORT;
		
		//Base32
		String b32 = BaseConverter.encode32(mPublic.getBytes());
		
		return b32+"@"+host+":"+port;
	}
	
	public MaximaMessage createMaximaMessage(String zFullTo, String zApplication, MiniData zData) {
		MaximaMessage maxima 	= new MaximaMessage();
		maxima.mFrom 			= new MiniString(getIdentity());
		maxima.mTo 				= new MiniString(zFullTo);
		maxima.mApplication 	= new MiniString(zApplication);
		maxima.mData 			= zData;
		
		return maxima;
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
			
			mInited = true;
			
			//Save the DB
			MinimaDB.getDB().saveUserDB();
			
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
			
			//Is it for us!
			if(!mpkg.mTo.isEqual(mPublic)) {
				MinimaLogger.log("MAXIMA message received to unknown PublicKey : "+mpkg.mTo.to0xString());
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
		
		}else if(zMessage.getMessageType().equals(MAXIMA_TEST_MSG)) {
			
			//Get the Maxima Message
			MaximaMessage maxima 	= createMaximaMessage(getIdentity(), "maxchat", new MiniData("0xFF"));
			
			//Send to Maxima..
			Message sender = new Message(Maxima.MAXIMA_SENDMESSAGE);
			sender.addObject("maxima", maxima);
			sender.addString("publickey", mPublic.to0xString());
			sender.addString("tohost", GeneralParams.MINIMA_HOST);
			sender.addInteger("toport", GeneralParams.MINIMA_PORT);
			
			//Post It!
			PostMessage(sender);
			
			//And again in a minute..
			PostTimerMessage(new TimerMessage(MAXIMA_TEST_TIMER, MAXIMA_TEST_MSG));
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
