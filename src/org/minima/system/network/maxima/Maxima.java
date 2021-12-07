package org.minima.system.network.maxima;

import java.io.DataOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.channels.SocketChannel;
import java.security.KeyPair;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.UserDB;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.objects.keys.TreeKey;
import org.minima.system.Main;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.minima.NIOMessage;
import org.minima.system.params.GeneralParams;
import org.minima.utils.BaseConverter;
import org.minima.utils.MinimaLogger;
import org.minima.utils.encrypt.CryptoPackage;
import org.minima.utils.encrypt.EncryptDecrypt;
import org.minima.utils.encrypt.GenerateKey;
import org.minima.utils.encrypt.SignVerify;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class Maxima extends MessageProcessor {

	/**
	 * Maxima Messages
	 */
	public static final String MAXIMA_INIT 			= "MAXIMA_INIT";
	public static final String MAXIMA_RECMESSAGE 	= "MAXIMA_RECMESSAGE";
	public static final String MAXIMA_SENDMESSAGE 	= "MAXIMA_SENDDMESSAGE";
	
	/**
	 * UserDB data
	 */
	private static final String MAXIMA_PUBKEY 	= "maxima_publickey";
	private static final String MAXIMA_PRIVKEY 	= "maxima_privatekey";
	
	/**
	 * RSA Keys
	 */
	MiniData mPublic;
	MiniData mPrivate;
	
	boolean mInited = false;
	
	public Maxima() {
		super("MAXIMA");
		PostMessage(MAXIMA_INIT);
	}
	
	public boolean isInited() {
		return mInited;
	}
	
	public String getIdentity() {
		
		//What is your Host..
		String host = GeneralParams.MINIMA_HOST;
		int port 	= GeneralParams.MINIMA_PORT;
		
		//What is your Encryption Public Key..
//		String rsapub = mPublic.to0xString();
		
		//Base32
		String b32 = BaseConverter.xencode32(mPublic.getBytes());
		
		return b32+"@"+host+":"+port;
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
			
		}else if(zMessage.getMessageType().equals(MAXIMA_SENDMESSAGE)) {
			
			//Who to
			String publickey	= zMessage.getString("publickey");
			
			MiniData pubk = null;
			if(publickey.startsWith("Mx")) {
				pubk = new MiniData(BaseConverter.xdecode32(publickey));
			}else {
				pubk = new MiniData(publickey);
			}
			
			String tohost 		= zMessage.getString("tohost");
			int toport			= zMessage.getInteger("toport");
			String application 	= zMessage.getString("application");
			String message 		= zMessage.getString("message");
			
			//What data
			MiniString ds 	= new MiniString(message);
			MiniData data 	= new MiniData(ds.getData());
				
			//First create the Maxima Message
			MaximaMessage mm = new MaximaMessage();
			mm.mFromAddress  = new MiniString(getIdentity());
			mm.mToPublic	 = pubk;
			mm.mApplication	 = new MiniString(application);
			mm.mData		 = data;
			
			//Sign the Data..
			byte[] sigBytes  = SignVerify.sign(mPrivate.getBytes(), data.getBytes());
			mm.mSignature	 = new MiniData(sigBytes); 
			
			//Now get the complete Message as a MiniData Package..
			MiniData mmdata = MiniData.getMiniDataVersion(mm);
			
			//Now Encrypt the Whole Thing..
			CryptoPackage cp = new CryptoPackage();
			cp.encrypt(mmdata.getBytes(), pubk.getBytes());
			
			//Now Construct a MaximaPackage
			MaximaPackage mp = new MaximaPackage( mm.mToPublic , cp.getCompleteEncryptedData());
			
			//Create the final Message
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
			MaximaMessage mm = MaximaMessage.ConvertMiniDataVersion(new MiniData(data));
			
			//Now we have a Message!
			MinimaLogger.log("MAXIMA : "+mm.toJSON().toString());
			
			//Get the public key of the sender..
			String fromstr 	= mm.mFromAddress.toString();
			int index		= fromstr.indexOf("@");
			String pubkstr  = fromstr.substring(0, index);
			MiniData pubk = null;
			if(pubkstr.startsWith("Mx")) {
				pubk = new MiniData(BaseConverter.xdecode32(pubkstr));
			}else {
				pubk = new MiniData(pubkstr);
			}
			
			//Check the Signature..
			boolean valid = SignVerify.verify(pubk.getBytes(), mm.mData.getBytes(), mm.mSignature.getBytes());
			if(!valid) {
				MinimaLogger.log("MAXIMA Invalid Signature on message : "+mpkg.mTo.to0xString());
				return;
			}
			
			//Notify The Web Hook Listeners
			JSONObject event = new JSONObject();
			event.put("event", "MAXIMA");
			event.put("message", mm.toJSON());
			
			Main.getInstance().PostNotifyEvent(event);
		}
	}

	private void sendMaximaMessage(String zHost, int zPort, MiniData zMaxMessage) {
		
		try {
			//Open the socket..
			Socket sock 			= new Socket(zHost, zPort);
			
			//Create the streams..
			OutputStream out 		= sock.getOutputStream();
			DataOutputStream dos 	= new DataOutputStream(out);
			
			//Write the data
			zMaxMessage.writeDataStream(dos);
			dos.flush();
			
			dos.close();
			out.close();
		
		}catch(Exception exc){
			MinimaLogger.log("Error sending Maxima message : "+exc.toString());
		}
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
