package org.minima.system.network.maxima;

import java.security.KeyPair;

import org.minima.database.MinimaDB;
import org.minima.database.userprefs.UserDB;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.objects.keys.TreeKey;
import org.minima.system.params.GeneralParams;
import org.minima.utils.MinimaLogger;
import org.minima.utils.encrypt.CryptoPackage;
import org.minima.utils.encrypt.EncryptDecrypt;
import org.minima.utils.encrypt.GenerateKey;
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
	
	private static final String MAXIMA_SIGKEY 		= "maxima_treekey";
	private static final String MAXIMA_SIGKEYUSES 	= "maxima_treekey_uses";
	
	/**
	 * Encryption Keys
	 */
	MiniData mPublicEncrypt;
	MiniData mPrivateEncrypt;
	
	/**
	 * Signature Key
	 */
	TreeKey mSignatureKey;
	
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
		
		//What is your Encryption Public Key..
		String encrypt = mPublicEncrypt.to0xString();
		
		//What is your Signature Key
		String sigpubkey = mSignatureKey.getPublicKey().to0xString();
		
		return sigpubkey+":"+encrypt+"@"+host;
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
				mPublicEncrypt 	= udb.getData(MAXIMA_PUBKEY, MiniData.ZERO_TXPOWID);
				mPrivateEncrypt = udb.getData(MAXIMA_PRIVKEY, MiniData.ZERO_TXPOWID);
				
				//And now the Signature..
				MiniData privseed 	= udb.getData(MAXIMA_SIGKEY, MiniData.ZERO_TXPOWID);
				MiniNumber uses 	= udb.getNumber(MAXIMA_SIGKEYUSES, MiniNumber.ZERO);
				
				mSignatureKey = new TreeKey(privseed, 128, 3);
				mSignatureKey.setUses(uses.getAsInt());
			}
			
			mInited = true;
			
		}else if(zMessage.getMessageType().equals(MAXIMA_SENDMESSAGE)) {
			
			//Who to
			String to = zMessage.getString("to");
			
			//Break it down..
			int index  = to.indexOf(":");
			int index2 = to.indexOf("@");
			
			String encryptkey 	= to.substring(index,index2);
			String host 		= to.substring(index2+1);
			
			//What port..
			String port = zMessage.getString("port");
			
			//What data
			String datastr = zMessage.getString("data");
			MiniData data = new MiniData(datastr);
			
			MinimaLogger.log("MAXIMA ENC  : "+encryptkey);
			MinimaLogger.log("MAXIMA HOST : "+host);
			MinimaLogger.log("MAXIMA PORT : "+port);
			
			//Construct the Maxima Message
			CryptoPackage cp = new CryptoPackage();
			cp.encrypt(data.getBytes(), encryptkey.getBytes());
			
			MaximaMessage maxmsg = new MaximaMessage();
			maxmsg.mFromAddress = new MiniString(getIdentity());
			maxmsg.mTo			= new MiniString(to+":"+port);
			maxmsg.mData		= cp.getCompleteEncryptedData();
			
			//Now convert that..
			
			
		}else if(zMessage.getMessageType().equals(MAXIMA_RECMESSAGE)) {
			
			
		}
		
	}

	public void createMaximaKeys() throws Exception {
		
		//Get the UserDB
		UserDB udb = MinimaDB.getDB().getUserDB();
		
		//Create a new new maxima ident..
		KeyPair generateKeyPair = GenerateKey.generateKeyPair();
		
		byte[] publicKey 		= generateKeyPair.getPublic().getEncoded();
		MiniData pubk 			= new MiniData(publicKey);
		
		byte[] privateKey	 	= generateKeyPair.getPrivate().getEncoded();
		MiniData privk 			= new MiniData(privateKey);
		
		//Put in the DB..
		udb.setData(MAXIMA_PUBKEY, pubk);
		udb.setData(MAXIMA_PRIVKEY, privk);
		
		//And Create the Signature Key
		MiniData rand = MiniData.getRandomData(32);
		udb.setData(MAXIMA_SIGKEY, rand);
		udb.setNumber(MAXIMA_SIGKEYUSES, MiniNumber.ZERO);
		
		//Get the data..
		mPublicEncrypt 	= udb.getData(MAXIMA_PUBKEY, MiniData.ZERO_TXPOWID);
		mPrivateEncrypt = udb.getData(MAXIMA_PRIVKEY, MiniData.ZERO_TXPOWID);
		
		//And now the Signature..
		MiniData privseed 	= udb.getData(MAXIMA_SIGKEY, MiniData.ZERO_TXPOWID);
		MiniNumber uses 	= udb.getNumber(MAXIMA_SIGKEYUSES, MiniNumber.ZERO);
		
		mSignatureKey = new TreeKey(privseed, 128, 3);
		mSignatureKey.setUses(uses.getAsInt());
	}
}
