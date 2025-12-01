package org.minima.system.commands.base;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;

import org.minima.objects.TxHeader;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.brains.TxPoWMiner;
import org.minima.system.commands.Command;
import org.minima.utils.MinimaLogger;
import org.minima.utils.jni.jnifunctions;
import org.minima.utils.json.JSONObject;

public class jnitest extends Command {

	/**
	 * The Large Byte MiniNumber to set the Header up for hashing
	 */
	private static MiniNumber START_NONCE_BYTES = new MiniNumber("100000000000000000.00000000000000000000000000000000000000001");
	
	public jnitest() {
		super("jnitest","(amount:) - Check the speed of header hashing with JNI calls. Defaults to 1 million hashes");
	}
	
	@Override
	public String getFullHelp() {
		return "\nnoncetest\n"
				+ "\n"
				+ "Check the speed of hashing of this device. Defaults to 1 million hashes.\n"
				+ "\n"
				+ "Returns the time taken in milliseconds and speed in megahashes/second.\n"
				+ "\n"
				+ "E.g. A speed of 0.5 MH/s indicates 500000 hashes per second.\n"
				+ "\n"
				+ "amount: (optional)\n"
				+ "    Number of hashes to execute.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "hashtest\n"
				+ "\n"
				+ "hashtest amount:2000000\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"amount"}));
	}
	
	private String outputByteArray(byte[] zData) {
		String ret = "";
		for(int i=0;i<zData.length;i++) {
			ret += Byte.toString(zData[i])+",";
		}
		return ret;
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();

		//Time the function..
		long timenow = System.currentTimeMillis();
		
		//First create a random header
		TxPoW txp = new TxPoW();
		
		//Hard set the Header Body hash - now we are mining it can never change
		txp.setHeaderBodyHash();
		
		//Set the nonce.. we make it a large size in bytes then edit those - no reserialisation
		txp.setNonce(START_NONCE_BYTES);
		
		//Get the byte data
		byte[] data = MiniData.getMiniDataVersion(txp.getTxHeader()).getBytes();
		
		//Set the initial Nonce..
		BigInteger newnonce = BigInteger.ZERO;
		byte[] noncebytes 	= newnonce.toByteArray();
		
		//Copy these into the byte array of the TxHeader 
		//start 2 numbers in so leading zero is not changed
		System.arraycopy(noncebytes, 0, data, 4, noncebytes.length);
		
		//The JNI Object
		jnifunctions jni = new jnifunctions();
		
		//Say hello test
		jni.sayHello();
		
		//Now send this to the JNI function..
		byte[] result = jni.hashHeader(data);
		
		//Now convert to a TxHeader
		TxHeader txh = TxHeader.convertMiniDataVersion(new MiniData(result));
		
		//What was the nonce..
		MiniNumber finalnonce = txh.mNonce;
		
		long timediff = System.currentTimeMillis() - timenow;
		
		//Now set the final nonce..
		txp.setNonce(finalnonce);
		
		//Calculate TxPoWID
		txp.calculateTXPOWID();
		
		JSONObject resp = new JSONObject();
		resp.put("result", outputByteArray(result));
		resp.put("nonce", finalnonce);
		resp.put("millitime", timediff);
		resp.put("txpowid", txp.getTxPoWID());
		
		//Add balance..
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new jnitest();
	}

}
