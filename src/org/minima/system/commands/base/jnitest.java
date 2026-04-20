package org.minima.system.commands.base;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;

import org.minima.objects.Magic;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.commands.Command;
import org.minima.utils.jni.jnifunctions;
import org.minima.utils.json.JSONObject;

public class jnitest extends Command {

	/**
	 * The Large Byte MiniNumber to set the Header up for hashing
	 */
	private static MiniNumber START_NONCE_BYTES = new MiniNumber("100000000000000000.00000000000000000000000000000000000000001");
	
	public jnitest() {
		super("jnitest","(maxattempts:) - Check the speed of header hashing with JNI calls. Defaults to 1000 hashes");
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
				+ "hashtest maxattempts:2000000\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"testnonce", "maxattempts", "targetdifficulty"}));
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

		//Get the input variables..
		int maxattempts 		= getNumberParam("maxattempts", new MiniNumber(1000)).getAsInt();
		MiniData targetdiff  	= getDataParam("targetdifficulty", Magic.MIN_TXPOW_WORK);
		
		//Create a TEST MiniNumber to return by default..
		MiniNumber testnonce = getNumberParam("testnonce", new MiniNumber("12345.54321"));
		byte[] testnoncedata = MiniData.getMiniDataVersion(testnonce).getBytes();
		
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
		
		//Time the function..
		long timenow = System.currentTimeMillis();
			
		//Now send the data to the JNI function..
		byte[] result = jni.hashHeaderWithDiff(testnoncedata, maxattempts, targetdiff.getBytes(), data);
		
		//Time it..
		long timediff = System.currentTimeMillis() - timenow;
		
		//Convert Byte array to MiniNumber
		DataInputStream dis = new DataInputStream(new ByteArrayInputStream(result));
		
		//Now check the returned MiniNumber nonce value..
		MiniNumber finalnonce = MiniNumber.ReadFromStream(dis);
		
		//Set this..
		txp.setNonce(finalnonce);
		
		//Calculate TxPoWID
		txp.calculateTXPOWID();
		
		//Have we found a valid txpow
		boolean foundvalidtxpow = false;
		if(txp.getTxPoWIDData().isLess(targetdiff)) {
			foundvalidtxpow = true;
		}
		
		JSONObject resp = new JSONObject();
		resp.put("result", outputByteArray(result));
		resp.put("nonce", finalnonce);
		resp.put("millitime", timediff);
		resp.put("targetdifficulty", targetdiff.to0xString());
		resp.put("txpowid", txp.getTxPoWID());
		resp.put("success", foundvalidtxpow);
		
		//Add balance..
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new jnitest();
	}

}
