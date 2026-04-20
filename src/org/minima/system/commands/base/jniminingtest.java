package org.minima.system.commands.base;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;

import org.minima.objects.Magic;
import org.minima.objects.TxHeader;
import org.minima.objects.TxPoW;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.commands.Command;
import org.minima.utils.Crypto;
import org.minima.utils.jni.jnifunctions;
import org.minima.utils.json.JSONObject;

public class jniminingtest extends Command {

	/**
	 * The Large Byte MiniNumber to set the Header up for hashing
	 */
	private static MiniNumber START_NONCE_BYTES = new MiniNumber("100000000000000000.00000000000000000000000000000000000000001");
	
	public jniminingtest() {
		super("jniminingtest","(maxattempts:) - Check the speed of header hashing with JNI and JAVA calls. Defaults to 1000 hashes");
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
				+ "benchmark hashes:2000000\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"maxattempts","testnonce", "maxattempts", "targetdifficulty"}));
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
		int amount 		= getNumberParam("maxattempts", new MiniNumber(1000000)).getAsInt();
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
		byte[] data 	= MiniData.getMiniDataVersion(txp.getTxHeader()).getBytes();
		byte[] datajava = MiniData.getMiniDataVersion(txp.getTxHeader()).getBytes();
		
		//First do the hashing using JAVA
		long timenowjava 		= System.currentTimeMillis();
		BigInteger newnoncejava = BigInteger.ZERO;
		MiniNumber javafinalnonce = MiniNumber.ZERO;
		boolean javafound=false;
		for(int i=0;i<amount;i++) {
			
			byte[] noncebytesjava = newnoncejava.toByteArray();
			newnoncejava 		  = newnoncejava.add(BigInteger.ONE);
			
			//Copy into the data array
			System.arraycopy(noncebytesjava, 0, datajava, 4, noncebytesjava.length);
			
			//Hash the data
			byte[] hashedbytes = Crypto.getInstance().hashData(datajava);
			
			//Make into a MiniData structure
			MiniData hash = new MiniData(hashedbytes);
			
			//Have we found a valid txpow
			if(hash.isLess(targetdiff)) {
				javafound=true;
				
				//Ok read in the final data..
				MiniData finaldata = new MiniData(datajava);
				
				//Now convert to a TxHeader
				TxHeader txh = TxHeader.convertMiniDataVersion(finaldata);
				
				//What was the nonce..
				javafinalnonce = txh.mNonce;
				
				break;
			}
			
		}
		long timediffjava = System.currentTimeMillis() - timenowjava;
		
		//Set this..
		txp.setNonce(javafinalnonce);
		
		//Calculate TxPoWID
		txp.calculateTXPOWID();
		
		//And get that..
		String javatxpowid = txp.getTxPoWID();
		
		//NOW DO THE JNI
		txp.setNonce(START_NONCE_BYTES);
		
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
		byte[] result = jni.hashHeaderWithDiff(testnoncedata, amount, targetdiff.getBytes(), data);
		
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
		resp.put("maxattempts", amount);
		resp.put("targetdifficulty", targetdiff.to0xString());
		
		JSONObject javaresp = new JSONObject();
		javaresp.put("millitime", timediffjava);
		javaresp.put("attempts", newnoncejava);
		javaresp.put("nonce", javafinalnonce);
		javaresp.put("found", javafound);
		javaresp.put("txpowid", javatxpowid);
		
		resp.put("java", javaresp);
		
		JSONObject jniresp = new JSONObject();
		jniresp.put("millitime", timediff);
		jniresp.put("nonce", finalnonce);
		jniresp.put("found", foundvalidtxpow);
		jniresp.put("txpowid", txp.getTxPoWID());
		
		resp.put("jni", jniresp);
		
		//Add balance..
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new jniminingtest();
	}

}
