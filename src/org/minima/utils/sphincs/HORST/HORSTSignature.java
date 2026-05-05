package org.minima.utils.sphincs.HORST;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.mmr.MMRProof;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class HORSTSignature implements Streamable {

	/**
	 * These are the Private key values at the referenced position in the private key
	 * 
	 * The Pre-images of the Public key values..
	 */
	ArrayList<MiniData> mSigValues = new ArrayList<>();
	
	/**
	 * These are the MMRProofs of each public key value in the public key tree
	 */
	ArrayList<MMRProof>  mPublicKeyProofs = new ArrayList<>();
	
	public HORSTSignature() {}
	
	public ArrayList<MiniData> getSignatureValues(){
		return mSigValues;
	}
	
	public ArrayList<MMRProof> getPublicKeyTreeProofs(){
		return mPublicKeyProofs;
	}
	
	public JSONObject toJSON() {
		JSONObject json = new JSONObject();
		
		int sigsize = mSigValues.size();
		
		JSONArray arr = new JSONArray();
		for(int i=0;i<sigsize;i++) {
			
			JSONObject sigval = new JSONObject();
			sigval.put("sigvalue", mSigValues.get(i).to0xString());
			
			MiniData proofdata = MiniData.getMiniDataVersion(mPublicKeyProofs.get(i));
			sigval.put("sigproof", proofdata.to0xString());
			
			arr.add(sigval);
		}
		
		json.put("values", arr);
		
		return json;
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		int sigsize = mSigValues.size();
		MiniNumber.WriteToStream(zOut, sigsize);
		
		for(int i=0;i<sigsize;i++) {
			mSigValues.get(i).writeDataStream(zOut);
			mPublicKeyProofs.get(i).writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		
		mSigValues 		 = new ArrayList<>();
		mPublicKeyProofs = new ArrayList<>();
		
		int sigsize = MiniNumber.ReadFromStream(zIn).getAsInt();
		for(int i=0;i<sigsize;i++) {
			mSigValues.add(MiniData.ReadFromStream(zIn));
			mPublicKeyProofs.add(MMRProof.ReadFromStream(zIn));
		}
	}
	
	/**
	 * Convert a MiniData version into a HORSTSignature
	 */
	public static HORSTSignature convertMiniDataVersion(MiniData zTxpData) {
		ByteArrayInputStream bais 	= new ByteArrayInputStream(zTxpData.getBytes());
		DataInputStream dis 		= new DataInputStream(bais);
		
		HORSTSignature horstsig = new HORSTSignature();
		
		try {
			//Convert data
			horstsig.readDataStream(dis);
		
			dis.close();
			bais.close();
			
		} catch (IOException e) {
			MinimaLogger.log(e);
		}
		
		return horstsig;
	}
}
