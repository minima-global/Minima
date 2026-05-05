package org.minima.utils.sphincs.FORS;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.mmr.MMRData;
import org.minima.objects.mmr.MMRProof;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.sphincs.HORST.HORSTSignature;

public class FORSSignature implements Streamable {

	/**
	 * The HORST signaure data
	 */
	HORSTSignature mHORSTSignature = new HORSTSignature();
	
	/**
	 * The Root key of Each HORST Tree
	 */
	ArrayList<MMRData> mHORSTRoots = new ArrayList<>();
	
	/**
	 * These are the MMRProofs of each HORST tree used
	 */
	ArrayList<MMRProof>  mHORSTTreeProofs = new ArrayList<>();
	
	public FORSSignature() {}
		
	public HORSTSignature getHORSTSignature() {
		return mHORSTSignature;
	}
	
	public ArrayList<MMRData> getHORSTRoots(){
		return mHORSTRoots;
	}
	
	public ArrayList<MMRProof> getHORSTTreeProofs(){
		return mHORSTTreeProofs;
	}
	
	public JSONObject toJSON() {
		JSONObject json = new JSONObject();
		
		//Output the HORST sig
		json.put("horstsignature",mHORSTSignature.toJSON());
		
		int sigsize   = mHORSTSignature.getSignatureValues().size();
		JSONArray arr = new JSONArray();
		for(int i=0;i<sigsize;i++) {
			
			JSONObject sigval = new JSONObject();
			
			MiniData horstroots = MiniData.getMiniDataVersion(mHORSTRoots.get(i));
			sigval.put("horstroot", mHORSTRoots.get(i).toJSON());
			
			MiniData horstdata = MiniData.getMiniDataVersion(mHORSTTreeProofs.get(i));
			sigval.put("horstproof", horstdata.to0xString());
			
			//Add to our array
			arr.add(sigval);
		}
		
		json.put("horstroots",arr);
		
		return json;
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mHORSTSignature.writeDataStream(zOut);
		
		//Now write the roots
		int sigsize   = mHORSTSignature.getSignatureValues().size();
		MiniNumber.WriteToStream(zOut, sigsize);
		
		for(int i=0;i<sigsize;i++) {
			mHORSTRoots.get(i).writeDataStream(zOut);
			mHORSTTreeProofs.get(i).writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mHORSTSignature	 =  new HORSTSignature();
		mHORSTSignature.readDataStream(zIn);
		
		mHORSTRoots 	 = new ArrayList<>();
		mHORSTTreeProofs = new ArrayList<>();
		
		int sigsize = MiniNumber.ReadFromStream(zIn).getAsInt();
		for(int i=0;i<sigsize;i++) {
			mHORSTRoots.add(MMRData.ReadFromStream(zIn));
			mHORSTTreeProofs.add(MMRProof.ReadFromStream(zIn));
		}
	}
	
	/**
	 * Convert a MiniData version into a FORSSignature
	 */
	public static FORSSignature convertMiniDataVersion(MiniData zTxpData) {
		ByteArrayInputStream bais 	= new ByteArrayInputStream(zTxpData.getBytes());
		DataInputStream dis 		= new DataInputStream(bais);
		
		FORSSignature forssig = new FORSSignature();
		
		try {
			//Convert data
			forssig.readDataStream(dis);
		
			dis.close();
			bais.close();
			
		} catch (IOException e) {
			MinimaLogger.log(e);
		}
		
		return forssig;
	}
}
