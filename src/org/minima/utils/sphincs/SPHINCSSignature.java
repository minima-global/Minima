package org.minima.utils.sphincs;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniData;
import org.minima.objects.keys.Signature;
import org.minima.objects.mmr.MMRData;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;
import org.minima.utils.sphincs.FORS.FORSSignature;

public class SPHINCSSignature implements Streamable {

	/**
	 * The WOTS Signature
	 */
	Signature mMinimaSignature;
	
	/**
	 * The Root of the FORS tree..
	 */
	MMRData mFORSRoot;
	
	/**
	 * The FORS Signature
	 * 
	 * The root of the FORS tree is signed by the WOTS
	 */
	FORSSignature mFORSSignature;
	
	public SPHINCSSignature() {}
	
	public SPHINCSSignature(Signature zMinimaSig, MMRData zFORSRoot, FORSSignature zFORSSig) {
		mMinimaSignature 	= zMinimaSig;
		mFORSRoot			= zFORSRoot;
		mFORSSignature		= zFORSSig;
	}
	
	public Signature getWOTSSignature() {
		return mMinimaSignature;
	}
	
	public MMRData getFORSRoot() {
		return mFORSRoot;
	}
	
	public FORSSignature getFORSSignature() {
		return mFORSSignature;
	}
	
	public JSONObject toJSON() {
		JSONObject json = new JSONObject();
		
		json.put("WOTSsignature", mMinimaSignature.toJSON());
		json.put("FORSroot", mFORSRoot.toJSON());
		json.put("FORSsignature", mFORSSignature.toJSON());
		
		return json;
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mMinimaSignature.writeDataStream(zOut);
		mFORSRoot.writeDataStream(zOut);
		mFORSSignature.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mMinimaSignature = Signature.ReadFromStream(zIn);
		mFORSRoot		 = MMRData.ReadFromStream(zIn);
		
		mFORSSignature	 = new FORSSignature();
		mFORSSignature.readDataStream(zIn);
	}
	
	/**
	 * Convert a MiniData version into a SPHINCSSignature
	 */
	public static SPHINCSSignature convertMiniDataVersion(MiniData zTxpData) {
		ByteArrayInputStream bais 	= new ByteArrayInputStream(zTxpData.getBytes());
		DataInputStream dis 		= new DataInputStream(bais);
		
		SPHINCSSignature sig = new SPHINCSSignature();
		
		try {
			//Convert data
			sig.readDataStream(dis);
		
			dis.close();
			bais.close();
			
		} catch (IOException e) {
			MinimaLogger.log(e);
		}
		
		return sig;
	}
}
