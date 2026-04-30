package org.minima.utils.sphincs;

import org.minima.database.mmr.MMRData;
import org.minima.objects.keys.Signature;
import org.minima.utils.sphincs.FORS.FORSSignature;

public class SPHINCSSignature {

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
}
