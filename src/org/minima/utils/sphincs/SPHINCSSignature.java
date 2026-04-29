package org.minima.utils.sphincs;

import org.minima.objects.keys.Signature;

public class SPHINCSSignature {

	/**
	 * The WOTS Signature
	 */
	Signature mMinimaSignature;
	
	/**
	 * The FORS Signature
	 * 
	 * The root of the FORS tree is signed by the WOTS
	 */
	FORSSignature mFORSSignature;
	
	public SPHINCSSignature(Signature zMinimaSig, FORSSignature zFORSSig) {
		mMinimaSignature 	= zMinimaSig;
		mFORSSignature		= zFORSSig;
	}
	
	
}
