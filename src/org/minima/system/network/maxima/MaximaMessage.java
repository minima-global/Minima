package org.minima.system.network.maxima;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;

public class MaximaMessage {

	/**
	 * The Date in milliseconds from Jan 1 1970
	 */
	public MiniNumber mTimeMilli;
	
	/**
	 * Who is this message from
	 */
	public MiniString mFromAddress;
	
	/**
	 * What is the FROM Public Key (For encrypting replies)
	 * 
	 * You can check the HASH pub key is correct ion the mFromAddress
	 */
	public MiniString mFromPublicKey;
	
	/**
	 * Who is it to
	 * 
	 * HASH(PUBKEY)@HOST:APPLICATION
	 * 
	 */
	public MiniString mTo;
	
	/**
	 * The Data
	 */
	public MiniData mData;
	
	
	public MaximaMessage() {}
	
	
}
