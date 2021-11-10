package org.minima.system.network.maxima;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;

public class MaximaMessage {

	/**
	 * Who is this message from
	 */
	public MiniString mFrom;
	
	/**
	 * Who is it to
	 */
	public MiniString mTo;
	
	/**
	 * The Encrypted Data
	 */
	public MiniData mData;
	
	
}
