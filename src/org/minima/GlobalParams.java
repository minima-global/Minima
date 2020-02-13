package org.minima;

import org.minima.objects.base.MiniNumber;

public class GlobalParams {

	/**
	 * Number of seconds before sending a pulse message
	 */
	public static final int USER_PULSE_FREQ      = 10 * 60;
	
	/**
	 * Just create a block every transaction. Useful when not mining 
	 * and just want a block every single transaction to debug.
	 */
	public static final boolean MINIMA_ZERO_DIFF_BLK  = false;
	
	/**
	 * Speed in blocks per second
	 * 
	 * 0.1 = 1 block every 10 seconds
	 * 0.05 = 20 second block time
	 */
	public static final double MINIMA_BLOCK_SPEED  = 0.1;
	
	/**
	 * How deep before we think confirmed..
	 */
	public static final MiniNumber MINIMA_CONFIRM_DEPTH  = new MiniNumber("3");
	
	/**
	 * Depth before we cascade..
	 */
	public static final int MINIMA_CASCADE_DEPTH     = 256;
	
}
