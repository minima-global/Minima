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
	 * Automatically disables the auto mining
	 */
	public static final boolean MINIMA_ZERO_DIFF_BLK  = false;
	
	/**
	 * Speed in blocks per second
	 */
	public static final MiniNumber MINIMA_BLOCK_SPEED  = new MiniNumber("0.05");
	
	/**
	 * How deep before we think confirmed..
	 */
	public static final MiniNumber MINIMA_CONFIRM_DEPTH  = new MiniNumber("2");
	
	/**
	 * Depth before we cascade..
	 */
	public static final int MINIMA_CASCADE_START_DEPTH   = 256;
	
	/**
	 * Minimum number of blocks at each cascade level 
	 */
	public static final int MINIMA_MINUMUM_CASCADE_LEVEL_NODES  = 32;
	
	/**
	 * How Many Cascade Levels are there
	 */
	public static final int MINIMA_CASCADE_LEVELS  = 21;
	
	/**
	 * Ratio of Cascade tree vs Old tree allowed
	 */
	public static final String MINIMA_CASCADE_RATIO  = "0.9";
	
	
}
