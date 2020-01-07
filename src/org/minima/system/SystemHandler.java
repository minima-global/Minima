package org.minima.system;

import org.minima.utils.messages.MessageProcessor;

public abstract class SystemHandler extends MessageProcessor {

	/**
	 * Hook to the Main handler
	 */
	private Main mMain;
	
	public SystemHandler(Main zMain) {
		this(zMain, "");
	}
	
	public SystemHandler(Main zMain, String zName) {
		super(zName);
		mMain = zMain;
	}
	
	public Main getMainHandler() {
		return mMain;
	}
}
