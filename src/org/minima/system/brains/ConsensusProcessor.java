package org.minima.system.brains;

import org.minima.database.MinimaDB;
import org.minima.system.Main;
import org.minima.system.network.NetworkHandler;
import org.minima.utils.messages.Message;

public class ConsensusProcessor {

	/**
	 * The Main Database
	 */
	MinimaDB mDB;
	
	/**
	 * The Actual Single Thread Consensus Critical Handler
	 */
	private ConsensusHandler mHandler;
	
	
	public ConsensusProcessor(MinimaDB zDB, ConsensusHandler zHandler) {
		mDB      = zDB;
		mHandler = zHandler;
	}
	
	protected MinimaDB getMainDB() {
		return mDB;
	}
	
	protected ConsensusHandler getConsensusHandler() {
		return mHandler;
	}
	
	protected NetworkHandler getNetworkHandler() {
		return Main.getMainHandler().getNetworkHandler();
	}
}
