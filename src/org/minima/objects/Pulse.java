package org.minima.objects;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;

public class Pulse implements Streamable {

	public static MiniNumber PULSE_VERSION = MiniNumber.ONE; 
	
	/**
	 * A list of the latest block hashes ( not all - just the last 60 minutes )
	 */
	ArrayList<MiniData> mBlockList;
	
	/**
	 * A recent piece of Work - currently not used..
	 */
	TxPoW mPulsePoW;
	
	public Pulse() {
		mBlockList = new ArrayList<>();
	}
	
	public void setBlockList(ArrayList<MiniData> zBlockList) {
		mBlockList = zBlockList;
	}
	
	public ArrayList<MiniData> getBlockList(){
		return mBlockList;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		//Write the Version
		PULSE_VERSION.writeDataStream(zOut);
		
		MiniNumber.WriteToStream(zOut, mBlockList.size());
		for(MiniData block : mBlockList) {
			block.writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mBlockList = new ArrayList<>();
		
		//Version may change in future
		MiniNumber version = MiniNumber.ReadFromStream(zIn);
		if(!version.isEqual(MiniNumber.ONE)) {
			MinimaLogger.log("UNKNOWN PULSE Version "+version.toString());
			return;
		}
		
		int len = MiniNumber.ReadFromStream(zIn).getAsInt();
		for(int i=0;i<len;i++) {
			MiniData block = MiniData.ReadFromStream(zIn);
			mBlockList.add(block);
		}
	}
	
	public static Pulse ReadFromStream(DataInputStream zIn) throws IOException {
		Pulse pp = new Pulse();
		pp.readDataStream(zIn);
		return pp;
	}

	/**
	 * Create a PULSE message to help peers keep in sync with you
	 */
	public static Pulse createPulse() {
		
		//New Pulse
		Pulse pulse = new Pulse();
		
		//Lock - don't want it changing half way through
		MinimaDB.getDB().readLock(true);
		
		try {
			
			//Get the Current Pulse
			ArrayList<MiniData> blocks = MinimaDB.getDB().getTxPoWTree().getPulseList();
			
			//Set it
			pulse.setBlockList(blocks);
			
		}catch(Exception exc) {
			MinimaLogger.log(exc);
		}
		
		//Unlock
		MinimaDB.getDB().readLock(false);
		
		return pulse;
	}
}
