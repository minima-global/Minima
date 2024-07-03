package org.minima.system.commands.backup.mmrsync;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.database.mmr.MegaMMR;
import org.minima.objects.IBD;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Streamable;

public class MegaMMRBackup implements Streamable {

	//The Mega MMR
	MegaMMR mMegaMMR;
	
	//The Current Valid IDB that goes with that MMR.. Cascade + tree
	IBD mIBD;
	
	public MegaMMRBackup() {}
	
	public MegaMMRBackup(MegaMMR zMMR, IBD zIDB) {
		mMegaMMR = zMMR;
		mIBD 	 = zIDB;
	}
	
	public MegaMMR getMegaMMR() {
		return mMegaMMR;
	}
	
	public IBD getIBD() {
		return mIBD;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		
		//Version
		MiniNumber.WriteToStream(zOut, 1);
		
		//Now the Mega MMR
		mMegaMMR.writeDataStream(zOut);
		
		//And the IBD
		mIBD.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		
		int version = MiniNumber.ReadFromStream(zIn).getAsInt();
		
		mMegaMMR = new MegaMMR();
		mMegaMMR.readDataStream(zIn);
		
		mIBD = IBD.ReadFromStream(zIn);
	}

}
