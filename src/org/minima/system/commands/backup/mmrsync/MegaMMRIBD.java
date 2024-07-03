package org.minima.system.commands.backup.mmrsync;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.objects.CoinProof;
import org.minima.objects.IBD;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.utils.Streamable;

public class MegaMMRIBD implements Streamable {

	IBD mInitialIBD;
	
	ArrayList<CoinProof> mAllCoinProofs;
	
	String mPeersList = "";
	
	public MegaMMRIBD() {}
	
	public MegaMMRIBD(IBD zIBD, ArrayList<CoinProof> zAllCoinProofs) {
		mInitialIBD 	= zIBD;
		mAllCoinProofs 	= zAllCoinProofs;
	}

	public IBD getIBD() {
		return mInitialIBD;
	}
	
	public ArrayList<CoinProof> getAllCoinProofs(){
		return mAllCoinProofs;
	}
	
	public void setPeersList(String zPeersList){
		mPeersList = zPeersList;
	}
	
	public String getPeersList(){
		return mPeersList;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		
		//Version number
		MiniNumber.WriteToStream(zOut, 2);
		
		//Write out the IBD
		mInitialIBD.writeDataStream(zOut);
		
		//And now all the proofs..
		int len = mAllCoinProofs.size();
		MiniNumber.WriteToStream(zOut, len);
		for(CoinProof cp : mAllCoinProofs) {
			cp.writeDataStream(zOut);
		}
		
		//And now the Peers
		MiniString.WriteToStream(zOut, mPeersList);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		
		int version = MiniNumber.ReadFromStream(zIn).getAsInt();
		
		mInitialIBD = new IBD();
		mInitialIBD.readDataStream(zIn);
		
		mAllCoinProofs = new ArrayList<>();
		int len = MiniNumber.ReadFromStream(zIn).getAsInt();
		for(int i=0;i<len;i++) {
			mAllCoinProofs.add(CoinProof.ReadFromStream(zIn));
		}
		
		mPeersList = MiniString.ReadFromStream(zIn).toString();
	}
}
