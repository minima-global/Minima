package org.minima.system.commands.backup.mmrsync;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONObject;

public class MegaMMRSyncData implements Streamable {

	ArrayList<MiniData> mAllAddresses;
	ArrayList<MiniData> mAllPublicKeys;
	
	public MegaMMRSyncData() {}
	
	public MegaMMRSyncData(ArrayList<MiniData> zAllAddresses, ArrayList<MiniData> zAllPublicKeys) {
		mAllAddresses 	= zAllAddresses;
		mAllPublicKeys 	= zAllPublicKeys;
	}
	
	public ArrayList<MiniData> getAllAddresses(){
		return mAllAddresses;
	}
	
	public ArrayList<MiniData> getAllPublicKeys(){
		return mAllPublicKeys;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		
		MiniNumber.WriteToStream(zOut, 1);
		
		int len = mAllAddresses.size();
		MiniNumber.WriteToStream(zOut, len);
		for(MiniData cp : mAllAddresses) {
			cp.writeDataStream(zOut);
		}
		
		len = mAllPublicKeys.size();
		MiniNumber.WriteToStream(zOut, len);
		for(MiniData cp : mAllPublicKeys) {
			cp.writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		
		int version = MiniNumber.ReadFromStream(zIn).getAsInt();
		
		mAllAddresses = new ArrayList<>();
		int len = MiniNumber.ReadFromStream(zIn).getAsInt();
		for(int i=0;i<len;i++) {
			mAllAddresses.add(MiniData.ReadFromStream(zIn));
		}
		
		mAllPublicKeys = new ArrayList<>();
		len = MiniNumber.ReadFromStream(zIn).getAsInt();
		for(int i=0;i<len;i++) {
			mAllPublicKeys.add(MiniData.ReadFromStream(zIn));
		}
	}
}
