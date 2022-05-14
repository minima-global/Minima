package org.minima.system.network.maxima;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;

public class MaximaCTRLMessage implements Streamable {

	public static final MiniByte MAXIMACTRL_TYPE_ID = new MiniByte(0);
	
	MiniByte mTye;
	
	MiniData mData;
	
	private MaximaCTRLMessage() {}
	
	public MaximaCTRLMessage(MiniByte zType) {
		mTye = zType;
	}
	
	public MiniByte getType() {
		return mTye;
	}
	
	public void setData(MiniData zData) {
		mData = zData;
	}
	
	public MiniData getData() {
		return mData;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mTye.writeDataStream(zOut);
		mData.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mTye 	= MiniByte.ReadFromStream(zIn);
		mData	= MiniData.ReadFromStream(zIn);
	}

	public static MaximaCTRLMessage ReadFromStream(DataInputStream zIn) throws IOException {
		MaximaCTRLMessage msg = new MaximaCTRLMessage();
		msg.readDataStream(zIn);
		return msg;
	}
	
	/**
	 * Convert a MiniData version into a Message
	 */
	public static MaximaCTRLMessage convertMiniDataVersion(MiniData zMsgData) {
		ByteArrayInputStream bais 	= new ByteArrayInputStream(zMsgData.getBytes());
		DataInputStream dis 		= new DataInputStream(bais);
		
		MaximaCTRLMessage msg = null;
		
		try {
			//Convert data
			msg = MaximaCTRLMessage.ReadFromStream(dis);
		
			dis.close();
			bais.close();
			
		} catch (IOException e) {
			MinimaLogger.log(e);
		}
		
		return msg;
	}
}
