package org.minima.system.network.maxima.message;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;

public class MaximaErrorMsg implements Streamable {

	MiniString mError;
	
	private MaximaErrorMsg() {}
	
	public MaximaErrorMsg(String zError) {
		mError = new MiniString(zError);		
	}
	
	public String getError() {
		return mError.toString();
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mError.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mError = MiniString.ReadFromStream(zIn);
	}
	
	public static MaximaErrorMsg ReadFromStream(DataInputStream zIn) throws IOException {
		MaximaErrorMsg mls = new MaximaErrorMsg();
		mls.readDataStream(zIn);
		return mls;
	}
	
	/**
	 * Convert a MiniData version into a MaximaErrorMsg
	 */
	public static MaximaErrorMsg convertMiniDataVersion(MiniData zTxpData) {
		ByteArrayInputStream bais 	= new ByteArrayInputStream(zTxpData.getBytes());
		DataInputStream dis 		= new DataInputStream(bais);
		
		MaximaErrorMsg mls = null;
		
		try {
			mls = MaximaErrorMsg.ReadFromStream(dis);
		
			dis.close();
			bais.close();
			
		} catch (IOException e) {
			MinimaLogger.log(e);
		}
		
		return mls;
	}
}
