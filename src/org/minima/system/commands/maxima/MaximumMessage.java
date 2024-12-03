package org.minima.system.commands.maxima;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.Date;

import org.minima.objects.base.MiniData;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
import org.minima.utils.encrypt.SignVerify;
import org.minima.utils.json.JSONObject;

public class MaximumMessage implements Streamable {

	public MiniData mData;
	public MiniData mPublicKey;
	public MiniData mSignature;
	
	public MaximumMessage() {}
	
	public MaximumMessage(MiniData zData) {
		mData = zData;
	}

	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		ret.put("data", mData.to0xString());
		ret.put("publickey", mPublicKey.to0xString());
		ret.put("signature", mSignature.to0xString());
		
		return ret;
	}
	
	public MiniData getData() {
		return mData;
	}
	
	
	public void createSignature(MiniData zPublicKey, MiniData zPrivateKey) throws Exception {
		
		//Store the Public key
		mPublicKey 		= zPublicKey;
		
		//And make the Signature
		byte[] sigBytes = SignVerify.sign(zPrivateKey.getBytes(), mData.getBytes());
		mSignature 		= new MiniData(sigBytes);
	}
	
	/*public boolean checkSignature() {
		
	}*/
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mData.writeDataStream(zOut);
		mPublicKey.writeDataStream(zOut);
		mSignature.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mData 		= MiniData.ReadFromStream(zIn);
		mPublicKey 	= MiniData.ReadFromStream(zIn);
		mSignature 	= MiniData.ReadFromStream(zIn);
	}
	
	public static MaximumMessage ConvertMiniDataVersion(MiniData zData) {
		ByteArrayInputStream bais 	= new ByteArrayInputStream(zData.getBytes());
		DataInputStream dis 		= new DataInputStream(bais);
		
		MaximumMessage mm = new MaximumMessage();
		
		try {
			mm.readDataStream(dis);
			dis.close();
			bais.close();
			
		} catch (IOException e) {
			MinimaLogger.log(e);
		}
		
		return mm;
	}
	
	public static MaximumMessage ReadFromStream(DataInputStream zIn) throws IOException {
		MaximumMessage mm = new MaximumMessage();
		mm.readDataStream(zIn);
		return mm;
	}
	
}
