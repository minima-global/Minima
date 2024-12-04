package org.minima.system.commands.maxima;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.Date;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
import org.minima.utils.encrypt.SignVerify;
import org.minima.utils.json.JSONObject;

public class MaximumMessage implements Streamable {

	public MiniNumber 	mVersion 	= MiniNumber.ONE;
	public MiniData 	mData	 	= MiniData.ZERO_TXPOWID;
	public MiniData 	mPublicKey 	= MiniData.ZERO_TXPOWID;;
	public MiniData 	mSignature 	= MiniData.ZERO_TXPOWID;;
	
	public MaximumMessage() {}
	
	public MaximumMessage(MiniData zData) {
		mData = zData;
	}

	public JSONObject toJSON() {
		JSONObject ret = new JSONObject();
		
		ret.put("data", mData.to0xString());
		ret.put("publickey", mPublicKey.to0xString());
		ret.put("signature", mSignature.to0xString());
		ret.put("valid", checkSignature());
		
		return ret;
	}
	
	public MiniData getData() {
		return mData;
	}

	public MiniData getPublicKey() {
		return mPublicKey;
	}
	
	public MiniData getSignature() {
		return mSignature;
	}
	
	public void createSignature(MiniData zPublicKey, MiniData zPrivateKey) throws Exception {
		
		//Store the Public key
		mPublicKey 		= zPublicKey;
		
		//And make the Signature
		byte[] sigBytes = SignVerify.sign(zPrivateKey.getBytes(), mData.getBytes());
		mSignature 		= new MiniData(sigBytes);
	}
	
	public boolean checkSignature() {	
		
		try {
			return SignVerify.verify(mPublicKey.getBytes(), mData.getBytes(), mSignature.getBytes());
		} catch (Exception e) {
			
		}
		
		return false;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mVersion.writeDataStream(zOut);
		mData.writeDataStream(zOut);
		mPublicKey.writeDataStream(zOut);
		mSignature.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mVersion	= MiniNumber.ReadFromStream(zIn);
		mData 		= MiniData.ReadFromStream(zIn);
		mPublicKey 	= MiniData.ReadFromStream(zIn);
		mSignature 	= MiniData.ReadFromStream(zIn);
	}
	
	public MiniData createMiniDataVersion() throws IOException {
		ByteArrayOutputStream baos 	= new ByteArrayOutputStream();
		DataOutputStream dos 		= new DataOutputStream(baos);
		
		writeDataStream(dos);
		dos.flush();
		
		dos.close();
		baos.close();
		
		return new MiniData(baos.toByteArray());
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
