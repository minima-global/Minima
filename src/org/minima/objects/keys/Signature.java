package org.minima.objects.keys;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.util.ArrayList;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.utils.MinimaLogger;
import org.minima.utils.Streamable;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class Signature implements Streamable {

	/**
	 * A list of Signature trees that sign the root of the child tree
	 */
	ArrayList<SignatureProof> mSignatures = new ArrayList<>();
	
	public Signature() {}

	public void addSignatureProof(SignatureProof zSignature) {
		mSignatures.add(zSignature);
	}
	
	public ArrayList<SignatureProof> getAllSignatureProofs(){
		return mSignatures;
	}
	
	public MiniData getRootPublicKey() {
		return mSignatures.get(0).getRootPublicKey();
	}
	
	public JSONObject toJSON() {
		JSONObject json = new JSONObject();
		
		JSONArray sigs = new JSONArray();
		for(SignatureProof sig : mSignatures) {
			sigs.add(sig.toJSON());
		}
		json.put("signatures", sigs);
		
		return json;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		MiniNumber.WriteToStream(zOut, mSignatures.size());
		for(SignatureProof sig : mSignatures) {
			sig.writeDataStream(zOut);
		}
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mSignatures = new ArrayList<>();
		
		int len = MiniNumber.ReadFromStream(zIn).getAsInt();
		for(int i=0;i<len;i++) {
			SignatureProof sig = SignatureProof.ReadFromStream(zIn);
			mSignatures.add(sig);
		}
	}
	
	public static Signature ReadFromStream(DataInputStream zIn) throws IOException {
		Signature sigtree = new Signature();
		sigtree.readDataStream(zIn);
		return sigtree;
	}
	
	/**
	 * Convert a MiniData version into a TxPoW
	 */
	public static Signature convertMiniDataVersion(MiniData zTxpData) {
		ByteArrayInputStream bais 	= new ByteArrayInputStream(zTxpData.getBytes());
		DataInputStream dis 		= new DataInputStream(bais);
		
		Signature signature = null;
		
		try {
			//Convert data into a TxPoW
			signature = Signature.ReadFromStream(dis);
		
			dis.close();
			bais.close();
			
		} catch (IOException e) {
			MinimaLogger.log(e);
		}
		
		return signature;
	}
}
