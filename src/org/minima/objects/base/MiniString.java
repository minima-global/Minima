package org.minima.objects.base;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.charset.Charset;

import org.minima.utils.Streamable;

public class MiniString implements Streamable {

	/**
	 * The UTF-8 String Data
	 */
	String mString;
	
	public MiniString(MiniString zString) {
		this(zString.toString().trim());
	}
	
	public MiniString(String zString) {
		mString = new String(zString.trim().getBytes(Charset.forName("UTF-8")));
	}
	
	@Override
	public String toString() {
		return mString;
	}
	
	public byte[] getData() {
		return mString.getBytes();
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		zOut.writeUTF(mString);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mString = zIn.readUTF();
	}
	
	public static MiniString ReadFromStream(DataInputStream zIn) throws IOException{
		MiniString data = new MiniString("");
		data.readDataStream(zIn);
		return data;
	}
}

