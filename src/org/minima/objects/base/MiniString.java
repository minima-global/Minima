package org.minima.objects.base;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.charset.Charset;

import org.minima.utils.Streamable;

public class MiniString implements Streamable {

	/**
	 *  Minima Charset
	 */
	public static Charset MINIMA_CHARSET = Charset.forName("UTF-8");
	
	/**
	 * The UTF-8 String Data
	 */
	String mString;
	
	public MiniString(MiniString zString) {
		this(zString.getData());
	}
	
	public MiniString(byte[] zBytesData) {
		mString = new String(zBytesData,MINIMA_CHARSET);
	}
	
	public MiniString(String zString) {
		mString = new String(zString.getBytes(MINIMA_CHARSET));
	}
	
	public boolean isEqual(String zString) {
		return toString().equals(zString);
	}
	
	@Override
	public String toString() {
		return mString;
	}
	
	public byte[] getData() {
		return mString.getBytes(MINIMA_CHARSET);
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		MiniData strdata = new MiniData(getData());
		strdata.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		MiniData strdata = MiniData.ReadFromStream(zIn);
		mString = new String(strdata.getData(),MINIMA_CHARSET);
	}
	
	public static MiniString ReadFromStream(DataInputStream zIn) throws IOException{
		MiniString data = new MiniString("");
		data.readDataStream(zIn);
		return data;
	}
}

