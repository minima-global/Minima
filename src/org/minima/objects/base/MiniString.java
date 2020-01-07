package org.minima.objects.base;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.charset.Charset;

import org.minima.utils.Streamable;

/*public class MiniString extends MiniData {
	
	public MiniString(String zString) {
		super(zString.getBytes(Charset.forName("US-ASCII")));
	}
	
	@Override
	public String toString() {
		return new String(getData(),Charset.forName("US-ASCII"));
	}

	public static MiniString ReadFromStream(DataInputStream zIn){
		MiniString data = new MiniString("");
		
		try {
			data.readDataStream(zIn);
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}
		
		return data;
	}
	
}*/

public class MiniString implements Streamable{

	String mString;
	
	public MiniString(String zString) {
		mString = new String(zString.getBytes(Charset.forName("US-ASCII")));
	}
	
	public MiniString(MiniString zString) {
		mString = new String(zString.toString().getBytes(Charset.forName("US-ASCII")));
	}
	
	@Override
	public String toString() {
		return mString;
	}
	
	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		zOut.writeUTF(mString);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mString = zIn.readUTF();
	}
	
	public static MiniString ReadFromStream(DataInputStream zIn){
		MiniString data = new MiniString("");
		
		try {
			data.readDataStream(zIn);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return null;
		}
		
		return data;
	}
	
}
