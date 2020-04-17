package org.minima.objects.base;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.charset.Charset;

import org.minima.kissvm.Contract;
import org.minima.utils.Streamable;

public class MiniScript extends MiniData {
	
	private MiniScript() {
		super();
	}
	
	public MiniScript(String zString) {
		this(zString,true);
	}
	
	public MiniScript(String zString, boolean zContractClean) {
		super(initMiniScript(zString, zContractClean));
	}
	
	private static byte[] initMiniScript(String zString, boolean zContractClean) {
		if(zContractClean) {
			return Contract.cleanScript(zString).getBytes(Charset.forName("US-ASCII"));
		}
		
		return zString.getBytes(Charset.forName("US-ASCII"));
	}
	
	@Override
	public String toString() {
		return new String(getData(),Charset.forName("US-ASCII"));
	}
	
	@Override 
	public String to0xString() {
		return null;
	}
	
	public static MiniScript ReadFromStream(DataInputStream zIn){
		MiniScript data = new MiniScript();
		
		try {
			data.readDataStream(zIn);
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}
		
		return data;
	}
}

/*public class MiniScript implements Streamable{

	String mString;
	
	public MiniScript(String zString) {
		mString = new String(zString.getBytes(Charset.forName("US-ASCII")));
	}
	
	public MiniScript(MiniScript zString) {
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
	
	public static MiniScript ReadFromStream(DataInputStream zIn){
		MiniScript data = new MiniScript("");
		
		try {
			data.readDataStream(zIn);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
			return null;
		}
		
		return data;
	}
	
}*/

