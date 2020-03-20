package org.minima.objects.base;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.nio.charset.Charset;

import org.minima.utils.Streamable;

public class MiniString extends MiniData {
	
	public MiniString(String zString) {
		super(zString.getBytes(Charset.forName("US-ASCII")));
	}
	
	public MiniString(MiniString zString) {
		this(zString.to0xString());
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
	
}

