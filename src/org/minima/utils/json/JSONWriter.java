package org.minima.utils.json;

public class JSONWriter {

	StringBuffer mBuffer;
	
	public JSONWriter() {
		mBuffer = new StringBuffer();
	}
	
	public String getFullString() {
		return mBuffer.toString();
	}
	
	public void write(String zString) {
		mBuffer.append(zString);
	}
	
	public void write(int zChar) {
		mBuffer.append((char) zChar);
	}
	
	@Override
	public String toString() {
		return mBuffer.toString();
	}
}
