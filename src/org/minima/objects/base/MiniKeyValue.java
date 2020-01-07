package org.minima.objects.base;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.utils.Streamable;

public class MiniKeyValue implements Streamable {

	/**
	 * A hash value for the key..
	 */
	MiniData32 mKey;
	
	/**
	 * The data..
	 */
	MiniData mValue;
	
	/**
	 * A key value pair used in lots of areas
	 */
	public MiniKeyValue() {}
		
	public MiniKeyValue(MiniData32 zKey, MiniData zValue) {
		mKey   = zKey;
		mValue = zValue;
	}

	@Override
	public void writeDataStream(DataOutputStream zOut) throws IOException {
		mKey.writeDataStream(zOut);
		mValue.writeDataStream(zOut);
	}

	@Override
	public void readDataStream(DataInputStream zIn) throws IOException {
		mKey   = MiniData32.ReadFromStream(zIn);
		mValue = MiniData.ReadFromStream(zIn);
	}
	
}
