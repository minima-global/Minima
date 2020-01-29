package org.minima.objects.base;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

import org.minima.utils.Streamable;

public class MiniKeyValue implements Streamable {

	/**
	 * A hash value for the key..
	 */
	MiniHash mKey;
	
	/**
	 * The data..
	 */
	MiniData mValue;
	
	/**
	 * A key value pair used in lots of areas
	 */
	public MiniKeyValue() {}
		
	public MiniKeyValue(MiniHash zKey, MiniData zValue) {
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
		mKey   = MiniHash.ReadFromStream(zIn);
		mValue = MiniData.ReadFromStream(zIn);
	}
	
}
