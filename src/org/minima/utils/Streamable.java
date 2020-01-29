package org.minima.utils;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

public interface Streamable {

	/**
	 * Write in full to the Data stream
	 * @param zOut
	 * @throws IOException
	 */
	public void writeDataStream(DataOutputStream zOut) throws IOException;
	
	/**
	 * Read in full from the data stream
	 * @param zIn
	 * @throws IOException
	 */
	public void readDataStream(DataInputStream zIn) throws IOException;
	
	
}
