package org.minima.utils;

import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;

public interface Streamable {

	public void writeDataStream(DataOutputStream zOut) throws IOException;
	public void readDataStream(DataInputStream zIn) throws IOException;
}
