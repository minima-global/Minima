package org.minima.system.commands.base;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.objects.Greeting;
import org.minima.objects.IBD;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.network.minima.NIOClientInfo;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.minima.NIOMessage;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;

public class test extends Command {

	public test() {
		super("test","test Funxtion");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//reset ALL the default data
		Main.getInstance().archiveResetReady();
		
		//Send him a message..
		IBD ibd = sendArchiveReq("127.0.0.1", 9001, MiniNumber.ZERO);
		
		MinimaLogger.log("IBD Rec : "+ibd.getTxBlocks().size());
		
		//Post it..
		Main.getInstance().getTxPoWProcessor().postProcessArchiveIBD(ibd, "0x00",true);
		
		MinimaLogger.log("IBD Posted..");
		
		ret.put("response", "Test run..");
	
		return ret;
	}
	
	@Override
	public Command getFunction() {
		return new test();
	}

	/**
	 * A special PING message to  check a valid connection..
	 */
	public IBD sendArchiveReq(String zHost, int zPort, MiniNumber zStartBlock) {
		
		IBD ibd= null;
		
		try {
			
			//Create the Network Message
			MiniData msg = NIOManager.createNIOMessage(NIOMessage.MSG_ARCHIVE_REQ, zStartBlock);
			
			//Open the socket..
			Socket sock = new Socket();

			//3 seconds to connect
			sock.connect(new InetSocketAddress(zHost, zPort), 3000);
			
			//10 seconds to read
			sock.setSoTimeout(10000);
			
			//Create the streams..
			OutputStream out 		= sock.getOutputStream();
			DataOutputStream dos 	= new DataOutputStream(out);
			
			InputStream in			= sock.getInputStream();
			DataInputStream dis 	= new DataInputStream(in);
			
			//Write the data
			msg.writeDataStream(dos);
			dos.flush();
			
			//Tell the NIO
//			Main.getInstance().getNIOManager().getTrafficListener().addWriteBytes(msg.getLength());
			
			//Load the message
			MiniData resp = MiniData.ReadFromStream(dis);
			
			//Tell the NIO
//			Main.getInstance().getNIOManager().getTrafficListener().addReadBytes(resp.getLength());
			
			//Close the streams..
			dis.close();
			in.close();
			dos.close();
			out.close();
			
			//Convert
			ByteArrayInputStream bais 	= new ByteArrayInputStream(resp.getBytes());
			DataInputStream bdis 		= new DataInputStream(bais);

			//What Type..
			MiniByte type = MiniByte.ReadFromStream(bdis);
			
			//Load the IBD
			ibd = IBD.ReadFromStream(bdis);
			
			bdis.close();
			bais.close();
		
		}catch(Exception exc){
			MinimaLogger.log(exc);
			
			ibd= null;
		}
		
		return ibd;
	}
}
