package org.minima.utils;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;
import java.nio.charset.Charset;
import java.sql.SQLException;
import java.util.ArrayList;

import org.minima.database.archive.MySQLConnect;
import org.minima.objects.IBD;
import org.minima.objects.TxBlock;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.minima.NIOMessage;
import org.minima.system.network.rpc.HTTPServer;

public class ArchiveServer extends HTTPServer {

	public MySQLConnect mMySQL = null;
	
	public class ArchiveHandler implements Runnable{

		Socket mSocket;
		
		public ArchiveHandler(Socket zSocket) {
			mSocket = zSocket;
		}
		
		@Override
		public void run() {
			try {
				DataInputStream maindis = new DataInputStream(mSocket.getInputStream());
				DataOutputStream dos	= new DataOutputStream(mSocket.getOutputStream());
				
				//First read in the complete MiniData package
				MiniData data = MiniData.ReadFromStream(maindis);
				
				//Convert..
				ByteArrayInputStream bais 	= new ByteArrayInputStream(data.getBytes());
				DataInputStream dis 		= new DataInputStream(bais);
				
				//What Type..
				MiniByte type = MiniByte.ReadFromStream(dis);
//				MinimaLogger.log("Type : "+type, false);
				
				//What block are we starting from..
				MiniNumber firstblock 	= MiniNumber.ReadFromStream(dis);
				MiniNumber lastblock 	= firstblock.add(MiniNumber.THOUSAND);
				
				MinimaLogger.log("Received request first block : "+firstblock+" - "+lastblock, false);
		
				//Get the IBD
				IBD ibd = new IBD();
				
				//Is this the initial
				if(firstblock.isEqual(MiniNumber.MINUSONE)) {
					//Testing the connection
					//Don't add anything..
				}else {
					
					//Do we have a cascade - only check on first call..
					if(firstblock.isEqual(MiniNumber.ONE)) {
						ibd.setCascade(mMySQL.loadCascade());
					}
					
					//Get the blocks
					ArrayList<TxBlock> ibdblocks = ibd.getTxBlocks(); 
					
					//Load the block range..
					ArrayList<TxBlock> blocks = mMySQL.loadBlockRange(firstblock, lastblock);
					for(TxBlock block : blocks) {
						ibdblocks.add(block);
					}
				}
				
				//Create the network message
				MiniData netdata = NIOManager.createNIOMessage(NIOMessage.MSG_ARCHIVE_DATA, ibd);
				
				//And send it..
				netdata.writeDataStream(dos);
				
				//Flush
				dos.flush();
				
				//Close the streams
				dis.close();
				dos.close();
				
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
	}
	
	public ArchiveServer(int zPort, String zServer, String zDB, String zUser, String zPassowrd) throws SQLException {
		super(zPort,false);
		
//		mMySQL = new MySQLConnect("127.0.0.1:3306","mydatabase","myuser","myuser");
		mMySQL = new MySQLConnect(zServer,zDB,zUser,zPassowrd);
		mMySQL.init();
		
		start();
	}

	@Override
	public Runnable getSocketHandler(Socket zSocket) {
		return new ArchiveHandler(zSocket);
	}
	
	public static void main(String[] zArgs) throws SQLException {
		
		String mysqlhost 	 = null;
		String mysqldb	 	 = null;
		String mysqluser 	 = null;
		String mysqlpassword = null;
		
		int arglen 	= zArgs.length;
		if(arglen > 0) {
			int counter	=	0;
			while(counter<arglen) {
				String arg 	= zArgs[counter];
				counter++;
				
				if(arg.equals("-mysqlhost")) {
					mysqlhost = zArgs[counter++];
					
				}else if(arg.equals("-mysqldb")) {
					mysqldb   = zArgs[counter++];
				
				}else if(arg.equals("-mysqluser")) {
					mysqluser   = zArgs[counter++];
				
				}else if(arg.equals("-mysqlpassword")) {
					mysqlpassword  = zArgs[counter++];
				
				}else if(arg.equals("-help")) {
					
					System.out.println("Archive Server Help");
					System.out.println(" -mysqlhost      : The MySQL Host server");
					System.out.println(" -mysqldb        : The MySQL Database");
					System.out.println(" -mysqluser      : The MySQL User");
					System.out.println(" -mysqlpassword  : The MySQL password");
					System.out.println(" -help           : Print this help");
					
					System.exit(1);
					
				}else {
					System.out.println("Unknown parameter : "+arg);
					System.exit(1);
				}
			}
		}
		
		
		ArchiveServer server 	= new ArchiveServer(8080, mysqlhost, mysqldb, mysqluser, mysqlpassword);
		
		//Listen for input
		InputStreamReader is    = new InputStreamReader(System.in, Charset.forName("UTF-8"));
	    BufferedReader bis      = new BufferedReader(is);
	    
	    //Loop until finished..
	    while(true){
	        try {
	            //Get a line of input
	            String input = bis.readLine();
	            
	            //Check valid..
	            if(input!=null && !input.equals("")) {
	            	//trim it..
	            	input = input.trim();
	            	
	            	//Check the Command
	            	if(input.equals("quit")) {
	            		break;
	            	}else {
	            		MinimaLogger.log("Unknown command : "+input, false);
	            	}
	            }
	            
	        } catch (Exception ex) {
	            MinimaLogger.log(ex);
	        }
	    }
	    
	    //Cross the streams..
	    try {
	        bis.close();
	        is.close();
	    } catch (IOException ex) {
	    	MinimaLogger.log(""+ex, false);
	    }
		
	    //Stop the Server
	    server.stop();

	    MinimaLogger.log("Archive Server stopped", false);
	}
}
