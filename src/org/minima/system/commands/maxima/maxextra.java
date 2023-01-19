package org.minima.system.commands.maxima;

import java.io.ByteArrayInputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.Hashtable;

import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.network.maxima.MaxMsgHandler;
import org.minima.system.network.maxima.MaximaManager;
import org.minima.system.network.maxima.mls.MLSPacketGETReq;
import org.minima.system.network.maxima.mls.MLSPacketGETResp;
import org.minima.system.network.maxima.mls.MLSPacketSET;
import org.minima.system.network.maxima.mls.MLSService;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class maxextra extends Command {

	public maxextra() {
		super("maxextra","Perform extra functions on Maxima");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","publickey","maxaddress","enable"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//Get Maxima
		MaximaManager max = Main.getInstance().getMaxima();
		
		//What action 
		String action = getParam("action");
		
		JSONObject details = new JSONObject();
		
		if(action.equals("addpermanent")) {
			
			MiniData pubkdat = getDataParam("publickey");
			String publickey = pubkdat.to0xString();
			max.addPermanentMaxima(publickey);
			ret.put("response", "Added Permanent Maxima ID : "+publickey);
		
		}else if(action.equals("removepermanent")) {
			MiniData pubkdat = getDataParam("publickey");
			String publickey = pubkdat.to0xString();
			max.removePermanentMaxima(publickey);
			ret.put("response", "Removed Permanent Maxima ID : "+publickey);
		
		}else if(action.equals("clearpermanent")) {
			max.clearPermanentMaxima();
			ret.put("response", "Cleared ALL Permanent Maxima ID");
		
		}else if(action.equals("listpermanent")) {
			ArrayList<String> allperm = max.getAllPermanent();
			JSONArray arr = new JSONArray();
			for(String all : allperm) {
				arr.add(all);
			}
			ret.put("response", arr);
		
		}else if(action.equals("getaddress")) {
			
			//Get the contact permanent address
			String address = getParam("maxaddress");
			
			if(!address.startsWith("MAX#")) {
				throw new CommandException("Permanent address MUST start with MAX# .. format MAX#PUBKEY#MLS_ADDRESS");
			}
			
			//Starts with MLS
			int pubkeystart = address.indexOf("#");
			int pubkeyend   = address.indexOf("#", pubkeystart+1);
				
			String pubkey 	  = address.substring(pubkeystart+1, pubkeyend);
			String MLSaddress = address.substring(pubkeyend+1);
			
			//Create a Get req
			MLSPacketGETReq req = new MLSPacketGETReq(pubkey, MiniData.getRandomData(32).to0xString());
			MiniData reqdata 	= MiniData.getMiniDataVersion(req);
			
			Message getreq 	= maxima.createSendMessage(MLSaddress,MaximaManager.MAXIMA_MLS_GETAPP,reqdata);
			
			//Who to..
			String host 	= getreq.getString("tohost");
			int port		= getreq.getInteger("toport");
			
			//Create the packet
			MiniData maxpacket = MaxMsgHandler.constructMaximaData(getreq);
			if(maxpacket == null) {
				throw new CommandException("Could not build Maxima message in time..");
			}
			
			//Now send that..
			MLSPacketGETResp resp = sendMLSMaxPacket(host, port, maxpacket);
			
			//Create response
			details.put("publickey", pubkey);
			details.put("mls", MLSaddress);
			
			if(resp == null) {
				//Now send it..
				details.put("success", false);
				details.put("mlsresponse", "{}");
				ret.put("response", details);
				
			}else {
				//Now send it..
				details.put("success", true);
				details.put("mlsresponse", resp.toJSON());
				ret.put("response", details);
			}
			
		}else if(action.equals("mlsinfo")) {
			
			JSONArray alldets = new JSONArray();
			
			MLSService serv = max.getMLSService();
			
			Hashtable<String, MLSPacketSET> allmls = serv.getCompleteMLS();
			Enumeration<String> pubkeys = allmls.keys();
			while(pubkeys.hasMoreElements()) {
				
				String pubkey 		= pubkeys.nextElement();
				MLSPacketSET mls 	= allmls.get(pubkey);
				
				JSONObject entry = new JSONObject();
				entry.put("publickey", pubkey);
				entry.put("mlsallowed", mls.toJSON());
				alldets.add(entry);
			}
			
			details.put("mlsservice", alldets);
			ret.put("response", details);
		
		}else if(action.equals("clearallowed")) {
			
			max.getContactsManager().clearAllowedContactRequest();
			details.put("allowed", new JSONArray());
			ret.put("response", details);
			
		}else if(action.equals("addallowed")) {
			
			MiniData pubkey = getDataParam("publickey");
			max.getContactsManager().addValidContactRequest(pubkey.to0xString());
			
			details.put("added", pubkey);
			ret.put("response", details);
			
		}else if(action.equals("listallowed")) {
			
			ArrayList<String> allowed = max.getContactsManager().getAllowed();
			JSONArray arr = new JSONArray();
			for(String all : allowed) {
				arr.add(all);
			}
			details.put("allowed", allowed);
			ret.put("response", details);
			
		}else if(action.equals("allowallcontacts")) {
			
			boolean enable = getBooleanParam("enable");
			
			max.getContactsManager().setAllowContact(enable);
			
			details.put("allowallcontacts", enable);
			ret.put("response", details);
		
		}else {
			throw new CommandException("Unknown action : "+action);
		}
		
		return ret;
	}

	public MLSPacketGETResp sendMLSMaxPacket(String zHost, int zPort, MiniData zMaxMessage) throws IOException {
		
		//Open the socket..
		Socket sock = new Socket();

		//20 seconds to connect
		sock.connect(new InetSocketAddress(zHost, zPort), 20000);
		
		//20 seconds to read
		sock.setSoTimeout(20000);
		
		//Create the streams..
		OutputStream out 		= sock.getOutputStream();
		DataOutputStream dos 	= new DataOutputStream(out);
		
		InputStream in			= sock.getInputStream();
		DataInputStream dis 	= new DataInputStream(in);
		
		//Write the data
		zMaxMessage.writeDataStream(dos);
		dos.flush();
		
		//Tell the NIO
		Main.getInstance().getNIOManager().getTrafficListener().addWriteBytes(zMaxMessage.getLength());
	
		//Read the data
		MiniData resp 	= MiniData.ReadFromStream(dis);
		byte[] msgdata 	= resp.getBytes();
		
		//Tell the NIO
		Main.getInstance().getNIOManager().getTrafficListener().addReadBytes(resp.getLength());
		
		ByteArrayInputStream bais 	= new ByteArrayInputStream(msgdata);
		DataInputStream respdis 	= new DataInputStream(bais);
		
		//What Type..
		MiniByte type = MiniByte.ReadFromStream(respdis);
		MiniData data = MiniData.ReadFromStream(respdis);
		
		//Convert
		MLSPacketGETResp mls = MLSPacketGETResp.convertMiniDataVersion(data);
	
		//Close the streams..
		dis.close();
		in.close();
		dos.close();
		out.close();
		
		return mls;
	}
	
	@Override
	public Command getFunction() {
		return new maxextra();
	}

}
