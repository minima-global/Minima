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
	public String getFullHelp() {
		return "\nmaxextra\n"
				+ "\n"
				+ "Perform extra functions on Maxima.\n"
				+ "\n"
				+ "Set your static mls host. Your mls shares your changing contact address with your existing contacts to ensure you remain connected.\n"
				+ "\n"
				+ "To permanently allow incoming messages over Maxima from non-contacts, you can allow 'getaddress' requests for your current contact address.\n"
				+ "\n"
				+ "'getaddress' requests require your permanent maxaddress (MAX#yourpubkey#yourmls) and are requested via your static mls.\n"
				+ "\n"
				+ "To set this up, on your static mls node, your Maxima public key must be added as permanent.\n"
				+ "\n"
				+ "To prevent public users adding you as a contact, you can disable adding new contacts.\n"
				+ "\n"
				+ "Optionally, you can explicitly allow specific Maxima public keys who can add you as a contact.\n"
				+ "\n"
				+ "action: \n"
				+ "    staticmls : Set an unchanging Maxima Location Service (mls) host for yourself.\n"
				+ "    addpermanent : On your static mls node, add your Maxima public key to allow 'getaddress' requests from anyone.\n"
				+ "    removepermanent : On your static mls node, remove your Maxima public key to stop allowing 'getaddress' requests.\n"
				+ "    listpermanent : On your static mls node, list all public keys currently allowing public requests for their contact address.\n"
				+ "    clearpermanent : On your static mls node, remove ALL public keys currently allowing requests for their contact address.\n"
				+ "    getaddress : Request the current contact address of a permanently accessible user from their static mls host.\n"
				+ "    mlsinfo : List info about users using you as their mls and the public keys of their contacts.\n"
				+ "    allowallcontacts : If you have shared your permanent maxaddress, you can disable/enable users adding you as a contact.\n"
				+ "    addallowed : If 'allowallcontacts' is disabled, you can authorise specific users to add you as a contact. Stored in RAM.\n"
				+ "    listallowed : List all the public keys which are allowed to add you as a Maxima contact.\n"
				+ "    clearallowed : Remove the public keys of ALL users which are allowed to add you as a Maxima contact.\n"
				+ "\n"
				+ "publickey: (optional)\n"
				+ "    The Maxima public key of the user who wants to share their permanent maxaddress to be publicly contactable over Maxima.\n"
				+ "    Or the Maxima public key of a user who is allowed to add you as a contact.\n"
				+ "\n"
				+ "maxaddress: (optional)\n"
				+ "    Used with 'action:getaddress' to get the contact address of a user using their permanent maxaddress.\n"
				+ "    Their maxaddress must be in the format MAX#pubkey#staticmls, with their public key and static mls host address.\n "
				+ "\n"
				+ "enable: (optional)\n"
				+ "    true or false, use with 'action:allowallcontacts' to enable or disable all new contacts.\n"
				+ "\n"
				+ "host: (optional)\n"
				+ "    The 'p2pidentity' of a server node which is always online. \n"
				+ "    Use with 'action:staticmls' to set the host of your static mls.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "maxextra action:staticmls host:Mx...@34.190.784.3:9001\n"
				+ "\n"
				+ "maxextra action:staticmls host:clear\n"
				+ "\n"
				+ "maxextra action:addpermanent publickey:0x3081..\n"
				+ "\n"
				+ "maxextra action:removepermanent publickey:0x3081..\n"
				+ "\n"
				+ "maxextra action:listpermanent\n"
				+ "\n"
				+ "maxextra action:clearpermanent\n"
				+ "\n"
				+ "maxextra action:getaddress maxaddress:MAX#0x3081..#Mx..@34.190.784.3:9001\n"
				+ "\n"
				+ "maxextra action:mlsinfo\n"
				+ "\n"
				+ "maxextra action:allowallcontacts enable:false\n"
				+ "\n"
				+ "maxextra action:addallowed publickey:0x2451..\n"
				+ "\n"
				+ "maxextra action:listallowed\n"
				+ "\n"
				+ "maxextra action:clearallowed\n";
		}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","publickey","maxaddress","enable","host"}));
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
			
			details.put("added", pubkey.to0xString());
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
		
		}else if(action.equals("staticmls")) {
		
			String host = getParam("host");
			if(!host.equals("clear") && !checkValidMxAddress(host)) {
				throw new CommandException("Invalid MLS address : MUST be of type Mx..@host:port");
			}
			
			//Check is valid..
			if(host.equals("clear")) {
				max.setStaticMLS(false, "");
			}else {
				max.setStaticMLS(true, host);
			}
			
			details.put("staticmls", max.isStaticMLS());
			details.put("mls", max.getMLSHost());
			ret.put("response", details);
			
			//Refresh
			max.PostMessage(MaximaManager.MAXIMA_REFRESH);
			
		}else {
			throw new CommandException("Unknown action : "+action);
		}
		
		return ret;
	}

	public static JSONObject getMaxAddress(String zAddress) throws Exception {
		
		JSONObject details = new JSONObject();
		
		if(!zAddress.startsWith("MAX#")) {
			throw new CommandException("Permanent address MUST start with MAX# .. format MAX#PUBKEY#MLS_ADDRESS");
		}
		
		//Starts with MLS
		int pubkeystart = zAddress.indexOf("#");
		int pubkeyend   = zAddress.indexOf("#", pubkeystart+1);
			
		String pubkey 	  = zAddress.substring(pubkeystart+1, pubkeyend);
		String MLSaddress = zAddress.substring(pubkeyend+1);
		
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
			
		}else {
			//Now send it..
			details.put("success", true);
			details.put("mlsresponse", resp.toJSON());
		}
		
		return details;
	}
	
	public static MLSPacketGETResp sendMLSMaxPacket(String zHost, int zPort, MiniData zMaxMessage) throws IOException {
		
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
		Main.getInstance().getNIOManager().getTrafficListener().addWriteBytes("sendMLSMaxPacket",zMaxMessage.getLength());
	
		//Read the data
		MiniData resp 	= MiniData.ReadFromStream(dis);
		byte[] msgdata 	= resp.getBytes();
		
		//Tell the NIO
		Main.getInstance().getNIOManager().getTrafficListener().addReadBytes("sendMLSMaxPacket",resp.getLength());
		
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
	
	//Check an MLS address
	public static boolean checkValidMxAddress(String zMaximaAddress) {
		
		if(!zMaximaAddress.startsWith("Mx")) {
			return false;
		}
		
		int indexp 	= zMaximaAddress.indexOf("@");
		int index 	= zMaximaAddress.indexOf(":");
		if(indexp == -1 || index==-1) {
			return false;
		}
		
		return true;
	}
	
	@Override
	public Command getFunction() {
		return new maxextra();
	}

}
