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

import org.minima.database.MinimaDB;
import org.minima.database.maxima.MaximaContact;
import org.minima.database.maxima.MaximaDB;
import org.minima.database.maxima.MaximaHost;
import org.minima.objects.Address;
import org.minima.objects.base.MiniByte;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.network.maxima.MaxMsgHandler;
import org.minima.system.network.maxima.MaximaManager;
import org.minima.system.network.maxima.message.MaximaMessage;
import org.minima.system.network.maxima.mls.MLSPacketGETReq;
import org.minima.system.network.maxima.mls.MLSPacketGETResp;
import org.minima.system.params.GeneralParams;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class maxima extends Command {

	public maxima() {
		super("maxima","[action:info|setname|hosts|send|refresh] (name:) (id:)|(to:)|(publickey:) (application:) (data:) (logs:true|false) (poll:true|false) - Check your Maxima details, send a message / data, enable logs");
	}
	
	@Override
	public String getFullHelp() {
		return "\nmaxima\n"
				+ "\n"
				+ "Check your Maxima details, send a message / data, enable logs.\n"
				+ "\n"
				+ "Maxima is an information transport layer running on top of Minima.\n"
				+ "\n"
				+ "action:\n"
				+ "    info : Show your Maxima details - name, publickey, staticmls, mls, local identity and contact address.\n"
				+ "    setname : Set your Maxima name so your contacts recognise you. Default 'noname'.\n"
				+ "    hosts : List your Maxima hosts and see their Maxima public key, contact address, last seen time and if you are connected.\n"
				+ "    send : Send a message to a contact. Must specify 'id|to|publickey', 'application' and 'data' parameters.\n"
				+ "    refresh : Refresh your contacts by sending them a network message. \n"
				+ "\n"
				+ "name: (optional)\n"
				+ "    Set your name. Use with 'action:setname'.\n"
				+ "\n"
				+ "id|to|publickey: (optional)\n"
				+ "    The id, contact address or public key of the recipient of the message. Use with 'action:send'.\n"
				+ "\n"
				+ "application: (optional)\n"
				+ "    The ip:port to send a message to. Use with 'action:send'.\n"
				+ "\n"
				+ "data: (optional)\n"
				+ "    The data to send. Can be HEX or a JSON object. Use with 'action:send'.\n"
				+ "\n"
				+ "poll: (optional)\n"
				+ "    true or false, true will poll the send action until successful. Use with 'action:send'.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "maxima action:info\n"
				+ "\n"
				+ "maxima action:setname name:myname\n"
				+ "\n"
				+ "maxima action:hosts\n"
				+ "\n"
				+ "maxima action:send id:1 application:ip:port data:0xFED5..\n"
				+ "\n"
				+ "maxima action:send to:MxG18H.. application:ip:port data:0xFED5..\n"
				+ "\n"
				+ "maxima action:send publickey:0xCD34.. application:ip:port data:0xFED5.. poll:true\n"
				+ "\n"
				+ "maxima action:refresh\n"
				+ "\n"
				+ "maxima logs:true\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","name","id","to",
				"publickey","application","data","poll","host","permanent"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String func = getParam("action", "info");
		
		MaximaManager max = Main.getInstance().getMaxima();
		if(max == null || !max.isInited()) {
			ret.put("status", false);
			ret.put("message", "Maxima still starting up..");
			return ret;
		}
		
		MaximaDB maxdb = MinimaDB.getDB().getMaximaDB();
		
		JSONObject details = new JSONObject();
		
		if(func.equals("info")) {
			
			//Your local IP address
			String fullhost = GeneralParams.MINIMA_HOST+":"+GeneralParams.MINIMA_PORT;
			
			//Show details
			details.put("logs", GeneralParams.MAXIMA_LOGS);
			details.put("name", MinimaDB.getDB().getUserDB().getMaximaName());
			details.put("publickey", max.getPublicKey().to0xString());
			details.put("staticmls", max.isStaticMLS());
			details.put("mls", max.getMLSHost());
			details.put("localidentity", max.getLocalMaximaAddress(false));
			details.put("p2pidentity", max.getLocalMaximaAddress(true));
			details.put("contact", max.getRandomMaximaAddress());
			
			ret.put("response", details);
		
		}else if(func.equals("staticmls")) {
		
			String host = getParam("host");
			if(!host.equals("clear") && !checkAddress(host)) {
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
			
		}else if(func.equals("setname")) {
			
			String name = getParam("name");
			name = name.replace("\"", "");
			name = name.replace("'", "");
			name = name.replace(";", "");
			
			MinimaDB.getDB().getUserDB().setMaximaName(name);
			
			details.put("name", name);
			
			ret.put("response", details);
			
			//Refresh
			max.PostMessage(MaximaManager.MAXIMA_REFRESH);
			
		}else if(func.equals("hosts")) {
			
			//Add ALL Hosts
			ArrayList<MaximaHost> hosts = maxdb.getAllHosts();
			JSONArray allhosts = new JSONArray();
			for(MaximaHost host : hosts) {
				allhosts.add(host.toJSON());
			}
			details.put("hosts", allhosts);
			
			ret.put("response", details);
			
		}else if(func.equals("refresh")) {
			
			//Refresh all the MLS servers
			max.PostMessage(MaximaManager.MAXIMA_CHECK_MLS);
			
			//Send a contact update message to all contacts - after the MLS Update
			max.PostMessage(MaximaManager.MAXIMA_REFRESH_TIMER);
			
			ret.put("response", "Update message sent to all contacts");
			
		}else if(func.equals("new")) {
			
			throw new CommandException("Not Supported yet..");
			
//			//Create a new Maxima Identity..
//			max.createMaximaKeys();
//			
//			//Show details
//			String ident = max.getMaximaIdentity();  
//			details.put("identity", ident);
//			ret.put("response", details);
	
		}else if(func.equals("addpermanent")) {
			MiniData pubkdat = getDataParam("publickey");
			String publickey = pubkdat.to0xString();
			max.addPermanentMaxima(publickey);
			ret.put("response", "Added Permanent Maxima ID : "+publickey);
		
		}else if(func.equals("removepermanent")) {
			MiniData pubkdat = getDataParam("publickey");
			String publickey = pubkdat.to0xString();
			max.removePermanentMaxima(publickey);
			ret.put("response", "Removed Permanent Maxima ID : "+publickey);
		
		}else if(func.equals("clearpermanent")) {
			max.clearPermanentMaxima();
			ret.put("response", "Cleared ALL Permanent Maxima ID");
		
		}else if(func.equals("listpermanent")) {
			ArrayList<String> allperm = max.getAllPermanent();
			JSONArray arr = new JSONArray();
			for(String all : allperm) {
				arr.add(all);
			}
			ret.put("response", arr);
		
		}else if(func.equals("getmlsaddress")) {
			
			//Get the contact permanent address
			String address 	= getParam("permanent");
			
			if(!address.startsWith("MAXIMA#")) {
				throw new CommandException("Permanent address MUST start with MAXIMA# .. format MAXIMA#PUBKEY#MLS_ADDRESS");
			}
			
			//Starts with MLS
			int pubkeystart = address.indexOf("#");
			int pubkeyend   = address.indexOf("#", pubkeystart+1);
				
			String pubkey 	  = address.substring(pubkeystart+1, pubkeyend);
			String MLSaddress = address.substring(pubkeyend+1);
			
			//Create a Get req
			MLSPacketGETReq req = new MLSPacketGETReq(pubkey, "0x00");
			MiniData reqdata 	= MiniData.getMiniDataVersion(req);
			
			Message getreq 	= createSendMessage(MLSaddress,MaximaManager.MAXIMA_MLS_GETAPP,reqdata);
			
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
			
		}else if(func.equals("send")) {
			
			if(!(existsParam("to") || existsParam("id")|| existsParam("publickey"))  || !existsParam("application") || !existsParam("data") ) {
				throw new CommandException("MUST specify to|id|publickey, application and data for a send command");
			}
			
			//Send a message..
			String fullto = null;
			if(existsParam("to")) {
				fullto 	= getParam("to");
				
			}else if(existsParam("publickey")) {
				
				//Load the contact from the public key..
				String pubkey = getParam("publickey");
				
				//Load the contact
				MaximaContact mcontact = maxdb.loadContactFromPublicKey(pubkey);
				if(mcontact == null) {
					throw new CommandException("No Contact found for publickey : "+pubkey);
				}
				
				//Get the address
				fullto = mcontact.getCurrentAddress();
			
			}else {
				
				//Load the contact from the id..
				String id = getParam("id");
				
				//Load the contact
				MaximaContact mcontact = maxdb.loadContactFromID(Integer.parseInt(id));
				if(mcontact == null) {
					throw new CommandException("No Contact found for ID : "+id);
				}
				
				//Get the address
				fullto = mcontact.getCurrentAddress();
			}
			
			//Which application
			String application 	= getParam("application");

			//What data
			MiniData mdata 	= null;
			if(isParamJSONObject("data")) {
				MiniString datastr = new MiniString(getJSONObjectParam("data").toString());
				mdata = new MiniData(datastr.getData());
			}else {
				mdata = getDataParam("data");
			} 
			
			//Now convert into the correct message..
			Message sender = createSendMessage(fullto, application, mdata);
			
			//Get the message
			MaximaMessage maxmessage = (MaximaMessage) sender.getObject("maxima");
			
			//Convert to JSON
			JSONObject json = maxmessage.toJSON();
			json.put("msgid", sender.getString("msgid"));
			
			String tohost 	= sender.getString("tohost");
			int toport 		= sender.getInteger("toport");
			
			//Are we polling..
			boolean pollsend = getBooleanParam("poll", false);
			json.put("poll", pollsend);
			
			if(pollsend) {
				
				//Add it to our Polling list of sends..
				max.PostMessage(sender);
				
				json.put("message", "Message added to send stack..");
				
				ret.put("response", json);
				
			}else {
				
				//Now construct a complete Maxima Data packet
				try {
					//Get some time values..
					long timenow = System.currentTimeMillis();
					
					//Create the packet
					MiniData maxpacket = MaxMsgHandler.constructMaximaData(sender);
					if(maxpacket == null) {
						throw new Exception("Could not build Maxima message in time..");
					}
					
					long creation = System.currentTimeMillis();
					
					//And Send it..
					MiniData validresp = MaxMsgHandler.sendMaxPacket(tohost, toport, maxpacket);
					long sending = System.currentTimeMillis();
					
					boolean valid = true;
					if(!validresp.isEqual(MaximaManager.MAXIMA_RESPONSE_OK)) {
						valid = false;
					}
					
					json.put("delivered", valid);
					json.put("creation", creation-timenow);
					json.put("sending", sending-creation);
					
					if(!valid) {
						if(validresp.isEqual(MaximaManager.MAXIMA_RESPONSE_TOOBIG)) {
							json.put("error", "Maxima Mesasge too big");
						}else if(validresp.isEqual(MaximaManager.MAXIMA_RESPONSE_UNKNOWN)) {
							json.put("error", "Unkown Address");
						}else if(validresp.isEqual(MaximaManager.MAXIMA_RESPONSE_WRONGHASH)) {
							json.put("error", "TxPoW Hash wrong");
						}else {
							json.put("error", "Not delivered");
						} 
					}
					
				}catch(Exception exc){
					//Something wrong
					json.put("delivered", false);
					json.put("error", exc.toString());
				}
				
				ret.put("response", json);
			}
		
		}else {
			throw new CommandException("Unknown Action : "+func);
		}
		
		return ret;
	}
	
	public static Message createSendMessage(String zFullTo, String zApplication, MiniData zData) {
		
		MaximaManager max = Main.getInstance().getMaxima();
		
		//Send a message..
		String fullto 	= zFullTo;
		int indexp 		= fullto.indexOf("@");
		int index 		= fullto.indexOf(":");
		
		//Get the Public Key
		String publickey = fullto.substring(0,indexp);
		if(publickey.startsWith("Mx")) {
			publickey = Address.convertMinimaAddress(publickey).to0xString();
		}
		
		//get the host and port..
		String tohost 	= fullto.substring(indexp+1,index);
		int toport		= Integer.parseInt(fullto.substring(index+1));	
		
		//Get the complete details..
		MaximaMessage maxmessage = max.createMaximaMessage(publickey, zApplication, zData);
		
		//Get the MinData version
		MiniData maxdata = MiniData.getMiniDataVersion(maxmessage);
		
		//Hash it..
		MiniData hash 	= Crypto.getInstance().hashObject(maxdata);
		
		//Send to Maxima..
		Message sender = new Message(MaximaManager.MAXIMA_SENDMESSAGE);
		sender.addObject("maxima", maxmessage);
		
		sender.addObject("mypublickey", max.getPublicKey());
		sender.addObject("myprivatekey", max.getPrivateKey());
		
		sender.addString("publickey", publickey);
		sender.addString("tohost", tohost);
		sender.addInteger("toport", toport);
		
		sender.addString("msgid", hash.to0xString());
		
		return sender;
	}

	//Check an MLS address
	public static boolean checkAddress(String zMLS) {
		
		if(!zMLS.startsWith("Mx")) {
			return false;
		}
		
		int indexp 	= zMLS.indexOf("@");
		int index 	= zMLS.indexOf(":");
		if(indexp == -1 || index==-1) {
			return false;
		}
		
		return true;
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
		return new maxima();
	}

}
