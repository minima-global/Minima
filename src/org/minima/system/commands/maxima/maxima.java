package org.minima.system.commands.maxima;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.maxima.MaximaContact;
import org.minima.database.maxima.MaximaDB;
import org.minima.database.maxima.MaximaHost;
import org.minima.objects.Address;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.commands.network.connect;
import org.minima.system.network.maxima.MaxMsgHandler;
import org.minima.system.network.maxima.MaximaManager;
import org.minima.system.network.maxima.message.MaximaMessage;
import org.minima.system.params.GeneralParams;
import org.minima.utils.Crypto;
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
				+ "logs: (optional)\n"
				+ "    true or false, true turns on detailed logs for Maxima.\n"
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
				"publickey","application","data","logs","poll","host"}));
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
		
		//Enable Logs..
		if(existsParam("logs")) {
			if(getParam("logs").equals("true")) {
				max.mMaximaLogs = true;
			}else {
				max.mMaximaLogs = false;
			}
		}
		
		JSONObject details = new JSONObject();
		
		if(func.equals("info")) {
			
			//Your local IP address
			String fullhost = GeneralParams.MINIMA_HOST+":"+GeneralParams.MINIMA_PORT;
			
			//Show details
			details.put("logs", max.mMaximaLogs);
			details.put("name", MinimaDB.getDB().getUserDB().getMaximaName());
			details.put("publickey", max.getPublicKey().to0xString());
			details.put("staticmls", max.isStaticMLS());
			details.put("mls", max.getMLSHost());
			details.put("localidentity", max.getLocalMaximaAddress());
			details.put("contact", max.getRandomMaximaAddress());
			
			ret.put("response", details);
		
		}else if(func.equals("staticmls")) {
		
			String host = getParam("host");
			
			//Check is valid..
			Message conn = connect.createConnectMessage(host);
			if(conn == null) {
				throw new CommandException("Invalid host.. must be host:port : "+host);
			}
			
			if(host.equals("clear")) {
				max.setStaticMLS(false, "");
			}else {
				max.setStaticMLS(true, host);
			}
			
			details.put("staticmls", max.isStaticMLS());
			details.put("mls", max.getMLSHost());
			ret.put("response", details);
			
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
			
		}else if(func.equals("statichost")) {
			
			String id = getParam("host");
			if(id.equals("random")) {
				details.put("static", false);
				max.setStaticAddress(false, "");
			}else {
				details.put("static", true);
				max.setStaticAddress(true, id);
			}
		
			details.put("contact", max.getRandomMaximaAddress());
			
			ret.put("response", details);
			
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
			
			throw new CommandException("Supported Soon..");
			
//			//Create a new Maxima Identity..
//			max.createMaximaKeys();
//			
//			//Show details
//			String ident = max.getMaximaIdentity();  
//			details.put("identity", ident);
//			ret.put("response", details);

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
	
	
	@Override
	public Command getFunction() {
		return new maxima();
	}

}
