package org.minima.system.commands.maxima;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.maxima.MaximaContact;
import org.minima.database.maxima.MaximaDB;
import org.minima.database.maxima.MaximaHost;
import org.minima.objects.Address;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.network.maxima.MaxMsgHandler;
import org.minima.system.network.maxima.MaximaManager;
import org.minima.system.network.maxima.message.MaximaMessage;
import org.minima.system.params.GeneralParams;
import org.minima.utils.Crypto;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.TimerMessage;

public class maxima extends Command {

	public maxima() {
		super("maxima","[action:info|setname|hosts|send|refresh] (name:) (id:)|(to:)|(publickey:) (application:) (data:) (poll:) (delay:) - Check your Maxima details, send a message / data, enable logs");
	}
	
	@Override
	public String getFullHelp() {
		return "\nmaxima\n"
				+ "\n"
				+ "Check your Maxima details, send a message / data.\n"
				+ "\n"
				+ "Maxima is an information transport layer running on top of Minima.\n"
				+ "\n"
				+ "action:\n"
				+ "    info : Show your Maxima details - name, publickey, staticmls, mls, local identity and contact address.\n"
				+ "    setname : Set your Maxima name so your contacts recognise you. Default 'noname'.\n"
				+ "    hosts : List your Maxima hosts and see their Maxima public key, contact address, last seen time and if you are connected.\n"
				+ "    send : Send a message to a contact. Must specify 'id|to|publickey', 'application' and 'data' parameters.\n"
				+ "    sendall : Send a message to ALL your contacts. Must specify 'application' and 'data' parameters.\n"
				+ "    refresh : Refresh your contacts by sending them a network message. \n"
				+ "\n"
				+ "name: (optional)\n"
				+ "    Set your name. Use with 'action:setname'.\n"
				+ "\n"
				+ "id|to|publickey: (optional)\n"
				+ "    The id, contact address or public key of the recipient of the message. Use with 'action:send'.\n"
				+ "\n"
				+ "application: (optional)\n"
				+ "    A string that identifies which application should process the message. Use with 'action:send'.\n"
				+ "\n"
				+ "data: (optional)\n"
				+ "    The data to send. Can be HEX or a JSON object. Use with 'action:send'.\n"
				+ "\n"
				+ "poll: (optional)\n"
				+ "    true or false, true will poll the send action until successful. Use with 'action:send'.\n"
				+ "\n"
				+ "delay: (optional)\n"
				+ "    Only used with poll or with sendall. Delay sending the message by this many milliseconds\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "maxima action:info\n"
				+ "\n"
				+ "maxima action:setname name:myname\n"
				+ "\n"
				+ "maxima action:hosts\n"
				+ "\n"
				+ "maxima action:send id:1 application:appname data:0xFED5..\n"
				+ "\n"
				+ "maxima action:send to:MxG18H.. application:appname data:0xFED5..\n"
				+ "\n"
				+ "maxima action:send publickey:0xCD34.. application:ip:port data:0xFED5.. poll:true\n"
				+ "\n"
				+ "maxima action:refresh\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","name","id","to",
				"publickey","application","data","poll","host","delay"}));
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
			
			//get the messages on the stack
			int msgnum = max.getMaxSender().getSize();
			details.put("poll", msgnum);
			
			ret.put("response", details);
		
		}else if(func.equals("setname")) {
			
			String name = getParam("name");
			
			//Remove naughty chars
			name = name.replace("\"", "");
			name = name.replace("'", "");
			name = name.replace(";", "");
						
			//Remove emojis..
//			String characterFilter = "[^\\p{L}\\p{M}\\p{N}\\p{P}\\p{Z}\\p{Cf}\\p{Cs}\\s]";
//			String emotionless = name.replaceAll(characterFilter,"");
//			MinimaDB.getDB().getUserDB().setMaximaName(emotionless);
			//MinimaLogger.log("Set Maxima Name : "+name);
			
			MinimaDB.getDB().getUserDB().setMaximaName(name);
			MinimaDB.getDB().saveUserDB();
			
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
	
		}else if(func.equals("send")) {
			
			if(!(existsParam("to") || existsParam("id")|| existsParam("publickey"))  || !existsParam("application") || !existsParam("data") ) {
				throw new CommandException("MUST specify to|id|publickey, application and data for a send command");
			}
			
			//Send a message..
			String fullto = null;
			if(existsParam("to")) {
				fullto 	= getParam("to");
				
				//Is it a MAX address
				if(fullto.startsWith("MAX#")) {
					
					//Get the Address..
					JSONObject maxaddress = maxextra.getMaxAddress(fullto);
					
					//Get the address bit..
					if((boolean)maxaddress.get("success")) {
						
						JSONObject resp 	= (JSONObject)maxaddress.get("response");
						JSONObject mlsresp 	= (JSONObject)maxaddress.get("mlsresponse");
						
						//And finally..
						fullto = mlsresp.getString("address");
				
						MinimaLogger.log("ATTEMPT SEND MAX CONTACT : "+fullto);
						
					}else {
						throw new CommandException("MAX address invalid..");
					}
				}
				
				//Check is a valid address
				if(!maxextra.checkValidMxAddress(fullto)) {
					throw new CommandException("Invalid MX address : "+fullto);
				}
				
			}else if(existsParam("publickey")) {
				
				//Load the contact from the public key..
				String pubkey = getParam("publickey");
				
				//Load the contact
				MaximaContact mcontact = maxdb.loadContactFromPublicKey(pubkey);
				if(mcontact == null) {
					
					//Is it Our Own key..
					if(pubkey.equals(max.getPublicKey().to0xString())) {
						
						//Get our own address
						fullto = max.getRandomMaximaAddress();
						
					}else {
						throw new CommandException("No Contact found for publickey : "+pubkey);
					}
				
				}else {
					//Get the address
					fullto = mcontact.getCurrentAddress();
				}
				
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
				
				if(!getParam("data").startsWith("0x")) {
				
					String text 	= getParam("data");
					MiniString str 	= new MiniString(text);
					mdata 			= new MiniData(str.getData());
					
				}else {
					mdata = getDataParam("data");
				}
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
			json.put("delay", 0);
			
			if(pollsend) {
				
				//You can add a delay when poll sending..
				MiniNumber delay = getNumberParam("delay", MiniNumber.ZERO);
				json.put("delay", delay.getAsLong());
				
				if(delay.isEqual(MiniNumber.ZERO)) {
					//Add it to our Polling list of sends..
					max.PostMessage(sender);
				
					json.put("message", "Message added to send stack..");
					
				}else {
					
					//Create a timed message
					TimerMessage timedsend = new TimerMessage(delay.getAsLong(), sender);
					
					//Post  timer message
					max.PostTimerMessage(timedsend);
					
					json.put("message", "Message added to send stack with a delay..");
				}
				
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
							json.put("error", "Unknown Address");
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
		
		}else if(func.equals("sendall")) {
			
			if(!existsParam("application") || !existsParam("data") ) {
				throw new CommandException("MUST specify application and data for a sendall command");
			}
			
			//Which application
			String application 	= getParam("application");

			//What data
			MiniData mdata 	= null;
			
			if(isParamJSONObject("data")) {
				MiniString datastr = new MiniString(getJSONObjectParam("data").toString());
				mdata = new MiniData(datastr.getData());
			}else {
				
				if(!getParam("data").startsWith("0x")) {
				
					String text 	= getParam("data");
					MiniString str 	= new MiniString(text);
					mdata 			= new MiniData(str.getData());
					
				}else {
					mdata = getDataParam("data");
				}
			} 
			
			//You can add a delay when poll sending..
			MiniNumber delay = getNumberParam("delay", MiniNumber.ZERO);
			
			//Now send to all of them - poll message
			ArrayList<MaximaContact> contacts = maxdb.getAllContacts();
			for(MaximaContact contact : contacts) {
				
				//The actual address
				String fullto = contact.getCurrentAddress();
			
				//Now convert into the correct message..
				Message sender = createSendMessage(fullto, application, mdata);
				
				//Is there a delay
				if(delay.isEqual(MiniNumber.ZERO)) {
					
					//Add it to our Polling list of sends..
					max.PostMessage(sender);
					
				}else {
					
					//Create a timed message
					TimerMessage timedsend = new TimerMessage(delay.getAsLong(), sender);
					
					//Post  timer message
					max.PostTimerMessage(timedsend);
				}
			}
			
			if(delay.isEqual(MiniNumber.ZERO)) {
				ret.put("response", "Message added to send stack for all contacts");
			}else {
				ret.put("response", "Message added to send stack for all contacts with a delay of "+delay.toString());
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
