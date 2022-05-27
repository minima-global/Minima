package org.minima.system.network.maxima;

import org.minima.database.MinimaDB;
import org.minima.database.maxima.MaximaContact;
import org.minima.database.maxima.MaximaDB;
import org.minima.database.maxima.MaximaHost;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.Address;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.commands.maxima.maxima;
import org.minima.system.network.minima.NIOClient;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.minima.NIOMessage;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.json.parser.JSONParser;
import org.minima.utils.messages.Message;
import org.minima.utils.messages.MessageProcessor;

public class MaximaContactManager extends MessageProcessor {

	public static final String CONTACT_APPLICATION 		 = "**maxima_contact_ctrl**";
	
	public static final String MAXCONTACTS_RECMESSAGE 	 = "MAXCONTACTS_RECMESSAGE";
	public static final String MAXCONTACTS_UPDATEINFO 	 = "MAXCONTACTS_SENDMESSAGE";
	
	public static final String MAXCONTACTS_DELETECONTACT = "MAXCONTACTS_DELETECONTACT";
	
	MaximaManager mManager;
	
	public MaximaContactManager(MaximaManager zManager) {
		super("MAXIMA_CONTACTS");
		
		mManager = zManager;
	}
	
	public JSONObject getMaximaContactInfo(boolean zIntro, boolean zDelete) {
		JSONObject ret = new JSONObject();
		
		ret.put("delete", zDelete);
		
		if(zDelete) {
			ret.put("intro", false);
			ret.put("publickey", mManager.getPublicKey().to0xString());
			ret.put("address", "");
			
			//Extra Data
			ret.put("name", "");
			ret.put("minimaaddress", "Mx00");
			ret.put("topblock",MiniNumber.ZERO.toString());
			ret.put("checkblock",MiniNumber.ZERO.toString());
			ret.put("checkhash",MiniData.ZERO_TXPOWID.toString());
			
		}else {
			
			//Get some info about the chain
			TxPoWTreeNode tip 	= MinimaDB.getDB().getTxPoWTree().getTip();
			TxPoWTreeNode tip50	= tip.getParent(50);
			
			ret.put("intro", zIntro);
			ret.put("publickey", mManager.getPublicKey().to0xString());
			ret.put("address", mManager.getRandomMaximaAddress());
			
			//Extra Data
			ret.put("name", MinimaDB.getDB().getUserDB().getMaximaName());
			
			String address 		= MinimaDB.getDB().getWallet().getDefaultAddress().getAddress();
			String mxaddress 	= Address.makeMinimaAddress(new MiniData(address)); 
			ret.put("minimaaddress", mxaddress);
			
			ret.put("topblock",tip.getBlockNumber().toString());
			ret.put("checkblock",tip50.getBlockNumber().toString());
			ret.put("checkhash",tip50.getTxPoW().getTxPoWID());
		}
		
		return ret;
	}
	
	@Override
	protected void processMessage(Message zMessage) throws Exception {
		
		//Get the DB
		MaximaDB maxdb = MinimaDB.getDB().getMaximaDB();
		
		if(zMessage.getMessageType().equals(MAXCONTACTS_RECMESSAGE)) {
			
			//get the max json
			JSONObject maxjson = (JSONObject) zMessage.getObject("maxmessage");
			
			//Get the public key
			String publickey = (String) maxjson.get("from");
			
			//Get the data
			String data 	= (String) maxjson.get("data");
			MiniData dat 	= new MiniData(data);
			
			//Convert to a JSON
			MiniString datastr 		= new MiniString(dat.getBytes());
			
			JSONObject contactjson 	= (JSONObject) new JSONParser().parse(datastr.toString());
			
			//Process this special contacts message..
			String contactkey = (String) contactjson.get("publickey"); 
			if(!contactkey.equals(publickey)) {
				MinimaLogger.log("Received contact message with mismatch public keys..");
				return;
			}
			
			//OK - lets get his current address
			boolean intro		= (boolean)contactjson.get("intro");
			boolean delete		= (boolean)contactjson.get("delete");
			
			//Their Address
			String address 		= (String) contactjson.get("address");
			
			//Few checks on name
			String name 		= (String) contactjson.get("name");
			name = name.replace("\"", "");
			name = name.replace("'", "");
			name = name.replace(";", "");
			
			//Create a Contact - if not there already
			MaximaContact checkcontact = maxdb.loadContactFromPublicKey(publickey);
			
			//Are we being deleted..
			if(delete) {
				MinimaLogger.log("DELETED contact request from : "+publickey);
				if(checkcontact != null) {
					maxdb.deleteContact(checkcontact.getUID());
				}
				
				return;
			}
			
			//The ExtraData
			String mxaddress		= (String) contactjson.get("minimaaddress");
			MiniNumber topblock 	= new MiniNumber((String) contactjson.get("topblock"));
			MiniNumber checkblock 	= new MiniNumber((String) contactjson.get("checkblock"));
			MiniData checkhash 		= new MiniData((String) contactjson.get("checkhash"));
			
			MaximaContact mxcontact = new MaximaContact(publickey);
			mxcontact.setCurrentAddress(address);
			
			if(checkcontact == null) {
				mxcontact.setname(name);
				mxcontact.setMinimaAddress(mxaddress);
				mxcontact.setMyAddress("newcontact");
				mxcontact.setBlockDetails(topblock, checkblock, checkhash);
				mxcontact.setLastSeen(System.currentTimeMillis());
				
				maxdb.newContact(mxcontact);
				
			}else{
				mxcontact.setExtraData(checkcontact.getExtraData());
				
				//Overwrite the new details
				mxcontact.setname(name);
				mxcontact.setMinimaAddress(mxaddress);
				mxcontact.setBlockDetails(topblock, checkblock, checkhash);
				mxcontact.setMyAddress(checkcontact.getMyAddress());
				mxcontact.setLastSeen(System.currentTimeMillis());
				
				maxdb.updateContact(mxcontact);
			}
			
			//Send them a contact message aswell..
			if(intro || checkcontact == null) {
				Message msg = new Message(MAXCONTACTS_UPDATEINFO);
				msg.addString("publickey", publickey);
				msg.addString("address", address);
				PostMessage(msg);
			}
			
		}else if(zMessage.getMessageType().equals(MAXCONTACTS_UPDATEINFO)) {
			
			//Are we deleting..
			boolean delete = false;
			if(zMessage.exists("delete")) {
				delete = zMessage.getBoolean("delete");
			}
			
			//Who To..
			String publickey = zMessage.getString("publickey");
			String address 	 = zMessage.getString("address");
			
			//Send a Contact info message to a user
			JSONObject contactinfo	= getMaximaContactInfo(false,delete);
			
			//Now Update Our DB..
			if(!delete) {
				MaximaContact mxcontact = maxdb.loadContactFromPublicKey(publickey);
				mxcontact.setMyAddress((String)contactinfo.get("address"));
				maxdb.updateContact(mxcontact);
			}
			
			MiniString str			= new MiniString(contactinfo.toString());
			MiniData mdata 			= new MiniData(str.getData());
			
			//Now convert into the correct message..
			Message sender = maxima.createSendMessage(address, CONTACT_APPLICATION , mdata);
			
			//Post it on the stack
			mManager.PostMessage(sender);	
		
		}else if(zMessage.getMessageType().equals(MAXCONTACTS_DELETECONTACT)) {
			
			//Few steps here..
			int id = zMessage.getInteger("id");
			
			//Get that contact
			MaximaContact mcontact = maxdb.loadContactFromID(id);
			if(mcontact == null) {
				MinimaLogger.log("Trying to remove unknown Contact ID : "+id);
				return;
			}
			
			//Where are WE 
			String myaddress = mcontact.getMyAddress();
			
			//Get the host..
			int index 	= myaddress.indexOf("@");
			String host = myaddress.substring(index+1);
			
			//Get the client
			MaximaHost mxhost = maxdb.loadHost(host);
			
			//Reset that host PubKey.. and Update DB
			if(mxhost != null) {
				mxhost.createKeys();
				maxdb.updateHost(mxhost);
			}
			
			//Delete the contact
			maxdb.deleteContact(id);
			
			//Tell them
			NIOClient nioc = Main.getInstance().getNIOManager().getNIOClient(host); 
			if(nioc != null) {
				//So we know the details.. Post them to him.. so he knows who we are..
				MaximaCTRLMessage maxmess = new MaximaCTRLMessage(MaximaCTRLMessage.MAXIMACTRL_TYPE_ID);
				maxmess.setData(mxhost.getPublicKey());
				NIOManager.sendNetworkMessage(nioc.getUID(), NIOMessage.MSG_MAXIMA_CTRL, maxmess);
			}
			
			//Refresh ALL users..
			mManager.PostMessage(MaximaManager.MAXIMA_REFRESH);
			
			//Send him a message saying we have deleted him..
			Message msg = new Message(MAXCONTACTS_UPDATEINFO);
			msg.addBoolean("delete", true);
			msg.addString("publickey", mcontact.getPublicKey());
			msg.addString("address", mcontact.getCurrentAddress());
			PostMessage(msg);
		}
		
	}

}
