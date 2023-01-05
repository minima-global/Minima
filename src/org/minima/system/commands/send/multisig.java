package org.minima.system.commands.send;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.StateVariable;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class multisig extends Command {

	/**
	 * Generic MULTISIG contract that people can use and track
	 * 
	 * Added by default to your scripts
	 * 
	 * CANNOT EVER CHANGE! - do a new function if need be
	 */
	public static final String MULTISIG_CONTRACT = "LET n=PREVSTATE(0) LET m=PREVSTATE(1) LET script=[RETURN MULTISIG(]+STRING(n) LET counter=0 WHILE counter LT m DO LET script=script+[ ]+STRING(PREVSTATE(counter+2)) LET counter=INC(counter) ENDWHILE LET script=script+[)] EXEC script";
	
	public multisig() {
		super("multisig","");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","required","publickey","file","publickeys","amount", "tokenid","coinid","address"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
	
		//What are we doing..
		String action = getParam("action");
		
		//The actual address
		String msaddress = new Address(multisig.MULTISIG_CONTRACT).getAddressData().to0xString();
		
		if(action.equals("create")) {
			
			//Get the required params
			MiniNumber amount 	= getNumberParam("amount");
			String tokenid 		= getParam("tokenid","0x00");
			
			//How many sigs required
			MiniNumber required = getNumberParam("required");
			
			//Get the pub keys
			int totalkeys=0;
			ArrayList<String> allkeys = new ArrayList<>();
			JSONArray pubkeys = getJSONArrayParam("publickeys");
			for(Object obj : pubkeys) {
				String pubkey = (String)obj;
				allkeys.add(pubkey);
				totalkeys++;
			}
			
			//Now construct the state params
			String stateparams = "{\"0\":\""+required+"\",\"1\":\""+totalkeys+"\"";
			
			//Now add all the public keys
			int counter=2;
			for(String pubk : allkeys) {
				stateparams += ",\""+counter+"\":\""+pubk+"\"";
				counter++;
			}
			
			//Close the JSON
			stateparams +="}";
			
			//Now construct the complete send function
			String sendfunction = "send tokenid:"+tokenid+" amount:"+amount.toString()+" address:"+msaddress+" state:"+stateparams;
			
			//Now run this!..
			JSONArray result 		= Command.runMultiCommand(sendfunction);
			JSONObject sendresult 	= (JSONObject) result.get(0); 
			if((boolean) sendresult.get("status")) {
				ret.put("response", sendresult.get("response"));
			}else {
				ret.put("status", false);
				if(sendresult.get("message") != null) {
					ret.put("message", sendresult.get("message"));
				}else if(sendresult.get("error") != null) {
					ret.put("message", sendresult.get("error"));
				}else {
					ret.put("message", sendresult);
				}
			}
		
		}else if(action.equals("list")) {
			
			//Get the tree tip..
			TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
			
			//List all the multi sig coins you have..
			ArrayList<Coin> coins = TxPoWSearcher.searchCoins(	tip, true, 
																false, MiniData.ZERO_TXPOWID,
																false,MiniNumber.ZERO,
																true, new MiniData(msaddress), 
																false, MiniData.ZERO_TXPOWID, false);
			
			//Put it all in an array
			JSONArray coinarr = new JSONArray();
			for(Coin cc : coins) {
				coinarr.add(cc.toJSON());
			}
			
			ret.put("response", coinarr);
			
		}else if(action.equals("spend")) {
			
			//Which coin
			String coinid 		= getParam("coinid");
			
			//Find the coin
			Coin cc = TxPoWSearcher.searchCoin(new MiniData(coinid));
			if(cc == null) {
				throw new CommandException("CoinID not found : "+coinid);
			}
			
			//How much
			MiniNumber amount 	= getNumberParam("amount");
			
			//Any change
			MiniNumber change = MiniNumber.ZERO;
			if(cc.getTokenID().isEqual(MiniData.ZERO_TXPOWID)) {
				change = cc.getAmount().sub(amount);
			}else {
				//It's a token
				MiniNumber scaletoken 	= cc.getToken().getScaledTokenAmount(cc.getAmount());
				change 					= scaletoken.sub(amount);
			}
			
			//Which key do we sign with
			String tokenid		= cc.getTokenID().to0xString();	
			String address		= getParam("address");
			String coinaddress  = cc.getAddress().to0xString();
			
			//CANNOT send funds back to yourself as do not storew the state
			if(address.equals(coinaddress)) {
				throw new CommandException("CANNOT send funds back to yourself!");
			}
			
			//The txnname..
			String txnname_default 	= "multispend_"+System.currentTimeMillis();
			String txnname 			= getParam("file",txnname_default);
			
			//Create a txn..
			String txnsender = 
						  "txncreate id:"+txnname+";"
						+ "txninput  id:"+txnname+" coinid:"+coinid+";"
						+ "txnoutput id:"+txnname+" storestate:false amount:"+amount+" address:"+address+" tokenid:"+tokenid+";";

			//Is there change
			if(change.isMore(MiniNumber.ZERO)) {
				
				//Copy the complete coin state
				ArrayList<StateVariable> allstate = cc.getState();
				for(StateVariable statevar : allstate) {
					txnsender +=  "txnstate id:"+txnname+" port:"+statevar.getPort()+" value:"+statevar.getData().toString()+";";	
				}
	
				//And the change
				txnsender +=  "txnoutput id:"+txnname+" storestate:true amount:"+change+" address:"+coinaddress+" tokenid:"+tokenid+";";
			}
						
			//And finish off..
			txnsender +=  "txnexport id:"+txnname+" file:"+txnname+".txn;"
						+ "txndelete id:"+txnname;
			
			//Run it..
			JSONArray result = Command.runMultiCommand(txnsender);
			ret.put("response", result);
		
		}else if(action.equals("sign")) {
			
			//Which publickey
			String pubkey = getParam("publickey");

			//Which file..
			String file = getParam("file");
			
			//The signer function
			String txnname  = "signed_"+file;
			String txnsigner = 
					  "txnimport id:"+txnname+" file:"+file+";"
					+ "txnsign   id:"+txnname+" publickey:"+pubkey+";"
					+ "txnexport id:"+txnname+" file:"+txnname+";"
					+ "txndelete id:"+txnname;
			
			//Run it..
			JSONArray result = Command.runMultiCommand(txnsigner);
			ret.put("response", result);
		
		}else if(action.equals("post")) {
			
			//Which file..
			String file 		= getParam("file");
			String txnname  	= "post_"+file;
			String txnsigner 	= 
					  "txnimport id:"+txnname+" file:"+file+";"
					+ "txnpost   id:"+txnname+" auto:true;"
					+ "txndelete id:"+txnname;
			
			//Run it..
			JSONArray result = Command.runMultiCommand(txnsigner);
			ret.put("response", result);
		
		}else if(action.equals("view")) {
			
			String file 		= getParam("file");
			String txnname  	= MiniData.getRandomData(32).to0xString();
			String txnview 		= 
					  "txnimport id:"+txnname+" file:"+file+";"
					+ "txndelete id:"+txnname;
			
			//Run it..
			JSONArray result = Command.runMultiCommand(txnview);
			ret.put("response", result);
			
		}else {
			throw new CommandException("Invalid action : "+action);
		}
		
		return ret;
	}
	
	@Override
	public Command getFunction() {
		return new multisig();
	}
}