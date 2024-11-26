package org.minima.system.commands.send;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.wallet.ScriptRow;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.StateVariable;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.commands.CommandRunner;
import org.minima.utils.Crypto;
import org.minima.utils.MiniFile;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class multisigread extends Command {

	/**
	 * Generic MULTISIG contract that people can use and track
	 * 
	 * Added by default to your scripts
	 * 
	 * CANNOT EVER CHANGE! - do a new function if need be
	 */
	public static final String MULTISIG_CONTRACT = 
			"LET root=PREVSTATE(1) IF root NEQ 0x21 THEN IF SIGNEDBY(root) THEN RETURN TRUE ENDIF ENDIF LET n=PREVSTATE(2) LET m=PREVSTATE(3) LET script=[RETURN MULTISIG(]+STRING(n) LET counter=0 WHILE counter LT m DO LET script=script+[ ]+STRING(PREVSTATE(counter+4)) LET counter=INC(counter) ENDWHILE LET script=script+[)] EXEC script";
	
	public multisigread() {
		super("multisigread","View, list or start a spend of a multisig coin that can be used by root OR n of m txns");
	}
	
	@Override
	public String getFullHelp() {
		return "\nmultisigread\n"
				+ "\n"
				+ "Utility Function for multisig in READ mode"
				+ "\n"
				+ "List, create a spend or get a key for a nultisig coin that can only be used in a txn signed by root OR n of m given public keys.\n"
				+ "\n"
				+ "action: \n"
				+ "    getkey : returns one of your default public keys to be provided when creating the coin.\n"
				+ "    list : lists all existing multisig coins.\n"
				+ "    spend : creates an unsigned transaction (.txn) file to spend a specified amount of a multisig coin.\n"
				+ "    post : posts the transaction to the network, must be signed as required by the contract before posting.\n"
				+ "    view : view the details of a multisig transaction file.\n"
				+ "\n"
				+ "id: (optional)\n"
				+ "    Create a multisig coin with an id or list by id.\n"
				+ "    Should be unique. This cannot be retrieved later.\n"
				+ "    The id is hashed and stored as state variable 0.\n"
				+ "\n"
				+ "amount: (optional)\n"
				+ "    The amount to lock in the multisig coin or the amount to spend.\n"
				+ "\n"
				+ "coinid: (optional)\n"
				+ "    The coinid of the multisig coin to spend. Alternatively, use the id.\n"
				+ "\n"
				+ "address: (optional)\n"
				+ "    The address to send the specified amount to.\n"
				+ "\n"
				+ "file: (optional)\n"
				+ "    The transaction (.txn) file to view or post.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "multisig action:getkey\n"
				+ "\n"
				+ "multisig action:list\n"
				+ "\n"
				+ "multisig action:list id:2of3multisig\n"
				+ "\n"
				+ "multisig action:spend id:3of3multisigroot amount:5 address:0xFF..\n"
				+ "\n"
				+ "multisig action:spend coinid:0x17EA.. amount:5 address:0xFF.. file:multisig.txn\n"
				+ "\n"
				+ "multisig action:view file:multisig.txn\n"
				+ "\n"
				+ "multisig action:post file:signed_multispend_1673351592845.txn\n"
				+ "\n"
				+ "multisig action:post file:signed_multisig.txn\n";
		}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"id","action","root","required",
				"file","publickeys","amount", "tokenid","coinid","address","password"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
	
		//What are we doing..
		String action = getParam("action");
		
		//The actual address
		String msaddress = new Address(multisigread.MULTISIG_CONTRACT).getAddressData().to0xString();
		
		//ID of the custom transaction
		String randomid 	= MiniData.getRandomData(32).to0xString();
		
		if(action.equals("getkey")) {
			
			//Get one of your gets..
			ScriptRow scrow = MinimaDB.getDB().getWallet().getDefaultAddress();
			
			//Get the public key
			String key = scrow.getPublicKey();
			
			JSONObject keyjson = new JSONObject();
			keyjson.put("publickey", key);
			
			ret.put("response", keyjson);
			
		}else if(action.equals("list")) {
			
			//Get the tree tip..
			TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
			
			//List all the multi sig coins you have..
			ArrayList<Coin> coins = TxPoWSearcher.searchCoins(	tip, true, 
																false, MiniData.ZERO_TXPOWID,
																false,MiniNumber.ZERO,
																true, new MiniData(msaddress), 
																false, MiniData.ZERO_TXPOWID, false);
			
			//Are we searching via id
			if(existsParam("id")) {
				
				//Get the hash of the ID
				MiniString sid 	= new MiniString(getParam("id"));
				String id 		= Crypto.getInstance().hashObject(sid).to0xString();
				
				//Search for it..
				JSONArray coinarr = new JSONArray();
				for(Coin cc : coins) {
					ArrayList<StateVariable> allstate = cc.getState();
					for(StateVariable statevar : allstate) {
						if(statevar.getData().toString().equals(id)) {
							coinarr.add(cc.toJSON());
						}
					}
				}
				
				ret.put("response", coinarr);
				
			}else {
			
				//Put it all in an array
				JSONArray coinarr = new JSONArray();
				for(Coin cc : coins) {
					coinarr.add(cc.toJSON());
				}
				
				ret.put("response", coinarr);
			}
			
		}else if(action.equals("spend")) {
			
			//Are we searching via id or coinid
			String coinid = null;
			if(existsParam("id")) {
				
				//Get the hash of the ID
				MiniString sid 	= new MiniString(getParam("id"));
				String id 		= Crypto.getInstance().hashObject(sid).to0xString();
				
				//Get the tree tip..
				TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
				
				//List all the multi sig coins you have..
				ArrayList<Coin> coins = TxPoWSearcher.searchCoins(	tip, true, 
																	false, MiniData.ZERO_TXPOWID,
																	false,MiniNumber.ZERO,
																	true, new MiniData(msaddress), 
																	false, MiniData.ZERO_TXPOWID, false);
				
				//Search for it..
				Coin fcoin = null;
				for(Coin cc : coins) {
					ArrayList<StateVariable> allstate = cc.getState();
					for(StateVariable statevar : allstate) {
						if(statevar.getData().toString().equals(id)) {
							fcoin = cc;
							break;
						}
					}
					
					if(fcoin!=null) {
						break;
					}
				}
				
				if(fcoin!=null) {
					coinid = fcoin.getCoinID().to0xString();
				}else {
					throw new CommandException("MultiSig with id not found : "+id);
				}
				
			}else {
				//Which coin
				coinid = getParam("coinid");
			}
			
			//Find the coin
			Coin cc = TxPoWSearcher.searchCoin(new MiniData(coinid),false);
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
			String txnname_default 	= "multispend_"+System.currentTimeMillis()+".txn";
			String txnname 			= getParam("file",txnname_default);
			
			//Create a txn..
			String txnsender = 
						  "txncreate id:"+randomid+";"
						+ "txninput  id:"+randomid+" coinid:"+coinid+";"
						+ "txnoutput id:"+randomid+" storestate:false amount:"+amount+" address:"+address+" tokenid:"+tokenid+";";

			//Is there change
			if(change.isMore(MiniNumber.ZERO)) {
				
				//Copy the complete coin state
				ArrayList<StateVariable> allstate = cc.getState();
				for(StateVariable statevar : allstate) {
					txnsender +=  "txnstate id:"+randomid+" port:"+statevar.getPort()+" value:"+statevar.getData().toString()+";";	
				}
	
				//And the change
				txnsender +=  "txnoutput id:"+randomid+" storestate:true amount:"+change+" address:"+coinaddress+" tokenid:"+tokenid+";";
			}
						
			//And finish off..
			txnsender +=  "txnexport id:"+randomid+" file:"+txnname+";"
						+ "txndelete id:"+randomid;
			
			//Run it..
			JSONArray result = CommandRunner.getRunner().runMultiCommand(txnsender);
			ret.put("response", result);
		
		}else if(action.equals("post")) {
			
			//Which file..
			String file 		= getParam("file");
			String txnsigner 	= 
					  "txnimport id:"+randomid+" file:"+file+";"
					+ "txnpost   id:"+randomid+" mine:true auto:true;"
					+ "txndelete id:"+randomid;
			
			//Run it..
			JSONArray result = CommandRunner.getRunner().runMultiCommand(txnsigner);
			ret.put("response", result);
		
		}else if(action.equals("view")) {
			
			String file 		= getParam("file");
			String txnview 		= 
					  "txnimport id:"+randomid+" file:"+file+";"
					+ "txndelete id:"+randomid;
			
			//Run it..
			JSONArray result = CommandRunner.getRunner().runMultiCommand(txnview);
			ret.put("response", result);
			
		}else {
			throw new CommandException("Invalid action : "+action);
		}
		
		return ret;
	}
	
	private File getRequiredFile() throws CommandException {
		
		//What is the filename - could be relative or absolute
		String filename 	= getParam("file");
		
		//Convert to an actual File
		File theactualfile 	= MiniFile.createBaseFile(filename);
		
		return theactualfile;
	}
	
	@Override
	public Command getFunction() {
		return new multisigread();
	}
}