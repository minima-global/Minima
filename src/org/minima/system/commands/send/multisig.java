package org.minima.system.commands.send;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.userprefs.txndb.TxnDB;
import org.minima.database.userprefs.txndb.TxnRow;
import org.minima.database.wallet.KeyRow;
import org.minima.database.wallet.ScriptRow;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.StateVariable;
import org.minima.objects.Transaction;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.commands.CommandRunner;
import org.minima.system.commands.backup.vault;
import org.minima.utils.Crypto;
import org.minima.utils.MiniFile;
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
	public static final String MULTISIG_CONTRACT = 
			"LET root=PREVSTATE(1) IF root NEQ 0x21 THEN IF SIGNEDBY(root) THEN RETURN TRUE ENDIF ENDIF LET n=PREVSTATE(2) LET m=PREVSTATE(3) LET script=[RETURN MULTISIG(]+STRING(n) LET counter=0 WHILE counter LT m DO LET script=script+[ ]+STRING(PREVSTATE(counter+4)) LET counter=INC(counter) ENDWHILE LET script=script+[)] EXEC script";
	
	public multisig() {
		super("multisig","Create a multisig coin that can be used by root OR n of m txns");
	}
	
	@Override
	public String getFullHelp() {
		return "\nmultisig\n"
				+ "\n"
				+ "Create a multisig coin that can only be used in a txn signed by root OR n of m given public keys.\n"
				+ "\n"
				+ "Can provide the Vault password to temporarily decrypt the private keys when creating the coin or signing a transaction.\n"
				+ "\n"
				+ "action: \n"
				+ "    create : create a new multisig coin.\n"
				+ "    getkey : returns one of your default public keys to be provided when creating the coin.\n"
				+ "    list : lists all existing multisig coins.\n"
				+ "    spend : creates an unsigned transaction (.txn) file to spend a specified amount of a multisig coin.\n"
				+ "    sign : signs a multisig transaction (.txn) file with the relevant public keys and outputs a new signed file.\n"
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
				+ "publickeys: (optional)\n"
				+ "    The full list of public keys that can sign the multisig transaction, in the format [\"pubkey1\",..,\"pubkeym\"]\n"
				+ "\n"
				+ "root: (optional)\n"
				+ "    A root public key which can spend the coin without passing the required signature threshold.\n"
				+ "\n"
				+ "required: (optional)\n"
				+ "    The minimum number of public keys from the list required to sign a txn which spends from the multisig.\n"
				+ "\n"
				+ "coinid: (optional)\n"
				+ "    The coinid of the multisig coin to spend. Alternatively, use the id.\n"
				+ "\n"
				+ "address: (optional)\n"
				+ "    The address to send the specified amount to.\n"
				+ "\n"
				+ "file: (optional)\n"
				+ "    The transaction (.txn) file to create, sign or post.\n"
				+ "\n"
				+ "password: (optional)\n"
				+ "    Vault password to decrypt the private keys. Use with action:create and action:sign if the node is password locked.\n"
				+ "    Keys will be re-encypted after.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "multisig action:create id:2of3multisig amount:100 publickeys:[\"0xFED5..\",\"0xABD6..\",\"0xFD8B..\"] required:2 password:your_password\n"
				+ "\n"
				+ "multisig action:create id:3of3multisigroot amount:100 publickeys:[\"0xFED5..\",\"0xABD6..\",\"0xFD8B..\"] required:3 root:0xFFE..\n"
				+ "\n"
				+ "multisig action:list\n"
				+ "\n"
				+ "multisig action:list id:2of3multisig\n"
				+ "\n"
				+ "multisig action:spend id:3of3multisigroot amount:5 address:0xFF..\n"
				+ "\n"
				+ "multisig action:spend coinid:0x17EA.. amount:5 address:0xFF.. file:multisig.txn\n"
				+ "\n"
				+ "multisig action:sign file:multispend_1673351592845.txn\n"
				+ "\n"
				+ "multisig action:sign file:multisig.txn password:your_password\n"
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
				"file","publickeys","amount", "tokenid","coinid","address","password","mine"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
	
		//What are we doing..
		String action = getParam("action");
		
		//The actual address
		String msaddress = new Address(multisig.MULTISIG_CONTRACT).getAddressData().to0xString();
		
		//ID of the custom transaction
		String randomid 	= MiniData.getRandomData(32).to0xString();
		
		if(action.equals("create")) {
			
			//Is there an ID
			String id = "";
			if(existsParam("id")) {
				MiniString sid 	= new MiniString(getParam("id"));
				id = Crypto.getInstance().hashObject(sid).to0xString();
			}
			
			//Is there a root key
			String root = getParam("root", "0x21");
			if(!root.equals("0x21")) {
				if(!root.startsWith("0x") || root.length()!=66) {
					throw  new CommandException("Invalid root key : "+root);
				}
			}
			
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
				
				if(!pubkey.startsWith("0x") || pubkey.length()!=66) {
					throw  new CommandException("Invalid public key : "+pubkey);
				}
				
				allkeys.add(pubkey);
				totalkeys++;
			}
			
			if(totalkeys<required.getAsInt()) {
				throw new CommandException("Cannot have LESS keys than required!");
			}
			
			//Now construct the state params
			String stateparams = null;
			if(id.equals("")) {
				stateparams = "{\"1\":\""+root+"\",\"2\":\""+required+"\",\"3\":\""+totalkeys+"\"";
			}else {
				stateparams = "{\"0\":\""+id+"\",\"1\":\""+root+"\",\"2\":\""+required+"\",\"3\":\""+totalkeys+"\"";
			}
			
			//Now add all the public keys
			int counter=4;
			for(String pubk : allkeys) {
				stateparams += ",\""+counter+"\":\""+pubk+"\"";
				counter++;
			}
			
			//Close the JSON
			stateparams +="}";
			
			//Now construct the complete send function
			String sendfunction ="";
			if(existsParam("password")) {
				String password=getParam("password");
				sendfunction = "send password:"+password+" tokenid:"+tokenid+" amount:"+amount.toString()+" address:"+msaddress+" state:"+stateparams;
			}else {
				sendfunction = "send tokenid:"+tokenid+" amount:"+amount.toString()+" address:"+msaddress+" state:"+stateparams;
			}
			
			//Now run this!..
			JSONArray result 		= CommandRunner.getRunner().runMultiCommand(sendfunction);
			JSONObject sendresult 	= (JSONObject) result.get(0); 
			if((boolean) sendresult.get("status")) {
				
				JSONObject sender = new JSONObject();
				sender.put("send", sendresult.get("response"));
				sender.put("id", id);
				ret.put("response", sender);
				
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
		
		}else if(action.equals("getkey")) {
			
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
			String address		= getAddressParam("address");
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
		
		}else if(action.equals("sign")) {

			//Which file..
			String file = getParam("file");
			
			//What is the file..
			File actualfile 	= MiniFile.createBaseFile(file);
			File signedtxn   	= new File(actualfile.getParentFile(), "signed_"+actualfile.getName());
			
			//The signer function
			String txnsigner = "txnimport id:"+randomid+" file:"+file+";";
			
			//Run that
			JSONArray result 		= CommandRunner.getRunner().runMultiCommand(txnsigner);
			JSONObject importres 	= (JSONObject) result.get(0); 
			if(!(boolean) importres.get("status")) {
				throw new CommandException(importres.get("error").toString());
			}
			
			//Rest now
			txnsigner = "";
			
			//Get the wallet
			Wallet wallet = MinimaDB.getDB().getWallet();
			
			//now find the public key
			TxnDB db 						  = MinimaDB.getDB().getCustomTxnDB();
			TxnRow txnrow 					  = db.getTransactionRow(randomid);
			Transaction trans 				  = txnrow.getTransaction();
			Coin multicoin 					  = trans.getAllInputs().get(0);
			ArrayList<StateVariable> allstate = multicoin.getState();
			for(StateVariable statevar : allstate) {
				//Get the vaklue..
				String possiblepubkey = statevar.getData().toString();
				KeyRow key = wallet.getKeyFromPublic(possiblepubkey);
				if(key != null) {
					txnsigner	+= "txnsign id:"+randomid+" publickey:"+possiblepubkey+";";
				}
			}
			
			//Finish up
			txnsigner	+= "txnexport id:"+randomid+" file:"+signedtxn.getAbsolutePath()+";"
						+  "txndelete id:"+randomid;
			
			//Unlock DB at the start - rather than every time you run txnsign
			boolean passwordlock = false;
			if(existsParam("password") && !MinimaDB.getDB().getWallet().isBaseSeedAvailable()) {
				
				//Lets unlock the DB
				vault.passowrdUnlockDB(getParam("password"));
				 
				//Lock at the end..
				passwordlock = true;
			}
			
			//Run it..
			result = CommandRunner.getRunner().runMultiCommand(txnsigner);
			
			//Are we locking the DB
			if(passwordlock) {
				//Lock the Wallet DB
				vault.passwordLockDB(getParam("password"));
			}
			
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
		return new multisig();
	}
}