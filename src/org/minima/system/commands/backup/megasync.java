package org.minima.system.commands.backup;

import java.io.File;
import java.math.BigInteger;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.database.wallet.ScriptRow;
import org.minima.database.wallet.SeedRow;
import org.minima.database.wallet.Wallet;
import org.minima.objects.Address;
import org.minima.objects.Coin;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.objects.base.MiniString;
import org.minima.objects.keys.TreeKey;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.params.GeneralParams;
import org.minima.utils.BIP39;
import org.minima.utils.Crypto;
import org.minima.utils.MiniFile;
import org.minima.utils.MinimaLogger;
import org.minima.utils.encrypt.PasswordCrypto;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;
import org.minima.utils.ssl.SSLManager;

public class megasync extends Command {

	public megasync() {
		super("megasync","Restore / Sync from a MegaMMR node");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","address","data"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String action = getParam("action", "seed");

		if(action.equals("mydetails")) {
			
			//Get all your public keys and addresses
			Wallet wal = MinimaDB.getDB().getWallet();
			
			ArrayList<ScriptRow> scrows = wal.getAllAddresses();
			
			JSONArray alldets = new JSONArray();
			for(ScriptRow row : scrows) {
				if(!row.getPublicKey().equals("0x00")) {
					
					JSONObject singledet = new JSONObject();
					singledet.put("publickey", row.getPublicKey());
					singledet.put("address", row.getAddress());
					
					alldets.add(singledet);
				}
			}
			
			JSONObject resp = new JSONObject();
			resp.put("details", alldets);
			
			ret.put("response", resp);
		
		}else if(action.equals("findcoins")) {
			
			MiniData address = getDataParam("address");
			
			ArrayList<MiniData> alladdr = new ArrayList<>();
			alladdr.add(address);
			
			ArrayList<Coin> allcoins = searchMegaCoins(alladdr, new ArrayList<>());
			
			for(Coin cc : allcoins) {
				
				JSONObject coinproofresp = Command.runSingleCommand("coinexport coinid:"+cc.getCoinID().to0xString());
				
				MinimaLogger.log(coinproofresp.toJSONString());
			}
		}
			
		return ret;
	}
	
	public static synchronized ArrayList<Coin> searchMegaCoins(
			ArrayList<MiniData> zAddresses,
			ArrayList<MiniData> zPublicKeys) {

		//The list of Coins
		ArrayList<Coin> coinentry = new ArrayList<>();
		
		//Start node position
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
		
		//A list of spent CoinID..
		HashSet<String> spentcoins = new HashSet<>();
		
		//Are we MEGAMMR
		boolean MEGACHECK = false; 
		
		//Cycle through
		while(tip!=null || MEGACHECK) {
		
			ArrayList<Coin> coins = null;
			
			if(!MEGACHECK) {
				
				//Get the Relevant coins..
				coins = tip.getAllCoins();
				
			}else {
				
				//Need to LOCK DB
				MinimaDB.getDB().readLock(true);
				
				//Get the MEGAMMR COINS..
				coins = new ArrayList<Coin>(MinimaDB.getDB().getMegaMMR().getAllCoins().values());
			}
			
			//Get the details..
			for(Coin coin : coins) {
			
				//Check this coin against all the relevant Addresses and pub keys
				boolean found = false;
				for(MiniData address : zAddresses) {
					if(coin.getAddress().isEqual(address) ||
					   coin.checkForStateVariable(address.to0xString()) ) {
						found = true;
						break;
					}
				}
				
				if(!found) {
					//Check for pub key && adresses in state
					for(MiniData pubkey : zPublicKeys) {
						if(coin.checkForStateVariable(pubkey.to0xString()) ) {
							found = true;
							break;
						}
					}
				}
				
				if(!found) {
					continue;
				}
				
				//Get the CoinID
				String coinid = coin.getCoinID().to0xString();
				
				//is it spent..
				boolean spent = coin.getSpent();
				
				//Add it to our list of spent coins..
				if(spent) {
					spentcoins.add(coinid);
				}else {
					//Check if this has been spent in a previous block..
					if(!spentcoins.contains(coinid)) {
					
						//Make a copy..
						Coin copycoin = coin.deepCopy();
						
						//OK - fresh unspent coin
						coinentry.add(copycoin);
						
						//And no more from now..
						spentcoins.add(coinid);
					}
				}
			}
		
			if(!MEGACHECK) {
				//And move back up the tree
				tip = tip.getParent();
				
				//Are we at the end..
				if(tip == null && GeneralParams.IS_MEGAMMR) {
					MEGACHECK = true;
				}
			}else {
				//Need to LOCK DB
				MinimaDB.getDB().readLock(false);
				
				//we just did a MEGAMMR check.. that's it..
				break;
			}
		}
		
		//Are we only showing simple Coins..
		ArrayList<Coin> finalcoins = coinentry;
			
		return finalcoins;
	}
	
	
	@Override
	public Command getFunction() {
		return new megasync();
	}
}
