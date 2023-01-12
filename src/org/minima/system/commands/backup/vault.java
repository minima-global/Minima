package org.minima.system.commands.backup;

import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.wallet.SeedRow;
import org.minima.database.wallet.Wallet;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.utils.BIP39;
import org.minima.utils.encrypt.PasswordCrypto;
import org.minima.utils.json.JSONObject;

public class vault extends Command {

	public vault() {
		super("vault","[action:seed|lock|unlock] (seed:) (phrase:) - BE CAREFUL. Wipe / Restore your private keys");
	}
	
	@Override
	public String getFullHelp() {
		return "\nvault\n"
				+ "\n"
				+ "BE CAREFUL. Wipe / Restore your private keys.\n"
				+ "\n"
				+ "View your passphrase and seed. DO NOT SHARE THESE WITH ANYONE.\n"
				+ "\n"
				+ "ENSURE YOU HAVE A BACKUP AND SECURE RECORD OF YOUR PASSPHRASE BEFORE LOCKING.\n"
				+ "\n"
				+ "You must have your passphrase to unlock your private keys.\n"
				+ "\n"
				+ "action: (optional)\n"
				+ "    seed : Show your seed phrase. The default. \n"
				+ "    lock : Lock your node to remove your private keys.\n"
				+ "    unlock : Unlock your node to reinstate your private keys.\n"
				+ "\n"
				+ "seed: (optional)\n"
				+ "    Enter your seed to lock your node.\n"
				+ "    This will delete your private keys.\n"
				+ "\n"
				+ "phrase: (optional)\n"
				+ "    Enter your passphrase in double quotes to unlock your node.\n"
				+ "    This will reinstate your private keys.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "vault\n"
				+ "\n"
				+ "vault action:lock seed:0xDD4E..\n"
				+ "\n"
				+ "vault action:unlock phrase:\"SPRAY LAMP..\"\n";
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"action","seed","phrase","password"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String action = getParam("action", "seed");
			
		//Display the base seed..
		SeedRow base = MinimaDB.getDB().getWallet().getBaseSeed();
		
		if(action.equals("seed")) {
			
			JSONObject json = new JSONObject();
			json.put("phrase", base.getPhrase());
			json.put("seed", base.getSeed());
			json.put("locked", !MinimaDB.getDB().getWallet().isBaseSeedAvailable());
			
			ret.put("response", json);
		
		}else if(action.equals("lock")) {
			
			checkAllKeysCreated();
			
			//Are we already locked..
			if(!MinimaDB.getDB().getWallet().isBaseSeedAvailable()) {
				throw new CommandException("DB already locked!");
			}
			
			//Get the seed.. TO SHOW THEY KNOW IT..
			MiniData seed = getDataParam("seed");
			if(!seed.to0xString().equals(base.getSeed())) {
				throw new CommandException("Incorrect seed for lock");
			}
			
			//Wipe the private keys (but keep the public keys, modifiers etc)
			MinimaDB.getDB().getWallet().wipeBaseSeedRow();
			
			ret.put("response", "All private keys wiped!");
		
		}else if(action.equals("unlock")) {
			
			//Check for one or the other..
			String initphrase = getParam("phrase");
			
			//Clean it up..
			String cleanphrase = BIP39.cleanSeedPhrase(initphrase);
			
			//Convert to a data hash
			MiniData seed = BIP39.convertStringToSeed(cleanphrase);
			
			//And now recreate the private keys
			boolean ok = MinimaDB.getDB().getWallet().resetBaseSeedPrivKeys(cleanphrase, seed.to0xString());

			if(!ok) {
				throw new CommandException("Error updating Private keys.. please try again");
			}

			JSONObject resp=new JSONObject();
			resp.put("entered", initphrase);
			resp.put("cleaned", cleanphrase);
			resp.put("same", cleanphrase.equalsIgnoreCase(initphrase));
			resp.put("result", "All private keys restored!");
			
			ret.put("response", resp);
		
		}else if(action.equals("passwordlock")) {
			
			checkAllKeysCreated();
			
			//Get the password
			String password = getParam("password");
			
			passwordLockDB(password);
			
			ret.put("response", "All private keys wiped! Stored encrypted in UserDB");
		
		}else if(action.equals("passwordunlock")) {
			
			//Get the password
			String password = getParam("password");
			
			passowrdUnlockDB(password);
						
			ret.put("response", "All private keys restored!");
			
			//NOT WORKING YET!
		}else if(action.equals("keyreset")) {
			
//			//Get the seed phrase
//			String phrase = getParam("phrase");
//			
//			//First stop everything.. and get ready to restore the files..
//			Main.getInstance().restoreReady();
//			
//			//Now delete the SQL DBs..
//			MinimaDB.getDB().getTxPoWDB().getSQLDB().wipeDB();
//			MinimaDB.getDB().getTxPoWDB().getSQLDB().saveDB();
//			
//			//Wipe ArchiveDB	
//			MinimaDB.getDB().getArchive().saveDB();
//			MinimaDB.getDB().getArchive().getSQLFile().delete();
//			
//			File basedb = MinimaDB.getDB().getBaseDBFolder();
//			MiniFile.deleteFileOrFolder(GeneralParams.DATA_FOLDER, new File(basedb,"cascade.db"));
//			MiniFile.deleteFileOrFolder(GeneralParams.DATA_FOLDER, new File(basedb,"chaintree.db"));
//			MiniFile.deleteFileOrFolder(GeneralParams.DATA_FOLDER, new File(basedb,"userprefs.db"));
//			MiniFile.deleteFileOrFolder(GeneralParams.DATA_FOLDER, new File(basedb,"p2p.db"));
//			
//			//And will need to recreate the SSL
//			MiniFile.deleteFileOrFolder(GeneralParams.DATA_FOLDER, SSLManager.getSSLFolder());
//			
//			//Reset all the private and public keys
//			Wallet wallet = MinimaDB.getDB().getWallet();
//			
//			//Wipe the whole DB..
//			wallet.resetDB(phrase);
//			
//			//Now create all the keys..
//			wallet.initDefaultKeys(Wallet.NUMBER_GETADDRESS_KEYS, true);
//			
//			//Now save the Databases..
//			MinimaDB.getDB().saveSQL();
//			
//			//Don't do the usual shutdown hook
//			Main.getInstance().setHasShutDown();
//			
//			//And NOW shut down..
//			Main.getInstance().stopMessageProcessor();
//			
//			ret.put("response", "All private keys restored!");
			
		}else {
			throw new CommandException("Invalid action : "+action);
		}
		
		return ret;
	}

	/**
	 * Check that all keys are created
	 */
	public static void checkAllKeysCreated() throws CommandException {
		
		//Are they all created..
		if(Main.getInstance().getAllKeysCreated()) {
			return;
		}
		
		throw new CommandException("Please wait for ALL your keys to be created. "
				+ "This can take 5 mins. "
				+ "Currently ("+Main.getInstance().getAllDefaultKeysSize()+"/"+Wallet.NUMBER_GETADDRESS_KEYS+")");
	}
	
	public static void passwordLockDB(String zPassword) throws CommandException {
		
		//Display the base seed..
		SeedRow base = MinimaDB.getDB().getWallet().getBaseSeed();
				
		//Are we already locked..
		if(!MinimaDB.getDB().getWallet().isBaseSeedAvailable()) {
			throw new CommandException("DB already locked!");
		}
		
		//Get the Seed phrase
		MiniString phrase = new MiniString(base.getPhrase());
		
		//Convert to data
		MiniData data = new MiniData(phrase.getData());
		
		//Encrypt it
		MiniData encrypted;
		try {
			encrypted = PasswordCrypto.encryptPassword(zPassword, data);
		} catch (Exception e) {
			throw new CommandException(e.toString());
		} 
		
		//Store it in the UserDB
		MinimaDB.getDB().getUserDB().setEncryptedSeed(encrypted);
		
		//Save UserDB
		MinimaDB.getDB().saveUserDB();
		
		//Wipe the private keys (but keep the public keys, modifiers etc)
		try {
			MinimaDB.getDB().getWallet().wipeBaseSeedRow();
		} catch (SQLException e) {
			throw new CommandException(e.toString());
		}
	}
	
	public static void passowrdUnlockDB(String zPassword) throws CommandException {
		
		//Are we already locked..
		if(MinimaDB.getDB().getWallet().isBaseSeedAvailable()) {
			throw new CommandException("DB already unlocked!");
		}
		
		//Get the encrypted data
		MiniData encrypted = MinimaDB.getDB().getUserDB().getEncryptedSeed();
		
		//Decrypt it..
		MiniData decrypt = null;
		try {
			decrypt = PasswordCrypto.decryptPassword(zPassword, encrypted);
		}catch(Exception exc) {
			throw new CommandException("Incorrect password!");
		}
		
		//Create the string
		String initphrase = new MiniString(decrypt.getBytes()).toString();
		
		//Convert to a data hash
		MiniData seed = BIP39.convertStringToSeed(initphrase);
		
		//And now recreate the private keys
		boolean ok = MinimaDB.getDB().getWallet().resetBaseSeedPrivKeys(initphrase, seed.to0xString());

		if(!ok) {
			throw new CommandException("Error updating Private keys.. please try again");
		}

		MinimaDB.getDB().getUserDB().setEncryptedSeed(MiniData.ZERO_TXPOWID);
		
		//Save UserDB
		MinimaDB.getDB().saveUserDB();
	}
	
	@Override
	public Command getFunction() {
		return new vault();
	}
}
