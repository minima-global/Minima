package org.minima.system.commands;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.system.commands.backup.archive;
import org.minima.system.commands.backup.backup;
import org.minima.system.commands.backup.restore;
import org.minima.system.commands.backup.restoresync;
import org.minima.system.commands.backup.vault;
import org.minima.system.commands.base.balance;
import org.minima.system.commands.base.block;
import org.minima.system.commands.base.burn;
import org.minima.system.commands.base.checkaddress;
import org.minima.system.commands.base.coinexport;
import org.minima.system.commands.base.coinimport;
import org.minima.system.commands.base.cointrack;
import org.minima.system.commands.base.consolidate;
import org.minima.system.commands.base.convert;
import org.minima.system.commands.base.getaddress;
import org.minima.system.commands.base.hash;
import org.minima.system.commands.base.hashtest;
import org.minima.system.commands.base.incentivecash;
import org.minima.system.commands.base.logs;
import org.minima.system.commands.base.maths;
import org.minima.system.commands.base.mmrcreate;
import org.minima.system.commands.base.mmrproof;
import org.minima.system.commands.base.newaddress;
import org.minima.system.commands.base.printtree;
import org.minima.system.commands.base.quit;
import org.minima.system.commands.base.random;
import org.minima.system.commands.base.status;
import org.minima.system.commands.base.tokencreate;
import org.minima.system.commands.base.tokenvalidate;
import org.minima.system.commands.base.trace;
import org.minima.system.commands.maxima.maxcontacts;
import org.minima.system.commands.maxima.maxcreate;
import org.minima.system.commands.maxima.maxima;
import org.minima.system.commands.maxima.maxsign;
import org.minima.system.commands.maxima.maxverify;
import org.minima.system.commands.mds.checkpending;
import org.minima.system.commands.mds.mds;
import org.minima.system.commands.network.connect;
import org.minima.system.commands.network.disconnect;
import org.minima.system.commands.network.message;
import org.minima.system.commands.network.network;
import org.minima.system.commands.network.rpc;
import org.minima.system.commands.network.webhooks;
import org.minima.system.commands.scripts.newscript;
import org.minima.system.commands.scripts.removescript;
import org.minima.system.commands.scripts.runscript;
import org.minima.system.commands.scripts.scripts;
import org.minima.system.commands.search.coins;
import org.minima.system.commands.search.history;
import org.minima.system.commands.search.keys;
import org.minima.system.commands.search.tokens;
import org.minima.system.commands.search.txpow;
import org.minima.system.commands.send.multisig;
import org.minima.system.commands.send.send;
import org.minima.system.commands.send.sendnosign;
import org.minima.system.commands.send.sendpoll;
import org.minima.system.commands.send.sendpost;
import org.minima.system.commands.send.sendsign;
import org.minima.system.commands.send.sendview;
import org.minima.system.commands.signatures.sign;
import org.minima.system.commands.signatures.verify;
import org.minima.system.commands.txn.txnbasics;
import org.minima.system.commands.txn.txncheck;
import org.minima.system.commands.txn.txnclear;
import org.minima.system.commands.txn.txncreate;
import org.minima.system.commands.txn.txndelete;
import org.minima.system.commands.txn.txnexport;
import org.minima.system.commands.txn.txnimport;
import org.minima.system.commands.txn.txninput;
import org.minima.system.commands.txn.txnlist;
import org.minima.system.commands.txn.txnoutput;
import org.minima.system.commands.txn.txnpost;
import org.minima.system.commands.txn.txnscript;
import org.minima.system.commands.txn.txnsign;
import org.minima.system.commands.txn.txnstate;
import org.minima.utils.json.JSONObject;

public class help extends Command {

	public help() {
		super("help","Show Help. [] are required. () are optional. Use 'help command:' for full help. Chain multiple commands with ;");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"command"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		JSONObject details = new JSONObject();
		
		String command = getParam("command","");
		
		if(!command.equals("")) {
		
			//Get the command..
			Command cmd = Command.getCommandOnly(command);
			if(cmd == null) {
				throw new CommandException("Command not found : "+command);
			}
			
			//Otherwise get the Full help..
			details.put("command", command);
			details.put("help", cmd.getHelp());
			details.put("fullhelp", cmd.getFullHelp());
			
		}else{

			addCommand(details, new help());
			
			addCommand(details, new whitepaper());
			
			addCommand(details, new status());
			addCommand(details, new block());
			addCommand(details, new printtree());
			addCommand(details, new burn());
			addCommand(details, new trace());
			addCommand(details, new logs());
			addCommand(details, new hashtest());
			addCommand(details, new checkaddress());
			
			addCommand(details, new history());
			addCommand(details, new txpow());
			addCommand(details, new coins());
			addCommand(details, new tokens());
			addCommand(details, new keys());
	
			addCommand(details, new getaddress());
			addCommand(details, new newaddress());
			addCommand(details, new send());
			addCommand(details, new sendpoll());
			
			addCommand(details, new sendnosign());
			addCommand(details, new sendview());
			addCommand(details, new sendsign());
			addCommand(details, new sendpost());
			addCommand(details, new multisig());
			
			addCommand(details, new balance());
			addCommand(details, new tokencreate());
			addCommand(details, new tokenvalidate());
			addCommand(details, new consolidate());
			
			addCommand(details, new hash());
			addCommand(details, new random());
			addCommand(details, new convert());
			addCommand(details, new maths());
			
			addCommand(details, new scripts());
			addCommand(details, new newscript());
			addCommand(details, new runscript());
			addCommand(details, new removescript());
			addCommand(details, new tutorial());
			
			addCommand(details, new mmrcreate());
			addCommand(details, new mmrproof());
			
			addCommand(details, new coinimport());
			addCommand(details, new coinexport());
			addCommand(details, new cointrack());
			
			addCommand(details, new sign());
			addCommand(details, new verify());
			
			addCommand(details, new txnlist());
			addCommand(details, new txncreate());
			addCommand(details, new txnbasics());
			addCommand(details, new txndelete());
			addCommand(details, new txncheck());
			addCommand(details, new txninput());
			addCommand(details, new txnoutput());
			addCommand(details, new txnstate());
			addCommand(details, new txnscript());
			addCommand(details, new txnsign());
			addCommand(details, new txnclear());
			addCommand(details, new txnpost());
			addCommand(details, new txnimport());
			addCommand(details, new txnexport());
			
			addCommand(details, new network());
			addCommand(details, new maxima());
			addCommand(details, new maxcontacts());
			
			addCommand(details, new maxcreate());
			addCommand(details, new maxsign());
			addCommand(details, new maxverify());
			
			addCommand(details, new message());
			addCommand(details, new connect());
			addCommand(details, new disconnect());
			addCommand(details, new rpc());
			addCommand(details, new webhooks());
			
			addCommand(details, new mds());
			addCommand(details, new checkpending());
			
			addCommand(details, new backup());
			addCommand(details, new restore());
			addCommand(details, new restoresync());
			addCommand(details, new archive());
			addCommand(details, new vault());
			
			addCommand(details, new incentivecash());
	
			//addCommand(details, new nodecount());
			addCommand(details, new quit());
		}
		
		ret.put("response", details);
		
		return ret;
	}

	
	private void addCommand(JSONObject zDetails, Command zCommand) {
		zDetails.put(getStrOfLength(15,zCommand.getName()), zCommand.getHelp());
	}
	
	public String getStrOfLength(int zDesiredLen, String zString) {
		String ret = new String(zString);
		int len    = ret.length();
		
		//The same or longer
		if(len >= zDesiredLen) {
			return ret.substring(0, zDesiredLen);
		}
		
		//If Shorter add zeros
		for(int i=0;i< zDesiredLen-len;i++) {
			ret = ret.concat(" ");
		}
		
		return ret;
	}
	
	@Override
	public Command getFunction() {
		return new help();
	}

}
