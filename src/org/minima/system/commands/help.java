package org.minima.system.commands;

import org.minima.system.commands.base.automine;
import org.minima.system.commands.base.backup;
import org.minima.system.commands.base.balance;
import org.minima.system.commands.base.coinexport;
import org.minima.system.commands.base.coinimport;
import org.minima.system.commands.base.cointrack;
import org.minima.system.commands.base.hash;
import org.minima.system.commands.base.hashtest;
import org.minima.system.commands.base.incentivecash;
import org.minima.system.commands.base.mmrcreate;
import org.minima.system.commands.base.mmrproof;
import org.minima.system.commands.base.newaddress;
import org.minima.system.commands.base.printtree;
import org.minima.system.commands.base.quit;
import org.minima.system.commands.base.restore;
import org.minima.system.commands.base.runscript;
import org.minima.system.commands.base.scripts;
import org.minima.system.commands.base.send;
import org.minima.system.commands.base.status;
import org.minima.system.commands.base.tokencreate;
import org.minima.system.commands.base.tokens;
import org.minima.system.commands.base.trace;
import org.minima.system.commands.base.tutorial;
import org.minima.system.commands.network.connect;
import org.minima.system.commands.network.disconnect;
import org.minima.system.commands.network.maxima;
import org.minima.system.commands.network.message;
import org.minima.system.commands.network.network;
import org.minima.system.commands.network.rpc;
import org.minima.system.commands.network.sshtunnel;
import org.minima.system.commands.network.webhooks;
import org.minima.system.commands.persistent.file;
import org.minima.system.commands.search.coins;
import org.minima.system.commands.search.keys;
import org.minima.system.commands.search.txpow;
import org.minima.system.commands.signatures.sign;
import org.minima.system.commands.signatures.verify;
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
		super("help","Show Help. [] are required. () are optional. Chain multiple commands with ;");
	}
	
	@Override
	public JSONObject runCommand() throws Exception{
		JSONObject ret = getJSONReply();
		
		JSONObject details = new JSONObject();
		
		addCommand(details, new help());
		
		addCommand(details, new status());
		addCommand(details, new printtree());
		addCommand(details, new trace());
		addCommand(details, new automine());
		addCommand(details, new hashtest());
//		addCommand(details, new debugflag());
		
		addCommand(details, new txpow());
		addCommand(details, new coins());
		addCommand(details, new tokens());
		addCommand(details, new keys());
		
		addCommand(details, new newaddress());
		addCommand(details, new send());
		addCommand(details, new balance());
		addCommand(details, new tokencreate());
		addCommand(details, new hash());
		addCommand(details, new file());
		
		addCommand(details, new scripts());
		addCommand(details, new runscript());
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
		addCommand(details, new message());
		addCommand(details, new connect());
		addCommand(details, new disconnect());
		addCommand(details, new rpc());
		addCommand(details, new webhooks());
		addCommand(details, new sshtunnel());
		
		addCommand(details, new backup());
		addCommand(details, new restore());
		addCommand(details, new incentivecash());
		
		addCommand(details, new quit());
		
		ret.put("response", details);
		
		return ret;
	}

	
	private void addCommand(JSONObject zDetails, Command zCommand) {
		zDetails.put(getStrOfLength(15,zCommand.getname()), zCommand.getHelp());
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
