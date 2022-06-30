package org.minima.system.commands.base;

import java.util.ArrayList;

import org.minima.database.MinimaDB;
import org.minima.database.archive.ArchiveManager;
import org.minima.objects.TxBlock;
import org.minima.objects.TxPoW;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.system.network.maxima.MaximaManager;
import org.minima.system.network.minima.NIOClient;
import org.minima.system.network.minima.NIOClientInfo;
import org.minima.system.network.minima.NIOManager;
import org.minima.system.network.minima.NIOMessage;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;
import org.minima.utils.messages.Message;

public class test extends Command {

	public test() {
		super("test","test Funxtion");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		Message mls = new Message(MaximaManager.MAXIMA_CHECK_MLS);
		mls.addBoolean("force", true);
		Main.getInstance().getMaxima().PostMessage(mls);
		
		ret.put("response", "Checking MLS servers..");
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new test();
	}

}
