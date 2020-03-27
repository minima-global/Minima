package org.minima.system.input.functions;

import java.util.ArrayList;

import org.minima.database.mmr.MMRData;
import org.minima.database.mmr.MMREntry;
import org.minima.database.mmr.MMRProof;
import org.minima.database.mmr.MMRSet;
import org.minima.miniscript.Contract;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniHash;
import org.minima.objects.base.MiniString;
import org.minima.system.brains.ConsensusBackup;
import org.minima.system.brains.ConsensusUser;
import org.minima.system.input.CommandFunction;
import org.minima.utils.MiniFormat;
import org.minima.utils.messages.Message;

public class mmrtree extends CommandFunction{

	public mmrtree() {
		super("mmrtree");
		setHelp("[script|hash] [ data_list ]", "Build an MMR Hash Tree from the data list", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Get a response message
		Message msg = getResponseMessage(ConsensusUser.CONSENSUS_MMRTREE);
		msg.addString("type", zInput[1].toLowerCase());
		
		//Get all of the input params.. clean and send..
		ArrayList<MiniString> data = new ArrayList<>();
		for(int i=2;i<zInput.length;i++) {
			data.add(new MiniString(Contract.cleanScript(zInput[i])));			
		}
		
		//Send a backup message - with no request to shutdown at the end..
		msg.addObject("leaves", data);
		getMainHandler().getConsensusHandler().PostMessage(msg);
	}
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new mmrtree();
	}
}
