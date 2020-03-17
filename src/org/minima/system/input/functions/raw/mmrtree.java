package org.minima.system.input.functions.raw;

import java.util.ArrayList;

import org.minima.database.mmr.MMRData;
import org.minima.database.mmr.MMREntry;
import org.minima.database.mmr.MMRProof;
import org.minima.database.mmr.MMRSet;
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
		setHelp("[ LIST_OF_DATA ]", "Create an MMR Tree from the data list, which can be # split", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		//Get a response message
		Message msg = getResponseMessage(ConsensusUser.CONSENSUS_MMRTREE);
		
		//Get all of the input params..
		ArrayList<MiniString> data = new ArrayList<>();
		for(int i=1;i<zInput.length;i++) {
			data.add(new MiniString(zInput[i]));			
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
