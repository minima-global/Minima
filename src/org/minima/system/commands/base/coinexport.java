package org.minima.system.commands.base;

import org.minima.database.MinimaDB;
import org.minima.database.mmr.MMRProof;
import org.minima.database.txpowtree.TxPoWTreeNode;
import org.minima.objects.Coin;
import org.minima.objects.CoinProof;
import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniNumber;
import org.minima.system.brains.TxPoWSearcher;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.params.GeneralParams;
import org.minima.utils.json.JSONObject;

public class coinexport extends Command {

	public coinexport() {
		super("coinexport","[coinid:] - Export a coin");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String id = getParam("coinid");
		
		Coin coin = TxPoWSearcher.searchCoin(new MiniData(id));
		if(coin == null) {
			throw new CommandException("Coin not found coinid : "+id);
		}
		
		//When was it made.. 
		MiniNumber created = coin.getBlockCreated();
		
		//Now get the MMR proof.
		TxPoWTreeNode tip = MinimaDB.getDB().getTxPoWTree().getTip();
		
		//How far back shall we go..
		MiniNumber history = new MiniNumber(256);
		if(GeneralParams.TEST_PARAMS) {
			history = new MiniNumber(8);
		}
		
		MiniNumber back = tip.getBlockNumber().sub(history);
		if(back.isLess(created)) {
			back = created;
		}
		
		//Get that Node..
		TxPoWTreeNode mmrnode = tip.getPastNode(back);
		
		//Now get the MMR PRoof of this coin..
		MMRProof proof = mmrnode.getMMR().getProofToPeak(coin.getMMREntryNumber());
		
		//Create the CoinProof..
		CoinProof cp = new CoinProof(coin, proof);
		
		//And create the Data version
		MiniData dataproof = MiniData.getMiniDataVersion(cp);
		
		ret.put("response", dataproof.to0xString());
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new coinexport();
	}

}
