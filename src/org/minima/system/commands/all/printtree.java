package org.minima.system.commands.all;

import java.math.BigDecimal;

import org.minima.database.MinimaDB;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class printtree extends Command {

	public printtree() {
		super("printtree", "Print a tree representation of the blockchain");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		//Print it..
		MinimaDB.getDB().getCascade().printCascade();
		MinimaDB.getDB().getTxPoWTree().printTree();
		
		BigDecimal total = MinimaDB.getDB().getTxPoWTree().getRoot().getTotalWeight().add(MinimaDB.getDB().getCascade().getTotalWeight());
		
		System.out.println("Cascade Weight :"+MinimaDB.getDB().getCascade().getTotalWeight());
		System.out.println("Chain Weight   :"+MinimaDB.getDB().getTxPoWTree().getRoot().getTotalWeight());
		System.out.println("Total  Weight  :"+total);
		System.out.println();
		
		ret.put("message", "Printed to stdout");
		return ret;
	}

	@Override
	public Command getFunction() {
		// TODO Auto-generated method stub
		return new printtree();
	}

}
