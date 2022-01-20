package org.minima.system.commands.base;

import org.minima.database.MinimaDB;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class printtree extends Command {

	public printtree() {
		super("printtree", "(depth:) (cascade:true|false) - Print a tree representation of the blockchain. Depth default 32, Cascade false.");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		JSONObject resp = new JSONObject();
		
		String depthstr = getParam("depth","32");
		int depth = Integer.parseInt(depthstr);
		
		boolean casc = getParam("cascade","false").equals("true");
		
		//Print it..
		if(casc) {
			String cascade = MinimaDB.getDB().getCascade().printCascade();
			resp.put("cascade", "\n"+cascade);
		}
		
		String treestr = MinimaDB.getDB().getTxPoWTree().printTree(depth);
		resp.put("chain", "\n"+treestr);
		
		ret.put("response", resp);
		
		return ret;
	}

	@Override
	public Command getFunction() {
		// TODO Auto-generated method stub
		return new printtree();
	}

}
