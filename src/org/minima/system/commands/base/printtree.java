package org.minima.system.commands.base;

import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class printtree extends Command {

	public printtree() {
		super("printtree", "(depth:) (cascade:true|false) - Print a tree representation of the blockchain. Depth default 32, Cascade false.");
	}
	
	@Override
	public String getFullHelp() {
		return "\nprinttree\n"
				+ "\n"
				+ "Print a tree representation of the blockchain.\n"
				+ "\n"
				+ "Default depth 32 blocks, can be increased to see more of the txpow tree.\n"
				+ "\n"
				+ "Optionally show the cascading chain, default is false.\n"
				+ "\n"
				+ "depth: (optional)\n"
				+ "    Number of blocks back from the tip to show in the txpow tree.\n"
				+ "\n"
				+ "cascade: (optional)\n"
				+ "    true or false, true shows the cascade.\n"
				+ "\n"
				+ "Examples:\n"
				+ "\n"
				+ "printtree\n"
				+ "\n"
				+ "printtree depth:500\n"
				+ "\n"
				+ "printtree cascade:true\n";
				
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"depth","cascade"}));
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
