package org.minima.system.commands.base;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.archive.ArchiveManager;
import org.minima.objects.base.MiniData;
import org.minima.system.Main;
import org.minima.system.commands.Command;
import org.minima.utils.json.JSONObject;

public class test extends Command {

	public test() {
		super("test","test Funxtion");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"show"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
	
		boolean show = getBooleanParam("show", true);
		
		//Send a notification
		JSONObject notification = new JSONObject();
		notification.put("uid", "0x01");
		notification.put("title", "My Title");
		notification.put("text", "My text");
		notification.put("show", show);
		
		//Post it
		Main.getInstance().PostNotifyEvent("NOTIFICATION", notification);
		
		ret.put("response", notification);
	
		return ret;
	}
	
	@Override
	public Command getFunction() {
		return new test();
	}

	public static void main(String[] zArgs) {
		
		for(int i=0;i<512;i++) {
			
			MiniData data = new MiniData(new BigInteger(Integer.toString(i)));
			
			System.out.println(data.to0xString());
			
		}
		
		
		
	}
}
