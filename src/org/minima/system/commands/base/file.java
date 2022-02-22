package org.minima.system.commands.base;

import java.io.File;

import org.minima.objects.base.MiniData;
import org.minima.objects.base.MiniString;
import org.minima.system.commands.Command;
import org.minima.system.commands.CommandException;
import org.minima.system.params.GeneralParams;
import org.minima.utils.Crypto;
import org.minima.utils.MiniFile;
import org.minima.utils.json.JSONArray;
import org.minima.utils.json.JSONObject;

public class file extends Command {

	public file() {
		super("file","[action:list|load|save|delete] [file:] (data:)- Load, save, delete or list files. / is root. Data is in 0xHEX format");
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
		
		String action 	= getParam("action");
		String thefile 	= getParam("file");
		
		//Check no back..
		if(thefile.indexOf("..")!=-1) {
			throw new CommandException("No .. parent allowed in filename");
		}
		
		//The Root..
		File root = new File(GeneralParams.DATA_FOLDER,"minifiles");
		if(!root.exists()) {
			root.mkdir();
		}
		
		//Which file are we listing..
		File touchfile = new File(root,thefile);
		
		if(action.equals("list")) {
		
			//Get the list of files..
			JSONArray allfiles = new JSONArray();
			File[] files = touchfile.listFiles();
			if(files!=null) {
				
				for(File ff : files) {
					
					JSONObject fjson = new JSONObject();
					fjson.put("name", ff.getName());
					fjson.put("directory", ff.isDirectory());
					fjson.put("size", ff.length());
					
					allfiles.add(fjson);
				}
			}
			
			//Now get the details..
			JSONObject resp = new JSONObject();
			resp.put("found", allfiles.size());
			resp.put("files", allfiles);
			ret.put("response", resp);
		
		}else if(action.equals("save")) {
			
			//Make sure the parent exists..
			File parent = touchfile.getParentFile();
			parent.mkdirs();
			
			//Get the adta param..
			MiniData data = getDataParam("data");
			
			//Save this data..
			MiniFile.writeDataToFile(touchfile, data.getBytes());
			
			//Now get the details..
			JSONObject resp = new JSONObject();
			resp.put("name", thefile);
			resp.put("size", touchfile.length());
			ret.put("response", resp);
		
		}else if(action.equals("load")) {
			
			//Load the file..
			byte[] datafile = MiniFile.readCompleteFile(touchfile);
			
			//The MinData object
			MiniData filedata = new MiniData(datafile);
			
			//Now get the details..
			JSONObject resp = new JSONObject();
			resp.put("name", thefile);
			resp.put("size", touchfile.length());
			resp.put("data", filedata.to0xString());
			ret.put("response", resp);
		
		}else if(action.equals("delete")) {
			
			//Delete the file..
			MiniFile.deleteFileOrFolder(root.getAbsolutePath(), touchfile);
			
			//Now get the details..
			JSONObject resp = new JSONObject();
			resp.put("name", thefile);
			resp.put("status", "deleted");
			ret.put("response", resp);
			
		}else {
			throw new CommandException("Invalid file function : "+action);
		}
		
		return ret;
	}

	@Override
	public Command getFunction() {
		return new file();
	}

}
