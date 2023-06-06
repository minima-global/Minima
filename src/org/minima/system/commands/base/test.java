package org.minima.system.commands.base;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;

import org.minima.objects.base.MiniData;
import org.minima.system.commands.Command;
import org.minima.utils.MinimaLogger;
import org.minima.utils.json.JSONObject;

public class test extends Command {

	public test() {
		super("test","test Funxtion");
	}
	
	@Override
	public ArrayList<String> getValidParams(){
		return new ArrayList<>(Arrays.asList(new String[]{"show","action"}));
	}
	
	@Override
	public JSONObject runCommand() throws Exception {
		JSONObject ret = getJSONReply();
	
		String file = "minihub/index.html";
		
		//Load a resource..
		InputStream is 		 	= getFileFromResourceAsStream(file);
		InputStreamReader ir 	= new InputStreamReader(is);
		BufferedReader br 		= new BufferedReader(ir);

		String line = null;
		while((line = br.readLine()) != null) {			
			MinimaLogger.log(line);
		}
		
		
		return ret;
	}
	
	// get a file from the resources folder
    // works everywhere, IDEA, unit test and JAR file.
    private InputStream getFileFromResourceAsStream(String fileName) {

        // The class loader that loaded the class
        ClassLoader classLoader = getClass().getClassLoader();
        InputStream inputStream = classLoader.getResourceAsStream(fileName);

        // the stream holding the file content
        if (inputStream == null) {
            MinimaLogger.log("file not found! " + fileName);
        }
            
        return inputStream;

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