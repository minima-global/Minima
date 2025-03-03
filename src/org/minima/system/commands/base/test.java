package org.minima.system.commands.base;

import java.io.InputStream;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;

import org.minima.database.MinimaDB;
import org.minima.database.txpowtree.TxPowTree;
import org.minima.objects.Address;
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
	
		//Wanna check the chaintree
		TxPowTree tree = MinimaDB.getDB().getTxPoWTree();
		
		MinimaLogger.log("Length : "+tree.getSize());
				
		
		
		
		
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
		
		System.out.println("Start test..");
		
		for(int i=0;i<100000;i++) {
			
			MiniData data = MiniData.getRandomData(32);
			
			String add = Address.makeMinimaAddress(data);
			int len = add.length(); 
			
			if(len != 63) {
				System.out.println(len+" "+add+" "+data.to0xString());
				//System.out.println("NOT 63! "+);
			}
			
		}
		
		System.out.println("Finish test..");
		
		
		
	}
}