package org.minima.system.input.functions;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;

import org.minima.system.input.CommandFunction;
import org.minima.utils.MinimaLogger;

public class tutorial extends CommandFunction{

	public tutorial() {
		super("tutorial");
		setHelp("", "Explain Minima and go through Scripting!", "");
	}
	
	@Override
	public void doFunction(String[] zInput) throws Exception {
		InputStream in = getClass().getClassLoader().getResourceAsStream("org/minima/system/input/functions/tutorial.txt");
		BufferedReader br = new BufferedReader(new InputStreamReader(in));
		
		int linenumber=1;
		String line  = null;
        String total = "";
		while ( (line = br.readLine()) != null) {
            // do something with the line here
        	getResponseStream().getDataJSON().put(getLineNumber(linenumber++), line);
        	
        	total += line+"\n";
        }
        
		getResponseStream().hardEndStatus(total);
		
//        if(getResponseStream().isLocal()) {
//        	
//        }else {
//        	getResponseStream().endStatus(true, "");
//        }
	}
	
	public String getLineNumber(int zLine) {
		if(zLine<10) {
			return "00"+zLine;
		}else if(zLine<100) {
			return "0"+zLine;
		}
		
		return ""+zLine;
	}
	
	
	@Override
	public CommandFunction getNewFunction() {
		// TODO Auto-generated method stub
		return new tutorial();
	}
}
