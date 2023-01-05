package org.minima.kissvm.functions.string;

import java.util.ArrayList;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.tokens.ScriptToken;
import org.minima.kissvm.tokens.ScriptTokenizer;
import org.minima.kissvm.values.StringValue;
import org.minima.kissvm.values.Value;
import org.minima.utils.MinimaLogger;

/**
 * CLEAN a script to it's minimal correct representation
 * 
 * @author spartacusrex
 */
public class CLEAN extends MinimaFunction {

	public CLEAN() {
		super("CLEAN");
	}

	@Override
	public Value runFunction(Contract zContract) throws ExecutionException {
		checkExactParamNumber(requiredParams());
		
		//Get the STRING value
		StringValue preclean = zContract.getStringParam(0, this);
		
		//Now create a UTF8 String
		String newstr = cleanScript(preclean.toString());
		
		return new StringValue(newstr);	
	}
	
	@Override
	public int requiredParams() {
		return 1;
	}
	
	@Override
	public MinimaFunction getNewFunction() {
		return new CLEAN();
	}
	
	/**
	 * Clean up a script..
	 * 
	 * @param zScript
	 * @return The Converted Script
	 */
	public static String cleanScript(String zScript) {
		return cleanScript(zScript, false);
	}
	
	public static String cleanScript(String zScript, boolean zLog) {
		
		//The final result
		StringBuffer ret = new StringBuffer();
		
		//Remove all the excess white space
		String script = zScript.replaceAll("\\s+"," ").trim();
		
		//Remove all \
//		script = script.replaceAll("\\\\","").trim();
		
		//First CONVERT..
		ScriptTokenizer tokz = new ScriptTokenizer(script, true);
		try {
			//Get the list of Tokens..
			ArrayList<ScriptToken> tokens = tokz.tokenize();
		
			//Now add them correctly..
			boolean whites 		= true;
			ScriptToken prevtok = null;
			for(ScriptToken tok : tokens) {
				
				if(zLog) {
					MinimaLogger.log(tok.toString());
				}
				
				if(tok.getTokenType() == ScriptToken.TOKEN_COMMAND) {
					String command = tok.getToken();

					//Always a space before and after a COMMAND
					if(ret.toString().endsWith(" ")) {
						ret.append(command+" ");
					}else {
						ret.append(" "+command+" ");
					}
					
					whites = true;
					
				}else if(ScriptTokenizer.BOOLEAN_TOKENS_LIST.contains(tok.getToken())) {
					ret.append(" "+tok.getToken()+" ");
				
					whites = true;
					
				}else if(tok.getToken().startsWith("0x")) {
					String hex = "0x"+tok.getToken().substring(2).toUpperCase();
					
					if(whites) {
						ret.append(hex);
					}else {
						ret.append(" "+hex);
					}
					
					whites = false;
					
				}else if(tok.getToken().startsWith("(") || tok.getToken().startsWith("[")) {
					
					String strtok = tok.getToken();
					
					if(whites) {
						boolean isspacerequired = prevtok!=null 
								&& (prevtok.getToken().endsWith(")") || prevtok.getToken().endsWith("]"));
						
						if(isspacerequired) {
							ret.append(" "+strtok);
						}else {
							ret.append(strtok);
						}
						
					}else {
					
						boolean isspacerequired = prevtok!=null 
								&& (ScriptTokenizer.mAllEOW.contains(prevtok.getToken()));
						
						if(isspacerequired) {
							ret.append(" "+strtok);
						}else {
							ret.append(strtok);
						}
					}
					
					whites=true;
				
				}else {
					String strtok = tok.getToken();
					
					boolean islastclosebracket = prevtok!=null && (prevtok.getToken().endsWith(")") || prevtok.getToken().endsWith("]"));
					
					//Is it an end of word or whitespace..
					if(islastclosebracket && !ScriptTokenizer.mAllAFTER.contains(strtok)) {
						
						ret.append(" "+strtok);
						whites = false;
						
					}else if(ScriptTokenizer.isWhiteSpace(strtok) || ScriptTokenizer.mAllEOW.contains(strtok)) {
						ret.append(strtok);
						whites = true;
					}else {
						if(whites) {
							ret.append(strtok);
						}else {
							ret.append(" "+strtok);
						}
						whites = false;
					}
				}
				
				//Keep the last letter of the previous token
				prevtok = tok;
			}
		
		} catch (MinimaParseException e) {
			MinimaLogger.log("Clean Script Error @ "+zScript+" "+e);
			return zScript;
		}
		
		return ret.toString().trim();
	}
}
