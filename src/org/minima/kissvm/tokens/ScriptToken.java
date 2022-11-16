/**
 * 
 */
package org.minima.kissvm.tokens;

import java.util.ArrayList;

import org.minima.kissvm.exceptions.MinimaParseException;

/**
 * @author Spartacus Rex
 *
 */
public class ScriptToken {

	public static final int TOKEN_COMMAND 		    = 0;
	public static final int TOKEN_FUNCTIION 		= 1;
	public static final int TOKEN_OPERATOR  		= 2;
	public static final int TOKEN_VALUE  			= 3;
	public static final int TOKEN_VARIABLE  		= 4;
	public static final int TOKEN_GLOBAL   			= 6;
	public static final int TOKEN_OPENBRACKET   	= 7;
	public static final int TOKEN_CLOSEBRACKET   	= 8;
	public static final int TOKEN_TRUE   			= 9;
	public static final int TOKEN_FALSE   			= 10;
	public static final int TOKEN_FUNCTIIONPARAM 	= 11;
	
	private int 	mTokenType;
	private String 	mToken;
	
	public ScriptToken(int zTokenType, String zToken) {
		mTokenType 	= zTokenType;
		mToken		= zToken;
	}
		
	public int getTokenType() {
		return mTokenType;
	}

	public String getTokenTypeString() {
		switch (mTokenType) {
		case TOKEN_CLOSEBRACKET:
			return "CLOSEBRACKET";
		case TOKEN_COMMAND:
			return "COMMAND";
		case TOKEN_TRUE:
			return "TRUE";
		case TOKEN_FALSE:
			return "FALSE";
		case TOKEN_FUNCTIION:
			return "FUNCTION";
		case TOKEN_FUNCTIIONPARAM:
			return "FUNCTIONPARAM";
		case TOKEN_VALUE:
			return "VALUE";
		case TOKEN_OPENBRACKET:
			return "OPENBRACKET";
		case TOKEN_OPERATOR:
			return "OPERATOR";
		case TOKEN_VARIABLE:
			return "VARIABLE";
		case TOKEN_GLOBAL:
			return "GLOBAL";
		
		default:
			break;
		};
		
		return "null";
	}
	
	public String getToken() {
		return mToken;
	}
	
	@Override
	public String toString() {
		return getTokenTypeString()+":"+getToken();
	}
	
	/**
	 * Utility functions to fix tests
	 */
	public static ArrayList<ScriptToken> tokenize(String zScript) throws MinimaParseException {
		ScriptTokenizer tokz = new ScriptTokenizer(zScript);
        return tokz.tokenize();
	}
}
