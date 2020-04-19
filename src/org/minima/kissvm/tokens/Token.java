/**
 * 
 */
package org.minima.kissvm.tokens;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.StringTokenizer;

import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.expressions.GlobalExpression;
import org.minima.kissvm.functions.MinimaFunction;

/**
 * @author Spartacus Rex
 *
 */
public class Token {

	public static final int TOKEN_COMMAND 		    = 0;
	public static final String[] TOKENS_COMMAND     = 
					{"LET",
					 "IF","THEN","ELSEIF","ELSE","ENDIF",
					 "RETURN",
					 "ASSERT",
					 "WHILE","DO","ENDWHILE",
					 "EXEC",
					 "MAST"};

	public static final int TOKEN_FUNCTIION 		= 1;
	
	public static final int TOKEN_OPERATOR  		= 2;
	public static final String[] TOKENS_OPERATOR = 
				{"+","-","/","*","%","&","|","^",">>","<<","=",
				 "LT","GT","GTE","LTE","EQ","NEQ","NEG","XOR",
				 "AND","OR","NXOR","NAND","NOR","NOT"};

	public static final int TOKEN_VALUE  			= 3;
	public static final int TOKEN_VARIABLE  		= 4;
	public static final int TOKEN_GLOBAL   			= 6;
	
	public static final int TOKEN_OPENBRACKET   	= 7;
	public static final int TOKEN_CLOSEBRACKET   	= 8;
	
	public static final int TOKEN_TRUE   			= 9;
	public static final int TOKEN_FALSE   			= 10;
	
	
	private int 	mTokenType;
	private String 	mToken;
	
	public Token(int zTokenType, String zToken) {
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
	
	/**
	 * Tokenize a MiniScript into a list of Tokens
	 * @param zMiniScript
	 * @return A list of all the tokens
	 * @throws MinimaParseException 
	 */
	public static List<Token> tokenize(String zMiniScript) throws MinimaParseException{
		List<Token> tokens = new ArrayList<Token>();
		
		//Get the defaults..
		List<String> allcommands  = Arrays.asList(TOKENS_COMMAND);
		List<String> alloperators = Arrays.asList(TOKENS_OPERATOR);
		
		List<String> allfunctions = new ArrayList<>();
		for(MinimaFunction func : MinimaFunction.ALL_FUNCTIONS) {
			allfunctions.add(func.getName());
		}
		
		//Quote are represented as [ and ] for ease of parsing.
		QuotedString qs = new QuotedString(zMiniScript);
		
		//Now run through..
		StringTokenizer strtok = new StringTokenizer(qs.getDeQuotedString()," ");
		while(strtok.hasMoreTokens()) {
			String tok = strtok.nextToken().trim();
			
			if(allcommands.contains(tok)) {
				tokens.add(new Token(TOKEN_COMMAND, tok));
				
			}else if(allfunctions.contains(tok)){
				tokens.add(new Token(TOKEN_FUNCTIION, tok));
			
			}else if(alloperators.contains(tok)){
				tokens.add(new Token(TOKEN_OPERATOR, tok));
			
			}else if(tok.startsWith(":")) {
				try {
					//which quote
					int quote = Integer.parseInt(tok.substring(1));
					
					//Add THAT entire quote
					tokens.add(new Token(TOKEN_VALUE, qs.getQuote(quote)));
					
				}catch(NumberFormatException exc) {
					throw new MinimaParseException("Incorrect Number Format in quoted parse : "+tok);
				}
				
			}else if(tok.startsWith("0x") || isNumeric(tok)) {
				tokens.add(new Token(TOKEN_VALUE, tok));
								
			}else if(tok.equals("(")) {
				tokens.add(new Token(TOKEN_OPENBRACKET, tok));
				
			}else if(tok.equals(")")) {
				tokens.add(new Token(TOKEN_CLOSEBRACKET, tok));
			
			}else if(tok.equals("TRUE")) {
				tokens.add(new Token(TOKEN_TRUE, tok));
				
			}else if(tok.equals("FALSE")) {
				tokens.add(new Token(TOKEN_FALSE, tok));
			
			}else if(tok.startsWith("@")) {
				//It's a Global parameter.. 
				tokens.add(new Token(TOKEN_GLOBAL, tok));
			
			}else if(isVariable(tok)){
				if(tok.length()>16) {
					throw new MinimaParseException("MAX variable name length is 16 : "+tok);	
				}
				
				tokens.add(new Token(TOKEN_VARIABLE, tok));
			}else{
				throw new MinimaParseException("Incorrect token in parse : "+tok);
			}
		}
		
		return tokens;
	}
	
	public static boolean isNumeric(String str){
	  return str.matches("-?\\d+(\\.\\d+)?");  //match a number with optional '-' and decimal.
	}
	
	public static boolean isVariable(String str){
		 return str.matches("[a-z]*"); 
	}
	
	
	public static void main(String[] zArgs) {
		String text = "asFg";
		System.out.println(text.matches("[a-z]*"));
	}
}
