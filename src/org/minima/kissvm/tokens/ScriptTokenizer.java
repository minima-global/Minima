package org.minima.kissvm.tokens;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.functions.MinimaFunction;

public class ScriptTokenizer {

	/**
	 * Main Commands
	 */
	public static final String[] TOKENS_COMMAND     = 
		{"LET",
		 "IF","THEN","ELSEIF","ELSE","ENDIF",
		 "RETURN",
		 "ASSERT",
		 "WHILE","DO","ENDWHILE",
		 "EXEC",
		 "MAST"};
	
	/**
	 * Number operators - << and >> and dealt with separately
	 */
	public static final String[] TOKENS_NUMBER_OPERATOR = 
		{"+","-","/","*","%","&","|","^","="};

	/**
	 * Boolean Operators
	 */
	public static final String[] TOKENS_BOOLEAN_OPERATOR = 
		{"LT","LTE","GT","GTE","EQ","NEQ",
		 "XOR","AND","OR",
		 "NXOR","NAND","NOR",
		 "NOT","NEG"};
	public static final List<String> BOOLEAN_TOKENS_LIST  = Arrays.asList(TOKENS_BOOLEAN_OPERATOR);
	
	/**
	 * Words end when they encounter 
	 */
	public static final String[] TOKENS_ENDOFWORD   = 
		{"+","-","/","*","%","&","|","^","=","(",")","[","]","<",">","<<",">>"};
	public static final List<String> mAllEOW  = Arrays.asList(TOKENS_ENDOFWORD);
	
	/**
	 * The script we are tokenizing
	 */
	StringBuffer mScript;
	
	/**
	 * Position and length
	 */
	int mPos;
	int mLength; 
	
	boolean mCaseInsensitive = false;
	
	public ScriptTokenizer(String zScript) {
		this(zScript, false);
	}
	
	public ScriptTokenizer(String zScript, boolean zCaseInsensitive) {
		mScript = new StringBuffer(zScript);
		mPos    = 0;
		mLength = mScript.length();
		mCaseInsensitive = zCaseInsensitive;
	}
	
	
	public String getNextWord() {
		String word = "";
		
		//Get all the characters up to the next End Of Word symbol..
		while(mPos<mLength){
			//get the next Character
			String c = Character.toString(mScript.charAt(mPos));
			
			//Is it an end of Word..
			if(mAllEOW.contains(c) || isWhiteSpace(c)) {
				break;
			}
			
			//Add to the word
			word += mScript.charAt(mPos);
			mPos++;
		}
		
		return word;
	}
	
	public static boolean isNumeric(String zWord){
		return zWord.matches("^[0-9]+(\\.[0-9]+)?");  //match a number with optional '-' and decimal.
	}
	
	public static boolean isHex(String zWord){
		return zWord.matches("0x[0-9a-fA-F]+");
	}
	
	public static boolean isVariable(String zWord){
		return zWord.matches("[a-z]+");
	}
	
	public static boolean isGlobal(String zWord){
		return zWord.matches("@[A-Z]+");
	}
	
	public static boolean isWhiteSpace(String zWord) {
		return zWord.matches("\\s+");
	}
	
	public ArrayList<ScriptToken> tokenize() throws MinimaParseException{
		ArrayList<ScriptToken> tokens = new ArrayList<ScriptToken>();
		
		//Get the defaults..
		List<String> allcommands  	= Arrays.asList(TOKENS_COMMAND);
		List<String> allnumops 		= Arrays.asList(TOKENS_NUMBER_OPERATOR);
		List<String> allboolops 	= Arrays.asList(TOKENS_BOOLEAN_OPERATOR);
		
		List<String> allfunctions 	= new ArrayList<>();
		for(MinimaFunction func : MinimaFunction.ALL_FUNCTIONS) {
			allfunctions.add(func.getName());
		}
		
		//Now run through..
		mPos = 0;
		boolean waslastspace = true;
		while(mPos<mLength) {
			//Get the next symbol..
			String nextchar = Character.toString(mScript.charAt(mPos));
			
			//space check..
			boolean wasspace = false;
			if(isWhiteSpace(nextchar)) {
				//ignore and move on..
				mPos++;
				wasspace = true;
				
				//Is it a one character operator
			}else if(allnumops.contains(nextchar)) {
				tokens.add(new ScriptToken(ScriptToken.TOKEN_OPERATOR, nextchar));
				mPos++;
			
				//Is it >> or <<
			}else if(nextchar.equals("<")) {
				String testchar = Character.toString(mScript.charAt(mPos+1));
				if(!testchar.equals("<")) {
					throw new MinimaParseException("Incorrect Token found @ "+mPos+" "+nextchar+testchar);
				}
				tokens.add(new ScriptToken(ScriptToken.TOKEN_OPERATOR, "<<"));
				mPos+=2;
			
			}else if(nextchar.equals(">")) {
				String testchar = Character.toString(mScript.charAt(mPos+1));
				if(!testchar.equals(">")) {
					throw new MinimaParseException("Incorrect Token found @ "+mPos+" "+nextchar+testchar);
				}
				tokens.add(new ScriptToken(ScriptToken.TOKEN_OPERATOR, ">>"));
				mPos+=2;
			
				//Is it a bracket
			}else if(nextchar.equals("(")) {
				tokens.add(new ScriptToken(ScriptToken.TOKEN_OPENBRACKET, nextchar));
				mPos++;
			}else if(nextchar.equals(")")) {
				tokens.add(new ScriptToken(ScriptToken.TOKEN_CLOSEBRACKET, nextchar));
				mPos++;

				//Is it a SQUARE bracket
			}else if(nextchar.equals("[")) {
				//Ok get to the end of this sequence..
				String str  = "";
				int sq		= 0;
				while(mPos<mLength) {
					nextchar = Character.toString(mScript.charAt(mPos));
					str 	+= nextchar;
					
					if(nextchar.equals("[")) {
						sq++;
					}else if(nextchar.equals("]")) {
						sq--;
						if(sq==0) {
							mPos++;
							break;
						}
					}
					mPos++;
				}
				
				//It's a String
				tokens.add(new ScriptToken(ScriptToken.TOKEN_VALUE, str));
				
				//Is it a function param
			}else if(nextchar.equals("$")) {
				//get the next word..
				String word = getNextWord();
				
				//It's a function param
				tokens.add(new ScriptToken(ScriptToken.TOKEN_FUNCTIIONPARAM, word));
				
			}else{
				//get the next word..
				String word = getNextWord();
				
				//Uppercase..Lowercase - for cleanscript..
				String uppercase = word;
				String lowercase = word;
				if(mCaseInsensitive) {
					uppercase = word.toUpperCase();
					lowercase = word.toLowerCase();
				}
				
					//What is it..
				if(allcommands.contains(uppercase)) {
					//Must have a space before a command word
					if(!waslastspace) {
						throw new MinimaParseException("Missing space before Command @ "+mPos+" "+uppercase);
					}
					
					//It's a command
					tokens.add(new ScriptToken(ScriptToken.TOKEN_COMMAND, uppercase));
				
				}else if(allfunctions.contains(uppercase)) {
					//It's a function
					tokens.add(new ScriptToken(ScriptToken.TOKEN_FUNCTIION, uppercase));
				
				}else if(allboolops.contains(uppercase)) {
					//It's a function
					tokens.add(new ScriptToken(ScriptToken.TOKEN_OPERATOR, uppercase));
				
				}else if(isNumeric(word) || isHex(word)) {
					//It's a number
					tokens.add(new ScriptToken(ScriptToken.TOKEN_VALUE, word));
				
				}else if(uppercase.equals("TRUE")) {
					//It's a number
					tokens.add(new ScriptToken(ScriptToken.TOKEN_TRUE, uppercase));
				
				}else if(uppercase.equals("FALSE")) {
					//It's a number
					tokens.add(new ScriptToken(ScriptToken.TOKEN_FALSE, uppercase));
				
				}else if(isGlobal(uppercase)) {
					//It's a global
					tokens.add(new ScriptToken(ScriptToken.TOKEN_GLOBAL, uppercase));
					
				}else if(isVariable(lowercase)) {
					//Check length
					if(lowercase.length() > 32) {
						throw new MinimaParseException("MAX Variable length is 32 @ "+mPos+" "+word);
					}
					
					//It's a number
					tokens.add(new ScriptToken(ScriptToken.TOKEN_VARIABLE, lowercase));
				
				}else {
					throw new MinimaParseException("Incorrect Token found @ "+mPos+" "+word);
				}
			}
			
			//Save this - commands have to have this as true
			waslastspace = wasspace;
		}
		
		return tokens;
	}
	
}
