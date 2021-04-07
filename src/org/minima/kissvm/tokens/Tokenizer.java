package org.minima.kissvm.tokens;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.SyntaxException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.values.NumberValue;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;

public class Tokenizer {

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
	
	/**
	 * Words end when they encounter 
	 */
	public final String[] TOKENS_ENDOFWORD   = 
		{" ","+","-","/","*","%","&","|","^","=","(",")","[","]","<",">"};
	public final List<String> mAllEOW  = Arrays.asList(TOKENS_ENDOFWORD);
	
	/**
	 * The script we are tokenizing
	 */
	StringBuffer mScript;
	
	/**
	 * Position and length
	 */
	int mPos;
	int mLength; 
	
	public Tokenizer(String zScript) {
		mScript = new StringBuffer(zScript);
		mPos    = 0;
		mLength = mScript.length();
	}
	
	
	public String getNextWord() {
		String word = "";
		
		//Get all the characters up to the next End Of Word symbol..
		while(mPos<mLength){
			//get the next Character
			String c = Character.toString(mScript.charAt(mPos));
			
			//Is it an end of Word..
			if(mAllEOW.contains(c)) {
				break;
			}
			
			//Add to the word
			word += mScript.charAt(mPos);
			mPos++;
		}
		
		return word;
	}
	
	public static boolean isNumber(String zWord){
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
	
	public ArrayList<Token> tokenize() throws SyntaxException{
		ArrayList<Token> tokens = new ArrayList<Token>();
		
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
			if(nextchar.equals(" ")) {
				//ignore and move on..
				mPos++;
				wasspace = true;
				
				//Is it a one character operator
			}else if(allnumops.contains(nextchar)) {
				tokens.add(new Token(Token.TOKEN_OPERATOR, nextchar));
				mPos++;
			
				//Is it >> or <<
			}else if(nextchar.equals("<")) {
				String testchar = Character.toString(mScript.charAt(mPos+1));
				if(!testchar.equals("<")) {
					throw new SyntaxException("Incorrect Token found @ "+mPos+" "+nextchar+testchar);
				}
				tokens.add(new Token(Token.TOKEN_OPERATOR, "<<"));
				mPos+=2;
			
			}else if(nextchar.equals(">")) {
				String testchar = Character.toString(mScript.charAt(mPos+1));
				if(!testchar.equals(">")) {
					throw new SyntaxException("Incorrect Token found @ "+mPos+" "+nextchar+testchar);
				}
				tokens.add(new Token(Token.TOKEN_OPERATOR, ">>"));
				mPos+=2;
			
				//Is it a bracket
			}else if(nextchar.equals("(")) {
				tokens.add(new Token(Token.TOKEN_OPENBRACKET, nextchar));
				mPos++;
			}else if(nextchar.equals(")")) {
				tokens.add(new Token(Token.TOKEN_CLOSEBRACKET, nextchar));
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
				tokens.add(new Token(Token.TOKEN_VALUE, str));
				
			}else{
				//get the next word..
				String word = getNextWord();
				
					//What is it..
				if(allcommands.contains(word)) {
					//Must have a space before a command word
					if(!waslastspace) {
						throw new SyntaxException("Missing space before Command @ "+mPos+" "+word);
					}
					
					//It's a command
					tokens.add(new Token(Token.TOKEN_COMMAND, word));
				
				}else if(allfunctions.contains(word)) {
					//It's a function
					tokens.add(new Token(Token.TOKEN_FUNCTIION, word));
				
				}else if(allboolops.contains(word)) {
					//It's a function
					tokens.add(new Token(Token.TOKEN_OPERATOR, word));
				
				}else if(isNumber(word) || isHex(word)) {
					//It's a number
					tokens.add(new Token(Token.TOKEN_VALUE, word));
				
				}else if(word.equals("TRUE")) {
					//It's a number
					tokens.add(new Token(Token.TOKEN_TRUE, word));
				
				}else if(word.equals("FALSE")) {
					//It's a number
					tokens.add(new Token(Token.TOKEN_FALSE, word));
				
				}else if(isGlobal(word)) {
					//It's a global
					tokens.add(new Token(Token.TOKEN_GLOBAL, word));
					
				}else if(isVariable(word)) {
					//It's a number
					tokens.add(new Token(Token.TOKEN_VARIABLE, word));
				
				}else {
					throw new SyntaxException("Incorrect Token found @ "+mPos+" "+word);
				}
			}
			
			//Save this - commands have to have this as true
			waslastspace = wasspace;
		}
		
		return tokens;
	}
	
	public static void main(String[] zArgs) {
		
//		System.out.println(isNumber("22"));
//		System.out.println(isNumber("22.88"));
//		System.out.println(isNumber("10.88"));
		
		String script = "LET  a =  -1.5 - -1.5";
		
		try {
			//Then run it..
			Contract cc = new Contract(script, "", new Witness(), new Transaction(), new ArrayList<>(),true);
			cc.setGlobalVariable("@BLKNUM", new NumberValue(22));
			cc.run();
			
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
	}
	
}
