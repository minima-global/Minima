package org.minima.kissvm.tokens;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.StringTokenizer;

import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.exceptions.SyntaxException;
import org.minima.kissvm.functions.MinimaFunction;

public class Tokenizer {

	/**
	 * Main Statements
	 */
	public final String[] TOKENS_COMMAND     = 
		{"let",
		 "if","then","elseif","else","endif",
		 "resturn",
		 "assert",
		 "while","do","endwhile",
		 "exec",
		 "mast"};
	
	/**
	 * Number operators
	 */
	public static final String[] TOKENS_OPERATOR = 
		{"+","-","/","*","%","&","|","^","="};
	
	/**
	 * Brackets
	 */
	public static final String[] TOKENS_BRACKETS = 
		{"(",")"};
	
	
	
	public final String[] TOKENS_EOW   = {" ","+","-","/","*","=","(",")"};
	public final List<String> mAllEOW  = Arrays.asList(TOKENS_EOW);
	
	
	StringBuffer mScript;
	
	int mPos;
	int mLength; 
	
	String mLastEOW = "";
	
	public Tokenizer(String zScript) {
		mScript = new StringBuffer(zScript);
		mPos    = 0;
		mLength = mScript.length();
		
	}
	
	
	public String getNextWord() {
		String word = "";
		
		//Get all the characters uop to the next End Of Word symbol..
		mLastEOW = "";
		while(mPos<mLength){
			//get the next Character
			String c = Character.toString(mScript.charAt(mPos));
			
			//Is it an end of Word..
			if(mAllEOW.contains(c)) {
				//Store the last EOW
				mLastEOW = c;
				
				//All done..
				break;
			}
			
			//Add to the word
			word += mScript.charAt(mPos++);
		}
		
		return word;
	}
	
	public boolean isNumber(String zWord){
		return zWord.matches("[0-9]+");
		//return str.matches("-?\\d+(\\.\\d+)?");  //match a number with optional '-' and decimal.
	}
	
	public boolean isVariable(String zWord){
		return zWord.matches("[a-z]+");
	}
	
	public ArrayList<Token> tokenize() throws SyntaxException{
		ArrayList<Token> tokens = new ArrayList<Token>();
		
		//Get the defaults..
		List<String> allcommands  	= Arrays.asList(TOKENS_COMMAND);
		List<String> allnumops 		= Arrays.asList(TOKENS_OPERATOR);
		List<String> allbrackets 	= Arrays.asList(TOKENS_BRACKETS);
		
		List<String> allfunctions 	= new ArrayList<>();
		for(MinimaFunction func : MinimaFunction.ALL_FUNCTIONS) {
			allfunctions.add(func.getName());
		}
		
		//What was the previous token - check for negative numbers..
		int previoustok = -1;
		
		//Now run through..
		mPos = 0;
		while(mPos<mLength) {
			//Get the next symbol..
			String nextchar = Character.toString(mScript.charAt(mPos));
			
			//space check..
			if(nextchar.equals(" ")) {
				//ignore and move on..
				mPos++;
			
				//Is it a one character operator
			}else if(allnumops.contains(nextchar)) {
				tokens.add(new Token(Token.TOKEN_OPERATOR, nextchar));
				mPos++;

				previoustok = Token.TOKEN_OPERATOR;
				
				//Is it a bracket
			}else if(allbrackets.contains(nextchar)) {
				if(nextchar.equals("(")) {
					tokens.add(new Token(Token.TOKEN_OPENBRACKET, nextchar));
				}else if(nextchar.equals(")")) {
					tokens.add(new Token(Token.TOKEN_CLOSEBRACKET, nextchar));
				}
				mPos++;

			}else{
				//get the next word..
				String word = getNextWord();
				
				//What is it..
				if(allcommands.contains(word)) {
					//It's a command
					tokens.add(new Token(Token.TOKEN_COMMAND, word));
				}else if(allfunctions.contains(word)) {
					//It's a function
					tokens.add(new Token(Token.TOKEN_FUNCTIION, word));
				
				}else if(isNumber(word)) {
					//It's a number
					tokens.add(new Token(Token.TOKEN_VALUE, word));
				
				}else if(isVariable(word)) {
					//It's a number
					tokens.add(new Token(Token.TOKEN_VARIABLE, word));
				
				}else {
					System.out.println("ERROR TOKEN "+word);
					break;
				}
				
			}
			
		}
		
		return tokens;
	}
	
	public static void main(String[] zArgs) {
		
		String script = "let f= 22--23+-67 ";
		
		try {
			Tokenizer tokz = new Tokenizer(script);
			
			ArrayList<Token> toks = tokz.tokenize();
		
			int count = 0;
			for(Token tok : toks) {
				System.out.println(count+") TOKEN > "+tok);
				count++;
			}
			
			System.out.println("TOTAL :  "+toks.size());
			
		} catch (SyntaxException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
	}
	
}
