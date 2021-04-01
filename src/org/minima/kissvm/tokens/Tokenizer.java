package org.minima.kissvm.tokens;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.StringTokenizer;

import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.exceptions.SyntaxException;
import org.minima.kissvm.functions.MinimaFunction;

public class Tokenizer {

	public final String[] TOKENS_EOW   = {" ","+","-","/","*","="};
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
	
	public static boolean isNumber(String zChar){
		return zChar.matches("[0-9]");
		
		//return str.matches("-?\\d+(\\.\\d+)?");  //match a number with optional '-' and decimal.
	}
	
	public ArrayList<Token> tokenize() throws SyntaxException{
		ArrayList<Token> tokens = new ArrayList<Token>();
		
		//Get the defaults..
		List<String> allcommands  = Arrays.asList(Token.TOKENS_COMMAND);
		List<String> alloperators = Arrays.asList(Token.TOKENS_OPERATOR);
		
		List<String> allfunctions = new ArrayList<>();
		for(MinimaFunction func : MinimaFunction.ALL_FUNCTIONS) {
			allfunctions.add(func.getName());
		}
		
		//Now run through..
		mPos = 0;
		while(mPos<mLength) {
			//Get the next symbol..
			String c = Character.toString(mScript.charAt(mPos));
			
			//space check..
			if(c.equals(" ")) {
				//ignore and move on..
				mPos++;
			
				//Variable
			}else if(c.equals("_")) {
				String var = getNextWord();
				tokens.add(new Token(Token.TOKEN_VARIABLE, var));
			
				//Number
			}else if(isNumber(c)) {
				String num = getNextWord();
				tokens.add(new Token(Token.TOKEN_VALUE, num));
			
			}else if(c.equals("=")) {
				tokens.add(new Token(Token.TOKEN_OPERATOR, c));
				mPos++;
				
			}else{
				System.out.println("Unkonwn Token! "+c);
				break;
			}
			
//			else if(c == 'L' || c == 'A' || c == 'E' || c == 'I' ||) {
//				//Check for LET
//				int index = strbuf.indexOf(" ", pos);
//				if(index == -1) {
//					index = len;
//				}
//				
//				//get the word..
//				String word = strbuf.substring(pos, index);
//				
//				if(word.equals("LET") || ) {
//					tokens.add(new Token(Token.TOKEN_COMMAND, word));
//				}else {
//					int starttext = pos-5;
//					if(starttext<0) {
//						starttext = 0;
//					}
//					throw new SyntaxException("Incorrect syntax @ "+pos+" .."+strbuf.substring(starttext, pos));
//				}
//				
//			}
			
		}
		
		return tokens;
	}
	
	public static void main(String[] zArgs) {
		
		String script = "let _f = 34 ";
		
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
