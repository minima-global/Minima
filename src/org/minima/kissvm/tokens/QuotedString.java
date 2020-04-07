package org.minima.kissvm.tokens;

import java.util.ArrayList;

import org.minima.kissvm.Contract;

public class QuotedString {

	String mString;
	
	String mQuotedString;
	
	ArrayList<String> mQuotes;
	
	int mCurrentPos;
	int mCurrentQuote;
	
	public QuotedString(String zString){
		mString = zString;
		mQuotes = new ArrayList<>();
	}
	
	public String getDeQuotedString() {
		mCurrentQuote = 0;
		mCurrentPos   = 0;
		mQuotedString = "";
		int len = mString.length();
		
		//Find the first Quote
		while(mCurrentPos < len ) {
			//Get the next char
			String next = Character.toString(mString.charAt(mCurrentPos));
			
			//IS it a normal or special char..
			if(next.equals("[")) {
				//This section will be replaced..
				String rep = ":"+mCurrentQuote;
				
				//Find the final ]
				int end = findFinalQuote(mCurrentPos+1);
			
				//Now copy that chunk..
				String quote = mString.substring(mCurrentPos, end+1);
				
				//Add the special token to the String
				mQuotedString += rep;
				
				//Add the quote to the quotes
				mQuotes.add(quote);
				mCurrentQuote++;
				
				//Move the counter
				mCurrentPos = end;
				
			}else {
				//It's normal just add..
				mQuotedString += next;
			}
		
			//Move along
			mCurrentPos++;
		}
		
		return mQuotedString;
	}
	
	private int findFinalQuote(int zCurrentPos) {
		
		int pos = zCurrentPos;
		int len = mString.length();
		
		while(pos < len) {
			String next = Character.toString(mString.charAt(pos));
			if(next.equals("[")) {
				//Go deep
				pos = findFinalQuote(pos+1);
				
			}else if(next.equals("]")) {
				//Close this loop
				return pos;
			}
			
			pos++;
		}
		
		return -1;
	}
	
	
	public String getQuote(int zQuote) {
		return mQuotes.get(zQuote);
	}
	
	public int getQuotesSize() {
		return mQuotes.size();
	}
	
	public static void main(String[] zArgs) {
		
		String tester = " let x = [   asjd let s=3  hajs [ asd asd ]asdad ]LET Y=[ 87876 ] ";
		System.out.println("String : "+tester);
		tester = Contract.cleanScript(tester);		
		System.out.println("Clean : "+tester);
		
		QuotedString qs = new QuotedString(tester);
		
		System.out.println("String : "+tester);
		System.out.println("Quoted : "+qs.getDeQuotedString());
		
		for(int i=0;i<qs.getQuotesSize();i++) {
			System.out.println("Quote ["+i+"]: "+qs.getQuote(i));	
		}
		
	}
	
}
