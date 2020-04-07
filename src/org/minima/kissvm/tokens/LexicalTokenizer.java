/**
 * 
 */
package org.minima.kissvm.tokens;

import java.util.List;

import org.minima.kissvm.exceptions.MinimaParseException;

/**
 * @author Spartacus Rex
 *
 */
public class LexicalTokenizer{
	List<Token> mTokens;
	int 		mPos;
	int 		mSize;
	
	public LexicalTokenizer(List<Token> zTokens) {
		mTokens = zTokens;
		mPos 	= 0;
		mSize   = zTokens.size();
	}
	
	public Token getNextToken() throws MinimaParseException {
		if(mPos >= mSize) {
			throw new MinimaParseException("Run out of tokens!..");
		}
		return mTokens.get(mPos++);
	}
	
	public int getCurrentPosition() {
		return mPos;
	}
	
	public void goBackToken() {
		mPos--;
	}
	
	public boolean checkAllTokensUsed() {
		return mPos == mSize;
	}
	
	public boolean hasMoreElements() {
		return mPos<mSize;
	}
}
