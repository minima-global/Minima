/**
 * 
 */
package org.minima.miniscript.tokens;

import java.util.List;

import org.minima.miniscript.exceptions.MinimaParseException;

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
		if(mPos >= mTokens.size()) {
			throw new MinimaParseException("Run out of tokens!..");
		}
		return mTokens.get(mPos++);
	}
		
	public void goBackToken() {
		mPos--;
	}
	
	public boolean checkAllTokensUsed() {
		return mPos == mTokens.size();
	}
	
	public boolean hasMoreElements() {
		return mPos<mSize;
	}
}
