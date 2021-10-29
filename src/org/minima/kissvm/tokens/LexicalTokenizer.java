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
	List<ScriptToken> mTokens;
	int 		mPos;
	int 		mSize;
	
	public LexicalTokenizer(List<ScriptToken> zTokens) {
		mTokens = zTokens;
		mPos 	= 0;
		mSize   = zTokens.size();
	}
	
	public ScriptToken getNextToken() throws MinimaParseException {
		if(mPos >= mSize) {
			throw new MinimaParseException("Run out of tokens!..");
		}
		return mTokens.get(mPos++);
	}
	
	public int getCurrentPosition() {
		return mPos;
	}
	
	public void goBackToken() throws MinimaParseException {
		if(mPos==0 ) {
			throw new MinimaParseException("LexicalTokenizer cannot go back as at 0 position");
		}
		mPos--;
	}
	
	public boolean checkAllTokensUsed() {
		return mPos == mSize;
	}
	
	public boolean hasMoreElements() {
		return mPos<mSize;
	}
}
