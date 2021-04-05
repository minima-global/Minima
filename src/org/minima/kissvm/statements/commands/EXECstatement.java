package org.minima.kissvm.statements.commands;

import java.util.List;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.expressions.Expression;
import org.minima.kissvm.statements.Statement;
import org.minima.kissvm.statements.StatementBlock;
import org.minima.kissvm.statements.StatementParser;
import org.minima.kissvm.tokens.Token;
import org.minima.kissvm.tokens.Tokenizer;
import org.minima.kissvm.values.ScriptValue;

/**
 * EXEC SCRIPT
 * 
 * @author spartacusrex
 *
 */
public class EXECstatement implements Statement{

	Expression mScript;
	
	public EXECstatement(Expression zScript) {
		mScript = zScript;
	}
	
	@Override
	public void execute(Contract zContract) throws ExecutionException {
		//get the Script..
		ScriptValue script = (ScriptValue) mScript.getValue(zContract);
		
		try {
			//Tokenize the script
			Tokenizer tokz = new Tokenizer(script.toString());
			
			//Convert the script to KISSVM!
			List<Token> tokens = tokz.tokenize();	
		
			//And now convert to a statement block..
			StatementBlock mBlock = StatementParser.parseTokens(tokens);
			
			//Now run it..
			mBlock.run(zContract);
			
		}catch(Exception exc) {
			throw new ExecutionException(exc.toString());			
		}
	}
	
	@Override
	public String toString() {
		return "EXEC "+mScript;
	}
}
