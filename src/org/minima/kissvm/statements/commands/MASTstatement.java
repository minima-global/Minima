package org.minima.kissvm.statements.commands;

import java.util.List;

import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.ExecutionException;
import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.expressions.Expression;
import org.minima.kissvm.statements.Statement;
import org.minima.kissvm.statements.StatementBlock;
import org.minima.kissvm.statements.StatementParser;
import org.minima.kissvm.tokens.Token;
import org.minima.kissvm.values.HEXValue;
import org.minima.kissvm.values.ScriptValue;
import org.minima.objects.Transaction;
import org.minima.objects.Witness;
import org.minima.objects.base.MiniData;
import org.minima.objects.proofs.ScriptProof;

public class MASTstatement implements Statement {

	/**
	 * The MAST script is a HEXvalue that is the hash of the script..
	 */
	Expression mMASTScript;
	
	public MASTstatement(Expression zMAST) {
		mMASTScript = zMAST;
	}
	
	@Override
	public void execute(Contract zContract) throws ExecutionException {
		//get the MAST Value..
		HEXValue mast = (HEXValue) mMASTScript.getValue(zContract);
		
		//Now get that Script from the transaction..
		Witness wit = zContract.getWitness();
		
		//Get the Script Proof
		ScriptProof scrpr = wit.getScript(mast.getMiniData());
		
		if(scrpr == null) {
			throw new ExecutionException("No script found for MAST "+mast.getMiniData());
		}
		
		//get the script of this hash value
		String script = scrpr.getScript().toString();
		
		try {
			//Convert the script to KISSVM!
			List<Token> tokens = Token.tokenize(script);
		
			//And now convert to a statement block..
			StatementBlock mBlock = StatementParser.parseTokens(tokens);

			//Now run it..
			mBlock.run(zContract);
		
		} catch (Exception e) {
			// TODO Auto-generated catch block
			throw new ExecutionException(e.toString());
		}		
	}

	@Override
	public String toString() {
		return "MAST "+mMASTScript.toString();
	}
}
