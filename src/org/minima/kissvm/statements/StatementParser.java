/**
 * 
 */
package org.minima.kissvm.statements;

import java.util.ArrayList;
import java.util.List;

import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.expressions.ConstantExpression;
import org.minima.kissvm.expressions.Expression;
import org.minima.kissvm.expressions.ExpressionParser;
import org.minima.kissvm.statements.commands.ASSERTstatement;
import org.minima.kissvm.statements.commands.EXECstatement;
import org.minima.kissvm.statements.commands.IFstatement;
import org.minima.kissvm.statements.commands.LETstatement;
import org.minima.kissvm.statements.commands.MASTstatement;
import org.minima.kissvm.statements.commands.RETURNstatement;
import org.minima.kissvm.statements.commands.WHILEstatement;
import org.minima.kissvm.tokens.LexicalTokenizer;
import org.minima.kissvm.tokens.ScriptToken;
import org.minima.kissvm.values.BooleanValue;

/**
 * IF..THEN ELSEIF.. THEN.. ELSE.. ENDIF
 * 
 * @author Spartacus Rex
 *
 */
public class StatementParser {

	/**
	 * Parse a list of tokens into a list of Statements
	 * @param zTokens
	 * @return the list of Statements
	 */
	public static StatementBlock parseTokens(List<ScriptToken> zTokens) throws Exception{
		List<Statement> stats = new ArrayList<>();
		
		//Cycle..
		int currentPosition	= 0;
		int totaltokens 	= zTokens.size();
		
		while(currentPosition<totaltokens) {
		
			//Get the current token
			ScriptToken tok = zTokens.get(currentPosition++);
			
			String token 	= tok.getToken();
			int type 		= tok.getTokenType();
			
			if(type != ScriptToken.TOKEN_COMMAND) {
				throw new MinimaParseException("Invalid Token where there should be a COMMMAND - "+token); 
			}
			
			//Cycle through commands
			if(token.equalsIgnoreCase("LET")) {
				//The next token is either the variable name or an array position..
				ScriptToken var = zTokens.get(currentPosition++);
				
				//Is it a simple variable LET or an ARRAY set LET
				if(var.getTokenType() == ScriptToken.TOKEN_OPENBRACKET) {
					//Get the tokens to the equals signn
					List<ScriptToken> arraypos = getTokensToNextEquals(zTokens, currentPosition);
					currentPosition += arraypos.size();
					
					//Check the last token is a close bracket
					int arrsize = arraypos.size();
					var = arraypos.get(arrsize-1);
					if(var.getTokenType() != ScriptToken.TOKEN_CLOSEBRACKET) {
						throw new MinimaParseException("Incorrect LET statement, missing ) .. "+var.getToken()+")");
					}
					
					//The next token is always =
					var = zTokens.get(currentPosition++);
					if(var.getTokenType() != ScriptToken.TOKEN_OPERATOR && !var.getToken().equals("=")) {
						throw new MinimaParseException("Incorrect LET statement, missing = (.."+var.getToken()+")");
					}
	
					//Remove the last token..
					arraypos.remove(arrsize-1);
					
					//Check is a valid non-empty expression
					if(arraypos.size() == 0) {
						throw new MinimaParseException("Incorrect LET statement, EMPTY ARRAY POS @ "+currentPosition);
					}
					
					//Create a Lexical Tokenizer.. there may be multiple expressions..
					LexicalTokenizer lt = new LexicalTokenizer(arraypos);
					
					ArrayList<Expression> exps = new ArrayList<Expression>();
					while(!lt.checkAllTokensUsed()) {
						//Now get each of the expressions
						Expression letexp = ExpressionParser.getExpression(lt);
						
						//Add it to out list for the LET statement
						exps.add(letexp);
					}
					
					//Now find the next Command, and everything in between is the expression
					List<ScriptToken> lettokens = getTokensToNextCommand(zTokens, currentPosition);
					currentPosition += lettokens.size();
					
					//Now create an expression from those tokens..
					Expression exp = ExpressionParser.getExpression(lettokens);
					
					//And finally create the LET statement..
					stats.add(new LETstatement(exps, exp));
					
				}else if(var.getTokenType() == ScriptToken.TOKEN_VARIABLE) {
					//The Variable name
					String varname = var.getToken();
					
					//The next token is always =
					var = zTokens.get(currentPosition++);
					if(!var.getToken().equals("=")) {
						throw new MinimaParseException("Incorrect LET statement, missing = (.."+var.getToken()+")");
					}
				
					//Now find the next Command, and everything in between is the expression
					List<ScriptToken> lettokens = getTokensToNextCommand(zTokens, currentPosition);
					currentPosition += lettokens.size();
					
					//Now create an expression from those tokens..
					Expression exp = ExpressionParser.getExpression(lettokens);
					
					//And finally create the LET statement..
					stats.add(new LETstatement(varname, exp));
					
				}else {
					throw new MinimaParseException("Not a variable or array after LET (.."+var.getToken()+")");
				}
				
			}else if(token.equalsIgnoreCase("EXEC")) {
				//Now find the next Command, and everything in between is the expression
				List<ScriptToken> exectokens = getTokensToNextCommand(zTokens, currentPosition);
				currentPosition += exectokens.size();
				
				//Now create an expression from those tokens..
				Expression exp = ExpressionParser.getExpression(exectokens);
				
				//And finally create the LET statement..
				stats.add(new EXECstatement(exp));
					
			}else if(token.equalsIgnoreCase("MAST")) {
				//Now find the next Command, and everything in between is the expression
				List<ScriptToken> masttokens = getTokensToNextCommand(zTokens, currentPosition);
				currentPosition += masttokens.size();
				
				//Now create an expression from those tokens..
				Expression exp = ExpressionParser.getExpression(masttokens);
				
				//And finally create the LET statement..
				stats.add(new MASTstatement(exp));
					
			}else if(token.equalsIgnoreCase("IF")) {
				//An IFX
				IFstatement ifsx = new IFstatement();
				
				//Get the IFConditional
				List<ScriptToken> conditiontokens = getTokensToNextCommand(zTokens, currentPosition);
				
				//Now create an expression from those tokens..
				Expression IFcondition = ExpressionParser.getExpression(conditiontokens);
				
				//Increments
				currentPosition += conditiontokens.size() + 1;
				
				//Now get the Expression..
				List<ScriptToken> actiontokens = getElseOrElseIfOrEndIF(zTokens, currentPosition,true);
				
				//Increment
				currentPosition += actiontokens.size();
				
				//Is it the ENDIF or the ELSE
				String nexttok = actiontokens.get(actiontokens.size()-1).getToken();
				
				//Remove the final ENDIF - This is done here as we need all the ENDIFs for the child IF clauses
				actiontokens = actiontokens.subList(0, actiontokens.size()-1);
				
				//And convert that block of Tokens into a block of code..
				StatementBlock IFaction = parseTokens(actiontokens);
				
				//Add what we know to the IF statement..
				ifsx.addCondition(IFcondition, IFaction);
				
				//Is it ELSE or END
				while(!nexttok.equals("ENDIF")) {
					//New branch
					Expression     ELSEcondition = null;
					StatementBlock ELSEaction    = null;
					
					if(nexttok.equals("ELSE")) {
						//ELSE is default
						ELSEcondition = new ConstantExpression(BooleanValue.TRUE);
						
					}else {
						//It's ELSEIF
						conditiontokens = getTokensToNextCommand(zTokens, currentPosition);
						
						//Create an Expression..
						ELSEcondition = ExpressionParser.getExpression(conditiontokens);
						
						//Increments
						currentPosition += conditiontokens.size() + 1;
					}
					
					//Now get the Action..
					actiontokens = getElseOrElseIfOrEndIF(zTokens, currentPosition,true);
					
					//Increment
					currentPosition += actiontokens.size();
					
					//Is it the ENDIF or the ELSE
					nexttok = actiontokens.get(actiontokens.size()-1).getToken();
					
					//Remove the final ENDIF - This is done here as we need all the ENDIFs for the child IF clauses
					actiontokens = actiontokens.subList(0, actiontokens.size()-1);
					
					//And convert that block of Tokens into a block of code..
					ELSEaction = parseTokens(actiontokens);
					
					//Add what we know to the IF statement..
					ifsx.addCondition(ELSEcondition, ELSEaction);
				}
				
				//Add..
				stats.add(ifsx);
										
			}else if(token.equalsIgnoreCase("WHILE")) {
				//Get the WHILE Conditional - stop at the next THEN
				List<ScriptToken> conditiontokens = getTokensToNextCommand(zTokens, currentPosition);
				
				//Now create an expression from those tokens..
				Expression WHILEcondition = ExpressionParser.getExpression(conditiontokens);
				
				//Increments
				currentPosition += conditiontokens.size() + 1;
				
				//Now get the Expression..
				List<ScriptToken> actiontokens = getEndWHILE(zTokens, currentPosition);
				
				//Increment
				currentPosition += actiontokens.size();
				
				//Remove the final ENDIF - This is done here as we need all the ENDIFs for the child IF clauses
				actiontokens = actiontokens.subList(0, actiontokens.size()-1);
				
				//And convert that block of Tokens into a block of code..
				StatementBlock WHILEaction = parseTokens(actiontokens);
				
				//Create an IF statement
				WHILEstatement ws = new WHILEstatement(WHILEcondition, WHILEaction);
				
				//Add..
				stats.add(ws);
				
			}else if(token.equalsIgnoreCase("ASSERT")) {
				//The Next tokens are the Expression..
				List<ScriptToken> returntokens = getTokensToNextCommand(zTokens, currentPosition);
				currentPosition += returntokens.size();
				
				//Now create an expression from those tokens..
				Expression exp = ExpressionParser.getExpression(returntokens);
				
				//Create a new RETURN statement
				stats.add(new ASSERTstatement(exp));
			
			}else if(token.equalsIgnoreCase("RETURN")) {
				//The Next tokens are the Expression..
				List<ScriptToken> returntokens = getTokensToNextCommand(zTokens, currentPosition);
				currentPosition += returntokens.size();
				
				//Now create an expression from those tokens..
				Expression exp = ExpressionParser.getExpression(returntokens);
				
				//Create a new RETURN statement
				stats.add(new RETURNstatement(exp));
			
			}else {
				throw new MinimaParseException("Invalid Token where there should be a Command - "+token); 
			}
		}
		
		return new StatementBlock(stats);
	}
	
	/*private static List<Token> getElseOrEndIF(List<Token> zTokens, int zCurrentPosition, boolean zElseAlso){
		List<Token> rettokens = new ArrayList<>();
		
		int currentpos  = zCurrentPosition;
		int total 		= zTokens.size();
		
		//Cycle through the tokens..
		while(currentpos<total) {
			
			//Get the next token
			Token tok = zTokens.get(currentpos);
			
			if(tok.getTokenType() == Token.TOKEN_COMMAND && tok.getToken().equals("ENDIF")) {
				//We've found the end to the current depth IF
				rettokens.add(tok);
				return rettokens;
			
			}else if(zElseAlso && (tok.getTokenType() == Token.TOKEN_COMMAND && tok.getToken().equals("ELSE")) ) {
				//We've found the end to the current depth IF
				rettokens.add(tok);
				return rettokens;
				
			}else if(tok.getTokenType() == Token.TOKEN_COMMAND && tok.getToken().equals("IF")) {
				//Add it..
				rettokens.add(tok);
				currentpos++;
				
				//Go down One Level
				List<Token> toks = getElseOrEndIF(zTokens, currentpos, false);
			
				rettokens.addAll(toks);
				currentpos += toks.size();
			
			}else {
				//Just add it to the list
				rettokens.add(tok);
				currentpos++;
				
			}
		}
		
		return rettokens;
	}*/
	
	private static List<ScriptToken> getElseOrElseIfOrEndIF(List<ScriptToken> zTokens, int zCurrentPosition, boolean zElseAlso){
		List<ScriptToken> rettokens = new ArrayList<>();
		
		int currentpos  = zCurrentPosition;
		int total 		= zTokens.size();
		
		//Cycle through the tokens..
		while(currentpos<total) {
			
			//Get the next token
			ScriptToken tok = zTokens.get(currentpos);
			
			if(tok.getTokenType() == ScriptToken.TOKEN_COMMAND && tok.getToken().equals("ENDIF")) {
				//We've found the end to the current depth IF
				rettokens.add(tok);
				break;
			
			}else if(zElseAlso && (tok.getTokenType() == ScriptToken.TOKEN_COMMAND && tok.getToken().equals("ELSEIF")) ) {
				//We've found the end to the current depth IF
				rettokens.add(tok);
				break;
			
			}else if(zElseAlso && (tok.getTokenType() == ScriptToken.TOKEN_COMMAND && tok.getToken().equals("ELSE")) ) {
				//We've found the end to the current depth IF
				rettokens.add(tok);
				break;
				
			}else if(tok.getTokenType() == ScriptToken.TOKEN_COMMAND && tok.getToken().equals("IF")) {
				//Add it..
				rettokens.add(tok);
				currentpos++;
				
				//Go down One Level
				List<ScriptToken> toks = getElseOrElseIfOrEndIF(zTokens, currentpos, false);
			
				rettokens.addAll(toks);
				currentpos += toks.size();
			
			}else{
				//Just add it to the list
				rettokens.add(tok);
				currentpos++;
			}
		}
		
		return rettokens;
	}
	
	/**
	 * Get the last ENDWHILE of a while statement - could recurse
	 * @param zTokens
	 * @param zCurrentPosition
	 * @return
	 */
	private static List<ScriptToken> getEndWHILE(List<ScriptToken> zTokens, int zCurrentPosition){
		List<ScriptToken> rettokens = new ArrayList<>();
		
		int currentpos  = zCurrentPosition;
		int total 		= zTokens.size();
		
		//Cycle through the tokens..
		while(currentpos<total) {
			
			//Get the next token
			ScriptToken tok = zTokens.get(currentpos);
			
			if(tok.getTokenType() == ScriptToken.TOKEN_COMMAND && tok.getToken().equals("ENDWHILE")) {
				//We've found the end to the current depth IF
				rettokens.add(tok);
				break;
				
			}else if(tok.getTokenType() == ScriptToken.TOKEN_COMMAND && tok.getToken().equals("WHILE")) {
				//Add it..
				rettokens.add(tok);
				currentpos++;
				
				//Go down One Level
				List<ScriptToken> toks = getEndWHILE(zTokens, currentpos);
			
				rettokens.addAll(toks);
				currentpos += toks.size();
			
			}else {
				//Just add it to the list
				rettokens.add(tok);
				currentpos++;
			}
		}
		
		return rettokens;
	}
	
	
	/**
	 * Simple function that returns all the tokens up to the very next COMMAND TOKEN.
	 * 
	 * @param zTokens
	 * @param zCurrentPosition
	 * @return The list of tokens
	 */
	private static List<ScriptToken> getTokensToNextCommand(List<ScriptToken> zTokens, int zCurrentPosition){
		List<ScriptToken> rettokens = new ArrayList<>();
		
		int ret   = zCurrentPosition;
		int total = zTokens.size();
		while(ret<total) {
			ScriptToken tok = zTokens.get(ret);
			if(tok.getTokenType() == ScriptToken.TOKEN_COMMAND) {
				break;
			}else {
				//Add it to the list
				rettokens.add(tok);
			}
			ret++;
		}
	
		
		return rettokens;
	}
	
	
	/**
	 * Get all the tokens to the next = sign.. 
	 * @param zTokens
	 * @param zCurrentPosition
	 * @return
	 */
	private static List<ScriptToken> getTokensToNextEquals(List<ScriptToken> zTokens, int zCurrentPosition){
		List<ScriptToken> rettokens = new ArrayList<>();
		
		int ret   = zCurrentPosition;
		int total = zTokens.size();
		while(ret<total) {
			ScriptToken tok = zTokens.get(ret);
			if(tok.getTokenType() == ScriptToken.TOKEN_OPERATOR && tok.getToken().equals("=")) {
				break;
			}else {
				//Add it to the list
				rettokens.add(tok);
			}
			ret++;
		}
	
		return rettokens;
	}
}
