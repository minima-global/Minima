/**
 * 
 */
package org.minima.kissvm.expressions;

import java.util.List;

import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.functions.MinimaFunction;
import org.minima.kissvm.tokens.LexicalTokenizer;
import org.minima.kissvm.tokens.ScriptToken;
import org.minima.kissvm.values.BooleanValue;
import org.minima.kissvm.values.NumberValue;
import org.minima.kissvm.values.Value;
import org.minima.objects.base.MiniNumber;

/**
 * @author Spartacus Rex
 */
public class ExpressionParser {
	
	/**
	 * The main entry point that converts a sequence of tokens 
	 * into a valid computable Expression
	 * 
	 * @param zTokens
	 * @return
	 */
	public static Expression getExpression(List<ScriptToken> zTokens) throws MinimaParseException{
		//Must have some tokens!
		if(zTokens.size() == 0) {
			throw new MinimaParseException("Cannot have EMPTY expression");
		}
		
		//Create a Lexical Tokenizer..
		LexicalTokenizer lt = new LexicalTokenizer(zTokens);
		
		//get the complete expression..
		Expression exp = getExpression(lt);
		
		//Did we use all the tokens..
		if(!lt.checkAllTokensUsed()) {
			throw new MinimaParseException("Incorrect token number in expression @ "
						+lt.getNextToken().getToken());
		}
		
		//return the final expression
		return exp;
	}
	
	/**
	 * Private classes to hierarchically break down the script into Valid Expressions with
	 * correct precedence.
	 * @param zTokens
	 * @param zPos
	 * @return
	 */
	public static Expression getExpression(LexicalTokenizer zTokens) throws MinimaParseException{
		//Top level..
		Expression exp = getRelation(zTokens);
		
		while(zTokens.hasMoreElements()) {
			ScriptToken tok = zTokens.getNextToken();
			
			if(tok.getToken().equals("AND")) {
				exp = new BooleanExpression(exp, getRelation(zTokens), BooleanExpression.BOOLEAN_AND);
			}else if(tok.getToken().equals("OR")) {
				exp = new BooleanExpression(exp, getRelation(zTokens), BooleanExpression.BOOLEAN_OR);
			}else if(tok.getToken().equals("XOR")) {
				exp = new BooleanExpression(exp, getRelation(zTokens), BooleanExpression.BOOLEAN_XOR);
			}else if(tok.getToken().equals("NAND")) {
				exp = new BooleanExpression(exp, getRelation(zTokens), BooleanExpression.BOOLEAN_NAND);
			}else if(tok.getToken().equals("NOR")) {
				exp = new BooleanExpression(exp, getRelation(zTokens), BooleanExpression.BOOLEAN_NOR);
			}else if(tok.getToken().equals("NXOR")) {
				exp = new BooleanExpression(exp, getRelation(zTokens), BooleanExpression.BOOLEAN_NXOR);
			}else{
				zTokens.goBackToken();
				break;
			}
		}
		
		return exp;
	}
		
	private static Expression getRelation(LexicalTokenizer zTokens) throws MinimaParseException{
		//Keep Drilling Down..
		Expression exp = getLogic(zTokens);
		
		while(zTokens.hasMoreElements()) {
			ScriptToken tok = zTokens.getNextToken();
			
			if(tok.getToken().equals("EQ")) {
				exp = new BooleanExpression(exp, getLogic(zTokens), BooleanExpression.BOOLEAN_EQ);
			}else if(tok.getToken().equals("NEQ")) {
				exp = new BooleanExpression(exp, getLogic(zTokens), BooleanExpression.BOOLEAN_NEQ);
			}else if(tok.getToken().equals("GT")) {
				exp = new BooleanExpression(exp, getLogic(zTokens), BooleanExpression.BOOLEAN_GT);
			}else if(tok.getToken().equals("GTE")) {
				exp = new BooleanExpression(exp, getLogic(zTokens), BooleanExpression.BOOLEAN_GTE);
			}else if(tok.getToken().equals("LT")) {
				exp = new BooleanExpression(exp, getLogic(zTokens), BooleanExpression.BOOLEAN_LT);
			}else if(tok.getToken().equals("LTE")) {
				exp = new BooleanExpression(exp, getLogic(zTokens), BooleanExpression.BOOLEAN_LTE);
			}else{
				zTokens.goBackToken();
				break;
			}			
		}
		
		return exp;
	}
	
	private static Expression getLogic(LexicalTokenizer zTokens) throws MinimaParseException{
		//Keep Drilling Down..
		Expression exp = getAddSub(zTokens);
		
		while(zTokens.hasMoreElements()) {
			ScriptToken tok = zTokens.getNextToken();
			
			if(tok.getToken().equals("&")) {
				exp = new OperatorExpression(exp,getAddSub(zTokens),OperatorExpression.OPERATOR_AND);
			}else if(tok.getToken().equals("|")) {
				exp = new OperatorExpression(exp,getAddSub(zTokens),OperatorExpression.OPERATOR_OR);
			}else if(tok.getToken().equals("^")) {
				exp = new OperatorExpression(exp,getAddSub(zTokens),OperatorExpression.OPERATOR_XOR);
			}else{
				zTokens.goBackToken();
				break;
			}			
		}
		
		return exp;
	}
	
	private static Expression getAddSub(LexicalTokenizer zTokens) throws MinimaParseException{
		Expression exp = getMulDiv(zTokens);
		
		while(zTokens.hasMoreElements()) {
			ScriptToken tok = zTokens.getNextToken();
			
			if(tok.getToken().equals("+")) {
				exp = new OperatorExpression(exp,getMulDiv(zTokens),OperatorExpression.OPERATOR_ADD);
			}else if(tok.getToken().equals("-")) {
				exp = new OperatorExpression(exp,getMulDiv(zTokens),OperatorExpression.OPERATOR_SUB);
			}else if(tok.getToken().equals("%")) {
				exp = new OperatorExpression(exp,getMulDiv(zTokens),OperatorExpression.OPERATOR_MODULO);
			}else if(tok.getToken().equals("<<")) {
				exp = new OperatorExpression(exp,getMulDiv(zTokens),OperatorExpression.OPERATOR_SHIFTL);
			}else if(tok.getToken().equals(">>")) {
				exp = new OperatorExpression(exp,getMulDiv(zTokens),OperatorExpression.OPERATOR_SHIFTR);
			}else {
				zTokens.goBackToken();
				break;
			}
		}
		
		return exp;
	}

	private static Expression getMulDiv(LexicalTokenizer zTokens) throws MinimaParseException{
		Expression exp = getPrimary(zTokens);
		
		while(zTokens.hasMoreElements()) {
			ScriptToken tok = zTokens.getNextToken();
			
			if(tok.getToken().equals("*")) {
				exp = new OperatorExpression(exp,getPrimary(zTokens),OperatorExpression.OPERATOR_MUL);
			}else if(tok.getToken().equals("/")) {
				exp = new OperatorExpression(exp,getPrimary(zTokens),OperatorExpression.OPERATOR_DIV);
			}else {
				zTokens.goBackToken();
				break;
			}
		}
		
		return exp;
	}
	
	private static Expression getPrimary(LexicalTokenizer zTokens) throws MinimaParseException{
		//The final result
		Expression exp = null; 
		
		//get the Token
		ScriptToken tok = zTokens.getNextToken();
		
		if(tok.getToken().equals("NOT")) {
			exp = new BooleanExpression(getPrimary(zTokens), BooleanExpression.BOOLEAN_NOT);
			
		}else if(tok.getToken().equals("NEG")) {
			exp = new OperatorExpression(getPrimary(zTokens), OperatorExpression.OPERATOR_NEG);
		
		}else {
			zTokens.goBackToken();
			exp = getBaseUnit(zTokens);
		}
		
		return exp;
	}
	
	private static Expression getBaseUnit(LexicalTokenizer zTokens) throws MinimaParseException{
		//The final result
		Expression exp = null; 
		
		//get the Token
		ScriptToken tok = zTokens.getNextToken();
		
		if(tok.getTokenType() == ScriptToken.TOKEN_VALUE) {
			exp = new ConstantExpression( Value.getValue(tok.getToken()) ); 
		
			//Negative NUmbers handled here..
		}else if(tok.getToken().equals("-")) {
			//The next token MUST be a number
			ScriptToken num = zTokens.getNextToken();
			
			//Create a Negative Number 
			MiniNumber numv = new MiniNumber(num.getToken()).mult(MiniNumber.MINUSONE);
			exp = new ConstantExpression(new NumberValue(numv));
			
		}else if(tok.getTokenType() == ScriptToken.TOKEN_GLOBAL) {
			exp = new GlobalExpression(tok.getToken());
		
		}else if(tok.getTokenType() == ScriptToken.TOKEN_VARIABLE) {
			exp = new VariableExpression(tok.getToken());
		
		}else if(tok.getTokenType() == ScriptToken.TOKEN_TRUE) {
			exp = new ConstantExpression(BooleanValue.TRUE);
		
		}else if(tok.getTokenType() == ScriptToken.TOKEN_FALSE) {
			exp = new ConstantExpression(BooleanValue.FALSE);
		
		}else if(tok.getTokenType() == ScriptToken.TOKEN_FUNCTIION) {
			//Which Function
			MinimaFunction func = MinimaFunction.getFunction(tok.getToken());
			
			//Remove the Front bracket.
			ScriptToken bracket = zTokens.getNextToken();
			if(bracket.getTokenType() != ScriptToken.TOKEN_OPENBRACKET) {
				throw new MinimaParseException("Missing opening bracket at start of function "+func.getName());
			}
			
			//Now accept variables until you find the closing bracket
			while(true) {
				//Have we reached the close bracket
				ScriptToken isclosebracket = zTokens.getNextToken();
				
				//It must be a close bracket
				if(isclosebracket.getTokenType() == ScriptToken.TOKEN_CLOSEBRACKET) {
					//That' it then..
					break;
					
				}else {
					//Go Back
					zTokens.goBackToken();
					
					//And get the next expression..
					func.addParameter(getExpression(zTokens));
				}
			}
						
			//Check the correct number of Parameters 
			func.checkParamNumberCorrect();
			
			//Now create the Complete Expression
			exp = new FunctionExpression(func);
			
		}else if(tok.getTokenType() == ScriptToken.TOKEN_OPENBRACKET) {
			//It's a new complete expression
			exp = getExpression(zTokens);
			
			//Next token MUST be a close bracket..
			ScriptToken closebracket = zTokens.getNextToken();
			
			if(closebracket.getTokenType() != ScriptToken.TOKEN_CLOSEBRACKET) {
				throw new MinimaParseException("Missing close bracket. Found : "+closebracket.getToken());
			}
			
		}else{
			throw new MinimaParseException("Incorrect Token in script "+tok.getToken()+" @ "+zTokens.getCurrentPosition());
		}
		
		return exp;
	}	
}
