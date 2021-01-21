package org.minima.tests.kissvm.expressions;

import java.util.List;

import org.junit.Test;
import org.minima.kissvm.Contract;
import org.minima.kissvm.exceptions.MinimaParseException;
import org.minima.kissvm.statements.StatementBlock;
import org.minima.kissvm.statements.StatementParser;
import org.minima.kissvm.tokens.Token;

public class ExpressionParserTests {

    @Test
    public void testConstructors() throws MinimaParseException, Exception {

        String RamScript
                = "let y=BOOL(0 1 2 3 4 adfgasdf) "
                + " IF y EQ 0 THEN "
                + "  let x =1 "
                + "ELSEif y EQ 3 THEN "
                + "  let x=2 "
                + "ELSE"
                + "  let x =3 "
                + "ENDIF let p=0";

        String CleanedRamScript = Contract.cleanScript(RamScript);
        List<Token> tokens = Token.tokenize(CleanedRamScript);
        StatementBlock sb = StatementParser.parseTokens(tokens);

        System.out.println(CleanedRamScript);
        System.out.println(sb.toString());

    }
}
