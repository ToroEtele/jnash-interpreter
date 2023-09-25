package com.interpreter.nash;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static com.interpreter.nash.TokenType.*;

/**
 * The parsing problem: The binary rule lets operands nest any which way you want, the same strings can produce different syntax trees.
 * To address this ambiguity we have to define rules for:
 * Precedence: which operator is evaluated first. Operators with higher precedence are evaluated before operators with lower precedence
 * Associativity: determines which operator is evaluated first in a series of the same operator.
 * Complete expression grammar: ( * represents a loop)
 * program → declaration* EOF
 * declaration → varDecl | statement
 * varDecl → "var" IDENTIFIER ( "=" expression)? ";"
 * statement → exprStmt | forStmt | ifStmt | whileStmt | printStmt | block
 * forStmt → "for" "(" ( varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement
 * whileStmt → "while" "(" expression ")" statement ;
 * exprStmt → expression";"
 * ifStmt → "if" "(" expression ")" statement ( "else" statement )?
 * printStmt → "print" expression ";"
 * block → "{" declaration* "}"
 * expression → assignment
 * assignment → IDENTIFIER "=" assignment | logic_or
 * logic_or → logic_and ( "or" logic_and )* ;
 * logic_and → equality ( "and" equality )* ;
 * equality → comparison ( ( "!=" | "==") comparison )*
 * comparison → term ( ( ">" | ">=" | "<" | "<=") term)*
 * term → factor ( ( "-" | "+") factor)*
 * factor → unary ( ( "/" | "*") unary)*
 * unary → ( "!" | "-") unary | primary
 * primary → NUMBER | STRING | "true" | "false" | "nil" | "(" expression ")" | IDENTIFIER
 */
public class Parser {
    private static class ParseError extends RuntimeException {}
    private final List<Token> tokens;
    private int current = 0;

    Parser(List<Token> tokens) {
        this.tokens = tokens;
    }

    List<Stmt> parse() {
        List<Stmt> statements = new ArrayList<>();
        while (!isAtEnd()) {
            statements.add(declaration());
        }
        return statements;
    }

    /**
     * This declaration() method is the method we call repeatedly when parsing a series of statements in a block or a script.
     * This is the right place to synchronize when the parser goes into panic mode.
     * */
    private Stmt declaration() {
        try {
            if (match(VAR)) return varDeclaration();
            return statement();
        } catch (ParseError error) {
            synchronize();
            return null;
        }
    }

    private Stmt varDeclaration() {
        Token name = consume(IDENTIFIER, "Expect variable name.");
        Expr initializer = null;
        if (match(EQUAL)) {
            initializer = expression();
        }
        consume(SEMICOLON, "Expect ';' after variable declaration.");
        return new Stmt.Var(name, initializer);
    }

    private Stmt statement() {
        if (match(FOR)) return forStatement();
        if (match(IF)) return ifStatement();
        if(match(PRINT)) return printStatement();
        if (match(WHILE)) return whileStatement();
        if (match(LEFT_BRACE)) return new Stmt.Block(block());
        return expressionStatement();
    }

    private Stmt forStatement() {
        consume(LEFT_PAREN, "Expect '(' after 'for'.");

        Stmt initializer;
        // Initializer not used
        if (match(SEMICOLON)) {
            initializer = null;
        } else if (match(VAR)) {    //Declared initializer
            initializer = varDeclaration();
        } else {    // Expression initializer, possibly an assignment
            initializer = expressionStatement();
        }

        Expr condition = null;
        if (!check(SEMICOLON)) {
            condition = expression();
        }
        consume(SEMICOLON, "Expect ';' after loop condition.");

        Expr increment = null;
        if (!check(RIGHT_PAREN)) {
            increment = expression();
        }
        consume(RIGHT_PAREN, "Expect ')' after for clauses.");

        // First we evaluate the body of the for loop as a statement, because for ex. nested loops are allowed
        Stmt body = statement();

        // If the increment is defined, we assign to the end of the statement list
        if (increment != null) {
            body = new Stmt.Block(
                Arrays.asList(
                    body,
                    new Stmt.Expression(increment)
                )
            );
        }
        // If the condition isn't specified in the for loop, we evaluate as always true
        if (condition == null) condition = new Expr.Literal(true);

        // Add the condition before the statements from the loop's body
        body = new Stmt.While(condition, body);

        // If the initializer is not null, we add to the beginning of the list
        if (initializer != null) {
            body = new Stmt.Block(Arrays.asList(initializer, body));
        }

        return body;
    }

    private Stmt whileStatement() {
        consume(LEFT_PAREN, "Expect '(' after 'while'.");
        Expr condition = expression();
        consume(RIGHT_PAREN, "Expect ')' after condition.");
        Stmt body = statement();
        return new Stmt.While(condition, body);
    }

    private Stmt ifStatement() {
        consume(LEFT_PAREN, "Expect '(' after 'if'.");
        Expr condition = expression();
        consume(RIGHT_PAREN, "Expect ')' after if condition.");
        Stmt thenBranch = statement();
        Stmt elseBranch = null;
        if (match(ELSE)) {
            elseBranch = statement();
        }
        return new Stmt.If(condition, thenBranch, elseBranch);
    }

    private List<Stmt> block() {
        List<Stmt> statements = new ArrayList<>();
        while (!check(RIGHT_BRACE) && !isAtEnd()) {
            statements.add(declaration());
        }
        consume(RIGHT_BRACE, "Expect '}' after block.");
        return statements;
    }

    private Stmt printStatement() {
        Expr value = expression();
        consume(SEMICOLON, "Expect ';' after value.");
        return new Stmt.Print(value);
    }

    private Stmt expressionStatement() {
        Expr expr = expression();
        consume(SEMICOLON, "Expect ';' after expression.");
        return new Stmt.Expression(expr);
    }

    /**
     * The top expression rule matches any expression at any precedence level. This could be eliminated, because equality has
     * the lowest precedence, but in this way the code becomes readable: we evaluate the parts inside a (...) as an expression.
    */
    private Expr expression() {
        return assignment();
    }

    private Expr assignment() {
        Expr expr = or();
        if (match(EQUAL)) {
            Token equals = previous();
            Expr value = assignment();
            if (expr instanceof Expr.Variable) {
                Token name = ((Expr.Variable) expr).name;
                return new Expr.Assign(name, value);
            }
            error(equals, "Invalid assignment target.");
        }
        return expr;
    }

    private Expr or() {
        Expr expr = and();
        while (match(OR)) {
            Token operator = previous();
            Expr right = and();
            expr = new Expr.Logical(expr, operator, right);
        }
        return expr;
    }

    private Expr and() {
        Expr expr = equality();
        while (match(AND)) {
            Token operator = previous();
            Expr right = equality();
            expr = new Expr.Logical(expr, operator, right);
        }
        return expr;
    }

    /**
     * Equality is right associative, so we first evaluate the left side, with the rule with 1 higher precedence level.
     * When the left side is evaluated, we are searching for the same precedence rules on this level ( ex. x == y === z)
     * Since the match() increments the current, the operator becomes the previous token
     * Then the right token gets evaluated, and the function sets the expr to a binary expression.
     * If the next token is still the same precedence level the left side of the binary becomes the previous binary,
     * and the right side gets evaluated ( ex. Binary(Binary(x, '==', y), '==', z) )
     * */
    private Expr equality() {
        Expr expr = comparison();
        while (match(BANG_EQUAL, EQUAL_EQUAL)) {
            Token operator = previous();
            Expr right = comparison();
            expr = new Expr.Binary(expr, operator, right);
        }
        return expr;
    }

    /**
     * Most of the following functions implement the same logic, they almost all are right associative rule,
     * But every of them should call the 1 level higher precedence level rule's evaluator
     * */
    private Expr comparison() {
        Expr expr = term();
        while (match(GREATER, GREATER_EQUAL, LESS, LESS_EQUAL)) {
            Token operator = previous();
            Expr right = term();
            expr = new Expr.Binary(expr, operator, right);
        }
        return expr;
    }

    private Expr term() {
        Expr expr = factor();
        while (match(MINUS, PLUS)) {
            Token operator = previous();
            Expr right = factor();
            expr = new Expr.Binary(expr, operator, right);
        }
        return expr;
    }

    private Expr factor() {
        Expr expr = unary();
        while (match(SLASH, STAR)) {
            Token operator = previous();
            Expr right = unary();
            expr = new Expr.Binary(expr, operator, right);
        }
        return expr;
    }

    /**
     * Unary operation is like → ( "!" | "-" ) unary this is a right associative operator,
     * so the right side should be evaluated first.
     * */
    private Expr unary() {
        if (match(BANG, MINUS)) {
            Token operator = previous();
            Expr right = unary();
            return new Expr.Unary(operator, right);
        }
        return primary();
    }

    /**
     * Primary rule has the highest precedence level, this is at the deepest point in the recursion.
     * This rule is evaluated first. It can be represented like this:
     * primary → NUMBER | STRING | "true" | "false" | "null" | "(" expression ")" ;
     * As you can see an inner expression is also evaluated as a primary element, but in it's the resolver the expression() function is called.
     * */
    private Expr primary() {
        if (match(FALSE)) return new Expr.Literal(false);
        if (match(TRUE)) return new Expr.Literal(true);
        if (match(NIL)) return new Expr.Literal(null);
        if (match(NUMBER, STRING)) {
            return new Expr.Literal(previous().literal);

        }
        if (match(IDENTIFIER)) {
            return new Expr.Variable(previous());
        }
        if (match(LEFT_PAREN)) {
            Expr expr = expression();
            consume(RIGHT_PAREN, "Expect ')' after expression.");
            return new Expr.Grouping(expr);
        }
        throw error(peek(), "Expect expression.");
    }

    /**
     * This function receives a set of tokenTypes, checks if the current token is one of these, and if yes, increments the current
     * */
    private boolean match(TokenType... types) {
        for (TokenType type : types) {
            if (check(type)) {
                advance();
                return true;
            }
        }
        return false;
    }

    /**
     * This function checks if the current token has the expected type, if so, increments the current position, else it throws an error
     * @param type Expected token
     * @param message Error message to print if the next token is not the expected type
     * */
    private Token consume(TokenType type, String message) {
        if (check(type)) return advance();
        throw error(peek(), message);
    }

    /**
     * @param type The type of the expected token
     * @return boolean, is the current token the expected type
     * */
    private boolean check(TokenType type) {
        if (isAtEnd()) return false;
        return peek().type == type;
    }

    /**
     * This function increments the current if we are not at EOF
     * @return returns the previous character after increment
     * */
    private Token advance() {
        if (!isAtEnd()) current++;
        return previous();
    }

    private boolean isAtEnd() {
        return peek().type == EOF;
    }

    private Token peek() {
        return tokens.get(current);
    }

    private Token previous() {
        return tokens.get(current - 1);
    }

    private ParseError error(Token token, String message) {
        Nash.error(token, message);
        return new ParseError();
    }

    private void synchronize() {
        advance();
        while (!isAtEnd()) {
            if (previous().type == SEMICOLON) return;
            switch (peek().type) {
                case CLASS:
                case FUN:
                case VAR:
                case FOR:
                case IF:
                case WHILE:
                case PRINT:
                case RETURN:
                    return;
            }
            advance();
        }
    }
}

