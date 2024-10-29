import ply.lex as lex
import ply.yacc as yacc

# ---------------- Lexer -----------------

# Reserved keywords and token types
reserved = {
    'if': 'IF',
    'while': 'WHILE',
    'setq': 'SETQ',
    'true': 'TRUE',
    'false': 'FALSE'
}

tokens = [
    'LPAREN', 'RPAREN',
    'NUMBER', 'VARIABLE',
    'PLUS', 'MINUS', 'MULT', 'DIV'
] + list(reserved.values())

# Token regex patterns
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_PLUS = r'\+'
t_MINUS = r'-'
t_MULT = r'\*'
t_DIV = r'/'

# Define literals and reserved word tokens
def t_VARIABLE(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'VARIABLE')  # Check for reserved words
    return t

def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

# Skip whitespace
t_ignore = ' \t'

# Error handling for invalid tokens
def t_error(t):
    print(f"Illegal character '{t.value[0]}'")
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

# ---------------- Parser -----------------

# Parsing rules
def p_program(p):
    '''program : statement
               | statement program'''
    pass

def p_statement_if(p):
    '''statement : LPAREN IF condition statement statement RPAREN 
                    | LPAREN IF condition statement RPAREN '''
    pass

def p_statement_while(p):
    '''statement : LPAREN WHILE condition statement RPAREN'''
    pass

def p_statement_assign(p):
    '''statement : LPAREN SETQ VARIABLE expression RPAREN'''
    pass

def p_condition(p):
    '''condition : expression
                 | TRUE
                 | FALSE'''
    pass

def p_expression(p):
    '''expression : VARIABLE
                  | NUMBER
                  | LPAREN operator expression expression RPAREN'''
    pass

def p_operator(p):
    '''operator : PLUS
                | MINUS
                | MULT
                | DIV'''
    pass

# Error rule for syntax errors
def p_error(p):
    global syntax_error
    syntax_error = True;
    if p:
        print(f"Syntax error at '{p.value}'")
    else:
        print("Syntax error at EOF")

# Build the parser
parser = yacc.yacc()

# ---------------- Test Cases -----------------

# Sample LISP code strings as comments
code_samples = [
    "(if true (setq x 10) (setq y 20))",       # Valid `if` statement
    "(while false (setq x (+ x 1)))",          # Valid `while` statement
    "(setq z (* 5 5))",                        # Valid assignment
    "(if (setq x 10) (setq y 20))",            # Invalid `if` syntax
    "(while true setq x 10)",                  # Invalid `while` syntax
    "(setq (+ x 10))"                          # Invalid assignment
]

def parse_input(code):
    global syntax_error
    syntax_error = False

    parser.parse(code)
    if syntax_error:
        print("Rejected")
    else:
        print("Accepted")

# Test each code sample
for i, code in enumerate(code_samples, start=1):
    syntax_error = False
    print(f"\n--- Sample {i} ---")
    print("Code:", code)

    # Lexical analysis
    print("\nTokens:")
    lexer.input(code)
    tokens = []
    for tok in lexer:
        tokens.append(tok)
        print(tok)

    # Parse the input
    print("\nParsing Result:")
    parse_input(code)   
