import ply.lex as lex
import ply.yacc as yacc

# Flag to indicate syntax errors
syntax_error = False

# --------------- Lexer -------------------

# Reserved keywords, updated to include 'defun'
reserved = {
    'if': 'IF',
    'while': 'WHILE',
    'setq': 'SETQ',
    'true': 'TRUE',
    'false': 'FALSE',
    'defun': 'DEFUN',
    'defstruct' : 'DEFSTRUCT',
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

# Updated VARIABLE token rule to handle reserved keywords
def t_VARIABLE(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value, 'VARIABLE')  # Check if the token is a reserved keyword
    return t

def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

# Ignore whitespace
t_ignore = ' \t'

# Error handling for invalid tokens
def t_error(t):
    print(f"Illegal character '{t.value[0]}' at line {t.lineno}")
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

# --------------- Parser -----------------

def p_program(p):
    '''program : statement
               | statement program'''
    pass

# If statement
def p_statement_if(p):
    '''statement : LPAREN IF condition statement RPAREN'''
    pass

# While statement
def p_statement_while(p):
    '''statement : LPAREN WHILE condition statement RPAREN'''
    pass

# Variable assignment
def p_statement_assign(p):
    '''statement : LPAREN SETQ VARIABLE expression RPAREN'''
    pass

# Structure definition
def p_statement_defstruct(p):
    '''statement : LPAREN DEFSTRUCT VARIABLE parameters RPAREN'''

# Function definition
def p_statement_defun(p):
    '''statement : LPAREN DEFUN VARIABLE LPAREN parameters RPAREN expression RPAREN'''
    pass

# Parameters for function definition
def p_parameters(p):
    '''parameters : VARIABLE
                  | VARIABLE parameters
                  | empty'''
    pass

# Condition
def p_condition(p):
    '''condition : expression
                 | TRUE
                 | FALSE'''
    pass

# Expressions
def p_expression(p):
    '''expression : VARIABLE
                  | NUMBER
                  | LPAREN operator expression expression RPAREN
                  | LPAREN VARIABLE arguments RPAREN'''  # For function calls
    pass

# Operator
def p_operator(p):
    '''operator : PLUS
                | MINUS
                | MULT
                | DIV'''
    pass

# Arguments for function call
def p_arguments(p):
    '''arguments : expression
                 | expression arguments
                 | empty'''
    pass

# Empty rule for optional parameters and arguments
def p_empty(p):
    'empty :'
    pass

# Error handling rule
def p_error(p):
    global syntax_error
    syntax_error = True
    if p:
        print(f"Syntax error at '{p.value}' (line {p.lineno}, position {p.lexpos})")
    else:
        print("Syntax error at EOF")

# Build the parser
parser = yacc.yacc()

# --------------- Test Cases -------------------

# Sample test code in the Lisp-like syntax
code_samples = [
    # If Statements
    "(if true (setq x 10) (setq y 20))",  # Simple if statement
    "(if true (setq x 10) (setq))",  # Invalid if statement (missing second argument for setq)
    "(if (setq x 10) (setq y 20))",  # Invalid if statement (missing else branch)
    "(while false (setq x (+ x 1)))",  # Simple while loop
    "(while x (setq y (+ y 1)))",  # Invalid while loop condition (should be a boolean expression)
    "(while true (setq a (+ a 2) (setq b 10)))",  # Invalid while loop (incorrect parentheses in setq)
    "(setq z (* 5 5))",  # Simple assignment
    "(setq x (+ 5))",  # Invalid assignment (missing second operand in the addition)
    "(defun square (x) (* x x))",  # Function definition
    "(defun add (x y) (+ x y))",  # Another valid function definition
    "(defun square () (+ x x))",  # Invalid function definition (missing parameter for square)
    "(defun multiply (x y) (+ x))",  # Invalid function definition (missing second operand in addition)
    "(defun add (x) (+ x y))",  # Invalid function definition (using undefined variable y)
    "(defstruct person name age)",  # Structure definition
    "(defstruct car model year)",  # Another valid structure definition
    "(defstruct person age)",  # Invalid structure definition (missing name parameter)
    "(defstruct car make)",  # Invalid structure definition (missing additional fields like model or year)
]

# Function to parse input code and print results
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
    print("\nTokens:")
    lexer.input(code)
    for tok in lexer:
        print(tok)
    print("\nParsing Result:")
    parse_input(code)
