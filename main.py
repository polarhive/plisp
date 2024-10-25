import os
from ply import lex, yacc

# List of tokens
tokens = (
    'ID',
    'NUMBER',
    'STRING',
    'LPAREN',
    'RPAREN',
    'QUOTE',
)

# Regular expressions for tokens
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_QUOTE = r'\''
t_ignore = ' \t'  # Ignore spaces and tabs

# Reserved words in Lisp
reserved = {
    'setq': 'SETQ',
    'if': 'IF',
    'while': 'WHILE',
    'loop': 'LOOP',
    'defun': 'DEFUN',
}

tokens = tokens + tuple(reserved.values())

# Function for identifier tokens
def t_ID(t):
    r'[a-zA-Z_\-\+\*\/\<\>\=\!\?][a-zA-Z0-9_\-\+\*\/\<\>\=\!\?]*'
    t.type = reserved.get(t.value, 'ID')  # Check for reserved words
    return t

# Function for number tokens
def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

# Function for string tokens
def t_STRING(t):
    r'"[^"]*"'
    t.value = t.value[1:-1]  # Remove the double quotes
    return t

# Function to handle new lines and track line numbers
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Ignore comments (Lisp comments start with ';' and go until the end of the line)
def t_COMMENT(t):
    r';[^\n]*'
    pass  # No return value. Ignore the comment.

# Error handling for illegal characters
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

# Parsing rules for Lisp constructs (setq, if, while, loop, defun)
def p_statement_setq(p):
    '''statement : LPAREN SETQ ID expression RPAREN'''
    print("Variable Assignment (setq):", p[3])

def p_statement_if(p):
    '''statement : LPAREN IF expression expression expression RPAREN'''
    print("If Statement")

def p_statement_while(p):
    '''statement : LPAREN WHILE expression statements RPAREN'''
    print("While Loop")

def p_statement_loop(p):
    '''statement : LPAREN LOOP statements RPAREN'''
    print("Loop")

def p_statement_defun(p):
    '''statement : LPAREN DEFUN ID LPAREN parameters RPAREN statements RPAREN'''
    print("Function Definition:", p[3])

# Parsing multiple parameters in function definitions
def p_parameters(p):
    '''parameters : ID
                  | ID parameters'''
    pass

# Expression handling for numbers, strings, identifiers, and nested expressions
def p_expression(p):
    '''expression : NUMBER
                  | STRING
                  | ID
                  | LPAREN operator expression_list RPAREN'''
    p[0] = p[1]

# Handle operators and function calls inside parentheses
def p_operator(p):
    '''operator : ID
                | '+' 
                | '-' 
                | '*' 
                | '/'
                | '<' 
                | '>'
                | '='
                '''
    p[0] = p[1]

# Handle a list of expressions (e.g., arguments to a function or operator)
def p_expression_list(p):
    '''expression_list : expression
                       | expression expression_list'''
    pass

# Multiple statements handling
def p_statements_multiple(p):
    '''statements : statement statements'''
    pass

# Single statement handling
def p_statements_single(p):
    '''statements : statement'''
    pass

# Error handling for parsing
def p_error(p):
    if p:
        print("Syntax error at", p.value)
    else:
        print("Syntax error at EOF")

# Build the parser
parser = yacc.yacc()

# Function to read Lisp files and parse them
def load_and_parse_file(filename):
    try:
        with open(filename, 'r') as file:
            data = file.read()
            print(f"\nParsing {filename}...\n")
            lexer.input(data)
            parser.parse(data)
    except FileNotFoundError:
        print(f"Error: File '{filename}' not found.")
    except Exception as e:
        print(f"Error reading file '{filename}': {e}")

# Test with Lisp files in the 'tests/' directory
files_to_parse = ['tests/if.lisp', 'tests/while.lisp', 'tests/var.lisp']
for filename in files_to_parse:
    load_and_parse_file(filename)
