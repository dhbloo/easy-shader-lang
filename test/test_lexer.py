import argparse
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))

from src.lexer import *

arg_parser = argparse.ArgumentParser()
arg_parser.add_argument("file", nargs='?', type=argparse.FileType('r'), default=sys.stdin)
args = arg_parser.parse_args()

lexer = lex.lex()

with args.file as f:
    program_str = f.read()
    print(program_str)
    print('=' * 60)
    
init_lexer_context()
lexer.input(program_str)

while True:
    tok = lexer.token()
    if not tok:
        break  # No more input
    print(tok)
