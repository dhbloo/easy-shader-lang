import argparse
import logging
import sys
import os
sys.path.append(os.path.join(os.path.dirname(__file__), '..'))

from src.lexer import create_lexer, init_lexer_context
from src.parser import create_parser

logging.basicConfig(
    level = logging.DEBUG,
    filename = "parselog.txt",
    filemode = "w",
    format = "%(filename)10s:%(lineno)4d:%(message)s"
)
log = logging.getLogger()
lexer = create_lexer()
parser = create_parser(debug=log)

arg_parser = argparse.ArgumentParser()
arg_parser.add_argument("file", nargs='?', type=argparse.FileType('r'), default=sys.stdin)
args = arg_parser.parse_args()
with args.file as f:
    program_str = f.read()
    print(program_str)
    print('=' * 60)
    
init_lexer_context()
result = parser.parse(program_str, tracking=True)
print(result)
