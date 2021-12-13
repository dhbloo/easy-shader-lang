import argparse
from sys import stdout
from termcolor import colored

from src.compiler import Compiler

def run():
    arg_parser = argparse.ArgumentParser()
    arg_parser.add_argument("source_file", type=str)
    arg_parser.add_argument("--ast_only", type=bool, default=False)
    args = arg_parser.parse_args()

    with open(args.source_file, 'r') as f:
        code_str = f.read()

    compiler = Compiler(ast_only=args.ast_only)
    success = compiler.compile(code_str, args.source_file)
    if success:
        print(colored('Compile success!', color='green'))
        compiler.dump_text(stdout)
    else:
        print(colored('Compile failed!', color='red'))
        print(colored(f'Error: {compiler.get_error_message()}', color='yellow'))


if __name__ == "__main__":
    run()