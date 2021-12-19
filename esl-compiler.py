import argparse
from sys import stdout
from termcolor import colored

from src.compiler import Compiler

def run():
    arg_parser = argparse.ArgumentParser()
    arg_parser.add_argument("source_file", type=str)
    arg_parser.add_argument("-o", type=str, default="", required=False)
    arg_parser.add_argument("--ast_only", type=bool, default=False)
    args = arg_parser.parse_args()

    with open(args.source_file, 'r', encoding='utf8') as f:
        code_str = f.read()

    compiler = Compiler(ast_only=args.ast_only)
    success = compiler.compile_to_ir(code_str, args.source_file)
    if success:
        compiler.dump_text(stdout)

        if compiler.verify():
            print(colored('Compile success!', color='green'))

            if args.o != "":
                with open(args.o, 'wb') as f:
                    compiler.dump_bitcode(f)
        else:
            print(colored('Error in IR code', color='red'))
            print(colored(f'Error: {compiler.get_error_message()}', color='yellow'))
    else:
        print(colored('Compile failed!', color='red'))
        print(colored(f'Error: {compiler.get_error_message()}', color='yellow'))


if __name__ == "__main__":
    run()
