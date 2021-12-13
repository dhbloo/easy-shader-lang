import argparse
from sys import stdout
from src.compiler import Compiler


def run():
    arg_parser = argparse.ArgumentParser()
    arg_parser.add_argument("source_file", type=str)
    args = arg_parser.parse_args()

    with open(args.source_file, 'r') as f:
        code_str = f.read()

    compiler = Compiler()
    success = compiler.compile(code_str, args.source_file)
    if success:
        print('compile success!')
        compiler.dump_text(stdout)


if __name__ == "__main__":
    run()