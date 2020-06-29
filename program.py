import os

import mel_parser
import mel_semantic


def execute(prog: str) -> None:
    prog = mel_parser.parse(prog)

    print('ast:')
    print(*prog.tree, sep=os.linesep)
    print()

    print('semantic_check:')
    try:
        scope = mel_semantic.prepare_global_scope()
        prog.semantic_check(scope)
        print(*prog.tree, sep=os.linesep)
    except mel_semantic.SemanticException as e:
        print('Ошибка: {}'.format(e.message))
        return
    print()
