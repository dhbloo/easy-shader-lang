class ParseError(RuntimeError):
    def __init__(self, *args: object) -> None:
        super().__init__(*args)
        


class SemanticError(RuntimeError):
    def __init__(self, *args: object) -> None:
        super().__init__(*args)
   
