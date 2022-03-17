from lexer import Lexer
from parser import Parser
from interpreter import Interpreter

def run(file_name, text):
    # generate tokens
    lexer = Lexer(file_name, text)
    tokens, error = lexer.create_token()
    if error: return None, error
    print(tokens)

    # generate AST
    parser = Parser(tokens)
    ast = parser.parse()
    if ast.error: return None, ast.error

    # execute the program
    interpreter = Interpreter()
    result = interpreter.interpret(ast.node)

    return result.value, result.error

while True:
    text = input("calculator > ")
    if text == 'exit':
        print('bye')
        break
    result, error = run('place_holder', text)

    if error:
        print(error.show_error())
    else:
        print(result)
