from runtime_result import RuntimeResult
from number import *
from token import *
from nodes import *

class Interpreter:
    def interpret(self, node):
        method_name = f"interpret_{type(node).__name__}"
        method = getattr(self, method_name, self.nothing_to_interpret)
        return method(node)

    def nothing_to_interpret(self, node):
        raise Exception(f"No intrepret {type(node).__name__} method defined")

    def interpret_NumberNode(self, node):
        return RuntimeResult().success(Number(node.token.token_value).set_position(node.start_position, node.end_position))

    def interpret_BinaryOperationNode(self, node):
        result = RuntimeResult()
        left = result.register(self.interpret(node.left_node))
        if result.error: return result
        right = result.register(self.interpret(node.right_node))
        if result.error: return result
        
        if str(node.operation_token) == str(PLUS_token):
            interpret_result, error =  left.addition(right)
            
        elif str(node.operation_token) == str(MINUS_token):
            interpret_result, error = left.subtraction(right)

        elif str(node.operation_token) == str(MUL_token):
            interpret_result, error = left.multiplication(right)

        elif str(node.operation_token) == str(DIV_token):
            interpret_result = left.division(right)[0]
            error = left.division(right)[1]

        if error:
            return result.failure(error)
        else:
            return result.success(interpret_result.set_position(node.start_position, node.end_position))

    def interpret_UnaryOperationNode(self, node):
        result = RuntimeResult()
        number = result.register(self.interpret(node.node))
        
        if result.error: return result

        error = None

        if str(node.operation_token) == str(MINUS_token):
            number = number.multiplication(Number(-1))

        if error:
            return result.failure(error)
        else:
            return result.success(number.set_position(node.start_position, node.end_position))



