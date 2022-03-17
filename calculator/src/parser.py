from string_with_arrows import *
from parse_result import *
from parse_result import *
from token import *
from nodes import *
from error import *

class Parser:
    # The parser will takes in a list of Token type as input:25
    def __init__(self, tokens):
        self.tokens = tokens
        self.token_index = -1
        self.move_one()

    def move_one(self):
        self.token_index += 1
        
        if self.token_index < len(self.tokens):
            self.current_token = self.tokens[self.token_index]

        return self.current_token
    
    # INT | FLOAT
    def factor(self):
        result = ParseResult()
        consider = self.current_token

        if consider.token_type in (PLUS_token, MINUS_token):
            result.register(self.move_one())
            factor = result.register(self.factor())
            if result.error:
                return result
            return result.success(UnaryOperationNode(consider, factor))

        elif consider.token_type == LEFT_P_token:
            result.register(self.move_one())
            expression = result.register(self.expression())
            if result.error:
                return result
            if self.current_token.token_type == RIGHT_P_token:
                result.register(self.move_one())
                return result.success(expression)
            else:
                return result.failure(InvalidSyntaxError(self.current_token.start_position, self.current_token.end_position, "Expected )"))

        elif consider.token_type in (INT_token, FLOAT_token):
            result.register(self.move_one())
            return result.success(NumberNode(consider))
        # no integer or float is found
        return result.failure(InvalidSyntaxError(consider.start_position, consider.end_position, "Expected INT for FLOAT"))

    # factor ([MUL | DIV] factor)*
    def term(self):
        return self.binary_operation(self.factor, (MUL_token, DIV_token))

    # term ([ADD | SUB] term)*
    def expression(self):
        return self.binary_operation(self.term,(PLUS_token, MINUS_token))

    def binary_operation(self, func, oper):
        result = ParseResult()
        left = result.register(func())
        if result.error:
            return result

        while self.current_token.token_type in oper:
            operation_token = self.current_token
            result.register(self.move_one())
            right = result.register(func())
            if result.error:
                return result
            left = BinaryOperationNode(operation_token, left, right)

        return result.success(left)

    def parse(self):
        result = self.expression()
        # All parseable token in parsed & we have reached the END token -> error in parsing
        if not result.error and self.current_token.token_type != END_TOKEN:
            return result.failure(InvalidSyntaxError(self.current_token.start_position, self.current_token.end_position, "Expected, '+', '-', '*', '/'"))
        return result 
