from token import *
from error import *
from position import Position

class Lexer:
    def __init__(self, file_name, text):
        self.file_name = file_name
        self.text = text
        self.position = Position(-1, 0, -1, file_name, text)
        self.current_char = None
        self.move_one()

    def move_one(self):
        self.position.move_one(self.current_char)
        
        if self.position.index < len(self.text):
            self.current_char = self.text[self.position.index]
        else:
            self.current_char = None

    def create_number(self):
        number_string = ''
        dot_count = 0
        position_start = self.position.copy()
        while self.current_char != None and self.current_char in DIGITS + '.':
            if self.current_char == '.' and dot_count == 0:
                dot_count += 1
                number_string += '.'
            elif self.current_char == '.' and dot_count != 0:
                break
            else:
                number_string += self.current_char
            self.move_one()

        if dot_count == 0:
            return Token(INT_token, int(number_string), position_start, self.position)
        else:
            return Token(FLOAT_token, float(number_string), position_start, self.position)

    def create_token(self):
        tokens = []
        while self.current_char != None:
            if self.current_char in ' \t':
                self.move_one()
            #check if the current is a part of a number
            elif self.current_char in DIGITS:
                tokens.append(self.create_number())
            elif self.current_char == '+':
                tokens.append(Token(PLUS_token, start_position=self.position))
                self.move_one()
            elif self.current_char == '-':
                tokens.append(Token(MINUS_token, start_position=self.position))
                self.move_one()
            elif self.current_char == '*':
                tokens.append(Token(MUL_token, start_position=self.position))
                self.move_one()
            elif self.current_char == '/':
                tokens.append(Token(DIV_token, start_position=self.position))
                self.move_one()
            elif self.current_char == '^':
                tokens.append(Token(EXP_token, start_position=self.position))
                self.move_one()
            elif self.current_char == '(':
                tokens.append(Token(LEFT_P_token, start_position=self.position))
                self.move_one()
            elif self.current_char == ')':
                tokens.append(Token(RIGHT_P_token, start_position=self.position))
                self.move_one()
            else:
                # Error in lexing
                pos_start = self.position.copy()
                char = self.current_char
                self.move_one()
                return [], IllegalCharError(pos_start, self.position, "'" + char + "'")


        tokens.append(Token(END_TOKEN, start_position=self.position))
        return tokens, None

