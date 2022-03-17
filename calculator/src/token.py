# token variables

INT_token = 'INT'
FLOAT_token = 'FLOAT'
PLUS_token = 'PLUS'
MINUS_token = 'MINUS'
MUL_token = 'MUL'
DIV_token = 'DIV'
EXP_token = 'EXP'
LEFT_P_token = 'LP'
RIGHT_P_token = 'RP'

END_TOKEN = 'END_OF_FILE'

DIGITS = '0123456789'

# token structure

class Token:
    def __init__(self, token_type , token_value=None, start_position=None, end_position=None):
        self.token_type = token_type
        self.token_value = token_value

        if start_position:
            self.start_position = start_position
            self.end_position = start_position
            self.end_position.move_one()

        if end_position:
            self.end_position = end_position

    def __repr__(self):
        if self.token_value: return f'{self.token_type}: {self.token_value}'
        return f'{self.token_type}'


