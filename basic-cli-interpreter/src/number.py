from runtime_result import *
from error import *

class Number:
    def __init__(self, value):
        self.value = value
        self.set_position()

    # primarily for error display
    def set_position(self, position_start=None, position_end=None):
        self.position_start = position_start
        self.position_end = position_end
        return self

    # arithmetic operations

    def addition(self, another_number):
        if isinstance(another_number, Number):
            return Number(self.value + another_number.value), None

    def subtraction(self, another_number):
        if isinstance(another_number, Number):
            return Number(self.value - another_number.value), None

    def multiplication(self, another_number):
        if isinstance(another_number, Number):
            return Number(self.value * another_number.value), None

    def division(self, another_number):
        if isinstance(another_number, Number):
            if another_number.value == 0:
                return None, RunTimeError(another_number.position_start, another_number.position_end, "Division By Zero")
            
            return Number(self.value / another_number.value), None

    # for display

    def __repr__(self):
        return str(self.value)
