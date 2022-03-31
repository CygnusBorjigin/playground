class Error:
    def __init__(self, position_start, position_end, error_name, error_detail):
        self.position_start = position_start
        self.position_end = position_end
        self.error_name = error_name
        self.error_detail = error_detail

    def show_error(self):
        result = f'{self.error_name}: {self.error_detail} in {self.position_start.file_name} line {self.position_start.line_number}'
        return result

class IllegalCharError(Error):
    def __init__(self, position_start, position_end, error_detail):
        super().__init__(position_start, position_end, "Unknown Character", error_detail)

class InvalidSyntaxError(Error):
    def __init__(self, position_start, position_end, error_detail):
        super().__init__(position_start, position_end, "Invalid Syntax", error_detail)
        
class RunTimeError(Error):
    def __init__(self, position_start, position_end, error_detail):
        super().__init__(position_start, position_end, "Run Time Error", error_detail)

