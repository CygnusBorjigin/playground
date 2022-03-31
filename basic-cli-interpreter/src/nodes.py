class NumberNode:
    def __init__(self, token):
        self.token = token
        self.start_position = self.token.start_position
        self.end_position = self.token.end_position

    def __repr__(self):
        return f'{self.token}'

class BinaryOperationNode:
    def __init__(self, operation_token, left_node, right_node):
        self.operation_token = operation_token
        self.left_node = left_node
        self.right_node = right_node
        self.start_position = self.left_node.start_position
        self.end_position = self.right_node.end_position

    def __repr__(self):
        return f'({self.operation_token} [{self.left_node}, {self.right_node}])'

class UnaryOperationNode:
    def __init__(self, operation_token, node):
        self.operation_token = operation_token
        self.node = node
        self.start_position = self.operation_token.start_position
        self.end_position = self.node.end_position

    def __repr__(self):
        return f'({self.operation_token}: [{self.node}])'


