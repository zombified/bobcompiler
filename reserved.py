class ReservedWords :
	Words = [';', '-', '+', '^', '*', '/', '"', '(', ')', '{', '}', ':', '=', '.', '[', ']', ',', '<', '>', '!',
						'PROGRAM', 'BEGIN', 'END', 'SWITCH', 'CASE', 'DEFAULT', 'WRITE', 'READ', 'FOR', 'TO',
							'STEP', 'DO', 'IF', 'THEN', 'ELSE', 'NUM', 'STRING', 'RETURN', 'ARRAY', 'PROCEDURE']
	WhiteSpace = [ ' ', '\n', '\r', '\t' ]

	SEMICOLON = 0
	MINUS = 1
	PLUS = 2
	POWER = 3
	MULTIPLY = 4
	DIVIDE = 5
	DQUOTE = 6
	LPAREN = 7
	RPAREN = 8
	LCURLY = 9
	RCURLY = 10
	COLON = 11
	EQUALS = 12
	PERIOD = 13
	LBRACKET = 14
	RBRACKET = 15
	COMMA = 16
	LESSTHAN = 17
	GREATERTHAN = 18
	NOT = 19
	PROGRAM = 20
	BEGIN = 21
	END = 22
	SWITCH = 23
	CASE = 24
	DEFAULT = 25
	WRITE = 26
	READ = 27
	FOR = 28
	TO = 29
	STEP = 30
	DO = 31
	IF = 32
	THEN = 33
	ELSE = 34
	NUM = 35
	STRING = 36
	RETURN = 37
	ARRAY = 38
	PROCEDURE = 39

	# Non-Reserved types
	VARIABLE_NAME = 100
	NUMBERLIT = 101
	STRINGLIT = 102
	NEG_NUMBERLIT = 103
	CONDITION = 104
	ISEQUAL = 105
	LESSTHAN_EQUAL = 106
	GREATERTHAN_EQUAL = 107
	NOTEQUAL = 108
	PARENT_SYMBOL_TABLE = 109
	PARAMETER = 110
	REF = 111

	# Statement Types
	NUM_DECLARATION_STATEMENT = 200
	EXP = 201
	WRITE_STATEMENT = 202
	ARRAY_DECLARATION_STATEMENT = 203
	DIMENSION = 204
	NUM_ASSIGNMENT_STATEMENT = 205
	ARRAY_ASSIGNMENT_STATEMENT = 206
	ARRAY_STATEMENT = 207
	READ_STATEMENT = 208
	IF_STATEMENT = 209
	STATEMENT = 210
	FOR_STATEMENT = 211
	PROCEDURE_STATEMENT = 212
	PROCEDURE_CALL_STATEMENT = 213




	PARENT_SYMBOL_NAME = '__________parent____________sym__________'

	def GetValueForReserved( self, token ) :
		if token.upper() in self.Words :
			for i in range( 0, len(self.Words) ) :
				if self.Words[i] == token.upper() :
					return i

			return None
		else :
			return None
