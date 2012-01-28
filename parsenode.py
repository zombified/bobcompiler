from reserved import ReservedWords

class ParseNode :

	def __init__( self, tokentype, tokenvalue ) :
		self.TokenType = tokentype
		self.TokenValue = tokenvalue
		self.ChildNodes = []

	def AddChild( self, child ) :
		self.ChildNodes.append( child )


	def Print( self, tabLevel=0 ) :
		res = ReservedWords()

		prtok = None
		if self.TokenType == res.VARIABLE_NAME :
			prtok = "VARIABLE_NAME"
		elif self.TokenType == res.NUMBERLIT :
			prtok = "NUMBERLIT"
		elif self.TokenType == res.STRINGLIT :
			prtok = "STRINGLIT"
		elif self.TokenType == res.NUM_DECLARATION_STATEMENT :
			prtok = "NUM_DECLARATION_STATEMENT"
		elif self.TokenType == res.EXP :
			prtok = "EXP"
		elif self.TokenType == res.WRITE_STATEMENT :
			prtok = "WRITE_STATEMENT"
		elif self.TokenType == res.ARRAY_DECLARATION_STATEMENT :
			prtok = "ARRAY_DECLARATION_STATEMENT"
		elif self.TokenType == res.DIMENSION :
			prtok = "DIMENSION"
		elif self.TokenType == res.NUM_ASSIGNMENT_STATEMENT :
			prtok = "NUM_ASSIGNMENT_STATEMENT"
		elif self.TokenType == res.ARRAY_ASSIGNMENT_STATEMENT :
			prtok = "ARRAY_ASSIGNMENT_STATEMENT"
		elif self.TokenType == res.NEG_NUMBERLIT :
			prtok = "NEG_NUMBERLIT"
		elif self.TokenType == res.ARRAY_STATEMENT :
			prtok = "ARRAY_STATEMENT"
		elif self.TokenType == res.READ_STATEMENT :
			prtok = "READ_STATEMENT"
		elif self.TokenType == res.IF_STATEMENT :
			prtok = "IF_STATEMENT"
		elif self.TokenType == res.CONDITION :
			prtok = "CONDITION"
		elif self.TokenType == res.ISEQUAL :
			prtok = "ISEQUAL"
		elif self.TokenType == res.LESSTHAN_EQUAL :
			prtok = "LESSTHAN_EQUAL"
		elif self.TokenType == res.GREATERTHAN_EQUAL :
			prtok = "GREATERTHAN_EQUAL"
		elif self.TokenType == res.NOTEQUAL :
			prtok = "NOTEQUAL"
		elif self.TokenType == res.STATEMENT :
			prtok = "STATEMENT"
		elif self.TokenType == res.FOR_STATEMENT :
			prtok = "FOR_STATEMENT"
		elif self.TokenType == res.PARENT_SYMBOL_TABLE :
			prtok = "PARENT_SYMBOL_TABLE"
		elif self.TokenType == res.PROCEDURE_STATEMENT :
			prtok = "PROCEDURE_STATEMENT"
		elif self.TokenType == res.PARAMETER :
			prtok = "PARAMETER"
		elif self.TokenType == res.REF :
			prtok = "REF"
		elif self.TokenType == res.PROCEDURE_CALL_STATEMENT :
			prtok = "PROCEDURE_CALL_STATEMENT"
		else :
			prtok = res.Words[self.TokenType]

		tabs = ""
		for i in range( 0, tabLevel ) :
			tabs += "\t"

		if self.TokenValue != None :
			print "%s(%s, %s)" % (tabs, self.TokenValue, prtok)
		else :
			print "%s(%s)" % (tabs, prtok)


		if len(self.ChildNodes) > 0 :
			t = tabLevel + 1
			#print "%d child nodes exist" % (len(self.ChildNodes))
			#print "%d child nodes exist in children" % (len(self.ChildNodes[0].ChildNodes))
			for c in self.ChildNodes :
				c.Print( t )
