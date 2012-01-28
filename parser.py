from lexer import Lexer
from reserved import ReservedWords
from symboltable import Symbol,SymbolTable
from parsenode import ParseNode

class Parser :
	_reserved = ReservedWords()
	_src = None
	_lexedSrc = None
	_tree = None

	_curPos = 0

	_symTable = SymbolTable()

	_closureCount = 0

	def __init__( self, src ) :
		self._src = src
		self._stripComments()

		lex = Lexer( self._src )
		self._lexedSrc = lex.LexedSrc()

		self._createTree()


	def GetTree( self ) :
		return self._tree

	def GetSymbolTable( self ) :
		return self._symTable


	def _stripComments( self ) :
		opentag = self._src.find( '/*' )
		while opentag != -1 :
			endtag = self._src.find( '*/', opentag + 2 )
			if endtag == -1 :
				print "un-closed multi-line comment detected"
				exit(0)
			self._src = self._src[:opentag] + self._src[endtag+2:]
			opentag = self._src.find( '/*' )

		opentag = self._src.find( '//' )
		while opentag != -1 :
			endtag = self._src.find( '\n', opentag + 2 )
			if endtag == -1 :
				self._src = self._src[:opentag]
			else :
				self._src = self._src[:opentag] + self._src[endtag+1:]
			opentag = self._src.find( '//' )

	def _eat( self, token, errormsg ) :
		if self._lexedSrc[self._curPos][0] != token :
			print errormsg
			exit(0)
		self._curPos += 1
		return self._lexedSrc[self._curPos-1]



	def _createTree( self ) :
		self._tree = self._program()


	def _program( self ) :
		# program
		self._eat( self._reserved.PROGRAM, "No program definition found." )
		tree = ParseNode( self._reserved.PROGRAM, None )

		# variable name
		self._variableName( tree )

		# semi-colon
		self._eat( self._reserved.SEMICOLON, "No termination after program variable name." )

		# begin
		self._eat( self._reserved.BEGIN, "No begin statement." )

		# statements!
		self._statement( tree, self._symTable )

		# end
		self._eat( self._reserved.END, "No end statement." )

		# period.
		self._eat( self._reserved.PERIOD, "No period after end statement." )

		return tree


	def _variableName( self, parent ) :
		# variable name
		variable = self._eat( self._reserved.VARIABLE_NAME, "Variable name expected." )

		if len(variable[1]) <= 0 :
			print "Variable name not specified."
			exit(0)

		if variable[0] in self._reserved.Words :
			print "Variable name '%s' is a reserved word." % (variable[0])
			exit(0)

		node = None
		if self._isLetter( variable[1][0] ) and self._isTheta(variable[1][1:]) :
			node = ParseNode( variable[0], variable[1] )
			parent.AddChild( node )
		else :
			print "'%s' is an invalid variable name." % (variable[1])
			exit(0)

		return node

	def _isLetter( self, character ) :
		charval = ord(character.upper())
		if charval < 65 or charval > 90 :
			return False

		return True

	def _isDigit( self, digit ) :
		digitval = ord(digit)
		if digitval < 48 or digitval > 57 :
			return False

		return True

	def _isTheta( self, val ) :
		if val == None or val == '' :
			return True

		if self._isVarCharacter( val[0] ) and self._isTheta( val[1:] ) :
			return True

		return False

	def _isVarCharacter( self, character ) :
		if character == None or character == '' :
			return False

		if character == '_' or self._isLetter( character ) or self._isDigit( character ) :
			return True

		return False



	def _statement( self, parent, symbolTable, parentClosures=[] ) :
		nexttoken = self._lexedSrc[self._curPos]

		# NUM
		if nexttoken[0] == self._reserved.NUM :
			self._numDeclarationStatement( parent, symbolTable )
			self._statement( parent, symbolTable )

		# STRING
		elif nexttoken[0] == self._reserved.STRING :
			pass

		# WRITE
		elif nexttoken[0] == self._reserved.WRITE :
			self._writeStatement( parent, symbolTable )
			self._statement( parent, symbolTable )

		# READ
		elif nexttoken[0] == self._reserved.READ :
			self._readStatement( parent, symbolTable )
			self._statement( parent, symbolTable )

		# ARRAY DECLARATION
		elif nexttoken[0] == self._reserved.ARRAY :
			self._arrayStatement( parent, symbolTable )
			self._statement( parent, symbolTable )

		# Assignment statement of some sort, or procedure call
		elif nexttoken[0] == self._reserved.VARIABLE_NAME :
#			print len(parentClosures)
#			symbolTable.Print()
#			print "-----------------"
			sym = symbolTable.GetSymbol( nexttoken[1] )
			if sym == None :
				i = len(parentClosures)-1
				while i > -1 :
#					print "aaaa"
#					parentClosures[i].Print()
#					print "-----------------"
					sym = parentClosures[i].GetSymbol( nexttoken[1] )
					if sym != None :
						break
					i -= 1

			if sym == None :
				print "'%s' -- variable not defined." % (nexttoken[1])
				exit(0)

			# NUMBER
			if sym.Type == self._reserved.NUM :
				self._numAssignmentStatement( parent, symbolTable )
				self._statement( parent, symbolTable )

			# STRING
			elif sym.Type == self._reserved.STRING :
				pass

			# ARRAY
			elif sym.Type == self._reserved.ARRAY :
				self._arrayAssignmentStatement( parent, symbolTable )
				self._statement( parent, symbolTable )

			# procedure call
			elif sym.Type == self._reserved.PROCEDURE :
				self._procedureCallStatement( parent, symbolTable )
				self._statement( parent, symbolTable )

			else :
				print "There was no type associated with the variable name in the symbol table. Please consider giving it a type!"
				exit(0)

		# IF
		elif nexttoken[0] == self._reserved.IF :
			self._ifStatement( parent, symbolTable, parentClosures )
			self._statement( parent, symbolTable, parentClosures )

		# FOR
		elif nexttoken[0] == self._reserved.FOR :
			self._forStatement( parent, symbolTable, parentClosures )
			self._statement( parent, symbolTable, parentClosures )

		# PROCEDURE
		elif nexttoken[0] == self._reserved.PROCEDURE :
			self._procedureStatement( parent, symbolTable )
			self._statement( parent, symbolTable )

	def _numDeclarationStatement( self, parent, symbolTable ) :
		node = ParseNode( self._reserved.NUM_DECLARATION_STATEMENT, None )
		parent.AddChild( node )

		# NUM
		self._eat( self._reserved.NUM, "Expected num declaration statement." )

		# VARIABLE NAME
		varval = self._variableName( node )
		if not symbolTable.AddSymbol( Symbol( self._reserved.NUM, varval.TokenValue ) ) :
			print "Variable '%s' already used in declaration statement or as a parameter." % (varval.TokenValue)
			exit(0)

		# ; or EXP
		if self._lexedSrc[self._curPos][0] == self._reserved.SEMICOLON :
			self._eat( self._reserved.SEMICOLON, "Expected semi colon after number declaration statement." )
		else :
			# =
			self._eat( self._reserved.EQUALS, "Expected equals sign in number declaration statement." )

			# EXP
			self._exp( node, symbolTable )

			# ;
			self._eat( self._reserved.SEMICOLON, "Expected semi colon after number declaration statement (that's assigning something)" )

	def _writeStatement( self, parent, symbolTable ) :
		# WRITE
		self._eat( self._reserved.WRITE, "Write statement expected, but not found." )
		node = ParseNode( self._reserved.WRITE_STATEMENT, None )

		# string literal
		tokvar = self._lexedSrc[self._curPos]
		if tokvar[0] == self._reserved.STRINGLIT :
			node.AddChild( ParseNode( tokvar[0], tokvar[1] ) )
			self._eat( self._reserved.STRINGLIT, "String literal expected, but not found." )

		# array statement
		elif tokvar[0] == self._reserved.VARIABLE_NAME :
			nexttokvar = self._lexedSrc[self._curPos+1]
			if nexttokvar[0] == self._reserved.LBRACKET :
				arrNode = ParseNode( self._reserved.ARRAY_STATEMENT, None )
				node.AddChild( arrNode )

				# varname
				arrNode.AddChild( ParseNode( tokvar[0], tokvar[1] ) )
				self._eat( self._reserved.VARIABLE_NAME, "Expected variable name." )

				# [
				self._eat( self._reserved.LBRACKET, "Expected '['." )

				# 5,3,2,5]
				self._arrayAssignmentList( arrNode, False )

			else :
				self._exp( node, symbolTable )

		# otherwise it should be a valid expression
		else :
			self._exp( node, symbolTable )

		parent.AddChild( node )

		# ;
		self._eat( self._reserved.SEMICOLON, "Semi colon expected after write statement, but not found." )

	def _readStatement( self, parent, symbolTable ) :
		# READ
		self._eat( self._reserved.READ, "Read statement expected, but not found." )
		node = ParseNode( self._reserved.READ_STATEMENT, None )
		parent.AddChild( node )

		# variable name!
		variable = self._eat( self._reserved.VARIABLE_NAME, "Read statement expectes variable name." )
		node.AddChild( ParseNode( variable[0], variable[1] ) )

		# ;
		self._eat( self._reserved.SEMICOLON, "Semi-colon expected after read statement." )



	def _exp( self, parent, symbolTable ) :
		expNode = ParseNode( self._reserved.EXP, None )
		parent.AddChild( expNode )
		self._li( expNode, symbolTable )
		self._ae( expNode, symbolTable )

	def _li( self, parent, symbolTable ) :
		curtoken = self._lexedSrc[self._curPos]

		# PAREN -- start of
		if curtoken[0] == self._reserved.LPAREN :
			self._paren( parent, symbolTable )

		# Negative number or Positive number or variable name
		elif curtoken[0] == self._reserved.MINUS or curtoken[0] == self._reserved.NUMBERLIT or  curtoken[0] == self._reserved.VARIABLE_NAME :
			self._op( parent, symbolTable )

		else :
			print "Unexpected token in expression."
			exit(0)

	def _ae( self, parent, symbolTable ) :
		curtoken = self._lexedSrc[self._curPos]

		if curtoken[0] == self._reserved.PLUS :
			self._eat( self._reserved.PLUS, "Expected '+', but did not find one." )
			parent.AddChild( ParseNode( self._reserved.PLUS, None ) )
			self._li( parent, symbolTable )
			self._ae( parent, symbolTable )

		elif curtoken[0] == self._reserved.MINUS :
			self._eat( self._reserved.MINUS, "Expected '-', but did not find one." )
			parent.AddChild( ParseNode( self._reserved.MINUS, None ) )
			self._li( parent, symbolTable )
			self._ae( parent, symbolTable )

		elif curtoken[0] == self._reserved.MULTIPLY :
			self._eat( self._reserved.MULTIPLY, "Expected '*', but did not find one." )
			parent.AddChild( ParseNode( self._reserved.MULTIPLY, None ) )
			self._li( parent, symbolTable )
			self._ae( parent, symbolTable )

		elif curtoken[0] == self._reserved.DIVIDE :
			self._eat( self._reserved.DIVIDE, "Expected '/', but did not find one." )
			parent.AddChild( ParseNode( self._reserved.DIVIDE, None ) )
			self._li( parent, symbolTable )
			self._ae( parent, symbolTable )

		elif curtoken[0] == self._reserved.POWER :
			self._eat( self._reserved.POWER, "Expected '^', but did not find one." )
			parent.AddChild( ParseNode( self._reserved.POWER, None ) )
			self._li( parent, symbolTable )
			self._ae( parent, symbolTable )

	def _paren( self, parent, symbolTable ) :
		self._eat( self._reserved.LPAREN, "Expected a '(' but did not find one." )
		self._exp( parent, symbolTable )
		self._eat( self._reserved.RPAREN, "Expected a ')' but did not find one." )

	def _op( self, parent, symbolTable ) :
		curtoken = self._lexedSrc[self._curPos]

		if curtoken[0] == self._reserved.MINUS :
			self._eat( self._reserved.MINUS, "Expected a minus sign on a negative number." )
			tokval = self._eat( self._reserved.NUMBERLIT, "Expecting number after minus sign." )
			parent.AddChild( ParseNode( self._reserved.NEG_NUMBERLIT, ('-%s' % (tokval[1])) ) )

		elif curtoken[0] == self._reserved.NUMBERLIT :
			tokval = self._eat( self._reserved.NUMBERLIT, "Expected number." )
			parent.AddChild( ParseNode( tokval[0], tokval[1] ) )

		elif curtoken[0] == self._reserved.VARIABLE_NAME :
			vname = self._variableName( parent )
			if not symbolTable.SymbolExistsInAll( vname.TokenValue ) :
				print "'%s' is not defined yet." % (vname.TokenValue)
				exit(0)

		else :
			print "Expected a number (positive or negative), a number literal or a variable name."
			exit(0)

	def _arrayStatement( self, parent, symbolTable ) :
		# ARRAY
		self._eat( self._reserved.ARRAY, "Expected array declaration statement." );
		node = ParseNode( self._reserved.ARRAY_DECLARATION_STATEMENT, None )
		parent.AddChild( node )

		# VARIABLE NAME
		varval = self._variableName( node )
		if not symbolTable.AddSymbol( Symbol( self._reserved.ARRAY, varval.TokenValue ) ) :
			print "Variable '%s' already used in declaration statement" % (varval.TokenValue)
			exit(0)

		# [
		self._eat( self._reserved.LBRACKET, "Expected '[' after variable name in array statement." )

		# the list of values
		self._arrayDeclarationList( node, False )

		# ;
		self._eat( self._reserved.SEMICOLON, "Expected semicolon after array declaration statement." )

	def _arrayDeclarationList( self, parent, checkForLeadingComma=True ) :
		nexttok = self._lexedSrc[self._curPos]

		# check for end of list ']'
		if nexttok[0] == self._reserved.RBRACKET :
			self._eat( self._reserved.RBRACKET, "Right bracket expected after array list." )
			return

		node = ParseNode( self._reserved.DIMENSION, None )
		parent.AddChild( node )

		# starts with a leading comma?
		if checkForLeadingComma and nexttok[0] == self._reserved.COMMA :
			self._eat( self._reserved.COMMA, "Comma expected, but there was none!" )
			nexttok = self._lexedSrc[self._curPos]

		# starts with minus (negative number)
		finNum = ''
		finType = self._reserved.NUMBERLIT
		if nexttok[0] == self._reserved.MINUS :
			finNum = '-'
			finType = self._reserved.NEG_NUMBERLIT
			self._eat( self._reserved.MINUS, "Negative number expected in array list declaration." )

		# number literal
		tokval = self._eat( self._reserved.NUMBERLIT, "1 Number literal expected in array list declaration." )
		node.AddChild( ParseNode( finType, finNum + tokval[1] ) )

		# Period
		self._eat( self._reserved.PERIOD, "Period expected after first number literal in array list declaration." )

		# Period
		self._eat( self._reserved.PERIOD, "Period expected after first number literal in array list declaration." )

		# starts with minus (negative number)
		nexttok = self._lexedSrc[self._curPos]
		finNum = ''
		finType = self._reserved.NUMBERLIT
		if nexttok[0] == self._reserved.MINUS :
			finNum = '-'
			finType = self._reserved.NEG_NUMBERLIT
			self._eat( self._reserved.MINUS, "Negative number expected in array list declaration." )

		# number literal
		tokval = self._eat( self._reserved.NUMBERLIT, "2 Number literal expected in array list declaration." )
		node.AddChild( ParseNode( finType, finNum + tokval[1] ) )

		# another dimension
		self._arrayDeclarationList( parent )

	def _numAssignmentStatement( self, parent, symbolTable ) :
		node = ParseNode( self._reserved.NUM_ASSIGNMENT_STATEMENT, None )
		parent.AddChild( node )

		# VARIABLE NAME
		varval = self._variableName( node )
		if not symbolTable.SymbolExistsInAll( varval.TokenValue ) :
			print "'%s' not declared." % (varval.TokenValue)
			exit(0)

		# =
		self._eat( self._reserved.EQUALS, "Expected equals sign in number assignment statement." )

		# EXP
		self._exp( node, symbolTable )

		# ;
		self._eat( self._reserved.SEMICOLON, "Expected semi colon after number declaration statement (that's assigning something)" )

	def _arrayAssignmentStatement( self, parent, symbolTable ) :
		node = ParseNode( self._reserved.ARRAY_ASSIGNMENT_STATEMENT, None )
		parent.AddChild( node )

		# VARIABLE NAME
		varval = self._variableName( node )
		if not symbolTable.SymbolExistsInAll( varval.TokenValue ) :
			print "'%s' not declared." % (varval.TokenValue)
			exit(0)

		# [
		self._eat( self._reserved.LBRACKET, "Expected '['." )

		# 5,3,2,5]
		self._arrayAssignmentList( node, symbolTable, False )

		# =
		self._eat( self._reserved.EQUALS, "Expected equals sign in array assignment statement" )

		# EXP
		self._exp( node, symbolTable )

		# ;
		self._eat( self._reserved.SEMICOLON, "Expected semicolon after array assignment statement" )

	def _arrayAssignmentList( self, parent, symbolTable, checkForLeadingComma=True ) :
		nexttok = self._lexedSrc[self._curPos]

		# check for end of list ']'
		if nexttok[0] == self._reserved.RBRACKET :
			self._eat( self._reserved.RBRACKET, "Right bracket expected after array assignment list." )
			return

		dimnode = ParseNode( self._reserved.DIMENSION, None )
		parent.AddChild( dimnode )

		# starts with a leading comma?
		if checkForLeadingComma and nexttok[0] == self._reserved.COMMA :
			self._eat( self._reserved.COMMA, "Comma expected, but there was none!" )
			nexttok = self._lexedSrc[self._curPos]

		self._exp( dimnode, symbolTable )

		# Any more in the list?
		self._arrayAssignmentList( parent, symbolTable )

	def _ifStatement( self, parent, symbolTable, parentClosures=[] ) :
		closure = SymbolTable()
		parentSym = Symbol( self._reserved.PARENT_SYMBOL_TABLE, self._reserved.PARENT_SYMBOL_NAME, symbolTable )
		closure.AddSymbol( parentSym )
		ifsym = Symbol( self._reserved.IF_STATEMENT, "if_%d" % (self._closureCount), closure )
		self._symTable.AddSymbol( ifsym )
		self._closureCount += 1
		node = ParseNode( self._reserved.IF_STATEMENT, ifsym.Name )
		parent.AddChild( node )

		# add to list of parent closures
		parentClosures.append( symbolTable )

		# IF
		self._eat( self._reserved.IF, "If statement expected." )

		# Condition
		self._condition( node, symbolTable )

		# THEN
		self._eat( self._reserved.THEN, "'Then' statement expected." )

		# {
		self._eat( self._reserved.LCURLY, "Left curly brace expected in if statement." )

		# STATEMENT
		statementNode = ParseNode( self._reserved.STATEMENT, None )
		node.AddChild( statementNode )
		self._statement( statementNode, closure, parentClosures ) # add to conditional

		# }
		self._eat( self._reserved.RCURLY, "Right curly brace expected in if" )


		nexttoken = self._lexedSrc[self._curPos]
		if nexttoken[0] == self._reserved.ELSE :
			# ELSE
			self._eat( self._reserved.ELSE, "Expected else statement" )

			# {
			self._eat( self._reserved.LCURLY, "Expected left curly brace in else statement" )

			# STATEMENT
			statementNode = ParseNode( self._reserved.ELSE, None )
			node.AddChild( statementNode )
			closure = SymbolTable()
			parentSym = Symbol( self._reserved.PARENT_SYMBOL_TABLE, self._reserved.PARENT_SYMBOL_NAME, symbolTable )
			closure.AddSymbol( parentSym )
			elsesym = Symbol( self._reserved.ELSE, "else_%d" % (self._closureCount), closure )
			statementNode.TokenValue = elsesym.Name
			self._symTable.AddSymbol( elsesym )
			self._closureCount += 1
			self._statement( statementNode, closure, parentClosures ) # add to conditional

			# }
			self._eat( self._reserved.RCURLY, "Expected right curly brace in else statement" )

		# remove the symbol table from the list of parents!
		parentClosures.pop( len(parentClosures)-1 )

	def _condition( self, parent, symbolTable ) :
		node = ParseNode( self._reserved.CONDITION, None )
		parent.AddChild( node )

		# EXP
		self._exp( node, symbolTable )

		# CONDITIONAL
		self._relationalOperator( node )

		# EXP
		self._exp( node, symbolTable )

	def _relationalOperator( self, parent ) :
		nexttoken = self._lexedSrc[self._curPos]
		if nexttoken[0] == self._reserved.EQUALS :
			self._eat( self._reserved.EQUALS, "Expected equal sign in relational operator." )
			self._eat( self._reserved.EQUALS, "Expected second equal sign in == relational operator." )
			parent.AddChild( ParseNode( self._reserved.ISEQUAL, None ) )

		elif nexttoken[0] == self._reserved.LESSTHAN :
			self._eat( self._reserved.LESSTHAN, "Expected less than sign in relational operator." )
			nexttoken = self._lexedSrc[self._curPos]
			if nexttoken[0] == self._reserved.EQUALS :
				self._eat( self._reserved.EQUALS, "Expected equal sign after less than sign." )
				parent.AddChild( ParseNode( self._reserved.LESSTHAN_EQUAL, None ) )
			else :
				parent.AddChild( ParseNode( self._reserved.LESSTHAN, None ) )

		elif nexttoken[0] == self._reserved.GREATERTHAN :
			self._eat( self._reserved.GREATERTHAN, "Expected greater than sign in relational operator." )
			nexttoken = self._lexedSrc[self._curPos]
			if nexttoken[0] == self._reserved.EQUALS :
				self._eat( self._reserved.EQUALS, "Expected equal sign after greater than sign." )
				parent.AddChild( ParseNode( self._reserved.GREATERTHAN_EQUAL, None ) )
			else :
				parent.AddChild( ParseNode( self._reserved.GREATERTHAN, None ) )

		elif nexttoken[0] == self._reserved.NOT :
			self._eat( self._reserved.NOT, "Expected exclamation point in relational operator." )
			self._eat( self._reserved.EQUALS, "Expected second equal sign in != relational operator." )
			parent.AddChild( ParseNode( self._reserved.NOTEQUAL, None ) )

	def _forStatement( self, parent, symbolTable, parentClosures=[] ) :
		closure = SymbolTable()
		parentSym = Symbol( self._reserved.PARENT_SYMBOL_TABLE, self._reserved.PARENT_SYMBOL_NAME, symbolTable )
		closure.AddSymbol( parentSym )
		forsym = Symbol( self._reserved.FOR_STATEMENT, "for_%d" % (self._closureCount), closure )
		self._symTable.AddSymbol( forsym )
		self._closureCount += 1
		node = ParseNode( self._reserved.FOR_STATEMENT, forsym.Name )
		parent.AddChild( node )

		parentClosures.append( symbolTable )

		# FOR
		self._eat( self._reserved.FOR, "Expected for to start for statement." )

		# VARIABLE NAME
		self._variableName( node )

		# =
		self._eat( self._reserved.EQUALS, "Expected equal symbol in for statement." )

		# EXP
		self._exp( node, symbolTable )

		# TO
		self._eat( self._reserved.TO, "Expected TO in for statement." )

		# EXP
		self._exp( node, symbolTable )

		# STEP
		self._eat( self._reserved.STEP, "Expected STEP in for statement." )

		# EXP
		self._exp( node, symbolTable )

		# DO
		self._eat( self._reserved.DO, "Expected DO in for statement." )

		# {
		self._eat( self._reserved.LCURLY, "Expected left curly brace in for statement." )

		# STATEMENT
		statementNode = ParseNode( self._reserved.STATEMENT, None )
		node.AddChild( statementNode )
		self._statement( statementNode, closure, parentClosures )

		# }
		self._eat( self._reserved.RCURLY, "Expected right curly to finish the for statement." )

		parentClosures.pop( len(parentClosures)-1 )

	def _procedureStatement( self, parent, symbolTable ) :
		closure = SymbolTable()
		parentSym = Symbol( self._reserved.PARENT_SYMBOL_TABLE, self._reserved.PARENT_SYMBOL_NAME, symbolTable )
		closure.AddSymbol( parentSym )
		procsym = Symbol( self._reserved.PROCEDURE_STATEMENT, "proc_%d" % (self._closureCount), closure )
		self._symTable.AddSymbol( procsym )
		self._closureCount += 1

		node = ParseNode( self._reserved.PROCEDURE_STATEMENT, procsym.Name )
		parent.AddChild( node )

		# PROCEDURE
		self._eat( self._reserved.PROCEDURE, "Expected 'procedure' to start 'procedure' statement." )

		# VARIABLE NAME
		vnode = self._variableName( node )
		symbolTable.AddSymbol( Symbol( self._reserved.PROCEDURE, vnode.TokenValue, None ) )

		# (
		self._eat( self._reserved.LPAREN, "Expected left parenthisis after procedure name." )

		# the procedure list
		self._procedureList( node, closure )

		# {
		self._eat( self._reserved.LCURLY, "Expected left curly brace to define start of procedure body." )

		# statements
		statementNode = ParseNode( self._reserved.STATEMENT, None )
		node.AddChild( statementNode )
		self._statement( statementNode, closure )

		# }
		self._eat( self._reserved.RCURLY, "Expected right curly to end procedure definition." )

	def _procedureList( self, parent, symbolTable ) :
		#node = ParseNode( self._reserved.PARAMETER, None )

		# ) right away
		nexttoken = self._lexedSrc[self._curPos]
		if nexttoken[0] == self._reserved.RPAREN :
			self._eat( self._reserved.RPAREN, "Expected right parenthisis after parameter list in procedure declaration" )
			return;

		node = ParseNode( self._reserved.PARAMETER, "" )
		parent.AddChild( node )

		# NUM or STRING
		nexttoken = self._lexedSrc[self._curPos]
		ndtype = None
		if nexttoken[0] == self._reserved.NUM :
			node.AddChild( ParseNode( (self._eat(self._reserved.NUM, "Expected NUM statement in parameter list."))[0], None ) )
			ndtype = self._reserved.NUM

		elif nexttoken[0] == self._reserved.STRING :
			node.AddChild( ParseNode( (self._eat(self._reserved.STRING, "Expected STRING statement in parameter list."))[0], None ) )
			ndtype = self._reserved.STRING

		# *
		isRef = False
		nexttoken = self._lexedSrc[self._curPos]
		if nexttoken[0] == self._reserved.MULTIPLY :
			isRef = True
			#node.AddChild( ParseNode( self._reserved.REF, None ) )
			self._eat( self._reserved.MULTIPLY, "Expected reference before parameter name." )

		# VARIABLE NAME
		vnode = self._variableName( node )
		symbolTable.AddSymbol( Symbol( ndtype, vnode.TokenValue, None, True, isRef ) )
		#symbolTable.AddSymbol( Symbol( self._reserved.PARAMETER, vnode.TokenValue, ndtype ) )
		node.TokenValue = vnode.TokenValue

		# , or )
		nexttoken = self._lexedSrc[self._curPos]
		if nexttoken[0] == self._reserved.COMMA :
			self._eat( self._reserved.COMMA, "Expected comma after parameter in declaration statement" )
			self._procedureList( parent, symbolTable )

		elif nexttoken[0] == self._reserved.RPAREN :
			self._eat( self._reserved.RPAREN, "Expected right parenthisis in procedure statement." )

	def _procedureCallStatement( self, parent, symbolTable ) :
		# variable name
		nexttoken = self._eat( self._reserved.VARIABLE_NAME, "Variable name expected in procedure call." )
		node = ParseNode( self._reserved.PROCEDURE_CALL_STATEMENT, nexttoken[1] )
		parent.AddChild( node )

		# (
		self._eat( self._reserved.LPAREN, "Left parenthisis expected after procedure name in procedure call." )

		# parameter list
		self._procedureCallList( node, symbolTable )

		# ;
		self._eat( self._reserved.SEMICOLON, "Expected semi-colon after procedure call." )

	def _procedureCallList( self, parent, symbolTable ) :

		# ) -- no more parameters
		nexttoken = self._lexedSrc[self._curPos]
		if nexttoken[0] == self._reserved.RPAREN :
			self._eat( self._reserved.RPAREN, "Right parenthisis expected after procedure call." )
			return

		# variable name
		varnode = self._eat( self._reserved.VARIABLE_NAME, "Expected variable name in parameter list of procedure call." )
		parent.AddChild( ParseNode( varnode[0], varnode[1] ) )

		# ) -- check to see if end of list has occured
		nexttoken = self._lexedSrc[self._curPos]
		if nexttoken[0] == self._reserved.RPAREN :
			self._eat( self._reserved.RPAREN, "Right parenthisis expected after procedure call." )
			return

		# ,
		self._eat( self._reserved.COMMA, "Expected comma to separate parameters in procedure call." )

		# more procedure parameters!
		self._procedureCallList( parent, symbolTable )
