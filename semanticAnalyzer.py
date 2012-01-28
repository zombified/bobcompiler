from reserved import ReservedWords
from symboltable import Symbol,SymbolTable
from parser import Parser
from parsenode import ParseNode


class SemanticAnalyzer :
	_reserved = ReservedWords()
	_src = None
	_ast = None
	_sym = None


	def __init__( self, src ) :
		self._src = src
		p = Parser( src )
		self._ast = p.GetTree()
		self._sym = p.GetSymbolTable()

		self._analyze()

	def GetSymbolTable( self ) :
		return self._sym

	def GetAST( self ) :
		return self._ast

	def _analyze( self ) :
		self._fixAllExpressions( self._ast )
		for child in self._ast.ChildNodes[1:] :
			self._pruneLesserSymbolTables( child, [''] )
		self._pruneLiteralDeclarations( self._ast )

	def _pruneLiteralDeclarations( self, node ) :
		if node != None and node.ChildNodes != None and len(node.ChildNodes) > 0 :
			x = 0
			while x < len(node.ChildNodes) :
				child = node.ChildNodes[x]

				# NUM DECLARATION
				if child.TokenType == self._reserved.NUM_DECLARATION_STATEMENT :
					# if the statement is just a declaration with NO expression, we can remove it because we know it's already in our symbol table
					if len(child.ChildNodes) == 1 :
						node.ChildNodes.remove(child)
						x -= 1

					# if the statement contains 2 nodes only (IE: num x = 1; or num x = y;)
					elif len(child.ChildNodes) == 2 :
						# the first node will always be the variable name, but what is the second node?
						if child.ChildNodes[1].TokenType == self._reserved.NEG_NUMBERLIT or child.ChildNodes[1].TokenType == self._reserved.NUMBERLIT :
							self._sym.AssignValueTo( child.ChildNodes[0].TokenValue, child.ChildNodes[1].TokenValue )
							node.ChildNodes.remove( child )
							x -= 1

						## if the statement contains 1 node on the right-hand side, and that node is a number literal
						#if len(child.ChildNodes[1].ChildNodes) == 1 and child.ChildNodes[1].ChildNodes[0].TokenType == self._reserved.NUMBERLIT :
						#	self._sym.AssignValueTo( child.ChildNodes[0].TokenValue, child.ChildNodes[1].ChildNodes[0].TokenValue )
						#	node.ChildNodes.remove(child)

				# ARRAY DECLARATION
				elif child.TokenType == self._reserved.ARRAY_DECLARATION_STATEMENT :
					varname = child.ChildNodes[0].TokenValue
					varval = []
					for arrchild in child.ChildNodes :
						if arrchild.TokenType == self._reserved.DIMENSION :
							low = int(arrchild.ChildNodes[0].TokenValue)
							high = int(arrchild.ChildNodes[1].TokenValue)

							offset = low
							count = abs(high - low) + 1

							varval.append( (offset, count) )

					self._sym.AssignValueTo( varname, varval )

				else :
					self._pruneLiteralDeclarations( child )

				x += 1

	def _fixAllExpressions( self, node ) :
		if len(node.ChildNodes) > 0 :
			for child in node.ChildNodes :
				if child.TokenType == self._reserved.EXP :
					self._fixExpression( child )
				else :
					self._fixAllExpressions( child )


	def _fixExpression( self, exp ) :
		# if the expression is just a number or a variable name, then
		#	just make the exp the value
		if len(exp.ChildNodes) <= 1 :
			if exp.ChildNodes[0].TokenType == self._reserved.EXP :
				exp.ChildNodes = exp.ChildNodes[0].ChildNodes
				self._fixExpression( exp )
				return
			else :
				exp.TokenType = exp.ChildNodes[0].TokenType
				exp.TokenValue = exp.ChildNodes[0].TokenValue
				exp.ChildNodes = []
				return

		# scan for other expressions in this expression
		for child in exp.ChildNodes :
			if child.TokenType == self._reserved.EXP :
				self._fixExpression( child )


		# scan for ^
		i = 1
		while len( exp.ChildNodes ) > 3 :
			if i >= len(exp.ChildNodes)-1 :
				break

			if exp.ChildNodes[i].TokenType == self._reserved.POWER :
				nd = ParseNode( exp.ChildNodes[i].TokenType, None )
				nd.AddChild( exp.ChildNodes[i-1] )
				nd.AddChild( exp.ChildNodes[i+1] )
				exp.ChildNodes.insert( i-1, nd )
				exp.ChildNodes.pop(i)
				exp.ChildNodes.pop(i)
				exp.ChildNodes.pop(i)

				if len(exp.ChildNodes) <= 3 :
					break
			else :
				i += 1


		# scan for * or /
		i = 1
		while len( exp.ChildNodes ) > 3 :
			if i >= len(exp.ChildNodes)-1 :
				break

			if exp.ChildNodes[i].TokenType == self._reserved.MULTIPLY or exp.ChildNodes[i].TokenType == self._reserved.DIVIDE :
				nd = ParseNode( exp.ChildNodes[i].TokenType, None )
				nd.AddChild( exp.ChildNodes[i-1] )
				nd.AddChild( exp.ChildNodes[i+1] )
				exp.ChildNodes.insert( i-1, nd )
				exp.ChildNodes.pop(i)
				exp.ChildNodes.pop(i)
				exp.ChildNodes.pop(i)

				if len(exp.ChildNodes) <= 3 :
					break
			else :
				i += 1


		# scan for + or -
		i = 1
		while len( exp.ChildNodes ) > 3 :
			if i >= len(exp.ChildNodes)-1 :
				break

			if exp.ChildNodes[i].TokenType == self._reserved.PLUS or exp.ChildNodes[i].TokenType == self._reserved.MINUS :
				nd = ParseNode( exp.ChildNodes[i].TokenType, None )
				nd.AddChild( exp.ChildNodes[i-1] )
				nd.AddChild( exp.ChildNodes[i+1] )
				exp.ChildNodes.insert( i-1, nd )
				exp.ChildNodes.pop(i)
				exp.ChildNodes.pop(i)
				exp.ChildNodes.pop(i)

				if len(exp.ChildNodes) <= 3 :
					break
			else :
				i += 1

		# At this point there should only be 3 nodes in exp.ChildNodes, so make the
		#	expression it's operator, and remove the operator from the children
		exp.TokenType = exp.ChildNodes[1].TokenType
		exp.ChildNodes.pop(1)


	def _pruneLesserSymbolTables( self, node, parentClosures, declaredInScope=[[]] ) :
		if node.TokenType == self._reserved.IF_STATEMENT :
			conditionnode = node.ChildNodes[0]
			statementnode = node.ChildNodes[1]

			self._pruneLesserSymbolTables( conditionnode, parentClosures, declaredInScope )

			declaredInScope.insert( 0, [] )
			parentClosures.insert( 0, node.TokenValue )

			self._pruneLesserSymbolTables( statementnode, parentClosures, declaredInScope )

			parentClosures.pop( 0 )
			declaredInScope.pop( 0 )

			if len(node.ChildNodes) > 2 : # if there is an else statment
				elsestatement = node.ChildNodes[2]

				declaredInScope.insert( 0, [] )
				parentClosures.insert( 0, elsestatement.TokenValue )

				self._pruneLesserSymbolTables( elsestatement, parentClosures, declaredInScope )

				parentClosures.pop( 0 )
				declaredInScope.pop( 0 )

			return

		elif node.TokenType == self._reserved.FOR_STATEMENT :
			varnode = node.ChildNodes[0]
			startnode = node.ChildNodes[1]
			endnode = node.ChildNodes[2]
			stepnode = node.ChildNodes[3]
			statementnode = node.ChildNodes[4]

			self._pruneLesserSymbolTables( varnode, parentClosures, declaredInScope );
			self._pruneLesserSymbolTables( startnode, parentClosures, declaredInScope );
			self._pruneLesserSymbolTables( endnode, parentClosures, declaredInScope );
			self._pruneLesserSymbolTables( stepnode, parentClosures, declaredInScope );

			declaredInScope.insert( 0, [] )
			parentClosures.insert( 0, node.TokenValue )

			self._pruneLesserSymbolTables( statementnode, parentClosures, declaredInScope );

			parentClosures.pop( 0 )
			declaredInScope.pop( 0 )

			return

		elif node.TokenType == self._reserved.PROCEDURE_STATEMENT :
			declaredInScope[0].insert( 0, node.ChildNodes[0].TokenValue )
			self._pruneLesserSymbolTables( node.ChildNodes[0], parentClosures, declaredInScope )

			declaredInScope.insert( 0, [] )
			parentClosures.insert( 0, node.TokenValue )

			for child in node.ChildNodes[1:-1] :
				declaredInScope[0].append( child.TokenValue )
				self._pruneLesserSymbolTables( child, parentClosures, declaredInScope )
				self._sym.AddSymbol( Symbol( child.ChildNodes[0].TokenType, child.ChildNodes[1].TokenValue ) )

			statementnode = node.ChildNodes[len(node.ChildNodes)-1]
			self._pruneLesserSymbolTables( statementnode, parentClosures, declaredInScope )

			parentClosures.pop( 0 )
			declaredInScope.pop( 0 )

			return

		elif node.TokenType == self._reserved.NUM_DECLARATION_STATEMENT :
			if len(node.ChildNodes) > 1 :
				self._pruneLesserSymbolTables( node.ChildNodes[1], parentClosures, declaredInScope )

			varnode = node.ChildNodes[0]
			declaredInScope[0].insert( 0, varnode.TokenValue )

			# create symbol to be added to global symbol table, and modify node's variable name -- only if we're not in the global scope
			if len(parentClosures) > 1 : # meaning the current scope is not the global scope
				newvname = "%s_%s" % (varnode.TokenValue, parentClosures[0])
				#print "newname: %s" % (newvname)
				newsym = Symbol( self._reserved.NUM, newvname )
				self._sym.AddSymbol( newsym )
				varnode.TokenValue = newvname

			return

		elif node.TokenType == self._reserved.VARIABLE_NAME :
			# if declared in recent scope, then it SHOULD be in the recent closure
			# otherwise loop through the scopes to see where it is defined, and go to that closure

			vname = node.TokenValue

			scope = 0
			found = False
			while scope < len(declaredInScope) :
				if vname in declaredInScope[scope] :
					found = True
					break

				scope += 1

			if not found :
				print "A variable ('%s') is missing from any scopes!" % (vname)
				exit(0)

#			print "========================"
#			print parentClosures
#			print "------------------------"
#			print declaredInScope
#			print "========================"
#			print "scope: %d" % (scope)

			sym = None
			newvarname = ""
			if scope == len(declaredInScope)-1 :
				sym = self._sym.GetSymbol( vname )
				newvarname = sym.Name
			else :
				closure = self._sym.GetSymbol( parentClosures[scope] ).Value
				sym = closure.GetSymbol( vname )
				newvarname = sym.Name + "_" + parentClosures[scope]

			newsym = Symbol( sym.Type, newvarname, sym.Value )
#			print "ns: %s -- %s -- %s" % (newsym.Type, newsym.Name, newsym.Value)
#			self._sym.AddSymbol( newsym )
			node.TokenValue = newsym.Name

			return

		for child in node.ChildNodes :
			self._pruneLesserSymbolTables( child, parentClosures, declaredInScope )
