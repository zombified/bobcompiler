import sys, os.path
from parsenode import ParseNode
from semanticAnalyzer import SemanticAnalyzer
from reserved import ReservedWords

class Translator :
	_reserved = ReservedWords()

	def __init__( self ) :
		self._filename = self._getFileName()
		fin = open( self._filename, 'r' )
		src = fin.read()
		fin.close()

		sem = SemanticAnalyzer( src )
		self._ast = sem.GetAST()
		self._sym = sem.GetSymbolTable()

		self._asm = """GLOBAL main

extern printf
extern scanf
extern exit

"""
		self._powerCounter = 0
		self._translate()

	def _getFileName( self ) :
		srcPath = ""
		if len( sys.argv ) > 1 :
			srcPath = sys.argv[1]

		if srcPath == "" :
			print """
	Invalid Usage. Use like:
	$ python %s /path/to/grammar
	""" % (sys.argv[0])
			exit(0)

		if not os.path.exists( srcPath ) :
			print """
	Path (%s) does not exist
	""" % (srcPath)
			exit(0)

		if not os.path.isfile( srcPath ) :
			print """
	Path (%s) is not a file.
	""" % (srcPath)
			exit(0)

		return srcPath

	def Translation( self ) :
		return self._asm

	def _translate( self ) :
		self._addDataSections()
		self._addCodeSection()
		self._addExit()


		self._ast.Print()
		print "\n\n"
		self._sym.Print()
		print "\n\n"


	def _addDataSections( self ) :
		init = self._sym.GetInitializedSymbols()
		uninit = self._sym.GetUninitializedSymbols()
		datasegval = ''
		bsssegval = ''
		if init != None and len(init) > 0 :
			for s in init :
				# NUM
				if s.Type == self._reserved.NUM :
					datasegval += '%s_	dd	%s\n' % (s.Name, s.Value)

				# Array's -- kind of a fake out. No array will ever be pre-initialized, but we store dimension data
				#	in the value area in the symbol table.
				elif s.Type == self._reserved.ARRAY :
					totalbytes = 1
					for dimension in s.Value :
						totalbytes *= int(dimension[1])

					bsssegval += '%s_	resd	%d\n' % (s.Name, totalbytes)

		if uninit != None and len(uninit) > 0 :
			for s in uninit :
				# NUM
				if s.Type == self._reserved.NUM :
					bsssegval += '%s_	resd	1\n' % (s.Name)

		self._asm += """section .data
%s

; statements for use in printing strings and numbers
strprinter	db "%s",0
numprinter	db "%s",0x0d,0x0a,0
intfmt		db "%s",0


section .bss
%s

""" % (datasegval, "%s", "%d", "%i", bsssegval)

	def _addExit( self ) :
		self._asm += """progexit:
	mov	eax, 0x0
	call	exit

"""

	def _addCodeSection( self ) :
		code = self._genStatements( self._ast )

		self._asm += """section .code
main:
%s

""" % (code)

	def _genStatements( self, node ) :
		code = ''
		for child in node.ChildNodes :
			# num x = y;
			if child.TokenType == self._reserved.NUM_DECLARATION_STATEMENT or child.TokenType == self._reserved.NUM_ASSIGNMENT_STATEMENT :
				code += self._genNumStatement( child )

			# x[5,12] = 5
			elif child.TokenType == self._reserved.ARRAY_ASSIGNMENT_STATEMENT :
				code += self._genArrayAssignmentStatement( child )

			# write
			elif child.TokenType == self._reserved.WRITE_STATEMENT :
				code += self._genWriteStatement( child )

			# read
			elif child.TokenType == self._reserved.READ_STATEMENT :
				code += self._genReadStatement( child )

			# if
			elif child.TokenType == self._reserved.IF_STATEMENT :
				code += self._genIfStatement( child )

			# for
			elif child.TokenType == self._reserved.FOR_STATEMENT :
				code += self._genForStatement( child )

			# procedure
			elif child.TokenType == self._reserved.PROCEDURE_STATEMENT :
				code += self._genProcedureStatement( child )

		return code

	def _genNumStatement( self, statement ) :
		lhvar = statement.ChildNodes[0]
		expasm = self._genExpression( statement.ChildNodes[1] )

		return """%s	pop edi
	mov DWORD[%s_], edi

""" % (expasm, lhvar.TokenValue)

	# generate the assembly to calculate the byte offset to a position in an array
	def _genArrayElementOffset( self, varnameNode, dimvals ) :
		totaldwords = 1
#		varnameNode.Print()
#		self._sym.Print()
		varsym = self._sym.GetSymbol( varnameNode.TokenValue )
		for dimension in varsym.Value :
			totaldwords *= dimension[1]

		tmptotal = totaldwords

		asm = """	push 0 ; push the initial final position onto the stack
	push %d ; push the initial array length onto the stack

""" % (tmptotal)

		finalpos = 0
		curdim = 0
		stackOffsetForTotal = 4
		for dimension in dimvals :
			offset = varsym.Value[curdim][0]
			count = varsym.Value[curdim][1]

			# for each dimension, I need to:
			#	1. Calculate the value of the expression
			#	2. Calculate the actual 0-based index the expression result represents
			#	3. Calculate the size of the current dimension
			#	4. Set the temporary size (size of current segment of array) to be that of the dimension
			#	5. Multiply the size of the dimension by the 0-based index of the element desired, and add it to the final position

			# calc value of expresion
			asm += self._genExpression( dimension.ChildNodes[0] )

			# calc 0-based index
			asm += """	; calc 0-based index
	pop eax
	sub eax, %d
	push eax

	; get cur size of array (for purposes of this dimension)
	mov eax, DWORD[esp+4]

	; calc size of cur dimension
	xor edx, edx
	mov edi, %d
	idiv edi
	mov DWORD[esp+4], edi ; set the new "size" of the array

	pop edi ; get the 0-based index
	mov eax, DWORD[esp+4] ; get the running total for the final position offset
	imul edi, DWORD[esp] ; multiply the current dimmension size and the 0-based index
	add eax, edi ; add the result to the running total
	mov DWORD[esp+4], eax ; set the running total on the stack

""" % (offset, count)

			curdim += 1

		asm += """	; clean up
	pop eax
	pop eax

	; calculate byte offset
	imul eax, 4

	; push calculated offset to stack
	push eax

"""
		return asm

	def _genArrayAssignmentStatement( self, statement ) :
#		statement.Print()
		varnameNode = statement.ChildNodes[0]
		expasm = self._genExpression( statement.ChildNodes[-1] )
		dimvals = statement.ChildNodes[1:len(statement.ChildNodes)-1]

		asm = self._genArrayElementOffset( varnameNode, dimvals )

		asm += """	; calculate assigned value
%s	; grab pointer to location in array and calculate location
	mov edi, %s_
	mov eax, DWORD[esp+4]
	add edi, eax
	pop eax ; grab the assigned expressions value
	mov DWORD[edi], eax
	pop eax ; clear the stack

"""	 % (expasm, varnameNode.TokenValue)

		return asm


	def _genExpression( self, node ) :
		# literal
		if node.TokenType == self._reserved.NUMBERLIT or node.TokenType == self._reserved.NEG_NUMBERLIT :
			return "	push %s\n" % (node.TokenValue)

		# variable
		elif node.TokenType == self._reserved.VARIABLE_NAME :
			return "	push DWORD[%s_]\n" % (node.TokenValue)

		# +
		elif node.TokenType == self._reserved.PLUS :
			lh = self._genExpression( node.ChildNodes[0] )
			rh = self._genExpression( node.ChildNodes[1] )
			return """%s%s	pop eax
	pop edi
	add edi, eax
	push edi

""" % (lh, rh)

		# -
		elif node.TokenType == self._reserved.MINUS :
			lh = self._genExpression( node.ChildNodes[0] )
			rh = self._genExpression( node.ChildNodes[1] )
			return """%s%s	pop eax
	pop edi
	sub edi, eax
	push edi

""" % (lh, rh)

		elif node.TokenType == self._reserved.MULTIPLY :
			lh = self._genExpression( node.ChildNodes[0] )
			rh = self._genExpression( node.ChildNodes[1] )
			return """%s%s	pop eax ; pop divisor
	pop edi ; pop numerator
	imul edi, eax
	push edi

""" % (lh, rh)

		elif node.TokenType == self._reserved.DIVIDE :
			lh = self._genExpression( node.ChildNodes[0] )
			rh = self._genExpression( node.ChildNodes[1] )
			return """%s%s	pop edi
	pop eax
	xor edx, edx
	idiv edi
	push eax

""" % (lh, rh)

		elif node.TokenType == self._reserved.POWER :
			self._powerCounter += 1
			lh = self._genExpression( node.ChildNodes[0] ) # push value onto stack
			rh = self._genExpression( node.ChildNodes[1] ) # push power onto stack
			return """%s%s
	pop edi ; power
	pop ecx ; value to raise to power

	mov eax, 0x00000001
	xor esi, esi
powT_%d:
	cmp esi, edi
	jz powB_%d
	imul eax, ecx
	inc esi
	jmp powT_%d
powB_%d:
	push eax

""" % (lh, rh, self._powerCounter, self._powerCounter, self._powerCounter, self._powerCounter)

	def _genWriteStatement( self, statement ) :
		valueToWrite = statement.ChildNodes[0]

		if valueToWrite.TokenType == self._reserved.STRINGLIT :
			pass

		elif valueToWrite.TokenType == self._reserved.ARRAY_STATEMENT :
			varnameNode = valueToWrite.ChildNodes[0]
			dimvals = valueToWrite.ChildNodes[1:]

			asm = self._genArrayElementOffset( varnameNode, dimvals )
			asm += """	; get the value to write from memory, and place it on the stack
	mov edi, %s_
	pop eax
	add edi, eax
	push DWORD[edi]

	; write the value and clear the stack
	push numprinter
	call printf
	add esp, 0x08

""" % (varnameNode.TokenValue)
			return asm

		else :
			expasm = self._genExpression( valueToWrite )
			return """%s
	; print the result of the expression (which should be on the stack
	push	numprinter
	call	printf
	add	esp,	0x08		; clear the arg off the stack

""" % (expasm)

	def _genReadStatement( self, statement ) :
		varname = statement.ChildNodes[0].TokenValue
		return """	pusha
	push %s_
	push dword intfmt
	call scanf
	add esp, 0x04
	popa

""" % (varname)

	def _genIfStatement( self, statement ) :
		# 				cmp edi, eax
		# 				jne if_3_longjmp
		#				jmp if_3_block
		# if_3_longjmp:
		#				jmp if_3_exit
		#
		# if_3_block:
		#				; do stuff
		#
		#				jmp if_3_start
		# if_3_exit:

		cond = statement.ChildNodes[0]
		ro = cond.ChildNodes[1]

		code = self._genExpression( cond.ChildNodes[0] ) + self._genExpression( cond.ChildNodes[2] )
		code += """	pop edi ; rhs
	pop eax ; lhs
	cmp eax, edi
"""
		jmptxt = 'exit'
		if len(statement.ChildNodes) > 2 :
			jmptxt = 'else'


		if ro.TokenType == self._reserved.ISEQUAL :
			code += """	jne %s_longjmp
""" % (statement.TokenValue)

		elif ro.TokenType == self._reserved.NOTEQUAL :
			code += """	je %s_longjmp
""" % (statement.TokenValue)

		elif ro.TokenType == self._reserved.GREATERTHAN :
			code += """	jle %s_longjmp
""" % (statement.TokenValue)

		elif ro.TokenType == self._reserved.GREATERTHAN_EQUAL :
			code += """	jl %s_longjmp
""" % (statement.TokenValue)

		elif ro.TokenType == self._reserved.LESSTHAN :
			code += """	jge %s_longjmp
""" % (statement.TokenValue)

		elif ro.TokenType == self._reserved.LESSTHAN_EQUAL :
			code += """	jg %s_longjmp
""" % (statement.TokenValue)

		code += """	jmp %s_block
%s_longjmp:
	jmp %s_%s
%s_block:
%s
""" % (statement.TokenValue, statement.TokenValue, statement.TokenValue, jmptxt, statement.TokenValue, self._genStatements( statement.ChildNodes[1] ))

		if len(statement.ChildNodes) > 2 :
			elstate = statement.ChildNodes[2]
			code += """	``jmp %s_exit
%s_else:
%s
""" % (statement.TokenValue, statement.TokenValue, self._genStatements( elstate ) )

		code += """%s_exit:
""" % (statement.TokenValue)

		return code

	def _genForStatement( self, statement ) :
		statename = statement.TokenValue
		incname = statement.ChildNodes[0].TokenValue
		startnode = statement.ChildNodes[1]
		endnode = statement.ChildNodes[2]
		stepnode = statement.ChildNodes[3]
		statementnode = statement.ChildNodes[4]

		startexp = self._genExpression( startnode )
		endexp = self._genExpression( endnode )
		stepexp = self._genExpression( stepnode )
		statementexp = self._genStatements( statementnode )

		code = """%s 				;startexp
	; set start value
	pop eax
	mov DWORD[%s_], eax 			;incname
	jmp %s_end	 					;statename
%s_begin:							;statename

%s									;statementexp

	; increment
%s									;stepexp
	pop eax
	mov edi, DWORD[%s_]				;incname
	add edi, eax
	mov DWORD[%s_], edi				;incname
%s_end:								;statename
	; calculate compare
%s									;endexp
%s									;stepexp
	pop eax
	xor edi, edi
	cmp eax, edi
	jl %s_incdown					;statename
%s_incup:							;statename
	pop eax
	mov edi, DWORD[%s_]				;incname
	cmp edi, eax
	jg %s_exit						;statename
	jmp %s_begin					;statename
%s_incdown:							;statename
	pop eax
	mov edi, DWORD[%s_]				;incname
	cmp edi, eax
	jl %s_exit						;statename
	jmp %s_begin					;statename
%s_exit:							;statename

""" % (startexp,incname,statename,statename,statementexp,stepexp,incname,incname,statename,endexp,stepexp,statename,statename,incname,statename,statename,statename,incname,statename,statename,statename)

		return code

	def _genProcedureStatement( self, statement ) :
		varnode = statement.ChildNodes[0]
		statementnode = statement.ChildNodes[-1]
		paramnodes = statement.ChildNodes[1:-1]

		statementcode = self._genStatements( statementnode )

		code = """%s_start:
%s
%s_exit:
	ret

""" % (varnode.TokenValue, statementcode, varnode.TokenValue)

		return code


if __name__ == '__main__' :
	t = Translator()
	fout = open( "prog.asm", "w" )
	fout.write( t.Translation() )
	fout.close()
	print t.Translation()
