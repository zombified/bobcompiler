from reserved import ReservedWords

class Lexer :
	_src = ''
	_lexedSrc = []
	_reserved = ReservedWords()
	
	def __init__( self, src ) :
		self._src = src
		self._analyze()
	
	def _analyze( self ) :
		tmptoken = ''
		curIsString = False
		for c in self._src :
			chartype = self._reserved.GetValueForReserved( c )
			
			if chartype == self._reserved.DQUOTE :
				if curIsString :
					self._lexedSrc.append( [self._reserved.STRINGLIT, tmptoken] )
					tmptoken = ''
					curIsString = False
				else :
					curIsString = True
			
			elif curIsString :
				tmptoken += c
			
			# if character is reserved
			elif chartype != None :
				# check to see if the value before is a reserved word or an integer or a variable name
				toktype = self._reserved.GetValueForReserved( tmptoken )
				if toktype != None :
					self._lexedSrc.append( [toktype] )
				else :
					if tmptoken != '' :
						try :
							val = int(tmptoken)
							self._lexedSrc.append( [self._reserved.NUMBERLIT, tmptoken] )
						except :
							self._lexedSrc.append( [self._reserved.VARIABLE_NAME, tmptoken.lower()] )
				
				self._lexedSrc.append( [chartype] )
				tmptoken = ''
			
			elif c in self._reserved.WhiteSpace and tmptoken != '' :
				# if we've gotten here, then tmptoken must be either an integer or a variable name
				# check to see if the value before is a reserved word or an integer or a variable name
				toktype = self._reserved.GetValueForReserved( tmptoken )
				if toktype != None :
					self._lexedSrc.append( [toktype] )
				else :
					if tmptoken != '' :
						try :
							val = int(tmptoken)
							self._lexedSrc.append( [self._reserved.NUMBERLIT, tmptoken] )
						except :
							self._lexedSrc.append( [self._reserved.VARIABLE_NAME, tmptoken.lower()] )
				
				tmptoken = ''
			
			else :
				if c not in self._reserved.WhiteSpace :
					tmptoken += c
			
		if curIsString :
			print "Undetermined String Literal."
			exit(0)
	
	def LexedSrc( self ) :
		return self._lexedSrc
