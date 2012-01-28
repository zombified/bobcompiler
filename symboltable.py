from reserved import ReservedWords

class Symbol :
	def __init__( self, symType, symName, symValue=None, isParameter=False, isRefType=False ) :
		self.Type = symType
		self.Name = symName
		self.Value = symValue
		self.IsParameter = isParameter
		self.IsRefType = isRefType

class SymbolTable :
	def __init__( self ) :
		self._table = []

	def AddSymbol( self, sym ) :
		# check to see if the symbol already exists in the table
		for s in self._table :
			if sym.Name == s.Name :
				return False

		self._table.append( sym )

		return True

	def GetSymbol( self, name ) :
		for s in self._table :
			if s.Name == name :
				return s

		return None

	def SymbolExists( self, name ) :
		if self.GetSymbol( name ) == None :
			return False
		else :
			return True
	
	def SymbolExistsInAll( self, name, table=None ) :
		if table == None :
			if self.SymbolExists( name ) :
				return True
			else :
				if self.SymbolExists( ReservedWords.PARENT_SYMBOL_NAME ) :
					return self.SymbolExistsInAll( name, self.GetSymbol( ReservedWords.PARENT_SYMBOL_NAME ).Value )
				else :
					return False
		
		else :
			if table.SymbolExists( name ) :
				return True
			
			else :
				if table.SymbolExists( ReservedWords.PARENT_SYMBOL_NAME ) :
					return table.SymbolExistsInAll( name, self.GetSymbol( ReservedWords.PARENT_SYMBOL_NAME ).Value )
				else :
					return False

	def AssignValueTo( self, name, value ) :
		s = self.GetSymbol( name )
		if s == None :
			return False

		s.Value = value
		return True

	def Print( self ) :
		for s in self._table :
			if s.Value == None :
				print "%d -- %s -- None" % (s.Type, s.Name)
			else :
				print "%d -- %s -- %s" % (s.Type, s.Name, s.Value)

	def GetInitializedSymbols( self ) :
		symbols = []
		for s in self._table :
			if s.Value != None :
				symbols.append(s)

		return symbols

	def GetUninitializedSymbols( self ) :
		symbols = []
		for s in self._table :
			if s.Value == None :
				symbols.append(s)

		return symbols
