#    overlay-upstream-tracking: a Gentoo overlay maintenance toolkit
#    Copyright (C) 2013  Gregory M. Turner <gmt@be-evil.net>
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this program; if not, write to the Free Software
#    Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA

import os, sys, re
import ply.lex as lex
import ply.yacc as yacc
from importlib import import_module
from inspect import isclass

class OOLexer(object, lex.Lexer):
	'''Wraps lex.Lexer as a new-style class.'''

	def __new__(cls, **kwargs):
		kwargs = kwargs.copy()
		obj = object.__new__(cls)

		if 'module' not in kwargs:
			kwargs['module'] = obj

		obj._lexer = cls.lex_new(**kwargs)
		return obj

	@classmethod
	def lex_new(cls, **kwargs):
		'''Handle lextab creation/naming during __new__'''
		kwargs = kwargs.copy()
		modname = cls.__module__ or 'lexer_' + cls.__name__
		if '.' in modname:
			modname = os.path.splitext(modname)[1][1:]

		if modname == '__main__':
			filename = sys.modules['__main__'].__file__
			modname = os.path.splitext(os.path.basename(filename))[0]

		modname += '_' + cls.__name__

		if 'lextab' in kwargs:
			_lextab = kwargs['lextab']
		else:
			_lextab = modname + '_lextab'

		if 'optimize' in kwargs:
			_optimize = kwargs['optimize']
		else:
			_optimize = 0

		# check if we can find a module correspondig to lextab in the module containing our class'
		# module; if so, use that in preference to whatever lex's choice would be
		if isinstance(_lextab, basestring):
			classmodname = cls.__module__ or None
			if classmodname != None and classmodname != '__main__' and ('.' in classmodname):
				lextab_modulename = os.path.splitext(classmodname)[0] + '.' + _lextab
				try:
					mod = sys.modules[classmodname]
					_lextab = import_module(lextab_modulename, mod.__package__)

					# When python is in -O mode or greater, force
					# optimize=1, but only if we sucecssfully loaded a module here.
					# This way, a "surprise" lextab the user didn't ask for will
					# never be generated, but we will not fail due to missing
					# doc-strings when a perfectly usable lextab is present.
					if sys.flags.optimize > 0:
						_optimize = 1

				except ImportError:
					pass

		kwargs['lextab'] = _lextab
		kwargs['optimize'] = _optimize
		return lex.lex(**kwargs)

	def __getattr__(self, name):
		# ensure no crazy recursion issues
		if name in [ '__dict__', '__new__', '__del__', '_lexer', '__getattr__', '__setattr__',
			     '__delattr__', '__str__', '__repr__', '__module__', '__name__' ]:
			return self.__dict__[name]

		try:
			return super(OOLexer, self).__getattr__(name)
		except AttributeError as e1:
			try:
				return getattr(self._lexer, name)
			except AttributeError as e2:
				if len(e1.args) > 0 and len(e2.args) > 0:
					e1.args = ('%s (and, from %s: %s)' % (e1.args[0],self._lexer,e2.args[0]) ,) + e1.args[1:]
				raise e1

	def __setattr__(self, name, value):
		if name in self.__dict__:
			return super(OOLexer, self).__setattr__(name, value)
		else:
			self.__dict__[name] = value

	def __delattr__(self, name):
		if name in self.__dict__:
			return super(NewLexer, self).__getattribute__(name)
		else:
			return delattr(self._lexer, name)

	def __str__(self):
		if hasattr(self, '_lexer'):
			return '<%s (with _lexer: %s)>' % (self.__class__.__name__, str(self._lexer))
		else:
			return '<%s (probably initializing; no _lexer yet)>' % self.__class__.__name__

	def __repr__(self):
		if hasattr(self, '_lexer'):
			return '<%s (with _lexer: %s)>' % (self.__class__.__name__, repr(self._lexer))
		else:
			return '<%s (probably initializing; no _lexer yet)>' % self.__class__.__name__

# Ideally this would descend from (object, LRParser).
# However, I think it is simply too complicated to be worth it.
# Frankly, it was probably too complicated to be worth it for
# the lexer as well.  Instead, we just implement the same
# interface and assume this will suffice.  Maybe fix later.
class OOParser(object):
	'''Base class for a lexer/parser that has the rules defined as methods'''
	def __init__(self, lexer_arg, **kwargs):
		self.debug = kwargs.get('debug', 0)

		modname = self.__class__.__module__ or 'parser_' + self.__class__.__name__
		if '.' in modname:
			modname = os.path.splitext(modname)[1][1:]

		if modname == '__main__':
			filename = sys.modules[self.__module__].__file__
			modname = os.path.splitext(os.path.basename(filename))[0]

		modname += '_' + self.__class__.__name__

		self.debugfile = kwargs.get('debugfile', modname + '.dbg')
		self.tabmodule = kwargs.get('tabmodule', modname + '_parsetab')

		self.lex_optimize = kwargs.get('lex_optimize', 0) or kwargs.get('optimize', 0)
		self.yacc_optimize = kwargs.get('optimize', 0)

		# Build the lexer (if passed a class for the lexer, instantiate it)
		if isclass(lexer_arg):
			lexer_kwargs = kwargs.copy()
			for arg in kwargs.keys():
				if not arg in ['debuglog', 'outputdir', 'errorlog', 'reflags', 'lextab', 'nowarn']:
					del(lexer_kwargs[arg])
			lexer_kwargs['debug'] = self.debug
			lexer_kwargs['optimize'] = self.lex_optimize
			self._lexer = lexer_arg(**lexer_kwargs)
		else:
			self._lexer = lexer_arg

		# check if we can find a module correspondig to tabmodule in the module containing our class'
		# module; if so, use that in preference to whatever yacc's choice would be
		if isinstance(self.tabmodule, basestring):
			classmodname = self.__class__.__module__ or None
			if classmodname != None and classmodname != '__main__' and ('.' in classmodname):
				tabmodule_modulename = os.path.splitext(classmodname)[0] + '.' + self.tabmodule
				try:
					mod = sys.modules[classmodname]
					self.tabmodule = import_module(tabmodule_modulename, mod.__package__)

					# When python is in -O mode or greater, force
					# optimize=1, but only if we sucecssfully loaded a module here.
					# This way we will not fail due to missing
					# doc-strings when a perfectly usable parsetab is present.
					if sys.flags.optimize > 0:
						self.yacc_optimize = 1
				except ImportError:
					pass

		kwargs = kwargs.copy()
		for arg in kwargs.keys():
			if not arg in [ 'method', 'start', 'check_recursion', 'write_tables',
					'outputdir', 'debuglog', 'errorlog', 'picklefile' ]:
				del(kwargs[arg])
		kwargs['module'] = self
		kwargs['debug'] = self.debug
		kwargs['debugfile'] = self.debugfile
		kwargs['tabmodule'] = self.tabmodule
		kwargs['optimize'] = self.yacc_optimize
		self._parser = yacc.yacc(**kwargs)

	def parse(self, data, **kwargs):
		if not 'lexer' in kwargs:
			kwargs['lexer'] = self._lexer
		return self._parser.parse(data, **kwargs)

	def parsedebug(self, data, **kwargs):
		if not 'lexer' in kwargs:
			kwargs = kwargs.copy()
			kwargs['lexer'] = self._lexer
		return self._parser.parsedebug(data, **kwargs)

	def parseopt(self, data, **kwargs):
		if not 'lexer' in kwargs:
			kwargs = kwargs.copy()
			kwargs['lexer'] = self._lexer
		return self._parser.parseopt(data, **kwargs)

	def parseopt_notrack(self, data, **kwargs):
		if not 'lexer' in kwargs:
			kwargs = kwargs.copy()
			kwargs['lexer'] = self._lexer
		return self._parser.parseopt_notrack(data, **kwargs)

	def restart(self):
		self._parser.restart()

	def errok(self):
		self._parser.errok()

	@property
	def tokens(self):
		return self._lexer.tokens

	@property
	def precedence(self):
		return self.get_precedence()

	def get_precedence(self):
		return ()
