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
from ply.yacc import YaccProduction, YaccSymbol
from importlib import import_module
from inspect import isclass
from OUT.FixPython import is_string
from itertools import chain
from copy import copy, deepcopy

class OOLexer(object):
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

# --------------- BaseProdution convenience class hierarchy ----------

# These classes may be used to create convenient object-oriented production
# class hierarchies which may be easily plugged into OOParser methods or
# traditional module-based yacc parsers.  Aside from a bit of knowledge about
# the structure of "p", they are not tightly coupled with anything but each other.
#
# They all support construction in the form FooProduction(p, [keyword arguments...]),
# and, unless a true 'no_auto_assign' keyword argument is passed in, they all will
# immediately replace p[0] with self upon instantiation (or raise an exception).
#
# There are, roughly three categories of BaseProduction subclasses; all of them may be
# subclassed to extend their behavior:
#
# o Abstract Interface Productions which act as Mix-Ins.  For example, SequenceProductions
#   merge equivalent behavior to collections.Sequence into the BaseProduction hierarchy
# o Concrete "Feature" Productions which serve as the workhorses of the framework:
#   Key amongst these are the AtomicProduction which serves to replace p[0] with an
#   atomic (in the sense of not being ContainerProductions) representation of whatever
#   is in p[1:], and UserListProductons, which transform whatever is in p[1:] into a
#   list-like structure.
# o ErrorProductions, which either raise an exception, or, if passed a True internalize_error
#   keyword argument, store information about parsing errors into p[0].
#
# They are designed to be reasonably performant, if not fast.  All subclasses in the framework
# are expected to implement __slots__ (or otherwise, custom __copy__ and __deepcopy__
# implementations.  They are built according to a four-phase process:
#
# During initialization, keyword arguments are sliced and diced and passed along the MRO
# chain.  If, by the time this processing reaches BaseProduction (presumably it will be
# the last BaseProduction in __mro__), any keyword arguments remain unprocessed, an
# exception is raised.  From BaseProduction.__init__, the remaining three phases are
# orchestrated:
#
# init_hook is invoked and propagated along the __mro__ chain.  This phase is mostly a convenience
# to allow MixIn classes to initialize themselves.  By separating this from the previous phase,
# MixIns are freed from having to worry about precisely how the MRO is constructed.  To this end,
# the convention is that super(<Production Class>, self).__init__(p, **kwargs) will be the last
# statement in every BaseProduction class's __init__.
#
# validate is then invoked and propogated along the __mro__ chain.  The purpose of this is to allow
# any class in the framework to raise an exception (or, by setting various attributes, to
# transform self into a pseudo-ErrorProduction), once the initialization phase has fully completed.
#
# finally, assuming no error has occured, optimize() is invoked and propogated along the __mro__ chain.
# This phase is an opportunity for Mix-In classes to transform the arguments into a more efficient or
# convenient representation.  Presumably, one that is semanticaly equivalent or one which fully or
# partially carries out the semantic processing which self symbolizes in the language being parsed.
# Since optimization might create opportunities for additional optimization not present before-hand,
# it is expected that after some changes are made, the MixIn which performed the optimization will
# set self.optimize_again to True.  In this case, optimize will be invoked once more after the current
# optimize() invocation has travarsed the __mro__ chain.  One concern here is that optimization might
# never complete, but instead continuously restart itself in some kind of loop.  No protection against
# this is taken at the framework level.  If this starts to become a problem then some kind of limit to
# the number of optimize() invocations could be added.

class RulesSyntaxError(Exception):
	'''Thrown if errors are encountered parsing the rules files'''
# not to be confused with an /internalized/ error which is a production representing an
# error which has been accepted into the tree of productions on the assumption that
# some container will know what to do with it later in the parsing process.
class RulesParserInternalError(Exception):
	'''Thrown if the parser gets hopelessly confused in some unexpected way'''
class GlobalRulesNotFoundException(Exception):
	'''Thrown if the global rules repository is not present where OUT.Rules
	   expects to find it (typically: /usr/share/overlay-upstream-tracking/outrules.d)'''
class BaseProduction(object):
	'''Base class for Productions containing mostly Error-Handling glue; accomodating
	   both instantly-explosive errors and error-delaying modes of operation'''
	__slots__ = ( 'is_error', 'error_msg', 'error_exception_class', 'optimize_again',
		      'p', 'value', 'prodtype' )
	def __init__(self, p, is_error=False, internalize_error=False, error_msg='Unknown Error',
			error_exception_class=RulesSyntaxError, no_auto_assign=False, 
			value=None, **kwargs):
		# handle any error
		self.is_error = is_error
		self.error_msg = error_msg
		self.error_exception_class = error_exception_class
		self.p = p
		self.value = value
		if 'prodtype' in kwargs:
			self.prodtype = kwargs['prodtype']
			del kwargs['prodtype']
		else:
			slice = getattr(p, 'slice', None)
			if slice:
				self.prodtype = slice[0]
			else:
				self.prodtype = None
		if self.__class__ == BaseProduction:
			self.error_exception_class = RulesParserInternalError
			self.error_msg = 'Abstract BaseProduction Instantiated'
			self.is_error = True
			self.Error()
		if len(kwargs) > 0:
			self.error_exception_class = RulesParserInternalError
			self.error_msg = 'non-empty kwargs: %s' % kwargs
			self.is_error = True
			self.Error()
		if self.is_error and not internalize_error:
			self.Error()

		# we know kwargs is empty as we tested for it above;
		# fine as we should be the last Production class on mro
		super(BaseProduction, self).__init__()

		# a hook for subclasses to perform final initialization steps after all
		# of the __init__ mro has completed.  After this, self should
		# be fully constructed at the python level.  For containers
		# this must finalize containment relationships
		self.init_hook()
		if self.is_error and not internalize_error:
			self.Error()

		# a hook for subclasses to perform final validation steps and raise
		# exceptions in response to semantic inconsistencies
		self.validate()
		if self.is_error and not internalize_error:
			self.Error()

		# a hook for subclasses to restructure their productions
		# using various recipies to simplify the semantic structure without
		# changing the meaning.  Keep optimizing until nobody requrests
		# reoptimization
		self.optimize_again = True
		while self.optimize_again:
			self.optimize_again = False
			self.optimize()
			if self.is_error and not internalize_error:
				self.Error()

		# all initializations, validations and optimizations successful: assign
		if not no_auto_assign:
			p[0] = self

	def init_hook(self):
		pass
	def validate(self):
		pass
	def optimize(self):
		pass

	def BlameSelfFor(self, error_production):
		'''Internalize the error metadata from a foreign production'''
		if not from_production.is_error:
			self.error_msg = 'Blamed for non error: %s' % error_production
			self.error_exception_class = RulesParserInternalError
			self.is_error = True
		else:
			self.error_msg = from_production.error_msg
			self.error_exception_class = from_production.error_exception_class
			self.is_error = from_production.is_error

	def Error(self, from_production=None):
		'''Raise an error corresponding to __init__ialized values (optionally cloned from
		   a foreign production)'''

		if from_production != None:
			self.BlameSelfFor(from_production)

		# make sure we really have an error (otherwise ... well now we are one due to that)
		if not self.is_error:
			self.is_error = True
			self.error_exception_class = RulesParserInternalError
			self.error_msg = 'Error invocation on non error %s' % self

		if self.p == None:
			error_msg = 'Unexpected EOF while parsing'
			if self.error_msg != 'Unknown Error':
				error_msg += ': ' + self.error_msg
		else:
			error_msg = 'line %s: ' % self.p.lexer.lineno + self.error_msg
			error_msg += ': %s' % self.value

		raise self.error_exception_class(error_msg)

	def __prod_repr(self):
		if isinstance(self.p, YaccProduction):
			p_repr = ''
			if self.prodtype is not None:
				p_repr += str(self.prodtype)
			if self.value is not None:
				p_repr += "/'"
				p_repr += str(self.value)
				p_repr += "'"
		else:
			p_repr += str(self.p)
		if self.is_error:
			return '<[ERROR: %s: %s] %s(%s)>' % (
				self.error_exception_class, self.error_msg, self.__class__.__name__, p_repr)
		else:
			return '<%s(%s)>' % (self.__class__.__name__, p_repr)
	def pprint_repr(self):
		return self.__prod_repr()
	def __repr__(self):
		return self.__prod_repr()
	def __str__(self):
		return self.__prod_repr()
	def __eq__(self, other):
		if self is other:
			return True
		if super(BaseProduction, self).__eq__(other):
			# good enough I guess
			return True
		if isinstance(other, BaseProduction):
			if self.is_error != other.is_error:
				return False
			elif self.is_error and self.error_msg != other.error_msg:
				return False
			elif self.is_error and self.error_exception_class != other.error_exception_class:
				return False
			elif self.p != other.p:
				return False
			elif self.value != other.value:
				return False
			elif self.prodtype != other.prodtype:
				return False
			else:
				return True
		else:
			return False

	# We don't know how __init__ was invoked and doing the right thing would be incredibly
	# complicated.  Hopefully this works like magic so long as everybody uses __slots__,
	# like they're supposed to.
	def __copy__(self):
		try:
			subcopy = super(BaseProduction, self).__copy__
		except AttributeError as e:
			subcopy = None
			pass
		if subcopy is None:
			# best guess impl.: we basically want this to work for the
			# "normal" case where the only superclass behind us on the
			# mro is object.  Otherwise, ... meh, they shoulda implemented
			# __copy__,  I guess, or done god-knows-what.
			rslt = self.__class__.__new__(self.__class__)
			if hasattr(self, '__dict__'):
				for attr in self.__dict__:
					if not attr.startswith("__"):
						setattr(rslt, attr, getattr(self, attr, None))
		else:
			# presumably they know what they are doing...
			rslt = subcopy()
		# subclasses of RulesProductions are required to use __slots__
		for attr in set(chain.from_iterable(
			getattr(cls, '__slots__', ())
				for cls in type(self).__mro__
					if issubclass(cls, BaseProduction)
		)):
			setattr(rslt, attr, getattr(self, attr, None))
		return rslt

	def __deepcopy__(self):
		try:
			subcopy = super(BaseProduction, self).__deepcopy__
		except AttributeError as e:
			subcopy = None
			pass
		if subcopy is None:
			# best guess impl.: we basically want this to work for the
			# "normal" case where the only superclass behind us on the
			# mro is object.  Otherwise, ... meh, they shoulda implemented
			# __copy__,  I guess, or done god-knows-what.
			rslt = self.__class__.__new__(self.__class__)
			if hasattr(self, '__dict__'):
				for attr in self.__dict__:
					if not attr.startswith("__"):
						setattr( rslt, attr,
							 deepcopy(getattr(self, attr, None)) )
		else:
			# assume they know what they're doing
			rslt = subcopy()
		for attr in set(chain.from_iterable(
			getattr(cls, '__slots__', ())
				for cls in type(self).__mro__
					if issubclass(cls, BaseProduction)
		)):
			itemref = getattr(self, attr, None)
			if attr == 'p':
				# presumably the cloning mostly occurs during optimize() when
				# assignment of p[0] has not yet happened; also, p may contain
				# backreferences to the full lexer and parser.  That's
				# a bit much.  It's reasonably safe to assume
				# nothing horrible will happen if we simply
				# create a shallow copy, however.
				setattr(rslt, attr, copy(itemref))
			elif isinstance(itemref, type):
				# cloning class objects seems very extreme :) leave 'em.
				setattr(rslt, attr, itemref)
			else:
				setattr(rslt, attr, deepcopy(itemref))
		return rslt

class ErrorProduction(BaseProduction):
	'''Abstract convenience subclass for creating a standard internalized error'''
	__slots__ = ()
	def __init__(self, p, error_msg='Error', internalize_error=False,
		     error_exception_class=None, **kwargs):
		kwargs['is_error'] = True
		if error_exception_class is None:
			kwargs['internalize_error'] = False
			kwargs['error_excpetion_class'] = RulesParserInternalError
			kwargs['error_msg'] = 'MetaError (%s)' % error_msg
		else:
			kwargs['error_msg'] = error_msg
			kwargs['internalize_error'] = internalize_error
			kwargs['error_exception_class'] = error_exception_class
		super(SyntaxErrorProduction, self).__init__(p, **kwargs)

class SyntaxErrorProduction(ErrorProduction):
	__slots__ = ()
	def __init__(self, p, error_exception_class=RulesSyntaxError, **kwargs):
		kwargs['error_exception_class'] = error_exception_class
		super(SyntaxErrorProduction, self).__init__(p, **kwargs)

class InternalErrorProduction(ErrorProduction):
	__slots__ = ()
	def __init__(self, p, error_exception_class=RulesParserInternalError, **kwargs):
		kwargs['error_exception_class'] = error_exception_class
		super(InternalErrorProduction, self).__init__(p, **kwargs)

# here we aggregate all the MixIn __slots__ variables so that we don't
# get layout conflicts during Metaclass construction.
class MixInProduction(BaseProduction):
	__slots__ = ( 'data', 'flatten_sequences_of' )
	pass

class IterableProduction(MixInProduction):
	'''Mixin abstract Iterable Production.  Mostly identical to collections.Iterable.'''
	__slots__ = ()
	def __iter__(self):
		while False:
			yield None

class ProductionIterator(IterableProduction):
	'''Production Iterator pseudo-mixin.... any need for this?'''
	__slots__ = ()
	def next(self):
		'Return the next item from the iterator.  When exhausted, raise StopIteration.'
		raise StopIteration
	def __iter__(self):
		return self

class SizedProduction(MixInProduction):
	'''Mixin abstract Sized Production.  Mostly identical to collections.SizedProduction.'''
	__slots__ = ()
	def __len__(self):
		return 0

class ContainerProduction(MixInProduction):
	'''Mixin abstract Container Production.  Mostly identical to collections.Container.'''
	__slots__ = ()
	def __contains__(self, x):
		return False

class SequenceProduction(SizedProduction, IterableProduction, ContainerProduction):
	'''Mixin abstract Sequence Production.  Mostly identical to collections.Sequence.
	   If Sequence methods from this class are called directly or via super() then it
	   will behave as if it is empty.  This class is an analogue to python's
	   collection.Sequence without metaclass baggage.  A concrete implementation
	   must implement at least __getitem__, and preferably __len__ as well, as
	   the implementation here is very dumb.'''
	__slots__ = ()
	def __getitem__(self, index):
		raise IndexError

	def __len__(self):
		return len(iter(self))

	def __iter__(self):
		i = 0
		try:
			while True:
				v=self[i]
				yield v
				i += 1
		except IndexError:
			return

	def __contains__(self, value):
		for v in self:
			if v == value:
				return True
		return False

	def __reversed__(self):
		for i in reversed(range(len(self))):
			yield self[i]

	def index(self, value):
		'''S.index(value) -> integer -- return first index of value.
		   Raises ValueError if the value is not present.'''
		for i,v in enumerate(self):
			if v == value:
				return i
		raise ValueError

	def count(self, value):
		'S.count(value) -> integer -- return number of occurences of value'
		return sum(1 for v in self if v == value)

class TupleAppearanceDummy(object):
	__slots__ = ( 'reprstr' )
	def __init__(self, reprstr):
		self.reprstr = reprstr
	def __repr__(self):
		return self.reprstr
	def __str__(self):
		return self.reprstr

class TupleAppearanceProduction(SequenceProduction):
	'''A Mixin that makes a SequenceProduction look like a tuple'''
	__slots__  = ()
	def __pprint_repr(self):
		# Get at the "inherited" (standard) Production pprint_repr behavior via super, and then
		# construct a TupleAppearanceDummy object to act as a proxy for it.  This enables it to
		# act not as a string, but an object, within the tuple, which means it will not
		# have extra quoting around it in the resulting pprint_repr object.
		return ( TupleAppearanceDummy(super(TupleAppearanceProduction, self).pprint_repr()), ) + \
		       tuple([getattr(item, 'pprint_repr', lambda: item)() for item in iter(self)])
	def pprint_repr(self):
		return self.__pprint_repr()
	def __repr__(self):
		return repr(self.__pprint_repr())
	def __str__(self):
		return str(self.__pprint_repr())

class EmptyProduction(SequenceProduction):
	"""Placeholder for empty productions, implemented as a cointainer since typically
	   we use empty in rules like 'foo : empty | foo bar'.  Note that we do not set
	   no_auto_assign -- therefore to make EmptyProductions disappear, something must
	   happen in optimize() of containee classes.'  Otherwise, we end up with a
	   SequenceProduction with a single None containee (so, if that's what you want,
	   don't use this)"""
	__slots__ = ()
	def pprint_repr(self):
		return '<EmptyProduction>'
	def __repr__(self):
		return '<EmptyProduction>'
	def __str__(self):
		return '<EmptyProduction>'
	def __contains__(self, item):
		return False
	def __eq__(self, other):
		return isinstance(other, EmptyProduction) or \
			super(EmptyProduction, self).__eq__(other)

class MutableSequenceProduction(SequenceProduction):
	'''Mixin Abstract Mutable Sequence Production.  If MutableSequence methods from this
	   class are called directly, or via super(), then they will behave as if the
	   object is empty and/or broken.

	   Concrete subclasses must provide: __getitem__, __setitem__, __delitem__, __len__,
	   and insert()'''
	__slots__ = ()
	def __init__(self, p, **kwargs):
		super(MutableSequenceProduction, self).__init__(p, **kwargs)
	def __setitem__(self, index, value):
		raise IndexError
	def __delitem__(self, index):
		raise IndexError
	def insert(self, index, value):
		'S.insert(index, object) -- insert object before index'
		raise IndexError
	def append(self, value):
		'S.append(object) -- append object to the end of the sequence'
		self.insert(len(self), value)
	def reverse(self):
		'S.reverse() -- reverse *IN PLACE*'
		n = len(self)
		for i in range(n//2):
			self[i], self[n-i-1] = self[n-i-1], self[i]
	def extend(self, values):
		'S.extend(iterable) -- extend sequence by appending elements from the iterable'
		for v in values:
			self.append(v)
	def pop(self, index=-1):
		'''S.pop([index]) -> item -- remove and return item at index (default last).
		   Raise IndexError if list is empty or index is out of range.'''
		v = self[index]
		del self[index]
		return v
	def remove(self, value):
		'''S.remove(value) -- remove first occurrence of value.
		   Raise ValueError if the value is not present.'''
		del self[self.index(value)]
	def __iadd__(self, values):
		self.extend(values)
		return self

# 90% boilerplate cloning of patterns in UserList from 2.7. Exceptions are:
#
#   __init__:      translate the UserList code to use the standard MixIn patterns
#                  appropriate in BaseProductions
#
#   __{,i,r}add__: copy() rather than attempt to unravel the rather complicated
#                  puzzle of self-constructing ourselves without side-effects
#                  note that clone() assumes that it is good enough to
#                  call __new__() (which probably just does object.__new__(self.__class__))
#                  and assign all the inherited attributes from __slots__.
#
#  __copy__:	   copy data along with self (as if we were a subclass of data.__class__)
class UserListProduction(MutableSequenceProduction, TupleAppearanceProduction):
	'''Production Mixin analogoue to the UserList class.'''
	__slots__ = ()
	def __init__(self, p, user_list_data=None, **kwargs):
		self.data = []
		if user_list_data != None:
			if type(init_list_data) == type(self.data):
				self.data[:] = init_list_data
			elif isinstance(init_list_data, UserListProduction):
				self.data[:] = init_list_data.data[:]
			elif isinstance(init_list_data, Sequence):
				self.data = [ x for x in iter(init_list_data) ]
			else:
				self.data = list(user_list_data)
		super(UserListProduction, self).__init__(p, **kwargs)

	def __copy__(self):
		# clone data as well, otherwise (rslt.data is self.data)
		# which is extrememly counterintuitive
		rslt = super(UserListProduction, self).__copy__()
		rslt.data = copy(rslt.data)
		return rslt

	def __lt__(self, other): return self.data < self.__cast(other)
	def __le__(self, other): return self.data <= self.__cast(other)
	def __eq__(self, other): return self.data == self.__cast(other)
	def __ne__(self, other): return self.data != self.__cast(other)
	def __gt__(self, other): return self.data > self.__cast(other)
	def __ge__(self, other): return self.data >= self.__cast(other)
	def __cast(self, other):
		if isinstance(other, UserListProduction): return other.data
		else: return other
	def __cmp__(self, other):
		return cmp(self.data, self.__cast(other))
	def __hash__(self):
		# not supported for mutables.
		return None
	def __contains__(self, item): return item in self.data
	def __len__(self): return len(self.data)
	def __getitem__(self, i): return self.data[i]
	def __setitem__(self, i, item): self.data[i] = item
	def __delitem__(self, i): del self.data[i]
	def __getslice__(self, i, j):
		i = max(i, 0)
		j = max(j, 0)
		return self.__class__(self.data[i:j])
	def __setslice__(self, i, j, other):
		i = max(i, 0)
		j = max(j, 0)
		if isinstance(other, UserListProduction):
			self.data[i:j] = other.data
		elif isinstance(other, type(self.data)):
			self.data[i:j] = other
		else:
			self.data[i:j] = list(other)
	def __delslice__(self, i, j):
		i = max(i, 0)
		j = max(j, 0)
		del self.data[i:j]
	def __add__(self, other):
		rslt = copy(self)
		if isinstance(other,UserListProduction):
			rslt.data += other.data
		elif isinstance(other, type(self.data)):
			rslt.data += other
		else:
			rslt.data += list(other)
		return rslt
	def __radd__(self, other):
		rslt = copy(self)
		if isinstance(other, UserListProduction):
			rslt.data = other.data
		elif isinstance(other, type(self.data)):
			rslt.data = other
		else:
			rslt.data = list(other)
		rslt.data += self.data
		return rslt
	def __iadd__(self, other):
		if isinstance(other, UserListProduction):
			self.data += other.data
		elif isinstance(other, type(self.data)):
			self.data += other
		else:
			self.data += list(other.data)
		return self
	def __mul__(self, n):
		rslt = copy(self)
		rslt.data *= n
		return rslt
	def __rmul__(self, n):
		rslt = copy(self)
		rslt.data *= n
		return rslt
	def __imul__(self, n):
		self.data *= n
		return self
	def append(self, item): self.data.append(item)
	def insert(self, i, item): self.data.insert(i, item)
	def pop(self, i=-1): self.data.pop(i)
	def remove(self, item): self.data.remove(item)
	def count(self, item): return self.data.count(item)
	def index(self, item, *args): return self.data.index(item, *args)
	def reverse(self): self.data.reverse()
	def sort(self, *args, **kwargs): self.data.sort(*args, **kwargs)
	def extend(self, other):
		if isinstance(other, UserListProduction):
			self.data.extend(other.data)
		else:
			self.data.extend(other)

class EmptyIgnoringMutableSequenceProduction(MutableSequenceProduction):
	'''Mixin SequenceProduction that disappears EmptyProductions in its children'''
	__slots__ = ()
	def optimize(self):
		itemindexes = []
		for i in range(0, len(self)):
			if isinstance(self[i], EmptyProduction):
				itemindexes.append(i)
		if len(itemindexes) > 0:
			self.optimize_again = True
			itemindexes.reverse()
			for i in itemindexes:
				del self[i]
		super(EmptyIgnoringMutableSequenceProduction, self).optimize()

# recursively flattens trees of like Productions
class SequenceFlatteningProduction(MutableSequenceProduction):
	'''Mixin MutableSequenceProduction that merges trees of containees of the same class as self'''
	__slots__ = ()
	def __init__(self, p, flatten_sequences_of=None, **kwargs):
		if flatten_sequences_of is None:
			kwargs['error_exception_class'] = RulesParserInternalError
			kwargs['error_msg'] = 'flatten_sequences_of not specified'
			kwargs['is_error'] = True
			kwargs['internalize_error'] = False
		else:
			self.flatten_sequences_of = flatten_sequences_of
		super(SequenceFlatteningProduction, self).__init__(p, **kwargs)
	def optimize(self):
		# go backwards so that indexes don't break while we muck around
		for i in range(len(self) - 1, -1, -1):
			if isinstance(self[i], self.flatten_sequences_of):
				self.optimize_again = True
				# if we go backwards we have less math to do
				for i2 in range(len(self[i]) - 1, -1, -1):
					self.insert(i + 1, self[i][i2])
				del self[i]
		super(SequenceFlatteningProduction, self).optimize()

class AtomicProduction(BaseProduction):
	'''Simple prodution for literals, tokens, and other un-sub-divisible stuff.
	   Holds two dicts, value_map and type_map, for determining the value of
	   its single useful property, 'ID', which is an arbitrary generated constant.

	   If p[1] is a string, then this Production sets its value property to that
	   string, regardless of the mapping used to compute ID, unless an explicit
	   value keyword argument is provided to the constructor.

	   ID is generated from p[1] according to the following recipe, during the
	   init_hook phase of __init__.

	   first, by checking the Production's prodtype (if any) for
	   a matching key in the type_map, and second, by checking the Production's
	   value for a matching key in the value_map.  In either case, the value of
	   ID becomes the corresponding value from the matched item.

	   If no match exists or both mappings are empty, and the Production has a
	   non-None value attribute, then that is used for the value of ID.  If no match
	   exists, and value is None, but prodtype is not None, then that is used.

	   Finally, if no match exists, value is None, and prodtype is None, then p[1] itself
	   is used for ID, unless a true value was provided to the require_match keyword
	   argument of the constructor or require_match has otherwise been set to True.

	   In that case, an exception is raised.  Any of these matching behaviors may be
	   suppressed by providing corresponding values to the constructor.  So, as a
	   pathological example, passing value_map=None, type_map=None, value=None, prodtype=None,
	   and require_match=True to the constructor, an exception would always be raised,
	   which is useless.  But if the same constructor arguments were passed but with
	   require_match set to False, then ID would always be an exact copy of p[1],
	   which could actually be quite useful.  By default, require_match is treated
	   as True.'''
	__slots__ = ( 'ID', 'require_match', 'type_map', 'value_map' )
	def __init__(self, p, require_match=True, value_map={}, type_map={}, **kwargs):
		self.ID = None
		if not 'value' in kwargs and len(p) > 1 and is_string(p[1]):
			kwargs['value'] = p[1]
		self.value_map = value_map
		self.type_map = type_map
		self.require_match = require_match
		super(AtomicProduction, self).__init__(p, **kwargs)

	def init_hook(self):
		if self.value is not None and self.value in self.value_map:
			self.ID = self.value_map[self.value]
		elif self.prodtype is not None and self.prodtype in self.type_map:
			self.ID = self.type_map[self.prodtype]
		elif self.value is not None:
			self.ID = self.value
		elif self.prodtype is not None:
			self.ID = self.prodtype
		elif self.require_match:
			self.error_exception_class = RulesSyntaxError
			self.error_msg = 'Illegal keyword'
			self.is_error = True
		else:
			self.ID = self.p[1]
		super(AtomicProduction, self).init_hook()
	def __prod_repr(self):
		strselfID = str(self.ID)
		if "'" in strselfID:
			if '"' in strselfID:
				id_repr = repr(self.ID)
			elif '\n' in strselfID:
				id_repr = repr(self.ID)
			else:
				id_repr = '"' + strselfID + '"'
		elif '\n' in strselfID:
			id_repr = repr(self.ID)
		else:
			id_repr = "'" + strselfID + "'"
		id_repr = 'ID=' + id_repr
		if self.prodtype is not None:
			id_repr = str(self.prodtype) + ': ' + id_repr
		if self.is_error:
			return '<(ERROR: %s: %s) %s(%s)>' % (
				self.error_exception_class, self.error_msg, self.__class__.__name__, id_repr)
		else:
			return '<%s(%s)>' % (self.__class__.__name__, id_repr)
	def pprint_repr(self):
		return TupleAppearanceDummy(self.__prod_repr())
	def __repr__(self):
		return self.__prod_repr()
	def __str__(self):
		return self.__prod_repr()

class NonOptimizingInfixOpProduction(TupleAppearanceProduction, SequenceProduction):
	'''A very simple production for infix operations with no intrinisic optimizations.
	   Simply, lvalue will be p[1], op will be p[2], and rvalue will be p[3:].
	   As a sequence, we look like the tuple ( p[1], ) + p[3:].'''
	__slots__ = ( 'operator', 'operands' )
	def __init__(self, p, **kwargs):
		# no reason to wait for init_hook, are good to go.
		self.operator = p[2]
		self.operands = ( p[1], ) + tuple( p[3:] )
		super(NonOptimizingInfixOpProduction, self).__init__(p, **kwargs)
	def __getitem__(self, index):
		return self.operands[index]
	def __len__(self):
		return len(self.operands)
	def _BaseProduction__prod_repr(self):
		# this is invoked by TupleAppearanceProduction to generate /just/ the 
		# non-tuple-ish representation part... kinda gross to override it, but w/e
		op_repr = 'op=' + repr(self.operator)
		if self.prodtype is not None:
			op_repr = str(self.prodtype) + ': ' + op_repr
		if self.is_error:
			return '<(ERROR: %s: %s) %s(%s)>' % (
				self.error_exception_class, self.error_msg, self.__class__.__name__, op_repr)
		else:
			return '<%s(%s)>' % (self.__class__.__name__, op_repr)
