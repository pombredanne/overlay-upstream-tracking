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

from pkg_resources import Requirement, resource_filename
from portage.const import EPREFIX
from ply.yacc import YaccProduction, YaccSymbol
from collections import Sequence, MutableSequence
from copy import copy, deepcopy
from itertools import chain
from pprint import pprint
import os

__all__ = [ 'EmptyProduction', 'GlobalRulesNotFoundException', 'get_global_rules_filepath',
	    'StatementsProduction', 'SyntaxErrorProduction', 'InternalErrorProduction',
	    'RulesProgramProduction', 'RulesSyntaxError', 'RulesParserInternalError',
	    'GlobalRulesNotFoundException', 'BaseProduction' ]

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

# FIXME: I hate that we are forced to stop pretending we are platform independent here.
# Even though we really aren't, the core idea rules engine could pretty easily be made
# platform independant and applied to other problem domains.  So I really wanted to avoid
# hard-coding any platform dependencies and have been going out of my way not to.
#
# TODO: ask upstream setuptools folks for help with this?
#
# Ideally this would use something like:
#
#   http://pythonhosted.org/setuptools/setuptools.html#non-package-data-files
#
# but, sadly, the install_data behavior documented there simply does not occur or work.
# Even if it did, that would put our files somewhere unacceptably obscure.
# Instead we rely on the 'files go in /usr' distutils default which setuptools (erroneously?)
# does not modify.... *confused*
_EXPECTED_GLOBAL_OUTRULES_D_LOCATION = EPREFIX + '/usr/share/overlay-upstream-tracking/outrules.d'

_global_rules_filepath = None
def get_global_rules_filepath():
	global _global_rules_filepath
	if _global_rules_filepath == None:
		# test for outrules.d/base... if it's there we'll assume everything's good.
		testpath = _EXPECTED_GLOBAL_OUTRULES_D_LOCATION + '/base'
		if not os.path.isfile(testpath):
			raise FileNotFoundException('Path %s not found' % _EXPECTED_OUTRULES_D_LOCATION)
		_global_rules_filepath = os.path.dirname(testpath)
	return _global_rules_filepath

# --------------------------- mini production framework (put in OOParsing?) ---------------

# nb: Mixin subclasses should come AFTER the main superclass in inherits clauses (MRO)
class BaseProduction(object):
	'''Base class for Productions containing mostly Error-Handling glue; accomodating
	   both instantly-explosive errors and error-delaying modes of operation'''
	__slots__ = [ 'is_error', 'error_msg', 'error_exception_class', 'optimize_again', 'p', 'value', 'type' ]
	def __init__(self, p, is_error=False, internalize_error=False, error_msg='Unknown Error',
			error_exception_class=RulesSyntaxError, no_auto_assign=False,
			value=None, type=None, **kwargs):
		# handle any error
		self.is_error = is_error
		self.error_msg = error_msg
		self.error_exception_class = error_exception_class
		self.p = p
		# kinda fuzzy on how this should work, oh well....
		try:
			self.value = getattr(p, 'value', getattr(p[0], 'value', None))
		except:
			self.value = None
			pass
		slice = getattr(p, 'slice', None)
		if slice:
			self.type = slice[0]
		else:
			self.type = None
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

		# a hook for subclasses to perform final validation steps and raise
		# exceptions in response to semantic inconsistencies
		self.validate()

		# a hook for subclasses to restructure their productions
		# using various recipies to simplify the semantic structure without
		# changing the meaning.  Keep optimizing until nobody requrests
		# reoptimization
		self.optimize_again = True
		while self.optimize_again:
			self.optimize_again = False
			self.optimize()

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
			if self.type is not None:
				p_repr += repr(self.type)
			if self.value is not None:
				p_repr += '/'
				p_repr += repr(self.value)
		else:
			p_repr = repr(self.p)
		if self.is_error:
			return '<(ERROR: %s: %s) %s: p=%s>' % (
				self.error_exception_class, self.error_msg, self.__class__.__name__, p_repr)
		else:
			return '<%s: p=%s>' % (self.__class__.__name__, p_repr)
	def pprint_repr(self):
		return self.__prod_repr()
	def __repr__(self):
		return self.__prod_repr()

	def __str__(self):
		return __prod_repr(self)
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
			elif self.type != other.type:
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
			getattr(cls, '__slots__', [])
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
			getattr(cls, '__slots__', [])
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
				# create a reference to the same p.
				setattr(rslt, attr, itemref)
			elif isinstance(itemref, type):
				# cloning class objects seems very extreme :) leave 'em.
				setattr(rslt, attr, itemref)
			else:
				setattr(rslt, attr, deepcopy(itemref))
		return rslt

class ErrorProduction(BaseProduction):
	'''Abstract convenience subclass for creating a standard internalized error'''
	__slots__ = []
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
	__slots__ = []
	def __init__(self, p, error_exception_class=RulesSyntaxError, **kwargs):
		kwargs['error_exception_class'] = error_exception_class
		super(SyntaxErrorProduction, self).__init__(p, **kwargs)

class InternalErrorProduction(ErrorProduction):
	__slots__ = []
	def __init__(self, p, error_exception_class=RulesParserInternalError, **kwargs):
		kwargs['error_exception_class'] = error_exception_class
		super(InternalErrorProduction, self).__init__(p, **kwargs)

# here we aggregate all the MixIn __slots__ variables so that we don't
# get layout conflicts during Metaclass construction
class MixInProduction(BaseProduction):
	__slots__ = [ 'data', 'flatten_sequences_of' ]
	pass

class SequenceProduction(MixInProduction, Sequence):
	'''Mixin nestable sequence abstraction for Productions.  If Sequence methods
	   from this class are called directly or via super() then it will behave as if
	   it is empty'''
	__slots__ = []
	def __init__(self, p, **kwargs):
		if type(self) == SequenceProduction:
			kwargs['is_error'] = True
			kwargs['internalize_error'] = False
			kwargs['error_msg'] = 'Abstract SequenceProduction instantiation'
			kwargs['error_exception_class'] = RulesParserInternalError
		super(SequenceProduction, self).__init__(p, **kwargs)

	def __getitem__(self, index):
		raise IndexError

	def __len__(self):
		return 0

	def _depth_iter(self, _class, seen, include_containers):
		'''Internal function implementing the depth_iter generator for a given set of arguments'''
		for i in iter(self):
			if isinstance(i, _class) and i not in seen:
				seen.append(i)
				if include_containers:
					yield(i)
				# infinite depthwise recursion:
				for i2 in i._depth_iter(_class, seen, include_containers):
					yield(i2)
			else:
				yield(i)
		return

	def depth_iter(self, container_class=None, include_containers=False):
		'''Returns an iterator that flattens any hierarchy of container elements,
		   treating any element which is not an instance of container_class as a
		   non-container.  Travereses depth first, yeilding only the leaves,
		   unless include_containers is True, in which case, also yeild the
		   containers, in-line, followed by their containees'''
		if not container_class:
			container_class = type(self)
		return self._depth_iter(container_class, [], include_containers)

	def deep_search(self, testfunc, test_containers=True):
		'''Checks all sub-items, recursively, for any item passing testfunc(item), with
		   automatic handling of self-containing items.  Only sub-items which are
		   instances of the same class as self are traversed.  Once a passing item
		   is found, it is immediately returned as ( True, item ); otherwise,
		   ( False, None ) is returned'''
		for i in self.depth_iter(include_containers=test_containers):
			if testfunc(i):
				return True, i
		return False, None

	def deep_contains(self, item):
		rslt, ignore = self.deep_search(lambda(x): x == item)
		return rslt
	def deep_contains_instance(self, _class=None):
		if not _class:
			_class = type(self)
		rslt, ignore = self.deep_search(lambda(x): isinstance(x, _class))
		return rslt

class TupleAppearanceProduction(SequenceProduction):
	'''A Mixin that makes a SequenceProduction look like a tuple'''
	__slots__  = []
	def __init__(self, p, **kwargs):
		if type(self) == TupleAppearanceProduction:
			kwargs['is_error'] = True
			kwargs['internalize_error'] = False
			kwargs['error_msg'] = 'Abstract TupleAppearanceProduction instantiation'
			kwargs['error_exception_class'] = RulesParserInternalError
		super(TupleAppearanceProduction, self).__init__(p, **kwargs)
	def __pprint_repr(self):
		# Get at the "inherited" (standard) Production __repr__ behavior with super()
		return ( super(TupleAppearanceProduction, self).pprint_repr(), ) + \
			 tuple([getattr(item, 'pprint_repr', lambda: item)() for item in iter(self)])
	def pprint_repr(self):
		return self.__pprint_repr()
	def __repr__(self):
		return repr(self.__pprint_repr())
	def __str__(self):
		return str(self.__pprint_repr())

class SingletonProduction(SequenceProduction):
	'''Mixin SequenceProduction that cannot deep_contain any instance of its own class'''
	__slots__= []
	def validate(self):
		if self.deep_contains_instance():
			self.is_error = True
			self.error_msg = 'Double Singleton encountered for ' + self.__class__.__name__
			self.error_exception_class = RulesSyntaxError
			self.Error()
		super(SingletonProduction, self).validate()

class EmptyProduction(SequenceProduction):
	"""Placeholder for empty productions, implemented as a cointainer since typically
	   we use empty in rules like 'foo : empty | foo bar'.  Note that we do not set
	   no_auto_assign -- therefore to make EmptyProductions disappear, something must
	   happen in optimize() of containee classes.'  Otherwise, we end up with a
	   SequenceProduction with a single None containee (so, if that's what you want,
	   don't use this)"""
	__slots__ = []
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

class MutableSequenceProduction(SequenceProduction, MutableSequence):
	'''Mixin Abstract Mutable Sequence Production.  If MutableSequence methods from this
	   class are called directly or via super() then it will behave as if it is empty/broken'''
	__slots__ = []
	def __init__(self, p, **kwargs):
		if type(self) == MutableSequenceProduction:
			kwargs['is_error'] = True
			kwargs['internalize_error'] = False
			kwargs['error_msg'] = 'Abstract MutableSequenceProduction instantiation'
			kwargs['error_exception_class'] = RulesParserInternalError
		super(MutableSequenceProduction, self).__init__(p, **kwargs)
	def __setitem__(self, index, value):
		raise IndexError
	def __delitem__(self, index):
		raise IndexError
	def __insert__(self, index, value):
		raise IndexError

# the following is 90% boilerplate cloning of the patterns in UserList.py.
# the only exceptions are:
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
	__slots__ = []
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
	__slots__ = []
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
	__slots__ = []
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

# ------------------------------ here is the semantically interesting stuff ------------------

class StatementsProduction(UserListProduction, SequenceFlatteningProduction, EmptyIgnoringMutableSequenceProduction):
	__slots__ = []
	def __init__(self, p, **kwargs):
		kwargs['flatten_sequences_of'] = StatementsProduction
		super(StatementsProduction, self).__init__(p, **kwargs)
	def init_hook(self):
		self.data = self.p[1:]
		super(StatementsProduction, self).init_hook()

class RulesProgramProduction(StatementsProduction, SingletonProduction):
	__slots__ = []
	pass
