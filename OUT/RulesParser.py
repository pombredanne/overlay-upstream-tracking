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

import ply.lex as lex
import re
import ply.yacc as yacc
import os
from inspect import isclass

class RulesSyntaxError(Exception):
	"""Thrown if errors are encountered parsing the rules files"""

class NewLexer(object, lex.Lexer):
	'''Wraps the old-style lex.Lexer class as a new-style class.'''

	def __new__(cls, *args, **kwargs):
		obj = object.__new__(cls)
		obj._lexer = lex.lex(module=obj, **kwargs)
		return obj

	def __getattr__(self, name):
		# ensure no crazy recursion issues
		if name in ('__dict__', '__new__', '_lexer', '__getattr__', '__setattr__', '__delattr__', '__str__', '__repr__'):
			return self.__dict__[name]

		try:
			return super(NewLexer, self).__getattr__(name)
		except AttributeError as e1:
			try:
				return getattr(self._lexer, name)
			except AttributeError as e2:
				if len(e1.args) > 0 and len(e2.args) > 0:
					e1.args = ('%s (and, from %s: %s)' % (e1.args[0],self._lexer,e2.args[0]) ,) + e1.args[1:]
				raise e1
	def __setattr__(self, name, value):
		if name in self.__dict__:
			return super(NewLexer, self).__setattr__(name, value)
		else:
			self.__dict__[name] = value
	def __delattr__(self, name):
		if name in self.__dict__:
			return super(NewLexer, self).__getattribute__(name)
		else:
			return delattr(self._lexer, name)

	def __str__(self):
		return "<%s (with _lexer: %s)>" % (self.__class__.__name__, str(self._lexer))
	def __repr__(self):
		return "<%s (with _lexer: %s)>" % (self.__class__.__name__, repr(self._lexer))

class NewParser(object):
	"""
	Base class for a lexer/parser that has the rules defined as methods
	"""
	def __init__(self, lexer, **kw):
		self.debug = kw.get('debug', 0)
		try:
			modname = os.path.split(os.path.splitext(__file__)[0])[1] + "_" + self.__class__.__name__
		except:
			modname = "parser"+"_"+self.__class__.__name__
		self.debugfile = kw.get('debugfile', modname + ".dbg")
		self.tabmodule = kw.get('tabmodule', modname + "_" + "parsetab")
		#print self.debugfile, self.tabmodule

		# Build the lexer (if passed a class for the lexer, instantiate it)
		if isclass(lexer):
			self._lexer = lexer(
				debug=self.debug,
				**dict(filter(lambda(x): x in [
					'optimize', 'debuglog', 'outputdir',
					'errorlog', 'reflags', 'lextab', 'nowarn'
				], kw))
			)
		else:
			self._lexer = lexer

		self._parser = yacc.yacc(
			module=self,
			debug=self.debug,
			debugfile=self.debugfile,
			tabmodule=self.tabmodule,
			**dict(filter(lambda(x): x in [
				'method', 'start', 'check_recursion', 'optimize', 'write_tables',
				'outputdir', 'debuglog', 'errorlog', 'picklefile'
			], kw))
		)

	def parse(self, data):
		return self._parser.parse(data, lexer=self._lexer)

	@property
	def tokens(self):
		return self._lexer.tokens

	@property
	def precedence(self):
		return self.get_precedence()

	def get_precedence(self):
		return ()

class Luthor(NewLexer):
	"""NewLexer-based lexer for rules language"""
	reserved = {
		'if': 'IF',
		'else': 'ELSE',
		'elif': 'ELIF',
		'revbump': 'REVBUMP',
		'pn': 'PN',
		'pv': 'PV',
		'pr': 'PR',
		'pvr': 'PVR',
		'p': 'P',
		'slot': 'SLOT',
		'category': 'CATEGORY',
		'portage_atom': 'PORTAGE_ATOM',
		'portage_repo': 'PORTAGE_REPO',
		'warn': 'WARN',
		'punt': 'PUNT',
		'die': 'DIE',
		'pass': 'PASS',
		'update': 'UPDATE',
		'upgrade_overlay': 'UPGRADEOVERLAY',
	}
	tokens = [
		'LPAREN',
		'RPAREN',
		'LCURLY',
		'RCURLY',
		'COLONEQUALS',
		'PLUSPLUSPLUS',
		'EQUALSEQUALS',
		'BANGEQUALS',
		'TILDEEQUALS',
		'GT',
		'LT',
		'GE',
		'LE',
		'SEMICOLON',
		'COMMENT',
		'OPENQUOTE',
		'CLOSEQUOTE',
		'VARIABLESUBSTITUTION',
		'ESCAPEDQUOTE',
		'ESCAPEDDOLLAR',
		'ESCAPEDSLASH',
		'ESCAPEDNEWLINE',
		'STRINGLITERALTEXT',
		'ID',
	] + list(reserved.values())

	states = (
		('stringliteral', 'exclusive'),
	)
	#
	# Regex rules for simple tokens
	#
	t_ignore = " \t"
	t_stringliteral_ignore = ""
	t_LPAREN = r'\('
	t_RPAREN = r'\)'
	t_LCURLY = r'{'
	t_RCURLY = r'}'
	t_COLONEQUALS = r':='
	t_PLUSPLUSPLUS = r'\+\+\+'
	t_EQUALSEQUALS = r'=='
	t_BANGEQUALS = r'!='
	t_TILDEEQUALS = r'~='
	t_GT = r'>'
	t_LT = r'<'
	t_GE = r'>='
	t_LE = r'<='
	t_SEMICOLON = r';'

	def t_newline(self, t):
		r'\n+'
		t.lexer.lineno += len(t.value)

	# need a function to ensure comments have precedence over everything but newlines
	def t_ignore_COMMENT(self, t):
		r'\#[^\n]*'
		pass

	# I see no need for fancy multi-line string capabilities,
	# even less so since we offer procedural string-concatenations,
	# and escaped newlines (already excessive for our purposes).
	def t_stringliteral_newline(self, t):
		r'\n'
		raise RulesSyntaxError("line %s: Illegal carraige return in string" % (t.lexer.lineno))

	# http://stackoverflow.com/questions/2039140
	def t_ID(self, t):
		r'[^\W\d]\w*'
		t.type = self.__class__.reserved.get(t.value, 'ID')
		return t

	def t_stringliteral_ESCAPEDQUOTE(self, t):
		r'\\"'
		return t

	def t_stringliteral_ESCAPEDDOLLAR(self, t):
		r'\\\$'
		return t

	def t_stringliteral_ESCAPEDSLASH(self, t):
		r'\\\\'
		return t

	def t_stringliteral_ESCAPEDNEWLINE(self, t):
		r'\\n'
		return t

	def t_stringliteral_VARIABLESUBSTITUTION(self, t):
		r'\$\{[^\W\d]\w*\}'
		return t

	# avoid confusing lexer results; valid variable substitutions
	# should have been picked up by the preceeding token rule
	def t_stringliteral_illegal_variable_substitution(self, t):
		r'\$[^}]*'
		raise RulesSyntaxError("line %s: Illegal variable substitution in string literal: '%s'" % (
			t.lexer.lineno, t.value))

	def t_stringliteral_STRINGLITERALTEXT(self, t):
		r'[^\\"\$][^\\"\$]*'
		return t

	def t_OPENQUOTE(self, t):
		r'"'
		t.lexer.begin('stringliteral')
		return t

	def t_stringliteral_CLOSEQUOTE(self, t):
		r'"'
		t.lexer.begin('INITIAL')
		return t

	def t_ANY_error(self, t):
		raise RulesSyntaxError("line %s: Illegal character in input: '%s'" % (
			t.lexer.lineno, t.value[0]))

	# test output
	def test(self, data):
		self.input(data)
		while True:
			tok = self.token()
			if not tok: break
			print tok

class RulesParser(NewParser):
	def __init__(self, lexer=None, **kwargs):
		if lexer == None:
			lexer = Luthor
		super(RulesParser, self).__init__(lexer, **kwargs)

	def p_rulesprogram(self, p):
		'rulesprogram : statements'
		p[0] = ( 'RULESPROGRAM', ) + p[1]

	def p_statements(self, p):
		'''statements : statements statement
		              | empty'''

		if len(p) == 3:
			p[0] = p[1] + ( p[2], )
		else:
			p[0] = ()

	def p_empty(self, p):
		'empty :'
		pass

	def p_statement(self, p):
		'statement : assignment SEMICOLON'
		p[0] = p[1]

	def p_assignment(self, p):
		'assignment : ID COLONEQUALS value'
		p[0] = ( 'ASSIGN', p[1], p[3] )

	def p_value(self, p):
		'value : string'
		p[0] = p[1]

	def p_string(self, p):
		'string : OPENQUOTE creamyfilling CLOSEQUOTE'
		p[0] = ( 'STRING', ) + p[2]

	def p_creamyfilling(self, p):
		'''creamyfilling : creamyfilling stringpart
		                 | empty'''
		if len(p) == 3:
			p[0] = p[1] + ( p[2], )
		else:
			p[0] = ()

	def p_stringpart(self, p):
		'''stringpart : varsub
		              | escape
			      | literal'''
		p[0] = p[1]

	def p_varsub(self, p):
		'varsub : VARIABLESUBSTITUTION'
		p[0] = ( 'VARIABLE-SUBSTITUTION', p[1][2:-1] )

	def p_escape(self, p):
		'''escape : ESCAPEDQUOTE
		          | ESCAPEDDOLLAR
			  | ESCAPEDSLASH
			  | ESCAPEDNEWLINE'''
		p[0] = ( 'STRINGLITERAL', p[1][1] )

	def p_literal(self, p):
		'literal : STRINGLITERALTEXT'
		p[0] = ( 'STRINGLITERAL', p[1] )

	def p_error(self, p):
		if p == None:
			raise RulesSyntaxError("Syntax error: unexpected end of file")
		else:
			raise RulesSyntaxError("line %s: Syntax error: '%s'" % (p.lexer.lineno, p.value))

# FIXME: move to some kind of documentation or manpage place & correct
# innacuracies
#
# Rules are applied in order of precedence to each ebuild and eclass in the overlay
# until one of them "binds" (more on what that means to follow).
#
# Rules are organized into two major "rulesets", each consisting of an arbitrary
# set of procedural "code".  However, the "language" is quite simplistic.
# Rulesets are organized into a shadow hierarchy with a somewhat similar
# structure to portage's "/etc/portage/env.d" (but not quite).
# The shadow hierarchy file structure must appear in the root
# of the overlay with the name "outrules.d" and might look like so:
#
# /outrules.d/some_rules_that_apply_to_everything
# /outrules.d/virtual/rules_that_apply_to_all_virtuals
# /outrules.d/virtual/more_rules_that_apply_to_all_virtuals
# /outrules.d/sys-devel/gcc/rules_that_apply_to_all_gcc_ebuilds
# /outrules.d/sys-devel/gcc/:4.7/rules_that_apply_to_all_gcc_ebuilds_in_the_4.7_slot
# /outrules.d/sys-devel/gcc/4.7.3-r1/rules_that_apply_only_to_gcc-4.7.3-r1
#
# The names of the rules files themselves in this structure are arbitrary.
# The default ruleset is located at ${EPREFIX}/usr/share/overlay-upstream-tracking/outrules.d
# and has the same structure and syntax as the overlay ruleset.
# Symbolic links may be used to eliminate redundancy; if you are using a lot
# of symlinks, you may wish to create a "template" directory under outrules.d
# to contain the linked-to rules and/or directories, so that you do not wind
# up with a hopeless muddle.  Hopefully, such sophisticated structures
# won't be needed anyhow.
#
# The order of ruleset evaluation is, first, the overlay ruleset, and second,
# the default ruleset.  Within each ruleset, the rules are evaluated starting
# with the most specific rule-file applicable to a given eclass in the overlay,
# on down to the least specific rule-file applicable, which, if any are present,
# would be the rules in the root of outrules.d.
#
# Rule-file evaluation occurs in a simple procedural "language".  Several variables
# are predefined for the evaluation of the rules.  Additional variables may be
# defined within the rulesets, but these are just "scratch" variables -- the single
# global namespace for variables is reinitialized to include only the predefined
# variables as each rule-file is executed.  These predefined variables are the familiar
# portage ones: P, PN, CATEGORY, PV, PR, PVR, PF, and SLOT.  Of these, SLOT has a
# notable performance penalty and is therefore evaluated lazily; however, the default
# rule for ebuilds does employ SLOT and therefore going out of your way to avoid it
# may not help much unless your overlay-level ruleset binds all the time (see below).
# None of the predefined variables may be modified; to attempt to assign to them
# is an error.
#
# The predefined variables always describe the /overlay/ ebuild or eclass for which
# rule-processing is currently underway -- they don't say anything about upstream
# at all; to learn about upstream, you must use the built-in functions (see below).
#
# The rule "language" consists of a series of statements.
# Supported statements are:
#
#   <variable-name> := <value>; (assignment)
#
#   if <boolean> { <statements> } [ elif <boolean> { <statements> } ... ] [ else { <statements> } ];
#
#   die <value> | pass | update <value>; (binding statements)
#
#   warn <value> | punt; (nonbinding statements)
#
#   upgrade_overlay <value>; (weird statements)
#
# <value>s are always strings.  They may be string literals, or they may be calculated by the built-in
# "functions".  More on those below.  The string literals are enclosed in double quotes and support a
# vaguely bash-like variable substitution syntax, i.e., "foo is \"${foo}\"."
# would evaluate to 'foo is "bar"' if the value of the foo variable were 'bar'.  No fancy bash variable
# substitution features are supported.  Literal '"' and '$' characters may be
# represented by quoting them with a backslash.  Literal '\' characters may also be
# similarly quoted, although it's hard to imagine why you'd want to.
#
# String values (there is no other kind) may be concatenated with the +++ operator.
#
# <boolean>s are always in one of three forms:
# (<value> COMPOP <value>), where COMPOP is one of:
# ==, !=, <, >, <=, >= (boolean comparison) or ~= (regular expression matching)
# or (<boolean> LOGOP <boolean>), where LOGOP is one of:
# && ||, or (! <boolean>).  The meanings should be fairly obvious.  True and False may
# also be used as boolean constants.
#
# The quantitative comparison operators are not alphabetical (indeed, attempting to use them for
# alphabetical comparison of non-like atoms will have undefined results and may be made an error
# in the future).  Instead, they are tuned to compare portage atoms (optionally truncated) with
# identical "${CATEGORY}/${PN}" values but different "${PV}" values.
#
# The die, pass and upgrade binding statements all cause further rule processing to permanently
# cease for the current ebuild or eclass.  die terminates with the provided error message
# and will also cause further processing of rules to cease.  Pass silently ignores the ebuild or
# eclass.  In the case of die and pass no changes will be made to the overlay tracking branch
# of any kind.
#
# upgrade <value> is the workhorse of the framework and will modify the overlay-tracking branch.
# The argument it takes should be a portage atom of sufficient specificity to single out a single
# ebuild or eclass (more on eclasses below, as they are a special case).  To find the
# correct ebuild or eclass, overlay-upstream-tracking will query portage in an environment
# that simulates removal of the overlay from ${PORTDIR_OVERLAY}; this can have side-effects so
# in all but the simplest cases it is best to provide a precise atom, starting with an equals sign,
# and, perhaps even ending with a particular ::repository specifier (these are respected by
# overlay-upstream-tracking even in contexts where portage doesn't permit them).  In addition to
# the individual ebuild, certain additional processing will occur:
#
# o $Id or $Header fields are blanked out
#
# o the entire ${FILESDIR} is marked for replication into the overlay-tracking-branch
#
# More features may be added to this list over time.
#
# The special value "---" may be passed to upgrade in order to signify that any existing upstream
# content should be dropped from the overlay-upstream-tracking branch.
#
# overlay-upstream-tracking will map the upstream ebuild specified by the upgrade argument onto
# the overlay ebuild rules were being evaluated for.  So if rules are being processed for foo/bar-1.0-r2
# in the overlay, and upgrade "foo/bar-1.0-r1" is invoked, the upstream contents of foo/bar/bar-1.0-r1.ebuild
# wind up, slightly modified, in the tracking branch as foo/ar/bar-1.0-r2.ebuild.
#
# The upgrade_overlay <value> statement is a special non-binding statement.  It provides the opportunity
# for semi-automatic version-bumping of in-overlay ebuilds, presumably in order to stay ahead of portage's
# corresponding versions.  By default, the command-line tools will respond to this statement (except in the
# trivial case where upgrade_overlay is requested but <value> already corresponds to the current
# ${CATEGORY}/${PF} of the existing ebuild) by emitting a warning along the lines of "it is recommended
# that you bump ${CATEGORY}/${PF} to <value>".  However if the user provides the --force argument to
# command-line tools, the upgrade will happen automatically.  The only exception is if the upgrade_overlay
# statement wants to change ${CATEGORY}/${PN} -- in that case, the upgrade never happens automatically,
# even if the user specified --force, and processing always terminates without changes to the upstream-
# tracking branch.
#
# Manifests and other such files are deliberately omittted.
#
# The functions pv, pvr, pf, pn, slot, and category are simple string-slicer-dicer functions with obvious
# meaning. the portage_repo <value> function finds the portage repository containing the upstream ebuild
# selected by <value> (if more than one are selected the results are undefined).  The portage_atom
# function will attempt to get a fully-qualified =cat/foo-3.3-r1:3.3 type atom corresponding to
# a given less-specific atom.  The function "revbump" takes a given fully-qualified atom and adds
# one to the revision number, so, i.e, =sys-devel/gcc-4.6.4 would become =sys-devel/gcc-4.6.4-r1.
#
# There is no such thing as user-defined functions, so henceforth 'functions' can be taken to mean
# 'built-in functions'.
#
# For eclasses, many of the built-in variables and functions are not applicable.  ${CATEGORY} is the fake-ish
# category "eclass" and ${PV} and similar varibles are always the empty string.  It is best to put separate
# logic for eclasses into the eclass directory to prevent any funny-business resulting from these differences.
#
# Two non-binding built-in statements exist: warn <value>, which will dump some output to the console but
# allow processing to continue as if nothing happened, and punt, which will terminate processing of the current
# rule-file but allow processing to continue if other rules are applicable further down in the
# rules-processing priority list.
#
# No changes will occur to the filesystem or repository until all rules are succesfully processed.  This
# does not mean that a rule must bind to every overlay ebuild -- indeed it is perfectly fine and normal
# for them not to, such as when no upstream ebuild or eclass corresponds to a given ebuild in the overlay.
# Instead the neccesary changes are memorized, checking for conflicting changes to FILESDIRs as it goes
# (any such conflicts result in an error and no changes are made).
#
# If a great number of novel ebuilds or eclasses exist in your overlay you may wish to automatically "pass"
# those ebuilds so that overlay-upstream-tracking will not get bogged down in pointeless, expensive SLOT
# computations for each one.
#
# Note that there is no portage_slot function -- this omission is because portage_atom always returns
# a result in ${CATEGORY}/${PN}-${PVR}:${SLOT} format, and therefore an equivalent result may be
# achieved by using slot(portage_atom("${foo}")).

