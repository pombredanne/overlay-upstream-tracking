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

from OUT.OOParsing import OOLexer, OOParser
from pprint import pprint

class RulesSyntaxError(Exception):
	'''Thrown if errors are encountered parsing the rules files'''

class Luthor(OOLexer):
	'''NewLexer-based lexer for rules language'''
	def __new__(cls, **kwargs):
		if not 'lextab' in kwargs:
			kwargs = kwargs.copy()
			kwargs['lextab'] = 'RulesParser_lextab'
		return OOLexer.__new__(cls, **kwargs)

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
		'true': 'TRUE',
		'false': 'FALSE',
	}
	tokens = [
		'LPAREN',
		'RPAREN',
		'LCURLY',
		'RCURLY',
		'COLONEQUALS',
		'PLUSPLUSPLUS',
		'EQUALSEQUALS',
		'EQUALSTILDE',
		'GT',
		'LT',
		'GE',
		'LE',
		'BANGEQUALS',
		'BANG',
		'OPENQUOTE',
		'CLOSEQUOTE',
		'VARIABLESUBSTITUTION',
		'ESCAPEDQUOTE',
		'ESCAPEDDOLLAR',
		'ESCAPEDSLASH',
		'ESCAPEDNEWLINE',
		'STRINGLITERALTEXT',
		'OR',
		'AND',
		'SEMICOLON',
		'ID',
	] + list(reserved.values())

	states = (
		('stringliteral', 'exclusive'),
	)
	#
	# Regex rules for simple tokens
	#
	t_ignore = ' \t'
	t_stringliteral_ignore = ''
	t_LPAREN = r'\('
	t_RPAREN = r'\)'
	t_LCURLY = r'{'
	t_RCURLY = r'}'
	t_COLONEQUALS = r':='
	t_PLUSPLUSPLUS = r'\+\+\+'
	t_EQUALSEQUALS = r'=='
	t_BANG = r'!'
	t_BANGEQUALS = r'!='
	t_EQUALSTILDE = r'=~'
	t_GT = r'>'
	t_LT = r'<'
	t_GE = r'>='
	t_LE = r'<='
	t_OR = r'\|\|'
	t_AND = r'&&'
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
		raise RulesSyntaxError('line %s: Illegal carraige return in string' % (t.lexer.lineno))

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

class RulesParser(OOParser):
	def __init__(self, lexer=None, **kwargs):
		if lexer == None:
			lexer = Luthor
		if not 'tabmodule' in kwargs:
			kwargs = kwargs.copy()
			kwargs['tabmodule'] = 'RulesParser_parsetab'
		super(RulesParser, self).__init__(lexer, **kwargs)

	def get_precedence(self):
		return (
			('left', 'OR'),
			('left', 'AND'),
			('right', 'BANG'),
		)

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
		'''statement : assignment SEMICOLON
		             | ifstmt SEMICOLON'''
		p[0] = p[1]

	def p_assignment(self, p):
		'assignment : ID COLONEQUALS value'
		p[0] = ( 'ASSIGN', p[1], p[3] )

	def p_value(self, p):
		'value : string'
		p[0] = p[1]

	def p_string(self, p):
		'string : OPENQUOTE creamyfilling CLOSEQUOTE'
		# we could just do p[0] = ( 'STRING', ) + p[2]; however, this will produce
		# ugly contiguous STRINGLITERAL sub-clauses; detect these and merge them
		quoteparts = []
		lastquotepart = [ None ]
		for quotepart in p[2]:
			quotepart = list(quotepart)
			if lastquotepart[0] == 'STRINGLITERAL' and quotepart[0] == 'STRINGLITERAL':
				lastquotepart[1] += quotepart[1]
			else:
				lastquotepart = quotepart
				quoteparts.append(lastquotepart)
		p[0] = ( 'STRING', ) + tuple([tuple(i) for i in quoteparts])

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

	def p_ifstmt(self, p):
		'''ifstmt : IF boolean LCURLY statements RCURLY optionalifclauses'''
		p[0] = ( 'IF', p[2], ( 'THEN', ) + p[4] ) + p[6]

	def p_optionalifclauses(self, p):
		'''optionalifclauses : ELIF boolean LCURLY statements RCURLY optionalifclauses
		                     | ELSE LCURLY statements RCURLY
				     | empty'''
		if len(p) == 7:
			p[0] = ( ( 'ELSE', ( 'IF', p[2], ( 'THEN', ) + p[4] ) + p[6] ), )
		elif len(p) == 5:
			p[0] = ( ( 'ELSE' ,) + p[3], )
		elif len(p) == 2:
			p[0] = ()
		else:
			raise RulesSyntaxError("line %s: Internal error: unexpected optionalifclauses production length %s" % (
				p.lexer.lineno, len(p)))

	def p_boolean(self, p):
		'''boolean : LPAREN leanboolean RPAREN'''
		p[0] = ( 'BOOLEAN' ,) + p[2]

	def p_leanboolean(self, p):
		'''leanboolean : TRUE
		               | FALSE
			       | BANG leanboolean
			       | LPAREN leanboolean RPAREN
			       | value compop value
			       | leanboolean OR leanboolean
			       | leanboolean AND leanboolean'''
		# True/False
		if len(p) == 2:
			p[0] = ( p[1], )
		# ! <leanboolean>
		elif len(p) == 3:
			if p[2] == ( 'true', ):
				p[0] = ( 'false', )
			elif p[2] == ( 'false', ):
				p[0] = ( 'true', )
			# optimize to avoid ( 'NOT', ( 'BOOLEAN', ( 'NOT' , ...)))
			elif len(p[2]) == 2 and p[2][0] == 'NOT':
				if len(p[2][1] != 2) or p[2][1][0] != 'BOOLEAN':
					raise RulesSyntaxError("line %s: Internal error: unexpected NOT target: %s" % (
						p.lexer.lineno, p[2][1]))
				p[0] = p[2][1][1]
			# theoretically we could do a bunch more optimizing here.... starts to get quite complicated
			# though, and probably not worth the effort.  For example ! ( x <= y || a == b ) ==> (x > y && a != b)
			# It might be worthwhile if we could drive 'NOT' out of the generated syntax entirely; however,
			# we can't, as there is no converse for the =~ regex matching operator.
			else:
				p[0] = ( 'NOT', ( 'BOOLEAN', ) + p[2] )
		# ( <leanboolean> )
		elif len(p) == 4 and p[1] == '(' and p[3] == ')':
			# just ignore the parenthesis, since this will become an unambiguous
			# ( 'BOOLEAN', <whatever> ) construct in all contexts where this production
			# is selected by the parser.
			p[0] = p[2]

		# leanboolean AND|OR leanboolean / value compop value
		elif  len(p) == 4:
			# optimize away silly shit like: ( 'OR' ( 'BOOLEAN' 'true' ) ('BOOLEAN' 'false' ))
			p[0] = 'unset'
			if p[2] == '||':
				logop = 'OR'
				if p[1] == ( 'true', ):
					p[0] = ( 'true' ,)
				elif p[3] == ( 'true', ):
					p[0] = ( 'true', )
				# false || XYZ ==> XYZ
				elif p[1] == ( 'false', ):
					p[0] = p[3]
				# XYZ || false ==> XYZ
				elif p[3]  == ( 'false', ):
					p[0] = p[1]
			elif p[2] == '&&':
				logop = 'AND'
				if p[1] == ( 'false', ):
					p[0] = ( 'false' ,)
				elif p[3] == ( 'false', ):
					p[0] = ( 'false', )
				# true && XYZ ==> XYZ
				elif p[1] == ( 'true', ):
					p[0] = p[3]
				# XYZ && true ==> XYZ
				elif p[3] == ( 'false', ):
					p[0] = p[1]

			# <value> compop <value>
			else:
				# optimizing away comparison-of-constants here is pretty hard / error-prone
				# plus, it will probably never come up in practice -- in short, it's not worth it.
				p[0] = ( ( p[2], p[1], p[3] ), )

			# non-optimized (so far) boolean logical operators: flatten nested same-logical-operator trees
			if p[0] == 'unset':
				# a few more optimizations we can try (comments assume logop == 'OR' but 'AND' treated identically)
				# (foo || bar) || (baz || quux) ==> ('OR' foo bar baz quux)
				if len(p[1][0]) >= 2 and p[1][0][0] == logop and len(p[3][0]) >= 2 and p[3][0][0] == logop:
					p[0] = ( ( logop, ) + p[1][0][1:] + p[3][0][1:], )
				# foo || (bar || baz) ==> ('OR' foo bar baz)
				elif len(p[3][0]) >= 2 and p[3][0][0] == logop:
					p[0] = ( ( logop, ( 'BOOLEAN', ) + p[1] ) + p[3][0][1:], )
				# foo || bar || baz ==> ('OR' foo bar baz)
				elif len(p[1][0]) >= 2 and p[1][0][0] == logop:
					p[0] = ( ( logop, ) + p[1][0][1:] + ( ( 'BOOLEAN', ) + p[3], ), )
				# non-optimizable boolean logical operator production:
				else:
					p[0] = ( ( logop, ( 'BOOLEAN', ) + p[1], ( 'BOOLEAN', ) + p[3] ), )
		else:
			raise RulesSyntaxError("line %s: Internal error: unexpected leanboolean production length %s" % (
				p.lexer.lineno, len(p)))

	def p_compop(self, p):
		'''compop : EQUALSEQUALS
		          | EQUALSTILDE
			  | BANGEQUALS
		          | GT
		          | LT
		          | GE
		          | LE'''
		p[0] = {
			'==': 'EQUAL',
			'!=': 'NOT-EQUAL',
			'=~': 'MATCH-REGEX',
			'GT': 'GREATER',
			'LT': 'LESS',
			'GE': 'GREATER-EQUAL',
			'LE': 'LESS-EQUAL',
		}[p[1]]

	def p_error(self, p):
		if p == None:
			raise RulesSyntaxError("Syntax error: unexpected end of file (missing semicolon?)")
		else:
			raise RulesSyntaxError("line %s: Syntax error: '%s'" % (p.lexer.lineno, p.value))

	def testparse(self, data):
		pprint(self.parse(data))

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
#   <variable-name> := <value>; 			(assignment)
#
#   if ( <boolean> ) { <statements> }			(conditional execution)
#       [ elif <boolean> { <statements> } ... ]
#       [ else { <statements> } ];
#
#   die <value> | pass | update <value>;		(binding statements)
#
#   warn <value> | punt;				(nonbinding statements)
#
#   upgrade_overlay <value>;				(special, see below)
#
# Note that the language meticulously avoids assigning any meaning to "=".
# Although this will probably trip C programmers up (sorry, guys!), the idea
# is to avoid any potential ambiguity/confusion/error about "=" vs. "==";
# I guess Pascal programmers will enjoy it, if there are any left :)
#
# Note also that none of the parenthesis, curly-braces, or semicolons above are
# optional.  When in doubt, spell it out!  This makes parsing much easier
# for the framework and avoids, i.e., the classic C dangling-else problem.
#
# In if clauses, it's legal for the <statements> parts to be empty, so, i.e.,
#
#   if (false) {} else { a:="foo"; };
#
# is valid and equivalent to just
#
#   a := "foo";
#
# <value>s are /always/ strings. They may be "literals", or they may be calculated by the built-in
# functions.  More on those below.  The string literals are enclosed in double quotes and support a
# vaguely bash-like variable substitution syntax, i.e.,
#
#   foo := "bar";
#   baz := "${foo}";
#
# would set the baz variable to the value "bar".
#
# No fancy bash variable substitution features are supported.  # Literal '"', '$' and '\' characters
# may be represented by quoting them with a backslash.  In order to prevent their default meanings
# (end-of-quote, variable-substitution and quoting, respectively), from being applied within a
# string literal.
#
# String values (there is no other kind) may be concatenated with the +++ operator.
#
# <boolean>s are always in one of these forms:
#
#   <value> COMPOP <value> (value comparison, see below)
#   <boolean> && <boolean> (logical and)
#   <boolean> || <boolean> (logical or)
#   ! <boolean>            (logical not)
#   true                   (true)
#   false                  (also true (just kidding, false, of course))
#   ( <boolean> )	   (to control order-of-operations)
#
# COMPOP may be any of the following comparison operators:
#
#   == (equality)
#   != (inequality)
#   <  (strictly less-than)
#   >  (strictly greater-than)
#   <= (less-than or equal to)
#   >= (greater-than or equal to)
#   =~ (matches regular-expression)
#
# Operator precedence and grouping for booleans does matter -- it works the same as in C.
#
# Note that, unlike most "real" languages, there is no kind of "coercion" allowed between values
# and booleans.  To effectively "coerce" a string into a boolean, you could always explicitly say
# what you wanted to happen and when, i.e.:
#
#   if ("${x}" == "true") {
#       y := "1";
#   } elif ("${x}" == "false") {
#       y := "0";
#   } else {
#       die();
#   };
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
# There is no support for user-defined functions.
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

