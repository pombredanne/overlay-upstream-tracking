import ply.lex as lex

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
# The functions pv, pvr, pf, slot, and category are simple string-extraction functions with obvious
# meaning. the portage_repo <value> function finds the portage repository containing the upstream ebuild
# selected by <value> (if more than one are selected the results are undefined).  The portage_atom
# function will attempt to get a fully-qualified =cat/foo-3.3-r1:3.3 type atom corresponding to
# a given less-specific atom.  The function "revbump" takes a given fully-qualified atom and adds
# one to the revision number, so, i.e, =sys-devel/gcc-4.6.4 would become =sys-devel/gcc-4.6.4-r1.
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
# Note that just because you request an upgrade in your code does not mean the corresponding filesystem
# changes will actually occur.  By default the command-line tools will simply return a warning in this
# case and refuse to upgrade the tracking branch.  However, if --force arguments are provided, the
#
class Luthor(object):
	tokens = (
			'WHEN',
			'LPAREN',
			'RPAREN',
			'LCURLY',
			'RCURLY',
			'EQUALSEQUALS',
			'COLONEQUALS',
			'SEMICOLON',
			'DO',
			'CATEGORY',
			'PN',
			'PV',
			'P',
			'SLOT',
			'MAJORSLOT',
			'MINORSLOT',
			'MATCHES',
			'ATOM',
			'WARN',
	)