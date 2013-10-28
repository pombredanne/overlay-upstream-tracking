#!/usr/bin/env python

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

from distutils.command.build import build as _build
from distutils.command.clean import clean as _clean
from distutils.spawn import spawn
from distutils.core import setup
from distutils.cmd import Command
from distutils import log
import os, sys, inspect

# http://stackoverflow.com/questions/279237
SETUP_PY_DIR = os.path.realpath(
	os.path.abspath(
		os.path.dirname(
			inspect.getfile(inspect.currentframe())
		)
	)
)
OUT_DIR = os.path.join(SETUP_PY_DIR, 'OUT')
RULESPARSER_PARSETAB = 'RulesParser_parsetab'
RULESPARSER_PARSETAB_PY_FULLPATH = os.path.join(OUT_DIR, RULESPARSER_PARSETAB + '.py')
RULESPARSER_PARSETAB_PYC_FULLPATH = os.path.join(OUT_DIR, RULESPARSER_PARSETAB + '.pyc')
# presumably there is no need for a PARSETAB_PYO_FILEPATH

class build_parsetabs(Command):
	"""sub-command to build parsetabs before build_py.  So far there is just the one,
	which lives at a hard-coded path, OUT/RulesParser_parsetab.py"""

	def initialize_options(self):
		pass

	def finalize_options(self):
		pass

	def run(self):
		"""build the parsetabs"""
		log.info("building OUT/RulesParser_parsetab.py from OUT.RulesParser")
		if os.path.exists(OUT_DIR) and os.path.exists(os.path.join(OUT_DIR, '__init__.py')) and not self.dry_run:
			pythonpath = sys.path or ''
			try:
				sys.path.insert(0, SETUP_PY_DIR)
				from OUT.RulesParser import RulesParser as _OUT_RulesParser
				O_RP = _OUT_RulesParser(outputdir=OUT_DIR)

				# instantiating it should have generated the parsetab
				if not os.path.exists(RULESPARSER_PARSETAB_PY_FULLPATH):
					log.warn("Failed to generate file '%s'.  Ignoring, but something ain't right" %
							RULESPARSER_PARSETAB_PY_FULLPATH)
			finally:
				del(sys.path[0])

class build(_build):
	"""Adds build_parsetabs subcommand to build"""
	def has_parsers(self):
		"""Always true until we find a reason to do anything more sophisticated"""
		return True

	_build.sub_commands.insert(0, ('build_parsetabs', has_parsers))

class clean(_clean):
	"""Cleans out parsetabs in addition to whatever else goes on in distutils.command.clean"""
	def run(self):
		_clean.run(self)
		for parsetab_fullpath in [RULESPARSER_PARSETAB_PY_FULLPATH, RULESPARSER_PARSETAB_PYC_FULLPATH]:
			if os.path.exists(parsetab_fullpath):
				log.info("Generated file '%s' present -- removing." % parsetab_fullpath)
				# go ahead and let any OSError flow through -- we just verified it exists so if we
				# cant remove it thats bad
				try:
					if not self.dry_run:
						os.remove(parsetab_fullpath)
				except OSError:
					log.warn("Failed to remove generated file '%s'." % parsetab_fullpath)
					pass
			else:
				log.warn("Generated file '%s' not present; can't remove it." % parsetab_fullpath)

setup(
	name='overlay-upstream-tracking',
	version='0.1.0',
	description='Gentoo Overlay Maintenance Toolkit',
	long_description='A toolkit for tracking and merging upstream changes into Gentoo overlays',
	license='GNU General Public License v2 (GPLv2)',
	classifiers=[
		'License :: OSI Approved :: GNU General Public License v2 (GPLv2)',
		'Topic :: Software Development :: Version Control',
	],
	author='Gregory M. Turner',
	author_email='gmt@be-evil.net',
	maintainer='Gregory M. Turner',
	maintainer_email='gmt@be-evil.net',
	packages=['OUT'],
	data_files=[('share/overlay-upstream-tracking/outrules.d', ['outrules.d/base'])],
	url='http://github.com/gmt/overlay-upstream-tracking',
	scripts=['scripts/pull-upstream-tracking', 'scripts/init-upstream-tracking'],
	requires=['dulwich (>=0.9.1)', 'ply (>=3.4)'],
	cmdclass={'build': build, 'build_parsetabs': build_parsetabs, 'clean': clean},
)
