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

from OverlayUpstreamTracking.RulesParser import RulesParser
from pkg_resources import Requirement, resource_filename
from portage.const import EPREFIX
import os

class FileNotFoundException(Exception):
	'''Thrown when file is not found'''

def get_global_rules_filepath():
	basepath = EPREFIX + '/usr/share/overlay-upstream-tracking/outrules.d/base';
	if not os.path.isfile(basepath):
		basepath = resource_filename(Requirement.parse("overlay-upstream-tracking"), "outrules.d/base")
		if not os.path.isfile(basepath):
			raise FileNotFoundException('outrules.d/base')
	return os.path.dirname(basepath)
