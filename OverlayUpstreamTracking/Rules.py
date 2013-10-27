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

__all__ = [ 'GlobalRulesNotFoundException', 'get_global_rules_filepath' ]

class GlobalRulesNotFoundException(Exception):
	'''Thrown if the global rules repository is not present where OverlayUpstreamTracking.Rules
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
