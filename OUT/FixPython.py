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

__all__ = [ 'string_types', 'is_string' ]

string_types = None

def init_stringtypes():
	strtypes=[ type('') ]
	try:
		strtypes.append(basestring)
	except NameError:
		pass
	btype=type(b'')
	if btype not in strtypes:
		strtypes.append(btype)
	try:
		eval("strtypes.append(type(u''))")
	except SyntaxError:
		pass

	global string_types
	string_types = tuple(set(strtypes))

if string_types is None:
	init_stringtypes()

def is_string(thing):
	return isinstance(thing, string_types)
