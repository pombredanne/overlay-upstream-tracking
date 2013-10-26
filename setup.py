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

from distutils.core import setup

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
	packages=['OverlayUpstreamTracking'],
	data_files=[('outrules.d', ['outrules.d/base'])],
	url='http://github.com/gmt/overlay-upstream-tracking',
	scripts=['scripts/pull-upstream-tracking', 'scripts/init-upstream-tracking'],
	requires=['dulwich (>=0.9.1)', 'ply (>=3.4)'],
)
