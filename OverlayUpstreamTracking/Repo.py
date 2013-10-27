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

from dulwich.repo import Repo as dwRepo
from dulwich.errors import NotGitRepository
from os import getcwd, F_OK, R_OK, X_OK, W_OK
from os.path import isdir, islink, dirname
import os
from stat import S_ISDIR, S_ISREG
from errno import EACCES, EPERM, ENOENT

class InvalidOverlayRepositoryError(Exception):
	"""Thrown if a valid git repository is specified but it does not contain a profiles/repo_name file"""

class NoSuchPathError(Exception):
	"""Thrown if an attempt is made to create a Repo corresponding to a non-existent path"""

class NotARepository(Exception):
	"""Thrown by Repo() when the provided repostiroy directory is not a repository of the specified type"""

class RepositoryPermissionsError(InvalidOverlayRepositoryError):
	"""Thrown when a requested operation cannot complete due to file permissions trouble"""

class VCS(object):
	"""Abstract VCS support class"""
	def __init__(self, outrepo):
		"""Create VCS instance; outrepo is the parent Repo to which this outVCS belongs.
		This is an abstract class by design -- it is the obligation of the subclass to
		call this inhertied __init__ and additionally to process a third argument to __init__,
		the overlay directory for the VCS repository instance, which, in the subclass constructor,
		must be used to initialize the repository object.

		:param outrepo: The Repo instance to which this VCS instance will be forever married."""
		self.repo = outrepo
		outrepo.vcs = self

	@property
	def overlay_root(self):
		# indirection hack supports polymorphic getter in superclass:
		# http://stackoverflow.com/questions/3393534
		return self.get_overlay_root()

	def get_overlay_root(self):
		raise NotImplementedError('VCS.get_overlay_root called directly')

class gitVCS(VCS):
	"""Git implementation of the VCS interface"""
	def __init__(self, outrepo, overlaydir):
		super(gitVCS, self).__init__(outrepo)
		self._gitrepo = None
		origoverlaydir = overlaydir
		while not self._gitrepo:
			try:
				self._gitrepo = dwRepo(overlaydir)
			except NotGitRepository as e:
				oldoverlaydir = overlaydir
				overlaydir = dirname(overlaydir)
				if oldoverlaydir == overlaydir:
					# apparently the original overlaydir argument was not a git repo dir
					raise NotARepository("Not a Git Repository: %s" % origoverlaydir)
				pass

		# bare repositories don't provide the regular-filessytem stuff on which we rely.
		# perhaps this could be supported by abstracting all filesystem operations through
		# the vcs; however, this sounds like a pain in the ass to implement!
		if not self._gitrepo.has_index():
			raise InvalidOverlayRepositoryError("Bare repository '%s' not supported" % self._gitrepo.path)

	def get_overlay_root(self):
		return self._gitrepo.path

class Repo(object):
	"""Represents an overlay-upstream-tracking-enabled overlay, presumptively
	under control by git"""

	def __init__(self, overlaydir=None, vcstype=gitVCS):
		"""Create a new Repo instance

		:param overlaydir: a path within a Gentoo overlay under git version-control.
		If not provided, the current working directory will be used as a default.
		outrepo will automatically determine the root directory of the VCS repository,
		much as the git command-line would do.

		:param vcstype: The VCS type to instantiate.  Currently, only gitVCS is implemented
		but hypothetically, any subclass implementing the OverlayUpstreamTracking.VCS interface
		could be used.

		:raise NotARepository:
		:raise NoSuchPathError:
		:raise InvalidOverlayRepositoryError:
		:return: OverlayUpstreamTracking.Repo"""
		if overlaydir == None or overlaydir == '':
			overlaydir=getcwd()
		else:
			# it is valid in dwRepo to pass a file path, here, but
			# for present purposes that seems crazy -- ensure it's
			# a directory.
			if islink(overlaydir) or (not isdir(overlaydir)):
				raise NoSuchPathError(overlaydir)

		# not strictly needed but for clarity: the one-off setter sort-of relies on this
		self._vcs = None
		self._vcs = vcstype(self, overlaydir)

		# filesystem-level sanity checks
		self._verify_overlay()

	def _check_path_isdir(self, path):
		"""Check for a readable, traverseable directory -- not a symlink or a subdirectory of
		a symlink -- at the specified path relative to overlay_root.  This is only for sanity-checking,
		not security boundary enforcement purposes!

		:return: True if a relative path exists under overlay_root, is a directory, and is not
		a symlink, and all parent directories also meet this criterion, up to overlay_root itself.
		Preceeding and trailing path separators are stripped."""
		while len(path) > 0 and path[0] == os.sep:
			path=path[1::]
		while len(path) > 0 and path[-1] == os.sep:
			path=path[:-1]
		testpath = self.overlay_root

		# recursively check all the parents first.  Then append path to testpath and
		# proceed with the outermost check.
		if len(path) > 0:
			if not self._check_path_isdir(dirname(path)):
				return False
			testpath = os.path.join(testpath, path)

		# the actual checking, translating errors we "support" into framework exceptions
		try:
			st = os.lstat(testpath)
		except OSError as e:
			if e.errno == ENOENT:
				raise NoSuchPathError("stat('%s') -> ENOENT: %s" % (testpath, e))
			if e.errno == EACCES:
				raise RepositoryPermissionsError("stat('%s') -> EACCES: %s" % (testpath, e))
			if e.errno == EPERM:
				raise RepositoryPermissionsError("stat('%s') -> EPERM: %s" % (testpath, e))
			else:
				raise e

		if not os.access(testpath, R_OK|X_OK):
			raise RepositoryPermissionsError("os.access('%s',R_OK|X_OK): False" % (testpath))

		m = st.st_mode
		if not S_ISDIR(m):
			# it exists but is not a directory.  It's probably a link.
			return False

		return True

	def _check_path_isreg(self, path):
		""":return: True iff the provided (relative) path is a regular file under overlay_root"""
		while len(path) > 0 and path[0] == os.sep:
			path=path[1::]

		testpath = self.overlay_root
		if len(testpath) > 0:
			testpath = os.path.join(testpath, path)
		try:
			st = os.lstat(testpath)
		except OSError as e:
			if e.errno == ENOENT:
				raise NoSuchPathError("stat('%s') -> ENOENT: %s" % (testpath, e))
			if e.errno == EACCES:
				raise RepositoryPermissionsError("stat('%s') -> EACCES: %s" % (testpath, e))
			if e.errno == EPERM:
				raise RepositoryPermissionsError("stat('%s') -> EPERM: %s" % (testpath, e))
			else:
				raise e
		if not os.access(testpath, R_OK):
			raise RepositoryPermissionsError("os.access('%s',R_OK|X_OK): False" % (testpath))
		m = st.st_mode
		if not S_ISREG(m):
			# exists but is not a regular file: perhaps it's a link.
			return False
		return True

	def _verify_overlay(self):
		# sanity check the overlay: we expect at least profile/repo_name to exist or else
		# we consider it a lost cause
		if not self._check_path_isdir('profiles'):
			raise InvalidOverlayRepositoryError("'profiles' seems to exist but is not a directory in '%s'" % self.overlay_root)
		if not self._check_path_isreg('profiles/repo_name'):
			raise InvalidOverlayRepositoryError("'profiles/repo_name' seems to exist but is not a regular file in '%s'" % self.overlay_root)

	@property
	def vcs(self):
		assert self._vcs
		return _vcs

	@vcs.setter
	def vcs(self, value):
		# vcs should only be set once, via self.__init__ -> VCS.__init__ codepath
		assert value.repo == self, _vcs == None
		_vcs = value

	@property
	def overlay_root(self):
		""":return: The directory of the targeted overlay repository"""
		return self._vcs.overlay_root

	def __repr__(self):
		return '<OverlayUpstreamTracking.Repo "%s" %s>' % (self.overlay_root, type(self._vcs).__name__)
