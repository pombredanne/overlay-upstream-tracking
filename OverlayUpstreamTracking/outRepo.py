from dulwich.repo import Repo
from dulwich.errors import NotGitRepository
from os import getcwd
from os.path import isdir, islink, dirname

class InvalidOverlayRepositoryError(Exception):
	"""Thrown if a valid git repository is specified but it does not contain a profiles/repo_name file"""

class NoSuchPathError(Exception):
	"""Thrown if an attempt is made to create an outRepo corresponding to a non-existent path"""

class outRepo(object):
	"""Represents an overlay-upstream-tracking-enabled overlay, presumptively
	under control by git"""

	def __init__(self, overlaydir=None):
		"""Create a new outRepo instance

		:param overlaydir: a path within a Gentoo overlay under git version-control.
		If not provided, the current working directory will be used as a default.
		outrepo will automatically determine the root directory of any git repository,
		much as the git command-line would do.

		:raise dulwich.errors.NotGitRepository:
		:raise NoSuchPathError:
		:raise InvalidOverlayRepositoryError:
		:return: OverlayUpstreamTracking.outRepo"""
		if overlaydir == None or overlaydir == '':
			overlaydir=getcwd()
		else:
			# it is valid in Repo to pass a file path, here, but 
			# for present purposes that seems crazy -- ensure it's
			# a directory.
			if islink(overlaydir) or (not isdir(overlaydir)):
				raise NoSuchPathError(overlaydir)

		self.repo = None
		while not self.repo:
			try:
				self.repo = Repo(overlaydir)
			except NotGitRepository:
				oldoverlaydir = overlaydir
				overlaydir = dirname(overlaydir)
				if oldoverlaydir == overlaydir:
					raise
				pass
		self._verify_overlay()

	def _verify_overlay(self):
		# hc = repo.head.commit
		# if (!hc)
		# 	raise InvalidOverlayRepositoryError("Headless repository probably contains no commits")
		# hct = hc.tree
		return True

	@property
	def overlay_repo_dir(self):
		""":return: The directory of the targeted overlay repository"""
		return self.repo.path

	def __eq__(self, rhs):
		if isinstance(rhs, outRepo):
			return self.overlay_repo_dir == rhs.overlay_repo_dir
		return False

	def __ne__(self, rhs):
		return not self.__eq__(rhs)

	def __repr__(self):
		return '<OverlayUpstreamTracking.outRepo "%s">' % self.overlay_repo_dir
