from git import Repo,GitCmdObjectDB
from git.exc import NoSuchPathError, InvalidGitRepositoryError
from os import getcwd
from os.path import isdir, islink

class InvalidOverlayRepositoryError(Exception):
	"""Thrown if a valid git repository is specified but it does not contain a profiles/repo_name file"""

class outRepo(object):
	"""Represents an overlay-upstream-tracking-enabled overlay, presumptively
	under control by git"""

	def __init__(self, overlaydir=None):
		"""Create a new outRepo instance

		:param overlaydir: a path or git-url which contains or is contained by
		a git repository containing a Gentoo overlay.  If not provided, the
		current working directory will be used as a default.

		:raise InvalidGitRepositoryError:
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

		self.repo=Repo(overlaydir, odbt=GitCmdObjectDB)
		self._verify_overlay()

	def _verify_overlay(self):
		hc = repo.head.commit
		if (!hc)
			raise InvalidOverlayRepositoryError("Headless repository probably contains no commits")
		hct = hc.tree

	@property
	def overlay_repo_dir(self):
		""":return: The directory of the targeted overlay repository"""
		return self.repo.working_dir

	def __eq__(self, rhs):
		if isinstance(rhs, outRepo):
			return self.repo.git_dir == rhs.repo.git_dir
		return False

	def __ne__(self, rhs):
		return not self.__eq__(rhs)

	def __hash__(self):
		return self.repo.hash()

	def __repr__(self):
		return '<OverlayUpstreamTracking.outRepo "%s">' % self.repo.git_dir
