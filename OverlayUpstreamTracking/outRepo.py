from dulwich.repo import Repo
from dulwich.errors import NotGitRepository
from os import getcwd
from os.path import isdir, islink, dirname

class InvalidOverlayRepositoryError(Exception):
	"""Thrown if a valid git repository is specified but it does not contain a profiles/repo_name file"""

class NoSuchPathError(Exception):
	"""Thrown if an attempt is made to create an outRepo corresponding to a non-existent path"""

class NotARepository(Exception):
	"""Thrown by outRepo() when the provided repostiroy directory is not a repository of the specified type"""

class VCS(object):
	"""Abstract VCS support class"""
	def __init__(self, outrepo):
		"""Create VCS instance; outrepo is the parent outRepo to which this outVCS belongs.
		This is an abstract class by design -- it is the obligation of the subclass to
		call this inhertied __init__ and additionally to process a third argument to __init__,
		the overlay directory for the VCS repository instance, which, in the subclass constructor,
		must be used to initialize the repository object.

		:param outrepo: The outRepo instance to which this VCS instance will be forever married."""
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
				self._gitrepo = Repo(overlaydir)
			except NotGitRepository as e:
				oldoverlaydir = overlaydir
				overlaydir = dirname(overlaydir)
				if oldoverlaydir == overlaydir:
					# apparently the original overlaydir argument was not a git repo dir
					raise NotARepository("Not a Git Repository: %s" % origoverlaydir)
				pass

	def get_overlay_root(self):
		return self._gitrepo.path

class outRepo(object):
	"""Represents an overlay-upstream-tracking-enabled overlay, presumptively
	under control by git"""

	def __init__(self, overlaydir=None, vcstype=gitVCS):
		"""Create a new outRepo instance

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
		:return: OverlayUpstreamTracking.outRepo"""
		if overlaydir == None or overlaydir == '':
			overlaydir=getcwd()
		else:
			# it is valid in Repo to pass a file path, here, but 
			# for present purposes that seems crazy -- ensure it's
			# a directory.
			if islink(overlaydir) or (not isdir(overlaydir)):
				raise NoSuchPathError(overlaydir)

		# not strictly needed but for clarity: the one-off setter sort-of relies on this
		self._vcs = None
		self._vcs = vcstype(self, overlaydir)
		self._verify_overlay()

	def _verify_overlay(self):
		# hc = repo.head.commit
		# if (!hc)
		# 	raise InvalidOverlayRepositoryError("Headless repository probably contains no commits")
		# hct = hc.tree
		return True

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
		return '<OverlayUpstreamTracking.outRepo "%s" %s>' % (self.overlay_root, type(self._vcs).__name__)
