from git import Repo,GitCmdObjectDB
from os import getcwd

class outRepo:
	def __init__(self, overlaydir=''):
		if overlaydir == '':
			overlaydir=getcwd()
		self.overlayRepo=Repo(overlaydir, odbt=GitCmdObjectDB)
	def getOverlayRepo(self):
		return self.overlayRepo
