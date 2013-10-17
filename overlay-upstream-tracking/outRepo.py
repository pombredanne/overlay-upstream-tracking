from git import Repo
from os import getcwd

class outRepo:
	def __init__(self, overlaydir=''):
		if overlaydir == '':
			overlaydir=os.getcwd()
		self.overlayRepo=Repo(overlaydir)
	def repo(self):
		return self.overlayRepo
