from distutils.core import setup
setup(name='overlay-upstream-tracking',
      version='0.1',
      classifiers=[
	      'License :: OSI Approved :: GNU General Public License v2 (GPLv2)',
	      'Topic :: Software Development :: Version Control',
	      ],
      description='Tools to partially automate tracking of upstream ebuild/eclass code deltas in order to ease the overlay maintenace process.',
      author='Gregory M. Turner',
      author_email='gmt@be-evil.net',
      packages=['OverlayUpstreamTracking'],
      url='http://fixme/',
      scripts=['scripts/pull-upstream-tracking'],
      requires=['GitPython (>=0.3.1)'],
      )
