* A set up for emacs that just works right!

Naming comes from combing old english words for ways to describe a toolmaker, and deciding that cobbling together werk(German for work) and wright(Proto-Germanic -> Old English for maker/builder) was a great way to describe emacs, since it's both a workmaker and it allows creating tools

Steals a lot from Prelude in overall structure, and lots of other stealing everywhere else

To install: Copy the early-init.el to e.g. ~/.emacs.d/ . You don't want to symbolic link it, since emacs will ask to follow it. You could have an early-early init and do (setq vc-follow-symlinks t) if you really hate this; I just didn't think it made sense.
