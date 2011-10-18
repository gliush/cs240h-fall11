all: task
task: task.hs
	ghc --make task.hs

clean:
	rm -f task.{hi,o} task

.PHONY: tags
tags:
	hasktags -c *.hs
