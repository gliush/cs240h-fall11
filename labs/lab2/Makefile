all: task
task: task.hs
	ghc --make task.hs

clean:
	rm -f task.{hi,o} task

test:
	ghc -fhpc test.hs --make

.PHONY: tags
tags:
	hasktags -c *.hs
