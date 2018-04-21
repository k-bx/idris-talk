all:
	cp ~/Dropbox/Deft/idris_talk_slides.md .
	cp -r ~/Dropbox/Deft/idris_for_haskell_developers/ .
	sudo docker build -t idris-talk .
	sudo docker run --rm -it -d --name idris-talk-container idris-talk /bin/bash
	sudo docker cp idris-talk-container:/src/idris_talk_slides.pdf ./idris_talk_slides.pdf
	sudo docker stop idris-talk-container
.PHONY: all
