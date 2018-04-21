all:
	docker build -t idris-talk .
	docker run --rm -it -d --name idris-talk-container idris-talk /bin/bash
	docker cp idris-talk-container:/src/idris_talk_slides.pdf ./idris_talk_slides.pdf
	docker stop idris-talk-container
.PHONY: all
