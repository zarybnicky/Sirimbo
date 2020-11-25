all: public/style.css

public/style.css: public/style/main.scss
	sassc -t compact public/style/main.scss public/style.css

style-watch:
	while inotifywait -e close_write public/style/; do \
	  sassc -t compact public/style/main.scss public/style.css; done

check: vendor/bin/phpstan
	vendor/bin/phpstan analyse --level max files

vendor/bin/phpstan:
	composer install

.PHONY: upload upload-beta style-watch
