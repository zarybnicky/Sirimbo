all: style

style:
	sassc -t compact public/style/main.scss public/style.css

style-watch:
	while inotifywait -e close_write public/style/; do \
	  sassc -t compact public/style/main.scss public/style.css; done

upload:
	rsync -azP files/ olymp.z:/var/www/html/files

.PHONY: upload
