file=index

flex:
	Rscript -e 'library(rmarkdown);rmarkdown::render("$(file).Rmd")'

run:
	Rscript -e 'library(rmarkdown);rmarkdown::run("$(file).Rmd", shiny_args = list(host = "127.0.0.1", port = 4040, launch.browser= FALSE))'

run_ufmt:
	Rscript -e 'library(rmarkdown);rmarkdown::run("$(file).Rmd", shiny_args = list(host = "200.17.60.42", port = 19127, launch.browser= FALSE))'

run_nyc:
	Rscript -e 'library(rmarkdown);rmarkdown::run("$(file).Rmd", shiny_args = list(host = "159.89.36.185", port = 4040, launch.browser= FALSE))'

open:
	qutebrowser $(file).html &

rsync_html:
	rsync -r -a -v -P -e ssh  $(file).html bibr@159.89.36.185:/var/www/roneyfraga.com/public_html/dash/2020_A4F/

rsync_files:
	rsync -avzhe ssh --info=progress2 --delete files_$(file)/ bibr@159.89.36.185:/var/www/roneyfraga.com/public_html/dash/$(file)/

