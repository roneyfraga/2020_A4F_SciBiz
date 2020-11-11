file=slides_networks

render:
	Rscript -e 'library(rmarkdown);rmarkdown::render("$(file).Rmd")'

render_fragment:
	Rscript -e 'library(rmarkdown);rmarkdown::render("$(file).Rmd", html_fragment())'

open:
	qutebrowser $(file).html &

