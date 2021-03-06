# PhD thesis

This repo contains source files for my PhD thesis titled, "Visualization and analysis of probability distributions of large temporal data", at Monash University. The pdf can be found [here](https://github.com/Sayani07/thesis-SG/blob/master/docs/thesis.pdf) and html can be found [here](sayani07.github.io/thesis-sg/).

# Summary


## Clone with `git-lfs`

Since this repo contains large data files (>= 50MB), you need to first download and install a git plugin called [`git-lfs`](https://git-lfs.github.com) for versioning large files, and set up Git LFS using command `git lfs install` in Terminal, in order to fully clone this repo.

## Reproducibility


The environment and R packages used to construct this thesis
can be recovered using the **renv** package. Run the following
code to install the packages used in this thesis:

```r
# install.packages("renv")
renv::restore()
```

The thesis can be compiled into both html and pdf format. Run the following codes in the terminal:

```zsh
Rscript --quiet _render.R "bookdown::pdf_book"
Rscript --quiet _render.R "bookdown::gitbook"
```


## Directories

* `Rmd/`: R Markdown source documents for thesis document.
* `scripts/`: R code to reproduce tables, figures and analysis.
* `data/`: Cleaned data used for thesis document.
* `data-raw/`: R code to generate data in `data/`.
* `img/`: Images made with other tools to illustrate ideas. 
* `bib/`: Bibliography files.
* `template/`: Monash thesis template from [robjhydman/MonashThesis](https://github.com/robjhyndman/MonashThesis).

## License

This work is licensed under a [![CC BY NC SA 4.0](https://img.shields.io/badge/License-CC%20BY%20NC%20SA%204.0-green.svg)](https://creativecommons.org/licenses/by-nc-sa/4.0/). The code contained in this work is available under the [MIT license](https://opensource.org/licenses/MIT).

## Acknowledgements

Thank you for creating this excellent reproducible thesis template, [Earo Wang](https://earo.me).
