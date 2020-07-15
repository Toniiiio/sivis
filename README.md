[![CRAN version](http://www.r-pkg.org/badges/version/sivis)](https://cran.r-project.org/package=sivis)


# Sivis: Turning browser clicks into reproducible scraping code.

A framework for creating and monitoring scrapers in R(Studio).

## Example video:

[![Sivis example](image.png)](https://www.youtube.com/watch?v=tFZ3os-GoNA)

## Status

Transitioning from pre-alpha to alpha version. Therefore, i am open for feedback and contributions. There are still
a lot of open issues, but i guess that will always stay that way as the scope is the www.

Currently working on
- transition to new style guide (style_guide.md)
- switching to oo
- removing major bugs

If you are interested to learn more, need help with the setup or get involved, feel free to write me: andreas.liebrand@hof.uni-frankfurt.de.
Any message is welcome!

## Installation
`devtools::install_github('Toniiiio/sivis')`

Run `sivis::chrome_addin_tutorial()` for instructions to add the chrome addin.

## Dependencies
Chrome

## Usage

In case target data is spread across multliple pages / requests 
- choose as many data as possible
- start on the second+ page. (first page data might be in another request)
- check if first page data can be derived from the requests that yield the 2nd, 3rd,.. pages (often the case).
