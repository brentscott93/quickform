
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quickform

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

I created this package because I wanted several features not implemented
in Google Forms. This is a spin off of the
[shinyforms](https://github.com/daattali/shinyforms) package. I wanted
survey participants to be able to come back to the survey and
edit/change/update their responses and I wanted to be able to have the
app email the unique ID that is created for them to allow them to return
and edit their responses. This package uses the {googledrive},
{googlesheets4}, and {gmailr} packages to persistently store data as
google sheets and email participants. Usually storing credentials and
authentication tokens can be tricky in shiny apps so this was designed
to hopefully *just work*. You will have to create the app and use it
interactively once and do the so-called “OAuth Dance” but then the
googleforms package will store the tokens in the ‘.secrets’ folder in
the shiny app directory. Make sure to upload that folder when publishing
to shinyapps.io. Additionally, to use {gmailr} you will need to set up a
‘credentials.json’ and manually move it into the shiny app home
directory as the app will look for the credentials by using
*here::here(‘credentials.json’)*. More information on setting up
[gmailr here](https://github.com/r-lib/gmailr).

## Installation

You can install the released version of quickform from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("quickform")
```
