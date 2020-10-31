
<!-- README.md is generated from README.Rmd. Please edit that file -->

# quickform

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

I created this package because I wanted several features not implemented
in Google Forms, but ended up not needing it so I stopped development.
It is what it is. This is a spin off of the
[shinyforms](https://github.com/daattali/shinyforms) package. I wanted
survey participants to be able to come back to the survey and
edit/change/update their responses and I wanted to be able to have the
app email the unique ID that is created for them to allow them to return
and edit their responses. This package uses the {googledrive},
{googlesheets4}, and {gmailr} packages to persistently store data as
google sheets and email participants. Usually storing credentials and
authentication tokens can be tricky in shiny apps so this was designed
to hopefully *just work*. You will have to create the app and use it
interactively once and do the so-called “OAuth Dance” and then
{quickform} will cache the tokens using `cache = '.secrets'` in the
shiny app directory. Make sure to upload all auth files/credentials when
publishing to shinyapps.io. Additionally, to use {gmailr} you will need
to set up a ‘credentials.json’ and manually move it into the shiny app
home directory as the app will look for the credentials as
`'credentials.json'`. More information on setting up [gmailr
here](https://github.com/r-lib/gmailr). Additionally, {quickform}
provides convenient wrappers for Shiny widgets that match the names from
google forms (i.e `shiny::selectInput` -\> `quickform::dropdown`).

## Installation

You can install the released version of quickform from GitHub with:

``` r
devtools::install_github('brentscott93/quickform')
```

## Examples

This is the entire `app.R` code for the example that is [currently
deployed on shinyapps.io](https://brentscott93.shinyapps.io/quickform/).
The responses are saved in a [public Google Drive
folder](https://drive.google.com/drive/folders/1sgV9XFyYkf8jGp-xGAHcmIJfN83OkHt9?usp=sharing)
under my gmail (<brentscott93@gmail.com>). I setup credentials for the
{gmailr} app so a “returning user ID” can be emailed to the respondents
and they can return to update their responses. *Hint: copy/paste of this
code will not work for you without first making a credentials to send
emails.*

``` r
library(quickform)

quickform(title = "Quickform Demo",
          description = "Description of survey here.",
          questions = list(
                        list(id = "age", type = "numeric", title = "Age (yrs)", required = T),
                        list(id = "height", type = "height", title = "Height (ft-in)", required = T),
                        list(id = "weight_lbs", type = "numeric", title = "Weight (lbs)", required = T),
                        list(id = 'ethnicity', type = "multiplechoice", 
                             title = "Are you of Hispanic, Latino, or of Spanish origin?" , 
                             choices = list('No', 'Yes'), required = T),
                        list(id = "race", type = "multiplechoice", title = "Race", 
                              choices = list( 'American Indian or Alaska Native',
                                              'Asian',
                                              'Black or African American',
                                              'Native Hawaiian or Other Pacific Islander',
                                              'White',
                                              'Other'), 
                              required = T),
                        list(id = "bp_systolic", type = "numeric", title = "Blood Pressure (Systolic)"),
                        list(id = "bp_diastolic", type = "numeric", title = "Blood Pressure (Diastolic)"),
                        list(id = "resting_HR", type = "numeric", title = "Resting Heart Rate")
                        ),

                        gmail = T,
                        folder = 'quickform-demo',
                        returningUser = T,
                        emailId = T,
                        subject = 'Quickform Demo Email ID'

)
```

``` r
knitr::include_app("https://brentscott93.shinyapps.io/quickform/")
```

<iframe src="https://brentscott93.shinyapps.io/quickform/?showcase=0" width="100%" height="400px">

</iframe>
