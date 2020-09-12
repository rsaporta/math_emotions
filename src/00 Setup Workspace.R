## ================================================================ ##
##                                                                  ##
##                          math_emotions                           ##
##                                                                  ##
## ================================================================ ##
##             00 Setup Workspace.R                                 ##
## ================================================================ ##
## ---------------------------------------------------------------- ##


## PACKAGES
library(rsutils,     quiet=TRUE)
library(data.table,  quiet=TRUE)
library(magrittr,    quiet=TRUE)
library(jsonlite,    quiet=TRUE)

## CONFIRM PROJECT CORRECTLY SET
confirmProjectIsSetTo(projName="math_emotions", fail.if.not=TRUE)

## S3 CONFIGURATION
#| set_default_s3_root("s3://<your_bucket>")
set_default_s3_download_folder()

## JSON SETTING FOR USE WITH jsonlite
options(auto_unbox = FALSE)


## VERBOSE FLAGS
rsugeneral::setVerbose(which_flag="qry",             value=TRUE)
rsugeneral::setVerbose(which_flag="default",         value=TRUE)
rsugeneral::setVerbose(which_flag="source.p",        value=TRUE)
rsugeneral::setVerbose(which_flag="aggregateDT",     value=TRUE)
rsugeneral::setVerbose(which_flag="ssh",             value=TRUE)
rsugeneral::setVerbose(which_flag="s3",              value=TRUE)
rsugeneral::setVerbose(which_flag="gg",              value=TRUE)
rsugeneral::setVerbose(which_flag="gg_compile_func", value=TRUE)

## OUTPUT PARAMS & GLOBAL VARS USED IN verboseMsg()
rsuconsoleutils::setWidth(142, confirm=FALSE)
rsugeneral::set_minw(101)
rsugeneral::assignIfNotExist("verbose",       value = TRUE, envir=globalenv())
rsugeneral::assignIfNotExist("verbose.qry",   value = TRUE, envir=globalenv())
rsugeneral::assignIfNotExist("level.verbose", value = 0L,   envir=globalenv())

## PROJECT-SPECIFIC GLOBAL VARIABLES, eg:
#|example| assignIfNotExist("Var1",  value=LETTERS,  envir=globalenv())
#|example| assignIfNotExist("Var2",  value=letters,  envir=globalenv())
#|example| assignIfNotExist("Var3",  value=10000,    envir=globalenv())

