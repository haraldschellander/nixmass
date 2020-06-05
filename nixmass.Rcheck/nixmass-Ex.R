pkgname <- "nixmass"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "nixmass-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('nixmass')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("hsdata")
### * hsdata

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: hsdata
### Title: Daily snow height data for a northern alpine station
### Aliases: hsdata

### ** Examples

## Load example data 
data("hsdata")

## explore dataset
head(hsdata)
plot(hsdata$hs, type="o")

## compute snow water equivalents
o <- nixmass(hsdata, model="delta.snow",verbose=TRUE)
plot(o)
     
o1 <- nixmass(hsdata, alt=600, region.jo09 = 6, region.gu19 = "central",
             snowclass.st10 = "alpine", verbose = FALSE)
plot(o1)
summary(o1)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("hsdata", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("nixmass")
### * nixmass

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: nixmass
### Title: SWE modeling with the delta.snow process based model and several
###   emprical regression models.
### Aliases: nixmass

### ** Examples

## Load example data with realistic snow height values 
## from a station at 600 meters in the northern Alps
## Note that the winter season is set to an arbitrary date 
## to mask its origin
data("hsdata")
o <- nixmass(hsdata, model="delta.snow",verbose=TRUE)
plot(o)
     
o1 <- nixmass(hsdata, alt=600, region.jo09=6, region.gu19 = "central",
             snowclass.st10 = "alpine", verbose = FALSE)
plot(o1)
summary(o1)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("nixmass", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
