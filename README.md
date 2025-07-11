<img src="PronghornLT_Logo.jpg" alt="Pronghorn LT Logo" width="600"/>

*Utilities for Pronghorn Line-transect Surveys in Wyoming*

## How to install
To install and load `pronghornLT` in R, run the following code.  Note, the 
`remotes` package is needed to install `pronghornLT` directly from GitHub, and 
the first line will install the `remotes` package if you don't already have it.

```
# Check whether the remotes package is installed, if not, install it from CRAN
if("remotes" %in% rownames(installed.packages()) == FALSE) {install.packages("remotes")}

# Install pronghornLT from GitHub
remotes::install_github("WyoGFD/pronghornLT")

# Load pronghornLT for use
require(pronghornLT)
```

## How to use
See the documentation and examples for the following functions:

### Survey design
- `calcLineLength`: determine how much transect line length needs to be surveyed to reach a target sample size of pronghorn groups detected
- `makeLines`:  create a spatial object of transect lines of a specified orientation and total length

### Data prep and QAQC
- `prepDataForAnalysis`: format and prepare raw survey data for analysis and print QAQC summaries

### Analysis
- `fitDistSampModel`: fit distance-sampling model to estimate abundance
