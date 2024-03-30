This is the first submission of ggtda to CRAN.

## Test environments

* Debian 10 local install, R 4.0.3
* OS X local install, R 4.0.3
* Ubuntu 16.04 (Travis CI), R 4.0.0
* Windows Server 2012 R2 (AppVeyor CI), R 4.0.2
* win-builder, r-devel

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE: new package submission to CRAN.

Some checks flagged words in the DESCRIPTION as possible misspellings, but these are correct:

* "ggplot2": the package to which ggtda is an extension
* "tidyverse": the collection of packages to which ggplot2 belongs
* "homology": the mathematical formalism underpinning algebraic topology and thence the concept of persistence
* "useRs": a stylized term for R users

There was one WARNING:
```
   Missing or unexported objects:
     ‘simplextree::expand’ ‘simplextree::maximal’
```
{ggtda} `Suggests` {simplextree}, but the version on CRAN is incompatible with the package from which it originally spun off, {Mapper} (itself not on CRAN). Instead, the older v0.9.1 remains compatible with {Mapper}. Since TDA workflows may rely both on {Mapper} and on {ggtda}, it will be necessary to ensure that {ggtda} is compatible with {simplextree} v0.9.1 until the incompatibility is resolved. The WARNING arises because the methods `simplextree::expand` and `simplextree::maximal` were not implemented in v0.9.1. To prevent them from being called when unavailable, the hidden object `.simplextree_version` is defined on load (see the file 'ggtda.R') and is queried inside functions to determine how to perform certain operations.

## Downstream dependencies

There are no downstream dependencies.
