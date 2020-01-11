## testing environments

* local installs: 3.6.1 (Mac OS X); 3.6.2 (Ubuntu 18.04 LTS and Windows 10)
* Rhub (via `devtools::check_rhub()`)
* win-builder (devel, release, and oldrelease; via `devtools::check_win_*()`)

## R CMD check results

There were no ERRORS, WARNINGS, or NOTEs.

Some checks flagged words in the DESCRIPTION as possible misspellings, but these are correct:

* "ggplot2": the `Depends` package to which ggtda is an extension
* "tidyverse": the collection of package to which ggplot2 belongs
* "homology": the mathematical formalism underpinning algebraic topology and thence the concept of persistence
* "useRs": a stylized term for R users

## downstream dependencies

There are no downstream dependencies.
