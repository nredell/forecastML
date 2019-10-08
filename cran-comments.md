## Test environments

* local Windows install, R 3.5.3 and devel
* ubuntu 16.04 (on travis-ci), R 3.6.1

## R CMD check results

0 errors | 0 warnings | 0 notes

## Submission history

* This is a resubmission of a new-to-CRAN release. Changes include:

* 10-8-2019
    + Reduced DESCRIPTION from 103 to 74 words.
    + Added a number of stop("message") checks on the input arguments for all functions and 
    methods (~15 additional). The checks include various missing(), is.null(), and argument type checks.
    + Minor changes in help docs for language consistency and to clarify required arguments and 
    argument types.
    + Added new unit tests; package has ~40% code coverage.

* 10-2-2019
    + Reduced build time by several minutes (< 5 min w/ devtools::check())
    + Removed donttest{} in all examples
    + Fixed copyright format
