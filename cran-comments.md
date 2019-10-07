## Test environments

* local Windows install, R 3.5.3 and devel
* ubuntu 16.04 (on travis-ci), R 3.6.1

## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a new release.

* 10-8-2019
    + Reduced DESCRIPTION from 103 to 72 words.
    + Added a number of stop("message") checks on the input arguments for all functions and 
    methods (~20). The checks include various missing(), is.null(), and argument type checks.
    + Minor changes in help docs for language consistency and to clarify required arguments and 
    argument types.

* 10-2-2019
    + Reduced build time by several minutes (< 5 min w/ devtools::check())
    + Removed donttest{} in all examples
    + Fixed copyright format
