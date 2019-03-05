## Test environments
* local OS X install, R 3.5.0
* ubuntu 14.04 (on travis-ci), R 3.5.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Addressed issues

This is a resubmission address comments by Swetlana Herbrandt (thank you!) I have incremented the package version to 0.2.1 for resubmission.

I have addressed the 3 requests:

1. please explain all acronyms (e.g. SUVR and DVR) in your Description text to avoid misunderstandings.

I have updated the description to address this.

2. Please write package names, software names and API names in single quotes (e.g. 'tacmagic') in your Description.

I have updated the description to address this.

3. Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace. That is not allow by CRAN policies. Please only write/save files if the user has specified a directory. In your examples/vignettes/tests you can write to tempdir().

The error was in the documentation of the 'fake_data' object; the save() function was meant to be a comment and it has been updated. Otherwise, save_tac() is the only function that writes to files. I have ensured that it only writes to files made by tmpdir() in the examples/vignette/test.



