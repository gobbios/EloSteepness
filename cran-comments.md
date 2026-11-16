## Resubmission

This is a patch release that fixes a stale URL in the `README.md`. And replaces a dependency (`aniDom`/`rptR` towards `lme4`).
The general testing and deployment workflow was also updated (see below for checking/testing via GitHub actions).

## Submitted version

EloSteepness 0.5.2

## Test environments
* local macOS, R 4.6.1
* win-builder (devel)
* GitHub actions:
  - macOS-latest (release)
  - ubuntu-latest (devel)
  - ubuntu-latest (oldrel-1)
  - ubuntu-latest (release)
  - windows-latest (release)

## R CMD check results
0 errors | 0 warnings | 0 notes

