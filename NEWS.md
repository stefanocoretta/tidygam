# tidygam 0.2.2.9000

## BREAKING

* Now `predict_gam()` uses the `lpmatrix` predictions which means that when excluding terms the user will have to set a value for the excluded variable. This is especially relevant when excluding (random) factor smooths: the user should pick a value for the random variable in the smooth to avoid the same predictions being output for all levels in the random variable (see the `Get started` vignette for examples).

## Developer

* Update renv and packages.


# tidygam 0.2.2

- Fixed date in DESCRIPTION.


# tidygam 0.2.1

- Fixed language field in DESCRIPTION.


# tidygam 0.2.0

## Added

* `get_difference()` returns difference between two smooths.

* `plot.tidygam.diff` method to plot difference smooth.

* Data `gest` and `struct`.

* `separate` and `sep_by` arguments in `predict_gam()` allow the user to separate variables in the model that were created with `interaction()`.

* Vignette `get-started.Rmd`.

## Fixed

* Error when predicting bivariate smooths (`s/te/ti()`, `fs/re` basis functions) where only the first variable was returned internally.

* Error when plotting difference smooth with no significant difference (closes #5).

* Error where `get_difference()` did not work when excluding random smooths (closes #8).



# tidygam 0.1.0

* First minor release of the package.

* Added a `NEWS.md` file to track changes to the package.
