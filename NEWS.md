# tidygam 0.1.0.9000

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
