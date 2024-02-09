# Latest developments

# kwb.abimo 0.3.0 (2023-10-26)

* Fix GitHub Actions workflow files (master -> v2)
* Remove dependency on package "stringr"
* New (exported) functions
    - `calculate_bagrov_curve()`
    - `get_bagrov_curves_from_abimo()`
    - `install_abimo()`
* The `CODE` column is now expected to be of type character, not integer
* `dive_into()` now returns objects that have a `get()` method
* Improve organisation of functions in files
* many more changes (TODO: complete this list)

# kwb.abimo 0.2.0 (2022-06-23)

* Set GITHUB_PAT correctly in GitHub Actions configuration files
* Change structure of configuration object and rename class to "abimo_config"
* add read_config(), save_config(), dive_into(), get_named_children(),
  append_suffix_from_attribute()
* Update Vignette: describe how to use the new configuration object
* Remove scripts in inst/extdata (move to repository "abimo.scripts"")
* Move fullwinpath() to kwb.utils
* Create extdata_file() with kwb.utils::createFunctionExtdataFile()
* Add Hauke Sonnenberg as contributor

# kwb.abimo 0.1.0 (2022-04-30)

First release with "official" Abimo input data:

* kwb.abimo::abimo_input_2019

and main functions to configure/run Abimo:

* kwb.abimo::create_configurator()
* kwb.abimo::run_abimo()

# kwb.abimo 0.0.0.9000

* Added a `NEWS.md` file to track changes to the package.

* see https://style.tidyverse.org/news.html for writing a good `NEWS.md`


