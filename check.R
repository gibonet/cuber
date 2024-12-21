
# devtools --------
library(devtools)

load_all()

# build_readme()

document()
check_man()

check()

install()

build()


# usethis ---------
library(usethis)
use_package_doc()

use_vignette(name = "cuber")


# pkgdown ---------
# usethis::use_pkgdown()
pkgdown::build_site()

