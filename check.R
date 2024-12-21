
# devtools --------
library(devtools)

load_all()

document()
check_man()

check()

install()

build()


# usethis ---------
library(usethis)
use_package_doc()

# pkgdown ---------
# usethis::use_pkgdown()
pkgdown::build_site()

