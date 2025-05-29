# Initialize renv if not already initialized
if (!require("renv")) install.packages("renv")
renv::init()

# Read required packages from YAML
required_packages <- suppressWarnings(yaml::read_yaml("packages.yml")$packages)

# Install missing packages
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    renv::install(pkg)
  }
}

# Create/update lockfile
renv::snapshot()
