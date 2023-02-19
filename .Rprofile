library(base)
library(utils)
if(!require(yaml)) {
  install.packages("yaml", repos = "")
}
library(yaml)

# set repos
options(repos="https://cran.ism.ac.jp/")

# pkgs
config <- yaml::read_yaml("config.yml")
pkgs   <- config$package
for(pkg in pkgs) {
  if(!require(pkg, character.only = T)) {
    install.packages(pkg)
  }
  require(pkg, character.only = T)
}


