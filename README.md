
### A repository for the code and data for [A rodent paradigm for studying perceptual decisions under asymmetric reward](https://arxiv.org/abs/2112.12278)

## How to install the package
1. `library(devtools)`
2. `install_git("https://github.com/xiaoyuezhuu/pgarXiv.git")`
3. `load_all()`
4. The package `pgarXiv` will be installed and the functions ready to use

## How to use it
1. Simply run `figure_xxx` and it will output a ggplot figure
2. All data is stored in `csv/`
3. All stan fits are stored in `fits/`
4. `.stan` files contain the stain models, there are here for reference only and won't be run by the pacakge
