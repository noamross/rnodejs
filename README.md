# rnodejs - A convenience package for wrapping node.js modules

This package is a tool for wrapping [node.js](http://nodejs.org/) modules inside R packages.  Node
modules are included in the R package itself, and rnodejs will install dependencies in the R package
directory, including binary dependencies that can not be hosted on CRAN.

Package users will still need to install node itself.

## Usage

-   Include the node module as a directory in `inst/node/`.
-   Create functions that call node commands like so:

        #' @import rnodejs
        my_r_fn = node_fn("module", "command") # 'command optional, e.g. ("dat", "init")

-   Pass command line arguments

        my_r_fn(file="foo.txt") # equivalent to `module command --file="foo.txt"

-   By default, `my_r_fn()` will return a list of output (0/1), `stdout` and `stderr`


