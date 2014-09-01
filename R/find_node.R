# Internal functions to look for an install of node, get the most recent
# version
# Gleefully stolen from the rmarkdown package's approach to finding pandoc

# Environment used to cache the current node directory and version
.node <- new.env()
.node$dir <- NULL
.node$version <- NULL
.node$npm <- NULL
.node$npm_ver <- NULL
.node$messages <- character()
.node$deps <- FALSE

# get the node binary
node <- function() {
  find_node()
  file.path(.node$dir, "node")
}

npm = function() {
  find_node()
  file.path(.node$npm, "npm")
}

# Scan for a copy of node and set the internal cache if it's found.
find_node <- function() {

  if (is.null(.node$dir)) {

    # define potential sources
    sys_node <- Sys.which("node")
    sys_npm <- Sys.which("npm")
    sources <- c(getOption("NodePath"),
                 ifelse(nzchar(sys_node), dirname(sys_node), ""))
    sources_npm <- c(getOption("NpmPath"),
                 ifelse(nzchar(sys_npm), dirname(sys_node), ""))
    if (!is_windows()) {
      sources <- c(sources, path.expand("~/opt/node"))
      sources_npm <- c(sources_npm, path.expand("~/opt/npm"))
    }

    # determine the versions of the sources
    versions <- lapply(sources, function(src) {
      if (file.exists(src))
        get_node_version(src)
      else
        numeric_version("0")
    })
    
    versions_npm <- lapply(sources_npm, function(src) {
      if (file.exists(src))
        get_node_version(src)
      else
        numeric_version("0")
    })    

    # find the maximum version
    found_src <- NULL
    found_ver <- numeric_version("0")
    for (i in 1:length(sources)) {
      ver <- versions[[i]]
      if (ver > found_ver) {
        found_ver <- ver
        found_src <- sources[[i]]
      }
    }

    found_npm <- NULL
    found_npmver <- numeric_version("0")
    for (i in 1:length(sources_npm)) {
      ver <- versions_npm[[i]]
      if (ver > found_npmver) {
        found_npmver <- ver
        found_npm <- sources[[i]]
      }
    }

    # did we find a version?
    if (!is.null(found_src)) {
      .node$dir <- found_src
      .node$npm <- found_npm
      .node$version <- found_ver
      .node$npmver <- found_npmver
    }
  }
}

# Get an S3 numeric_version for the node utility at the specified path
get_node_version <- function(node_dir) {
  node_path <- file.path(node_dir, "node")
  version_info <- system(paste(shQuote(node_path), "--version"),
                           intern = TRUE)
  version = sub("v", "", version_info)
  numeric_version(version)
}

get_npm_version <- function(npm_dir) {
  node_path <- file.path(node_dir, "npm")
  version_info <- system(paste(shQuote(node_path), "--version"),
                           intern = TRUE)
  numeric_version(version)
}

is_windows <- function() {
  identical(.Platform$OS.type, "windows")
}


