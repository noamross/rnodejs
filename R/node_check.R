#' Check if node is installed
#' 
#' Put this function in \link{.onAttach} for in your package containing a node
#' module.  It will stop package loading (by throwing an error) if node is not 
#' available, and print a message if it is.
#' @export 
check_node_installed = function() {
  find_node()
  if (is.null(.node$dir)) {
    stop("This package requires node.js, which does not appear to be installed on your machine. Get node at http://nodejs.org.\nIf you have node installed in a non-standard directory, set the directory path with:\n\noptions(NodePath=PATH)")
  }
}

#' Check node module dependencies and install if missing
#' 
#' Put this function in \link{.onAttach} to check and install node module
#' dependencies on first load.  This is useful because some dependencies are 
#' binary and can not be hosted on CRAN.  Note that it is also run each time a
#' function created by \link{node_fn} is called. 
#' 
#' @param node_package the directory name of the node package in the R package
#' @param r_package the package name which wraps the function. Must be specified
#'                  as the function can't automatically detect the package
#'                  inside \link{.onAttach}
#' @param node_dir the directory where node packages are kept.  Defaults to
#'                 'node', which should be a directory under 'inst' when
#'                 creaing your own package
#' @export
check_node_deps = function(node_package, r_package, node_dir="node") {
  find_node()
  nodepath = system.file(node_dir, package=r_package)
  nodepackage_path = file.path(nodepath, node_package)
  if(.node$deps) {
    return(TRUE)
  } else {
    check_node_ver(nodepackage_path)
  }
  if(file.exists(file.path(nodepackage_path, "DEPS_INSTALLED")) &&
       (file.info(file.path(nodepackage_path, "DEPS_INSTALLED"))$mtime > 
          file.info(file.path(nodepackage_path, "node_modules"))$mtime)) {
    .node$deps = TRUE
    return(TRUE)
  } else if (npm_uptodate(nodepackage_path)) {
    file.create(file.path(nodepackage_path, "DEPS_INSTALLED"))
    .node$deps = TRUE
    return(TRUE)
  } else {
    node_deps_update(nodepackage_path)
    file.create(file.path(nodepackage_path, "DEPS_INSTALLED"))
    .node$deps = TRUE
  }
}

node_deps_update = function(nodepackage_path, verbose=FALSE) {
  if(!npm_connected()) stop("Cannot reach https://registry.npmjs.org/ to update dependencies")
  message("Updating node module dependencies")
  if(!verbose) {
    npm_out=system3(npm(), args=paste0("update --prefix ", nodepackage_path))
    npm_out=system3(npm(), args=paste0("update --prefix ", nodepackage_path))
  } else {
    npm_out = system2(npm(), args=paste0("update --prefix ", nodepackage_path))
    npm_out = system2(npm(), args=paste0("update --prefix ", nodepackage_path))
    status = attr(npm_out, "status")
    if(length(status)==0) status=0
    return(status)
  }
}

#' @import stringi
npm_uptodate = function(nodepackage_path) {
  if(!npm_connected()) {
    stop("Cannot reach https://registry.npmjs.org/ to check dependencies")
  }
  message("Checking node module depencies online")
  out = system3(npm(), args=paste0("outdated --prefix ", nodepackage_path))
  out = stri_replace_all_fixed(out$stdout, " > ", ">")
  out = stri_replace_all_regex(out, "[^\\S\\n]+", ",")
  out = read.csv(text=out, stringsAsFactors=FALSE)
  if(any(out$Current=="MISSING")) {
    return(FALSE)
  } else {
    Wanted = numeric_version(out$Wanted)
    Current = numeric_version(out$Current)
    if(any(Current < Wanted)) return(FALSE)
  }
  return(TRUE)
}

#' @import RCurl
npm_connected = function() {
  url.exists('https://registry.npmjs.org/')
}

check_node_ver = function(nodepackage_path) {
  package.data = fromJSON(file.path(nodepackage_path, "package.json"))
  package_name = package.data$name
  node_ver_req = package.data$engines["node"]
  if(is.null(node_ver_req) || node_ver_req=="") return(TRUE)
  
  if(numeric_version(node_ver_req) > .node$version) {
    stop(package_name, " requires node ", node_ver_req, ". You have node ",
         .node$version, ". Please update node.")
  } else {
    return(TRUE)
  }
}