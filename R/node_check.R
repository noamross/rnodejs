#' Check if node is installed
#' 
#' Put this function in \link{.onAttach} for in your package containing a node
#' module.  It will stop package loading (by throwing an error) if node is not 
#' available, and print a message if it is.
#' @export 
check_node_installed = function() {
  find_node()
  if (is.null(.node$dir)) {
    stop("This package requires node.js, which does not appear to be installed on your machine.  Get node at http://nodejs.org.\nIf you have node installed in a non-standard directory, set the directory path with:\n\noptions(NodePath=PATH)")
  } else {
    message("You have node.js ", .node$version, " installed.")
  }
}

#' Check node module dependencies and install if missing
#' 
#' Put this function in \link{.onAttach} to check and install node module
#' dependencies.  This is useful because some dependencies are binary and can
#' not be hosted on CRAN.
#' 
#' @param node_package the directory name of the node package in the R package
#' @param node_dir the directory where node packages are kept.  Defaults to
#'                 'node', which should be a directory under 'inst' when
#'                 creaing your own package.
#' @param r_package the package name which wraps the function. Must be specified
#'                  as the function can't automatically detect the package
#'                  inside \link{.onAttach}
#' @param ask Ask before installing dependencies?
#' @param verbose Show verbose installation?  If \code{ask=TRUE}, user decides
#' @export
check_node_fn_deps = function(node_package, node_dir = "node", r_package = environmentName(parent.frame()),
                              ask=TRUE, verbose=FALSE) {
  
  # if(is.null(r_package)) r_package = environmentName(parent.frame()) 
  nodepath = system.file(node_dir, package=r_package)
  nodepackage_path = file.path(nodepath, node_package)
  package.json = file.path(nodepackage_path, "package.json")
  if(!file.exists(package.json)) {
    nodepackage_path = list.files(path=nodepath, pattern=node_package, 
                                  recursive=TRUE, include.dirs=TRUE)
    package.json = file.path(nodepath, nodepackage_path,"package.json")
    if(!file.exists(package.json)) {
      stop("Node package '", node_package, "' not found in R package '",
            r_package, "' under directory '", node_dir, "'.")
    }
  }
  package.data = fromJSON(package.json)
  package_name = package.data$name
  node_ver_req = package.data$engines["node"]
  
  message(package_name, " requires node ", node_ver_req, ". You have node ",
          .node$version)
  
  installed = node_deps_installed(nodepackage_path)
  if(installed) {
    message(package_name, " dependencies are installed.")
  } else {
    if(ask) {
      ask = readline(paste0(package_name, " has missing dependencies.  Install from www.npmjs.org? (y/n, 'v' for verbose): "))
      
      if(ask=="v") {
        verbose=TRUE
        ask="yes"
      }
      if (ask=="") {
        ask = FALSE
      } else {
        ask = match.arg(ask, c("yes", "no"))
        ask = ask=="yes"
      }
    }
    if(ask) {
        inst_success = node_deps_update(nodepackage_path, verbose=verbose)
      if(inst_success != 0) {
        stop("Install failed")
      } else {
        message("Install successful!")
      }
    } else {
      message("Package functions will likely fail without dependencies. Re-load to try installing again")
      if(interactive()) stop()
    }
  }
}

node_deps_update = function(nodepackage_path, verbose=FALSE) {
  if(!verbose) {
    npm_out=system3(npm(), args=paste0("update --prefix ", nodepackage_path))
    npm_out=system3(npm(), args=paste0("update --prefix ", nodepackage_path))
    return(npm_out$output)
  } else {
    npm_out = system2(npm(), args=paste0("update --prefix ", nodepackage_path))
    npm_out = system2(npm(), args=paste0("update --prefix ", nodepackage_path))
    status = attr(npm_out, "status")
    if(length(status)==0) status=0
    return(status)
  }
}

#' @import stringi
node_deps_installed = function(nodepackage_path) {
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
