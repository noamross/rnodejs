% Generated by roxygen2 (4.0.1): do not edit by hand
\name{node_fn}
\alias{node_fn}
\title{Create a function to call a wrapped node.js package}
\usage{
node_fn(node_package, node_cmd = "", node_bin = node_package,
  r_package = environmentName(parent.frame()), node_dir = "node",
  return_list = TRUE, ...)
}
\arguments{
\item{node_package}{the directory name of the node package}

\item{node_bin}{the 'bin' command of the node package.  Defaults to the package name}

\item{node_dir}{the directory where node packages are kept.  Defaults to
'node', which should be a directory under 'inst' when
creaing your own package.}

\item{node_cmd}{Optional. Command argument following the node binary, e.g.,
"init" in "dat init".}

\item{r_package}{the package name which wraps the function.  Defaults to the
\link{parent.frame}, assuming that \code{node_fn} is
being used in a package.}

\item{return_list}{If \code{TRUE}, the new function will return a list of
the return value, stdout, and stderr from the call to the
node.js function.  If \code{FALSE}, the new function will
return the results of a \link{system2} call.}

\item{...}{Additional parameters to pass to \link{system2} if
                   \code{return_list=FALSE}}
}
\description{
Use this function to create functions that call node.js module commands. The
returned function will take arguments in the form of \code{argument=value} that
are passed to the node module CLI as \code{--argument=value}.
}
\details{
The function will retrun a list comprised of \code{output} (0/1 for success/
failure), \code{stdout}, and \code{stderr} from the node command.
}

