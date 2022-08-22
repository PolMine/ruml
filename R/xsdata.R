#' Locate xsdata utility.
#' 
#' This auxiliary function will check whether xsdata is on the path, available
#' in the directory `/opt/homebrew/bin` or at the environment variable `XSDATA_BIN`.
#' 
#' If xsdata is not available, the recommended approach is to call `pip install
#' xsdata[cli,lxml,soap]`. Optionally, try `pip3 install xsdata[cli,lxml,soap]`.
#' See [xsdata installation
#' instructions](https://xsdata.readthedocs.io/en/latest/installation.html)
#' @export
get_xsdata_bin <- function(){
  bin_candidates <- c(
    path = "xsdata",
    homebrew = "/opt/homebrew/bin/xsdata",
    env = Sys.getenv("XSDATA_BIN")
  )
  works <- sapply(
    bin_candidates,
    function(bin){
      y <- tryCatch(
        system2(bin, stdout = TRUE, stderr = TRUE),
        error = function(e) FALSE
      )
      if (isFALSE(y)) FALSE else TRUE
    }
  )
  if (!any(works)){
    warning("xsdata not available. See https://xsdata.readthedocs.io/en/latest/installation.html for installation instructions.")
  }
  bin_candidates[works][1]
}