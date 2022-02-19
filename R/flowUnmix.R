#' Unmix your flow cytometry files
#'
#' flowUnmix requires you to input a flowset of single colour controls, optionally a negative flowframe, and a flowset of samples.
#' It then unmixes this data and outputs a new fcs file.  Various unmixing methods are available.
#'
#' This function is the wrapper function for unmix_ff() and controlData().  You should provide clean positive controls.  If you controls include negative events, use guessPop=TRUE
#'
#' @param fs A flowSet of files to unmix
#' @param cs A flowset of single colour controls
#' @param unstained A flowframe of the negative control (optional, unless guessPop = TRUE)
#' @param unmixMethod Choose an inbuilt unmixing method: lsfit (the default), ginv, qr.solve ,lm.fit, crosspod, nnls, baselm
#' @param multiplier Most of these methods produce 0-1 values.  Multiply these to make them more palatable to your eye (default=10000)
#' @param guessPop Should flowUnmix try to select the positive population of the controls. Required for controls with both a positive and a negative
#' @return unmixed fcs files and optionally spectrum images
#' @author Christopher Hall, Babraham Institute
#' @family flowUnmix
#' @seealso \code{\link[flowUnmix]{controlData}},
#' \code{\link[flowUnmix]{unmix_ff}},
#' \code{\link[flowUnmix]{popchooser}}
#' @examples
#'
#' ## load files using flowCore()
#' bfFiles<-list.files("C:/Users/Chris/FCS_files/", full.names = TRUE)
#' controls_data<-read.flowSet(bfFiles[c(1:12)])
#' files2unmix<-read.flowSet(bfFiles[13])
#' negative_control<-read.FCS(bfFiles[14])
#'
#' ## run flowUnmix and ask it to guess the positive events
#' flowUnmix(fs=all_files2unmix,cs=controls_data,unstained=negative_control, guessPop = TRUE, popCheck = TRUE)
#' @export
#'
flowUnmix<-function(fs, cs, unstained=NULL, unmixMethod = "lsfit", multiplier=10000, guessPop = FALSE, popCheck = FALSE){
  if(unmixMethod=="all"){
    list<-c("lsfit","ginv","qr.solve","lm.fit","crosspod","nnls","baselm")
    for (item in list) {
      fsApply(fs, function(x)unmix_ff(fs=x,control=controlData(cs,unstained, guessPop, popCheck), unmixMethod = item, multiplier=multiplier))
    }
  } else {
    fsApply(fs, function(x)unmix_ff(fs=x,control=controlData(cs,unstained, guessPop, popCheck), unmixMethod = unmixMethod, multiplier=multiplier))
  }
}
