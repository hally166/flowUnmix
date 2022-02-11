#' Unmix your flow cytometry files
#'
#' flowUnmix requires you to input a flowset of single colour controls, optionally a negative flowframe, and a flowset of samples.
#' It then unmixes this data and outputs a new fcs file.  Various unmixing methods are available.
#'
#' This function is the wrapper function for unmix_ff() and controlData()
#'
#' @param fs A flowSet of files to unmix
#' @param cs A flowset of single colour controls
#' @param unstained A flowframe of the negative control (optional)
#' @param unmixMethod Choose an inbuilt unmixing method: lsfit (the default), ginv, qr.solve ,lm.fit, crosspod, nnls, baselm
#' @param multiplier Most of these methods produce 0-1 values.  Multiply these to make them more palatable to your eye (default=10000)
#' @param spectrum TRUE uses flowSpectrum to output spectral plots
#' @return fcs file and optionally spectrum images
#' @export
#'
flowUnmix<-function(fs, cs, unstained=NULL, unmixMethod = "lsfit", multiplier=10000, spectrum=FALSE){
  if(spectrum==TRUE){
    flowSpectrum::spectralplot(cs, save = TRUE)
  }
  if(unmixMethod=="all"){
    list<-c("lsfit","ginv","qr.solve","lm.fit","crosspod","nnls","baselm")
    for (item in list) {
      fsApply(fs, function(x)unmix_ff(fs=x,control=controlData(cs,unstained), unmixMethod = item, multiplier=multiplier))
    }
  } else {
    fsApply(fs, function(x)unmix_ff(fs=x,control=controlData(cs,unstained), unmixMethod = unmixMethod, multiplier=multiplier))
  }
}
