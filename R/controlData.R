#' Formatting the control data
#'
#' flowUnmix requires you to input a flowset of single colour controls, optionally a negative flowframe, and a flowset of samples.
#' It then unmixes this data and outputs a new fcs file.  Various unmixing methods are available.
#'
#' controlData() creates data matrix of the control data for importing into unmix_ff()
#'
#' @param cs A flowset of single colour controls
#' @param unstained A flowframe of the negative control (optional)
#' @return fcs file
#' @export
#'
controlData <- function(cs,unstained) {
  Control_Spectrums<-fsApply(cs,each_col,median)
  Control_Spectrums<-as.data.frame(Control_Spectrums)
  Control_Spectrums<-Control_Spectrums[,-grep("FSC|SSC", names(Control_Spectrums))]
  Control_Spectrums<-Control_Spectrums[,grep("-A", names(Control_Spectrums))]

  if(!is.null(unstained)) {
    unstainedData<-exprs(unstained)
    unstainedData<-apply(unstainedData,2,median)
    unstainedData<-as.data.frame(t(unstainedData))
    unstainedData<-unstainedData[,-grep("FSC|SSC", names(unstainedData))]
    unstainedData<-unstainedData[,grep("-A", names(unstainedData))]
    Control_Spectrums2<-mapply('-', Control_Spectrums, unstainedData, SIMPLIFY = TRUE)
    Control_Spectrums3<-data.frame(Control_Spectrums2)
    rownames(Control_Spectrums3)<-rownames(Control_Spectrums)
    colnames(Control_Spectrums3)<-colnames(Control_Spectrums)
    Control_Spectrums<-Control_Spectrums3
  } else{
    Control_Spectrums<-Control_Spectrums
  }
}
