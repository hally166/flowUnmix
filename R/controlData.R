#' Formatting the control data
#'
#' flowUnmix requires you to input a flowset of single colour controls, optionally a negative flowframe, and a flowset of samples.
#' It then unmixes this data and outputs a new fcs file.  Various unmixing methods are available.
#'
#' controlData() creates data matrix of the control data for importing into unmix_ff()
#'
#' @param cs A flowset of single colour controls
#' @param unstainedctrl A flowframe of the negative control (optional)
#' @param guessPop Should flowUnmix try to select the positive population of the controls
#' @param popCheck Print chosen spectra
#' @return control data matrix
#' @author Christopher Hall, Babraham Institute
#' @seealso \code{\link[flowUnmix]{flowUnmix}},
#' \code{\link[flowUnmix]{unmix_ff}},
#' \code{\link[flowUnmix]{popchooser}}
#' @examples
#'
#' ## load fcs files using flowCore()
#' bfFiles<-list.files("C:/Users/Chris/FCS_files/", full.names = TRUE)
#' controls_data<-read.flowSet(bfFiles[c(1:12)])
#' negative_control<-read.FCS(bfFiles[14])
#'
#' #run controlData(), find positive events, and print spectra
#' Control_Spectrums<-fsApply(controls_data,function(x)popChooser(singlePositives=x,negative=negative_control, guessPop= TRUE, popCheck=TRUE))
#' @export
#'
controlData <- function(cs, unstainedctrl, guessPop, popCheck) {
  if (guessPop==TRUE) {
    Control_Spectrums<-fsApply(cs,function(x)popChooser(singlePositives=x,negative=unstainedctrl, popCheck=popCheck))
  } else {
    Control_Spectrums<-fsApply(cs,each_col,median)
  }
  Control_Spectrums<-as.data.frame(Control_Spectrums)
  Control_Spectrums<-Control_Spectrums[,grep("-A", names(Control_Spectrums))]
  Control_Spectrums<-Control_Spectrums[,grep("SC|SS|FS", names(Control_Spectrums), invert=TRUE)]

  if(!is.null(unstainedctrl)) {
    unstainedData<-exprs(unstainedctrl)
    unstainedData<-apply(unstainedData,2,median)
    unstainedData<-as.data.frame(t(unstainedData))
    unstainedData<-unstainedData[,-grep("SC|SS|FS", names(unstainedData))]
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

