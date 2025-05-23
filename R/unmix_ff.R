#' core unmixing functionality
#'
#' flowUnmix requires you to input a flowset of single colour controls, optionally a negative flowframe, and a flowset of samples.
#' It then unmixes this data and outputs a new fcs file.  Various unmixing methods are available.
#'
#' unmix_ff() is the main looping function that unmixes the data
#'
#' @param fs A flowSet of files to unmix
#' @param control Matrix of the control data
#' @param negative A flowframe of the negative control (optional)
#' @param unstainedsamp A flowframe of the negative control of the sample if different from unstainedctrl (optional)
#' @param unmixMethod Choose an inbuilt unmixing method: lsfit (the default), ginv, qr.solve ,lm.fit, crosspod, nnls, baselm
#' @return unmixed fcs file
#' @author Christopher Hall, Babraham Institute
#' @seealso \code{\link[flowUnmix]{controlData}},
#' \code{\link[flowUnmix]{flowUnmix}},
#' \code{\link[flowUnmix]{popchooser}}
#' @export
#'
unmix_ff <- function(fs, control, unstainedsamp, unmixMethod) {
  control<-data.frame(t(apply(control,1, function(x)((x-min(x))/(max(x)-min(x))))),check.names = FALSE)
  expresionData<-exprs(fs)
  expresionData<-as.data.frame(expresionData,check.names = FALSE)
  expresionData<-expresionData[,-grep("SC|SS|FS", names(expresionData))]
  expresionData<-expresionData[,grep("-A", names(expresionData))]
  control<-control[,names(expresionData)] #Reorder data to match the expression data - vital if using exported data from FlowJo
  if(!is.null(unstainedsamp)) {
    unstainedData<-exprs(unstainedsamp)
    unstainedData<-apply(unstainedData,2,median)
    unstainedData<-as.data.frame(t(unstainedData),check.names = FALSE)
    unstainedData<-unstainedData[,-grep("SC|SS|FS", names(unstainedData))]
    unstainedData<-unstainedData[,grep("-A", names(unstainedData))]
    unstained_control<-mapply('-', expresionData, unstainedData, SIMPLIFY = TRUE)
    unstained_control2<-data.frame(unstained_control,check.names = FALSE)
    rownames(unstained_control2)<-rownames(expresionData)
    colnames(unstained_control2)<-colnames(expresionData)
    expresionData<-unstained_control2
  } else {
    expresionData<-expresionData
  }
  if(unmixMethod=="lsfit") {
  ls_corr <- lsfit(x = t(control), y = t(expresionData), intercept = FALSE)
  unmixResult <- t(ls_corr$coefficients)
  }
  else if(unmixMethod=="ginv"){
  pseudoinverse<-ginv(as.matrix(t(control)))
  colnames(pseudoinverse)<-colnames(control)
  rownames(pseudoinverse)<-rownames(control)
  unmixResult<-t(apply(expresionData,1,function(x)colSums(t(pseudoinverse)*x)))
  }
  else if(unmixMethod=="qr.solve"){
  qrs<-qr.solve(as.matrix(t(control)),as.matrix(t(expresionData)))
  unmixResult<-as.data.frame(t(qrs))
  unmixResult<-as.matrix(unmixResult)
  }
  else if(unmixMethod=="lm.fit"){
  lmfit<-.lm.fit(as.matrix(t(control)),as.matrix(t(expresionData)))
  unmixResult <- t(lmfit$coefficients)
  unmixResult<-data.frame(unmixResult,check.names = FALSE)
  colnames(unmixResult)<-rownames(control)
  unmixResult<-as.matrix(unmixResult)
  }
  else if(unmixMethod=="crosspod"){
  crspod<-solve(crossprod(as.matrix(t(control)))) %*% crossprod(as.matrix(t(control)),as.matrix(t(expresionData)))
  crspod<-data.frame(t(crspod),check.names = FALSE)
  colnames(crspod)<-rownames(control)
  unmixResult<-as.matrix(crspod)
  }
  else if(unmixMethod=="nnls"){
  expresionData2<-as.matrix(expresionData)
  nnlsunmixed<-apply(expresionData2, 1, function(x)nnls(as.matrix(t(control)), x)$x)
  nnlsunmixed<-data.frame(t(nnlsunmixed),check.names = FALSE)
  colnames(nnlsunmixed)<-rownames(control)
  unmixResult<-as.matrix(nnlsunmixed)
  }
  else if(unmixMethod=="baselm"){
  lmbase<-apply(expresionData, 1, function(x)lm (x ~ t(control))$coefficients)
  lmbase<-data.frame(t(lmbase),check.names = FALSE)
  lmbase<-lmbase[,2:ncol(lmbase)]
  colnames(lmbase)<-rownames(control)
  unmixResult<-as.matrix(lmbase)
  }
  new_fr<-fr_append_cols(fs, unmixResult)
  keyword(new_fr)$`FIL`<-paste(keyword(new_fr)$`FIL`,unmixMethod)
  keyword(new_fr)$GUID<-paste(keyword(new_fr)$GUID,unmixMethod)
  keyword(new_fr)$TUBENAME<-paste(keyword(new_fr)$TUBENAME,unmixMethod)
  write.FCS(new_fr, filename=paste0(keyword(new_fr)$GUID,".fcs"))
}
