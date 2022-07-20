#' Selecting the positive events in the unmixing controls
#'
#' flowUnmix requires you to input a flowset of single colour controls, optionally a negative flowframe, and a flowset of samples.
#' It then unmixes this data and outputs a new fcs file.  Various unmixing methods are available.
#'
#' popChooser() tries to guess the positive population and optionally prints the normalised spectrum of the chosen population.
#'
#' @param singlePositives A flowset of single colour controls
#' @param negative A flowframe of the negative control
#' @param popCheck Prints to R a normalised spectrum of the chosen events
#' @return median fluorescence intensity of each parameter in each single colour control
#' @author Christopher Hall, Babraham Institute
#' @seealso \code{\link[flowUnmix]{controlData}},
#' \code{\link[flowUnmix]{unmix_ff}},
#' \code{\link[flowUnmix]{flowUnmix}}
#' @export
#'

popChooser <- function(singlePositives,negative,popCheck){
  control_df<-exprs(negative)
  control_df<-as.data.frame(control_df)
  control_df<-control_df[,-grep("SC|SS|FS", names(control_df))]
  control_df<-control_df[,grep("-A", names(control_df))]
  control_df<-control_df[!apply(control_df, 1, function(x) {any(x > as.numeric(keyword(negative)$`$P6R`))}),]
  todelete<-(apply(control_df, 2, median))
  sample_df<-exprs(singlePositives)
  sample_df<-as.data.frame(sample_df)
  sample_df<-sample_df[,-grep("SC|SS|FS", names(sample_df))]
  sample_df<-sample_df[,grep("-A", names(sample_df))]
  sample_df<-sample_df[!apply(sample_df, 1, function(x) {any(x > as.numeric(keyword(singlePositives)$`$P6R`))}),]

  sample_df<-sweep(sample_df, 2, todelete, "-")
  means<-apply(sample_df, 2, mean)

  tosort<-sort(means,decreasing = TRUE)[1]
  sample_pos<- sample_df[order(sample_df[,names(tosort)],decreasing = TRUE),]
  sample_pos <- sample_pos[1:200,]
  control_values<-apply(sample_pos, 2, median)

  if(popCheck == TRUE){
    medians<-apply(sample_pos,2,median) #Calculate the medians and factorise the data
    df<-data.frame(medians/max(medians))
    df[df<0] <- 0.01
    df<-cbind(rownames(df),df)
    df$`rownames(df)`<-factor(df$`rownames(df)`, levels = df$`rownames(df)`)
    if(singlePositives@description$`$CYT`=="Bigfoot"){
      flowfile_colnames<-grep(pattern = "-A", unique(markernames(singlePositives)), value=TRUE)
      flowfile_colnames<-grep(pattern = "Spectral ", flowfile_colnames, invert = TRUE, value=TRUE)
      df[,'rownames(df)']<-flowfile_colnames
      df<-df[,order(names(df))]
    }
    p<- ggplot(data=df, aes(x=df[,'rownames(df)'], y=df[,'medians.max.medians.'], group=1)) +
          geom_line(size=1)+
          geom_point(size=2)+
          theme_bw() +
          theme(axis.text.x = element_text(colour = "black",angle = 90, vjust = 0.5, hjust=1))+
          ggtitle(identifier(singlePositives)) +
          xlab("Detector") +
          ylab("Normalized Intensity")
    print(p)
  }
  return(control_values)
}
