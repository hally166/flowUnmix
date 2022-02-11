# flowUnmix
An R package to unmix flow cytometry data

This is an early beta (Feb 2022), I will add documentation later, but there are help files in R.


Basic usage:

```
#load data
controls_data<-read.flowSet(list.files('C:/Spectral unmixing Aurora/Reference Group', full.names=TRUE))
files2unmix<-read.flowSet(list.files("C:/Spectral unmixing Aurora/Group_001", full.names = TRUE, pattern = ".fcs"))
negative_control<-read.FCS('C:/Spectral unmixing Aurora/Unstained (Beads).fcs')

#run unmixing
unmix_ff(fs=files2unmix, cs=Control_Spectrums, unstained=negative_control)
```
