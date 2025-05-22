# flowUnmix
An R package to unmix flow cytometry data

Author: Christopher Hall, Babraham Institute, UK

https://www.babraham.ac.uk/science-services/flow-cytometry


```
devtools::install_github("hally166/flowUnmix")
library(flowUnmix)
??flowUnmix
```

Basic usage:

```
#load data
controls_data<-read.flowSet(list.files('C:/Spectral unmixing Aurora/Reference Group', full.names=TRUE))
files2unmix<-read.flowSet(list.files("C:/Spectral unmixing Aurora/Group_001", full.names = TRUE, pattern = ".fcs"))
negative_control<-read.FCS('C:/Spectral unmixing Aurora/Unstained (Beads).fcs')

#run unmixing
flowUnmix(fs=files2unmix, cs=Control_Spectrums, unstainedctrl=negative_control, guessPop = TRUE, popCheck = TRUE)
```
```guessPop``` needs an unstained control.

```popCheck``` displays the spectral signature of the control.

If you want autofluorescence subtraction you can add an extra signature to the controls by gating the autofluorescent cells and passing them as a control.
