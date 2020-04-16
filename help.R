

getHelpTxt <- function(){
  paste0( 
     "<h2>Parameters</h2>",
     "All parameters can be specified in the GUI but you can also give them default values in the URL <br>",
     "be adding their names and values at the end, e.g. www.YOURURL.com?HR=0.5&timePred=10,21&useDropouts=No<br><br>",
     "<i>numPatients</i>: Number of patients in trial<br>",
     "<i>studyDuration</i>: Length of trial in months<br>", 
     "<i>recDuration</i>: Length of recruitment period in months<br>", 
     "<i>k</i>: Recruitment shape parameter<br>", 
     "<i>r</i>: Randomization imbalance (number experimental arm/number in control arm)<br>", 
     "<i>HR</i>: Hazard ratio (0,1)<br>", 
     "<i>ctrlMedian</i>: Control median in months<br>", 
     "<i>shape</i>: Weibull shape parameter<br>",
     "<i>ctrlM0</i>: Control median before change point<br>", 
     "<i>chP0</i>: Time of change point in months <br>", 
     "<i>HR0</i>: HR before change point<br>", 
     "<i>ctrlM1</i>: Control median after change point<br>", 
     "<i>HR1</i>: HR after change point<br>", 
     "<i>shape1</i>: Weibull shape parameter before and after change-point<br>", 
     "<i>dropoutPropCtrl</i>: Probability of patient dropping out in control arm (in absence of event)<br>",
     "<i>dropoutPropExp</i>: Probability of patient dropping out from experimental arm (in absence of event)<br>", 
     "<i>alpha</i>: Significance level using in critical number of events computation <br>", 
     "<i>power</i>: Power used in critical number of events computation <br>",
     "<i>timePred</i>: Time points in month when predicting number of events, comma-separated<br>", 
     "<i>eventPred</i>: Number of events when predicting time in months, comma-separated<br>", 
     "<i>useDropouts</i>: If dropouts should be used (Yes/No)<br>", 
     "<i>twoSided</i>: If two sided test should be used (Yes/No)<br>" )
}