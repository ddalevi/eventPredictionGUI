

getHelpTxt <- function(){
  paste0( 
     "<h2>Help</h2>More information about the functionality can be found in the <a href='http://ec2-18-185-18-135.eu-central-1.compute.amazonaws.com/predict_from_parameters.pdf', target='_blank'>vignette</a>",
     "<h2>Parameters</h2>",
     "All parameters can be specified in the GUI but you can also give them default values in the URL <br>",
     "be adding their names and values at the end, e.g. www.YOURURL.com?HR=0.5&timePred=10,21&useDropouts=No<br><br>",
     "<b>numPatients</b>: Number of patients in trial<br>",
     "<b>studyDuration</b>: Length of trial in months<br>", 
     "<b>recDuration</b>: Length of recruitment period in months<br>", 
     "<b>k</b>: Recruitment shape parameter<br>", 
     "<b>r</b>: Randomization imbalance (number experimental arm/number in control arm)<br>", 
     "<b>HR</b>: Hazard ratio (0,1)<br>", 
     "<b>ctrlMedian</b>: Control median in months<br>", 
     "<b>shape</b>: Weibull shape parameter<br>",
     "<b>ctrlM0</b>: Control median before change point<br>", 
     "<b>chP0</b>: Time of change point in months <br>", 
     "<b>HR0</b>: HR before change point<br>", 
     "<b>ctrlM1</b>: Control median after change point<br>", 
     "<b>HR1</b>: HR after change point<br>", 
     "<b>shape1</b>: Weibull shape parameter before and after change-point<br>", 
     "<b>dropoutPropCtrl</b>: Probability of patient dropping out in control arm (in absence of event)<br>",
     "<b>dropoutPropExp</b>: Probability of patient dropping out from experimental arm (in absence of event)<br>", 
     "<b>alpha</b>: Significance level using in critical number of events computation <br>", 
     "<b>power</b>: Power used in critical number of events computation <br>",
     "<b>timePred</b>: Time points in month when predicting number of events, comma-separated<br>", 
     "<b>eventPred</b>: Number of events when predicting time in months, comma-separated<br>", 
     "<b>useDropouts</b>: If dropouts should be used (Yes/No)<br>", 
     "<b>twoSided</b>: If two sided test should be used (Yes/No)<br>" )
}