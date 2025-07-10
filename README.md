# survival-analysis-plots-with-tables-risk-hearing-loss
R codes create final deliverable that shows plots and tables survival rates for types of hearing loss

Background: A Kaplan-Meier (KM) curve - a graphical representation of survival rates over time, showing the probability of surviving to a certain point. Plots time on the x-axis and the survival rate on the y-axis. 
__________________________________________________________
Program for survival analysis. Modifies the KM plots by extracting the data behind the plots made from the KM (Kaplan-Meier) plot function, and then using that data to make ggplots which then allows user to modify plot background,  legends, data labels, adjust margins, add legend labels at end of curves if needed, change y-axis value limit to the same value for all plots, add plot grid-line if needed, add minor and major tick marks to x-axis line, adjust width and length of tick marks, and other plot features. See codes.
__________________________________
Types of hearing loss data sets analyzed for KM Plot final deliverable:

#TED - Thyroid Eye Disease hearing loss
#GD (without TED)  - refers to Graves' disease (GD) hearing loss
#GP - refers to the general population with the risk for hearing loss without
#TED and GD.

-The three KM plots in the Final Deliverable attached are for the different types of hearing loss listed below:

1. Figure 1 (plot_combined.co_cohort data set): Plot of Kaplan Meier curve with the risk of hearing loss (composite outcome) among TED, GD (without TED), and general population (without TED/GD)

2. Figure 2 (plot_ted.co.gender data set): Plot of Kaplan Meier curve with the risk of sensorineural hearing loss (without age-related hearing loss) among TED patients by gender

3. Figure 3 (plot_ted.co.gender data set) : Plot of Kaplan Meier curve with the risk of sensorineural hearing loss (without age-related hearing loss) among TED patients by baseline age category

 

 


