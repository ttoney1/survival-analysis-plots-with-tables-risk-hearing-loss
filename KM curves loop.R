#OBJECTIVE: -Survival Analysis -  Create loops to make KM plots and tables
-#This is the loop version where you create different loops to loop over 
#several different types of data sets to create multiple plots 
  
#BACKGROUND - The way the code is able to modify the KM plots was by extracting 
#the data behind the plots made from the KM (Kaplan-Meier) plot function, and 
#then using that data to make ggplots which you then allowed you to modify
#plot features. 
#A Kaplan-Meier curve is a graphical representation of 
#survival rates over time, showing the probability of surviving to a certain 
#point. It plots time on the x-axis and the survival rate on the y-axis. 

# Notes:
# TED_CO_KM	-	Fig5, Fig8 and Fig11 (co_year, co_01 by gender, age_grp, insurance)
# GD_CO_KM	-	Fig6, Fig9 and Fig12 (co_year, co_01 by gender, age_grp, insurance)
# GP_CO_KM	-	Fig7, Fig10 and Fig13(co_year, co_01 by gender, age_grp, insurance)
# COMBINED_CO_KM	-	Fig1 (co_year, co_01 by cohort)
# TED_SH_KM	-Fig3, Fig4 (sh_year, sh_01 by gender, age_grp)
# COMBINED_SH_KM - 	Fig2 (sh_year, sh_01 by cohort)
  
#NOTES ON FINAL DELIVERABLE: Types of hearing loss data sets analyzed:
#TED  = (Thyroid Eye Disease) hearing loss 
#GD (without TED)  = likely refers to Graves' disease (GD) hearing loss
#GP - refers to the general populaiton with the risk for hearing loss without
#TED and GD.
#The three KM plots in the Final Deliverable that you created with this code:
#1. Figure 1 (plot_combined.co_cohort data set): Plot of Kaplan Meier curve with the risk of hearing loss (composite outcome) among TED, GD (without TED), and general population (without TED/GD)
#2. Figure 2 (plot_ted.co.gender data set): Plot of Kaplan Meier curve with the risk of sensorineural hearing loss (without age-related hearing loss) among TED patients by gender
#3. Figure 3 (plot_ted.co.gender data set) : Plot of Kaplan Meier curve with the risk of sensorineural hearing loss (without age-related hearing loss) among TED patients by baseline age category
#-------------------------------------------------------------------------

#PACKAGES  

#------------------------------------------------------------------

#REQUIRED PACKAGES
library(gtsummary) # to make survival table - tbl_survfit() - for making table for under plot!
library(qpdf) # to combine pdf's
library(ggplot2)
#install.packages("markdown")
library(markdown)
library(officer) # to use fp_border()
#install.packages("ggsurvfit")
library(ggsurvfit)  # to add risk table below plot if needed
library(flextable)
library(mgsub) # allows multiple patterns
library(knitr) # has kable
#install.packages("kableExtra")
library(kableExtra) # has column_spec
library(tidyr)
library(survival)
library(survminer)
library(ggtext) # esp. needed for geom_richtext()
library(ggthemes)
library(knitr)
library(purrr) # to loop over use map_dbl to list column and extract the first element
library(dplyr)
library(tibble)
library(gghighlight)
library(ggrepel)
library(devtools)
#install_github('sinhrks/ggfortify')
# if (!require("remotes")) install.packages("remotes")
# remotes::install_github('sinhrks/ggfortify')
library(ggfortify) # IMPORTANT to use autoplot!
#remotes::install_github('Mikata-Project/ggthemr')
library(ggthemr) 
#remotes::install_github('vankesteren/firatheme')
library(firatheme)
library(plotly)
library(scales) #to use label_percent
library(extrafont)
library(gridExtra)
library(grid)
library(stringr) #to use str_wrap
library(RColorBrewer)
library(wesanderson)#use specific color palette
library(viridis) #currently the most robust color palette
library(cowplot)
#remotes::install_github("hrbrmstr/hrbrthemes")
library(hrbrthemes)
#install.packages("ggprism")
library(ggprism)


# devtools::install_github("zabore/ezfun")
#ezfun::set_ccf_palette("contrast")

#-------------

# CONNECT TO DATABRICKS THROUGH SECRETS

#Connect to Databricks so that user can access OMOP CDM data
#Set up connection Details through Secrets
#secrets::login(TRUE)

#Only run below once
# secrets::set_personal_secret(s_type = "databricks",
#                              shard  = "edl-rwd",
#                              env    = "prod",
#                              token  = "enter your Databricks token",
#                              override = TRUE)


#Note If you get an error similar ‘Request failed...', then un-comment the 
#below code and run. Enter your Amgen password when prompted
#secrets::login(reset_ad_credentials = TRUE)

# Retrieve Secrets - connect to Notebook atlas-dashboard-sqlwh-endpoint
s_atl<-secrets::get_secret("secret/cfor/public/databricks/rwd/prod/atlas-dashboard-sqlwh-endpoint")

#Connect to Databricks
connectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "spark", user = "token",
  connectionString = s_atl$.Connection_String,
  password  = s_atl$pwd,
  pathToDriver = s_atl$Pathtodriver,
  port = s_atl$Port
)

# Establish the connection to Databricks based on above details
#If you get error 'Connection propery 'password' is NULL' after running below,
# then try changing case above code from s_atl$pwd to s_atl$Pwd or any other
# cases to match column name cases inside s_atl object
connection <- DatabaseConnector::connect(connectionDetails = connectionDetails)
on.exit(DatabaseConnector::disconnect(connection = connection))

#-----------------------------------------------------------------

#          LOOP  - IMPORT DATA FROM DATABRICKS

#create group for first loop -- set to lower case. Add tables as needed if
# for p24_rd_021 schema 
p24_rd_021.tables <- tolower(c('TED_CO_KM','GD_CO_KM','GP_CO_KM','COMBINED_CO_KM',
               'TED_SH_KM','COMBINED_SH_KM'))

#apply loop through importing data tables from DataBricks schema p24_rd_021.
#cannot use assign operator (<-) with paste in for loops. Use 'assign' instead.
#MAY TAKE ABOUT 10 MINUTES TO RUN
for (i in p24_rd_021.tables)
{
 assign(paste0(i,"_db"),DBI::dbGetQuery(connection, paste0("SELECT * FROM
                                          p24_rd_021.", i ))
)
}


#--------------------------------------------------------------

# LOOPS - GENERATE INITIAL KM PLOT TO LATER EXTRACT PROBABILTIES FROM THEM 
# will create different loops for grouping similar data frames that have same column
#such as gender

# Notes:
# TED_CO_KM	-	Fig5, Fig8 and Fig11 (co_year, co_01 by gender, age, insurance)
# GD_CO_KM	-	Fig6, Fig9 and Fig12 (co_year, co_01 by gender, age, insurance)
# GP_CO_KM	-	Fig7, Fig10 and Fig13(co_year, co_01 by gender, age, insurance)
# COMBINED_CO_KM	-	Fig1 (co_year, co_01 by cohort)
# TED_SH_KM	-Fig3, Fig4 (sh_year, sh_01 by gender, age)
# COMBINED_SH_KM - 	Fig2 (sh_year, sh_01 by cohort)

#---
# Note data from ap.ted.co.gender - should have 3,918 plot points --UNLESS new data

# un-comment to clear list if needed
# rm(i, h, j)
 rm(list = ls(, pattern = "tbl_x")) # Removes all objects whose name contain pattern 



#--
# LOOP -- BASED ON GENDER, INSURANCE, ETC ONE PER LOOP)

# standardize year and event column names across data sets
ted_sh_km_db <- ted_sh_km_db %>% mutate(year = sh_year, event = sh_01)
ted_co_km_db <- ted_co_km_db %>% mutate(year = co_year, event = co_01)
gp_co_km_db <- gp_co_km_db %>% mutate(year = co_year, event = co_01)
gd_co_km_db <-  gd_co_km_db %>% mutate(year = co_year, event = co_01)
combined_co_km_db <-  combined_co_km_db  %>% mutate(year = co_year, event = co_01)
combined_sh_km_db <-  combined_sh_km_db  %>% mutate(year = sh_year, event = sh_01)



#gender & age group
# table group - must create list AFTER adding above new columns
demog.tables<- list('ted_co' = ted_co_km_db, 'gp_co' = gp_co_km_db, 
                     'gd_co' = gd_co_km_db, 'ted_sh' = ted_sh_km_db)

#insurance group
demog2.tables<- list('ted_co' = ted_co_km_db, 'gp_co' = gp_co_km_db, 
                     'gd_co' = gd_co_km_db)

#cohort group
cohort.tables<- list('combined_co' = combined_co_km_db, 
                     'combined_sh' = combined_sh_km_db)

#---------------------------------------------------------
#Make a column that contains the max year by group rounded to nearest whole number
#so that later tbl_survfit function to accurately capture it.

#             Apply loops

# insurance - apply loop for insurance before gender & age so that data  keeps all columns

num.ins <- 0
for (i in demog2.tables)
{
  num.ins <- num.ins + 1
  
  assign(paste0(names(demog2.tables[num.ins]),"_km_db"),
         i %>%
           group_by(insurance) %>%
           mutate(year_insurance = ifelse(year %in% max(year), round(year), year)
           )
  )}

#update insurance group
demog2.tables<- list('ted_co' = ted_co_km_db, 'gp_co' = gp_co_km_db, 
                     'gd_co' = gd_co_km_db)



#----
#gender
num.gen <- 0
for (i in demog.tables)
{
  num.gen <- num.gen + 1
  
  assign(paste0(names(demog.tables[num.gen]),"_km_db"),
         i %>%
           group_by(gender) %>%
           mutate(year_gender = ifelse(year %in% max(year), round(year), year)
           )
)}

# update gender & age group
# table group - must create list AFTER adding above new columns
demog.tables<- list('ted_co' = ted_co_km_db, 'gp_co' = gp_co_km_db, 
                    'gd_co' = gd_co_km_db, 'ted_sh' = ted_sh_km_db)

#-----
# age group 

num.age <- 0
for (i in demog.tables)
{
  num.age <- num.age + 1
  
  assign(paste0(names(demog.tables[num.age]),"_km_db"),
         i %>%
           group_by(age_grp) %>%
           mutate(year_age_grp = ifelse(year %in% max(year), round(year), year)
           )
  )}

# update gender & age group
demog.tables<- list('ted_co' = ted_co_km_db, 'gp_co' = gp_co_km_db, 
                    'gd_co' = gd_co_km_db, 'ted_sh' = ted_sh_km_db)


#-----
#---

# cohort

num.cohort <- 0
for (i in cohort.tables)
{
  num.cohort<- num.cohort + 1
  
  assign(paste0(names(cohort.tables[num.cohort]),"_km_db"),
         i %>%
           group_by(cohort) %>%
           mutate(year_cohort = ifelse(year %in% max(year), round(year), year)
           )
  )}

#update cohort group
cohort.tables<- list('combined_co' = combined_co_km_db, 
                     'combined_sh' = combined_sh_km_db)

#-----------------------------------------------------
#             Apply loop for gender to make auto-plots- 

# Best not to use any nested loops because inner loop tend to use results of last loop
# instead of iterating.
# Below  geom_label_repel and theme are optional code
#for (i in gender.tables[1:length(gender.tables)])
num.gen <- 0
for (i in demog.tables)
{
  num.gen <- num.gen + 1
  
  assign(paste0("ap.", names(demog.tables[num.gen]),".gender"),
         autoplot(survfit(Surv(year, event) ~ gender
                          , data = i), fun = 'event', legendLabs = FALSE) +
           geom_label_repel(data = . %>% group_by(strata) %>% summarise(x = mean(time), y = mean(surv)), 
                            aes(x = x, y = y, label = strata, color = strata)) +
           theme(legend.position = 'none')
  )
}


# --- 
#             Apply loop for age group to make auto-plots- 
num.age <- 0
for (i in demog.tables)
{
  num.age <- num.age + 1
  
  assign(paste0("ap.", names(demog.tables[num.age]),".age"),
         autoplot(survfit(Surv(year, event) ~ age_grp
                          , data = i), fun = 'event', legendLabs = FALSE) +
           geom_label_repel(data = . %>% group_by(strata) %>% summarise(x = mean(time), y = mean(surv)), 
                            aes(x = x, y = y, label = strata, color = strata)) +
           theme(legend.position = 'none')
  )
}


# --- 
#             Apply loop for insurance group to make autoplots- 
num.ins <- 0
for (i in demog2.tables)
{
  num.ins <- num.ins + 1
  
  assign(paste0("ap.", names(demog2.tables[num.ins]),".insurance"),
         autoplot(survfit(Surv(year, event) ~ insurance
                          ,data = i), fun = 'event', legendLabs = FALSE) 
  )
}


# --- 
#             Apply loop for cohort group to make auto-plots
num.cohort <- 0
for (i in cohort.tables)
{
  num.cohort<- num.cohort + 1
  
  assign(paste0("ap.", names(cohort.tables[num.cohort]),".cohort"),
         autoplot(survfit(Surv(year, event) ~ cohort
                          , data = i), fun = 'event', legendLabs = FALSE) 
  )
}

#----------------------------------------------------------

# TEST - DISPALY AUTO PLOTS - OPTIONAL

# un-comment to check and display any of the plots that resulted from above chunk
#ap.combined_co.cohort
#ap.combined_sh.cohort
# ap.ted_co.gender
# ap.gd_co.age
# ap.ted_sh.age
# ap.ted_co.insurance

# un-comment to view interactive plot
 # ggplotly(ap.ted_co.gender)


#------------------------------------------------------------------------

# LOOPS - EXTRACT PROBABILTIES FROM INITIAL KM PLOTS


#CREATE GROUP FOR PLOTS. -- Must create names for plots in list. 
km.ap.plot.list <- list(
  cmbnd.co =  ap.combined_co.cohort, cmbnd.sh = ap.combined_sh.cohort, 
  gd.age = ap.gd_co.age, gd.sex = ap.gd_co.gender, gd.ins = ap.gd_co.insurance,
  gp.age= ap.gp_co.age, gp.sex = ap.gp_co.gender, gp.ins = ap.gp_co.insurance,
  ted.co.age = ap.ted_co.age, ted.co.sex = ap.ted_co.gender, ted.co.ins = ap.ted_co.insurance, 
     ted.sh.age = ap.ted_sh.age, ted.sh.sex = ap.ted_sh.gender)


# LOOP - EXTRACT INTERMEDIATE DATA BEHIND PLOTS  
#ted.fig.5.df<- ted.fig.5.ap$data # sample code without loop
#ap.km.plots[['gd.age']][['data']] # view one of the data sets in group
#ap.km.plots[[1]][['data']] # view data
#names(ap.km.plots)[1] # view first name
#Apply loop
 
#  for (i in ap.km.plots)
# {
#   for (j in names(ap.km.plots)) {
#     
#   assign(paste0("df_", j), ap.km.plots[[j]][['data']]
#   )
#   }}
#  
# Commented out above chunk to avoid to using nested loops since creates issues
 num.ap.plots <- 0
 for (i in km.ap.plot.list)
 {
     
   num.ap.plots <- num.ap.plots + 1
   
     assign(paste0("df_", names(km.ap.plot.list[num.ap.plots])
                   ), km.ap.plot.list[[num.ap.plots]][['data']]
     )
   }
 

 
#-------------------------------------------------------------------------
# test -OPTIONAL - un-comment to test - must use quotes below to view data
#test1 <- km.ap.plot.list[['ted.co.sex']][['data']]

 # un-commment to view a data frame from last loop
 #df_gd.ins
#--------------------------------------------------------------------------------

 # LOOP TO ADD COLUMNS 
 
#Add a column to data for legend labels that will go over curves 
#inside chart
 
#One data frame without loop: 
#  df_ted.co.sex <- df_ted.co.sex %>%
 #   group_by(strata) %>%
 #   mutate(strata_label = ifelse(time %in% max(time, na.rm =T) &
 #                                  surv %in% max(surv, na.rm =T),
 #                                as.character(strata), NA)) %>%  ungroup()
 

#Make group -- order by figure number
km.data.frame.list <- list(cmbnd.co = df_cmbnd.co, cmbnd.sh = df_cmbnd.sh,
                            ted.sh.sex = df_ted.sh.sex,  ted.sh.age = df_ted.sh.age,  
                            ted.co.sex = df_ted.co.sex, gd.sex= df_gd.sex, gp.sex = df_gp.sex,
                            ted.co.age = df_ted.co.age,  gd.age = df_gd.age, gp.age = df_gp.age,
                            ted.co.ins = df_ted.co.ins,  gd.ins = df_gd.ins, gp.ins = df_gp.ins
 ) 
 
#un-comment to view 
# names(km.data.frame.list[3])
# names(km.data.frame.list)
# km.data.frame.list[3]
# 
# df_ted.co.sex_backup <- df_ted.co.sex

# Apply loop to add column
num.new.colm <- 0
for (i in km.data.frame.list)
 {
   num.new.colm<- num.new.colm + 1
   
  # assign(paste0("de_", names(km.data.frame.list[num.new.colm])), # to make new tables
   assign(paste0("df_", names(km.data.frame.list[num.new.colm])), # to use same tables
                 
          i %>%
    group_by(strata) %>%
    mutate(strata_label = ifelse(time %in% max(time, na.rm =T) &
    surv %in% max(surv, na.rm =T),
    as.character(strata), NA)) %>%  ungroup() 
   )
 }
 
#---------------------------------------------------------------------

# MAKE PLOT USING GGPLOT BASED ON PROBABILITIES AND ADD DATA LABELS 

# make same group of dat frames again but with new  strata group column 

#Make group --order by figure number
km.data.frame.list <- list(cmbnd.co = df_cmbnd.co, cmbnd.sh = df_cmbnd.sh,
                     ted.sh.sex = df_ted.sh.sex,  ted.sh.age = df_ted.sh.age,  
                     ted.co.sex = df_ted.co.sex, gd.sex= df_gd.sex, gp.sex = df_gp.sex,
                     ted.co.age = df_ted.co.age,  gd.age = df_gd.age, gp.age = df_gp.age,
                     ted.co.ins = df_ted.co.ins,  gd.ins = df_gd.ins, gp.ins = df_gp.ins
) 

#-----
#make title group:
km.plot.titles  <- c(
  "hearing loss (composite outcome) among TED, GD (without TED), and general population (without TED/GD)",
  "sensorineural hearing loss (without age-related hearing loss) among TED, GD (without TED), and general population (without TED/GD)",
  "sensorineural hearing loss (without age-related hearing loss) among TED patients by gender", 
  "sensorineural hearing loss (without age-related hearing loss) among TED patients by baseline age category",
  "hearing loss (composite outcome) among TED patients by gender",
  "hearing loss (composite outcome) among GD (without TED) patients by gender",
  "hearing loss (composite outcome) among general population (without TED/GD) patients by gender",
  "hearing loss (composite outcome) among TED patients by age category",
  "hearing loss (composite outcome) among GD (without TED) patients by  age category",
  "hearing loss (composite outcome) among general population (without TED/GD) patients by  age category",
  "hearing loss (composite outcome) among TED patients by insurance type",
  "hearing loss (composite outcome) among GD (without TED) patients by insurance type",
  "hearing loss (composite outcome) among general population (without TED/GD) patients by insurance type"
)

#-----


# Apply loop to make initial ggplots with titles and legend labels
num.new.plt <- 0
for (i in km.data.frame.list) 
{
  num.new.plt<- num.new.plt + 1
assign(paste0("p_", names(km.data.frame.list[num.new.plt])),
  ggplot(
    #width = 8, height = 6,
    #width = 7, height = 3,
    width = 12,
    i, aes(x = time, y =surv, group = strata, color = strata,
           label = strata_label
    )) +
    geom_richtext( 
     # data = i # already named data above, so commented out
     # ,aes(label = strata_label), # already input label above, so commented out
      #color = "black", # un-comment to change text color
      size = 5, # changes lable size
      label.size = NA, # remove borders
      # fill = 'white',
      fill = NA , # remove fill
      label.margin = unit(4, "pt"),
      label.padding = unit(3, "pt"),
      hjust = 0
      #show.legend = FALSE, 
      #,fontface = "bold"
    ) +
    coord_cartesian(clip = "off") +
    guides(color = "none") +
  # geom_smooth(method = loess) # uncomment to add shadow to curve
  geom_smooth( se = FALSE) +
  xlab("Time (years)")  +
  ylab("Probability of Having Hearing Loss") + 
  # ggtitle(str_wrap(paste0("Figure ", num.new.plt,
  #                           ": Plot of Kaplan-Meier curve with the risk of "
  #                           ,km.plot.titles[num.new.plt]) , 100)) +
    # ggtitle(str_wrap(paste0(
    #                         "Plot of Kaplan-Meier curve with the risk of "
    #                         ,km.plot.titles[num.new.plt]) , 80)) +
  theme(plot.title.position = "plot", # moves title more to left edge
   # plot.margin = margin(38, 50, 16, 32))  # with title - wide right margin (top, right, bottom, left)
  plot.margin = margin(38, 50, 16, 32))  # without title - wide right margin (top, right, bottom, left)

)
}

#display initial plots one at a time - OPTIONAL:
# p_cmbnd.co # fig 1
# p_cmbnd.sh #- make sure spelling consistent 
# p_ted.sh.sex
# p_ted.sh.age
# p_ted.co.sex # fig 5 - requested 
# p_gd.sex 
# p_gp.sex
# p_ted.co.age # fig 8 - requested
# p_gd.age
# p_gp.age # fig 10
# p_ted.co.ins
# p_gd.ins
# p_gp.ins

#----------------------------------------------------------------

#  FORMAT PLOTS 

# Must run every time to add new plots to list prior to running below loop
#Make group  - order by figure number (e.g. Figure 1, Figure 2,...)
km.plot.list <- list(cmbnd.co = p_cmbnd.co, cmbnd.sh = p_cmbnd.sh,
      ted.sh.sex = p_ted.sh.sex,  ted.sh.age = p_ted.sh.age,  
      ted.co.sex = p_ted.co.sex, gd.sex= p_gd.sex, gp.sex = p_gp.sex,
      ted.co.age = p_ted.co.age,  gd.age = p_gd.age, gp.age = p_gp.age,
      ted.co.ins = p_ted.co.ins,  gd.ins = p_gd.ins, gp.ins = p_gp.ins
) 


#-------


# Apply loop for  all additional formatting at one time

num.new.plt <- 0
for (i in km.plot.list)
{
  num.new.plt<- num.new.plt + 1
  
  assign(paste0("p2_", names(km.plot.list[num.new.plt])),
      i +
  # Update y-axis limits to greater than max percentage among all charts
    scale_y_continuous(
        limits = c(0,.25) # update number if max y-axis value changes
        ,breaks = seq(0, .25  # update number if max y-axis value changes
                      ,by = .04)
        ,labels = scales::percent) +
  # Add minor tick marks to x-axis - better to add after applying theme if have theme
    # guides(
    #     x = guide_axis(minor.ticks = TRUE)) +
    scale_x_continuous(
        breaks = seq(0, 6.5, by = 1) # controls what numbers are displayed
        #,limits = c(0, 8)
        ,limits = c(0, 6.5)  #number not displayed, but larger number will extend plot
        ,expand = c(0, 0)
       # ,minor_breaks = seq(0, 6.5, 0.05) #last number increases amount of minor tick marks

    )+
    theme(
        panel.grid.major.x = element_blank(), # Remove x grid major lines 
        panel.grid.minor.x = element_blank(),  # Remove x grid minor lines
        axis.ticks.length.x = unit(6, "pt"), # Adjust all tick length on x-axis if adjusting minor tick length later
        axis.ticks.length.y = unit(6, "pt"), # Adjust all tick length on x-axis if adjusting minor tick length later
        # adjust minor tick length -- must do after adjusting all tick mark length:
     #   axis.minor.ticks.length.x = unit(6.5, "pt" ), 
        # axis.minor.ticks.x.bottom = element_line(
        #     #color = 'red', # un-comment to change tick mark color
        #   size = 0.2 ),  # adjust tick mark width
        #axis.ticks = element_line(color = "black"), # un-comment to change all tick color
        # 
        plot.title = element_text(size=13  #Adjust plot title size
                                 # ,face = "bold" # Makes title bold
                                   ,vjust =5 # un-comment for theme_economist or to move title up/down
        ),
        # 
        plot.background = element_rect(color = 'grey78'), #outside border line color
       # panel.background = element_rect(fill = NA ,color = 'grey78'),# inside box line color
        panel.background = element_rect(fill = NA ,color = NA),# inside box line color
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), # remove gridlines 
        panel.spacing = unit(0, "lines"),
        #axis.text.x = element_text(color = "grey20", size = 20, angle = 90, hjust = .5, vjust = .5, face = "plain"),
        axis.text.x = element_text( size = 14),
        axis.text.y = element_text( size = 14), 
        axis.title.x = element_text( size = 14),
        axis.title.y = element_text( size = 14, margin = margin(t = 0, r = 13, b = 0, l = 0)),
        axis.line.x = element_line(color='grey78'
                                 #  ,size = 0.5
                                   ),
        axis.line.y = element_line(color='grey78'
                                  # ,size = 0.5
                                   )
    ) 
  
  )
}

#display plots one at a time - OPTIONAL:
# p2_cmbnd.co # fig 1
# p2_cmbnd.sh #- make sure spelling consistent 
# p2_ted.sh.sex
# p2_ted.sh.age
# p2_ted.co.sex # fig 5
# p2_gd.sex 
# p2_gp.sex
# p2_ted.co.age #fig 8 - usually has max y-axis value among all plots
# p2_gd.age
# p2_gp.age # fig 10
# p2_ted.co.ins
# p2_gd.ins
# p2_gp.ins



#--------------------------------------------------------------------------------------------

# REMOVE ANY PLOT ELEMENTS  - OPTIONAL CODE CHUNKS 


# Remove any desired layer from a specific plot (non-loop)
#p_ted.co.ins$layers #display layers
#p_ted.co.ins$layers[[2]] <- NULL # un-comment and update layer number to remove it




##############################################################################

                          # CREATE TABLES


                      #gender


#Use original raw data
num.gen <- 0
for (i in demog.tables)
{
  num.gen <- num.gen + 1
  
  assign(paste0("tbl_1.", names(demog.tables[num.gen]),".gender"), # update two texts and table name
  tbl_survfit(
    survfit(Surv(year_gender, event) ~ gender,  data = i), # update
    statistic = "{estimate}", #only display the rate and not the conf. intervals
    #estimate_fun = ~style_pvalue(., digits = 3), # this adds decimal places
    estimate_fun = ~style_percent(., symbol = TRUE, digits = 1),
    times = c(0, 1, 2, 3, 4, 5, 6),
    reverse = TRUE, # TRUE gets failure/risk rates instead of success rate
   # label_header = "Year {time # un-comment to add 'Year' prefix
    label_header = "{time}"
  ) %>%
  add_n()%>%
    as_data_frame()
  )}

#display some tables:
tbl_1.gd_co.gender
tbl_1.ted_co.gender

#----------

#age groups


#Use original raw data
num.age <- 0
for (i in demog.tables)
{
  num.age <- num.age + 1
  
  assign(paste0("tbl_1.", names(demog.tables[num.age]),".age"), # update two texts and table name
         tbl_survfit(
           survfit(Surv(year_age_grp, event) ~ age_grp,  data = i), # update
           statistic = "{estimate}", #only display the rate and not the conf. intervals
           #estimate_fun = ~style_pvalue(., digits = 3), # this adds decimal places
           estimate_fun = ~style_percent(., symbol = TRUE, digits = 1),
           times = c(0, 1, 2, 3, 4, 5, 6),
           reverse = TRUE, # TRUE gets failure/risk rates instead of success rate
           #label_header = "Year {time}" # un-comment to add 'Year' prefix
           label_header = "{time}"
         ) %>%
           add_n() %>%
           as_data_frame()
  )}
#-------------
        # insurance 

num.ins <- 0
for (i in demog2.tables)
{
  num.ins <- num.ins + 1
  assign(paste0("tbl_1.", names(demog2.tables[num.ins]),".ins"), # update two texts and table name
         tbl_survfit(
           survfit(Surv(year_insurance, event) ~ insurance,  data = i), # update
           statistic = "{estimate}", #only display the rate and not the conf. intervals
           #estimate_fun = ~style_pvalue(., digits = 3), # this adds decimal places
           estimate_fun = ~style_percent(., symbol = TRUE, digits = 1),
           times = c(0, 1, 2, 3, 4, 5, 6),
           reverse = TRUE, # TRUE gets failure/risk rates instead of success rate
          # label_header = "Year {time}" # un-comment to add 'Year' prefix
          label_header =  "{time}"
         ) %>%
           add_n() %>%
           as_data_frame()
  )}
#------------------
# cohort tables

num.cohort <- 0
for (i in cohort.tables)
{
  num.cohort<- num.cohort + 1
  assign(paste0("tbl_1.", names(cohort.tables[num.cohort]),".cohort"), # update two texts and table name
         tbl_survfit(
           survfit(Surv(year_cohort, event) ~ cohort,  data = i),  # update
           statistic = "{estimate}", #only display the rate and not the conf. intervals
           #estimate_fun = ~style_pvalue(., digits = 3), # this adds decimal places
           estimate_fun = ~style_percent(., symbol = TRUE, digits = 1),
           times = c(0, 1, 2, 3, 4, 5, 6),
           reverse = TRUE, # TRUE gets failure/risk rates instead of success rate
           #label_header = "Year {time}" # un=comment to add 'Year' prefix
           label_header = "{time}"
         ) %>%
           add_n() %>%
          as_data_frame()  # make a dataframe
  
            )}
 
#-----

#           Fix or add some columns 

#Make groups -- order by earlier db list

#gender
tbl_1.df.list.gen <- list(
  ted_co = tbl_1.ted_co.gender,
  gp_co = tbl_1.gp_co.gender,
  gd_co= tbl_1.gd_co.gender, 
  ted_sh = tbl_1.ted_sh.gender
) 

#-------

#age group
tbl_1.df.list.age <- list(
  ted_co = tbl_1.ted_co.age,
  gp_co = tbl_1.gp_co.age,
  gd_co= tbl_1.gd_co.age, 
  ted_sh = tbl_1.ted_sh.age
) 
#-----

#insurance
tbl_1.df.list.ins <- list(
  ted_co = tbl_1.ted_co.ins,  
  gp_co = tbl_1.gp_co.ins, gd_co = tbl_1.gd_co.ins
)

#cohort
tbl_1.df.list.cohort <- list(
  combined_co  = tbl_1.combined_co.cohort, combined_sh  = tbl_1.combined_sh.cohort
)


# Apply loop to add columns  - #Error for NULL object is OK

#gender
num.new.colm <- 0
for (i in tbl_1.df.list.gen)
{
  num.new.colm<- num.new.colm + 1
  
  #  assign(paste0("de_tbl1", names(tbl_1.df.list.gen[num.new.colm])), # to make new tables
  assign(paste0("tbl_1.", names(tbl_1.df.list.gen[num.new.colm]), ".gender"), # to use same tables
         i %>%
           mutate(
             `0` =str_extract(`0`, "^[^ ]+"),
             `0` = gsub('c|,|"|\\(', "", `0`) 
           )%>%
           left_join(demog.tables[[num.new.colm]]%>%  #update list name
              group_by(gender) %>% summarise(count_cases = n()), 
                     by=c("**Characteristic**"="gender")) %>%
           mutate(    
             Gender = `**Characteristic**`, 
             # completely fill gender column
             N =
               ifelse(is.na(count_cases),
                      `**N**`[`**Characteristic**` %in% 'gender'], # where N column in 'gender'
                      count_cases))
         
  )
}

#-------
#age group
num.new.colm <- 0
for (i in tbl_1.df.list.age)
{
  num.new.colm<- num.new.colm + 1
  
  #  assign(paste0("tbl_2.", names(tbl_1.df.list.age[num.new.colm])), # to make new tables
  assign(paste0("tbl_1.", names(tbl_1.df.list.age[num.new.colm]), ".age"), # to use same tables
         i %>%
           mutate(
             `0` =str_extract(`0`, "^[^ ]+"),
             `0` = gsub('c|,|"|\\(', "", `0`) 
           )%>%
           left_join(demog.tables[[num.new.colm]] %>% #update list
                     group_by(age_grp) %>% summarise(count_cases = n()), 
                     by=c("**Characteristic**"="age_grp")) %>%
           mutate(    
             `Age Group` = `**Characteristic**`, 
             # completely fill strata column
             N =
               ifelse(is.na(count_cases),
                      `**N**`[`**Characteristic**` %in% 'age_grp'], # where N column in strata
                      count_cases))
         
  )
}


#insurance

num.new.colm <- 0
for (i in tbl_1.df.list.ins)
{
  num.new.colm<- num.new.colm + 1
  
  #  assign(paste0("tbl_2.", names(tbl_1.df.list.ins[num.new.colm])), # to make new tables
  assign(paste0("tbl_1.", names(tbl_1.df.list.ins[num.new.colm]), ".ins"), # to use same tables
         i %>%
           mutate(
             `0` =str_extract(`0`, "^[^ ]+"),
             `0` = gsub('c|,|"|\\(', "", `0`) 
           )%>%
           left_join(demog2.tables[[num.new.colm]] %>% #update list to demog2!
                     group_by(insurance) %>% summarise(count_cases = n()), 
                      by=c("**Characteristic**" = "insurance")) %>%
           mutate(
              Insurance = `**Characteristic**`, 
             # completely fill strata column
             N =
               ifelse(is.na(count_cases),
                      `**N**`[`**Characteristic**` %in% 'insurance'], # where N column in strata
                      count_cases)) 
         
  )
}
#-----------

#cohort:
num.new.colm <- 0
for (i in tbl_1.df.list.cohort)
{
  num.new.colm<- num.new.colm + 1
  
  #  assign(paste0("tbl_2.", names(tbl_1.df.list.cohort[num.new.colm])), # to make new tables
  assign(paste0("tbl_1.", names(tbl_1.df.list.cohort[num.new.colm]), ".cohort"), # to use same tables
         i %>%
           mutate(
             `0` =str_extract(`0`, "^[^ ]+"),
             `0` = gsub('c|,|"|\\(', "", `0`) 
           )%>%
           left_join(cohort.tables[[num.new.colm]]%>% count(cohort), #update list to cohort list!
                     by=c("**Characteristic**" = "cohort")) %>%
           mutate(   
             Cohort = `**Characteristic**`, 
             # completely fill strata column
             N =
               ifelse(is.na(n),
                      `**N**`[`**Characteristic**` %in% 'cohort'], # where N column in strata
                      n))
         
  )
}


###############################################################


# TABLE FORMATTING - MAKE FLEX


#make group for data sets

tbl_2.df.list <- list(
  ted_co.gender = tbl_1.ted_co.gender, gp_co.gender = tbl_1.gp_co.gender,
  gd_co.gender = tbl_1.gd_co.gender, ted_sh.gender = tbl_1.ted_sh.gender,
  ted_co.age = tbl_1.ted_co.age, gp_co.age = tbl_1.gp_co.age,
  gd_co.age = tbl_1.gd_co.age, ted_sh.age = tbl_1.ted_sh.age,
  ted.co.ins = tbl_1.ted_co.ins,  
  gp.ins = tbl_1.gp_co.ins, gd.ins = tbl_1.gd_co.ins,
  combined_co = tbl_1.combined_co.cohort, combined_sh = tbl_1.combined_sh.cohort
)


#---

#test - optional to view columns will select next
rm(see2)
see2<- tbl_1.ted_co.age %>%
  select(11, 12, 3:9)

#apply loop to make flex table
num.new.colm <- 0
for (i in tbl_2.df.list)
{
  num.new.colm<- num.new.colm + 1
  
  assign(paste0("tbl_2.", names(tbl_2.df.list[num.new.colm])), 
         i %>%
 # select(`**Characteristic**`, N, `Year 0`:`Year 6`)  %>% #select only needed columns
#select columns by index number due to different column names:
   select(11, 12, 3:9) %>% #columns are category, N, years
  subset(! `0` %in% 'NULL')  %>% #remove the overall N row
   # dplyr::rename(label = `**Characteristic**`) %>% #new name on left
  mutate(N = comma(as.numeric(N))) %>% #add comma format to numbers
    mutate_all(as.character) %>% # needs to be character to show in flex table
    qflextable() %>% #convert to flex table
    flextable::font(fontname = "Calibri", part = "all") %>% #change font to a font that is registered in the postscriptFonts
  #add second header row before adding first header row - order matters! 
  add_header_row(
    values = c("","", "Time (years)", "","","",  "","","")
    ,colwidths = c(1,1,1, 1,1,1,  1,1,1),top = TRUE)%>% #must total to same amount of columns
  #Add the most top header row last so that it will go on top!
   add_header_row(values =
                    c("Probability of Having Hearing Loss"),
                              colwidths = c(9) # the count of listed values must equal listed col widths
    ) %>%
    bold(bold = TRUE, part = "header") %>%
    # Below year header will later become header over sub-header 
    # set_header_labels(
    #                               "Year 0" = "Time (years)", 
    #                               "N" = "" # remove column name (will later place in loswer row)
    # ) %>%
    theme_zebra() %>%
    #below merge columns to later apply sub-headers
  merge_at(i = 2, j = 3:9, part = "header") %>% # merge year columns
  merge_at(i = 2, j = 1:2, part = "header") %>%  #merge first two blank columns
# add back in the sub-headers
  # add_header_row(values =
  #            c("", "N","0", "1", "2", "3", "4" , "5", "6"), top = FALSE) %>%
  bold(bold = TRUE, part = "header") %>%
  align(align = "center", part = "body") %>%
  align( align = "center", part = "header") %>%
  align(align = "left", part = "footer") %>%
  align(i = 1, align = "left", part = "header") %>%
  align(j = 1:2, align = "left", part = "all") %>% #align left first & 2nd column of all table
  align(i = 2, align = "center", part = "header") %>% # center align 'Time (years)'
  padding(padding = 2, part = "all") %>% # make padding narrow
  empty_blanks() %>%
  autofit() %>%
 #fit_to_width( max_width = 6) %>%# this is smaller than auto-fit
  vline( j = c(1:9), border = fp_border(), part = "all") %>% # add vertical line
  border_outer(border = fp_border(), part = "header") %>%
  border_outer(border = fp_border(), part = "body") %>%
  border_outer( border = NULL, part = "footer") %>%
  bg(bg = "transparent", part = "footer") %>%
  hline(i = 2, j = c(1:9), border = fp_border(), part = "header") #%>%
  #align(i = 1, j = 1, align = "left", part = "body") #%>%
  #bold(bold = TRUE, i = 1, j= 1, part = "body")
  )
}

# view tables
tbl_2.combined_co # fig 1 - requested 
tbl_2.combined_sh # fig2
tbl_2.ted_sh.gender
tbl_2.ted_sh.age #fig 4
tbl_2.ted_co.gender # fig 5 - requested 
tbl_2.gd_co.gender # fig 6
tbl_2.gp_co.gender
tbl_2.ted_co.age #fig 8 - requested usually has max y-axis value among all plots
tbl_2.gd_co.age
tbl_2.gp_co.age # fig 10
tbl_2.ted_co.ins
tbl_2.gd.ins
tbl_2.gp.ins


############################################################


# SAVE to R environment TO USE FOR QUARTO WORD DOC

#cmbnd.co
save(tbl_2.combined_co, file = 'km_curve/tbl_2.combined_co.RData')
save(p2_cmbnd.co, file = 'km_curve/p2_cmbnd.co.RData')

#ted co gender
save(tbl_2.ted_co.gender, file = 'km_curve/tbl_2.ted_co.gender.RData')
save(p2_ted.co.sex, file = 'km_curve/p2_ted.co.sex.RData')

#ted_co.age
save(tbl_2.ted_co.age, file = 'km_curve/tbl_2.ted_co.age.RData')
save(p2_ted.co.age, file = 'km_curve/p2_ted.co.age.RData')

#PNG save
png(file="plot_ted.co.age.png"
    #,width=600, height=350
    ,width=600, height=500
    )
p2_ted.co.age
dev.off()


png(file="plot_combined.co_cohort.png"
   # ,width=600, height=350
    ,width=600, height=500
)
p2_cmbnd.co
dev.off()


png(file="plot_ted.co.gender.png"
    ,width=600, height=350
    #,width=600, height=500
)
p2_ted.co.sex
dev.off()

#save_as_image(tbl_2.ted_co.age, path = "table.ted_co.age.png", res = 300)
