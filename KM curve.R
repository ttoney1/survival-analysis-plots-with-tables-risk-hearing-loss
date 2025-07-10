#OBJECTIVE: Survival Analysis - Create KM (Kaplan-Meier) plots and tables which
#shows survival rates and modify plot data labels, plot  background, grid lines, 
#tick marks, tick mark length / width, and many other plot features.

#IMPORATANT - The FINAL version of this code is the loop version where you loop
#over several different types of data sets for hearing loss to 
#create multiple plots at once. 

#BACKGROUND - This is hearing loss data for different types of hearing loss. 
#- The way the code is able to modify the KM plots was by extracting 
#the data behind the plots made from the KM (Kaplan-Meier) plot function, and 
#then using that data to make ggplots which you then allowed you to modify
#plot features. A Kaplan-Meier curve is a graphical representation of 
#survival rates over time, showing the probability of surviving to a certain 
#point. It plots time on the x-axis and the survival rate on the y-axis. 

#NOTES: Types of hearing loss data sets analyzed:
#TED  = (Thyroid Eye Disease) hearing loss 
#GD (without TED)  = likely refers to Graves' disease (GD) hearing loss
#GP - refers to the general populaiton with the risk for hearing loss without
#TED and GD.

#-------------------------------------------------------------------------

#PACKAGES
library(gtsummary) # to make survival table - tbl_survfit()
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

# IMPORT DATA 

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


# import data from DataBricks:
ted_co_km <- DBI::dbGetQuery(connection,
                "SELECT *FROM p24_rd_021.TED_CO_KM")


#------------------------------------
#         MAKE KM CHART, SO THAT YOU CAN GET PROBABILITIES 



#first make KM curve to later get data which include probabilities behind plot

#use a ggplot autoplot

#Figure 5

ted.fig.5.ap <-
autoplot(survfit(Surv(co_year, co_01) ~ gender, data = ted_co_km), fun = 'event', legendLabs = FALSE) +
  geom_label_repel(data = . %>% group_by(strata) %>% summarise(x = mean(time), y = mean(surv)), 
                   aes(x = x, y = y, label = strata, color = strata)) +
  theme(legend.position = 'none')
ted.fig.5.ap

#un-comment to convert to plotly for interactive if needed
#Note. The later plotly based on ggplot will have more formatting if needed.
# ted.fig.5.apy1 <-
# ggplotly(ted.fig.5.ap)
# ted.fig.5.apy1


#-----------------------------

# GET INTERMEDIATE DATA BEHIND PLOT

# get data behind plot   
ted.fig.5.df<- ted.fig.5.ap$data


#------------------
# add a column of data labels to data that has fewer data points since
# too many data points for later plot function to create data labels.
#rm(ted.fig.5.df)
ted.fig.5.df <- ted.fig.5.df %>%
  group_by(strata) %>%
  mutate(surv_val_n = ntile(surv, 10)) %>%
  mutate(surv_val_first = +(!duplicated(surv_val_n))) %>%
  ungroup() %>%
  # mutate(surv_data_label = ifelse(surv_val_first %in% 1, 
  #                                 label_percent(0.01)(surv), NA))
mutate(surv_data_label = ifelse(surv_val_first %in% 1, 
                                surv, NA))

#check 
table(ted.fig.5.df$surv_val_first)
table(ted.fig.5.df$surv_data_label)


#---

#Add a column to data for legend labels that will go over curves 
#inside chart
ted.fig.5.df <- ted.fig.5.df %>%
  group_by(strata) %>%
  mutate(strata_label = ifelse(time %in% max(time, na.rm =T) &
                                     surv %in% max(surv, na.rm =T),
                                   as.character(strata), NA)) %>%  ungroup()

#---------------------------------------------
# MAKE PLOT USING GGPLOT BASED ON PROBABILITIES AND ADD DATA LABELS 

#update title below
ted.fig.5.p10 <-
ggplot(
  #width = 8, height = 6,
  width = 7, height = 3,
  ted.fig.5.df, aes(x = time, y =surv, group = strata, color = strata,
)) +
  # geom_smooth(method = loess) # uncomment to add shadow to curve
  geom_smooth( se = FALSE) +
  xlab("Time (years)")  +
  ylab("Failure probability (%)") + 
  ggtitle(str_wrap("Figure 5: Plot of Kaplan-Meier curve with the risk of hearing
                   loss (composite outcome) among TED patients by gender", 80))


#display
ted.fig.5.p10
#slateblue

#un-comment to covert to plotly if need plot to be interactive 
#ted.fig.5.py1 <-
#ggplotly(ted.fig.5.p1)
#dispaly
#ted.fig.5.py1

#----------------------------------------------
# ADD THEME

#apply different themes to present different options 
ted.fig.5.p2 <- ted.fig.5.p1 +theme_economist(dkpanel = T) + scale_color_economist()
ted.fig.5.p3 <-ted.fig.5.p1 + theme_stata() + scale_color_stata()
ted.fig.5.p4 <-ted.fig.5.p1 + theme_economist_white()

# make more plots after formatting
ted.fig.5.p2_2 <- ted.fig.5.p2
ted.fig.5.p3_2 <- ted.fig.5.p3 +theme(plot.background = element_rect(
  fill = "lemonchiffon"
))
# ted.fig.5.p3_3 <-ted.fig.5.p3 +theme(plot.background = element_rect(fill = "lightyellow1"
# ))

# custom themes
#theme1
ted.fig.5.p5 <-ted.fig.5.p1 +theme(plot.background = element_rect(fill = "lightblue2"
))
ted.fig.5.p6 <-ted.fig.5.p5 +theme(plot.background = element_rect(fill = "grey78"
))
#theme 2
#ted.fig.5.p1 +theme(plot.background = element_rect(fill = "lightblue2"
#))
#theme 3
#ted.fig.5.p1+theme_economist(dkpanel = T) + scale_color_economist() +
#  theme(panel.background = element_rect(fill = "lightgrey"))

#------------------------------------------------------
#APPLY PLOT FORMATTING. Breaking it up into parts so it is easier to code.

# change line color if needed 
ted.fig.5.p1 <-
ted.fig.5.p1 + scale_color_manual(values = c('#40E0D0', '#FF0000'))
#'#00B0F0', '#FF0000', '#0092FF', '#FFDB00' , '#40E0D0' # good colors 



# change grid line color if needed
ted.fig.5.p1 <-
ted.fig.5.p1 + 
  theme(
panel.grid = element_line(colour = "grey60", linewidth= 0.2))

#remove grid lines if needed
ted.fig.5.p10 <-
ted.fig.5.p10 +
theme(
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank())
#----------------------------------

# Adjust plot title size 
ted.fig.5.p10 <-
  ted.fig.5.p10 + 
theme(
  #legend.text = element_text(size=10),
  plot.title = element_text(size=9
                            ,vjust =4) #, # un-comment for theme_economist
  # legend.title = element_text(size=8) ,
  # legend.position="top"
)

# update title only

ted.fig.5.p10 <-
  ted.fig.5.p10 +
  ggtitle(str_wrap("Figure 5: Plot of Kaplan-Meier curve with the risk of hearing
                   loss (composite outcome) among TED patients by gender", 100))

ted.fig.5.p10_title.3line <-
  ted.fig.5.p10_title.3line +
  ggtitle(str_wrap("Figure 5: Plot of Kaplan-Meier curve with the risk of hearing
                   loss (composite outcome) among TED patients by gender", 52))
ted.fig.5.p10

#edit title
ted.fig.5.p10 <-
  ted.fig.5.p10 +
  theme(
plot.title.position = "plot"
,plot.title = element_text(
  size = 10
  #,hjust = 0.95
  # ,hjust = 0 # left align title 
  # ,margin = margin(l = 0.1)
  #,margin = margin(t = 10, b = 5)
  ,face = "bold"
  #, color = "darkblue", 
  #hjust = 0.5
  #,vjust = 4 # moves up/down
  #family = "arial"
  #,family = "serif"
) 
)
ted.fig.5.p10

# remove bold from plot title
#ted.fig.5.p10.for.quarto <-
  ted.fig.5.p10_title.3line <-
    ted.fig.5.p10_title.3line +
  theme(
    plot.title = element_text(
      face = "plain")
  )



#--------------------

# remove any desired layer from the plot
ted.fig.5.p11$layers #display layers
ted.fig.5.p10.for.quarto$layers[[2]] <- NULL #update layer number to remove it

#------------------------


#Add legend labels to end of curves inside chart if needed
#hjust 0 + wide right plot margin + coord_cartesian clip off are important to
#push labels to end of lines
ted.fig.5.p10.for.quarto <-
  ted.fig.5.p10.for.quarto +
#geom_label_repel(aes(label = ted.fig.5.df$strata_label),
geom_richtext(data = ted.fig.5.df, aes(label = strata_label), 
#geom_text(aes(label = ted.fig.5.df$strata_label),
              #color = "black", # un-comment to change text color
              label.size = NA, # un-comment to remove borders
             # fill = 'white',
              fill = NA , # un-comment to remove fill
              label.margin = unit(4, "pt"),
              label.padding = unit(3, "pt"),
              hjust = 0
              #show.legend = FALSE, # already removed legend
              #,fontface = "bold"
          ) +
  coord_cartesian(clip = "off") +
  theme(plot.margin = margin(16, 50, 16, 32)) + # wide right margin
  guides(color = "none")


# try margin directly in the plot
ted.fig.5.p11 <-
  ted.fig.5.p10 +
  #theme(margin(t= 0, r= 0, b=0, l=0,unit ="pt" ))
  theme(
    #panel.spacing  = margin(0, 0, 0, 0)
    panel.spacing.x  = unit(0, "lines")
)

# plot.margin = margin(t = 16,  # Top margin
#                      r = 32,  # Right margin
#                      b = 16,  # Bottom margin
#                      l = 32) # Left margin


# for theme_economist
ted.fig.5.p2_2 <-
  ted.fig.5.p2_2 +
theme(plot.margin = margin(31, 50, 16, 32)) 

#---

#----
# add data labels

ted.fig.5.p2 <-
  ted.fig.5.p2 +
geom_label_repel(
  aes(label = label_percent(0.1)(ted.fig.5.df$surv_data_label)),
  force = 10,
  label.size = NA, fill = NA ,
   # color = 'black', # color for labels
  color = 'slateblue',
  point.size = 1,
  # label.padding = 1,
  box.padding = .5,
  # alpha = 0.8 ,
  segment.alpha = 0.8, alpha = 1, # to bring labels forward
  size = 3 #,
  #  segment.color = "black", segment.size = 1
) 
  

#-----------

#adjust margins so that it later looks normally spaced on PDF
# already changed margins 
ted.fig.5.p10<-
  ted.fig.5.p10 +
  theme(
    #- To adjust page margins  in the PDF, adjust below:
    plot.margin = margin(t = 16,  # Top margin
                         r = 32,  # Right margin
                         b = 16,  # Bottom margin
                         l = 32) # Left margin
  )




ted.fig.5.p1.backup <-ted.fig.5.p1
#----

 

#--

#Scale ggplot2 x-axis by converting decimals to percentages 
#NOTE - Update the max limit below based on the maximum max limit of all figures
# ted.fig.5.p1<-
# 
#   ted.fig.5.p1+
#   scale_y_continuous(labels = scales::percent,
#                      limits = c(round(min(ted.fig.5.df$surv, na.rm =T),2),
#                                 round(max(ted.fig.5.df$surv, na.rm =T),2)
#                      )
#                      # , n.breaks=0.02
#   ) 



#UPDATE Y-AXIS MAX PERCENTAGE  -- NEEDS TO BE SAME ACROSS ALL PLOTS!
#Update y-axis limits to greater than max percentage among all charts. It
# should be greater than the max percentage as opposed to equal to the
# max percentage so that plot does not get cut off!
ted.fig.5.p10 <-         
  ted.fig.5.p10 +
  scale_y_continuous(
                          limits = c(0,.13) #update if max changes
                          ,breaks = seq(0, .13  #update if max changes
                                  ,by = .02
                                 )
                     ,labels = scales::percent) 


#--
#remove legend title -- already removed
# ted.fig.5.p1<-
#   ted.fig.5.p1+
#   theme(legend.title=element_blank())
# ted.fig.5.p1

#--

#remove entire legend if needed 
ted.fig.5.p10<-
  ted.fig.5.p10+
  theme(legend.position="none")

---
# Add minor breaks so that you can later Customize grid lines if needed
#grid line color likely to be determined by final panel color. So, may come
#back to this code after applying final panel color.
ted.fig.5.p4 <-
ted.fig.5.p4 +
  theme(
   # panel.grid.minor.x = element_line(color = "white") ,
 # panel.grid.minor.x = element_line(color = "slategray2"),  # un-comment for theme_stata
    panel.grid.minor.x = element_line(color = "grey"),  # un-comment for economist_white
     #scale_x_continuous(breaks = seq(0, 6, by = .05)),
    minor_breaks_n(.5)
                   )

#---
# Add major grid-lines to x-axis if needed
ted.fig.5.p2<-
ted.fig.5.p2 +
theme(panel.grid.major.x = element_line(
                                      # color = "slategray2"# for theme_stata
                                        ,color = "white"  #un-coment for grey panek
                                    # ,color = "grey" # un-comment for white panel
                                    ,size = 0.5 # fine for economist 
                                       # ,size = 0.4 # fine for economist 
                                        #, linetype = 2
                                       ))

#----
# Add minor grid-lines to x-axis if needed
ted.fig.5.p5 <-
  ted.fig.5.p5 +
  theme(panel.grid.minor.x = element_line(
                                          color = "white" 
                                        #color = "grey"
                                          #  color = "slategray2"
                                          ,size = 0.17
                                         # ,size = 0.11 # for theme_stata
                                          #, linetype = 2
  ))
ted.fig.5.p5

#--
# MUST ADD CAPTION AFTER THEME OR ELSE THEME WILL NOT GET APPLIED!
# Add any important captions 
table(ted.fig.5.df$group)
# for data with gender
ted.fig.5.df.n.f <- nrow(ted.fig.5.df %>% filter(group %in% 'Female') )
ted.fig.5.df.n.m <- nrow(ted.fig.5.df %>% filter(group %in% 'Male') )
ted.fig.5.df.n.f.cat <- paste0(ted.fig.5.df.n.f, ' for female')
ted.fig.5.df.n.m.cat <- paste0(ted.fig.5.df.n.m, ' for male')

ted.fig.5.df.n <- nrow(ted.fig.5.df) # get row cont to include in caption
ted.fig.5.df.n

#manually update caption if needed
ted.fig.5.p1.backup <-ted.fig.5.p1
table(ted.fig.5.df$group)
ted.fig.5.p4<-
ted.fig.5.p4+
  labs(
    caption=str_wrap('Note. Plot contains 3918 plot points (2109 for females, 1809 for males). Therefore, not all plot points are shown.'
                  ,width =100  )) +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0 #left
                                    ,size=8 
                                    ,vjust = -3 #move up/down
                                    ))
#Un- comment below chunk to use automated caption    
   # labs(
   #      caption=str_wrap(paste('Note. Plot contains', ted.fig.5.df.n, 
   #      ' plot points (', ted.fig.5.df.n.f.cat, ',', 
   #      ted.fig.5.df.n.m.cat,
   #      '). Therefore, not all plot points are shown on plot.'),
   #      width = 10))+
  # ted.fig.5.p2+  
  # theme(
  # plot.caption = element_text(size=8
  #                            # ,hjust = 0.06
  #                           #  ,vjust =1
  #                            # , vjust= -5 #small numbers move caption down
  #                             ))

ted.fig.5.p2

#--
# Remove caption if needed
ted.fig.5.p5 <-
  ted.fig.5.p5+
labs(caption = NULL) 

#--
# Add tick marks - it will automatically increase the minor grid lines
# Better to adjust tick marks, and breaks AFTER APPLYING THEME!
#IMPORTANT - To increase the amount of minor tick marks, use the minor_breaks 
# sub-command!
#Add minor tick marks! - this increases tick marks and helps legend labels to move out
ted.fig.5.p10 <-
ted.fig.5.p10+
  guides(
    x = guide_axis(minor.ticks = TRUE)) +
  scale_x_continuous(
                      breaks = seq(0, 6.5, by = 1),
                     limits = c(0, 6.5),
                     expand = c(0, 0),
                     minor_breaks = seq(0, 6.5, 0.1)
                     # minor_breaks = seq(
                     #   round(min(ted.fig.5.df$time, na.rm =T)),
                     #   round(max(ted.fig.5.df$time, na.rm =T)),
                     #   0.1) 
                     )

# scale_x_continuous(breaks = round(seq(min(ted.fig.5.df$time),
#                   max(ted.fig.5.df$time), by = 0.2),1))
ted.fig.5.p10
ted.fig.5.p10.backup<- ted.fig.5.p10

#---
# Adjust x-axis scale - already done in minor ticks command
ted.fig.5.p5 <-
ted.fig.5.p10 +
scale_x_continuous(breaks = seq(0, 6.5, by = 1))




#---
#Adjust tick length (increase length) ---first have to change all before minor ticks
ted.fig.5.p10 <-
  ted.fig.5.p10 +
   theme(axis.ticks.length.x = unit(8, "pt"))
ted.fig.5.p10  

#view
ted.fig.5.p10

#Adjust minor tick length (decrease length)
ted.fig.5.p10 <-
ted.fig.5.p10 +
  theme(axis.minor.ticks.length.x = unit(4.5, "pt" ))



# Adjust tick mark color if needed
ted.fig.5.p9.test<-
  ted.fig.5.p9.test +
theme(axis.ticks = element_line(color = "black")) 
ted.fig.5.p9.test

#Adjust thickness / thinness of  tick mark line width if needed
#IMPORTANT: there is no axis.minor.ticks setting without the position 
#suffixes, as they inherit from the major ticks.
ted.fig.5.p10 <-
ted.fig.5.p10 +
  theme(axis.minor.ticks.x.bottom = element_line(
    #colour = 'red', # un-comment to change color
    size = 0.2 ))
ted.fig.5.p10

# Change axis label rotation if needed
ted.fig.5.p12 <-
ted.fig.5.p12 +
theme(axis.text.y = element_text(angle = 360))

#Adjust the distance between the y-axis title and numbers if needed
#y-axis - only for theme_economist 
ted.fig.5.p2 <-
ted.fig.5.p2 +
  theme(axis.title.y = element_text(margin = 
               margin(t = 0, r = 6, b = 0, l = 0))) # use for theme_economist

#Adjust the distance between the x-axis title and numbers if needed
#x-axis
ted.fig.5.p4 <-
ted.fig.5.p4 +
  theme(axis.title.x = element_text(margin = 
              margin(t = 1.5, r = 0, b = 0, l = 0))) # use for theme_economist

# make backups 
ted.fig.5.p2.backup <-ted.fig.5.p2
ted.fig.5.p3.backup <-ted.fig.5.p3
ted.fig.5.p4.backup <-ted.fig.5.p4
ted.fig.5.p10.backup <-ted.fig.5.p10

#un-comment to revert to backup
# ted.fig.5.p2 <- ted.fig.5.p2.backup
# ted.fig.5.p3<-ted.fig.5.p3.backup
 ted.fig.5.p10 <- ted.fig.5.p10.backup




#Add border
# Note - 'plot.border' is NOT usually an element and will break charts!
ted.fig.5.p2_2 <-
  ted.fig.5.p2_2 +
  theme(
plot.background = element_rect(color = NA # remove line color   
#plot.background = element_rect(color = "steelblue"
#plot.background = element_rect(color = "#01a2d9"
 #,linewidth = 1
 ))

# remove border if needed
ted.fig.5.p2 +
theme(
  #plot.background = element_blank())
plot.background = element_rect(color =NA
                               #,linewidth = 1
))


#----
#remove and adjust theme elements at once
ted.fig.5.p10<-
  ted.fig.5.p10 +
  
  theme(
#outside border line color
    plot.background = element_rect(color = 'grey78'), 
   # plot.background = element_rect(color = NA), 
#removes inside fill color, changes inside border line color 
   # panel.background = element_rect(fill = NA ,color = 'grey78'),
#color is line color of inside box 
    panel.background = element_rect(fill = NA ,color = 'grey78'),
# make inside fill color grey
  # panel.background = element_rect(fill = '#E5E5E5' ,color = 'grey78')
# remove gridlines 
    ,panel.grid.major = element_blank(), panel.grid.minor = element_blank()
)

#------

ted.fig.5.p10<-
  ted.fig.5.p10 +
  theme(panel.spacing = unit(0, "lines"))


ted.fig.5.p10<-
  ted.fig.5.p10 +
  theme(
    plot.margin = 
  hjust =10)

#--------
# fix title only 

# remove ggtitle first since it is not easily modified 
ted.fig.5.p10<-
ted.fig.5.p10 +
  theme(plot.title=element_blank())
ted.fig.5.p10

# add a title using labs instead 
ted.fig.5.p10 <-
  ted.fig.5.p10 +
  theme(
labs(
 title = str_wrap("Figure 5: Plot of Kaplan-Meier curve with the risk of hearing
                   loss (composite outcome) among TED patients by gender", 80)
)
,plot.title = element_text(
  size = 10
  #,hjust = 0.95
 # ,hjust = 0 # left align title 
 # ,margin = margin(l = 0.1)
  #,margin = margin(t = 10, b = 5)
  ,face = "bold"
 #, color = "darkblue", 
  #hjust = 0.5
  #,vjust = 4 # moves up/down
  #family = "arial"
  #,family = "serif"
) 
 # ,plot.title.position = "plot"
)
ted.fig.5.p10

#adjust title vjust
# ted.fig.5.p10_title.3line <-
#   ted.fig.5.p10_title.3line +
ted.fig.5.p10 <-
  ted.fig.5.p10 +
  theme(
plot.title = element_text(
  vjust = 4)
)

#  FINAL  PLOT MARGINS - CONTROLS MARGINS IN PDF!
# ted.fig.5.p10_title.3line <-
#   ted.fig.5.p10_title.3line +
ted.fig.5.p10 <-
  ted.fig.5.p10 +
  theme(plot.margin=margin( t = 20 ,r = 32, b = 16, l= 32)) # use for final margins

#######################################################

# CREATE TABLE 

#Extract only event rows the round to the nearest whole number year times
#by group 

rm(ted.tab.5)
rm(ted.tab.5a)
rm(ted.tab.5b)

# create column for a cumulative sum of number of events by group in the data

ted.fig.5.df <- ted.fig.5.df %>%
  arrange(time) %>% #sort time ascending 
  group_by(strata) %>% 
  #mutate(female_cum_n.event = cumsum(n.event[strata = "Female"])
         mutate(cum_n.event_by_group = cumsum(n.event)
  ) %>%
  relocate (cum_n.event_by_group, .after =n.event)




#first need to keep only automated zero from plot because creates duplicates
# filter out the automated zero exactly from surv (is not originally part of raw data)
ted.tab.5a <- ted.fig.5.df %>%
  filter( ! (surv %in% 0))

# pivot wider by the group
ted.tab.5b <- ted.tab.5a %>%
  pivot_wider(
    id_cols = c(time, n.risk, n.event, cum_n.event_by_group, n.censor), 
              names_from = strata, 
              values_from = surv)



# group times by their number before their whole number before decimal
ted.tab.5b <- ted.tab.5b %>%
  mutate(time_group = substr(time,1,1),
         )
#check
table(ted.tab.5b_f$f_time_group)

#create columns for rates that are nearest whole number years (1,2,3,4,5,6)
ted.tab.5b <- ted.tab.5b %>%
  group_by(time_group) %>%
  mutate(
    female_time_group_max = ifelse(time > 0, #if time is greater than zero
      max(time[! is.na(Female)], na.rm = T), NA), #where female rate is not blank
    male_time_group_max = ifelse(time > 0, #if time is greater than zero
      max(time[! is.na(Male)], na.rm = T), NA) # where male rate is not balnk
  ) %>%
  mutate(female_time_whole_num =
      ifelse(time %in% 0, 0,
      ifelse(time_group %in% 0 & time >= max(female_time_group_max, na.rm =T), 1,
      ifelse(time_group %in% 1 & time >= max(female_time_group_max, na.rm =T), 2,
      ifelse(time_group %in% 2 & time >= max(female_time_group_max, na.rm =T), 3,
      ifelse(time_group %in% 3 & time >= max(female_time_group_max, na.rm =T), 4,
      ifelse(time_group %in% 4 & time >= max(female_time_group_max, na.rm =T), 5,
      ifelse(time_group %in% 5 & time >= max(female_time_group_max, na.rm =T), 6,
        NA))))))) 
        
  ) %>%
  mutate(male_time_whole_num =
           ifelse(time %in% 0, 0,
                  ifelse(time_group %in% 0 & time >= max(male_time_group_max, na.rm =T), 1,
                         ifelse(time_group %in% 1 & time >= max(male_time_group_max, na.rm =T), 2,
                                ifelse(time_group %in% 2 & time >= max(male_time_group_max, na.rm =T), 3,
                                       ifelse(time_group %in% 3 & time >= max(male_time_group_max, na.rm =T), 4,
                                              ifelse(time_group %in% 4 & time >= max(male_time_group_max, na.rm =T), 5,
                                                     ifelse(time_group %in% 5 & time >= max(male_time_group_max, na.rm =T), 6,
                                                            NA))))))) 
         
  )

# To  check above-- make sure NA rate cell for '4' group for male,
# but max time for '4' group for female is  NA time for male

# note -  rates from data to compare
# round(0.999316, digits =0)  # row 364 in ted.fig.5.df
# round(1.002053, digits =3)
# substr(0.999316, 1,1)

#make sure numeric formats 
class(ted.tab.5b$time)
class(ted.tab.5b$time_group)

# filter for maximum times (which rounded to their whole number year times)
#in the group above the desired number, e.g. 0.9999 in zero group = 1
#e.g. 1.9999 in 'one' group = 2
ted.tab.5c <- ted.tab.5b %>%
  filter(
    time %in% 0|
    time %in% female_time_group_max |
    time %in% male_time_group_max
) %>%
  mutate(
  Male_long = Male,
  Female_long = Female) %>%
  mutate(Female = percent(Female_long, 0.1), # format as percent
         Male = percent(Male_long, 0.1)
         )


# Optional - un-comment if you also want pivot from wide to long 
#For 'names_to' and 'values_to', you create new column names that you want
# ted.tab.5d_long <- ted.tab.5d %>%
#   pivot_longer(c(Male, Female), names_to = "gender", values_to = "surv_pct")

# keep only needed columns
ted.tab.5e <- ted.tab.5d %>%
  select(Female, Male)


# ------------------------------------------------------
# ALTERANTIVE TO CREATE ABOVE MANUAL TABLE - USE FUNCTION INSTEAD

ted.fig.5.tbl.a<-
  tbl_survfit(
    survfit(Surv(co_year, co_01) ~ gender,  data = ted_co_km), 
    #estimate_fun = ~style_pvalue(., digits = 3), # this adds decimal places
    statistic = "{estimate}", #only display the rate and not the conf. intervals
    estimate_fun = ~style_percent(., symbol = TRUE, digits = 1),
    times = c(0, 1, 2, 3, 4, 5, 6),
    reverse = TRUE, # TRUE gets failure/risk rates instead of success rate
    label_header = "Year {time}"
  ) %>%
  add_n() #%>%
#as_data_frame()
#as_flex_table()

# get data frame version of above
ted.fig.5.tbl.a_df <-ted.fig.5.tbl.a$table_body

#replace nulls with NA's so you can omit them - does not work on list columns
# so commented out. Will use later on new char columns
# ted.fig.5.tbl.a_dfx[ ted.fig.5.tbl.a_dfx == 'NULL'] <- NA

# create columns of the rates ONLY from the table 
# year 5 (stat 6) and year 6 (stat 7) should have same rates for this data set  
ted.fig.5.tbl.a_df  <- ted.fig.5.tbl.a_df  %>%
  mutate(stat_7 = stat_6) %>%
  mutate(
  Year_0 =str_extract(stat_1, "^[^ ]+"),
  Year_1 =str_extract(stat_2, "^[^ ]+"),     
  Year_2 =str_extract(stat_3, "^[^ ]+"),
  Year_3 =str_extract(stat_4, "^[^ ]+"), 
  Year_4 =str_extract(stat_5, "^[^ ]+"),
  Year_5 =str_extract(stat_6, "^[^ ]+"),
  Year_6 =str_extract(stat_7, "^[^ ]+")      
         )  %>%
# Use the pipe in gsub to specify multiple characters to remove.
# Must use escape \\ symbols for parenthesis.
  mutate(Year_0 = gsub('c|"|\\(', "", Year_0),
  Year_1 = gsub('c|"|\\(', "", Year_1),   
  Year_2 = gsub('c|"|\\(', "", Year_2), 
  Year_3 = gsub('c|"|\\(', "", Year_3),   
  Year_4 = gsub('c|"|\\(', "", Year_4), 
  Year_5 = gsub('c|"|\\(', "", Year_5),   
  Year_6 = gsub('c|"|\\(', "", Year_6)      
         ) 

#replace nulls with NA's --only possible in new columns
ted.fig.5.tbl.a_df[ ted.fig.5.tbl.a_df == 'NULL'] <- NA

# get populations counts of groups 
fig5.grp <- 
  ggsurvplot(survfit(Surv(co_year, co_01) ~ gender, data = ted_co_km),
             fun = 'event', break.time.by = 1,risk.table = "abs_pct")
fig5.grp

# data
fig5.grp_pops <-fig5.grp$table$data
# sub
fig5.grp_pops_sub <-fig5.grp_pops %>%
  filter(time %in% 0) %>%
  select(gender, n.risk) %>%
  mutate(gender = str_to_title(gender)) # poroper case for later match key fields


# join group pops to data
ted.fig.5.tbl.a_df <-
  left_join(ted.fig.5.tbl.a_df, fig5.grp_pops_sub, 
            by = c("label" = "gender"))

#completely fill population column
ted.fig.5.tbl.a_df <-ted.fig.5.tbl.a_df %>%
  mutate(
    n.risk=
    ifelse(is.na(n.risk),N[label %in% 'gender'], # where N column in 'gender'
           n.risk),
    N_original = N,
    N = n.risk,
# make column proper case if needed
  label = str_to_title(label)
)


#--------------------------------------------------

# TABLE FORMATTING

#select only needed columns
ted.fig.5.tbl.b<- ted.fig.5.tbl.a_df %>%
  select(label, N, Year_0:Year_6)

# create Flex Table table for report - select only needed columns
table5a <- qflextable(ted.fig.5.tbl.b)
table5a #display

#MUST change font to a font that is registered in the postscriptFonts
# database!
#postscriptFonts() #  un-comment to display registered fonts
table5e <- flextable::font(table5e, fontname = "Calibri", part = "all")
table5e
#available fonts: Helvetica, Courier, AvantGarde, Bookman, NewCenturySchoolbook,
#Palatino, URWGothic, URWBookman, NimbusSan, URWHelvetica, NimbusSanCond,
#CenturySch, URWPalladio, NimbusRom, URWTimes, ArialMT, ComputerModern,
#ComputerModernItalic,Japan1, CNS1, DejaVu Math TeX Gyre, DejaVu Sans,
#DejaVu Sans Condensed, DejaVu Serif, Fira Sans, Lato

#add second header row
table5a <- add_header_row(table5a, values =
    c("Failure probability (%)"),
    colwidths = c(9) # the count of listed values must equal listed col widths
    )
table5a

table5a <- bold(table5a, bold = TRUE, part = "header")
table5a

# Year header will later become header over sub-header 
table5a  <- set_header_labels(table5a,
                        "Year_0" = "Time (years)", 
                        "label"  = ""
)
table5a

table5a <- theme_zebra(table5a)
table5a

# merge columns to later apply sub-headers
table5a <- merge_at(table5a, i = 2, j = 3:9, part = "header") # merge year columns

# add back in the sub-headers
table5a<- add_header_row(table5a, values =
                c("", "","0", "1", "2", "3", "4" , "5", "6"), top = FALSE)


table5a<- bold(table5a, bold = TRUE, part = "header")
table5a

# un-comment any option to add it to the table:
#table5a <- bold(table5a, bold = TRUE, i = 1, part = "body")
#table5a <- bold(table5a, bold = FALSE, i = 1, part = "footer")
#table5a <- fontsize(table5a, size = 11, part = "all")
#table5a <- fontsize(table5a, size = 10, part = "footer")

table5a <- align(table5a, align = "center", part = "body")
table5a<- align(table5a, align = "center", part = "header")
table5a <- align(table5a, align = "left", part = "footer")
table5a <- align(table5a, i = 1, align = "left", part = "header")
table5a <- align(table5a, i = 1, j = 1, align = "left", part = "body")
table5a
# TT- note - align top header back to center if want
#table5a <- align(table5a, i = 1, j = 1, align = "center", part = "header")


#table5a <- padding(table5a, padding = 5, part = "all") # padding back to wide
table5a <- padding(table5a, padding = 2, part = "all") # make padding narrow
table5a
table5a <- empty_blanks(table5a)
table5a
table5a <- autofit(table5a)
#table5a<- fit_to_width(table5a, max_width = 7.5)
# TT note- try diff sizes and autofit
table5e<- fit_to_width(table5e, max_width = 6)# this is smaller than auto-fit
#table5c<- fit_to_width(table5c, max_width = 6)
#table5e
table5f<-autofit(table5a)
table5f
# un-comment to update and add a footer
# table5a <- table5a %>%
#   add_footer("Data Source" = paste0(
#     "ECRF = Electronic Case Report Form; ",
#     "EMR = Electronic Medical Record."
#   ))
#table5a <- merge_at(table5a, i = 1, j = 1:7, part = "footer")

table5a<- vline(table5a, j = c(1:9), border = fp_border(), part = "all")
table5a <- border_outer(table5a, border = fp_border(), part = "header")
table5a
table5a <- border_outer(table5a, border = fp_border(), part = "body")
table5a
table5a<- border_outer(table5a, border = NULL, part = "footer")
table5a
table5a <- bg(table5a, bg = "transparent", part = "footer")

# TT added below
table5a <-hline(table5a, i = 2, j = c(3:9), border = fp_border(), part = "header")
table5a <- align(table5a, i = 1, j = 1, align = "left", part = "body")
table5a
table5a <- bold(table5a, bold = TRUE, i = 1, j= 1, part = "body")
#table5a <- bold(table5a, bold = TRUE, j= 1, part = "body")
#theme_zebra(table5a, odd_header = 'lightblue')
#color(table5a, color = "white", part = "header") # change header font color
table5e <-bg(table5e, bg = "#cfcfcf", part = "header") # change fill color-looks default grey
table5e_blue <- bg(table5e, bg = "#CFE9F1", part = "header") # change fill color 
table5c <-bg(table5a, bg = "lightblue", part = "header") # change fill color 


# display:
table5a

#----------------------------------------------------

# ARRANGE PLOT WITH TABLE

#convert flextable to a grob
?gen_grob
g_table5a <-
gen_grob(
  table5a,
  fit = c("fixed"),
  scaling = c("fixed"),
 # fit = c("auto", "width", "fixed"),
  #scaling = c("min", "full", "fixed"),
  wrapping = TRUE
  ,autowidths = FALSE
  ,just = 1 # MOST IMPORTANT - THIS NUMBER ADJUSTS THE MARGINS!
 #notes for  just = "left", "right", "centre", "center", "bottom", and "top"
# hjust =20,
 ,margin(t = 0, r = 5, b = 0, l = 10)
# ,width=12, height=7, units="in"
)



#display
grid.newpage() 
grid.draw(g_table5f)

grid.arrange(ted.fig.5.p10, g_table5f)



#----
# CONTROLS MARGINS IN PDF!
ted.fig.5.p10<-
ted.fig.5.p10+
  theme(plot.margin=margin( t = 36 ,r = 32, b = 16, l= 32))


# below does not do anything - commented out
# ted.fig.5.p10<-
#   ted.fig.5.p10 +
#   theme(
#   margin(t = 0,  # Top margin
#          r = 0,  # Right margin
#          b = 0,  # Bottom margin
#          l = 0) # Left margin
#   )
  

ted.fig.5.p10_gt<-
  grid.arrange(ted.fig.5.p10, g_table5a
               ,ncol = 1
               # margin = t = 16,  # Top margin
               #        r = 32,  # Right margin
               #        b = 16,  # Bottom margin
               #        l = 32) # Left margin
  )
 #theme(plot.margin=margin(10,10,10,10))))

ted.fig.5.p10_gt<-
grid.arrange(arrangeGrob(ted.fig.5.p10, g_table5f
                         ,ncol = 1
                         ,padding = unit(0, "line")
                         ,left = 25
                         ,right = 50
                         ))

?grid.arrange
#----------------------------
plot_grid(ted.fig.5.p10, g_table5a= 2 ,ncol = 1,align="hv") + 
  theme(plot.margin = unit(c(20,20,20,20), "points"))





#----------------------------------------------------------
# SAVE

# SAVE MULTPLE PLOTS TO SAME DOCUMENT  IN PDF:
# pdf("Figure 5 KM curve options_03142025.pdf", width=11, height=8.5) 
# #par(mar = c(bottom, left, top, right)) # page margins
# par(mar = c(bottom = 2, left =20, top =20, right = 50))
# # #so must set margins in the themes with 'plot.margin command!
# #grid.arrange()
# ggarrange(
#   ted.fig.5.p10_gt
#  # ted.fig.5.p10, img2
#   #, ted.fig.5.p10
#   ,nrow = 2, ncol = 2
# )  # enter plots for 1 page
# dev.off()
# 
# 
# SAVE PLOT WITH TABLE
pdf("Figure 5 KM curve options_03122025.pdf", width=11, height=8.5)    
# par(mar=c(1,1,1,1)) # page margins has no effect on margins in PDF --
# #so must set margins in the themes with 'plot.margin command!
#grid.arrange()
ggarrange(
  ted.fig.5.p11,
  nrow = 2, ncol = 2
)  # enter plots for 1 page
dev.off()
#################################################################3
# SAVE to R environment
save(table5a, file = 'km_curve/table5a.RData')
save(ted.fig.5.p10, file = 'km_curve/ted.fig.5.p10.RData')
save(ted.fig.5.p10, file = 'km_curve/ted.fig.5.p10.RData')
save(ted.fig.5.p10.for.quarto, file = 'km_curve/ted.fig.5.p10.for.quarto.RData')
save(table5b, file = 'km_curve/table5b.RData')
save(table5c, file = 'km_curve/table5c.RData')
save(table5e, file = 'km_curve/table5e.RData')
save(table5f, file = 'km_curve/table5f.RData')
save(table5e_blue, file = 'km_curve/table5e_blue.RData')



# SAVE MULTPLE PLOTS TO SAME DOCUMENT  IN PDF:
pdf("Figure 5 KM curve OPTION 1_03142025.pdf"
    ,paper = "letter"
    , width=8.5, height=11 # standard
   # , width=11, height=8.5
 #  ,par(mar = c(bottom = 0, left =10, top = 30, right = 20))
    ) 
#grid.arrange(  
  ggarrange(
# note below 3-line fig is used for ncol 2, nrow 2
  #  ted.fig.5.p10_title.3line/
     ted.fig.5.p10/
  gen_grob(
    table5a,
    fit = c("width"),
   scaling = c("full"),
    # fit = c("auto", "width", "fixed"),
    #scaling = c("min", "full", "fixed"),
    wrapping = TRUE
    ,autowidths = TRUE
  # ,just = c(-1)
    # ,just = c(-17.4, -0.61) # MOST IMPORTANT - THIS NUMBER ADJUSTS HORIZONTAL MARGINS
    # ,VERTICAL
    #notes for  just = "left", "right", "centre", "center", "bottom", and "top"
    # hjust =20,
   # ,margin(t = 0, r = 0, b = 0, l = 0)
    # ,width=12, height=7, units="in"
    ),
 #  g_table5a,
#ted.fig.5.p10_gt,
ncol = 1, nrow =2)
dev.off()
#####################

#USE GGSAVE INSTAED TO CONTROL MARGINS BETTER

#margin order below is top; right (so large numbers moves plot to left);
#bottom; so larger numbers  create space at bottom, left)
ted.fig.5.p12<-
  ted.fig.5.p10 + theme(plot.margin=grid::unit(c(25,25,5,25), "mm"))

ggsave(filename="test2.pdf"
       ,plot=
       ,ggarrange(
      ted.fig.5.p12 / 
         
         gen_grob(
           table5a,
            fit = c("fixed"),
            scaling = c("full"),
           wrapping = TRUE
           ,autowidths = TRUE
           #  ,just = c(-17.4, -0.61) # MOST IMPORTANT - THIS NUMBER ADJUSTS HORIZONTAL MARGINS
           # #  VERTICAL
           #notes for  just = "left", "right", "centre", "center", "bottom", and "top"
           # hjust =20,
           # ,margin(t = 0, r = 0, b = 0, l = 0)
           # ,width=12, height=7, units="in"
         ),
       ncol = 2, nrow =2)
         
       ,width=8.5 #standard letter size
       , height=11 #standard letter size
       ,units="in" 
)

#un-comment to combine pdf's
#pdf_combine(input = c("My pdf.pdf", "test2.pdf"), "combo.pdf")
