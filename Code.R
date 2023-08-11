# load required packages
#if (!requireNamespace("BiocManager", qcuietly = TRUE))
 #install.packages("BiocManager")
#BiocManager::install(version = "3.11")
options(repos=BiocManager::repositories()) 
library(bnlearn)
library(scales)
library(gRbase)
library(gRain)

# Define working direction 

#getwd()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load require data ------------------------------------------------------------

# general_diagnosis.csv: Assignments of e.g. headings and default values.
# input_diagnosis.csv: definition of variable names and states of biological metrics
# output_diagnosis.csv: Definition of variable names and states of potential degradation causes

## Streams / rivers of the pre-alpine region (SRPAR)
general_info_SRPAR <- read.csv(file = "data_text/general_diagnosis_SRPAR.csv")
input_info_SRPAR <- read.csv2(file = "data_text/input_diagnosis_SRPAR.csv")
output_info_SRPAR <- read.csv2(file = "data_text/output_diagnosis_SRPAR.csv")

## Streams of the low mountain ranges (SLMR)
general_info_SLMR <- read.csv(file = "data_text/general_diagnosis_SLMR.csv")
input_info_SLMR <- read.csv2(file = "data_text/input_diagnosis_SLMR.csv")
output_info_SLMR <- read.csv2(file = "data_text/output_diagnosis_SLMR.csv")

## Rivers of the low mountain ranges (RLMR)
general_info_RLMR <- read.csv(file = "data_text/general_diagnosis_RLMR.csv")
input_info_RLMR <- read.csv2(file = "data_text/input_diagnosis_RLMR.csv")
output_info_RLMR <- read.csv2(file = "data_text/output_diagnosis_RLMR.csv")

### Layout specification of the SRPAR diagnostic tool --------------------------

# Definition of the parameters of the pop-up windows of individual potential degradation causes

popup_plot_textandimage_SRPAR <- cbind(as.character(output_info_SRPAR$html_file_name),
                              as.character(output_info_SRPAR$image_name),
                              as.character(output_info_SRPAR$image_source))

# Definition of the path to the diagnostic model (Bayesian network, BBN)

path_to_network_file_SRPAR <- as.character(general_info_SRPAR[general_info_SRPAR[,1] == "network_file_name",2])
path_to_network_file_SRPAR <- paste("data_text/network_files/",path_to_network_file_SRPAR,sep = "")

# Definition of names of the variables in the diagnostic tool

nodes_for_observation_SRPAR <- as.character(output_info_SRPAR$node_name)

# Definition of states of the variables in the diagnostic tool

rchartlabs_SRPAR <- as.character(output_info_SRPAR$node_names_to_show)

# Definition of the interactive user input incl. help icons for metrics

for_plot_SRPAR <- (as.character(output_info_SRPAR$class_to_use_in_radar_chart))
for_plot_SRPAR <- strsplit(x = for_plot_SRPAR,"|",fixed = T)
for_plot_SRPAR <- lapply(X = 1:length(for_plot_SRPAR), function(i) round(as.numeric(for_plot_SRPAR[[i]]),digits = 0))

# Definition of the interactive user input incl. help icons for metrics

evidence_SRPAR <- as.character(input_info_SRPAR$node_name)
input_description_SRPAR <- as.character(input_info_SRPAR$description)
input_help_SRPAR <- as.character(input_info_SRPAR$help_entry)
input_choices_SRPAR <- c()
for(i in 1:nrow(input_info_SRPAR)){
  temp_SRPAR <- input_info_SRPAR[i,]
  temp_SRPAR <- temp_SRPAR[!is.na(temp_SRPAR)]
  temp_SRPAR <- temp_SRPAR[-c(1:4)]
  temp_SRPAR <- c(temp_SRPAR, NA, "Unknown")
  temp_mat_SRPAR <- matrix(data = temp_SRPAR,nrow = 2)
  temp_SRPAR <- temp_mat_SRPAR[1,]
  names(temp_SRPAR) <- temp_mat_SRPAR[2,]
  input_choices_SRPAR <- c(input_choices_SRPAR, list(temp_SRPAR))
}

# Definition of the slider for adjusting the % representation

max_threshold_scale_SRPAR <- as.numeric(as.character(general_info_SRPAR[general_info_SRPAR[,1] == "threshold_slider_max",2]))
min_threshold_scale_SRPAR <- as.numeric(as.character(general_info_SRPAR[general_info_SRPAR[,1] == "threshold_slider_min",2]))
default_threshold_scale_SRPAR <- as.numeric(as.character(general_info_SRPAR[general_info_SRPAR[,1] == "threshold_slider_default",2]))

#  Title of diagnostic tool window 

title_panel_SRPAR <- as.character(general_info_SRPAR[general_info_SRPAR[,1] == "main_tab_title",2])
window_title_SRPAR <- ""

### Layout specification of the SLMR diagnostic tool ---------------------------

# Definition of the parameters of the pop-up windows of individual potential degradation causes

popup_plot_textandimage_SLMR <- cbind(as.character(output_info_SLMR$html_file_name),
                                     as.character(output_info_SLMR$image_name),
                                     as.character(output_info_SLMR$image_source))

# Definition of the path to the diagnostic model (Bayesian network, BBN)

path_to_network_file_SLMR <- as.character(general_info_SLMR[general_info_SLMR[,1] == "network_file_name",2])
path_to_network_file_SLMR <- paste("data_text/network_files/",path_to_network_file_SLMR,sep = "")

# Definition of names of the variables in the diagnostic tool

nodes_for_observation_SLMR <- as.character(output_info_SLMR$node_name)

# Definition of states of the variables in the diagnostic tool

rchartlabs_SLMR <- as.character(output_info_SLMR$node_names_to_show)

# Definition of the interactive user input incl. help icons for metrics

for_plot_SLMR <- (as.character(output_info_SLMR$class_to_use_in_radar_chart))
for_plot_SLMR <- strsplit(x = for_plot_SLMR,"|",fixed = T)
for_plot_SLMR <- lapply(X = 1:length(for_plot_SLMR), function(i) round(as.numeric(for_plot_SLMR[[i]]),digits = 0))

# Definition of the interactive user input incl. help icons for metrics

evidence_SLMR <- as.character(input_info_SLMR$node_name)
input_description_SLMR <- as.character(input_info_SLMR$description)
input_help_SLMR <- as.character(input_info_SLMR$help_entry)
input_choices_SLMR <- c()
for(i in 1:nrow(input_info_SLMR)){
  temp_SLMR <- input_info_SLMR[i,]
  temp_SLMR <-  temp_SLMR[!is.na( temp_SLMR)]
  temp_SLMR <-  temp_SLMR[-c(1:4)]
  temp_SLMR <- c( temp_SLMR, NA, "Unknown")
  temp_mat_SLMR <- matrix(data =  temp_SLMR,nrow = 2)
  temp_SLMR <- temp_mat_SLMR[1,]
  names( temp_SLMR) <- temp_mat_SLMR[2,]
  input_choices_SLMR <- c(input_choices_SLMR, list( temp_SLMR))
}

# Definition of the slider for adjusting the % representation

max_threshold_scale_SLMR <- as.numeric(as.character(general_info_SLMR[general_info_SLMR[,1] == "threshold_slider_max",2]))
min_threshold_scale_SLMR <- as.numeric(as.character(general_info_SLMR[general_info_SLMR[,1] == "threshold_slider_min",2]))
default_threshold_scale_SLMR <- as.numeric(as.character(general_info_SLMR[general_info_SLMR[,1] == "threshold_slider_default",2]))

# Title of the diagnostic window

title_panel_SLMR <- as.character(general_info_SLMR[general_info_SLMR[,1] == "main_tab_title",2])
window_title_SLMR <- ""

### Defintionen für Layout des online RLMR-Diagnosetools ---------------------------------

# Definition of the parameters of the pop-up windows of individual potential degradation causes

popup_plot_textandimage_RLMR <- cbind(as.character(output_info_RLMR$html_file_name),
                                     as.character(output_info_RLMR$image_name),
                                     as.character(output_info_RLMR$image_source))

# Definition of the path to the diagnostic model (Bayesian network, BBN)

path_to_network_file_RLMR <- as.character(general_info_RLMR[general_info_RLMR[,1] == "network_file_name",2])
path_to_network_file_RLMR <- paste("data_text/network_files/",path_to_network_file_RLMR,sep = "")

#  Definition of names of the variables in the diagnostic tool

nodes_for_observation_RLMR <- as.character(output_info_RLMR$node_name)

# Definition of states of the variables in the diagnostic tool

rchartlabs_RLMR <- as.character(output_info_RLMR$node_names_to_show)

# Definition of the interactive user input incl. help icons for metrics

for_plot_RLMR <- (as.character(output_info_RLMR$class_to_use_in_radar_chart))
for_plot_RLMR <- strsplit(x = for_plot_RLMR,"|",fixed = T)
for_plot_RLMR <- lapply(X = 1:length(for_plot_RLMR), function(i) round(as.numeric(for_plot_RLMR[[i]]),digits = 0))

# Definition of the interactive user input incl. help icons for metrics

evidence_RLMR <- as.character(input_info_RLMR$node_name)
input_description_RLMR <- as.character(input_info_RLMR$description)
input_help_RLMR <- as.character(input_info_RLMR$help_entry)
input_choices_RLMR <- c()
for(i in 1:nrow(input_info_RLMR)){
  temp_RLMR <- input_info_RLMR[i,]
  temp_RLMR <- temp_RLMR[!is.na(temp_RLMR)]
  temp_RLMR <- temp_RLMR[-c(1:4)]
  temp_RLMR <- c(temp_RLMR, NA, "Unknown")
  temp_mat_RLMR <- matrix(data = temp_RLMR,nrow = 2)
  temp_RLMR <- temp_mat_RLMR[1,]
  names(temp_RLMR) <- temp_mat_RLMR[2,]
  input_choices_RLMR <- c(input_choices_RLMR, list(temp_RLMR))
}

# Definition of the slider for adjusting the % representation

max_threshold_scale_RLMR <- as.numeric(as.character(general_info_RLMR[general_info_RLMR[,1] == "threshold_slider_max",2]))
min_threshold_scale_RLMR <- as.numeric(as.character(general_info_RLMR[general_info_RLMR[,1] == "threshold_slider_min",2]))
default_threshold_scale_RLMR <- as.numeric(as.character(general_info_RLMR[general_info_RLMR[,1] == "threshold_slider_default",2]))

# Title of diagnostic window

title_panel_RLMR <- as.character(general_info_RLMR[general_info_RLMR[,1] == "main_tab_title",2])
window_title_RLMR <- ""

### Definition of the BBN for SRPAR --------------------------------------------

original_net_SRPAR <- bnlearn::read.net(file = path_to_network_file_SRPAR) # load BBN 
grain_net_SRPAR <- bnlearn::as.grain(x = original_net_SRPAR) # change format to grain 
grain_net_SRPAR <- gRbase::compile(object = grain_net_SRPAR) 

# Definition of priors 

priors_SRPAR <- gRain::querygrain(object = grain_net_SRPAR, nodes = nodes_for_observation_SRPAR, type = "marginal")
priors_SRPAR <- priors_SRPAR[nodes_for_observation_SRPAR]

# Definition of selectable states of metrics in the interactive selection bar

temp_SRPAR <- rep(NA, length(evidence_SRPAR)) # evidence_SRPAR on variables
temp_SRPAR <- as.list(temp_SRPAR)
names(temp_SRPAR) <- evidence_SRPAR
evidence_SRPAR <- temp_SRPAR

# Definition of posteriors

posteriors_SRPAR <- gRain::querygrain(object = grain_net_SRPAR, nodes = nodes_for_observation_SRPAR, type = "marginal", evidence = evidence_SRPAR) 

### Definition of the BBN for SRPAR --------------------------------------------

original_net_SLMR <- bnlearn::read.net(file = path_to_network_file_SLMR) # load BBN
grain_net_SLMR <- bnlearn::as.grain(x = original_net_SLMR) # change format to grain 
grain_net_SLMR <- gRbase::compile(object = grain_net_SLMR) 

# Definition of priors

priors_SLMR <- gRain::querygrain(object = grain_net_SLMR, nodes = nodes_for_observation_SLMR, type = "marginal") # priors_SLMR; conditinal distributions
priors_SLMR <- priors_SLMR[nodes_for_observation_SLMR]

# Definition of selectable states of metrics in the interactive selection bar

temp_SLMR <- rep(NA, length(evidence_SLMR)) 
temp_SLMR <- as.list(temp_SLMR)
names(temp_SLMR) <- evidence_SLMR
evidence_SLMR <- temp_SLMR

# Definition of posteriors

posteriors_SLMR <- gRain::querygrain(object = grain_net_SLMR, nodes = nodes_for_observation_SLMR, type = "marginal", evidence = evidence_SLMR) 

### Definition of the BBN for RLMR ---------------------------------------------

original_net_RLMR <- bnlearn::read.net(file = path_to_network_file_RLMR) # load BBN
grain_net_RLMR <- bnlearn::as.grain(x = original_net_RLMR) # change format to zu grain änden
grain_net_RLMR <- gRbase::compile(object = grain_net_RLMR) 

# Definition of priors

priors_RLMR <- gRain::querygrain(object = grain_net_RLMR, nodes = nodes_for_observation_RLMR, type = "marginal") 
priors_RLMR <- priors_RLMR[nodes_for_observation_RLMR]

# Definition of selectable states of metrics in the interactive selection bar

temp_RLMR <- rep(NA, length(evidence_RLMR)) 
temp_RLMR <- as.list(temp_RLMR)
names(temp_RLMR) <- evidence_RLMR
evidence_RLMR <- temp_RLMR

# Definition of posteriors 

posteriors_RLMR <- gRain::querygrain(object = grain_net_RLMR, nodes = nodes_for_observation_RLMR, type = "marginal", evidence = evidence_RLMR)


### Definition of funtions for the diagnostic tools-----------------------------

# Output: spider plot

plot_radar_chart <- function(prior, post, pscale,for_plot, rchartlabs,col = "red"){
  labs <- names(post)
  scores <- c()
  for(i in 1:length(post)){
    fp <- for_plot[[i]]
    fpsum <- sum(sign(fp) * (post[[i]][abs(fp)] - prior[[i]][abs(fp)]))
    scores <- c(scores , fpsum)
  }
  names(scores) <- NULL
  scores[scores < 0.00] <- 0
  scores_plot <- list("probability" = 1*scores)
  
  scores_plot <- scores_plot[[1]]
  scores_plot <- rbind(rep(pscale,length(scores_plot)),
                       rep(0,length(scores_plot)),
                       scores_plot)
  colnames(scores_plot) <- labs
  rownames(scores_plot) <- NULL
  scores_plot <- as.data.frame(scores_plot)
  
  fmsb::radarchart(df = scores_plot,
                   maxmin = T,
                   axistype = 0,
                   centerzero = T,
                   seg  = 3,
                   plwd = 2,
                   pfcol = scales::alpha(col,0.25),
                   cglcol = scales::alpha("black",0.35),
                   cglty = 1,
                   cglwd = 1,
                   axislabcol = "gray",title = "",
                   vlcex = 1.2,
                   caxislabels = "thth",vlabels = rchartlabs)
}

# Output: hierarchy table of potential degradation causes

table_radar_chart <- function(prior, post, pscale,for_plot, rchartlabs,columnames){
  labs <- names(post)
  scores <- c()
  for(i in 1:length(post)){
    #   cat("____",i,"_____")
    fp <- for_plot[[i]]
    fpsum <- sum(sign(fp) * (post[[i]][abs(fp)] - prior[[i]][abs(fp)]))
    scores <- c(scores , fpsum)
    #    scores <- c(scores , prior[[i]][for_plot_SRPAR[i]] - post[[i]][for_plot_SRPAR[i]] +0.0)
  }
  names(scores) <- NULL
  scores[scores < 0.00] <- 0
  scores_plot <- list("probability " = 1*scores)
  
  scores_plot <- scores_plot[[1]]
  scores_plot <- rbind(rep(pscale,length(scores_plot)),
                       rep(0,length(scores_plot)),
                       scores_plot)
  colnames(scores_plot) <- labs
  rownames(scores_plot) <- NULL
  scores_plot <- as.data.frame(scores_plot)
  result <- as.vector(t(scores_plot[3,]))
  result <- as.data.frame(result)
  result <- cbind(rchartlabs,result)
  rownames(result) <- NULL
  colnames(result) <- columnames
  result <- result[order(result[,2],decreasing = T),]
  result[,2] <- result[,2] * 100
  return(result)
}

# Outout: eaction to user's selection of metric values

clicked_lab <- function(rad_click,labs){
  labs_click <- rep(F,length(labs))
  n <- length(labs_click)
  r <- sqrt((rad_click$x)^2 + (rad_click$y)^2)
  
  if(rad_click$y >= 0) {
    phi <- atan2(y = rad_click$y,x = rad_click$x) * 180 / pi
  }else{
    phi <- 360 + (atan2(y = rad_click$y,x = rad_click$x) * 180 / pi)
  }
  
  phi <- (phi - 90 + (360/(2*n))) %% 360
  
  if(r > 0 && r < 100) labs_click[ceiling(phi / (360/n))] <- T
  print(paste(rad_click$x, rad_click$y, r, phi, ceiling(phi / (360/n)),labs_click,sep = "  |  "))
  return(labs_click)
}

# Required variables for RShiny

variables_for_shiny <- list(nodes_for_observation_SRPAR = nodes_for_observation_SRPAR,
                            rchartlabs_SRPAR = rchartlabs_SRPAR,
                            evidence_SRPAR = evidence_SRPAR,
                            title_panel_SRPAR = title_panel_SRPAR,
                            window_title_SRPAR = window_title_SRPAR,
                            priors_SRPAR = priors_SRPAR,
                            posteriors_SRPAR = posteriors_SRPAR,
                            input_description_SRPAR = input_description_SRPAR,
                            input_choices_SRPAR = input_choices_SRPAR,
                            max_threshold_scale_SRPAR = max_threshold_scale_SRPAR,
                            min_threshold_scale_SRPAR = min_threshold_scale_SRPAR,
                            default_threshold_scale_SRPAR = default_threshold_scale_SRPAR,
                            for_plot_SRPAR = for_plot_SRPAR,
                            labs_SRPAR = names(priors_SRPAR),
                            popup_plot_textandimage_SRPAR = popup_plot_textandimage_SRPAR,
                            input_help_SRPAR = input_help_SRPAR,
                            
                            nodes_for_observation_SLMR = nodes_for_observation_SLMR,
                            rchartlabs_SLMR = rchartlabs_SLMR,
                            evidence_SLMR = evidence_SLMR,
                            title_panel_SLMR = title_panel_SLMR,
                            window_title_SLMR = window_title_SLMR,
                            priors_SLMR = priors_SLMR,
                            posteriors_SLMR = posteriors_SLMR,
                            input_description_SLMR = input_description_SLMR,
                            input_choices_SLMR = input_choices_SLMR,
                            max_threshold_scale_SLMR = max_threshold_scale_SLMR,
                            min_threshold_scale_SLMR = min_threshold_scale_SLMR,
                            default_threshold_scale_SLMR = default_threshold_scale_SLMR,
                            for_plot_SLMR = for_plot_SLMR,
                            labs_SLMR = names(priors_SLMR),
                            popup_plot_textandimage_SLMR = popup_plot_textandimage_SLMR,
                            input_help_SLMR = input_help_SLMR,
                            
                            nodes_for_observation_RLMR = nodes_for_observation_RLMR,
                            rchartlabs_RLMR = rchartlabs_RLMR,
                            evidence_RLMR = evidence_RLMR,
                            title_panel_RLMR = title_panel_RLMR,
                            window_title_RLMR = window_title_RLMR,
                            priors_RLMR = priors_RLMR,
                            posteriors_RLMR = posteriors_RLMR,
                            input_description_RLMR = input_description_RLMR,
                            input_choices_RLMR = input_choices_RLMR,
                            max_threshold_scale_RLMR = max_threshold_scale_RLMR,
                            min_threshold_scale_RLMR = min_threshold_scale_RLMR,
                            default_threshold_scale_RLMR = default_threshold_scale_RLMR,
                            for_plot_RLMR = for_plot_RLMR,
                            labs_RLMR = names(priors_RLMR),
                            popup_plot_textandimage_RLMR = popup_plot_textandimage_RLMR,
                            input_help_RLMR = input_help_RLMR
                            )

# Saving required variables and function for desktop-based application

save(list = c("variables_for_shiny","grain_net_SRPAR","grain_net_SLMR","grain_net_RLMR", 
              "plot_radar_chart","clicked_lab","table_radar_chart"),
     file = "data_for_shiny.Rdata") 





