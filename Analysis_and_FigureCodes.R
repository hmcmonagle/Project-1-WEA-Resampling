################################################################################
################################################################################
## Evaluation of indices with and without depth - All species
## By Kaitlyn O'Brien 
################################################################################
################################################################################

#This file includes all the analysis of the output from the sdmTMB models for 
#the spatial closure project. Additionally, all the code to create figures in 
#the manuscript (supplementary and main document) as well as other figures are incorporated throughout

#####################
## Libraries 
#####################
library(sdmTMB)
library("ggplot2")
library("raster")
library("terra")
library("tidyverse")
library("RANN")
library("sf")
library("rnaturalearth")
library("devtools")
library("sdmTMB")
library("ggpubr")
library("patchwork")
library("ggh4x")
library("gridExtra")
library("dplyr")
library("sdmTMB")
library("ggpmisc")
library("ggbreak")
library("nwfscSurvey")
library("eply")
library("patchwork")
#remotes::install_github("pbs-assess/sdmTMBextra", dependencies = TRUE) # always load sdmTMBextra after sdmTMB
library("sdmTMBextra")
library("units")
library("RANN")
library("scales")
library("ggh4x")
library("pbapply")
library(cowplot)
library("officedown")
library(fmesher)
library(grid)
library("png")
gc()
## Working directory 
WD.1=("C:/Users/kaitlyn.o'brien/Documents/GitHub/Offshore_Wind_PJ1/Data")

# HM = more recent edit from Helena McMonagle
# HM: Edit for local machine where WCGBTS data is stored
library(here)
WD.1=here() # check later if missing any files that should be in this folder 
  
###############################
#### Shapefile information ####
###############################
#### Coast outline - for plotting 
coast_outline <- ne_states(country = 'United States of America', returnclass = 'sf') %>%
  filter(gn_name %in% c('California','Oregon','Washington','Nevada','Idaho')) %>%
  st_transform(crs = "+proj=utm +zone=10 +datum=WGS84 +units=km")#make sure to convernt to UTM 

# Bathy=st_as_sf(as(raster(paste0(WD.1, "/Shapefile/Bathymetry_EPOTC.tif")), "SpatialGridDataFrame"))
# Bathy = Bathy %>%
#   st_transform(crs = st_crs("+proj=utm +zone=10 +datum=WGS84 +units=km"))
# st_coordinates(Bathy)
# 
#Projection shapefile - WCGBTS
Projection_extent = readRDS(paste0(WD.1, "/Projection_shapefile_WCGBTWS.rds"))


# Projection data frame - WCGBTS
Proj_df = readRDS(paste0(WD.1, "/Projection_Dataset_WCGBTWS.rds"))
## OSW scenarios ##

#Spatial scenario 1
OSW_current = readRDS(paste0(WD.1, "/OSW_current.rds"))
OSW_Scen1 = OSW_current
OSW_Scen1$Scenario = "Scenario 1"
# Spatial scenario 2
OSW_future = readRDS(paste0(WD.1, "/OSW_Fut.rds"))
OSW_future = OSW_future %>%
  filter(!group =="Mendocino1")

OSW_Scen2 = bind_rows(list(OSW_current,OSW_future))
OSW_Scen2$Scenario = "Scenario 2"

# Spatial scenario 2 outlines  
OSW_outline = readRDS(paste0(WD.1, "/OSW_Fut_outline.rds"))
# OSW_outline[OSW_outline$group=="Humboldt2",]$group = "Humboldt 2"
# OSW_outline[OSW_outline$group=="Humboldt3",]$group = "Humboldt 3"
# #OSW_outline[OSW_outline$group=="Mendocino1",]$group = "Mendocino 1"
# OSW_outline[OSW_outline$group=="Mendocino2",]$group = "Mendocino 2"

OSW_Scen2_outline = bind_rows(list(OSW_current, OSW_outline))
OSW_Scen2_outline$group2 = NA
OSW_Scen2_outline[OSW_Scen2_outline$group %in% c("Humboldt", "Morro", "Coos Bay", "Brookings"),]$group2 = "Current"
OSW_Scen2_outline[!OSW_Scen2_outline$group %in% c("Humboldt", "Morro", "Coos Bay", "Brookings"),]$group2 = "Other"
OSW_Scen2_outline$Scenario = "Scenario 2"
# Spatial scenario 3 - large areas 
OSW_Large = readRDS(paste0(WD.1, "/OSW_Scen_3.rds"))

OSW_Scen3 = bind_rows(list(OSW_current,OSW_Large))

OSW_Scen3 = OSW_Scen3 %>% filter(!group %in% c("Coos Bay", "Brookings"))
OSW_Scen3$Scenario = "Scenario 3"

#####################################
#### Plotting functions ####
#####################################

### Plotting functions for different performance metrics###
#Index plot - this plots the mean scaled index for each region across the different scenarios 
Ind_func = readRDS(paste0(WD.1, "/Functions/Ind_func.rds"))# 

# Index performance metrics - gets information regarding the average CV and performance metrics for point estimates
Ind_PM_func = readRDS(paste0(WD.1, "/Functions/Ind_PM_func.rds"))

# # Density performance metric plots - ENTIRE REGION
# # plots the performance metrics for the entire region
# Den_pm_ER_plot_Func = readRDS(paste0(WD.1, "/Functions/Den_pm_ER_plot_Func.rds"))
# 
# #Density plot function
 Dens_plot_func = readRDS(paste0(WD.1, "/Functions/Dens_plot_func.rds"))#Index function 
 Dens_plot_func2 = readRDS(paste0(WD.1, "/Functions/Dens_plot_func2.rds"))#Index function 
# 
# #Density percent change 
Dens_Prc_Change_func.b = readRDS(paste0(WD.1, "/Functions/Dens_perc_change_func_b.rds"))#Index function 
# 

#########################################
# Step 1 - load in and organize data ####
#########################################

### Aurora ####
Aurora_all = list()
## Load in the function information on the biomass and density estimates####
#Aurora_all = list(readRDS(paste0(WD.1, "/sdmTMB_func/Aurora rockfish_Scenario 1_2025-05-29_NEW4.rds")),
#                       readRDS(paste0(WD.1, "/sdmTMB_func/Aurora rockfish_Scenario 2_2025-05-29_NEW4.rds")),
#                       readRDS(paste0(WD.1, "/sdmTMB_func/Aurora rockfish_Scenario 3_2025-05-29_NEW4.rds")))

# HM: edited to match folder name where these RDS files are stored (Data_files-sdmTMB, instead of sdmTMB_func)
Aurora_all = list(readRDS(paste0(WD.1, "/Data_files-sdmTMB/Aurora rockfish_Scenario 1_2025-05-29_NEW4.rds")),
                  readRDS(paste0(WD.1, "/Data_files-sdmTMB/Aurora rockfish_Scenario 2_2025-05-29_NEW4.rds")),
                  readRDS(paste0(WD.1, "/Data_files-sdmTMB/Aurora rockfish_Scenario 3_2025-05-29_NEW4.rds")))

#Now divide this information between the model that included depth and did not 
Aurora_nd = list()
Aurora_d = list()
for(i in 1:3){
  Aurora_nd[[i]] = Aurora_all[[i]][c(1,3:7)]
  Aurora_d[[i]] = Aurora_all[[i]][c(2, 8:12)]
  names(Aurora_d[[i]]) = names(Aurora_nd[[i]])
}


### Longspine ####
Longspine_all = list()
## Load in the function information on the biomass and density estimates####
#Longspine_all = list(readRDS(paste0(WD.1, "/sdmTMB_func/Longspine thornyhead_Scenario 1_2025-05-28_NEW3.rds")),
#                  readRDS(paste0(WD.1, "/sdmTMB_func/Longspine thornyhead_Scenario 2_2025-05-28_NEW3.rds")),
#                  readRDS(paste0(WD.1, "/sdmTMB_func/Longspine thornyhead_Scenario 3_2025-05-28_NEW3.rds")))

# HM: edited to match folder name where these RDS files are stored (Data_files-sdmTMB, instead of sdmTMB_func)
Longspine_all = list(readRDS(paste0(WD.1, "/Data_files-sdmTMB/Longspine thornyhead_Scenario 1_2025-05-28_NEW3.rds")),
                  readRDS(paste0(WD.1, "/Data_files-sdmTMB/Longspine thornyhead_Scenario 2_2025-05-28_NEW3.rds")),
                  readRDS(paste0(WD.1, "/Data_files-sdmTMB/Longspine thornyhead_Scenario 3_2025-05-28_NEW3.rds")))


#Now divide this information between the model that included depth and did not 
Longspine_nd = list()
Longspine_d = list()
for(i in 1:3){
  Longspine_nd[[i]] = Longspine_all[[i]][c(1,3:7)]
  Longspine_d[[i]] = Longspine_all[[i]][c(2, 8:12)]
  names(Longspine_d[[i]]) = names(Longspine_nd[[i]])
}


### Sablefish ####
Sablefish_all = list()

#Sablefish_all = list(readRDS(paste0(WD.1, "/sdmTMB_func/Sablefish_Scenario 1_2025-05-21_NEW3.rds")),
#                     readRDS(paste0(WD.1, "/sdmTMB_func/Sablefish_Scenario 2_2025-05-21_NEW3.rds")),
#                     readRDS(paste0(WD.1, "/sdmTMB_func/Sablefish_Scenario 3_2025-05-21_NEW3.rds")))

# HM: edited to match folder name where these RDS files are stored (Data_files-sdmTMB, instead of sdmTMB_func)
Sablefish_all = list(readRDS(paste0(WD.1, "/Data_files-sdmTMB/Sablefish_Scenario 1_2025-05-21_NEW3.rds")),
                     readRDS(paste0(WD.1, "/Data_files-sdmTMB/Sablefish_Scenario 2_2025-05-21_NEW3.rds")),
                     readRDS(paste0(WD.1, "/Data_files-sdmTMB/Sablefish_Scenario 3_2025-05-21_NEW3.rds")))

Sablefish_nd = list()
Sablefish_d = list()
for(i in 1:3){
  Sablefish_nd[[i]] = Sablefish_all[[i]][c(1,3:7)]
  Sablefish_d[[i]] = Sablefish_all[[i]][c(2, 8:12)]
  names(Sablefish_d[[i]]) = names(Sablefish_nd[[i]])
}

### Shortspine ####
Shortspine_all = list()

#Shortspine_all = list(readRDS(paste0(WD.1, "/sdmTMB_func/Shortspine thornyhead_Scenario 1_2025-05-29_NEW4.rds")),
#                     readRDS(paste0(WD.1, "/sdmTMB_func/Shortspine thornyhead_Scenario 2_2025-05-29_NEW4.rds")),
#                     readRDS(paste0(WD.1, "/sdmTMB_func/Shortspine thornyhead_Scenario 3_2025-05-30_NEW4.rds")))

# HM: edited to match folder name where these RDS files are stored (Data_files-sdmTMB, instead of sdmTMB_func)
Shortspine_all = list(readRDS(paste0(WD.1, "/Data_files-sdmTMB/Shortspine thornyhead_Scenario 1_2025-05-29_NEW4.rds")),
                      readRDS(paste0(WD.1, "/Data_files-sdmTMB/Shortspine thornyhead_Scenario 2_2025-05-29_NEW4.rds")),
                      readRDS(paste0(WD.1, "/Data_files-sdmTMB/Shortspine thornyhead_Scenario 3_2025-05-30_NEW4.rds")))


Shortspine_nd = list()
Shortspine_d = list()
for(i in 1:3){
  Shortspine_nd[[i]] = Shortspine_all[[i]][c(1,3:7)]
  Shortspine_d[[i]] = Shortspine_all[[i]][c(2, 8:12)]
  names(Shortspine_d[[i]]) = names(Shortspine_nd[[i]])
}

### Rex ####
Rex_all = list()

#Rex_all = list(readRDS(paste0(WD.1, "/sdmTMB_func/Rex sole_Scenario 1_2025-05-21_NEW2.rds")),
#                     readRDS(paste0(WD.1, "/sdmTMB_func/Rex sole_Scenario 2_2025-05-22_NEW2.rds")),
#                     readRDS(paste0(WD.1, "/sdmTMB_func/Rex sole_Scenario 3_2025-05-22_NEW2.rds")))

# HM: edited to match folder name where these RDS files are stored (Data_files-sdmTMB, instead of sdmTMB_func)
Rex_all = list(readRDS(paste0(WD.1, "/Data_files-sdmTMB/Rex sole_Scenario 1_2025-05-21_NEW2.rds")),
                 readRDS(paste0(WD.1, "/Data_files-sdmTMB/Rex sole_Scenario 2_2025-05-22_NEW2.rds")),
                 readRDS(paste0(WD.1, "/Data_files-sdmTMB/Rex sole_Scenario 3_2025-05-22_NEW2.rds")))

Rex_nd = list()
Rex_d = list()
for(i in 1:3){
  Rex_nd[[i]] = Rex_all[[i]][c(1,3:7)]
  Rex_d[[i]] = Rex_all[[i]][c(2, 8:12)]
  names(Rex_d[[i]]) = names(Rex_nd[[i]])
}

### Dover ####
Dover_all = list()

#Dover_all = list(readRDS(paste0(WD.1, "/sdmTMB_func/Dover sole_Scenario 1_2025-05-22_NEW3.rds")),
#                     readRDS(paste0(WD.1, "/sdmTMB_func/Dover sole_Scenario 2_2025-05-23_NEW3.rds")),
#                     readRDS(paste0(WD.1, "/sdmTMB_func/Dover sole_Scenario 3_2025-05-23_NEW3.rds")))

# HM: edited to match folder name where these RDS files are stored (Data_files-sdmTMB, instead of sdmTMB_func)
Dover_all = list(readRDS(paste0(WD.1, "/Data_files-sdmTMB/Dover sole_Scenario 1_2025-05-22_NEW3.rds")),
                 readRDS(paste0(WD.1, "/Data_files-sdmTMB/Dover sole_Scenario 2_2025-05-23_NEW3.rds")),
                 readRDS(paste0(WD.1, "/Data_files-sdmTMB/Dover sole_Scenario 3_2025-05-23_NEW3.rds")))

Dover_nd = list()
Dover_d = list()
for(i in 1:3){
  Dover_nd[[i]] = Dover_all[[i]][c(1,3:7)]
  Dover_d[[i]] = Dover_all[[i]][c(2, 8:12)]
  names(Dover_d[[i]]) = names(Dover_nd[[i]])
}

# Generate files to use in Project 2

# Use Dover_d to generate a separate df for each scenario for project 2 that only includes rows where group = Entire region
Dover_d_scen1 <- filter(Dover_d[[1]]$Index, group == "Entire region")  # Scenario 1
Dover_d_scen2 <- filter(Dover_d[[2]]$Index, group == "Entire Region")  # Scenario 2

# save these 3 Dover dataframes as CSV files for use in Project 2
Dover_d_scen1 %>% write.csv(paste0(WD.1, "/Outputs_for_Project_2/Dover_depth_scen1_index.csv"), row.names = FALSE)
Dover_d_scen2 %>% write.csv(paste0(WD.1, "/Outputs_for_Project_2/Dover_depth_scen2_index.csv"), row.names = FALSE)

# repeat for sablefish
Sablefish_d_scen1 <- filter(Sablefish_d[[1]]$Index, group == "Entire region")  # Scenario 1
Sablefish_d_scen2 <- filter(Sablefish_d[[2]]$Index, group == "Entire Region")  # Scenario 2

# save
Sablefish_d_scen1 %>% write.csv(paste0(WD.1, "/Outputs_for_Project_2/Sablefish_depth_scen1_index.csv"), row.names = FALSE)
Sablefish_d_scen2 %>% write.csv(paste0(WD.1, "/Outputs_for_Project_2/Sablefish_depth_scen2_index.csv"), row.names = FALSE)

############################################
#### Density Maps ####
############################################
gc()
OSW_all_scen = list(OSW_Scen1, OSW_Scen2_outline, OSW_Scen3)
Species_vec = c( "Aurora rockfish","Sablefish", "Longspine thornyhead", 
                 "Shortspine thornyhead", "Dover sole",
                 "Rex sole")

All_species_nd = list(Aurora_nd, Sablefish_nd, Longspine_nd, Shortspine_nd,Dover_nd,Rex_nd)
#saveRDS(All_species_nd, paste0(WD.1,"/All_Species_Function_Output_NO_DEPTH.rds"))

All_species_d = list(Aurora_d, Sablefish_d, Longspine_d, Shortspine_d,Dover_d,Rex_d)
#saveRDS(All_species_d, paste0(WD.1,"/All_Species_Function_Output_DEPTH.rds"))


## Density Information ####
Dens_list = list()
Dens_list2 = list()
gc()
#Get the density maps for when depth is included 
for(j in c(1,3)){
  print(paste0("Starting iteration for ", Species_vec[j]))
  Spec_data = All_species_d[[j]]
  
  #spec_list = list()
  spec_list2 = list()
  
  for(i in 1:3){
    print(paste0("Starting iteration for ", OSW_all_scen[[i]]$Scenario[1]))
    
    if(Species_vec[j] %in% c("Sablefish", "Dover sole", "Rex sole", "Shortspine thornyhead", "Aurora rockfish")){
      # spec_list[[i]] = Dens_plot_func2(Dens_data = Spec_data[[1]][[i]]$Density,
      #                                  #Dens_dif = Dover$Density_Dif,
      #                                  scen = year_df,
      #                                  Proj_extent = Projection_extent,
      #                                  OSW_shp = OSW_all_scen[[i]],
      #                                  Coast = coast_outline,
      #                                  lev_scen = c("All data", "1 year", "5 years", "10 years", "15 years", "19 years","Never surveyed"),
      #                                  Term.year = 2023)
      spec_list2[[i]] = Dens_plot_func2(Dens_data = Spec_data[[i]]$Density,
                                        #Dens_dif = Dover$Density_Dif,
                                        scen = year_df,
                                        Proj_extent = Projection_extent,
                                        OSW_shp = OSW_all_scen[[i]],
                                        Coast = coast_outline,
                                        lev_scen = c("All data", "1 year", "5 years", "10 years", "15 years", "19 years","Never surveyed"),
                                        Term.year = 2023)
      
      
    }else{
      # spec_list[[i]] = Dens_plot_func(Dens_data = Spec_data[[1]][[i]]$Density,
      #                                 #Dens_dif = Dover$Density_Dif,
      #                                 scen = year_df,
      #                                 Proj_extent = Projection_extent,
      #                                 OSW_shp = OSW_all_scen[[i]],
      #                                 Coast = coast_outline,
      #                                 lev_scen = c("All data", "1 year", "5 years", "10 years", "15 years", "19 years","Never surveyed"),
      #                                 Term.year = 2023)
      spec_list2[[i]] = Dens_plot_func(Dens_data = Spec_data[[i]]$Density,
                                       #Dens_dif = Dover$Density_Dif,
                                       scen = year_df,
                                       Proj_extent = Projection_extent,
                                       OSW_shp = OSW_all_scen[[i]],
                                       Coast = coast_outline,
                                       lev_scen = c("All data", "1 year", "5 years", "10 years", "15 years", "19 years","Never surveyed"),
                                       Term.year = 2023)
    }
    
  }
  #plot.data = lapply(spec_list, "[[", 1)
  plot.data2 = lapply(spec_list2, "[[", 1)
  
  #Dens_list[[j]] = plot.data#Grab the density information accross the entire region
  Dens_list2[[j]] = plot.data2#Grab the density information accross the entire region
  
  # Dens_plot_ER = lapply(plot.data, function(x){
  #   ggplot(coast_outline)+
  #     geom_sf()+
  #     geom_tile(data = x, aes(x=X, y=Y, ,fill = Est))+
  #     facet_wrap(~scen, nrow = 1)+
  #     scale_fill_viridis_c(na.value = "yellow",
  #                          #midpoint = quantile(bind_rows(plot.data)$Est, 0.1),
  #                          limits = c(0, quantile(bind_rows(plot.data)$Est, 0.99)))+
  #     # scale_fill_gradient2(low = "#fcfbfd",
  #     #                      mid = "#efedf5",
  #     #                      high = "#3f007d",
  #     #                      na.value = "#3f007d",
  #     #                      midpoint = quantile(bind_rows(plot.data)$Est, 0.1),
  #     #                      limits = c(0, quantile(bind_rows(plot.data)$Est, 0.99)))+
  #     # scale_fill_gradient2(low = "#0571b0",
  #     #                      mid = "#f7f7f7",
  #     #                      high = "#ca0020",
  #     #                      limits = c(min(gr_pred_dif$Dif),
  #     #                                 max(gr_pred_dif$Dif))) +
  #     labs(x = "", y="") +
  #     theme_classic()+
  #     xlim(min(bind_rows(plot.data)$X),
  #          max(bind_rows(plot.data)$X))+
  #     ylim(min(bind_rows(plot.data)$Y),
  #          max(bind_rows(plot.data)$Y))+
  #     #geom_sf(data = OSW_shp, fill = 'transparent', colour = 'black', lwd = .05)+
  #     theme(#axis.text = element_text(size = 5),
  #       axis.line = element_blank(),
  #       axis.ticks = element_blank(),
  #       axis.text = element_blank(),
  #       axis.title = element_blank(),
  #       legend.position = "none",
  #       legend.title = element_blank(),
  #       legend.key.width = unit(1, "cm"))
  # })
  
  Dens_plot_ER2 = lapply(plot.data2, function(x){
    ggplot(coast_outline)+
      geom_sf()+
      geom_tile(data = x, aes(x=X, y=Y, ,fill = Est))+
      facet_wrap(~scen, nrow = 1)+
      scale_fill_viridis_c(na.value = "yellow",
                           #midpoint = quantile(bind_rows(plot.data)$Est, 0.1),
                           limits = c(0, quantile(bind_rows(plot.data2)$Est, 0.99)))+
      # scale_fill_gradient2(low = "#0571b0",
      #                      mid = "#f7f7f7",
      #                      high = "#ca0020",
      #                      limits = c(min(gr_pred_dif$Dif),
      #                                 max(gr_pred_dif$Dif))) +
      labs(x = "", y="") +
      theme_classic()+
      xlim(min(bind_rows(plot.data2)$X),
           max(bind_rows(plot.data2)$X))+
      ylim(min(bind_rows(plot.data2)$Y),
           max(bind_rows(plot.data2)$Y))+
      #geom_sf(data = OSW_shp, fill = 'transparent', colour = 'black', lwd = .05)+
      theme(#axis.text = element_text(size = 5),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.key.width = unit(1, "cm"))
  })
  
  
    # ggsave(paste0(WD.1, "/Plots/Density/Entire region/",Species_vec[j],"_density_2023_",Sys.Date(),".png"),
    #      ggarrange(ggarrange(grid.arrange(Dens_plot_ER[[1]] +
    #                                         geom_sf(data = OSW_Scen1, fill = 'transparent',
    #                                                 colour = "red", lwd = .1),
    #                                       left = "Spatial Scenario 1"),
    #                          grid.arrange(Dens_plot_ER[[2]] +
    #                                         geom_sf(data = OSW_Scen2_outline, fill = 'transparent',
    #                                                 colour = "red", lwd = .1),
    #                                       left = "Spatial Scenario 2"),
    #                          grid.arrange(Dens_plot_ER[[3]] +
    #                                         geom_sf(data = OSW_Scen3, fill = 'transparent',
    #                                                 colour = "red", lwd = .1),
    #                                       left = "Spatial Scenario 3"),
    #                          ncol = 1, nrow = 3, align = "hv"),
    #                get_plot_component(Dens_plot_ER[[1]] +
    #                                     theme(legend.position = "right",
    #                                           legend.key.width = unit(.25, "cm")),
    #                                   "guide-box-right",return_all = T),
    #                nrow = 1, ncol = 2, widths = c(1.05,.07)),
    #      height = 8, width = 8.5, units = "in", dpi = 300, bg = "white")
    # 
  
  
  ggsave(paste0(WD.1, "/Plots/Density/Entire region/",Species_vec[j],"_density_2023_DEPTH_",Sys.Date(),".png"),
         ggarrange(ggarrange(grid.arrange(Dens_plot_ER2[[1]] +
                                            geom_sf(data = OSW_Scen1, fill = 'transparent',
                                                    colour = "red", lwd = .1),
                                          left = "Spatial Scenario 1"),
                             grid.arrange(Dens_plot_ER2[[2]] +
                                            geom_sf(data = OSW_Scen2_outline, fill = 'transparent',
                                                    colour = "red", lwd = .1),
                                          left = "Spatial Scenario 2"),
                             grid.arrange(Dens_plot_ER2[[3]] +
                                            geom_sf(data = OSW_Scen3, fill = 'transparent',
                                                    colour = "red", lwd = .1),
                                          left = "Spatial Scenario 3"),
                             ncol = 1, nrow = 3, align = "hv"),
                   get_plot_component(Dens_plot_ER2[[1]] +
                                        theme(legend.position = "right",
                                              legend.key.width = unit(.25, "cm")),
                                      "guide-box-right",return_all = T),
                   nrow = 1, ncol = 2, widths = c(1.05,.07)),
         height = 8, width = 8.5, units = "in", dpi = 300, bg = "white")
  
}

#Get the density maps for when depth is NOT included 
for(j in c(1,3)){
  print(paste0("Starting iteration for ", Species_vec[j]))
  Spec_data = All_species_nd[[j]]
  
  spec_list = list()
  #spec_list2 = list()
  
  for(i in 1:3){
    print(paste0("Starting iteration for ", OSW_all_scen[[i]]$Scenario[1]))
    
    if(Species_vec[j] %in% c("Sablefish", "Dover sole", "Rex sole", "Shortspine thornyhead", "Aurora rockfish")){
      spec_list[[i]] = Dens_plot_func2(Dens_data = Spec_data[[i]]$Density,
                                       #Dens_dif = Dover$Density_Dif,
                                       scen = year_df,
                                       Proj_extent = Projection_extent,
                                       OSW_shp = OSW_all_scen[[i]],
                                       Coast = coast_outline,
                                       lev_scen = c("All data", "1 year", "5 years", "10 years", "15 years", "19 years","Never surveyed"),
                                       Term.year = 2023)
      # spec_list2[[i]] = Dens_plot_func2(Dens_data = Spec_data[[i]]$Density,
      #                                   #Dens_dif = Dover$Density_Dif,
      #                                   scen = year_df,
      #                                   Proj_extent = Projection_extent,
      #                                   OSW_shp = OSW_all_scen[[i]],
      #                                   Coast = coast_outline,
      #                                   lev_scen = c("All data", "1 year", "5 years", "10 years", "15 years", "19 years","Never surveyed"),
      #                                   Term.year = 2023)
      
      
    }else{
      spec_list[[i]] = Dens_plot_func(Dens_data = Spec_data[[i]]$Density,
                                      #Dens_dif = Dover$Density_Dif,
                                      scen = year_df,
                                      Proj_extent = Projection_extent,
                                      OSW_shp = OSW_all_scen[[i]],
                                      Coast = coast_outline,
                                      lev_scen = c("All data", "1 year", "5 years", "10 years", "15 years", "19 years","Never surveyed"),
                                      Term.year = 2023)
    #   spec_list2[[i]] = Dens_plot_func(Dens_data = Spec_data[[i]]$Density,
    #                                    #Dens_dif = Dover$Density_Dif,
    #                                    scen = year_df,
    #                                    Proj_extent = Projection_extent,
    #                                    OSW_shp = OSW_all_scen[[i]],
    #                                    Coast = coast_outline,
    #                                    lev_scen = c("All data", "1 year", "5 years", "10 years", "15 years", "19 years","Never surveyed"),
    #                                    Term.year = 2023)
     }
    
  }
  plot.data = lapply(spec_list, "[[", 1)
  #plot.data2 = lapply(spec_list2, "[[", 1)
  
  Dens_list[[j]] = plot.data#Grab the density information accross the entire region
  #Dens_list2[[j]] = plot.data2#Grab the density information accross the entire region
  
  Dens_plot_ER = lapply(plot.data, function(x){
    ggplot(coast_outline)+
      geom_sf()+
      geom_tile(data = x, aes(x=X, y=Y, ,fill = Est))+
      facet_wrap(~scen, nrow = 1)+
      scale_fill_viridis_c(na.value = "yellow",
                           #midpoint = quantile(bind_rows(plot.data)$Est, 0.1),
                           limits = c(0, quantile(bind_rows(plot.data)$Est, 0.99)))+
      # scale_fill_gradient2(low = "#fcfbfd",
      #                      mid = "#efedf5",
      #                      high = "#3f007d",
      #                      na.value = "#3f007d",
      #                      midpoint = quantile(bind_rows(plot.data)$Est, 0.1),
      #                      limits = c(0, quantile(bind_rows(plot.data)$Est, 0.99)))+
      # scale_fill_gradient2(low = "#0571b0",
      #                      mid = "#f7f7f7",
      #                      high = "#ca0020",
      #                      limits = c(min(gr_pred_dif$Dif),
      #                                 max(gr_pred_dif$Dif))) +
      labs(x = "", y="") +
      theme_classic()+
      xlim(min(bind_rows(plot.data)$X),
           max(bind_rows(plot.data)$X))+
      ylim(min(bind_rows(plot.data)$Y),
           max(bind_rows(plot.data)$Y))+
      #geom_sf(data = OSW_shp, fill = 'transparent', colour = 'black', lwd = .05)+
      theme(#axis.text = element_text(size = 5),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.key.width = unit(1, "cm"))
  })
  
  # Dens_plot_ER2 = lapply(plot.data2, function(x){
  #   ggplot(coast_outline)+
  #     geom_sf()+
  #     geom_tile(data = x, aes(x=X, y=Y, ,fill = Est))+
  #     facet_wrap(~scen, nrow = 1)+
  #     scale_fill_viridis_c(na.value = "yellow",
  #                          #midpoint = quantile(bind_rows(plot.data)$Est, 0.1),
  #                          limits = c(0, quantile(bind_rows(plot.data)$Est, 0.99)))+
  #     # scale_fill_gradient2(low = "#0571b0",
  #     #                      mid = "#f7f7f7",
  #     #                      high = "#ca0020",
  #     #                      limits = c(min(gr_pred_dif$Dif),
  #     #                                 max(gr_pred_dif$Dif))) +
  #     labs(x = "", y="") +
  #     theme_classic()+
  #     xlim(min(bind_rows(plot.data)$X),
  #          max(bind_rows(plot.data)$X))+
  #     ylim(min(bind_rows(plot.data)$Y),
  #          max(bind_rows(plot.data)$Y))+
  #     #geom_sf(data = OSW_shp, fill = 'transparent', colour = 'black', lwd = .05)+
  #     theme(#axis.text = element_text(size = 5),
  #       axis.line = element_blank(),
  #       axis.ticks = element_blank(),
  #       axis.text = element_blank(),
  #       axis.title = element_blank(),
  #       legend.position = "none",
  #       legend.title = element_blank(),
  #       legend.key.width = unit(1, "cm"))
  # })
  
  
  ggsave(paste0(WD.1, "/Plots/Density/Entire region/",Species_vec[j],"_density_2023_",Sys.Date(),".png"),
       ggarrange(ggarrange(grid.arrange(Dens_plot_ER[[1]] +
                                          geom_sf(data = OSW_Scen1, fill = 'transparent',
                                                  colour = "red", lwd = .1),
                                        left = "Spatial Scenario 1"),
                           grid.arrange(Dens_plot_ER[[2]] +
                                          geom_sf(data = OSW_Scen2_outline, fill = 'transparent',
                                                  colour = "red", lwd = .1),
                                        left = "Spatial Scenario 2"),
                           grid.arrange(Dens_plot_ER[[3]] +
                                          geom_sf(data = OSW_Scen3, fill = 'transparent',
                                                  colour = "red", lwd = .1),
                                        left = "Spatial Scenario 3"),
                           ncol = 1, nrow = 3, align = "hv"),
                 get_plot_component(Dens_plot_ER[[1]] +
                                      theme(legend.position = "right",
                                            legend.key.width = unit(.25, "cm")),
                                    "guide-box-right",return_all = T),
                 nrow = 1, ncol = 2, widths = c(1.05,.07)),
       height = 8, width = 8.5, units = "in", dpi = 300, bg = "white")

  
  
  # ggsave(paste0(WD.1, "/Plots/Density/Entire region/",Species_vec[j],"_density_2023_DEPTH_",Sys.Date(),".png"),
  #        ggarrange(ggarrange(grid.arrange(Dens_plot_ER2[[1]] +
  #                                           geom_sf(data = OSW_Scen1, fill = 'transparent',
  #                                                   colour = "red", lwd = .1),
  #                                         left = "Spatial Scenario 1"),
  #                            grid.arrange(Dens_plot_ER2[[2]] +
  #                                           geom_sf(data = OSW_Scen2_outline, fill = 'transparent',
  #                                                   colour = "red", lwd = .1),
  #                                         left = "Spatial Scenario 2"),
  #                            grid.arrange(Dens_plot_ER2[[3]] +
  #                                           geom_sf(data = OSW_Scen3, fill = 'transparent',
  #                                                   colour = "red", lwd = .1),
  #                                         left = "Spatial Scenario 3"),
  #                            ncol = 1, nrow = 3, align = "hv"),
  #                  get_plot_component(Dens_plot_ER2[[1]] +
  #                                       theme(legend.position = "right",
  #                                             legend.key.width = unit(.25, "cm")),
  #                                     "guide-box-right",return_all = T),
  #                  nrow = 1, ncol = 2, widths = c(1.05,.07)),
  #        height = 8, width = 8.5, units = "in", dpi = 300, bg = "white")
  # 
}

############################################
#### Biomass Estimates ####
############################################
## Standardized index information ####

All_species_list = list(list(Aurora_nd, Aurora_d),list(Sablefish_nd, Sablefish_d),
                        list(Longspine_nd, Longspine_d), list(Shortspine_nd, Shortspine_d),
                        list(Dover_nd, Dover_d), list(Rex_nd, Rex_d))


Ind_list = list()
Ind_CV = list()
Ind_plot = list()
gc()
#Add in colors for the different temporal scenarios 
Scen_colors = c("Never surveyed" = "#440154FF",
                "19 years" = "#443A83FF",
                "15 years" = "#31688EFF",
                "10 years" = "#21908CFF",
                "5 years" = "#35B779FF",
                "1 year" = "#8FD744FF",
                "All data" = "#FDE725FF")

# Grab the data and label them for each model (with or without depth)
for(j in 1:6){
  print(paste0("Starting iteration for ", Species_vec[j]))
  Spec_data = All_species_list[[j]]
  
  scen_data = list()
  Ind_quick = list()
  Ind_Pm_CV = list()
  
  scen_data2 = list()
  Ind_quick2 = list()
  Ind_Pm_CV2 = list()
  for(i in 1:3){  
    print(paste0("Starting iteration for ", OSW_all_scen[[i]]$Scenario[1]))
    
    ## Original model ##
    Ind_quick[[i]] = Ind_func(dt1 = Spec_data[[1]][[i]]$Index,#Index data from the sdm function
                              Spec_title = Species_vec[j],
                              lev_scen = c("All data", "1 year", "5 years", "10 years", "15 years", "19 years","Never surveyed"),#Levels for the different scenarios
                              lev_osw = c("Entire region", "North", "South", "OSW", unique(OSW_all_scen[[i]]$group)))
    Ind_quick[[i]]$Scaled_data$Species = Species_vec[j]
    Ind_quick[[i]]$Scaled_data$Scenario = OSW_all_scen[[i]]$Scenario[1]
    Ind_quick[[i]]$Scaled_data$Model = "No depth"
    
    Ind_Pm_CV[[i]] = merge(Spec_data[[1]][[i]]$Index %>% filter(!year %in% 2020, group %in% "Entire region") %>%
                             mutate(cv = sqrt(exp(se^2) - 1)) %>%
                             group_by(scen, group) %>%
                             summarise(Mean_AD = mean(cv)) %>%
                             filter(scen == "All data") %>%
                             select(group, Mean_AD),
                           Spec_data[[1]][[i]]$Index %>% filter(!year %in% 2020, group %in% "Entire region") %>%
                             mutate(cv = sqrt(exp(se^2) - 1)) %>%
                             group_by(scen, group) %>%
                             summarise(Mean_cv = mean(cv)),
                           by = c("group")) %>%
      mutate(dif_CV = Mean_cv - Mean_AD)
    
    Ind_Pm_CV[[i]]$Species = Species_vec[j]
    Ind_Pm_CV[[i]]$Scenario = OSW_all_scen[[i]]$Scenario[1]
    Ind_Pm_CV[[i]]$Model = "No depth"
    
    scen_data[[i]] = Spec_data[[1]][[i]]$Index %>% filter(group == "Entire region") %>%
      mutate(est = est,
             upr = upr,
             lwr = lwr)
    scen_data[[i]]$Species = Species_vec[j]
    scen_data[[i]]$Scenario = OSW_all_scen[[i]]$Scenario[1]
    scen_data[[i]]$Model = "No depth"
    
    ## New model ##
    Ind_quick2[[i]] = Ind_func(dt1 = Spec_data[[2]][[i]]$Index,#Index data from the sdm function
                               Spec_title = Species_vec[j],
                               lev_scen = c("All data", "1 year", "5 years", "10 years", "15 years", "19 years","Never surveyed"),#Levels for the different scenarios
                               lev_osw = c("Entire region", "North", "South", "OSW", unique(OSW_all_scen[[i]]$group)))
    Ind_quick2[[i]]$Scaled_data$Species = Species_vec[j]
    Ind_quick2[[i]]$Scaled_data$Scenario = OSW_all_scen[[i]]$Scenario[1]
    Ind_quick2[[i]]$Scaled_data$Model = "Depth"
    
    Ind_Pm_CV2[[i]] = merge(Spec_data[[2]][[i]]$Index %>% filter(!year %in% 2020, group %in% "Entire region") %>%
                              mutate(cv = sqrt(exp(se^2) - 1)) %>%
                              group_by(scen, group) %>%
                              summarise(Mean_AD = mean(cv)) %>%
                              filter(scen == "All data") %>%
                              select(group, Mean_AD),
                            Spec_data[[2]][[i]]$Index %>% filter(!year %in% 2020, group %in% "Entire region") %>%
                              mutate(cv = sqrt(exp(se^2) - 1)) %>%
                              group_by(scen, group) %>%
                              summarise(Mean_cv = mean(cv)),
                            by = c("group")) %>%
      mutate(dif_CV = Mean_cv - Mean_AD)
    
    Ind_Pm_CV2[[i]]$Species = Species_vec[j]
    Ind_Pm_CV2[[i]]$Scenario = OSW_all_scen[[i]]$Scenario[1]
    Ind_Pm_CV2[[i]]$Model = "Depth"
    
    scen_data2[[i]] = Spec_data[[2]][[i]]$Index %>% filter(group == "Entire region") %>%
      mutate(est = est,
             upr = upr,
             lwr = lwr)
    scen_data2[[i]]$Species = Species_vec[j]
    scen_data2[[i]]$Scenario = OSW_all_scen[[i]]$Scenario[1]
    scen_data2[[i]]$Model = "Depth"
    
    
  }
  plot.data = bind_rows(lapply(Ind_quick, "[[", 1),lapply(Ind_quick2, "[[", 1))
  Ind_list[[j]] = plot.data#Grab the density information accross the entire region
  
  Ind_CV[[j]] = bind_rows(Ind_Pm_CV, Ind_Pm_CV2)
  
  plot_data = bind_rows(scen_data, scen_data2)
  
  plot_data$Species = factor(plot_data$Species, levels = c("Dover sole", "Sablefish", "Shortspine thornyhead", 
                                                           "Longspine thornyhead","Aurora rockfish", "Rex sole"), 
                             labels =  c("Dover", "Sablefish", "Shortspine", "Longspine","Aurora", "Rex"))
  
  plot_data$Scenario = factor(plot_data$Scenario, levels = c("Scenario 1", "Scenario 2", "Scenario 3"), 
                              labels =  c("Spatial 1", "Spatial 2", "Spatial 3"))
  
  plot_data = plot_data %>% 
    mutate(lower = floor(min(na.omit(lwr))),
           range = round((ceiling(max(na.omit(upr))) - floor(min(na.omit(lwr))))/2,0)+1
    )%>%
    mutate(upper = lower + (range*2))
  
  
  Ind_plot[[j]] = ggplot(data = plot_data,
                         aes(x = year, y = est, ymin = lwr, ymax = upr, colour = scen, fill = scen)) + 
    geom_ribbon(alpha = 0.08, colour = NA) +
    geom_line(lwd = .4) +
    xlab('') + ylab(' ')+
    #labs(title = paste0(Spec_title)) + 
    scale_colour_manual(name = "Scenario", values = Scen_colors)+
    scale_fill_manual(name = "Scenario", values = Scen_colors)+
    scale_linetype(name = "Scenario")+
    facet_nested(Species + Model ~ Scenario, 
                 scales = "free_y",
                 nest_line = element_line(linetype = 1))+
    theme_classic() +
    guides(colour = guide_legend(nrow = 1)) +
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0), limits = c(plot_data$lower[1],plot_data$upper[1]), 
                       breaks = seq(plot_data$lower[1],plot_data$upper[1],plot_data$range[1])
                       #limits = c(0,max(round(na.omit(DTS_Scaled_Current$upr_s),0)))
    )+
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          panel.spacing.y = unit(.75, "lines"),
          #strip.background.x = element_blank(),
          ggh4x.facet.nestline = element_line(colour = "grey20"),
          strip.placement = "outside")+
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)
  
  
}


# Now organize all the data for plotting - specifically give columns the correct titles for facets
Scaled_Ind = bind_rows(Ind_list)
Scaled_Ind$Species = factor(Scaled_Ind$Species, levels = c("Dover sole", "Sablefish", "Shortspine thornyhead", 
                                                           "Longspine thornyhead","Aurora rockfish", "Rex sole"), 
                            labels =  c("Dover", "Sablefish", "Shortspine", "Longspine","Aurora", "Rex"))

Scaled_Ind$Scenario = factor(Scaled_Ind$Scenario, levels = c("Scenario 1", "Scenario 2", "Scenario 3"), 
                             labels =  c("Spatial 1", "Spatial 2", "Spatial 3"))

Ind_CV = bind_rows(Ind_CV)
Ind_CV$Species = factor(Ind_CV$Species, levels = c("Dover sole", "Sablefish", "Shortspine thornyhead", 
                                                   "Longspine thornyhead","Aurora rockfish", "Rex sole"), 
                        labels =  c("Dover", "Sablefish", "Shortspine", "Longspine","Aurora", "Rex"))

Ind_CV$Scenario = factor(Ind_CV$Scenario, levels = c("Scenario 1", "Scenario 2", "Scenario 3"), 
                         labels =  c("Spatial 1", "Spatial 2", "Spatial 3"))


lapply(split(Scaled_Ind %>% filter(group %in% "Entire region"),Scaled_Ind%>% filter(group %in% "Entire region") %>% select(Species)), summary)

Scaled_Ind_ER = Scaled_Ind %>% filter(group %in% "Entire region") %>% 
  group_by(Species) %>% 
  mutate(upper = ifelse(round(max(na.omit(upr_s/2)),1)*2<10,
                        round(max(na.omit(upr_s/2)),1)*2,
                        ceiling((ceiling(max(na.omit(upr_s/2)))*2))))

Scaled_Ind %>% filter(group %in% "Entire region") %>% 
  group_by(Species) %>% 
  summarise(upper = ifelse(round(max(na.omit(upr_s/2)),1)*2<10,
                           round(max(na.omit(upr_s/2)),1)*2,
                           ceiling((ceiling(max(na.omit(upr_s/2)))*2))),
            up1 = round(max(na.omit(upr_s)),2))

### Mean scaled Biomass ####

Scal_ind_plot_b = list()
Scal_ind_plot_d = list()
Scal_ind_plot_nd = list()

## Now plot all the mean scaled indices on a common access - mean scaled to the all data scenario
for(i in 1:6){
  plot_data = Scaled_Ind_ER %>% filter(Species %in% unique(Scaled_Ind_ER$Species)[i],
                                       group %in% "Entire region")
  Scal_ind_plot_b[[i]] = ggplot(plot_data, 
                              aes(as.numeric(as.character(year)), est_s, colour = scen, fill = scen, lty = scen)) +
    geom_hline(yintercept = 1, lwd=.4, lty = 2)+
    geom_ribbon(aes(ymin = lwr_s, ymax = upr_s), alpha = 0.08, colour = NA) +
    geom_line(lwd = .4) +
    xlab('') + ylab(' ')+
    #labs(title = paste0(Spec_title)) + 
    scale_colour_manual(name = "Scenario", values = Scen_colors)+
    scale_fill_manual(name = "Scenario", values = Scen_colors)+
    scale_linetype(name = "Scenario")+
    facet_nested(Species + Model~ Scenario, 
                 scales = "free_y",
                 nest_line = element_line(linetype = 1))+
    theme_classic() +
    guides(colour = guide_legend(nrow = 1)) +
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0), limits = c(0,plot_data$upper[1]), 
                       breaks = seq(0,plot_data$upper[1],plot_data$upper[1]/2)
                       #limits = c(0,max(round(na.omit(DTS_Scaled_Current$upr_s),0)))
    )+
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.line = element_line(linewidth = 0.25),
          axis.ticks = element_line(linewidth = 0.25),
          panel.spacing.y = unit(.75, "lines"),
          #strip.background.x = element_blank(),
          ggh4x.facet.nestline = element_line(colour = "grey20"),
          strip.placement = "outside")+
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = 0.25)+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd = 0.25)
  
  Scal_ind_plot_nd[[i]] = ggplot(plot_data %>% filter(Model=="No depth"), 
                                aes(as.numeric(as.character(year)), est_s, colour = scen, fill = scen, lty = scen)) +
    geom_hline(yintercept = 1, lwd=.4, lty = 2)+
    geom_ribbon(aes(ymin = lwr_s, ymax = upr_s), alpha = 0.08, colour = NA) +
    geom_line(lwd = .4) +
    xlab('') + ylab(' ')+
    #labs(title = paste0(Spec_title)) + 
    scale_colour_manual(name = "Scenario", values = Scen_colors)+
    scale_fill_manual(name = "Scenario", values = Scen_colors)+
    scale_linetype(name = "Scenario")+
    facet_nested(Species ~ Scenario, 
                 scales = "free_y",
                 nest_line = element_line(linetype = 1))+
    theme_classic() +
    guides(colour = guide_legend(nrow = 1)) +
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0), limits = c(0,plot_data$upper[1]), 
                       breaks = seq(0,plot_data$upper[1],plot_data$upper[1]/2)
                       #limits = c(0,max(round(na.omit(DTS_Scaled_Current$upr_s),0)))
    )+
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.line = element_line(linewidth = 0.25),
          axis.ticks = element_line(linewidth = 0.25),
          panel.spacing.y = unit(.75, "lines"),
          #strip.background.x = element_blank(),
          ggh4x.facet.nestline = element_line(colour = "grey20"),
          strip.placement = "outside")+
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = 0.25)+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd = 0.25)
  
  Scal_ind_plot_d[[i]] = ggplot(plot_data %>% filter(Model=="Depth"), 
                                 aes(as.numeric(as.character(year)), est_s, colour = scen, fill = scen, lty = scen)) +
    geom_hline(yintercept = 1, lwd=.4, lty = 2)+
    geom_ribbon(aes(ymin = lwr_s, ymax = upr_s), alpha = 0.08, colour = NA) +
    geom_line(lwd = .4) +
    xlab('') + ylab(' ')+
    #labs(title = paste0(Spec_title)) + 
    scale_colour_manual(name = "Scenario", values = Scen_colors)+
    scale_fill_manual(name = "Scenario", values = Scen_colors)+
    scale_linetype(name = "Scenario")+
    facet_nested(Species ~ Scenario, 
                 scales = "free_y",
                 nest_line = element_line(linetype = 1))+
    theme_classic() +
    guides(colour = guide_legend(nrow = 1)) +
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0), limits = c(0,plot_data$upper[1]), 
                       breaks = seq(0,plot_data$upper[1],plot_data$upper[1]/2)
                       #limits = c(0,max(round(na.omit(DTS_Scaled_Current$upr_s),0)))
    )+
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.line = element_line(linewidth = 0.25),
          axis.ticks = element_line(linewidth = 0.25),
          panel.spacing.y = unit(.75, "lines"),
          #strip.background.x = element_blank(),
          ggh4x.facet.nestline = element_line(colour = "grey20"),
          strip.placement = "outside")+
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = 0.25)+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd = 0.25)
}
## Organize the plots for both depth and no depth ####
# Depth ####
Scal_ind_plot_d[[1]] # Aurora - looks good 
Scal_ind_plot_d[[2]] # Sablefish - needs work on y axis 
Scal_ind_plot_d[[2]] = Scal_ind_plot_d[[2]] +
  scale_y_continuous(expand = c(0,0), limits = c(0,3.8), 
                     breaks = seq(0,3.8,3.8/2)
                     #limits = c(0,max(round(na.omit(DTS_Scaled_Current$upr_s),0)))
  )+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.line = element_line(linewidth = 0.25),
        axis.ticks = element_line(linewidth = 0.25),
        panel.spacing.y = unit(.75, "lines"),
        #strip.background.x = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "grey20"),
        strip.placement = "outside")+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = 0.25)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd = 0.25)

Scal_ind_plot_d[[3]] # Longspine -  needs work on y axis 

Scal_ind_plot_d[[3]] = Scal_ind_plot_d[[3]] +
  scale_y_continuous(expand = c(0,0), limits = c(0,2.2), 
                     breaks = seq(0,2.2,2.2/2)
                     #limits = c(0,max(round(na.omit(DTS_Scaled_Current$upr_s),0)))
  )+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.line = element_line(linewidth = 0.25),
        axis.ticks = element_line(linewidth = 0.25),
        panel.spacing.y = unit(.75, "lines"),
        #strip.background.x = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "grey20"),
        strip.placement = "outside")+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = 0.25)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd = 0.25)

Scal_ind_plot_d[[4]] # Shortspine - good  
Scal_ind_plot_d[[5]] # Dover - good  
Scal_ind_plot_d[[6]] # rex - good  

# Without depth ####
# Value check for manuscript 
Scaled_Ind_ER %>% filter(Species %in% unique(Scaled_Ind_ER$Species)[2],
                         group %in% "Entire region",
                         Model =="No depth",
                         year == 2023,
                         Scenario == "Spatial 3")

Scaled_Ind_ER %>% filter(Species %in% unique(Scaled_Ind_ER$Species)[3],
                         group %in% "Entire region",
                         Model =="No depth",
                         year == 2023,
                         Scenario == "Spatial 3") %>% data.frame()

Scal_ind_plot_nd[[1]] # Aurora - looks good 
Scal_ind_plot_nd[[2]] # Sablefish - needs work on y axis *BUT ALSO WANT ONE WITH THIS FULL AXIS FOR SUPPLEMENTARY  
Sablefish_nd_short= ggplot(Scaled_Ind_ER %>% filter(Species %in% unique(Scaled_Ind_ER$Species)[2],
                                                    group %in% "Entire region",
                                                    Model =="No depth"), 
                           aes(as.numeric(as.character(year)), est_s, colour = scen, fill = scen, lty = scen)) +
  geom_hline(yintercept = 1, lwd=.4, lty = 2)+
  geom_ribbon(aes(ymin = lwr_s, ymax = ifelse(upr_s>3.8,3.8,upr_s)), alpha = 0.08, colour = NA) +
  geom_line(lwd = .4) +
  xlab('') + ylab(' ')+
  #labs(title = paste0(Spec_title)) + 
  scale_colour_manual(name = "Scenario", values = Scen_colors)+
  scale_fill_manual(name = "Scenario", values = Scen_colors)+
  scale_linetype(name = "Scenario")+
  facet_nested(Species ~ Scenario, 
               scales = "free_y",
               nest_line = element_line(linetype = 1))+
  theme_classic() +
  guides(colour = guide_legend(nrow = 1)) +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), limits = c(0,3.8), 
                     breaks = seq(0,3.8,3.8/2)
                     #limits = c(0,max(round(na.omit(DTS_Scaled_Current$upr_s),0)))
  )+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.line = element_line(linewidth = 0.25),
        axis.ticks = element_line(linewidth = 0.25),
        panel.spacing.y = unit(.75, "lines"),
        #strip.background.x = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "grey20"),
        strip.placement = "outside")+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = 0.25)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd = 0.25)

Scal_ind_plot_nd[[3]] # Longspine -  needs work on y axis but also want the full axis  
Longspine_nd_short= ggplot(Scaled_Ind_ER %>% filter(Species %in% unique(Scaled_Ind_ER$Species)[3],
                                                    group %in% "Entire region",
                                                    Model =="No depth"), 
                           aes(as.numeric(as.character(year)), est_s, colour = scen, fill = scen, lty = scen)) +
  geom_hline(yintercept = 1, lwd=.4, lty = 2)+
  geom_ribbon(aes(ymin = lwr_s, ymax = ifelse(upr_s>3.2,3.2,upr_s)), alpha = 0.08, colour = NA) +
  geom_line(lwd = .4) +
  xlab('') + ylab(' ')+
  #labs(title = paste0(Spec_title)) + 
  scale_colour_manual(name = "Scenario", values = Scen_colors)+
  scale_fill_manual(name = "Scenario", values = Scen_colors)+
  scale_linetype(name = "Scenario")+
  facet_nested(Species ~ Scenario, 
               scales = "free_y",
               nest_line = element_line(linetype = 1))+
  theme_classic() +
  guides(colour = guide_legend(nrow = 1)) +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), limits = c(0,3.2), 
                     breaks = seq(0,3.2,3.2/2)
                     #limits = c(0,max(round(na.omit(DTS_Scaled_Current$upr_s),0)))
  )+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.line = element_line(linewidth = 0.25),
        axis.ticks = element_line(linewidth = 0.25),
        panel.spacing.y = unit(.75, "lines"),
        #strip.background.x = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "grey20"),
        strip.placement = "outside")+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = 0.25)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd = 0.25)

Scal_ind_plot_nd[[3]] = Scal_ind_plot_nd[[3]] +
  scale_y_continuous(expand = c(0,0), 
                     breaks = seq(0,3766,3766/2),
                     limits = c(0,3766)
  )+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.line = element_line(linewidth = 0.25),
        axis.ticks = element_line(linewidth = 0.25),
        panel.spacing.y = unit(.75, "lines"),
        #strip.background.x = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "grey20"),
        strip.placement = "outside")+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = 0.25)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd = 0.25)

Scal_ind_plot_nd[[4]] # Shortspine - good  
Scal_ind_plot_nd[[5]] # Dover - need to adjust y axis 
Scal_ind_plot_nd[[5]] = Scal_ind_plot_nd[[5]] +
  scale_y_continuous(expand = c(0,0), limits = c(0,2.8), 
                     breaks = seq(0,2.8,2.8/2)
                     #limits = c(0,max(round(na.omit(DTS_Scaled_Current$upr_s),0)))
  )+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.line = element_line(linewidth = 0.25),
        axis.ticks = element_line(linewidth = 0.25),
        panel.spacing.y = unit(.75, "lines"),
        #strip.background.x = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "grey20"),
        strip.placement = "outside")+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = 0.25)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd = 0.25)
Scal_ind_plot_nd[[6]] # rex - good  

# Plots for manuscript and supplementary ####
# No depth ####
gridExtra::grid.arrange(ggarrange(
  ggarrange(
    ggarrange(NULL, 
              Scal_ind_plot_nd[[5]] + 
                theme(axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      legend.position = "none"),
              ncol = 2, widths = c(0,1)), NULL,
    ggarrange(NULL, Sablefish_nd_short + 
                theme(axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      legend.position = "none",
                      strip.text.x = element_blank(),
                      strip.background.x = element_blank()),
              ncol = 2, widths = c(0,1)), NULL,
    ggarrange(NULL,Scal_ind_plot_nd[[4]] + 
                theme(axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      legend.position = "none",
                      strip.text.x = element_blank(),
                      strip.background.x = element_blank()),
              ncol = 2, widths = c(0, 1)),NULL,
    ggarrange(NULL, Longspine_nd_short + 
                theme(axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      legend.position = "none",
                      strip.text.x = element_blank(),
                      strip.background.x = element_blank()),
              ncol = 2, widths = c(-0,1)),NULL,
    ggarrange(NULL, Scal_ind_plot_nd[[1]] + 
                theme(axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      legend.position = "none",
                      strip.text.x = element_blank(),
                      strip.background.x = element_blank()),
              ncol = 2, widths = c(-0, 1)),NULL,
    Scal_ind_plot_nd[[6]] + 
      theme(legend.position = "none",
            strip.text.x = element_blank(),
            strip.background.x = element_blank()),
    nrow = 11, 
    ncol = 1, heights = c(1.25,-.10, 1,-.10,1,-.10,1,-.10,1,-.10,1.10),
    labels = c("","","*","","","","*","","","",""), hjust = -3, font.label = c(size = 10)),
  get_plot_component(Scal_ind_plot_nd[[1]],
                     "guide-box-bottom",return_all = T),
  ncol=1, heights = c(1.4,.05)), left = "Mean scaled biomass estimates", 
  padding = unit(1, "line"))

ggsave(paste0(WD.1, "/Plots/Index/Scaled Index ",Sys.Date(),".png"),
       gridExtra::grid.arrange(ggarrange(
         ggarrange(
           ggarrange(NULL, 
                     Scal_ind_plot_nd[[5]] + 
                       theme(axis.text.x = element_blank(),
                             axis.ticks.x = element_blank(),
                             legend.position = "none"),
                     ncol = 2, widths = c(0,1)), NULL,
           ggarrange(NULL, Sablefish_nd_short + 
                       theme(axis.text.x = element_blank(),
                             axis.ticks.x = element_blank(),
                             legend.position = "none",
                             strip.text.x = element_blank(),
                             strip.background.x = element_blank()),
                     ncol = 2, widths = c(0,1)), NULL,
           ggarrange(NULL,Scal_ind_plot_nd[[4]] + 
                       theme(axis.text.x = element_blank(),
                             axis.ticks.x = element_blank(),
                             legend.position = "none",
                             strip.text.x = element_blank(),
                             strip.background.x = element_blank()),
                     ncol = 2, widths = c(0, 1)),NULL,
           ggarrange(NULL, Longspine_nd_short + 
                       theme(axis.text.x = element_blank(),
                             axis.ticks.x = element_blank(),
                             legend.position = "none",
                             strip.text.x = element_blank(),
                             strip.background.x = element_blank()),
                     ncol = 2, widths = c(-0,1)),NULL,
           ggarrange(NULL, Scal_ind_plot_nd[[1]] + 
                       theme(axis.text.x = element_blank(),
                             axis.ticks.x = element_blank(),
                             legend.position = "none",
                             strip.text.x = element_blank(),
                             strip.background.x = element_blank()),
                     ncol = 2, widths = c(-0, 1)),NULL,
           Scal_ind_plot_nd[[6]] + 
             theme(legend.position = "none",
                   strip.text.x = element_blank(),
                   strip.background.x = element_blank()),
           nrow = 11, 
           ncol = 1, heights = c(1.25,-.10, 1,-.10,1,-.10,1,-.10,1,-.10,1.10),
           labels = c("","","*","","","","*","","","",""), hjust = -3, font.label = c(size = 10)),
         get_plot_component(Scal_ind_plot_nd[[1]],
                            "guide-box-bottom",return_all = T),
         ncol=1, heights = c(1.4,.05)), left = "Mean scaled biomass estimates", 
         padding = unit(1, "line")),
       height = 230, width = 170, units = "mm", dpi = 300, bg = "white")

## No depth - full scale for sablefish and longspine ##
ggsave(paste0(WD.1, "/Plots/Index/Scaled Index ",Sys.Date(),"_Sup1.png"),
       gridExtra::grid.arrange(ggarrange(
         ggarrange(
           ggarrange(NULL, 
                     Scal_ind_plot_nd[[2]] + 
                       theme(axis.text.x = element_blank(),
                             axis.ticks.x = element_blank(),
                             legend.position = "none"),
                     ncol = 2, widths = c(.01,1)), NULL,
           ggarrange(NULL,Scal_ind_plot_nd[[3]] + 
                       theme(#axis.text.x = element_blank(),
                             #axis.ticks.x = element_blank(),
                             legend.position = "none",
                             strip.text.x = element_blank(),
                             strip.background.x = element_blank()),
                     ncol = 2, widths = c(0, 1)),
           nrow = 3, 
           ncol = 1, heights = c(1.25,-.05, 1.10)),
         get_plot_component(Scal_ind_plot_nd[[1]],
                            "guide-box-bottom",return_all = T),
         ncol=1, heights = c(1.4,.05)), left = "Mean scaled biomass estimates", 
         padding = unit(1, "line")),
       height = (230/6)*2.25, width = 170, units = "mm", dpi = 300, bg = "white")


# Depth ####
gridExtra::grid.arrange(ggarrange(
  ggarrange(
    ggarrange(NULL, 
              Scal_ind_plot_d[[5]] + 
                theme(axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      legend.position = "none"),
              ncol = 2, widths = c(0,1)), NULL,
    ggarrange(NULL, Scal_ind_plot_d[[2]] + 
                theme(axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      legend.position = "none",
                      strip.text.x = element_blank(),
                      strip.background.x = element_blank()),
              ncol = 2, widths = c(0,1)), NULL,
    ggarrange(NULL,Scal_ind_plot_d[[4]] + 
                theme(axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      legend.position = "none",
                      strip.text.x = element_blank(),
                      strip.background.x = element_blank()),
              ncol = 2, widths = c(0, 1)),NULL,
    ggarrange(NULL, Scal_ind_plot_d[[3]] + 
                theme(axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      legend.position = "none",
                      strip.text.x = element_blank(),
                      strip.background.x = element_blank()),
              ncol = 2, widths = c(-0,1)),NULL,
    ggarrange(NULL, Scal_ind_plot_d[[1]] + 
                theme(axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      legend.position = "none",
                      strip.text.x = element_blank(),
                      strip.background.x = element_blank()),
              ncol = 2, widths = c(-0, 1)),NULL,
    Scal_ind_plot_d[[6]] + 
      theme(legend.position = "none",
            strip.text.x = element_blank(),
            strip.background.x = element_blank()),
    nrow = 11, 
    ncol = 1, heights = c(1.25,-.10, 1,-.10,1,-.10,1,-.10,1,-.10,1.10),
    labels = c("","","*","","","","*","","","",""), hjust = -3, font.label = c(size = 10)),
  get_plot_component(Scal_ind_plot_d[[1]],
                     "guide-box-bottom",return_all = T),
  ncol=1, heights = c(1.4,.05)), left = "Mean scaled biomass estimates", 
  padding = unit(1, "line"))

ggsave(paste0(WD.1, "/Plots/Index/Scaled Index depth",Sys.Date(),".png"),
       gridExtra::grid.arrange(ggarrange(
         ggarrange(
           ggarrange(NULL, 
                     Scal_ind_plot_d[[5]] + 
                       theme(axis.text.x = element_blank(),
                             axis.ticks.x = element_blank(),
                             legend.position = "none"),
                     ncol = 2, widths = c(0,1)), NULL,
           ggarrange(NULL, Scal_ind_plot_d[[2]] + 
                       theme(axis.text.x = element_blank(),
                             axis.ticks.x = element_blank(),
                             legend.position = "none",
                             strip.text.x = element_blank(),
                             strip.background.x = element_blank()),
                     ncol = 2, widths = c(0,1)), NULL,
           ggarrange(NULL,Scal_ind_plot_d[[4]] + 
                       theme(axis.text.x = element_blank(),
                             axis.ticks.x = element_blank(),
                             legend.position = "none",
                             strip.text.x = element_blank(),
                             strip.background.x = element_blank()),
                     ncol = 2, widths = c(0, 1)),NULL,
           ggarrange(NULL, Scal_ind_plot_d[[3]] + 
                       theme(axis.text.x = element_blank(),
                             axis.ticks.x = element_blank(),
                             legend.position = "none",
                             strip.text.x = element_blank(),
                             strip.background.x = element_blank()),
                     ncol = 2, widths = c(-0,1)),NULL,
           ggarrange(NULL, Scal_ind_plot_d[[1]] + 
                       theme(axis.text.x = element_blank(),
                             axis.ticks.x = element_blank(),
                             legend.position = "none",
                             strip.text.x = element_blank(),
                             strip.background.x = element_blank()),
                     ncol = 2, widths = c(-0, 1)),NULL,
           Scal_ind_plot_d[[6]] + 
             theme(legend.position = "none",
                   strip.text.x = element_blank(),
                   strip.background.x = element_blank()),
           nrow = 11, 
           ncol = 1, heights = c(1.25,-.10, 1,-.10,1,-.10,1,-.10,1,-.10,1.10),
           labels = c("","","*","","","","*","","","",""), hjust = -3, font.label = c(size = 10)),
         get_plot_component(Scal_ind_plot_d[[1]],
                            "guide-box-bottom",return_all = T),
         ncol=1, heights = c(1.4,.05)), left = "Mean scaled biomass estimates", 
         padding = unit(1, "line")),
       height = 230, width = 170, units = "mm", dpi = 300, bg = "white")


## North and South analysis ####
# For this analysis we are only looking at the no depth models 

Scaled_Ind_NS = Scaled_Ind %>% filter(group %in% c("North", "South"),
                                      Model == "No depth") %>% 
  group_by(Species) %>% 
  mutate(upper = ifelse(round(max(na.omit(upr_s/2)),1)*2<10,
                        round(max(na.omit(upr_s/2)),1)*2,
                        ceiling((ceiling(max(na.omit(upr_s/2)))*2))))

Scal_ind_plot_N = list()
for(i in 1:6){
  plot_data = Scaled_Ind_NS %>% filter(Species %in% unique(Scaled_Ind_NS$Species)[i],
                                       group %in% "North")
  Scal_ind_plot_N[[i]] = ggplot(plot_data, 
                                aes(as.numeric(as.character(year)), est_s, colour = scen, fill = scen, lty = scen)) +
    geom_hline(yintercept = 1, lwd=.4, lty = 2)+
    geom_ribbon(aes(ymin = lwr_s, ymax = upr_s), alpha = 0.08, colour = NA) +
    geom_line(lwd = .4) +
    xlab('') + ylab(' ')+
    #labs(title = paste0(Spec_title)) + 
    scale_colour_manual(name = "Scenario", values = Scen_colors)+
    scale_fill_manual(name = "Scenario", values = Scen_colors)+
    scale_linetype(name = "Scenario")+
    facet_nested(Species~ Scenario, 
                 scales = "free_y",
                 nest_line = element_line(linetype = 1))+
    theme_classic() +
    guides(colour = guide_legend(nrow = 1)) +
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0), limits = c(0,plot_data$upper[1]), 
                       breaks = seq(0,plot_data$upper[1],plot_data$upper[1]/2)
                       #limits = c(0,max(round(na.omit(DTS_Scaled_Current$upr_s),0)))
    )+
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          panel.spacing.y = unit(.75, "lines"),
          #strip.background.x = element_blank(),
          ggh4x.facet.nestline = element_line(colour = "grey20"),
          strip.placement = "outside")+
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)
}

Scal_ind_plot_S = list()
for(i in 1:6){
  plot_data = Scaled_Ind_NS %>% filter(Species %in% unique(Scaled_Ind_NS$Species)[i],
                                       group %in% "South")
  Scal_ind_plot_S[[i]] = ggplot(plot_data, 
                                aes(as.numeric(as.character(year)), est_s, colour = scen, fill = scen, lty = scen)) +
    geom_hline(yintercept = 1, lwd=.4, lty = 2)+
    geom_ribbon(aes(ymin = lwr_s, ymax = upr_s), alpha = 0.08, colour = NA) +
    geom_line(lwd = .4) +
    xlab('') + ylab(' ')+
    #labs(title = paste0(Spec_title)) + 
    scale_colour_manual(name = "Scenario", values = Scen_colors)+
    scale_fill_manual(name = "Scenario", values = Scen_colors)+
    scale_linetype(name = "Scenario")+
    facet_nested(Species~ Scenario, 
                 scales = "free_y",
                 nest_line = element_line(linetype = 1))+
    theme_classic() +
    guides(colour = guide_legend(nrow = 1)) +
    scale_x_continuous(expand = c(0,0))+
    scale_y_continuous(expand = c(0,0), limits = c(0,plot_data$upper[1]), 
                       breaks = seq(0,plot_data$upper[1],plot_data$upper[1]/2)
                       #limits = c(0,max(round(na.omit(DTS_Scaled_Current$upr_s),0)))
    )+
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          panel.spacing.y = unit(.75, "lines"),
          #strip.background.x = element_blank(),
          ggh4x.facet.nestline = element_line(colour = "grey20"),
          strip.placement = "outside")+
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)
}
# Based on visual examination for the different models I need to include insets for both longspine and sablefish 
# Sablefish ####

inset_Sab_S=ggplot(Scaled_Ind_NS %>% filter(Species %in% unique(Scaled_Ind_NS$Species)[2],
                                           group %in% "South",
                                           Scenario %in% "Spatial 3",
                                           Model=="No depth"), 
                  aes(as.numeric(as.character(year)), est_s, colour = scen, fill = scen, lty = scen)) +
  geom_hline(yintercept = 1, lwd=.4, lty = 2)+
  geom_ribbon(aes(ymin = lwr_s, ymax = upr_s), alpha = 0.08, colour = NA) +
  geom_line(lwd = .4) +
  xlab('') + ylab(' ')+
  #labs(title = paste0(Spec_title)) + 
  scale_colour_manual(name = "Scenario", values = Scen_colors)+
  scale_fill_manual(name = "Scenario", values = Scen_colors)+
  scale_linetype(name = "Scenario")+
  facet_nested(Species~ Scenario, 
               scales = "free_y",
               nest_line = element_line(linetype = 1))+
  theme_classic() +
  guides(colour = guide_legend(nrow = 1)) +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), limits = c(0,710), 
                     breaks = seq(0,710,710/2)
                     #limits = c(0,max(round(na.omit(DTS_Scaled_Current$upr_s),0)))
  )+
  theme(legend.position = "none",
        axis.title = element_blank(),
        legend.title = element_blank(),
        axis.line = element_line(linewidth = 0.25),
        axis.ticks = element_line(linewidth = 0.25),
        plot.background = element_rect(fill = "white"),
        panel.spacing.y = unit(0, "lines"),
        #strip.background.x = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "grey20")                                            ,
        strip.text = element_blank(),
        axis.text.y = element_text(size = 7),
        strip.background = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        strip.placement = "outside")+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = 0.25)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd = 0.25)

inset_Sab_N=ggplot(Scaled_Ind_NS %>% filter(Species %in% unique(Scaled_Ind_NS$Species)[2],
                                            group %in% "North",
                                            Scenario %in% "Spatial 3",
                                            Model=="No depth"), 
                   aes(as.numeric(as.character(year)), est_s, colour = scen, fill = scen, lty = scen)) +
  geom_hline(yintercept = 1, lwd=.4, lty = 2)+
  geom_ribbon(aes(ymin = lwr_s, ymax = upr_s), alpha = 0.08, colour = NA) +
  geom_line(lwd = .4) +
  xlab('') + ylab(' ')+
  #labs(title = paste0(Spec_title)) + 
  scale_colour_manual(name = "Scenario", values = Scen_colors)+
  scale_fill_manual(name = "Scenario", values = Scen_colors)+
  scale_linetype(name = "Scenario")+
  facet_nested(Species~ Scenario, 
               scales = "free_y",
               nest_line = element_line(linetype = 1))+
  theme_classic() +
  guides(colour = guide_legend(nrow = 1)) +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), limits = c(0,200), 
                     breaks = seq(0,200,200/2)
                     #limits = c(0,max(round(na.omit(DTS_Scaled_Current$upr_s),0)))
  )+
  theme(legend.position = "none",
        axis.title = element_blank(),
        legend.title = element_blank(),
        axis.line = element_line(linewidth = 0.25),
        axis.ticks = element_line(linewidth = 0.25),
        plot.background = element_rect(fill = "white"),
        panel.spacing.y = unit(0, "lines"),
        #strip.background.x = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "grey20")                                            ,
        strip.text = element_blank(),
        axis.text.y = element_text(size = 7),
        strip.background = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        strip.placement = "outside")+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = 0.25)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd = 0.25)

data.tb_S <- tibble(x = c(2008,2008, 2003.5), 
                  y = c(1.5, -1.4,2.95), 
                  Scenario = factor(c("Spatial 1", "Spatial 2", "Spatial 3")),
                  plot = list(ggplot + geom_blank(), ggplot+geom_blank(), inset_Sab_S))

data.tb_N <- tibble(x = c(2008,2008, 2003.5), 
                    y = c(1.5, -1.4,2.95), 
                    Scenario = factor(c("Spatial 1", "Spatial 2", "Spatial 3")),
                    plot = list(ggplot + geom_blank(), ggplot+geom_blank(), inset_Sab_N))


Scal_ind_plot_N[[2]] = ggplot(Scaled_Ind_NS %>% filter(Species %in% unique(Scaled_Ind_NS$Species)[2],
                                                       group %in% "North",
                                                       Model=="No depth"), 
                              aes(as.numeric(as.character(year)), est_s, colour = scen, fill = scen, lty = scen)) +
  geom_hline(yintercept = 1, lwd=.4, lty = 2)+
  geom_ribbon(aes(ymin = lwr_s, ymax = ifelse(upr_s>3,3,upr_s)), alpha = 0.08, colour = NA) +
  geom_line(lwd = .4) +
  xlab('') + ylab(' ')+
  #labs(title = paste0(Spec_title)) + 
  scale_colour_manual(name = "Scenario", values = Scen_colors)+
  scale_fill_manual(name = "Scenario", values = Scen_colors)+
  scale_linetype(name = "Scenario")+
  geom_plot(data = data.tb_N, vp.width = .8, vp.height = .42, aes(x, y, label = plot))+
  facet_nested(Species~ Scenario,
               scales = "free_y",
               nest_line = element_line(linetype = 1))+
  theme_classic() +
  guides(colour = guide_legend(nrow = 1)) +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), limits = c(-0,3), 
                     breaks = seq(0,3,3/2)
                     #limits = c(0,max(round(na.omit(DTS_Scaled_Current$upr_s),0)))
  )+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        #axis.line = element_line(linewidth = 0.25),
        axis.ticks = element_line(linewidth = 0.25),
        panel.spacing.y = unit(.75, "lines"),
        #strip.background.x = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "grey20"),
        strip.placement = "outside")+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = 0.25)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd = 0.25)

Scal_ind_plot_S[[2]] = ggplot(Scaled_Ind_NS %>% filter(Species %in% unique(Scaled_Ind_NS$Species)[2],
                                                       group %in% "South",
                                                       Model=="No depth"), 
                              aes(as.numeric(as.character(year)), est_s, colour = scen, fill = scen, lty = scen)) +
  geom_hline(yintercept = 1, lwd=.4, lty = 2)+
  geom_ribbon(aes(ymin = lwr_s, ymax = ifelse(upr_s>3,3,upr_s)), alpha = 0.08, colour = NA) +
  geom_line(lwd = .4) +
  xlab('') + ylab(' ')+
  #labs(title = paste0(Spec_title)) + 
  scale_colour_manual(name = "Scenario", values = Scen_colors)+
  scale_fill_manual(name = "Scenario", values = Scen_colors)+
  scale_linetype(name = "Scenario")+
  geom_plot(data = data.tb_S, vp.width = .8, vp.height = .42, aes(x, y, label = plot))+
  facet_nested(Species~ Scenario,
               scales = "free_y",
               nest_line = element_line(linetype = 1))+
  theme_classic() +
  guides(colour = guide_legend(nrow = 1)) +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), limits = c(-0,3), 
                     breaks = seq(0,3,3/2)
                     #limits = c(0,max(round(na.omit(DTS_Scaled_Current$upr_s),0)))
  )+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        #axis.line = element_line(linewidth = 0.25),
        axis.ticks = element_line(linewidth = 0.25),
        panel.spacing.y = unit(.75, "lines"),
        #strip.background.x = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "grey20"),
        strip.placement = "outside")+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = 0.25)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd = 0.25)



# Longspine thornyhead ####
inset_Long_S=ggplot(Scaled_Ind_NS %>% filter(Species %in% unique(Scaled_Ind_NS$Species)[3],
                                            group %in% "South",
                                            Scenario %in% "Spatial 3",
                                            Model=="No depth"), 
                   aes(as.numeric(as.character(year)), est_s, colour = scen, fill = scen, lty = scen)) +
  geom_hline(yintercept = 1, lwd=.4, lty = 2)+
  geom_ribbon(aes(ymin = lwr_s, ymax = upr_s), alpha = 0.08, colour = NA) +
  geom_line(lwd = .4) +
  xlab('') + ylab(' ')+
  #labs(title = paste0(Spec_title)) + 
  scale_colour_manual(name = "Scenario", values = Scen_colors)+
  scale_fill_manual(name = "Scenario", values = Scen_colors)+
  scale_linetype(name = "Scenario")+
  facet_nested(Species~ Scenario, 
               scales = "free_y",
               nest_line = element_line(linetype = 1))+
  theme_classic() +
  guides(colour = guide_legend(nrow = 1)) +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), limits = c(0,10000), 
                     breaks = seq(0,10000,10000/2)
                     #limits = c(0,max(round(na.omit(DTS_Scaled_Current$upr_s),0)))
  )+
  theme(legend.position = "none",
        axis.title = element_blank(),
        legend.title = element_blank(),
        axis.line = element_line(linewidth = 0.25),
        axis.ticks = element_line(linewidth = 0.25),
        plot.background = element_rect(fill = "white"),
        panel.spacing.y = unit(0, "lines"),
        #strip.background.x = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "grey20")                                            ,
        strip.text = element_blank(),
        axis.text.y = element_text(size = 7),
        strip.background = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        strip.placement = "outside")+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = 0.25)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd = 0.25)

inset_Long_N=ggplot(Scaled_Ind_NS %>% filter(Species %in% unique(Scaled_Ind_NS$Species)[3],
                                            group %in% "North",
                                            Scenario %in% "Spatial 3",
                                            Model=="No depth"), 
                   aes(as.numeric(as.character(year)), est_s, colour = scen, fill = scen, lty = scen)) +
  geom_hline(yintercept = 1, lwd=.4, lty = 2)+
  geom_ribbon(aes(ymin = lwr_s, ymax = upr_s), alpha = 0.08, colour = NA) +
  geom_line(lwd = .4) +
  xlab('') + ylab(' ')+
  #labs(title = paste0(Spec_title)) + 
  scale_colour_manual(name = "Scenario", values = Scen_colors)+
  scale_fill_manual(name = "Scenario", values = Scen_colors)+
  scale_linetype(name = "Scenario")+
  facet_nested(Species~ Scenario, 
               scales = "free_y",
               nest_line = element_line(linetype = 1))+
  theme_classic() +
  guides(colour = guide_legend(nrow = 1)) +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), limits = c(0,3500), 
                     breaks = seq(0,3500,3500/2)
                     #limits = c(0,max(round(na.omit(DTS_Scaled_Current$upr_s),0)))
  )+
  theme(legend.position = "none",
        axis.title = element_blank(),
        legend.title = element_blank(),
        axis.line = element_line(linewidth = 0.25),
        axis.ticks = element_line(linewidth = 0.25),
        plot.background = element_rect(fill = "white"),
        panel.spacing.y = unit(0, "lines"),
        #strip.background.x = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "grey20")                                            ,
        strip.text = element_blank(),
        axis.text.y = element_text(size = 7),
        strip.background = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        strip.placement = "outside")+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = 0.25)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd = 0.25)

data.tb_S <- tibble(x = c(2008,2008, 2003.5), 
                    y = c(1.5, -1.4,2.95), 
                    Scenario = factor(c("Spatial 1", "Spatial 2", "Spatial 3")),
                    plot = list(ggplot + geom_blank(), ggplot+geom_blank(), inset_Long_S))

data.tb_N <- tibble(x = c(2008,2008, 2003.5), 
                    y = c(1.5, -1.4,2.95), 
                    Scenario = factor(c("Spatial 1", "Spatial 2", "Spatial 3")),
                    plot = list(ggplot + geom_blank(), ggplot+geom_blank(), inset_Long_N))


Scal_ind_plot_N[[3]] = ggplot(Scaled_Ind_NS %>% filter(Species %in% unique(Scaled_Ind_NS$Species)[3],
                                                       group %in% "North",
                                                       Model=="No depth"), 
                              aes(as.numeric(as.character(year)), est_s, colour = scen, fill = scen, lty = scen)) +
  geom_hline(yintercept = 1, lwd=.4, lty = 2)+
  geom_ribbon(aes(ymin = lwr_s, ymax = ifelse(upr_s>3,3,upr_s)), alpha = 0.08, colour = NA) +
  geom_line(lwd = .4) +
  xlab('') + ylab(' ')+
  #labs(title = paste0(Spec_title)) + 
  scale_colour_manual(name = "Scenario", values = Scen_colors)+
  scale_fill_manual(name = "Scenario", values = Scen_colors)+
  scale_linetype(name = "Scenario")+
  geom_plot(data = data.tb_N, vp.width = .8, vp.height = .42, aes(x, y, label = plot))+
  facet_nested(Species~ Scenario,
               scales = "free_y",
               nest_line = element_line(linetype = 1))+
  theme_classic() +
  guides(colour = guide_legend(nrow = 1)) +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), limits = c(-0,3), 
                     breaks = seq(0,3,3/2)
                     #limits = c(0,max(round(na.omit(DTS_Scaled_Current$upr_s),0)))
  )+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        #axis.line = element_line(linewidth = 0.25),
        axis.ticks = element_line(linewidth = 0.25),
        panel.spacing.y = unit(.75, "lines"),
        #strip.background.x = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "grey20"),
        strip.placement = "outside")+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = 0.25)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd = 0.25)

Scal_ind_plot_S[[3]] = ggplot(Scaled_Ind_NS %>% filter(Species %in% unique(Scaled_Ind_NS$Species)[3],
                                                       group %in% "South",
                                                       Model=="No depth"), 
                              aes(as.numeric(as.character(year)), est_s, colour = scen, fill = scen, lty = scen)) +
  geom_hline(yintercept = 1, lwd=.4, lty = 2)+
  geom_ribbon(aes(ymin = lwr_s, ymax = ifelse(upr_s>3,3,upr_s)), alpha = 0.08, colour = NA) +
  geom_line(lwd = .4) +
  xlab('') + ylab(' ')+
  #labs(title = paste0(Spec_title)) + 
  scale_colour_manual(name = "Scenario", values = Scen_colors)+
  scale_fill_manual(name = "Scenario", values = Scen_colors)+
  scale_linetype(name = "Scenario")+
  geom_plot(data = data.tb_S, vp.width = .8, vp.height = .42, aes(x, y, label = plot))+
  facet_nested(Species~ Scenario,
               scales = "free_y",
               nest_line = element_line(linetype = 1))+
  theme_classic() +
  guides(colour = guide_legend(nrow = 1)) +
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), limits = c(-0,3), 
                     breaks = seq(0,3,3/2)
                     #limits = c(0,max(round(na.omit(DTS_Scaled_Current$upr_s),0)))
  )+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        #axis.line = element_line(linewidth = 0.25),
        axis.ticks = element_line(linewidth = 0.25),
        panel.spacing.y = unit(.75, "lines"),
        #strip.background.x = element_blank(),
        ggh4x.facet.nestline = element_line(colour = "grey20"),
        strip.placement = "outside")+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = 0.25)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd = 0.25)



# Combine together into a single plot ####
gridExtra::grid.arrange(ggarrange(ggarrange(ggarrange(Scal_ind_plot_N[[1]] +
                                                        theme(axis.text.x = element_blank(),
                                                              axis.ticks.x = element_blank(),
                                                              axis.line = element_line(size = 0.25),
                                                              legend.position = "none",
                                                              strip.text.y = element_blank(),
                                                              strip.background.y = element_blank()), NULL,
                                                      Scal_ind_plot_N[[2]] + 
                                                        theme(axis.text.x = element_blank(),
                                                              axis.ticks.x = element_blank(),
                                                              legend.position = "none",
                                                              #axis.line = element_line(size = 0.25),
                                                              strip.text.x = element_blank(),
                                                              strip.background.x = element_blank(),
                                                              strip.text.y = element_blank(),
                                                              strip.background.y = element_blank()), NULL,
                                                      ggarrange(NULL, Scal_ind_plot_N[[3]] + 
                                                                  theme(axis.text.x = element_blank(),
                                                                        axis.ticks.x = element_blank(),
                                                                        legend.position = "none",
                                                                        strip.text.x = element_blank(),
                                                                        strip.background.x = element_blank(),
                                                                        strip.text.y = element_blank(),
                                                                        strip.background.y = element_blank()),
                                                                ncol = 2, widths = c(0,1), labels = c("",""),
                                                                font.label = list(size = 10), hjust = -1.5),NULL,
                                                      Scal_ind_plot_N[[4]] + 
                                                        theme(axis.text.x = element_blank(),
                                                              axis.ticks.x = element_blank(),
                                                              legend.position = "none",
                                                              strip.text.x = element_blank(),
                                                              strip.background.x = element_blank(),
                                                              strip.text.y = element_blank(),
                                                              strip.background.y = element_blank()),NULL,
                                                      ggarrange(NULL, Scal_ind_plot_N[[5]] + 
                                                                  scale_y_continuous(limits = c(0,4.2),
                                                                                     breaks = seq(0,4.2,4.2/2))+ 
                                                                  theme(axis.text.x = element_blank(),
                                                                        axis.ticks.x = element_blank(),
                                                                        legend.position = "none",
                                                                        strip.text.x = element_blank(),
                                                                        strip.background.x = element_blank(),
                                                                        strip.text.y = element_blank(),
                                                                        strip.background.y = element_blank()),
                                                                ncol = 2, widths = c(-0,1), labels = c("",""),
                                                                font.label = list(size = 10), hjust = -2),NULL,
                                                      Scal_ind_plot_N[[6]] + 
                                                        theme(legend.position = "none",
                                                              strip.text.x = element_blank(),
                                                              strip.background.x = element_blank(),
                                                              axis.line = element_line(size = 0.25),
                                                              axis.text.x = element_text(size = 7, angle = 45,vjust = .75),
                                                              strip.text.y = element_blank(),
                                                              strip.background.y = element_blank()),
                                                      nrow = 11,
                                                      ncol = 1, heights = c(1.25,-.15, 1,-.15,1,-.15,1,-.15,1,-.15,1.15)),
                                            ggarrange(Scal_ind_plot_S[[1]] + 
                                                        theme(axis.text.x = element_blank(),
                                                              axis.ticks.x = element_blank(),
                                                              axis.line = element_line(size = 0.25),
                                                              legend.position = "none"), NULL,
                                                      Scal_ind_plot_S[[2]] + 
                                                        theme(axis.text.x = element_blank(),
                                                              axis.ticks.x = element_blank(),
                                                              #axis.line = element_line(size = 0.25),
                                                              legend.position = "none",
                                                              strip.text.x = element_blank(),
                                                              strip.background.x = element_blank()), NULL,
                                                      ggarrange(NULL, Scal_ind_plot_S[[3]] + 
                                                                  theme(axis.text.x = element_blank(),
                                                                        axis.ticks.x = element_blank(),
                                                                        legend.position = "none",
                                                                        strip.text.x = element_blank(),
                                                                        strip.background.x = element_blank()),
                                                                ncol = 2, widths = c(0,1), labels = c("",""),
                                                                font.label = list(size = 10), hjust = -1.5),NULL,
                                                      Scal_ind_plot_S[[4]] + 
                                                        theme(axis.text.x = element_blank(),
                                                              axis.ticks.x = element_blank(),
                                                              legend.position = "none",
                                                              strip.text.x = element_blank(),
                                                              strip.background.x = element_blank()),NULL,
                                                      ggarrange(NULL, Scal_ind_plot_S[[5]] + 
                                                                  scale_y_continuous(limits = c(0,4.2),
                                                                                     breaks = seq(0,4.2,4.2/2))+ 
                                                                  theme(axis.text.x = element_blank(),
                                                                        axis.ticks.x = element_blank(),
                                                                        legend.position = "none",
                                                                        strip.text.x = element_blank(),
                                                                        strip.background.x = element_blank()),
                                                                ncol = 2, widths = c(-0,1), labels = c("",""),
                                                                font.label = list(size = 10), hjust = -2), NULL,
                                                      Scal_ind_plot_S[[6]] + 
                                                        theme(legend.position = "none",
                                                              strip.text.x = element_blank(),
                                                              strip.background.x = element_blank(),
                                                              axis.line = element_line(size = 0.25),
                                                              axis.text.x = element_text(size = 7, angle = 45,vjust = .75)),
                                                      nrow = 11, 
                                                      ncol = 1, heights = c(1.25,-.15, 1,-.15,1,-.15,1,-.15,1,-.15,1.15)),
                                            ncol = 2, labels = c("A", "B"), font.label = list(size = 10)),
                                  get_plot_component(Scal_ind_plot_N[[1]],
                                                     "guide-box-bottom",return_all = T),
                                  ncol=1, heights = c(1,.05)), left = "Mean scaled biomass estimates", 
                        padding = unit(1, "line"))

ggsave(paste0(WD.1, "/Plots/Index/Scaled Index NS NO DEPTH_",Sys.Date(),".png"),
       height = 230, width = 180, units = "mm", dpi = 300, bg = "white")

## Performance metrics for biomass estimates ####
# For the performance metrics I am focusing on the NO DEPTH scenario 

Sca_ind_PM=merge(Scaled_Ind %>% filter(group == "Entire region",
                                       scen == "All data",
                                       Model =="No depth") %>%
                   select(Species, group, Scenario, est_s, scen, year) %>%
                   rename(est_A = est_s)%>%
                   select(Species, group, Scenario, est_A, year),
                 Scaled_Ind %>% filter(group == "Entire region",
                                       !scen == "All data",
                                       Model =="No depth"),
                 by = c("Species","group", "Scenario","year")) %>%
  mutate(Dif = est_s - est_A) %>%group_by(Species, Scenario, scen) %>%
  mutate(num1 = n()-1) %>%
  summarise(RMSE = (sqrt(sum(na.omit(Dif)^2)/num1)),
            MAE = (abs(sum(na.omit(Dif))))/num1,
            MRE = (sum(na.omit(Dif)/na.omit(est_A)))/num1
  ) %>%
  distinct() %>%
  pivot_longer(c(RMSE, MAE, MRE), names_to = "Error", values_to = "Val")

Sca_ind_PM_ER =  Sca_ind_PM %>%  
  group_by(Species) %>% 
  mutate(upr = round(max(Val)+.001,2)+.005,
         lwr = round(min(Val),2)) %>%
  mutate(upr = ifelse(max(upr)>10, round(upr+.5,0), ifelse(
    max(upr)>.25 & max(upr)<10, round(upr+.05,1), round(upr+.005,2))),
    lwr = ifelse(max(upr)>10, round(lwr,0), round(lwr,2)))

Sca_ind_PM_ER %>% filter(Species %in% c("Dover", "Sablefish", "Longspine"),
                         scen %in% c("19 years", "Never surveyed"),
                         Error == "RMSE",
                         Scenario == "Spatial 3") 

Sca_ind_PM %>%  
  group_by(Species) %>% 
  summarise(upr = round(max(Val),2)+.01,
            lwr = round(min(Val),2),
            upr2 = max(Val)) 
# 

Ind_CV = Ind_CV %>% filter(Model=="No depth") %>%  
  group_by(Species) %>% 
  mutate(lwr = round(min(dif_CV),2),
         range = ((round(max(dif_CV),4))/2))%>%
  mutate(range = round(range,2)+.01) %>%
  mutate(upr = lwr+(range*2))

Ind_CV$scen = factor(Ind_CV$scen.y, levels = (c("1 year", "5 years", "10 years", "15 years", "19 years", "Never surveyed")))

Ind_CV = rbind(Ind_CV %>% filter(group == "Entire region",
                                 Mean_cv>0.6) %>%
                 mutate(Sig = 1),
               Ind_CV %>% filter(group == "Entire region",
                                 Mean_cv<0.6) %>%
                 mutate(Sig = 2))

#Percent increase for average CV in
Ind_CV %>% mutate(Perc_Chan = (Mean_cv- Mean_AD)/Mean_AD *100) %>% group_by(Species, Scenario) %>% summarise(min1 = min(Perc_Chan),
                                                                                                             max1 = max(Perc_Chan),
                                                                                                             Species1 = unique(Species),
                                                                                                             Scenario1 = unique(Scenario))
Scale_Ind_PM_plot = list()
Ind_CV_plot = list()
for(i in 1:6){
  ## Performance Metrics - biomass estiamtes ##
  plot_data = Sca_ind_PM_ER %>% filter(Species %in% unique(Sca_ind_PM_ER$Species)[i])
  Scale_Ind_PM_plot[[i]] = ggplot(plot_data, aes(x = scen, y = Val, fill = Error))+
    geom_bar(stat = "identity", position = position_dodge(0.9), width = 0.9)+
    facet_nested(Species~ Scenario, space = "fixed") + 
    theme_classic()+
    theme(axis.text = element_text(size = 6))+
    geom_hline(yintercept = 0) + 
    labs(x = "", y = "")+
    scale_y_continuous(expand=c(0,0), , limits = c(plot_data$lwr[1],plot_data$upr[1]), 
                       breaks = seq(0,plot_data$upr[1],(0+plot_data$upr[1])/2)
                       #limits = c(0,max(round(na.omit(DTS_Scaled_Current$upr_s),0)))n.breaks = 3 )+
    )+
    #scale_x_discrete(guide = guide_axis(n.dodge = 2))+
    theme(strip.placement = "outside", 
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          legend.position = "bottom",
          axis.line = element_line(linewidth = 0.25),
          axis.ticks = element_line(linewidth = 0.2),
          #plot.margin = margin(.05,.05,.05,.05, "cm"),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = .9),
          legend.title = element_blank())+
    scale_fill_manual( values = c("#1b9e77", "#d95f02", "#7570b3"))+
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = .25, colour = "black")+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd = .25, colour = "black")
  
  ## Performance metrics - CVs ##
  plot_cv = Ind_CV %>% filter(Species %in% unique(Sca_ind_PM_ER$Species)[i],
                              !scen.y =="All data")
  if(any(plot_cv$Sig==1)){
      Ind_CV_plot[[i]] = ggplot(plot_cv, aes(x = Scenario, y = dif_CV, fill = scen))+
    geom_bar(stat = "identity", position = position_dodge(0.9))+
    facet_nested(Species~group, space = "fixed") + 
    theme_classic()+
    geom_hline(yintercept = 0) + 
    #scale_colour_manual(values = Scen_colors)+
    scale_fill_manual(values = Scen_colors)+
    labs(x = "", y = "")+
    guides(fill = guide_legend(override.aes = list(size = .6)))+
    scale_y_continuous(expand=c(0,0), , limits = c(plot_cv$lwr[1],round(plot_cv$upr[1]+(.102 *plot_cv$upr[1]),2)), 
                       breaks = seq(plot_cv$lwr[1],round(plot_cv$upr[1]+(.102 *plot_cv$upr[1]),2),
                                    (round(plot_cv$upr[1]+(.102 *plot_cv$upr[1]),2) - plot_cv$lwr[1])/2)
                       #limits = c(0,max(round(na.omit(DTS_Scaled_Current$upr_s),0)))n.breaks = 3 )+
    )+
    geom_text(data = plot_cv, aes(label = ifelse(Sig == 1, "*", ""), x = Scenario, y = dif_CV), 
              position = position_dodge(width = .9), vjust = .45, size = 15/.pt)+
    #scale_x_discrete(guide = guide_axis(n.dodge = 2))+
    theme(strip.placement = "outside", 
          axis.text = element_text(size = 6),
          legend.position = "bottom",
          axis.line = element_line(linewidth = 0.25),
          strip.background.x = element_rect(fill = "white", colour = "white"),
          strip.text.x = element_text(colour = "white"),
          axis.ticks = element_line(linewidth = 0.2),
          plot.margin = unit(c(0.0, 0, 0, 0), "cm"),
          legend.text = element_text(size = 9),
          #plot.margin = margin(.05,.05,.05,.05, "cm"),
          # axis.text.x = element_text(angle = 45, vjust = 1, hjust = .9),
          legend.title = element_blank())+
    #scale_fill_manual( values = c("#bdbdbd", "#525252", "grey12"))+
    annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = .25, colour = "black")+
    annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd = .25, colour = "black")
  }else{
    Ind_CV_plot[[i]] = ggplot(plot_cv, aes(x = Scenario, y = dif_CV, fill = scen))+
      geom_bar(stat = "identity", position = position_dodge(0.9))+
      facet_nested(Species~group, space = "fixed") + 
      theme_classic()+
      geom_hline(yintercept = 0) + 
      #scale_colour_manual(values = Scen_colors)+
      scale_fill_manual(values = Scen_colors)+
      labs(x = "", y = "")+
      guides(fill = guide_legend(override.aes = list(size = .55)))+
      scale_y_continuous(expand=c(0,0), , limits = c(plot_cv$lwr[1],plot_cv$upr[1]), 
                         breaks = seq(plot_cv$lwr[1],plot_cv$upr[1],plot_cv$range[1])
                         #limits = c(0,max(round(na.omit(DTS_Scaled_Current$upr_s),0)))n.breaks = 3 )+
      )+
      # geom_text(data = plot_cv, aes(label = ifelse(Sig == 1, "*", ""), x = Scenario, y = dif_CV), 
      #           position = position_dodge(width = .9), vjust = (-.1 *plot_cv$range[1]), size = 20 / .pt)+
      #scale_x_discrete(guide = guide_axis(n.dodge = 2))+
      theme(strip.placement = "outside", 
            axis.text = element_text(size = 6),
            legend.position = "bottom",
            axis.line = element_line(linewidth = 0.25),
            strip.background.x = element_rect(fill = "white", colour = "white"),
            strip.text.x = element_text(colour = "white"),
            axis.ticks = element_line(linewidth = 0.2),
            plot.margin = unit(c(0.0, 0, 0, 0), "cm"),
            legend.text = element_text(size = 9),
            #plot.margin = margin(.05,.05,.05,.05, "cm"),
            # axis.text.x = element_text(angle = 45, vjust = 1, hjust = .9),
            legend.title = element_blank())+
      #scale_fill_manual( values = c("#bdbdbd", "#525252", "grey12"))+
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = .25, colour = "black")+
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd = .25, colour = "black")
  }
  
}
Ind_CV_plot[[2]]
Ind_CV_plot[[4]]
# Some of the y axis will need updating 
Ind_CV_plot[[6]] + scale_y_continuous(limits = c(-0.02,0.01),
                                      expand = c(0,0))

left_ind <- textGrob("Average difference in CVs for biomass estimates", gp = gpar(fontsize = 11), rot = 90)
ggarrange(grid.arrange(ggarrange(gridExtra::grid.arrange(ggarrange(
  ggarrange(NULL, Scale_Ind_PM_plot[[1]] + 
              theme(axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    legend.position = "none",
                    strip.text.y = element_blank(),
                    strip.background.y = element_blank()),
            ncol = 2, 
            widths = c(0, 1)), NULL,
  ggarrange(NULL, Scale_Ind_PM_plot[[2]] + 
    scale_y_continuous(limits = c(0,24), breaks = seq(0,24,24/2), expand=c(0,0))+
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "none",
          strip.text.x = element_blank(),
          strip.background.x = element_blank(),
          strip.text.y = element_blank(),
          strip.background.y = element_blank()),
    ncol = 2, widths = c(0.008,1)), NULL,
  Scale_Ind_PM_plot[[3]] + 
    scale_y_continuous(expand = c(0,0), limits = c(-0.03,.2),
                       breaks = seq(0,.2,.2/2))+
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "none",
          strip.text.x = element_blank(),
          strip.background.x = element_blank(),
          strip.text.y = element_blank(),
          strip.background.y = element_blank()),NULL,
    ggarrange(NULL, Scale_Ind_PM_plot[[4]] + 
              scale_y_continuous(expand = c(0,0), limits = c(0,92),
                                 breaks = seq(0,92,92/2))+
              theme(axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    legend.position = "none",
                    strip.text.x = element_blank(),
                    strip.background.x = element_blank(),
                    strip.text.y = element_blank(),
                    strip.background.y = element_blank()),ncol = 2,
            widths = c(.01, 1)),NULL,
  ggarrange(NULL, Scale_Ind_PM_plot[[5]] + 
              scale_y_continuous(expand = c(0,0), limits = c(-.01,.1),
                                 breaks = seq(0,.1,.1))+ 
              theme(axis.text.x = element_blank(),
                    axis.ticks.x = element_blank(),
                    legend.position = "none",
                    strip.text.x = element_blank(),
                    strip.background.x = element_blank(),
                    strip.text.y = element_blank(),
                    strip.background.y = element_blank()),ncol = 2, 
            widths = c(0, 1)),NULL,
  Scale_Ind_PM_plot[[6]] + 
    scale_y_continuous(expand = c(0,0), limits = c(-.01,.2),
                       breaks = seq(0,.2,.2/2))+ 
    theme(legend.position = "none",
          strip.text.x = element_blank(),
          strip.background.x = element_blank(),
          strip.text.y = element_blank(),
          strip.background.y = element_blank()),
  nrow = 11, 
  ncol = 1, heights = c(1.2,-.05, 1,-.05,1,-.05,1,-.05,1,-.05,1.35), 
  labels = "A", font.label = list(size = 12),
  hjust = 0.6), 
  left = "Performance metrics for mean scaled biomass estimates"),
  get_plot_component(Scale_Ind_PM_plot[[1]]+
                       theme(legend.text = element_text(size = 6)),
                     "guide-box-bottom",return_all = T),
  ncol=1, heights = c(1,.05)), 
  ggplot()+geom_blank()+theme_void(),
  ggarrange(gridExtra::grid.arrange(ggarrange(ggarrange(NULL, Ind_CV_plot[[1]] +
                                                theme(axis.text.x = element_blank(),
                                                      axis.ticks.x = element_blank(),
                                                      legend.position = "none"),
                                                ncol = 2, widths = c(0.02,1)), NULL,
                                              ggarrange(NULL, Ind_CV_plot[[2]] + 
                                                theme(axis.text.x = element_blank(),
                                                      axis.ticks.x = element_blank(),
                                                      legend.position = "none",
                                                      strip.text.x = element_blank(),
                                                      strip.background.x = element_blank()),
                                                ncol = 2, widths = c(0.02,1)), NULL,
                                              Ind_CV_plot[[3]] + 
                                                scale_y_continuous(limits = c(-0.01,0.01),expand = c(0,0),
                                                                   breaks = seq(-0.01,0.01,0.01)) + 
                                                theme(axis.text.x = element_blank(),
                                                      axis.ticks.x = element_blank(),
                                                      legend.position = "none",
                                                      strip.text.x = element_blank(),
                                                      strip.background.x = element_blank()),NULL,
                                              ggarrange(NULL, Ind_CV_plot[[4]] + 
                                                theme(axis.text.x = element_blank(),
                                                      axis.ticks.x = element_blank(),
                                                      legend.position = "none",
                                                      strip.text.x = element_blank(),
                                                      strip.background.x = element_blank()),
                                                ncol = 2, widths = c(0.02,1)),NULL,
                                              ggarrange(NULL, Ind_CV_plot[[5]] + 
                                                theme(axis.text.x = element_blank(),
                                                      axis.ticks.x = element_blank(),
                                                      legend.position = "none",
                                                      strip.text.x = element_blank(),
                                                      strip.background.x = element_blank()),
                                               ncol = 2, widths = c(0.02,1)),NULL,
                                              Ind_CV_plot[[6]] + 
                                                scale_y_continuous(limits = c(-0.02,0.02),expand = c(0,0),
                                                                   breaks = seq(-0.02,0.02,0.02))+ 
                                                theme(legend.position = "none",
                                                      strip.text.x = element_blank(),
                                                      strip.background.x = element_blank()),NULL,
                                              nrow = 12, 
                                              ncol = 1, heights = c(1.2,-0.05, 1,-0.05,1,-0.05,1,-0.05,1,-0.05,1, 0.38),
                                              labels = "B", font.label = list(size = 12),
                                              hjust = 0.6), 
                                    left = left_ind, padding = unit(.2, "line")),
            get_plot_component(Ind_CV_plot[[1]] + guides(fill = guide_legend(
              override.aes = list(shape = 15, size =1.5)),
              #colour = guide_legend(nrow=2)
            )+
              theme(legend.text = element_text(size = 6)),
            "guide-box-bottom",return_all = T),
            ncol=1, heights = c(1,.05)),
  ncol = 3, widths = c(1,0.1,.8)),NULL, nrow = 2, heights = c(1,0.02))

ggsave(paste0(WD.1, "/Plots/Index/Scaled Index PM & CV ",Sys.Date(),".png"),
       height = 175, width = 170, units = "mm", dpi = 300, bg = "white")

##########################
## Density Estimates ####
##########################

## Maps of RMSE and MAE - NO DEPTH ####
# All temporal scenarios in 1 plot ####
## RMSE ###
for(j in 4:6){
  gc()
  print(paste0("Starting iteration for ", Species_vec[j]))
  Spec_data = All_species_nd[[j]]
  #Spec_data=Spec_data[[1]]
  spec_list = list()
  spec_plots = list()
  for(i in 1:3){  
    
    print(paste0("Starting iteration for ", OSW_all_scen[[i]]$Scenario[1]))
    
    spec_list[[i]] = Spec_data[[i]]$Dens_PM_ER %>% mutate(Scenario = OSW_all_scen[[i]]$Scenario[1])
    spec_list[[i]]$scen = factor(spec_list[[i]]$scen,
                                 levels = c("1 year", "5 years", "10 years", "15 years", "19 years", "Never surveyed"))#Assign the levels 
    
    
  }
  #Dens_list[[j]] = spec_list#Grab the density information accross the entire region
  gc()
  Dens_plot_ER = lapply(spec_list, function(plot.data){
    gr_plot_OSW_RMSE = ggplot()+
      geom_sf(data=coast_outline)+
      geom_tile(data = plot.data, 
                aes(x=X, y=Y,fill = RMSE), size = .1)+
      facet_nested(~scen)+
      scale_fill_gradient2(low = "#2166ac",
                           mid = "#f7f7f7",
                           high = "#b2182b",
                           midpoint = quantile(bind_rows(spec_list)$RMSE, 0.5),
                           limits = c(0, quantile(bind_rows(spec_list)$RMSE, 0.995)),
                           na.value = "#b2182b", name = paste0(Species_vec[j],": RMSE"))+
      theme_classic()+
      xlim(min(bind_rows(spec_list)$X),
           max(bind_rows(spec_list)$X))+
      ylim(min(bind_rows(spec_list)$Y),
           max(bind_rows(spec_list)$Y))+
      #geom_sf(data = OSW_shp, fill = 'transparent', colour = 'black', lwd = .05)+
      theme(#axis.text = element_text(size = 5),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.key.width = unit(1, "cm"))
    
  })
  
  Density_EntireRegion_ggplot=ggarrange(grid.arrange(Dens_plot_ER[[1]] + 
                                                       geom_sf(data = OSW_Scen1, fill = 'transparent', 
                                                               colour = "grey15", lwd = .1),
                                                     left = "Spatial Scenario 1"),
                                        grid.arrange(Dens_plot_ER[[2]] + 
                                                       geom_sf(data = OSW_Scen2_outline, fill = 'transparent', 
                                                               colour = "grey15", lwd = .1)+
                                                       theme(strip.background.x = element_blank(),
                                                             strip.text.x = element_blank()),
                                                     left = "Spatial Scenario 2"),
                                        grid.arrange(Dens_plot_ER[[3]] + 
                                                       geom_sf(data = OSW_Scen3, fill = 'transparent',
                                                               colour = "grey15", lwd = .1)+
                                                       theme(strip.background.x = element_blank(),
                                                             strip.text.x = element_blank()),
                                                     left = "Spatial Scenario 3"),
                                        get_plot_component(Dens_plot_ER[[1]] + 
                                                             theme(legend.position = "bottom",
                                                                   #legend.key.width = unit(2, "cm"),
                                                                   legend.title = element_text(size = 11),
                                                                   legend.text = element_text(size = 8)),
                                                           "guide-box-bottom",return_all = T),
                                        ncol = 1, nrow = 5, align = "hv", heights = c(1.1,1,1,0.15,.03))
  
  Density_EntireRegion_ggplot
  
  ggsave(paste0(WD.1, "/Plots/Density/RMSE/",Species_vec[j],"_density_RMSE_",Sys.Date(),".png"),
         Density_EntireRegion_ggplot,
         width = 170, height = 200, units = "mm", dpi = 300, bg = "white")
  
  
  # Dens_plot_ER = lapply(spec_list, function(plot.data){
  #   gr_plot_OSW_MAE = ggplot()+
  #     geom_sf(data=coast_outline)+
  #     geom_tile(data = plot.data, 
  #               aes(x=X, y=Y,fill = MAE), size = .1)+
  #     facet_nested(~scen)+
  #     scale_fill_gradient2(low = "#b2182b",
  #                          mid = "#f7f7f7",
  #                          high = "#2166ac",
  #                          midpoint = quantile(bind_rows(spec_list)$MAE, 0.5),
  #                          limits = c(0, quantile(bind_rows(spec_list)$MAE, 0.99)),
  #                          na.value = "#2166ac", name = paste0(Species_vec[j],": MAE"))+
  #     theme_classic()+
  #     xlim(min(bind_rows(spec_list)$X),
  #          max(bind_rows(spec_list)$X))+
  #     ylim(min(bind_rows(spec_list)$Y),
  #          max(bind_rows(spec_list)$Y))+
  #     #geom_sf(data = OSW_shp, fill = 'transparent', colour = 'black', lwd = .05)+
  #     theme(#axis.text = element_text(size = 5),
  #       axis.line = element_blank(),
  #       axis.ticks = element_blank(),
  #       axis.text = element_blank(),
  #       axis.title = element_blank(),
  #       legend.position = "none",
  #       legend.title = element_blank(),
  #       legend.key.width = unit(1, "cm"))
  #   
  # })
  # 
  # Density_EntireRegion_ggplot=ggarrange(grid.arrange(Dens_plot_ER[[1]] + 
  #                                                      geom_sf(data = OSW_Scen1, fill = 'transparent', 
  #                                                              colour = "grey30", lwd = .1),
  #                                                    left = "Spatial Scenario 1"),
  #                                       grid.arrange(Dens_plot_ER[[2]] + 
  #                                                      geom_sf(data = OSW_Scen2_outline, fill = 'transparent', 
  #                                                              colour = "grey30", lwd = .1)+
  #                                                      theme(strip.background.x = element_blank(),
  #                                                            strip.text.x = element_blank()),
  #                                                    left = "Spatial Scenario 2"),
  #                                       grid.arrange(Dens_plot_ER[[3]] + 
  #                                                      geom_sf(data = OSW_Scen3, fill = 'transparent',
  #                                                              colour = "grey30", lwd = .1)+
  #                                                      theme(strip.background.x = element_blank(),
  #                                                            strip.text.x = element_blank()),
  #                                                    left = "Spatial Scenario 3"),
  #                                       get_plot_component(Dens_plot_ER[[1]] + 
  #                                                            theme(legend.position = "bottom",
  #                                                                  #legend.key.width = unit(2, "cm"),
  #                                                                  legend.title = element_text(size = 11),
  #                                                                  legend.text = element_text(size = 8)),
  #                                                          "guide-box-bottom",return_all = T),
  #                                       ncol = 1, nrow = 5, align = "hv", heights = c(1.1,1,1,0.15,.03))
  # 
  # Density_EntireRegion_ggplot
  # 
  # ggsave(paste0(WD.1, "/Plots/Density/MAE/",Species_vec[j],"_density_MAE_",Sys.Date(),".png"),
  #        Density_EntireRegion_ggplot,
  #        width = 170, height = 200, units = "mm", dpi = 300, bg = "white")
}

## MAE ###
for(j in 1:3){
  gc()
  print(paste0("Starting iteration for ", Species_vec[j]))
  Spec_data = All_species_nd[[j]]
  #Spec_data=Spec_data[[1]]
  spec_list = list()
  spec_plots = list()
  for(i in 1:3){  
    
    print(paste0("Starting iteration for ", OSW_all_scen[[i]]$Scenario[1]))
    
    spec_list[[i]] = Spec_data[[i]]$Dens_PM_ER %>% mutate(Scenario = OSW_all_scen[[i]]$Scenario[1])
    spec_list[[i]]$scen = factor(spec_list[[i]]$scen,
                                 levels = c("1 year", "5 years", "10 years", "15 years", "19 years", "Never surveyed"))#Assign the levels 
    
    
  }
  #Dens_list[[j]] = spec_list#Grab the density information accross the entire region
  gc()
  # Dens_plot_ER = lapply(spec_list, function(plot.data){
  #   gr_plot_OSW_RMSE = ggplot()+
  #     geom_sf(data=coast_outline)+
  #     geom_tile(data = plot.data, 
  #               aes(x=X, y=Y,fill = RMSE), size = .1)+
  #     facet_nested(~scen)+
  #     scale_fill_gradient2(low = "#2166ac",
  #                          mid = "#f7f7f7",
  #                          high = "#b2182b",
  #                          midpoint = quantile(bind_rows(spec_list)$RMSE, 0.5),
  #                          limits = c(0, quantile(bind_rows(spec_list)$RMSE, 0.995)),
  #                          na.value = "#b2182b", name = paste0(Species_vec[j],": RMSE"))+
  #     theme_classic()+
  #     xlim(min(bind_rows(spec_list)$X),
  #          max(bind_rows(spec_list)$X))+
  #     ylim(min(bind_rows(spec_list)$Y),
  #          max(bind_rows(spec_list)$Y))+
  #     #geom_sf(data = OSW_shp, fill = 'transparent', colour = 'black', lwd = .05)+
  #     theme(#axis.text = element_text(size = 5),
  #       axis.line = element_blank(),
  #       axis.ticks = element_blank(),
  #       axis.text = element_blank(),
  #       axis.title = element_blank(),
  #       legend.position = "none",
  #       legend.title = element_blank(),
  #       legend.key.width = unit(1, "cm"))
  #   
  # })
  # 
  # Density_EntireRegion_ggplot=ggarrange(grid.arrange(Dens_plot_ER[[1]] + 
  #                                                      geom_sf(data = OSW_Scen1, fill = 'transparent', 
  #                                                              colour = "grey15", lwd = .1),
  #                                                    left = "Spatial Scenario 1"),
  #                                       grid.arrange(Dens_plot_ER[[2]] + 
  #                                                      geom_sf(data = OSW_Scen2_outline, fill = 'transparent', 
  #                                                              colour = "grey15", lwd = .1)+
  #                                                      theme(strip.background.x = element_blank(),
  #                                                            strip.text.x = element_blank()),
  #                                                    left = "Spatial Scenario 2"),
  #                                       grid.arrange(Dens_plot_ER[[3]] + 
  #                                                      geom_sf(data = OSW_Scen3, fill = 'transparent',
  #                                                              colour = "grey15", lwd = .1)+
  #                                                      theme(strip.background.x = element_blank(),
  #                                                            strip.text.x = element_blank()),
  #                                                    left = "Spatial Scenario 3"),
  #                                       get_plot_component(Dens_plot_ER[[1]] + 
  #                                                            theme(legend.position = "bottom",
  #                                                                  #legend.key.width = unit(2, "cm"),
  #                                                                  legend.title = element_text(size = 11),
  #                                                                  legend.text = element_text(size = 8)),
  #                                                          "guide-box-bottom",return_all = T),
  #                                       ncol = 1, nrow = 5, align = "hv", heights = c(1.1,1,1,0.15,.03))
  # 
  # Density_EntireRegion_ggplot
  # 
  # ggsave(paste0(WD.1, "/Plots/Density/RMSE/",Species_vec[j],"_density_RMSE_",Sys.Date(),".png"),
  #        Density_EntireRegion_ggplot,
  #        width = 170, height = 200, units = "mm", dpi = 300, bg = "white")
  
  
  Dens_plot_ER = lapply(spec_list, function(plot.data){
    gr_plot_OSW_MAE = ggplot()+
      geom_sf(data=coast_outline)+
      geom_tile(data = plot.data,
                aes(x=X, y=Y,fill = MAE), size = .1)+
      facet_nested(~scen)+
      scale_fill_gradient2(low = "#b2182b",
                           mid = "#f7f7f7",
                           high = "#2166ac",
                           midpoint = quantile(bind_rows(spec_list)$MAE, 0.5),
                           limits = c(0, quantile(bind_rows(spec_list)$MAE, 0.995)),
                           na.value = "#2166ac", name = paste0(Species_vec[j],": MAE"))+
      theme_classic()+
      xlim(min(bind_rows(spec_list)$X),
           max(bind_rows(spec_list)$X))+
      ylim(min(bind_rows(spec_list)$Y),
           max(bind_rows(spec_list)$Y))+
      #geom_sf(data = OSW_shp, fill = 'transparent', colour = 'black', lwd = .05)+
      theme(#axis.text = element_text(size = 5),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.key.width = unit(1, "cm"))

  })

  Density_EntireRegion_ggplot=ggarrange(grid.arrange(Dens_plot_ER[[1]] +
                                                       geom_sf(data = OSW_Scen1, fill = 'transparent',
                                                               colour = "grey30", lwd = .1),
                                                     left = "Spatial Scenario 1"),
                                        grid.arrange(Dens_plot_ER[[2]] +
                                                       geom_sf(data = OSW_Scen2_outline, fill = 'transparent',
                                                               colour = "grey30", lwd = .1)+
                                                       theme(strip.background.x = element_blank(),
                                                             strip.text.x = element_blank()),
                                                     left = "Spatial Scenario 2"),
                                        grid.arrange(Dens_plot_ER[[3]] +
                                                       geom_sf(data = OSW_Scen3, fill = 'transparent',
                                                               colour = "grey30", lwd = .1)+
                                                       theme(strip.background.x = element_blank(),
                                                             strip.text.x = element_blank()),
                                                     left = "Spatial Scenario 3"),
                                        get_plot_component(Dens_plot_ER[[1]] +
                                                             theme(legend.position = "bottom",
                                                                   #legend.key.width = unit(2, "cm"),
                                                                   legend.title = element_text(size = 11),
                                                                   legend.text = element_text(size = 8)),
                                                           "guide-box-bottom",return_all = T),
                                        ncol = 1, nrow = 5, align = "hv", heights = c(1.1,1,1,0.15,.03))

  Density_EntireRegion_ggplot

  ggsave(paste0(WD.1, "/Plots/Density/MAE/",Species_vec[j],"_density_MAE_",Sys.Date(),".png"),
         Density_EntireRegion_ggplot,
         width = 170, height = 200, units = "mm", dpi = 300, bg = "white")
}

# Only the never surveyed map - Potentially for the manuscript#
OSW_Scen1$Scenario = factor(OSW_Scen1$Scenario, levels = c("Scenario 1", "Scenario 2", "Scenario 3"), 
                            labels =  c("Spatial 1", "Spatial 2", "Spatial 3"))
OSW_Scen2_outline$Scenario = factor(OSW_Scen2_outline$Scenario, levels = c("Scenario 1", "Scenario 2", "Scenario 3"), 
                                    labels =  c("Spatial 1", "Spatial 2", "Spatial 3"))
OSW_Scen3$Scenario = factor(OSW_Scen3$Scenario, levels = c("Scenario 1", "Scenario 2", "Scenario 3"), 
                            labels =  c("Spatial 1", "Spatial 2", "Spatial 3"))
for(j in 1:6){
  print(paste0("Starting iteration for ", Species_vec[j]))
  Spec_data = All_species_nd[[j]]
  #Spec_data=Spec_data[[1]]
  spec_list = list()
  spec_plots = list()
  for(i in 1:3){  
    
    print(paste0("Starting iteration for ", OSW_all_scen[[i]]$Scenario[1]))
    
    spec_list[[i]] = Spec_data[[i]]$Dens_PM_ER %>% mutate(Scenario = OSW_all_scen[[i]]$Scenario[1]) %>%
      filter(scen == "Never surveyed")
    #spec_list[[i]]$scen = factor(spec_list[[i]]$scen,
     #                            levels = c(#"1 year", "5 years", "10 years", "15 years", "19 years", 
      #                                      "Never surveyed"))#Assign the levels 
    spec_list[[i]]$Scenario = factor(spec_list[[i]]$Scenario, levels = c("Scenario 1", "Scenario 2", "Scenario 3"), 
                             labels =  c("Spatial 1", "Spatial 2", "Spatial 3"))

    
  }
  #Dens_list[[j]] = spec_list#Grab the density information accross the entire region
  
  Dens_plot_ER = lapply(spec_list, function(plot.data){
    gr_plot_OSW_RMSE = ggplot()+
      geom_sf(data=coast_outline)+
      geom_tile(data = plot.data, 
                aes(x=X, y=Y,fill = RMSE), size = .1)+
      facet_nested(scen~Scenario)+
      scale_fill_gradient2(low = "#2166ac",
                           mid = "#f7f7f7",
                           high = "#b2182b",
                           midpoint = quantile(bind_rows(spec_list)$RMSE, 0.5),
                           limits = c(0, quantile(bind_rows(spec_list)$RMSE, 0.99)),
                           na.value = "#b2182b", name = paste0(Species_vec[j],": RMSE")
      )+
      # scale_fill_gradient2(low = "#0D0887FF",
      #                      mid = "#CC4678FF",
      #                      high = "#F0F921FF",
      #                      midpoint = quantile(bind_rows(spec_list)$RMSE, 0.99),
      #                      limits = c(0, quantile(bind_rows(spec_list)$RMSE, 0.999)),
      #                      na.value = "#F0F921FF", name = paste0(Species_vec[j]," RMSE"))+      
      theme_classic()+
      xlim(min(bind_rows(spec_list)$X),
           max(bind_rows(spec_list)$X))+
      ylim(min(bind_rows(spec_list)$Y),
           max(bind_rows(spec_list)$Y))+
      #geom_sf(data = OSW_shp, fill = 'transparent', colour = 'black', lwd = .05)+
      theme(#axis.text = element_text(size = 5),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.key.width = unit(1, "cm"),
        strip.background.y = element_blank(),
        strip.text.y = element_blank())
    
  })
  
  Density_EntireRegion_ggplot=ggarrange(ggarrange(Dens_plot_ER[[1]] + 
                                                       geom_sf(data = OSW_Scen1, fill = 'transparent', 
                                                               colour = "grey30", lwd = .15),
                                                  Dens_plot_ER[[2]] + 
                                                       geom_sf(data = OSW_Scen2_outline, fill = 'transparent', 
                                                               colour = "grey30", lwd = .15),
                                                  Dens_plot_ER[[3]] + 
                                                       geom_sf(data = OSW_Scen3, fill = 'transparent',
                                                               colour = "grey30", lwd = .15),
                                                  ncol = 3, nrow = 1, widths = c(1,1,1)
                                                     #left = "Spatial Scenario 3"
                                                     ),NULL,
                                        get_plot_component(Dens_plot_ER[[1]] + 
                                                             theme(legend.position = "bottom",
                                                                   #legend.key.width = unit(2, "cm"),
                                                                   legend.title = element_text(size = 9),
                                                                   legend.text = element_text(size = 7),
                                                                   legend.key.height = unit(.75, "cm"),
                                                                   legend.key.width = unit(.8, "cm")),
                                                           "guide-box-bottom",return_all = T),
                                        NULL,
                                        ncol = 1, nrow = 4, heights = c(1,-0.1,0.1, 0.02))
  
  Density_EntireRegion_ggplot
  
  ggsave(paste0(WD.1, "/Plots/Density/RMSE/",Species_vec[j],"_density_RMSE_NS_",Sys.Date(),".png"),
         Density_EntireRegion_ggplot,
         width = (170/6)*3.2, height = 90, units = "mm", dpi = 300, bg = "white")
  
  Dens_plot_ER = lapply(spec_list, function(plot.data){
    gr_plot_OSW_MAE = ggplot()+
      geom_sf(data=coast_outline)+
      geom_tile(data = plot.data,
                aes(x=X, y=Y,fill = (MAE)), size = .1)+
      facet_nested(scen~Scenario)+
      scale_fill_gradient2(low = "#b2182b",
                           mid = "#f7f7f7",
                           high = "#2166ac",
                           midpoint = quantile(bind_rows(spec_list)$MAE, 0.5),
                           limits = c(0, quantile(bind_rows(spec_list)$MAE, 0.99)),
                           na.value = "#2166ac", name = paste0(Species_vec[j],": MAE")
      )+
      # scale_fill_gradient2(low = "#40498EFF",
      #                      mid = "#38AAACFF",
      #                      high = "#DEF5E5FF",
      #                      midpoint = quantile(log(bind_rows(spec_list)$MAE), 0.9),
      #                      limits = c(0, quantile(log(bind_rows(spec_list)$MAE), 0.999)),
      #                      na.value = "#DEF5E5FF", name = paste0(Species_vec[j]," MAE"))+      
      theme_classic()+
      xlim(min(bind_rows(spec_list)$X),
           max(bind_rows(spec_list)$X))+
      ylim(min(bind_rows(spec_list)$Y),
           max(bind_rows(spec_list)$Y))+
      #geom_sf(data = OSW_shp, fill = 'transparent', colour = 'black', lwd = .05)+
      theme(#axis.text = element_text(size = 5),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(3, "cm"),
        strip.background.y = element_blank(),
        strip.text.y = element_blank())

  })

  Density_EntireRegion_ggplot=ggarrange(ggarrange(Dens_plot_ER[[1]] + 
                                                    geom_sf(data = OSW_Scen1, fill = 'transparent', 
                                                            colour = "firebrick", lwd = .15),
                                                  Dens_plot_ER[[2]] + 
                                                    geom_sf(data = OSW_Scen2_outline, fill = 'transparent', 
                                                            colour = "firebrick", lwd = .15),
                                                  Dens_plot_ER[[3]] + 
                                                    geom_sf(data = OSW_Scen3, fill = 'transparent',
                                                            colour = "firebrick", lwd = .15),
                                                  ncol = 3, nrow = 1, widths = c(1,1,1)
                                                  #left = "Spatial Scenario 3"
  ),
  NULL,
  get_plot_component(Dens_plot_ER[[1]] + 
                       theme(legend.position = "bottom",
                             #legend.key.width = unit(2, "cm"),
                             legend.title = element_text(size = 9),
                             legend.text = element_text(size = 7),
                             legend.key.height = unit(.75, "cm"),
                             legend.key.width = unit(.8, "cm")),
                     "guide-box-bottom",return_all = T),
  NULL,
  ncol = 1, nrow = 4, heights = c(1,-0.1,0.1, 0.02))
  
  ggsave(paste0(WD.1, "/Plots/Density/MAE/",Species_vec[j],"_density_MAE_NS_",Sys.Date(),".png"),
         Density_EntireRegion_ggplot,
         width = (170/6)*3.2, height = 90, units = "mm", dpi = 300, bg = "white")
}


# Now one comparing the extreme plots with and without depth ####
for(j in 1:6){
  gc()
  print(paste0("Starting iteration for ", Species_vec[j]))
  Spec_data = All_species_nd[[j]]
  Spec_data_d = All_species_d[[j]]
  #Spec_data=Spec_data[[1]]
  spec_list = list()
  spec_plots = list()
  spec_list2 = list()
  spec_plots2 = list()
  
  for(i in 1:3){  
    gc()
    print(paste0("Starting iteration for ", OSW_all_scen[[i]]$Scenario[1]))
    
    spec_list[[i]] = Spec_data[[i]]$Dens_PM_ER %>% mutate(Scenario = OSW_all_scen[[i]]$Scenario[1]) %>%
      filter(scen == "Never surveyed")

    spec_list2[[i]] = Spec_data_d[[i]]$Dens_PM_ER %>% mutate(Scenario = OSW_all_scen[[i]]$Scenario[1]) %>%
      filter(scen == "Never surveyed")
    
        #spec_list[[i]]$scen = factor(spec_list[[i]]$scen,
    #                            levels = c(#"1 year", "5 years", "10 years", "15 years", "19 years", 
    #                                      "Never surveyed"))#Assign the levels 
    spec_list[[i]]$Scenario = factor(spec_list[[i]]$Scenario, levels = c("Scenario 1", "Scenario 2", "Scenario 3"), 
                                     labels =  c("Spatial 1", "Spatial 2", "Spatial 3"))
    
    spec_list2[[i]]$Scenario = factor(spec_list2[[i]]$Scenario, levels = c("Scenario 1", "Scenario 2", "Scenario 3"), 
                                     labels =  c("Spatial 1", "Spatial 2", "Spatial 3"))
    
  }
  #Dens_list[[j]] = spec_list#Grab the density information accross the entire region
  
  ### RMSE ####
  Dens_plot_ER = lapply(spec_list, function(plot.data){
    gr_plot_OSW_RMSE = ggplot()+
      geom_sf(data=coast_outline)+
      geom_tile(data = plot.data, 
                aes(x=X, y=Y,fill = RMSE), size = .1)+
      facet_nested(scen~Scenario)+
      scale_fill_gradient2(low = "#2166ac",
                           mid = "#f7f7f7",
                           high = "#b2182b",
                           midpoint = quantile(bind_rows(spec_list)$RMSE, 0.5),
                           limits = c(0, quantile(bind_rows(spec_list)$RMSE, 0.99)),
                           na.value = "#b2182b", name = paste0(Species_vec[j],": RMSE")
      )+
      # scale_fill_gradient2(low = "#0D0887FF",
      #                      mid = "#CC4678FF",
      #                      high = "#F0F921FF",
      #                      midpoint = quantile(bind_rows(spec_list)$RMSE, 0.99),
      #                      limits = c(0, quantile(bind_rows(spec_list)$RMSE, 0.999)),
      #                      na.value = "#F0F921FF", name = paste0(Species_vec[j]," RMSE"))+      
      theme_classic()+
      xlim(min(bind_rows(spec_list)$X),
           max(bind_rows(spec_list)$X))+
      ylim(min(bind_rows(spec_list)$Y),
           max(bind_rows(spec_list)$Y))+
      #geom_sf(data = OSW_shp, fill = 'transparent', colour = 'black', lwd = .05)+
      theme(#axis.text = element_text(size = 5),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.key.width = unit(1, "cm"),
        strip.background.y = element_blank(),
        strip.text.y = element_blank())
    
  })
  
  Dens_plot_ER_d = lapply(spec_list2, function(plot.data){
    gr_plot_OSW_RMSE = ggplot()+
      geom_sf(data=coast_outline)+
      geom_tile(data = plot.data, 
                aes(x=X, y=Y,fill = RMSE), size = .1)+
      facet_nested(scen~Scenario)+
      scale_fill_gradient2(low = "#2166ac",
                           mid = "#f7f7f7",
                           high = "#b2182b",
                           midpoint = quantile(bind_rows(spec_list)$RMSE, 0.5),
                           limits = c(0, quantile(bind_rows(spec_list)$RMSE, 0.99)),
                           na.value = "#b2182b", name = paste0(Species_vec[j],": RMSE")
      )+
      # scale_fill_gradient2(low = "#0D0887FF",
      #                      mid = "#CC4678FF",
      #                      high = "#F0F921FF",
      #                      midpoint = quantile(bind_rows(spec_list)$RMSE, 0.99),
      #                      limits = c(0, quantile(bind_rows(spec_list)$RMSE, 0.999)),
      #                      na.value = "#F0F921FF", name = paste0(Species_vec[j]," RMSE"))+      
      theme_classic()+
      xlim(min(bind_rows(spec_list)$X),
           max(bind_rows(spec_list)$X))+
      ylim(min(bind_rows(spec_list)$Y),
           max(bind_rows(spec_list)$Y))+
      #geom_sf(data = OSW_shp, fill = 'transparent', colour = 'black', lwd = .05)+
      theme(#axis.text = element_text(size = 5),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.key.width = unit(1, "cm"),
        strip.background.y = element_blank(),
        strip.text.y = element_blank())
    
  })
  
  
  Density_EntireRegion_ggplot=ggarrange(grid.arrange(ggarrange(Dens_plot_ER[[1]] + 
                                                    geom_sf(data = OSW_Scen1, fill = 'transparent', 
                                                            colour = "grey30", lwd = .15),
                                                  Dens_plot_ER[[2]] + 
                                                    geom_sf(data = OSW_Scen2_outline, fill = 'transparent', 
                                                            colour = "grey30", lwd = .15),
                                                  Dens_plot_ER[[3]] + 
                                                    geom_sf(data = OSW_Scen3, fill = 'transparent',
                                                            colour = "grey30", lwd = .15),
                                                  ncol = 3, nrow = 1, widths = c(1,1,1)
                                                  #left = "Spatial Scenario 3"
  ), left = "No Depth"),NULL,
  grid.arrange(ggarrange(Dens_plot_ER_d[[1]] + theme(strip.background.x = element_blank(),
                                                     strip.text.x=element_blank()) +
              geom_sf(data = OSW_Scen1, fill = 'transparent', 
                      colour = "grey30", lwd = .15),
            Dens_plot_ER_d[[2]] + theme(strip.background.x = element_blank(),
                                        strip.text.x=element_blank())+ 
              geom_sf(data = OSW_Scen2_outline, fill = 'transparent', 
                      colour = "grey30", lwd = .15),
            Dens_plot_ER_d[[3]] + theme(strip.background.x = element_blank(),
                                        strip.text.x=element_blank())+ 
              geom_sf(data = OSW_Scen3, fill = 'transparent',
                      colour = "grey30", lwd = .15),
            ncol = 3, nrow = 1, widths = c(1,1,1)
            #left = "Spatial Scenario 3"
  ), left = "Depth"),
  NULL,
  get_plot_component(Dens_plot_ER[[1]] + 
                       theme(legend.position = "bottom",
                             #legend.key.width = unit(2, "cm"),
                             legend.title = element_text(size = 9),
                             legend.text = element_text(size = 7),
                             legend.key.height = unit(.75, "cm"),
                             legend.key.width = unit(.8, "cm")),
                     "guide-box-bottom",return_all = T),
  NULL,
  ncol = 1, nrow = 6, heights = c(1, -0.1,1,-0.05,0.1, 0.03))
  
  Density_EntireRegion_ggplot
  
  ggsave(paste0(WD.1, "/Plots/Density/RMSE/",Species_vec[j],"_density_RMSE_NS_D&ND_",Sys.Date(),".png"),
         Density_EntireRegion_ggplot,
         width = (170/6)*3.2, height = 150, units = "mm", dpi = 300, bg = "white")
  
  Dens_plot_ER = lapply(spec_list, function(plot.data){
    gr_plot_OSW_MAE = ggplot()+
      geom_sf(data=coast_outline)+
      geom_tile(data = plot.data, 
                aes(x=X, y=Y,fill = MAE), size = .1)+
      facet_nested(scen~Scenario)+
      scale_fill_gradient2(low = "#2166ac",
                           mid = "#f7f7f7",
                           high = "#2166ac",
                           midpoint = quantile(bind_rows(spec_list)$MAE, 0.5),
                           limits = c(0, quantile(bind_rows(spec_list)$MAE, 0.99)),
                           na.value = "#2166ac", name = paste0(Species_vec[j],": MAE")
      )+
      # scale_fill_gradient2(low = "#0D0887FF",
      #                      mid = "#CC4678FF",
      #                      high = "#F0F921FF",
      #                      midpoint = quantile(bind_rows(spec_list)$MAE, 0.99),
      #                      limits = c(0, quantile(bind_rows(spec_list)$MAE, 0.999)),
      #                      na.value = "#F0F921FF", name = paste0(Species_vec[j]," MAE"))+      
      theme_classic()+
      xlim(min(bind_rows(spec_list)$X),
           max(bind_rows(spec_list)$X))+
      ylim(min(bind_rows(spec_list)$Y),
           max(bind_rows(spec_list)$Y))+
      #geom_sf(data = OSW_shp, fill = 'transparent', colour = 'black', lwd = .05)+
      theme(#axis.text = element_text(size = 5),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.key.width = unit(1, "cm"),
        strip.background.y = element_blank(),
        strip.text.y = element_blank())
    
  })
  
  Dens_plot_ER_d = lapply(spec_list2, function(plot.data){
    gr_plot_OSW_MAE = ggplot()+
      geom_sf(data=coast_outline)+
      geom_tile(data = plot.data, 
                aes(x=X, y=Y,fill = MAE), size = .1)+
      facet_nested(scen~Scenario)+
      scale_fill_gradient2(low = "#2166ac",
                           mid = "#f7f7f7",
                           high = "#2166ac",
                           midpoint = quantile(bind_rows(spec_list)$MAE, 0.5),
                           limits = c(0, quantile(bind_rows(spec_list)$MAE, 0.99)),
                           na.value = "#2166ac", name = paste0(Species_vec[j],": MAE")
      )+
      # scale_fill_gradient2(low = "#0D0887FF",
      #                      mid = "#CC4678FF",
      #                      high = "#F0F921FF",
      #                      midpoint = quantile(bind_rows(spec_list)$MAE, 0.99),
      #                      limits = c(0, quantile(bind_rows(spec_list)$MAE, 0.999)),
      #                      na.value = "#F0F921FF", name = paste0(Species_vec[j]," MAE"))+      
      theme_classic()+
      xlim(min(bind_rows(spec_list)$X),
           max(bind_rows(spec_list)$X))+
      ylim(min(bind_rows(spec_list)$Y),
           max(bind_rows(spec_list)$Y))+
      #geom_sf(data = OSW_shp, fill = 'transparent', colour = 'black', lwd = .05)+
      theme(#axis.text = element_text(size = 5),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.key.width = unit(1, "cm"),
        strip.background.y = element_blank(),
        strip.text.y = element_blank())
    
  })
  
  
  Density_EntireRegion_ggplot=ggarrange(grid.arrange(ggarrange(Dens_plot_ER[[1]] + 
                                                                 geom_sf(data = OSW_Scen1, fill = 'transparent', 
                                                                         colour = "grey30", lwd = .15),
                                                               Dens_plot_ER[[2]] + 
                                                                 geom_sf(data = OSW_Scen2_outline, fill = 'transparent', 
                                                                         colour = "grey30", lwd = .15),
                                                               Dens_plot_ER[[3]] + 
                                                                 geom_sf(data = OSW_Scen3, fill = 'transparent',
                                                                         colour = "grey30", lwd = .15),
                                                               ncol = 3, nrow = 1, widths = c(1,1,1)
                                                               #left = "Spatial Scenario 3"
  ), left = "No Depth"),NULL,
  grid.arrange(ggarrange(Dens_plot_ER_d[[1]] + theme(strip.background.x = element_blank(),
                                                     strip.text.x=element_blank()) +
                           geom_sf(data = OSW_Scen1, fill = 'transparent', 
                                   colour = "grey30", lwd = .15),
                         Dens_plot_ER_d[[2]] + theme(strip.background.x = element_blank(),
                                                     strip.text.x=element_blank())+ 
                           geom_sf(data = OSW_Scen2_outline, fill = 'transparent', 
                                   colour = "grey30", lwd = .15),
                         Dens_plot_ER_d[[3]] + theme(strip.background.x = element_blank(),
                                                     strip.text.x=element_blank())+ 
                           geom_sf(data = OSW_Scen3, fill = 'transparent',
                                   colour = "grey30", lwd = .15),
                         ncol = 3, nrow = 1, widths = c(1,1,1)
                         #left = "Spatial Scenario 3"
  ), left = "Depth"),
  NULL,
  get_plot_component(Dens_plot_ER[[1]] + 
                       theme(legend.position = "bottom",
                             #legend.key.width = unit(2, "cm"),
                             legend.title = element_text(size = 9),
                             legend.text = element_text(size = 7),
                             legend.key.height = unit(.75, "cm"),
                             legend.key.width = unit(.8, "cm")),
                     "guide-box-bottom",return_all = T),
  NULL,
  ncol = 1, nrow = 6, heights = c(1, -0.1,1,-0.05,0.1, 0.03))
  
  Density_EntireRegion_ggplot
  
  ggsave(paste0(WD.1, "/Plots/Density/MAE/",Species_vec[j],"_density_MAE_NS_D&ND_",Sys.Date(),".png"),
         Density_EntireRegion_ggplot,
         width = (170/6)*3.2, height = 150, units = "mm", dpi = 300, bg = "white")
}
# 1 year, 15, and never surveyed in 1 plot - for ease of viewing  ####
for(j in 1:6){
  gc()
  print(paste0("Starting iteration for ", Species_vec[j]))
  Spec_data = All_species_list[[j]]
  Spec_data=Spec_data[[1]]
  spec_list = list()
  spec_plots = list()
  for(i in 1:3){  
    
    print(paste0("Starting iteration for ", OSW_all_scen[[i]]$Scenario[1]))
    
    spec_list[[i]] = Spec_data[[i]]$Dens_PM_ER %>% mutate(Scenario = OSW_all_scen[[i]]$Scenario[1])
    spec_list[[i]]$scen = factor(spec_list[[i]]$scen,
                                 levels = c("1 year", "5 years", "10 years", 
                                            "15 years", "19 years", 
                                            "Never surveyed"))#Assign the levels 
    
    
  }
  #Dens_list[[j]] = spec_list#Grab the density information accross the entire region
  Dens_plot_ER = lapply(spec_list, function(plot.data){
    gr_plot_OSW_RMSE = ggplot()+
      geom_sf(data=coast_outline)+
      geom_tile(data = plot.data, 
                aes(x=X, y=Y,fill = RMSE), size = .1)+
      facet_nested(~scen)+
      scale_fill_viridis_c(option = "plasma",
                           limits = c(0, quantile(bind_rows(spec_list)$RMSE, 0.999)),
                           na.value = "#F0F921FF", name = paste0(Species_vec[j]," RMSE"))+
      theme_classic()+
      xlim(min(bind_rows(spec_list)$X),
           max(bind_rows(spec_list)$X))+
      ylim(min(bind_rows(spec_list)$Y),
           max(bind_rows(spec_list)$Y))+
      #geom_sf(data = OSW_shp, fill = 'transparent', colour = 'black', lwd = .05)+
      theme(#axis.text = element_text(size = 5),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.key.width = unit(1, "cm"))
    
  })
  
  Density_EntireRegion_ggplot=ggarrange(grid.arrange(Dens_plot_ER[[1]] + 
                                                       geom_sf(data = OSW_Scen1, fill = 'transparent', 
                                                               colour = "black", lwd = .1),
                                                     left = "Spatial Scenario 1"),
                                        grid.arrange(Dens_plot_ER[[2]] + 
                                                       geom_sf(data = OSW_Scen2_outline, fill = 'transparent', 
                                                               colour = "black", lwd = .1)+
                                                       theme(strip.background.x = element_blank(),
                                                             strip.text.x = element_blank()),
                                                     left = "Spatial Scenario 2"),
                                        grid.arrange(Dens_plot_ER[[3]] + 
                                                       geom_sf(data = OSW_Scen3, fill = 'transparent',
                                                               colour = "black", lwd = .1)+
                                                       theme(strip.background.x = element_blank(),
                                                             strip.text.x = element_blank()),
                                                     left = "Spatial Scenario 3"),
                                        get_plot_component(Dens_plot_ER[[1]] + 
                                                             theme(legend.position = "bottom",
                                                                   #legend.key.width = unit(2, "cm"),
                                                                   legend.title = element_text(size = 11),
                                                                   legend.text = element_text(size = 8)),
                                                           "guide-box-bottom",return_all = T),
                                        ncol = 1, nrow = 5, align = "hv", heights = c(1.1,1,1,0.15,.03))
  
  Density_EntireRegion_ggplot
  
  ggsave(paste0(WD.1, "/Plots/Density/RMSE/",Species_vec[j],"_density_RMSE_1_15_NS_",Sys.Date(),".png"),
         Density_EntireRegion_ggplot,
         width = 170, height = 200, units = "mm", dpi = 300, bg = "white")
  
  
  Dens_plot_ER = lapply(spec_list, function(plot.data){
    gr_plot_OSW_MAE = ggplot()+
      geom_sf(data=coast_outline)+
      geom_tile(data = plot.data, 
                aes(x=X, y=Y,fill = MAE), size = .1)+
      facet_nested(~scen)+
      # scale_fill_gradient2(low = "#b2182b",
      #                      mid = "#f7f7f7",
      #                      high = "#2166ac",
      #                      midpoint = quantile(bind_rows(spec_list)$MAE, 0.5),
      #                      limits = c(0, quantile(bind_rows(spec_list)$MAE, 0.999)),
      #                      na.value = "#2166ac", name = paste0(Species_vec[j]," MAE")
      # )+
      scale_fill_viridis_c(option = "mako",
                           limits = c(0, quantile(bind_rows(spec_list)$RMSE, 0.999)),
                           na.value = "#DEF5E5FF", name = paste0(Species_vec[j]," MAE"))+
      
      theme_classic()+
      xlim(min(bind_rows(spec_list)$X),
           max(bind_rows(spec_list)$X))+
      ylim(min(bind_rows(spec_list)$Y),
           max(bind_rows(spec_list)$Y))+
      #geom_sf(data = OSW_shp, fill = 'transparent', colour = 'black', lwd = .05)+
      theme(#axis.text = element_text(size = 5),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.position = "none",
        legend.title = element_blank(),
        legend.key.width = unit(1, "cm"))
    
  })
  
  Density_EntireRegion_ggplot=ggarrange(grid.arrange(Dens_plot_ER[[1]] + 
                                                       geom_sf(data = OSW_Scen1, fill = 'transparent', 
                                                               colour = "red", lwd = .05),
                                                     left = "Spatial Scenario 1"),
                                        grid.arrange(Dens_plot_ER[[2]] + 
                                                       geom_sf(data = OSW_Scen2_outline, fill = 'transparent', 
                                                               colour = "red", lwd = .05)+
                                                       theme(strip.background.x = element_blank(),
                                                             strip.text.x = element_blank()),
                                                     left = "Spatial Scenario 2"),
                                        grid.arrange(Dens_plot_ER[[3]] + 
                                                       geom_sf(data = OSW_Scen3, fill = 'transparent',
                                                               colour = "red", lwd = .05)+
                                                       theme(strip.background.x = element_blank(),
                                                             strip.text.x = element_blank()),
                                                     left = "Spatial Scenario 3"),
                                        get_plot_component(Dens_plot_ER[[1]] + 
                                                             theme(legend.position = "bottom",
                                                                   #legend.key.width = unit(2, "cm"),
                                                                   legend.title = element_text(size = 11),
                                                                   legend.text = element_text(size = 8)),
                                                           "guide-box-bottom",return_all = T),
                                        ncol = 1, nrow = 5, align = "hv", heights = c(1.1,1,1,0.15,.03))
  
  Density_EntireRegion_ggplot
  
  ggsave(paste0(WD.1, "/Plots/Density/MAE/",Species_vec[j],"_density_MAE__1_15_NS_",Sys.Date(),".png"),
         Density_EntireRegion_ggplot,
         width = 170, height = 200, units = "mm", dpi = 300, bg = "white")
}
## IQR Line plots ####

Data_Kern = list()
Data_PM_dens = list()
Data_Kern_ER = list()
Data_PM_dens_ER = list()
for(j in 1:6){
  Data_spec = All_species_nd[[j]]
  #Data_spec = Data_spec[[1]]
  for(i in 1:3){
    Data_PM_dens[[i]] = Data_spec[[i]]$Density_PM_OSW %>% mutate(Spec = Species_vec[j],
                                                                 Scenario = OSW_all_scen[[i]]$Scenario[1]) %>%
      mutate(RMSE2 = ifelse(RMSE>quantile(RMSE,0.99), quantile(RMSE,0.99), RMSE))
    
    Data_PM_dens_ER[[i]] = Data_spec[[i]]$Dens_PM_ER %>% mutate(Spec = Species_vec[j],
                                                                Scenario = OSW_all_scen[[i]]$Scenario[1]) %>%
      mutate(RMSE2 = ifelse(RMSE>quantile(RMSE,0.99), quantile(RMSE,0.99), RMSE)) 
    
  }
  Data_Kern[[j]] = merge(bind_rows(Data_PM_dens),
                         bind_rows(Data_PM_dens_ER)%>%
                           filter(scen == "Never surveyed",
                                  Scenario == "Scenario 3") %>%
                           group_by(Spec) %>%
                           summarise(RMSE_max = quantile(RMSE,1),
                                     MAE_max = quantile(MAE,1)), by = "Spec")
  
  Data_Kern_ER[[j]]= merge(bind_rows(Data_PM_dens_ER) %>%
                             group_by(Spec, Scenario, scen ) %>%
                             summarise(RMSE_ER = median(RMSE),
                                       MAE_ER = median(MAE)),
                           bind_rows(Data_PM_dens_ER)%>%
                             filter(scen == "Never surveyed",
                                    Scenario == "Scenario 3") %>%
                             group_by(Spec) %>%
                             summarise(RMSE_max = quantile(RMSE,1),
                                       MAE_max = quantile(MAE,1)), by = "Spec") %>%
    group_by(Spec, Scenario, scen) %>%
    summarise(RMSE_ER = RMSE_ER/RMSE_max,
              MAE_ER = MAE_ER/MAE_max)
}


AVG_ER = bind_rows(Data_Kern_ER)%>%
  rename(RMSE = RMSE_ER,
         MAE = MAE_ER) %>%
  pivot_longer(c(RMSE, MAE), names_to = "Met", values_to = "Val")

AVG_ER$Species = factor(AVG_ER$Spec, levels = c("Dover sole", "Sablefish", "Shortspine thornyhead", 
                                                "Longspine thornyhead","Aurora rockfish", "Rex sole"), 
                        labels =  c("Dover", "Sablefish", "Shortspine", "Longspine","Aurora", "Rex"))

AVG_ER$Scenario = factor(AVG_ER$Scenario, levels = c("Scenario 1", "Scenario 2", "Scenario 3"), 
                         labels =  c("Spatial 1", "Spatial 2", "Spatial 3"))
AVG_ER$scen = factor(AVG_ER$scen, levels = (c("1 year", "5 years", "10 years", "15 years", "19 years", "Never surveyed")))

summary(bind_rows(Data_Kern)%>%filter(Spec =="Aurora rockfish", OSW == "Del Norte"))

Avg_ind_OSW = rbind(bind_rows(Data_Kern) %>%
                      group_by(scen, Spec, Scenario, OSW) %>%
                      summarise(Lwr = quantile(RMSE,0.25)/unique(RMSE_max),
                                Upr = quantile(RMSE,0.75)/unique(RMSE_max),
                                Median = median(RMSE)/unique(RMSE_max),
                                Met = "RMSE") %>%
                      pivot_longer(c(Lwr, Upr, Median), names_to = "Quan", values_to = "Val"),
                    # bind_rows(Data_Kern) %>%
                    # group_by(Spec) %>% 
                    # mutate(max_RMSE= quantile(RMSE,1),
                    #        max_MAE = quantile(MAE, 1)) %>% ungroup() %>%
                    bind_rows(Data_Kern) %>%
                      group_by(scen,Spec, Scenario, OSW) %>%
                      summarise(Lwr = quantile(MAE,0.25)/unique(MAE_max),
                                Upr = quantile(MAE,0.75)/unique(MAE_max),
                                Median = median(MAE)/unique(MAE_max),
                                Met = "MAE") %>%
                      pivot_longer(c(Lwr, Upr, Median), names_to = "Quan", values_to = "Val")) 


Avg_ind_OSW$Species = factor(Avg_ind_OSW$Spec, levels = c("Dover sole", "Sablefish", "Shortspine thornyhead", 
                                                          "Longspine thornyhead","Aurora rockfish", "Rex sole"), 
                             labels =  c("Dover", "Sablefish", "Shortspine", "Longspine","Aurora", "Rex"))

Avg_ind_OSW$Scenario = factor(Avg_ind_OSW$Scenario, levels = c("Scenario 1", "Scenario 2", "Scenario 3"), 
                              labels =  c("Spatial 1", "Spatial 2", "Spatial 3"))

Avg_ind_OSW$scen = factor(Avg_ind_OSW$scen, levels = (c("1 year", "5 years", "10 years", "15 years", "19 years", "Never surveyed")))
Avg_ind_OSW$OSW = factor(Avg_ind_OSW$OSW, levels = rev(c("Coos Bay", "Coos Bay_Large", "Brookings", "Brookings_Large", "Del Norte", "Del Norte Oval","Humboldt Oval", "Humboldt", "Humboldt2", "Humboldt3", "Mendocino2", "Monterey", "Morro", "Morro Bay Oval")))

# 
# Colour_opt = c("Leased WEAs" ="#AA4499" ,
#                "Designated WEAs" = "#88ccee",
#                "CA Sea Space AOIs" = "#332288",
#                "CA Suitable Sea Space" = "#117733",
#                "OR Call Areas" = "#999933" )
Avg_ind_OSW = Avg_ind_OSW %>% mutate(WEA_t = ifelse(OSW %in% c("Humboldt", "Morro"), "Leased WEAs",
                                                    ifelse(OSW %in% c("Brookings", "Coos Bay"), "Designated WEAs",
                                                           ifelse(OSW %in% c("Del Norte", "Humboldt2", "Humboldt3", "Mendocino2", "Monterey"), 
                                                                  "CA Suitable Sea Space",
                                                                  ifelse(OSW %in% c("Brookings_Large", "Coos Bay_Large"), "OR Call Areas", "CA Sea Space AOIs")))))

OSW_col = c("Coos Bay" = "#88ccee", "Coos Bay_Large" = "#999933", "Brookings" = "#88ccee", "Brookings_Large" = "#999933",
            "Del Norte" = "#117733", "Del Norte Oval" = "#332288", "Humboldt Oval" = "#332288", "Humboldt" = "#AA4499",
            "Humboldt2" = "#117733", "Humboldt3" = "#117733", "Mendocino2" = "#117733", "Monterey" = "#117733",
            "Morro" = "#AA4499", "Morro Bay Oval" = "#332288")

Avg_ind_OSW=Avg_ind_OSW %>% group_by(Species, Met) %>% mutate(upr = round(round(max(Val)+.005,3),2)+.001)
Avg_ind_OSW %>% group_by(Species, Met) %>% summarise(upr = round(round(max(Val)+.005,3),2)+.001)
Avg_ind_OSW %>% group_by(Species, Met) %>% summarise(upr = round(round(max(Val)+.005,3),2)+.001,
                                                     up = max(Val))
Avg_ind_OSW = Avg_ind_OSW %>% mutate(OSW_3 = ifelse(OSW %in% "Humboldt", "Hm ",
                                                    ifelse(OSW %in% "Brookings", "Br ",
                                                           ifelse(OSW %in% "Coos Bay", "Cb ",
                                                                  ifelse(OSW %in% "Morro", "Mb ",
                                                                         ifelse(OSW %in% "Del Norte", "Dn ",
                                                                                ifelse(OSW %in% "Humboldt2", "Hm2",
                                                                                       ifelse(OSW %in% "Humboldt3", "Hm3",
                                                                                              ifelse(OSW %in% "Mendocino2", "Md ",
                                                                                                     ifelse(OSW %in% "Monterey","Mr ",
                                                                                                            ifelse(OSW %in% "Brookings_Large", "BrL",
                                                                                                                   ifelse(OSW %in% "Coos Bay_Large", "CbL",
                                                                                                                          ifelse(OSW %in% "Del Norte Oval", "DnO", 
                                                                                                                                 ifelse(OSW %in% "Humboldt Oval", "HmO", "MbO"))))))))))))))

Avg_ind_OSW$OSW_3 = factor(Avg_ind_OSW$OSW_3, levels = rev(c("Cb ", "CbL", "Br ", "BrL", "Dn ", "DnO","HmO", "Hm ", "Hm2", "Hm3", "Md ", "Mr ", "Mb ", "MbO")))

Colour_opt = c("Leased WEAs" ="#AA4499" ,
               "Designated WEAs" = "#88ccee",
               "CA Sea Space AOIs" = "#332288",
               "CA Suitable Sea Space" = "#117733",
               "OR Call Areas" = "#999933" )

OSW_RMSE_plot = list()
OSW_MAE_plot = list()

for (i in 1:6) {
  plot_data = Avg_ind_OSW %>% filter(Species %in% unique(Avg_ind_OSW$Species)[i])
  plot_data_avg = AVG_ER %>% filter(Species %in% unique(Avg_ind_OSW$Species)[i])
  
  OSW_rmse_plot1 = list()
  OSW_mae_plot1 = list()
  
  for (j in 1:3){
    plot_data2 = plot_data %>% filter(Scenario %in% unique(plot_data$Scenario)[j])
    plot_data_avg2 = plot_data_avg %>% filter(Scenario %in% unique(plot_data_avg$Scenario)[j])
    
    plot_data2$Species = plot_data2$Species[drop=T]
    OSW_rmse_plot1[[j]] = ggplot(data = plot_data2 %>% filter(Met == "RMSE", 
                                                              !Quan == "Median", 
                                                              scen %in% c("1 year", "15 years", "Never surveyed"),
                                                              Scenario %in% plot_data2$Scenario[1],
                                                              Species %in% plot_data2$Species[1]))+
      geom_vline(data = plot_data_avg2 %>% filter(Met == "RMSE", 
                                                  scen %in% c("1 year", "15 years", "Never surveyed"),
                                                  Scenario %in% plot_data_avg2$Scenario[1],
                                                  Species %in% plot_data_avg2$Species[1]),
                 aes(xintercept = Val), lty = 2, col = "grey45", lwd = 0.35)+
      ## For presentation 
      geom_line(aes(x = Val, y = OSW_3, colour = OSW), lwd = 1.05, alpha = 0.4) + 
      ## For Manuscript
      # geom_line(aes(x = Val, y = OSW_3, colour = OSW), lwd = .55, alpha = 0.4) + 
      ## For presentation 
      geom_point(data = Avg_ind_OSW %>% filter(Met == "RMSE",  
                                               Quan == "Median",
                                               scen %in% c("1 year", "15 years", "Never surveyed"),
                                               Scenario %in% plot_data2$Scenario[1],
                                               Species %in% plot_data2$Species[1]),
                 aes(x = Val, y = OSW_3, fill = OSW), #colour = "black", 
                 pch = 21, size = 1.2)+
      ## For manuscript
      # geom_point(data = Avg_ind_OSW %>% filter(Met == "RMSE",  
      #                                         Quan == "Median",
      #                                         scen %in% c("1 year", "15 years", "Never surveyed"),
      #                                         Scenario %in% plot_data2$Scenario[1],
      #                                         Species %in% plot_data2$Species[1]),
      #           aes(x = Val, y = OSW_3, fill = OSW), #colour = "black", 
      #           pch = 21, size = .75)+
      facet_nested( scen ~  Species, scales = "free",
                    #nest_line = element_line(linetype = 1), 
                    shrink = T) + 
      scale_colour_manual(values = OSW_col) + 
      scale_fill_manual(values = OSW_col)+
      theme_classic()+
      scale_x_continuous(expand = c(0,plot_data2[plot_data2$Met=="RMSE",]$upr[1]*.01), 
                         limits = c(-1*(plot_data2[plot_data2$Met=="RMSE",]$upr[1]*.05) , 
                                    plot_data2[plot_data2$Met=="RMSE",]$upr[1]), 
                         breaks = seq(0, round(plot_data2[plot_data2$Met=="RMSE",]$upr[1],2), 
                                      round(plot_data2[plot_data2$Met=="RMSE",]$upr[1]/2,2)))+
      labs(x = "Scaled RMSE")+
      #scale_x_continuous(n.breaks = 3)+
      theme(legend.position = "none",
            strip.placement = "outside",
            panel.spacing.y = unit(1.2, "lines"),
            strip.background.y = element_blank(),
            axis.text.y = element_text(size = 7),
            axis.ticks.x = element_blank(), 
            axis.title.x = element_blank(),
            axis.line = element_line(linewidth = 0.25),
            axis.ticks.y = element_line(linewidth = 0.25))+
      labs(y = "", x = "RMSE")+
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = 0.25)+
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd = 0.25)
    
    OSW_mae_plot1[[j]] = ggplot(data = plot_data2 %>% filter(Met == "MAE", 
                                                             !Quan == "Median", 
                                                             scen %in% c("1 year", "15 years", "Never surveyed"),
                                                             Scenario %in% plot_data2$Scenario[1],
                                                             Species %in% plot_data2$Species[1]))+
      geom_vline(data = plot_data_avg2 %>% filter(Met == "MAE", 
                                                  scen %in% c("1 year", "15 years", "Never surveyed"),
                                                  Scenario %in% plot_data_avg2$Scenario[1],
                                                  Species %in% plot_data_avg2$Species[1]),
                 aes(xintercept = Val), lty = 2, col = "grey45", lwd = 0.35)+
      geom_line(aes(x = Val, y = OSW_3, colour = OSW), lwd = 0.55, alpha = 0.4) + 
      geom_point(data = Avg_ind_OSW %>% filter(Met == "MAE",  
                                               Quan == "Median",
                                               scen %in% c("1 year", "15 years", "Never surveyed"),
                                               Scenario %in% plot_data2$Scenario[1],
                                               Species %in% plot_data2$Species[1]),
                 aes(x = Val, y = OSW_3, fill = OSW, colour = OSW), #colour = "black", 
                 pch = 19, size = .75)+
      facet_nested( scen ~  Species, scales = "free",
                    #nest_line = element_line(linetype = 1), 
                    shrink = T) + 
      scale_colour_manual(values = OSW_col) + 
      scale_fill_manual(values = OSW_col)+
      theme_classic()+
      scale_x_continuous(expand = c(0,0), limits = c(-1*(plot_data2[plot_data2$Met=="MAE",]$upr[1]*.05) , 
                                                     plot_data2[plot_data2$Met=="MAE",]$upr[1]), 
                         breaks = seq(0, plot_data2[plot_data2$Met=="MAE",]$upr[1]-.001, 
                                      (plot_data2[plot_data2$Met=="MAE",]$upr[1]-.001)/2))+
      labs(x = "MAE")+
      #scale_x_continuous(n.breaks = 3)+
      theme(legend.position = "none",
            strip.placement = "outside",
            panel.spacing.y = unit(1.2, "lines"),
            strip.background.y = element_blank(),
            axis.text.y = element_text(size = 7),
            axis.ticks.x = element_blank(), 
            axis.title.x = element_blank(),
            axis.line = element_line(linewidth = 0.25),
            axis.ticks.y = element_line(linewidth = 0.25))+
      labs(y = "", x = "MAE")+
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = 0.25)+
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd = 0.25)
    
  }
  OSW_RMSE_plot[[i]] = OSW_rmse_plot1
  OSW_MAE_plot[[i]] = OSW_mae_plot1
  
}
Scenario1_RMSE_plot = lapply(OSW_RMSE_plot, "[[", 1)
Scenario2_RMSE_plot = lapply(OSW_RMSE_plot, "[[", 2)
Scenario3_RMSE_plot = lapply(OSW_RMSE_plot, "[[", 3)

Scenario1_MAE_plot = lapply(OSW_MAE_plot, "[[", 1)
Scenario2_MAE_plot = lapply(OSW_MAE_plot, "[[", 2)
Scenario3_MAE_plot = lapply(OSW_MAE_plot, "[[", 3)

## Other Scenarios for Figures in Github
OSW_RMSE_Sup = list()
OSW_MAE_Sup = list()

for (i in 1:6) {
  plot_data = Avg_ind_OSW %>% filter(Species %in% unique(Avg_ind_OSW$Species)[i])
  plot_data_avg = AVG_ER %>% filter(Species %in% unique(Avg_ind_OSW$Species)[i])
  
  OSW_rmse_Sup1 = list()
  OSW_mae_Sup1 = list()
  
  for (j in 1:3){
    plot_data2 = plot_data %>% filter(Scenario %in% unique(plot_data$Scenario)[j])
    plot_data_avg2 = plot_data_avg %>% filter(Scenario %in% unique(plot_data_avg$Scenario)[j])
    
    plot_data2$Species = plot_data2$Species[drop=T]
    OSW_rmse_Sup1[[j]] = ggplot(data = plot_data2 %>% filter(Met == "RMSE", 
                                                             !Quan == "Median", 
                                                             !scen %in% c("1 year", "15 years", "Never surveyed"),
                                                             Scenario %in% plot_data2$Scenario[1],
                                                             Species %in% plot_data2$Species[1]))+
      geom_vline(data = plot_data_avg2 %>% filter(Met == "RMSE", 
                                                  !scen %in% c("1 year", "15 years", "Never surveyed"),
                                                  Scenario %in% plot_data_avg2$Scenario[1],
                                                  Species %in% plot_data_avg2$Species[1]),
                 aes(xintercept = Val), lty = 2, col = "grey45", lwd = 0.35)+
      ## For presentation 
      # geom_line(aes(x = Val, y = OSW_3, colour = OSW), lwd = 1.05, alpha = 0.4) + 
      ## For Manuscript
       geom_line(aes(x = Val, y = OSW_3, colour = OSW), lwd = .55, alpha = 0.4) + 
      ## For presentation 
      # geom_point(data = Avg_ind_OSW %>% filter(Met == "RMSE",  
      #                                          Quan == "Median",
      #                                          !scen %in% c("1 year", "15 years", "Never surveyed"),
      #                                          Scenario %in% plot_data2$Scenario[1],
      #                                          Species %in% plot_data2$Species[1]),
      #            aes(x = Val, y = OSW_3, fill = OSW), #colour = "black", 
      #            pch = 21, size = 1.2)+
      ## For manuscript
      geom_point(data = Avg_ind_OSW %>% filter(Met == "RMSE",
                                              Quan == "Median",
                                              scen %in% c("1 year", "15 years", "Never surveyed"),
                                              Scenario %in% plot_data2$Scenario[1],
                                              Species %in% plot_data2$Species[1]),
                aes(x = Val, y = OSW_3, fill = OSW), #colour = "black",
                pch = 21, size = .75)+
      facet_nested( scen ~  Species, scales = "free",
                    #nest_line = element_line(linetype = 1), 
                    shrink = T) + 
      scale_colour_manual(values = OSW_col) + 
      scale_fill_manual(values = OSW_col)+
      theme_classic()+
      scale_x_continuous(expand = c(0,plot_data2[plot_data2$Met=="RMSE",]$upr[1]*.01), 
                         limits = c(-1*(plot_data2[plot_data2$Met=="RMSE",]$upr[1]*.05) , 
                                    plot_data2[plot_data2$Met=="RMSE",]$upr[1]), 
                         breaks = seq(0, round(plot_data2[plot_data2$Met=="RMSE",]$upr[1],2), 
                                      round(plot_data2[plot_data2$Met=="RMSE",]$upr[1]/2,2)))+
      labs(x = "Scaled RMSE")+
      #scale_x_continuous(n.breaks = 3)+
      theme(legend.position = "none",
            strip.placement = "outside",
            panel.spacing.y = unit(1.2, "lines"),
            strip.background.y = element_blank(),
            axis.text.y = element_text(size = 7),
            axis.ticks.x = element_blank(), 
            axis.title.x = element_blank(),
            axis.line = element_line(linewidth = 0.25),
            axis.ticks.y = element_line(linewidth = 0.25))+
      labs(y = "", x = "RMSE")+
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = 0.25)+
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd = 0.25)
    
    OSW_mae_Sup1[[j]] = ggplot(data = plot_data2 %>% filter(Met == "MAE", 
                                                            !Quan == "Median", 
                                                            !scen %in% c("1 year", "15 years", "Never surveyed"),
                                                            Scenario %in% plot_data2$Scenario[1],
                                                            Species %in% plot_data2$Species[1]))+
      geom_vline(data = plot_data_avg2 %>% filter(Met == "MAE", 
                                                  !scen %in% c("1 year", "15 years", "Never surveyed"),
                                                  Scenario %in% plot_data_avg2$Scenario[1],
                                                  Species %in% plot_data_avg2$Species[1]),
                 aes(xintercept = Val), lty = 2, col = "grey45", lwd = 0.35)+
      geom_line(aes(x = Val, y = OSW_3, colour = OSW), lwd = 0.55, alpha = 0.4) + 
      geom_point(data = Avg_ind_OSW %>% filter(Met == "MAE",  
                                               Quan == "Median",
                                               !scen %in% c("1 year", "15 years", "Never surveyed"),
                                               Scenario %in% plot_data2$Scenario[1],
                                               Species %in% plot_data2$Species[1]),
                 aes(x = Val, y = OSW_3, fill = OSW, colour = OSW), #colour = "black", 
                 pch = 19, size = .75)+
      facet_nested( scen ~  Species, scales = "free",
                    #nest_line = element_line(linetype = 1), 
                    shrink = T) + 
      scale_colour_manual(values = OSW_col) + 
      scale_fill_manual(values = OSW_col)+
      theme_classic()+
      scale_x_continuous(expand = c(0,0), limits = c(-1*(plot_data2[plot_data2$Met=="MAE",]$upr[1]*.05) , 
                                                     plot_data2[plot_data2$Met=="MAE",]$upr[1]), 
                         breaks = seq(0, plot_data2[plot_data2$Met=="MAE",]$upr[1]-.001, 
                                      (plot_data2[plot_data2$Met=="MAE",]$upr[1]-.001)/2))+
      labs(x = "MAE")+
      #scale_x_continuous(n.breaks = 3)+
      theme(legend.position = "none",
            strip.placement = "outside",
            panel.spacing.y = unit(1.2, "lines"),
            strip.background.y = element_blank(),
            axis.text.y = element_text(size = 7),
            axis.ticks.x = element_blank(), 
            axis.title.x = element_blank(),
            axis.line = element_line(linewidth = 0.25),
            axis.ticks.y = element_line(linewidth = 0.25))+
      labs(y = "", x = "MAE")+
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = 0.25)+
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd = 0.25)
    
  }
  OSW_RMSE_Sup[[i]] = OSW_rmse_Sup1
  OSW_MAE_Sup[[i]] = OSW_mae_Sup1
  
}
Scenario1_RMSE_Sup = lapply(OSW_RMSE_Sup, "[[", 1)
Scenario2_RMSE_Sup = lapply(OSW_RMSE_Sup, "[[", 2)
Scenario3_RMSE_Sup = lapply(OSW_RMSE_Sup, "[[", 3)

Scenario1_MAE_Sup = lapply(OSW_MAE_Sup, "[[", 1)
Scenario2_MAE_Sup = lapply(OSW_MAE_Sup, "[[", 2)
Scenario3_MAE_Sup = lapply(OSW_MAE_Sup, "[[", 3)


IQR_legend=ggplot(data = Avg_ind_OSW %>% filter(Met == "RMSE", !Quan == "Median", scen %in% c("1 year", "5 years", "10 years")))+
  geom_vline(data = AVG_ER %>% filter(Met == "RMSE", scen %in% c("1 year", "5 years", "10 years")), 
             aes(xintercept = Val), lty = 2, col = "grey45")+
  geom_line(aes(x = Val, y = OSW, colour = WEA_t), lwd = 1.2, alpha = 0.4) + 
  geom_point(data = Avg_ind_OSW %>% filter(Met == "RMSE",  Quan == "Median",
                                           scen %in% c("1 year", "15 years", "Never surveyed")),
             aes(x = Val, y = OSW, fill = WEA_t, colour = WEA_t), pch = 21, size = 1.5)+
  facet_nested(Scenario + scen ~  Species, scales = "free",
               nest_line = element_line(linetype = 1), shrink = T) + 
  scale_colour_manual(values = Colour_opt, breaks = c("Designated WEAs", "Leased WEAs", "CA Suitable Sea Space",
                                                      "OR Call Areas", "OTHER",
                                                      "CA Sea Space AOIs")) + 
  scale_fill_manual(values = Colour_opt, breaks = c("Designated WEAs", "Leased WEAs", "CA Suitable Sea Space",
                                                    "OR Call Areas", "OTHER",
                                                    "CA Sea Space AOIs"))+
  theme_classic()+
  labs(x = "Scaled RMSE")+
  scale_x_continuous(n.breaks = 3)+
  guides(colour = guide_legend(nrow = 2),
         fill = guide_legend(nrow = 2),
         pch = guide_legend(nrow = 2))+
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 8.5),
        strip.placement = "outside",
        panel.spacing.y = unit(1.2, "lines"),
        strip.background.y = element_blank(),
        axis.text.y = element_text(size = 7),
        axis.ticks.x = element_blank(), 
        axis.title.x = element_blank())+
  labs(y = "", x = "Max scaled RMSE")+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)


## Plot for manuscript ####
ggarrange( grid.arrange(
  ggarrange(NULL,
            # Spatial Scneario 1
            ggarrange(NULL, Scenario1_MAE_plot[[2]] + 
                        #scale_x_continuous(limits = c(-0.05*0.14,0.14), breaks = c(0,0.07,0.14), expand = c(0,0.01))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              plot.margin = margin(.05,-.2,.05,.0, "cm"),
                              strip.text.x = element_text(size = 9),
                              axis.text.y = element_text(size = 7),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25),
                              axis.ticks.y = element_line(linewidth = 0.25)),
                      Scenario1_MAE_plot[[5]] + 
                        #scale_x_continuous(limits = c(-0.05*0.52,0.54), breaks = c(0,0.26,0.52), expand = c(0,0.01))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),                  
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              strip.text.x = element_text(size = 9),
                              axis.ticks.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.text.x = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario1_MAE_plot[[6]] + 
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              strip.text.x = element_text(size = 9),
                              axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario1_MAE_plot[[3]] + 
                        scale_x_continuous(limits = c(-0.05*.08,.08), breaks = seq(0,.08, 0.08/2), expand = c(0,0.0002))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              strip.text.x = element_text(size = 9),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario1_MAE_plot[[1]] + 
                        scale_x_continuous(limits = c(-0.05*.5,.5), breaks = seq(0,.5, .5/2), expand = c(0,0.0002))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              strip.text.x = element_text(size = 9),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario1_MAE_plot[[4]] + 
                        # facet_nested(Scenario + scen ~  Species, scales = "free",
                        #              nest_line = element_line(linetype = 1), 
                        #              shrink = T)+
                        theme(strip.text.y = element_text(size = 7),
                              axis.text.y = element_blank(),
                              strip.text.x = element_text(size = 9),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,.0,.05,-.05, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      ncol = 7, widths = c(0.045, 1.28, 1, 1, 1, 1, 1.3)),NULL,
            ## Spatial Scenario 2
            ggarrange(NULL, Scenario2_MAE_plot[[2]] + 
                        #scale_x_continuous(limits = c(-0.05*0.14,0.14), breaks = c(0,0.07,0.14), expand = c(0,0.01))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),                  
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              plot.margin = margin(.05,-.2,.05,.0, "cm"),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.text.y = element_text(size = 7),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario2_MAE_plot[[5]] + 
                        #scale_x_continuous(limits = c(-0.05*0.52,0.54), breaks = c(0,0.26,0.52), expand = c(0,0.01))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),                  
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.ticks.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.text.x = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario2_MAE_plot[[6]] + 
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario2_MAE_plot[[3]] + 
                        scale_x_continuous(limits = c(-0.05*.08,.08), breaks = seq(0,.08, 0.08/2), expand = c(0,0.0002))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario2_MAE_plot[[1]] + 
                        scale_x_continuous(limits = c(-0.05*.5,.5), breaks = seq(0,.5, .5/2), expand = c(0,0.0002))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario2_MAE_plot[[4]] + 
                        # facet_nested(Scenario + scen ~  Species, scales = "free",
                        #              nest_line = element_line(linetype = 1), 
                        #              shrink = T)+
                        theme(strip.text.y = element_text(size = 7),
                              axis.text.y = element_blank(),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,.0,.05,-.05, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      ncol = 7, widths = c(0.02,1.3, 1, 1, 1, 1, 1.3)),NULL,
            # Spatial Scenario 3
            ggarrange(Scenario3_MAE_plot[[2]] + 
                        #scale_x_continuous(limits = c(-0.05*0.14,0.14), breaks = c(0,0.07,0.14), expand = c(0,0.01))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),                  
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              plot.margin = margin(.05,-.2,.05,.0, "cm"),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.text.y = element_text(size = 7),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.text.x = element_text(size = 7),
                              axis.line = element_line(linewidth = 0.25),
                              axis.ticks.x = element_line(linewidth = 0.25)),
                      Scenario3_MAE_plot[[5]] + 
                        #scale_x_continuous(limits = c(-0.05*0.52,0.54), breaks = c(0,0.26,0.52), expand = c(0,0.0001))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),                  
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.ticks.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.text.x = element_text(size = 7),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.line = element_line(linewidth = 0.25),
                              axis.ticks.x = element_line(linewidth = 0.25)),
                      Scenario3_MAE_plot[[6]] + 
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_text(size = 7),
                              axis.line = element_line(linewidth = 0.25),
                              axis.ticks.x = element_line(linewidth = 0.25)),
                      Scenario3_MAE_plot[[3]] + 
                        scale_x_continuous(limits = c(-0.05*.08,.08), breaks = seq(0,.08, 0.08/2), expand = c(0,0.0002))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_text(size = 7),
                              axis.line = element_line(linewidth = 0.25),
                              axis.ticks.x = element_line(linewidth = 0.25)),
                      Scenario3_MAE_plot[[1]] + 
                        scale_x_continuous(limits = c(-0.05*.5,.5), breaks = seq(0,.5, .5/2), expand = c(0,0.0002))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_text(size = 7),
                              axis.line = element_line(linewidth = 0.25),
                              axis.ticks.x = element_line(linewidth = 0.25)),
                      Scenario3_MAE_plot[[4]] + 
                        # facet_nested(Scenario + scen ~  Species, scales = "free",
                        #              nest_line = element_line(linetype = 1), 
                        #              shrink = T)+
                        theme(strip.text.y = element_text(size = 7),
                              axis.text.y = element_blank(),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,.0,.05,-.05, "cm"),
                              axis.text.x = element_text(size = 7),
                              axis.line = element_line(linewidth = 0.25),
                              axis.ticks.x = element_line(linewidth = 0.25)),
                      ncol = 6, widths = c(1.3, 1, 1, 1, 1, 1.3)),
            
            ncol = 1, nrow = 6, 
            labels = c("","A: Spatial Scenario 1","" ,"B: Spatial Scenario 2","" ,"C: Spatial Scenario 3"), 
            heights = c(0.05,1.02,.05,1,0.05,1),
            font.label = list(size = 10.5), vjust=-0.1, hjust = 0),
  bottom = "MAE/max(MAE)", padding = unit(.5, "cm")), 
  get_plot_component(IQR_legend,
                     "guide-box-bottom",return_all = T), ncol = 1, nrow = 2, heights = c(1,0.08))
ggsave(paste0(WD.1, "/Plots/Density/MAE/MAE_line_plot_",Sys.Date(),".png"),
       height = 220, width = 170, units = "mm", dpi = 300, bg = "white")

# Supplementary MAE ####
ggarrange( grid.arrange(
  ggarrange(NULL,
            # Spatial Scneario 1
            ggarrange(NULL, Scenario1_MAE_Sup[[2]] + 
                        #scale_x_continuous(limits = c(-0.05*0.14,0.14), breaks = c(0,0.07,0.14), expand = c(0,0.01))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              plot.margin = margin(.05,-.2,.05,.0, "cm"),
                              strip.text.x = element_text(size = 9),
                              axis.text.y = element_text(size = 7),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25),
                              axis.ticks.y = element_line(linewidth = 0.25)),
                      Scenario1_MAE_Sup[[5]] + 
                        #scale_x_continuous(limits = c(-0.05*0.52,0.54), breaks = c(0,0.26,0.52), expand = c(0,0.01))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),                  
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              strip.text.x = element_text(size = 9),
                              axis.ticks.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.text.x = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario1_MAE_Sup[[6]] + 
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              strip.text.x = element_text(size = 9),
                              axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario1_MAE_Sup[[3]] + 
                        scale_x_continuous(limits = c(-0.05*.08,.08), breaks = seq(0,.08, 0.08/2), expand = c(0,0.0002))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              strip.text.x = element_text(size = 9),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario1_MAE_Sup[[1]] + 
                        scale_x_continuous(limits = c(-0.05*.5,.5), breaks = seq(0,.5, .5/2), expand = c(0,0.0002))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              strip.text.x = element_text(size = 9),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario1_MAE_Sup[[4]] + 
                        # facet_nested(Scenario + scen ~  Species, scales = "free",
                        #              nest_line = element_line(linetype = 1), 
                        #              shrink = T)+
                        theme(strip.text.y = element_text(size = 7),
                              axis.text.y = element_blank(),
                              strip.text.x = element_text(size = 9),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,.0,.05,-.05, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      ncol = 7, widths = c(0.045, 1.28, 1, 1, 1, 1, 1.3)),NULL,
            ## Spatial Scenario 2
            ggarrange(NULL, Scenario2_MAE_Sup[[2]] + 
                        #scale_x_continuous(limits = c(-0.05*0.14,0.14), breaks = c(0,0.07,0.14), expand = c(0,0.01))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),                  
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              plot.margin = margin(.05,-.2,.05,.0, "cm"),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.text.y = element_text(size = 7),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario2_MAE_Sup[[5]] + 
                        #scale_x_continuous(limits = c(-0.05*0.52,0.54), breaks = c(0,0.26,0.52), expand = c(0,0.01))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),                  
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.ticks.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.text.x = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario2_MAE_Sup[[6]] + 
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario2_MAE_Sup[[3]] + 
                        scale_x_continuous(limits = c(-0.05*.08,.08), breaks = seq(0,.08, 0.08/2), expand = c(0,0.0002))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario2_MAE_Sup[[1]] + 
                        scale_x_continuous(limits = c(-0.05*.5,.5), breaks = seq(0,.5, .5/2), expand = c(0,0.0002))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario2_MAE_Sup[[4]] + 
                        # facet_nested(Scenario + scen ~  Species, scales = "free",
                        #              nest_line = element_line(linetype = 1), 
                        #              shrink = T)+
                        theme(strip.text.y = element_text(size = 7),
                              axis.text.y = element_blank(),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,.0,.05,-.05, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      ncol = 7, widths = c(0.02,1.3, 1, 1, 1, 1, 1.3)),NULL,
            # Spatial Scenario 3
            ggarrange(Scenario3_MAE_Sup[[2]] + 
                        #scale_x_continuous(limits = c(-0.05*0.14,0.14), breaks = c(0,0.07,0.14), expand = c(0,0.01))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),                  
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              plot.margin = margin(.05,-.2,.05,.0, "cm"),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.text.y = element_text(size = 7),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.text.x = element_text(size = 7),
                              axis.line = element_line(linewidth = 0.25),
                              axis.ticks.x = element_line(linewidth = 0.25)),
                      Scenario3_MAE_Sup[[5]] + 
                        #scale_x_continuous(limits = c(-0.05*0.52,0.54), breaks = c(0,0.26,0.52), expand = c(0,0.0001))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),                  
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.ticks.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.text.x = element_text(size = 7),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.line = element_line(linewidth = 0.25),
                              axis.ticks.x = element_line(linewidth = 0.25)),
                      Scenario3_MAE_Sup[[6]] + 
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_text(size = 7),
                              axis.line = element_line(linewidth = 0.25),
                              axis.ticks.x = element_line(linewidth = 0.25)),
                      Scenario3_MAE_Sup[[3]] + 
                        scale_x_continuous(limits = c(-0.05*.08,.08), breaks = seq(0,.08, 0.08/2), expand = c(0,0.0002))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_text(size = 7),
                              axis.line = element_line(linewidth = 0.25),
                              axis.ticks.x = element_line(linewidth = 0.25)),
                      Scenario3_MAE_Sup[[1]] + 
                        scale_x_continuous(limits = c(-0.05*.5,.5), breaks = seq(0,.5, .5/2), expand = c(0,0.0002))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_text(size = 7),
                              axis.line = element_line(linewidth = 0.25),
                              axis.ticks.x = element_line(linewidth = 0.25)),
                      Scenario3_MAE_Sup[[4]] + 
                        # facet_nested(Scenario + scen ~  Species, scales = "free",
                        #              nest_line = element_line(linetype = 1), 
                        #              shrink = T)+
                        theme(strip.text.y = element_text(size = 7),
                              axis.text.y = element_blank(),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,.0,.05,-.05, "cm"),
                              axis.text.x = element_text(size = 7),
                              axis.line = element_line(linewidth = 0.25),
                              axis.ticks.x = element_line(linewidth = 0.25)),
                      ncol = 6, widths = c(1.3, 1, 1, 1, 1, 1.3)),
            
            ncol = 1, nrow = 6, 
            labels = c("","A: Spatial Scenario 1","" ,"B: Spatial Scenario 2","" ,"C: Spatial Scenario 3"), 
            heights = c(0.05,1.02,.05,1,0.05,1),
            font.label = list(size = 10.5), vjust=-0.1, hjust = 0),
  bottom = "MAE/max(MAE)", padding = unit(.5, "cm")), 
  get_plot_component(IQR_legend,
                     "guide-box-bottom",return_all = T), ncol = 1, nrow = 2, heights = c(1,0.08))

ggsave(paste0(WD.1, "/Plots/Density/MAE/MAE_line_plot_SUP_",Sys.Date(),".png"),
       height = 220, width = 170, units = "mm", dpi = 300, bg = "white")

# RMSE IQR plot ####
ggarrange( grid.arrange(
  ggarrange(NULL,
            # Spatial Scneario 1
            ggarrange(NULL, Scenario1_RMSE_plot[[2]] + 
                        #scale_x_continuous(limits = c(-0.05*0.14,0.14), breaks = c(0,0.07,0.14), expand = c(0,0.01))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              plot.margin = margin(.05,-.2,.05,.0, "cm"),
                              strip.text.x = element_text(size = 9),
                              axis.text.y = element_text(size = 7),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25),
                              axis.ticks.y = element_line(linewidth = 0.25)),
                      Scenario1_RMSE_plot[[5]] + 
                       # scale_x_continuous(limits = c(-0.05*0.52,0.54), breaks = c(0,0.26,0.52), expand = c(0,0.01))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),                  
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              strip.text.x = element_text(size = 9),
                              axis.ticks.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.text.x = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario1_RMSE_plot[[6]] + 
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              strip.text.x = element_text(size = 9),
                              axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario1_RMSE_plot[[3]] + 
                        scale_x_continuous(limits = c(-0.05*.08,.08), breaks = seq(0,.08, 0.08/2), expand = c(0,0.0002))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              strip.text.x = element_text(size = 9),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario1_RMSE_plot[[1]] + 
                        scale_x_continuous(limits = c(-0.05*0.7,0.7), breaks = c(0,0.7, 0.7/2), expand = c(0,0.0002))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              strip.text.x = element_text(size = 9),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario1_RMSE_plot[[4]] + 
                        # facet_nested(Scenario + scen ~  Species, scales = "free",
                        #              nest_line = element_line(linetype = 1), 
                        #              shrink = T)+
                        theme(strip.text.y = element_text(size = 7),
                              axis.text.y = element_blank(),
                              strip.text.x = element_text(size = 9),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,.0,.05,-.05, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      ncol = 7, widths = c(0.045, 1.28, 1, 1, 1, 1, 1.3)),NULL,
            ## Spatial Scenario 2
            ggarrange(NULL, Scenario2_RMSE_plot[[2]] + 
                        #scale_x_continuous(limits = c(-0.05*0.14,0.14), breaks = c(0,0.07,0.14), expand = c(0,0.01))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),                  
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              plot.margin = margin(.05,-.2,.05,.0, "cm"),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.text.y = element_text(size = 7),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario2_RMSE_plot[[5]] + 
                        #scale_x_continuous(limits = c(-0.05*0.52,0.54), breaks = c(0,0.26,0.52), expand = c(0,0.01))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),                  
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.ticks.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.text.x = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario2_RMSE_plot[[6]] + 
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario2_RMSE_plot[[3]] + 
                        scale_x_continuous(limits = c(-0.05*.08,.08), breaks = seq(0,.08, 0.08/2), expand = c(0,0.0002))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario2_RMSE_plot[[1]] + 
                        scale_x_continuous(limits = c(-0.05*0.7,0.7), breaks = c(0,0.7, 0.7/2), expand = c(0,0.0002))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario2_RMSE_plot[[4]] + 
                        # facet_nested(Scenario + scen ~  Species, scales = "free",
                        #              nest_line = element_line(linetype = 1), 
                        #              shrink = T)+
                        theme(strip.text.y = element_text(size = 7),
                              axis.text.y = element_blank(),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,.0,.05,-.05, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      ncol = 7, widths = c(0.02,1.3, 1, 1, 1, 1, 1.3)),NULL,
            # Spatial Scenario 3
            ggarrange(Scenario3_RMSE_plot[[2]] + 
                        #scale_x_continuous(limits = c(-0.05*0.14,0.14), breaks = c(0,0.07,0.14), expand = c(0,0.01))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),                  
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              plot.margin = margin(.05,-.2,.05,.0, "cm"),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.text.y = element_text(size = 7),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.text.x = element_text(size = 7),
                              axis.line = element_line(linewidth = 0.25),
                              axis.ticks.x = element_line(linewidth = 0.25)),
                      Scenario3_RMSE_plot[[5]] + 
                        #scale_x_continuous(limits = c(-0.05*0.52,0.54), breaks = c(0,0.26,0.52), expand = c(0,0.0001))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),                  
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.ticks.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.text.x = element_text(size = 7),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.line = element_line(linewidth = 0.25),
                              axis.ticks.x = element_line(linewidth = 0.25)),
                      Scenario3_RMSE_plot[[6]] + 
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_text(size = 7),
                              axis.line = element_line(linewidth = 0.25),
                              axis.ticks.x = element_line(linewidth = 0.25)),
                      Scenario3_RMSE_plot[[3]] + 
                        scale_x_continuous(limits = c(-0.05*.08,.08), breaks = seq(0,.08, 0.08/2), expand = c(0,0.0002))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_text(size = 7),
                              axis.line = element_line(linewidth = 0.25),
                              axis.ticks.x = element_line(linewidth = 0.25)),
                      Scenario3_RMSE_plot[[1]] + 
                        scale_x_continuous(limits = c(-0.05*0.7,0.7), breaks = c(0,0.7, 0.7/2), expand = c(0,0.0002))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_text(size = 7),
                              axis.line = element_line(linewidth = 0.25),
                              axis.ticks.x = element_line(linewidth = 0.25)),
                      Scenario3_RMSE_plot[[4]] + 
                        # facet_nested(Scenario + scen ~  Species, scales = "free",
                        #              nest_line = element_line(linetype = 1), 
                        #              shrink = T)+
                        theme(strip.text.y = element_text(size = 7),
                              axis.text.y = element_blank(),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,.0,.05,-.05, "cm"),
                              axis.text.x = element_text(size = 7),
                              axis.line = element_line(linewidth = 0.25),
                              axis.ticks.x = element_line(linewidth = 0.25)),
                      ncol = 6, widths = c(1.3, 1, 1, 1, 1, 1.3)),
            
            ncol = 1, nrow = 6, 
            labels = c("","A: Spatial Scenario 1","" ,"B: Spatial Scenario 2","" ,"C: Spatial Scenario 3"), 
            heights = c(0.05,1.02,.05,1,0.05,1),
            font.label = list(size = 10.5), vjust=-0.1, hjust = 0),
  bottom = "RMSE/max(RMSE)", padding = unit(.5, "cm")), 
  get_plot_component(IQR_legend,
                     "guide-box-bottom",return_all = T), ncol = 1, nrow = 2, heights = c(1,0.08))

ggsave(paste0(WD.1, "/Plots/Density/RMSE/RMSE_line_plot_",Sys.Date(),".png"),
       height = 220, width = 170, units = "mm", dpi = 300, bg = "white")

### Supplementary option ####
ggarrange( grid.arrange(
  ggarrange(NULL,
            # Spatial Scneario 1
            ggarrange(NULL, Scenario1_RMSE_Sup[[2]] + 
                        #scale_x_continuous(limits = c(-0.05*0.14,0.14), breaks = c(0,0.07,0.14), expand = c(0,0.01))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              plot.margin = margin(.05,-.2,.05,.0, "cm"),
                              strip.text.x = element_text(size = 9),
                              axis.text.y = element_text(size = 7),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25),
                              axis.ticks.y = element_line(linewidth = 0.25)),
                      Scenario1_RMSE_Sup[[5]] + 
                        #scale_x_continuous(limits = c(-0.05*0.52,0.54), breaks = c(0,0.26,0.52), expand = c(0,0.01))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),                  
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              strip.text.x = element_text(size = 9),
                              axis.ticks.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.text.x = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario1_RMSE_Sup[[6]] + 
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              strip.text.x = element_text(size = 9),
                              axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario1_RMSE_Sup[[3]] + 
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              strip.text.x = element_text(size = 9),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario1_RMSE_Sup[[1]] + 
                        scale_x_continuous(limits = c(-0.05*0.7,0.7), breaks = c(0,0.7, 0.7/2), expand = c(0,0.0002))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              strip.text.x = element_text(size = 9),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario1_RMSE_Sup[[4]] + 
                        # facet_nested(Scenario + scen ~  Species, scales = "free",
                        #              nest_line = element_line(linetype = 1), 
                        #              shrink = T)+
                        theme(strip.text.y = element_text(size = 7),
                              axis.text.y = element_blank(),
                              strip.text.x = element_text(size = 9),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,.0,.05,-.05, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      ncol = 7, widths = c(0.045, 1.28, 1, 1, 1, 1, 1.3)),NULL,
            ## Spatial Scenario 2
            ggarrange(NULL, Scenario2_RMSE_Sup[[2]] + 
                        #scale_x_continuous(limits = c(-0.05*0.14,0.14), breaks = c(0,0.07,0.14), expand = c(0,0.01))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),                  
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              plot.margin = margin(.05,-.2,.05,.0, "cm"),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.text.y = element_text(size = 7),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario2_RMSE_Sup[[5]] + 
                        #scale_x_continuous(limits = c(-0.05*0.52,0.54), breaks = c(0,0.26,0.52), expand = c(0,0.01))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),                  
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.ticks.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.text.x = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario2_RMSE_Sup[[6]] + 
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario2_RMSE_Sup[[3]] + 
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario2_RMSE_Sup[[1]] + 
                        scale_x_continuous(limits = c(-0.05*0.7,0.7), breaks = c(0,0.7, 0.7/2), expand = c(0,0.0002))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      Scenario2_RMSE_Sup[[4]] + 
                        # facet_nested(Scenario + scen ~  Species, scales = "free",
                        #              nest_line = element_line(linetype = 1), 
                        #              shrink = T)+
                        theme(strip.text.y = element_text(size = 7),
                              axis.text.y = element_blank(),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,.0,.05,-.05, "cm"),
                              axis.text.x = element_blank(),
                              axis.line = element_line(linewidth = 0.25)),
                      ncol = 7, widths = c(0.02,1.3, 1, 1, 1, 1, 1.3)),NULL,
            # Spatial Scenario 3
            ggarrange(Scenario3_RMSE_Sup[[2]] + 
                        #scale_x_continuous(limits = c(-0.05*0.14,0.14), breaks = c(0,0.07,0.14), expand = c(0,0.01))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),                  
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              plot.margin = margin(.05,-.2,.05,.0, "cm"),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.text.y = element_text(size = 7),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.text.x = element_text(size = 7),
                              axis.line = element_line(linewidth = 0.25),
                              axis.ticks.x = element_line(linewidth = 0.25)),
                      Scenario3_RMSE_Sup[[5]] + 
                        #scale_x_continuous(limits = c(-0.05*0.52,0.54), breaks = c(0,0.26,0.52), expand = c(0,0.0001))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),                  
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.ticks.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.text.x = element_text(size = 7),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.line = element_line(linewidth = 0.25),
                              axis.ticks.x = element_line(linewidth = 0.25)),
                      Scenario3_RMSE_Sup[[6]] + 
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_text(size = 7),
                              axis.line = element_line(linewidth = 0.25),
                              axis.ticks.x = element_line(linewidth = 0.25)),
                      Scenario3_RMSE_Sup[[3]] + 
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_text(size = 7),
                              axis.line = element_line(linewidth = 0.25),
                              axis.ticks.x = element_line(linewidth = 0.25)),
                      Scenario3_RMSE_Sup[[1]] + 
                        scale_x_continuous(limits = c(-0.05*0.7,0.7), breaks = c(0,0.7, 0.7/2), expand = c(0,0.0002))+
                        theme(strip.text.y = element_text(size = .01, colour = "white"),
                              strip.background.y = element_rect(fill = "transparent", colour = "transparent"),
                              axis.text.y = element_blank(),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,-.2,.05,-.2, "cm"),
                              axis.text.x = element_text(size = 7),
                              axis.line = element_line(linewidth = 0.25),
                              axis.ticks.x = element_line(linewidth = 0.25)),
                      Scenario3_RMSE_Sup[[4]] + 
                        # facet_nested(Scenario + scen ~  Species, scales = "free",
                        #              nest_line = element_line(linetype = 1), 
                        #              shrink = T)+
                        theme(strip.text.y = element_text(size = 7),
                              axis.text.y = element_blank(),
                              strip.text.x = element_blank(),
                              strip.background.x = element_blank(),
                              panel.spacing.y = unit(.2, "cm"),
                              axis.ticks.y = element_blank(),
                              plot.margin = margin(.05,.0,.05,-.05, "cm"),
                              axis.text.x = element_text(size = 7),
                              axis.line = element_line(linewidth = 0.25),
                              axis.ticks.x = element_line(linewidth = 0.25)),
                      ncol = 6, widths = c(1.3, 1, 1, 1, 1, 1.3)),
            
            ncol = 1, nrow = 6, 
            labels = c("","A: Spatial Scenario 1","" ,"B: Spatial Scenario 2","" ,"C: Spatial Scenario 3"), 
            heights = c(0.05,1,.05,1,0.05,1),
            font.label = list(size = 10.5), vjust=-0.1, hjust = 0),
  bottom = "RMSE/max(RMSE)", padding = unit(.5, "cm")), 
  get_plot_component(IQR_legend,
                     "guide-box-bottom",return_all = T), ncol = 1, nrow = 2, heights = c(1,0.08))

ggsave(paste0(WD.1, "/Plots/Density/RMSE/RMSE_line_plot_SUP_",Sys.Date(),".png"),
       height = 220, width = 170, units = "mm", dpi = 300, bg = "white")

## WEA and Closed Area Analysis ####

OSW_all = dplyr::bind_rows(OSW_Scen1, OSW_Scen2, OSW_Scen3)

#First get the area that overlaps with each WEA 
df_area = bind_rows(lapply(split(OSW_all, OSW_all$group), function(x){
  df1 = data.frame(OSW = x$group[1],
                   Area = OSW_all %>% filter(group %in% x$group[1]) %>% 
                     distinct(geometry) %>% st_intersection(Projection_extent) %>% 
                     st_area %>% sum)
  return(df1)
}))
df_area$Area = as.numeric(as.character(df_area$Area))

Avg_ind_OSW2 = merge(Avg_ind_OSW, df_area, by = "OSW")

Spec_shp = c("Dover" = 20,
             "Sablefish" = 3,
             "Shortspine" = 15,
             "Longspine" = 18,
             "Aurora" = 6, 
             "Rex" = 1)

ggplot(data = Avg_ind_OSW2 %>% filter(Quan%in% "Median",
                                      Met %in% "RMSE"), aes(x = Area, y = Val, shape = Species, colour = WEA_t, size = Val))+
  geom_point(alpha = 0.4)+
  facet_nested(scen~.)+
  scale_colour_manual(values = Colour_opt, name = "")+
  scale_y_continuous(expand = c(0,0), limits = c(0,0.4), breaks = seq(0,0.4,0.2), name = "RMSE/max(RMSE)")+
  scale_x_continuous(expand = c(0,0), limits = c(0,4200), name = bquote(Area~(km^2)))+
  scale_shape_manual(values = Spec_shp)+
  #scale_size_manual(name = "RMSE/max(RMSE)")+
  theme_classic()+
  guides(colour = guide_legend(position = "bottom", nrow = 2),
         size = guide_legend(position = "right", title = "RMSE/max(RMSE)"))+
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf)+
  theme(panel.spacing.y = unit(.4, "cm"),
        strip.text = element_text(size = 8))

ggsave(paste0(WD.1, "/Plots/Density/RMSE/RMSE_area_plot_",Sys.Date(),".png"),
       height = 220, width = 170, units = "mm", dpi = 300, bg = "white")

#######################
## Difference plots 
#######################

Dens_list_dif_ER = list()
Dens_list_dif_WEA = list()

# sp_PM = Dens_dif %>%
#   group_by(scen, X, Y) %>%
#   summarise(Dif = mean(Dif)) %>% ungroup() %>%
#   #filter(year == Term.year) %>%
#   mutate(Max_df = max(abs(Dif))) %>%
#   mutate(Dif = (Dif/Max_df)) %>%
#   group_by(scen, X, Y) %>%
#   summarise(Dif = mean(Dif)) %>%
#   select(X, Y, scen, Dif) %>%
#   ungroup()
gr_pred_dif_OSW

for(i in 1:6){
  gc()
  print(paste0("Starting iteration for species ", Species_vec[i]))
  Dif_data = All_species_nd[[i]]
  
  dif_data1 = list()
  for(j in 1:3){
    dif_data1[[j]] = Dens_Prc_Change_func.b(Dens_data = Dif_data[[j]]$Density,
                                          Dens_dif = Dif_data[[j]]$Density_Dif,
                                          #scen = year_df,
                                          Proj_extent = Projection_extent,
                                          OSW_shp = OSW_all_scen[[j]],
                                          Coast = coast_outline,
                                          lev_scen = c("All data", "1 year", "5 years", "10 years", "15 years", "19 years","Never surveyed"),
                                          Term.year = 2023
    )
    dif_data1[[j]]$Density_dif_ER = dif_data1[[j]]$Density_dif_ER %>%
      mutate(Spatial = OSW_all_scen[[j]]$Scenario[1],
             Species = Species_vec[[i]])
    
    dif_data1[[j]]$Density_dif_OSW = dif_data1[[j]]$Density_dif_OSW %>%
      mutate(Spatial = OSW_all_scen[[j]]$Scenario[1],
             Species = Species_vec[[i]])
    
    dif_data1[[j]]$Prc_change = dif_data1[[j]]$Dif %>%
      mutate(Spatial = OSW_all_scen[[j]]$Scenario[1],
             Species = Species_vec[[i]])
    
  }
  Dens_list_dif_ER[[i]] = bind_rows(lapply(dif_data1, "[[", 1))
  Dens_list_dif_WEA[[i]] = bind_rows(lapply(dif_data1, "[[", 2))
}
gc()

species_dif=bind_rows(Dens_list_dif_ER) %>% group_by(Species) %>% summarise(Max_Dif = max(Dif),
                                                                            Dif_99 = quantile(Dif,0.999))

# Dif_spec_plotdata =rbind(merge(bind_rows(Dens_list_dif_WEA), species_dif, by = "Species") %>% 
#         group_by(Species, scen, OSW, Spatial) %>%
#         summarise(Dif_s = mean(Dif)/unique(Max_Dif)) %>% mutate(Col = ifelse(Dif_s>0,"Pos", "Neg")) %>%
#         mutate(Dif_s = 0),
#         merge(bind_rows(Dens_list_dif_WEA), species_dif, by = "Species") %>% 
#         group_by(Species, scen, OSW, Spatial) %>%
#         summarise(Dif_s = mean(Dif)/unique(Max_Dif)) %>% mutate(Col = ifelse(Dif_s>0,"Pos", "Neg"))
#       )

Dif_spec_plotdata2 =merge(bind_rows(Dens_list_dif_WEA), species_dif, by = "Species") %>% 
  group_by(Species, scen, OSW, Spatial) %>%
  summarise(Dif_s = median(Dif)/unique(Dif_99),
            low = quantile(Dif,.25)/unique(Dif_99),
            up = quantile(Dif,.75)/unique(Dif_99))%>% 
  mutate(Col = ifelse(Dif_s>0,"Pos", "Neg")) %>% 
  pivot_longer(c(Dif_s, low, up), names_to = "Dif", values_to = "Val") 

## For manuscript - pull out numbers for longspine, spatial 3, Del norte 

merge(bind_rows(Dens_list_dif_WEA), species_dif, by = "Species") %>%
  group_by(Species, scen, OSW, Spatial) %>%
  summarise(Dif_s = median(Dif),
            low = quantile(Dif,.25),
            up = quantile(Dif,.75))%>%
  mutate(Col = ifelse(Dif_s>0,"Pos", "Neg")) %>%
  pivot_longer(c(Dif_s, low, up), names_to = "Dif", values_to = "Val")%>%
  filter(Species %in% "Longspine thornyhead",
         OSW %in% "Brookings_Large")


#Adjust the factor levels
Dif_spec_plotdata2$Species = factor(Dif_spec_plotdata2$Species, levels = c("Dover sole", "Sablefish", "Shortspine thornyhead", 
                                                                           "Longspine thornyhead","Aurora rockfish", "Rex sole"), 
                                    labels =  c("Dover", "Sablefish", "Shortspine", "Longspine","Aurora", "Rex"))

Dif_spec_plotdata2$Spatial = factor(Dif_spec_plotdata2$Spatial, levels = c("Scenario 1", "Scenario 2", "Scenario 3"), 
                                    labels =  c("Spatial 1", "Spatial 2", "Spatial 3"))

Dif_spec_plotdata2$scen = factor(Dif_spec_plotdata2$scen, levels = rev(c("1 year", "5 years", "10 years", "15 years", "19 years", "Never surveyed")))

Dif_spec_plotdata2 = Dif_spec_plotdata2 %>% mutate(OSW_3 = ifelse(OSW %in% "Humboldt", "Hm ",
                                                                  ifelse(OSW %in% "Brookings", "Br ",
                                                                         ifelse(OSW %in% "Coos Bay", "Cb ",
                                                                                ifelse(OSW %in% "Morro", "Mb ",
                                                                                       ifelse(OSW %in% "Del Norte", "Dn ",
                                                                                              ifelse(OSW %in% "Humboldt2", "Hm2",
                                                                                                     ifelse(OSW %in% "Humboldt3", "Hm3",
                                                                                                            ifelse(OSW %in% "Mendocino2", "Md ",
                                                                                                                   ifelse(OSW %in% "Monterey","Mr ",
                                                                                                                          ifelse(OSW %in% "Brookings_Large", "BrL",
                                                                                                                                 ifelse(OSW %in% "Coos Bay_Large", "CbL",
                                                                                                                                        ifelse(OSW %in% "Del Norte Oval", "DnO", 
                                                                                                                                               ifelse(OSW %in% "Humboldt Oval", "HmO", "MbO"))))))))))))))

Dif_spec_plotdata2$OSW_3 = factor(Dif_spec_plotdata2$OSW_3, levels = (c("Cb ", "CbL", "Br ", "BrL", "Dn ", "DnO", "Hm ","HmO", "Hm2", "Hm3", "Md ", "Mr ", "Mb ", "MbO")))

#Create x limits 

Dif_spec_plotdata2 %>% filter(Dif %in% c("up", "low"))%>% group_by(Species) %>%
  summarize(Upr = round(max(abs(Val))+.007,2),
            Upr2 = max(abs(Val)))

Dif_spec_plotdata2 = merge(Dif_spec_plotdata2 %>% filter(Dif %in% c("up", "low"))%>% group_by(Species) %>%
                             summarize(Upr = round(max(abs(Val))+.007,2),
                                       Upr2 = max(abs(Val))), 
                           Dif_spec_plotdata2, by = "Species"
)


spatial_vec = c("Spatial 1", "Spatial 2", "Spatial 3")
gc()
OSW_dif_plot = list()
for (i in 1:6){
  plot_data = Dif_spec_plotdata2 %>% filter(Species %in% unique(Dif_spec_plotdata2$Species)[i],
                                            OSW %in% c("Brookings","Humboldt", "Del Norte",
                                                       "Humboldt2", "Humboldt3", "Mendocino2",
                                                       "Brookings_Large", "Del Norte Oval", "Humboldt Oval"))
  
  OSW_dif_plot1 = list()
  
  for (j in 1:3){
    plot_data2 = plot_data %>% filter(Spatial %in% spatial_vec[j])
    
    plot_data2$Species = plot_data2$Species[drop=T]
    plot_data2$OSW_3 = plot_data2$OSW_3[drop=T]
    
    OSW_dif_plot1[[j]] = ggplot()+
      geom_vline(xintercept = 0, col = "grey50", lty = 2, lwd = 0.2)+
      geom_line(data = plot_data2 %>% filter(#scen %in% c("1 year", "15 years", "Never surveyed"),
                                             Spatial %in% spatial_vec[j],
                                             Species %in% plot_data2$Species[1],
                                             Dif %in% c("low","up")), 
                aes(y = scen, x = Val, colour = Col), lwd = .75, alpha = 0.4) +
      geom_point(data = plot_data2 %>% filter(#scen %in% c("1 year", "15 years", "Never surveyed"),
                                              Spatial %in% spatial_vec[j],
                                              Species %in% plot_data2$Species[1],
                                              Dif %in% c("Dif_s")),
                 aes(y = scen, x = Val, colour = Col), size = 0.25)+
      #geom_path(arrow = arrow(length = unit(0.05, "cm"), ends = "last", type = "closed"))+
      facet_nested(OSW_3 ~Species)+
      theme_classic()+
      scale_x_continuous(expand = c(0,0.001), limits = c(-1*abs(unique(plot_data2$Upr)+.02), abs(unique(plot_data2$Upr))+.02),
                         breaks=seq(-1*abs(unique(plot_data2$Upr)+.02), abs(unique(plot_data2$Upr))+.02,
                                    abs(unique(plot_data2$Upr)+.02)))+
      scale_colour_manual(values = c("Neg" = "#2166ac", "Pos" = "#b2182b"))+
      #annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = 0.25)+
      #annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, linewidth = 0.25)+
      labs(x = "mean(Dif)/max(Dif)", y = "")+
      theme(legend.position = "none",
            strip.placement = "outside",
            panel.spacing.y = unit(.2, "lines"),
            strip.background.y = element_blank(),
            axis.text.y = element_text(size = 7),
            strip.text.x = element_text(size = 9),
            strip.text.y = element_text(size = 8),
            plot.margin = unit(c(0, 0, 0, 0), "cm"),                  
            axis.ticks.x = element_blank(), 
            axis.text.x = element_text(size = 0.75, angle=90),
            axis.title.x = element_blank(),
            axis.line = element_line(linewidth = 0.25),
            axis.ticks.y = element_line(linewidth = 0.25))+
      labs(y = "", x = "RMSE")+
      annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = 0.25)+
      annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd = 0.25)
    
    
  }
  OSW_dif_plot[[i]] = OSW_dif_plot1
  
}

OSW_dif_plot1 = lapply(OSW_dif_plot, "[[", 1)
OSW_dif_plot2 = lapply(OSW_dif_plot, "[[", 2)
OSW_dif_plot3 = lapply(OSW_dif_plot, "[[", 3)

gc()

Difference_plot=grid.arrange(
  ggarrange(NULL,
            #Spatial Scenario 1
            ggarrange(OSW_dif_plot1[[2]] + 
                        theme(strip.background.y = element_blank(),
                              strip.text.y = element_blank(),
                              axis.text.x = element_blank()), NULL,
                      OSW_dif_plot1[[5]] +
                        scale_x_continuous(expand = c(0,0), limits = c(-.66,.66),
                                           breaks = seq(-0.66, 0.66, 0.66))+
                        theme(strip.background.y = element_blank(),
                              strip.text.y = element_blank(),
                              axis.text.y = element_blank(),
                              axis.text.x = element_blank(),
                              axis.ticks.y = element_blank()), NULL,
                      OSW_dif_plot1[[6]] + 
                        theme(strip.background.y = element_blank(),
                              strip.text.y = element_blank(),
                              axis.text.y = element_blank(),
                              axis.text.x = element_blank(),
                              axis.ticks.y = element_blank()), NULL,
                      OSW_dif_plot1[[3]] + 
                                  scale_x_continuous(expand = c(0,0), limits = c(-.42,.42),
                                                     breaks = seq(-0.42, 0.42, 0.42))+
                                  theme(strip.background.y = element_blank(),
                              strip.text.y = element_blank(),
                              axis.text.y = element_blank(),
                              axis.text.x = element_blank(),
                              axis.ticks.y = element_blank()), NULL,
                      OSW_dif_plot1[[1]] + 
                        theme(strip.background.y = element_blank(),
                              strip.text.y = element_blank(),
                              axis.text.y = element_blank(),
                              axis.text.x = element_blank(),
                              axis.ticks.y = element_blank()), NULL,
                      OSW_dif_plot1[[4]] + 
                        scale_x_continuous(expand = c(0,0), limits = c(-.66,.66),
                                           breaks = seq(-0.66, 0.66, 0.66))+
                        theme(axis.text.y = element_blank(),
                              axis.text.x = element_blank(),
                              axis.ticks.y = element_blank()), ncol = 11, nrow = 1, 
                      widths = c(1.75,0.03,1, 0.03, 1, 0.03, 1, 0.03, 1, 0.03, 1.15), 
                      labels = c("","","*","","","","*","","","",""),
                      hjust = -1.9), NULL,
            #Spatial scenario 2
            ggarrange(OSW_dif_plot2[[2]] + 
                        theme(strip.background.y = element_blank(),
                              strip.background.x = element_blank(),
                              strip.text.x = element_blank(),
                              strip.text.y = element_blank(),
                              axis.text.x = element_blank()), NULL,
                      OSW_dif_plot2[[5]] +
                        scale_x_continuous(expand = c(0,0), limits = c(-.66,.66),
                                           breaks = seq(-0.66, 0.66, 0.66))+
                        theme(strip.background.y = element_blank(),
                              strip.text.y = element_blank(),
                              strip.background.x = element_blank(),
                              strip.text.x = element_blank(),
                              axis.text.y = element_blank(),
                              axis.text.x = element_blank(),
                              axis.ticks.y = element_blank()), NULL,
                      OSW_dif_plot2[[6]] + 
                        theme(strip.background.y = element_blank(),
                              strip.text.y = element_blank(),
                              strip.background.x = element_blank(),
                              strip.text.x = element_blank(),
                              axis.text.y = element_blank(),
                              axis.text.x = element_blank(),
                              axis.ticks.y = element_blank()), NULL,
                      OSW_dif_plot2[[3]] + 
                        scale_x_continuous(expand = c(0,0), limits = c(-.42,.42),
                                           breaks = seq(-0.42, 0.42, 0.42))+
                        theme(strip.background.y = element_blank(),
                              strip.text.y = element_blank(),
                              strip.background.x = element_blank(),
                              strip.text.x = element_blank(),
                              axis.text.y = element_blank(),
                              axis.text.x = element_blank(),
                              axis.ticks.y = element_blank()), NULL,
                      OSW_dif_plot2[[1]] + 
                        theme(strip.background.y = element_blank(),
                              strip.text.y = element_blank(),
                              strip.background.x = element_blank(),
                              strip.text.x = element_blank(),
                              axis.text.y = element_blank(),
                              axis.text.x = element_blank(),
                              axis.ticks.y = element_blank()), NULL,
                      OSW_dif_plot2[[4]] + 
                        scale_x_continuous(expand = c(0,0), limits = c(-.66,.66),
                                           breaks = seq(-0.66, 0.66, 0.66))+
                        theme(strip.background.x = element_blank(),
                              strip.text.x = element_blank(),
                              axis.text.y = element_blank(),
                              axis.text.x = element_blank(),
                              axis.ticks.y = element_blank()), ncol = 11, nrow = 1, 
                      widths = c(1.75,0.03,1, 0.03, 1, 0.03, 1, 0.03, 1, 0.03, 1.15)), NULL,
            # Spatial sceanrio 3
            ggarrange(OSW_dif_plot3[[2]] + 
                        theme(strip.background.y = element_blank(),
                              strip.background.x = element_blank(),
                              strip.text.x = element_blank(),
                              strip.text.y = element_blank(),
                              axis.text.x = element_text(size = 6)), NULL,
                      OSW_dif_plot3[[5]]  +
                        scale_x_continuous(expand = c(0,0), limits = c(-.66,.66),
                                           breaks = seq(-0.66, 0.66, 0.66))+
                        theme(strip.background.y = element_blank(),
                              strip.text.y = element_blank(),
                              strip.background.x = element_blank(),
                              strip.text.x = element_blank(),
                              axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(),
                              axis.text.x = element_text(size = 6)), NULL,
                      OSW_dif_plot3[[6]] + 
                        theme(strip.background.y = element_blank(),
                              strip.text.y = element_blank(),
                              strip.background.x = element_blank(),
                              strip.text.x = element_blank(),
                              axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(),
                              axis.text.x = element_text(size = 6)), NULL,
                      OSW_dif_plot3[[3]] + 
                        scale_x_continuous(expand = c(0,0), limits = c(-.42,.42),
                                           breaks = seq(-0.42, 0.42, 0.42))+
                        theme(strip.background.y = element_blank(),
                              strip.text.y = element_blank(),
                              strip.background.x = element_blank(),
                              strip.text.x = element_blank(),
                              axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(),
                              axis.text.x = element_text(size = 6)), NULL,
                      OSW_dif_plot3[[1]] + 
                        theme(strip.background.y = element_blank(),
                              strip.text.y = element_blank(),
                              strip.background.x = element_blank(),
                              strip.text.x = element_blank(),
                              axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(),
                              axis.text.x = element_text(size = 6)), NULL,
                      OSW_dif_plot3[[4]] + 
                        scale_x_continuous(expand = c(0,0), limits = c(-.66,.66),
                                           breaks = seq(-0.66, 0.66, 0.66))+
                        theme(strip.background.x = element_blank(),
                              strip.text.x = element_blank(),
                              axis.text.y = element_blank(),
                              axis.ticks.y = element_blank(),
                              axis.text.x = element_text(size = 6)), ncol = 11, nrow = 1, 
                      widths = c(1.75,0.03,1, 0.03, 1, 0.03, 1, 0.03, 1, 0.03, 1.15)),
            ncol = 1, nrow = 6, labels = c("A: Spatial Scenario 1","", "B: Spatial Scenario 2","", "C: Spatial Scenario 3",""), 
            font.label = list(size = 10),heights = c(.085,.6,0.07,1.1,0.07,1), hjust = 0),
  bottom = grid.text(expression(frac(Dif[WEA],Dif["99%, ER"]))))
ggsave(paste0(WD.1, "/Plots/Density/Difference LINE ",Sys.Date(),".png"),
       Difference_plot,
       height = 220, width = 170, units = "mm", dpi = 300, bg = "white")
head(Dif_spec_plotdata)

gc()

full_plot = ggarrange(ggarrange(OSW_dif_plot3[[5]] + 
            scale_x_continuous(expand = c(0,0), limits = c(-1, 19.1),
                               breaks = c(-1, 0, 19))+ 
            theme(strip.background.y = element_blank(),
                                     strip.text.y = element_blank(),
                  axis.text.x = element_text(size = 6, angle = 0)) , NULL,
          OSW_dif_plot3[[3]]  +             
            scale_x_continuous(expand = c(0,0), limits = c(-1, 6.1), breaks = c(-1, 0, 6))+ 
            theme(axis.text.y = element_blank(),
                  axis.text.x = element_text(size = 6, angle = 0)),
          ncol = 3, widths = c(1,00,1)), NULL, nrow = 2, heights = c(1,0.025))
 


ggsave(paste0(WD.1, "/Plots/Density/Difference LINE FULL_",Sys.Date(),".png"),
       grid.arrange(full_plot,
             bottom = grid.text(expression(frac(Dif[WEA],Dif["99%, ER"])))),
       height = 220/2.5, width = 170, units = "mm", dpi = 300, bg = "white")


Dif_spec_plotdata2 %>% filter(Species=="Sablefish",
                              #Spatial == "Spatial 3",
                              scen == "Never surveyed")

Dif_spec_plotdata2 %>% filter(Species=="Longspine",
                              #Spatial == "Spatial 3",
                              scen == "Never surveyed")

#############################################
## Correlation/median-bassed linear analysis 
#############################################

#Originally started with a correlation analysis. however, highly correlated values of 1 could be for high density high error or 
#low density low error - meaning that it isn't really reaching the question I wanted. For that reason I am moving forward with 
#a median based 

# RMSE 
Sablefish_nd[[1]]$Density
Sab1_plotdata = list()
Spat_1 = c("Spatial 1", "Spatial 2", "Spatial 3")

for(j in 1:3){
  # Get density estimates 
  sabl1 = Dens_plot_func2(Dens_data = Sablefish_nd[[j]]$Density,
                                 #Dens_dif = Dover$Density_Dif,
                                 scen = year_df,
                                 Proj_extent = Projection_extent,
                                 OSW_shp = OSW_all_scen[[j]],
                                 Coast = coast_outline,
                                 lev_scen = c("All data", "1 year", "5 years", "10 years", "15 years", "19 years","Never surveyed"),
                                 Term.year = 2023)
  Sab1_plotdata[[j]]=merge(Sablefish_nd[[j]]$Density_PM_OSW, 
                           sabl1$Density_dif_OSW %>% filter(scen %in% "All data"), 
                           by = c("X", "Y", "OSW"))
  Sab1_plotdata[[j]]$Scenario = Spat_1[j]
  
}

library("mblm")


Sab_plot_data = Sab_plot_data %>% mutate(RMSE2 = RMSE/max(RMSE),
                         Est2 = Est/max(Est))

Med_reg_df=lapply(split(Sab_plot_data, f = with(Sab_plot_data, list(OSW, Scenario, scen))),
       function(x){
           if(dim(x)[1] == 0){
             try1 = NA
             try_df = data.frame(OSW = NA,
                                      Scenario = NA,
                                      scen = NA,
                                      Slope = NA,
                                      sig = NA,
                                      sig.test = NA)
             
           }else{
             try1 = mblm(RMSE~Est, data = x, repeated = T)
             try_df = data.frame(OSW = x$OSW[1],
                                 Scenario = x$Scenario[1],
                                 scen = x$scen[1],
                                 Slope = try1$coefficients[2],
                                 sig = summary(try1)[[4]][2,4],
                                 sig.test = ifelse(summary(try1)[[4]][2,4]<0.05,1,0))
             
         }
        return(list(try_df, try1))
       })

Sab_plot_datab = Sab_plot_data %>% group_by(OSW, Scenario, scen) %>% mutate(RMSE2 = RMSE/max(RMSE),
                                                                               Est2 = Est/max(Est),
                                                                            OSW_Scen = paste0(OSW, "_",Scenario)) %>%
  summarize(RMSE_med = median(RMSE2),
            Est_med = median(Est2))
Sab_plot_datab = Sab_plot_datab %>% data.frame()
rownames(Sab_plot_datab) = with(Sab_plot_datab, paste(OSW, Scenario, scen))
lapply(split(Sab_plot_datab, f = list(Sab_plot_datab$OSW, Sab_plot_datab$Scenario, Sab_plot_datab$scen))[[1]], function(x){
  df = data.frame(cor1 = cor())
})
cor(Sab_plot_datab[,4:5])

Sab_plot_data$scen = factor(Sab_plot_data$scen.x, levels = (c("1 year", "5 years", "10 years", "15 years", "19 years", "Never surveyed")))
gc()
Temp_scen = c("1 year", "5 years", "10 years", "15 years", "19 years", "Never surveyed")

ggplot(data = Sab_plot_data %>% filter(Scenario == "Spatial 3"), aes(y = (RMSE2), x = (Est2), colour = scen))+
  geom_point(alpha = 0.1, size = 0.25)+
  facet_nested(Scenario ~ OSW)+
  #stat_ellipse()+
  theme_classic()+
  scale_colour_viridis_d()

Sab_plot_data2 = Sab_plot_data %>% select(Scenario, scen, RMSE2, Est2, OSW, X, Y) %>%
  pivot_wider(names_from = scen, values_from = RMSE2)

Sab_Corplot_Data1=bind_rows(lapply(split(Sab_plot_data2, f = list(Sab_plot_data2$Scenario,Sab_plot_data2$OSW)), function(x){
  cor1 = list()
    for(i in 1:length(Temp_scen)){
      x2 = x %>% dplyr::select(Temp_scen[i], Est2) %>% drop_na(Temp_scen[i])
      colnames(x2) = c("RMSE", "Est")
      if(dim(x2)[1] == 0){
        cor1[[i]] = NA
        cor1[[i]] = cor1[[i]] %>% data.frame()
        rownames(cor1[[i]]) = Temp_scen[i]
      }else{
        cor1[[i]]=cor(x2  #%>% mutate(Log_RMSE = log(RMSE)) 
                      %>% dplyr::select(RMSE),
          x2 %>% #mutate(Log_Est=log(Est))%>% 
            dplyr::select(Est)) 
        cor1[[i]] = cor1[[i]] %>% data.frame()
        rownames(cor1[[i]]) = Temp_scen[i]
        
      }
      
    }
  cor2 = bind_rows(cor1)
  cor2b = cbind(rownames(cor2), cor2, x$OSW[1], x$Scenario[1])
  colnames(cor2b) = c("scen", "Est", "OSW", "Spatial")
  return(cor2b)
}))
Sab_Corplot_Data1 = Sab_Corplot_Data1 %>% drop_na(Est)


Sab_Corplot_Data1$scen = factor(Sab_Corplot_Data1$scen, levels = (c("1 year", "5 years", "10 years", "15 years", "19 years", "Never surveyed")))

ggplot(data = Sab_Corplot_Data1 %>% filter(Spatial =="Spatial 2"), aes(x = OSW, y = scen, fill = Est))+
  geom_tile() + 
  scale_fill_gradient2(low="blue",
                       high = "red",
                       mid = "white",
                       limits = c(-1,1),
                       midpoint = 0)
