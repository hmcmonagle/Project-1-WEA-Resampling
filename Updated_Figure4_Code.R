################################################################################
################################################################################
## Figure 4 creation (and supplementary plots)
## By Kaitlyn O'Brien 
################################################################################
################################################################################

#This file includes all the analysis of the output from the sdmTMB models for 
#the spatial closure project. Additionally, all the code to create figures in 
#the manuscript (supplementary and main document) as well as other figures are incorporated throughout

#####################
## Libraries 
#####################
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
library("ggpmisc")
library("ggbreak")
#library("nwfscSurvey")
library("eply")
#library("sdmTMBextra")
library("units")
library("RANN")
library("scales")
library("ggh4x")
library("pbapply")
library(cowplot)
library(fmesher)
library(grid)
library("png")
library(cowplot)
library(grid)
gc()
## Working directory 
WD.1=("D:/OAI_job/Offshore_Wind_PJ1/Data")

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

OSW_all_scen = list(OSW_Scen1, OSW_Scen2_outline, OSW_Scen3)

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
#Rotate the shapefile 
rotate_sf <- function(data, angle_degrees, pivot) {
  a <- angle_degrees * pi / 180
  rot_matrix <- matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
  geom <- st_geometry(data)
  rotated_geom <- (geom - pivot) * rot_matrix + pivot
  
  # If the input is a full sf data frame, replace its geometry column
  if (inherits(data, "sf")) {
    st_geometry(data) <- rotated_geom
    st_crs(data) <- NA 
    return(data)
  } else {
    # If the input is just raw geometry (sfc/sfg), return the rotated geometry directly
    st_crs(rotated_geom) <- NA
    return(rotated_geom)
  }
}#get_rotated_polygons (Clockwise)

#Rotate the data 
get_rotated_polygons <- function(grid_data, x_col = "X", y_col = "Y", 
                                 resolution, angle_degrees, 
                                 origin_x = NULL, origin_y = NULL) {
  
  # 1. Setup rotation math
  angle_rad <- angle_degrees * (pi / 180)
  if(is.null(origin_x)) origin_x <- mean(grid_data[[x_col]], na.rm = TRUE)
  if(is.null(origin_y)) origin_y <- mean(grid_data[[y_col]], na.rm = TRUE)
  
  # 2. Give each cell a unique ID (required by geom_polygon to know which corners connect)
  grid_data$cell_id <- 1:nrow(grid_data)
  h_res <- resolution / 2 # Half resolution to find corners from the center
  
  # 3. Calculate the 4 corners for every single center point
  # Order matters for polygons: bottom-left, bottom-right, top-right, top-left
  corners_list <- lapply(1:nrow(grid_data), function(i) {
    cx <- grid_data[[x_col]][i]
    cy <- grid_data[[y_col]][i]
    id <- grid_data$cell_id[i]
    
    data.frame(
      cell_id = id,
      corner_x = c(cx - h_res, cx + h_res, cx + h_res, cx - h_res),
      corner_y = c(cy - h_res, cy - h_res, cy + h_res, cy + h_res)
    )
  })
  
  # Bind all corners into one dataframe
  corners <- do.call(rbind, corners_list)
  
  # 4. Merge the original data back in (so you keep your prediction values for coloring)
  poly_data <- merge(corners, grid_data, by = "cell_id")
  
  # 5. Apply the rotation matrix to the CORNERS (not the centers)
  x_shifted <- poly_data$corner_x - origin_x
  y_shifted <- poly_data$corner_y - origin_y
  
  poly_data$X_rot <- (x_shifted * cos(angle_rad)) - (y_shifted * sin(angle_rad)) + origin_x
  poly_data$Y_rot <- (x_shifted * sin(angle_rad)) + (y_shifted * cos(angle_rad)) + origin_y
  
  return(poly_data)
} #get_rotated_polygons (Counter-Clockwise)

########################
## Data Loading 
########################

#Rotated polygon/shapefiles for plotting information 
pivot_point <- st_centroid(st_union(coast_outline))

# Dover sole #
Dover1 = readRDS("C:/Users/User/Dropbox/OAI_job/RDS/Rotated_Polygon_Dover sole.rds")

# Now I need to create a new grid/tiles for plotting that are tilted along the new rotation axis 

head(Dover1)#Includes both RMSE and MAE for plots 

#CREATE THE GRID BASED ON THE ROTATED VALUE 
grid_pred <- st_bbox(Dover1 %>% filter(scen %in% "1 year",
                                     Scenario %in% "Scenario 1") %>% 
                       dplyr::select(scen, Scenario, X_rot, Y_rot) %>% 
                       distinct() %>% 
                       st_as_sf(coords = c("X_rot", "Y_rot"))) %>% 
  st_make_grid(cellsize = c(0.5), what = "polygons") %>% 
  st_as_sf() %>% 
  mutate(grid_id = row_number())

# Calculate centroids and X/Y on the grid
coords <- st_coordinates(st_centroid(grid_pred))
grid_pred <- grid_pred %>%
  mutate(
    X = coords[, "X"],
    Y = coords[, "Y"]
  )

#Cut the grid based on survey scale 
grid_pred <- grid_pred %>% st_intersection(rotate_sf(data = Projection_extent, angle_degrees = 20, pivot = pivot_point))

Species_vec = c( "Aurora rockfish","Sablefish", "Longspine thornyhead", 
                 "Shortspine thornyhead", "Dover sole",
                 "Rex sole")

for(j in 1:length(Species_vec)){
  print(paste0("Starting plot for ", Species_vec[j]))
  #Load species infomration 
  spec_rot = readRDS(paste0("C:/Users/User/Dropbox/OAI_job/RDS/Rotated_Polygon_",Species_vec[j],".rds"))
  
  #Get on the intersection/new grid cells for the rotated data 
  sf_pred <- st_join(spec_rot %>% dplyr::select(X_rot, Y_rot, RMSE, MAE, scen, Scenario) %>% distinct() %>%
                       st_as_sf(coords = c("X_rot", "Y_rot")), grid_pred, join = st_intersects)
  
  #Now we need to shift the scenarios to the left 
  
  # 1. Calculate how far to slide each map (Grid Width + 10% padding gap)
  x_range <- range(grid_pred$X, na.rm = TRUE)
  x_width <- x_range[2] - x_range[1]
  slide_distance <- x_width * 1.15 # Adjust the 1.10 to make the gap wider or narrower
  
  # 2. Assign a slide multiplier to your scenarios
  # "Never surveyed" is 0 (stays in original spot). 
  # Sliding left means using negative multipliers (-1, -2, -3).
  sf_pred <- sf_pred %>%
    mutate(
      # NOTE: Ensure these string names exactly match your data in the `scen` column!
      slide_mult = case_when(
        scen == "Never surveyed" ~ 0,
        scen == "19 years"         ~ -1,
        scen == "15 years"        ~ -2, # Add/remove scenarios as needed
        scen == "10 years"        ~ -3,
        scen == "5 years"        ~ -4,
        scen == "1 year"        ~ -5,
        TRUE                     ~ 0
      ),
      # Calculate the new slid X coordinate
      X_shifted = X + (slide_mult * slide_distance)
    )
  sf_pred <- sf_pred %>% 
    mutate(scen2 = case_match(
      scen,
      "Never surveyed" ~ "NS",
      "1 year" ~"1",
      "5 years" ~"5",
      "10 years" ~"10",
      "15 years" ~"15",
      "19 years" ~"19",
      .default = scen
    ))
  # 3. Slide the OSW
  unique_multipliers <- unique(sf_pred$slide_mult[!is.na(sf_pred$slide_mult)])
  
  Shift_OSW = list()
  for(i in 1:3){
    Shift_OSW[[i]] <- purrr::map_dfr(unique_multipliers, function(mult) {
      # Rotate the base coast first
      OSW_rot <- rotate_sf(data = OSW_all_scen[[i]], angle_degrees = 20, pivot = pivot_point)
      
      # Shift the geometry by the exact same distance we shifted the tiles
      geom <- st_geometry(OSW_rot)
      geom <- geom + c(mult * slide_distance, 0) # Add X shift, 0 Y shift
      st_crs(geom) <- st_crs(OSW_rot) # Reassign the Coordinate Reference System
      st_geometry(OSW_rot) <- geom
      
      return(OSW_rot)
    })
  }
  
  Text_df = sf_pred %>% filter(Scenario == "Scenario 1") %>% 
    group_by(scen2, Scenario) %>% 
    summarize(X_center = mean(X_shifted, na.rm=T), Y_top = max(Y, na.rm=T))
  
  sf_pred = sf_pred %>% drop_na()
  
  summary(sf_pred)
  gc()
  # 4. Plot it all together!
  Fig4_spec_RMSE=ggplot() + 
    geom_sf(data = rotate_sf(data = coast_outline, angle_degrees = 20, pivot = pivot_point)) + 
    # Plot ALL shifted tiles at once (removed the filter)
    geom_tile(
      data = sf_pred %>% filter(!is.na(RMSE)), # Filter NAs to keep grid clean 
      aes(x = X_shifted, y = Y, fill = RMSE)
    ) +
    # Styling
    scale_fill_gradient2(
      low = "#fff5f0", 
      mid = "#fff5f0", 
      high = "#a50f15",
      midpoint = quantile(sf_pred$RMSE, 0.75),
      limits = c(0, quantile(sf_pred$RMSE, 0.995)),
      na.value = "#a50f15", name = paste0("RMSE"))+
    # Add the shifted coastlines underneath/around the tiles
    geom_sf(
      data = bind_rows(Shift_OSW),
      fill = "transparent",
      color = "black",
      linewidth = 0.1
    ) +
    #Add text labels above each map (Optional, but highly recommended!)
    # geom_text(
    #   data = Text_df,
    #   aes(x = X_center+(x_width * .05), y = Y_top + (x_width * 0.2), label = scen2),
    #   size = 3.8
    # ) +
    geom_label(
      data = Text_df,
      aes(x = X_center, y = Y_top + (x_width * 0.21), label = scen2),
      size = 3.8,
      color = "black",          # Set text color to white
      label.padding = unit(0.25, "lines"), # Adjust box padding
      label.size = 0.25,         # Adjust box border size
      nudge_y = 0.05             # Nudge the labels slightly
    )+
    facet_wrap(Scenario~., strip.position = "left", nrow = 3)+
    scale_x_continuous(limits = c(-2150,735), expand = c(0,0))+
    coord_sf() + # Ensures aspect ratio stays 1:1 so maps don't stretch
    theme_classic() +
    theme(
      legend.position = "bottom", # Moved legend so it doesn't crowd the left side
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = 13),
      legend.key.width = unit(1, "cm")
    )
  
  Fig4_spec_MAE=ggplot() + 
    geom_sf(data = rotate_sf(data = coast_outline, angle_degrees = 20, pivot = pivot_point)) + 
    # Plot ALL shifted tiles at once (removed the filter)
    geom_tile(
      data = sf_pred %>% filter(!is.na(MAE)), # Filter NAs to keep grid clean 
      aes(x = X_shifted, y = Y, fill = MAE)
    ) +
    # Styling
    scale_fill_gradient2(
      low = "#ece7f2", 
      mid = "#ece7f2", 
      high = "#045a8d",
      midpoint = quantile(sf_pred$MAE, 0.5),
      limits = c(0, quantile(sf_pred$MAE, 0.99)),
      na.value = "#045a8d", name = paste0("MAE"))+
    # Add the shifted coastlines underneath/around the tiles
    geom_sf(
      data = bind_rows(Shift_OSW),
      fill = "transparent",
      color = "black",
      linewidth = 0.1
    ) +
    #Add text labels above each map (Optional, but highly recommended!)
    # geom_text(
    #   data = Text_df,
    #   aes(x = X_center+(x_width * .05), y = Y_top + (x_width * 0.2), label = scen2),
    #   size = 3.8
    # ) +
    geom_label(
      data = Text_df,
      aes(x = X_center+(x_width * .05), y = Y_top + (x_width * 0.23), label = scen2),
      size = 3.8,
      color = "black",          # Set text color to white
      label.padding = unit(0.25, "lines"), # Adjust box padding
      label.size = 0.25,         # Adjust box border size
      nudge_y = 0.05             # Nudge the labels slightly
    )+
    facet_wrap(Scenario~., strip.position = "left", nrow = 3)+
    scale_x_continuous(limits = c(-2150,735), expand = c(0,0))+
    coord_sf() + # Ensures aspect ratio stays 1:1 so maps don't stretch
    theme_classic() +
    theme(
      legend.position = "bottom", # Moved legend so it doesn't crowd the left side
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      strip.background = element_blank(),
      strip.text = element_text(size = 13, colour = "white"),
      legend.key.width = unit(1, "cm")
    )
  
  print(paste0("Plotting the RMSE and MAE for ", Species_vec[j]))
  ggsave(paste0("D:/OAI_job/Offshore_Wind_PJ1/Data/Plots/Paper_Fisheries_Research/NEW/",Species_vec[j],"_RMSE_MAE_",Sys.Date(),".png"),
         ggpubr::ggarrange(NULL, ggpubr::ggarrange(Fig4_spec_RMSE, Fig4_spec_MAE, ncol = 2, widths = c(1,1), labels = c("A", "B"),
                                                   vjust = .5,
                                                   font.label = list(size = 12)), nrow = 2, heights = c(0.02,1)),
         width = 7480, height = 8800, units = "px", dpi = 1000, bg = "white")
  
  
}

 xx?ggsave
?ggpubr::ggarrange

## Facetted version 
 
 Dov_land_MAE=ggplot() + 
   geom_sf(data = rotate_sf(data = coast_outline, angle_degrees = 20, pivot = pivot_point)) + 
   # Plot ALL shifted tiles at once (removed the filter)
   geom_tile(
     data = sf_pred %>% filter(!is.na(MAE)), # Filter NAs to keep grid clean 
     aes(x = X, y = Y, fill = MAE)
   ) +
   # Styling
   scale_fill_gradient2(
     low = "#f7fbff", 
     mid = "#f7fbff", 
     high = "#08519c",
     midpoint = quantile(sf_pred$MAE, 0.5),
     limits = c(0, quantile(sf_pred$MAE, 0.99)),
     na.value = "#08519c", name = paste0("MAE"))+
   # Add the shifted coastlines underneath/around the tiles
   geom_sf(
     data = bind_rows(Shift_OSW),
     fill = "transparent",
     color = "black",
     linewidth = 0.1
   ) +
   ggh4x::facet_nested(Scenario~scen2, switch ="y")+
   scale_x_continuous(limits = c(200,735), expand = c(0,0))+
   coord_sf() + # Ensures aspect ratio stays 1:1 so maps don't stretch
   theme_classic() +
   theme(
     legend.position = "bottom", # Moved legend so it doesn't crowd the left side
     axis.line = element_blank(),
     strip.placement = "outside",
     axis.text = element_blank(),
     axis.ticks = element_blank(),
     axis.title = element_blank(),
     strip.background.y = element_blank(),
     strip.text.x = element_text(size = 13),
     strip.text.y = element_text(size = 13, colour = "white"),
     legend.key.width = unit(1, "cm")
   )
 
 Dov_land_RMSE=ggplot() + 
   geom_sf(data = rotate_sf(data = coast_outline, angle_degrees = 20, pivot = pivot_point)) + 
   # Plot ALL shifted tiles at once (removed the filter)
   geom_tile(
     data = sf_pred %>% filter(!is.na(RMSE)), # Filter NAs to keep grid clean 
     aes(x = X, y = Y, fill = RMSE)
   ) +
   # Styling
   scale_fill_gradient2(
     low = "#fff5f0", 
     mid = "#fff5f0", 
     high = "#a50f15",
     midpoint = quantile(sf_pred$RMSE, 0.5),
     limits = c(0, quantile(sf_pred$RMSE, 0.99)),
     na.value = "#a50f15", name = paste0("RMSE"))+
   # Add the shifted coastlines underneath/around the tiles
   geom_sf(
     data = bind_rows(Shift_OSW),
     fill = "transparent",
     color = "black",
     linewidth = 0.1
   ) +
   #Add text labels above each map (Optional, but highly recommended!)
   # geom_text(
   #   data = Text_df,
   #   aes(x = X_center+(x_width * .05), y = Y_top + (x_width * 0.2), label = scen2),
   #   size = 3.8
   # ) +
   # geom_label(
   #   data = Text_df,
   #   aes(x = X_center, y = Y_top + (x_width * 0.15), label = scen2),
   #   fontface = "bold", size = 4,
   #   color = "black",          # Set text color to white
   #   label.padding = unit(0.25, "lines"), # Adjust box padding
   #   label.size = 0.25,         # Adjust box border size
   #   nudge_y = 0.05             # Nudge the labels slightly
   # )+
   ggh4x::facet_nested(Scenario~scen2, switch ="y")+
   #facet_wrap(Scenario~., strip.position = "left", nrow = 3)+
   scale_x_continuous(limits = c(200,735), expand = c(0,0))+
   coord_sf() + # Ensures aspect ratio stays 1:1 so maps don't stretch
   theme_classic() +
   theme(
     legend.position = "bottom", # Moved legend so it doesn't crowd the left side
     axis.line = element_blank(),
     strip.placement = "outside",
     axis.text = element_blank(),
     axis.ticks = element_blank(),
     axis.title = element_blank(),
     #strip.background = element_blank(),
     strip.text = element_text(size = 13),
     legend.key.width = unit(1, "cm")
   )
 
 
 ggsave(paste0("D:/OAI_job/Offshore_Wind_PJ1/Data/Plots/Paper_Fisheries_Research/NEW/Dover_RMSE_MAE_2",Sys.Date(),".png"),
        ggpubr::ggarrange(NULL, ggpubr::ggarrange(Dov_land_RMSE, Dov_land_MAE, ncol = 2, widths = c(1,1), labels = c("A", "B"),
                                                  vjust = .5,
                                                  font.label = list(size = 12)), nrow = 2, heights = c(0.02,1)),
        width = 7480, height = 7800, units = "px", dpi = 1000, bg = "white")
 

 