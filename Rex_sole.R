################################################################################
################################################################################
## Rex sole - WEA resampling experiment 
## By Kaitlyn O'Brien
################################################################################
################################################################################

#######################
## Libraries 
#######################
library("ggplot2")
library("raster")
library("terra")
library("tidyverse")
library("RANN")
library("sf")
library("sp")
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
#library("ggbreak")
library("nwfscSurvey")
library("eply")
#install.packages("sdmTMBextra")
library("units")
library("RANN")
library("scales")
library("ggh4x")
library("pbapply")
library("cowplot")
#library("officedown")
library("fmesher")
library("grid")
#library("png")
## Working directory 
WD.1=("C:/Users/kaitlyn.o'brien/Documents/GitHub/Offshore_Wind_PJ1/Data")

#############################
## Shapefile information 
##############################
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
#### OSW scenarios ######

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

ggplot(data = Proj_df)+
  geom_sf(data = coast_outline)+
  geom_point( aes(x = lon, y = lat,colour = avg_dep_m), size = 0.1)+
  scale_colour_viridis_c()+
  theme_classic()+
  geom_sf(data = OSW_Scen1, fill = "transparent", colour = "red")+
  geom_sf(data = OSW_Scen2_outline, fill = "transparent", colour = "red")+
  geom_sf(data = OSW_Scen3, fill = "transparent", colour = "red")

################################################################################
## Modeling ####
################################################################################

# Upon advice and comments from Eric Ward, model configurations were matched as 
# closely as possible to current stock assessment configurations for each species.  
# These configurations can be found on github at 
#https://github.com/pfmc-assessments/indexwc/blob/main/data-raw/configuration.csv
# Additionally, following analysis from a simulation study, the incorporation of 
# a mechanistic covariate (e.g., depth) was included to evaluate if this would 
# impact results 

###############
### Rex ####
###############

#### Functions ####
# Internal functions (used in the main function) ####
Pred_sdm_function = function(Pred_data = NA, model = NA, Mod_data = NA){
  ## Prediction ####
  Proj_df_years = replicate_df(Pred_data[,c("area_km2","lon","lat","depth_m", "group")], "year",
                               unique(Mod_data$year)) #Repeat the prediction data frame for the years that were analyzed 
  predictions_All_TMB <- predict(model, newdata = Proj_df_years, return_tmb_object=T)
  
  index <- get_index(predictions_All_TMB, 
                     area = predictions_All_TMB$data$area_km2, 
                     bias_correct = TRUE)
  index = rbind(index %>% dplyr::select(year, est, lwr, upr, log_est, se), 
                data.frame(year = 2020,
                           est = NA,
                           lwr = NA, 
                           upr = NA, 
                           log_est = NA,
                           se = NA))
  index$group = Pred_data[1,]$group
  ggplot(index, aes(as.numeric(as.character(year)), est)) + 
    geom_line() +
    geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.4) +
    xlab('Year') + ylab('Biomass estimate (kg)')
  return(list(Index = index,
              Pred = predictions_All_TMB$data))
} #This does the predictions of the sdmTMB model - specifically getting index information 

Dens_PM_func_DG = function(Dens_data = NA, 
                        scen = NA,
                        Proj_extent = NA,
                        OSW_shp = NA){
  Dens_data$Est = plogis(Dens_data$est1)*exp(Dens_data$est2)
  # List to be filled 
  SP_dens = Dens_data
  SP_dens_ER = Dens_data %>%
    filter(group %in% "Entire region")
  
  
  
  group_dif_ER = list()
  gr_group_xy <- list()
  gr_group = list()
  gr_pred_group = list()
  group_dif = list()
  group_list = list()
  
  #Grab points for plotting - entire region 
  gr=st_bbox(SP_dens[SP_dens$scen=="All data",] %>%
               st_as_sf(coords = c("lon", "lat"),
                        crs = st_crs("+proj=utm +zone=10 +datum=WGS84 +units=km"))) %>%
    st_make_grid(cellsize=.5, what='centers')%>%
    st_as_sf() %>%
    st_intersection(st_as_sfc((Proj_extent)))
  gr_xy <- st_coordinates(gr)
  
  #Grab points for plotting - OSW areas 
  for(i in 1:length(unique(OSW_shp$group))){ 
    #Create a data frame for each OSW area
    gr_group[[i]]=gr %>%
      st_intersection(st_as_sfc(OSW_shp[OSW_shp$group==unique(OSW_shp$group)[i],]))
    gr_group_xy[[i]] = st_coordinates(gr_group[[i]])
  }
  names(gr_group) = unique(OSW_shp$group)
  names(gr_group_xy) = unique(OSW_shp$group)
  
  
  
  for(i in 1:length(unique(scen$Scenario))){
    ### Part 1 - set up the data frame ####
    #Create dataframe of density where things are properly labeled and estimate is Exp
    df1 = SP_dens_ER %>%
      mutate(Exp = Est) %>%
      rename(X = lon, Y= lat) %>%
      dplyr::select(X, Y, scen, Exp, year) %>%
      rename(Scenario = scen)
    #Create two data frames for the two different scenarios of concern 
    df_scen =df1 %>%
      filter(Scenario == unique(scen$Scenario)[i]) %>%
      dplyr::select(X,Y,Exp,year)
    colnames(df_scen) = c("X", "Y", unique(scen$Scenario)[i], "year")
    df_all_data = df1 %>%
      filter(Scenario == "All data") %>%
      dplyr::select(X,Y,Exp,year) %>%
      rename("All data" = Exp)   
    #Merge the two dataframes together 
    DF_dif = merge(df_all_data, df_scen, by = c("X", "Y", "year"))
    group_dif_ER[[i]] = DF_dif %>% 
      mutate(Dif = .[[5]]-.[[4]],
             scen = unique(scen$Scenario)[i]) 
    colnames(group_dif_ER[[i]]) = c("X","Y","year", "All data", "Est", "Dif", "scen")
  }
  
  sp_Dif = bind_rows(group_dif_ER) #Want to save this as it has the estimates and the dif for each scneario option 
  
  #Now calculate the density performance metrics 
  sp_PM = sp_Dif %>%
    mutate(ID = paste0(X,"_",Y)) %>%
    group_by(ID, scen) %>%
    summarise(RMSE = sqrt((sum((Dif)^2))/length(unique(sp_Dif$year))),
              MAE = abs(sum(Dif))/length(unique(sp_Dif$year))) %>%
    separate(ID, into = c("X", "Y"), sep = "\\_")%>%
    mutate(X = as.numeric(as.character(X)),
           Y = as.numeric(as.character(Y)))
  
  gr_pred_PM = bind_rows(lapply(split(sp_PM, sp_PM$scen), function(x){
    pred_points <- x %>%
      dplyr::select(X,Y) %>% as.matrix()
    nns <- nn2(pred_points,gr_xy[,c("X", "Y")],k=1)$nn.idx
    gr_pred <- gr_xy %>% as_tibble() %>% mutate(RMSE=x$RMSE[nns],
                                                MAE = x$MAE[nns])
    gr_pred$scen = x$scen[1]
    return(gr_pred)
  }))
  
  gr_pred_PM_OSW = bind_rows(lapply(split(sp_PM, sp_PM$scen), function(x){
    scen_list = list()
    for(i in 1:length(gr_group_xy)){
      pred_points <- x %>%
        dplyr::select(X,Y) %>% as.matrix()
      nns <- nn2(pred_points,gr_group_xy[[i]][,c("X","Y")],k=1)$nn.idx
      gr_pred <- gr_group_xy[[i]] %>% as_tibble() %>% mutate(RMSE=x$RMSE[nns],
                                                             MAE = x$MAE[nns])
      gr_pred$scen = x$scen[1]
      gr_pred$OSW = names(gr_group_xy)[i]
      scen_list[[i]] = gr_pred
    }
    return(bind_rows(scen_list))
  }))
  return(list(Density_PM_ER = gr_pred_PM,
              Density_PM_OSW = gr_pred_PM_OSW,
              Density_PM_all = sp_PM,
              Density_Dif = sp_Dif))
} #This model predicts the model to get the density estimates over time


# Large Projection Function #### 

#This function fits the specific sdmTMB models to the data for the different temporal scenarios 
#It requires the data for the model, prediction data (in this case the WCGBTS data), the different spatial closures (OSW_shape), 
#a list of proposed temporal scenarios set up in a data frame (scen), and information for the mesh creation (S.edge, M.edge, and c.off),
# and the start and terminal years for the analysis. proj_extent is the polygon of the survey domain
PROJ_Func_Rex = function(scen = NA, #Data frame of the different scenarios of interest - column 1 is the scenario, column 2 is the terminal year where sampling occurred in the OSW area 
                               prediction_data = NA, #Data frame for predictions of the sdmTMB model - no years included at the moment
                               model_data = NA, #Data for the species for fitting the models (in this case the WCGBTS data)
                               OSW_shp = NA,#SF file (polygon) for the different areas of interest (ensure that there is a column identifying them - here it is called "group")
                               S.edge = NA,
                               M.edge = NA,
                               S.off = NA,
                               M.off = NA,
                               Start.year = NA,
                               Term.year = NA, #The terminal year to assess - only pull out the density for this year 
                               Proj_extent = NA, #Shapefile or data frame to trim the projection
                               c.off = NA){ #cutoff for the INLA mesh
  ## For this attempt I will be fitting models how they are specified in the "indexwc' webpage for the NWFSC
  ## For Rex this involves a tweedie distribution with ~500 knots and iid spatiotemporal component  
  
  #website - https://github.com/pfmc-assessments/indexwc/blob/main/data-raw/configuration.csv
  ################################################################################################
  #### Gather the prediction data frames - specifically separate the individual areas        ####
  ################################################################################################
  
  #### Step 1 - Set up the data frame so that we can remove the spatial closure areas ####
  #The entire prediction data frame is supplied to the function, but we need to specify the data frames for each spatial closure
  
  #This for loop goes through and pulls out the prediction data inside each spatial closure - in this case the proposed OSW areas
  Proj_df_OSW=list(NA)
  for(x in 1:length(unique(OSW_shp$group))){
    groups = unique(OSW_shp$group)
    OSW_a=OSW_shp%>%
      filter(group==groups[x])
    
    OSW_b=st_intersection(prediction_data %>% filter(depth_m>min(model_data$depth_m),
                                                     depth_m<max(model_data$depth_m)) %>%
                            st_as_sf(coords = c("lon", "lat"),
                                     crs = st_crs("+proj=utm +zone=10 +datum=WGS84 +units=km")), OSW_a)
    OSW_b$lon = st_coordinates(OSW_b)[,1]
    OSW_b$lat = st_coordinates(OSW_b)[,2]
    
    Proj_df_OSW[[x]] = data.frame(OSW_b)
    Proj_df_OSW[[x]]$group = unique(OSW_shp$group)[x]#Make sure group is labeled correctly 
  }  
  names(Proj_df_OSW) = unique(OSW_shp$group)#Name each element of the list by the OSW area
  
  #### Step 2 - Grab other spatial scales for index analsysis ####
  ## For this analysis we want to look at biomass estimates for other potential management scales - in this case north and south of 40.10
  
  Proj_df_OSW$North = prediction_data %>% filter(depth_m>min(model_data$depth_m)) %>% dplyr::select(lon, lat) %>%
    st_as_sf(coords = c("lon", "lat"),
             crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>%
    st_transform(crs = st_crs("+proj=longlat + datum=WGS84 + no_dfs")) %>% 
    st_coordinates() %>% as_tibble() %>% rename(Lon_dd = X, Lat_dd = Y) %>%
    cbind(prediction_data%>% filter(depth_m>min(model_data$depth_m))) %>%
    filter(Lat_dd >=40.16667) %>% #40.10 management line for the North South Division 
    mutate(group = "North")
  
  Proj_df_OSW$South = prediction_data %>% filter(depth_m>min(model_data$depth_m)) %>% dplyr::select(lon, lat) %>%
    st_as_sf(coords = c("lon", "lat"),
             crs = "+proj=utm +zone=10 +datum=WGS84 +units=km") %>%
    st_transform(crs = st_crs("+proj=longlat + datum=WGS84 + no_dfs")) %>% 
    st_coordinates() %>% as_tibble() %>% rename(Lon_dd = X, Lat_dd = Y) %>%
    cbind(prediction_data%>% filter(depth_m>min(model_data$depth_m))) %>%
    filter(Lat_dd <40.16667) %>% #40.10 management line for the North South Division 
    mutate(group = "South")
  
  ## Consider adding in an even smaller spatial scale (like state management)
  
  #### Step 3 - fit the model to ALL the data #####
  ### Step 3a - create the mesh for analysis ####
  print(paste0("Starting prediction for all years and all data for ", model_data$common_name[1]))
  OG_mesh1 = fmesher::fm_mesh_2d_inla(loc = cbind(model_data$lon, model_data$lat),
                                      max.edge = c(S.edge,M.edge),offset = c(S.off, M.off),
                                      cutoff = c.off)
  OG_mesh = make_mesh(model_data, xy_cols = c("lon", "lat"), mesh = OG_mesh1)
  plot(OG_mesh)
  OG_mesh$mesh$n
  
  ### Step 3b - fitting the initial model based on https://github.com/pfmc-assessments/indexwc/blob/main/data-raw/configuration.csv ####
  #For Rex that means ~250 knots in the mesh and only year (as a factor) as a covariate, a delta-gamma distribution, 
  # and spatiotemporal effects for both binomial and gamma, and anisotropy true - Through multiple testing the model would not converge
  # when anisotrophy was true, but would run when False continuing with false as the option 
  print("Model WITHOUT depth")
  start = Sys.time()
  Mod1 = tryCatch({ 
    sdmTMB(data = model_data,
           formula = cpue_kg_km2 ~ 0 + as.factor(year),#Currently including year and depth as variables to consider
           mesh = OG_mesh,
           time = "year",
           family = delta_gamma(),
           spatial = list("on","on"),
           spatiotemporal = list("iid","iid"),
           anisotropy = T,
           share_range = F
    )}, error = function(e) NA)
  print(Sys.time()-start)
  
  ### Step 3c - save the fit model for residual analysis later ####
  saveRDS(Mod1, paste0(WD.1, "/sdmTMB_func/Model_",model_data$common_name[1],"_",OSW_shp$Scenario[1],Sys.Date(),"_All_Data_NEW.rds"))
  ### Step 3d - quick model check  ####
  sanity(Mod1)
  AIC(Mod1)
  
  ### Step 3e - fit the same model BUT include a smoothed term for Depth 
  print("Model WITH depth")
  start = Sys.time()
  Mod1_d = tryCatch({ 
    sdmTMB(data = model_data,
           formula = cpue_kg_km2 ~ 0 + as.factor(year) + s(depth_m, k = 3),#Currently including year and depth as variables to consider
           mesh = OG_mesh,
           time = "year",
           family = delta_gamma(),
           spatial = list("on","on"),
           spatiotemporal = list("iid","iid"),
           anisotropy = T,
           share_range = F
    )}, error = function(e) NA)
  print(Sys.time()-start)
  saveRDS(Mod1_d, paste0(WD.1, "/sdmTMB_func/Model_",model_data$common_name[1],"_",OSW_shp$Scenario[1],Sys.Date(),"_All_Data_NEW_Depth.rds"))
  
  sanity(Mod1_d)
  AIC(Mod1, Mod1_d)
  
  #### Step 4 - model predictions for biomass estimates ####
  ### Step 4b - predictions across the entire region ####
  Entire_region_all_y = Pred_sdm_function(Pred_data = prediction_data %>% filter(depth_m>min(model_data$depth_m)),
                                          model = Mod1,
                                          Mod_data = model_data)
  
  Entire_region_all_y_depth = Pred_sdm_function(Pred_data = prediction_data %>% filter(depth_m>min(model_data$depth_m)),
                                                model = Mod1_d,
                                                Mod_data = model_data)
  
  ### Step 4c - predictions across each spatial closure separately area ####
  OSW_all_y=list(NA)
  OSW_all_y_depth=list(NA)
  
  Proj_df_OSW2 = bind_rows(Proj_df_OSW) %>% filter(group %in% c("North", "South"))
  for(j in 1:length(unique(Proj_df_OSW2$group))){
    Pred_data1 = Proj_df_OSW2 %>% filter(group %in% unique(Proj_df_OSW2$group)[[j]],
                                         depth_m>min(model_data$depth_m))
    #Pred_data1$group == unique(OSW_shp$group)[j]
    OSW_all_y[[j]] = Pred_sdm_function(Pred_data = Pred_data1,
                                       model = Mod1,
                                       Mod_data = model_data)
    OSW_all_y_depth[[j]] = Pred_sdm_function(Pred_data = Pred_data1,
                                             model = Mod1_d,
                                             Mod_data = model_data)
  }
  # ### Step 4d - predictions across all spatial closures together (this does not include north and south) ####
  # Proj_df_OSW_all = bind_rows(Proj_df_OSW[unique(OSW_shp$group)])
  # Proj_df_OSW_all$group = "OSW"
  # OSW_region_all_y = Pred_sdm_function(Pred_data = Proj_df_OSW_all,
  #                                      model = Mod1,
  #                                      Mod_data = model_data)
  
  ### Step 4e -  combine the data together for future index and density analysis and plots ####
  
  Index_all = rbind(Entire_region_all_y$Index, #Get all the index values together across the different regions 
                    bind_rows(lapply(OSW_all_y, '[[',1)))
  Index_all$scen = "All data"
  Density_all = rbind(Entire_region_all_y$Pred, #Get all the density estimates across the different regions 
                      bind_rows(lapply(OSW_all_y, '[[',2)))
  Density_all$scen = "All data"
  
  ##Depth models ###
  Index_all_depth = rbind(Entire_region_all_y_depth$Index, #Get all the index values together across the different regions 
                          bind_rows(lapply(OSW_all_y_depth, '[[',1)))
  Index_all_depth$scen = "All data"
  Density_all_depth = rbind(Entire_region_all_y_depth$Pred, #Get all the density estimates across the different regions 
                            bind_rows(lapply(OSW_all_y_depth, '[[',2)))
  Density_all_depth$scen = "All data"
  
  ###################### NOW STARTING THE TEMPORAL SCENARIOS #########################
  
  #### Step 5 - remove all the data within the spatial closures at different time steps ####
  
  
  Index_OSW = list(NA)
  Density_OSW = list(NA)
  Index_OSW_depth = list(NA)
  Density_OSW_depth = list(NA)
  Scen_mod = list(NA)
  
  for(i in 1:nrow(scen)){
    print(paste0("Starting model and prediction for scenario this is missing ", scen[i,]$Scenario," of data"))
    
    # Start by removing data inside the areas of interest for the different scenario years 
    if(scen[i,]$Year %in% Start.year){
      pt2a = data.frame(st_intersection(st_as_sf(data.frame(model_data), coords = c("lon", "lat"),
                                                 crs = "+proj=utm +zone=10 +datum=WGS84 +units=km"), 
                                        OSW_shp))
      pt.mod = model_data[!model_data$trawl_id %in% pt2a$trawl_id,]
    }else{#If you are only removing the data for a certain number of years 
      pt1 = model_data[model_data$year %in%  c(Start.year:scen[i,]$Year),]#Years that still sample the areas of interest 
      pt2 = model_data[!model_data$year %in% c(Start.year:scen[i,]$Year),]#Years to remove samples inside the OSW 
      pt2a = data.frame(st_intersection(st_as_sf(data.frame(pt2), coords = c("lon", "lat"),
                                                 crs = "+proj=utm +zone=10 +datum=WGS84 +units=km"), 
                                        OSW_shp))
      
      pt.mod = rbind(pt1, 
                     pt2[!pt2$trawl_id %in% pt2a$trawl_id,]) #Create a data frame that has removed the OSW sample areas for 
    }
    
    #### Step 6 - refit the model with the new scenario based data set ####
    print("Model WITHOUT depth")
    start = Sys.time()
    Mod2 = tryCatch({ 
      sdmTMB(data = pt.mod,
             formula = cpue_kg_km2 ~ 0 + as.factor(year),#Currently including year and depth as variables to consider
             mesh = make_mesh(pt.mod, xy_cols = c("lon", "lat"), mesh = OG_mesh$mesh),
             time = "year",
             family = delta_gamma(),
             spatial = list("on","on"),
             spatiotemporal = list("iid","iid"),
             anisotropy = T,
             share_range = F
      )}, error = function(e) NA)
    print(Sys.time()-start)
    
    ### Step 6b - save the model ####
    saveRDS(Mod2, paste0(WD.1, "/sdmTMB_func/Model_",pt.mod$common_name[1],"_",OSW_shp$Scenario[1],"_",scen$Scenario[i],Sys.Date(),"_NEW.rds"))
    ### Step 6c - Model check ####
    sanity(Mod2)
    
    print("Model WITH depth")
    
    start = Sys.time()
    Mod2_d = tryCatch({ 
      sdmTMB(data = pt.mod,
             formula = cpue_kg_km2 ~ 0 + as.factor(year) + s(depth_m, k = 3),#Currently including year and depth as variables to consider
             mesh = make_mesh(pt.mod, xy_cols = c("lon", "lat"), mesh = OG_mesh$mesh),
             time = "year",
             family = delta_gamma(),
             spatial = list("on","on"),
             spatiotemporal = list("iid","iid"),
             anisotropy = T,
             share_range = F
      )}, error = function(e) NA)
    print(Sys.time()-start)
    
    ### Step 6b - save the model ####
    saveRDS(Mod2_d, paste0(WD.1, "/sdmTMB_func/Model_",pt.mod$common_name[1],"_",OSW_shp$Scenario[1],"_",scen$Scenario[i],Sys.Date(),"_NEW_Depth.rds"))
    ### Step 6c - Model check ####
    sanity(Mod2_d)
    
    #### Step 7 - model predictions ####
    ### Step 7b - entire region predictions ####
    Entire_region_OSW_y = Pred_sdm_function(Pred_data = prediction_data %>% filter(depth_m>min(model_data$depth_m)),
                                            model = Mod2,
                                            Mod_data = pt.mod)
    
    Entire_region_OSW_y_depth = Pred_sdm_function(Pred_data = prediction_data %>% filter(depth_m>min(model_data$depth_m)),
                                                  model = Mod2_d,
                                                  Mod_data = pt.mod)
    
    ### Step 7c - each spatial closure separately ####
    OSW_missing_y=list(NA)
    OSW_missing_y_depth=list(NA)
    
    Proj_df_OSW2 = bind_rows(Proj_df_OSW) %>% filter(group %in% c("North", "South"))
    for(j in 1:length(unique(Proj_df_OSW2$group))){
      Pred_data1 = Proj_df_OSW2 %>% filter(group %in% unique(Proj_df_OSW2$group)[[j]],
                                           depth_m>min(model_data$depth_m))
      #Pred_data1$group == unique(OSW_shp$group)[j]
      OSW_missing_y[[j]] = Pred_sdm_function(Pred_data = Pred_data1,
                                             model = Mod2,
                                             Mod_data = pt.mod)
      OSW_missing_y_depth[[j]] = Pred_sdm_function(Pred_data = Pred_data1,
                                                   model = Mod2_d,
                                                   Mod_data = pt.mod)
    }
    
    # ### Step 7d - spatial closures combined (not including north and south) ####
    # OSW_region_missing_y = Pred_sdm_function(Pred_data = Proj_df_OSW_all,
    #                                          model = Mod2,
    #                                          Mod_data = pt.mod)
    
    ### Step 7e -  combine the data together for future index and density analysis and plots ####
    Index_OSW[[i]] = rbind(Entire_region_OSW_y$Index, #Get all the index values together across the different regions 
                           bind_rows(lapply(OSW_missing_y, '[[',1)))
    Index_OSW[[i]]$scen = scen[i,]$Scenario
    Density_OSW_df = rbind(Entire_region_OSW_y$Pred, #Get all the density estimates across the different regions 
                           bind_rows(lapply(OSW_missing_y, '[[',2)))
    Density_OSW[[i]] = Density_OSW_df#[Density_OSW_df$year == Term.year,]
    Density_OSW[[i]]$scen = scen[i,]$Scenario
    
    Index_OSW_depth[[i]] = rbind(Entire_region_OSW_y_depth$Index, #Get all the index values together across the different regions 
                                 bind_rows(lapply(OSW_missing_y_depth, '[[',1)))
    Index_OSW_depth[[i]]$scen = scen[i,]$Scenario
    Density_OSW_depth_df = rbind(Entire_region_OSW_y_depth$Pred, #Get all the density estimates across the different regions 
                                 bind_rows(lapply(OSW_missing_y_depth, '[[',2)))
    Density_OSW_depth[[i]] = Density_OSW_depth_df#[Density_OSW_depth_df$year == Term.year,]
    Density_OSW_depth[[i]]$scen = scen[i,]$Scenario
    
  }
  ##############################
  ## Performance metrics for index values 
  ##############################
  print("Starting performance metrics for the index values")
  SP_Index = rbind(Index_all,bind_rows(Index_OSW[1:length(Index_OSW)]))#Combine all the regions together
  SP_Index = SP_Index %>%
    mutate(CI = upr-lwr) #Calculate the CI for each year
  
  print("Starting performance metrics for the index values")
  SP_Index_depth = rbind(Index_all_depth,bind_rows(Index_OSW_depth[1:length(Index_OSW_depth)]))#Combine all the regions together
  SP_Index_depth = SP_Index_depth %>%
    mutate(CI = upr-lwr) #Calculate the CI for each year
  
  # SP_term_PM = bind_rows(lapply(split(SP_Index, SP_Index$group), function(x){ #grab the performance metrics 
  #   x %>%
  #     group_by(group, scen) %>%
  #     dplyr::filter(year %in% Term.year) %>%
  #     mutate(CI_perc = ((CI - SP_Index[SP_Index$year ==Term.year &
  #                                        SP_Index$scen == "All data" &
  #                                        SP_Index$group %in% x$group,]$CI)/
  #                         ((CI + SP_Index[SP_Index$year ==Term.year &
  #                                           SP_Index$scen == "All data" &
  #                                           SP_Index$group %in% x$group,]$CI)/2))*100,
  #            est_RMSE = sqrt(((est - SP_Index[SP_Index$year ==Term.year &
  #                                               SP_Index$scen == "All data" &
  #                                               SP_Index$group %in% x$group,]$est))^2)#,
  #     )
  # })) %>%
  #   dplyr::select(group, scen, CI_perc, est_RMSE)
  
  #########################################
  ## Performance metrics for density values 
  #########################################
  #Create lists/vectors to be filled in later 
  print("Starting performance metrics for the density estimates")
  print("Without depth")
  Dens_func_values = Dens_PM_func_DG(Dens_data = rbind(Density_all,bind_rows(Density_OSW[1:length(Density_OSW)])),
                                  Proj_extent = Proj_extent %>% filter(Avg_Dep_m >min(model_data$depth_m)),
                                  scen = scen,
                                  OSW_shp = OSW_shp)
  
  print("With depth")
  Dens_func_values_depth = Dens_PM_func_DG(Dens_data = rbind(Density_all_depth,bind_rows(Density_OSW_depth[1:length(Density_OSW_depth)])),
                                        Proj_extent = Proj_extent %>% filter(Avg_Dep_m >min(model_data$depth_m)),
                                        scen = scen,
                                        OSW_shp = OSW_shp)
  
  #######################################################################################
  return(list(Index = SP_Index,
              Index_d = SP_Index_depth,
              #Ind_pm = SP_term_PM,
              Density = rbind(Density_all,bind_rows(Density_OSW[1:length(Density_OSW)])),
              Dens_PM_ER = Dens_func_values$Density_PM_ER,
              Density_PM_OSW = Dens_func_values$Density_PM_OSW,
              Density_PM_all = Dens_func_values$Density_PM_all,
              Density_Dif = Dens_func_values$Density_Dif,
              Density_d = rbind(Density_all_depth,bind_rows(Density_OSW_depth[1:length(Density_OSW_depth)])),
              Dens_PM_ER_d = Dens_func_values_depth$Density_PM_ER,
              Density_PM_OSW_d = Dens_func_values_depth$Density_PM_OSW,
              Density_PM_all_d = Dens_func_values_depth$Density_PM_all,
              Density_Dif_d = Dens_func_values_depth$Density_Dif
  ))
}

#### Start the modeling for Rex  ####

## Step 1 - get an approximate mesh (500 knots)####

Rex_opt = readRDS(paste0(WD.1, "/Rex_sole_expanded.rds"))

Mesh_check = fmesher::fm_mesh_2d_inla(loc = cbind(Rex_opt$lon, Rex_opt$lat),
                                      max.edge = c(87,105),offset = c(50, 75),
                                      cutoff = 40.35)
Mesh_check2 = make_mesh(Rex_opt, xy_cols = c("lon", "lat"), mesh = Mesh_check)

Mesh_check2$mesh$n #500 knots - same as in current methods 
plot(Mesh_check2)

### Step 2 - set up the temporal scenarios ####
year_df = data.frame(Scenario = c("1 year", "5 years", "10 years", "15 years", "19 years","Never surveyed"
),
Year = c(2022, 2018, 2012, 2008, 2004, 2003
))

### Step 3 - create a list of spatial scenarios to run through####

OSW_list = list(OSW_Scen1, OSW_Scen2, OSW_Scen3)

### Step 4 - Run the function!! ####
start3_check = Sys.time()
for(i in 1:3){
  gc()
  start2_check = Sys.time()
  print(paste0("Starting iterations for ",OSW_list[[i]]$Scenario[i] ))
  Big_Fun_data = PROJ_Func_Rex(scen = year_df,
                                     prediction_data = Proj_df,
                                     model_data = Rex_opt ,
                                     c.off = 40.35,
                                     S.edge = 87,
                                     M.edge = 105,
                                     S.off = 50,
                                     M.off = 75,
                                     Start.year = 2003,
                                     Proj_extent = Projection_extent,
                                     OSW_shp = OSW_list[[i]],
                                     Term.year = 2023)
  saveRDS(Big_Fun_data, file = paste0(WD.1, "/sdmTMB_func/",Rex_opt$common_name[1],"_",OSW_list[[i]]$Scenario[1],"_",Sys.Date(),"_NEW2.rds"))
  print(Sys.time()-start2_check)#~5.8 hrs per spatial scenario 
}
print(Sys.time()-start3_check) #~15.4 hrs
