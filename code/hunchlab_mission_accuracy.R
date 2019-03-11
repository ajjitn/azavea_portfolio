#This script is meant to assess the accuracy of HUnchlabs' predicted missions by comparing it to actual crime data. This script is meant to be sourced from the Command Line with a few accompanying options (see full list below in code). It writes out a series of CSVs and html files that assess the accuracy of the missions by crimteype, mission type, 

# Required parameters are:
#   - inputdirectory: file path the the Hunchlab data dump 'package' folder
#   - outputdirectory: file path to save CSV and html outputs
#   - missiongenconfig: mission gen config to test accuracy for

#The script was tested using data from Hunchlabs Philly patrol missions from 2/10/18 to 2/12/18. Data dump provided by Kenny Shepard and Jeremy Heffner, pulled on Feb 25th 2018.


#########
## SETUP 
#########

options(repos=structure(c(CRAN="https://mran.microsoft.com/snapshot/2017-01-22")))
#Required packages
required_pkgs = c("tidyverse","raster","sp","leaflet","rgdal", "geojsonio",
                  "rjson","rgeos", "optparse", "stringr","sf","broom","tidyr","jsonlite",
                  "lwgeom","data.table","ggplot2", "htmlwidgets","raster","htmltools")

#Load packages
for (p in required_pkgs) {
  if (!suppressPackageStartupMessages(require(p, quietly=T, character.only=T))) {
    install.packages(p)
    require(p)
  }}

#resolve some naming quirks between packages
fromJSON = jsonlite::fromJSON
select = dplyr::select
transpose = data.table::transpose
remove(p)
remove(required_pkgs)

#Set Command Line parmas
option_list = list(
  make_option(c("--verbose"), type="logical", default=FALSE,
              help="Logical. Do you want script to output function messages? Default is FALSE. 
              [default %default]", metavar="number"),
  make_option(c("--missiongenconfig", type = "character", default = NA, 
                help = "exact wording of mission_gen_config to filter on 
                (ie name of subfolder in mission_gen_config folder")),
  make_option(c("--datestart"), type = "character", default= NA,
              help="Date for events to start from, format yyyy/mm/dd [default %default]",
              metavar="character"),
  make_option(c("--dateend"), type = "character", default= NA,
              help="Date for events to end at, format yyyy/mm/dd [default %default]",
              metavar="character"),
  make_option(c("--timestart", type = "character", default = NA, 
                help = "exact time_start to filter on, in the format HH_MM. Ex: 14_00")),
  make_option(c("--timeend", type = "character", default = NA, 
                help = "exact time_end filter on, in the format HH_MM. Ex: 14_00")),
  make_option(c("--plots", action  = "logical", default = FALSE, 
                help = "Logical. Should static maps of missons and event be generated? 
                Defualts to False")),
  make_option(c("--map", type = "logical", default = FALSE, 
                help = "Logical. Do you want leaflet map of missions and events to be 
                produced?. Defaults to False ")),
  make_option(c("--attach_boundaries", type = "logical", default = FALSE, 
                help = "Logical. Do you want to attach all boundary info(beats/districts/
                divisions etc) to output csv? Defaults to False "))
  )

# parse the options from CL
parser = OptionParser(usage = "%prog [options] inputdirectory outputdirectory", option_list=option_list)
arguments = parse_args(parser, positional_arguments=TRUE)


# check if input and output directories were provided. If yes, save the directories, if not stop & print a warning
if(length(arguments$args) == 2) {
  kinput_dir = arguments$args[1]
  koutput_dir = arguments$args[2]
} else {
  stop("One or more of input directory/output directory is not provided. Run with --help for assistance.  
       Input and output directories should be full path")
}

# extract the remaining options and perform checks/give warnings as needed
opt = arguments$options

kVerbose = opt$verbose
kmissiontype = opt$missiongenconfig
kdatestart = as.Date(opt$datestart)
kdateend   = as.Date(opt$dateend)
ktimestart = opt$timestart
ktimeend = opt$timeend
kplot = opt$plots
kmap = opt$map
kattach_boundaries = opt$attach_boundaries


kplot = ifelse(is.null(kplot), FALSE, kplot)
kmap = ifelse(is.null(kmap), FALSE, kmap)
kattach_boundaries = ifelse(is.null(kattach_boundaries), FALSE, kattach_boundaries)
ktimestart = ifelse(is.null(ktimestart), NA, ktimestart)
ktimeend = ifelse(is.null(ktimeend), NA, ktimeend)
kcity = str_split(kinput_dir, "/")[[1]] %>% .[length(.) - 1]      


if(is.null(kmissiontype)){
  print("mission_gen_config type required. Please provide one.")
  stop()
  kcrimemodel = NA}


#helper function to create dataframe of files from a directory, and perform  renaming
#dir=directory path, pattern1 = file pattern to search for (regex), colnames=vector of column names
dir_to_df = function(dir, pattern1 = "", colnames = c()){
  file_vec =  list.files(dir, pattern= pattern1, recursive = T, full.names = T)
  df = data.frame(do.call(rbind, strsplit(file_vec, "/", fixed = T)))
  df = cbind(df, filenames = file_vec)
  
  if(length(colnames)!= 0){
    for (i in 1:length(colnames)){
      df = df%>% rename_at(ncol(df)-i,funs(paste0(as.character(colnames[i]))))
    }}
  return(df)
}


#######################
### READING IN MISSIONS
#######################

#Create mission files dataframe
all_missions = dir_to_df(paste0(kinput_dir,"/missions"), ".geojson", c("mission_name","day","month","year","mission_gen_config"  ))


#Append date column 
all_missions = all_missions %>%
  mutate(date = as.Date(paste(year, month, day, sep ="-")))

#### Filtering mission files by inputted dates and missiontype
#If optional params (datestart and dateend) are provided in command line call, then keep TIFS betweeen those dates
if(!is.na(kdatestart) & !is.na(kdateend)){
  all_missions = all_missions %>%
    filter(date>= kdatestart & date <= kdateend)
}

#If only optional param (datestart) is provided, then filter to just that date
if(!is.na(kdatestart) & is.na(kdateend)){
  all_missions = all_missions %>%
    filter(date>= kdatestart & date <= kdatestart)
}

## Filtering by mission_type
#If mimssiontype is provided, then filter on that mission
if (!is.na(kmissiontype)){
  all_missions = all_missions  %>%
    mutate(filenames = as.character(filenames)) %>%
    filter(mission_gen_config == kmissiontype)}
#If no optional params are provided, we keep all tifs in the directory. No filtering needed



#Adding datetime columns (using annoying regex on mission_name column)
all_missions = all_missions %>% mutate(datetime_start = gsub("T"," ", mission_name),
                                       datetime_start = gsub("Z.geojson","", datetime_start),
                                       datetime_start = gsub("Z-.*","", datetime_start),
                                       datetime_start = sub("_","-", datetime_start),
                                       datetime_start = sub("_","-", datetime_start),
                                       datetime_start = sub("_",":", datetime_start),
                                       datetime_start = sub("_",":", datetime_start),
                                       datetime_start = as.POSIXct(datetime_start, tz="UTC"),
                                       datetime_end = gsub("T"," ", mission_name),
                                       datetime_end = gsub("Z.geojson","", datetime_end),
                                       datetime_end = gsub(".*Z-","", datetime_end),
                                       datetime_end = sub("_","-", datetime_end),
                                       datetime_end = sub("_","-", datetime_end),
                                       datetime_end = sub("_",":", datetime_end),
                                       datetime_end = sub("_",":", datetime_end),
                                       datetime_end = as.POSIXct(datetime_end, tz="UTC"))


## Reading in missions as list
missions = list()
w=1

for (i in all_missions$filenames){
  temp = st_read(dsn = i, stringsAsFactors = F, quiet=ifelse(kverbose,T,F))
  missions[[w]] = temp
  missions[[w]]$mission_area = st_area(missions[[w]])
  names(missions)[w] = paste((all_missions$datetime_start)[w], (all_missions$datetime_end)[w], sep = " to ")
  names(missions[[w]]$geometry) = NULL
  w=w+1
}




#########################
###  gen_config -> event models transalation list
########################
all_gen = dir_to_df(paste0(kinput_dir,"/mission-gen-configs"), pattern= ".json", c("mission_gen_config_name","mission_gen_config"))


## Filtering by mission_type
if (!is.na(kmissiontype)){
  all_gen = all_gen  %>%
    mutate(filenames = as.character(filenames)) %>%
    filter(mission_gen_config == kmissiontype)}

## Reading in gen_config ---> event_models as list
gen_config_crimes = list()
w=1
for (i in all_gen$filenames){
  temp = fromJSON(i)
  gen_config_crimes[[w]] = temp
  w=w+1
}

#creating list
gen_config_to_event_models= lapply(gen_config_crimes, function(x) x$event_models$slug_label)
#creating unique event models vector
unique_event_models = unique(unlist(gen_config_to_event_models))
#getting the boundary used for generating missions from gen_config
gen_config_boundary = gen_config_crimes[[1]]$boundaries$name


#########################
### getting event_models -> crime class transalation list
########################

all_crime = dir_to_df(paste0(kinput_dir,"/event-models"), pattern= ".json", c("event_model_name","event_model"))

## Filtering by mission_type
if (!is.na(kmissiontype)){
  all_crime = all_crime  %>%
    mutate(filenames = as.character(filenames)) %>%
    filter(event_model %in% unique_event_models)}

## Reading in event_models -> crime class lists
event_model_crimes = list()
w=1
for (i in all_crime$filenames){
  temp = fromJSON(i)
  event_model_crimes[[w]] = temp
  w=w+1
}

#getting necesary transalation lists for crimes -> event models
event_models_to_crime_classes= lapply(event_model_crimes, function(x)x$event_model_class_config$event_classes)
names(event_models_to_crime_classes)=  all_crime$event_model
unique_crime_classes = unique(unlist(event_models_to_crime_classes))


#test case-overlapping crime classes
event_models_to_crime_classes$`philadelphia_open_data-robbery` = append(
  event_models_to_crime_classes$`philadelphia_open_data-robbery`,
  "philadelphia_open_data-aggravated_assault_no_firearm-public_csv")

#Printing crimeclasses that fall into multiple event_models
dup_crime_class_in_events = unlist(event_models_to_crime_classes)[duplicated(unlist(event_models_to_crime_classes)) |
                                                                    duplicated(unlist(event_models_to_crime_classes), fromLast = T)]

if(length(dup_crime_class_in_events>0)){
  print(paste0("The following crime classes appear in multiple event_models: ", unique(dup_crime_class_in_events), 
               "These are the offending event models: ", names(dup_crime_class_in_events)))
}

#creating event_model_weights df from event_models list
event_model_weights = t(sapply(event_model_crimes, function(x){
  sev = x$event_model_mission_config$severity_weight
  eff = x$event_model_mission_config$effectiveness
  temp= c(sev, eff)
  return(temp)})) %>% 
  as.data.frame %>%
  rename(event_model_severity_weight = V1,
         event_model_effectiveness_weight= V2) %>%
  mutate_all(as.character) %>%
  mutate_all(as.numeric) %>%
  mutate(event_model = all_crime$event_model)




##########################
### GETTING CITY POLYGONS
##########################

all_poly = dir_to_df(paste0(kinput_dir,"/boundaries"),pattern= ".geojson", c("boundaries"))
all_poly = all_poly %>% mutate(boundaries = gsub(".geojson","",boundaries))                

# reading in polygon files as list of geojsons
boundaries = list()
w=1
for (i in all_poly$filenames){
  temp = st_read(i)
  boundaries[[w]] = temp
  w=w+1
}

names(boundaries) = all_poly$boundaries

#extracting city wide boundaries, which should have least # (usually 1) polygons
city_bounds = boundaries[[which.min(lapply(boundaries, nrow))]]

gen_config_bound =boundaries[[gen_config_boundary]] %>% 
  mutate(area= st_area(boundaries[[gen_config_boundary]]))


# Right now, script will only append beat/district from gen_config json($boundaries$name)
# If you want, you can manually provide other boundaries (names of subfolders in 'boundaries')

#########################
### Reading in Crime Data
#########################

#assuming only one csv in events folder
all_events = list.files(paste0(kinput_dir,"/events"), pattern = ".csv", recursive = T, full.names = T)
all_events = (read.csv(all_events, stringsAsFactors = F))

#data cleaning and converting to sf 
all_events = all_events %>%
  mutate(lat = pointy,
         lon = pointx,
         report_time = gsub("T", " ", report_time),
         report_time = gsub("Z", " ", report_time),
         report_time = as.POSIXct(report_time, tz = "UTC"),
         class = as.factor(class)) %>%
  select(id, class, report_time, lon, lat) %>%
  #tiny buffer on dates to make sure edge cases are pulled in
  filter(between(report_time, kdatestart-1,kdateend+2)) %>%
  st_as_sf(coords =c("lon", "lat"), crs=4326)


# spread data (if crime falls under multiple crime classes, it is now in multiple rows) and 
# filter on crimes within mission parameters
all_events = all_events %>% separate(class, c("1", "2", "3"), sep="\\|") %>%
  gather("class_dupl_num", "class", `1`:`3`) %>%
  filter(class != 0)  %>%
  filter(class %in% unique_crime_classes)%>%
  mutate(class = as.factor(class),
         class_dupl_num = as.numeric(class_dupl_num)) %>%
  arrange(report_time)




#classdupl_num is the a number for whether the row is a duplicate. If value =2, then its the second duplicate, etc, etc


####################
# Appending rasterized areas of PSAs for accurate PAI 
####################

#reading in extent raster
city_extent= dir_to_df(paste0(kinput_dir,"/extents"),pattern= ".tif", c("extent"))
city_extent=raster(as.character(city_extent$filenames))

#reprojecting extent to same crs as gen_config_bound (usually WGS84 lon/lat)
city_extent <- projectRaster(city_extent, crs = crs(as(gen_config_bound, "Spatial")))

#rasterizing gen_config_bound
gen_config_bound_raster = fasterize(gen_config_bound, city_extent, field ="id")

#zonal sums for each PSA (or whatever unit of gen config bound)
zonal_area_sum = as.data.frame(zonal(area(gen_config_bound_raster), gen_config_bound_raster,
                    fun = "sum")) %>%
  rename(id=zone,
         rasterized_area=sum) %>%
  #transforming km^2 -> m^2 
  mutate(rasterized_area=1000000*rasterized_area)

#appending zonal area sum to gen_config_bound sf object
gen_config_bound = left_join(gen_config_bound, zonal_area_sum, by ="id")

#setting correct units on rasterized area
units(gen_config_bound$rasterized_area) = units(gen_config_bound$area)


#cleaning up
remove(city_extent)
remove(zonal_area_sum)
remove(all_poly)
remove(all_crime)


## Slippy map to compare gen_config_bound raster with original gen_config_bound_raster
#   pal1 = colorNumeric(palette = "YlOrRd", domain = range(values(gen_config_bound_raster), na.rm = T),
#                       na.color = "transparent")
#   
#   leaflet_map = leaflet() %>% 
#     addProviderTiles(providers$CartoDB.Positron) %>%
#     addRasterImage(gen_config_bound_raster, opacity=0.6) %>%
#     addPolygons(data=gen_config_bound, opacity = 0.2, fillOpacity = 0.03,
#                 color = "grey")%>%
# x= st_as_sf(rasterToPolygons(gen_config_bound_raster)) %>% rename(label=layer)


######################
### Creating all_events_appended csv
######################

#generating all_events_appended list, plots list, and other setup for loop
all_events_appended_list = list()

if(kplot){
  plots=list()}

#names of columns to be appended in loop
crime_bound   =paste0("crime_event_",gen_config_boundary, "_label")
crime_area   =paste0("crime_event_",gen_config_boundary, "_area")
crime_r_area =paste0("crime_event", gen_config_boundary,"_rasterized_area")
mission_bound_label =paste0("mission_",gen_config_boundary, "_label")
mission_bound_area =paste0("mission_",gen_config_boundary, "_area")
mission_bound_r_area=paste0("mission_", gen_config_boundary,"_rasterized_area")
mission_bound_c_area=paste0(mission_bound_area,"_const")


#Main loop
for(j in 1:nrow(all_missions)){
  #get datetime bounds for mission
  dt_start = all_missions$datetime_start[j]
  dt_end   = all_missions$datetime_end[j]
  
  #filter all_events df to events within mission timeframe
  temp_events = all_events %>%
    filter(between(report_time, dt_start,dt_end))
  
  #if no filtered, events, we go on to the next one
  if(nrow(temp_events)==0){
    all_events_appended_list[[j]] = temp_events 
    names(all_events_appended_list)[j] = paste((all_missions$datetime_start)[j], 
                                               (all_missions$datetime_end)[j], 
                                               sep = " to ")
    next()}
  
  
  #Appending which PSA each mission centroid is located in to missions_temp df
  mission_temp = left_join(missions[[j]], st_intersection(st_centroid(missions[[j]]), 
                                                          gen_config_bound) %>% 
                             select(id, label,area, rasterized_area) %>%
                             rename(!!mission_bound_label := label,
                                    !!mission_bound_area  := area,
                                    !!mission_bound_r_area:= rasterized_area) %>%
                             st_set_geometry(NULL))
  
  
  if (kattach_boundaries){
    for (k in names(boundaries)[!names(boundaries) %in% gen_config_boundary]){
      
      mission_temp = left_join(mission_temp, st_intersection(st_centroid(missions[[j]]), 
                                                             boundaries[[k]]) %>% 
                                 select(id, label) %>%
                                 rename(!!paste0("mission_",k,
                                                 "_label") := label) %>%
                                 st_set_geometry(NULL))
    }}
  
  
  
  
  #appending constrained mission areas (ie mission area within the PSA )
  ar = c()
  for (k in 1:nrow(mission_temp)){
    mis= mission_temp[k,] %>% select(!!mission_bound_label)
    bound=gen_config_bound %>% filter(label == 
                                       (mis %>% st_set_geometry(NULL))[,1])
    int = st_intersection(mis, bound)
    ar = append(ar,as.numeric(st_area(int)))
  }
  
  #right units
  units(ar) = units(mission_temp$mission_area[1])
  mission_temp=mission_temp %>% mutate(!!mission_bound_c_area := ar)
  

  #Now for each event in temp_event:
  ## append a 0/1 caught column and select mission vars: id.mission, dt_start and dt_end, 
  temp_events = st_join(temp_events, mission_temp) %>%
    mutate(id = id.x,
           id.mission = id.y,
           caught = as.factor(ifelse(is.na(id.mission),0,1)),
           dt_start = dt_start,
           dt_end = dt_end) %>%
    select(starts_with("mission"), dt_start, dt_end, id, id.mission, caught,class, report_time)%>%
    select(-mission_set, -mission_buffer) %>%
    
    #getting 'beat' that crime event is located in
    st_join(( gen_config_bound %>% 
                select(label, area, rasterized_area ))) %>% 
    #reanming columns to be crime_event_beat_label and crime_event_beat_area
    rename(!!crime_bound := label,
           !!crime_area := area,
           !!crime_r_area:= rasterized_area)  %>%
    #replacing NAs in mission_PSA_label (ie uncaptured crimes) with thier crime_PSA_label
    #So for uncaptured crimes, we use their location to fill in thier mission_PSA_label
    #NOTE: This behavior may not be optimal for edge cases, we want to in future use 
      # gen_Config_bound_raster to determine what PSA each crime falls into
    mutate(!!crime_bound := as.character(UQ(as.name(crime_bound))),
           #!!crime_area := as.character(UQp),
           !!mission_bound_label := as.character(UQ(as.name(mission_bound_label))),
           #if mission_beat_label is NA (ie uncaptured crime), then fill in with crime_event_beat_label
           !!mission_bound_label := ifelse(is.na(UQ(as.name(mission_bound_label))), 
                                           UQ(as.name(crime_bound)),
                                           UQ(as.name(mission_bound_label))),
           !!mission_bound_area := ifelse(is.na(UQ(as.name(mission_bound_area))), 
                                          UQ(as.name(crime_area)),
                                          UQ(as.name(mission_bound_area))),

           !!mission_bound_r_area := ifelse(is.na(UQ(as.name(mission_bound_r_area))), 
                                          UQ(as.name(crime_r_area)),
                                          UQ(as.name(mission_bound_r_area))))
  
  
  #calculating dist to closest mission (in meters) and appending
  dist_closest_mission = st_distance(temp_events, mission_temp) %>%as.tibble %>%
    transpose %>% summarize_all(funs(min))%>% transpose
  temp_events = temp_events %>% mutate(dist_m_closest_mission = dist_closest_mission$V1)
  
  
  #standardizing names
  name = paste((all_missions$datetime_start)[j], (all_missions$datetime_end)[j], 
               sep = " to ")
  
  #saving plots
  if (kplot){
    plot = ggplot() +
      geom_sf(data=city_bounds, fill="white")+
      geom_sf(data=mission_temp, fill ="white")+
      geom_sf(data=temp_events,size=1, aes(color =caught)) +
      theme(text=element_text(size=12), 
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.title.x= element_blank(),
            axis.title.y= element_blank(),
            axis.ticks = element_blank(),
            title = element_text(size = 12),
            #rect = element_blank(),
            panel.grid.major.x = element_line(color = "white", linetype = 1)) +
      ggtitle(name)
    #saving output
    plots[[j]] = plot
  }
  
  #saving outputs and naming them as date_time tag
  all_events_appended_list[[j]] = temp_events 
  names(all_events_appended_list)[j] = name
  
  
}

#collate list of dfs into one dataframe
all_events_appended =  do.call("rbind", all_events_appended_list) %>% arrange(report_time)

#need start and end index for appending event_model column
start=ncol(all_events_appended)

#append event_model columns as dummy variables
for (i in 1:length(unique_event_models)){
  all_events_appended=all_events_appended %>% 
    mutate(!!unique_event_models[i]:= 
             ifelse(class %in% 
                      event_models_to_crime_classes[[i]],1,0))}
end =ncol(all_events_appended)-1

#turn dummy variable columns into one event_model column. 
#Note this ensures that crime_classes that fall under multiple event_models have dup. rows
all_events_appended=all_events_appended%>% 
  gather(event_model, value, start:end) %>% 
  filter (value!=0) %>% 
  select (-value) %>%
  mutate(event_model = as.factor(event_model)) %>%
  arrange(dt_start,class) %>%
  left_join(event_model_weights)



#######################################
### Generating aggregate summary tables
#######################################

# global crime stats broken down by unique crime classes/event_models
crime_stats_by_crime_class = left_join(all_events_appended %>% 
                                         group_by(class, event_model,
                                                  event_model_severity_weight, 
                                                  event_model_effectiveness_weight) %>% 
                                         summarise(actual= n()) %>%
                                         as.data.frame() %>% 
                                         select(-geometry),
                                       all_events_appended %>% 
                                         filter(caught ==1) %>% 
                                         group_by(class) %>% 
                                         summarise(caught= n())%>%
                                         as.data.frame() %>% 
                                         select(-geometry)) %>%
  mutate(caught = ifelse(is.na(caught),0,caught), pct_caught = caught/actual)


crime_stats_by_shift_starts =  left_join(all_events_appended %>% 
                                           group_by(hour(dt_start), hour(dt_end)) %>%
                                           summarise(actual= n()) %>%
                                           as.data.frame() %>% 
                                           select(-geometry),
                                         all_events_appended %>% 
                                           filter(caught ==1) %>% 
                                           group_by(hour(dt_start), hour(dt_end)) %>%
                                           summarise(caught= n())%>%
                                           as.data.frame() %>% 
                                           select(-geometry))%>%
  mutate(caught = ifelse(is.na(caught),0,caught), pct_caught = caught/actual)



crime_stats_by_event_model = left_join(all_events_appended %>% 
                                         group_by(event_model,
                                                  event_model_severity_weight, 
                                                  event_model_effectiveness_weight) %>% 
                                         summarise(actual= n()) %>%
                                         as.data.frame() %>% 
                                         select(-geometry),
                                       all_events_appended %>% 
                                         filter(caught ==1) %>% 
                                         group_by(event_model) %>% 
                                         summarise(caught= n())%>%
                                         as.data.frame() %>% 
                                         select(-geometry)) %>%
  mutate(caught = ifelse(is.na(caught),0,caught), pct_caught = caught/actual)

crime_stats_by_district = left_join(all_events_appended %>% 
                                      group_by(district_label_mission) %>% 
                                      summarise(actual= n()) %>%
                                      as.data.frame() %>% 
                                      select(-geometry),
                                    all_events_appended %>% 
                                      filter(caught ==1) %>% 
                                      group_by(district_label_mission) %>% 
                                      summarise(caught= n())%>%
                                      as.data.frame() %>% 
                                      select(-geometry)) %>%
  mutate(caught = ifelse(is.na(caught),0,caught), pct_caught = caught/actual)

non_matching_events= all_events_appended %>% filter(district_label_crime_event != district_label_mission)
non_matching_missions = all_missions_df %>% filter(id %in% non_matching_missions$id.mission)

plot(boundaries$Districts$geometry)
plot(non_matching_missions$geometry, col = "steelblue", add = T, alpha=0.1)
plot(non_matching_events$geometry, col="red", add =T, pch =19, cex=0.5)


#getting % of total crime,  harm, and preventable harm caught
agg_crime_percent_captured=crime_stats_by_event_model %>% summarise(percent_crimes_captured = sum(caught) / 
                                                                      sum(actual),
                                                                    percent_prev_harm_captured = sum(caught*event_model_severity_weight) / 
                                                                      sum(actual*event_model_severity_weight),
                                                                    percent_pred_prev_harm_captured = sum(caught*event_model_effectiveness_weight*
                                                                                                            event_model_severity_weight) / 
                                                                      sum(actual*event_model_effectiveness_weight*
                                                                            event_model_severity_weight))




####################################
##### Writing outputs
###################################


#Create output dir (even if its nested), if output_dir exists then supress warning
dir.create(file.path(koutput_dir), showWarnings = F, recursive = T)
setwd(file.path(koutput_dir))

#writing csvs
st_write(all_events_appended, paste0(kmissiontype,"_all_events_appended_", kdatestart,"_",kdateend ,".csv"), layer_options = "GEOMETRY=AS_XY") 
write_csv(crime_stats_by_crime_class, paste0(kmissiontype,"_crime_stats_by_crime_class_", kdatestart,"_",kdateend ,".csv")) 
write_csv(crime_stats_by_event_model, paste0(kmissiontype,"_crime_stats_by_event_model_", kdatestart,"_",kdateend ,".csv")) 
write_csv(crime_stats_by_shift_starts, paste0(kmissiontype,"_crime_stats_by_shift_starts_", kdatestart,"_",kdateend ,".csv")) 
write_csv(agg_crime_percent_captured, paste0(kmissiontype,"_agg_crime_percent_captured_", kdatestart,"_",kdateend ,".csv")) 



#generating and saving slippy map
if (kmap){
  #Create leaflet map with layers as shifts
  pal <- colorFactor(c("maroon", "springgreen3"), domain = c(0, 1))
  pal1 = colorNumeric(palette = "YlOrRd", domain = range(all_events_appended$dist_m_closest_mission),
                      na.color = "transparent")
  
  leaflet_map = leaflet(options = leafletOptions(zoomControl = F)) %>% 
    addProviderTiles(providers$CartoDB.Positron)
  
  for (i in 1:length(missions)){
    leaflet_map = leaflet_map %>%
      addPolygons(data=missions[[i]], 
                  opacity = 0.2,
                  fillOpacity = 0.19, 
                  color = "grey",
                  group = names(missions)[i]) %>%
      addCircleMarkers(data=all_events_appended_list[[i]], 
                       weight = 1, 
                       color = ~ifelse(dist_m_closest_mission==0, pal(caught), pal1(dist_m_closest_mission)), 
                       stroke=T, 
                       radius= 3,
                       fillOpacity = 1,group = names(missions)[i])
  }
  
  g_title <- tags$span(tags$style("span {color: black; font-size:17px}"),
                       tags$b(paste(kcity, kmissiontype, "Missions", kdatestart ,"to", kdateend)))
  
  leaflet_map = leaflet_map %>% 
    addLayersControl(overlayGroups = names(missions), 
                     options = layersControlOptions(collapsed = T)) %>%
    addControl(g_title, position = "topleft") 
  
  saveWidget(leaflet_map, file=paste0(kcity, kmissiontype,"_leaflet_map_", kdatestart,"_",kdateend ,".html"))
  
}






