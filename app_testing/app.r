library(shiny)
library(leaflet)
library(htmltools)
library(rgdal)
library(sp)
library(googledrive)


### Download Google Drive data ###

# might be a better idea to add this to the deployApp script
	# not sure how this will work when deploying the app

# Could set this to the Aerial_survey_data folder, 
	# in which case list.files below isn't really needed,
	# files$name would have the same info
# Alternatively, might want to set pattern = "some pattern" to make sure only data of interest is downloaded
	# might need multiple calls to drive_find() in that case
# may also need to navigate to the correct folder in Google Drive
	# haven't tested this yet

#fwc.sightings = drive_find(pattern = "FWS[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]_Sightings")
#fwc.surveytrack = drive_find(pattern = "FWS[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]_SurveyTrack")
#S2S.sightings = drive_find(pattern = "GWS[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]_Sightings")
#S2S.surveytrack = drive_find(pattern = "GWS[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]_SurveyTrack")

# can't set wd in actual app, this is for testing outside of using runApp()	
#setwd("R:/Projects/WhaleMap/app_testing")

# Loop through files on Google Drive and download them
	# saves them as whatever they are named on drive
	# could rename them with path = "new name" argument
#if(dim(fwc.sightings)[1] > 0){
#	for(f in fwc.sightings$name){
#		drive_download(f, path = paste0("googledrive_downloads/", f), overwrite=T)
#	}
#}
#if(dim(fwc.surveytrack)[1] > 0){
#	for(f in fwc.surveytrack$name){
#		drive_download(f, path = paste0("googledrive_downloads/", f), overwrite=T)
#	}
#}
#if(dim(S2S.sightings)[1] > 0){
#	for(f in S2S.sightings$name){
#		drive_download(f, path = paste0("googledrive_downloads/", f), overwrite=T)
#	}
#}
#if(dim(S2S.surveytrack)[1] > 0){
#	for(f in S2S.surveytrack$name){
#		drive_download(f, path = paste0("googledrive_downloads/", f), overwrite=T)
#	}
#}

# file$drive_resource contains a lot of info about files on Google Drive
	# for instance $createdTime and modifiedTime could be useful, if only recent files should be downloaded


### Survey data ###

# processed aerial survey data from 2015Â–2016 season
# setwd("R:/Projects/WhaleMap")
#tracks.FWS = list.files(path = "googledrive_downloads", pattern = "FWS[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]_SurveyTrack.csv")
#tracks.GWS = list.files(path = "googledrive_downloads", pattern = "GWS[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]_SurveyTrack.csv")

# Ideally find a better way to format date; 
# currently taking it from file name because it was a quick workaround for as.Date() not recognizing the date values
#dat = NULL
#if(length(tracks.FWS) > 0){
#	for(i in tracks.FWS){
#		temp = read.csv(paste0("googledrive_downloads/", i), stringsAsFactors=F)
#		temp$DateFormatted = as.Date(strptime(temp$Date, format = '%m/%d/%Y %H:%M:%S'))
#		dat = rbind(dat, temp)
#	}
#}
#if(length(tracks.GWS) > 0){
#	for(i in tracks.GWS){
#		temp = read.csv(paste0("googledrive_downloads/", i), stringsAsFactors=F)
#		temp$DateFormatted = as.Date(strptime(temp$Date, format = '%m/%d/%Y %H:%M:%S'))
#		dat = rbind(dat, temp)
#	}
#}

#dat = dat[,c("Survey", "Date", "Latitude", "Longitude", "DateFormatted")]
#gc()

### Sighting data ###

#whales.FWS = list.files(path = "googledrive_downloads", pattern = "FWS[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]_Sightings.csv")
#whales.GWS = list.files(path = "googledrive_downloads", pattern = "GWS[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]_Sightings.csv")

#whales_dat = NULL
#if(length(whales.FWS) > 0){
#	for(i in whales.FWS){
#		temp = read.csv(paste0("googledrive_downloads/", i), stringsAsFactors=F)
#		if(nrow(temp) > 0){
#			temp$DateFormatted = as.Date(strptime(temp$Date, format = '%m/%d/%Y %H:%M:%S'))
#			temp$DDSOURCE = "FWS"
#			whales_dat = rbind(whales_dat, temp)
#		}
#	}
#}

#if(length(whales.GWS) > 0){
#	for(i in whales.GWS){
#		temp = read.csv(paste0("googledrive_downloads/", i), stringsAsFactors=F)
#		if(nrow(temp) > 0){
#			temp$DateFormatted = as.Date(strptime(temp$Date, format = '%m/%d/%Y %H:%M:%S'))
#			temp$DDSOURCE = "GWS1819"
#			whales_dat = rbind(whales_dat, temp)
#		}
#	}
#}

#whales_dat = whales_dat[,c("Date", "SpeciesCode", "GroupSize", "Calves", "SightingTime", "SightingLat", "SightingLon", "DateFormatted", "DDSOURCE")]
#gc()

# handle issue where -9 was in calf field
#if(nrow(whales_dat[whales_dat$Calves < 0,]) > 0){
#	whales_dat[whales_dat$Calves < 0,]$Calves = 0
#}

# Times of whale sightings might be in UTC time, may need changing

#whales_dat$SPECCODE = whales_dat$SpeciesCode
#if(nrow(whales_dat[substr(whales_dat$SpeciesCode,3,4) %in% "WH" & !(whales_dat$SpeciesCode %in% c("RIWH", "HUWH")),]) > 0){ 
#	whales_dat[substr(whales_dat$SpeciesCode,3,4) %in% "WH" & !(whales_dat$SpeciesCode %in% c("RIWH", "HUWH")),]$SpeciesCode = "Other"
#}

dat = read.csv("Survey_Track_Shiny_Data.csv")
dat$DateFormatted = as.Date(dat$DateFormatted)
whales_dat = read.csv("Sightings_Shiny_Data.csv")
whales_dat$DateFormatted = as.Date(whales_dat$DateFormatted)


### Photo ID data ###
# need to download photoID summaries and save tham as .csv
#tabs = list.files(path = "PhotoID")
# this should find the most recent summary, given current naming convention
#PID_table_name = tabs[which(as.numeric(substr(tabs,1,8)) %in% max(as.numeric(substr(tabs,1,8)),na.rm=T) & 
#				substr(tabs,12,15) %in% ".csv")]
photoIDtab = read.csv("PhotoID_Shiny_Data.csv", check.names=F)
pidCol = grep("Whale ID", names(photoIDtab))
sexCol = grep("Sex", names(photoIDtab))
moCol = grep("Month", names(photoIDtab))
daCol = grep("Day", names(photoIDtab))
srvyCol = grep("Survey", names(photoIDtab))
ageCol = grep("AgeClass", names(photoIDtab))
clfCol = grep("w/Calf", names(photoIDtab))
dtCol = grep("DateFormatted", names(photoIDtab))
photoIDtab[,dtCol] = as.Date(photoIDtab[,dtCol])
photoIDtab[,clfCol] = as.Date(photoIDtab[,clfCol])
#PID_reportdate = as.Date(paste(substr(PID_table_name,1,4), substr(PID_table_name,5,6), substr(PID_table_name,7,8), sep="-"))
PID_reportdate = Sys.Date()

# age column was too messy to deal with
#photoIDtab$AgeClass = NA
#for(i in 1:nrow(photoIDtab)){
#	if(!is.na(as.numeric(as.character(photoIDtab[i,ageCol])))){
#		if(as.numeric(as.character(photoIDtab[i,ageCol])) >= 9){
#			photoIDtab[i,]$AgeClass = "A"
#		} else{
#			photoIDtab[i,]$AgeClass = "J"
#		}
#	} else if(substr(as.character(photoIDtab[i,ageCol]),1,1) %in% ">"){
#		if(nchar(as.character(photoIDtab[i,ageCol]))>2){
#			photoIDtab[i,]$AgeClass = "A"
#		} else if(substr(as.character(photoIDtab[i,ageCol]),2,2) %in% c("8", "9")){
#			photoIDtab[i,]$AgeClass = "A"
#		} else{
#			phototIDtab[i,]$AgeClass = "J"
#		}
#	} else if(as.character(photoIDtab[i,ageCol]) %in% c("Calf", "calf")){
#		photoIDtab[i,]$AgeClass = "C"
#	} else if(as.character(photoIDtab[i,ageCol]) %in% c("Unk", "unk", "Unknown", "unknown")){
#		photoIDtab[i,]$AgeClass = "U"
#	}
#}


### GIS polygon layers ###
#SEUS SMA shapefile
seusSMA <- readOGR("GIS_layers", "SEUS_SMA_latlong")

#SEUS Recommended Shipping Lanes shapefile
seusRECLANES <- readOGR("GIS_layers", "SEUS_RecLanes_latlong")

whale_icon <- makeIcon("Fluke_Icon2.png", iconWidth=30, iconHeight = 25,
	iconAnchorX = 15, iconAnchorY = 20)
	
# Habitat Model Predictions
CurHabModLyr = spTransform(readOGR(dsn = "PredictiveTool.gdb", layer = "HabModelPredictions"), CRS("+proj=longlat +datum=WGS84"))
LTHabModLyr = spTransform(readOGR(dsn = "PredictiveTool.gdb", layer = "LongTermPredictions"), CRS("+proj=longlat +datum=WGS84"))

Current_names = names(CurHabModLyr)[grep("Pres", names(CurHabModLyr))]
# would need to update this each season with current dates; maybe get date from computer date()
Current_names = c(Current_names[grep("18", Current_names)], Current_names[grep("19", Current_names)])
LongTerm_names = names(LTHabModLyr)[grep("pres", names(LTHabModLyr))]

CurHabModLyr = CurHabModLyr[,names(CurHabModLyr) %in% Current_names]
LTHabModLyr = LTHabModLyr[,names(LTHabModLyr) %in% LongTerm_names]
gc()

# if this is not updated when a new field is added to CurHabModLyr, NA will be displayed in the drop down for the new field
# therefore, update this file
SST_predictDates = read.csv("SST_dates.csv")

# would be wise to actually match these up instead of assuming correct order
#SST_predictDates$match = Current_names

SST_choices = Current_names
names(SST_choices) = sapply(1:nrow(SST_predictDates), function(x) paste0(SST_predictDates[x,1], "; ", SST_predictDates[x,2]))

# HabMod palette
bins = c(0,0.01,0.03,0.05,0.08,0.12,0.17,0.2,0.3,0.4,0.6)
cols.hsv = matrix(c(200, 80, 78,
				190, 42, 70,
				135, 21, 74,
				77, 35, 83,
				63, 52, 93,
				51, 64, 99,
				36, 73, 99,
				25, 80, 98,
				15, 86, 95,
				359, 93, 91), 
				ncol = 3, byrow=T)
cols = sapply(1:nrow(cols.hsv), function(x) hsv(cols.hsv[x,1]/360, cols.hsv[x,2]/100, cols.hsv[x,3]/100))
pal = colorBin(cols, domain = c(0,1), bins = bins)
#pal = colorBin("YlGnBu", domain = c(0,1), bins = bins)

# having issues with start = ifelse(); below is workaround
if((max(dat$DateFormatted)-14)<min(dat$DateFormatted)){
	s = min(dat$DateFormatted)
} else{
	s = max(dat$DateFormatted)-14
}

ui = fluidPage(
	titlePanel("WhaleMap–Southeast"),

	sidebarLayout(
		sidebarPanel(

			# select dates of interest; controls tracklines and whale sightings in leaflet map (input$dateRange)
			dateRangeInput("dateRange", 
				"Select date range:",
				start = s,
				end = max(dat$DateFormatted),
				min = min(dat$DateFormatted),
				max = max(dat$DateFormatted)),

			# select survey teams of interest; controls tracklines and whale sightings in leaflet map (input$surveyTeams)
			checkboxGroupInput("surveyTeams",
				"Select survey teams:",
				choices = c("FL Fish and Wildlife Conservation Commission" = "FWS", "Sea to Shore Alliance" = "GWS1819"),
				selected = c("FWS", "GWS1819")),

			# select whale species of interest; controls whale sightings in leaflet map (input$whaleSpecies)
			checkboxGroupInput("whaleSpecies",
				"Select whale species:",
				choices = c("North Atlantic Right Whale" = "RIWH", "Humpback Whale" = "HUWH", "Other large whale Sp." = "Other"),
				selected = c("RIWH")),
				
			strong("Management features:"),

			# select SEUS_SMA
			checkboxInput("SEUS_SMA",
				"Southeastern US seasonal management area",
				value = TRUE
			),
			
			# select Recommended Shipping Lanes
			checkboxInput("RecLanes",
				"Recommended shipping lanes",
				value = TRUE
			),

			# select habitat model attribute; would need to add another selection criteria each time this is updated
			selectInput("CurrentHabMod",
				"Display current habitat model prediction from:",
				choices = c("Select", SST_choices)
			),
			
			# select long term habitat model attribute
			selectInput("LTHabMod",
				"Display long term habitat model prediction from:",
				choices = c("Select", LongTerm_names)
			),
			
			em("Disclaimer: This graphical representation is provided for informational purposes and should not be considered authoritative for navigational, engineering, legal, and other uses. Data are preliminary, subject to change, and intended only for the coordination of field research efforts. Any other use of data requires permission from the originating datasource.")
			
		), 
		mainPanel(
			leafletOutput("map", height = 750),
			fluidRow(
				column(8,
					h3("Photo ID summary:"),
					sliderInput("photoID.Date",
						"Select photo ID data before:",
						min = as.Date("2018-12-01"),
						max = PID_reportdate,
						value = PID_reportdate),
					tableOutput('PhotoID'),
					em("*Photo ID summary includes sightings from the aerial and vessel based surveys and the public")
				)
			)
		)
	)
)


server = function(input, output, session){

	# get trackline data of interest according to inputs selected
	trackline_data = reactive({
		dat[dat$DateFormatted>=input$dateRange[1] & dat$DateFormatted<=input$dateRange[2] & 
			dat$Survey %in% input$surveyTeams,]
	})

	# get whale sighting data of interest according to input selected
	whales = reactive({
		whales_dat[whales_dat$SpeciesCode %in% input$whaleSpecies & 
			whales_dat$DateFormatted>=input$dateRange[1] & 
			whales_dat$DateFormatted<=input$dateRange[2] &
			whales_dat$Survey %in% input$surveyTeams,]
	})
	
	# summarize photo ID data
	
	PIDtab = reactive({
		data.frame(matrix(c("Calf", 
		length(unique(photoIDtab[!is.na(photoIDtab[,clfCol]) & photoIDtab[,clfCol] < input$photoID.Date, pidCol])),
		length(unique(photoIDtab[!is.na(photoIDtab[,clfCol]), pidCol])),
		"Adult Female",
		length(unique(photoIDtab[photoIDtab[,sexCol] %in% 'F' & photoIDtab$AgeClass %in% "A" & photoIDtab[,dtCol] < input$photoID.Date, pidCol])),
		length(unique(photoIDtab[photoIDtab[,sexCol] %in% 'F' & photoIDtab$AgeClass %in% "A", pidCol])),
		"Juvenile Female",
		length(unique(photoIDtab[photoIDtab[,sexCol] %in% 'F' & photoIDtab$AgeClass %in% "J" & photoIDtab[,dtCol] < input$photoID.Date, pidCol])),
		length(unique(photoIDtab[photoIDtab[,sexCol] %in% 'F' & photoIDtab$AgeClass %in% "J", pidCol])),
		"Adult Male",
		length(unique(photoIDtab[photoIDtab[,sexCol] %in% 'M' & photoIDtab$AgeClass %in% "A" & photoIDtab[,dtCol] < input$photoID.Date, pidCol])),
		length(unique(photoIDtab[photoIDtab[,sexCol] %in% 'M' & photoIDtab$AgeClass %in% "A", pidCol])),
		"Juvenile Male",
		length(unique(photoIDtab[photoIDtab[,sexCol] %in% 'M' & photoIDtab$AgeClass %in% "J" & photoIDtab[,dtCol] < input$photoID.Date, pidCol])),
		length(unique(photoIDtab[photoIDtab[,sexCol] %in% 'M' & photoIDtab$AgeClass %in% "J", pidCol])),
		"Unknown",
		length(unique(photoIDtab[photoIDtab[,sexCol] %in% 'Unk' & photoIDtab[,dtCol] < input$photoID.Date,pidCol])),
		length(unique(photoIDtab[photoIDtab[,sexCol] %in% 'Unk',pidCol])),
		"Total",
		length(unique(photoIDtab[photoIDtab[,dtCol] < input$photoID.Date, pidCol])) + length(unique(photoIDtab[!is.na(photoIDtab[,clfCol]) & photoIDtab[,clfCol] < input$photoID.Date, pidCol])),
		length(unique(photoIDtab[, pidCol])) + length(unique(photoIDtab[!is.na(photoIDtab[,clfCol]), pidCol]))),ncol = 3, nrow = 7, byrow = T))		
	})
	PIDtab_wHeaders = reactive({
		temp = PIDtab()
		colnames(temp)[1] = ""
		colnames(temp)[2] = paste0("Number of individual right whales (2018-12-01 to ", input$photoID.Date, ")")
#		if(input$dateRange[1]>PID_reportdate){
#			colnames(temp)[2] = paste0("Photo ID data unavailable past ", PID_reportdate)
#		} else{
#			if(input$dateRange[2]>PID_reportdate){ 
#				max_date = PID_reportdate 
#			}else{
#				max_date = input$dateRange[2]
#			}
#			colnames(temp)[2] =  paste0("Number of individual right whales (", input$dateRange[1], " to ", max_date, ")")
#		}
		colnames(temp)[3] = paste0("Number of individual right whales (2018-12-01 to ", PID_reportdate, ")")	
		return(temp)
	})
	
	output$PhotoID = renderTable(PIDtab_wHeaders(), align='c')
 
	# create basemap to add "observers" to
	output$map <- renderLeaflet({
		leaflet() %>% 
  			# add ocean basemap
  			addProviderTiles(providers$Esri.OceanBasemap) %>%
  
  			# focus map in a certain area / zoom level
  			setView(lng = -80, lat = 30, zoom = 7) %>%
  
  			# add layer with place names
  			addProviderTiles(providers$Hydda.RoadsAndLabels, group = 'Place names') %>%
  
  			# add layers control
  			addLayersControl(overlayGroups = c('Place names'),
                   	options = layersControlOptions(collapsed = FALSE),
                   	position = 'topright') %>%

			# add scalebar and measurement tool
  			addScaleBar(position = 'topright') %>% 

  			addMeasure(
    				primaryLengthUnit = "kilometers",
    				secondaryLengthUnit = 'miles', 
					primaryAreaUnit = "hectares",
    				secondaryAreaUnit="acres", 
    				position = 'topleft') %>%
					
			hideGroup('Place names')
	})
	
	# add seasonal management area
	observeEvent(input$SEUS_SMA, {
		proxy = leafletProxy("map") %>% clearGroup("sma")
		if(input$SEUS_SMA){
			proxy %>% addPolygons(data = seusSMA,
				color="black", weight=1, 
				opacity=1, fillOpacity=0,
				group = "sma")
		}
	})

	# add recommended shipping lanes
	observeEvent(input$RecLanes, {
		proxy = leafletProxy("map") %>% clearGroup("reclanes")
		if(input$RecLanes){
			proxy %>% addPolygons(data=seusRECLANES,
				color="gray", weight=1, 
				opacity=1, group="reclanes")
		}
	})

	# add current habitat model predictions
	observeEvent(input$CurrentHabMod, {
		proxy = leafletProxy("map") %>% clearGroup("CurHabMod") %>% clearGroup("LTHabMod") %>% clearControls()
		if(!("Select" %in% input$CurrentHabMod)){
			proxy %>% 
			addPolygons(data=CurHabModLyr[!is.na(CurHabModLyr@data[,input$CurrentHabMod]),],
				fillColor= ~pal(CurHabModLyr@data[!is.na(CurHabModLyr@data[,input$CurrentHabMod]),input$CurrentHabMod]),				
				fillOpacity = 0.6, weight=0, opacity=1, group="CurHabMod") %>%
			addLegend("bottomleft", pal = pal, 
				values = bins,
				title = "Probability of Right Whale sighting", 
				opacity = 1)
		}
	})

	# add long term habitat model predictions
	observeEvent(input$LTHabMod, {
		proxy = leafletProxy("map") %>% clearGroup("LTHabMod") %>% clearGroup("CurHabMod") %>% clearControls()
		if(!("Select" %in% input$LTHabMod)){
			proxy %>% 
			addPolygons(data=LTHabModLyr[!is.na(LTHabModLyr@data[,input$LTHabMod]),],
				fillColor= ~pal(LTHabModLyr@data[!is.na(LTHabModLyr@data[,input$LTHabMod]),input$LTHabMod]),				
				fillOpacity = 0.6, weight=0, opacity=1, group="LTHabMod") %>%
			addLegend("bottomleft", pal = pal, 
				values = bins,
				title = "Probability of Right Whale sighting", 
				opacity = 1)
		}
	})

	# add trackline data observer
	observe({
		proxy = leafletProxy("map", data=trackline_data()) %>%
			clearGroup("tldat") %>%
  			addPolylines(~Longitude, ~Latitude,
               		weight = 2,
               		color = 'red',
               		opacity = 0.5,
               		#popup = dat$Trackline, #this slows performance
               		smoothFactor = 3,
               		group = "tldat")
	})

	# add whale data observer
	observe({
		proxy = leafletProxy("map", data=whales()) %>%
			clearGroup("whldat") %>%
			#add whale sightings
			addMarkers(~Longitude, ~Latitude,
				icon = whale_icon,
				#clusterOptions = markerClusterOptions(),
				label = ~lapply(paste(sep = "<br/>",
					paste0("Survey Team: ", Survey),
					paste0("Date: ", DateFormatted),
					paste0("Time: ", strftime(strptime(Time, format = "%m/%d/%Y %H:%M:%S"), format = "%H:%M")),
					paste0("Lat, Lon: ", format(round(Latitude, 1), nsmall=1), ", ", format(round(Longitude, 1), nsmall=1)),
					paste0("Species: ", SpeciesCode),
					paste0("Number: ", NUMBER),
					paste0("Calves: ", NUMCALF)
				),HTML),
            	group = "whldat") 
	})
		
}

#Would currently need to work on layering the different features
#Whale points would need to be on top or at least the labels need to show when scrolled over, which isn't currently happening

shinyApp(ui = ui, server = server)

