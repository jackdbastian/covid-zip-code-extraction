## This script extracts zip code level COVID-19 data and report date info (where available) from 14 different state dept of health API's. 

# "arcgis_api_date" function extracts the "Last edit date" value from arcGIS API endpoints.  This function is included in the body of the "zip_cases_w_date_extract" function.

# "zip_cases_w_date_extract" function extracts the case data and report date.  Currently 9 out of 14 states are including report date.

arcgis_api_date <- function(endpoint){
  require(rvest)
  require(qdapRegex)
  require(anytime)
  require(magrittr)
  
  api_page <- sub("query.*", "", endpoint)
  read_html(api_page) %>% 
    html_nodes(".restBody") %>% 
    html_text() %>%
    rm_between('Date: ', ' PMSupported', extract = TRUE) %>%
    extract2(1) %>%
    anydate()
}

zip_cases_w_date_extract <- function() {
  require(dplyr)
  require(magrittr)
  require(readr)
  require(jsonlite)
  require(anytime)
  
  AZ_endpoint <- "https://services1.arcgis.com/mpVYz37anSdrK4d8/ArcGIS/rest/services/CVD_ZIPS_FORWEBMAP/FeatureServer/0/query?where=OBJECTID%3E0&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnGeometry=false&returnCentroid=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pjson&token="
  FL_endpoint <- "https://services1.arcgis.com/CY1LXxl9zlJeBuRZ/ArcGIS/rest/services/Florida_COVID19_Cases_by_Zip_Code_vw/FeatureServer/0//query?where=0%3D0&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=ZIP%2C+Cases_1&returnGeometry=false&returnCentroid=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pjson&token="
  IL_endpoint <- "https://www.dph.illinois.gov/sitefiles/COVIDZip.json?nocache=1"
  IN_endpoint <- "https://hub.mph.in.gov/dataset/14a59397-9ebc-4902-a7c7-fd7ca3c08101/resource/3ea01356-42e4-42aa-8935-493709313ca3/download/covid_count_per_zip_all.csv"
  MD_endpoint <- "https://services.arcgis.com/njFNhDsUCentVYJW/ArcGIS/rest/services/MDH_COVID_19_Dashboard_Feature_Layer_ZIPCodes_MEMA/FeatureServer/0/query?where=0%3D0&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=ZIPCODE1%2C+ProtectedCount&returnGeometry=false&returnCentroid=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pjson&token="
  NM_endpoint <- "https://e7p503ngy5.execute-api.us-west-2.amazonaws.com/prod/GetPublicZipsData"
  NC_endpoint <- "https://services.arcgis.com/iFBq2AW9XO0jYYF7/ArcGIS/rest/services/Covid19byZIPnew/FeatureServer/0/query?where=OBJECTID%3E0&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=*&returnGeometry=false&returnCentroid=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pjson&token="
  NJ_endpoint <- "https://services7.arcgis.com/Z0rixLlManVefxqY/ArcGIS/rest/services/COVID_Zip_Code_Data/FeatureServer/0/query?where=0%3D0&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=ZIP_CODE%2C+Case_Count_per_Zip_code&returnGeometry=false&returnCentroid=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pjson&token="
  OK_endpoint <- "https://storage.googleapis.com/ok-covid-gcs-public-download/oklahoma_cases_zip.csv"
  OR_endpoint <- "https://projects.oregonlive.com/coronavirus/data/rona_zip.json"
  PA_endpoint <- "https://services2.arcgis.com/xtuWQvb2YQnp0z3F/ArcGIS/rest/services/ZIP_Code_PA_COVID/FeatureServer/0/query?where=0%3D0&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=POSTCODE%2C+Positive&returnGeometry=false&returnCentroid=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pjson&token="
  RI_endpoint <- "https://static.dwcdn.net/data/59UZY.csv"
  SC_endpoint <- "https://services2.arcgis.com/XZg2efAbaieYAXmu/ArcGIS/rest/services/COVID19/FeatureServer/1//query?where=0%3D0&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=POSTCODE%2C+Positive&returnGeometry=false&returnCentroid=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pjson&token="
  VA_endpoint <- "https://www.vdh.virginia.gov/content/uploads/sites/182/2020/05/VDH-COVID-19-PublicUseDataset-ZIPCode.csv"
  
  bind_rows(fromJSON(AZ_endpoint)$features$attributes %>%
              transmute(zip_code = as.character(POSTCODE), case_count = as.numeric(ConfirmedCaseCount), report_date = arcgis_api_date(AZ_endpoint)),
            fromJSON(FL_endpoint)$features$attributes %>%
              transmute(zip_code = as.character(ZIP), case_count = as.numeric(Cases_1), report_date = arcgis_api_date(FL_endpoint)),
            fromJSON(IL_endpoint)$zip_values %>% 
              transmute(zip_code = as.character(zip), case_count = as.numeric(confirmed_cases), report_date = anydate(paste(unlist(fromJSON(IL_endpoint)$LastUpdateDate), collapse  = "-"))),
            read_csv(IN_endpoint) %>% 
              transmute(zip_code = as.character(ZIP_CD), case_count = as.numeric(PATIENT_COUNT)),
            fromJSON(MD_endpoint)$features$attributes %>% 
              transmute(zip_code = as.character(ZIPCODE1), case_count = as.numeric(ProtectedCount), report_date = arcgis_api_date(MD_endpoint)),
            fromJSON(NM_endpoint)$data %>% 
              transmute(zip_code = as.character(zip), case_count = as.numeric(cases)),
            fromJSON(NC_endpoint)$features$attributes %>% 
              transmute(zip_code = as.character(ZIPCode), case_count = as.numeric(Cases), report_date = arcgis_api_date(NC_endpoint)),
            fromJSON(NJ_endpoint)$features$attributes %>% 
              transmute(zip_code = as.character(ZIP_CODE), case_count = as.numeric(Case_Count_per_Zip_code), report_date = arcgis_api_date(NJ_endpoint)),
            read_csv(OK_endpoint) %>% 
              transmute(zip_code = as.character(Zip), case_count = as.numeric(Cases), report_date = ReportDate),
            fromJSON(OR_endpoint)$features$properties %>% 
              transmute(zip_code = as.character(GEOID10), case_count = as.numeric(cases_num)),
            fromJSON(PA_endpoint)$features$attributes %>% 
              transmute(zip_code = as.character(POSTCODE), case_count = as.numeric(Positive), report_date = arcgis_api_date(PA_endpoint)) %>% 
              na_if(-1),
            read_csv(RI_endpoint) %>% 
              transmute(zip_code = as.character(ZCTA), case_count = as.numeric(`Rhode Island COVID-19 cases`)),
            fromJSON(SC_endpoint)$features$attributes %>% 
              transmute(zip_code = as.character(POSTCODE), case_count = as.numeric(Positive)),
            read_csv(VA_endpoint) %>% 
              transmute(zip_code = as.character(ZCTA), case_count = as.numeric(`Number of Cases`), report_date = anydate(`Report Date`))
  ) %>% 
    filter(grepl("^\\d*$", zip_code))
}
