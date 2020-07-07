library(dplyr)
library(magrittr)
library(jsonlite)

FL_endpoint <- "https://services1.arcgis.com/CY1LXxl9zlJeBuRZ/ArcGIS/rest/services/Florida_COVID19_Cases_by_Zip_Code_vw/FeatureServer/0/query?where=0%3D0&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&returnGeodetic=false&outFields=ZIP%2C+LabelY&returnGeometry=false&returnCentroid=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pjson&token="
FL_zip<-
  fromJSON(FL_endpoint) %>%
  extract2("features") %>%
  extract2("attributes") %>%
  set_colnames(c("zip_code", "case_count")) %>%
  replace(FL_zip == -2, NA) #-2 indicates <5 cases