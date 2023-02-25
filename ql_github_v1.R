# this script create a function to generate location quotient from municipalities, in comparison
# to the immediate region, according IBGE classification

# Observatory of public policies and government of Ponte Nova
# Author: Lauro Marques Vicari

# loading packages

pacman::p_load(dplyr, sf, leaflet, readxl, stringi, viridis)

# loading data

# rais database

rais <- read.csv("rais_mg_munics_2021.csv",
                 sep = ";",
                 header = T)

rais$COD_MUNIC <- rais$COD_MUNIC |> as.numeric()

# regint e regim ibge

regs_ibge <- read_excel("regint_regim_ibge.xlsx")
regs_ibge$CD_GEOCODI <- regs_ibge$CD_GEOCODI |> as.numeric()
regs_ibge$nome_rgi <- toupper(stri_trans_general(str = regs_ibge$nome_rgi, id = "Latin-ASCII"))
regs_ibge$nome_rgint <- toupper(stri_trans_general(str = regs_ibge$nome_rgint, id = "Latin-ASCII"))

# reading shapefile

mg_munics <- st_read("MG_Municipios_2020.shp")

mg_munics <- st_simplify(mg_munics, preserveTopology = TRUE, dTolerance = 1000)
mg_munics$CD_MUN <- mg_munics$CD_MUN |> as.numeric()

# joining data

rais_regs <- left_join(rais, regs_ibge, by = c("COD_MUNIC" = "CD_GEOCODI"))

# function

ql <- function(sector, rgi, plot) {
  
  # filtering by geography
  rgi_format = toupper(stri_trans_general(str = rgi, id = "Latin-ASCII"))
  rais_regs_select = rais_regs[rais_regs$nome_rgi == rgi_format,]
  
  # calculating ql
  rais_regs_select$ql = (rais_regs_select[ ,paste0(sector, "_vinc_ativo")] / sum(rais_regs_select[ ,paste0(sector, "_vinc_ativo")])) / (rais_regs_select$total_vinc_ativo / sum(rais_regs_select$total_vinc_ativo)) 
  rais_regs_select$ql = round(rais_regs_select$ql, digits = 2)
  
  if (missing(plot)) {
    
    return(rais_regs_select[,c("NM_MUNIC", "ql")])
    
  } else {
    
    map_rgi <- left_join(rais_regs_select, mg_munics, by = c("COD_MUNIC" = "CD_MUN")) |>
      st_as_sf() |>
      st_transform(st_crs('+proj=longlat +datum=WGS84'))
    
    pallete <- colorNumeric(
      palette = "Blues",
      domain = map_rgi$ql)
    
    mapa <- leaflet() |>
      addTiles() |>
      addProviderTiles("OpenStreetMap.Mapnik", group = "Street") |>
      addPolygons(data = map_rgi, color = "black", weight = 3,
                  opacity = 1, fillColor = ~pallete(ql), fillOpacity = 0.7,
                  popup = paste0("<div style='text-align:center'>", 
                                 "<b></b>", "<strong>", map_rgi$NM_MUN ,"</strong>",
                                 "<b></b>", "<br>", "<b></b>", "<br>",
                                 "<b>Setor: </b>", sector, "<br>",
                                 "<b></b>", "<br>",
                                 "<b>Q.L: </b>", map_rgi$ql, "<br>")) 
    
    return(list(df = rais_regs_select[,c("NM_MUNIC", "ql")], mapa = mapa))
    
  }
  
}

# testing

ql(sector = "agric_pec", rgi = "ponte nova", plot = T)

