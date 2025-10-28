# Cap02

## Scripts Metadata: 

1) `ucs_shape.R` -> Gathering and cleaning UCS data. Use DATA/BR_UC_CD2022/BR_UC_Publicacao_CD2022.shp and DATA/cnuc_2024_10.csv and generates PA_clean_by_year.rds

2) `TI_shape.R` -> gathering data from indigenous territories. Use DATA/garbage/tis_poligonais/tis_poligonaisPolygon.shp and generates Outputs/IT_shape.gpkg

3) `socioeco_UC.R` -> We need to erase all rows after "old idea". Use Outputs/PA_clean_by_year.rds, DATA/inf_mort_UC2022.xlsx, waste_UC2022.xlsx, sanitation_UC2022.xlsx, water_UC2022.xlsx, lit_npeople_UC2022.xlsx, pop_UC2022.xlsx, and genrates UC_socio_data.rds.

4) `socioeco_IT.R` -> Use Outputs/IT_shape.gpkg, DATA/inf_mort_IT2022.xlsx, waste_IT2022.xlsx, sanitation_IT2022.xlsx, water_IT2022.xlsx, lit_npeople_IT2022.xlsx, pop_IT2022.xlsx, and generates Outputs/IT_socio_data.gpkg

5) `malha_treatments.R` -> Use Outputs/UC_socio_data.rds and IT_socio_data.gpkg. Genarates PA_IT_shape.gpkg.

6) `Elevation_fabdem_download.R` -> Download elevation data from each UC_TI. Use DATA/FABDEM_data_elevation/FABDEM_v1-2_tiles.geojson and Outputs/PA_IT_shape.gpkg. Generates DATA/FABDEM_data_elevation.

7) `elev_extract.R` -> Use Outputs/PA_IT_shape.gpkg, DATA/FABDEM_data_elevation/FABDEM_v1-2_tiles.geojson, and DATA/FABDEM_data_elevation/All. Generates "Outputs/PA_elevation.gpkg"

8) `defor_extract.R` -> Extracting deforestation 2022 data from UC + IT. Use DATA/Mapbiomas/deforestation/defor_c09_2022.tif and Outputs/PA_IT_shape.gpkg. Generates mapbiomas_raw_extract.rds and PA_defor.gpkg.

9) `Clim_extract.R` -> Use PA_IT_shape.gpkg, and DATA/WorldClim. Generates PA_climatic.gpkg

10) `Distance_extract.R` -> Estimating distance to urban pixel. We Should use mapbiomas land use data and PA_IT_shape.gpkg.

### To exclude Script:

`testing_popUCS.R `-> A script to test data from indigenous, ucs, quilombos with populations (UBGE) data
`Excluir_cap02.R`-> A script that contains code backups

