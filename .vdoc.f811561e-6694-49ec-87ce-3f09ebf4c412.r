#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| warning: false

# Carregar pacotes
library(ConR)
library(geobr)
library(sf)
library(leaflet)

# 1. Obter o estado do MS
ms <- read_state(code_state = "MS", year = 2020, showProgress = FALSE)
ms <- st_transform(ms, 4326)

# 2. Gerar pontos aleatórios no MS
set.seed(42)
pontos <- st_sample(ms, size = 10)
pontos_sf <- st_sf(geometry = pontos, crs = 4326)

# 3. Criar data.frame no formato esperado por ConR
coords <- st_coordinates(pontos_sf)
XY <- data.frame(
    latitude = coords[, 2],
    longitude = coords[, 1],
    tax = rep("Especie_teste", length.out = nrow(coords))
)

# 4. Calcular EOO e exportar shapefile temporário
resultado <- EOO.computing(
    XY,
    export_shp = TRUE,
    write_shp = TRUE,
    method.range = "convex.hull",
    proj_type = "cea",
    show_progress = FALSE
)

resultado

# 5. Ler o shapefile gerado
eoo_shape <- st_read("shapesIUCN/EOO_poly.shp", quiet = TRUE)
eoo_shape <- st_transform(eoo_shape, 4326)

# 6. Visualizar no leaflet
leaflet() |>
    addTiles(group = "Mapa base") |>
    addProviderTiles(providers$Esri.WorldImagery, group = "Imagem de satélite") |>
    addPolygons(data = eoo_shape, color = "red", weight = 2, fillOpacity = 0.3, group = "EOO") |>
    addCircleMarkers(data = pontos_sf, radius = 5, color = "blue", group = "Ocorrências") |>
    addLayersControl(
        baseGroups = c("Mapa base", "Imagem de satélite"),
        overlayGroups = c("Ocorrências", "EOO"),
        options = layersControlOptions(collapsed = FALSE)
    )

#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| warning: false

library(sf)
library(ggplot2)
library(units)

# 1. Definir a área de interesse (um retângulo simples)
# Vamos criar uma área com limites de coordenadas fictícias em WGS84 (EPSG:4326)
area <- st_sfc(st_polygon(list(matrix(c(
  -43.0, -22.9,  # Ponto 1 (lon, lat)
  -42.9, -22.9,  # Ponto 2
  -42.9, -22.8,  # Ponto 3
  -43.0, -22.8,  # Ponto 4
  -43.0, -22.9   # Fechar o polígono
), ncol = 2, byrow = TRUE))))

# Atribuindo CRS WGS84 (EPSG:4326) à área
st_crs(area) <- 4326  # WGS84

# 2. Transformar a área para a projeção UTM (zona 23S, que é uma projeção comum para o Brasil)
area_utm <- st_transform(area, crs = 32723)  # Sistema UTM, zona 23S

# 3. Gerar pontos dentro da área
pontos <- st_sample(area_utm, size = 20)  # Gerar 20 pontos dentro da área

# 4. Criar o grid com células de 2 km
cellsize <- 2000  # 2000 metros = 2 km por célula
grid <- st_make_grid(area_utm, cellsize = cellsize, square = TRUE)

# 5. Verificar quais células do grid contêm pontos
intersections <- st_intersects(grid, pontos)
grid_ocupado <- grid[lengths(intersections) > 0]  # Seleciona as células com pontos

# 6. Visualização
ggplot() +
  geom_sf(data = grid, fill = "lightgray", color = "black", lwd = 0.5) +  # Mostrar o grid
  geom_sf(data = grid_ocupado, fill = "red", color = "black", lwd = 0.5) +  # Células ocupadas
  geom_sf(data = pontos, color = "blue", size = 3) +  # Mostrar os pontos
  theme_minimal()

# Área total do grid (em km²)
area_total_grid <- set_units(sum(st_area(grid)), "km^2") 
area_total_grid

# Área total das células ocupadas (em km²)
area_total_ocupado <- set_units(sum(st_area(grid_ocupado)), "km^2")
area_total_ocupado

#
#
#
#| warning: false

library(sf)
library(geobr)
library(leaflet)

# 1. Estado do MS e pontos aleatórios
ms <- read_state(code_state = "MS", year = 2020, showProgress = FALSE)
ms <- st_transform(ms, 4326)

set.seed(42)
pontos <- st_sample(ms, size = 10)
pontos_sf <- st_sf(geometry = pontos, crs = 4326)

# 2. Transformar para projeção métrica (para buffer em metros)
pontos_proj <- st_transform(pontos_sf, 3857)

# 3. Verifica número de pontos únicos
unique_coords <- unique(st_coordinates(pontos_proj))
n_pontos <- nrow(unique_coords)

# 4. Criar grade de 2km x 2km ao redor dos pontos (para todos os pontos)
cellsize <- 2000 # 2 km

bbox <- st_bbox(area_utm)

grid <- st_make_grid(pontos_proj, cellsize = cellsize, square = TRUE)

# Intersecção do grid com os pontos
intersections <- st_intersects(grid, pontos_proj)
grid_intersect <- grid[lengths(intersections) > 0]
aoo_grid <- st_sf(geometry = grid_intersect)

# 5. Calcular área da AOO (em km²)
aoo_area <- sum(st_area(aoo_grid)) / 10^6
print(paste("AOO estimado:", round(aoo_area, 2), "km²"))

# 6. Visualização com todos os pontos
leaflet() |>
  addTiles(group = "Mapa base") |>
  addProviderTiles(providers$Esri.WorldImagery, group = "Imagem de satélite") |>
  addPolygons(data = st_transform(aoo_grid, 4326), color = "green", weight = 2, fillOpacity = 0.3, group = "AOO") |>
  addCircleMarkers(data = pontos_sf, radius = 5, color = "blue", group = "Ocorrências") |>
  addLayersControl(
    baseGroups = c("Mapa base", "Imagem de satélite"),
    overlayGroups = c("Ocorrências", "AOO"),
    options = layersControlOptions(collapsed = FALSE)
  )

# 7. Visualização com apenas um ponto (usando o primeiro ponto)
pontos_sf_single <- pontos_sf[1, ] # Seleciona o primeiro ponto

# 8. Agora cria a grade para o ponto único
ponto_unico <- pontos_proj[1, ]  # Seleciona o primeiro ponto para o grid

# 9. Criar buffer circular de 2 km ao redor do ponto
buffer_size <- 1000 # Tamanho do buffer (1 km)
buffer_circular <- st_buffer(ponto_unico, dist = buffer_size)

# 10. Obter a bounding box do buffer circular
bbox <- st_bbox(buffer_circular)

# 11. Criar um quadrado a partir da bounding box
min_x <- bbox["xmin"]
max_x <- bbox["xmax"]
min_y <- bbox["ymin"]
max_y <- bbox["ymax"]

# Calcular o lado do quadrado como o maior comprimento (para garantir que seja um quadrado)
lado_quadrado <- max(max_x - min_x, max_y - min_y)

# Ajustar o quadrado para a coordenada mínima
quadrado <- st_sfc(st_polygon(list(matrix(c(min_x, min_y,
                                            min_x + lado_quadrado, min_y,
                                            min_x + lado_quadrado, min_y + lado_quadrado,
                                            min_x, min_y + lado_quadrado,
                                            min_x, min_y), ncol = 2, byrow = TRUE))), crs = st_crs(ponto_unico))

# 12. Visualizar o buffer quadrado ao redor do ponto único
leaflet() |>
  addTiles(group = "Mapa base") |>
  addProviderTiles(providers$Esri.WorldImagery, group = "Imagem de satélite") |>
  addPolygons(data = st_transform(quadrado, 4326), color = "red", weight = 2, fillOpacity = 0.3, group = "Buffer Quadrado (Ponto Único)") |>
  addCircleMarkers(data = pontos_sf_single, radius = 5, color = "blue", group = "Ocorrências") |>
  addLayersControl(
    baseGroups = c("Mapa base", "Imagem de satélite"),
    overlayGroups = c("Ocorrências", "Buffer Quadrado (Ponto Único)"),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  # Adicionar o plugin de medição de distância
  addMeasure(
    primaryLengthUnit = "kilometers",  # unidade de comprimento em quilômetros
    secondaryLengthUnit = "meters",    # unidade secundária em metros
    primaryAreaUnit = "sqmeters",      # unidade de área em metros quadrados
    secondaryAreaUnit = "hectares"     # unidade de área secundária em hectares
  )
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#| fig-cap: "Software Vortex 10, para simulação estocástica de processos de extinção."
#| column: margin
#| echo: false

knitr::include_graphics("vortex.jpg")
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
