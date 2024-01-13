libs<-c(
  "terra","giscoR","sf","tidyverse","ggtern","elevatr",
  "png","rayshader","magick"
)
installed_libraries<-libs%in% rownames(
  installed.packages()
)
if(any(installed_libraries==F)){
  install.packages(
    libs[!installed_libraries]
  )
}
invisible(
  lapply(
    libs,library,character.only=T
)
)
country_sf<-giscoR::gisco_get_countries(
  country = "LK",
  resolution = "1"
)
urls<-c(
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2021/44P_20210101-20220101.tif",
  "https://lulctimeseries.blob.core.windows.net/lulctimeseriesv003/lc2021/44N_20210101-20220101.tif"
)
  
for (url in urls) {
  filename <- basename(url)
  if (!file.exists(filename)) {
    download.file(url = url, destfile = filename, mode = "wb")
  } else {
    cat(sprintf("File '%s' already exists. Skipping download.\n", filename))
  }
}
raster_files<-list.files(
  path=getwd(),
  pattern="tif",
  full.name=T
)
crs<-"EPSG:4326"




for(raster in raster_files){
  rasters<-terra::rast(raster)
  
  country<-country_sf|>
    sf::st_transform(
      crs=terra::crs(
        rasters
      )
    )
  land_cover<-terra::crop(
    rasters,
    terra::vect(
      country
    ),
    snap="in",
    mask=T
  )|>
    terra::aggregate(
      fact=5,
      fun="modal"
    )|>
    terra::project(crs)
  terra::writeRaster(
    land_cover,
    paste0(
      raster,
      "_sri_lanka",
      ".tif"
      
    )
    
  )
}
r_list<-list.files(
  path=getwd(),
  pattern="_sri_lanka",
  full.names=T
)

land_cover_vrt<-terra::vrt(
  r_list,
  "sri_land_cover_vrt.vrt",
  overwrite=T
)
print(land_cover_vrt)


ras<-terra::rast(
  raster_files[[1]]
)
raster_color_table<- do.call(
  data.frame,
  terra::coltab(ras)
  
)
head(raster_color_table)

hex_code<-ggtern::rgb2hex(
  r=raster_color_table[,2],
  g=raster_color_table[,3],
  b=raster_color_table[,4]
)
print(hex_code)

c("#000000" ,"#419bdf" ,"#397d49" ,"#000000", "#7a87c6", "#e49635" ,"#000000", "#c4281b", "#a59b8f", "#a8ebff", "#616161",
   "#e3e2c3")
cols<-hex_code[c(2:3,5:6,8:12)]

from<-c(1:2,4:5,7:11)

to<-t(col2rgb(cols))

land_cover_vrt<-na.omit(land_cover_vrt)
land_cover_srilanka<-terra::subst(
  land_cover_vrt,
  from=from,
  to=to,
  names=cols
)
terra::plotRGB(land_cover_srilanka)

elev<-elevatr::get_elev_raster(
  locations = country_sf,
  z=9,clip="locations"
  
)


land_cover_srilanka_resampled<-terra::resample(
  x=land_cover_srilanka,
  y=terra::rast(elev),
  method="near"
)

terra::plotRGB(land_cover_srilanka_resampled)

img_file<-"land_cover_srilanka.png"

terra::writeRaster(
  land_cover_srilanka_resampled,
  img_file,
  overwrite=T,
  NAflag=255
)
img<-png::readPNG(img_file)

elmat<-rayshader::raster_to_matrix(
  elev
)
h<-nrow(elev)
w<-ncol(elev)


elmat|>
  rayshader::height_shade(
    texture=colorRampPalette(
      cols[9]
    )(256)
  )|>
  rayshader::add_overlay(
    img,
    alphalayer = 1
  )|>
  rayshader::plot_3d(
    elmat,
    zscale = 12,
    solid = F,
    shadow=T,
    shadow_darkness = 1,
    background = "white",
    windowsize = c(
      w/5,h/5
    ),
    zoom = .5,
    phi=85,
    theta = 0
  )
rayshader::render_camera(
  zoom = .58
)

filename<-"3d_land_cover_srilanka.png"

rayshader::render_highquality(
  filename = filename,
  preview=T,
  light = F,
  environment_light="C:/Users/sarat/Desktop/Materials/r codes/sl/3d_landcover/air_museum_playground_4k.hdr",
  intensity_env=1,
  rotate_env=90,
  parallel=T,
  width = w*1.5,
  height = h*1.5
)
c("#000000" ,"#419bdf" ,"#397d49" ,"#000000", "#7a87c6", "#e49635" ,"#000000", "#c4281b", "#a59b8f", "#a8ebff", "#616161",
  "#e3e2c3")

legend_name<-"land_cover_legend_last.png"
png(legend_name)
par(family="mono")

plot(
  NULL,
  xaxt="n",
  yaxt="n",
  bty="n",
  ylab="",
  xlab="",
  xlim=0:1,
  ylim=0:1,
  xaxs="i",
  yaxs="i"
)
legend(
  "center",
  legend=c(
    "Water",
    "Trees",
    "Crops",
    "Built area",
    "Rangeland"
  ),
  pch=15,
  cex=3,
  pt.cex = 2,
  bty = "n",
  col = c(cols[c(1:2,4:5,9)]),
  fill = c(cols[c(1:2,4:5,9)]),
  border = "grey20"
)
dev.off()

lc_img<-magick::image_read(
  filename
)
my_legend<-magick::image_read(
  legend_name
)
my_legend_scaled<-magick::image_scale(
  magick::image_background(
    my_legend,"none"
  ),2000
)

p<-magick::image_composite(
  magick::image_scale(
    lc_img,"x9000"
  ),
  my_legend_scaled,
  gravity = "northeast",
  offset = "+50+100"
  
)
magick::image_write(
  p,"3d_srilanka_land_cover_legend_last.png"
)
















