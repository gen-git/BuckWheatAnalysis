
make_municipal_heat_map = function(shpfile, 
                                   df, #data frame that includes municipalities code and value
                                   graph_title,
                                   legend_title = NA, 
                                   x = NA, #municicolities code name in df
                                   y = "Y", #value (ex: yields)
                                   z = NA, #EPSG code of shp file (2016~: 6668(default), 1993~2015: 4612)
                                   col = NA,#col palette
                                   topleft_label = "",
                                   xlims = NA,
                                   ylims = NA,
                                   ymax  = NA,
                                   ymin  = NA,
                                   ssize = 12)
                                   
{
  if(is.na(xlims[1])) xlims = c(123,150)
  if(is.na(ylims[1])) ylims = c(24,46)
  
  #make data set
  if (is.na(x)) {
    x = "Municipalities_code"
  }
  if (is.na(as.character(legend_title))) {
    legend_title = ""
  }
  Japan_shp = shpfile
  crop_df = df
  datnew = data.frame(X=crop_df[x], Y=crop_df[y])
  names(datnew) = c("X","Y")
  datnew$X = formatC(datnew$X, width = 5, flag = "0")
  left_join(Japan_shp, datnew, by = c("N03_007" = "X")) -> crop_shp
  
  #make color colette
  if (is.na(col[1])) {
    col <- colorRampPalette(c("lightskyblue1","lightyellow1","tomato1"))(12)
  }
  
  #plot heat map
  if (is.na(z)) {
    z = 6668
  }
  vis <- ggplot2::ggplot(data = crop_shp,aes_string(fill=crop_shp$Y)) + 
    geom_sf(lwd = 0,colour = "transparent") 

  if(is.na(ymin) | is.na(ymax)){
    vis <- vis + scale_fill_gradientn(colors = col,name = legend_title)
  }
  if(!is.na(ymin) & !is.na(ymax)){
    vis <- vis + scale_fill_gradientn(colors = col,name = legend_title, limits = c(ymin, ymax))
  }
  
  vis <- vis + 
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          legend.title.align = 0.5,
          legend.position = c(1, 0.2), legend.justification = c(1,0.5),
          legend.background = element_rect(fill = "white", colour = "grey41"),
          text = element_text(size = ssize)
          ) +
    coord_sf(xlim = xlims, ylim = ylims,datum = sf::st_crs(z),) +
    labs(title = graph_title, x="", y="") + 
    annotate("text", x=123.7,   y=46, label=topleft_label)
  # return(vis)
  plot(vis)
}
