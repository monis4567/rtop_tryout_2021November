

# get background map as spatial polygon instead of sf
norw_map_sp <- ne_countries(country="norway",scale = 10, returnclass = "sp")

p02 <- 
  ggplot() +
  geom_polygon(data=norw_map_sp,aes(x = long, y = lat, group = group),color = "black", fill = "azure3") +
  geom_jitter(data = df03,
              aes(x = dlon, y = dlat, #,
                  color=gen_specnm2,
                  fill=SQmean,
                  shape=eval1),
              width = jitlvl, #0.07, jitter width
              height = jitlvl, #0.07, # jitter height
              size = 3) +
  #define shape of points
  scale_shape_manual(values=c(21,3)) +
  # define outline of each pch point
  scale_color_manual(values=c("purple","red")) +
  # split in mulitple plots per group
  facet_wrap(~gen_specnm2, nrow = 1, ncol=2) + #'facet_wrap' subsets by column value in dataframe
  #define colour gradient for fill
  scale_fill_gradientn(colors = alpha(c("white","red","black"),0.6),
                       guide = guide_colorbar(order = 1)) +
  #define limits of map
  ggplot2::coord_sf(xlim = c(10.15, 10.35) ,
                    ylim = c(59.065, 59.265),
                    #default_crs = sf::st_crs(4326),
                    expand = FALSE) +
  xlab("longitude") + ylab("latitude")

# see the plot
p02




