

library(tidyverse)
library(data.table)
library(cowplot)
library(ggplot2)
library(plotly)

#*****************************************************************************************
#******IMPORTING AND EXTRACTING DATA USING FUNCTION ****************************************
#*****************************************************************************************
# #importing the function of the extraction script
# source("~/OneDrive/DRAWDOWN LABS/400_RUNNING PROJECTS/OWN DEVELOPMENTS/DATA BASE/DATA_EXTRACTOR.R")
# 
# #DEFINE THE LIST AND PATH OF WHERE THE STATIONS TO BE IMPORTED ARE STORED
# filename='Station_list.csv'
# mydir<-"~/OneDrive/DRAWDOWN LABS/400_RUNNING PROJECTS/OWN DEVELOPMENTS/DATA BASE"
# pathname<-paste(mydir,filename, sep = "/")
# 
# #Calling the function to extract
# result<-extract_data(pathname)
# 
# #DE-TIBBLE THE WHOLE STUFF
# data_sets<-result$RESULT[[1]]
# meta_sets<-result$RESULT[[2]]
# eto_sets<-result$RESULT[[3]]

#*****************************************************************************************
#******FUNCTION FOR DELTA T PLOTTING *****************************************************
#*****************************************************************************************

#CREATES DELTA T FROM A DF
delta_t<-function(Tscale, RHscale){
  #expanding the grid for deltaT chart
  df_deltaT <- expand.grid(T=Tscale, RH=RHscale)

  #calculating the DElta T from WetBulb Temperature
  #Formula:   #Tw = T * arctan[0.151977 * (rh% + 8.313659)^(1/2)] + arctan(T + rh%) - arctan(rh% - 1.676331) + 0.00391838 *(rh%)^(3/2) * arctan(0.023101 * rh%) - 4.686035
  df_deltaT$term1<- df_deltaT$T*atan(0.151977*(df_deltaT$RH+8.313659)^(1/2))
  df_deltaT$term2<- atan(df_deltaT$T+df_deltaT$RH)
  df_deltaT$term3<- (-atan(df_deltaT$RH-1.676331))
  df_deltaT$term4<- 0.00391838*df_deltaT$RH^(3/2)*atan(0.023101*df_deltaT$RH)
  df_deltaT$offset<- (-4.686035)
  df_deltaT$Tw <- df_deltaT$term1+df_deltaT$term2+df_deltaT$term3+df_deltaT$term4+df_deltaT$offset
  df_deltaT$DeltaT<-(df_deltaT$T-df_deltaT$Tw)
  
  df_deltaT
}

#CREATES DELTA T MATRIX FROM 2 VECTORS VIA OUTER PROUDCT
delta_t2<-function( RH, T){
  #calculating the DElta T from WetBulb Temperature
  #Formula:   #Tw = T * arctan[0.151977 * (rh% + 8.313659)^(1/2)] + arctan(T + rh%) - arctan(rh% - 1.676331) + 0.00391838 *(rh%)^(3/2) * arctan(0.023101 * rh%) - 4.686035
  term1<- T*atan(0.151977*(RH+8.313659)^(1/2))
  term2<- atan(T+RH)
  term3<- (-atan(RH-1.676331))
  term4<- 0.00391838*RH^(3/2)*atan(0.023101*RH)
  offset<- (-4.686035)
  Tw <- term1+term2+term3+term4+offset
  DeltaT<-T-Tw
  DeltaT
}

#FUNCTION WHICH CREATES PLOT WITH OVERLAYED LAYERS
create_heatmap_plot<-function(heatmap_df, plot_df){
  #creating a own color palette for heatmap
  mycolors<-c('#FF8B00', '#FFBC00','#4CB258','#4CB258', '#FFBC00', '#FFBC00','#FF8B00','#B80000','#B80000','#B80000','#B80000','#B80000','#B80000')
  ggplot() +
    geom_tile(data = heatmap_df, mapping = aes(x = T,y = RH,fill = DeltaT)) +
    geom_point(data = plot_df,  mapping=aes(x = T,y = RH), colour = '#44546A', size = 0.5)+
    geom_density_2d(data = plot_df,  mapping=aes(x = T,y = RH))+
    scale_fill_gradientn(colors = mycolors)+
    theme_minimal()+
    xlab(label = "Temperature")+
    ylab(label = "Relative Humidity")
}


#FUNCTION WHICH CREATES PLOT WITH OVERLAYED LAYERS
create_heatmap_plot_line<-function(heatmap_df, df){
  #creating a own color palette for heatmap
  mycolors<-c('#FF8B00', '#FFBC00','#4CB258','#4CB258', '#FFBC00', '#FFBC00','#FF8B00','#B80000','#B80000','#B80000','#B80000','#B80000','#B80000')
  ggplot() +
    geom_tile(data = heatmap_df, mapping = aes(x = T,y = RH,fill = DeltaT)) +
    geom_line(data = df,  mapping=aes(x = T,y = RH), colour = '#44546A', size = 0.5)+
    scale_fill_gradientn(colors = mycolors)+
    theme_minimal()+
    xlab(label = "Temperature")+
    ylab(label = "Relative Humidity")
}



#FUNCTION WHICH CREATES PLOT WITH OVERLAYED LAYERS
create_heatmap_plot4<-function(heatmap_df, df1, df2, df3, df4, cols){
  #creating a own color palette for heatmap
  
  cols<-c('#44546A', '#036bfc', '#df03fc', '#d2bce6')
  
  mycolors<-c('#FF8B00', '#FFBC00','#4CB258','#4CB258', '#FFBC00', '#FFBC00','#FF8B00','#B80000','#B80000','#B80000','#B80000','#B80000','#B80000')
  ggplot() +
    geom_tile(data = heatmap_df, mapping = aes(x = T,y = RH,fill = DeltaT)) +
    geom_point(data = df1,  mapping=aes(x = T,y = RH), colour = cols[1], size = 0.5)+
    geom_point(data = df2,  mapping=aes(x = T,y = RH), colour = cols[2], size = 0.5)+
    geom_point(data = df3,  mapping=aes(x = T,y = RH), colour = cols[3], size = 0.5)+
    geom_point(data = df4,  mapping=aes(x = T,y = RH), colour = cols[4], size = 0.5)+
    geom_density_2d(data = df1,  mapping=aes(x = T,y = RH), colour = cols[1])+
    geom_density_2d(data = df2,  mapping=aes(x = T,y = RH), colour = cols[2])+
    geom_density_2d(data = df3,  mapping=aes(x = T,y = RH), colour = cols[3])+
    geom_density_2d(data = df4,  mapping=aes(x = T,y = RH), colour = cols[4])+
    
    scale_fill_gradientn(colors = mycolors)+
    theme_minimal()+
    xlab(label = "Temperature")+
    ylab(label = "Relative Humidity")
}


#FUNCTION WHICH CREATES PURE DELTA T PLOT
create_heatmap<-function(heatmap_df){
  #creating a own color palette for heatmap
  mycolors<-c('#FF8B00', '#FFBC00','#4CB258','#4CB258', '#FFBC00', '#FFBC00','#FF8B00','#B80000','#B80000','#B80000','#B80000','#B80000','#B80000')
  ggplot() +
    geom_tile(data = heatmap_df, mapping = aes(x = T,y = RH,fill = DeltaT)) +
    scale_fill_gradientn(colors = mycolors)+
    theme_minimal()+
    xlab(label = "Temperature")+
    ylab(label = "Relative Humidity")
}

#FUNCTION CREATES A MATRIX GRID OF THE DELTA T VALUES ACCORDING TO THE DEFINED SCALES
create_DFMatrix<-function(Tscale, RHscale){
  deltaT_matrix<-outer(RHscale, Tscale, FUN = "delta_t2")
  rownames(deltaT_matrix)<-RHscale
  colnames(deltaT_matrix)<-Tscale
  deltaT_matrix
}

#FUNCTION DEFINES A COLOR SCALE ACCORDING TO DELTAT THRESHOLDS
def_colorscale<-function(deltaT_matrix){
  #Getting normalized color thresholds
  max_val<-max(deltaT_matrix)
  min_val<-min(deltaT_matrix)
  a<- (2/max_val)
  b<- (4.5/max_val)
  c<- (6.3/max_val)
  d<- (9/max_val)
  e<- (12/max_val)
  f<- (16/max_val)
  #mapping normalized thresholds to colors
  palette1 = list(c(0, a, b, c,d, e,f, 1), c("#FF8B00", "#FFBC00", "#4CB258", "#4CB258", "#FFBC00", "#FF8B00", "#B80000", "#B80000"))
  

  palette1
}

#CREATING THE PLOT WITH THE DENSITY OVERLAYED TO THE HEATMAP
create_3Dgraph<-function(deltaT_matrix, dens, palette1 ,palette2){
  #define layout options
  axx <- list(title = "AIR TEMPERATURE",showbackground=TRUE)
  axy <- list(title = "RELATIVE HUMIDITY",showbackground=TRUE)
  axz <- list(title = "FREQUENCY", showticklabels = FALSE, showbackground=FALSE)
  angle<-list(eye = list(x=0, y=-0.6, z=2))
  #define basic plot scale
  fig <-  plot_ly(x = dens$y, y = dens$x)
  #create density plot
  fig1<- fig %>% add_surface(z = (dens$z*10000000000+100),colorscale=palette2, opacity=0.7) #scale density to make heatmap flat
  fig1 <- fig1 %>% hide_colorbar()
  #create heatmap (note: mask as surface with veeeeery small relative height to create heatmap in 3D space)
  fig2<- fig%>% add_surface(z = (deltaT_matrix), colorscale=palette1)
  ##overlay 2 graphs via subplot define the layout options and return
  fig3<-subplot(fig1, fig2)
  fig3 <- fig3 %>% layout(scene = list(xaxis=axx,yaxis=axy,zaxis=axz, camera=angle))
  fig3
}


#CREATING AN ANIMATED GRAPH
day_animation<-function(df1, title){
  
  
  # df1<-day_case2 %>% mutate(MONTH = TIMESTAMP$mon, HOUR = TIMESTAMP$hour)%>%
  #   group_by(HOUR)%>%
  #   summarise(RH = mean(RH), T=mean(T))%>%
  #   select(T, RH, HOUR)
  # 
  # 
  # 
  # create the background matrix
  Tscale<-seq(0, 50, by=1)
  RHscale<-seq(0, 100, by=2)
  deltaT_matrix<-create_DFMatrix(Tscale, RHscale)
  palette1<-def_colorscale(deltaT_matrix)
  
  cls<-list()
  for (i in 1:length(palette1[[1]])){
    elem<-list(c(palette1[[1]][i], palette1[[2]][i]))
    cls<-append(cls, elem)
  }
  #plotting the DeltaT Heatmap

  
  df2 <- df1 %>% accumulate_by(~HOUR)
  
  # adding the animated trace
  graph4<- df2%>%plot_ly(
    x = ~T,
    y = ~RH,
    frame = ~frame,
    name = "DeltaT",
    type = 'scatter',
    mode = 'lines+markers',
    line = list(
      color = 'rgba(231, 99, 250, 0.5)',
      width = 15
    ),
    marker = list(
      color = 'rgba(231, 99, 250, 0.95)',
      size = 15

      ),
    
    showlegend = T
  ) 
  

  graph3 <- graph4%>%add_heatmap(
    x = colnames(deltaT_matrix),
    y = rownames(deltaT_matrix),
    z = deltaT_matrix,
    coloraxis = "coloraxis",

    type = "heatmap"
  ) %>%  animation_opts(
    easing = "linear",
    frame=500,
    transition = 0,
    redraw = T,
    mode = "immediate"
  )  %>%
    animation_slider(
      currentvalue = list(prefix = "Hora do dia ")
    )%>% layout(coloraxis=list(colorscale=cls),
                            title=title, xaxis = list(title= "TEMPERATURA"), yaxis = list(title= "HUMIDADE RELATIVA"))

  return(graph3)
}



accumulate_by <- function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
}
# 

# 
# 
# # 
# 
# fig <- fig1 %>%
#   plot_ly(
#     x = ~T, 
#     y = ~RH,
#     #split = ~city,
#     frame = ~frame, 
#     type = 'scatter',
#     mode = 'lines+markers', 
#     line = list(simplyfy = F)
#   )
# fig
# fig <- fig %>% layout(
#   xaxis = list(
#     title = "Date",
#     zeroline = F
#   ),
#   yaxis = list(
#     title = "Median",
#     zeroline = F
#   )
# ) 
# fig <- fig %>% animation_opts(
#   frame = 100, 
#   transition = 0, 
#   redraw = FALSE
# )
# fig <- fig %>% animation_slider(
#   hide = T
# )
# fig <- fig %>% animation_button(
#   x = 1, xanchor = "right", y = 0, yanchor = "bottom"
# )
# 
# fig









#*****************************************************************************************
#******HERE I AM DOIOING THE 2D PLOT*************************************************
#*****************************************************************************************

# #Initiating the delta T heatmap Grid
# #expanding the grid for the plot of T vs RH
# Tscale<-c(1:50)
# RHscale<-c(1:100)
# #calling the function which calculates the DeltaT
# heatmap_df<-delta_t(Tscale, RHscale)
# #creating the Heatmap plot
# create_heatmap(heatmap_df)
# 
# 
# #extracting data point 1 and storing in a Df with names like heatmap
# case1<-data_sets[[2]][[1]]
# #putting them in a df for plot and giving them the same names and the heatmap df columns
# plot_df1<-as.data.frame(cbind(case1$`HC_Air_temperature_[°C]`, case1$`HC_Relative_humidity_[%]`))
# colnames(plot_df1)<-c('T', 'RH')
# 
# #extracting data point 2 and storing in a Df with names like heatmap
# case2<-data_sets[[2]][[4]]
# plot_df2<-as.data.frame(cbind(case2$`Temperatura_do_Ar_[°C]`, case2$`Umidade_Relativa_[%]`))
# colnames(plot_df2)<-c('T', 'RH')
# 
# #creating the heatmap graph WITH DATA
# graph<-create_heatmap_plot(heatmap_df, plot_df1)
# graph


#*****************************************************************************************
#******HERE I AM DOIOING THE 3D PLOT*************************************************
#*****************************************************************************************

# #CREATING THE PURE HEATMAP
# #creating the matrix Grid for the Delta T Values
# Tscale<-seq(0.5, 50, by=0.5)
# RHscale<-seq(1, 100, by=1)
# deltaT_matrix<-create_DFMatrix(Tscale, RHscale)
# #calling function to create colorscale1
# palette1<-def_colorscale(deltaT_matrix)
# 
# 
# #CREATING THE DENSITY PLOT
# #extracting data point 1 and storing in a Df with names like heatmap
# case1<-data_sets[[2]][[1]]
# #putting them in a df for plot and giving them the same names and the heatmap df columns
# plot_df1<-as.data.frame(cbind(case1$`HC_Air_temperature_[°C]`, case1$`HC_Relative_humidity_[%]`))
# colnames(plot_df1)<-c('T', 'RH')
# #extracting data point 2 and storing in a Df with names like heatmap
# case2<-data_sets[[2]][[4]]
# plot_df2<-as.data.frame(cbind(case2$`Temperatura_do_Ar_[°C]`, case2$`Umidade_Relativa_[%]`))
# colnames(plot_df2)<-c('T', 'RH')
# #Creating the Kernel Density Estimation for frequency
# dens<-MASS::kde2d(x=plot_df1$RH, y=plot_df1$T, n=100, lims = c(0, 100, 0, 50))
# #define color palette for Density Graph
# palette2 = list(c(0, 0.1,1), c("#D6DCE4", "#8497B0", "#44546A"))
# 
# #OVERLAYING THE PLOTS
# #calling the 3D overlayed plot function
# create_3Dgraph(deltaT_matrix, dens, palette1 ,palette2)
#   
