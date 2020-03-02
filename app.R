library(shiny)
library(glmtools)
library(ncdf4)
library(tidyverse)
library(lubridate)
library(ggplot2)
# library(colourschemes)
load("data/mendota.RData")

ui <- fluidPage(
    titlePanel("GLMviz"),
    'Visualization of the flexible Lagrangian layer setup of GLM (General Lake Model), by Tadhg Moore and Robert Ladwig',
    strong("Move the sliders to look at a different timestamp:"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("timeInput", "DateTime", min = 1, max = 10200, value = 3000, post = "", animate = animationOptions( interval = 500, loop = F))
        ),
        mainPanel(
            plotOutput("lakeplot")
        )
    )
)

server <- function(input, output) {
    output$lakeplot <- renderPlot({
        
        ix = input$timeInput
        met2 <- met[met$Date == time[ix],]
        met2$hour <- hour(met2$Date)
        colnames(met2)[4] <- 'temp'
        
        stamptime <- format(time[ix], format = '%Y-%m-%d %H:%M:%S')
        z <- sim_z[!is.na(max(sim_z[,ix], na.rm = TRUE)-sim_z[,ix]),ix]
        z[1] <- 0
        z <- -z
        z <- min(z) - z
        temp <- sim_temp[!is.na(sim_temp[,ix]),ix]
        
        if (sim_ice[ix] > 0){
            z <- z + sim_ice[ix]
            z <- c(z, 0)
            temp <- c(temp, -50)
        }
        
        rbPal <- colorRampPalette(c('blue','red'))
        col <- rbPal(10)[as.numeric(cut(temp,breaks = 10))]
        col[is.na(col)] <- "#FFFFFF"
        # col = rs(temp)
        dat <- data.frame('z' = z, 'temp' = temp, 'colfill' = col,
                          'x1' = rep(0,length(z)), 'x2' = rep(2,length(z)), 'x3' = approx(hyps$depth, hyps$area, xout = z)$y,
                          'y1' = c(0,z[-length(z)]), 'y2' = z)

        ggplot() +
            geom_point(data = met2, aes(x = (max(dat$x3, na.rm = TRUE)/2), y = 5, colour = ShortWave), size = 20)+
            geom_rect(data = dat, mapping = aes(xmin =x1,xmax =x3,ymin =y1, ymax =y2, fill = temp), color = 'black') +
            geom_rect(data = met2, mapping = aes(xmin =0,xmax =max(dat$x3, na.rm = TRUE),ymin =0, ymax =2, fill = temp), color = 'grey') +
            geom_segment(data = met2, aes(x =0, xend = ((WindSpeed/10)*max(dat$x3, na.rm = TRUE)), y = 1, yend = 1), size = 2,
                         arrow = arrow(length = unit(0.5, "cm")))+
            geom_hline(yintercept = 0, linetype = 'dashed', colour = 'black')+
            scale_fill_gradientn(colours = cols, limits = c(-10,35), name = 'Temperature (\u00B0C)')+
            scale_color_gradient2(limits = c(0,840), low = 'white',mid = 'white', high = 'red', midpoint = 100)+
            xlab('')+
            ylab('Depth (m)')+
            theme_classic(base_size = bsiz)+
            coord_cartesian(ylim = c(-25,6))+
            theme(
                # axis.text.x=element_blank(),
                panel.background = element_rect(fill = 'white')
            ) +
            ggtitle(stamptime)
        
        # nam = str_pad(ix, 3, pad = 0)
        
    })
}

shinyApp(ui = ui, server = server)