# Data visualisation example code ----------------------------------------------

# Written by Luke Arundel, November 2023 

# Example code that can be used to create engaging charts in R
# Theme can be adjusted according to individual brand guidelines
# Data used here from TASO's report 'Approaches to addressing the ethnicity degree award gap' 

# Necessary files to run the code ---------------------------------------------

# example_data_vis.csv
# logo_transp.png
# Fonts need to be loaded in

# Loading libraries and data ---------------------------------------------------

#setwd("C:/Users/lukearundel/Documents/Data visualisation")
# Uncomment above and set WD as appropriate

# Load libraries (install if needed)

library(tidyverse) 
library(extrafont) # for loading fonts
library(ggtext) # for formatting chart text

# Load data 

df <- read.csv("example_data_vis.csv")

# Loading in fonts 

# There are multiple ways to load your own fonts into R 
# This method loads in the Windows fonts 

# font_import() # Uncomment if not run before
# Importing fonts takes a few minutes, but only needs to be run once
loadfonts(device = "win") # May need to be changed on Apple devices
fonts()

# Loading in a logo / images 

png <- magick::image_read("logo_transp.png")
logo <- grid::rasterGrob(png, interpolate = TRUE)

# Creating a horizontal bar chart ----------------------------------------------

example_chart <- ggplot(df, aes(x = count, y = factor(category, levels = c("No", 
                                                                           "Yes - inadequately specified", 
                                                                           "Yes - explores the general approach", 
                                                                           "Yes - adequately specified (institutional level)", 
                                                                           "Yes - adequately specified (intervention level)")))) +
  # ^ sorting the bars in the chart in desired order
  geom_bar(stat = "identity", fill = "#3b66bc") + # Bar chart coloured blue
  theme_minimal() + # Setting the theme elements as minimal, adding elements we want back in 
  # Title, subtitle, axis titles and caption 
  labs(
    title = "Over 40 providers did not include a Theory of Change (ToC)\nin their Access and Participation Plan (APP)",
    subtitle = "Figure 3: The number of Higher Education Providers (HEPs) who included a ToC\nin their APP",
    caption = "Source: TASO (2023), Approaches to addressing the ethnicity degree awarding gap",
    x = NULL, # Not showing X or Y axis title for this example chart
    y = NULL 
  ) +
  # Theme elements
  theme(text = element_text(family = "Arial"), # Specifying font for chart
  ## Title and subtitle elements 
  plot.title.position = "plot", # Aligning title to entire plot (not just panel)
  plot.title = element_text(size = 16, face = "bold"), 
  plot.subtitle = element_text(size = 12), 
  ## Caption elements 
  plot.caption.position = "plot", # Aligning caption to entire plot
  plot.caption = element_text(hjust = 0, # Aligning caption to the left
                              size = 10, # Changing size
                              face = "italic"), # Italicising
  ## Gridlines 
  panel.grid.major.x = element_line(colour = "#E4E2D9"), # Including x major gridlines and setting their colour
  ## Other theme elements
  plot.background = element_rect(fill = "#EDEBE3"), # Changing background colour
  plot.margin = margin(0.25, 0.25, 0.25, 0.25, "in"), # Setting margin in inches
  axis.text.y = element_text(size = 11), # Setting x and y axis text size
  axis.text.x = element_text(size = 11)
  ) + 
  # Other helpful elements
  coord_cartesian(clip = "off") + # Helpful for positioning things outside of the plot
  annotation_custom(logo, ymin = -6.2, xmin = 32, xmax = 42) 
  # Adding logo with annotation_custom can be fiddly. You may need to play around
  # with x and y min/max to size and place your image as you want

# Saving the chart 

# Saving here as a png, with a specified width and height
ggsave("example_chart.png", example_chart, width = 180, height = 120, units = "mm")

# Adding extra touches - highlighting datapoint of interest --------------------

extra_chart <- ggplot(df, aes(x = count, y = factor(category, levels = c("No", 
                                                                     "Yes - inadequately specified", 
                                                                     "Yes - explores the general approach", 
                                                                     "Yes - adequately specified (institutional level)", 
                                                                     "Yes - adequately specified (intervention level)")))) +
  geom_bar(stat = "identity", fill = "#EDEBE3") + # Adding bars of same colour as background underneath so alpha can be used for colouring
  geom_bar(stat = "identity", fill = "#3b66bc", alpha = 0.4) + # Making the bars a paler blue to help highlight point of interest later on
  theme_minimal() + 
  # Title, subtitle, axis titles and caption 
  labs(
    title = 'Over 40 providers <span style="color:#3b66bc;">did not include a Theory of Change</span> (ToC)  \nin their Access and Participation Plan (APP)',
    # By using ggtext::element_markdown, we can use markdown stylings to colour specific words in the chart to highlight a point
    subtitle = "Figure 3: The number of Higher Education Providers (HEPs) who included a ToC\nin their APP",
    caption = "Source: TASO (2023), Approaches to addressing the ethnicity degree awarding gap",
    x = NULL, 
    y = NULL 
  ) +
  # Theme elements
  theme(text = element_text(family = "Arial"), 
        ## Title and subtitle elements 
        plot.title.position = "plot", 
        plot.title = ggtext::element_markdown(face = "bold", 
        # ^ Using ggtext::element_markdown so we can use markdown stylings to colour specific words
                                              size = 16), 
        plot.subtitle = element_text(size = 12), 
        ## Caption elements 
        plot.caption.position = "plot", 
        plot.caption = element_text(hjust = 0, 
                                    size = 10, 
                                    face = "italic"),
        ## Other theme elements
        plot.background = element_rect(fill = "#EDEBE3"), 
        plot.margin = margin(0.25, 0.25, 0.25, 0.25, "in"), 
        axis.text.x = element_blank(), # Removing x axis text as we are labelling bars directly 
        axis.text.y = element_text(size = 11),
  ) + 
  # Other helpful elements
  coord_cartesian(clip = "off") + 
  annotation_custom(logo, ymin = -5.5, xmin = 32, xmax = 42) 

# Highlighting "No" 
extra_chart <- extra_chart + 
  geom_bar(data = subset(df, category == "No"), fill = "#3b66bc", stat = "identity", show.legend = FALSE) +
  # Highlighting "No" by colouring it solid blue
  geom_text(aes(label = count, colour = category), vjust = 0.5, hjust = 1.5, size = 4, fontface = "bold", 
            data = subset(df, category == "No"), color = "white") +
  # Adding data labels on the bars 
  geom_text(aes(label = count, colour = category), vjust = 0.5, hjust = 1.5, size = 4, 
            data = subset(df, category != "No"), color = "black") 

ggsave("extra_chart.png", extra_chart, width = 180, height = 120, units = "mm")
