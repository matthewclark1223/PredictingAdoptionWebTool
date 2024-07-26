library(tidyverse)

####Individual####
#Financial costs
#Environmental benefit
#Extension support
#Trialability
#benefits are compatible with needs
#Individuals are Empowered to make decisions
#Available financial resources

###Both###
#Adaptable
#Security of tenure
#Simple to understand
#Aligned with current practices
#Supported by global discourse
#Supported by national policies
#availability of local NGOs
#Immediacy of benefits
#Reputation advantage


###Social###
#Easily observable
#Social proximity
#Social connectivity
#Local champions 


# Create dataset
data <- data.frame(
  #individual=paste( "Trait", seq(1,20), sep=""),
  
  individual =c("Financial\nbenefit","Environmental\nbenefit","Extension\nsupport","Trialability","Compatible\nwith needs",
                "Empowerment","Economic\nwellbeing","Adaptability","Tenure","Simplicity","Aligned with\npractices",
                "Globally\nsupported","Nationally\nsupported","NGOs","Timing of\nbenefits","Reputational\nbenefit",
                "Observable","Proximity","Social\nconnectivity","Local\nchampions"),
  
  group=factor(c( rep('Individual\nuptake', 7), rep('Social\ntransmission', 4), rep('Both', 9))),
  value=sample( seq(1,4), 20, replace=T)
)

#data = data %>% arrange(group, value)

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 3
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))

# Get the name and the y position of each label
label_data <- data
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(Trait=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=value, fill=group)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 4, xend = start, yend = 4), colour = "#525252", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 3, xend = start, yend = 3), colour = "#525252", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 2, xend = start, yend = 2), colour = "#525252", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 1, xend = start, yend = 1), colour = "#525252", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data$id),4), y = c(1, 2, 3, 4), label = c("Poor", "Below average", "Above average", "Excellent") , color="#525252", size=3 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  viridis::scale_fill_viridis(discrete = TRUE)+
  ylim(-5,5) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=value+0.5, label=individual, hjust=hjust), color="#525252", fontface="bold",alpha=0.9, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = Trait, y = -0.75, label=group), hjust=c(1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)+
   theme(plot.background = element_rect(fill = 'white', colour = 'white'))

p

ggsave("Plot.png",p,dpi = 400)
