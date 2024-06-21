############ ############ ############ ############ ############ 
############ 1 to 1 plot for theoretical figure
pacman::p_load(dplyr, ggplot2, tidyverse, here)
source(here("Scripts/Functions/Results.R"))
data1 <- read.csv("output/checking Tbs/Ctenophorus isolepis/diffs_Ctenophorus_isolepisSiteR.csv") %>% 
  select(Genus.species, datetime2, BT, TC_interp, AT, TA_interp, diff_TC, diff_TA)
data2 <- read.csv("output/checking Tbs/Ctenophorus isolepis/diffs_Ctenophorus_isolepisSiteL.csv") %>% 
select(Genus.species, datetime2, BT, TC_interp, AT, TA_interp, diff_TC, diff_TA) 
Tb_Ta_data <-rbind(data1, data2)

# get just TA data and rename
Ta_data <- Tb_Ta_data %>% 
  select(Genus.species, datetime2, AT, TA_interp, diff_TA) %>% 
  rename(predicted = TA_interp, 
         observed = AT,
         difference = diff_TA) %>% 
  mutate(Test = "Ta")

# get just Tb data and rename
Tb_data <- Tb_Ta_data %>% 
  select(Genus.species, datetime2, BT, TC_interp, diff_TC) %>% 
  rename(predicted = TC_interp, 
         observed = BT,
         difference = diff_TC) %>% 
  mutate(Test = "Tb")
# final df for plotting both TA and TB
Tb_Ta_data_final <- rbind(Ta_data, Tb_data)


##########
### Stats for plots
# Tb data stats
obspred_Tb <- Tb_data %>% 
  select(predicted, observed) %>% 
filter(!is.na(predicted) & !is.na(predicted))

rmsd.Tb <- round(sqrt(mean(((obspred_Tb[, 1] - obspred_Tb[, 2]) ^ 2), 
                           na.rm = TRUE)), digits = 2)
bias.Tb <- round(mean((obspred_Tb[, 1] - obspred_Tb[, 2]), 
                      na.rm = TRUE), digits = 2)
# Ta data stats
obspred_Ta <- Ta_data %>% 
  select(predicted, observed) %>% 
  filter(!is.na(predicted) & !is.na(predicted))

rmsd.Ta <- round(sqrt(mean(((obspred_Ta[, 1] - obspred_Ta[, 2]) ^ 2), 
                           na.rm = TRUE)), digits = 2)
bias.Ta <- round(mean((obspred_Ta[, 1] - obspred_Ta[, 2]), 
                      na.rm = TRUE), digits = 2)




##########
### Figure with TA and TB
my_colors <- c("tomato1", "dodgerblue") 
Tb_Ta_data_final$Test <- factor(Tb_Ta_data_final$Test, levels = c("Tb", "Ta"))

ggplot(Tb_Ta_data_final, aes(x = observed, y = predicted, color = Test)) +
  geom_point(alpha = 0.3, size = 4.5) +
  scale_color_manual(values = my_colors, 
                     labels = c(expression(T[b]), expression(T[a])), 
                     breaks = c("Tb", "Ta")) +
  geom_abline(slope = 1, intercept = 0, color = "black") +
  scale_x_continuous(name = expression(paste("Predicted", " (", degree, "C)", sep = "")),
                     limits = c(12, 45), breaks = seq(15, 45, by = 5)) +
  scale_y_continuous(name = expression(paste("Observed", " (", degree, "C)", sep = "")),
                     limits = c(12, 45), breaks = seq(15, 45, by = 5)) +
  annotate("text", x = 25, y = 45, label = expression(italic("Ctenophorus (Amphibolourus) isolepis")),
           size = 12, fontface = "bold", color = "black") +
  annotate("text", x = 40.5, y = 27, label = "Body temperature",
           size = 10, color = "tomato1")+
  annotate("text", x = 40.5, y = 25, label = paste0("RMSD = ", rmsd.Tb),
           size = 10, color = "tomato1") +
  annotate("text", x = 39.9, y = 23, label = paste0("Bias = ", bias.Tb),
           size = 10, color = "tomato1") +
  annotate("text", x = 17.4, y = 34, label = "Air temperature",
           size = 10, color = "dodgerblue")+
  annotate("text", x = 17.4, y = 32, label = paste0("RMSD = ", rmsd.Ta),
           size = 10, color = "dodgerblue") +
  annotate("text", x = 16.6, y = 30, label = paste0("Bias = ", bias.Ta),
           size = 10, color = "dodgerblue") +
  theme_bw() +
  theme(axis.title = element_text(size = 50), 
        axis.text = element_text(size = 40), 
        legend.position = "none", # c(.93, .15)
        legend.key = element_blank(),
        legend.box.background = element_rect(color = "black", size = 1, linetype = "solid"),  # Add a box around the legend with a black border
        legend.title = element_text(size = 24, face = 'bold'),
        legend.text = element_text(size = 20))


# Ta data stats
obspred_Ta <- Tb_Ta_data %>% 
  select(TA_interp, AT) %>% 
  filter(!is.na(TA_interp) & !is.na(AT))

rmsd.Tb <- round(sqrt(mean(((obspred_Ta[, 1] - obspred_Ta[, 2]) ^ 2), na.rm = TRUE)), digits = 2)
bias.Tb <- round(mean((obspred_Ta[, 1] - obspred_Ta[, 2]), na.rm = TRUE), digits = 2)

# TA data
ggplot(Tb_Ta_data, aes(x = AT, y = TA_interp)) +
  geom_point(alpha = 0.3, color = "dodgerblue", size = 4.5) + # Add points
  geom_abline(slope = 1, intercept = 0, color = "black") + # Add a 1:1 diagonal line
  scale_x_continuous(name = expression(paste("Observed ", T[a], " (", degree, "C)", sep = "")), 
                     limits = c(10, 42), breaks = seq(10, 40, by = 5)) + # Customize the x-axis with subscript
  scale_y_continuous(name = expression(paste("Predicted ", T[a], " (", degree, "C)", sep = "")), 
                     limits = c(10, 42), breaks = seq(10, 40, by = 5)) + # Customize the y-axis with subscript
  annotate("text", x = 23, y = 41, 
           label = expression(italic("Ctenophorus isolepis")), 
           size = 12, fontface = "bold")+ #
  annotate("text", x =15.6, y = 36.8, label = paste0("RMSD = ", rmsd.Tb), 
           size = 12)+
  annotate("text", x =15, y = 34.5, label = paste0("Bias = ", bias.Tb), 
           size = 12)+
  theme_bw() + # Use a minimal theme
  theme(axis.title = element_text(size = 50), 
        axis.text = element_text(size = 40)) # Set axis title font size to 20





#########
#Grabbing plotting data from one day on Tb simulation
data2 <- data2 %>%
  mutate(date = format(as.Date(datetime2, format = "%d/%m/%Y %H:%M"), "%d/%m/%Y"))


summary <- data2 %>% 
  select(date, diff_TC, diff_TA) %>% 
  group_by(date) %>% 
  summarise(mean = mean(abs(diff_TC) + abs(diff_TA)),
            TC_mean = mean(abs(diff_TC)),
            n = n())


###########
# GRAB plotting data from one day on Tb simulation "Ctenophorus_isolepis_SiteL"
# 1978-11-28
# 1979-02-08 AWESOME EXAMPLE OF BEING TOO HOT and when they found animals
environ <- environ %>% filter(date == "1978-11-28")
old_par <- par()
par(mar = c(5, 6, 2, 5) + 0.1) # Increase the right margin
plot(environ$dates, environ$TC, type = 'p', pch = 16, 
     cex = ((environ$ACT + 1)/3)*1.7, col = 'grey', ylim = c(0, 50), 
     ylab = 'Body temperature (°C)', xlab = "Time of day", 
     cex.axis = 2.3, cex.lab = 2.8)

# Scale SHADE values to fit within half of the primary Y-axis range
# Assuming SHADE values are currently scaled 0-100, adjust if necessary
scaled_shade <- (environ$SHADE / max(environ$SHADE)) * 23.5
# night time values
scaled_shade_2 <- c(23.50, 23.50, 23.50, 23.50, 23.50, 23.50,  
                    0.00,  0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00,
                    0.00, 0.00,  0.00, 23.50, 23.50, 23.50, 23.50, 23.50)

# Add scaled SHADE values as horizontal lines
segments(x0 = environ$dates, y0 = 0, 
         x1 = environ$dates, 
         y1 = scaled_shade, col = 'darkgreen', lwd = 3)

segments(x0 = environ$dates, y0 = 0, 
         x1 = environ$dates, 
         y1 = scaled_shade_2, col = 'grey60', lwd = 3)

# Re-add the temperature points to ensure they are not covered by the shade lines
lines(environ$dates, environ$TC, col = 'black')
points(environ$dates, environ$TC, type = 'p', pch = 16,
       cex =  ((environ$ACT + 1)/3)*1.7, col = 'black')

# Add BT data points
points(merged$datetime2, merged$BT, pch = 16, cex = 2, col = 2)

# Add horizontal lines for T_pref, T_F_max, and T_F_min
abline(h = T_pref, col = 'orange', lty = 2)
abline(h = T_F_max, col = 'red', lty = 2)
abline(h = T_F_min, col = 'blue', lty = 2)
tick_positions <- seq(0, 23.5, length.out = 6) # Adjust 'length.out' based on the desired number of ticks
tick_labels <- seq(0, 100, by = 20) # Assuming the original shade scale is 0-100

# Add the secondary axis on the right
axis(side = 4, at = tick_positions, 
     labels = tick_labels, las = 1, col.ticks = 'darkgreen',
     col.axis = "darkgreen", cex.axis = 2.3)

# Add a label for the secondary axis
legend("topleft", legend = c("Predicted", "Observed"), 
       pch = c(16, 16), col = c("black", "red"), cex = 2,
       bg = "transparent", bty = "n")

par(old_par)

