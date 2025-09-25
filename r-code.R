
getwd()
# Load necessary packages
library(dplyr)
library(ggplot2)
library(scales) 
library(plm)   # Panel data models
library(lmtest) # Model diagnostics
library(sandwich) # Robust standard errors
library(stargazer) # Model summary tables

##
# upload dataset
library(readr)
panel_data <- read_csv("panel_data.csv")
View(panel_data)

##
#converting into panel data
final_dataset1 <- panel_data %>%
  mutate(Country = as.factor(Country),
         Year = as.factor(Year))

final_dataset1 <- final_dataset1[, c("Country", "Year", "GDP", "FDI", "Inflation", "Exchange_rate", "Corruption_con", "Pol_sta", "Trade")]

View(final_dataset1)


###
sum(!is.na(final_dataset1)) # sum of the missing observations
nrow(final_dataset1) # no of observations = 12*23= 266
summary(final_dataset1$FDI)
ls() # to  check the loaded dataset
length(unique(final_dataset1$Country)) # count of unique non-duplicates eg countries (23-countries)


## Summary
discr <- summary(final_dataset1[, c("FDI", "GDP", "Inflation", "Exchange_rate", "Corruption_con", 

                                        "Trade","Pol_sta")])
print(discr)
View(discr)

### or summary descriptive
install.packages("psych")
library(psych)

vars <- c("FDI", "GDP", "Inflation", "Exchange_rate", "Corruption_con", "Trade", "Pol_sta")
summary_sta = describe(final_dataset1[, vars])

install.packages("stargazer")
library(stargazer)
stargazer(summary_sta, type = "latex", summary = FALSE) # produce latex tables

##
#Correlation matrix

cor_matrix1 <- cor(final_dataset1[, c("FDI", "GDP", "Inflation", "Exchange_rate", "Corruption_con", 
                                     "Pol_sta", "Trade")], use = "pairwise.complete.obs")


print(cor_matrix1)

##
# Heat map of correlation
install.packages("ggcorrplot")
library(ggcorrplot)

ggcorrplot(cor_matrix1, lab = TRUE, colors = c("red", "white", "blue"))

##
# Visualization of trends using point-range plot

## BOX plot of FDI by country
library(ggplot2)
library(scales)

ggplot(final_dataset1, aes(x = reorder(Country, FDI, median), y = FDI / 1e9, fill = Country)) +
  geom_boxplot(alpha = 0.6, outlier.size = 1.5, outlier.shape = 21) +  
  stat_summary(fun = mean, geom = "text", 
               aes(label = round(..y.., 2)),  # Displays mean in billions with 2 decimals
               vjust = -1, size = 4, color = "black", fontface = "bold") + 
  scale_y_continuous(labels = function(x) paste0(x, "B")) +  # Converts to billion scale
  labs(title = "FDI Boxplot by Country", 
       x = "Country", y = "FDI (Billion $ Scale)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  
    axis.text.y = element_text(size = 10), 
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none"  
  )

# Boxplot of Corruption Control/corruption perception index by Country

ggplot(final_dataset1, aes(x = Country, y = Corruption_con, fill = Country)) +
  geom_boxplot(alpha = 0.6) +
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 1)), vjust = -1, size = 4) +
  labs(title = "Boxplot of Corruption Perception Index by Country", x = "Country", y = "CPI Score") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  
    axis.text.y = element_text(size = 10), 
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "none"  
  )


# FDI MEAN ACROSS COUNTRIES

ggplot(final_dataset1, aes(x = Country, y = FDI)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  scale_y_continuous(
    labels = label_number(scale = 1e-9, suffix = "B", accuracy = 0.1),  # Formats as "XB" (billions)
    breaks = seq(-6e9, 3e9, by = 1.5e9)  # Sets breaks at -6B, -4.5B, ..., 3B
  ) +
  labs(
    title = "Heterogeneity in FDI Across Countries",
    subtitle = "Mean FDI with 95% Confidence Intervals",
    x = "",
    y = "FDI (Billions USD)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.major.y = element_line(color = "grey90")
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red")  # Highlights zero FDI



## Histogram of FDI
library(ggplot2)

ggplot(final_dataset1, aes(x = FDI)) + 
  geom_histogram(fill = "blue", color = "black", bins = 30) +
  labs(title = "Distribution of FDI", x = "FDI", y = "Frequency") +  # Corrected 'y' label
  theme_minimal()

## Histogram of Corruption control/ CPI scores
library(ggplot2)

ggplot(final_dataset1, aes(x = CPI_score)) + 
  geom_histogram(fill = "blue", color = "black", bins = 30) +
  labs(title = "Distribution of FDI", x = "FDI", y = "Frequency") +  # Corrected 'y' label
  theme_minimal()


##
# FDI trends across countries
ggplot(final_dataset1, aes(x = Year, y = FDI, group = Country, color = Country)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::dollar, breaks = scales::pretty_breaks()) +
  scale_x_discrete() +
  labs(title = "FDI Trends by Country", x = "Year", y = "FDI Inflows")


#
# Scatter plot of FDI vs Corruption_con

library(ggplot2)
library(scales)

ggplot(final_dataset1, aes(x = Corruption_con, y = FDI / 1e9, color = Country)) +
  geom_point(alpha = 0.7, size = 3) +  # Improves visibility
  scale_y_continuous(labels = function(x) paste0(x, "B")) +  # Converts to billion scale
  labs(title = "Scatter Plot of FDI vs Corruption Perception Index", 
       x = "Corruption Control Index", 
       y = "FDI (Billion Scale)") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 12),
    legend.position = "bottom"
  )


## checking for relationship between corruption control and FID
cor(final_dataset1$FDI, final_dataset1$Corruption_con, use = "complete.obs", method = "pearson")




##
# Ensure Year is numeric
final_dataset2 <- final_dataset1
final_dataset2$Year <- as.numeric(as.character(final_dataset2$Year))

sapply(final_dataset1, function(x) length(unique(x))) ## printing the n obseration by variable


##
# PANEL REGRESSION MODELS
# Normal model (Linear model)

# OLS MODEL
l_model <- lm(log (FDI)
 ~ log(GDP) + Inflation + Exchange_rate + Corruption_con +  Pol_sta + Trade + factor(Year), 
                    data = final_dataset1 )
summary(l_model)

## 
stargazer(l_model, type = "latex", summary = FALSE) # produce latex tables



###
# Checking for VIFs models
library(car)
vif(l_model)  # Drop variables with VIF > 5 # all < 5, thus No multicollinearity found



##
# Reset test for checking non-linearity of the OLS model
library(lmtest)
resettest(l_model, power=2:3, type="fitted") ## the model is non-linear



# Model Diagnostics
plot(l_model$residuals, ylab = "Residuals")  # Should be random noise
qqnorm(l_model$residuals)  # Check normality



# Panel regresions
#Pooled model -its assumes no heregeneity
pooled_model <- plm(log (FDI) ~ log (GDP) + Inflation + Exchange_rate + Corruption_con + 
                      Pol_sta + Trade + factor (Year), 
                    data = final_dataset1,index =c("Country", "Year"), model = "pooling")

summary(pooled_model)

# Fixed Effects Model -its accounts for country specific factor
fixed_model <- plm(log(FDI+1) ~ log(GDP) + Inflation + Exchange_rate + Corruption_con + 
                     Pol_sta + Trade + factor(Year) ,
                   data = final_dataset1,index =c("Country", "Year"),  model = "within")

summary(fixed_model)

#Random Effects Model -its accounts for variation across countries
random_model <- plm(log(FDI) ~ log(GDP) + Inflation + Exchange_rate + Corruption_con + 
                      Pol_sta + Trade + factor(Year),
                    data = final_dataset1, index = c("Country", "Year"), 
                    model = "random", random.method = "walhus")

summary(random_model)

##
# Hausman test

phtest(random_model, fixed_model) #Hausman test for RE vs FE, RE is preferred


library(car)

vif(random_model) # no multicollinearity in RE model



# Model Diagnostics
plot(random_model$residuals, ylab = "Residuals")  # Should be random noise
qqnorm(random_model$residuals)  # Check normality




# 2nd Model
##Test for Non-linearity assumption
# OLS MODEL
l_nonlin <- lm(log (FDI) ~ log(GDP) + Inflation + I(Inflation^2) + Exchange_rate  + Corruption_con + 
                 Pol_sta +  Trade , 
               data = final_dataset1) 
              
summary(l_nonlin)



# check for multicollinearity
vif(l_nonlin)  # Drop variables with VIF > 10



# Non-linear PE Model
pe_nonlin <- plm(log (FDI) ~ log(GDP) + Inflation + I(Corruption_con^2) + Exchange_rate  + Corruption_con + 
                   Pol_sta +  Trade , 
                 data = final_dataset1, model = "pooling", 
                           index = c("Country", "Year"))
summary(pe_nonlin)

# Non-linear FE Model

fe_nonlin <- plm(log (FDI) ~ log(GDP) + Inflation + I(Inflation^2) + Exchange_rate  + Corruption_con + 
                    Pol_sta +  Trade + factor (Year), 
                  data = final_dataset1, index = c("Country", "Year"), model = "within"))

summary(fe_nonlin)


# Non-Linear DE Model
re_nonlin <- plm(log (FDI) ~ log(GDP) + Inflation + I(Inflation^2) + Exchange_rate  + Corruption_con + 
                    Pol_sta +  Trade + factor(Year), 
                  data = final_dataset1,index = c("Country", "Year"), model = "random", random.method = "walhus")
                       
summary(re_nonlin)





##
# 3rd Model, Interaction terms
# OLS Linear Model with Interaction Terms


l_interact <- lm(log(FDI) ~ log(GDP) + Inflation + Exchange_rate +  Corruption_con +  
                   Pol_sta + Trade + factor(Year) +  # Interaction terms           
                   + Corruption_con * Pol_sta, 
                        data = final_dataset1)

# Model Summary
summary(l_interact)
## PE interaction model
pe_interact <-  plm(log(FDI) ~ log(GDP) + Inflation + Exchange_rate +  Corruption_con +  
                      Pol_sta + Trade + factor(Year) +  # Interaction terms 
                       log(GDP) * Inflation + log(GDP) * Exchange_rate +          
                      Corruption_con * Pol_sta + Trade * Exchange_rate, data = final_dataset1, 
                    index = c("Country", "Year"), model = "pooling")

# Model Summary
summary(pe_interact)

## FE Interaction term model

# Fixed Effects Model with Interaction Terms
fe_interact <-  plm(log(FDI) ~ log(GDP) + Inflation + Exchange_rate +  Corruption_con +  
                      Pol_sta + Trade + factor(Year) +  # Interaction terms 
                      log(GDP) * Inflation +  log(GDP) * Exchange_rate +          
                      Corruption_con * Pol_sta + Trade * Exchange_rate, data = final_dataset1, 
                      index = c("Country", "Year"), model = "within")
                                

# Model Summary
summary(fe_interact)

## RE Interaction term model

re_interact <- plm(log(FDI) ~ log(GDP) + Inflation + Exchange_rate +  Corruption_con +  
                     Pol_sta  + factor(Year) +  # Interaction terms 
                      I(Inflation ^2) +          
                      + Trade * Exchange_rate, data = final_dataset1,           
                   index = c("Country", "Year"), model = "random", random.method = "walhus")
            
# Model Summary
summary(re_interact)                            
                                  
                        
#check for multicollinearity

library(car)
vif(pe_interact) # there is multicollinearity
vif(re_interact) # there is multicollinearity


        
###
Thesis

#### thesis models
# Panel regresions
#Pooled model -its assumes no heregeneity
pooled_model <- plm(log (FDI) ~ log (GDP) + Inflation + Exchange_rate + Corruption_con + 
                      Pol_sta + Trade + factor (Year), 
                    data = final_dataset1,index =c("Country", "Year"), model = "pooling")

summary(pooled_model)

# Fixed Effects Model -its accounts for country specific factor
fixed_model <- plm(log(FDI+1) ~ log(GDP) + Inflation + Exchange_rate + Corruption_con + 
                     Pol_sta + Trade + factor(Year) ,
                   data = final_dataset1,index =c("Country", "Year"),  model = "within")

summary(fixed_model)

#Random Effects Model -its accounts for variation across countries
random_model <- plm(log(FDI) ~ log(GDP) + Inflation + Exchange_rate + Corruption_con + 
                      Pol_sta + Trade + factor(Year),
                    data = final_dataset1, index = c("Country", "Year"), 
                    model = "random", random.method = "walhus")

summary(random_model)

##
# Hausman test 

phtest(random_model, fixed_model) #Hausman test for RE vs FE, RE is preferred


library(car)

vif(random_model) # no multicollinearity in RE model



# Model Diagnostics
plot(random_model$residuals, ylab = "Residuals")  # Should be random noise
qqnorm(random_model$residuals)  # Check normality



# 4rd Model, Interaction terms
# OLS Linear Model with Interaction Terms


l_interact <- lm(log(FDI) ~ log(GDP) + Inflation + Exchange_rate +  Corruption_con +  
                   Pol_sta + Trade + factor(Year) +  # Interaction terms           
                   + Corruption_con * Pol_sta, 
                 data = final_dataset1)
summary(l_interact)


l_random <- plm(log(FDI) ~ log(GDP) + Inflation + Exchange_rate + Corruption_con + 
                      Pol_sta + Trade + factor(Year) + Corruption_con * Pol_sta,
                    data = final_dataset1, index = c("Country", "Year"), 
                    model = "random", random.method = "walhus")

summary(l_random)



# 5th model, simple Non-linear

pe_nonlin <- lm(log(FDI) ~ log(GDP) + Inflation + Exchange_rate +  Corruption_con +  
                   Pol_sta + Trade + factor(Year) +  # Interaction terms           
                   + I(Corruption_con^2), 
                 data = final_dataset1)

pe_nonlin1 <- plm(log (FDI) ~ log(GDP) + Inflation + I(Corruption_con^2) + Exchange_rate  + Corruption_con + 
                   Pol_sta +  Trade + I(Corruption_con^2), 
                 data = final_dataset1, model = "pooling", 
                 index = c("Country", "Year"))
summary(pe_nonlin)

r_nonlin <- plm(log(FDI) ~ log(GDP) + Inflation + Exchange_rate + Corruption_con + 
                  Pol_sta + Trade + factor(Year) + I(Corruption_con^2),
                data = final_dataset1, index = c("Country", "Year"), 
                model = "random", random.method = "walhus")

summary(r_nonlin)




#Final thesis models#
1. PE
#Pooled model -its assumes no heregeneity
pooled_model <- plm(log (FDI+1) ~ log (GDP) + Inflation + Exchange_rate + Corruption_con + 
                      Pol_sta + Trade + factor (Year), 
                    data = final_dataset1,index =c("Country", "Year"), model = "pooling")

summary(pooled_model)

2. RE
#Random Effects Model -its accounts for variation across countries
random_model <- plm(log(FDI+1) ~ log(GDP) + Inflation + Exchange_rate + Corruption_con + 
                      Pol_sta + Trade + factor(Year),
                    data = final_dataset1, index = c("Country", "Year"), 
                    model = "random", random.method = "walhus")

summary(random_model)

3. FE
# Fixed Effects Model -its accounts for country specific factor
fixed_model <- plm(log(FDI+1) ~ log(GDP) + Inflation + Exchange_rate + Corruption_con + 
                     Pol_sta + Trade + factor(Year) ,
                   data = final_dataset1,index =c("Country", "Year"),  model = "within")

summary(fixed_model)

# Step 3: Perform the Specification Tests
## robust test
# Hausman test 

library(plm)
library(lmtest)

# Test 1: Breusch-Pagan LM Test (RE vs. PE)
bp_test <- plmtest(pooled_model, type = "bp")
print(bp_test) # If p-value < 0.05, reject H0. This means Random Effects is preferred over Pooled OLS

#Test 2: Chow Test (F-test) for Fixed Effects (FE vs. PE)
chow_test <- pFtest(fixed_model, pooled_model)
print(chow_test) #cat("\nInterpretation: If p-value < 0.05, reject H0. This means Fixed Effects is preferred over Pooled OLS.\n")

#Test 3: Hausman Test (FE vs. RE)
hausman_test <- phtest(fixed_model, random_model)
print(hausman_test) # cat("\nInterpretation: If p-value < 0.05, reject H0. This means Fixed Effects is the preferred model. Otherwise, use Random Effects.\n")


library(car)

vif(random_model) # no multicollinearity in RE model

# Model Diagnostics
plot(random_model$residuals, ylab = "Residuals")  # Should be random noise
qqnorm(random_model$residuals)  # Check normality

4. Interaction term
# simple OLS Linear Model with Interaction Terms


l_interact <- plm(log(FDI) ~ log(GDP) + Inflation + Exchange_rate +  Corruption_con +  
                   Pol_sta + Trade + factor(Year) +  # Interaction terms           
                   + Corruption_con * Pol_sta, 
                 data = final_dataset1, index = c("Country", "Year"), 
                 model = "random", random.method = "walhus")
summary(l_interact)


5. Quadratic model

# simple OLS Linear Model with quadratic model
l_nonlin <- plm(log(FDI) ~ log(GDP) + Inflation + Exchange_rate +  Corruption_con +  
                  Pol_sta + Trade + factor(Year) +  # Interaction terms           
                  + I(Corruption_con^2), 
               data = final_dataset1, index = c("Country", "Year"), 
               model = "random", random.method = "walhus")

summary(l_nonlin)



%%%
library(plm)
library(lmtest)

%% Step 3: Perform the Specification Tests

# Test 1: Breusch-Pagan LM Test (RE vs. PE)
H0: Pooled OLS is adequate (no significant random effects)
H1: Random Effects is preferred

#cat("1. Breusch-Pagan LM Test for Random Effects vs. Pooled OLS\n")

bp_test <- plmtest(pooled_model, type = "bp")
print(bp_test) #cat("\nInterpretation: If p-value < 0.05, reject H0. This means Random Effects is preferred over Pooled OLS.\n")


#Test 2: Chow Test (F-test) for Fixed Effects (FE vs. PE)
H0: Pooled OLS is adequate (all country intercepts are equal)
H1: Fixed Effects is preferred

#cat("2. Chow Test (F-test) for Fixed Effects vs. Pooled OLS\n")

chow_test <- pFtest(fixed_effects_model, pooled_model)
print(chow_test) #cat("\nInterpretation: If p-value < 0.05, reject H0. This means Fixed Effects is preferred over Pooled OLS.\n")


# Test 3: Hausman Test (FE vs. RE)
H0: Random Effects is consistent and efficient (preferred model)
H1: Random Effects is inconsistent; Fixed Effects is preferred

cat("3. Hausman Test for Fixed Effects vs. Random Effects\n")

hausman_test <- phtest(fixed_effects_model, random_effects_model)
print(hausman_test) # cat("\nInterpretation: If p-value < 0.05, reject H0. This means Fixed Effects is the preferred model. Otherwise, use Random Effects.\n")
























