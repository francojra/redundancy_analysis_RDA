
# Análise de Redundância RDA ---------------------------------------------------------------------------------------------------------------
# Autoria do script: Jeanne Franco ---------------------------------------------------------------------------------------------------------
# Data: 27/05/22 ---------------------------------------------------------------------------------------------------------------------------
# Referência: http://r.qcbs.ca/workshop10/book-en/redundancy-analysis.html -----------------------------------------------------------------

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

# Make sure the files are in your working directory!  If R
# cannot find the dataset, set your working directory with
# setwd() to the folder in which your data is stored (e.g.
# setwd('~/Desktop/workshop10'))

# Species community data frame (fish abundance)
spe <- read.csv("doubsspe.csv", row.names = 1)
spe <- spe[-8, ]  # Site number 8 contains no species, so we remove row 8 (site 8)
spe
# Be careful to only run this command line once as you are
# overwriting 'spe' each time!

# Environmental data frame: “DoubsEnv.csv”
env <- read.csv("doubsenv.csv", row.names = 1)
env <- env[-8, ]  # Remove corresponding abiotic data for site 8 (because removed from fish data). 
env
# Again, be careful to only run the last line once.

# Análise RDA ------------------------------------------------------------------------------------------------------------------------------

# Nós computamos a análise de RDA usando a função rda() do pacote vegan.

library(vegan)

# Step 1: Standardize and/or transform the data.

# Apply Hellinger transformation to correct for the double
# zero problem
spe.hel <- decostand(spe, method = "hellinger")
env.z <- decostand(env, method = "standardize")

# Iremos remover a variável das porque ela apresenta colinearidade dcom várias outras
# variáveis.

env.z <- subset(env.z, select = -das)

# Step 2: Run the RDA.

spe.rda <- rda(spe.hel ~ ., data = env.z)

# Step 3: Extract key results of the RDA.

summary(spe.rda)

# The first section of the summary contains 
# the pieces we need to verify the performance of our RDA.

## Partitioning of variance:
##               Inertia Proportion
## Total          0.5025     1.0000
## Constrained    0.3689     0.7341
## Unconstrained  0.1336     0.2659

# Constrained Proportion: variance of Y explained by X (73.41%)
# Unconstrained Proportion: unexplained variance in  Y (26.59%)

# How would you report these results? You could say: “The included environmental
# variables explain 73.41% of the variation in fish community composition across 
# sites.”

# Seleção de variáveis ---------------------------------------------------------------------------------------------------------------------

# Se quiser simplificar o modelo, nós podemos performar um stepwise. Essa seleção
# nos ajuda a selecionar variáveis estatisticamente significantes. Entretanto, 
# selecionar variáveis ecológicas é mais importante.

# Nós iremos fazer a seleção das nossas 11 variáveis ambientais. Para isso, nós
# usamos a função ordiR2step().

# Forward selection of variables:
fwd.sel <- ordiR2step(rda(spe.hel ~ 1, data = env.z), # lower model limit (simple!)
               scope = formula(spe.rda), # upper model limit (the "full" model)
               direction = "forward",
               R2scope = TRUE, # can't surpass the "full" model's R2
               pstep = 1000,
               trace = FALSE) # change to TRUE to see the selection process!

# Which variables are retained by the forward selection?

# Check the new model with forward-selected variables
fwd.sel$call

# Resultado:

## rda(formula = spe.hel ~ alt + oxy + dbo, data = env.z)

# What is the adjusted R2 of the RDA with the selected variables?

# Write our new model
spe.rda.signif <- rda(spe.hel ~ alt + oxy + dbo, data = env.z)
# check the adjusted R2 (corrected for the number of
# explanatory variables)
RsquareAdj(spe.rda.signif)

# Resultado:

## $r.squared
## [1] 0.5894243
## 
## $adj.r.squared
## [1] 0.5401552

# The explanatory variables (altitude, oxygen and biological oxygen demand) now 
# explain 59% of the variance in  Y (species abundances across sites, or community 
# composition). When we correct for the number of variables in X, the adjusted  
# R2 tells us that three selected variables explain 54% of the variance in species 
# abundances.

# Because the adjusted R2 is corrected for the number of explanatory variables, 
# it is comparable across models and datasets. For this reason, you should report 
# the adjusted R2 when writing up the result of an RDA for an article, or in a 
# study which compares the explanatory power of different models.

# Testando a significância -----------------------------------------------------------------------------------------------------------------

# The significance of your RDA can be tested using the function anova.cca().

anova.cca(spe.rda.signif, step = 1000)

# You can also test the significance of each variable with by = "term".

anova.cca(spe.rda.signif, step = 1000, by = "term")

# You can also test the significance of each canonical axis with by = "axis". 
# Recall that these axes represent the variation in explanatory variables in 
# fewer dimensions.

anova.cca(spe.rda.signif, step = 1000, by = "axis")

# Our full model is statistically significant (p = 0.001), and every variable 
# included in this model is significant as well (p = 0.001). Every canonical 
# axis resulting from the RDA is also statistically significant (p = 0.001).

# Gráfico RDA ------------------------------------------------------------------------------------------------------------------------------

# One of the most powerful aspects of RDA is the simultaneous visualization of 
# your response and explanatory variables (i.e. species and environmental variables).

# Assim como na PCA existem dois tipos de escalas:
# - Tipo 1: distância entre os objetos reflete as similaridades entre eles.
# - Tipo 2: ângulos entre as variáveis reflete as corelações entre elas.

# Type 1 scaling
ordiplot(spe.rda.signif, scaling = 1, type = "text")
# Type 2 scaling
ordiplot(spe.rda.signif, scaling = 2, type = "text")

# Type 1 scaling: shows similarities between objects in the response matrix.
# Sites (numbers) that are closer together have more similar communities.
# Species that are closer together occupy more sites in common.

# Type 2 scaling: shows the effects of explanatory variables.
# Longer arrows mean this variable strongly drives the variation 
# in the community matrix.
# Arrows pointing in opposite directions have a negative relationship.
# Arrows pointing in the same direction have a positive relationship.

# Customizando o gráfico RDA ---------------------------------------------------------------------------------------------------------------

# Both plot() and ordiplot() make quick and simple ordination plots, but you can 
# customize your plots by extracting scores with scores() and manually setting 
# the aesthetics of points(), text(), and arrows(). 

# Here is an example of a custom triplot. Feel free to play around with the colours 
# and other parameters to make it your own!

# Custom triplot code!

## extract % explained by the first 2 axes
perc <- round(100*(summary(spe.rda.signif)$cont$importance[2, 1:2]), 2)

## extract scores - these are coordinates in the RDA space
sc_si <- scores(spe.rda.signif, display = "sites", choices = c(1,2), scaling = 1)
sc_sp <- scores(spe.rda.signif, display = "species", choices = c(1,2), scaling = 1)
sc_bp <- scores(spe.rda.signif, display = "bp", choices = c(1, 2), scaling = 1)

## Custom triplot, step by step

# Set up a blank plot with scaling, axes, and labels
plot(spe.rda.signif,
     scaling = 1, # set scaling type 
     type = "none", # this excludes the plotting of any points from the results
     frame = FALSE,
     # set axis limits
     xlim = c(-1,1), 
     ylim = c(-1,1),
     # label the plot (title, and axes)
     main = "Triplot RDA - scaling 1",
     xlab = paste0("RDA1 (", perc[1], "%)"), 
     ylab = paste0("RDA2 (", perc[2], "%)") 
)
# add points for site scores
points(sc_si, 
       pch = 21, # set shape (here, circle with a fill colour)
       col = "black", # outline colour
       bg = "steelblue", # fill colour
       cex = 1.2) # size
# add points for species scores
points(sc_sp, 
       pch = 22, # set shape (here, square with a fill colour)
       col = "black",
       bg = "#f2bd33", 
       cex = 1.2)
# add text labels for species abbreviations
text(sc_sp + c(0.03, 0.09), # adjust text coordinates to avoid overlap with points 
     labels = rownames(sc_sp), 
     col = "grey40", 
     font = 2, # bold
     cex = 0.6)
# add arrows for effects of the expanatory variables
arrows(0,0, # start them from (0,0)
       sc_bp[,1], sc_bp[,2], # end them at the score value
       col = "red", 
       lwd = 3)
# add text labels for arrows
text(x = sc_bp[,1] - 0.1, # adjust text coordinate to avoid overlap with arrow tip
     y = sc_bp[,2] - 0.03, 
     labels = rownames(sc_bp), 
     col = "red", 
     cex = 1, 
     font = 2)
