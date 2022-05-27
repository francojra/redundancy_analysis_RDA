
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