library(readxl)

combo <- read_excel("data/combo.xlsx")

normalize <- function(x){
  round((x - min(x)) / (max(x) - min(x)), digits = 2)
}

# weight_negatively <- function(x){
  # x <- x * -1
# }

combo_norm <- combo %>%
  mutate_if(is.numeric, funs(normalize))

combo_norm

Score <- combo_norm %>%
  group_by(`Country Name`) %>%
  summarise(sum = sum(`Improved water source (% of population with access)`,
                      `Life expectancy at birth, total (years)`,
                      `Urban population growth (annual %)`,
                      `Adult Literacy Rate (% Ages 15 and older)`,
                      `Population with at least some secondary education (% ages 25 and older)`,
                      `Mean years of schooling (years)`,
                      `Inequality in education (%)`,
                      `GNI per capita (2011 PPP$)`,
                      `Press Freedom Score (0 worst - 100 best)`)) %>%
  as.matrix() %>%
  t()


Score <- Score[-1,]

pander::panderOptions('table.split.table', Inf)

emphasize.strong.cols(which(combo_norm$`Country Name` == "Syrian Arab Republic",
                            arr.ind = TRUE))

combo_norm_matr_trans <- combo_norm %>%
  as.matrix() %>%
  t() %>%
  na.omit() 

combo_matrix <- rbind(combo_norm_matr_trans, Score)

# emphasize.strong.rows(1)
emphasize.strong.rows(c(1, nrow(combo_matrix)))

pander(combo_matrix)
