# Define DAG: methods -> score
library(dagitty)
library(ggdag)
library(ggplot2)

dag <- dagitty('
dag {
bb="-6.421,-7.624,6.826,6.874"
"Accuracy score" [outcome,pos="5.506,2.291"]
"Epidemic dynamics" [latent,pos="1.743,-0.912"]
"Model structure" [exposure,pos="-2.105,2.885"]
"Modeller strategy" [latent,pos="-2.133,-1.027"]
"Observed incidence" [adjusted,pos="3.234,0.948"]
"Single/multi-country" [pos="0.210,1.926"]
Country [adjusted,pos="-0.231,-0.950"]
Horizon [adjusted,pos="2.155,1.907"]
Model [adjusted,pos="1.658,3.000"]
Prediction [pos="3.418,3.058"]
Trend [adjusted,pos="3.262,-0.164"]
Variant [adjusted,pos="0.210,-1.794"]
"Epidemic dynamics" -> "Observed incidence"
"Epidemic dynamics" -> Trend
"Model structure" -> "Single/multi-country"
"Model structure" -> Model
"Modeller strategy" -> "Model structure"
"Modeller strategy" -> "Single/multi-country"
"Observed incidence" -> "Accuracy score"
"Observed incidence" <-> Trend
"Single/multi-country" -> Prediction
Country -> "Epidemic dynamics"
Country -> "Observed incidence"
Horizon -> Prediction
Model -> Prediction
Prediction -> "Accuracy score"
Variant -> "Epidemic dynamics"
}
')

tidy_ggdag <- tidy_dagitty(dag)

p_adj <- ggdag_adjustment_set(tidy_ggdag, node_size = 14) +
  theme(legend.position = "bottom")

p <- ggdag_status(dag, text = FALSE, use_labels = "name") +
  theme_dag_blank()
