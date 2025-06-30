variables_group5 <-
  data.frame(
    variable = c(
      "farm_size",
      "dry_year_incidence",
      "dry_year_damage",
      "heavy_rainfall_incidence",
      "heavy_rainfall_damage",
      "drought_incidence",
      "drought_damage",
      "cocoa_pest_and_disease_incidence",
      "cocoa_pest_and_disease_damage",
      "labor_price",
      "labor_persons_full_time",
      "labor_persons_seasonal",
      "fertilizer_cost",
      "fertilizer_amount",
      "pesticide_cost",
      "pesticide_frequency",
      "family_labor",
      "cocoa_harvest",
      "cocoa_price_world_market",
      "cocoa_price_percent_farmgate",
      "pesticide_effect"
    ),
    lower = c(
      2,
      0.001,
      0.3,
      0.1,
      0.05,
      0.01,
      0.3,
      0.5,
      0.05,
      30000,
      0,
      1,
      500,
      0,
      23000,
      5,
      1,
      75,
      488.86,
      0.45,
      0
    ),
    median = NA,
    upper = c(
      6,
      0.05,
      0.6,
      0.4,
      0.4,
      0.1,
      0.8,
      0.9,
      0.8,
      100000,
      2,
      5,
      1000,
      1,
      35000,
      15,
      3,
      500,
      6082.97,
      1.1,
      0.028
    ),
    distribution = c(
      "norm",
      "tnorm_0_1",
      "norm",
      "norm",
      "norm",
      "norm",
      "norm",
      "norm",
      "norm",
      "norm",
      "norm",
      "norm",
      "norm",
      "norm",
      "norm",
      "norm",
      "norm",
      "norm",
      "norm",
      "norm",
      "norm"
    ),
    label = c(
      "Farm Size (ha)",
      "Dry Year Incidence (probability)",
      "Dry Year Damage (yield loss fraction)",
      "Heavy Rainfall Incidence (probability)",
      "Heavy Rainfall Damage (yield loss or multiplier)",
      "Drought Incidence (probability)",
      "Drought Damage (yield loss fraction)",
      "Cocoa Pest and Disease Incidence (probability)",
      "Cocoa Pest and Disease Damage (yield loss fraction)",
      "Labor Price (CFA/month/person)",
      "Full-time Labor (persons)",
      "Seasonal Labor (persons)",
      "Fertilizer Cost (CFA/kg)",
      "Fertilizer Amount (kg)",
      "Pesticide Cost (CFA)",
      "Pesticide Frequency (treatments/year)",
      "Family Labor Cost (CFA/year)",
      "Cocoa Harvest (kg)",
      "World Market Cocoa Price (USD/ton)",
      "Farmgate Price Share (fraction of world price)",
      "pesticide efficiency"
      ),
    Description = c(
      "Total cultivated area under cocoa production in hectares",
      "Likelihood of experiencing a dry year affecting cocoa yield",
      "Estimated fraction of yield lost due to dry years",
      "Probability of excessive rainfall during the season",
      "Loss or increased disease risk from heavy rain",
      "Chance of drought conditions negatively impacting cocoa",
      "Estimated yield reduction due to drought",
      "Frequency of pest and disease occurrence",
      "Yield loss caused by pests and diseases",
      "Monthly wage per worker in CFA",
      "Number of full-time workers employed year-round",
      "Number of workers hired during harvest season",
      "Price per kilogram of fertilizer in CFA",
      "Quantity of fertilizer applied (kg)",
      "Total cost of pesticide treatments based on farm size",
      "Number of pesticide applications per year",
      "Estimated value of unpaid family labor annually",
      "Total annual cocoa yield (based on farm size)",
      "Global price of cocoa in CFA per kg",
      "Portion of global price received by farmers",
      "pesticide application effect on yield"
      
    )
  )
write.csv(variables_group5, file = "C:/Users/franc/OneDrive/Dokumente/Uni Bonn/Decision analysis/R Project/Git_Backed/DecisionAnalysis/Variables_Group5.csv")
library(decisionSupport)
        
make_variables <- function(est,n=1)
{ x<-random(rho=est, n=n)
for(i in colnames(x)) assign(i,
                             as.numeric(x[1,i]),envir=.GlobalEnv)
}
make_variables(as.estimate(variables_group5))



model_function_group5 <- function(){
  n_years <- 30
  cocoa_harvest_30<-rep(0,n_years)
  cocoa_harvest_30<-vv(cocoa_harvest, var_CV = 0, n_years)
  
  cocoa_farm_harvest <- farm_size * cocoa_harvest_30 * chance_event(chance = dry_year_incidence, 
                                                                    value_if = dry_year_damage, 
                                                                    n = n_years)
  
  cocoa_farm_harvest<-farm_size*cocoa_harvest*  (1 - dry_year_incidence * dry_year_damage) *
    (1 - heavy_rainfall_incidence * heavy_rainfall_damage) *
    (1 - drought_incidence * drought_damage) *
    (1 - cocoa_pest_and_disease_incidence * cocoa_pest_and_disease_damage)*(1+pesticide_frequency*pesticide_effect)
  income<-cocoa_farm_harvest*cocoa_price_world_market*cocoa_price_percent_farmgate
  labor_cost  <- labor_persons_full_time*12+labor_persons_seasonal*5*labor_price+family_labor*labor_price
  pesticide_cost <-23000-30500*farm_size*pesticide_frequency
  overall_cost<-pesticide_cost+labor_cost+fertilizer_cost
  # Estimate the final results from the model
  final_result <- income - overall_cost
  # Generate the list of outputs from the Monte Carlo simulation
  return(list(final_result = final_result))}


library(decisionSupport)

example_mc_simulation <- mcSimulation(estimate = as.estimate(variables_group5),
                                      model_function = model_function_group5,
                                      numberOfModelRuns = 10000,
                                      functionSyntax = "plainNames")
plot_distributions(mcSimulation_object = example_mc_simulation,
                   vars = "final_result",
                   method = "boxplot_density",
                   old_names = "final_result",
                   new_names = "Outcome distribution for profits")
