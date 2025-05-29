## Survey design for svy estiamtes
svy_design <- survey::svydesign(
  ids = ~1, # Cluster ID; none
  data = data,
  weights = ~WEIGHT_EN
)
