





hex_hectares <-  bps_hexs |>
  group_by(id) |>
  summarize(sum_has = sum(hectares))

print(mean(hex_hectares$sum_has))

# hexagons mean size = 8,333 has


ltas_hectares <-  bps_ltas |>
  group_by(id) |>
  summarize(sum_has = sum(hectares))

print(range(ltas_hectares$sum_has))

# LTAs mean size = 2, 702 has; standard de
