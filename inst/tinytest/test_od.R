library(od)

# Placeholder with simple test
expect_equal(1 + 1, 2)

# Test that OD operations with NA coordinates generate warnings:
o = od_data_df
z = od_data_zones_min
expect_true(is(od_to_sf(o, z), "sf"))

o$geo_code1[1] = "444"
expect_error(od_to_sf(o, z))
o = od_data_df
o$geo_code2[1] = "444"
expect_error(od_to_sf(o, z))

o = od_data_df
# test crss
expect_true({
  z_projected = sf::st_transform(z, 27700)
  is((l = od_to_sfc(o, z_projected)), "sfc")
})
expect_true(length(l) == nrow(x))
