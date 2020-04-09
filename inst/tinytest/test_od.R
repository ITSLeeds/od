library(od)

# Placeholder with simple test
expect_equal(1 + 1, 2)

# Test that OD operations with NA coordinates generate warnings:
o = od_data_df
z = od_data_zones_min
od_to_sf(o, z)

o$geo_code2[1] = "444"
od_to_sf(o, z) # should error
sf::st_geometry(od_to_sf(o, z))

# formal test with tinytest
library(tinytest)
expect_error(od_to_sf(o, z))
o$geo_code1[1] = "444"
expect_error(od_to_sf(o, z))
