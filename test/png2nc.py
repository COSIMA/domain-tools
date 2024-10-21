#!/usr/bin/env python3
"""
Convert test_topo.png to test_topo.nc to use for testing deseas.
"""

from PIL import Image
import numpy as np
import xarray as xr

image = Image.open ("test_topo.png")
data = np.flipud(np.array(image)).astype("float")

coords = {"ny": range(0, data.shape[0]),
          "nx": range(0, data.shape[1])}
da = xr.DataArray(data, dims=[ k for k in coords ], coords=coords, name="depth")
ds = xr.Dataset(data_vars={"depth": da,
                           "sea_area_fraction": da, # dummy data
                           "geolon_t": da, # dummy data
                           "geolat_t": da # dummy data
                          })
ds.to_netcdf("test_topo.nc")