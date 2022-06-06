# Domain Tools

Code and tools to edit and manipulate ocean model grids and topographies.

Below is a list of included tools and short documentation for each.

## Russ' Fortran tools

### gen_topo
Generate a new topography file `topog_new.nc` from GEBCO bathymetry.
Takes no arguments but requires `mosaic.nc`, `ocean_mosaic.nc`, `ocean_hgrid.nc` and `gebco_2014_rot.nc` to be present.

### deseas
Remove enclosed seas from `topog.nc` file.
Usage:
```bash
./deseas topog_in.nc topog_out.nc
```

### min_max_depth
Set minimum depth to the depth at a specified level (same as `min_depth` above), and also set maximum depth to the deepest in `ocean_vgrid.nc`.
Usage:
```bash
./min_max_depth topog_in.nc topog_out.nc level
```
where *level* is the minimum number of depth levels (e.g. 4).
Requires `ocean_vgrid.nc` to be present.
Can produce non-advective cells.

### fix_nonadvective_mosaic

Fix cells that are non-advective on a B grid.
Usage:
```bash
./fix_nonadvective_mosaic topog_in.nc topog_out.nc
```
Requires `ocean_vgrid.nc` to be present.
