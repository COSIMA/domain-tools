# Domain Tools

Code and tools to edit and manipulate ocean model grids and topographies.

Below is a list of included tools and short documentation for each.


## topogtools (Russ' Fortran tools)

```
usage: topogtools [--help] <command> [<args>]
```

Collection of tools to edit and manipulate ocean model topographies.
See `topogtools --help <command>` to read about a specific subcommand.

Available commands:
  * `gen_topo` - Generate a new topography file from a bathymetry
  * `deseas` - Remove enclosed seas
  * `min_max_depth` - Set minimum and maximum depth
  * `check_nonadvective` - Check for non-advective cells
  * `fix_nonadvective` - Fix non-advective cells
  * `mask` - Generate mask

### gen_topo

```
usage: topogtools gen_topo --input <input_file> --output <output_file>
                           --hgrid <grid> [<options>]
```

Generate a new topography from `<input_file>` on the tracer points of `<grid>` and
writes the result to `<output_file>`. Note that `<grid>` must be a super-grid

Options
  * `--tripolar`                  horizontal grid is a tripolar grid
  * `--longitude-offset <value>`  offset (in degrees) between the central longitude of the ocean horizontal grid and of the bathymetry grid (default '0.0')

### deseas

```
usage: topogtools deseas --input <input_file> --output <output_file>
```

Remove enclosed seas from <input_file> and writes the result to <output_file>.

### min_max_depth

```
usage: topogtools min_max_depth --input <input_file> --output <output_file>
                                --level <level> [--vgrid <vgrid>]
```

Set minimum depth to the depth at a specified level and set maximum depth to
deepest in `<vgrid>`. `<level>` is the minimum number of depth levels (e.g. 4).
Can produce non-advective cells.

Options
  * `--vgrid <vgrid>`  vertical grid (default 'ocean_vgrid.nc')

### check_nonadvective

```
usage: topogtools check_nonadvective --input <input_file>
                                     [--vgrid <vgrid> --potholes --coastal-cells]
```

Check for non-advective cells. There are two types of checks available: potholes
and non-advective coastal cells. Checking for non-advective coastal cells should
only be needed when using a B-grid.

Options
  * `--vgrid <vgrid>` vertical grid (default 'ocean_vgrid.nc')
  * `--potholes`      check for potholes
  * `--coastal-cells` check for non-advective coastal cells

### fix_nonadvective

```
usage: topogtools fix_nonadvective --input <input_file> --output <output_file>
                                   [--vgrid <vgrid> --potholes --coastal-cells]
```

Fix non-advective cells. There are two types of fixes available: potholes and
non-advective coastal cells. Fixes to non-advective coastal cells should only be
needed when using a B-grid.

Options
  * `--vgrid <vgrid>` vertical grid (default 'ocean_vgrid.nc')
  * `--potholes`      fix potholes
  * `--coastal-cells` fix non-advective coastal cells

### mask

```
usage: topogtools mask  --input <input_file> --output <output_file>
                        [--fraction <frac>]
```

Creates a land mask from a topography.

Options
  * `--fraction <frac>`  cells with a fraction of sea area smaller than `<frac>` will be set as land (default '0.0')


## float_vgrid

```
usage: float_vgrid [--vgrid <vgrid>]
```

Alter values in ocean vertical grid so they can be used with both single- and
double-precision topography file.

Options
  * `--vgrid <vgrid>`  vertical grid (default 'ocean_vgrid.nc')
