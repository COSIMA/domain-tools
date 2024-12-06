[![compilation](https://github.com/COSIMA/domain-tools/actions/workflows/compilation.yml/badge.svg)](https://github.com/COSIMA/domain-tools/actions/workflows/compilation.yml)

# Domain Tools

Code and tools to edit and manipulate ocean model grids and topographies.

Below is a list of included tools and short documentation for each.

**Note:** these tools support two types of vertical grids: MOM5 grids (with
$2n+1$ values for an $n$-level model) and MOM6 grids (with $n+1$ values). It is
important to select the correct type or the tools will produce incorrect
results.

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
  * `fill_fraction` - Set cells with unsufficient ocean fraction to land
  * `check_nonadvective` - Check for non-advective cells
  * `fix_nonadvective` - Fix non-advective cells
  * `mask` - Generate mask

All commands other than `gen_topo` will add the following attributes to the
`depth` variable in `<output_file>`. Except where explained below, these
attributes will be copied from `<input_file>` if present, or otherwise set to
the default values in this table.

| Attribute        | Default  | Description |
|---|:-:|---|
| `grid_type`      | 'B' | Arakawa grid type (B or C); determines advective connectivity between cells when counting seas in `deseas`, `fill_fraction`, `check_nonadvective` and `fix_nonadvective`  |
| `lakes_removed`  | 'no ' | Whether all isolated water bodies have been replaced by land |
| `nonadvective_cells_removed` | 'yes' | Whether `fix_nonadvective` has been applied (BUG: always 'yes') |

### gen_topo

```
usage: topogtools gen_topo --input <input_file> --output <output_file>
                           --hgrid <grid> [<options>]
```

Generate a new topography from `<input_file>` on the tracer points of `<grid>` and
writes the result to `<output_file>`. Note that `<grid>` must be a super-grid.

Options
  * `--tripolar`                  set this option if the horizontal grid is a tripolar grid
  * `--longitude-offset <value>`  offset (in degrees) between the central longitude of the ocean horizontal grid and of the bathymetry grid (default '0.0')

### deseas

```
usage: topogtools deseas --input <input_file> --output <output_file> [--grid_type <type>]
```

Remove enclosed seas from `<input_file>` and write the result to
`<output_file>`, using advective connectivity rules set by `--grid_type` (B or
C); if `--grid_type` is not specified, the `grid_type` attribute of `depth` in
`<input_file>` is used, defaulting to B if that attribute is absent.

Sets the `lakes_removed` attribute in `<output_file>` to 'yes'. If `--grid_type`
is specified, this  sets the `grid_type` attribute in `<output_file>`.

Also creates a `sea_num.nc` file showing how the seas are numbered.

Options
  * `--grid_type <type>` Arakawa type of horizontal grid (B or C; default is B)

### min_max_depth

```
usage: topogtools min_max_depth --input <input_file> --output <output_file>
                                --level <level>
                                [--vgrid <vgrid> --vgrid_type <type>]
```

Set minimum depth to the depth at level `<level>` and set maximum depth to
deepest in `<vgrid>`. `<level>` is the minimum number of depth levels (e.g. 4).
These values are recorded in the `minimum_levels`, `minimum_depth` and
`maximum_depth` attributes of `depth` in `<output_file>`.

Can produce non-advective cells.

Options
  * `--vgrid <vgrid>`      vertical grid (default 'ocean_vgrid.nc')
  * `--vgrid_type <type>`  can be mom5 or mom6 (default 'mom5')

### fill_fraction

```
usage: topogtools fill_fraction --input <input_file> --output <output_file>
                                --fraction <frac>
```

Cells with a fraction of sea area smaller than `<frac>` will have their depth
set to zero.

Can produce non-advective cells.

Can also produce new isolated seas - if this is the case, a warning is given and
the `lakes_removed` attribute of `depth` is set to 'no '.

### check_nonadvective

```
usage: topogtools check_nonadvective --input <input_file>
                                      [--vgrid <vgrid> --vgrid_type <type>
                                       --potholes --coastal-cells]
```

Check topography for non-advective cells. There are two types of checks
available: potholes and non-advective coastal cells. B-grid connectivity rules
are assumed. Aborts if `grid_type` attribute of `depth` in `<input_file>` is
present and not 'B'.

Options
  * `--vgrid <vgrid>`      vertical grid (default 'ocean_vgrid.nc')
  * `--vgrid_type <type>`  can be mom5 or mom6 (default 'mom5')
  * `--potholes`           check for potholes
  * `--coastal-cells`      check for non-advective coastal cells

### fix_nonadvective

```
usage: topogtools fix_nonadvective --input <input_file> --output <output_file>
                                   [--vgrid <vgrid>  --vgrid_type <type>
                                    --potholes --coastal-cells]
```

Fix non-advective cells. There are two types of checks available: potholes and
non-advective coastal cells. If either is used, the `nonadvective_cells_removed`
attribute of `depth` is set to 'yes'. B-grid connectivity rules are assumed.
Aborts if `grid_type` attribute of `depth` in `<input_file>` is present and not 'B'.

Can produce new isolated seas. If this is the case, a warning is given and the
`lakes_removed` attribute of `depth` is set to 'no '.

Options
  * `--vgrid <vgrid>`      vertical grid (default 'ocean_vgrid.nc')
  * `--vgrid_type <type>`  can be mom5 or mom6 (default 'mom5')
  * `--potholes`           fix potholes
  * `--coastal-cells`      fix non-advective coastal cells

### mask

```
usage: topogtools mask  --input <input_file> --output <output_file>
```

Creates a land mask from a topography.

## float_vgrid

```
usage: float_vgrid [--vgrid <vgrid>]
```

Alter values in ocean vertical grid so they can be used with both single- and
double-precision topography file.

Options
  * `--vgrid <vgrid>`  vertical grid (default 'ocean_vgrid.nc')

## min_dy

```
usage: topogtools min_dy --input <input_file> --output <output_file> --cutoff <cutoff_value>
                        [--hgrid <hgrid_file>]
```

Convert ocean cells into land if their y size is smaller than `<cutoff_value>`, expressed in the same units as `dy` in `<hgrid_file>` (typically metres).

Options
  * `--hgrid <hgrid_file>`  horizontal supergrid file (default 'ocean_hgrid.nc')

## test/png2nc.py

```
usage: png2nc.py
```

Converts `test_topo.png` to `test_topo.nc` for use as a test input file for `topogtools deseas`.


## editTopo.py

`editTopo.py` provides a GUI for hand-editing `topog.nc` files, recording every
change, and applying these changes to other files. This was initially copied from 
[here](https://github.com/COSIMA/topogtools/blob/6b4983127aa18dfdf1b62b2e18b581d82d4a64d4/editTopo.py),
which is in turn an updated version of Alistair Adcroft's `editTopo.py` from
[here](https://github.com/aekiss/MOM6-examples/blob/1c3dc5216139f84b20ce3a5d8ea758bdc7912e8e/ice_ocean_SIS2/OM4_025/preprocessing/editTopo.py)
and is under a [LGPLv3
license](https://github.com/NOAA-GFDL/MOM6-examples/blob/dev/gfdl/LICENSE.md).
For usage details, do `./editTopo.py -h`. 

## General Instructions

### Prerequisites

To compile these tools one needs:
- Fortran compiler
- netCDF-Fortran
- CMake

### Step-by-step instructions

`domain-tools` uses CMake for building and installation. Therefore the procedure
to build the sources and install the executables is fairly standard:
```console
git clone https://github.com/COSIMA/domain-tools.git
cd domain-tools
cmake -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build
cmake --install build --prefix=<directory where to install the tools>
```

## Gadi

An installation of these tools is available on Gadi. To use it, you need to be a
member of group `ik11` and load the corresponding module:

```console
module use /g/data/ik11/spack/modules
module load domain-tools
```

If instead you wish to build and install the tools from the sources, you can
follow a slighly modified version of above step-be-step instructions:
```console
module load intel-compiler netcdf
export NetCDF_ROOT=$(nc-config --prefix)
git clone https://github.com/COSIMA/domain-tools.git
cd domain-tools
cmake -B build -DCMAKE_BUILD_TYPE=Release -DNetCDF_Fortran_LIBRARY=$NetCDF_ROOT/lib/Intel/libnetcdff.so -DNetCDF_C_LIBRARY=$NetCDF_ROOT/lib/libnetcdf.so -DNetCDF_Fortran_INCLUDE_DIRS=$NetCDF_ROOT/include/Intel
cmake --build build
cmake --install build --prefix=<directory where to install the tools>
```
