# ggtda v0.1.0

This is the inaugural release. It contains several basic plotting layers and associated themes, with a small data set and two vignettes for illustration.

## barcode and persistence diagrams

The `geom_barcode()` layer takes persistence data with the two required aesthetics `start` (or `xmin`) and `end` (or `xmax`) and produces a barcode diagram, which can optionally be grouped and color-coded by an additional variable (usually dimension).

The `stat_persistence()` and `stat_frontier()` layers also take persistence data, though they only accept the required aesthetics `start` and `end`. They transform these data into the coordinates of a persistence diagram and into the endpoints of line segments that outline the persistence frontier, respectively. They can be oriented according to "flat", diagonal, or landscape formats.

The themes `theme_persist()` (equivalent to `theme_tda()`) and `theme_barcode()` format plots to resemble the persistence and barcode diagrams found in the literature.

## illustration tools

The `stat_disk()`, `stat_vietoris*()`, and `stat_cech*()` layers can be used to visualize the raw mathematical elements of persistent homology on a point cloud in the coordinate plane. By default, they use the `geom_face()` layer, which is equivalent to `geom_polygon()` but with different aesthetic defaults.

## vignettes

The two vignettes illustrate these two collections of tools. The vignettes call the **tdaunif** and **risperr** packages, for this reason included under `Suggests` in the DESCSRIPTION, to generate data and compute persistent homology.
