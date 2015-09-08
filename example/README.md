# Angicart Example

This directory shows an example of angicart usage by reproducing the analysis
of the paper [Newberry MG, Ennis DB, Savage VM (2015) Testing Foundations of Biological Scaling Theory Using Automated Measurements of Vascular Networks. PLoS Comput Biol 11(8): e1004455. doi:10.1371/journal.pcbi.1004455](https://dx.doi.org/10.1371/journal.pcbi.1004455)

The real substance is the `process` script and the comments within it.  These
illustrate how to process data using angicart.  This README serves as a guide
to using the `process` script to reproduce the analysis of Newberry et al. 2015
as quickly as possible and explains the context in which `process` runs.  For
further details, consult the `--help` output of the angicart programs
themselves, the comments within the `process` script, and the README in the
main angicart directory.

## Directory Contents

```
dicom_1
dicom_2
dicom_3
```

The input to angicart is a series of sequentially-names PNG images.  The
`dicom_N` directories contain the exported raster planes of original DICOM
files that have been converted to PNG.

```
downsample_dicom.sh
```

Running `./downsample_dicom.sh` in the `example` directory will resample the
files in `dicom_N` by averaging each 2x2x2-voxel cube into a single voxel as
described in the Methods and Materials, Image Acquisition section of Newberry
et al. 2015, and record the output in `dicom_small_N`.  This step is required
to run the `example/process` script.  The `downsample_dicom.sh` script requires
ImageMagick to be installed.

```
process
process.large
```

Running `process` from the angicart source directory will execute the
analysis on the resampled images (`dicom_small_N`) and store the results in
`example/out`.  Invoke the scripts from the angicart source directory as
`example/process`, since they refer to programs in the angicart source
directory such as `./make.sh`.  The `process.large` script runs the analysis
on the original-dimension PNG images in `dicom_N`, and is not recommended as
the excess of pixel-scale noise in these images will introduce erroneous
vessels.

## Reproducing the Analysis

If angicart is installed and working correctly, the analysis should proceed as
in the following transcript:

```
angicart/ $ cd example
angicart/example/ $ ./downsample_dicom.sh
dicom_1/00000.png dicom_1/00001.png -> dicom_small_1/00000.png
dicom_1/00002.png dicom_1/00003.png -> dicom_small_1/00001.png
[...]
angicart/example/ $ cd ..
angicart $ example/process
[...]
```

This will create the main output data in `example/out/dicom_small.all.tsv`.
The columns of the spreadsheet are
- `tag`, an arbitrary string naming the input file
- `name`, an arbitrary string naming the vessel segment.  angicart typically
  uses a textual description of the vessel endpoints, as this is guaranteed to
  uniquely specify the vessel within a given image.
- `len`, the vessel segment length, in physical units (mm in the above
  example).
- `vol`, the vessel segment volume, in physical units as above.
- `rad`, the vessel radius, computed as the square root of (`vol`/(π×`len`)).
- `voxc`, the count of voxels in the segment
- `defc`, the number of voxels outside a distance `rad` + 1 from the vessel
  centerline.
- `col`, the color of a vessel in the `.vis.gd` file.
- `tips`, the number of vessel endpoints downstream of this vessel.
- `parent`, the name of this vessel's parent
- `beta`, the ratio of this vessel's radius to its parent's radius, or NA if no
  parent exists.
- `gamma`, the ratio of this vessel's length to its parent's length, or NA if
  no parent exists.
- `nchild`, the number of children of this vessel.
- `q`, the conserved exponent of radius, if any, of the downstream branching
  junction.
- `s`, the conserved exponent of length, if any, of the downstream branching
  junction.

