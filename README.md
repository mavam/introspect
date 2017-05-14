Introspect
==========

**introspect** visualizes your body internals over time so that you can
discover trends and take action accordingly.

Usage
=====

In order to produce output grpahs, **introspect** needs its data in a specific
format. Currently, there exists 3 types of data:

1. [Complete Blood Count (CBC)](https://en.wikipedia.org/wiki/Complete_blood_count)
2. [Comprehensive Metabolic Panel (CMP)](https://en.wikipedia.org/wiki/Comprehensive_metabolic_panel)
3. [Lipid Panel](https://en.wikipedia.org/wiki/Lipid_profile)

Each requires a specific input format. The [examples][examples] directory
contains sample files for each format.

After having populated your data files in the `input` directory, you can
now generate the plots:

    ./introspect

You can now find a set of PDFs in the directory `output` for which input data
existsed.

See `./introspect -h` for more detailed customization options, such as changing
the input/output directories, file format, and more.

License
=======

This code ships ships with a 3-clause BSD license.
