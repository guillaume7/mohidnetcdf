Here we list the improvements required in order to add HDF5 reading functionality to the matlab scripts.

# Functions to Change #

This is a list of the m files that need to be adapted in order to work with HDF5 files input as well:

  * do\_attr
  * do\_coords
  * get\_var
  * get\_ncs (???)

# New routines #

  * gethdf5 --> is the equivalent of the getnc.
  * atthdf5 --> is the equivalent of the attnc.
  * do\_nccoords --> is the old do\_coords.
  * do\_coords --> switches between the do\_nccoords and the do\_hdf5coords.