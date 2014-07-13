# Canonicalizing bibtex entries

The purpose of the ``bibcanon`` package is to clean up and canonicalize certain fields
in a bibtex entry. In particular, it will try to identify the authors and the journal
in order to put them in a standardized format.

## Canonicalizing authors
 When ``bibcanon`` encounters an author name, it will try to link that name to an
 existing author record.