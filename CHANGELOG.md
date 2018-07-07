# 0.5.0.1

* GHC 8.6 pre-release compatibility (h/t @galenhuntington)

# 0.5.0.0

* Added variant types

# 0.4.0.0

* Added `RecAll`. [#4]

* Exported `Sort` (previously hidden).

* Fixed a bug where 'toJSON' was producing a list instead of an object. [#7]

* When compiling with GHCJS, native JavaScript records are now used as
  underlying representation.

* Sorting fields is now done with merge sort instead of insertion sort.

# 0.3.0.0

We didn't keep a changelog for this and previous versions.
