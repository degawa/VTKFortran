---
src_dir: ./src
output_dir: ./api-doc
page_dir: ./doc
media_dir: ./doc/media
exclude_dir: ./src/tests
             ./src/third_party
project: VTKFortran
summary: a pure Fortran library to parse and emit VTK files, VTK standard
license: by-nc
docmark: !
docmark_alt: *
predocmark: >
predocmark_alt: |
display: public
         protected
         private
sort: permission-alpha
search: true
source: false
extra_mods: iso_fortran_env: https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
            iso_c_binding: https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fC_005fBINDING.html
            ieee_arithmetic: https://gcc.gnu.org/onlinedocs/gfortran/IEEE-modules.html
            penf: https://github.com/szaghi/PENF
            befor64: https://github.com/szaghi/BeFoR64
            stringifor: https://github.com/szaghi/StringiFor
graph: true
coloured_edges: true
print_creation_date: true
creation_date: %Y-%m-%d %H:%M %z
md_extensions: markdown.extensions.toc
               markdown.extensions.smarty
---
<!-- document's top page content --->
{!README.md!}