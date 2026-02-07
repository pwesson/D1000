# printsystem
# Copyright (C) 2026
# b82d9f2c-c3e0-415d-a623-dcb7b6508fa9
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https:#www.gnu.org/licenses/>.
# These Terms shall be governed and construed in accordance with the laws of 
# England and Wales, without regard to its conflict of law provisions.


def printsystem(tt, sys, systemname, system, numplayers, numsystems, housename,
                whatclass, whatbase, whatcommod):
    """
    Python translation of the Fortran printsystem subroutine.

    tt          : file handle (already opened for writing)
    sys         : system index (0-based in Python)
    systemname  : list of system names
    system      : 2D list [numsystems][7]
    housename   : list of house names
    whatclass   : function returning class string
    whatbase    : function returning base string
    whatcommod  : function returning commodity string
    """

    # Fortran arrays are 1-based; Python is 0-based.
    # The original code uses system(sys,1), system(sys,3), etc.
    # These map to system[sys][0], system[sys][2], etc.

    class_str  = whatclass(system[sys][0])
    base_str   = whatbase(system[sys][2])
    commod_str = whatcommod(system[sys][3])

    sysname = systemname[sys]
    char = class_str  # Fortran stored class in a 1-char variable

    # Write output (Fortran-style formatting preserved)
    tt.write("\n")
    tt.write(
        f"System {sys+1} : {sysname}, located at - "
        f"[{system[sys][4]},{system[sys][5]},{system[sys][6]}] "
        f"- class {char}, {commod_str}\n"
    )

    controller = housename[ system[sys][1] - 1 ]  # Fortran index fix

    tt.write(
        f" controlled by the House of the {controller} - {base_str}\n"
    )