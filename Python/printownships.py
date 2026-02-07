# printownships
# Copyright (C) 2026
# 3a543e0e-532f-49f3-a0b0-c13194272629
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


def printownships(tt, sys, player, data, numsystems,
                  numplayers, numdesigns, names, ship):
    """
    Python translation of the Fortran printownships subroutine.

    tt          : file handle (already opened for writing)
    sys         : system index (0-based)
    player      : player index (0-based)
    data        : 3D list [numplayers][numdesigns][10]
    ship        : 3D list [numsystems][numplayers][numdesigns]
    names       : 3D list [numplayers][numdesigns][2]
    """

    for d in range(numdesigns):
        if ship[sys][player][d] > 0:
            tt.write(
                f" {ship[sys][player][d]:3d} {names[player][d][0]:9s}"
            )

    tt.write("\n")