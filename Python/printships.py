# printships
# Copyright (C) 2026
# 343f48a2-d9d5-472e-aa47-a626967aa9b8
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


def printships(tt, sys, player, data, numsystems,
               numplayers, numdesigns, names, ship):
    """
    Python translation of the Fortran printships subroutine.

    tt          : file handle (already opened for writing)
    sys         : system index (0-based)
    player      : player index (0-based)
    data        : 3D list [numplayers][numdesigns][10]
    ship        : 3D list [numsystems][numplayers][numdesigns]
    names       : 3D list [numplayers][numdesigns][2]
    """

    # -----------------------------
    # Starships (data(player,d,2) != 0)
    # -----------------------------
    string = " Starship:"
    for d in range(numdesigns):
        if data[player][d][1] != 0 and ship[sys][player][d] > 0:
            tt.write(
                f" {string:15s} {ship[sys][player][d]:3d} "
                f"{names[player][d][0]:9s} class {names[player][d][1]:15s}\n"
            )
            string = " " * 9  # Fortran: '         '

    # -----------------------------
    # Systemships (data(player,d,2) == 0)
    # -----------------------------
    string = " Systemships:"
    for d in range(numdesigns):
        if data[player][d][1] == 0 and ship[sys][player][d] > 0:
            tt.write(
                f" {string:15s} {ship[sys][player][d]:3d} "
                f"{names[player][d][0]:9s} class {names[player][d][1]:15s}\n"
            )
            string = " " * 9