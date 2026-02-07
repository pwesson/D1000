# writestocks
# Copyright (C) 2026
# 7de32a38-4088-4dce-a515-1edcaa814559
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


def writestocks(commodity, stocks, market, recieved, numplayers, numstocks):
    """
    Python translation of the Fortran writestocks subroutine.

    commodity : list of strings, length numstocks
    stocks    : 2D list [numplayers][numstocks+2]
    recieved  : 2D list [numplayers][numstocks]
    market    : 2D list [numstocks][2]
    """

    for player in range(numplayers):
        # Fortran writes to unit = player + 10
        filename = f"player_{player+1}.txt"
        with open(filename, "w") as f:

            f.write("\n")
            f.write("You have the following commodity stocks\n")
            f.write("=======================================\n\n")
            f.write("              Recieved  Stocks   Market     Price\n")

            for s in range(numstocks):
                line = (
                    f"{commodity[s]:15s}   "
                    f"{recieved[player][s]:3d}       "
                    f"{stocks[player][s+2]:3d}       "
                    f"{market[s][1]:3d}       "
                    f"{market[s][0]:3d}\n"
                )
                f.write(line)