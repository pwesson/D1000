# writereport
# Copyright (C) 2026
# bbe40a56-5bc0-4b5d-808b-e3a06f3fc54a
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


def writereport(system, ship, data, numplayers,
                numdesigns, numsystems, names,
                systemname, housename,
                printsystem, printownships, printships):
    """
    Python translation of the Fortran writereport subroutine.

    system      : 2D list [numsystems][7]
    ship        : 3D list [numsystems][numplayers][numdesigns]
    data        : 3D list [numplayers][numdesigns][10]
    names       : 3D list [numplayers][numdesigns][2]
    systemname  : list of system names
    housename   : list of house names

    printsystem, printownships, printships are Python functions
    corresponding to the Fortran subroutines.
    """

    # ------------------------------------------------------------
    # First: write the header for each player's report file
    # ------------------------------------------------------------
    for player in range(numplayers):
        filename = f"player_{player+1}.txt"
        with open(filename, "w") as f:
            f.write("\n")
            f.write("Status reports, espionage results, rumours and news\n")
            f.write("====================================================\n")

    # ------------------------------------------------------------
    # Now loop over systems and players
    # ------------------------------------------------------------
    for sys in range(numsystems):
        for player in range(numplayers):

            # Count player's ships at this system
            total = sum(ship[sys][player][d] for d in range(numdesigns))

            if total > 0:
                # Player has ships here → open their report file
                filename = f"player_{player+1}.txt"
                with open(filename, "a") as tt:

                    # Print system information
                    printsystem(
                        tt, sys, systemname, system,
                        numplayers, numsystems, housename
                    )

                    # Print player's own ships
                    printownships(
                        tt, sys, player, data, numsystems,
                        numplayers, numdesigns, names, ship
                    )

                    # ------------------------------------------------
                    # Intelligence: show other players' ships present
                    # ------------------------------------------------
                    for p in range(numplayers):
                        if p != player:
                            sump = sum(ship[sys][p][d] for d in range(numdesigns))

                            if sump > 0:
                                tt.write(
                                    f"Player {p+1}, the House of "
                                    f"{housename[p]}\n"
                                )

                                printships(
                                    tt, sys, p, data, numsystems,
                                    numplayers, numdesigns, names, ship
                                )