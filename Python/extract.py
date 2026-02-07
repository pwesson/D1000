# extract
# Copyright (C) 2026
# e334c174-bd92-4bd0-af6c-a3e0594bfc08
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

from atoi import atoi


def extract():
    info = [""] * 20
    party = [""] * 20

    van = [0] * 5
    res1 = [0] * 5

    takesystem = 0
    retreat = 0
    system = 0

    print(chr(98))          # Fortran: a = 98 ; print*,char(a)
    print(ord('b'))         # Fortran: print*,ichar('b')

    # Read file
    with open("orders", "r") as f:
        for line in f:
            line = line.rstrip("\n")
            if line.startswith("combat"):
                print()
                print()
                print(line[:79])

                # -----------------------------------------
                # Extract tokens from the combat line
                # -----------------------------------------
                count = 0
                i = 0
                length = len(line)

                while i < length:
                    if line[i] != ' ':
                        count += 1
                        start = i
                        value = ord(line[i]) - 48

                        i += 1
                        while i < length and line[i] != ' ':
                            value = value * 10 + (ord(line[i]) - 48)
                            i += 1

                        token = line[start:i]
                        info[count - 1] = token

                        # Map values to variables
                        if count == 2:
                            system = value

                        if 5 <= count <= 9:
                            van[count - 5] = value

                        if 10 <= count <= 14:
                            res1[count - 10] = value

                        if count == 15:
                            retreat = value

                        if count == 16:
                            takesystem = value
                    else:
                        i += 1

                # -----------------------------------------
                # Print extracted data
                # -----------------------------------------
                print()
                print("Fighting at system =", system)
                print("The vanguard is :", van)
                print("First reserve is:", res1)
                print("Retreat factor =", retreat)
                print("Take system    =", takesystem)

                # -----------------------------------------
                # Parse attack groups from info(3)
                # -----------------------------------------
                groups = info[2]
                numgroups = 0
                start = 0
                party_list = []

                for i, ch in enumerate(groups):
                    if ch == ',':
                        numgroups += 1
                        party_list.append(groups[start:i])
                        start = i + 1
                    if ch == '0':
                        break

                print("Number of groups =", numgroups)

                # -----------------------------------------
                # Parse each party group
                # -----------------------------------------
                for group in party_list:
                    print()
                    start = 0
                    for i, ch in enumerate(group + " "):
                        if ch in [' ', '=']:
                            substring = group[start:i]
                            if substring:
                                value = atoi(substring)
                                print(value)
                            start = i + 1

            # End of file condition
            if line.startswith('.'):
                break


# Run the program
if __name__ == "__main__":
    extract()