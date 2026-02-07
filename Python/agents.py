# Agents
# Copyright (C) 2026
# 37e1185f-a09d-43f8-8a8e-501e28c6419b
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


def agents():

    # Read base experience
    print("1.... normal system")
    print("2.... HQ")
    experience = float(input("Enter experience: "))

    # System type multiplier
    print("2.0    a")
    print("1.5    b")
    print("1.2    c")
    print("1.0    d")
    print("0.7    e")
    print("0.5    f")
    prod = float(input("Enter system type multiplier: "))
    experience *= prod

    # Ally/enemy multiplier
    print("1.0    own system")
    print("1.5    ally system")
    print("2.0    enemy system")
    prod = float(input("Enter location multiplier: "))
    experience *= prod

    # With fleet or on planet
    print("1      WITH FLEET")
    print("2      ON PLANET")
    ans = int(input("Enter choice: "))

    if ans == 1:
        print("0.7    small")
        print("1.0    medium")
        print("1.5    huge")
        prod = float(input("Enter fleet size multiplier: "))
        experience *= prod

        print("1      nothing")
        print("3      retaliate")
        print("3      attack and withdraw")
        print("5      fight at enemy system")
        prod = float(input("Enter action multiplier: "))
        experience *= prod

    else:
        print("1      nothing")
        print("2      increase production")
        print("4      attempt sabotage")
        print("6      sabotage")
        print("7      assassinate agent")
        prod = float(input("Enter action multiplier: "))
        experience *= prod

    print("Experience =", experience)


# Run the program
if __name__ == "__main__":
    agents()