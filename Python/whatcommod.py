# whatcommod
# Copyright (C) 2026
# c111e33f-b062-43aa-a136-16745263c22a
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


def whatcommod(number: int) -> str:
    """
    Python translation of the Fortran whatcommod subroutine.
    Returns a 10-character commodity string based on the number.
    """

    # Default value
    commod = "XXXXXXXXXX"

    # In the original Fortran, all cases return the same string,
    # but the structure is preserved here for future expansion.
    if number == 1:
        return "XXXXXXXXXX"
    if number == 2:
        return "XXXXXXXXXX"
    if number == 3:
        return "XXXXXXXXXX"
    if number == 4:
        return "XXXXXXXXXX"
    if number == 5:
        return "XXXXXXXXXX"
    if number == 6:
        return "XXXXXXXXXX"

    return commod