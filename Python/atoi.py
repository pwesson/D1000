# atoi
# Copyright (C) 2026
# 30059caa-7583-48eb-878f-5b24442c3da7
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

def atoi(s):

    """Fortran-style atoi: stops at first space."""
    value = 0
    for ch in s:
        if ch == ' ':
            break
        value = value * 10 + (ord(ch) - 48)
    return value


