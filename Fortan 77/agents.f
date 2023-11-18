       program agents
c
c This program works out the promotion
c
c These are the ranks            exp   cap  skill
c
c    15 ........ Grand Admiral   350+  max   5D+2
c    14 ........ Sector Admiral  300   -15   5D+1
c    13 ........ Fleet Admiral   260   -15   5D
c    12 ........ Commodore       220   -10    4D+2
c    11 ........ Exp Captain     180   -10    4D+1
c    10 ........ Captain         150   -10    4D
c     9 ........ Commander       120    -5   3D+2
c     8 ........ Lieut Commander  90    -5   3D+1
c     7 ........ Lieutenant       70    -5   3D
c     6 ........ SubLieutenant    50          2D+2
c     5 ........ Cheif Petty      30          2D+1
c     4 ........ Petty Officer    20          2D
c     3 ........ Able Spacehand   10         1D+2
c     2 ........ Spacehand         5         1D+1
c     1 ........ Recruit           0         1D
c
c
c sabotage can only take place if influtration has been done.
c This depends on the credits the agent spends.
c Only one agent can try and sabotage.
c
c        sabotage an A ....... 20 at least Commander
c        sabotage an b ....... 16 at least Lieutenant
c        sabotage an c ....... 12 at least Petty Officer
c        sabotage an d .......  8 at least Able Spacehand
c        sabotage an e .......  4 at least anyone
c        sabotage an f ....... It's a lump of rock!!!
c
c
c
c
c
       integer ans
       real prod
       real experience

c      print '($,2a)',char(22),char(12)

       print*,'1.... normal system'
       print*,'2.... HQ'
       read(unit=*,fmt=*) experience

       print*,'2.0    a'
       print*,'1.5    b'
       print*,'1.2    c'
       print*,'1.0    d'
       print*,'0.7    e'
       print*,'0.5    f'
       read(unit=*,fmt=*) prod
       experience = experience * prod

       print*,'1.0    own system'
       print*,'1.5    ally system'
       print*,'2.0    enermy system'
       read(unit=*,fmt=*) prod
       experience = experience * prod

       print*,'1      WITH FLEET'
       print*,'2      ON PLANET'
       read(unit=*,fmt=*) ans

       if (ans.eq.1) then
         print*,'0.7    small'
         print*,'1.0    medium'
         print*,'1.5    huge'
         read(unit=*,fmt=*) prod
         experience = experience * prod

         print*,'1      nothing'
         print*,'3      retailiate'
         print*,'3      attack and withdraw'
         print*,'5      fight at enermy system'
         read(unit=*,fmt=*) prod
         experience = experience * prod

       else
      
         print*,'1      nothing'
         print*,'2      increase production'
         print*,'4      attempt sabotage'
         print*,'6      sabotage'
         print*,'7      assassinate agent'
         read(unit=*,fmt=*) prod
         experience = experience * prod

       endif

       print*,'Experience = ',experience
 
       stop
       end
