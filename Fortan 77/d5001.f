c
c         *****************************************         
c         *                                       *
c         *    DDD    5555   000    000    000    *
c         *    D  D   5     0   0  0   0  0   0   *
c         *    D  D   555   0   0  0   0  0   0   *
c         *    D  D      5  0   0  0   0  0   0   *
c         *    DDD    555    000    000    000    *
c         *                                       *
c         *****************************PJW & MAW***
c
       program D5001
c
c These are the agreeded formats of the arrays 
c that the program will use
c
c          PJ Wesson
c
c numsystems = number of systems
c numplayers = number of players
c numdesigns = number of ship designs
c numagents  = number of agents in each house
c numstocks  = number of stocks
c
c ship(numsystems,numplayers,numdesigns)
c
c system(numsystems,7)
c                   1.... Type 1=a,2=b,3=c,4=d,5=e,6=f
c                   2.... Owner 1=player1, 2=player2, etc
c                   3.... Base 0=none
c                   4.... Commodity
c                   5.... X coordinate
c                   6.... Y coordinate
c                   7.... Z coordinate
c
c hq(numplayers)    system number of location of players HQ
c
c agent(numplayers,numagents,4)
c                   1.... location
c                   2.... rank
c                   3.... job
c                   4.... completion
c
c design(numplayers,numdesigns,9)
c                   1.... size of ship
c                   2.... jump capability
c                   3.... SM
c                   4.... PD
c                   5.... AT
c                   6.... armr
c                   7.... cap
c                   8.... PU's
c                              
c name(numplayers,numdesigns,2)
c                   1.... ship name 
c                   2.... ship class eg Battleship
c
c market(numstock,2)
c                   1.... cost
c                   2.... amount available
c
c stocks(numplayers,numstocks+2)
c                   1.... PU's
c                   2.... credits
c                   3.... commodity 1
c                   4.... commodity 2
c                   X.... commodity numstocks
c
c jumps(total,arrival,player,to,numdesigns)
c
c


       parameter(numsystems=4,
     &           numplayers=3,
     &           numdesigns=5,
     &           numagents=2,
     &           numstocks=5)
c
       integer i,j,k
       integer player

       integer ship(numsystems,numplayers,numdesigns)
       integer system(numsystems,7)
       integer hq(numplayers)
       integer agent(numplayers,numagents,4)
       integer design(numplayers,numdesigns,9)
       integer market(numstocks,2)
       integer stocks(numplayers,numstocks+2)
c      integer jumps(total,arrival,player,to,numdesigns)
c
       integer turnnumber
       integer maxincome(numplayers)
       integer fleet(numplayers)
       integer morale(numplayers)
       integer data(numplayers,numdesigns,10)
       integer recieved(numplayers,numstocks)
                        
       character*15 commodity(numstocks)
       character*15 names(numplayers,numdesigns,2)
       character*60 filename(numplayers)
       character*15 HouseName(numplayers)
       character*15 PlayerName(numplayers)
       character*15 SystemName(numsystems)
c
c initialise the arrays
c
       turnnumber = 1

       commodity(1)='organics'
       commodity(2)='foodstuffs'
       commodity(3)='industry'
       commodity(4)='alloys'
       commodity(5)='minerals'
 
       filename( 1)='player01'
       filename( 2)='player02'
       filename( 3)='player03'

       HouseName(1)='Star Eagles'
       HouseName(2)='Lord'
       HouseName(3)='Psychopaths'     
  
       PlayerName(1)='Paul'
       PlayerName(2)='Brevan & Mark'
       PlayerName(3)='Mark'

       ship(1,1,1)=10
       ship(1,1,2)=10
       ship(1,1,3)=2
       ship(1,1,4)=0
       ship(1,1,5)=0
       ship(1,2,1)=10
       ship(1,2,2)=0
       ship(1,2,3)=10
       ship(1,2,4)=5
       ship(1,2,5)=1
       ship(1,3,1)=0
       ship(1,3,2)=0
       ship(1,3,3)=1
       ship(1,3,4)=2
       ship(1,3,5)=0
                   
       ship(2,1,1)=0
       ship(2,1,2)=0
       ship(2,1,3)=0
       ship(2,1,4)=0
       ship(2,1,5)=0
       ship(2,2,1)=20
       ship(2,2,2)=10
       ship(2,2,3)=5
       ship(2,2,4)=0
       ship(2,2,5)=1
       ship(2,3,1)=12
       ship(2,3,2)=0
       ship(2,3,3)=6
       ship(2,3,4)=0
       ship(2,3,5)=10

       ship(3,1,1)=5
       ship(3,1,2)=5
       ship(3,1,3)=1
       ship(3,1,4)=2
       ship(3,1,5)=1
       ship(3,2,1)=20
       ship(3,2,2)=0
       ship(3,2,3)=15
       ship(3,2,4)=20
       ship(3,2,5)=0
       ship(3,3,1)=10
       ship(3,3,2)=0
       ship(3,3,3)=0
       ship(3,3,4)=2
       ship(3,3,5)=5
                   
       ship(4,1,1)=0
       ship(4,1,2)=10
       ship(4,1,3)=4
       ship(4,1,4)=0
       ship(4,1,5)=0
       ship(4,2,1)=5
       ship(4,2,2)=0
       ship(4,2,3)=10
       ship(4,2,4)=0
       ship(4,2,5)=3
       ship(4,3,1)=20
       ship(4,3,2)=8
       ship(4,3,3)=0
       ship(4,3,4)=5
       ship(4,3,5)=10


c     
       systemName(1)='Dark Nebula'      
       system(1,1)= 1
       system(1,2)= 1
       system(1,3)= 0
       system(1,4)= 2
       system(1,5)= 1
       system(1,6)= 2
       system(1,7)= 2
                                  
       SystemName(2)='Thingol'
       system(2,1)= 2
       system(2,2)= 2
       system(2,3)= 0
       system(2,4)= 1
       system(2,5)= 2
       system(2,6)= 3
       system(2,7)= 1
               
       SystemName(3)='Auraiur'
       system(3,1)= 3
       system(3,2)= 1
       system(3,3)= 0
       system(3,4)= 3
       system(3,5)= 4
       system(3,6)= 3
       system(3,7)= 1
                              
       SystemName(4)='Weston'
       system(4,1)= 1
       system(4,2)= 3
       system(4,3)= 0
       system(4,4)= 4
       system(4,5)= 1
       system(4,6)= 3
       system(4,7)= 2

       print '($,2a)',char(22),char(12) 

c       write(unit=*,fmt=600) 'Here is some text '
c600    format( 1x,a20,$)
c       print*,' And some more on the same line'


       call initialise(data,numplayers,numdesigns,names)
c
c the code
c
       print*,'Starting D5000'

       do 10 player=1,numplayers
         open(unit=player+10,file=filename(player))
         write(unit=player+10,fmt=*) '  '
         write(unit=player+10,fmt=*) 'Dimension 5001'
         write(unit=player+10,fmt=*) '  '
         write(unit=player+10,fmt=*) 'Turnsheet for player ',player,
     &       ', the House of the ',HouseName(player),
     &       ' - Turn ',turnnumber
         write(unit=player+10,fmt=*) '  '
         write(unit=player+10,fmt=*) 'Hello ',Playername(player)
         write(unit=player+10,fmt=*) '  '
         write(unit=player+10,fmt=*) 'Your House Morale is',
     &          ' currently ',morale(player)
         write(unit=player+10,fmt=*) 'The total income possible from',
     &          ' your holdings is ',maxincome(player)
         write(unit=player+10,fmt=*) 'Your fleet is of total value ',
     &          fleet(player),'.'
         write(unit=player+10,fmt=*) ' '
         write(unit=player+10,fmt=*) 'Combat Results '
         write(unit=player+10,fmt=*) '============== '
         write(unit=player+10,fmt=*) ' '
10     continue

       call writedesigns(numplayers,numdesigns,data,names)
                   
       do 20 player=1,numplayers
         write(unit=player+10,fmt=*) ' '
         write(unit=player+10,fmt=*) 'You have the following',
     & ' personalities'                                      
         write(unit=player+10,fmt=*) '======================',
     & '=============='
         write(unit=player+10,fmt=*) ' '
20     continue

       call writeowner(system,numsystems,numplayers,
     &                   systemname)

       call writestocks(commodity,stocks,market,
     &                   recieved,numplayers,numstocks)
                 
       call writereport(system,ship,data,numplayers,
     &                   numdesigns,numsystems,names,
     &                   systemname,housename)

       stop
       end


 


c-----------------------------------------------------------------
       subroutine initialise(data,numplayers,numships,names)
c-----------------------------------------------------------------
c
c declare the variables used
c
           integer numplayers,numships
           integer houses
           integer data(numplayers,numships,10)  
           integer a,b,c,d,e,f,g,i,j,h,k
           character*60 line
           character*15 names(numplayers,numships,2)
c
c load in the data file of the ship designs.
c 
           open(unit=20,file='/users/wesson/D5000/designs',
     &                       status='old')
           read(unit=20,fmt='(a30)') line
           read(unit=20,fmt='(a30)') line
           read(unit=20,fmt=*) houses
           print*,'Number of houses = ',houses
           do 10 i=1,houses
               print*,' '
               print*,'Player ',i
               read(unit=20,fmt='(a30)') line
               read(unit=20,fmt='(a30)') line
               do 20 j=1,5
                read(unit=20,fmt=600) k,a,b,c,d,e,f,g,h,k,
     &                      names(i,j,1),names(i,j,2)
 
                write(unit=*,fmt=600) j,a,b,c,d,e,f,g,h,k,
     &                      names(i,j,1),names(i,j,2)
                       data(i,j,1)=a
                       data(i,j,2)=b
                       data(i,j,3)=c
                       data(i,j,4)=d
                       data(i,j,5)=e
                       data(i,j,6)=f
                       data(i,j,7)=g             
                       data(i,j,8)=h
                       data(i,j,9)=k 
20          continue
10     continue

600    format( i1,i7,i4,i5,i5,i5,i4,i4,i5,i5,a9,1x,a15 )    
        close(20)


       return
       end


       
c-------------------------------------------------------------------
       subroutine writedesigns(numplayers,numdesigns,data,names
     &              )  
c-------------------------------------------------------------------
       integer numplayers
       integer numdesigns 
       integer d
       integer player
       integer ff

       integer data(numplayers,numdesigns,10)
      
       character*15 names(numplayers,numdesigns,2)
 

       do 10 player=1,numplayers
         ff = player+10
         write(unit=ff,fmt=*) 'Ship Designs '
         write(unit=ff,fmt=*) '============== '
         write(unit=ff,fmt=*) ' '
         write(unit=ff,fmt=*) 
     &    'Tp  Name       # size jp  SM  PD  AT arm cap val Pus',
     &    ' Description'
         do 20 d=1,numdesigns 
           write(unit=ff,fmt=6000) d,names(player,d,1),
     &        0,data(player,d,1),data(player,d,2),
     &        data(player,d,3),data(player,d,4),data(player,d,5),
     &        data(player,d,6),data(player,d,7),data(player,d,8),
     &        data(player,d,9),
     &        names(player,d,2)
20       continue   
10     continue               
6000   format(1x,i1,1x,a9,1x,i4,9(1x,i3),1x,a15)
     
       return
       end 



c--------------------------------------------------------------------
       subroutine writeowner(system,numsystems,numplayers,
     &                SystemName)
c--------------------------------------------------------------------
       integer numplayers
       integer numsystems
       integer player
       integer tt
       integer cl
       integer sys
       integer system(numsystems,7)

       character*4 base
       character*4 class
       character*10 commod
       character*15 SystemName(numsystems)

       do 10 player=1,numplayers
         tt = player+10                 

         write(unit=tt,fmt=*) 'You Control the Following Systems'
         write(unit=tt,fmt=*) '================================='
         write(unit=tt,fmt=*) ' '
         write(unit=tt,fmt=*) ' Name           system no.   ',
     &   'location    class   base   commodity'
10     continue                                  
c
c loop over the classes. 1=a, ...6=f
c 
       do 30 cl=1,6
         do 20 sys=1,numsystems                       
           call whatclass( class,system(sys,1))
           call whatbase( base,system(sys,3))
           call whatcommod( commod,system(sys,4))
           tt = system(sys,2)+10
           if (system(sys,1).eq.cl) then
             write(unit=tt,fmt=6000) SystemName(sys),sys,system(sys,5),
     &       system(sys,6),system(sys,7),class,base,commod      
           endif
20       continue
30     continue                               
6000   format( a15,4x,i3,'       [',i2,',',i2,',',i2,']     ',
     &          a4,2x,a4,3x,a10)
                                   
       return
       end



c---------------------------------------------------------------
       subroutine whatclass( class, number )
c---------------------------------------------------------------
       integer number
       character*4 class
       class=' '
       if (number.eq.1) class='a'
       if (number.eq.2) class='b'
       if (number.eq.3) class='c'
       if (number.eq.4) class='d'
       if (number.eq.5) class='e'
       if (number.eq.6) class='f'
       return
       end
 

c---------------------------------------------------------------
       subroutine whatbase( base, number )
c---------------------------------------------------------------
       integer number
       character*4 base
       base='XXXX'
       if (number.eq.1) base=' '
       if (number.eq.2) base=' '
       if (number.eq.3) base=' '
       if (number.eq.4) base=' '
       if (number.eq.5) base=' '
       if (number.eq.6) base=' '
       return
       end
      

c---------------------------------------------------------------
       subroutine whatcommod( commod, number )
c---------------------------------------------------------------
       integer number
       character*10 commod
       commod='XXXXXXXXXX'
       if (number.eq.1) commod='XXXXXXXXXX'
       if (number.eq.2) commod='XXXXXXXXXX'
       if (number.eq.3) commod='XXXXXXXXXX'
       if (number.eq.4) commod='XXXXXXXXXX'
       if (number.eq.5) commod='XXXXXXXXXX'
       if (number.eq.6) commod='XXXXXXXXXX'
       return
       end
      


c---------------------------------------------------------------
       subroutine writereport(system,ship,data,numplayers,
     &                        numdesigns,numsystems,names,
     &                        systemname,housename)
c---------------------------------------------------------------
       integer numplayers
       integer numdesigns
       integer numsystems
       integer tt
       integer sys,sum
       integer d,sump,p
       integer player                   
       integer system(numsystems,7)
       integer ship(numsystems,numplayers,numdesigns)
       integer data(numplayers,numdesigns,10)
       character*15 names(numplayers,numdesigns,2)
       character*15 systemname(numsystems)
       character*15 housename(numplayers)


       do 10 player=1,numplayers
         tt=player+10
         write(unit=tt,fmt=*) ' '
         write(unit=tt,fmt=*) 'Status reports, espionage',
     &   ' results, rumours and news'
         write(unit=tt,fmt=*) '=========================',
     &   '=========================='

10     continue
c
c loop over the systems
c 
       do 20 sys = 1,numsystems 
         do 30 player=1,numplayers
           sum=0
           tt = player+10
           do 40 d=1,numdesigns
             sum=sum+ship(sys,player,d)
40         continue
c
c if sum>0 then the player has ships present at the system
c                  
           if (sum.gt.0) then
             call printsystem(tt,sys,systemname,system,
     &               numplayers,numsystems,housename)
       
             call printownships(tt,sys,player,data,numsystems,
     &               numplayers,numdesigns,names,ship)
c
c because the player has ships at the system, the player
c receives intelligence reports from the system showing what
c otherships are present.
c                        
             do 50 p=1,numplayers
               if (p.ne.player) then
                 sump=0
                 do 60 d=1,numdesigns
                   sump=sump+ship(sys,p,d)
60               continue
c
c if sump>0 then player p has some ships there.
c
                 if (sump.gt.0) then
                   write(unit=tt,fmt=*)
     &             'Player ',p,', the House of ',housename(p)
                   call printships(tt,sys,p,data,numsystems,
     &               numplayers,numdesigns,names,ship)
                 endif
               endif
50           continue
           endif
30       continue
20     continue
                           
       return
       end

c------------------------------------------------------------------
       subroutine printownships(tt,sys,player,data,numsystems,
     &               numplayers,numdesigns,names,ship)
c------------------------------------------------------------------
       integer numplayers
       integer numsystems
       integer numdesigns
       integer player
       integer tt                
       integer sys,sum
       integer d
       integer data(numplayers,numdesigns,10)
       integer ship(numsystems,numplayers,numdesigns)
       character*15 names(numplayers,numdesigns,2)
       character*15 string
c
c This subroutine will print out the player own ships 
c at a system
c
       do 10 d=1,numdesigns
         if (ship(sys,player,d).gt.0) then
           write(unit=tt,fmt=6000) ship(sys,player,d),
     &       names(player,d,1)
         endif
10     continue
       write(unit=tt,fmt=*) ' '
6000   format( 1x,i3,1x,a9,$)      
          
       return
       end




c------------------------------------------------------------------
       subroutine printships(tt,sys,player,data,numsystems,
     &               numplayers,numdesigns,names,ship)
c------------------------------------------------------------------
       integer numplayers
       integer numsystems
       integer numdesigns
       integer player
       integer tt                
       integer sys
       integer d
       integer data(numplayers,numdesigns,10)
       integer ship(numsystems,numplayers,numdesigns)
       character*15 names(numplayers,numdesigns,2)
       character*15 string

       string=' Starship:'
       do 10 d=1,numdesigns
         if (data(player,d,2).ne.0.and.
     &            ship(sys,player,d).gt.0) then
           write(unit=tt,fmt=6000) string,ship(sys,player,d),
     &               names(player,d,1),names(player,d,2)
           string='         '
         endif           
10     continue

       string=' Systemships:' 
       do 20 d=1,numdesigns
         if (data(player,d,2).eq.0.and.
     &            ship(sys,player,d).gt.0) then
           write(unit=tt,fmt=6000) string,ship(sys,player,d),
     &               names(player,d,1),names(player,d,2)
           string='         '
         endif             
20     continue
6000   format( 1x,a15,1x,i3,1x,a9,' class ',a15)


       return
       end




c---------------------------------------------------------------------
       subroutine printsystem(tt,sys,systemname,system,
     &               numplayers,numsystems,housename)
c---------------------------------------------------------------------
       integer numplayers
       integer numsystems
       integer tt
       integer sys
       integer system(numsystems,7)
       character*15 systemname(numsystems)
       character*11 sysname
       character*1 char
       character*4 base
       character*4 class
       character*10 commod
       character*15 housename(numplayers)

       call whatclass ( class, system(sys,1) )
       call whatbase  (  base, system(sys,3) )
       call whatcommod(commod, system(sys,4) )
                           
       sysname=systemname(sys)  
       char = class

       write(unit=tt,fmt=*) ' '
       write(unit=tt,fmt=*) 'System ',sys,' : ',sysname,',',
     &   ' located at - [',system(sys,5),',',system(sys,6),',',
     &   system(sys,7),'] - class ',char,', ',commod
       write(unit=tt,fmt=*) ' controlled by the House of the ',
     &      housename(system(sys,2)),' - ',base 

       return
       end




c--------------------------------------------------------------------
       subroutine writestocks(commodity,stocks,market,
     &                   recieved,numplayers,numstocks)
c--------------------------------------------------------------------
       integer numplayers            
       integer player
       integer numstocks
       integer tt,s
       integer stocks(numplayers,numstocks+2) 
       integer recieved(numplayers,numstocks)
       integer market(numstocks,2)
       character*15 commodity(numstocks)           

       do 10 player=1,numplayers
         tt=player+10
         write(unit=tt,fmt=*) ' '
         write(unit=tt,fmt=*) 'You have the following',
     & ' commodity stocks'
         write(unit=tt,fmt=*) '======================',
     & '================='
         write(unit=tt,fmt=*) ' '
         write(unit=tt,fmt=*) '              Recieved  Stocks',
     &   '   Market     Price' 
         do 20 s=1,numstocks
           write(unit=tt,fmt=*) commodity(s),'     ',
     &     recieved(player,s),
     &     '       ',stocks(player,s+2),'       ',market(s,2),
     &     '       ',market(s,1)
20       continue
10     continue                         

       return
       end
