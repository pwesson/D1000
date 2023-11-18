 
       program combat
c
c This program will simulate space ship  in D5000
c
c           PJ Wesson  10 February 1992
c
c fist declare the arrays
c
        integer numplayers,numships
        parameter( numplayers=7,numships=5 )
c
        integer k,d
        integer player

        character*60  names(numplayers,numships)
	character*60  class(numplayers,numships)

        integer damagepd
        integer damagesm
        integer damageat
        integer atwho
        integer group

        integer attlist(numplayers,numplayers,numplayers)
        integer retlist(numplayers,numplayers,numplayers)
        integer hitby(numplayers,numplayers)
        integer react(numplayers,numplayers)
        integer van(numplayers,numships)
        integer res1(numplayers,numships)
        integer res2(numplayers,numships)
        integer holdfire(numplayers)
        integer much(numplayers)
        integer hittingwho(numplayers,numplayers)  

        integer smallpd(numplayers)
        integer smallat(numplayers)
        integer smallsm(numplayers)
        integer smallsize(numplayers)
        integer largepd(numplayers)
        integer largeat(numplayers)
        integer largesm(numplayers)
        integer largesize(numplayers)

        integer house(numplayers)
        integer attackpd(numplayers)
        integer attacksm(numplayers)
        integer attackat(numplayers)
        integer size(numplayers)
        integer dampd(numplayers,numplayers)
        integer damat(numplayers,numplayers)
        integer damsm(numplayers,numplayers)
        integer data(numplayers,numships,10)
        integer arm(numplayers,numships)
        integer hps(numplayers,numships) 
        integer avail(numships) 
        integer hit(numplayers)
        integer retf(numplayers)
        integer initsize(numplayers)
	integer atsystem(numplayers)

        logical present(numplayers) 
        logical ret(numplayers)
        logical cont
        logical novan
        logical fighting
        logical looking
        logical stillfighting

        stillfighting = .false.

	do 1 i=1,numplayers
		present(i)=.true.
		atsystem(i)=1
1	continue
                                         
        do 710 player=1,numplayers
          do 720 j=1,numplayers
            hitby(player,j) = 0
720       continue
710     continue

c
        call fightwho(attlist,numplayers)
        call fightback(retlist,numplayers)
c
        call vangard(van,res1,res2,numplayers,numships,retf,
     &			present,atsystem)
c 

        call initialise(data,numplayers,numships,names,class,atsystem)

        do 1010 player=1,numplayers
          do 1020 k=1,numships
            hps(player,k)=data(player,k,1)
            arm(player,k)=data(player,k,6)
1020      continue
1010    continue

     
        iround=0

        do 1011 player=1,numplayers
          initsize(player) = 0
          do 1012 d=1,numships
            initsize(player) = initsize(player) +
     &        (van(player,d)+res1(player,d)+res2(player,d))*
     &        data(player,d,1)
1012      continue
1011    continue




10      continue
                                   
          iround=iround + 1
          print*,' '
          print*,'-------------------------- Round ',iround,
     &				' ---------------------------'
         
     
          do 2000 player=1,numplayers
            holdfire(player)=0  
            smallpd(player)=0
            smallat(player)=0
            smallsm(player)=0
            smallsize(player)=0
            largepd(player)=0
            largeat(player)=0
            largesm(player)=0
            largesize(player)=0
            attackpd(player)=0
            attackat(player)=0
            attacksm(player)=0

            do 2010 k=1,numships
c
c calculate the amount of attack and size of each player
c      
              if (data(player,k,1).lt.10) then
c
c the size of small size.
c             
                smallsm(player)=smallsm(player)+
     &                          van(player,k)*data(player,k,3)
                smallpd(player)=smallpd(player)+
     &                          van(player,k)*data(player,k,4)
                smallat(player)=smallat(player)+
     &                          van(player,k)*data(player,k,5)
                smallsize(player)=smallsize(player)+
     &                          (van(player,k)-1)*data(player,k,1)+
     &                          hps(player,k)
              else
c
c the ship is of large size
c
                largesm(player)=largesm(player)+
     &                          van(player,k)*data(player,k,3)
                largepd(player)=largepd(player)+
     &                          van(player,k)*data(player,k,4)
                largeat(player)=largeat(player)+
     &                          van(player,k)*data(player,k,5)
                largesize(player)=largesize(player)+
     &                          (van(player,k)-1)*data(player,k,1)+
     &                          hps(player,k) 
              endif    
2010        continue


c           size(player)=smallsize(player)+largesize(player)                   
c
c Check that the player has dropped out.... ie wiped out.
c
c            if (size(player).eq.0) then
c              print*,'Player ',player,' has been ELIMINATED!!'
c              present(player)=.false.
c            endif
2000      continue
          

999       continue 
c
c work out the present size at the system
c    

c	do 954 player=1,numplayers
c	   print*,'===================================='
c           print*,'**** Player ',player
c           print*,'Damage going to give SM PD AT'
c           print*,smallsm(player),smallpd(player),smallat(player),
c     &				smallsize(player)
c           print*,largesm(player),largepd(player),largeat(player),
c     &				largesize(player)
c	   print*,'===================================='
c954	continue

                                    
          do 2501 player=1,numplayers
            if (present(player)) then
              ret(player) = .true.
              siz = 0
              do 2502 d=1,numships
                siz = siz + 
     &          (van(player,d)-1+res1(player,d)+res2(player,d))*
     &          data(player,d,1)+hps(player,d)
2502          continue
c
c Does the House want to start to retreat this turn? 
c                                               
              if (siz.gt.(100-retf(player))*
     &                             initsize(player)/100.0) then
                ret(player)=.false.
              else
		if (atsystem(player).lt.2) then
                print*,' Player ',player,
     &              ' will withdraw at the end of the combat round.'
			atsystem(player) = 2
		endif
              endif
           endif
2501     continue

c
c Okay WORK OUT WHO IS FIGHTING WHO!!!!
c
         do 2590 player=1,numplayers
           do 2591 k=1,numplayers
             hittingwho(player,k)=0
2591       continue
2590     continue

         do 2600 player=1,numplayers
           fighting = .false.
           group = 0
2620       continue
           group=group+1
           looking = .false.
           do 2610 atwho=1,numplayers
             react(player,atwho) = 0
             if (attlist(player,atwho,group).gt.0) then
c
c still looking at players to attack
c
               looking = .true.
               if (present(atwho)) then 
c
c we have someone to fight
c
                 fighting = .true.
                 react(player,atwho) = 1
                 hittingwho(player,atwho) = 1
c
c update the array to tell the player "atwho" has been attacked
c by "player". This will be usefull if the player "atwho" wants
c to retaliate.
c
                 hitby(atwho,player) = 1 
               endif    
             endif

c            print*,player,group,atwho,attlist(player,atwho,
c     &               group),react(player,atwho)

2610       continue
           if (.not.fighting.and.looking) goto 2620
c
c Tell the user if no more Houses on the attack list
c
           if (.not.looking.and.present(player)) then   
c
c all targets have been destroyed. Now look at the list
c of players who has been attacking you.
c

             fighting = .false.
             group = 0
3620         continue
             group=group+1
             looking = .false.
             do 3610 atwho=1,numplayers
               react(player,atwho) = 0
               if (retlist(player,atwho,group).gt.0) then
c
c still looking at players to attack
c
                 looking = .true.
                 if (present(atwho).and.
     &                  hitby(player,atwho).gt.0) then 
c
c player has someone to fight
c
                   fighting = .true.
                   react(player,atwho) = 1
                   hittingwho(player,atwho) = 2
c
c update the array to tell the player "atwho" has been attacked
c by "player". This will be usefull if the player "atwho" wants
c to retaliate.
c
                   hitby(atwho,player) = 1
                 endif    
               endif

c              print*,player,group,atwho,attlist(player,atwho,
c     &               group),react(player,atwho)

3610         continue
             if (.not.fighting.and.looking) goto 3620
           endif 
2600     continue                          

 
       call show2(player,van,res1,res2,numplayers,numships,
     &                 hittingwho,house,present,atsystem)

	stillfighting = .false.

       call pdonsmallships(player,numplayers,numships,holdfire,
     &              smallsm,smallat,smallpd,smallsize,
     &              largesm,largeat,largepd,largesize,
     &              van,res1,res2,data,dampd,arm,hps,
     &              react,hit,avail,present,size,
     &              stillfighting,much)


       call pdonlargeships(player,numplayers,numships,holdfire,
     &              smallsm,smallat,smallpd,smallsize,
     &              largesm,largeat,largepd,largesize,
     &              van,res1,res2,data,dampd,arm,hps,
     &              react,hit,avail,present,size,attackpd,
     &              stillfighting,much)


       call atonallships(player,numplayers,numships,holdfire,
     &                smallsm,smallat,smallpd,smallsize,
     &                largesm,largeat,largepd,largesize,
     &                van,res1,res2,data,damat,arm,hps,
     &                react,hit,avail,present,size,attackat,
     &                stillfighting,much)


       call smonlargeships(player,numplayers,numships,holdfire,
     &                smallsm,smallat,smallpd,smallsize,
     &                largesm,largeat,largepd,largesize,
     &                van,res1,res2,data,damsm,arm,hps,
     &                react,hit,avail,present,size,attacksm,
     &                stillfighting,much)


c         print*,'>>>> PD <<<< Damage still remaining to give.'  
c         do 830 player=1,numplayers
c           if (present(player)) then                              
c             print*,'From player ',player,' is ',attackpd(player)
c           endif
c830     continue


c         print*,'>>>> AT <<<< Damage still remaining to give.'  
c         do 831 player=1,numplayers
c           if (present(player)) then                              
c             print*,'From player ',player,' is ',attackat(player)
c           endif
c831     continue
c
c         print*,'>>>> SM <<<< Damage still remaining to give.'  
c         do 832 player=1,numplayers
c           if (present(player)) then                              
c             print*,'From player ',player,' is ',attacksm(player)
c           endif
c832     continue
c       print*,' '



c        print*,'Still fighting ',stillfighting
       if (.not.stillfighting) then
c		print*,'Not fighting'
		print*,' '
		print*,'***D5000*************************************',
     &			'*******************'
		print*,' '
		stop  
	endif
       stillfighting=.false.




c
c Loop over the players at the system working out how much the
c attack is reduced. 
c
       do 1120 player=1,numplayers
         if (present(player)) then 
c
c calculate the total damage being received
c
           sumpd = attackpd(player)
           sumat = attackat(player)
           sumsm = attacksm(player)
           damagesm = 0             
           damagepd = 0             
           damageat = 0              
c
c calculate available attack at the system
c
           do 1230 k=1,numships
             damagesm = damagesm +van(player,k)*data(player,k,3)
             damagepd = damagepd +van(player,k)*data(player,k,4)
             damageat = damageat +van(player,k)*data(player,k,5)
 1230      continue 
          
c
c If damage still to do is less than attack able to give then
c reduce the overall  being given proportionally over the
c players receiving the damage. 
c

           if (sumpd.gt.damagepd) then
               attackpd(player)=attackpd(player)*damagepd*1.0/sumpd
           endif
           if (sumat.gt.damageat) then
               attackat(player)=attackat(player)*damageat*1.0/sumat
           endif
           if (sumsm.gt.damagesm) then
               attacksm(player)=attacksm(player)*damagesm*1.0/sumsm
           endif

         endif

c         print*,'Player ',player
c         print*,'Max damage SM PD AT able to give ',damagesm,
c     &                     damagepd,damageat
c         print*,'Reduces damage given to ',attacksm(player),
c     &                   attackpd(player),attackat(player)

1120   continue





         cont=.false.
         do 1130 player=1,numplayers
           if (present(player)) then
             novan=.true.
c
c do we require a new vanguard?
c
             do 1140 k=1,numships
               if (van(player,k).gt.0) then
                 novan=.false.
               endif
1140         continue

             smallpd(player)=0
             smallat(player)=0
             smallsm(player)=0
             largepd(player)=0
             largeat(player)=0
             largesm(player)=0
             smallsize(player)=0
             largesize(player)=0

             if (novan) then
               novan=.false.
               do 1150 k=1,numships
                 if (res1(player,k).gt.0) then
                   novan=.true.
                 endif
 1150          continue
             end if

             if (novan) then
              sum=0      
              print*,' Bring in player ',player,'''s reserves '
              cont=.true.
              do 1160 k=1,numships
               van(player,k)=res1(player,k)
               res1(player,k)=res2(player,k)
               res2(player,k)=0

               if (data(player,k,1).lt.10) then
                smallsm(player)=smallsm(player)+van(player,k)*
     &                          data(player,k,3)
                smallpd(player)=smallpd(player)+van(player,k)*
     &                          data(player,k,4)
                smallat(player)=smallat(player)+van(player,k)*
     &                          data(player,k,5)
               else
                largesm(player)=largesm(player)+van(player,k)*
     &                          data(player,k,3)
                largepd(player)=largepd(player)+van(player,k)*
     &                          data(player,k,4)
                largeat(player)=largeat(player)+van(player,k)*
     &                          data(player,k,5)
               endif    
c
1160          continue
	     endif
c
c work out the total size of ships at the system
c
	     do 1161 k=1,numships
               if (data(player,k,1).lt.10) then
                smallsize(player)=smallsize(player)+van(player,k)*
     &                          data(player,k,1)
               else
                largesize(player)=largesize(player)+van(player,k)*
     &                          data(player,k,1)
               endif    
1161	     continue
 

           endif
1130     continue



c
c loop over the players working out total attack
c and the total size of the players force at the system.
c Taking the damage left to do and including the reserves
c attack. 
c
       do 1170 player=1,numplayers
           if (present(player)) then
               size(player)=0
               do 1180 k=1,numships
                 size(player)=size(player)+(van(player,k)-1)*
     &                         data(player,k,1)+hps(player,k)
1180           continue                        
c
c include the  still required to do against the 
c other players.
c
               smallpd(player)=smallpd(player)+attackpd(player) 
               smallsm(player)=smallsm(player)+attacksm(player) 
               smallat(player)=smallat(player)+attackat(player) 
               smallsize(player)=smallsize(player) 
               largepd(player)=largepd(player)
               largesm(player)=largesm(player)
               largeat(player)=largeat(player)
               largesize(player)=largesize(player)
           endif 
c
c           print*,'**** Player ',player
c           print*,'Damage going to give SM PD AT'
c           print*,smallsm(player),smallpd(player),smallat(player),
c     &				smallsize(player)
c           print*,largesm(player),largepd(player),largeat(player),
c     &				largesize(player)
1170   continue


c
c Work out if the player is going to retreat
c
       do 1200 player=1,numplayers
           if (present(player)) then
c
c Only allow a House to retreat at the end of the combat round. 
c
c               if (ret(player)) then
c                   present(player)=.false.
c               endif
c
c calculate the number of ships present. If none are then
c the player is FORCED!!! to retreat and leave the system.
c
               sum=0
               do 1210 k=1,numships
                   sum=sum+van(player,k)+res1(player,k)+res2(player,k)
1210           continue
               if (sum.eq.0) then
c                   print*,'Player ',player,' is destroyed.'
                   present(player)=.false.
                   ret(player)=.true.
		   atsystem(player) = 4
               endif
           endif
1200   continue

c
c If there is still more attack to do then loop back.
c

        if (cont) then
c             print*,'Looping back'
             goto 999
        endif


        do 1201 player=1,numplayers
           if (present(player)) then
               if (ret(player)) then
                   present(player)=.false.
		   atsystem(player)=3
               endif
	   endif
1201	continue




       if (1.eq.1) go to 10       
c
c return to the main program
c
       end


 


c-----------------------------------------------------------------
       subroutine initialise(data,numplayers,numships,names,class,
     &			atsystem)
c-----------------------------------------------------------------
c
c declare the variables used
c
           integer numplayers,numships
           integer houses
           integer data(numplayers,numships,10)  
           integer a,b,c,d,e,f,g,i,j,h,k
	   integer atsystem(numplayers)
           character*80 line
           character*60 names(numplayers,numships)
	   character*60 class(numplayers,numships)
c
c load in the data file of the ship designs.
c 
           open(unit=20,file='designs',
     &                                      status='old')
           read(unit=20,fmt='(a30)') line
           read(unit=20,fmt='(a30)') line
           read(unit=20,fmt=*) houses
	   if (atsystem(i).gt.0) then
          	 print*,'Number of houses = ',houses
	   endif
           do 10 i=1,houses
               read(unit=20,fmt='(a80)') line
               read(unit=20,fmt='(a80)') line
	       if (atsystem(i).gt.0) then
			print*,' '
			print*,'Player ',i
			print*,line
	       endif
               do 20 j=1,5
                read(unit=20,fmt=600) k,a,b,c,d,e,f,g,h,k,line
		names(i,j) = line(1:10)
		class(i,j) = line(11:30)
		if (atsystem(i).gt.0) then
                	write(unit=*,fmt=601) j,a,b,c,d,e,f,g,h,k,line
		endif
                       data(i,j,1)=a
                       data(i,j,2)=b
                       data(i,j,3)=c
                       data(i,j,4)=d
                       data(i,j,5)=e
                       data(i,j,6)=f
                       data(i,j,7)=g
20          continue
10     continue

600     format( i1,i7,i4,i5,i5,i5,i4,i4,i5,i5,a30)
601     format( i1,i7,i4,i5,i5,i5,i4,i4,i5,i5,1x,a30)

        close(20)


       return
       end


                   

c-------------------------------------------------------------------
            subroutine vangard(van,res1,res2,
     &                      	numplayers,numships,retf,
     &				present,atsystem)
c-------------------------------------------------------------------
c
	integer numplayers,numships
	integer retf(numplayers)
	integer van(numplayers,numships)
	integer res1(numplayers,numships)
	integer res2(numplayers,numships)
	integer i,houses
	character*60 line
	logical present(numplayers)
	integer atsystem(numplayers)
c     
c Load in the data from file
c                           
	open(unit=20,file='vanguard', status='old')
	read(unit=20,fmt='(a30)') line
	read(unit=20,fmt='(a30)') line
	read(unit=20,fmt=*) houses
	do 10 i=1,houses
               read(unit=20,fmt='(a30)') line
               read(unit=20,fmt=*)  retf(i)
               read(unit=20,fmt=*)  van(i,1),van(i,2),van(i,3),
     &                            van(i,4),van(i,5)
               read(unit=20,fmt=*)  res1(i,1),res1(i,2),res1(i,3),
     &                            res1(i,4),res1(i,5)
               read(unit=20,fmt=*)  res2(i,1),res2(i,2),res2(i,3),
     &                            res2(i,4),res2(i,5)


	if (van(i,1)+van(i,2)+van(i,3)+van(i,4)+van(i,5)+
     &	res1(i,1)+res1(i,2)+res1(i,3)+res1(i,4)+res1(i,5)+
     &	res2(i,1)+res2(i,2)+res2(i,3)+res2(i,4)+res2(i,5).eq.0) then
		present(i)=.false.
		atsystem(i)=0
	endif
 10     continue

	
            close(20)

            return
            end

 
                                      
c--------------------------------------------------------------
            subroutine fightwho2(attlist,numplayers)
c--------------------------------------------------------------
c
            integer numplayers
            integer attlist(numplayers,numplayers,numplayers)
            integer i,j,k   
            integer houses
            character*60 line
c
c Who does the first player want to attack?
c 1=Yes else 0=No
c
            open(unit=20,file='fightwho',status='old')
c
            read(unit=20,fmt='(a30)') line
            read(unit=20,fmt='(a30)') line
            read(unit=20,fmt=*) houses


       do 10 i=1,houses
               read(unit=20,fmt='(a30)') line
               read(unit=20,fmt='(a30)') line
               do 20 j=1,houses
                       read(unit=20,fmt=*) 
     &                    (attlist(i,j,k),k=1,numplayers)
20             continue
10     continue
       close(20)

c
c return to the main program
c
       return
        end


 
                                      
c--------------------------------------------------------------
            subroutine fightwho(attlist,numplayers)
c--------------------------------------------------------------
c
	integer numplayers
	integer attlist(numplayers,numplayers,numplayers)
	integer i,j,k  
	integer ptr
	integer ptrstart
	integer ptrfinish
	integer ptrlength
	integer value
	integer houses
	character*60 line
	character*60 orders
	character*60 whom(100)
c
c Who does the first player want to attack?
c 1=Yes else 0=No
c
	do 1 i=1,numplayers
	    do 2 j=1,numplayers
		do 3 k=1,numplayers
			attlist(i,j,k)=0
3		continue
2	    continue
1	continue
c
	open(unit=20,file='fightwho',status='old')

	read(unit=20,fmt='(a30)') line
	read(unit=20,fmt='(a30)') line
	read(unit=20,fmt=*) houses
	
	do 10 i=1,houses
		read(unit=20,fmt='(a30)') line
c
c The variable line now holds the information of who to fight
c It is in the form 3,1,4=2=5,0 where house 3 is attacked before
c House 1 and then Houses 2,4 and 5 are attacked together.
c
c Want to put the information in the array attlist(i,j,k)
c where i is the House in question
c       j is the House to attack
c       k is the order in which to attack them
c 
		ptr=0
		ptrstart=0
		ptrfinish=0
30		continue
		  ptr=ptr+1
		  if (line(ptr:ptr).gt.' ') then
	 	   ptrstart=ptr
40	 	   continue
			ptr=ptr+1
			if (line(ptr:ptr+1).eq.',0') then
				ptrfinish=ptr-1
			        goto 50
			endif
		    goto 40
		  endif
		  goto 30
50		continue
		orders=line(ptrstart:ptrfinish)
c		print*,ptrstart,ptrfinish,'   ',orders
		ptrlength=ptrfinish - ptrstart + 1
c
c We have so far extracted the relavent parts of the orders into
c the character array  orders. 
c    
	numgroups = 0
	ptr=0
	ptrstart=1
	numgroups = 1
c	print*,ptrlength
	do 60 ptr=1,ptrlength
		if (orders(ptr:ptr).eq.',') then
			numgroups=numgroups+1
			ptrstart = ptr
		endif
		if (numgroups.eq.1) then
			whom(numgroups)=orders(ptrstart:ptr)
		else
			whom(numgroups)=orders(ptrstart+1:ptr)
		endif
60	continue

c	print*,'numgroups = ',numgroups
c	do 70 ptr=1,numgroups
c		print*,':  ',whom(ptr)
c70	continue

	do 80 group=1,numgroups
	   ptrstart=1
	   ptr=0
90	   continue
		ptr=ptr+1
		if (whom(group)(ptr:ptr).eq.' ') then
                  call atoi( whom(group)(ptrstart:ptr-1),value)
c                  print*,'Value is ',value
		  attlist(i,value,group) = 1
                  goto 210
              endif
                                  
              if (whom(group)(ptr:ptr).eq.'=') then           
                  call atoi( whom(group)(ptrstart:ptr-1),value)
c                  print*,'Value is ',value
		  attlist(i,value,group) = 1
                  ptrstart=ptr+1
              endif
              goto 90
210	      continue

80	   continue



c               print*,'Fighting other Houses'
c     		 do 20 j=1,houses
c                      write(*,*)  (attlist(i,j,k),k=1,numplayers)
c20             continue
10     continue
       close(20)

c
c return to the main program
c
       return
        end


 



                                      
c--------------------------------------------------------------
            subroutine fightback(retlist,numplayers)
c--------------------------------------------------------------
c
        integer numplayers
        integer retlist(numplayers,numplayers,numplayers)
        integer i,j,k   
        integer houses
        character*60 line
	integer ptr
	integer ptrstart
	integer ptrfinish
	integer ptrlength
	integer value
	character*60 orders
	character*60 whom(100)
c
c Who does the first player want to attack?
c 1=Yes else 0=No
c
	do 1 i=1,numplayers
	    do 2 j=1,numplayers
		do 3 k=1,numplayers
			retlist(i,j,k)=0
3		continue
2	    continue
1	continue
c
	open(unit=20,file='fightback',status='old')
c
	read(unit=20,fmt='(a30)') line
	read(unit=20,fmt='(a30)') line
	read(unit=20,fmt=*) houses


	do 10 i=1,houses
		read(unit=20,fmt='(a30)') line
c
c The variable line now holds the information of who to fight
c It is in the form 3,1,4=2=5,0 where house 3 is attacked before
c House 1 and then Houses 2,4 and 5 are attacked together.
c
c Want to put the information in the array retlist(i,j,k)
c where i is the House in question
c       j is the House to attack
c       k is the order in which to attack them
c 
		ptr=0
		ptrstart=0
		ptrfinish=0
30		continue
		  ptr=ptr+1
		  if (line(ptr:ptr).gt.' ') then
	 	   ptrstart=ptr
40	 	   continue
			ptr=ptr+1
			if (line(ptr:ptr+1).eq.',0') then
				ptrfinish=ptr-1
			        goto 50
			endif
		    goto 40
		  endif
		  goto 30
50		continue
		orders=line(ptrstart:ptrfinish)
c		print*,ptrstart,ptrfinish,'   ',orders
		ptrlength=ptrfinish - ptrstart + 1
c
c We have so far extracted the relavent parts of the orders into
c the character array  orders. 
c    
	numgroups = 0
	ptr=0
	ptrstart=1
	numgroups = 1
c	print*,ptrlength
	do 60 ptr=1,ptrlength
		if (orders(ptr:ptr).eq.',') then
			numgroups=numgroups+1
			ptrstart = ptr
		endif
		if (numgroups.eq.1) then
			whom(numgroups)=orders(ptrstart:ptr)
		else
			whom(numgroups)=orders(ptrstart+1:ptr)
		endif
60	continue

c	print*,'numgroups = ',numgroups
c	do 70 ptr=1,numgroups
c		print*,':  ',whom(ptr)
c70	continue

	do 80 group=1,numgroups
	   ptrstart=1
	   ptr=0
90	   continue
		ptr=ptr+1
		if (whom(group)(ptr:ptr).eq.' ') then
                  call atoi( whom(group)(ptrstart:ptr-1),value)
c                  print*,'Value is ',value
		  retlist(i,value,group) = 1
                  goto 210
              endif
                                  
              if (whom(group)(ptr:ptr).eq.'=') then           
                  call atoi( whom(group)(ptrstart:ptr-1),value)
c                  print*,'Value is ',value
		  retlist(i,value,group) = 1
                  ptrstart=ptr+1
              endif
              goto 90
210	      continue

80	   continue



c               do 20 j=1,houses
c		      print*,'Fighting other Houses'
c                      write(*,*)  (retlist(i,j,k),k=1,numplayers)
c20             continue
10     continue
       close(20)

c
c return to the main program
c
       return
        end



                                      
c--------------------------------------------------------------
            subroutine fightback2(retlist,numplayers)
c--------------------------------------------------------------
c
            integer numplayers
            integer retlist(numplayers,numplayers,numplayers)
            integer i,j,k   
            integer houses
            character*60 line
c
c Who does the first player want to attack?
c 1=Yes else 0=No
c
            open(unit=20,file='fightback',status='old')
c
            read(unit=20,fmt='(a30)') line
            read(unit=20,fmt='(a30)') line
            read(unit=20,fmt=*) houses
 
       do 10 i=1,houses
               read(unit=20,fmt='(a30)') line
               read(unit=20,fmt='(a30)') line
               do 20 j=1,houses
                   read(unit=20,fmt=*)
     &                  (retlist(i,j,k),k=1,numplayers)
20             continue
10     continue
       close(20)

c
c return to the main program
c
       return
        end

                                      
c-------------------------------------------------------------------
       subroutine show(player,van,res1,res2,numplayers,numships,
     &                      hittingwho)
c-------------------------------------------------------------------
c
       integer numplayers,numships
       integer player

       integer van(numplayers,numships)
       integer res1(numplayers,numships)
       integer res2(numplayers,numships)  
       integer hittingwho(numplayers,numplayers)
                 

         do 3700 player=1,numplayers
           do 3710 k=1,numplayers
             if (hittingwho(player,k).eq.1) then
               print*,' Player ',player,' fighting against: ',k
             endif
             if (hittingwho(player,k).eq.2) then
               print*,' Player ',player,' retaliating against:',k
             endif
3710       continue
3700     continue

       print*,' '

       do 10 player=1,numplayers 
       write(unit=*,fmt=2000) player,
     &                    van(player,1),res1(player,1),res2(player,1),
     &                    van(player,2),res1(player,2),res2(player,2),
     &                    van(player,3),res1(player,3),res2(player,3),
     &                    van(player,4),res1(player,4),res2(player,4),
     &                    van(player,5),res1(player,5),res2(player,5)
10     continue


2000     format( 1x,i3,
     &           ' (',i3,':',i3,':',i3,')  ',
     &           '(',i3,':',i3,':',i3,')  ',
     &           '(',i3,':',i3,':',i3,')  ',
     &           '(',i3,':',i3,':',i3,')  ',
     &           '(',i3,':',i3,':',i3,')  ')
c
c Try out printing using a different format
c
c     print*,'Player ',player,' - ',
c     &      '(',van(player,1),':',res1(player,1),':',res2(player,1),') ',
c     &      '(',van(player,2),':',res1(player,2),':',res2(player,2),') ',
c     &      '(',van(player,3),':',res1(player,3),':',res2(player,3),') ',
c     &      '(',van(player,4),':',res1(player,4),':',res2(player,4),') ',
c     &      '(',van(player,5),':',res1(player,5),':',res2(player,5),') '
c

       return
       end



                                      
c-------------------------------------------------------------------
        subroutine show2(player,van,res1,res2,numplayers,numships,
     &                      hittingwho,house,present,atsystem)
c-------------------------------------------------------------------
c    
        integer numplayers,numships
        integer player
        integer number

        integer house(numplayers)
        integer van(numplayers,numships)
        integer res1(numplayers,numships)
        integer res2(numplayers,numships)  
        integer hittingwho(numplayers,numplayers)
     
        logical present(numplayers)
	integer atsystem(numplayers)

        character*16 string            
                     
        print*,' '
 
        do 3700 player=1,numplayers

         string=' - not fighting '
	 if (atsystem(player).eq.3) string=' - retreated  '
	 if (atsystem(player).eq.4) string=' - destroyed  '
         number = 0
	 if (present(player)) then

         do 3710 k=1,numplayers
           if (hittingwho(player,k).eq.1) then
             string=' - attacking '
             number=number+1
             house(number) = k
           endif
           if (hittingwho(player,k).eq.2) then
             string=' - retaliating '
             number=number+1
             house(number) = k
           endif
3710     continue
	 if (atsystem(player).eq.2) string=' - retreating '

	 endif

         if (atsystem(player).gt.0) then

         write(unit=*,fmt=6000)  player,van(player,1),
     &      van(player,2),van(player,3),van(player,4),van(player,5),
     &      string,(house(i),i=1,number)

6000     format(' Player ',i3,' - Ships: ',5(i3,1x),a15,10i3)

c         print*,'Player ',player,' - Ships: ',van(player,1),
c     &      van(player,2),van(player,3),van(player,4),van(player,5),
c     &      ' ',string,(house(i),i=1,number)
         endif
3700   continue

	print*,' '

       return
       end





       subroutine pdonsmallships(player,numplayers,numships,holdfire,
     &                smallsm,smallat,smallpd,smallsize,
     &                largesm,largeat,largepd,largesize,
     &                van,res1,res2,data,dampd,arm,hps,
     &                react,hit,avail,present,size,
     &                stillfighting,much)
c
c define the variables used
c
       integer player
       integer numplayers
       integer numships        
       integer much(numplayers)
       integer smallsm(numplayers)
       integer smallat(numplayers)
       integer smallpd(numplayers)
       integer smallsize(numplayers)
       integer largesm(numplayers)
       integer largeat(numplayers)
       integer largepd(numplayers)
       integer largesize(numplayers)
       integer holdfire(numplayers)
       integer van(numplayers,numships)
       integer res1(numplayers,numships)
       integer res2(numplayers,numships)
       integer dampd(numplayers,numplayers)
       integer arm(numplayers,numships)
       integer hps(numplayers,numships)
       integer react(numplayers,numplayers)
       integer hit(numplayers)
       integer avail(numships)
       integer size(numplayers)
       integer data(numplayers,numships,10)

       integer k

       logical present(numplayers) 
       logical stillfighting

c
c We now know what forces that will be involved in the battle
c lets calculate the "pd" damage against each house.
c                                       
       call attract(much,smallpd,1,smallat,1,smallsm,1,
     &                   largepd,0,largeat,0,largesm,0,
     &                   numplayers,numships,van,res1,res2,
     &                   data) 

         do 3000 player=1,numplayers
           if (present(player)) then 
             percentatt=0 
             percentsiz=0
               do 3010 k=1,numplayers
c
c "player" wants to attack player "k" is react = 1
c 
                 if (react(player,k).gt.0.and.present(k)) then
                   percentatt=percentatt+much(k)
                   percentsiz=percentsiz+smallsize(k)
                 endif
3010           continue
               holdfire(player)=0
               if (percentatt.eq.0.and.percentsiz.eq.0) then
c 
c no small ships to fire at in the vanguard so holdfire until
c the large ships come forward
c
                 holdfire(player)=smallpd(player)+largepd(player)
               endif
c
c Work out how we are going to divide up our attack against the
c Houses which we are attacking 
c
c               print*,'Attack given out from player ',player,percentatt
               do 3020 k=1,numplayers 
c
c dampd(player,k) is the damage player gives to player "k"
c                                 
                 dampd(player,k)=0
c
c divide ATT proportional to ATT of the player aimed at
c
                 if (percentatt.ne.0) then
                   if (present(k).and.react(player,k).gt.0) then
                     dampd(player,k)=(smallpd(player)+largepd(player))*
     &               much(k)/percentatt
                   endif
                 else                      
c
c If no ATT is available then divide up ATT proportinal to size
c                 
c                   print*,'Small Dividing up proportional to size.'
                   if (present(k).and.react(player,k).gt.0) then
                     if (percentsiz.gt.0) then
                     dampd(player,k)=(smallpd(player)+largepd(player))*
     &                 smallsize(k)/percentsiz
                     else 
                       dampd(player,k)=0
                     endif
                   endif
                 endif
c                print*,'Player ',player,' attacking small player ',
c     &                         k,' with ',dampd(player,k)

3020           continue
              endif
3000     continue
c
c Work out the  going to be taken from the other players
c
         total=0   
         do 4000 player=1,numplayers
           hit(player)=0
           do 4010 k=1,numplayers
             total=total+dampd(player,k)
             hit(player)=hit(player)+dampd(k,player)
4010       continue
c          print*,'Against player ',player,'  is = ',hit(player)
4000     continue
c        print*,'Total fighting power = ',total
c        print*,' '
c        print*,'******************'
c        print*,' '
c
c the fight 
c       
         do 1030 player=1,numplayers 
           if (present(player)) then
             count=0
             do 1040 k=1,numships
               if (data(player,k,1).lt.10) then
                 count=count+
     &                (van(player,k)-1)*data(player,k,1)+
     &                hps(player,k)+
     &                4*((van(player,k)-1)*data(player,k,6)+
     &                arm(player,k))
               endif
1040         continue
c             print*,'Destroy all of player ',player,' you need ',count
           
             damage =hit(player)
             if (damage.gt.0) then
                stillfighting=.true.
             endif
c             print*,' getting = ',damage,player
c------------------------
             if (damage.gt.count) then
c------------------------
c               print*,'Vanguard totally destroyed!!'
               do 1050 k=1,numships
                 if (data(player,k,1).lt.10) then
                   van(player,k)=0
                   hps(player,k)=data(player,k,1)
                   arm(player,k)=data(player,k,6)
                 endif
1050           continue
CC             print*,'Reducing the excess damage.'
c
c recalculate the damage still wanted against "player"
c
               do 1060 k=1,numplayers
                 dampd(k,player)=dampd(k,player)*(1-count*1.0/damage)                
1060           continue
c------------------------
             else
c------------------------
10000          continue
               if (damage.gt.0) then
c                 print*,'damage is going to be applied.'
                 sum=0
                 excess=0
                 do 1070 k=1,numships
                   avail(k)=0
                   if (data(player,k,1).lt.10) then
                     avail(k)=van(player,k)*
     &               (data(player,k,3)+data(player,k,4)/2.0+
     &                data(player,k,5))
                     sum=sum+avail(k)
                   endif
1070             continue
c                 print*,'Divide out to the designs = ',sum
                 if (sum.eq.0) then
                   do 1080 k=1,numships
                     avail(k)=0
                     if (data(player,k,1).lt.10) then
                       avail(k)=(van(player,k)-1)*data(player,k,1)+
     &                                     hps(player,k)    
c                       print*,' avail(k) k = ',avail(k),k 
                       sum=sum+avail(k) 
                     endif
1080               continue
                 endif

                 do 1100 k=1,numships
                   damtodo=damage*(avail(k)*1.0/sum)
c                   print*,'dam to do to design ',k,damtodo
10001              continue
                   if (damtodo.gt.0.and.van(player,k).gt.0) then
CC                   print*,'Trapped',damtodo,van(player,k)
c
                     if (arm(player,k).ge.0) then
CC                     print*,'Inside armour loop.'
                       if (arm(player,k)*4.lt.damtodo) then
                         damtodo=damtodo - arm(player,k)*4
                         arm(player,k)=0
                       endif   
                       if (arm(player,k)*4.gt.damtodo) then
                         arm(player,k)=arm(player,k)-damtodo/4
                         damtodo=0
                       endif
                     endif  
               
                     if (hps(player,k).gt.0) then
                       if (hps(player,k).gt.damtodo) then
                         hps(player,k)=hps(player,k)-damtodo
                         damtodo=0
                       endif
                       if (hps(player,k).le.damtodo) then
                         damtodo=damtodo - hps(player,k)
                         van(player,k)=van(player,k)-1
                         hps(player,k)=data(player,k,1)
                         arm(player,k)=data(player,k,6)
                       endif
                     endif

                     goto 10001
                   endif
CC                 print*,'ex excess = ',excess
                   excess=excess+damtodo
CC                 print*,'excess now = ',excess
1100             continue
c                      
c No more damage required against "player" 
c
                 do 1110 k=1,numplayers
                   dampd(k,player)=0
1110             continue
                 damage=excess
CC               print*,'Excess  for next turn ', 
c
                 goto 10000
               endif
c---------------------------
             endif
c---------------------------
  
c             print*,' '
c             print*,'These are the stats of the present ships'
c             do 708 d=1,numships
c               print*,d,hps(player,d),arm(player,d)
c708          continue  
c             print*,'Damage still remaining'
c             do 709 k=1,numplayers
c               print*,k,dampd(k,player)
c709          continue
c             print*,' '
           endif
1030     continue
                    
         return
         end







       subroutine pdonlargeships(player,numplayers,numships,holdfire,
     &                smallsm,smallat,smallpd,smallsize,
     &                largesm,largeat,largepd,largesize,
     &                van,res1,res2,data,dampd,arm,hps,
     &                react,hit,avail,present,size,attack,
     &                stillfighting,much)
c
c define the variables used
c
       integer player
       integer numplayers
       integer numships                          
       integer much(numplayers)
       integer holdfire(numplayers)
       integer smallsm(numplayers)
       integer smallat(numplayers)
       integer smallpd(numplayers)
       integer smallsize(numplayers)
       integer largesm(numplayers)
       integer largeat(numplayers)
       integer largepd(numplayers)
       integer largesize(numplayers)
       integer van(numplayers,numships)
       integer res1(numplayers,numships)
       integer res2(numplayers,numships)
       integer dampd(numplayers,numplayers)
       integer arm(numplayers,numships)
       integer hps(numplayers,numships)
       integer react(numplayers,numplayers)
       integer hit(numplayers)
       integer avail(numships)
       integer size(numplayers)
       integer data(numplayers,numships,10)
       integer attack(numplayers)

       integer k

       logical present(numplayers) 
       logical stillfighting

c       print*,'PD against large ships'

       do 10 player = 1,numplayers
         attack(player) = holdfire(player)
         do 20 k = 1,numplayers
           attack(player)=attack(player)+dampd(player,k)
20       continue
10     continue

c
c We now know what forces that will be involved in the battle
c lets calculate the "pd" damage against each house.
c

       call attract(much,smallpd,0,smallat,0,smallsm,0,
     &                   largepd,1,largeat,1,largesm,1,
     &                   numplayers,numships,van,res1,res2,
     &                   data) 
  
         do 3000 player=1,numplayers
           if (present(player)) then 
             percentatt=0 
             percentsiz=0
               do 3010 k=1,numplayers
c
c "player" wants to attack player "k" is react = 1
c 
                 if (react(player,k).gt.0.and.present(k)) then
                   percentatt=percentatt+much(k)
                   percentsiz=percentsiz+largesize(k)
                 endif
3010           continue 
               holdfire(player)=0
               if (percentatt.eq.0.and.percentsiz.eq.0) then
                 holdfire(player)=attack(player)
               endif
c
c Work out how we are going to divide up our attack against the
c Houses which we are attacking 
c
               do 3020 k=1,numplayers 
c
c dampd(player,k) is the damage player gives to player "k"
c                                 
                 dampd(player,k)=0
c
c divide ATT proportional to ATT of the player aimed at
c
                 if (percentatt.ne.0) then
                   if (present(k).and.react(player,k).gt.0) then
                     dampd(player,k)=attack(player)*
     &               much(k)/percentatt
                   endif
                 else                      
c
c If no ATT is available then divide up ATT proportinal to size
c                 
c                   print*,'Large Dividing up proportional to size.'
                   if (present(k).and.react(player,k).gt.0) then
                     if (percentsiz.gt.0) then
                       dampd(player,k)=attack(player)*
     &                           largesize(k)/percentsiz
                     else
                       dampd(player,k) = 0
                     endif
                   endif
                 endif
c                print*,'Player ',player,' attacking player ',
c     &                         k,' with ',dampd(player,k)
3020           continue
              endif
3000     continue
c
c Work out the  going to be taken from the other players
c
         total=0   
         do 4000 player=1,numplayers
           hit(player)=0
           do 4010 k=1,numplayers
             total=total+dampd(player,k)
             hit(player)=hit(player)+dampd(k,player)
4010       continue
c          print*,'Against player ',player,'  is = ',hit(player)
4000     continue

c
c the fight 
c       
         do 1030 player=1,numplayers 
           if (present(player)) then
             count = 0
             do 1040 k=1,numships
               if (data(player,k,1).ge.10) then
                 count=count+
     &       2*((van(player,k)-1)*data(player,k,1)+hps(player,k))+
     &       8*((van(player,k)-1)*data(player,k,6)+arm(player,k))
               endif
1040         continue
c             print*,'Destroy all of player ',player,' you need ',
c     &              count,' and receiving ',hit(player)
c           
             damage =hit(player)
             if (damage.gt.0) then
                stillfighting=.true.
             endif
c             print*,' getting = ',damage,player
c------------------------
             if (damage.gt.count) then
c------------------------
c               print*,'Vanguard totally destroyed!!'
               do 1050 k=1,numships
                 if (data(player,k,1).ge.10) then
                   van(player,k)=0
                   hps(player,k)=data(player,k,1)
                   arm(player,k)=data(player,k,6)
                 endif
1050           continue
CC             print*,'Reducing the excess damage.'
c
c recalculate the damage still wanted against "player"
c
               do 1060 k=1,numplayers
                 dampd(k,player)=dampd(k,player)*(1-count*1.0/damage)                
1060           continue
c------------------------
             else
c------------------------
10000          continue
               if (damage.gt.0) then
c                 print*,'damage is going to be applied.'
                 sum=0
                 excess=0
                 do 1070 k=1,numships
                   avail(k)=0
                   if (data(player,k,1).ge.10) then
                     avail(k)=van(player,k)*
     &               (data(player,k,3)+data(player,k,4)/2.0+
     &                data(player,k,5))
                     sum=sum+avail(k)
                   endif
1070             continue
c                 print*,'Divide out to the designs = ',sum
                 if (sum.eq.0) then
                   do 1080 k=1,numships
                     avail(k)=0
                     if (data(player,k,1).ge.10) then
                       avail(k)=(van(player,k)-1)*data(player,k,1)+
     &                                     hps(player,k)    
c                       print*,' avail(k) k = ',avail(k),k 
                       sum=sum+avail(k) 
                     endif
1080               continue
                 endif

                 if (sum.eq.0) then
                     excess=damtodo
                     goto 10002
                 endif

                 do 1100 k=1,numships
                   damtodo=damage*(avail(k)*1.0/sum)
c                   print*,'dam to do to design ',k,damtodo
10001              continue
                   if (damtodo.gt.0.and.van(player,k).gt.0) then
CC                   print*,'Trapped',damtodo,van(player,k)
c
                     if (arm(player,k).ge.0) then
CC                     print*,'Inside armour loop.'
                       if (arm(player,k)*8.lt.damtodo) then
                         damtodo=damtodo - arm(player,k)*8
                         arm(player,k)=0
                       endif   
                       if (arm(player,k)*8.gt.damtodo) then
                         arm(player,k)=arm(player,k)-damtodo/8
                         damtodo=0
                       endif
                     endif  
               
                     if (hps(player,k).gt.0) then
                       if (hps(player,k)*2.gt.damtodo) then
                         hps(player,k)=hps(player,k)-damtodo/2
                         damtodo=0
                       endif
                       if (hps(player,k)*2.le.damtodo) then
                         damtodo=damtodo - hps(player,k)/2
                         van(player,k)=van(player,k)-1
                         hps(player,k)=data(player,k,1)
                         arm(player,k)=data(player,k,6)
                       endif
                     endif

                     goto 10001
                   endif
CC                 print*,'ex excess = ',excess
                   excess=excess+damtodo
CC                 print*,'excess now = ',excess
1100             continue

10002            continue
c                      
c No more damage required against "player" 
c
                 do 1110 k=1,numplayers
                   dampd(k,player)=0
1110             continue
                 damage=excess
CC               print*,'Excess  for next turn ', 
c
                 goto 10000
               endif
c---------------------------
             endif
c---------------------------
  
c             print*,' '
c             print*,'These are the stats of the present ships'
c             do 708 d=1,numships
c                print*,d,hps(player,d),arm(player,d)
c708          continue  

           endif
1030     continue
 

         do 830 player=1,numplayers
           attack(player) = holdfire(player)
           do 820 k = 1,numplayers
             attack(player)=attack(player)+dampd(player,k)
820        continue         
           if (present(player)) then                              

           else
             attack(player)=0
           endif
830     continue                   
c         print*,' '
         return
         end



c---------------------------------------------------------------------
       subroutine atonallships(player,numplayers,numships,holdfire,
     &                smallsm,smallat,smallpd,smallsize,
     &                largesm,largeat,largepd,largesize,
     &                van,res1,res2,data,damat,arm,hps,
     &                react,hit,avail,present,size,attack,
     &                stillfighting,much)
c---------------------------------------------------------------------

c
c define the variables used
c
       integer player
       integer numplayers
       integer numships
       integer much(numplayers)
       integer holdfire(numplayers)
       integer smallsm(numplayers)
       integer smallat(numplayers)
       integer smallpd(numplayers)
       integer smallsize(numplayers)
       integer largesm(numplayers)
       integer largeat(numplayers)
       integer largepd(numplayers)
       integer largesize(numplayers)
       integer van(numplayers,numships)
       integer res1(numplayers,numships)
       integer res2(numplayers,numships)
       integer damat(numplayers,numplayers)
       integer arm(numplayers,numships)
       integer hps(numplayers,numships)
       integer react(numplayers,numplayers)
       integer hit(numplayers)
       integer avail(numships)
       integer size(numplayers)
       integer data(numplayers,numships,10)
       integer attack(numplayers)

       integer k

       logical present(numplayers) 
       logical stillfighting

c      print*,'***** AT against all ships *****'    
c
c work out how much each house attracts of the fire
c
       call attract(much,smallpd,1,smallat,1,smallsm,1,
     &                   largepd,1,largeat,1,largesm,1,
     &                   numplayers,numships,van,res1,res2,
     &                   data)    
c
c We now know what forces that will be involved in the battle
c lets calculate the "at" damage against each house.
c                                                   
         do 3000 player=1,numplayers
           if (present(player)) then 
             percentatt=0 
             percentsiz=0
               do 3010 k=1,numplayers
c
c "player" wants to attack player "k" is react = 1
c 
                 if (react(player,k).gt.0.and.present(k)) then
                   percentatt=percentatt+much(k)
                   percentsiz=percentsiz+smallsize(k)+largesize(k)
                 endif
3010           continue
 
               holdfire(player)=0
               if (percentatt.eq.0.and.percentsiz.eq.0) then
                 holdfire(player)=attack(player)
               endif
c
c Work out how we are going to divide up our attack against the
c Houses which we are attacking 
c
               do 3020 k=1,numplayers 
c
c damat(player,k) is the damage player gives to player "k"
c                                 
                 damat(player,k)=0
c
c divide ATT proportional to ATT of the player aimed at
c
                 if (percentatt.ne.0) then
                   if (present(k).and.react(player,k).gt.0) then
                     damat(player,k) = 
     &               (smallat(player)+largeat(player))*
     &               much(k)/percentatt
                   endif
                 else                      
c
c If no ATT is available then divide up ATT proportinal to size
c                 
c                   print*,'Large Dividing up proportional to size.'
                   if (present(k).and.react(player,k).gt.0) then
                     if (percentsiz.gt.0) then
                damat(player,k)=(smallat(player)+largeat(player))*
     &                    (smallsize(k)+largesize(k))/percentsiz
                     else
                       damat(player,k) = 0
                     endif
                   endif
                 endif
c                print*,'Player ',player,' attacking AT player ',
c     &                         k,' with ',damat(player,k)
3020           continue
              endif
3000     continue
c
c Work out the  going to be taken from the other players
c
         total=0   
         do 4000 player=1,numplayers
           hit(player)=0
           do 4010 k=1,numplayers
             total=total+damat(player,k)
             hit(player)=hit(player)+damat(k,player)
4010       continue
          if (present(player)) then
           print*,'Against player ',player,'  is = ',hit(player)
	   print*,'Enter modified value'
	   read(*,*) amodif
	   hit(player) = hit(player) + amodif
	  endif
4000     continue

c
c the fight 
c       
         do 1030 player=1,numplayers 
           if (present(player)) then
             count=0
             do 1040 k=1,numships
               if (data(player,k,1).ge.0) then
                 count=count+
     &            (van(player,k)-1)*data(player,k,1)+hps(player,k)+
     &         2*((van(player,k)-1)*data(player,k,6)+arm(player,k))
               endif
1040         continue
c             print*,'Destroy all of player ',player,' you need ',count
           
             damage = hit(player)
             if (damage.gt.0) then
                stillfighting = .true.
             endif
c             print*,' getting = ',damage,player
c------------------------
             if (damage.gt.count) then
c------------------------
c               print*,'Vanguard totally destroyed!!'
               do 1050 k=1,numships
                 if (data(player,k,1).ge.0) then
                   van(player,k)=0
                   hps(player,k)=data(player,k,1)
                   arm(player,k)=data(player,k,6)
                 endif
1050           continue
CC             print*,'Reducing the excess damage.'
c
c recalculate the damage still wanted against "player"
c
               do 1060 k=1,numplayers
                 damat(k,player)=damat(k,player)*(1-count*1.0/damage)                
1060           continue
c------------------------
             else
c------------------------
10000          continue
               if (damage.gt.0) then
c                 print*,'damage is going to be applied.'
                 sum=0
                 excess=0
                 do 1070 k=1,numships
                   avail(k)=0
                   if (data(player,k,1).ge.0) then
                     avail(k)=van(player,k)*
     &               (data(player,k,3)+data(player,k,4)/2.0+
     &                data(player,k,5))
                     sum=sum+avail(k)
                   endif
1070             continue
c                 print*,'Divide out to the designs = ',sum
                 if (sum.eq.0) then
                   do 1080 k=1,numships
                     avail(k)=0
                     if (data(player,k,1).ge.0) then
                       avail(k)=(van(player,k)-1)*data(player,k,1)+
     &                                     hps(player,k)    
c                       print*,' avail(k) k = ',avail(k),k 
                       sum=sum+avail(k) 
                     endif
1080               continue
                 endif

                 if (sum.eq.0) then
                   excess=damtodo
                   goto 10002
                 endif

                 do 1100 k=1,numships
                   damtodo=damage*(avail(k)*1.0/sum)
c                   print*,'dam to do to design ',k,damtodo
10001              continue
                   if (damtodo.gt.0.and.van(player,k).gt.0) then
CC                   print*,'Trapped',damtodo,van(player,k)
c
                     if (arm(player,k).ge.0) then
CC                     print*,'Inside armour loop.'
                       if (arm(player,k)*2.lt.damtodo) then
                         damtodo=damtodo - arm(player,k)*2
                         arm(player,k)=0
                       endif   
                       if (arm(player,k)*2.gt.damtodo) then
                         arm(player,k)=arm(player,k)-damtodo/2
                         damtodo=0
                       endif
                     endif  
               
                     if (hps(player,k).gt.0) then
                       if (hps(player,k).gt.damtodo) then
                         hps(player,k)=hps(player,k)-damtodo
                         damtodo=0
                       endif
                       if (hps(player,k).le.damtodo) then
                         damtodo=damtodo - hps(player,k)
                         van(player,k)=van(player,k)-1
                         hps(player,k)=data(player,k,1)
                         arm(player,k)=data(player,k,6)
                       endif
                     endif

                     goto 10001
                   endif
CC                 print*,'ex excess = ',excess
                   excess=excess+damtodo
CC                 print*,'excess now = ',excess
1100             continue
c                      
c No more damage required against "player" 
c
                 do 1110 k=1,numplayers
                   damat(k,player)=0
1110             continue

10002            continue
                 damage=excess
CC               print*,'Excess  for next turn ', 
c
                 goto 10000
               endif
c---------------------------
             endif
c---------------------------
  
c             print*,' '
c             print*,'These are the stats of the present ships'
c             do 708 d=1,numships
c               print*,d,hps(player,d),arm(player,d)
c708          continue  
c             print*,'Damage still remaining'
c             do 709 k=1,numplayers
c               print*,k,damat(k,player)
c709          continue


c             print*,' '
           endif
1030     continue


c         print*,'>>>> AT <<<< Damage still remaining to give.'  
         do 1031 player=1,numplayers
           attack(player) = holdfire(player)
           do 720 k = 1,numplayers
             attack(player)=attack(player)+damat(player,k)
720        continue        
           if (present(player)) then                    
c            print*,'From player ',player,' is ',attack(player)
           else
             attack(player)=0
           endif
1031     continue
c         print*,' '
                    
         return
         end





c---------------------------------------------------------------------
       subroutine smonlargeships(player,numplayers,numships,holdfire,
     &                smallsm,smallat,smallpd,smallsize,
     &                largesm,largeat,largepd,largesize,
     &                van,res1,res2,data,damsm,arm,hps,
     &                react,hit,avail,present,size,attack,
     &                stillfighting,much)
c---------------------------------------------------------------------

c
c define the variables used
c
       integer player
       integer numplayers
       integer numships
       integer much(numplayers)
       integer holdfire(numplayers)
       integer smallsm(numplayers)
       integer smallat(numplayers)
       integer smallpd(numplayers)
       integer smallsize(numplayers)
       integer largesm(numplayers)
       integer largeat(numplayers)
       integer largepd(numplayers)
       integer largesize(numplayers)
       integer van(numplayers,numships)
       integer res1(numplayers,numships)
       integer res2(numplayers,numships)
       integer damsm(numplayers,numplayers)
       integer arm(numplayers,numships)
       integer hps(numplayers,numships)
       integer react(numplayers,numplayers)
       integer hit(numplayers)
       integer avail(numships)
       integer size(numplayers)
       integer data(numplayers,numships,10)
       integer attack(numplayers)

       integer k

       logical present(numplayers) 
       logical stillfighting

c       print*,'***** SM against all ships *****'    
c
c work out how much each house attracts of the fire
c
       call attract(much,smallpd,0,smallat,0,smallsm,0,
     &                   largepd,1,largeat,1,largesm,1,
     &                   numplayers,numships,van,res1,res2,
     &                   data)    
c
c We now know what forces that will be involved in the battle
c lets calculate the "sm" damage against each house.
c                                                   
         do 3000 player=1,numplayers
           if (present(player)) then 
             percentatt=0 
             percentsiz=0
               do 3010 k=1,numplayers
c
c "player" wants to attack player "k" is react = 1
c 
                 if (react(player,k).gt.0.and.present(k)) then
                   percentatt=percentatt+much(k)
                   percentsiz=percentsiz+largesize(k)
                 endif
3010           continue 

               holdfire(player)=0
               if (percentatt.eq.0.and.percentsiz.eq.0) then
                 holdfire(player)=smallsm(player)+largesm(player)
               endif
c
c
c Work out how we are going to divide up our attack against the
c Houses which we are attacking 
c
               do 3020 k=1,numplayers 
c
c damat(player,k) is the damage player gives to player "k"
c                                 
                 damsm(player,k)=0
c
c divide ATT proportional to ATT of the player aimed at
c
                 if (percentatt.ne.0) then
                   if (present(k).and.react(player,k).gt.0) then
                     damsm(player,k) = 
     &               (smallsm(player)+largesm(player))*
     &               much(k)/percentatt
                   endif
                 else                      
c
c If no ATT is available then divide up ATT proportinal to size
c                 
c                   print*,'Large Dividing up proportional to size.'
                   if (present(k).and.react(player,k).gt.0) then
                     if (percentsiz.gt.0) then
                damsm(player,k)=(smallsm(player)+largesm(player))*
     &                    (largesize(k))/percentsiz
                     else
                       damsm(player,k) = 0
                     endif
                   endif
                 endif
c                print*,'Player ',player,' attacking SM player ',
c     &                         k,' with ',damsm(player,k)
3020           continue
              endif
3000     continue
c
c Work out the  going to be taken from the other players
c
         total=0   
         do 4000 player=1,numplayers
           hit(player)=0
           do 4010 k=1,numplayers
             total=total+damsm(player,k)
             hit(player)=hit(player)+damsm(k,player)
4010       continue
c           print*,'Against player ',player,'  is = ',hit(player)
4000     continue

c
c the fight 
c       
         do 1030 player=1,numplayers 
           if (present(player)) then
             count=0
             do 1040 k=1,numships
               if (data(player,k,1).ge.0) then
                 count=count+
     &            (van(player,k)-1)*data(player,k,1)+hps(player,k)
               endif
1040         continue
c             print*,'Destroy all of player ',player,' you need ',count
           
             damage = hit(player)
             if (damage.gt.0) then
                stillfighting = .true.
             endif
c             print*,' getting = ',damage,player
c------------------------
             if (damage.gt.count) then
c------------------------
c               print*,'Vanguard totally destroyed!!'
               do 1050 k=1,numships
                 if (data(player,k,1).ge.10) then
                   van(player,k)=0
                   hps(player,k)=data(player,k,1)
                   arm(player,k)=data(player,k,6)
                 endif
1050           continue
CC             print*,'Reducing the excess damage.'
c
c recalculate the damage still wanted against "player"
c
               do 1060 k=1,numplayers
                 damsm(k,player)=damsm(k,player)*(1-count*1.0/damage)                
1060           continue
c------------------------
             else
c------------------------
10000          continue
               if (damage.gt.0) then
c                 print*,'damage is going to be applied.'
                 sum=0
                 excess=0
                 do 1070 k=1,numships
                   avail(k)=0
                   if (data(player,k,1).ge.10) then
                     avail(k)=van(player,k)*
     &               (data(player,k,3)+data(player,k,4)/2.0+
     &                data(player,k,5))
                     sum=sum+avail(k)
                   endif
1070             continue
c                 print*,'Divide out to the designs = ',sum
                 if (sum.eq.0) then
                   do 1080 k=1,numships
                     avail(k)=0
                     if (data(player,k,1).ge.10) then
                       avail(k)=(van(player,k)-1)*data(player,k,1)+
     &                                     hps(player,k)    
c                       print*,' avail(k) k = ',avail(k),k 
                       sum=sum+avail(k) 
                     endif
1080               continue
                 endif   

                 if (sum.eq.0) then
                   excess=damtodo
                   goto 10002
                 endif

                 do 1100 k=1,numships
                   damtodo=damage*(avail(k)*1.0/sum)
c                   print*,'dam to do to design ',k,damtodo
10001              continue
                   if (damtodo.gt.0.and.van(player,k).gt.0) then
c                     print*,'Trapped',damtodo,van(player,k)
c
c                     if (arm(player,k).ge.0) then
c                      print*,'Inside armour loop.'
c                       if (arm(player,k)*2.lt.damtodo) then
c                         damtodo=damtodo - arm(player,k)*2
c                         arm(player,k)=0
c                       endif   
c                       if (arm(player,k)*2.gt.damtodo) then
c                         arm(player,k)=arm(player,k)-damtodo/2
c                         damtodo=0
c                       endif
c                     endif  
c               
                     if (hps(player,k).gt.0) then
                       if (hps(player,k).gt.damtodo) then
                         arm(player,k)=(arm(player,k)*
     &                               damtodo)/hps(player,k)
                         hps(player,k)=hps(player,k)-damtodo
                         damtodo=0
                       endif
                       if (hps(player,k).le.damtodo) then
                         damtodo=damtodo - hps(player,k)
                         van(player,k)=van(player,k)-1
                         hps(player,k)=data(player,k,1)
                         arm(player,k)=data(player,k,6)
                       endif
                     endif

                     goto 10001
                   endif
CC                 print*,'ex excess = ',excess
                   excess=excess+damtodo
CC                 print*,'excess now = ',excess
1100             continue
c                      
c No more damage required against "player" 
c
                 do 1110 k=1,numplayers
                   damsm(k,player)=0
1110             continue

10002            continue
                 damage=excess
CC               print*,'Excess  for next turn ', 
c
                 goto 10000
               endif
c---------------------------
             endif
c---------------------------
  
c             print*,' '
c             print*,'These are the stats of the present ships'
c             do 708 d=1,numships
c               print*,d,hps(player,d),arm(player,d)
c708          continue  
c
           endif
1030     continue



c         print*,'>>>> SM <<<< Damage still remaining to give.'  
         do 1031 player=1,numplayers
           attack(player) = holdfire(player)
c           print*,'attack ',attack(player)
           do 720 k = 1,numplayers
c             attack(player)=attack(player)+damsm(player,k)
c             print*,'attack ',attack(player)
720        continue    
           if (present(player)) then                                   
c             print*,'From player ',player,' is ',attack(player) 
           else
             attack(player)=0 
           endif
1031     continue
c         print*,' '
                    
         return
         end

                                          




c------------------------------------------------------------------
         subroutine attract(much,smallpd,t1,smallat,t2,smallsm,t3,
     &                     largepd,t4,largeat,t5,largesm,t6,
     &                     numplayers,numships,van,res1,res2,
     &                     data)       
c------------------------------------------------------------------
         integer numplayers
         integer numships 
         integer much(numplayers)
         integer smallpd(numplayers)
         integer smallat(numplayers)
         integer smallsm(numplayers)
         integer largepd(numplayers)
         integer largeat(numplayers)
         integer largesm(numplayers)
         integer van(numplayers,numships)
         integer res1(numplayers,numships)
         integer res2(numplayers,numships)
         integer data(numplayers,numships,10)
         integer t1
         integer t2
         integer t3
         integer t4
         integer t5
         integer t6
         integer d
         integer player
c
c work out what percentage they attract
c
         do 10 player = 1,numplayers
           much(player) = 0
           do 20 d=1,numships
             if (data(player,d,1).lt.10) then
c
c dealing with small ships pd
c
               if (t1.eq.1) then
                 much(player)=much(player)+
     &                        van(player,d)*data(player,d,4)/2.0
               endif
c
c small ships AT
c
               if (t2.eq.2) then
                 much(player)=much(player)+
     &                        van(player,d)*data(player,d,5)
               endif
c 
c small ships SM
               if (t3.eq.3) then
                 much(player)=much(player)+
     &                        van(player,d)*data(player,d,3)
               endif
             else
c
c dealing with large ships pd
c
               if (t4.eq.1) then
                 much(player)=much(player)+
     &                        van(player,d)*data(player,d,4)/2.0
c                 print*,van(player,d)*data(player,d,4)/2.0
               endif
c
c large ships AT
c
               if (t5.eq.1) then
                 much(player)=much(player)+
     &                        van(player,d)*data(player,d,5)
c                 print*,van(player,d)*data(player,d,5)
               endif
c 
c large ships SM
               if (t6.eq.1) then
                 much(player)=much(player)+
     &                        van(player,d)*data(player,d,3)
c                 print*,van(player,d)*data(player,d,3)
               endif 
             endif
20         continue
c           print*,'Much ',player,much(player)
10       continue

         return
         end





c
c subroutine to calcuate the numerical value of a ansi character
c
       subroutine atoi(string,value)
c
       character*80 string
       integer value
       integer i
c
       i = 0            
       value = 0
10     continue
       i=i+1
       if (string(i:i).eq.' ') goto 20
       value = value*10 + ichar(string(i:i))-48

20     continue
       return
       end


