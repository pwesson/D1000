       program extract
c
c Load in the turnsheet instructions
c
       character*80 line
       character*80 info(20)
       character*80 party(20)
       integer a,start
       integer count
       integer value
       integer takesystem
       integer retreat
       integer system
       integer van(5),res1(5)

c
c       print '($,2a)',char(22),char(12) 
c
       a= 98
       print*,char(a)

       print*,ichar('b')
c
       open(unit=20,file='orders',status='old')
       
10     continue
       read(unit=20,fmt='(a80)') line
       
       if (line(1:6).eq.'combat') then
            print*,' '
            print*,' '
            print*,line(1:79)
c
c okay want to extract all the information I can
c
             count=0
             i=0
30           continue
                 i=i+1
                 if (line(i:i).gt.' ') then
                    count=count+1
                    start=i
                    value=ichar(line(i:i))-48
40                  continue 
                    i=i+1
                    if (line(i:i).eq.' ') then
                          info(count)=line(start:i-1)

                          if (count.eq.2) then
                             system=value
                          endif
                  
                          if (count.eq.5) then
                             van(1)=value
                          endif
                          if (count.eq.6) then
                             van(2)=value
                          endif
                          if (count.eq.7) then
                             van(3)=value
                          endif
                          if (count.eq.8) then
                             van(4)=value
                          endif
                          if (count.eq.9) then
                             van(5)=value
                          endif

                          if (count.eq.10) then
                             res1(1)=value
                          endif
                          if (count.eq.11) then
                             res1(2)=value
                          endif
                          if (count.eq.12) then
                             res1(3)=value
                          endif
                          if (count.eq.13) then
                             res1(4)=value
                          endif
                          if (count.eq.14) then
                             res1(5)=value 
                          endif

                          if (count.eq.15) then
                             retreat=value
                          endif
                          if (count.eq.16) then
                             takesystem=value 
                          endif


                          goto 30
                    else
                        value=value*10+ichar(line(i:i))-48
                    endif
                    goto 40
                 endif
                 if (i.ne.80) goto 30
c                 print*,count
                  print*,' ' 
                  print*,'Fighting at system = ',system
                  print*,'The vanguard is : ',
     &                   van(1),van(2),van(3),van(4),van(5)
                  print*,'First reserve is: ',
     &                   res1(1),res1(2),res1(3),res1(4),res1(5)
                  print*,'Retreat factor = ',retreat
                  print*,'Take system    = ',takesystem
c
c okay lets work out who is on the attack list
c
        numgroups=0

        i=0
        start=1
100     continue
        i=i+1
        if (info(3)(i:i).eq.'0') goto 110
        if (info(3)(i:i).eq.',') then           
             numgroups=numgroups+1
             party(numgroups)=info(3)(start:i-1)
             start=i+1
        endif
        goto 100
110     continue

        print*,'Number of groups = ',numgroups
        do 120 k=1,numgroups
c              print*,k,'  ',party(k)
               print*,' '
              i=0
              start=1
200           continue
              i=i+1
              if (party(k)(i:i).eq.' ') then
                  houses=houses+1
                  call atoi( party(k)(start:i-1),value)
                  print*,value
                  goto 210
              endif
                                   
              if (party(k)(i:i).eq.'=') then           
                  houses=houses+1
                  call atoi( party(k)(start:i-1),value)
                  print*,value
                  start=i+1
              endif
              goto 200
210           continue
120     continue

         
  
        endif

c
c If not at the end of the file then continue
c
       if (line(1:1).ne.'.') goto 10

20     continue
       
       stop
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


