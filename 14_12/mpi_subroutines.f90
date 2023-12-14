MODULE MPI_SUBROUTINES

        IMPLICIT NONE
PRIVATE
        DOUBLE PRECISION,PUBLIC,SAVE :: dummy
        INTEGER,PUBLIC,SAVE :: icount, iroot &
        ,IFINISH, MPInumprocs,nummpitask_tot,idummy,islavetasks  
       INTEGER,PUBLIC,SAVE :: MPImyidmax,MPImaxnum
       INTEGER,PUBLIC,SAVE :: ipara_ini_tot, ipara_fin_tot, ipara_ini, ipara_fin
        !!!
        !!! maximum number of processors available
        !!!
        INTEGER,PARAMETER,PUBLIC :: nummpiprocsm=10000000
        INTEGER,DIMENSION(0:nummpiprocsm),PUBLIC,SAVE :: isendcounts
        INTEGER,DIMENSION(1:2,0:nummpiprocsm),PUBLIC,SAVE :: ipara_ini_fin

PUBLIC :: mpi_begin
PUBLIC :: mpi_getdim
PUBLIC :: mpi_distrbt

       CONTAINS

       SUBROUTINE MPI_BEGIN !(MPImyid,MPInumprocs)
        USE GLOBAL_MPI_VAR,   ONLY: MPImyid,dir,fext
        IMPLICIT NONE
        !INTEGER,INTENT(OUT)::MPImyid,MPInumprocs
        INCLUDE 'mpif.h'
        LOGICAL ::ex1
        INTEGER :: ierr
        
        !dir = ''
        !fext = '.txt'
        !INQUIRE(FILE='mpi_begin.txt',exist=ex1)
        !IF(.NOT.ex1)THEN
        !  OPEN(UNIT=10,FILE='mpi_begin.txt',status='unknown')
        !ELSE
        !  OPEN(UNIT=10,FILE='mpi_begin.txt',status='old',access='append')
        !ENDIF          
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!c
!c THIS IS EXECUTED BY ALL PROCESSORS
!c
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!c
!c     IBM SP SPTOOLS system statistics on Seaborg
!c      call system_stats()
!c
         CALL MPI_INIT( ierr )
         CALL MPI_COMM_RANK( MPI_COMM_WORLD, MPImyid, ierr )
         CALL MPI_COMM_SIZE( MPI_COMM_WORLD, MPInumprocs, ierr )
!c      
         !WRITE(10,'(A,I5,A,I10,A)')'Processor # ', MPImyid, ' of ' &
         ! , MPInumprocs, ' is alive and kicking'
         IF (MPInumprocs > nummpiprocsm ) THEN
            WRITE(*,'(4x,A)')'Too many MPI processors, increase'
            WRITE(*,'(4x,A)')'nummpiprocsm in mpi_variables.inc'
            WRITE(*,'(4x,I5,I5)') MPInumprocs, nummpiprocsm
            CALL MPI_FINALIZE( IFINISH )
            STOP
        ENDIF
        ! CLOSE(10)
        RETURN
        END SUBROUTINE
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        SUBROUTINE MPI_GETDIM(max_ipara,ipara_ini,ipara_fin,MPImaxnum)
        USE GLOBAL_MPI_VAR,   ONLY: dir,fext,MPImyid
        IMPLICIT NONE
        INTEGER,INTENT(IN):: max_ipara !,MPImyid, MPInumprocs
        INTEGER,INTENT(OUT):: ipara_ini, ipara_fin, MPImaxnum
        INCLUDE 'mpif.h'
        LOGICAL ::ex1
        INTEGER :: ierr, dummy,ii
        !!!
        !INQUIRE(file=TRIM(dir)//'mpi_getdim'//fext,exist=ex1)
        !IF(.NOT.ex1)THEN
        !  OPEN(UNIT=10,file=TRIM(dir)//'mpi_getdim'//fext,status='unknown')
        !ELSE
        OPEN(UNIT=10,file=TRIM(dir)//'mpi_getdim'//fext,status='old',access='append')
        !ENDIF
        !!!
        IF(MPImyid.eq.0)then
           WRITE(10,'(A,I4,A)')' Master process # ',MPImyid,' starting'
           !!!        
           CALL MPI_DISTRBT(max_ipara,ipara_ini_fin)
           !!!
           !!! Find a processor who is doing most job
           MPImyidmax = 0
           dummy = ipara_ini_fin(2,0)-ipara_ini_fin(1,0) + 1
           DO ii = 1,MPInumprocs-1
              IF((ipara_ini_fin(2,ii)-ipara_ini_fin(1,ii) + 1)>dummy)THEN
                  dummy = ipara_ini_fin(2,ii)-ipara_ini_fin(1,ii) + 1
                  MPImyidmax = ii
              ENDIF
           ENDDO
        ENDIF ! master processor selection
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!c
!c THIS IS EXECUTED BY ALL (MASTER+SLAVE) PROCESSORS
!c
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        WRITE(10,'(A,I5,A)')' Slave process # ',MPImyid,' starting'
!c      blocking receive of integer data
        icount=2*MPInumprocs
        iroot=0
        !CALL MPI_COMM_RANK( MPI_COMM_WORLD, MPImyid, ierr )
        CALL MPI_BCAST(ipara_ini_fin, SIZE(ipara_ini_fin), MPI_INTEGER, iroot,MPI_COMM_WORLD, ierr)
!c       call the serial version
        ipara_ini=ipara_ini_fin(1,MPImyid)
        ipara_fin=ipara_ini_fin(2,MPImyid)
        MPImaxnum = MAXVAL(ipara_ini_fin(2,:)-ipara_ini_fin(1,:)+1)
        WRITE(10,'(A,I5,A,I10,A,I10)')'Slave # ',MPImyid, &
           ' computing from ipara ',ipara_ini,' to ',ipara_fin
        close(10)
        return
       end subroutine
!c!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c           DISTRIBUTING THE PROGRAM
!c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
       SUBROUTINE MPI_DISTRBT(max_ipara,ipara_ini_fin)
                 !cc This program is done by Sepp, I have seperate it to a subroutine
                 USE GLOBAL_MPI_VAR,   ONLY: dir,fext,MPImyid
                 IMPLICIT NONE 
                 INTEGER,INTENT(IN):: max_ipara !,MPInumprocs
                 INTEGER, DIMENSION(1:2,0:nummpiprocsm) ,INTENT(OUT):: ipara_ini_fin                                      
                 INCLUDE 'mpif.h'
                 LOGICAL::ex1
                 INTEGER::MPInumprocs1, ii, ierr
                  !!!
                  !INQUIRE(file=TRIM(dir)//'mpi_distrbt'//fext,exist=ex1)
                  !if(.not.ex1)then
                  !  OPEN(UNIT=11,file=TRIM(dir)//'mpi_distrbt'//fext,status='unknown')
                  !else
                  OPEN(UNIT=11,file=TRIM(dir)//'mpi_distrbt'//fext,status='old',access='append')
                  !endif  
                  !!! THESE ARE THE START AND FINISH VALUES OF OLD LOOP, NOW PARALLELIZED
                  !!!
                  ipara_ini_tot=1
                  ipara_fin_tot=max_ipara
                  WRITE(11,'(A,I10,A,I10)')' ipara ranging from ', &
                    ipara_ini_tot,' to ',ipara_fin_tot
                  !broadcast tasks to slaves
                  WRITE(11,'(A)')' '
                  nummpitask_tot=ipara_fin_tot-ipara_ini_tot+1
                  WRITE(11,'(A,I10,A)')'MPI MASTER: Scattering total of ',nummpitask_tot, &
                  ' tasks'
                  WRITE(11,'(A)')' '
                 if (nummpitask_tot < MPInumprocs ) then
                    WRITE(11,'(A)')'nummpitask_tot < MPInumprocs '
                    WRITE(11,'(A)')'Making nummpitask_tot = MPInumprocs '    
                    WRITE(11,'(3I10)')ipara_ini_tot, ipara_fin_tot, MPInumprocs
                    MPInumprocs1 = ipara_fin_tot
                 else
                    MPInumprocs1 =  MPInumprocs
                 endif   
        ipara_ini_fin(:,:) = 0           
        dummy=nummpitask_tot/MPInumprocs1
        islavetasks=floor(dummy)+1
        isendcounts(0)=nummpitask_tot-(MPInumprocs1-1)*islavetasks
       
           
        if ( isendcounts(0) > 0 ) then
           ipara_ini_fin(1,0)=ipara_ini_tot
           ipara_ini_fin(2,0)=ipara_ini_tot+isendcounts(0)-1
           do ii=1,(MPInumprocs1-1)
              isendcounts(ii)=islavetasks
              ipara_ini_fin(1,ii)=ipara_ini_fin(2,ii-1)+1
              ipara_ini_fin(2,ii)=ipara_ini_fin(1,ii)+isendcounts(ii)-1
           end do
           
           WRITE(*,'(10x,A,1x,I15,1x,A,1x,I15,1x,A)') &
            'Total No. of points',max_ipara,'are distributed over',MPInumprocs,'processors.' 
           WRITE(*,'(10x,I15,A)')isendcounts(0),' tasks for the master'
           WRITE(*,'(10x,I15,A,I15,A)')isendcounts(1),' tasks for each of the ', (MPInumprocs1-1),' slaves'
        else
!c     if number of tasks is only slightly larger than MPInumprocs
           islavetasks=floor(dummy)
           isendcounts(0)=islavetasks
           ipara_ini_fin(1,0)=ipara_ini_tot
           ipara_ini_fin(2,0)=ipara_ini_tot+isendcounts(0)-1
           idummy=nummpitask_tot-(MPInumprocs1)*islavetasks
           do ii=1,(MPInumprocs1-1)
              isendcounts(ii)=islavetasks
              if ( ii <= idummy ) then
                 isendcounts(ii)=isendcounts(ii)+1
              end if
              ipara_ini_fin(1,ii)=ipara_ini_fin(2,ii-1)+1
              ipara_ini_fin(2,ii)=ipara_ini_fin(1,ii)+isendcounts(ii)-1
           end do
           WRITE(*,'(10x,A,1x,I15,1x,A,1x,I15,1x,A)') &
              'Total No. of points ',max_ipara,' are distributed over ',MPInumprocs,' processors.'
           WRITE(*,'(10x,I15,A)') isendcounts(0),' tasks for the master'
            IF(idummy>0)WRITE(*,'(10x,I15,A,I15,A)')isendcounts(1),' tasks for each of the first ',idummy,' slaves and'
            WRITE(*,'(10x,I15,A,I15,A)')isendcounts(idummy+1),' tasks for each of the other', &
                                              (MPInumprocs1-1)-idummy,' slaves'
          end if
!c     
          close(11)     
          return
          end subroutine
!c
         END MODULE
