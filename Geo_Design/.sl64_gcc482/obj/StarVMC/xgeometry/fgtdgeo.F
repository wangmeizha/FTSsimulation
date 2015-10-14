C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
* $Id: fgtdgeo.g,v 1.7 2007/11/13 21:30:03 perev Exp $
* $Log: fgtdgeo.g,v $
* Revision 1.7  2007/11/13 21:30:03  perev
* material ALKAP fixed
*
* Revision 1.6  2005/04/19 19:39:22  potekhin
* Minor naming convention fix, in the attribute name
*
* Revision 1.5  2005/04/18 23:26:54  potekhin
* As Kai pointed out, need to change the FGAR
* hist defintition to something else, and FGSC fits
* teh convention
*
* Revision 1.4  2005/04/15 14:53:14  potekhin
* Add hit description to the FGT detector
*
* Revision 1.3  2005/03/31 18:34:25  potekhin
* Declare the sector of the ring a sensitive volume,
* and leave comment to that effect
*
* Revision 1.2  2005/02/19 05:14:27  potekhin
* Roughly, it's done according to Bernd's powerpoint specs.
* Things they might want to add:
* a) rotation around Z by 90 (orientation not fixed in the
* design I assume)
* b) Improve kapton deifnition (immaterial)
* c) Add hits (ask Kai)
*
* Revision 1.1  2005/02/19 04:12:20  potekhin
* Waypoint check-in: created the first cut with
* simplified geometry of the Forward GEM tracker.
* The rings and sectors are in place, but layers
* aren't yet.
*
******************************************************************************
      SUBROUTINE FGTDGEO                                                  36
C FGTDGEO IS THE GEOMETRY OF THE FORWARD GEM TRACKING DETECTOR            36
      IMPLICIT NONE                                                       36
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       36
      DATA MODULE/'FGTDGEO'/, BANK_TITLE/'*'/                             36
      DATA MCOMMENT/'IS THE GEOMETRY OF THE FORWARD GEM TRACKING DETECTO  36
     *R'/                                                                 36
      LOGICAL FIRST                                                       36
      DATA FIRST/.TRUE./                                                  36
      SAVE FIRST                                                          36
      INTEGER JUMPAD                                                      36
      EXTERNAL JUMPAD                                                     36
      INTEGER OK                                                          36
      PARAMETER (OK=0)                                                    36
      INTEGER AG_I,AG_J                                                   36
      SAVE AG_I,AG_J                                                      36
C CREATED 02/18/05                                                        36
      DATA AG_I/0/,AG_J/0/                                                37
      CHARACTER*40 CREATED /'02/18/05'/                                   37
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  37
C AUTHOR MAXIM POTEKHIN                                                   37
      CHARACTER*40 AUTHOR /'MAXIM POTEKHIN'/                              38
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  38
******************************************************************************
C - Advanced Geant interface                                              40
      CHARACTER*20 AG_MODULE,AG_TITLE,AG_EXNAME,AG_PARLIST,AG_CHDIR,      40
     *AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM                       40
      CHARACTER*4 AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,         40
     *AG_OPTION,AG_ORT,AG_MARK                                            40
      INTEGER AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,         40
     *AG_IMED,AG_IMAT,AG_IFIELD, AG_NLMAT,AG_IERROR,AG_NWBUF,AG_NPAR,     40
     *AG_ISTATUS,AG_IROT,AG_NBITS,AG_SERIAL, AG_ATTRIBUTE(6),AG_WORK,     40
     *AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,AG_JDU,AG_ORTI, AG_NDIV,    40
     *AG_IAXIS,AG_NDVMAX,AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,          40
     *AG_ENDSCR, AG_IDTYPE,AG_ISET,AG_IDET,AG_ISVOL,AG_LEVEL,AG_IRESER,   40
     *AG_IGEOM,AG_STANDALONE, AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,     40
     *AG_LDETU,AG_NPDIV,AG_NPDV,AG_NZ,AG_NCOPY, AG_IDEBU,AG_IGRAP,        40
     *AG_IHIST,AG_IMFLD,AG_ISIMU,AG_ENDSAVE,AG_CODE,AG_TRKTYP, AG_MODE,   40
     *AG_ECODE,AG_PDG,IPRIN, AG_ALPHAN,AG_ALPHAK(10),IGOR                 40
      REAL AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,AG_STMIN,     40
     *AG_DENS,AG_RADL,AG_ABSL, AG_THETAX,AG_PHIX,AG_THETAY,AG_PHIY,       40
     *AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ, AG_ALPHAL(10),        40
     *AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ,AG_TYPE, AG_DX, AG_DX1, AG_DX2,       40
     *AG_DY, AG_DY1, AG_DY2, AG_DMAXMS, AG_TWIST, AG_THET, AG_THE1,       40
     *AG_THE2, AG_PHI, AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2,       40
     *AG_RMIN, AG_RMAX, AG_RMN, AG_RMX, AG_ZI , AG_RMN1, AG_RMN2,         40
     *AG_RMX1, AG_RMX2, AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,     40
     *AG_DPHI, AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,       40
     *AG_STEP, AG_C0, AG_PAR, AG_AA, AG_ZZ, AG_WW, AG_LX, AG_LY, AG_LZ,   40
     *AG_HX, AG_HY, AG_HZ, AG_P1, AG_P2, AG_STACK,AG_UBUF,AG_XHMAX,       40
     *AG_YHMAX,AG_ZHMAX,AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN, AG_CHARGE,     40
     *AG_MASS,AG_TLIFE,AG_BRATIO                                          40
      PARAMETER (AG_LSTACK=130,AG_NWUHIT=10,AG_NWUVOL=3,AG_MAGIC=         40
     *-696969,AG_LDETU=250)                                               40
      COMMON/AGCGLOB/ AG_MODULE,AG_CHDIR,AG_LEVEL,AG_IDTYPE,AG_IERROR,    40
     *AG_STANDALONE,IPRIN, AG_IPRIN,AG_IGEOM,AG_IDEBU,AG_IGRAP,AG_IHIST,  40
     *AG_IMFLD,AG_ISIMU                                                   40
C Inherited variables a: saved during internal calls                      40
C b: reset at the entry in a block and then saved                         40
      COMMON/AGCPARA/ AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,        40
     *AG_ISHAPE, AG_IMED, AG_IMAT, AG_IFIELD, AG_FIELDM, AG_TMAXFD,       40
     *AG_STEMAX, AG_DEEMAX, AG_EPSIL, AG_STMIN, AG_DENS, AG_RADL,         40
     *AG_ABSL, AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1, AG_DY2, AG_RMN1,     40
     *AG_RMN2, AG_RMX1, AG_RMX2, AG_THET, AG_THE1, AG_THE2, AG_PHI,       40
     *AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2, AG_H1, AG_BL1,         40
     *AG_TL1, AG_H2, AG_BL2, AG_TL2, AG_RMIN, AG_RMAX, AG_DPHI,           40
     *AG_NPDIV, AG_NZ, AG_DZ, AG_TWIS, AG_LX, AG_LY, AG_LZ, AG_HX,        40
     *AG_HY, AG_HZ, AG_A, AG_ZA, AG_W, AG_NLMAT, AG_WORK, AG_SEEN,        40
     *AG_LSTY, AG_LWID, AG_COLO, AG_FILL, AG_SERIAL, AG_ISVOL,            40
     *AG_ISTATUS, AG_ZI(16), AG_RMN(16), AG_RMX(16), AG_VOLUME,           40
     *AG_MOTHER, AG_SHAPE, AG_CNICK, AG_ENDSAVE, AG_RESET1, AG_THETAX,    40
     *AG_PHIX,AG_THETAY,AG_PHIY,AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,      40
     *AG_ALFAZ, AG_X,AG_Y,AG_Z, AG_STEP,AG_C0,AG_NDIV,AG_IAXIS,           40
     *AG_NDVMAX,AG_ORTI,AG_NCOPY, AG_ALPHAN,AG_ALPHAK,AG_ALPHAL,          40
     *AG_RESET2, AG_KONLY, AG_ORT, AG_MARK                                40
C local variables valid inside same block                                 40
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       40
     *AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  40
     *AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         40
     *AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    40
     *AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          40
     *AG_OPTION                                                           40
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,        40
     *AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                             40
      COMMON/AGCSTAC/ AG_STACK(AG_LSTACK,15)                              40
C temporarly                                                              40
      EQUIVALENCE (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),           40
     *(AG_ALPHAX,AG_ALFAX), (AG_ALPHAY,AG_ALFAY),(AG_ALPHAZ,AG_ALFAZ),    40
     *(AG_TWIST,AG_TWIS),(AG_NPDIV,AG_NPDV),  (AG_P1,AG_HX),(AG_P2,       40
     *AG_HY)                                                              40
      REAL MIL_P,INCH_P                                                   40
      PARAMETER (INCH_P=2.54,MIL_P=0.00254)                               40
C - GEANT description of logical units                                    40
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            40
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      40
      COMMON/GCMAIL/ CHMAIL                                               40
      CHARACTER*132 CHMAIL                                                40
      REAL RADDEG, CENTER, DR, R1, R2, DPHI                               42
      INTEGER IXRING,IXSEC, NS                                            43
      INTEGER BLOCK_FGMO                                                  45
      INTEGER BLOCK_FGAM                                                  45
      INTEGER BLOCK_FGRN                                                  45
      INTEGER BLOCK_FGSC                                                  45
      INTEGER BLOCK_FGGT                                                  45
      INTEGER BLOCK_FGKP                                                  45
C -                                                                       47
      REAL FGTG_VERSION                                                   47
      EQUIVALENCE (BANK_FGTG(3),FGTG_VERSION,RE_FGTG_VERSION),            47
     *(NAMES_FGTG(1),NFGTG_VERSION)                                       47
      EQUIVALENCE (COMMEN_FGTG(1),CFGTG_VERSION),(MAP_FGTG(1,1),          47
     *MFGTG_VERSION)                                                      47
      CHARACTER CFGTG_VERSION*40/' '/                                     47
      CHARACTER NFGTG_VERSION*12/'R.VERSION'/                             47
      INTEGER MFGTG_VERSION(2)/1,1/                                       47
      REAL RE_FGTG_VERSION                                                47
C -                                                                       47
      REAL FGTG_NRING                                                     47
      EQUIVALENCE (BANK_FGTG(4),FGTG_NRING,RE_FGTG_NRING),                47
     *(NAMES_FGTG(2),NFGTG_NRING)                                         47
      EQUIVALENCE (COMMEN_FGTG(2),CFGTG_NRING),(MAP_FGTG(1,2),            47
     *MFGTG_NRING)                                                        47
      CHARACTER CFGTG_NRING*40/' '/                                       47
      CHARACTER NFGTG_NRING*12/'R.NRING'/                                 47
      INTEGER MFGTG_NRING(2)/1,1/                                         47
      REAL RE_FGTG_NRING                                                  47
C -                                                                       47
      REAL FGTG_THK                                                       47
      EQUIVALENCE (BANK_FGTG(5),FGTG_THK,RE_FGTG_THK),(NAMES_FGTG(3),     47
     *NFGTG_THK)                                                          47
      EQUIVALENCE (COMMEN_FGTG(3),CFGTG_THK),(MAP_FGTG(1,3),MFGTG_THK)    47
      CHARACTER CFGTG_THK*40/' '/                                         47
      CHARACTER NFGTG_THK*12/'R.THK'/                                     47
      INTEGER MFGTG_THK(2)/1,1/                                           47
      REAL RE_FGTG_THK                                                    47
C -                                                                       47
      REAL FGTG_ZEND                                                      47
      EQUIVALENCE (BANK_FGTG(6),FGTG_ZEND,RE_FGTG_ZEND),(NAMES_FGTG(4),   47
     *NFGTG_ZEND)                                                         47
      EQUIVALENCE (COMMEN_FGTG(4),CFGTG_ZEND),(MAP_FGTG(1,4),             47
     *MFGTG_ZEND)                                                         47
      CHARACTER CFGTG_ZEND*40/' '/                                        47
      CHARACTER NFGTG_ZEND*12/'R.ZEND'/                                   47
      INTEGER MFGTG_ZEND(2)/1,1/                                          47
      REAL RE_FGTG_ZEND                                                   47
C -                                                                       47
      REAL FGTG_G10THK                                                    47
      EQUIVALENCE (BANK_FGTG(7),FGTG_G10THK,RE_FGTG_G10THK),              47
     *(NAMES_FGTG(5),NFGTG_G10THK)                                        47
      EQUIVALENCE (COMMEN_FGTG(5),CFGTG_G10THK),(MAP_FGTG(1,5),           47
     *MFGTG_G10THK)                                                       47
      CHARACTER CFGTG_G10THK*40/' '/                                      47
      CHARACTER NFGTG_G10THK*12/'R.G10THK'/                               47
      INTEGER MFGTG_G10THK(2)/1,1/                                        47
      REAL RE_FGTG_G10THK                                                 47
C -                                                                       47
      REAL FGTG_ARTHK                                                     47
      EQUIVALENCE (BANK_FGTG(8),FGTG_ARTHK,RE_FGTG_ARTHK),                47
     *(NAMES_FGTG(6),NFGTG_ARTHK)                                         47
      EQUIVALENCE (COMMEN_FGTG(6),CFGTG_ARTHK),(MAP_FGTG(1,6),            47
     *MFGTG_ARTHK)                                                        47
      CHARACTER CFGTG_ARTHK*40/' '/                                       47
      CHARACTER NFGTG_ARTHK*12/'R.ARTHK'/                                 47
      INTEGER MFGTG_ARTHK(2)/1,1/                                         47
      REAL RE_FGTG_ARTHK                                                  47
C -                                                                       47
      REAL FGTG_KAPTONTHK                                                 47
      EQUIVALENCE (BANK_FGTG(9),FGTG_KAPTONTHK,RE_FGTG_KAPTONTHK),        47
     *(NAMES_FGTG(7),NFGTG_KAPTONTHK)                                     47
      EQUIVALENCE (COMMEN_FGTG(7),CFGTG_KAPTONTHK),(MAP_FGTG(1,7),        47
     *MFGTG_KAPTONTHK)                                                    47
      CHARACTER CFGTG_KAPTONTHK*40/' '/                                   47
      CHARACTER NFGTG_KAPTONTHK*12/'R.KAPTONTHK'/                         47
      INTEGER MFGTG_KAPTONTHK(2)/1,1/                                     47
      REAL RE_FGTG_KAPTONTHK                                              47
C -                                                                       47
      REAL FGTG_RMIN                                                      47
      EQUIVALENCE (BANK_FGTG(10),FGTG_RMIN,RE_FGTG_RMIN),(NAMES_FGTG(8),  47
     *NFGTG_RMIN)                                                         47
      EQUIVALENCE (COMMEN_FGTG(8),CFGTG_RMIN),(MAP_FGTG(1,8),             47
     *MFGTG_RMIN)                                                         47
      CHARACTER CFGTG_RMIN*40/' '/                                        47
      CHARACTER NFGTG_RMIN*12/'R.RMIN'/                                   47
      INTEGER MFGTG_RMIN(2)/1,1/                                          47
      REAL RE_FGTG_RMIN                                                   47
C -                                                                       47
      REAL FGTG_RMAX                                                      47
      EQUIVALENCE (BANK_FGTG(11),FGTG_RMAX,RE_FGTG_RMAX),(NAMES_FGTG(9),  47
     *NFGTG_RMAX)                                                         47
      EQUIVALENCE (COMMEN_FGTG(9),CFGTG_RMAX),(MAP_FGTG(1,9),             47
     *MFGTG_RMAX)                                                         47
      CHARACTER CFGTG_RMAX*40/' '/                                        47
      CHARACTER NFGTG_RMAX*12/'R.RMAX'/                                   47
      INTEGER MFGTG_RMAX(2)/1,1/                                          47
      REAL RE_FGTG_RMAX                                                   47
C -                                                                       47
      REAL FGTG_NSECTORS(5)                                               47
      EQUIVALENCE (BANK_FGTG(12),FGTG_NSECTORS,RE_FGTG_NSECTORS),         47
     *(NAMES_FGTG(10),NFGTG_NSECTORS)                                     47
      EQUIVALENCE (COMMEN_FGTG(10),CFGTG_NSECTORS),(MAP_FGTG(1,10),       47
     *MFGTG_NSECTORS)                                                     47
      CHARACTER CFGTG_NSECTORS*40/' '/                                    47
      CHARACTER NFGTG_NSECTORS*12/'R.NSECTORS'/                           47
      INTEGER MFGTG_NSECTORS(2)/5,1/                                      47
      REAL RE_FGTG_NSECTORS                                               47
C -                                                                       47
      INTEGER LEN_FGTG,LLEN_FGTG,LINK_FGTG                                47
      PARAMETER (LEN_FGTG=16)                                             47
      PARAMETER (LLEN_FGTG=10)                                            47
      INTEGER FLAG_FGTG/0/,MAP_FGTG(2,LLEN_FGTG),IDNUM_FGTG(10)           47
      CHARACTER NAMES_FGTG(LLEN_FGTG)*12,COMMEN_FGTG(LLEN_FGTG)*40        47
      DATA IDNUM_FGTG/10*0/                                               47
      REAL BANK_FGTG(LEN_FGTG)                                            47
      CHARACTER*60 BPATH_FGTG/' '/                                        47
      SAVE FLAG_FGTG,MAP_FGTG,IDNUM_FGTG,NAMES_FGTG,COMMEN_FGTG,          47
     *BPATH_FGTG                                                          47
      DATA LINK_FGTG/0/,BANK_FGTG/LEN_FGTG*0/                             47
C *                                                                       47
*
* -----------------------------------------------------------------------------
*
C *                                                                       51
      CALL AGSBEGM(MODULE,IPRIN)                                          51
      IF (IPRIN.LT.0) RETURN                                              51
C Filling bank FGTG - FORWARD GEM TRACKER GEOMETRY DATA                   51
      BANK_TITLE='FORWARD GEM TRACKER GEOMETRY DATA'                      51
      BPATH_FGTG='FGTG*'                                                  51
      IDNUM_FGTG(1)=0                                                     52
C VERSION OF THE TRACKER                                                  52
      FGTG_VERSION=1                                                      52
      CFGTG_VERSION='VERSION OF THE TRACKER'                              52
C NUMBER OF RINGS                                                         53
      FGTG_NRING=5                                                        53
      CFGTG_NRING='NUMBER OF RINGS'                                       53
C THICKNESS                                                               54
      FGTG_THK=2.05                                                       54
      CFGTG_THK='THICKNESS'                                               54
C WHERE IS ENDS                                                           55
      FGTG_ZEND=268.0                                                     55
      CFGTG_ZEND='WHERE IS ENDS'                                          55
C G10 THICKNESS                                                           56
      FGTG_G10THK=0.5                                                     56
      CFGTG_G10THK='G10 THICKNESS'                                        56
C ARGON THICKNESS                                                         57
      FGTG_ARTHK=1.0                                                      57
      CFGTG_ARTHK='ARGON THICKNESS'                                       57
C KAPTON THICKNESS                                                        58
      FGTG_KAPTONTHK=0.05                                                 58
      CFGTG_KAPTONTHK='KAPTON THICKNESS'                                  58
C INNER RADIUS                                                            60
      FGTG_RMIN=75.0                                                      60
      CFGTG_RMIN='INNER RADIUS'                                           60
C OUTER RADIUS                                                            61
      FGTG_RMAX=200.0                                                     61
      CFGTG_RMAX='OUTER RADIUS'                                           61
C SECTORS IN A RING                                                       63
      FGTG_NSECTORS(1)=24                                                 63
      FGTG_NSECTORS(2)=30                                                 63
      FGTG_NSECTORS(3)=36                                                 63
      FGTG_NSECTORS(4)=42                                                 63
      FGTG_NSECTORS(5)=50                                                 63
      CFGTG_NSECTORS='SECTORS IN A RING'                                  63
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FGTG',    64
     *BPATH_FGTG,IDNUM_FGTG,1, MAP_FGTG,NAMES_FGTG,COMMEN_FGTG,           64
     *BANK_FGTG,LEN_FGTG,LLEN_FGTG,LINK_FGTG,FLAG_FGTG)                   64
C *                                                                       64
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FGTG',    64
     *BPATH_FGTG,IDNUM_FGTG,1, MAP_FGTG,NAMES_FGTG,COMMEN_FGTG,           64
     *BANK_FGTG,LEN_FGTG,LLEN_FGTG,LINK_FGTG,FLAG_FGTG)                   64
*
******************************************************
C use default FGTG bank                                                   69
      BPATH_FGTG='FGTG*'                                                  69
      IDNUM_FGTG(1)=0                                                     69
      CALL AGDGETP('FGTDGEO','*',-999,'FGTG',' ',BANK_FGTG, BPATH_FGTG,   69
     *IDNUM_FGTG,1,MAP_FGTG,NAMES_FGTG,BANK_FGTG,LEN_FGTG,LLEN_FGTG,      69
     *LINK_FGTG,FLAG_FGTG)                                                69
      RADDEG=3.14159265/180.0                                             71
      DR = (FGTG_RMAX-FGTG_RMIN)/FGTG_NRING                               73
*      angle=360.0/FSTG_Nsec ! opening angle of the sector
      CENTER = FGTG_ZEND-FGTG_THK/2.0                                     76
      CALL AGSBEGM(MODULE,IPRIN)                                          78
C Check IPRIN<0                                                           78
      IF (IPRIN.LT.0) RETURN                                              78
C Create FGMO :                                                           78
C Call To Local Procedure FGMO                                            78
      ASSIGN 5020 TO BLOCK_FGMO                                           78
      GO TO  5010                                                         78
5020  CONTINUE                                                            78
C *                                                                       78
C Geant POSITION : FGMO ===> MOTHER='CAVE' Z=+CENTER <===                 79
      CALL AGSRESET                                                       79
      AG_MOTHER = 'CAVE'                                                  79
      AG_Z = +CENTER                                                      79
      AG_TITLE = 'FGMO'                                                   79
      AG_EXNAME = 'POSITION'                                              79
      AG_PARLIST = 'MOTHER_Z'                                             79
      CALL AXPOSITION                                                     79
C *                                                                       79
* -----------------------------------------------------------------------------
      CALL AGSENDM                                                        82
      RETURN                                                              82
C Begin Local Procedure FGMO                                              82
C FGMO IS THE MOTHER OF THE FORWARD GEM DETECTOR                          82
5010  CONTINUE                                                            82
         AG_TITLE='FGMO'                                                  82
         AG_MODULE=MODULE                                                 82
         AG_MARK='V00'                                                    82
         CALL AGSPUSH                                                     82
         AG_MODULE=MODULE                                                 82
C    *                                                                    82
C    Geant MATERIAL : AIR ===> inherited arguments used <===              83
         CALL AGSRESET                                                    83
         AG_TITLE = 'AIR'                                                 83
         AG_EXNAME = 'MATERIAL'                                           83
         AG_PARLIST = 'NONE'                                              83
         CALL AXMATERIAL                                                  83
C    *                                                                    83
C    Geant ATTRIBUTE : FGMO ===> SEEN=1 COLO=6 <===                       83
         CALL AGSRESET                                                    84
         AG_SEEN = 1                                                      84
         AG_COLO = 6                                                      84
         AG_TITLE = 'FGMO'                                                84
         AG_EXNAME = 'ATTRIBUTE'                                          84
         AG_PARLIST = 'SEEN_COLO'                                         84
         CALL AXATTRIBUTE                                                 84
C    *                                                                    84
C    Geant SHAPE : TUBE ===> RMIN=FGTG_RMIN RMAX=FGTG_RMAX DZ=FGTG_THK/2  86
C    .0 <===                                                              86
         CALL AGSRESET                                                    86
         AG_RMIN = FGTG_RMIN                                              86
         AG_RMAX = FGTG_RMAX                                              86
         AG_DZ = FGTG_THK/2.0                                             86
         AG_TITLE = 'TUBE'                                                86
         AG_EXNAME = 'SHAPE'                                              86
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      86
         CALL AXSHAPE                                                     86
C    *                                                                    86
C    Create FGGT :                                                        89
C    Call To Local Procedure FGGT                                         89
         ASSIGN 5040 TO BLOCK_FGGT                                        89
         GO TO  5030                                                      89
5040     CONTINUE                                                         89
C    *                                                                    89
C    Geant POSITION : FGGT ===> Z=FGTG_THK/2-FGTG_G10THK/2 <===           90
         CALL AGSRESET                                                    90
         AG_Z = FGTG_THK/2-FGTG_G10THK/2                                  90
         AG_TITLE = 'FGGT'                                                90
         AG_EXNAME = 'POSITION'                                           90
         AG_PARLIST = 'Z'                                                 90
         CALL AXPOSITION                                                  90
C    *                                                                    90
C    Geant POSITION : FGGT ===> Z=-FGTG_THK/2+FGTG_G10THK/2 <===          90
         CALL AGSRESET                                                    91
         AG_Z = -FGTG_THK/2+FGTG_G10THK/2                                 91
         AG_TITLE = 'FGGT'                                                91
         AG_EXNAME = 'POSITION'                                           91
         AG_PARLIST = 'Z'                                                 91
         CALL AXPOSITION                                                  91
C    *                                                                    91
C    Create FGKP :                                                        93
C    Call To Local Procedure FGKP                                         93
         ASSIGN 5060 TO BLOCK_FGKP                                        93
         GO TO  5050                                                      93
5060     CONTINUE                                                         93
C    *                                                                    93
C    Geant POSITION : FGKP ===> Z=FGTG_THK/2-FGTG_G10THK-FGTG_KAPTONTHK/  93
C    2 <===                                                               93
         CALL AGSRESET                                                    93
         AG_Z = FGTG_THK/2-FGTG_G10THK-FGTG_KAPTONTHK/2                   93
         AG_TITLE = 'FGKP'                                                93
         AG_EXNAME = 'POSITION'                                           93
         AG_PARLIST = 'Z'                                                 93
         CALL AXPOSITION                                                  93
C    *                                                                    93
C    Create FGAM :                                                        94
C    Call To Local Procedure FGAM                                         94
         ASSIGN 5080 TO BLOCK_FGAM                                        94
         GO TO  5070                                                      94
5080     CONTINUE                                                         94
C    *                                                                    94
C    Geant POSITION : FGAM ===> Z=FGTG_THK/2-FGTG_G10THK-FGTG_KAPTONTHK-  94
C    FGTG_ARTHK/2 <===                                                    94
         CALL AGSRESET                                                    94
         AG_Z = FGTG_THK/2-FGTG_G10THK-FGTG_KAPTONTHK-FGTG_ARTHK/2        94
         AG_TITLE = 'FGAM'                                                94
         AG_EXNAME = 'POSITION'                                           94
         AG_PARLIST = 'Z'                                                 94
         CALL AXPOSITION                                                  94
C    *                                                                    94
5011  CALL AGSPOPB                                                        97
      GO TO BLOCK_FGMO, (5010 ,5020)                                      97
C End of Local Procedure                                                  97
* -----------------------------------------------------------------------------
C Begin Local Procedure FGAM                                              99
C FGAM IS THE MOTHER OF THE ARGON VOLUME                                  99
5070  CONTINUE                                                            99
         AG_TITLE='FGAM'                                                  99
         AG_MODULE=MODULE                                                 99
         AG_MARK='V00'                                                    99
         CALL AGSPUSH                                                     99
         AG_MODULE=MODULE                                                 99
C    *                                                                    99
C    Geant MATERIAL : AIR ===> inherited arguments used <===              100
         CALL AGSRESET                                                    100
         AG_TITLE = 'AIR'                                                 100
         AG_EXNAME = 'MATERIAL'                                           100
         AG_PARLIST = 'NONE'                                              100
         CALL AXMATERIAL                                                  100
C    *                                                                    100
C    Geant ATTRIBUTE : FGAM ===> SEEN=1 COLO=6 <===                       100
         CALL AGSRESET                                                    101
         AG_SEEN = 1                                                      101
         AG_COLO = 6                                                      101
         AG_TITLE = 'FGAM'                                                101
         AG_EXNAME = 'ATTRIBUTE'                                          101
         AG_PARLIST = 'SEEN_COLO'                                         101
         CALL AXATTRIBUTE                                                 101
C    *                                                                    101
C    Geant SHAPE : TUBE ===> RMIN=FGTG_RMIN RMAX=FGTG_RMAX DZ=FGTG_ARTHK  103
C    /2.0 <===                                                            103
         CALL AGSRESET                                                    103
         AG_RMIN = FGTG_RMIN                                              103
         AG_RMAX = FGTG_RMAX                                              103
         AG_DZ = FGTG_ARTHK/2.0                                           103
         AG_TITLE = 'TUBE'                                                103
         AG_EXNAME = 'SHAPE'                                              103
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      103
         CALL AXSHAPE                                                     103
C    *                                                                    103
C    *                                                                    103
         IXRING=1                                                         104
            GO TO 5093                                                    104
5091        IXRING=IXRING+1                                               104
5093        IF(IXRING-(FGTG_NRING).GT.0) GO TO 5092                       104
            R1=FGTG_RMIN+(IXRING-1)*DR                                    104
            R2=R1+DR                                                      105
            NS=FGTG_NSECTORS(IXRING)                                      106
            DPHI=360.0/NS                                                 107
C       Create FGRN :                                                     108
C       Call To Local Procedure FGRN                                      108
            ASSIGN 5110 TO BLOCK_FGRN                                     108
            GO TO  5100                                                   108
5110        CONTINUE                                                      108
C       *                                                                 108
C       Geant POSITION : FGRN ===> inherited arguments used <===          108
            CALL AGSRESET                                                 108
            AG_TITLE = 'FGRN'                                             108
            AG_EXNAME = 'POSITION'                                        108
            AG_PARLIST = 'NONE'                                           108
            CALL AXPOSITION                                               108
C       *                                                                 108
         GO TO 5091                                                       109
5092     CONTINUE                                                         109
5071  CALL AGSPOPB                                                        110
      GO TO BLOCK_FGAM, (5070 ,5080)                                      110
C End of Local Procedure                                                  110
* -----------------------------------------------------------------------------
C Begin Local Procedure FGRN                                              112
C FGRN IS A RING IN THE GEM DETECTOR                                      112
5100  CONTINUE                                                            112
         AG_TITLE='FGRN'                                                  112
         AG_MODULE=MODULE                                                 112
         AG_MARK='V00'                                                    112
         CALL AGSPUSH                                                     112
         AG_MODULE=MODULE                                                 112
C    *                                                                    112
C    Geant MATERIAL : AIR ===> inherited arguments used <===              113
         CALL AGSRESET                                                    113
         AG_TITLE = 'AIR'                                                 113
         AG_EXNAME = 'MATERIAL'                                           113
         AG_PARLIST = 'NONE'                                              113
         CALL AXMATERIAL                                                  113
C    *                                                                    113
C    Geant ATTRIBUTE : FGRN ===> SEEN=1 COLO=1 <===                       113
         CALL AGSRESET                                                    114
         AG_SEEN = 1                                                      114
         AG_COLO = 1                                                      114
         AG_TITLE = 'FGRN'                                                114
         AG_EXNAME = 'ATTRIBUTE'                                          114
         AG_PARLIST = 'SEEN_COLO'                                         114
         CALL AXATTRIBUTE                                                 114
C    *                                                                    114
C    Geant SHAPE : TUBE ===> RMIN=R1 RMAX=R2 DZ=FGTG_ARTHK/2.0 <===       116
         CALL AGSRESET                                                    116
         AG_RMIN = R1                                                     116
         AG_RMAX = R2                                                     116
         AG_DZ = FGTG_ARTHK/2.0                                           116
         AG_TITLE = 'TUBE'                                                116
         AG_EXNAME = 'SHAPE'                                              116
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      116
         CALL AXSHAPE                                                     116
C    *                                                                    116
C    Create FGSC :                                                        118
C    Call To Local Procedure FGSC                                         118
         ASSIGN 5130 TO BLOCK_FGSC                                        118
         GO TO  5120                                                      118
5130     CONTINUE                                                         118
C    *                                                                    118
C    *                                                                    119
         IXSEC=1                                                          119
            GO TO 5143                                                    119
5141        IXSEC=IXSEC+1                                                 119
5143        IF(IXSEC-(NS).GT.0) GO TO 5142                                119
C       Geant POSITION : FGSC ===> ALPHAZ=(IXSEC-1)*DPHI <===             120
            CALL AGSRESET                                                 120
            AG_ALPHAN=AG_ALPHAN+1                                         120
            AG_ALPHAK(AG_ALPHAN)=3                                        120
            AG_ALPHAL(AG_ALPHAN)=(IXSEC-1)*DPHI                           120
            AG_TITLE = 'FGSC'                                             120
            AG_EXNAME = 'POSITION'                                        120
            AG_PARLIST = 'ALPHAZ'                                         120
            CALL AXPOSITION                                               120
C       *                                                                 120
         GO TO 5141                                                       121
5142     CONTINUE                                                         121
5101  CALL AGSPOPB                                                        123
      GO TO BLOCK_FGRN, (5100 ,5110)                                      123
C End of Local Procedure                                                  123
* -----------------------------------------------------------------------------
* Sensitive volume in this model. 90%Ar+10%CO2
*
C Begin Local Procedure FGSC                                              127
C FGSC IS AN AR SECTOR OF A RING IN THE GEM DETECTOR                      127
5120  CONTINUE                                                            127
         AG_TITLE='FGSC'                                                  127
         AG_MODULE=MODULE                                                 127
         AG_MARK='V00'                                                    127
         CALL AGSPUSH                                                     127
         AG_MODULE=MODULE                                                 127
C    *                                                                    127
C    Geant COMPONENT : AR ===> A=39.95 Z=18. W=0.9 <===                   128
         CALL AGSRESET                                                    128
         AG_A = 39.95                                                     128
         AG_Z = 18.                                                       128
         AG_W = 0.9                                                       128
         AG_TITLE = 'AR'                                                  128
         AG_EXNAME = 'COMPONENT'                                          128
         AG_PARLIST = 'A_Z_W'                                             128
         CALL AXCOMPONENT                                                 128
C    *                                                                    128
C    Geant COMPONENT : C ===> A=12.01 Z=6. W=0.1*1*12.01/44.01 <===       128
         CALL AGSRESET                                                    129
         AG_A = 12.01                                                     129
         AG_Z = 6.                                                        129
         AG_W = 0.1*1*12.01/44.01                                         129
         AG_TITLE = 'C'                                                   129
         AG_EXNAME = 'COMPONENT'                                          129
         AG_PARLIST = 'A_Z_W'                                             129
         CALL AXCOMPONENT                                                 129
C    *                                                                    129
C    Geant COMPONENT : O ===> A=16. Z=8. W=0.1*2*16./44.01 <===           129
         CALL AGSRESET                                                    130
         AG_A = 16.                                                       130
         AG_Z = 8.                                                        130
         AG_W = 0.1*2*16./44.01                                           130
         AG_TITLE = 'O'                                                   130
         AG_EXNAME = 'COMPONENT'                                          130
         AG_PARLIST = 'A_Z_W'                                             130
         CALL AXCOMPONENT                                                 130
C    *                                                                    130
C    Geant MIXTURE : AR_MIX ===> DENS=0.0018015 ISVOL=1 <===              130
         CALL AGSRESET                                                    131
         AG_DENS = 0.0018015                                              131
         AG_ISVOL = 1                                                     131
         AG_TITLE = 'AR_MIX'                                              131
         AG_EXNAME = 'MIXTURE'                                            131
         AG_PARLIST = 'DENS_ISVOL'                                        131
         CALL AXMIXTURE                                                   131
C    *                                                                    131
C    Geant MATERIAL : SENSITIVE ===> ISVOL=1 <===                         133
         CALL AGSRESET                                                    133
         AG_ISVOL = 1                                                     133
         AG_TITLE = 'SENSITIVE'                                           133
         AG_EXNAME = 'MATERIAL'                                           133
         AG_PARLIST = 'ISVOL'                                             133
         CALL AXMATERIAL                                                  133
C    *                                                                    133
C    Geant ATTRIBUTE : FGSC ===> SEEN=1 COLO=2 <===                       135
         CALL AGSRESET                                                    135
         AG_SEEN = 1                                                      135
         AG_COLO = 2                                                      135
         AG_TITLE = 'FGSC'                                                135
         AG_EXNAME = 'ATTRIBUTE'                                          135
         AG_PARLIST = 'SEEN_COLO'                                         135
         CALL AXATTRIBUTE                                                 135
C    *                                                                    135
C    Geant SHAPE : TUBS ===> PHI1=0 PHI2=DPHI <===                        137
         CALL AGSRESET                                                    137
         AG_PHI1 = 0                                                      137
         AG_PHI2 = DPHI                                                   137
         AG_TITLE = 'TUBS'                                                137
         AG_EXNAME = 'SHAPE'                                              137
         AG_PARLIST = 'PHI1_PHI2'                                         137
         CALL AXSHAPE                                                     137
C    *                                                                    137
         CALL GSTPAR (AG_IMED,'STRA',1.)                                  139
C    Defining Hits: Z:.001:S Y:.001: X:.001: PTOT:16:(0,100), CX:10: CY:  143
C    10: CZ:10: SLENG:16:(0,500), TOF:16:(0,1.E-6) STEP:.01: ELOSS:16:(0  143
C    ,0.001)                                                              143
         AG_TITLE = 'FGSC'                                                143
         AG_EXNAME = 'HITS'                                               143
         AG_TYPE=AG_MAGIC                                                 143
         CALL AGSDETV('FGSC')                                             143
C    *                                                                    143
         AG_PARLIST='Z'                                                   143
         AG_TITLE = 'DETH'                                                143
         AG_NBITS = AG_MAGIC                                              143
         AG_BIN=.001                                                      143
         AG_OPTION='S'                                                    143
         AG_FHMIN = AG_MAGIC                                              143
         AG_FHMAX = AG_MAGIC                                              143
         CALL AGSDETH (0)                                                 143
C    *                                                                    143
         AG_PARLIST='Y'                                                   143
         AG_TITLE = 'DETH'                                                143
         AG_NBITS = AG_MAGIC                                              143
         AG_BIN=.001                                                      143
         AG_FHMIN=AG_MAGIC                                                143
         AG_FHMAX=AG_MAGIC                                                143
         AG_OPTION='*'                                                    143
         CALL AGSDETH (0)                                                 143
C    *                                                                    143
         AG_PARLIST='X'                                                   143
         AG_TITLE = 'DETH'                                                143
         AG_NBITS = AG_MAGIC                                              143
         AG_BIN=.001                                                      143
         AG_FHMIN=AG_MAGIC                                                143
         AG_FHMAX=AG_MAGIC                                                143
         AG_OPTION='*'                                                    143
         CALL AGSDETH (0)                                                 143
C    *                                                                    143
         AG_PARLIST='PTOT'                                                143
         AG_TITLE = 'DETH'                                                143
         AG_NBITS = 16                                                    143
         AG_BIN=AG_MAGIC                                                  143
         AG_OPTION='*'                                                    143
         AG_FHMIN = 0                                                     143
         AG_FHMAX = 100                                                   143
         CALL AGSDETH (0)                                                 143
C    *                                                                    143
         AG_PARLIST='CX'                                                  143
         AG_TITLE = 'DETH'                                                143
         AG_NBITS = 10                                                    143
         AG_BIN=AG_MAGIC                                                  143
         AG_FHMIN=AG_MAGIC                                                143
         AG_FHMAX=AG_MAGIC                                                143
         AG_OPTION='*'                                                    143
         CALL AGSDETH (0)                                                 143
C    *                                                                    143
         AG_PARLIST='CY'                                                  143
         AG_TITLE = 'DETH'                                                143
         AG_NBITS = 10                                                    143
         AG_BIN=AG_MAGIC                                                  143
         AG_FHMIN=AG_MAGIC                                                143
         AG_FHMAX=AG_MAGIC                                                143
         AG_OPTION='*'                                                    143
         CALL AGSDETH (0)                                                 143
C    *                                                                    143
         AG_PARLIST='CZ'                                                  143
         AG_TITLE = 'DETH'                                                143
         AG_NBITS = 10                                                    143
         AG_BIN=AG_MAGIC                                                  143
         AG_FHMIN=AG_MAGIC                                                143
         AG_FHMAX=AG_MAGIC                                                143
         AG_OPTION='*'                                                    143
         CALL AGSDETH (0)                                                 143
C    *                                                                    143
         AG_PARLIST='SLENG'                                               143
         AG_TITLE = 'DETH'                                                143
         AG_NBITS = 16                                                    143
         AG_BIN=AG_MAGIC                                                  143
         AG_OPTION='*'                                                    143
         AG_FHMIN = 0                                                     143
         AG_FHMAX = 500                                                   143
         CALL AGSDETH (0)                                                 143
C    *                                                                    143
         AG_PARLIST='TOF'                                                 143
         AG_TITLE = 'DETH'                                                143
         AG_NBITS = 16                                                    143
         AG_BIN=AG_MAGIC                                                  143
         AG_OPTION='*'                                                    143
         AG_FHMIN = 0                                                     143
         AG_FHMAX = 1.E-6                                                 143
         CALL AGSDETH (0)                                                 143
C    *                                                                    143
         AG_PARLIST='STEP'                                                143
         AG_TITLE = 'DETH'                                                143
         AG_NBITS = AG_MAGIC                                              143
         AG_BIN=.01                                                       143
         AG_FHMIN=AG_MAGIC                                                143
         AG_FHMAX=AG_MAGIC                                                143
         AG_OPTION='*'                                                    143
         CALL AGSDETH (0)                                                 143
C    *                                                                    143
         AG_PARLIST='ELOSS'                                               143
         AG_TITLE = 'DETH'                                                143
         AG_NBITS = 16                                                    143
         AG_BIN=AG_MAGIC                                                  143
         AG_OPTION='*'                                                    143
         AG_FHMIN = 0                                                     143
         AG_FHMAX = 0.001                                                 143
         CALL AGSDETH (0)                                                 143
C    *                                                                    143
         CALL AGSDETU('FGSC')                                             143
C    *                                                                    143
5121  CALL AGSPOPB                                                        145
      GO TO BLOCK_FGSC, (5120 ,5130)                                      145
C End of Local Procedure                                                  145
* -----------------------------------------------------------------------------
C Begin Local Procedure FGGT                                              147
C FGGT IS THE G10 PLATE                                                   147
5030  CONTINUE                                                            147
         AG_TITLE='FGGT'                                                  147
         AG_MODULE=MODULE                                                 147
         AG_MARK='V00'                                                    147
         CALL AGSPUSH                                                     147
         AG_MODULE=MODULE                                                 147
C    *                                                                    147
*     G10 is about 60% SiO2 and 40% epoxy
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===          149
         CALL AGSRESET                                                    149
         AG_A = 28.08                                                     149
         AG_Z = 14                                                        149
         AG_W = 0.6*1*28./60.                                             149
         AG_TITLE = 'SI'                                                  149
         AG_EXNAME = 'COMPONENT'                                          149
         AG_PARLIST = 'A_Z_W'                                             149
         CALL AXCOMPONENT                                                 149
C    *                                                                    149
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. <===               149
         CALL AGSRESET                                                    150
         AG_A = 16                                                        150
         AG_Z = 8                                                         150
         AG_W = 0.6*2*16./60.                                             150
         AG_TITLE = 'O'                                                   150
         AG_EXNAME = 'COMPONENT'                                          150
         AG_PARLIST = 'A_Z_W'                                             150
         CALL AXCOMPONENT                                                 150
C    *                                                                    150
C    Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===              150
         CALL AGSRESET                                                    151
         AG_A = 12                                                        151
         AG_Z = 6                                                         151
         AG_W = 0.4*8*12./174.                                            151
         AG_TITLE = 'C'                                                   151
         AG_EXNAME = 'COMPONENT'                                          151
         AG_PARLIST = 'A_Z_W'                                             151
         CALL AXCOMPONENT                                                 151
C    *                                                                    151
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===               151
         CALL AGSRESET                                                    152
         AG_A = 1                                                         152
         AG_Z = 1                                                         152
         AG_W = 0.4*14*1./174.                                            152
         AG_TITLE = 'H'                                                   152
         AG_EXNAME = 'COMPONENT'                                          152
         AG_PARLIST = 'A_Z_W'                                             152
         CALL AXCOMPONENT                                                 152
C    *                                                                    152
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.4*4*16./174. <===              152
         CALL AGSRESET                                                    153
         AG_A = 16                                                        153
         AG_Z = 8                                                         153
         AG_W = 0.4*4*16./174.                                            153
         AG_TITLE = 'O'                                                   153
         AG_EXNAME = 'COMPONENT'                                          153
         AG_PARLIST = 'A_Z_W'                                             153
         CALL AXCOMPONENT                                                 153
C    *                                                                    153
C    Geant MIXTURE : G10 ===> DENS=1.7 <===                               153
         CALL AGSRESET                                                    154
         AG_DENS = 1.7                                                    154
         AG_TITLE = 'G10'                                                 154
         AG_EXNAME = 'MIXTURE'                                            154
         AG_PARLIST = 'DENS'                                              154
         CALL AXMIXTURE                                                   154
C    *                                                                    154
C    Geant ATTRIBUTE : FGGT ===> SEEN=1 COLO=3 <===                       156
         CALL AGSRESET                                                    156
         AG_SEEN = 1                                                      156
         AG_COLO = 3                                                      156
         AG_TITLE = 'FGGT'                                                156
         AG_EXNAME = 'ATTRIBUTE'                                          156
         AG_PARLIST = 'SEEN_COLO'                                         156
         CALL AXATTRIBUTE                                                 156
C    *                                                                    156
C    Geant SHAPE : TUBE ===> RMIN=FGTG_RMIN RMAX=FGTG_RMAX DZ=FGTG_G10TH  158
C    K/2.0 <===                                                           158
         CALL AGSRESET                                                    158
         AG_RMIN = FGTG_RMIN                                              158
         AG_RMAX = FGTG_RMAX                                              158
         AG_DZ = FGTG_G10THK/2.0                                          158
         AG_TITLE = 'TUBE'                                                158
         AG_EXNAME = 'SHAPE'                                              158
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      158
         CALL AXSHAPE                                                     158
C    *                                                                    158
5031  CALL AGSPOPB                                                        160
      GO TO BLOCK_FGGT, (5030 ,5040)                                      160
C End of Local Procedure                                                  160
* -----------------------------------------------------------------------------
C Begin Local Procedure FGKP                                              162
C FGKP IS THE KAPTON LAYER                                                162
5050  CONTINUE                                                            162
         AG_TITLE='FGKP'                                                  162
         AG_MODULE=MODULE                                                 162
         AG_MARK='V00'                                                    162
         CALL AGSPUSH                                                     162
         AG_MODULE=MODULE                                                 162
C    *                                                                    162
* use aluminized mylar mixture instead of kapton
C    Geant COMPONENT : C5 ===> A=12 Z=6 W=5 <===                          164
         CALL AGSRESET                                                    164
         AG_A = 12                                                        164
         AG_Z = 6                                                         164
         AG_W = 5                                                         164
         AG_TITLE = 'C5'                                                  164
         AG_EXNAME = 'COMPONENT'                                          164
         AG_PARLIST = 'A_Z_W'                                             164
         CALL AXCOMPONENT                                                 164
C    *                                                                    164
C    Geant COMPONENT : H4 ===> A=1 Z=1 W=4 <===                           164
         CALL AGSRESET                                                    165
         AG_A = 1                                                         165
         AG_Z = 1                                                         165
         AG_W = 4                                                         165
         AG_TITLE = 'H4'                                                  165
         AG_EXNAME = 'COMPONENT'                                          165
         AG_PARLIST = 'A_Z_W'                                             165
         CALL AXCOMPONENT                                                 165
C    *                                                                    165
C    Geant COMPONENT : O2 ===> A=16 Z=8 W=2 <===                          165
         CALL AGSRESET                                                    166
         AG_A = 16                                                        166
         AG_Z = 8                                                         166
         AG_W = 2                                                         166
         AG_TITLE = 'O2'                                                  166
         AG_EXNAME = 'COMPONENT'                                          166
         AG_PARLIST = 'A_Z_W'                                             166
         CALL AXCOMPONENT                                                 166
C    *                                                                    166
C    Geant COMPONENT : AL ===> A=27 Z=13 W=0.2302 <===                    166
         CALL AGSRESET                                                    167
         AG_A = 27                                                        167
         AG_Z = 13                                                        167
         AG_W = 0.2302                                                    167
         AG_TITLE = 'AL'                                                  167
         AG_EXNAME = 'COMPONENT'                                          167
         AG_PARLIST = 'A_Z_W'                                             167
         CALL AXCOMPONENT                                                 167
C    *                                                                    167
C    Geant MIXTURE : ALKAP ===> DENS=1.432 <===                           167
         CALL AGSRESET                                                    168
         AG_DENS = 1.432                                                  168
         AG_TITLE = 'ALKAP'                                               168
         AG_EXNAME = 'MIXTURE'                                            168
         AG_PARLIST = 'DENS'                                              168
         CALL AXMIXTURE                                                   168
C    *                                                                    168
C    Geant ATTRIBUTE : FGKP ===> SEEN=1 COLO=4 <===                       170
         CALL AGSRESET                                                    170
         AG_SEEN = 1                                                      170
         AG_COLO = 4                                                      170
         AG_TITLE = 'FGKP'                                                170
         AG_EXNAME = 'ATTRIBUTE'                                          170
         AG_PARLIST = 'SEEN_COLO'                                         170
         CALL AXATTRIBUTE                                                 170
C    *                                                                    170
C    Geant SHAPE : TUBE ===> RMIN=FGTG_RMIN RMAX=FGTG_RMAX DZ=FGTG_KAPTO  172
C    NTHK/2.0 <===                                                        172
         CALL AGSRESET                                                    172
         AG_RMIN = FGTG_RMIN                                              172
         AG_RMAX = FGTG_RMAX                                              172
         AG_DZ = FGTG_KAPTONTHK/2.0                                       172
         AG_TITLE = 'TUBE'                                                172
         AG_EXNAME = 'SHAPE'                                              172
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      172
         CALL AXSHAPE                                                     172
C    *                                                                    172
5051  CALL AGSPOPB                                                        174
      GO TO BLOCK_FGKP, (5050 ,5060)                                      174
C End of Local Procedure                                                  174
* -----------------------------------------------------------------------------
      END                                                                 180
* $Id: fgtdgeo1.g,v 1.4 2007/11/13 21:29:45 perev Exp $
* $Log: fgtdgeo1.g,v $
* Revision 1.4  2007/11/13 21:29:45  perev
* material ALKAP fixed
*
* Revision 1.3  2007/02/23 21:16:37  potekhin
* Replacing a dummy version (a placeholder) of the FGT (formerly known
* as IGT) code with a piece that supposedly works, and has 6 GEM disks.
* All the structures and volumes were renames to comply with the new
* naming convention (i.e. FGT, not IGT anymore).
*
* Revision 1.2  2007/02/13 20:41:03  potekhin
* Remove the hit declaration just to make sure
* there is no confusion downstream -- this version
* if for material balance puprpose only.
*
* Revision 1.1  2007/02/08 00:01:47  potekhin
* First cut of the "new" FGT which is identical to the previously
* used IGT. Will soon be replaced by a newer version.
*
*
* FGTDGEO - Forward GEM Tracking Detector Geometry
*
* Describes a few disks of triple GEM detectors for a possible STAR upgrade
* Distinct from, and alternative to, the previosuly proposed
* LAGRGE GEM surfaces; formerly used to reside in the IGT branch.
*
* Inner and outer radii as well as position along the Z axis are variable
* parameters as well as the geometry, foil thicknesses, and spacing of the
* triple GEM detectors.
*
* First design 2006/05/01 - D.K. Hasell
*
******************************************************************************
      SUBROUTINE FGTDGEO1                                                 216
C FGTDGEO1 IS THE GEOMETRY OF THE FORWARD GEM TRACKING DETECTOR           216
      IMPLICIT NONE                                                       216
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       216
      DATA MODULE/'FGTDGEO1'/, BANK_TITLE/'*'/                            216
      DATA MCOMMENT/'IS THE GEOMETRY OF THE FORWARD GEM TRACKING DETECTO  216
     *R'/                                                                 216
      LOGICAL FIRST                                                       216
      DATA FIRST/.TRUE./                                                  216
      SAVE FIRST                                                          216
      INTEGER JUMPAD                                                      216
      EXTERNAL JUMPAD                                                     216
      INTEGER OK                                                          216
      PARAMETER (OK=0)                                                    216
      INTEGER AG_I,AG_J                                                   216
      SAVE AG_I,AG_J                                                      216
C CREATED 2/7/07                                                          216
      DATA AG_I/0/,AG_J/0/                                                217
      CHARACTER*40 CREATED /'2/7/07'/                                     217
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  217
C AUTHOR TUP TEA                                                          217
      CHARACTER*40 AUTHOR /'TUP TEA'/                                     218
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  218
******************************************************************************
C - Advanced Geant interface                                              220
      CHARACTER*20 AG_MODULE,AG_TITLE,AG_EXNAME,AG_PARLIST,AG_CHDIR,      220
     *AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM                       220
      CHARACTER*4 AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,         220
     *AG_OPTION,AG_ORT,AG_MARK                                            220
      INTEGER AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,         220
     *AG_IMED,AG_IMAT,AG_IFIELD, AG_NLMAT,AG_IERROR,AG_NWBUF,AG_NPAR,     220
     *AG_ISTATUS,AG_IROT,AG_NBITS,AG_SERIAL, AG_ATTRIBUTE(6),AG_WORK,     220
     *AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,AG_JDU,AG_ORTI, AG_NDIV,    220
     *AG_IAXIS,AG_NDVMAX,AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,          220
     *AG_ENDSCR, AG_IDTYPE,AG_ISET,AG_IDET,AG_ISVOL,AG_LEVEL,AG_IRESER,   220
     *AG_IGEOM,AG_STANDALONE, AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,     220
     *AG_LDETU,AG_NPDIV,AG_NPDV,AG_NZ,AG_NCOPY, AG_IDEBU,AG_IGRAP,        220
     *AG_IHIST,AG_IMFLD,AG_ISIMU,AG_ENDSAVE,AG_CODE,AG_TRKTYP, AG_MODE,   220
     *AG_ECODE,AG_PDG,IPRIN, AG_ALPHAN,AG_ALPHAK(10),IGOR                 220
      REAL AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,AG_STMIN,     220
     *AG_DENS,AG_RADL,AG_ABSL, AG_THETAX,AG_PHIX,AG_THETAY,AG_PHIY,       220
     *AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ, AG_ALPHAL(10),        220
     *AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ,AG_TYPE, AG_DX, AG_DX1, AG_DX2,       220
     *AG_DY, AG_DY1, AG_DY2, AG_DMAXMS, AG_TWIST, AG_THET, AG_THE1,       220
     *AG_THE2, AG_PHI, AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2,       220
     *AG_RMIN, AG_RMAX, AG_RMN, AG_RMX, AG_ZI , AG_RMN1, AG_RMN2,         220
     *AG_RMX1, AG_RMX2, AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,     220
     *AG_DPHI, AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,       220
     *AG_STEP, AG_C0, AG_PAR, AG_AA, AG_ZZ, AG_WW, AG_LX, AG_LY, AG_LZ,   220
     *AG_HX, AG_HY, AG_HZ, AG_P1, AG_P2, AG_STACK,AG_UBUF,AG_XHMAX,       220
     *AG_YHMAX,AG_ZHMAX,AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN, AG_CHARGE,     220
     *AG_MASS,AG_TLIFE,AG_BRATIO                                          220
      PARAMETER (AG_LSTACK=130,AG_NWUHIT=10,AG_NWUVOL=3,AG_MAGIC=         220
     *-696969,AG_LDETU=250)                                               220
      COMMON/AGCGLOB/ AG_MODULE,AG_CHDIR,AG_LEVEL,AG_IDTYPE,AG_IERROR,    220
     *AG_STANDALONE,IPRIN, AG_IPRIN,AG_IGEOM,AG_IDEBU,AG_IGRAP,AG_IHIST,  220
     *AG_IMFLD,AG_ISIMU                                                   220
C Inherited variables a: saved during internal calls                      220
C b: reset at the entry in a block and then saved                         220
      COMMON/AGCPARA/ AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,        220
     *AG_ISHAPE, AG_IMED, AG_IMAT, AG_IFIELD, AG_FIELDM, AG_TMAXFD,       220
     *AG_STEMAX, AG_DEEMAX, AG_EPSIL, AG_STMIN, AG_DENS, AG_RADL,         220
     *AG_ABSL, AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1, AG_DY2, AG_RMN1,     220
     *AG_RMN2, AG_RMX1, AG_RMX2, AG_THET, AG_THE1, AG_THE2, AG_PHI,       220
     *AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2, AG_H1, AG_BL1,         220
     *AG_TL1, AG_H2, AG_BL2, AG_TL2, AG_RMIN, AG_RMAX, AG_DPHI,           220
     *AG_NPDIV, AG_NZ, AG_DZ, AG_TWIS, AG_LX, AG_LY, AG_LZ, AG_HX,        220
     *AG_HY, AG_HZ, AG_A, AG_ZA, AG_W, AG_NLMAT, AG_WORK, AG_SEEN,        220
     *AG_LSTY, AG_LWID, AG_COLO, AG_FILL, AG_SERIAL, AG_ISVOL,            220
     *AG_ISTATUS, AG_ZI(16), AG_RMN(16), AG_RMX(16), AG_VOLUME,           220
     *AG_MOTHER, AG_SHAPE, AG_CNICK, AG_ENDSAVE, AG_RESET1, AG_THETAX,    220
     *AG_PHIX,AG_THETAY,AG_PHIY,AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,      220
     *AG_ALFAZ, AG_X,AG_Y,AG_Z, AG_STEP,AG_C0,AG_NDIV,AG_IAXIS,           220
     *AG_NDVMAX,AG_ORTI,AG_NCOPY, AG_ALPHAN,AG_ALPHAK,AG_ALPHAL,          220
     *AG_RESET2, AG_KONLY, AG_ORT, AG_MARK                                220
C local variables valid inside same block                                 220
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       220
     *AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  220
     *AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         220
     *AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    220
     *AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          220
     *AG_OPTION                                                           220
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,        220
     *AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                             220
      COMMON/AGCSTAC/ AG_STACK(AG_LSTACK,15)                              220
C temporarly                                                              220
      EQUIVALENCE (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),           220
     *(AG_ALPHAX,AG_ALFAX), (AG_ALPHAY,AG_ALFAY),(AG_ALPHAZ,AG_ALFAZ),    220
     *(AG_TWIST,AG_TWIS),(AG_NPDIV,AG_NPDV),  (AG_P1,AG_HX),(AG_P2,       220
     *AG_HY)                                                              220
      REAL MIL_P,INCH_P                                                   220
      PARAMETER (INCH_P=2.54,MIL_P=0.00254)                               220
C - GEANT description of logical units                                    220
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            220
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      220
      COMMON/GCMAIL/ CHMAIL                                               220
      CHARACTER*132 CHMAIL                                                220
* Declare variables used in the code.
C CENTER OF COMPLETE ASSEMBLY ALONG Z AXIS                                224
      REAL CENTER                                                         224
C LENGTH OF COMPLETE ASSEMBLY ALONG Z AXIS                                225
      REAL LENGTH                                                         225
C THICKNESS IN Z OF A SINGLE GEM DISK                                     226
      REAL THICK                                                          226
C USED TO DETERMINE THE MINIMUM RADIUS                                    227
      REAL RMN                                                            227
C USED TO DETERMINE THE MAXIMUM RADIUS                                    228
      REAL RMX                                                            228
C INNER RADIUS FOR EACH GEM DETECTOR                                      229
      REAL RIN                                                            229
C OUTER RADIUS FOR EACH GEM DETECTOR                                      230
      REAL ROUT                                                           230
C ACCUMULATES Z POSITION FOR LAYERS IN GEM                                231
      REAL ZSUM                                                           231
C HALF THICKNESS OF LAYER IN EACH GEM                                     232
      REAL THK                                                            232
C INDEX FOR COUNTING OVER THE GEM DISKS                                   234
      INTEGER DISK                                                        234
C INDEX FRO COUNTING OVER THE LAYERS IN GEM                               235
      INTEGER LAYER                                                       235
* Declare the blocks to be defined in the code.
      INTEGER BLOCK_FGMO                                                  238
      INTEGER BLOCK_FGDO                                                  238
      INTEGER BLOCK_FGFO                                                  238
      INTEGER BLOCK_FGIS                                                  238
      INTEGER BLOCK_FGOS                                                  238
      INTEGER BLOCK_FGRL                                                  238
      INTEGER BLOCK_FGSC                                                  238
C -                                                                       240
      REAL FGTV_VERSION                                                   240
      EQUIVALENCE (BANK_FGTV(3),FGTV_VERSION,RE_FGTV_VERSION),            240
     *(NAMES_FGTV(1),NFGTV_VERSION)                                       240
      EQUIVALENCE (COMMEN_FGTV(1),CFGTV_VERSION),(MAP_FGTV(1,1),          240
     *MFGTV_VERSION)                                                      240
      CHARACTER CFGTV_VERSION*40/' '/                                     240
      CHARACTER NFGTV_VERSION*12/'R.VERSION'/                             240
      INTEGER MFGTV_VERSION(2)/1,1/                                       240
      REAL RE_FGTV_VERSION                                                240
C -                                                                       240
      INTEGER FGTV_CONFIG                                                 240
      EQUIVALENCE (BANK_FGTV(4),FGTV_CONFIG,RE_FGTV_CONFIG),              240
     *(NAMES_FGTV(2),NFGTV_CONFIG)                                        240
      EQUIVALENCE (COMMEN_FGTV(2),CFGTV_CONFIG),(MAP_FGTV(1,2),           240
     *MFGTV_CONFIG)                                                       240
      CHARACTER CFGTV_CONFIG*40/' '/                                      240
      CHARACTER NFGTV_CONFIG*12/'I.CONFIG'/                               240
      INTEGER MFGTV_CONFIG(2)/1,1/                                        240
      REAL RE_FGTV_CONFIG                                                 240
C -                                                                       240
      INTEGER LEN_FGTV,LLEN_FGTV,LINK_FGTV                                240
      PARAMETER (LEN_FGTV=4)                                              240
      PARAMETER (LLEN_FGTV=2)                                             240
      INTEGER FLAG_FGTV/0/,MAP_FGTV(2,LLEN_FGTV),IDNUM_FGTV(10)           240
      CHARACTER NAMES_FGTV(LLEN_FGTV)*12,COMMEN_FGTV(LLEN_FGTV)*40        240
      DATA IDNUM_FGTV/10*0/                                               240
      REAL BANK_FGTV(LEN_FGTV)                                            240
      CHARACTER*60 BPATH_FGTV/' '/                                        240
      SAVE FLAG_FGTV,MAP_FGTV,IDNUM_FGTV,NAMES_FGTV,COMMEN_FGTV,          240
     *BPATH_FGTV                                                          240
      DATA LINK_FGTV/0/,BANK_FGTV/LEN_FGTV*0/                             240
C *                                                                       240
* Declare the structure FGTG which defines the detector geometry.
C -                                                                       243
      REAL FGTG_CONFIG                                                    243
      EQUIVALENCE (BANK_FGTG(3),FGTG_CONFIG,RE_FGTG_CONFIG),              243
     *(NAMES_FGTG(1),NFGTG_CONFIG)                                        243
      EQUIVALENCE (COMMEN_FGTG(1),CFGTG_CONFIG),(MAP_FGTG(1,1),           243
     *MFGTG_CONFIG)                                                       243
      CHARACTER CFGTG_CONFIG*40/' '/                                      243
      CHARACTER NFGTG_CONFIG*12/'R.CONFIG'/                               243
      INTEGER MFGTG_CONFIG(2)/1,1/                                        243
      REAL RE_FGTG_CONFIG                                                 243
C -                                                                       243
      REAL FGTG_RI(6)                                                     243
      EQUIVALENCE (BANK_FGTG(4),FGTG_RI,RE_FGTG_RI),(NAMES_FGTG(2),       243
     *NFGTG_RI)                                                           243
      EQUIVALENCE (COMMEN_FGTG(2),CFGTG_RI),(MAP_FGTG(1,2),MFGTG_RI)      243
      CHARACTER CFGTG_RI*40/' '/                                          243
      CHARACTER NFGTG_RI*12/'R.RI'/                                       243
      INTEGER MFGTG_RI(2)/6,1/                                            243
      REAL RE_FGTG_RI                                                     243
C -                                                                       243
      REAL FGTG_RO(6)                                                     243
      EQUIVALENCE (BANK_FGTG(10),FGTG_RO,RE_FGTG_RO),(NAMES_FGTG(3),      243
     *NFGTG_RO)                                                           243
      EQUIVALENCE (COMMEN_FGTG(3),CFGTG_RO),(MAP_FGTG(1,3),MFGTG_RO)      243
      CHARACTER CFGTG_RO*40/' '/                                          243
      CHARACTER NFGTG_RO*12/'R.RO'/                                       243
      INTEGER MFGTG_RO(2)/6,1/                                            243
      REAL RE_FGTG_RO                                                     243
C -                                                                       243
      REAL FGTG_ZSTART                                                    243
      EQUIVALENCE (BANK_FGTG(16),FGTG_ZSTART,RE_FGTG_ZSTART),             243
     *(NAMES_FGTG(4),NFGTG_ZSTART)                                        243
      EQUIVALENCE (COMMEN_FGTG(4),CFGTG_ZSTART),(MAP_FGTG(1,4),           243
     *MFGTG_ZSTART)                                                       243
      CHARACTER CFGTG_ZSTART*40/' '/                                      243
      CHARACTER NFGTG_ZSTART*12/'R.ZSTART'/                               243
      INTEGER MFGTG_ZSTART(2)/1,1/                                        243
      REAL RE_FGTG_ZSTART                                                 243
C -                                                                       243
      REAL FGTG_Z(6)                                                      243
      EQUIVALENCE (BANK_FGTG(17),FGTG_Z,RE_FGTG_Z),(NAMES_FGTG(5),        243
     *NFGTG_Z)                                                            243
      EQUIVALENCE (COMMEN_FGTG(5),CFGTG_Z),(MAP_FGTG(1,5),MFGTG_Z)        243
      CHARACTER CFGTG_Z*40/' '/                                           243
      CHARACTER NFGTG_Z*12/'R.Z'/                                         243
      INTEGER MFGTG_Z(2)/6,1/                                             243
      REAL RE_FGTG_Z                                                      243
C -                                                                       243
      REAL FGTG_FTHK(6)                                                   243
      EQUIVALENCE (BANK_FGTG(23),FGTG_FTHK,RE_FGTG_FTHK),(NAMES_FGTG(6),  243
     *NFGTG_FTHK)                                                         243
      EQUIVALENCE (COMMEN_FGTG(6),CFGTG_FTHK),(MAP_FGTG(1,6),             243
     *MFGTG_FTHK)                                                         243
      CHARACTER CFGTG_FTHK*40/' '/                                        243
      CHARACTER NFGTG_FTHK*12/'R.FTHK'/                                   243
      INTEGER MFGTG_FTHK(2)/6,1/                                          243
      REAL RE_FGTG_FTHK                                                   243
C -                                                                       243
      REAL FGTG_STHK(6)                                                   243
      EQUIVALENCE (BANK_FGTG(29),FGTG_STHK,RE_FGTG_STHK),(NAMES_FGTG(7),  243
     *NFGTG_STHK)                                                         243
      EQUIVALENCE (COMMEN_FGTG(7),CFGTG_STHK),(MAP_FGTG(1,7),             243
     *MFGTG_STHK)                                                         243
      CHARACTER CFGTG_STHK*40/' '/                                        243
      CHARACTER NFGTG_STHK*12/'R.STHK'/                                   243
      INTEGER MFGTG_STHK(2)/6,1/                                          243
      REAL RE_FGTG_STHK                                                   243
C -                                                                       243
      REAL FGTG_SR                                                        243
      EQUIVALENCE (BANK_FGTG(35),FGTG_SR,RE_FGTG_SR),(NAMES_FGTG(8),      243
     *NFGTG_SR)                                                           243
      EQUIVALENCE (COMMEN_FGTG(8),CFGTG_SR),(MAP_FGTG(1,8),MFGTG_SR)      243
      CHARACTER CFGTG_SR*40/' '/                                          243
      CHARACTER NFGTG_SR*12/'R.SR'/                                       243
      INTEGER MFGTG_SR(2)/1,1/                                            243
      REAL RE_FGTG_SR                                                     243
C -                                                                       243
      REAL FGTG_RTHK                                                      243
      EQUIVALENCE (BANK_FGTG(36),FGTG_RTHK,RE_FGTG_RTHK),(NAMES_FGTG(9),  243
     *NFGTG_RTHK)                                                         243
      EQUIVALENCE (COMMEN_FGTG(9),CFGTG_RTHK),(MAP_FGTG(1,9),             243
     *MFGTG_RTHK)                                                         243
      CHARACTER CFGTG_RTHK*40/' '/                                        243
      CHARACTER NFGTG_RTHK*12/'R.RTHK'/                                   243
      INTEGER MFGTG_RTHK(2)/1,1/                                          243
      REAL RE_FGTG_RTHK                                                   243
C -                                                                       243
      INTEGER LEN_FGTG,LLEN_FGTG,LINK_FGTG                                243
      PARAMETER (LEN_FGTG=36)                                             243
      PARAMETER (LLEN_FGTG=9)                                             243
      INTEGER FLAG_FGTG/0/,MAP_FGTG(2,LLEN_FGTG),IDNUM_FGTG(10)           243
      CHARACTER NAMES_FGTG(LLEN_FGTG)*12,COMMEN_FGTG(LLEN_FGTG)*40        243
      DATA IDNUM_FGTG/10*0/                                               243
      REAL BANK_FGTG(LEN_FGTG)                                            243
      CHARACTER*60 BPATH_FGTG/' '/                                        243
      SAVE FLAG_FGTG,MAP_FGTG,IDNUM_FGTG,NAMES_FGTG,COMMEN_FGTG,          243
     *BPATH_FGTG                                                          243
      DATA LINK_FGTG/0/,BANK_FGTG/LEN_FGTG*0/                             243
C *                                                                       243
* -----------------------------------------------------------------------------
*      RI      = {  7.5,  7.5,  7.5, 16.5, 16.5, 16.5 }    ! inner radii for each GEM. ORG{3, 7.5, 12, 16.5, 21, 25.5}
C *                                                                       248
      CALL AGSBEGM(MODULE,IPRIN)                                          248
      IF (IPRIN.LT.0) RETURN                                              248
C Filling bank FGTV - FGT GEOMETRY VERSION                                248
      BANK_TITLE='FGT GEOMETRY VERSION'                                   248
      BPATH_FGTV='FGTV*'                                                  248
      IDNUM_FGTV(1)=0                                                     249
C GEOMETRY VERSION - DUMMY                                                249
      FGTV_VERSION=1                                                      249
      CFGTV_VERSION='GEOMETRY VERSION - DUMMY'                            249
C CONFIG                                                                  250
      FGTV_CONFIG=1                                                       250
      CFGTV_CONFIG='CONFIG'                                               250
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FGTV',    251
     *BPATH_FGTV,IDNUM_FGTV,1, MAP_FGTV,NAMES_FGTV,COMMEN_FGTV,           251
     *BANK_FGTV,LEN_FGTV,LLEN_FGTV,LINK_FGTV,FLAG_FGTV)                   251
C *                                                                       251
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FGTV',    251
     *BPATH_FGTV,IDNUM_FGTV,1, MAP_FGTV,NAMES_FGTV,COMMEN_FGTV,           251
     *BANK_FGTV,LEN_FGTV,LLEN_FGTV,LINK_FGTV,FLAG_FGTV)                   251
C *                                                                       253
      CALL AGSBEGM(MODULE,IPRIN)                                          253
      IF (IPRIN.LT.0) RETURN                                              253
C Filling bank FGTG - INNER GEM TRACKER GEOMETRY DATA                     253
      BANK_TITLE='INNER GEM TRACKER GEOMETRY DATA'                        253
      BPATH_FGTG='FGTG*'                                                  253
      IDNUM_FGTG(1)=0                                                     254
C VERSION                                                                 254
      FGTG_CONFIG=1                                                       254
      CFGTG_CONFIG='VERSION'                                              254
C INNER RADII FOR EACH GEM. ORG{3, 7.5, 12, 16.5, 21, 25.5}               255
      FGTG_RI(1)=7.5                                                      255
      FGTG_RI(2)=7.5                                                      255
      FGTG_RI(3)=7.5                                                      255
      FGTG_RI(4)=7.5                                                      255
      FGTG_RI(5)=7.5                                                      255
      FGTG_RI(6)=7.5                                                      255
      CFGTG_RI='INNER RADII FOR EACH GEM. ORG{3, 7.5, 12, 16.5, 21, 25.5  255
     *}'                                                                  255
C OUTER RADII FOR EACH GEM.                                               256
      FGTG_RO(1)=43.0                                                     256
      FGTG_RO(2)=43.0                                                     256
      FGTG_RO(3)=43.0                                                     256
      FGTG_RO(4)=43.0                                                     256
      FGTG_RO(5)=43.0                                                     256
      FGTG_RO(6)=43.0                                                     256
      CFGTG_RO='OUTER RADII FOR EACH GEM.'                                256
C STARTING POSITION ALONG Z AXIS                                          257
      FGTG_ZSTART=60.0                                                    257
      CFGTG_ZSTART='STARTING POSITION ALONG Z AXIS'                       257
C Z POSITIONS FOR GEM FRONT FACE                                          258
      FGTG_Z(1)=0.0                                                       258
      FGTG_Z(2)=18.0                                                      258
      FGTG_Z(3)=36.0                                                      258
      FGTG_Z(4)=54.0                                                      258
      FGTG_Z(5)=72.0                                                      258
      FGTG_Z(6)=90.0                                                      258
      CFGTG_Z='Z POSITIONS FOR GEM FRONT FACE'                            258
C FOIL THICKNESSES INSIDE GEM                                             259
      FGTG_FTHK(1)=0.05                                                   259
      FGTG_FTHK(2)=0.05                                                   259
      FGTG_FTHK(3)=0.05                                                   259
      FGTG_FTHK(4)=0.05                                                   259
      FGTG_FTHK(5)=0.05                                                   259
      FGTG_FTHK(6)=0.05                                                   259
      CFGTG_FTHK='FOIL THICKNESSES INSIDE GEM'                            259
C SUPPORT/SPACING THICKNESSES                                             260
      FGTG_STHK(1)=0.4                                                    260
      FGTG_STHK(2)=0.3                                                    260
      FGTG_STHK(3)=0.3                                                    260
      FGTG_STHK(4)=0.3                                                    260
      FGTG_STHK(5)=0.3                                                    260
      FGTG_STHK(6)=0.4                                                    260
      CFGTG_STHK='SUPPORT/SPACING THICKNESSES'                            260
C RADIAL SIZE FOR SUPPORT                                                 261
      FGTG_SR=1.0                                                         261
      CFGTG_SR='RADIAL SIZE FOR SUPPORT'                                  261
C READOUT PLANE THICKNESS                                                 262
      FGTG_RTHK=0.3                                                       262
      CFGTG_RTHK='READOUT PLANE THICKNESS'                                262
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FGTG',    263
     *BPATH_FGTG,IDNUM_FGTG,1, MAP_FGTG,NAMES_FGTG,COMMEN_FGTG,           263
     *BANK_FGTG,LEN_FGTG,LLEN_FGTG,LINK_FGTG,FLAG_FGTG)                   263
C *                                                                       263
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FGTG',    263
     *BPATH_FGTG,IDNUM_FGTG,1, MAP_FGTG,NAMES_FGTG,COMMEN_FGTG,           263
     *BANK_FGTG,LEN_FGTG,LLEN_FGTG,LINK_FGTG,FLAG_FGTG)                   263
*******************************************************************************
* Calculate some parameters from the input variables.
C use default FGTV bank                                                   269
      BPATH_FGTV='FGTV*'                                                  269
      IDNUM_FGTV(1)=0                                                     269
      CALL AGDGETP('FGTDGEO1','*',-999,'FGTV',' ',BANK_FGTV, BPATH_FGTV,  269
     *IDNUM_FGTV,1,MAP_FGTV,NAMES_FGTV,BANK_FGTV,LEN_FGTV,LLEN_FGTV,      269
     *LINK_FGTV,FLAG_FGTV)                                                269
C use FGTG bank                                                           269
      BPATH_FGTG='FGTG*'                                                  269
      IDNUM_FGTG(1)=0                                                     269
      FGTG_CONFIG=FGTV_CONFIG                                             269
      CALL AGDGETP('FGTDGEO1','*',-999,'FGTG',NFGTG_CONFIG,               269
     *RE_FGTG_CONFIG,BPATH_FGTG,IDNUM_FGTG,1,MAP_FGTG,NAMES_FGTG,         269
     *BANK_FGTG,LEN_FGTG,LLEN_FGTG,LINK_FGTG,FLAG_FGTG)                   269
      WRITE(*,*) 'FGTD Version:',FGTV_CONFIG                              271
* use aluminized mylar mixture instead of kapton
C Geant COMPONENT : C5 ===> A=12 Z=6 W=5 <===                             274
      CALL AGSRESET                                                       274
      AG_A = 12                                                           274
      AG_Z = 6                                                            274
      AG_W = 5                                                            274
      AG_TITLE = 'C5'                                                     274
      AG_EXNAME = 'COMPONENT'                                             274
      AG_PARLIST = 'A_Z_W'                                                274
      CALL AXCOMPONENT                                                    274
C *                                                                       274
C Geant COMPONENT : H4 ===> A=1 Z=1 W=4 <===                              274
      CALL AGSRESET                                                       275
      AG_A = 1                                                            275
      AG_Z = 1                                                            275
      AG_W = 4                                                            275
      AG_TITLE = 'H4'                                                     275
      AG_EXNAME = 'COMPONENT'                                             275
      AG_PARLIST = 'A_Z_W'                                                275
      CALL AXCOMPONENT                                                    275
C *                                                                       275
C Geant COMPONENT : O2 ===> A=16 Z=8 W=2 <===                             275
      CALL AGSRESET                                                       276
      AG_A = 16                                                           276
      AG_Z = 8                                                            276
      AG_W = 2                                                            276
      AG_TITLE = 'O2'                                                     276
      AG_EXNAME = 'COMPONENT'                                             276
      AG_PARLIST = 'A_Z_W'                                                276
      CALL AXCOMPONENT                                                    276
C *                                                                       276
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.2302 <===                       276
      CALL AGSRESET                                                       277
      AG_A = 27                                                           277
      AG_Z = 13                                                           277
      AG_W = 0.2302                                                       277
      AG_TITLE = 'AL'                                                     277
      AG_EXNAME = 'COMPONENT'                                             277
      AG_PARLIST = 'A_Z_W'                                                277
      CALL AXCOMPONENT                                                    277
C *                                                                       277
C Geant MIXTURE : ALKAP ===> DENS=1.432 <===                              277
      CALL AGSRESET                                                       278
      AG_DENS = 1.432                                                     278
      AG_TITLE = 'ALKAP'                                                  278
      AG_EXNAME = 'MIXTURE'                                               278
      AG_PARLIST = 'DENS'                                                 278
      CALL AXMIXTURE                                                      278
C *                                                                       278
*     G10 is about 60% SiO2 and 40% epoxy
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             280
      CALL AGSRESET                                                       280
      AG_A = 28.08                                                        280
      AG_Z = 14                                                           280
      AG_W = 0.6*1*28./60.                                                280
      AG_TITLE = 'SI'                                                     280
      AG_EXNAME = 'COMPONENT'                                             280
      AG_PARLIST = 'A_Z_W'                                                280
      CALL AXCOMPONENT                                                    280
C *                                                                       280
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. <===                  280
      CALL AGSRESET                                                       281
      AG_A = 16                                                           281
      AG_Z = 8                                                            281
      AG_W = 0.6*2*16./60.                                                281
      AG_TITLE = 'O'                                                      281
      AG_EXNAME = 'COMPONENT'                                             281
      AG_PARLIST = 'A_Z_W'                                                281
      CALL AXCOMPONENT                                                    281
C *                                                                       281
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 281
      CALL AGSRESET                                                       282
      AG_A = 12                                                           282
      AG_Z = 6                                                            282
      AG_W = 0.4*8*12./174.                                               282
      AG_TITLE = 'C'                                                      282
      AG_EXNAME = 'COMPONENT'                                             282
      AG_PARLIST = 'A_Z_W'                                                282
      CALL AXCOMPONENT                                                    282
C *                                                                       282
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  282
      CALL AGSRESET                                                       283
      AG_A = 1                                                            283
      AG_Z = 1                                                            283
      AG_W = 0.4*14*1./174.                                               283
      AG_TITLE = 'H'                                                      283
      AG_EXNAME = 'COMPONENT'                                             283
      AG_PARLIST = 'A_Z_W'                                                283
      CALL AXCOMPONENT                                                    283
C *                                                                       283
C Geant COMPONENT : O ===> A=16 Z=8 W=0.4*4*16./174. <===                 283
      CALL AGSRESET                                                       284
      AG_A = 16                                                           284
      AG_Z = 8                                                            284
      AG_W = 0.4*4*16./174.                                               284
      AG_TITLE = 'O'                                                      284
      AG_EXNAME = 'COMPONENT'                                             284
      AG_PARLIST = 'A_Z_W'                                                284
      CALL AXCOMPONENT                                                    284
C *                                                                       284
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  284
      CALL AGSRESET                                                       285
      AG_DENS = 1.7                                                       285
      AG_TITLE = 'G10'                                                    285
      AG_EXNAME = 'MIXTURE'                                               285
      AG_PARLIST = 'DENS'                                                 285
      CALL AXMIXTURE                                                      285
C *                                                                       285
* Loop over the layers in a GEM detector and accumulate the total thickness.
      THICK = 0.0                                                         290
C *                                                                       292
      DO 5011 LAYER=1,4                                                   292
         THICK = THICK + FGTG_FTHK(LAYER) + FGTG_STHK(LAYER)              293
5011  CONTINUE                                                            294
5012  CONTINUE                                                            294
      THICK = THICK + FGTG_RTHK                                           296
* Loop over the GEM disks and determine the min and max radius and max Z.
      RMN = 10000.0                                                       300
      RMX = - RMN                                                         301
      LENGTH = 0.0                                                        302
C *                                                                       304
      DO 5021 DISK=1,6                                                    304
         IF (RMN .GT. FGTG_RI(DISK)) THEN                                 306
         RMN = FGTG_RI(DISK)                                              307
         ENDIF                                                            308
         IF (RMX .LT. FGTG_RO(DISK)) THEN                                 310
         RMX = FGTG_RO(DISK)                                              311
         ENDIF                                                            312
         IF (LENGTH .LT. FGTG_Z(DISK)) THEN                               314
         LENGTH = FGTG_Z(DISK)                                            315
         ENDIF                                                            316
5021  CONTINUE                                                            317
5022  CONTINUE                                                            317
      LENGTH = LENGTH + THICK                                             319
* Calculate centre of assembly.
      CENTER = FGTG_ZSTART + LENGTH / 2.0                                 323
      CALL AGSBEGM(MODULE,IPRIN)                                          325
C Check IPRIN<0                                                           325
      IF (IPRIN.LT.0) RETURN                                              325
C Create FGMO :                                                           325
C Call To Local Procedure FGMO                                            325
      ASSIGN 5040 TO BLOCK_FGMO                                           325
      GO TO  5030                                                         325
5040  CONTINUE                                                            325
C *                                                                       325
C Geant POSITION : FGMO ===> MOTHER='CAVE' Z=+CENTER KONLY='MANY' <===    326
      CALL AGSRESET                                                       326
      AG_MOTHER = 'CAVE'                                                  326
      AG_Z = +CENTER                                                      326
      AG_KONLY = 'MANY'                                                   326
      AG_TITLE = 'FGMO'                                                   326
      AG_EXNAME = 'POSITION'                                              326
      AG_PARLIST = 'MOTHER_Z_KONLY'                                       326
      CALL AXPOSITION                                                     326
C *                                                                       326
* -----------------------------------------------------------------------------
      CALL AGSENDM                                                        329
      RETURN                                                              329
C Begin Local Procedure FGMO                                              329
C FGMO IS THE MOTHER VOLUME FOR THE FGTD                                  329
5030  CONTINUE                                                            329
         AG_TITLE='FGMO'                                                  329
         AG_MODULE=MODULE                                                 329
         AG_MARK='V00'                                                    329
         CALL AGSPUSH                                                     329
         AG_MODULE=MODULE                                                 329
C    *                                                                    329
C    Geant MATERIAL : AIR ===> inherited arguments used <===              330
         CALL AGSRESET                                                    330
         AG_TITLE = 'AIR'                                                 330
         AG_EXNAME = 'MATERIAL'                                           330
         AG_PARLIST = 'NONE'                                              330
         CALL AXMATERIAL                                                  330
C    *                                                                    330
C    Geant ATTRIBUTE : FGMO ===> SEEN=0 COLO=6 <===                       330
         CALL AGSRESET                                                    331
         AG_SEEN = 0                                                      331
         AG_COLO = 6                                                      331
         AG_TITLE = 'FGMO'                                                331
         AG_EXNAME = 'ATTRIBUTE'                                          331
         AG_PARLIST = 'SEEN_COLO'                                         331
         CALL AXATTRIBUTE                                                 331
C    *                                                                    331
C    Geant SHAPE : TUBE ===> RMIN=RMN RMAX=RMX DZ=LENGTH/2.0 <===         333
         CALL AGSRESET                                                    333
         AG_RMIN = RMN                                                    333
         AG_RMAX = RMX                                                    333
         AG_DZ = LENGTH/2.0                                               333
         AG_TITLE = 'TUBE'                                                333
         AG_EXNAME = 'SHAPE'                                              333
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      333
         CALL AXSHAPE                                                     333
C    *                                                                    333
* Loop over the four GEM disks.
C    *                                                                    337
         DISK=1                                                           337
            GO TO 5053                                                    337
5051        DISK=DISK+1                                                   337
5053        IF(DISK-(6).GT.0) GO TO 5052                                  337
            RIN = FGTG_RI(DISK)                                           338
            ROUT = FGTG_RO(DISK)                                          339
C       Create FGDO :                                                     340
C       Call To Local Procedure FGDO                                      340
            ASSIGN 5070 TO BLOCK_FGDO                                     340
            GO TO  5060                                                   340
5070        CONTINUE                                                      340
C       *                                                                 340
C       Geant POSITION : FGDO ===> Z=-LENGTH/2.0 + THICK/2.0 + FGTG_Z(DI  340
C       SK) <===                                                          340
            CALL AGSRESET                                                 340
            AG_Z = -LENGTH/2.0 + THICK/2.0 + FGTG_Z(DISK)                 340
            AG_TITLE = 'FGDO'                                             340
            AG_EXNAME = 'POSITION'                                        340
            AG_PARLIST = 'Z'                                              340
            CALL AXPOSITION                                               340
C       *                                                                 340
         GO TO 5051                                                       341
5052     CONTINUE                                                         341
5031  CALL AGSPOPB                                                        343
      GO TO BLOCK_FGMO, (5030 ,5040)                                      343
C End of Local Procedure                                                  343
* -----------------------------------------------------------------------------
C Begin Local Procedure FGDO                                              346
C FGDO IS THE MOTHER VOLUME OF THE INDIVIDUAL GEM DISKS                   346
5060  CONTINUE                                                            346
         AG_TITLE='FGDO'                                                  346
         AG_MODULE=MODULE                                                 346
         AG_MARK='V00'                                                    346
         CALL AGSPUSH                                                     346
         AG_MODULE=MODULE                                                 346
C    *                                                                    346
C    Geant COMPONENT : AR ===> A=39.95 Z=18. W=0.9 <===                   347
         CALL AGSRESET                                                    347
         AG_A = 39.95                                                     347
         AG_Z = 18.                                                       347
         AG_W = 0.9                                                       347
         AG_TITLE = 'AR'                                                  347
         AG_EXNAME = 'COMPONENT'                                          347
         AG_PARLIST = 'A_Z_W'                                             347
         CALL AXCOMPONENT                                                 347
C    *                                                                    347
C    Geant COMPONENT : C ===> A=12.01 Z=6. W=0.1*1*12.01/44.01 <===       347
         CALL AGSRESET                                                    348
         AG_A = 12.01                                                     348
         AG_Z = 6.                                                        348
         AG_W = 0.1*1*12.01/44.01                                         348
         AG_TITLE = 'C'                                                   348
         AG_EXNAME = 'COMPONENT'                                          348
         AG_PARLIST = 'A_Z_W'                                             348
         CALL AXCOMPONENT                                                 348
C    *                                                                    348
C    Geant COMPONENT : O ===> A=16. Z=8. W=0.1*2*16./44.01 <===           348
         CALL AGSRESET                                                    349
         AG_A = 16.                                                       349
         AG_Z = 8.                                                        349
         AG_W = 0.1*2*16./44.01                                           349
         AG_TITLE = 'O'                                                   349
         AG_EXNAME = 'COMPONENT'                                          349
         AG_PARLIST = 'A_Z_W'                                             349
         CALL AXCOMPONENT                                                 349
C    *                                                                    349
C    Geant MIXTURE : AR_MIX ===> DENS=0.0018015 <===                      349
         CALL AGSRESET                                                    350
         AG_DENS = 0.0018015                                              350
         AG_TITLE = 'AR_MIX'                                              350
         AG_EXNAME = 'MIXTURE'                                            350
         AG_PARLIST = 'DENS'                                              350
         CALL AXMIXTURE                                                   350
C    *                                                                    350
C    Geant ATTRIBUTE : FGDO ===> SEEN=0 COLO=6 <===                       352
         CALL AGSRESET                                                    352
         AG_SEEN = 0                                                      352
         AG_COLO = 6                                                      352
         AG_TITLE = 'FGDO'                                                352
         AG_EXNAME = 'ATTRIBUTE'                                          352
         AG_PARLIST = 'SEEN_COLO'                                         352
         CALL AXATTRIBUTE                                                 352
C    *                                                                    352
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROUT DZ=THICK/2.0 <===         354
         CALL AGSRESET                                                    354
         AG_RMIN = RIN                                                    354
         AG_RMAX = ROUT                                                   354
         AG_DZ = THICK/2.0                                                354
         AG_TITLE = 'TUBE'                                                354
         AG_EXNAME = 'SHAPE'                                              354
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      354
         CALL AXSHAPE                                                     354
C    *                                                                    354
* Loop over the layers inside the GEM.
C    START AT THE FRONT FACE.                                             358
         ZSUM = -THICK/2.0                                                358
C    *                                                                    360
         LAYER=1                                                          360
            GO TO 5083                                                    360
5081        LAYER=LAYER+1                                                 360
5083        IF(LAYER-(4).GT.0) GO TO 5082                                 360
* Create GEM foil.
C       HALF THICKNESS OF FOIL                                            364
            THK = FGTG_FTHK(LAYER)/2.0                                    364
C       POSITION OF FOIL CENTRE                                           365
            ZSUM = ZSUM + THK                                             365
C       Create FGFO :                                                     367
C       Call To Local Procedure FGFO                                      367
            ASSIGN 5100 TO BLOCK_FGFO                                     367
            GO TO  5090                                                   367
5100        CONTINUE                                                      367
C       *                                                                 367
C       Geant POSITION : FGFO ===> Z=ZSUM <===                            367
            CALL AGSRESET                                                 367
            AG_Z = ZSUM                                                   367
            AG_TITLE = 'FGFO'                                             367
            AG_EXNAME = 'POSITION'                                        367
            AG_PARLIST = 'Z'                                              367
            CALL AXPOSITION                                               367
C       *                                                                 367
C       POSITION OF FOIL REAR FACE                                        369
            ZSUM = ZSUM + THK                                             369
* Create inner and outer spacer/support.
C       HALF THICKNESS OF SPACER                                          373
            THK = FGTG_STHK(LAYER)/2.0                                    373
C       POSITION OF SPACER CENTRE                                         374
            ZSUM = ZSUM + THK                                             374
C       Create FGIS :                                                     376
C       Call To Local Procedure FGIS                                      376
            ASSIGN 5120 TO BLOCK_FGIS                                     376
            GO TO  5110                                                   376
5120        CONTINUE                                                      376
C       *                                                                 376
C       Geant POSITION : FGIS ===> Z=ZSUM <===                            376
            CALL AGSRESET                                                 376
            AG_Z = ZSUM                                                   376
            AG_TITLE = 'FGIS'                                             376
            AG_EXNAME = 'POSITION'                                        376
            AG_PARLIST = 'Z'                                              376
            CALL AXPOSITION                                               376
C       *                                                                 376
C       Create FGOS :                                                     377
C       Call To Local Procedure FGOS                                      377
            ASSIGN 5140 TO BLOCK_FGOS                                     377
            GO TO  5130                                                   377
5140        CONTINUE                                                      377
C       *                                                                 377
C       Geant POSITION : FGOS ===> Z=ZSUM <===                            377
            CALL AGSRESET                                                 377
            AG_Z = ZSUM                                                   377
            AG_TITLE = 'FGOS'                                             377
            AG_EXNAME = 'POSITION'                                        377
            AG_PARLIST = 'Z'                                              377
            CALL AXPOSITION                                               377
C       *                                                                 377
* First gap is active volume -> create it.
            IF (LAYER .EQ. 1) THEN                                        381
C       Create FGSC :                                                     382
C       Call To Local Procedure FGSC                                      382
            ASSIGN 5160 TO BLOCK_FGSC                                     382
            GO TO  5150                                                   382
5160        CONTINUE                                                      382
C       *                                                                 382
C       Geant POSITION : FGSC ===> Z=ZSUM <===                            382
            CALL AGSRESET                                                 382
            AG_Z = ZSUM                                                   382
            AG_TITLE = 'FGSC'                                             382
            AG_EXNAME = 'POSITION'                                        382
            AG_PARLIST = 'Z'                                              382
            CALL AXPOSITION                                               382
C       *                                                                 382
            ENDIF                                                         382
C       POSITION OF SPACER REAR FACE                                      385
            ZSUM = ZSUM + THK                                             385
         GO TO 5081                                                       387
5082     CONTINUE                                                         387
* Create readout layer as last layer.
C    HALF THICKNESS OF READOUT LAYER                                      391
         THK = FGTG_RTHK/2.0                                              391
C    POSITION OF READOUT LAYER CENTRE                                     392
         ZSUM = ZSUM + THK                                                392
C    Create FGRL :                                                        394
C    Call To Local Procedure FGRL                                         394
         ASSIGN 5180 TO BLOCK_FGRL                                        394
         GO TO  5170                                                      394
5180     CONTINUE                                                         394
C    *                                                                    394
C    Geant POSITION : FGRL ===> Z=ZSUM <===                               394
         CALL AGSRESET                                                    394
         AG_Z = ZSUM                                                      394
         AG_TITLE = 'FGRL'                                                394
         AG_EXNAME = 'POSITION'                                           394
         AG_PARLIST = 'Z'                                                 394
         CALL AXPOSITION                                                  394
C    *                                                                    394
5061  CALL AGSPOPB                                                        396
      GO TO BLOCK_FGDO, (5060 ,5070)                                      396
C End of Local Procedure                                                  396
* -----------------------------------------------------------------------------
C Begin Local Procedure FGFO                                              399
C FGFO DESCRIBES THE GEM FOILS                                            399
5090  CONTINUE                                                            399
         AG_TITLE='FGFO'                                                  399
         AG_MODULE=MODULE                                                 399
         AG_MARK='V00'                                                    399
         CALL AGSPUSH                                                     399
         AG_MODULE=MODULE                                                 399
C    *                                                                    399
C    Geant MATERIAL : ALKAP ===> inherited arguments used <===            400
         CALL AGSRESET                                                    400
         AG_TITLE = 'ALKAP'                                               400
         AG_EXNAME = 'MATERIAL'                                           400
         AG_PARLIST = 'NONE'                                              400
         CALL AXMATERIAL                                                  400
C    *                                                                    400
C    Geant ATTRIBUTE : FGFO ===> SEEN=1 COLO=4 <===                       400
         CALL AGSRESET                                                    401
         AG_SEEN = 1                                                      401
         AG_COLO = 4                                                      401
         AG_TITLE = 'FGFO'                                                401
         AG_EXNAME = 'ATTRIBUTE'                                          401
         AG_PARLIST = 'SEEN_COLO'                                         401
         CALL AXATTRIBUTE                                                 401
C    *                                                                    401
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROUT DZ=THK <===               403
         CALL AGSRESET                                                    403
         AG_RMIN = RIN                                                    403
         AG_RMAX = ROUT                                                   403
         AG_DZ = THK                                                      403
         AG_TITLE = 'TUBE'                                                403
         AG_EXNAME = 'SHAPE'                                              403
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      403
         CALL AXSHAPE                                                     403
C    *                                                                    403
5091  CALL AGSPOPB                                                        405
      GO TO BLOCK_FGFO, (5090 ,5100)                                      405
C End of Local Procedure                                                  405
* -----------------------------------------------------------------------------
C Begin Local Procedure FGIS                                              408
C FGIS DESCRIBES THE INNER SUPPORT OR SPACER                              408
5110  CONTINUE                                                            408
         AG_TITLE='FGIS'                                                  408
         AG_MODULE=MODULE                                                 408
         AG_MARK='V00'                                                    408
         CALL AGSPUSH                                                     408
         AG_MODULE=MODULE                                                 408
C    *                                                                    408
C    Geant MATERIAL : G10 ===> inherited arguments used <===              409
         CALL AGSRESET                                                    409
         AG_TITLE = 'G10'                                                 409
         AG_EXNAME = 'MATERIAL'                                           409
         AG_PARLIST = 'NONE'                                              409
         CALL AXMATERIAL                                                  409
C    *                                                                    409
C    Geant ATTRIBUTE : FGIS ===> SEEN=1 COLO=3 <===                       409
         CALL AGSRESET                                                    410
         AG_SEEN = 1                                                      410
         AG_COLO = 3                                                      410
         AG_TITLE = 'FGIS'                                                410
         AG_EXNAME = 'ATTRIBUTE'                                          410
         AG_PARLIST = 'SEEN_COLO'                                         410
         CALL AXATTRIBUTE                                                 410
C    *                                                                    410
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=RIN+FGTG_SR DZ=THK <===        412
         CALL AGSRESET                                                    412
         AG_RMIN = RIN                                                    412
         AG_RMAX = RIN+FGTG_SR                                            412
         AG_DZ = THK                                                      412
         AG_TITLE = 'TUBE'                                                412
         AG_EXNAME = 'SHAPE'                                              412
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      412
         CALL AXSHAPE                                                     412
C    *                                                                    412
5111  CALL AGSPOPB                                                        414
      GO TO BLOCK_FGIS, (5110 ,5120)                                      414
C End of Local Procedure                                                  414
* -----------------------------------------------------------------------------
C Begin Local Procedure FGOS                                              417
C FGOS DESCRIBES THE OUTER SUPPORT OR SPACER                              417
5130  CONTINUE                                                            417
         AG_TITLE='FGOS'                                                  417
         AG_MODULE=MODULE                                                 417
         AG_MARK='V00'                                                    417
         CALL AGSPUSH                                                     417
         AG_MODULE=MODULE                                                 417
C    *                                                                    417
C    Geant MATERIAL : G10 ===> inherited arguments used <===              418
         CALL AGSRESET                                                    418
         AG_TITLE = 'G10'                                                 418
         AG_EXNAME = 'MATERIAL'                                           418
         AG_PARLIST = 'NONE'                                              418
         CALL AXMATERIAL                                                  418
C    *                                                                    418
C    Geant ATTRIBUTE : FGOS ===> SEEN=1 COLO=3 <===                       418
         CALL AGSRESET                                                    419
         AG_SEEN = 1                                                      419
         AG_COLO = 3                                                      419
         AG_TITLE = 'FGOS'                                                419
         AG_EXNAME = 'ATTRIBUTE'                                          419
         AG_PARLIST = 'SEEN_COLO'                                         419
         CALL AXATTRIBUTE                                                 419
C    *                                                                    419
C    Geant SHAPE : TUBE ===> RMIN=ROUT-FGTG_SR RMAX=ROUT DZ=THK <===      421
         CALL AGSRESET                                                    421
         AG_RMIN = ROUT-FGTG_SR                                           421
         AG_RMAX = ROUT                                                   421
         AG_DZ = THK                                                      421
         AG_TITLE = 'TUBE'                                                421
         AG_EXNAME = 'SHAPE'                                              421
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      421
         CALL AXSHAPE                                                     421
C    *                                                                    421
5131  CALL AGSPOPB                                                        423
      GO TO BLOCK_FGOS, (5130 ,5140)                                      423
C End of Local Procedure                                                  423
* -----------------------------------------------------------------------------
C Begin Local Procedure FGRL                                              426
C FGRL DESCRIBES THE READOUT LAYER                                        426
5170  CONTINUE                                                            426
         AG_TITLE='FGRL'                                                  426
         AG_MODULE=MODULE                                                 426
         AG_MARK='V00'                                                    426
         CALL AGSPUSH                                                     426
         AG_MODULE=MODULE                                                 426
C    *                                                                    426
C    Geant MATERIAL : G10 ===> inherited arguments used <===              427
         CALL AGSRESET                                                    427
         AG_TITLE = 'G10'                                                 427
         AG_EXNAME = 'MATERIAL'                                           427
         AG_PARLIST = 'NONE'                                              427
         CALL AXMATERIAL                                                  427
C    *                                                                    427
C    Geant ATTRIBUTE : FGRL ===> SEEN=1 COLO=3 <===                       427
         CALL AGSRESET                                                    428
         AG_SEEN = 1                                                      428
         AG_COLO = 3                                                      428
         AG_TITLE = 'FGRL'                                                428
         AG_EXNAME = 'ATTRIBUTE'                                          428
         AG_PARLIST = 'SEEN_COLO'                                         428
         CALL AXATTRIBUTE                                                 428
C    *                                                                    428
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROUT DZ=THK <===               430
         CALL AGSRESET                                                    430
         AG_RMIN = RIN                                                    430
         AG_RMAX = ROUT                                                   430
         AG_DZ = THK                                                      430
         AG_TITLE = 'TUBE'                                                430
         AG_EXNAME = 'SHAPE'                                              430
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      430
         CALL AXSHAPE                                                     430
C    *                                                                    430
5171  CALL AGSPOPB                                                        432
      GO TO BLOCK_FGRL, (5170 ,5180)                                      432
C End of Local Procedure                                                  432
* -----------------------------------------------------------------------------
C Begin Local Procedure FGSC                                              435
C FGSC DESCRIBES THE SENSITIVE AREA                                       435
5150  CONTINUE                                                            435
         AG_TITLE='FGSC'                                                  435
         AG_MODULE=MODULE                                                 435
         AG_MARK='V00'                                                    435
         CALL AGSPUSH                                                     435
         AG_MODULE=MODULE                                                 435
C    *                                                                    435
C    Geant COMPONENT : AR ===> A=39.95 Z=18. W=0.9 <===                   436
         CALL AGSRESET                                                    436
         AG_A = 39.95                                                     436
         AG_Z = 18.                                                       436
         AG_W = 0.9                                                       436
         AG_TITLE = 'AR'                                                  436
         AG_EXNAME = 'COMPONENT'                                          436
         AG_PARLIST = 'A_Z_W'                                             436
         CALL AXCOMPONENT                                                 436
C    *                                                                    436
C    Geant COMPONENT : C ===> A=12.01 Z=6. W=0.1*1*12.01/44.01 <===       436
         CALL AGSRESET                                                    437
         AG_A = 12.01                                                     437
         AG_Z = 6.                                                        437
         AG_W = 0.1*1*12.01/44.01                                         437
         AG_TITLE = 'C'                                                   437
         AG_EXNAME = 'COMPONENT'                                          437
         AG_PARLIST = 'A_Z_W'                                             437
         CALL AXCOMPONENT                                                 437
C    *                                                                    437
C    Geant COMPONENT : O ===> A=16. Z=8. W=0.1*2*16./44.01 <===           437
         CALL AGSRESET                                                    438
         AG_A = 16.                                                       438
         AG_Z = 8.                                                        438
         AG_W = 0.1*2*16./44.01                                           438
         AG_TITLE = 'O'                                                   438
         AG_EXNAME = 'COMPONENT'                                          438
         AG_PARLIST = 'A_Z_W'                                             438
         CALL AXCOMPONENT                                                 438
C    *                                                                    438
C    Geant MIXTURE : AR_MIX ===> DENS=0.0018015 ISVOL=1 <===              438
         CALL AGSRESET                                                    439
         AG_DENS = 0.0018015                                              439
         AG_ISVOL = 1                                                     439
         AG_TITLE = 'AR_MIX'                                              439
         AG_EXNAME = 'MIXTURE'                                            439
         AG_PARLIST = 'DENS_ISVOL'                                        439
         CALL AXMIXTURE                                                   439
C    *                                                                    439
C    Geant MATERIAL : SENSITIVE ===> ISVOL=1 <===                         441
         CALL AGSRESET                                                    441
         AG_ISVOL = 1                                                     441
         AG_TITLE = 'SENSITIVE'                                           441
         AG_EXNAME = 'MATERIAL'                                           441
         AG_PARLIST = 'ISVOL'                                             441
         CALL AXMATERIAL                                                  441
C    *                                                                    441
C    Geant ATTRIBUTE : FGSC ===> SEEN=1 COLO=6 <===                       441
         CALL AGSRESET                                                    442
         AG_SEEN = 1                                                      442
         AG_COLO = 6                                                      442
         AG_TITLE = 'FGSC'                                                442
         AG_EXNAME = 'ATTRIBUTE'                                          442
         AG_PARLIST = 'SEEN_COLO'                                         442
         CALL AXATTRIBUTE                                                 442
C    *                                                                    442
C    Geant SHAPE : TUBE ===> RMIN=RIN+FGTG_SR RMAX=ROUT-FGTG_SR DZ=FGTG_  443
C    STHK(1)/2.0 <===                                                     443
         CALL AGSRESET                                                    443
         AG_RMIN = RIN+FGTG_SR                                            443
         AG_RMAX = ROUT-FGTG_SR                                           443
         AG_DZ = FGTG_STHK(1)/2.0                                         443
         AG_TITLE = 'TUBE'                                                443
         AG_EXNAME = 'SHAPE'                                              443
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      443
         CALL AXSHAPE                                                     443
C    *                                                                    443
C    Defining Hits: Z:.001:S Y:.001: X:.001: PTOT:16:(0,100), CX:10: CY:  448
C    10: CZ:10: SLENG:16:(0,500), TOF:16:(0,1.E-6) STEP:.01: ELOSS:16:(0  448
C    ,0.001)                                                              448
         AG_TITLE = 'FGSC'                                                448
         AG_EXNAME = 'HITS'                                               448
         AG_TYPE=AG_MAGIC                                                 448
         CALL AGSDETV('FGSC')                                             448
C    *                                                                    448
         AG_PARLIST='Z'                                                   448
         AG_TITLE = 'DETH'                                                448
         AG_NBITS = AG_MAGIC                                              448
         AG_BIN=.001                                                      448
         AG_OPTION='S'                                                    448
         AG_FHMIN = AG_MAGIC                                              448
         AG_FHMAX = AG_MAGIC                                              448
         CALL AGSDETH (0)                                                 448
C    *                                                                    448
         AG_PARLIST='Y'                                                   448
         AG_TITLE = 'DETH'                                                448
         AG_NBITS = AG_MAGIC                                              448
         AG_BIN=.001                                                      448
         AG_FHMIN=AG_MAGIC                                                448
         AG_FHMAX=AG_MAGIC                                                448
         AG_OPTION='*'                                                    448
         CALL AGSDETH (0)                                                 448
C    *                                                                    448
         AG_PARLIST='X'                                                   448
         AG_TITLE = 'DETH'                                                448
         AG_NBITS = AG_MAGIC                                              448
         AG_BIN=.001                                                      448
         AG_FHMIN=AG_MAGIC                                                448
         AG_FHMAX=AG_MAGIC                                                448
         AG_OPTION='*'                                                    448
         CALL AGSDETH (0)                                                 448
C    *                                                                    448
         AG_PARLIST='PTOT'                                                448
         AG_TITLE = 'DETH'                                                448
         AG_NBITS = 16                                                    448
         AG_BIN=AG_MAGIC                                                  448
         AG_OPTION='*'                                                    448
         AG_FHMIN = 0                                                     448
         AG_FHMAX = 100                                                   448
         CALL AGSDETH (0)                                                 448
C    *                                                                    448
         AG_PARLIST='CX'                                                  448
         AG_TITLE = 'DETH'                                                448
         AG_NBITS = 10                                                    448
         AG_BIN=AG_MAGIC                                                  448
         AG_FHMIN=AG_MAGIC                                                448
         AG_FHMAX=AG_MAGIC                                                448
         AG_OPTION='*'                                                    448
         CALL AGSDETH (0)                                                 448
C    *                                                                    448
         AG_PARLIST='CY'                                                  448
         AG_TITLE = 'DETH'                                                448
         AG_NBITS = 10                                                    448
         AG_BIN=AG_MAGIC                                                  448
         AG_FHMIN=AG_MAGIC                                                448
         AG_FHMAX=AG_MAGIC                                                448
         AG_OPTION='*'                                                    448
         CALL AGSDETH (0)                                                 448
C    *                                                                    448
         AG_PARLIST='CZ'                                                  448
         AG_TITLE = 'DETH'                                                448
         AG_NBITS = 10                                                    448
         AG_BIN=AG_MAGIC                                                  448
         AG_FHMIN=AG_MAGIC                                                448
         AG_FHMAX=AG_MAGIC                                                448
         AG_OPTION='*'                                                    448
         CALL AGSDETH (0)                                                 448
C    *                                                                    448
         AG_PARLIST='SLENG'                                               448
         AG_TITLE = 'DETH'                                                448
         AG_NBITS = 16                                                    448
         AG_BIN=AG_MAGIC                                                  448
         AG_OPTION='*'                                                    448
         AG_FHMIN = 0                                                     448
         AG_FHMAX = 500                                                   448
         CALL AGSDETH (0)                                                 448
C    *                                                                    448
         AG_PARLIST='TOF'                                                 448
         AG_TITLE = 'DETH'                                                448
         AG_NBITS = 16                                                    448
         AG_BIN=AG_MAGIC                                                  448
         AG_OPTION='*'                                                    448
         AG_FHMIN = 0                                                     448
         AG_FHMAX = 1.E-6                                                 448
         CALL AGSDETH (0)                                                 448
C    *                                                                    448
         AG_PARLIST='STEP'                                                448
         AG_TITLE = 'DETH'                                                448
         AG_NBITS = AG_MAGIC                                              448
         AG_BIN=.01                                                       448
         AG_FHMIN=AG_MAGIC                                                448
         AG_FHMAX=AG_MAGIC                                                448
         AG_OPTION='*'                                                    448
         CALL AGSDETH (0)                                                 448
C    *                                                                    448
         AG_PARLIST='ELOSS'                                               448
         AG_TITLE = 'DETH'                                                448
         AG_NBITS = 16                                                    448
         AG_BIN=AG_MAGIC                                                  448
         AG_OPTION='*'                                                    448
         AG_FHMIN = 0                                                     448
         AG_FHMAX = 0.001                                                 448
         CALL AGSDETH (0)                                                 448
C    *                                                                    448
         CALL AGSDETU('FGSC')                                             448
C    *                                                                    448
5151  CALL AGSPOPB                                                        450
      GO TO BLOCK_FGSC, (5150 ,5160)                                      450
C End of Local Procedure                                                  450
* -----------------------------------------------------------------------------
      END                                                                 454
*
*
* FGTDGEO - Forward GEM Tracking Detector Geometry
*
* Describes a few disks of triple GEM detectors for a possible STAR upgrade
* Distinct from, and alternative to, the previosuly proposed
* LAGRGE GEM surfaces; formerly used to reside in the IGT branch.
*
* Inner and outer radii as well as position along the Z axis are variable
* parameters as well as the geometry, foil thicknesses, and spacing of the
* triple GEM detectors.
*
* First design 2006/05/01 - D.K. Hasell
*
******************************************************************************
      SUBROUTINE FGTDGEO2                                                 471
C FGTDGEO2 IS THE GEOMETRY OF THE FORWARD GEM TRACKING DETECTOR, VERSION  471
C  UPGR16                                                                 471
      IMPLICIT NONE                                                       471
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       471
      DATA MODULE/'FGTDGEO2'/, BANK_TITLE/'*'/                            471
      DATA MCOMMENT/'IS THE GEOMETRY OF THE FORWARD GEM TRACKING DETECTO  471
     *R, VERSION UPGR16'/                                                 471
      LOGICAL FIRST                                                       471
      DATA FIRST/.TRUE./                                                  471
      SAVE FIRST                                                          471
      INTEGER JUMPAD                                                      471
      EXTERNAL JUMPAD                                                     471
      INTEGER OK                                                          471
      PARAMETER (OK=0)                                                    471
      INTEGER AG_I,AG_J                                                   471
      SAVE AG_I,AG_J                                                      471
C CREATED 6/4/2008                                                        471
      DATA AG_I/0/,AG_J/0/                                                472
      CHARACTER*40 CREATED /'6/4/2008'/                                   472
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  472
C AUTHOR JAN BALEWSKI                                                     472
      CHARACTER*40 AUTHOR /'JAN BALEWSKI'/                                473
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  473
* The single triple GEM assembly in GEANT as follows, in this order:
* Front support disk:
C  * 0.25mm FR4, rho=1.91 g/cm3, composition as  G10                      477
C  * 5.00mm Nomex,0.048 g/cm3                                             478
C  * 0.25mm FR4, rho=1.91 g/cm3                                           479
* Equivalent to 3GEMs+HV layer + readout layer:
C  * 3.0mm ArC02_70_30  (active volume)                                   481
C  * 0.2mm Cu, rho=1.16 g/cm3  (stretch ~13 x and reduced density)        482
C  * 0.3mm Kapton, rho=1.02g/cm3  ( use 0.7 density to account for holes  483
C )                                                                       483
C  * 0.2mm FR4, rho=1.19g/cm3, strech factor=1.6 ???                      484
C  * 5.8mm ArC02_70_30  (passive drift volume)                            485
C  * 2 rings of FR4 at inner/outer radii, 1 cm wide, thickness=9.5 cm     486
C  * 4 bars at quadrant boundaries ????                                   487
C  * the above values are old , only approximate                          488
* Total depth=15.0mm
*********************************************************************
C - Advanced Geant interface                                              492
      CHARACTER*20 AG_MODULE,AG_TITLE,AG_EXNAME,AG_PARLIST,AG_CHDIR,      492
     *AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM                       492
      CHARACTER*4 AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,         492
     *AG_OPTION,AG_ORT,AG_MARK                                            492
      INTEGER AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,         492
     *AG_IMED,AG_IMAT,AG_IFIELD, AG_NLMAT,AG_IERROR,AG_NWBUF,AG_NPAR,     492
     *AG_ISTATUS,AG_IROT,AG_NBITS,AG_SERIAL, AG_ATTRIBUTE(6),AG_WORK,     492
     *AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,AG_JDU,AG_ORTI, AG_NDIV,    492
     *AG_IAXIS,AG_NDVMAX,AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,          492
     *AG_ENDSCR, AG_IDTYPE,AG_ISET,AG_IDET,AG_ISVOL,AG_LEVEL,AG_IRESER,   492
     *AG_IGEOM,AG_STANDALONE, AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,     492
     *AG_LDETU,AG_NPDIV,AG_NPDV,AG_NZ,AG_NCOPY, AG_IDEBU,AG_IGRAP,        492
     *AG_IHIST,AG_IMFLD,AG_ISIMU,AG_ENDSAVE,AG_CODE,AG_TRKTYP, AG_MODE,   492
     *AG_ECODE,AG_PDG,IPRIN, AG_ALPHAN,AG_ALPHAK(10),IGOR                 492
      REAL AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,AG_STMIN,     492
     *AG_DENS,AG_RADL,AG_ABSL, AG_THETAX,AG_PHIX,AG_THETAY,AG_PHIY,       492
     *AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ, AG_ALPHAL(10),        492
     *AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ,AG_TYPE, AG_DX, AG_DX1, AG_DX2,       492
     *AG_DY, AG_DY1, AG_DY2, AG_DMAXMS, AG_TWIST, AG_THET, AG_THE1,       492
     *AG_THE2, AG_PHI, AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2,       492
     *AG_RMIN, AG_RMAX, AG_RMN, AG_RMX, AG_ZI , AG_RMN1, AG_RMN2,         492
     *AG_RMX1, AG_RMX2, AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,     492
     *AG_DPHI, AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,       492
     *AG_STEP, AG_C0, AG_PAR, AG_AA, AG_ZZ, AG_WW, AG_LX, AG_LY, AG_LZ,   492
     *AG_HX, AG_HY, AG_HZ, AG_P1, AG_P2, AG_STACK,AG_UBUF,AG_XHMAX,       492
     *AG_YHMAX,AG_ZHMAX,AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN, AG_CHARGE,     492
     *AG_MASS,AG_TLIFE,AG_BRATIO                                          492
      PARAMETER (AG_LSTACK=130,AG_NWUHIT=10,AG_NWUVOL=3,AG_MAGIC=         492
     *-696969,AG_LDETU=250)                                               492
      COMMON/AGCGLOB/ AG_MODULE,AG_CHDIR,AG_LEVEL,AG_IDTYPE,AG_IERROR,    492
     *AG_STANDALONE,IPRIN, AG_IPRIN,AG_IGEOM,AG_IDEBU,AG_IGRAP,AG_IHIST,  492
     *AG_IMFLD,AG_ISIMU                                                   492
C Inherited variables a: saved during internal calls                      492
C b: reset at the entry in a block and then saved                         492
      COMMON/AGCPARA/ AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,        492
     *AG_ISHAPE, AG_IMED, AG_IMAT, AG_IFIELD, AG_FIELDM, AG_TMAXFD,       492
     *AG_STEMAX, AG_DEEMAX, AG_EPSIL, AG_STMIN, AG_DENS, AG_RADL,         492
     *AG_ABSL, AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1, AG_DY2, AG_RMN1,     492
     *AG_RMN2, AG_RMX1, AG_RMX2, AG_THET, AG_THE1, AG_THE2, AG_PHI,       492
     *AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2, AG_H1, AG_BL1,         492
     *AG_TL1, AG_H2, AG_BL2, AG_TL2, AG_RMIN, AG_RMAX, AG_DPHI,           492
     *AG_NPDIV, AG_NZ, AG_DZ, AG_TWIS, AG_LX, AG_LY, AG_LZ, AG_HX,        492
     *AG_HY, AG_HZ, AG_A, AG_ZA, AG_W, AG_NLMAT, AG_WORK, AG_SEEN,        492
     *AG_LSTY, AG_LWID, AG_COLO, AG_FILL, AG_SERIAL, AG_ISVOL,            492
     *AG_ISTATUS, AG_ZI(16), AG_RMN(16), AG_RMX(16), AG_VOLUME,           492
     *AG_MOTHER, AG_SHAPE, AG_CNICK, AG_ENDSAVE, AG_RESET1, AG_THETAX,    492
     *AG_PHIX,AG_THETAY,AG_PHIY,AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,      492
     *AG_ALFAZ, AG_X,AG_Y,AG_Z, AG_STEP,AG_C0,AG_NDIV,AG_IAXIS,           492
     *AG_NDVMAX,AG_ORTI,AG_NCOPY, AG_ALPHAN,AG_ALPHAK,AG_ALPHAL,          492
     *AG_RESET2, AG_KONLY, AG_ORT, AG_MARK                                492
C local variables valid inside same block                                 492
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       492
     *AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  492
     *AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         492
     *AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    492
     *AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          492
     *AG_OPTION                                                           492
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,        492
     *AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                             492
      COMMON/AGCSTAC/ AG_STACK(AG_LSTACK,15)                              492
C temporarly                                                              492
      EQUIVALENCE (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),           492
     *(AG_ALPHAX,AG_ALFAX), (AG_ALPHAY,AG_ALFAY),(AG_ALPHAZ,AG_ALFAZ),    492
     *(AG_TWIST,AG_TWIS),(AG_NPDIV,AG_NPDV),  (AG_P1,AG_HX),(AG_P2,       492
     *AG_HY)                                                              492
      REAL MIL_P,INCH_P                                                   492
      PARAMETER (INCH_P=2.54,MIL_P=0.00254)                               492
C - GEANT description of logical units                                    492
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            492
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      492
      COMMON/GCMAIL/ CHMAIL                                               492
      CHARACTER*132 CHMAIL                                                492
* Declare variables used in the code.
C CENTER OF COMPLETE ASSEMBLY ALONG Z AXIS                                496
      REAL CENTERZ                                                        496
C LENGTH OF COMPLETE ASSEMBLY ALONG Z AXIS                                497
      REAL LENGTHZ                                                        497
C THICKNESS IN Z OF A SINGLE TRIPPLE-GEM DISK                             498
      REAL THICKGG                                                        498
C INNER RADIUS A KOMPONENT                                                499
      REAL RIN                                                            499
C OUTER RADIUS A KOMPONENT                                                500
      REAL ROUT                                                           500
C ACCUMULATES Z POSITION FOR SET OF COMPONENTS                            501
      REAL ZSUM                                                           501
C INDEX FOR COUNTING OVER THE GEM DISKS                                   502
      INTEGER DISK                                                        502
C INDEX FOR COUNTING QUADRANTS IN ONE GEM DISK                            503
      INTEGER QUAD                                                        503
C INDEX FOR COUNTING CABLES FOR ONE GEM DISK                              504
      INTEGER CABLE                                                       504
C INDEX FOR COUNTING CABLE SEGMENT                                        505
      INTEGER CABSEG                                                      505
C WORKING VARIABLES                                                       506
      REAL X1,X2,X3                                                       506
C WORKING VARIABLES                                                       507
      REAL ZD,Z1,Z2                                                       507
C VARIABLE SIZE OF FGSC                                                   509
      REAL FGSCRMIN, FGSCRMAX, FGSCDZ, FGSCZ                              509
* Declare the blocks to be defined in the code.
      INTEGER BLOCK_FGMO                                                  514
      INTEGER BLOCK_FGGD                                                  514
      INTEGER BLOCK_FGSD                                                  514
      INTEGER BLOCK_FGSC                                                  514
      INTEGER BLOCK_FGZC                                                  514
      INTEGER BLOCK_FGZD                                                  514
      INTEGER BLOCK_FGFP                                                  514
      INTEGER BLOCK_FGFC                                                  514
      INTEGER BLOCK_FGFA                                                  514
      INTEGER BLOCK_FGFL                                                  514
      INTEGER BLOCK_FGOC                                                  514
      INTEGER BLOCK_FGXR                                                  514
      INTEGER BLOCK_FGGF                                                  514
      INTEGER BLOCK_FGGN                                                  514
      INTEGER BLOCK_FGGC                                                  514
      INTEGER BLOCK_FGGK                                                  514
      INTEGER BLOCK_FGGP                                                  514
      INTEGER BLOCK_FGGR                                                  514
      INTEGER BLOCK_FGOR                                                  514
      INTEGER BLOCK_FGOA                                                  514
      INTEGER BLOCK_FGOU                                                  514
      INTEGER BLOCK_FGCT                                                  514
      INTEGER BLOCK_FGRS                                                  514
      INTEGER BLOCK_FGAP                                                  514
      INTEGER BLOCK_FGAL                                                  514
      INTEGER BLOCK_FGAS                                                  514
      INTEGER BLOCK_FGAB                                                  514
      INTEGER BLOCK_FGAD                                                  514
      INTEGER BLOCK_FGSU                                                  514
      INTEGER BLOCK_FGSV                                                  514
      INTEGER BLOCK_FGXC                                                  514
      INTEGER BLOCK_FGXD                                                  514
      INTEGER BLOCK_FGXE                                                  514
* volumes added/changed by Jan:
* FGFP - front plate carbon nomex honeycomb
* FGFC - front plate carbon fiber
* FGFA - front plate inner Alu ring
* FGFL - front plate lip of the inner Alu ring
* FGOC - outer cylinder , carbon fiber
* FGOR - outer rails , Alu
* FGOA - outer Alu ring
* FGOU - FGT  utility cables, mix
* FGXR - mock interface Alu ring to SSD
* FGXC - mock supporting C ring in SSD
* FGXD - mock supporting C ring in IST
* FGXE - mock supporting C ring in HFT
* FGGD - tripple-GEM disk , FR4 reduced to inner + outer spacer rings
* FGGF - FR4 front foil of  tripple-GEM
* FGGN - Nomex front support of  tripple-GEM
* FGSC - active volume of tripple-GEM
* FGZC - active volume divisions of FGSC
* FGGC - Cu layer of  tripple-GEM
* FGGK - Kapton layer of  tripple-GEM
* FGGP - passive gas layer of  tripple-GEM
* FGGR - FR4 readout layer of  tripple-GEM
* FGCT - G10 Tube for IFC resitor chain
* FGRS - Resistor Strip for IFC resitor chain
* FGAP - APV-chips encapsulation volume
* FGAL - APV-chips large end-board
* FGAS - APV-chips smale end-board
* FGAB - APV-chips balde-boards, there are 2
* FGAD - APV-chips bottom-board, equivalent to edges between quadrants
* FGSU - SSD utility cables-long, mix
* FGSV - SSD utility cables-tube , mix
* Declare the structure FGTG which defines the detector geometry.
      INTEGER MXDISK                                                      548
      PARAMETER (MXDISK=8)                                                549
C STUPID, SHOULD BE USED BELOW AS PARAM,JAN                               549
C -                                                                       560
      REAL FGTG_CONFIGJAN                                                 560
      EQUIVALENCE (BANK_FGTG(3),FGTG_CONFIGJAN,RE_FGTG_CONFIGJAN),        560
     *(NAMES_FGTG(1),NFGTG_CONFIGJAN)                                     560
      EQUIVALENCE (COMMEN_FGTG(1),CFGTG_CONFIGJAN),(MAP_FGTG(1,1),        560
     *MFGTG_CONFIGJAN)                                                    560
      CHARACTER CFGTG_CONFIGJAN*40/' '/                                   560
      CHARACTER NFGTG_CONFIGJAN*12/'R.CONFIGJAN'/                         560
      INTEGER MFGTG_CONFIGJAN(2)/1,1/                                     560
      REAL RE_FGTG_CONFIGJAN                                              560
C -                                                                       560
      REAL FGTG_GEMRI                                                     560
      EQUIVALENCE (BANK_FGTG(4),FGTG_GEMRI,RE_FGTG_GEMRI),                560
     *(NAMES_FGTG(2),NFGTG_GEMRI)                                         560
      EQUIVALENCE (COMMEN_FGTG(2),CFGTG_GEMRI),(MAP_FGTG(1,2),            560
     *MFGTG_GEMRI)                                                        560
      CHARACTER CFGTG_GEMRI*40/' '/                                       560
      CHARACTER NFGTG_GEMRI*12/'R.GEMRI'/                                 560
      INTEGER MFGTG_GEMRI(2)/1,1/                                         560
      REAL RE_FGTG_GEMRI                                                  560
C -                                                                       560
      REAL FGTG_GEMRO                                                     560
      EQUIVALENCE (BANK_FGTG(5),FGTG_GEMRO,RE_FGTG_GEMRO),                560
     *(NAMES_FGTG(3),NFGTG_GEMRO)                                         560
      EQUIVALENCE (COMMEN_FGTG(3),CFGTG_GEMRO),(MAP_FGTG(1,3),            560
     *MFGTG_GEMRO)                                                        560
      CHARACTER CFGTG_GEMRO*40/' '/                                       560
      CHARACTER NFGTG_GEMRO*12/'R.GEMRO'/                                 560
      INTEGER MFGTG_GEMRO(2)/1,1/                                         560
      REAL RE_FGTG_GEMRO                                                  560
C -                                                                       560
      REAL FGTG_ZSTART                                                    560
      EQUIVALENCE (BANK_FGTG(6),FGTG_ZSTART,RE_FGTG_ZSTART),              560
     *(NAMES_FGTG(4),NFGTG_ZSTART)                                        560
      EQUIVALENCE (COMMEN_FGTG(4),CFGTG_ZSTART),(MAP_FGTG(1,4),           560
     *MFGTG_ZSTART)                                                       560
      CHARACTER CFGTG_ZSTART*40/' '/                                      560
      CHARACTER NFGTG_ZSTART*12/'R.ZSTART'/                               560
      INTEGER MFGTG_ZSTART(2)/1,1/                                        560
      REAL RE_FGTG_ZSTART                                                 560
C -                                                                       560
      REAL FGTG_ZDISK(12)                                                 560
      EQUIVALENCE (BANK_FGTG(7),FGTG_ZDISK,RE_FGTG_ZDISK),                560
     *(NAMES_FGTG(5),NFGTG_ZDISK)                                         560
      EQUIVALENCE (COMMEN_FGTG(5),CFGTG_ZDISK),(MAP_FGTG(1,5),            560
     *MFGTG_ZDISK)                                                        560
      CHARACTER CFGTG_ZDISK*40/' '/                                       560
      CHARACTER NFGTG_ZDISK*12/'R.ZDISK'/                                 560
      INTEGER MFGTG_ZDISK(2)/12,1/                                        560
      REAL RE_FGTG_ZDISK                                                  560
C -                                                                       560
      REAL FGTG_SR                                                        560
      EQUIVALENCE (BANK_FGTG(19),FGTG_SR,RE_FGTG_SR),(NAMES_FGTG(6),      560
     *NFGTG_SR)                                                           560
      EQUIVALENCE (COMMEN_FGTG(6),CFGTG_SR),(MAP_FGTG(1,6),MFGTG_SR)      560
      CHARACTER CFGTG_SR*40/' '/                                          560
      CHARACTER NFGTG_SR*12/'R.SR'/                                       560
      INTEGER MFGTG_SR(2)/1,1/                                            560
      REAL RE_FGTG_SR                                                     560
C -                                                                       560
      REAL FGTG_RTHK                                                      560
      EQUIVALENCE (BANK_FGTG(20),FGTG_RTHK,RE_FGTG_RTHK),(NAMES_FGTG(7),  560
     *NFGTG_RTHK)                                                         560
      EQUIVALENCE (COMMEN_FGTG(7),CFGTG_RTHK),(MAP_FGTG(1,7),             560
     *MFGTG_RTHK)                                                         560
      CHARACTER CFGTG_RTHK*40/' '/                                        560
      CHARACTER NFGTG_RTHK*12/'R.RTHK'/                                   560
      INTEGER MFGTG_RTHK(2)/1,1/                                          560
      REAL RE_FGTG_RTHK                                                   560
C -                                                                       560
      REAL FGTG_NDISK                                                     560
      EQUIVALENCE (BANK_FGTG(21),FGTG_NDISK,RE_FGTG_NDISK),               560
     *(NAMES_FGTG(8),NFGTG_NDISK)                                         560
      EQUIVALENCE (COMMEN_FGTG(8),CFGTG_NDISK),(MAP_FGTG(1,8),            560
     *MFGTG_NDISK)                                                        560
      CHARACTER CFGTG_NDISK*40/' '/                                       560
      CHARACTER NFGTG_NDISK*12/'R.NDISK'/                                 560
      INTEGER MFGTG_NDISK(2)/1,1/                                         560
      REAL RE_FGTG_NDISK                                                  560
C -                                                                       560
      REAL FGTG_GR2                                                       560
      EQUIVALENCE (BANK_FGTG(22),FGTG_GR2,RE_FGTG_GR2),(NAMES_FGTG(9),    560
     *NFGTG_GR2)                                                          560
      EQUIVALENCE (COMMEN_FGTG(9),CFGTG_GR2),(MAP_FGTG(1,9),MFGTG_GR2)    560
      CHARACTER CFGTG_GR2*40/' '/                                         560
      CHARACTER NFGTG_GR2*12/'R.GR2'/                                     560
      INTEGER MFGTG_GR2(2)/1,1/                                           560
      REAL RE_FGTG_GR2                                                    560
C -                                                                       560
      REAL FGTG_GR3                                                       560
      EQUIVALENCE (BANK_FGTG(23),FGTG_GR3,RE_FGTG_GR3),(NAMES_FGTG(10),   560
     *NFGTG_GR3)                                                          560
      EQUIVALENCE (COMMEN_FGTG(10),CFGTG_GR3),(MAP_FGTG(1,10),            560
     *MFGTG_GR3)                                                          560
      CHARACTER CFGTG_GR3*40/' '/                                         560
      CHARACTER NFGTG_GR3*12/'R.GR3'/                                     560
      INTEGER MFGTG_GR3(2)/1,1/                                           560
      REAL RE_FGTG_GR3                                                    560
C -                                                                       560
      REAL FGTG_FRAROUT                                                   560
      EQUIVALENCE (BANK_FGTG(24),FGTG_FRAROUT,RE_FGTG_FRAROUT),           560
     *(NAMES_FGTG(11),NFGTG_FRAROUT)                                      560
      EQUIVALENCE (COMMEN_FGTG(11),CFGTG_FRAROUT),(MAP_FGTG(1,11),        560
     *MFGTG_FRAROUT)                                                      560
      CHARACTER CFGTG_FRAROUT*40/' '/                                     560
      CHARACTER NFGTG_FRAROUT*12/'R.FRAROUT'/                             560
      INTEGER MFGTG_FRAROUT(2)/1,1/                                       560
      REAL RE_FGTG_FRAROUT                                                560
C -                                                                       560
      REAL FGTG_FRARIN                                                    560
      EQUIVALENCE (BANK_FGTG(25),FGTG_FRARIN,RE_FGTG_FRARIN),             560
     *(NAMES_FGTG(12),NFGTG_FRARIN)                                       560
      EQUIVALENCE (COMMEN_FGTG(12),CFGTG_FRARIN),(MAP_FGTG(1,12),         560
     *MFGTG_FRARIN)                                                       560
      CHARACTER CFGTG_FRARIN*40/' '/                                      560
      CHARACTER NFGTG_FRARIN*12/'R.FRARIN'/                               560
      INTEGER MFGTG_FRARIN(2)/1,1/                                        560
      REAL RE_FGTG_FRARIN                                                 560
C -                                                                       560
      REAL FGTG_FRATHK                                                    560
      EQUIVALENCE (BANK_FGTG(26),FGTG_FRATHK,RE_FGTG_FRATHK),             560
     *(NAMES_FGTG(13),NFGTG_FRATHK)                                       560
      EQUIVALENCE (COMMEN_FGTG(13),CFGTG_FRATHK),(MAP_FGTG(1,13),         560
     *MFGTG_FRATHK)                                                       560
      CHARACTER CFGTG_FRATHK*40/' '/                                      560
      CHARACTER NFGTG_FRATHK*12/'R.FRATHK'/                               560
      INTEGER MFGTG_FRATHK(2)/1,1/                                        560
      REAL RE_FGTG_FRATHK                                                 560
C -                                                                       560
      REAL FGTG_FPTHK                                                     560
      EQUIVALENCE (BANK_FGTG(27),FGTG_FPTHK,RE_FGTG_FPTHK),               560
     *(NAMES_FGTG(14),NFGTG_FPTHK)                                        560
      EQUIVALENCE (COMMEN_FGTG(14),CFGTG_FPTHK),(MAP_FGTG(1,14),          560
     *MFGTG_FPTHK)                                                        560
      CHARACTER CFGTG_FPTHK*40/' '/                                       560
      CHARACTER NFGTG_FPTHK*12/'R.FPTHK'/                                 560
      INTEGER MFGTG_FPTHK(2)/1,1/                                         560
      REAL RE_FGTG_FPTHK                                                  560
C -                                                                       560
      REAL FGTG_FPCTHK                                                    560
      EQUIVALENCE (BANK_FGTG(28),FGTG_FPCTHK,RE_FGTG_FPCTHK),             560
     *(NAMES_FGTG(15),NFGTG_FPCTHK)                                       560
      EQUIVALENCE (COMMEN_FGTG(15),CFGTG_FPCTHK),(MAP_FGTG(1,15),         560
     *MFGTG_FPCTHK)                                                       560
      CHARACTER CFGTG_FPCTHK*40/' '/                                      560
      CHARACTER NFGTG_FPCTHK*12/'R.FPCTHK'/                               560
      INTEGER MFGTG_FPCTHK(2)/1,1/                                        560
      REAL RE_FGTG_FPCTHK                                                 560
C -                                                                       560
      REAL FGTG_OCTHK                                                     560
      EQUIVALENCE (BANK_FGTG(29),FGTG_OCTHK,RE_FGTG_OCTHK),               560
     *(NAMES_FGTG(16),NFGTG_OCTHK)                                        560
      EQUIVALENCE (COMMEN_FGTG(16),CFGTG_OCTHK),(MAP_FGTG(1,16),          560
     *MFGTG_OCTHK)                                                        560
      CHARACTER CFGTG_OCTHK*40/' '/                                       560
      CHARACTER NFGTG_OCTHK*12/'R.OCTHK'/                                 560
      INTEGER MFGTG_OCTHK(2)/1,1/                                         560
      REAL RE_FGTG_OCTHK                                                  560
C -                                                                       560
      REAL FGTG_OCLEN                                                     560
      EQUIVALENCE (BANK_FGTG(30),FGTG_OCLEN,RE_FGTG_OCLEN),               560
     *(NAMES_FGTG(17),NFGTG_OCLEN)                                        560
      EQUIVALENCE (COMMEN_FGTG(17),CFGTG_OCLEN),(MAP_FGTG(1,17),          560
     *MFGTG_OCLEN)                                                        560
      CHARACTER CFGTG_OCLEN*40/' '/                                       560
      CHARACTER NFGTG_OCLEN*12/'R.OCLEN'/                                 560
      INTEGER MFGTG_OCLEN(2)/1,1/                                         560
      REAL RE_FGTG_OCLEN                                                  560
C -                                                                       560
      REAL FGTG_FRALRIN                                                   560
      EQUIVALENCE (BANK_FGTG(31),FGTG_FRALRIN,RE_FGTG_FRALRIN),           560
     *(NAMES_FGTG(18),NFGTG_FRALRIN)                                      560
      EQUIVALENCE (COMMEN_FGTG(18),CFGTG_FRALRIN),(MAP_FGTG(1,18),        560
     *MFGTG_FRALRIN)                                                      560
      CHARACTER CFGTG_FRALRIN*40/' '/                                     560
      CHARACTER NFGTG_FRALRIN*12/'R.FRALRIN'/                             560
      INTEGER MFGTG_FRALRIN(2)/1,1/                                       560
      REAL RE_FGTG_FRALRIN                                                560
C -                                                                       560
      REAL FGTG_GGSCTHK                                                   560
      EQUIVALENCE (BANK_FGTG(32),FGTG_GGSCTHK,RE_FGTG_GGSCTHK),           560
     *(NAMES_FGTG(19),NFGTG_GGSCTHK)                                      560
      EQUIVALENCE (COMMEN_FGTG(19),CFGTG_GGSCTHK),(MAP_FGTG(1,19),        560
     *MFGTG_GGSCTHK)                                                      560
      CHARACTER CFGTG_GGSCTHK*40/' '/                                     560
      CHARACTER NFGTG_GGSCTHK*12/'R.GGSCTHK'/                             560
      INTEGER MFGTG_GGSCTHK(2)/1,1/                                       560
      REAL RE_FGTG_GGSCTHK                                                560
C -                                                                       560
      REAL FGTG_GGFR4THK                                                  560
      EQUIVALENCE (BANK_FGTG(33),FGTG_GGFR4THK,RE_FGTG_GGFR4THK),         560
     *(NAMES_FGTG(20),NFGTG_GGFR4THK)                                     560
      EQUIVALENCE (COMMEN_FGTG(20),CFGTG_GGFR4THK),(MAP_FGTG(1,20),       560
     *MFGTG_GGFR4THK)                                                     560
      CHARACTER CFGTG_GGFR4THK*40/' '/                                    560
      CHARACTER NFGTG_GGFR4THK*12/'R.GGFR4THK'/                           560
      INTEGER MFGTG_GGFR4THK(2)/1,1/                                      560
      REAL RE_FGTG_GGFR4THK                                               560
C -                                                                       560
      REAL FGTG_GGNTHK                                                    560
      EQUIVALENCE (BANK_FGTG(34),FGTG_GGNTHK,RE_FGTG_GGNTHK),             560
     *(NAMES_FGTG(21),NFGTG_GGNTHK)                                       560
      EQUIVALENCE (COMMEN_FGTG(21),CFGTG_GGNTHK),(MAP_FGTG(1,21),         560
     *MFGTG_GGNTHK)                                                       560
      CHARACTER CFGTG_GGNTHK*40/' '/                                      560
      CHARACTER NFGTG_GGNTHK*12/'R.GGNTHK'/                               560
      INTEGER MFGTG_GGNTHK(2)/1,1/                                        560
      REAL RE_FGTG_GGNTHK                                                 560
C -                                                                       560
      REAL FGTG_GGCUTHK                                                   560
      EQUIVALENCE (BANK_FGTG(35),FGTG_GGCUTHK,RE_FGTG_GGCUTHK),           560
     *(NAMES_FGTG(22),NFGTG_GGCUTHK)                                      560
      EQUIVALENCE (COMMEN_FGTG(22),CFGTG_GGCUTHK),(MAP_FGTG(1,22),        560
     *MFGTG_GGCUTHK)                                                      560
      CHARACTER CFGTG_GGCUTHK*40/' '/                                     560
      CHARACTER NFGTG_GGCUTHK*12/'R.GGCUTHK'/                             560
      INTEGER MFGTG_GGCUTHK(2)/1,1/                                       560
      REAL RE_FGTG_GGCUTHK                                                560
C -                                                                       560
      REAL FGTG_GGKATHK                                                   560
      EQUIVALENCE (BANK_FGTG(36),FGTG_GGKATHK,RE_FGTG_GGKATHK),           560
     *(NAMES_FGTG(23),NFGTG_GGKATHK)                                      560
      EQUIVALENCE (COMMEN_FGTG(23),CFGTG_GGKATHK),(MAP_FGTG(1,23),        560
     *MFGTG_GGKATHK)                                                      560
      CHARACTER CFGTG_GGKATHK*40/' '/                                     560
      CHARACTER NFGTG_GGKATHK*12/'R.GGKATHK'/                             560
      INTEGER MFGTG_GGKATHK(2)/1,1/                                       560
      REAL RE_FGTG_GGKATHK                                                560
C -                                                                       560
      REAL FGTG_GGPATHK                                                   560
      EQUIVALENCE (BANK_FGTG(37),FGTG_GGPATHK,RE_FGTG_GGPATHK),           560
     *(NAMES_FGTG(24),NFGTG_GGPATHK)                                      560
      EQUIVALENCE (COMMEN_FGTG(24),CFGTG_GGPATHK),(MAP_FGTG(1,24),        560
     *MFGTG_GGPATHK)                                                      560
      CHARACTER CFGTG_GGPATHK*40/' '/                                     560
      CHARACTER NFGTG_GGPATHK*12/'R.GGPATHK'/                             560
      INTEGER MFGTG_GGPATHK(2)/1,1/                                       560
      REAL RE_FGTG_GGPATHK                                                560
C -                                                                       560
      REAL FGTG_GGRLTHK                                                   560
      EQUIVALENCE (BANK_FGTG(38),FGTG_GGRLTHK,RE_FGTG_GGRLTHK),           560
     *(NAMES_FGTG(25),NFGTG_GGRLTHK)                                      560
      EQUIVALENCE (COMMEN_FGTG(25),CFGTG_GGRLTHK),(MAP_FGTG(1,25),        560
     *MFGTG_GGRLTHK)                                                      560
      CHARACTER CFGTG_GGRLTHK*40/' '/                                     560
      CHARACTER NFGTG_GGRLTHK*12/'R.GGRLTHK'/                             560
      INTEGER MFGTG_GGRLTHK(2)/1,1/                                       560
      REAL RE_FGTG_GGRLTHK                                                560
C -                                                                       560
      REAL FGTG_APVX                                                      560
      EQUIVALENCE (BANK_FGTG(39),FGTG_APVX,RE_FGTG_APVX),                 560
     *(NAMES_FGTG(26),NFGTG_APVX)                                         560
      EQUIVALENCE (COMMEN_FGTG(26),CFGTG_APVX),(MAP_FGTG(1,26),           560
     *MFGTG_APVX)                                                         560
      CHARACTER CFGTG_APVX*40/' '/                                        560
      CHARACTER NFGTG_APVX*12/'R.APVX'/                                   560
      INTEGER MFGTG_APVX(2)/1,1/                                          560
      REAL RE_FGTG_APVX                                                   560
C -                                                                       560
      REAL FGTG_APVY                                                      560
      EQUIVALENCE (BANK_FGTG(40),FGTG_APVY,RE_FGTG_APVY),                 560
     *(NAMES_FGTG(27),NFGTG_APVY)                                         560
      EQUIVALENCE (COMMEN_FGTG(27),CFGTG_APVY),(MAP_FGTG(1,27),           560
     *MFGTG_APVY)                                                         560
      CHARACTER CFGTG_APVY*40/' '/                                        560
      CHARACTER NFGTG_APVY*12/'R.APVY'/                                   560
      INTEGER MFGTG_APVY(2)/1,1/                                          560
      REAL RE_FGTG_APVY                                                   560
C -                                                                       560
      REAL FGTG_APVZ                                                      560
      EQUIVALENCE (BANK_FGTG(41),FGTG_APVZ,RE_FGTG_APVZ),                 560
     *(NAMES_FGTG(28),NFGTG_APVZ)                                         560
      EQUIVALENCE (COMMEN_FGTG(28),CFGTG_APVZ),(MAP_FGTG(1,28),           560
     *MFGTG_APVZ)                                                         560
      CHARACTER CFGTG_APVZ*40/' '/                                        560
      CHARACTER NFGTG_APVZ*12/'R.APVZ'/                                   560
      INTEGER MFGTG_APVZ(2)/1,1/                                          560
      REAL RE_FGTG_APVZ                                                   560
C -                                                                       560
      REAL FGTG_APVANG                                                    560
      EQUIVALENCE (BANK_FGTG(42),FGTG_APVANG,RE_FGTG_APVANG),             560
     *(NAMES_FGTG(29),NFGTG_APVANG)                                       560
      EQUIVALENCE (COMMEN_FGTG(29),CFGTG_APVANG),(MAP_FGTG(1,29),         560
     *MFGTG_APVANG)                                                       560
      CHARACTER CFGTG_APVANG*40/' '/                                      560
      CHARACTER NFGTG_APVANG*12/'R.APVANG'/                               560
      INTEGER MFGTG_APVANG(2)/1,1/                                        560
      REAL RE_FGTG_APVANG                                                 560
C -                                                                       560
      REAL FGTG_NQUAD                                                     560
      EQUIVALENCE (BANK_FGTG(43),FGTG_NQUAD,RE_FGTG_NQUAD),               560
     *(NAMES_FGTG(30),NFGTG_NQUAD)                                        560
      EQUIVALENCE (COMMEN_FGTG(30),CFGTG_NQUAD),(MAP_FGTG(1,30),          560
     *MFGTG_NQUAD)                                                        560
      CHARACTER CFGTG_NQUAD*40/' '/                                       560
      CHARACTER NFGTG_NQUAD*12/'R.NQUAD'/                                 560
      INTEGER MFGTG_NQUAD(2)/1,1/                                         560
      REAL RE_FGTG_NQUAD                                                  560
C -                                                                       560
      REAL FGTG_APVD                                                      560
      EQUIVALENCE (BANK_FGTG(44),FGTG_APVD,RE_FGTG_APVD),                 560
     *(NAMES_FGTG(31),NFGTG_APVD)                                         560
      EQUIVALENCE (COMMEN_FGTG(31),CFGTG_APVD),(MAP_FGTG(1,31),           560
     *MFGTG_APVD)                                                         560
      CHARACTER CFGTG_APVD*40/' '/                                        560
      CHARACTER NFGTG_APVD*12/'R.APVD'/                                   560
      INTEGER MFGTG_APVD(2)/1,1/                                          560
      REAL RE_FGTG_APVD                                                   560
C -                                                                       560
      REAL FGTG_APVY2                                                     560
      EQUIVALENCE (BANK_FGTG(45),FGTG_APVY2,RE_FGTG_APVY2),               560
     *(NAMES_FGTG(32),NFGTG_APVY2)                                        560
      EQUIVALENCE (COMMEN_FGTG(32),CFGTG_APVY2),(MAP_FGTG(1,32),          560
     *MFGTG_APVY2)                                                        560
      CHARACTER CFGTG_APVY2*40/' '/                                       560
      CHARACTER NFGTG_APVY2*12/'R.APVY2'/                                 560
      INTEGER MFGTG_APVY2(2)/1,1/                                         560
      REAL RE_FGTG_APVY2                                                  560
C -                                                                       560
      REAL FGTG_APVDY                                                     560
      EQUIVALENCE (BANK_FGTG(46),FGTG_APVDY,RE_FGTG_APVDY),               560
     *(NAMES_FGTG(33),NFGTG_APVDY)                                        560
      EQUIVALENCE (COMMEN_FGTG(33),CFGTG_APVDY),(MAP_FGTG(1,33),          560
     *MFGTG_APVDY)                                                        560
      CHARACTER CFGTG_APVDY*40/' '/                                       560
      CHARACTER NFGTG_APVDY*12/'R.APVDY'/                                 560
      INTEGER MFGTG_APVDY(2)/1,1/                                         560
      REAL RE_FGTG_APVDY                                                  560
C -                                                                       560
      REAL FGTG_APVDDZ                                                    560
      EQUIVALENCE (BANK_FGTG(47),FGTG_APVDDZ,RE_FGTG_APVDDZ),             560
     *(NAMES_FGTG(34),NFGTG_APVDDZ)                                       560
      EQUIVALENCE (COMMEN_FGTG(34),CFGTG_APVDDZ),(MAP_FGTG(1,34),         560
     *MFGTG_APVDDZ)                                                       560
      CHARACTER CFGTG_APVDDZ*40/' '/                                      560
      CHARACTER NFGTG_APVDDZ*12/'R.APVDDZ'/                               560
      INTEGER MFGTG_APVDDZ(2)/1,1/                                        560
      REAL RE_FGTG_APVDDZ                                                 560
C -                                                                       560
      REAL FGTG_APVDDY                                                    560
      EQUIVALENCE (BANK_FGTG(48),FGTG_APVDDY,RE_FGTG_APVDDY),             560
     *(NAMES_FGTG(35),NFGTG_APVDDY)                                       560
      EQUIVALENCE (COMMEN_FGTG(35),CFGTG_APVDDY),(MAP_FGTG(1,35),         560
     *MFGTG_APVDDY)                                                       560
      CHARACTER CFGTG_APVDDY*40/' '/                                      560
      CHARACTER NFGTG_APVDDY*12/'R.APVDDY'/                               560
      INTEGER MFGTG_APVDDY(2)/1,1/                                        560
      REAL RE_FGTG_APVDDY                                                 560
C -                                                                       560
      REAL FGTG_FGORRAD                                                   560
      EQUIVALENCE (BANK_FGTG(49),FGTG_FGORRAD,RE_FGTG_FGORRAD),           560
     *(NAMES_FGTG(36),NFGTG_FGORRAD)                                      560
      EQUIVALENCE (COMMEN_FGTG(36),CFGTG_FGORRAD),(MAP_FGTG(1,36),        560
     *MFGTG_FGORRAD)                                                      560
      CHARACTER CFGTG_FGORRAD*40/' '/                                     560
      CHARACTER NFGTG_FGORRAD*12/'R.FGORRAD'/                             560
      INTEGER MFGTG_FGORRAD(2)/1,1/                                       560
      REAL RE_FGTG_FGORRAD                                                560
C -                                                                       560
      REAL FGTG_FGORANG                                                   560
      EQUIVALENCE (BANK_FGTG(50),FGTG_FGORANG,RE_FGTG_FGORANG),           560
     *(NAMES_FGTG(37),NFGTG_FGORANG)                                      560
      EQUIVALENCE (COMMEN_FGTG(37),CFGTG_FGORANG),(MAP_FGTG(1,37),        560
     *MFGTG_FGORANG)                                                      560
      CHARACTER CFGTG_FGORANG*40/' '/                                     560
      CHARACTER NFGTG_FGORANG*12/'R.FGORANG'/                             560
      INTEGER MFGTG_FGORANG(2)/1,1/                                       560
      REAL RE_FGTG_FGORANG                                                560
C -                                                                       560
      REAL FGTG_FGOADELR                                                  560
      EQUIVALENCE (BANK_FGTG(51),FGTG_FGOADELR,RE_FGTG_FGOADELR),         560
     *(NAMES_FGTG(38),NFGTG_FGOADELR)                                     560
      EQUIVALENCE (COMMEN_FGTG(38),CFGTG_FGOADELR),(MAP_FGTG(1,38),       560
     *MFGTG_FGOADELR)                                                     560
      CHARACTER CFGTG_FGOADELR*40/' '/                                    560
      CHARACTER NFGTG_FGOADELR*12/'R.FGOADELR'/                           560
      INTEGER MFGTG_FGOADELR(2)/1,1/                                      560
      REAL RE_FGTG_FGOADELR                                               560
C -                                                                       560
      REAL FGTG_FGOADELZ                                                  560
      EQUIVALENCE (BANK_FGTG(52),FGTG_FGOADELZ,RE_FGTG_FGOADELZ),         560
     *(NAMES_FGTG(39),NFGTG_FGOADELZ)                                     560
      EQUIVALENCE (COMMEN_FGTG(39),CFGTG_FGOADELZ),(MAP_FGTG(1,39),       560
     *MFGTG_FGOADELZ)                                                     560
      CHARACTER CFGTG_FGOADELZ*40/' '/                                    560
      CHARACTER NFGTG_FGOADELZ*12/'R.FGOADELZ'/                           560
      INTEGER MFGTG_FGOADELZ(2)/1,1/                                      560
      REAL RE_FGTG_FGOADELZ                                               560
C -                                                                       560
      REAL FGTG_FGOUDELR                                                  560
      EQUIVALENCE (BANK_FGTG(53),FGTG_FGOUDELR,RE_FGTG_FGOUDELR),         560
     *(NAMES_FGTG(40),NFGTG_FGOUDELR)                                     560
      EQUIVALENCE (COMMEN_FGTG(40),CFGTG_FGOUDELR),(MAP_FGTG(1,40),       560
     *MFGTG_FGOUDELR)                                                     560
      CHARACTER CFGTG_FGOUDELR*40/' '/                                    560
      CHARACTER NFGTG_FGOUDELR*12/'R.FGOUDELR'/                           560
      INTEGER MFGTG_FGOUDELR(2)/1,1/                                      560
      REAL RE_FGTG_FGOUDELR                                               560
C -                                                                       560
      REAL FGTG_FGOUANG                                                   560
      EQUIVALENCE (BANK_FGTG(54),FGTG_FGOUANG,RE_FGTG_FGOUANG),           560
     *(NAMES_FGTG(41),NFGTG_FGOUANG)                                      560
      EQUIVALENCE (COMMEN_FGTG(41),CFGTG_FGOUANG),(MAP_FGTG(1,41),        560
     *MFGTG_FGOUANG)                                                      560
      CHARACTER CFGTG_FGOUANG*40/' '/                                     560
      CHARACTER NFGTG_FGOUANG*12/'R.FGOUANG'/                             560
      INTEGER MFGTG_FGOUANG(2)/1,1/                                       560
      REAL RE_FGTG_FGOUANG                                                560
C -                                                                       560
      REAL FGTG_FGOUDELA                                                  560
      EQUIVALENCE (BANK_FGTG(55),FGTG_FGOUDELA,RE_FGTG_FGOUDELA),         560
     *(NAMES_FGTG(42),NFGTG_FGOUDELA)                                     560
      EQUIVALENCE (COMMEN_FGTG(42),CFGTG_FGOUDELA),(MAP_FGTG(1,42),       560
     *MFGTG_FGOUDELA)                                                     560
      CHARACTER CFGTG_FGOUDELA*40/' '/                                    560
      CHARACTER NFGTG_FGOUDELA*12/'R.FGOUDELA'/                           560
      INTEGER MFGTG_FGOUDELA(2)/1,1/                                      560
      REAL RE_FGTG_FGOUDELA                                               560
C -                                                                       560
      REAL FGTG_NCABLES                                                   560
      EQUIVALENCE (BANK_FGTG(56),FGTG_NCABLES,RE_FGTG_NCABLES),           560
     *(NAMES_FGTG(43),NFGTG_NCABLES)                                      560
      EQUIVALENCE (COMMEN_FGTG(43),CFGTG_NCABLES),(MAP_FGTG(1,43),        560
     *MFGTG_NCABLES)                                                      560
      CHARACTER CFGTG_NCABLES*40/' '/                                     560
      CHARACTER NFGTG_NCABLES*12/'R.NCABLES'/                             560
      INTEGER MFGTG_NCABLES(2)/1,1/                                       560
      REAL RE_FGTG_NCABLES                                                560
C -                                                                       560
      REAL FGTG_IFCRIN                                                    560
      EQUIVALENCE (BANK_FGTG(57),FGTG_IFCRIN,RE_FGTG_IFCRIN),             560
     *(NAMES_FGTG(44),NFGTG_IFCRIN)                                       560
      EQUIVALENCE (COMMEN_FGTG(44),CFGTG_IFCRIN),(MAP_FGTG(1,44),         560
     *MFGTG_IFCRIN)                                                       560
      CHARACTER CFGTG_IFCRIN*40/' '/                                      560
      CHARACTER NFGTG_IFCRIN*12/'R.IFCRIN'/                               560
      INTEGER MFGTG_IFCRIN(2)/1,1/                                        560
      REAL RE_FGTG_IFCRIN                                                 560
C -                                                                       560
      REAL FGTG_IFCTUBRIN                                                 560
      EQUIVALENCE (BANK_FGTG(58),FGTG_IFCTUBRIN,RE_FGTG_IFCTUBRIN),       560
     *(NAMES_FGTG(45),NFGTG_IFCTUBRIN)                                    560
      EQUIVALENCE (COMMEN_FGTG(45),CFGTG_IFCTUBRIN),(MAP_FGTG(1,45),      560
     *MFGTG_IFCTUBRIN)                                                    560
      CHARACTER CFGTG_IFCTUBRIN*40/' '/                                   560
      CHARACTER NFGTG_IFCTUBRIN*12/'R.IFCTUBRIN'/                         560
      INTEGER MFGTG_IFCTUBRIN(2)/1,1/                                     560
      REAL RE_FGTG_IFCTUBRIN                                              560
C -                                                                       560
      REAL FGTG_IFCTUBROUT                                                560
      EQUIVALENCE (BANK_FGTG(59),FGTG_IFCTUBROUT,RE_FGTG_IFCTUBROUT),     560
     *(NAMES_FGTG(46),NFGTG_IFCTUBROUT)                                   560
      EQUIVALENCE (COMMEN_FGTG(46),CFGTG_IFCTUBROUT),(MAP_FGTG(1,46),     560
     *MFGTG_IFCTUBROUT)                                                   560
      CHARACTER CFGTG_IFCTUBROUT*40/' '/                                  560
      CHARACTER NFGTG_IFCTUBROUT*12/'R.IFCTUBROUT'/                       560
      INTEGER MFGTG_IFCTUBROUT(2)/1,1/                                    560
      REAL RE_FGTG_IFCTUBROUT                                             560
C -                                                                       560
      REAL FGTG_IFCLENZ                                                   560
      EQUIVALENCE (BANK_FGTG(60),FGTG_IFCLENZ,RE_FGTG_IFCLENZ),           560
     *(NAMES_FGTG(47),NFGTG_IFCLENZ)                                      560
      EQUIVALENCE (COMMEN_FGTG(47),CFGTG_IFCLENZ),(MAP_FGTG(1,47),        560
     *MFGTG_IFCLENZ)                                                      560
      CHARACTER CFGTG_IFCLENZ*40/' '/                                     560
      CHARACTER NFGTG_IFCLENZ*12/'R.IFCLENZ'/                             560
      INTEGER MFGTG_IFCLENZ(2)/1,1/                                       560
      REAL RE_FGTG_IFCLENZ                                                560
C -                                                                       560
      REAL FGTG_IFCRESANG                                                 560
      EQUIVALENCE (BANK_FGTG(61),FGTG_IFCRESANG,RE_FGTG_IFCRESANG),       560
     *(NAMES_FGTG(48),NFGTG_IFCRESANG)                                    560
      EQUIVALENCE (COMMEN_FGTG(48),CFGTG_IFCRESANG),(MAP_FGTG(1,48),      560
     *MFGTG_IFCRESANG)                                                    560
      CHARACTER CFGTG_IFCRESANG*40/' '/                                   560
      CHARACTER NFGTG_IFCRESANG*12/'R.IFCRESANG'/                         560
      INTEGER MFGTG_IFCRESANG(2)/1,1/                                     560
      REAL RE_FGTG_IFCRESANG                                              560
C -                                                                       560
      REAL FGTG_IFCTUBDELA                                                560
      EQUIVALENCE (BANK_FGTG(62),FGTG_IFCTUBDELA,RE_FGTG_IFCTUBDELA),     560
     *(NAMES_FGTG(49),NFGTG_IFCTUBDELA)                                   560
      EQUIVALENCE (COMMEN_FGTG(49),CFGTG_IFCTUBDELA),(MAP_FGTG(1,49),     560
     *MFGTG_IFCTUBDELA)                                                   560
      CHARACTER CFGTG_IFCTUBDELA*40/' '/                                  560
      CHARACTER NFGTG_IFCTUBDELA*12/'R.IFCTUBDELA'/                       560
      INTEGER MFGTG_IFCTUBDELA(2)/1,1/                                    560
      REAL RE_FGTG_IFCTUBDELA                                             560
C -                                                                       560
      REAL FGTG_IFCRESDELA                                                560
      EQUIVALENCE (BANK_FGTG(63),FGTG_IFCRESDELA,RE_FGTG_IFCRESDELA),     560
     *(NAMES_FGTG(50),NFGTG_IFCRESDELA)                                   560
      EQUIVALENCE (COMMEN_FGTG(50),CFGTG_IFCRESDELA),(MAP_FGTG(1,50),     560
     *MFGTG_IFCRESDELA)                                                   560
      CHARACTER CFGTG_IFCRESDELA*40/' '/                                  560
      CHARACTER NFGTG_IFCRESDELA*12/'R.IFCRESDELA'/                       560
      INTEGER MFGTG_IFCRESDELA(2)/1,1/                                    560
      REAL RE_FGTG_IFCRESDELA                                             560
C -                                                                       560
      REAL FGTG_IFCRESDELR                                                560
      EQUIVALENCE (BANK_FGTG(64),FGTG_IFCRESDELR,RE_FGTG_IFCRESDELR),     560
     *(NAMES_FGTG(51),NFGTG_IFCRESDELR)                                   560
      EQUIVALENCE (COMMEN_FGTG(51),CFGTG_IFCRESDELR),(MAP_FGTG(1,51),     560
     *MFGTG_IFCRESDELR)                                                   560
      CHARACTER CFGTG_IFCRESDELR*40/' '/                                  560
      CHARACTER NFGTG_IFCRESDELR*12/'R.IFCRESDELR'/                       560
      INTEGER MFGTG_IFCRESDELR(2)/1,1/                                    560
      REAL RE_FGTG_IFCRESDELR                                             560
C -                                                                       560
      REAL FGTG_RINLES                                                    560
      EQUIVALENCE (BANK_FGTG(65),FGTG_RINLES,RE_FGTG_RINLES),             560
     *(NAMES_FGTG(52),NFGTG_RINLES)                                       560
      EQUIVALENCE (COMMEN_FGTG(52),CFGTG_RINLES),(MAP_FGTG(1,52),         560
     *MFGTG_RINLES)                                                       560
      CHARACTER CFGTG_RINLES*40/' '/                                      560
      CHARACTER NFGTG_RINLES*12/'R.RINLES'/                               560
      INTEGER MFGTG_RINLES(2)/1,1/                                        560
      REAL RE_FGTG_RINLES                                                 560
C -                                                                       560
      INTEGER LEN_FGTG,LLEN_FGTG,LINK_FGTG                                560
      PARAMETER (LEN_FGTG=65)                                             560
      PARAMETER (LLEN_FGTG=52)                                            560
      INTEGER FLAG_FGTG/0/,MAP_FGTG(2,LLEN_FGTG),IDNUM_FGTG(10)           560
      CHARACTER NAMES_FGTG(LLEN_FGTG)*12,COMMEN_FGTG(LLEN_FGTG)*40        560
      DATA IDNUM_FGTG/10*0/                                               560
      REAL BANK_FGTG(LEN_FGTG)                                            560
      CHARACTER*60 BPATH_FGTG/' '/                                        560
      SAVE FLAG_FGTG,MAP_FGTG,IDNUM_FGTG,NAMES_FGTG,COMMEN_FGTG,          560
     *BPATH_FGTG                                                          560
      DATA LINK_FGTG/0/,BANK_FGTG/LEN_FGTG*0/                             560
C *                                                                       560
* ----------------------------------------------------------------------
C *                                                                       563
      CALL AGSBEGM(MODULE,IPRIN)                                          563
      IF (IPRIN.LT.0) RETURN                                              563
C Filling bank FGTG - INNER GEM TRACKER GEOMETRY DATA                     563
      BANK_TITLE='INNER GEM TRACKER GEOMETRY DATA'                        563
      BPATH_FGTG='FGTG*'                                                  563
      IDNUM_FGTG(1)=0                                                     564
C INTERNAL RECONFIGURATION OF UPGR16, 3=DEFAULT=ALL_IN                    564
      FGTG_CONFIGJAN=3                                                    564
      CFGTG_CONFIGJAN='INTERNAL RECONFIGURATION OF UPGR16, 3=DEFAULT=ALL  564
     *_IN'                                                                564
C OUTER GAP BETWEEN FGT DISK AND SKIN                                     566
      FGTG_GR2=0.3                                                        566
      CFGTG_GR2='OUTER GAP BETWEEN FGT DISK AND SKIN'                     566
C OUTER ARTIFICIAL GAP BEYOND SKIN FOR SERVICES                           567
      FGTG_GR3=2.3                                                        567
      CFGTG_GR3='OUTER ARTIFICIAL GAP BEYOND SKIN FOR SERVICES'           567
C OUTER RADIUS OF THE FRONT ALU RING                                      568
      FGTG_FRAROUT=22.45                                                  568
      CFGTG_FRAROUT='OUTER RADIUS OF THE FRONT ALU RING'                  568
C INNER RADIUS OF THE FRONT ALU RING                                      569
      FGTG_FRARIN=17.45                                                   569
      CFGTG_FRARIN='INNER RADIUS OF THE FRONT ALU RING'                   569
C INNER RADIUS OF THE LIP OF FRONT ALU RING                               570
      FGTG_FRALRIN=22.20                                                  570
      CFGTG_FRALRIN='INNER RADIUS OF THE LIP OF FRONT ALU RING'           570
C THICKNESS OF THE FRONT ALU RING                                         571
      FGTG_FRATHK=0.5                                                     571
      CFGTG_FRATHK='THICKNESS OF THE FRONT ALU RING'                      571
C OVERALL THICKNESS OF THE FRONT PLATE                                    572
      FGTG_FPTHK=2.0                                                      572
      CFGTG_FPTHK='OVERALL THICKNESS OF THE FRONT PLATE'                  572
C THICKNESS OF THE FRONT PLATE CARBON LAYER                               573
      FGTG_FPCTHK=0.1                                                     573
      CFGTG_FPCTHK='THICKNESS OF THE FRONT PLATE CARBON LAYER'            573
C THICKNESS OF THE OUTER CARBON FIBER CYLINDER                            574
      FGTG_OCTHK=0.3                                                      574
      CFGTG_OCTHK='THICKNESS OF THE OUTER CARBON FIBER CYLINDER'          574
C LENGTH OF THE OUTER CARBON FIBER CYLINDER                               575
      FGTG_OCLEN=167.72                                                   575
      CFGTG_OCLEN='LENGTH OF THE OUTER CARBON FIBER CYLINDER'             575
C RADIUS OF THE RAILS                                                     576
      FGTG_FGORRAD=0.3                                                    576
      CFGTG_FGORRAD='RADIUS OF THE RAILS'                                 576
C TILT OF THE RAILS                                                       577
      FGTG_FGORANG=26.                                                    577
      CFGTG_FGORANG='TILT OF THE RAILS'                                   577
C R-THICKNESS OF THE OUTER ALU RING                                       578
      FGTG_FGOADELR=1.0                                                   578
      CFGTG_FGOADELR='R-THICKNESS OF THE OUTER ALU RING'                  578
C THICKNESS OF THE OUTER ALU RING                                         579
      FGTG_FGOADELZ=5.0                                                   579
      CFGTG_FGOADELZ='THICKNESS OF THE OUTER ALU RING'                    579
C NUMBER OF CABLE BOUNDLES FOR FGT                                        581
      FGTG_NCABLES=3                                                      581
      CFGTG_NCABLES='NUMBER OF CABLE BOUNDLES FOR FGT'                    581
C TILT OF THE CABLES                                                      582
      FGTG_FGOUANG=10.                                                    582
      CFGTG_FGOUANG='TILT OF THE CABLES'                                  582
C ANGULAR SPAN OF THE CABLES FOR 1 DISK                                   583
      FGTG_FGOUDELA=0.67                                                  583
      CFGTG_FGOUDELA='ANGULAR SPAN OF THE CABLES FOR 1 DISK'              583
C R-THICKNESS OF THE FGT CABLES                                           584
      FGTG_FGOUDELR=0.5                                                   584
      CFGTG_FGOUDELR='R-THICKNESS OF THE FGT CABLES'                      584
C # OF DISKS                                                              586
      FGTG_NDISK=8                                                        586
      CFGTG_NDISK='# OF DISKS'                                            586
C INNER RADII FOR PHYSICAL GEM VOLUME.                                    587
      FGTG_GEMRI=10.5                                                     587
      CFGTG_GEMRI='INNER RADII FOR PHYSICAL GEM VOLUME.'                  587
C OUTER RADII FOR PHYSICAL GEM VOLUME.                                    588
      FGTG_GEMRO=38.1                                                     588
      CFGTG_GEMRO='OUTER RADII FOR PHYSICAL GEM VOLUME.'                  588
C STARTING POSITION ALONG Z AXIS                                          589
      FGTG_ZSTART=62.98                                                   589
      CFGTG_ZSTART='STARTING POSITION ALONG Z AXIS'                       589
C Z POSITIONS OF GEM FRONT FACE                                           590
      FGTG_ZDISK(1)=5.4                                                   590
      FGTG_ZDISK(2)=15.4                                                  590
      FGTG_ZDISK(3)=25.4                                                  590
      FGTG_ZDISK(4)=35.4                                                  590
      FGTG_ZDISK(5)=45.4                                                  590
      FGTG_ZDISK(6)=55.4                                                  590
      FGTG_ZDISK(7)=105.                                                  590
      FGTG_ZDISK(8)=155.                                                  590
      CFGTG_ZDISK='Z POSITIONS OF GEM FRONT FACE'                         590
C INER RADII FOR THE 9TH DISK FOR LES                                     591
      FGTG_RINLES=4.1                                                     591
      CFGTG_RINLES='INER RADII FOR THE 9TH DISK FOR LES'                  591
C THICKNESS OF ACTIVE GAS VOLUME                                          593
      FGTG_GGSCTHK=0.3                                                    593
      CFGTG_GGSCTHK='THICKNESS OF ACTIVE GAS VOLUME'                      593
C THICKNESS OF FR4 LAYER IN GEM DISK                                      594
      FGTG_GGFR4THK=0.025                                                 594
      CFGTG_GGFR4THK='THICKNESS OF FR4 LAYER IN GEM DISK'                 594
C THICKNESS OF NOMEX LAYER IN GEM DISK                                    595
      FGTG_GGNTHK=0.5                                                     595
      CFGTG_GGNTHK='THICKNESS OF NOMEX LAYER IN GEM DISK'                 595
C THICKNESS OF COMBINED CU LAYER IN GEM DISK                              596
      FGTG_GGCUTHK=0.02                                                   596
      CFGTG_GGCUTHK='THICKNESS OF COMBINED CU LAYER IN GEM DISK'          596
C THICKNESS OF COMBINED KAPTON LAYER IN GEM DISK                          597
      FGTG_GGKATHK=0.03                                                   597
      CFGTG_GGKATHK='THICKNESS OF COMBINED KAPTON LAYER IN GEM DISK'      597
C THICKNESS OF COMBINED PASSIVE GAS LAYER IN GEM DISK                     598
      FGTG_GGPATHK=0.58                                                   598
      CFGTG_GGPATHK='THICKNESS OF COMBINED PASSIVE GAS LAYER IN GEM DISK  598
     *'                                                                   598
C THICKNESS OF FR4 READOUT LAYER IN GEM DISK                              599
      FGTG_GGRLTHK=0.02                                                   599
      CFGTG_GGRLTHK='THICKNESS OF FR4 READOUT LAYER IN GEM DISK'          599
C RADIAL SIZE FOR INNER & OUTER SPACERS                                   600
      FGTG_SR=1.0                                                         600
      CFGTG_SR='RADIAL SIZE FOR INNER & OUTER SPACERS'                    600
* approximate APV chip mass, at the edge of quadrant boundary
C # OF QUADRANTS PER GEM DISK                                             603
      FGTG_NQUAD=4                                                        603
      CFGTG_NQUAD='# OF QUADRANTS PER GEM DISK'                           603
C LENGTH OF APV ASSEMBLEY (AT TWO QUADRANTS BOUNDARY)                     604
      FGTG_APVX=26.                                                       604
      CFGTG_APVX='LENGTH OF APV ASSEMBLEY (AT TWO QUADRANTS BOUNDARY)'    604
C WIDTH OF APV ASSEMBLEY                                                  605
      FGTG_APVY=6                                                         605
      CFGTG_APVY='WIDTH OF APV ASSEMBLEY'                                 605
C LENGTH OF APV ASSEMBLEY                                                 606
      FGTG_APVZ=6.                                                        606
      CFGTG_APVZ='LENGTH OF APV ASSEMBLEY'                                606
C TILT OF QUADRANT BOUNDARY, DEG                                          607
      FGTG_APVANG=-15.                                                    607
      CFGTG_APVANG='TILT OF QUADRANT BOUNDARY, DEG'                       607
C THICKNESS OF PC BOARDS ,                                                608
      FGTG_APVD=0.25                                                      608
      CFGTG_APVD='THICKNESS OF PC BOARDS ,'                               608
*                 adjusted to yield 700g total for 1 FGT disk (4 assembly)
C Y-WIDTH OF SHORT END-BOARD                                              610
      FGTG_APVY2=3.0                                                      610
      CFGTG_APVY2='Y-WIDTH OF SHORT END-BOARD'                            610
C SEPARATION BETWEEN BLADE-BOARDS                                         611
      FGTG_APVDY=1.2                                                      611
      CFGTG_APVDY='SEPARATION BETWEEN BLADE-BOARDS'                       611
C MIMICS DEAD AREA BETWEEN QUADRANTS                                      612
      FGTG_APVDDY=2.                                                      612
      CFGTG_APVDDY='MIMICS DEAD AREA BETWEEN QUADRANTS'                   612
C MIMICS DEAD AREA BETWEEN QUADRANTS                                      613
      FGTG_APVDDZ=1.                                                      613
      CFGTG_APVDDZ='MIMICS DEAD AREA BETWEEN QUADRANTS'                   613
* IFC resitor chain, one for EAST+WEST
C INNER RADII OF IFC                                                      616
      FGTG_IFCRIN=46.1                                                    616
      CFGTG_IFCRIN='INNER RADII OF IFC'                                   616
C INNER RADII OF CARBON PIPE                                              617
      FGTG_IFCTUBRIN=1.17                                                 617
      CFGTG_IFCTUBRIN='INNER RADII OF CARBON PIPE'                        617
C OUTER RADII OF CARBON PIPE                                              618
      FGTG_IFCTUBROUT=1.27                                                618
      CFGTG_IFCTUBROUT='OUTER RADII OF CARBON PIPE'                       618
C LEN OF CARBIN PIPE                                                      619
      FGTG_IFCLENZ=400                                                    619
      CFGTG_IFCLENZ='LEN OF CARBIN PIPE'                                  619
C PHI LOCATION OF RESITR CHAIN                                            620
      FGTG_IFCRESANG=106.                                                 620
      CFGTG_IFCRESANG='PHI LOCATION OF RESITR CHAIN'                      620
C ANGULAR SEPARATION BETWEEN CARBON PIPES                                 621
      FGTG_IFCTUBDELA=11.3                                                621
      CFGTG_IFCTUBDELA='ANGULAR SEPARATION BETWEEN CARBON PIPES'          621
C ANGULAR WIDTH OF RESITOR STRIP                                          622
      FGTG_IFCRESDELA=2.48                                                622
      CFGTG_IFCRESDELA='ANGULAR WIDTH OF RESITOR STRIP'                   622
C THICKNESS OF RESITOR STRIP                                              623
      FGTG_IFCRESDELR=0.13                                                623
      CFGTG_IFCRESDELR='THICKNESS OF RESITOR STRIP'                       623
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FGTG',    625
     *BPATH_FGTG,IDNUM_FGTG,1, MAP_FGTG,NAMES_FGTG,COMMEN_FGTG,           625
     *BANK_FGTG,LEN_FGTG,LLEN_FGTG,LINK_FGTG,FLAG_FGTG)                   625
C *                                                                       625
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FGTG',    625
     *BPATH_FGTG,IDNUM_FGTG,1, MAP_FGTG,NAMES_FGTG,COMMEN_FGTG,           625
     *BANK_FGTG,LEN_FGTG,LLEN_FGTG,LINK_FGTG,FLAG_FGTG)                   625
*****************************************************************
C use default FGTG bank                                                   629
      BPATH_FGTG='FGTG*'                                                  629
      IDNUM_FGTG(1)=0                                                     629
      CALL AGDGETP('FGTDGEO2','*',-999,'FGTG',' ',BANK_FGTG, BPATH_FGTG,  629
     *IDNUM_FGTG,1,MAP_FGTG,NAMES_FGTG,BANK_FGTG,LEN_FGTG,LLEN_FGTG,      629
     *LINK_FGTG,FLAG_FGTG)                                                629
      PRINT * ,'**********FGT UPGR16 geometry, ConfigJan=',               630
     *FGTG_CONFIGJAN                                                      630
      IF (FGTG_CONFIGJAN.LE.0) THEN                                       631
      PRINT *, '  EMPTY: no FGT disks, no West Cone, no cables, no IST/H  632
     *FT/SSD suport'                                                      632
      RETURN                                                              633
      ELSEIF (FGTG_CONFIGJAN.EQ.1) THEN                                   634
      PRINT *, '  no FGT disks, add West Cone, add cables, no IST/HFT/SS  635
     *D suport'                                                           635
      FGTG_NDISK=6                                                        636
      ELSEIF (FGTG_CONFIGJAN.EQ.2) THEN                                   637
      PRINT *, '  6 FGT disks, add West Cone, add cables, add IST/HFT/SS  638
     *D suport'                                                           638
      FGTG_NDISK=6                                                        639
      ELSEIF (FGTG_CONFIGJAN.EQ.3) THEN                                   640
      PRINT *, '  6 FGT disks, add West Cone, add cables, add IST/HFT/SS  641
     *D suport'                                                           641
      PRINT *, ' and  2 FGT disks at large Z, and 2 large FGT active vol  642
     *umes at both ends of the cone'                                      642
      ENDIF                                                               643
* Calculate some parameters from the input variables.
* Kapton   C22-H10-N2-O4
C Geant COMPONENT : C ===> A=12 Z=6 W=22.0*12.0/366.0 <===                648
      CALL AGSRESET                                                       648
      AG_A = 12                                                           648
      AG_Z = 6                                                            648
      AG_W = 22.0*12.0/366.0                                              648
      AG_TITLE = 'C'                                                      648
      AG_EXNAME = 'COMPONENT'                                             648
      AG_PARLIST = 'A_Z_W'                                                648
      CALL AXCOMPONENT                                                    648
C *                                                                       648
C Geant COMPONENT : H ===> A=1 Z=1 W=10.0*1.0/366.0 <===                  648
      CALL AGSRESET                                                       649
      AG_A = 1                                                            649
      AG_Z = 1                                                            649
      AG_W = 10.0*1.0/366.0                                               649
      AG_TITLE = 'H'                                                      649
      AG_EXNAME = 'COMPONENT'                                             649
      AG_PARLIST = 'A_Z_W'                                                649
      CALL AXCOMPONENT                                                    649
C *                                                                       649
C Geant COMPONENT : N ===> A=14 Z=7 W=2.0*14.0/366.0 <===                 649
      CALL AGSRESET                                                       650
      AG_A = 14                                                           650
      AG_Z = 7                                                            650
      AG_W = 2.0*14.0/366.0                                               650
      AG_TITLE = 'N'                                                      650
      AG_EXNAME = 'COMPONENT'                                             650
      AG_PARLIST = 'A_Z_W'                                                650
      CALL AXCOMPONENT                                                    650
C *                                                                       650
C Geant COMPONENT : O ===> A=16 Z=8 W=4.0*16.0/366.0 <===                 650
      CALL AGSRESET                                                       651
      AG_A = 16                                                           651
      AG_Z = 8                                                            651
      AG_W = 4.0*16.0/366.0                                               651
      AG_TITLE = 'O'                                                      651
      AG_EXNAME = 'COMPONENT'                                             651
      AG_PARLIST = 'A_Z_W'                                                651
      CALL AXCOMPONENT                                                    651
C *                                                                       651
C Geant MIXTURE : KAPTON_DENS1 ===> DENS=1.02 <===                        651
      CALL AGSRESET                                                       652
      AG_DENS = 1.02                                                      652
      AG_TITLE = 'KAPTON_DENS1'                                           652
      AG_EXNAME = 'MIXTURE'                                               652
      AG_PARLIST = 'DENS'                                                 652
      CALL AXMIXTURE                                                      652
C *                                                                       652
* FR4=G10 is about 60% SiO2 and 40% epoxy
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             655
      CALL AGSRESET                                                       655
      AG_A = 28.08                                                        655
      AG_Z = 14                                                           655
      AG_W = 0.6*1*28./60.                                                655
      AG_TITLE = 'SI'                                                     655
      AG_EXNAME = 'COMPONENT'                                             655
      AG_PARLIST = 'A_Z_W'                                                655
      CALL AXCOMPONENT                                                    655
C *                                                                       655
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. <===                  655
      CALL AGSRESET                                                       656
      AG_A = 16                                                           656
      AG_Z = 8                                                            656
      AG_W = 0.6*2*16./60.                                                656
      AG_TITLE = 'O'                                                      656
      AG_EXNAME = 'COMPONENT'                                             656
      AG_PARLIST = 'A_Z_W'                                                656
      CALL AXCOMPONENT                                                    656
C *                                                                       656
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 656
      CALL AGSRESET                                                       657
      AG_A = 12                                                           657
      AG_Z = 6                                                            657
      AG_W = 0.4*8*12./174.                                               657
      AG_TITLE = 'C'                                                      657
      AG_EXNAME = 'COMPONENT'                                             657
      AG_PARLIST = 'A_Z_W'                                                657
      CALL AXCOMPONENT                                                    657
C *                                                                       657
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  657
      CALL AGSRESET                                                       658
      AG_A = 1                                                            658
      AG_Z = 1                                                            658
      AG_W = 0.4*14*1./174.                                               658
      AG_TITLE = 'H'                                                      658
      AG_EXNAME = 'COMPONENT'                                             658
      AG_PARLIST = 'A_Z_W'                                                658
      CALL AXCOMPONENT                                                    658
C *                                                                       658
C Geant COMPONENT : O ===> A=16 Z=8 W=0.4*4*16./174. <===                 658
      CALL AGSRESET                                                       659
      AG_A = 16                                                           659
      AG_Z = 8                                                            659
      AG_W = 0.4*4*16./174.                                               659
      AG_TITLE = 'O'                                                      659
      AG_EXNAME = 'COMPONENT'                                             659
      AG_PARLIST = 'A_Z_W'                                                659
      CALL AXCOMPONENT                                                    659
C *                                                                       659
C Geant MIXTURE : FR4 ===> DENS=1.80 <===                                 659
      CALL AGSRESET                                                       660
      AG_DENS = 1.80                                                      660
      AG_TITLE = 'FR4'                                                    660
      AG_EXNAME = 'MIXTURE'                                               660
      AG_PARLIST = 'DENS'                                                 660
      CALL AXMIXTURE                                                      660
C *                                                                       660
* FR4=G10 is about 60% SiO2 and 40% epoxy
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             663
      CALL AGSRESET                                                       663
      AG_A = 28.08                                                        663
      AG_Z = 14                                                           663
      AG_W = 0.6*1*28./60.                                                663
      AG_TITLE = 'SI'                                                     663
      AG_EXNAME = 'COMPONENT'                                             663
      AG_PARLIST = 'A_Z_W'                                                663
      CALL AXCOMPONENT                                                    663
C *                                                                       663
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. <===                  663
      CALL AGSRESET                                                       664
      AG_A = 16                                                           664
      AG_Z = 8                                                            664
      AG_W = 0.6*2*16./60.                                                664
      AG_TITLE = 'O'                                                      664
      AG_EXNAME = 'COMPONENT'                                             664
      AG_PARLIST = 'A_Z_W'                                                664
      CALL AXCOMPONENT                                                    664
C *                                                                       664
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 664
      CALL AGSRESET                                                       665
      AG_A = 12                                                           665
      AG_Z = 6                                                            665
      AG_W = 0.4*8*12./174.                                               665
      AG_TITLE = 'C'                                                      665
      AG_EXNAME = 'COMPONENT'                                             665
      AG_PARLIST = 'A_Z_W'                                                665
      CALL AXCOMPONENT                                                    665
C *                                                                       665
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  665
      CALL AGSRESET                                                       666
      AG_A = 1                                                            666
      AG_Z = 1                                                            666
      AG_W = 0.4*14*1./174.                                               666
      AG_TITLE = 'H'                                                      666
      AG_EXNAME = 'COMPONENT'                                             666
      AG_PARLIST = 'A_Z_W'                                                666
      CALL AXCOMPONENT                                                    666
C *                                                                       666
C Geant COMPONENT : O ===> A=16 Z=8 W=0.4*4*16./174. <===                 666
      CALL AGSRESET                                                       667
      AG_A = 16                                                           667
      AG_Z = 8                                                            667
      AG_W = 0.4*4*16./174.                                               667
      AG_TITLE = 'O'                                                      667
      AG_EXNAME = 'COMPONENT'                                             667
      AG_PARLIST = 'A_Z_W'                                                667
      CALL AXCOMPONENT                                                    667
C *                                                                       667
C Geant MIXTURE : FR4_DENS1 ===> DENS=1.12 <===                           667
      CALL AGSRESET                                                       668
      AG_DENS = 1.12                                                      668
      AG_TITLE = 'FR4_DENS1'                                              668
      AG_EXNAME = 'MIXTURE'                                               668
      AG_PARLIST = 'DENS'                                                 668
      CALL AXMIXTURE                                                      668
C *                                                                       668
* FR4+5%Cu
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             671
      CALL AGSRESET                                                       671
      AG_A = 28.08                                                        671
      AG_Z = 14                                                           671
      AG_W = 0.6*1*28./60.                                                671
      AG_TITLE = 'SI'                                                     671
      AG_EXNAME = 'COMPONENT'                                             671
      AG_PARLIST = 'A_Z_W'                                                671
      CALL AXCOMPONENT                                                    671
C *                                                                       671
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. <===                  671
      CALL AGSRESET                                                       672
      AG_A = 16                                                           672
      AG_Z = 8                                                            672
      AG_W = 0.6*2*16./60.                                                672
      AG_TITLE = 'O'                                                      672
      AG_EXNAME = 'COMPONENT'                                             672
      AG_PARLIST = 'A_Z_W'                                                672
      CALL AXCOMPONENT                                                    672
C *                                                                       672
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 672
      CALL AGSRESET                                                       673
      AG_A = 12                                                           673
      AG_Z = 6                                                            673
      AG_W = 0.4*8*12./174.                                               673
      AG_TITLE = 'C'                                                      673
      AG_EXNAME = 'COMPONENT'                                             673
      AG_PARLIST = 'A_Z_W'                                                673
      CALL AXCOMPONENT                                                    673
C *                                                                       673
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  673
      CALL AGSRESET                                                       674
      AG_A = 1                                                            674
      AG_Z = 1                                                            674
      AG_W = 0.4*14*1./174.                                               674
      AG_TITLE = 'H'                                                      674
      AG_EXNAME = 'COMPONENT'                                             674
      AG_PARLIST = 'A_Z_W'                                                674
      CALL AXCOMPONENT                                                    674
C *                                                                       674
C Geant COMPONENT : O ===> A=16 Z=8 W=0.4*4*16./174. <===                 674
      CALL AGSRESET                                                       675
      AG_A = 16                                                           675
      AG_Z = 8                                                            675
      AG_W = 0.4*4*16./174.                                               675
      AG_TITLE = 'O'                                                      675
      AG_EXNAME = 'COMPONENT'                                             675
      AG_PARLIST = 'A_Z_W'                                                675
      CALL AXCOMPONENT                                                    675
C *                                                                       675
C Geant COMPONENT : CU ===> A=63.5 Z=29 W=0.05 !EDUCATED GUESS <===       675
      CALL AGSRESET                                                       676
      AG_A = 63.5                                                         676
      AG_Z = 29                                                           676
C EDUCATED GUESS                                                          676
      AG_W = 0.05                                                         676
      AG_TITLE = 'CU'                                                     676
      AG_EXNAME = 'COMPONENT'                                             676
      AG_PARLIST = 'A_Z_W'                                                676
      CALL AXCOMPONENT                                                    676
C *                                                                       676
C Geant MIXTURE : FR4CU ===> DENS=1.8 <===                                676
      CALL AGSRESET                                                       677
      AG_DENS = 1.8                                                       677
      AG_TITLE = 'FR4CU'                                                  677
      AG_EXNAME = 'MIXTURE'                                               677
      AG_PARLIST = 'DENS'                                                 677
      CALL AXMIXTURE                                                      677
C *                                                                       677
*  Nomex - PN  8/3/96 following I.Sacreda note
C Geant COMPONENT : C ===> A=12 Z=6 W=5 <===                              680
      CALL AGSRESET                                                       680
      AG_A = 12                                                           680
      AG_Z = 6                                                            680
      AG_W = 5                                                            680
      AG_TITLE = 'C'                                                      680
      AG_EXNAME = 'COMPONENT'                                             680
      AG_PARLIST = 'A_Z_W'                                                680
      CALL AXCOMPONENT                                                    680
C *                                                                       680
C Geant COMPONENT : H ===> A=1 Z=1 W=8 <===                               680
      CALL AGSRESET                                                       681
      AG_A = 1                                                            681
      AG_Z = 1                                                            681
      AG_W = 8                                                            681
      AG_TITLE = 'H'                                                      681
      AG_EXNAME = 'COMPONENT'                                             681
      AG_PARLIST = 'A_Z_W'                                                681
      CALL AXCOMPONENT                                                    681
C *                                                                       681
C Geant COMPONENT : O ===> A=16 Z=8 W=2 <===                              681
      CALL AGSRESET                                                       682
      AG_A = 16                                                           682
      AG_Z = 8                                                            682
      AG_W = 2                                                            682
      AG_TITLE = 'O'                                                      682
      AG_EXNAME = 'COMPONENT'                                             682
      AG_PARLIST = 'A_Z_W'                                                682
      CALL AXCOMPONENT                                                    682
C *                                                                       682
C Geant MIXTURE : NOMEX ===> DENS=0.032 <===                              682
      CALL AGSRESET                                                       683
      AG_DENS = 0.032                                                     683
      AG_TITLE = 'NOMEX'                                                  683
      AG_EXNAME = 'MIXTURE'                                               683
      AG_PARLIST = 'DENS'                                                 683
      CALL AXMIXTURE                                                      683
C *                                                                       683
* Ar-C02  gass mixture,  70%-30%
C Geant COMPONENT : AR ===> A=39.95 Z=18. W=0.7 <===                      686
      CALL AGSRESET                                                       686
      AG_A = 39.95                                                        686
      AG_Z = 18.                                                          686
      AG_W = 0.7                                                          686
      AG_TITLE = 'AR'                                                     686
      AG_EXNAME = 'COMPONENT'                                             686
      AG_PARLIST = 'A_Z_W'                                                686
      CALL AXCOMPONENT                                                    686
C *                                                                       686
C Geant COMPONENT : C ===> A=12.01 Z=6. W=0.3*1*12.01/44.01 <===          686
      CALL AGSRESET                                                       687
      AG_A = 12.01                                                        687
      AG_Z = 6.                                                           687
      AG_W = 0.3*1*12.01/44.01                                            687
      AG_TITLE = 'C'                                                      687
      AG_EXNAME = 'COMPONENT'                                             687
      AG_PARLIST = 'A_Z_W'                                                687
      CALL AXCOMPONENT                                                    687
C *                                                                       687
C Geant COMPONENT : O ===> A=16. Z=8. W=0.3*2*16./44.01 <===              687
      CALL AGSRESET                                                       688
      AG_A = 16.                                                          688
      AG_Z = 8.                                                           688
      AG_W = 0.3*2*16./44.01                                              688
      AG_TITLE = 'O'                                                      688
      AG_EXNAME = 'COMPONENT'                                             688
      AG_PARLIST = 'A_Z_W'                                                688
      CALL AXCOMPONENT                                                    688
C *                                                                       688
C Geant MIXTURE : ARCO2_70_30 ===> DENS=0.0018015 ISVOL=1 <===            688
      CALL AGSRESET                                                       689
      AG_DENS = 0.0018015                                                 689
      AG_ISVOL = 1                                                        689
      AG_TITLE = 'ARCO2_70_30'                                            689
      AG_EXNAME = 'MIXTURE'                                               689
      AG_PARLIST = 'DENS_ISVOL'                                           689
      CALL AXMIXTURE                                                      689
C *                                                                       689
* PVC material
C Geant COMPONENT : C ===> A=12 Z=6 W=2 <===                              692
      CALL AGSRESET                                                       692
      AG_A = 12                                                           692
      AG_Z = 6                                                            692
      AG_W = 2                                                            692
      AG_TITLE = 'C'                                                      692
      AG_EXNAME = 'COMPONENT'                                             692
      AG_PARLIST = 'A_Z_W'                                                692
      CALL AXCOMPONENT                                                    692
C *                                                                       692
C Geant COMPONENT : H ===> A=1 Z=1 W=3 <===                               692
      CALL AGSRESET                                                       693
      AG_A = 1                                                            693
      AG_Z = 1                                                            693
      AG_W = 3                                                            693
      AG_TITLE = 'H'                                                      693
      AG_EXNAME = 'COMPONENT'                                             693
      AG_PARLIST = 'A_Z_W'                                                693
      CALL AXCOMPONENT                                                    693
C *                                                                       693
C Geant COMPONENT : CL ===> A=35 Z=17 W=1 <===                            693
      CALL AGSRESET                                                       694
      AG_A = 35                                                           694
      AG_Z = 17                                                           694
      AG_W = 1                                                            694
      AG_TITLE = 'CL'                                                     694
      AG_EXNAME = 'COMPONENT'                                             694
      AG_PARLIST = 'A_Z_W'                                                694
      CALL AXCOMPONENT                                                    694
C *                                                                       694
C Geant MIXTURE : PVC ===> DENS=1.38 <===                                 694
      CALL AGSRESET                                                       695
      AG_DENS = 1.38                                                      695
      AG_TITLE = 'PVC'                                                    695
      AG_EXNAME = 'MIXTURE'                                               695
      AG_PARLIST = 'DENS'                                                 695
      CALL AXMIXTURE                                                      695
C *                                                                       695
* SSD  cables & isolation mix
C Geant COMPONENT : C ===> A=12 Z=6 W=2 <===                              698
      CALL AGSRESET                                                       698
      AG_A = 12                                                           698
      AG_Z = 6                                                            698
      AG_W = 2                                                            698
      AG_TITLE = 'C'                                                      698
      AG_EXNAME = 'COMPONENT'                                             698
      AG_PARLIST = 'A_Z_W'                                                698
      CALL AXCOMPONENT                                                    698
C *                                                                       698
C Geant COMPONENT : H ===> A=1 Z=1 W=3 <===                               698
      CALL AGSRESET                                                       699
      AG_A = 1                                                            699
      AG_Z = 1                                                            699
      AG_W = 3                                                            699
      AG_TITLE = 'H'                                                      699
      AG_EXNAME = 'COMPONENT'                                             699
      AG_PARLIST = 'A_Z_W'                                                699
      CALL AXCOMPONENT                                                    699
C *                                                                       699
C Geant COMPONENT : CL ===> A=35 Z=17 W=1 <===                            699
      CALL AGSRESET                                                       700
      AG_A = 35                                                           700
      AG_Z = 17                                                           700
      AG_W = 1                                                            700
      AG_TITLE = 'CL'                                                     700
      AG_EXNAME = 'COMPONENT'                                             700
      AG_PARLIST = 'A_Z_W'                                                700
      CALL AXCOMPONENT                                                    700
C *                                                                       700
C Geant COMPONENT : CU ===> A=63.5 Z=29 W=0.20 <===                       700
      CALL AGSRESET                                                       701
      AG_A = 63.5                                                         701
      AG_Z = 29                                                           701
      AG_W = 0.20                                                         701
      AG_TITLE = 'CU'                                                     701
      AG_EXNAME = 'COMPONENT'                                             701
      AG_PARLIST = 'A_Z_W'                                                701
      CALL AXCOMPONENT                                                    701
C *                                                                       701
C Geant MIXTURE : CABLE_SSD ===> DENS=1.6 <===                            701
      CALL AGSRESET                                                       702
      AG_DENS = 1.6                                                       702
      AG_TITLE = 'CABLE_SSD'                                              702
      AG_EXNAME = 'MIXTURE'                                               702
      AG_PARLIST = 'DENS'                                                 702
      CALL AXMIXTURE                                                      702
C *                                                                       702
* FGT  cables & isolation mix
C Geant COMPONENT : C ===> A=12 Z=6 W=2 <===                              705
      CALL AGSRESET                                                       705
      AG_A = 12                                                           705
      AG_Z = 6                                                            705
      AG_W = 2                                                            705
      AG_TITLE = 'C'                                                      705
      AG_EXNAME = 'COMPONENT'                                             705
      AG_PARLIST = 'A_Z_W'                                                705
      CALL AXCOMPONENT                                                    705
C *                                                                       705
C Geant COMPONENT : H ===> A=1 Z=1 W=3 <===                               705
      CALL AGSRESET                                                       706
      AG_A = 1                                                            706
      AG_Z = 1                                                            706
      AG_W = 3                                                            706
      AG_TITLE = 'H'                                                      706
      AG_EXNAME = 'COMPONENT'                                             706
      AG_PARLIST = 'A_Z_W'                                                706
      CALL AXCOMPONENT                                                    706
C *                                                                       706
C Geant COMPONENT : CL ===> A=35 Z=17 W=1 <===                            706
      CALL AGSRESET                                                       707
      AG_A = 35                                                           707
      AG_Z = 17                                                           707
      AG_W = 1                                                            707
      AG_TITLE = 'CL'                                                     707
      AG_EXNAME = 'COMPONENT'                                             707
      AG_PARLIST = 'A_Z_W'                                                707
      CALL AXCOMPONENT                                                    707
C *                                                                       707
C Geant COMPONENT : CU ===> A=63.5 Z=29 W=2.0 <===                        707
      CALL AGSRESET                                                       708
      AG_A = 63.5                                                         708
      AG_Z = 29                                                           708
      AG_W = 2.0                                                          708
      AG_TITLE = 'CU'                                                     708
      AG_EXNAME = 'COMPONENT'                                             708
      AG_PARLIST = 'A_Z_W'                                                708
      CALL AXCOMPONENT                                                    708
C *                                                                       708
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.12 <===                         708
      CALL AGSRESET                                                       709
      AG_A = 27                                                           709
      AG_Z = 13                                                           709
      AG_W = 0.12                                                         709
      AG_TITLE = 'AL'                                                     709
      AG_EXNAME = 'COMPONENT'                                             709
      AG_PARLIST = 'A_Z_W'                                                709
      CALL AXCOMPONENT                                                    709
C *                                                                       709
C Geant MIXTURE : CABLE_FGT ===> DENS=3.2 <===                            709
      CALL AGSRESET                                                       710
      AG_DENS = 3.2                                                       710
      AG_TITLE = 'CABLE_FGT'                                              710
      AG_EXNAME = 'MIXTURE'                                               710
      AG_PARLIST = 'DENS'                                                 710
      CALL AXMIXTURE                                                      710
C *                                                                       710
* Alumnia - ceramic ued for resistors
C Geant COMPONENT : AL ===> A=27 Z=13 W=2 <===                            713
      CALL AGSRESET                                                       713
      AG_A = 27                                                           713
      AG_Z = 13                                                           713
      AG_W = 2                                                            713
      AG_TITLE = 'AL'                                                     713
      AG_EXNAME = 'COMPONENT'                                             713
      AG_PARLIST = 'A_Z_W'                                                713
      CALL AXCOMPONENT                                                    713
C *                                                                       713
C Geant COMPONENT : O ===> A=16 Z=8 W=3 <===                              713
      CALL AGSRESET                                                       714
      AG_A = 16                                                           714
      AG_Z = 8                                                            714
      AG_W = 3                                                            714
      AG_TITLE = 'O'                                                      714
      AG_EXNAME = 'COMPONENT'                                             714
      AG_PARLIST = 'A_Z_W'                                                714
      CALL AXCOMPONENT                                                    714
C *                                                                       714
C Geant MIXTURE : ALUMINA ===> DENS=3.90 <===                             714
      CALL AGSRESET                                                       715
      AG_DENS = 3.90                                                      715
      AG_TITLE = 'ALUMINA'                                                715
      AG_EXNAME = 'MIXTURE'                                               715
      AG_PARLIST = 'DENS'                                                 715
      CALL AXMIXTURE                                                      715
C *                                                                       715
* Definition for Carbon fiber
C Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                              718
      CALL AGSRESET                                                       718
      AG_A = 12                                                           718
      AG_Z = 6                                                            718
      AG_W = 1                                                            718
      AG_TITLE = 'C'                                                      718
      AG_EXNAME = 'COMPONENT'                                             718
      AG_PARLIST = 'A_Z_W'                                                718
      CALL AXCOMPONENT                                                    718
C *                                                                       718
C Geant MIXTURE : CFIBER ===> DENS=1.713 <===                             718
      CALL AGSRESET                                                       719
      AG_DENS = 1.713                                                     719
      AG_TITLE = 'CFIBER'                                                 719
      AG_EXNAME = 'MIXTURE'                                               719
      AG_PARLIST = 'DENS'                                                 719
      CALL AXMIXTURE                                                      719
C *                                                                       719
* Definition for dilluted copper
C Geant COMPONENT : CU ===> A=63.540 Z=29 W=1 <===                        722
      CALL AGSRESET                                                       722
      AG_A = 63.540                                                       722
      AG_Z = 29                                                           722
      AG_W = 1                                                            722
      AG_TITLE = 'CU'                                                     722
      AG_EXNAME = 'COMPONENT'                                             722
      AG_PARLIST = 'A_Z_W'                                                722
      CALL AXCOMPONENT                                                    722
C *                                                                       722
C Geant MIXTURE : COPPER_DENS1 ===> DENS=1.16 <===                        722
      CALL AGSRESET                                                       723
      AG_DENS = 1.16                                                      723
      AG_TITLE = 'COPPER_DENS1'                                           723
      AG_EXNAME = 'MIXTURE'                                               723
      AG_PARLIST = 'DENS'                                                 723
      CALL AXMIXTURE                                                      723
C *                                                                       723
C Check FGTG_NDISK>MXDISK                                                 726
      IF (FGTG_NDISK.GT.MXDISK) THEN                                      726
      PRINT *, 'nDisk=',FGTG_NDISK,' is too large, fix the code,JB'       727
      CALL EXIT(1)                                                        728
      ENDIF                                                               729
* Calculate some secondary dimensions
      THICKGG=2*FGTG_GGFR4THK +FGTG_GGNTHK +FGTG_GGCUTHK +FGTG_GGKATHK    733
     *+FGTG_GGPATHK +FGTG_GGSCTHK+ FGTG_GGRLTHK                           733
      PRINT *,'thickGG=',THICKGG                                          734
      RIN = FGTG_GEMRI                                                    735
      ROUT = FGTG_GEMRO+ FGTG_OCTHK +FGTG_GR2                             736
      LENGTHZ = FGTG_FPTHK+FGTG_OCLEN                                     737
* Calculate centre of assembly.
      CENTERZ = FGTG_ZSTART + LENGTHZ / 2.0                               740
* SSD simplified mounting Alu ring
      CALL AGSBEGM(MODULE,IPRIN)                                          743
C Check IPRIN<0                                                           743
      IF (IPRIN.LT.0) RETURN                                              743
C Create FGXR :                                                           743
C Call To Local Procedure FGXR                                            743
      ASSIGN 5020 TO BLOCK_FGXR                                           743
      GO TO  5010                                                         743
5020  CONTINUE                                                            743
C *                                                                       743
C Geant POSITION : FGXR ===> MOTHER='CAVE' Z=57 <===                      744
      CALL AGSRESET                                                       744
      AG_MOTHER = 'CAVE'                                                  744
      AG_Z = 57                                                           744
      AG_TITLE = 'FGXR'                                                   744
      AG_EXNAME = 'POSITION'                                              744
      AG_PARLIST = 'MOTHER_Z'                                             744
      CALL AXPOSITION                                                     744
C *                                                                       744
* SSD cables in front of FGT
      CALL AGSBEGM(MODULE,IPRIN)                                          746
C Check IPRIN<0                                                           746
      IF (IPRIN.LT.0) RETURN                                              746
C Create FGSV :                                                           746
C Call To Local Procedure FGSV                                            746
      ASSIGN 5040 TO BLOCK_FGSV                                           746
      GO TO  5030                                                         746
5040  CONTINUE                                                            746
C *                                                                       746
C Geant POSITION : FGSV ===> MOTHER='CAVE' Z=58. <===                     747
      CALL AGSRESET                                                       747
      AG_MOTHER = 'CAVE'                                                  747
      AG_Z = 58.                                                          747
      AG_TITLE = 'FGSV'                                                   747
      AG_EXNAME = 'POSITION'                                              747
      AG_PARLIST = 'MOTHER_Z'                                             747
      CALL AXPOSITION                                                     747
C *                                                                       747
      IF (FGTG_CONFIGJAN.GT.1) THEN                                       749
* SSD simplified supporting Carbon ring
      CALL AGSBEGM(MODULE,IPRIN)                                          751
C Check IPRIN<0                                                           751
      IF (IPRIN.LT.0) RETURN                                              751
C Create FGXC :                                                           751
C Call To Local Procedure FGXC                                            751
      ASSIGN 5060 TO BLOCK_FGXC                                           751
      GO TO  5050                                                         751
5060  CONTINUE                                                            751
C *                                                                       751
C Geant POSITION : FGXC ===> MOTHER='CAVE' Z=51.0 <===                    752
      CALL AGSRESET                                                       752
      AG_MOTHER = 'CAVE'                                                  752
      AG_Z = 51.0                                                         752
      AG_TITLE = 'FGXC'                                                   752
      AG_EXNAME = 'POSITION'                                              752
      AG_PARLIST = 'MOTHER_Z'                                             752
      CALL AXPOSITION                                                     752
C *                                                                       752
* IST simplified supporting Carbon ring
      CALL AGSBEGM(MODULE,IPRIN)                                          754
C Check IPRIN<0                                                           754
      IF (IPRIN.LT.0) RETURN                                              754
C Create FGXD :                                                           754
C Call To Local Procedure FGXD                                            754
      ASSIGN 5080 TO BLOCK_FGXD                                           754
      GO TO  5070                                                         754
5080  CONTINUE                                                            754
C *                                                                       754
C Geant POSITION : FGXD ===> MOTHER='CAVE' Z=24.0 <===                    755
      CALL AGSRESET                                                       755
      AG_MOTHER = 'CAVE'                                                  755
      AG_Z = 24.0                                                         755
      AG_TITLE = 'FGXD'                                                   755
      AG_EXNAME = 'POSITION'                                              755
      AG_PARLIST = 'MOTHER_Z'                                             755
      CALL AXPOSITION                                                     755
C *                                                                       755
* HFT simplified supporting Carbon ring
      CALL AGSBEGM(MODULE,IPRIN)                                          758
C Check IPRIN<0                                                           758
      IF (IPRIN.LT.0) RETURN                                              758
C Create FGXE :                                                           758
C Call To Local Procedure FGXE                                            758
      ASSIGN 5100 TO BLOCK_FGXE                                           758
      GO TO  5090                                                         758
5100  CONTINUE                                                            758
C *                                                                       758
C Geant POSITION : FGXE ===> MOTHER='CAVE' Z=11.0 <===                    759
      CALL AGSRESET                                                       759
      AG_MOTHER = 'CAVE'                                                  759
      AG_Z = 11.0                                                         759
      AG_TITLE = 'FGXE'                                                   759
      AG_EXNAME = 'POSITION'                                              759
      AG_PARLIST = 'MOTHER_Z'                                             759
      CALL AXPOSITION                                                     759
C *                                                                       759
      ENDIF                                                               759
* TPC resistor chain in IFC
      CALL AGSBEGM(MODULE,IPRIN)                                          763
C Check IPRIN<0                                                           763
      IF (IPRIN.LT.0) RETURN                                              763
C Create FGCT :                                                           763
C Call To Local Procedure FGCT                                            763
      ASSIGN 5120 TO BLOCK_FGCT                                           763
      GO TO  5110                                                         763
5120  CONTINUE                                                            763
C *                                                                       763
C R-DISTANCE                                                              764
      X1=FGTG_IFCRIN-FGTG_IFCTUBROUT                                      764
C TITL ANGLE IN RAD                                                       765
      X2=(FGTG_IFCRESANG-FGTG_IFCTUBDELA/2.0)/180.*3.1416                 765
      CALL AGSBEGM(MODULE,IPRIN)                                          766
C Check IPRIN<0                                                           766
      IF (IPRIN.LT.0) RETURN                                              766
C Create FGCT :                                                           766
C Call To Local Procedure FGCT                                            766
      ASSIGN 5130 TO BLOCK_FGCT                                           766
      GO TO  5110                                                         766
5130  CONTINUE                                                            766
C *                                                                       766
C Geant POSITION : FGCT ===> Z=0. X=X1*COS(X2) Y=X1*SIN(X2) <===          766
      CALL AGSRESET                                                       766
      AG_Z = 0.                                                           766
      AG_X = X1*COS(X2)                                                   766
      AG_Y = X1*SIN(X2)                                                   766
      AG_TITLE = 'FGCT'                                                   766
      AG_EXNAME = 'POSITION'                                              766
      AG_PARLIST = 'Z_X_Y'                                                766
      CALL AXPOSITION                                                     766
C *                                                                       766
C TITL ANGLE IN RAD                                                       766
      X2=(FGTG_IFCRESANG+FGTG_IFCTUBDELA/2.0)/180.*3.1416                 767
      CALL AGSBEGM(MODULE,IPRIN)                                          768
C Check IPRIN<0                                                           768
      IF (IPRIN.LT.0) RETURN                                              768
C Create FGCT :                                                           768
C Call To Local Procedure FGCT                                            768
      ASSIGN 5140 TO BLOCK_FGCT                                           768
      GO TO  5110                                                         768
5140  CONTINUE                                                            768
C *                                                                       768
C Geant POSITION : FGCT ===> Z=0. X=X1*COS(X2) Y=X1*SIN(X2) <===          768
      CALL AGSRESET                                                       768
      AG_Z = 0.                                                           768
      AG_X = X1*COS(X2)                                                   768
      AG_Y = X1*SIN(X2)                                                   768
      AG_TITLE = 'FGCT'                                                   768
      AG_EXNAME = 'POSITION'                                              768
      AG_PARLIST = 'Z_X_Y'                                                768
      CALL AXPOSITION                                                     768
C *                                                                       768
      CALL AGSBEGM(MODULE,IPRIN)                                          770
C Check IPRIN<0                                                           770
      IF (IPRIN.LT.0) RETURN                                              770
C Create FGRS :                                                           770
C Call To Local Procedure FGRS                                            770
      ASSIGN 5160 TO BLOCK_FGRS                                           770
      GO TO  5150                                                         770
5160  CONTINUE                                                            770
C *                                                                       770
      CALL AGSBEGM(MODULE,IPRIN)                                          771
C Check IPRIN<0                                                           771
      IF (IPRIN.LT.0) RETURN                                              771
C Create FGRS :                                                           771
C Call To Local Procedure FGRS                                            771
      ASSIGN 5170 TO BLOCK_FGRS                                           771
      GO TO  5150                                                         771
5170  CONTINUE                                                            771
C *                                                                       771
C Geant POSITION : FGRS ===> ALPHAZ=FGTG_IFCRESANG <===                   771
      CALL AGSRESET                                                       771
      AG_ALPHAN=AG_ALPHAN+1                                               771
      AG_ALPHAK(AG_ALPHAN)=3                                              771
      AG_ALPHAL(AG_ALPHAN)=FGTG_IFCRESANG                                 771
      AG_TITLE = 'FGRS'                                                   771
      AG_EXNAME = 'POSITION'                                              771
      AG_PARLIST = 'ALPHAZ'                                               771
      CALL AXPOSITION                                                     771
C *                                                                       771
* real FGT
      CALL AGSBEGM(MODULE,IPRIN)                                          775
C Check IPRIN<0                                                           775
      IF (IPRIN.LT.0) RETURN                                              775
C Create FGMO :                                                           775
C Call To Local Procedure FGMO                                            775
      ASSIGN 5190 TO BLOCK_FGMO                                           775
      GO TO  5180                                                         775
5190  CONTINUE                                                            775
C *                                                                       775
*VP      write (*,*) '###################### FGMO Z=',centerZ
C Geant POSITION : FGMO ===> MOTHER='CAVE' Z=CENTERZ <===                 777
      CALL AGSRESET                                                       777
      AG_MOTHER = 'CAVE'                                                  777
      AG_Z = CENTERZ                                                      777
      AG_TITLE = 'FGMO'                                                   777
      AG_EXNAME = 'POSITION'                                              777
      AG_PARLIST = 'MOTHER_Z'                                             777
      CALL AXPOSITION                                                     777
C *                                                                       777
* -------------------------------------------------------------
      CALL AGSENDM                                                        780
      RETURN                                                              780
C Begin Local Procedure FGMO                                              780
C FGMO IS THE MOTHER VOLUME FOR THE WHOLE FGT ASSEMBLY                    780
5180  CONTINUE                                                            780
         AG_TITLE='FGMO'                                                  780
         AG_MODULE=MODULE                                                 780
         AG_MARK='V00'                                                    780
         CALL AGSPUSH                                                     780
         AG_MODULE=MODULE                                                 780
C    *                                                                    780
C    Geant MATERIAL : AIR ===> inherited arguments used <===              781
         CALL AGSRESET                                                    781
         AG_TITLE = 'AIR'                                                 781
         AG_EXNAME = 'MATERIAL'                                           781
         AG_PARLIST = 'NONE'                                              781
         CALL AXMATERIAL                                                  781
C    *                                                                    781
C    Geant ATTRIBUTE : FGMO ===> SEEN=0 COLO=6 <===                       781
         CALL AGSRESET                                                    782
         AG_SEEN = 0                                                      782
         AG_COLO = 6                                                      782
         AG_TITLE = 'FGMO'                                                782
         AG_EXNAME = 'ATTRIBUTE'                                          782
         AG_PARLIST = 'SEEN_COLO'                                         782
         CALL AXATTRIBUTE                                                 782
C    *                                                                    782
C    Geant SHAPE : TUBE ===> RMIN=FGTG_RINLES RMAX=ROUT+FGTG_GR3 DZ=LENG  782
C    THZ/2.0 <===                                                         782
         CALL AGSRESET                                                    783
         AG_RMIN = FGTG_RINLES                                            783
         AG_RMAX = ROUT+FGTG_GR3                                          783
         AG_DZ = LENGTHZ/2.0                                              783
         AG_TITLE = 'TUBE'                                                783
         AG_EXNAME = 'SHAPE'                                              783
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      783
         CALL AXSHAPE                                                     783
C    *                                                                    783
* assembly enclosure
C    Create FGFP :                                                        786
C    Call To Local Procedure FGFP                                         786
         ASSIGN 5210 TO BLOCK_FGFP                                        786
         GO TO  5200                                                      786
5210     CONTINUE                                                         786
C    *                                                                    786
C    Geant POSITION : FGFP ===> Z=-LENGTHZ/2.0+FGTG_FPTHK/2.0 <===        786
         CALL AGSRESET                                                    786
         AG_Z = -LENGTHZ/2.0+FGTG_FPTHK/2.0                               786
         AG_TITLE = 'FGFP'                                                786
         AG_EXNAME = 'POSITION'                                           786
         AG_PARLIST = 'Z'                                                 786
         CALL AXPOSITION                                                  786
C    *                                                                    786
C    Create FGFA :                                                        787
C    Call To Local Procedure FGFA                                         787
         ASSIGN 5230 TO BLOCK_FGFA                                        787
         GO TO  5220                                                      787
5230     CONTINUE                                                         787
C    *                                                                    787
C    Geant POSITION : FGFA ===> Z=-LENGTHZ/2.0+FGTG_FRATHK/2.0 <===       787
         CALL AGSRESET                                                    787
         AG_Z = -LENGTHZ/2.0+FGTG_FRATHK/2.0                              787
         AG_TITLE = 'FGFA'                                                787
         AG_EXNAME = 'POSITION'                                           787
         AG_PARLIST = 'Z'                                                 787
         CALL AXPOSITION                                                  787
C    *                                                                    787
C    Create FGFL :                                                        788
C    Call To Local Procedure FGFL                                         788
         ASSIGN 5250 TO BLOCK_FGFL                                        788
         GO TO  5240                                                      788
5250     CONTINUE                                                         788
C    *                                                                    788
C    Geant POSITION : FGFL ===> Z=-LENGTHZ/2.0+0.5*FGTG_FRATHK+FGTG_FPTH  788
C    K/2.0 <===                                                           788
         CALL AGSRESET                                                    788
         AG_Z = -LENGTHZ/2.0+0.5*FGTG_FRATHK+FGTG_FPTHK/2.0               788
         AG_TITLE = 'FGFL'                                                788
         AG_EXNAME = 'POSITION'                                           788
         AG_PARLIST = 'Z'                                                 788
         CALL AXPOSITION                                                  788
C    *                                                                    788
C    Create FGOC :                                                        789
C    Call To Local Procedure FGOC                                         789
         ASSIGN 5270 TO BLOCK_FGOC                                        789
         GO TO  5260                                                      789
5270     CONTINUE                                                         789
C    *                                                                    789
C    Geant POSITION : FGOC ===> Z=+FGTG_FPTHK/2.0 <===                    789
         CALL AGSRESET                                                    789
         AG_Z = +FGTG_FPTHK/2.0                                           789
         AG_TITLE = 'FGOC'                                                789
         AG_EXNAME = 'POSITION'                                           789
         AG_PARLIST = 'Z'                                                 789
         CALL AXPOSITION                                                  789
C    *                                                                    789
C    Create FGOA :                                                        790
C    Call To Local Procedure FGOA                                         790
         ASSIGN 5290 TO BLOCK_FGOA                                        790
         GO TO  5280                                                      790
5290     CONTINUE                                                         790
C    *                                                                    790
C    Geant POSITION : FGOA ===> Z=+LENGTHZ/2.0-FGTG_FGOADELZ/2.0 <===     790
         CALL AGSRESET                                                    790
         AG_Z = +LENGTHZ/2.0-FGTG_FGOADELZ/2.0                            790
         AG_TITLE = 'FGOA'                                                790
         AG_EXNAME = 'POSITION'                                           790
         AG_PARLIST = 'Z'                                                 790
         CALL AXPOSITION                                                  790
C    *                                                                    790
* material inside enclosure
* rails
C    R-DISTANCE                                                           794
         X1=ROUT+FGTG_FGORRAD                                             794
C    TITL ANGLE IN RAD                                                    795
         X2=FGTG_FGORANG/180.*3.1416                                      795
C    Create FGOR :                                                        796
C    Call To Local Procedure FGOR                                         796
         ASSIGN 5310 TO BLOCK_FGOR                                        796
         GO TO  5300                                                      796
5310     CONTINUE                                                         796
C    *                                                                    796
C    Geant POSITION : FGOR ===> Z=FGTG_FPTHK/2.0 X=X1*COS(X2) Y= X1*SIN(  796
C    X2) <===                                                             796
         CALL AGSRESET                                                    796
         AG_Z = FGTG_FPTHK/2.0                                            796
         AG_X = X1*COS(X2)                                                796
         AG_Y = X1*SIN(X2)                                                796
         AG_TITLE = 'FGOR'                                                796
         AG_EXNAME = 'POSITION'                                           796
         AG_PARLIST = 'Z_X_Y'                                             796
         CALL AXPOSITION                                                  796
C    *                                                                    796
C    Create FGOR :                                                        797
C    Call To Local Procedure FGOR                                         797
         ASSIGN 5320 TO BLOCK_FGOR                                        797
         GO TO  5300                                                      797
5320     CONTINUE                                                         797
C    *                                                                    797
C    Geant POSITION : FGOR ===> Z=FGTG_FPTHK/2.0 X=-X1*COS(X2) Y=-X1*SIN  797
C    (X2) <===                                                            797
         CALL AGSRESET                                                    797
         AG_Z = FGTG_FPTHK/2.0                                            797
         AG_X = -X1*COS(X2)                                               797
         AG_Y = -X1*SIN(X2)                                               797
         AG_TITLE = 'FGOR'                                                797
         AG_EXNAME = 'POSITION'                                           797
         AG_PARLIST = 'Z_X_Y'                                             797
         CALL AXPOSITION                                                  797
C    *                                                                    797
* Loop over the regular triple-GEM disks.
C    *                                                                    801
         DISK=1                                                           801
            GO TO 5333                                                    801
5331        DISK=DISK+1                                                   801
5333        IF(DISK-(FGTG_NDISK).GT.0) GO TO 5332                         801
C       PRINT *, 'do disk=',DISK, FGTG_CONFIGJAN                          802
            IF (FGTG_CONFIGJAN.GT.1) THEN                                 803
C       Z OF THIS DISK                                                    804
            ZD=-LENGTHZ/2.0 + THICKGG/2.0 + FGTG_ZDISK(DISK)              804
C       Create FGGD :                                                     805
C       Call To Local Procedure FGGD                                      805
            ASSIGN 5350 TO BLOCK_FGGD                                     805
            GO TO  5340                                                   805
5350        CONTINUE                                                      805
C       *                                                                 805
C       Geant POSITION : FGGD ===> Z=ZD <===                              805
            CALL AGSRESET                                                 805
            AG_Z = ZD                                                     805
            AG_TITLE = 'FGGD'                                             805
            AG_EXNAME = 'POSITION'                                        805
            AG_PARLIST = 'Z'                                              805
            CALL AXPOSITION                                               805
C       *                                                                 805
C       ADD APV CHIPS MATERIAL, WARN: USES 'zsum' FROM FGGD               807
C       RXY OF THE APV-CHIP CENTER                                        808
            X3=RIN+FGTG_APVX/2.                                           808
C       *                                                                 809
            QUAD=1                                                        809
               GO TO 5363                                                 809
5361           QUAD=QUAD+1                                                809
5363           IF(QUAD-(FGTG_NQUAD).GT.0) GO TO 5362                      809
C          TITL ANGLE                                                     810
               X2=FGTG_APVANG+QUAD*360./FGTG_NQUAD                        810
C          Create FGAP :                                                  813
C          Call To Local Procedure FGAP                                   813
               ASSIGN 5380 TO BLOCK_FGAP                                  813
               GO TO  5370                                                813
5380           CONTINUE                                                   813
C          *                                                              813
C          Geant POSITION : FGAP ===> Z=ZD+ZSUM+FGTG_APVZ/2.0+FGTG_APVDD  813
C          Z/2 ALPHAZ=X2 X=X3*COS(X2/180.*3.1416) Y=X3*SIN(X2/180.*3.141  813
C          6) <===                                                        813
               CALL AGSRESET                                              813
               AG_Z = ZD+ZSUM+FGTG_APVZ/2.0+FGTG_APVDDZ/2                 813
               AG_ALPHAN=AG_ALPHAN+1                                      813
               AG_ALPHAK(AG_ALPHAN)=3                                     813
               AG_ALPHAL(AG_ALPHAN)=X2                                    813
               AG_X = X3*COS(X2/180.*3.1416)                              813
               AG_Y = X3*SIN(X2/180.*3.1416)                              813
               AG_TITLE = 'FGAP'                                          813
               AG_EXNAME = 'POSITION'                                     813
               AG_PARLIST = 'Z_ALPHAZ_X_Y'                                813
               CALL AXPOSITION                                            813
C          *                                                              813
            GO TO 5361                                                    814
5362        CONTINUE                                                      814
            ENDIF                                                         815
* FGT utility lines start at beginning of every disk:
C       *                                                                 818
            CABLE=1                                                       818
               GO TO 5393                                                 818
5391           CABLE=CABLE+1                                              818
5393           IF(CABLE-(FGTG_NCABLES).GT.0) GO TO 5392                   818
C          CENTER ANGLE                                                   819
               X2=90+FGTG_FGOUANG + CABLE*90                              819
C          FINAL PHI ORIENTATION, SPACE THEM APART                        820
               X3=X2+DISK*FGTG_FGOUDELA*3.0                               820
C          CABLE LENGTH                                                   821
               Z1=FGTG_OCLEN/2.0-ZD                                       821
C          PRINT *, 'disk=',DISK, 'z1=',Z1                                822
C          ' inner radii                                                  823
C    *              '                                                     823
               X1=FGTG_GEMRO+FGTG_GR2+FGTG_OCTHK                          823
C          Create FGOU :                                                  827
C          Call To Local Procedure FGOU                                   827
               ASSIGN 5410 TO BLOCK_FGOU                                  827
               GO TO  5400                                                827
5410           CONTINUE                                                   827
C          *                                                              827
C          Geant POSITION : FGOU ===> RMIN=X1 RMAX=X1+FGTG_FGOUDELR DZ=Z  827
C          1/2.0 Z=(FGTG_OCLEN-Z1)/2.0 ALPHAZ=X3 PHI1=0 PHI2=FGTG_FGOUDE  827
C          LA <===                                                        827
               CALL AGSRESET                                              827
               AG_RMIN = X1                                               827
               AG_RMAX = X1+FGTG_FGOUDELR                                 827
               AG_DZ = Z1/2.0                                             827
               AG_Z = (FGTG_OCLEN-Z1)/2.0                                 827
               AG_ALPHAN=AG_ALPHAN+1                                      827
               AG_ALPHAK(AG_ALPHAN)=3                                     827
               AG_ALPHAL(AG_ALPHAN)=X3                                    827
               AG_PHI1 = 0                                                827
               AG_PHI2 = FGTG_FGOUDELA                                    827
               AG_TITLE = 'FGOU'                                          827
               AG_EXNAME = 'POSITION'                                     827
               AG_PARLIST = 'RMIN_RMAX_DZ_Z_ALPHAZ_PHI1_PHI2'             827
               CALL AXPOSITION                                            827
C          *                                                              827
            GO TO 5391                                                    828
5392        CONTINUE                                                      828
C       END OF CABLES                                                     828
         GO TO 5331                                                       829
5332     CONTINUE                                                         829
C    END OF REGULAR DISKS                                                 829
* add one extra FGT sensitive volume in front, requested by Les
         IF (FGTG_CONFIGJAN.GT.2) THEN                                    832
         FGSCRMIN=FGTG_RINLES                                             832
         FGSCRMAX=FGTG_GEMRO                                              832
         FGSCDZ=FGTG_GGSCTHK/2.0                                          832
*JAN        FGSCz = lengthZ/2.0-3.0
C    JAN CHANGE                                                           835
         FGSCZ = -LENGTHZ/2.0+4.0                                         835
C    *VP        write(*,*)'########## FGSC rMin,rMax,Z=',FGSCrmin,FGSCrm  836
C    ax,FGSCz                                                             836
C    Create FGSD :                                                        837
C    Call To Local Procedure FGSD                                         837
         ASSIGN 5430 TO BLOCK_FGSD                                        837
         GO TO  5420                                                      837
5430     CONTINUE                                                         837
C    *                                                                    837
C    Geant POSITION : FGSD ===> Z=FGSCZ ALPHAZ=-15 <===                   837
         CALL AGSRESET                                                    837
         AG_Z = FGSCZ                                                     837
         AG_ALPHAN=AG_ALPHAN+1                                            837
         AG_ALPHAK(AG_ALPHAN)=3                                           837
         AG_ALPHAL(AG_ALPHAN)=-15                                         837
         AG_TITLE = 'FGSD'                                                837
         AG_EXNAME = 'POSITION'                                           837
         AG_PARLIST = 'Z_ALPHAZ'                                          837
         CALL AXPOSITION                                                  837
C    *                                                                    837
         ENDIF                                                            837
* add SSD utility lines, continue engle
C    *                                                                    841
         CABLE=1                                                          841
            GO TO 5443                                                    841
5441        CABLE=CABLE+1                                                 841
5443        IF(CABLE-(FGTG_NCABLES).GT.0) GO TO 5442                      841
C       ' outer radii                                                     842
C    *      '                                                             842
            X1=FGTG_GEMRO+FGTG_GR2+FGTG_OCTHK+0.1                         842
C       Create FGSU :                                                     845
C       Call To Local Procedure FGSU                                      845
            ASSIGN 5460 TO BLOCK_FGSU                                     845
            GO TO  5450                                                   845
5460        CONTINUE                                                      845
C       *                                                                 845
C       Geant POSITION : FGSU ===> RMIN=X1 RMAX=X1+1.5 DZ=FGTG_OCLEN/2.0  845
C        Z=0 ALPHAZ=80+FGTG_FGOUANG+CABLE*90 PHI1=30 PHI2=45 <===         845
            CALL AGSRESET                                                 845
            AG_RMIN = X1                                                  845
            AG_RMAX = X1+1.5                                              845
            AG_DZ = FGTG_OCLEN/2.0                                        845
            AG_Z = 0                                                      845
            AG_ALPHAN=AG_ALPHAN+1                                         845
            AG_ALPHAK(AG_ALPHAN)=3                                        845
            AG_ALPHAL(AG_ALPHAN)=80+FGTG_FGOUANG+CABLE*90                 845
            AG_PHI1 = 30                                                  845
            AG_PHI2 = 45                                                  845
            AG_TITLE = 'FGSU'                                             845
            AG_EXNAME = 'POSITION'                                        845
            AG_PARLIST = 'RMIN_RMAX_DZ_Z_ALPHAZ_PHI1_PHI2'                845
            CALL AXPOSITION                                               845
C       *                                                                 845
         GO TO 5441                                                       846
5442     CONTINUE                                                         846
C    END OF CABLES                                                        846
5181  CALL AGSPOPB                                                        847
      GO TO BLOCK_FGMO, (5180 ,5190)                                      847
C End of Local Procedure                                                  847
* ---------------------------------------------------------------------
C Begin Local Procedure FGGD                                              850
C FGGD IS THE MOTHER VOLUME OF THE INDIVIDUAL TRIPPLE-GEM DISKS           850
5340  CONTINUE                                                            850
         AG_TITLE='FGGD'                                                  850
         AG_MODULE=MODULE                                                 850
         AG_MARK='V00'                                                    850
         CALL AGSPUSH                                                     850
         AG_MODULE=MODULE                                                 850
C    *                                                                    850
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              851
         CALL AGSRESET                                                    851
         AG_TITLE = 'FR4'                                                 851
         AG_EXNAME = 'MATERIAL'                                           851
         AG_PARLIST = 'NONE'                                              851
         CALL AXMATERIAL                                                  851
C    *                                                                    851
C    Geant ATTRIBUTE : FGGD ===> SEEN=1 COLO=7 <===                       851
         CALL AGSRESET                                                    852
         AG_SEEN = 1                                                      852
         AG_COLO = 7                                                      852
         AG_TITLE = 'FGGD'                                                852
         AG_EXNAME = 'ATTRIBUTE'                                          852
         AG_PARLIST = 'SEEN_COLO'                                         852
         CALL AXATTRIBUTE                                                 852
C    *                                                                    852
C    Geant SHAPE : TUBE ===> RMIN=FGTG_GEMRI RMAX=FGTG_GEMRO DZ=THICKGG/  852
C    2.0 <===                                                             852
         CALL AGSRESET                                                    853
         AG_RMIN = FGTG_GEMRI                                             853
         AG_RMAX = FGTG_GEMRO                                             853
         AG_DZ = THICKGG/2.0                                              853
         AG_TITLE = 'TUBE'                                                853
         AG_EXNAME = 'SHAPE'                                              853
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      853
         CALL AXSHAPE                                                     853
C    *                                                                    853
* assemble layres of one tripple-GEM disk
C    START AT THE FRONT FACE.                                             856
         ZSUM = -THICKGG/2.0                                              856
         RIN=FGTG_GEMRI                                                   857
         ROUT=FGTG_GEMRO                                                  858
C    Create FGGF :                                                        860
C    Call To Local Procedure FGGF                                         860
         ASSIGN 5480 TO BLOCK_FGGF                                        860
         GO TO  5470                                                      860
5480     CONTINUE                                                         860
C    *                                                                    860
C    Geant POSITION : FGGF ===> Z=ZSUM+ FGTG_GGFR4THK/2.0 <===            860
         CALL AGSRESET                                                    860
         AG_Z = ZSUM+ FGTG_GGFR4THK/2.0                                   860
         AG_TITLE = 'FGGF'                                                860
         AG_EXNAME = 'POSITION'                                           860
         AG_PARLIST = 'Z'                                                 860
         CALL AXPOSITION                                                  860
C    *                                                                    860
         ZSUM=ZSUM+FGTG_GGFR4THK                                          860
C    Create FGGN :                                                        863
C    Call To Local Procedure FGGN                                         863
         ASSIGN 5500 TO BLOCK_FGGN                                        863
         GO TO  5490                                                      863
5500     CONTINUE                                                         863
C    *                                                                    863
C    Geant POSITION : FGGN ===> Z=ZSUM+ FGTG_GGNTHK/2.0 <===              863
         CALL AGSRESET                                                    863
         AG_Z = ZSUM+ FGTG_GGNTHK/2.0                                     863
         AG_TITLE = 'FGGN'                                                863
         AG_EXNAME = 'POSITION'                                           863
         AG_PARLIST = 'Z'                                                 863
         CALL AXPOSITION                                                  863
C    *                                                                    863
         ZSUM=ZSUM+FGTG_GGNTHK                                            863
C    Create FGGF :                                                        866
C    Call To Local Procedure FGGF                                         866
         ASSIGN 5510 TO BLOCK_FGGF                                        866
         GO TO  5470                                                      866
5510     CONTINUE                                                         866
C    *                                                                    866
C    Geant POSITION : FGGF ===> Z=ZSUM+ FGTG_GGFR4THK/2.0 <===            866
         CALL AGSRESET                                                    866
         AG_Z = ZSUM+ FGTG_GGFR4THK/2.0                                   866
         AG_TITLE = 'FGGF'                                                866
         AG_EXNAME = 'POSITION'                                           866
         AG_PARLIST = 'Z'                                                 866
         CALL AXPOSITION                                                  866
C    *                                                                    866
         ZSUM=ZSUM+FGTG_GGFR4THK                                          866
C    CHANGE RIN/ROUT FOR THE VOLUMES BELOW                                869
         RIN=FGTG_GEMRI+FGTG_SR                                           869
         ROUT=FGTG_GEMRO-FGTG_SR                                          870
         FGSCRMIN=RIN                                                     870
         FGSCRMAX=ROUT                                                    870
         FGSCDZ=FGTG_GGSCTHK/2.0                                          870
         FGSCZ = ZSUM+FGTG_GGSCTHK/2.0                                    872
C    Create FGSC :                                                        873
C    Call To Local Procedure FGSC                                         873
         ASSIGN 5530 TO BLOCK_FGSC                                        873
         GO TO  5520                                                      873
5530     CONTINUE                                                         873
C    *                                                                    873
C    Geant POSITION : FGSC ===> Z=FGSCZ ALPHAZ=-15 <===                   873
         CALL AGSRESET                                                    873
         AG_Z = FGSCZ                                                     873
         AG_ALPHAN=AG_ALPHAN+1                                            873
         AG_ALPHAK(AG_ALPHAN)=3                                           873
         AG_ALPHAL(AG_ALPHAN)=-15                                         873
         AG_TITLE = 'FGSC'                                                873
         AG_EXNAME = 'POSITION'                                           873
         AG_PARLIST = 'Z_ALPHAZ'                                          873
         CALL AXPOSITION                                                  873
C    *                                                                    873
         ZSUM=ZSUM+FGTG_GGSCTHK                                           873
C    Create FGGC :                                                        876
C    Call To Local Procedure FGGC                                         876
         ASSIGN 5550 TO BLOCK_FGGC                                        876
         GO TO  5540                                                      876
5550     CONTINUE                                                         876
C    *                                                                    876
C    Geant POSITION : FGGC ===> Z=ZSUM+ FGTG_GGCUTHK/2.0 <===             876
         CALL AGSRESET                                                    876
         AG_Z = ZSUM+ FGTG_GGCUTHK/2.0                                    876
         AG_TITLE = 'FGGC'                                                876
         AG_EXNAME = 'POSITION'                                           876
         AG_PARLIST = 'Z'                                                 876
         CALL AXPOSITION                                                  876
C    *                                                                    876
         ZSUM=ZSUM+FGTG_GGCUTHK                                           876
C    Create FGGK :                                                        879
C    Call To Local Procedure FGGK                                         879
         ASSIGN 5570 TO BLOCK_FGGK                                        879
         GO TO  5560                                                      879
5570     CONTINUE                                                         879
C    *                                                                    879
C    Geant POSITION : FGGK ===> Z=ZSUM+ FGTG_GGKATHK/2.0 <===             879
         CALL AGSRESET                                                    879
         AG_Z = ZSUM+ FGTG_GGKATHK/2.0                                    879
         AG_TITLE = 'FGGK'                                                879
         AG_EXNAME = 'POSITION'                                           879
         AG_PARLIST = 'Z'                                                 879
         CALL AXPOSITION                                                  879
C    *                                                                    879
         ZSUM=ZSUM+FGTG_GGKATHK                                           879
C    Create FGGP :                                                        882
C    Call To Local Procedure FGGP                                         882
         ASSIGN 5590 TO BLOCK_FGGP                                        882
         GO TO  5580                                                      882
5590     CONTINUE                                                         882
C    *                                                                    882
C    Geant POSITION : FGGP ===> Z=ZSUM+ FGTG_GGPATHK/2.0 <===             882
         CALL AGSRESET                                                    882
         AG_Z = ZSUM+ FGTG_GGPATHK/2.0                                    882
         AG_TITLE = 'FGGP'                                                882
         AG_EXNAME = 'POSITION'                                           882
         AG_PARLIST = 'Z'                                                 882
         CALL AXPOSITION                                                  882
C    *                                                                    882
         ZSUM=ZSUM+FGTG_GGPATHK                                           882
C    Create FGGR :                                                        885
C    Call To Local Procedure FGGR                                         885
         ASSIGN 5610 TO BLOCK_FGGR                                        885
         GO TO  5600                                                      885
5610     CONTINUE                                                         885
C    *                                                                    885
C    Geant POSITION : FGGR ===> Z=ZSUM+ FGTG_GGRLTHK/2.0 <===             885
         CALL AGSRESET                                                    885
         AG_Z = ZSUM+ FGTG_GGRLTHK/2.0                                    885
         AG_TITLE = 'FGGR'                                                885
         AG_EXNAME = 'POSITION'                                           885
         AG_PARLIST = 'Z'                                                 885
         CALL AXPOSITION                                                  885
C    *                                                                    885
         ZSUM=ZSUM+FGTG_GGRLTHK                                           885
5341  CALL AGSPOPB                                                        888
      GO TO BLOCK_FGGD, (5340 ,5350)                                      888
C End of Local Procedure                                                  888
* ------------------------------------------------------------------------
C Begin Local Procedure FGGF                                              892
C FGGF DESCRIBES THE FR4 LAYER OF 3GEM                                    892
5470  CONTINUE                                                            892
         AG_TITLE='FGGF'                                                  892
         AG_MODULE=MODULE                                                 892
         AG_MARK='V00'                                                    892
         CALL AGSPUSH                                                     892
         AG_MODULE=MODULE                                                 892
C    *                                                                    892
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              893
         CALL AGSRESET                                                    893
         AG_TITLE = 'FR4'                                                 893
         AG_EXNAME = 'MATERIAL'                                           893
         AG_PARLIST = 'NONE'                                              893
         CALL AXMATERIAL                                                  893
C    *                                                                    893
C    Geant ATTRIBUTE : FGGF ===> SEEN=1 COLO=4 <===                       893
         CALL AGSRESET                                                    894
         AG_SEEN = 1                                                      894
         AG_COLO = 4                                                      894
         AG_TITLE = 'FGGF'                                                894
         AG_EXNAME = 'ATTRIBUTE'                                          894
         AG_PARLIST = 'SEEN_COLO'                                         894
         CALL AXATTRIBUTE                                                 894
C    *                                                                    894
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROUT DZ=FGTG_GGFR4THK/2.0 <==  894
C    =                                                                    894
         CALL AGSRESET                                                    895
         AG_RMIN = RIN                                                    895
         AG_RMAX = ROUT                                                   895
         AG_DZ = FGTG_GGFR4THK/2.0                                        895
         AG_TITLE = 'TUBE'                                                895
         AG_EXNAME = 'SHAPE'                                              895
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      895
         CALL AXSHAPE                                                     895
C    *                                                                    895
5471  CALL AGSPOPB                                                        895
      GO TO BLOCK_FGGF, (5470 ,5480,5510)                                 896
C End of Local Procedure                                                  896
* ----------------------------------------------------------------------
C Begin Local Procedure FGFP                                              900
C FGFP FRONT PLATE, HEONECOMB CARBON                                      900
5200  CONTINUE                                                            900
         AG_TITLE='FGFP'                                                  900
         AG_MODULE=MODULE                                                 900
         AG_MARK='V00'                                                    900
         CALL AGSPUSH                                                     900
         AG_MODULE=MODULE                                                 900
C    *                                                                    900
C    Geant MATERIAL : NOMEX ===> inherited arguments used <===            901
         CALL AGSRESET                                                    901
         AG_TITLE = 'NOMEX'                                               901
         AG_EXNAME = 'MATERIAL'                                           901
         AG_PARLIST = 'NONE'                                              901
         CALL AXMATERIAL                                                  901
C    *                                                                    901
C    Geant ATTRIBUTE : FGFP ===> SEEN=1 COLO=1 <===                       901
         CALL AGSRESET                                                    902
         AG_SEEN = 1                                                      902
         AG_COLO = 1                                                      902
         AG_TITLE = 'FGFP'                                                902
         AG_EXNAME = 'ATTRIBUTE'                                          902
         AG_PARLIST = 'SEEN_COLO'                                         902
         CALL AXATTRIBUTE                                                 902
C    *                                                                    902
C    Geant SHAPE : TUBE ===> RMIN=FGTG_FRAROUT RMAX=ROUT DZ=FGTG_FPTHK/2  902
C    .0 <===                                                              902
         CALL AGSRESET                                                    903
         AG_RMIN = FGTG_FRAROUT                                           903
         AG_RMAX = ROUT                                                   903
         AG_DZ = FGTG_FPTHK/2.0                                           903
         AG_TITLE = 'TUBE'                                                903
         AG_EXNAME = 'SHAPE'                                              903
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      903
         CALL AXSHAPE                                                     903
C    *                                                                    903
C    Create FGFC :                                                        904
C    Call To Local Procedure FGFC                                         904
         ASSIGN 5630 TO BLOCK_FGFC                                        904
         GO TO  5620                                                      904
5630     CONTINUE                                                         904
C    *                                                                    904
C    Geant POSITION : FGFC ===> Z=-FGTG_FPTHK/2.0 +FGTG_FPCTHK/2.0 <===   904
         CALL AGSRESET                                                    904
         AG_Z = -FGTG_FPTHK/2.0 +FGTG_FPCTHK/2.0                          904
         AG_TITLE = 'FGFC'                                                904
         AG_EXNAME = 'POSITION'                                           904
         AG_PARLIST = 'Z'                                                 904
         CALL AXPOSITION                                                  904
C    *                                                                    904
C    Create FGFC :                                                        905
C    Call To Local Procedure FGFC                                         905
         ASSIGN 5640 TO BLOCK_FGFC                                        905
         GO TO  5620                                                      905
5640     CONTINUE                                                         905
C    *                                                                    905
C    Geant POSITION : FGFC ===> Z=+FGTG_FPTHK/2.0 -FGTG_FPCTHK/2.0 <===   905
         CALL AGSRESET                                                    905
         AG_Z = +FGTG_FPTHK/2.0 -FGTG_FPCTHK/2.0                          905
         AG_TITLE = 'FGFC'                                                905
         AG_EXNAME = 'POSITION'                                           905
         AG_PARLIST = 'Z'                                                 905
         CALL AXPOSITION                                                  905
C    *                                                                    905
5201  CALL AGSPOPB                                                        905
      GO TO BLOCK_FGFP, (5200 ,5210)                                      906
C End of Local Procedure                                                  906
* ----------------------------------------------------------------------
C Begin Local Procedure FGGN                                              909
C FGGN TRIPPLE-GEM SUPPORT PLATE, HEONECOMB NOMEX                         909
5490  CONTINUE                                                            909
         AG_TITLE='FGGN'                                                  909
         AG_MODULE=MODULE                                                 909
         AG_MARK='V00'                                                    909
         CALL AGSPUSH                                                     909
         AG_MODULE=MODULE                                                 909
C    *                                                                    909
C    Geant MATERIAL : NOMEX ===> inherited arguments used <===            910
         CALL AGSRESET                                                    910
         AG_TITLE = 'NOMEX'                                               910
         AG_EXNAME = 'MATERIAL'                                           910
         AG_PARLIST = 'NONE'                                              910
         CALL AXMATERIAL                                                  910
C    *                                                                    910
C    Geant ATTRIBUTE : FGGN ===> SEEN=1 COLO=1 <===                       910
         CALL AGSRESET                                                    911
         AG_SEEN = 1                                                      911
         AG_COLO = 1                                                      911
         AG_TITLE = 'FGGN'                                                911
         AG_EXNAME = 'ATTRIBUTE'                                          911
         AG_PARLIST = 'SEEN_COLO'                                         911
         CALL AXATTRIBUTE                                                 911
C    *                                                                    911
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROUT DZ=FGTG_GGNTHK/2.0 <===   911
         CALL AGSRESET                                                    912
         AG_RMIN = RIN                                                    912
         AG_RMAX = ROUT                                                   912
         AG_DZ = FGTG_GGNTHK/2.0                                          912
         AG_TITLE = 'TUBE'                                                912
         AG_EXNAME = 'SHAPE'                                              912
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      912
         CALL AXSHAPE                                                     912
C    *                                                                    912
5491  CALL AGSPOPB                                                        912
      GO TO BLOCK_FGGN, (5490 ,5500)                                      913
C End of Local Procedure                                                  913
* ----------------------------------------------------------------------
C Begin Local Procedure FGGC                                              916
C FGGC COMBINED CU-LAYER IN TRIPPLE-GEM                                   916
5540  CONTINUE                                                            916
         AG_TITLE='FGGC'                                                  916
         AG_MODULE=MODULE                                                 916
         AG_MARK='V00'                                                    916
         CALL AGSPUSH                                                     916
         AG_MODULE=MODULE                                                 916
C    *                                                                    916
C    Geant MATERIAL : COPPER_DENS1 ===> inherited arguments used <===     917
         CALL AGSRESET                                                    917
         AG_TITLE = 'COPPER_DENS1'                                        917
         AG_EXNAME = 'MATERIAL'                                           917
         AG_PARLIST = 'NONE'                                              917
         CALL AXMATERIAL                                                  917
C    *                                                                    917
C    Geant ATTRIBUTE : FGGC ===> SEEN=1 COLO=3 <===                       917
         CALL AGSRESET                                                    918
         AG_SEEN = 1                                                      918
         AG_COLO = 3                                                      918
         AG_TITLE = 'FGGC'                                                918
         AG_EXNAME = 'ATTRIBUTE'                                          918
         AG_PARLIST = 'SEEN_COLO'                                         918
         CALL AXATTRIBUTE                                                 918
C    *                                                                    918
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROUT DZ=FGTG_GGCUTHK/2.0 <===  918
         CALL AGSRESET                                                    919
         AG_RMIN = RIN                                                    919
         AG_RMAX = ROUT                                                   919
         AG_DZ = FGTG_GGCUTHK/2.0                                         919
         AG_TITLE = 'TUBE'                                                919
         AG_EXNAME = 'SHAPE'                                              919
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      919
         CALL AXSHAPE                                                     919
C    *                                                                    919
5541  CALL AGSPOPB                                                        919
      GO TO BLOCK_FGGC, (5540 ,5550)                                      920
C End of Local Procedure                                                  920
* ----------------------------------------------------------------------
C Begin Local Procedure FGGK                                              923
C FGGK COMBINED KAPTON LAYER IN TRIPPLE-GEM                               923
5560  CONTINUE                                                            923
         AG_TITLE='FGGK'                                                  923
         AG_MODULE=MODULE                                                 923
         AG_MARK='V00'                                                    923
         CALL AGSPUSH                                                     923
         AG_MODULE=MODULE                                                 923
C    *                                                                    923
C    Geant MATERIAL : KAPTON_DENS1 ===> inherited arguments used <===     924
         CALL AGSRESET                                                    924
         AG_TITLE = 'KAPTON_DENS1'                                        924
         AG_EXNAME = 'MATERIAL'                                           924
         AG_PARLIST = 'NONE'                                              924
         CALL AXMATERIAL                                                  924
C    *                                                                    924
C    Geant ATTRIBUTE : FGGK ===> SEEN=1 COLO=4 <===                       924
         CALL AGSRESET                                                    925
         AG_SEEN = 1                                                      925
         AG_COLO = 4                                                      925
         AG_TITLE = 'FGGK'                                                925
         AG_EXNAME = 'ATTRIBUTE'                                          925
         AG_PARLIST = 'SEEN_COLO'                                         925
         CALL AXATTRIBUTE                                                 925
C    *                                                                    925
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROUT DZ=FGTG_GGKATHK/2.0 <===  925
         CALL AGSRESET                                                    926
         AG_RMIN = RIN                                                    926
         AG_RMAX = ROUT                                                   926
         AG_DZ = FGTG_GGKATHK/2.0                                         926
         AG_TITLE = 'TUBE'                                                926
         AG_EXNAME = 'SHAPE'                                              926
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      926
         CALL AXSHAPE                                                     926
C    *                                                                    926
5561  CALL AGSPOPB                                                        926
      GO TO BLOCK_FGGK, (5560 ,5570)                                      927
C End of Local Procedure                                                  927
* ----------------------------------------------------------------------
C Begin Local Procedure FGGP                                              930
C FGGP COMBINED PASSIVE GAS LAYER IN TRIPPLE-GEM                          930
5580  CONTINUE                                                            930
         AG_TITLE='FGGP'                                                  930
         AG_MODULE=MODULE                                                 930
         AG_MARK='V00'                                                    930
         CALL AGSPUSH                                                     930
         AG_MODULE=MODULE                                                 930
C    *                                                                    930
C    Geant MATERIAL : ARCO2_70_30 ===> inherited arguments used <===      931
         CALL AGSRESET                                                    931
         AG_TITLE = 'ARCO2_70_30'                                         931
         AG_EXNAME = 'MATERIAL'                                           931
         AG_PARLIST = 'NONE'                                              931
         CALL AXMATERIAL                                                  931
C    *                                                                    931
C    Geant ATTRIBUTE : FGGP ===> SEEN=1 COLO=5 <===                       931
         CALL AGSRESET                                                    932
         AG_SEEN = 1                                                      932
         AG_COLO = 5                                                      932
         AG_TITLE = 'FGGP'                                                932
         AG_EXNAME = 'ATTRIBUTE'                                          932
         AG_PARLIST = 'SEEN_COLO'                                         932
         CALL AXATTRIBUTE                                                 932
C    *                                                                    932
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROUT DZ=FGTG_GGPATHK/2.0 <===  932
         CALL AGSRESET                                                    933
         AG_RMIN = RIN                                                    933
         AG_RMAX = ROUT                                                   933
         AG_DZ = FGTG_GGPATHK/2.0                                         933
         AG_TITLE = 'TUBE'                                                933
         AG_EXNAME = 'SHAPE'                                              933
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      933
         CALL AXSHAPE                                                     933
C    *                                                                    933
5581  CALL AGSPOPB                                                        933
      GO TO BLOCK_FGGP, (5580 ,5590)                                      934
C End of Local Procedure                                                  934
* ----------------------------------------------------------------------
C Begin Local Procedure FGGR                                              937
C FGGR FR4 READOUT LAYER IN TRIPPLE-GEM                                   937
5600  CONTINUE                                                            937
         AG_TITLE='FGGR'                                                  937
         AG_MODULE=MODULE                                                 937
         AG_MARK='V00'                                                    937
         CALL AGSPUSH                                                     937
         AG_MODULE=MODULE                                                 937
C    *                                                                    937
C    Geant MATERIAL : FR4_DENS1 ===> inherited arguments used <===        938
         CALL AGSRESET                                                    938
         AG_TITLE = 'FR4_DENS1'                                           938
         AG_EXNAME = 'MATERIAL'                                           938
         AG_PARLIST = 'NONE'                                              938
         CALL AXMATERIAL                                                  938
C    *                                                                    938
C    Geant ATTRIBUTE : FGGR ===> SEEN=1 COLO=4 <===                       938
         CALL AGSRESET                                                    939
         AG_SEEN = 1                                                      939
         AG_COLO = 4                                                      939
         AG_TITLE = 'FGGR'                                                939
         AG_EXNAME = 'ATTRIBUTE'                                          939
         AG_PARLIST = 'SEEN_COLO'                                         939
         CALL AXATTRIBUTE                                                 939
C    *                                                                    939
C    Geant SHAPE : TUBE ===> RMIN=RIN RMAX=ROUT DZ=FGTG_GGRLTHK/2.0 <===  939
         CALL AGSRESET                                                    940
         AG_RMIN = RIN                                                    940
         AG_RMAX = ROUT                                                   940
         AG_DZ = FGTG_GGRLTHK/2.0                                         940
         AG_TITLE = 'TUBE'                                                940
         AG_EXNAME = 'SHAPE'                                              940
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      940
         CALL AXSHAPE                                                     940
C    *                                                                    940
5601  CALL AGSPOPB                                                        940
      GO TO BLOCK_FGGR, (5600 ,5610)                                      941
C End of Local Procedure                                                  941
* ---------------------------------------------------------------
C Begin Local Procedure FGFC                                              944
C FGFC FRONT PLATE CARBON FIBER                                           944
5620  CONTINUE                                                            944
         AG_TITLE='FGFC'                                                  944
         AG_MODULE=MODULE                                                 944
         AG_MARK='V00'                                                    944
         CALL AGSPUSH                                                     944
         AG_MODULE=MODULE                                                 944
C    *                                                                    944
C    Geant MATERIAL : CFIBER ===> inherited arguments used <===           945
         CALL AGSRESET                                                    945
         AG_TITLE = 'CFIBER'                                              945
         AG_EXNAME = 'MATERIAL'                                           945
         AG_PARLIST = 'NONE'                                              945
         CALL AXMATERIAL                                                  945
C    *                                                                    945
C    Geant ATTRIBUTE : FGFC ===> SEEN=1 COLO=3 <===                       945
         CALL AGSRESET                                                    946
         AG_SEEN = 1                                                      946
         AG_COLO = 3                                                      946
         AG_TITLE = 'FGFC'                                                946
         AG_EXNAME = 'ATTRIBUTE'                                          946
         AG_PARLIST = 'SEEN_COLO'                                         946
         CALL AXATTRIBUTE                                                 946
C    *                                                                    946
C    Geant SHAPE : TUBE ===> RMIN=FGTG_FRAROUT RMAX=ROUT DZ=FGTG_FPCTHK/  946
C    2.0 <===                                                             946
         CALL AGSRESET                                                    947
         AG_RMIN = FGTG_FRAROUT                                           947
         AG_RMAX = ROUT                                                   947
         AG_DZ = FGTG_FPCTHK/2.0                                          947
         AG_TITLE = 'TUBE'                                                947
         AG_EXNAME = 'SHAPE'                                              947
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      947
         CALL AXSHAPE                                                     947
C    *                                                                    947
5621  CALL AGSPOPB                                                        947
      GO TO BLOCK_FGFC, (5620 ,5630,5640)                                 948
C End of Local Procedure                                                  948
* ---------------------------------------------------------------
C Begin Local Procedure FGCT                                              951
C FGCT G10 TUBE FOR IFC RESITOR CHAIN                                     951
5110  CONTINUE                                                            951
         AG_TITLE='FGCT'                                                  951
         AG_MODULE=MODULE                                                 951
         AG_MARK='V00'                                                    951
         CALL AGSPUSH                                                     951
         AG_MODULE=MODULE                                                 951
C    *                                                                    951
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              952
         CALL AGSRESET                                                    952
         AG_TITLE = 'FR4'                                                 952
         AG_EXNAME = 'MATERIAL'                                           952
         AG_PARLIST = 'NONE'                                              952
         CALL AXMATERIAL                                                  952
C    *                                                                    952
C    Geant ATTRIBUTE : FGCT ===> SEEN=1 COLO=3 <===                       952
         CALL AGSRESET                                                    953
         AG_SEEN = 1                                                      953
         AG_COLO = 3                                                      953
         AG_TITLE = 'FGCT'                                                953
         AG_EXNAME = 'ATTRIBUTE'                                          953
         AG_PARLIST = 'SEEN_COLO'                                         953
         CALL AXATTRIBUTE                                                 953
C    *                                                                    953
C    Geant SHAPE : TUBE ===> RMIN=FGTG_IFCTUBRIN RMAX=FGTG_IFCTUBROUT DZ  953
C    =FGTG_IFCLENZ/2.0 <===                                               953
         CALL AGSRESET                                                    954
         AG_RMIN = FGTG_IFCTUBRIN                                         954
         AG_RMAX = FGTG_IFCTUBROUT                                        954
         AG_DZ = FGTG_IFCLENZ/2.0                                         954
         AG_TITLE = 'TUBE'                                                954
         AG_EXNAME = 'SHAPE'                                              954
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      954
         CALL AXSHAPE                                                     954
C    *                                                                    954
5111  CALL AGSPOPB                                                        954
      GO TO BLOCK_FGCT, (5110 ,5120,5130,5140)                            955
C End of Local Procedure                                                  955
* ---------------------------------------------------------------
C Begin Local Procedure FGFA                                              958
C FGFA FRONT PLATE INNER ALU RING                                         958
5220  CONTINUE                                                            958
         AG_TITLE='FGFA'                                                  958
         AG_MODULE=MODULE                                                 958
         AG_MARK='V00'                                                    958
         CALL AGSPUSH                                                     958
         AG_MODULE=MODULE                                                 958
C    *                                                                    958
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        959
         CALL AGSRESET                                                    959
         AG_TITLE = 'ALUMINIUM'                                           959
         AG_EXNAME = 'MATERIAL'                                           959
         AG_PARLIST = 'NONE'                                              959
         CALL AXMATERIAL                                                  959
C    *                                                                    959
C    Geant ATTRIBUTE : FGFA ===> SEEN=1 COLO=1 <===                       959
         CALL AGSRESET                                                    960
         AG_SEEN = 1                                                      960
         AG_COLO = 1                                                      960
         AG_TITLE = 'FGFA'                                                960
         AG_EXNAME = 'ATTRIBUTE'                                          960
         AG_PARLIST = 'SEEN_COLO'                                         960
         CALL AXATTRIBUTE                                                 960
C    *                                                                    960
C    Geant SHAPE : TUBE ===> RMIN=FGTG_FRARIN RMAX=FGTG_FRAROUT DZ=FGTG_  960
C    FRATHK/2.0 <===                                                      960
         CALL AGSRESET                                                    961
         AG_RMIN = FGTG_FRARIN                                            961
         AG_RMAX = FGTG_FRAROUT                                           961
         AG_DZ = FGTG_FRATHK/2.0                                          961
         AG_TITLE = 'TUBE'                                                961
         AG_EXNAME = 'SHAPE'                                              961
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      961
         CALL AXSHAPE                                                     961
C    *                                                                    961
5221  CALL AGSPOPB                                                        961
      GO TO BLOCK_FGFA, (5220 ,5230)                                      962
C End of Local Procedure                                                  962
* ---------------------------------------------------------------
C Begin Local Procedure FGFL                                              965
C FGFL FRONT PLATE LIP OF THE INNER ALU RING                              965
5240  CONTINUE                                                            965
         AG_TITLE='FGFL'                                                  965
         AG_MODULE=MODULE                                                 965
         AG_MARK='V00'                                                    965
         CALL AGSPUSH                                                     965
         AG_MODULE=MODULE                                                 965
C    *                                                                    965
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        966
         CALL AGSRESET                                                    966
         AG_TITLE = 'ALUMINIUM'                                           966
         AG_EXNAME = 'MATERIAL'                                           966
         AG_PARLIST = 'NONE'                                              966
         CALL AXMATERIAL                                                  966
C    *                                                                    966
C    Geant ATTRIBUTE : FGFL ===> SEEN=1 COLO=1 <===                       966
         CALL AGSRESET                                                    967
         AG_SEEN = 1                                                      967
         AG_COLO = 1                                                      967
         AG_TITLE = 'FGFL'                                                967
         AG_EXNAME = 'ATTRIBUTE'                                          967
         AG_PARLIST = 'SEEN_COLO'                                         967
         CALL AXATTRIBUTE                                                 967
C    *                                                                    967
C    Geant SHAPE : TUBE ===> RMIN=FGTG_FRALRIN RMAX=FGTG_FRAROUT DZ=(FGT  967
C    G_FPTHK-FGTG_FRATHK)/2.0 <===                                        967
         CALL AGSRESET                                                    968
         AG_RMIN = FGTG_FRALRIN                                           968
         AG_RMAX = FGTG_FRAROUT                                           968
         AG_DZ = (FGTG_FPTHK-FGTG_FRATHK)/2.0                             968
         AG_TITLE = 'TUBE'                                                968
         AG_EXNAME = 'SHAPE'                                              968
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      968
         CALL AXSHAPE                                                     968
C    *                                                                    968
5241  CALL AGSPOPB                                                        968
      GO TO BLOCK_FGFL, (5240 ,5250)                                      969
C End of Local Procedure                                                  969
* ---------------------------------------------------------------
C Begin Local Procedure FGOR                                              972
C FGOR OUTER RAILS , ALU                                                  972
5300  CONTINUE                                                            972
         AG_TITLE='FGOR'                                                  972
         AG_MODULE=MODULE                                                 972
         AG_MARK='V00'                                                    972
         CALL AGSPUSH                                                     972
         AG_MODULE=MODULE                                                 972
C    *                                                                    972
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        973
         CALL AGSRESET                                                    973
         AG_TITLE = 'ALUMINIUM'                                           973
         AG_EXNAME = 'MATERIAL'                                           973
         AG_PARLIST = 'NONE'                                              973
         CALL AXMATERIAL                                                  973
C    *                                                                    973
C    Geant ATTRIBUTE : FGOR ===> SEEN=1 COLO=2 <===                       973
         CALL AGSRESET                                                    974
         AG_SEEN = 1                                                      974
         AG_COLO = 2                                                      974
         AG_TITLE = 'FGOR'                                                974
         AG_EXNAME = 'ATTRIBUTE'                                          974
         AG_PARLIST = 'SEEN_COLO'                                         974
         CALL AXATTRIBUTE                                                 974
C    *                                                                    974
C    Geant SHAPE : TUBE ===> RMIN=0 RMAX=FGTG_FGORRAD DZ=FGTG_OCLEN/2.0   974
C    <===                                                                 974
         CALL AGSRESET                                                    975
         AG_RMIN = 0                                                      975
         AG_RMAX = FGTG_FGORRAD                                           975
         AG_DZ = FGTG_OCLEN/2.0                                           975
         AG_TITLE = 'TUBE'                                                975
         AG_EXNAME = 'SHAPE'                                              975
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      975
         CALL AXSHAPE                                                     975
C    *                                                                    975
5301  CALL AGSPOPB                                                        975
      GO TO BLOCK_FGOR, (5300 ,5310,5320)                                 976
C End of Local Procedure                                                  976
* ---------------------------------------------------------------
C Begin Local Procedure FGOA                                              979
C FGOA OUTER ALU RING                                                     979
5280  CONTINUE                                                            979
         AG_TITLE='FGOA'                                                  979
         AG_MODULE=MODULE                                                 979
         AG_MARK='V00'                                                    979
         CALL AGSPUSH                                                     979
         AG_MODULE=MODULE                                                 979
C    *                                                                    979
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        980
         CALL AGSRESET                                                    980
         AG_TITLE = 'ALUMINIUM'                                           980
         AG_EXNAME = 'MATERIAL'                                           980
         AG_PARLIST = 'NONE'                                              980
         CALL AXMATERIAL                                                  980
C    *                                                                    980
C    Geant ATTRIBUTE : FGOA ===> SEEN=1 COLO=1 <===                       980
         CALL AGSRESET                                                    981
         AG_SEEN = 1                                                      981
         AG_COLO = 1                                                      981
         AG_TITLE = 'FGOA'                                                981
         AG_EXNAME = 'ATTRIBUTE'                                          981
         AG_PARLIST = 'SEEN_COLO'                                         981
         CALL AXATTRIBUTE                                                 981
C    *                                                                    981
C    Geant SHAPE : TUBE ===> RMIN=ROUT RMAX=ROUT+FGTG_FGOADELR DZ=FGTG_F  981
C    GOADELZ/2.0 <===                                                     981
         CALL AGSRESET                                                    982
         AG_RMIN = ROUT                                                   982
         AG_RMAX = ROUT+FGTG_FGOADELR                                     982
         AG_DZ = FGTG_FGOADELZ/2.0                                        982
         AG_TITLE = 'TUBE'                                                982
         AG_EXNAME = 'SHAPE'                                              982
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      982
         CALL AXSHAPE                                                     982
C    *                                                                    982
5281  CALL AGSPOPB                                                        982
      GO TO BLOCK_FGOA, (5280 ,5290)                                      983
C End of Local Procedure                                                  983
* ------------------------------
C Begin Local Procedure FGOU                                              986
C FGOU FGT UTILITY LINES, ALL MIXED AND AVERAGED                          986
5400  CONTINUE                                                            986
         AG_TITLE='FGOU'                                                  986
         AG_MODULE=MODULE                                                 986
         AG_MARK='V00'                                                    986
         CALL AGSPUSH                                                     986
         AG_MODULE=MODULE                                                 986
C    *                                                                    986
C    Geant MATERIAL : CABLE_FGT ===> inherited arguments used <===        987
         CALL AGSRESET                                                    987
         AG_TITLE = 'CABLE_FGT'                                           987
         AG_EXNAME = 'MATERIAL'                                           987
         AG_PARLIST = 'NONE'                                              987
         CALL AXMATERIAL                                                  987
C    *                                                                    987
C    Geant ATTRIBUTE : FGOU ===> SEEN=1 COLO=1 <===                       987
         CALL AGSRESET                                                    988
         AG_SEEN = 1                                                      988
         AG_COLO = 1                                                      988
         AG_TITLE = 'FGOU'                                                988
         AG_EXNAME = 'ATTRIBUTE'                                          988
         AG_PARLIST = 'SEEN_COLO'                                         988
         CALL AXATTRIBUTE                                                 988
C    *                                                                    988
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=0 DZ=0 PHI1=0 PHI2=0 <===        988
         CALL AGSRESET                                                    989
         AG_RMIN = 0                                                      989
         AG_RMAX = 0                                                      989
         AG_DZ = 0                                                        989
         AG_PHI1 = 0                                                      989
         AG_PHI2 = 0                                                      989
         AG_TITLE = 'TUBS'                                                989
         AG_EXNAME = 'SHAPE'                                              989
         AG_PARLIST = 'RMIN_RMAX_DZ_PHI1_PHI2'                            989
         CALL AXSHAPE                                                     989
C    *                                                                    989
5401  CALL AGSPOPB                                                        989
      GO TO BLOCK_FGOU, (5400 ,5410)                                      990
C End of Local Procedure                                                  990
* ------------------------------
C Begin Local Procedure FGSU                                              993
C FGSU SSD UTILITY LINES, ALL MIXED AND AVERAGED                          993
5450  CONTINUE                                                            993
         AG_TITLE='FGSU'                                                  993
         AG_MODULE=MODULE                                                 993
         AG_MARK='V00'                                                    993
         CALL AGSPUSH                                                     993
         AG_MODULE=MODULE                                                 993
C    *                                                                    993
C    Geant MATERIAL : CABLE_SSD ===> inherited arguments used <===        994
         CALL AGSRESET                                                    994
         AG_TITLE = 'CABLE_SSD'                                           994
         AG_EXNAME = 'MATERIAL'                                           994
         AG_PARLIST = 'NONE'                                              994
         CALL AXMATERIAL                                                  994
C    *                                                                    994
C    Geant ATTRIBUTE : FGSU ===> SEEN=1 COLO=7 <===                       994
         CALL AGSRESET                                                    995
         AG_SEEN = 1                                                      995
         AG_COLO = 7                                                      995
         AG_TITLE = 'FGSU'                                                995
         AG_EXNAME = 'ATTRIBUTE'                                          995
         AG_PARLIST = 'SEEN_COLO'                                         995
         CALL AXATTRIBUTE                                                 995
C    *                                                                    995
C    Geant SHAPE : TUBS ===> RMIN=0 RMAX=0 DZ=0 PHI1=0 PHI2=0 <===        995
         CALL AGSRESET                                                    996
         AG_RMIN = 0                                                      996
         AG_RMAX = 0                                                      996
         AG_DZ = 0                                                        996
         AG_PHI1 = 0                                                      996
         AG_PHI2 = 0                                                      996
         AG_TITLE = 'TUBS'                                                996
         AG_EXNAME = 'SHAPE'                                              996
         AG_PARLIST = 'RMIN_RMAX_DZ_PHI1_PHI2'                            996
         CALL AXSHAPE                                                     996
C    *                                                                    996
5451  CALL AGSPOPB                                                        996
      GO TO BLOCK_FGSU, (5450 ,5460)                                      997
C End of Local Procedure                                                  997
* ------------------------------
C Begin Local Procedure FGSV                                              1000
C FGSV SSD UTILITY -TUBE, ALL MIXED AND AVERAGED                          1000
5030  CONTINUE                                                            1000
         AG_TITLE='FGSV'                                                  1000
         AG_MODULE=MODULE                                                 1000
         AG_MARK='V00'                                                    1000
         CALL AGSPUSH                                                     1000
         AG_MODULE=MODULE                                                 1000
C    *                                                                    1000
C    Geant MATERIAL : CABLE_SSD ===> inherited arguments used <===        1001
         CALL AGSRESET                                                    1001
         AG_TITLE = 'CABLE_SSD'                                           1001
         AG_EXNAME = 'MATERIAL'                                           1001
         AG_PARLIST = 'NONE'                                              1001
         CALL AXMATERIAL                                                  1001
C    *                                                                    1001
C    Geant ATTRIBUTE : FGSV ===> SEEN=1 COLO=7 <===                       1001
         CALL AGSRESET                                                    1002
         AG_SEEN = 1                                                      1002
         AG_COLO = 7                                                      1002
         AG_TITLE = 'FGSV'                                                1002
         AG_EXNAME = 'ATTRIBUTE'                                          1002
         AG_PARLIST = 'SEEN_COLO'                                         1002
         CALL AXATTRIBUTE                                                 1002
C    *                                                                    1002
C    Geant SHAPE : TUBE ===> RMIN=36.10 RMAX=36.27 DZ=5.0 <===            1002
         CALL AGSRESET                                                    1003
         AG_RMIN = 36.10                                                  1003
         AG_RMAX = 36.27                                                  1003
         AG_DZ = 5.0                                                      1003
         AG_TITLE = 'TUBE'                                                1003
         AG_EXNAME = 'SHAPE'                                              1003
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1003
         CALL AXSHAPE                                                     1003
C    *                                                                    1003
5031  CALL AGSPOPB                                                        1003
      GO TO BLOCK_FGSV, (5030 ,5040)                                      1004
C End of Local Procedure                                                  1004
* ---------------------------------------------------------------
C Begin Local Procedure FGRS                                              1008
C FGRS RESISTOR STRIP FOR IFC RESITOR CHAIN                               1008
5150  CONTINUE                                                            1008
         AG_TITLE='FGRS'                                                  1008
         AG_MODULE=MODULE                                                 1008
         AG_MARK='V00'                                                    1008
         CALL AGSPUSH                                                     1008
         AG_MODULE=MODULE                                                 1008
C    *                                                                    1008
C    Geant MATERIAL : ALUMINA ===> inherited arguments used <===          1009
         CALL AGSRESET                                                    1009
         AG_TITLE = 'ALUMINA'                                             1009
         AG_EXNAME = 'MATERIAL'                                           1009
         AG_PARLIST = 'NONE'                                              1009
         CALL AXMATERIAL                                                  1009
C    *                                                                    1009
C    Geant ATTRIBUTE : FGRS ===> SEEN=1 COLO=1 ! WAS 3 <===               1009
         CALL AGSRESET                                                    1010
         AG_SEEN = 1                                                      1010
C    WAS 3                                                                1010
         AG_COLO = 1                                                      1010
         AG_TITLE = 'FGRS'                                                1010
         AG_EXNAME = 'ATTRIBUTE'                                          1010
         AG_PARLIST = 'SEEN_COLO'                                         1010
         CALL AXATTRIBUTE                                                 1010
C    *                                                                    1010
C    Geant SHAPE : TUBS ===> RMIN=FGTG_IFCRIN-FGTG_IFCRESDELR-FGTG_IFCTU  1010
C    BROUT RMAX=FGTG_IFCRIN-FGTG_IFCTUBROUT DZ=FGTG_IFCLENZ/2.0 PHI1=-FG  1010
C    TG_IFCRESDELA/2.0 PHI2=FGTG_IFCRESDELA/2.0 <===                      1010
         CALL AGSRESET                                                    1014
         AG_RMIN = FGTG_IFCRIN-FGTG_IFCRESDELR-FGTG_IFCTUBROUT            1014
         AG_RMAX = FGTG_IFCRIN-FGTG_IFCTUBROUT                            1014
         AG_DZ = FGTG_IFCLENZ/2.0                                         1014
         AG_PHI1 = -FGTG_IFCRESDELA/2.0                                   1014
         AG_PHI2 = FGTG_IFCRESDELA/2.0                                    1014
         AG_TITLE = 'TUBS'                                                1014
         AG_EXNAME = 'SHAPE'                                              1014
         AG_PARLIST = 'RMIN_RMAX_DZ_PHI1_PHI2'                            1014
         CALL AXSHAPE                                                     1014
C    *                                                                    1014
5151  CALL AGSPOPB                                                        1014
      GO TO BLOCK_FGRS, (5150 ,5160,5170)                                 1015
C End of Local Procedure                                                  1015
* ---------------------------------------------------------------
C Begin Local Procedure FGXR                                              1019
C FGXR SSD INTERFACE ALU RING                                             1019
5010  CONTINUE                                                            1019
         AG_TITLE='FGXR'                                                  1019
         AG_MODULE=MODULE                                                 1019
         AG_MARK='V00'                                                    1019
         CALL AGSPUSH                                                     1019
         AG_MODULE=MODULE                                                 1019
C    *                                                                    1019
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1020
         CALL AGSRESET                                                    1020
         AG_TITLE = 'ALUMINIUM'                                           1020
         AG_EXNAME = 'MATERIAL'                                           1020
         AG_PARLIST = 'NONE'                                              1020
         CALL AXMATERIAL                                                  1020
C    *                                                                    1020
C    Geant ATTRIBUTE : FGXR ===> SEEN=1 COLO=1 <===                       1020
         CALL AGSRESET                                                    1021
         AG_SEEN = 1                                                      1021
         AG_COLO = 1                                                      1021
         AG_TITLE = 'FGXR'                                                1021
         AG_EXNAME = 'ATTRIBUTE'                                          1021
         AG_PARLIST = 'SEEN_COLO'                                         1021
         CALL AXATTRIBUTE                                                 1021
C    *                                                                    1021
C    Geant SHAPE : TUBE ===> RMIN=28.0 RMAX=36.0 DZ=0.7/2.0 <===          1021
         CALL AGSRESET                                                    1022
         AG_RMIN = 28.0                                                   1022
         AG_RMAX = 36.0                                                   1022
         AG_DZ = 0.7/2.0                                                  1022
         AG_TITLE = 'TUBE'                                                1022
         AG_EXNAME = 'SHAPE'                                              1022
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1022
         CALL AXSHAPE                                                     1022
C    *                                                                    1022
5011  CALL AGSPOPB                                                        1022
      GO TO BLOCK_FGXR, (5010 ,5020)                                      1023
C End of Local Procedure                                                  1023
* ---------------------------------------------------------------
C Begin Local Procedure FGXC                                              1026
C FGXC SSD SUPPORTING CARBON RING                                         1026
5050  CONTINUE                                                            1026
         AG_TITLE='FGXC'                                                  1026
         AG_MODULE=MODULE                                                 1026
         AG_MARK='V00'                                                    1026
         CALL AGSPUSH                                                     1026
         AG_MODULE=MODULE                                                 1026
C    *                                                                    1026
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           1027
         CALL AGSRESET                                                    1027
         AG_TITLE = 'CARBON'                                              1027
         AG_EXNAME = 'MATERIAL'                                           1027
         AG_PARLIST = 'NONE'                                              1027
         CALL AXMATERIAL                                                  1027
C    *                                                                    1027
C    Geant ATTRIBUTE : FGXC ===> SEEN=1 COLO=1 <===                       1027
         CALL AGSRESET                                                    1028
         AG_SEEN = 1                                                      1028
         AG_COLO = 1                                                      1028
         AG_TITLE = 'FGXC'                                                1028
         AG_EXNAME = 'ATTRIBUTE'                                          1028
         AG_PARLIST = 'SEEN_COLO'                                         1028
         CALL AXATTRIBUTE                                                 1028
C    *                                                                    1028
C    Geant SHAPE : TUBE ===> RMIN=2.5 RMAX=21.0 DZ=0.1/2.0 <===           1028
         CALL AGSRESET                                                    1029
         AG_RMIN = 2.5                                                    1029
         AG_RMAX = 21.0                                                   1029
         AG_DZ = 0.1/2.0                                                  1029
         AG_TITLE = 'TUBE'                                                1029
         AG_EXNAME = 'SHAPE'                                              1029
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1029
         CALL AXSHAPE                                                     1029
C    *                                                                    1029
5051  CALL AGSPOPB                                                        1029
      GO TO BLOCK_FGXC, (5050 ,5060)                                      1030
C End of Local Procedure                                                  1030
* ---------------------------------------------------------------
C Begin Local Procedure FGXD                                              1033
C FGXD IST SUPPORTING CARBON RING                                         1033
5070  CONTINUE                                                            1033
         AG_TITLE='FGXD'                                                  1033
         AG_MODULE=MODULE                                                 1033
         AG_MARK='V00'                                                    1033
         CALL AGSPUSH                                                     1033
         AG_MODULE=MODULE                                                 1033
C    *                                                                    1033
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           1034
         CALL AGSRESET                                                    1034
         AG_TITLE = 'CARBON'                                              1034
         AG_EXNAME = 'MATERIAL'                                           1034
         AG_PARLIST = 'NONE'                                              1034
         CALL AXMATERIAL                                                  1034
C    *                                                                    1034
C    Geant ATTRIBUTE : FGXD ===> SEEN=1 COLO=1 <===                       1034
         CALL AGSRESET                                                    1035
         AG_SEEN = 1                                                      1035
         AG_COLO = 1                                                      1035
         AG_TITLE = 'FGXD'                                                1035
         AG_EXNAME = 'ATTRIBUTE'                                          1035
         AG_PARLIST = 'SEEN_COLO'                                         1035
         CALL AXATTRIBUTE                                                 1035
C    *                                                                    1035
C    Geant SHAPE : TUBE ===> RMIN=2.5 RMAX=13.0 DZ=0.1/2.0 <===           1035
         CALL AGSRESET                                                    1036
         AG_RMIN = 2.5                                                    1036
         AG_RMAX = 13.0                                                   1036
         AG_DZ = 0.1/2.0                                                  1036
         AG_TITLE = 'TUBE'                                                1036
         AG_EXNAME = 'SHAPE'                                              1036
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1036
         CALL AXSHAPE                                                     1036
C    *                                                                    1036
5071  CALL AGSPOPB                                                        1036
      GO TO BLOCK_FGXD, (5070 ,5080)                                      1037
C End of Local Procedure                                                  1037
* ---------------------------------------------------------------
C Begin Local Procedure FGXE                                              1040
C FGXE HFT SUPPORTING CARBON RING                                         1040
5090  CONTINUE                                                            1040
         AG_TITLE='FGXE'                                                  1040
         AG_MODULE=MODULE                                                 1040
         AG_MARK='V00'                                                    1040
         CALL AGSPUSH                                                     1040
         AG_MODULE=MODULE                                                 1040
C    *                                                                    1040
C    Geant MATERIAL : CARBON ===> inherited arguments used <===           1041
         CALL AGSRESET                                                    1041
         AG_TITLE = 'CARBON'                                              1041
         AG_EXNAME = 'MATERIAL'                                           1041
         AG_PARLIST = 'NONE'                                              1041
         CALL AXMATERIAL                                                  1041
C    *                                                                    1041
C    Geant ATTRIBUTE : FGXE ===> SEEN=1 COLO=1 <===                       1041
         CALL AGSRESET                                                    1042
         AG_SEEN = 1                                                      1042
         AG_COLO = 1                                                      1042
         AG_TITLE = 'FGXE'                                                1042
         AG_EXNAME = 'ATTRIBUTE'                                          1042
         AG_PARLIST = 'SEEN_COLO'                                         1042
         CALL AXATTRIBUTE                                                 1042
C    *                                                                    1042
C    Geant SHAPE : TUBE ===> RMIN=2.5 RMAX=8.0 DZ=0.1/2.0 <===            1042
         CALL AGSRESET                                                    1043
         AG_RMIN = 2.5                                                    1043
         AG_RMAX = 8.0                                                    1043
         AG_DZ = 0.1/2.0                                                  1043
         AG_TITLE = 'TUBE'                                                1043
         AG_EXNAME = 'SHAPE'                                              1043
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1043
         CALL AXSHAPE                                                     1043
C    *                                                                    1043
5091  CALL AGSPOPB                                                        1043
      GO TO BLOCK_FGXE, (5090 ,5100)                                      1044
C End of Local Procedure                                                  1044
* ---------------------------------------------------------------
C Begin Local Procedure FGAP                                              1048
C FGAP APV-CHIPS BLOCK ENCAPSULATION VOLUME                               1048
5370  CONTINUE                                                            1048
         AG_TITLE='FGAP'                                                  1048
         AG_MODULE=MODULE                                                 1048
         AG_MARK='V00'                                                    1048
         CALL AGSPUSH                                                     1048
         AG_MODULE=MODULE                                                 1048
C    *                                                                    1048
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1049
         CALL AGSRESET                                                    1049
         AG_TITLE = 'AIR'                                                 1049
         AG_EXNAME = 'MATERIAL'                                           1049
         AG_PARLIST = 'NONE'                                              1049
         CALL AXMATERIAL                                                  1049
C    *                                                                    1049
C    Geant ATTRIBUTE : FGAP ===> SEEN=0 COLO=6 <===                       1049
         CALL AGSRESET                                                    1050
         AG_SEEN = 0                                                      1050
         AG_COLO = 6                                                      1050
         AG_TITLE = 'FGAP'                                                1050
         AG_EXNAME = 'ATTRIBUTE'                                          1050
         AG_PARLIST = 'SEEN_COLO'                                         1050
         CALL AXATTRIBUTE                                                 1050
C    *                                                                    1050
C    Geant SHAPE : BOX ===> DX=FGTG_APVX/2.0 DY=FGTG_APVY/2.0 DZ=FGTG_AP  1050
C    VZ/2.0+FGTG_APVDDZ/2 <===                                            1050
         CALL AGSRESET                                                    1051
         AG_DX = FGTG_APVX/2.0                                            1051
         AG_DY = FGTG_APVY/2.0                                            1051
         AG_DZ = FGTG_APVZ/2.0+FGTG_APVDDZ/2                              1051
         AG_TITLE = 'BOX'                                                 1051
         AG_EXNAME = 'SHAPE'                                              1051
         AG_PARLIST = 'DX_DY_DZ'                                          1051
         CALL AXSHAPE                                                     1051
C    *                                                                    1051
C         * add PC boards                                                 1051
C    Create FGAD :                                                        1053
C    Call To Local Procedure FGAD                                         1053
         ASSIGN 5660 TO BLOCK_FGAD                                        1053
         GO TO  5650                                                      1053
5660     CONTINUE                                                         1053
C    *                                                                    1053
C    Geant POSITION : FGAD ===> X=0 Y=0 Z=-FGTG_APVZ/2 <===               1053
         CALL AGSRESET                                                    1053
         AG_X = 0                                                         1053
         AG_Y = 0                                                         1053
         AG_Z = -FGTG_APVZ/2                                              1053
         AG_TITLE = 'FGAD'                                                1053
         AG_EXNAME = 'POSITION'                                           1053
         AG_PARLIST = 'X_Y_Z'                                             1053
         CALL AXPOSITION                                                  1053
C    *                                                                    1053
C    Create FGAS :                                                        1054
C    Call To Local Procedure FGAS                                         1054
         ASSIGN 5680 TO BLOCK_FGAS                                        1054
         GO TO  5670                                                      1054
5680     CONTINUE                                                         1054
C    *                                                                    1054
C    Geant POSITION : FGAS ===> X=(-FGTG_APVX + FGTG_APVD)/2.0 Y=0 Z=FGT  1054
C    G_APVDDZ/2 <===                                                      1054
         CALL AGSRESET                                                    1054
         AG_X = (-FGTG_APVX + FGTG_APVD)/2.0                              1054
         AG_Y = 0                                                         1054
         AG_Z = FGTG_APVDDZ/2                                             1054
         AG_TITLE = 'FGAS'                                                1054
         AG_EXNAME = 'POSITION'                                           1054
         AG_PARLIST = 'X_Y_Z'                                             1054
         CALL AXPOSITION                                                  1054
C    *                                                                    1054
C    Create FGAL :                                                        1055
C    Call To Local Procedure FGAL                                         1055
         ASSIGN 5700 TO BLOCK_FGAL                                        1055
         GO TO  5690                                                      1055
5700     CONTINUE                                                         1055
C    *                                                                    1055
C    Geant POSITION : FGAL ===> X=( FGTG_APVX - FGTG_APVD)/2.0 Y=0 Z=FGT  1055
C    G_APVDDZ/2 <===                                                      1055
         CALL AGSRESET                                                    1055
         AG_X = ( FGTG_APVX - FGTG_APVD)/2.0                              1055
         AG_Y = 0                                                         1055
         AG_Z = FGTG_APVDDZ/2                                             1055
         AG_TITLE = 'FGAL'                                                1055
         AG_EXNAME = 'POSITION'                                           1055
         AG_PARLIST = 'X_Y_Z'                                             1055
         CALL AXPOSITION                                                  1055
C    *                                                                    1055
C    Create FGAB :                                                        1056
C    Call To Local Procedure FGAB                                         1056
         ASSIGN 5720 TO BLOCK_FGAB                                        1056
         GO TO  5710                                                      1056
5720     CONTINUE                                                         1056
C    *                                                                    1056
C    Geant POSITION : FGAB ===> X=0 Y=-FGTG_APVDY/2.0 Z=FGTG_APVDDZ/2 <=  1056
C    ==                                                                   1056
         CALL AGSRESET                                                    1056
         AG_X = 0                                                         1056
         AG_Y = -FGTG_APVDY/2.0                                           1056
         AG_Z = FGTG_APVDDZ/2                                             1056
         AG_TITLE = 'FGAB'                                                1056
         AG_EXNAME = 'POSITION'                                           1056
         AG_PARLIST = 'X_Y_Z'                                             1056
         CALL AXPOSITION                                                  1056
C    *                                                                    1056
C    Create FGAB :                                                        1057
C    Call To Local Procedure FGAB                                         1057
         ASSIGN 5730 TO BLOCK_FGAB                                        1057
         GO TO  5710                                                      1057
5730     CONTINUE                                                         1057
C    *                                                                    1057
C    Geant POSITION : FGAB ===> X=0 Y=+FGTG_APVDY/2.0 Z=FGTG_APVDDZ/2 <=  1057
C    ==                                                                   1057
         CALL AGSRESET                                                    1057
         AG_X = 0                                                         1057
         AG_Y = +FGTG_APVDY/2.0                                           1057
         AG_Z = FGTG_APVDDZ/2                                             1057
         AG_TITLE = 'FGAB'                                                1057
         AG_EXNAME = 'POSITION'                                           1057
         AG_PARLIST = 'X_Y_Z'                                             1057
         CALL AXPOSITION                                                  1057
C    *                                                                    1057
5371  CALL AGSPOPB                                                        1057
      GO TO BLOCK_FGAP, (5370 ,5380)                                      1058
C End of Local Procedure                                                  1058
* ---------------------------------------------------------------
C Begin Local Procedure FGAL                                              1062
C FGAL APV- LARGE END-BOARD MADE OF FR4+CU                                1062
5690  CONTINUE                                                            1062
         AG_TITLE='FGAL'                                                  1062
         AG_MODULE=MODULE                                                 1062
         AG_MARK='V00'                                                    1062
         CALL AGSPUSH                                                     1062
         AG_MODULE=MODULE                                                 1062
C    *                                                                    1062
C    Geant MATERIAL : FR4CU ===> inherited arguments used <===            1063
         CALL AGSRESET                                                    1063
         AG_TITLE = 'FR4CU'                                               1063
         AG_EXNAME = 'MATERIAL'                                           1063
         AG_PARLIST = 'NONE'                                              1063
         CALL AXMATERIAL                                                  1063
C    *                                                                    1063
C    Geant ATTRIBUTE : FGAL ===> SEEN=1 COLO=7 <===                       1063
         CALL AGSRESET                                                    1064
         AG_SEEN = 1                                                      1064
         AG_COLO = 7                                                      1064
         AG_TITLE = 'FGAL'                                                1064
         AG_EXNAME = 'ATTRIBUTE'                                          1064
         AG_PARLIST = 'SEEN_COLO'                                         1064
         CALL AXATTRIBUTE                                                 1064
C    *                                                                    1064
C    Geant SHAPE : BOX ===> DX=FGTG_APVD/2.0 DY=FGTG_APVY/2.0 DZ=FGTG_AP  1064
C    VZ/2.0 <===                                                          1064
         CALL AGSRESET                                                    1065
         AG_DX = FGTG_APVD/2.0                                            1065
         AG_DY = FGTG_APVY/2.0                                            1065
         AG_DZ = FGTG_APVZ/2.0                                            1065
         AG_TITLE = 'BOX'                                                 1065
         AG_EXNAME = 'SHAPE'                                              1065
         AG_PARLIST = 'DX_DY_DZ'                                          1065
         CALL AXSHAPE                                                     1065
C    *                                                                    1065
5691  CALL AGSPOPB                                                        1065
      GO TO BLOCK_FGAL, (5690 ,5700)                                      1066
C End of Local Procedure                                                  1066
* ---------------------------------------------------------------
C Begin Local Procedure FGAS                                              1069
C FGAS APV- SMALL END-BOARD MADE OF FR4+CU                                1069
5670  CONTINUE                                                            1069
         AG_TITLE='FGAS'                                                  1069
         AG_MODULE=MODULE                                                 1069
         AG_MARK='V00'                                                    1069
         CALL AGSPUSH                                                     1069
         AG_MODULE=MODULE                                                 1069
C    *                                                                    1069
C    Geant MATERIAL : FR4CU ===> inherited arguments used <===            1070
         CALL AGSRESET                                                    1070
         AG_TITLE = 'FR4CU'                                               1070
         AG_EXNAME = 'MATERIAL'                                           1070
         AG_PARLIST = 'NONE'                                              1070
         CALL AXMATERIAL                                                  1070
C    *                                                                    1070
C    Geant ATTRIBUTE : FGAS ===> SEEN=1 COLO=7 <===                       1070
         CALL AGSRESET                                                    1071
         AG_SEEN = 1                                                      1071
         AG_COLO = 7                                                      1071
         AG_TITLE = 'FGAS'                                                1071
         AG_EXNAME = 'ATTRIBUTE'                                          1071
         AG_PARLIST = 'SEEN_COLO'                                         1071
         CALL AXATTRIBUTE                                                 1071
C    *                                                                    1071
C    Geant SHAPE : BOX ===> DX=FGTG_APVD/2.0 DY=FGTG_APVY2/2.0 DZ=FGTG_A  1071
C    PVZ/2.0 <===                                                         1071
         CALL AGSRESET                                                    1072
         AG_DX = FGTG_APVD/2.0                                            1072
         AG_DY = FGTG_APVY2/2.0                                           1072
         AG_DZ = FGTG_APVZ/2.0                                            1072
         AG_TITLE = 'BOX'                                                 1072
         AG_EXNAME = 'SHAPE'                                              1072
         AG_PARLIST = 'DX_DY_DZ'                                          1072
         CALL AXSHAPE                                                     1072
C    *                                                                    1072
5671  CALL AGSPOPB                                                        1072
      GO TO BLOCK_FGAS, (5670 ,5680)                                      1073
C End of Local Procedure                                                  1073
* ---------------------------------------------------------------
C Begin Local Procedure FGAB                                              1076
C FGAB APV- BLADE-BOARD MADE OF FR4+CU                                    1076
5710  CONTINUE                                                            1076
         AG_TITLE='FGAB'                                                  1076
         AG_MODULE=MODULE                                                 1076
         AG_MARK='V00'                                                    1076
         CALL AGSPUSH                                                     1076
         AG_MODULE=MODULE                                                 1076
C    *                                                                    1076
C    Geant MATERIAL : FR4CU ===> inherited arguments used <===            1077
         CALL AGSRESET                                                    1077
         AG_TITLE = 'FR4CU'                                               1077
         AG_EXNAME = 'MATERIAL'                                           1077
         AG_PARLIST = 'NONE'                                              1077
         CALL AXMATERIAL                                                  1077
C    *                                                                    1077
C    Geant ATTRIBUTE : FGAB ===> SEEN=1 COLO=7 <===                       1077
         CALL AGSRESET                                                    1078
         AG_SEEN = 1                                                      1078
         AG_COLO = 7                                                      1078
         AG_TITLE = 'FGAB'                                                1078
         AG_EXNAME = 'ATTRIBUTE'                                          1078
         AG_PARLIST = 'SEEN_COLO'                                         1078
         CALL AXATTRIBUTE                                                 1078
C    *                                                                    1078
C    Geant SHAPE : BOX ===> DX=FGTG_APVX/2.0-FGTG_APVD DY=FGTG_APVD/2.0   1078
C    DZ=FGTG_APVZ/2.0 <===                                                1078
         CALL AGSRESET                                                    1079
         AG_DX = FGTG_APVX/2.0-FGTG_APVD                                  1079
         AG_DY = FGTG_APVD/2.0                                            1079
         AG_DZ = FGTG_APVZ/2.0                                            1079
         AG_TITLE = 'BOX'                                                 1079
         AG_EXNAME = 'SHAPE'                                              1079
         AG_PARLIST = 'DX_DY_DZ'                                          1079
         CALL AXSHAPE                                                     1079
C    *                                                                    1079
5711  CALL AGSPOPB                                                        1079
      GO TO BLOCK_FGAB, (5710 ,5720,5730)                                 1080
C End of Local Procedure                                                  1080
* ---------------------------------------------------------------
C Begin Local Procedure FGAD                                              1083
C FGAD APV- MATERIAL BETWEEN QUADRANTS MADE OF FR4+CU                     1083
5650  CONTINUE                                                            1083
         AG_TITLE='FGAD'                                                  1083
         AG_MODULE=MODULE                                                 1083
         AG_MARK='V00'                                                    1083
         CALL AGSPUSH                                                     1083
         AG_MODULE=MODULE                                                 1083
C    *                                                                    1083
C    Geant MATERIAL : FR4CU ===> inherited arguments used <===            1084
         CALL AGSRESET                                                    1084
         AG_TITLE = 'FR4CU'                                               1084
         AG_EXNAME = 'MATERIAL'                                           1084
         AG_PARLIST = 'NONE'                                              1084
         CALL AXMATERIAL                                                  1084
C    *                                                                    1084
C    Geant ATTRIBUTE : FGAD ===> SEEN=1 COLO=1 <===                       1084
         CALL AGSRESET                                                    1085
         AG_SEEN = 1                                                      1085
         AG_COLO = 1                                                      1085
         AG_TITLE = 'FGAD'                                                1085
         AG_EXNAME = 'ATTRIBUTE'                                          1085
         AG_PARLIST = 'SEEN_COLO'                                         1085
         CALL AXATTRIBUTE                                                 1085
C    *                                                                    1085
C    Geant SHAPE : BOX ===> DX=FGTG_APVX/2.0 DY=FGTG_APVDDY/2.0 DZ=FGTG_  1085
C    APVDDZ/2.0 <===                                                      1085
         CALL AGSRESET                                                    1086
         AG_DX = FGTG_APVX/2.0                                            1086
         AG_DY = FGTG_APVDDY/2.0                                          1086
         AG_DZ = FGTG_APVDDZ/2.0                                          1086
         AG_TITLE = 'BOX'                                                 1086
         AG_EXNAME = 'SHAPE'                                              1086
         AG_PARLIST = 'DX_DY_DZ'                                          1086
         CALL AXSHAPE                                                     1086
C    *                                                                    1086
5651  CALL AGSPOPB                                                        1086
      GO TO BLOCK_FGAD, (5650 ,5660)                                      1087
C End of Local Procedure                                                  1087
* ---------------------------------------------------------------
C Begin Local Procedure FGOC                                              1090
C FGOC OUTER CYLINDER, CARBON FIBER FGTG_OCTHK                            1090
5260  CONTINUE                                                            1090
         AG_TITLE='FGOC'                                                  1090
         AG_MODULE=MODULE                                                 1090
         AG_MARK='V00'                                                    1090
         CALL AGSPUSH                                                     1090
         AG_MODULE=MODULE                                                 1090
C    *                                                                    1090
C    Geant MATERIAL : CFIBER ===> inherited arguments used <===           1091
         CALL AGSRESET                                                    1091
         AG_TITLE = 'CFIBER'                                              1091
         AG_EXNAME = 'MATERIAL'                                           1091
         AG_PARLIST = 'NONE'                                              1091
         CALL AXMATERIAL                                                  1091
C    *                                                                    1091
C    Geant ATTRIBUTE : FGOC ===> SEEN=1 COLO=3 <===                       1091
         CALL AGSRESET                                                    1092
         AG_SEEN = 1                                                      1092
         AG_COLO = 3                                                      1092
         AG_TITLE = 'FGOC'                                                1092
         AG_EXNAME = 'ATTRIBUTE'                                          1092
         AG_PARLIST = 'SEEN_COLO'                                         1092
         CALL AXATTRIBUTE                                                 1092
C    *                                                                    1092
C    Geant SHAPE : TUBE ===> RMIN=FGTG_GEMRO+FGTG_GR2 RMAX=FGTG_GEMRO+FG  1092
C    TG_GR2+FGTG_OCTHK DZ=FGTG_OCLEN/2.0 <===                             1092
         CALL AGSRESET                                                    1094
         AG_RMIN = FGTG_GEMRO+FGTG_GR2                                    1094
         AG_RMAX = FGTG_GEMRO+FGTG_GR2+FGTG_OCTHK                         1094
         AG_DZ = FGTG_OCLEN/2.0                                           1094
         AG_TITLE = 'TUBE'                                                1094
         AG_EXNAME = 'SHAPE'                                              1094
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1094
         CALL AXSHAPE                                                     1094
C    *                                                                    1094
5261  CALL AGSPOPB                                                        1094
      GO TO BLOCK_FGOC, (5260 ,5270)                                      1095
C End of Local Procedure                                                  1095
* -------------------------------------------------------------------
C Begin Local Procedure FGSC                                              1098
C FGSC DESCRIBES THE SENSITIVE AREA                                       1098
5520  CONTINUE                                                            1098
         AG_TITLE='FGSC'                                                  1098
         AG_MODULE=MODULE                                                 1098
         AG_MARK='V00'                                                    1098
         CALL AGSPUSH                                                     1098
         AG_MODULE=MODULE                                                 1098
C    *                                                                    1098
C    Geant MATERIAL : ARCO2_70_30 ===> inherited arguments used <===      1099
         CALL AGSRESET                                                    1099
         AG_TITLE = 'ARCO2_70_30'                                         1099
         AG_EXNAME = 'MATERIAL'                                           1099
         AG_PARLIST = 'NONE'                                              1099
         CALL AXMATERIAL                                                  1099
C    *                                                                    1099
C    Geant MATERIAL : SENSITIVE ===> ISVOL=1 <===                         1099
         CALL AGSRESET                                                    1100
         AG_ISVOL = 1                                                     1100
         AG_TITLE = 'SENSITIVE'                                           1100
         AG_EXNAME = 'MATERIAL'                                           1100
         AG_PARLIST = 'ISVOL'                                             1100
         CALL AXMATERIAL                                                  1100
C    *                                                                    1100
C    Geant ATTRIBUTE : FGSC ===> SEEN=1 COLO=6 <===                       1100
         CALL AGSRESET                                                    1101
         AG_SEEN = 1                                                      1101
         AG_COLO = 6                                                      1101
         AG_TITLE = 'FGSC'                                                1101
         AG_EXNAME = 'ATTRIBUTE'                                          1101
         AG_PARLIST = 'SEEN_COLO'                                         1101
         CALL AXATTRIBUTE                                                 1101
C    *                                                                    1101
C    Geant SHAPE : TUBE ===> RMIN=FGSCRMIN RMAX=FGSCRMAX DZ=FGSCDZ <===   1101
         CALL AGSRESET                                                    1102
         AG_RMIN = FGSCRMIN                                               1102
         AG_RMAX = FGSCRMAX                                               1102
         AG_DZ = FGSCDZ                                                   1102
         AG_TITLE = 'TUBE'                                                1102
         AG_EXNAME = 'SHAPE'                                              1102
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1102
         CALL AXSHAPE                                                     1102
C    *                                                                    1102
C    Create FGZC :                                                        1104
C    Call To Local Procedure FGZC                                         1104
         ASSIGN 5750 TO BLOCK_FGZC                                        1104
         GO TO  5740                                                      1104
5750     CONTINUE                                                         1104
C    *                                                                    1104
5521  CALL AGSPOPB                                                        1105
      GO TO BLOCK_FGSC, (5520 ,5530)                                      1105
C End of Local Procedure                                                  1105
* -------------------------------------------------------------------
C Begin Local Procedure FGZC                                              1108
C FGZC DESCRIBES THE SENSITIVE AREA                                       1108
5740  CONTINUE                                                            1108
         AG_TITLE='FGZC'                                                  1108
         AG_MODULE=MODULE                                                 1108
         AG_MARK='V00'                                                    1108
         CALL AGSPUSH                                                     1108
         AG_MODULE=MODULE                                                 1108
C    *                                                                    1108
C    Geant ATTRIBUTE : FGZC ===> SEEN=1 COLO=7 <===                       1109
         CALL AGSRESET                                                    1109
         AG_SEEN = 1                                                      1109
         AG_COLO = 7                                                      1109
         AG_TITLE = 'FGZC'                                                1109
         AG_EXNAME = 'ATTRIBUTE'                                          1109
         AG_PARLIST = 'SEEN_COLO'                                         1109
         CALL AXATTRIBUTE                                                 1109
C    *                                                                    1109
C    Geant SHAPE : DIVISION ===> IAXIS=2 NDIV=4 <===                      1109
         CALL AGSRESET                                                    1110
         AG_IAXIS = 2                                                     1110
         AG_NDIV = 4                                                      1110
         AG_TITLE = 'DIVISION'                                            1110
         AG_EXNAME = 'SHAPE'                                              1110
         AG_PARLIST = 'IAXIS_NDIV'                                        1110
         CALL AXSHAPE                                                     1110
C    *                                                                    1110
C    Defining Hits: Z:.001:S Y:.001: X:.001: PTOT:16:(0,100), CX:10: CY:  1114
C    10: CZ:10: SLENG:16:(0,500), TOF:16:(0,1.E-6) STEP:.01: ELOSS:16:(0  1114
C    ,0.001)                                                              1114
         AG_TITLE = 'FGZC'                                                1114
         AG_EXNAME = 'HITS'                                               1114
         AG_TYPE=AG_MAGIC                                                 1114
         CALL AGSDETV('FGZC')                                             1114
C    *                                                                    1114
         AG_PARLIST='Z'                                                   1114
         AG_TITLE = 'DETH'                                                1114
         AG_NBITS = AG_MAGIC                                              1114
         AG_BIN=.001                                                      1114
         AG_OPTION='S'                                                    1114
         AG_FHMIN = AG_MAGIC                                              1114
         AG_FHMAX = AG_MAGIC                                              1114
         CALL AGSDETH (0)                                                 1114
C    *                                                                    1114
         AG_PARLIST='Y'                                                   1114
         AG_TITLE = 'DETH'                                                1114
         AG_NBITS = AG_MAGIC                                              1114
         AG_BIN=.001                                                      1114
         AG_FHMIN=AG_MAGIC                                                1114
         AG_FHMAX=AG_MAGIC                                                1114
         AG_OPTION='*'                                                    1114
         CALL AGSDETH (0)                                                 1114
C    *                                                                    1114
         AG_PARLIST='X'                                                   1114
         AG_TITLE = 'DETH'                                                1114
         AG_NBITS = AG_MAGIC                                              1114
         AG_BIN=.001                                                      1114
         AG_FHMIN=AG_MAGIC                                                1114
         AG_FHMAX=AG_MAGIC                                                1114
         AG_OPTION='*'                                                    1114
         CALL AGSDETH (0)                                                 1114
C    *                                                                    1114
         AG_PARLIST='PTOT'                                                1114
         AG_TITLE = 'DETH'                                                1114
         AG_NBITS = 16                                                    1114
         AG_BIN=AG_MAGIC                                                  1114
         AG_OPTION='*'                                                    1114
         AG_FHMIN = 0                                                     1114
         AG_FHMAX = 100                                                   1114
         CALL AGSDETH (0)                                                 1114
C    *                                                                    1114
         AG_PARLIST='CX'                                                  1114
         AG_TITLE = 'DETH'                                                1114
         AG_NBITS = 10                                                    1114
         AG_BIN=AG_MAGIC                                                  1114
         AG_FHMIN=AG_MAGIC                                                1114
         AG_FHMAX=AG_MAGIC                                                1114
         AG_OPTION='*'                                                    1114
         CALL AGSDETH (0)                                                 1114
C    *                                                                    1114
         AG_PARLIST='CY'                                                  1114
         AG_TITLE = 'DETH'                                                1114
         AG_NBITS = 10                                                    1114
         AG_BIN=AG_MAGIC                                                  1114
         AG_FHMIN=AG_MAGIC                                                1114
         AG_FHMAX=AG_MAGIC                                                1114
         AG_OPTION='*'                                                    1114
         CALL AGSDETH (0)                                                 1114
C    *                                                                    1114
         AG_PARLIST='CZ'                                                  1114
         AG_TITLE = 'DETH'                                                1114
         AG_NBITS = 10                                                    1114
         AG_BIN=AG_MAGIC                                                  1114
         AG_FHMIN=AG_MAGIC                                                1114
         AG_FHMAX=AG_MAGIC                                                1114
         AG_OPTION='*'                                                    1114
         CALL AGSDETH (0)                                                 1114
C    *                                                                    1114
         AG_PARLIST='SLENG'                                               1114
         AG_TITLE = 'DETH'                                                1114
         AG_NBITS = 16                                                    1114
         AG_BIN=AG_MAGIC                                                  1114
         AG_OPTION='*'                                                    1114
         AG_FHMIN = 0                                                     1114
         AG_FHMAX = 500                                                   1114
         CALL AGSDETH (0)                                                 1114
C    *                                                                    1114
         AG_PARLIST='TOF'                                                 1114
         AG_TITLE = 'DETH'                                                1114
         AG_NBITS = 16                                                    1114
         AG_BIN=AG_MAGIC                                                  1114
         AG_OPTION='*'                                                    1114
         AG_FHMIN = 0                                                     1114
         AG_FHMAX = 1.E-6                                                 1114
         CALL AGSDETH (0)                                                 1114
C    *                                                                    1114
         AG_PARLIST='STEP'                                                1114
         AG_TITLE = 'DETH'                                                1114
         AG_NBITS = AG_MAGIC                                              1114
         AG_BIN=.01                                                       1114
         AG_FHMIN=AG_MAGIC                                                1114
         AG_FHMAX=AG_MAGIC                                                1114
         AG_OPTION='*'                                                    1114
         CALL AGSDETH (0)                                                 1114
C    *                                                                    1114
         AG_PARLIST='ELOSS'                                               1114
         AG_TITLE = 'DETH'                                                1114
         AG_NBITS = 16                                                    1114
         AG_BIN=AG_MAGIC                                                  1114
         AG_OPTION='*'                                                    1114
         AG_FHMIN = 0                                                     1114
         AG_FHMAX = 0.001                                                 1114
         CALL AGSDETH (0)                                                 1114
C    *                                                                    1114
         CALL AGSDETU('FGZC')                                             1114
C    *                                                                    1114
5741  CALL AGSPOPB                                                        1115
      GO TO BLOCK_FGZC, (5740 ,5750)                                      1115
C End of Local Procedure                                                  1115
* ----------------------------------------------------------------------
* -------------------------------------------------------------------
C Begin Local Procedure FGSD                                              1118
C FGSD IS FGSC OF DIFFERENT SIZE                                          1118
5420  CONTINUE                                                            1118
         AG_TITLE='FGSD'                                                  1118
         AG_MODULE=MODULE                                                 1118
         AG_MARK='V00'                                                    1118
         CALL AGSPUSH                                                     1118
         AG_MODULE=MODULE                                                 1118
C    *                                                                    1118
C    Geant MATERIAL : ARCO2_70_30 ===> inherited arguments used <===      1119
         CALL AGSRESET                                                    1119
         AG_TITLE = 'ARCO2_70_30'                                         1119
         AG_EXNAME = 'MATERIAL'                                           1119
         AG_PARLIST = 'NONE'                                              1119
         CALL AXMATERIAL                                                  1119
C    *                                                                    1119
C    Geant MATERIAL : SENSITIVE ===> ISVOL=1 <===                         1119
         CALL AGSRESET                                                    1120
         AG_ISVOL = 1                                                     1120
         AG_TITLE = 'SENSITIVE'                                           1120
         AG_EXNAME = 'MATERIAL'                                           1120
         AG_PARLIST = 'ISVOL'                                             1120
         CALL AXMATERIAL                                                  1120
C    *                                                                    1120
C    Geant ATTRIBUTE : FGSD ===> SEEN=1 COLO=6 <===                       1120
         CALL AGSRESET                                                    1121
         AG_SEEN = 1                                                      1121
         AG_COLO = 6                                                      1121
         AG_TITLE = 'FGSD'                                                1121
         AG_EXNAME = 'ATTRIBUTE'                                          1121
         AG_PARLIST = 'SEEN_COLO'                                         1121
         CALL AXATTRIBUTE                                                 1121
C    *                                                                    1121
C    Geant SHAPE : TUBE ===> RMIN=FGSCRMIN RMAX=FGSCRMAX DZ=FGSCDZ <===   1121
         CALL AGSRESET                                                    1122
         AG_RMIN = FGSCRMIN                                               1122
         AG_RMAX = FGSCRMAX                                               1122
         AG_DZ = FGSCDZ                                                   1122
         AG_TITLE = 'TUBE'                                                1122
         AG_EXNAME = 'SHAPE'                                              1122
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1122
         CALL AXSHAPE                                                     1122
C    *                                                                    1122
C    Create FGZD :                                                        1124
C    Call To Local Procedure FGZD                                         1124
         ASSIGN 5770 TO BLOCK_FGZD                                        1124
         GO TO  5760                                                      1124
5770     CONTINUE                                                         1124
C    *                                                                    1124
5421  CALL AGSPOPB                                                        1125
      GO TO BLOCK_FGSD, (5420 ,5430)                                      1125
C End of Local Procedure                                                  1125
* -------------------------------------------------------------------
C Begin Local Procedure FGZD                                              1127
C FGZD DESCRIBES THE SENSITIVE AREA                                       1127
5760  CONTINUE                                                            1127
         AG_TITLE='FGZD'                                                  1127
         AG_MODULE=MODULE                                                 1127
         AG_MARK='V00'                                                    1127
         CALL AGSPUSH                                                     1127
         AG_MODULE=MODULE                                                 1127
C    *                                                                    1127
C    Geant ATTRIBUTE : FGZD ===> SEEN=1 COLO=7 <===                       1128
         CALL AGSRESET                                                    1128
         AG_SEEN = 1                                                      1128
         AG_COLO = 7                                                      1128
         AG_TITLE = 'FGZD'                                                1128
         AG_EXNAME = 'ATTRIBUTE'                                          1128
         AG_PARLIST = 'SEEN_COLO'                                         1128
         CALL AXATTRIBUTE                                                 1128
C    *                                                                    1128
C    Geant SHAPE : DIVISION ===> IAXIS=2 NDIV=4 <===                      1128
         CALL AGSRESET                                                    1129
         AG_IAXIS = 2                                                     1129
         AG_NDIV = 4                                                      1129
         AG_TITLE = 'DIVISION'                                            1129
         AG_EXNAME = 'SHAPE'                                              1129
         AG_PARLIST = 'IAXIS_NDIV'                                        1129
         CALL AXSHAPE                                                     1129
C    *                                                                    1129
C    Defining Hits: Z:.001:S Y:.001: X:.001: PTOT:16:(0,100), CX:10: CY:  1133
C    10: CZ:10: SLENG:16:(0,500), TOF:16:(0,1.E-6) STEP:.01: ELOSS:16:(0  1133
C    ,0.001)                                                              1133
         AG_TITLE = 'FGZD'                                                1133
         AG_EXNAME = 'HITS'                                               1133
         AG_TYPE=AG_MAGIC                                                 1133
         CALL AGSDETV('FGZD')                                             1133
C    *                                                                    1133
         AG_PARLIST='Z'                                                   1133
         AG_TITLE = 'DETH'                                                1133
         AG_NBITS = AG_MAGIC                                              1133
         AG_BIN=.001                                                      1133
         AG_OPTION='S'                                                    1133
         AG_FHMIN = AG_MAGIC                                              1133
         AG_FHMAX = AG_MAGIC                                              1133
         CALL AGSDETH (0)                                                 1133
C    *                                                                    1133
         AG_PARLIST='Y'                                                   1133
         AG_TITLE = 'DETH'                                                1133
         AG_NBITS = AG_MAGIC                                              1133
         AG_BIN=.001                                                      1133
         AG_FHMIN=AG_MAGIC                                                1133
         AG_FHMAX=AG_MAGIC                                                1133
         AG_OPTION='*'                                                    1133
         CALL AGSDETH (0)                                                 1133
C    *                                                                    1133
         AG_PARLIST='X'                                                   1133
         AG_TITLE = 'DETH'                                                1133
         AG_NBITS = AG_MAGIC                                              1133
         AG_BIN=.001                                                      1133
         AG_FHMIN=AG_MAGIC                                                1133
         AG_FHMAX=AG_MAGIC                                                1133
         AG_OPTION='*'                                                    1133
         CALL AGSDETH (0)                                                 1133
C    *                                                                    1133
         AG_PARLIST='PTOT'                                                1133
         AG_TITLE = 'DETH'                                                1133
         AG_NBITS = 16                                                    1133
         AG_BIN=AG_MAGIC                                                  1133
         AG_OPTION='*'                                                    1133
         AG_FHMIN = 0                                                     1133
         AG_FHMAX = 100                                                   1133
         CALL AGSDETH (0)                                                 1133
C    *                                                                    1133
         AG_PARLIST='CX'                                                  1133
         AG_TITLE = 'DETH'                                                1133
         AG_NBITS = 10                                                    1133
         AG_BIN=AG_MAGIC                                                  1133
         AG_FHMIN=AG_MAGIC                                                1133
         AG_FHMAX=AG_MAGIC                                                1133
         AG_OPTION='*'                                                    1133
         CALL AGSDETH (0)                                                 1133
C    *                                                                    1133
         AG_PARLIST='CY'                                                  1133
         AG_TITLE = 'DETH'                                                1133
         AG_NBITS = 10                                                    1133
         AG_BIN=AG_MAGIC                                                  1133
         AG_FHMIN=AG_MAGIC                                                1133
         AG_FHMAX=AG_MAGIC                                                1133
         AG_OPTION='*'                                                    1133
         CALL AGSDETH (0)                                                 1133
C    *                                                                    1133
         AG_PARLIST='CZ'                                                  1133
         AG_TITLE = 'DETH'                                                1133
         AG_NBITS = 10                                                    1133
         AG_BIN=AG_MAGIC                                                  1133
         AG_FHMIN=AG_MAGIC                                                1133
         AG_FHMAX=AG_MAGIC                                                1133
         AG_OPTION='*'                                                    1133
         CALL AGSDETH (0)                                                 1133
C    *                                                                    1133
         AG_PARLIST='SLENG'                                               1133
         AG_TITLE = 'DETH'                                                1133
         AG_NBITS = 16                                                    1133
         AG_BIN=AG_MAGIC                                                  1133
         AG_OPTION='*'                                                    1133
         AG_FHMIN = 0                                                     1133
         AG_FHMAX = 500                                                   1133
         CALL AGSDETH (0)                                                 1133
C    *                                                                    1133
         AG_PARLIST='TOF'                                                 1133
         AG_TITLE = 'DETH'                                                1133
         AG_NBITS = 16                                                    1133
         AG_BIN=AG_MAGIC                                                  1133
         AG_OPTION='*'                                                    1133
         AG_FHMIN = 0                                                     1133
         AG_FHMAX = 1.E-6                                                 1133
         CALL AGSDETH (0)                                                 1133
C    *                                                                    1133
         AG_PARLIST='STEP'                                                1133
         AG_TITLE = 'DETH'                                                1133
         AG_NBITS = AG_MAGIC                                              1133
         AG_BIN=.01                                                       1133
         AG_FHMIN=AG_MAGIC                                                1133
         AG_FHMAX=AG_MAGIC                                                1133
         AG_OPTION='*'                                                    1133
         CALL AGSDETH (0)                                                 1133
C    *                                                                    1133
         AG_PARLIST='ELOSS'                                               1133
         AG_TITLE = 'DETH'                                                1133
         AG_NBITS = 16                                                    1133
         AG_BIN=AG_MAGIC                                                  1133
         AG_OPTION='*'                                                    1133
         AG_FHMIN = 0                                                     1133
         AG_FHMAX = 0.001                                                 1133
         CALL AGSDETH (0)                                                 1133
C    *                                                                    1133
         CALL AGSDETU('FGZD')                                             1133
C    *                                                                    1133
5761  CALL AGSPOPB                                                        1134
      GO TO BLOCK_FGZD, (5760 ,5770)                                      1134
C End of Local Procedure                                                  1134
* -------------------------------------------------------------------
      END                                                                 1136
* $Log: fgtdgeo2.g,v $
* Revision 1.8  2008/12/01 23:44:03  perev
* Jan changes the length
*
* Revision 1.7  2008/10/28 17:59:24  perev
* mistype FGSC ==> FGSD
*
* Revision 1.6  2008/10/25 02:10:16  perev
* Remove debug prints
*
* Revision 1.5  2008/10/25 02:08:55  perev
* FGSD is FGSC of differen size
*
* Revision 1.4  2008/10/22 17:46:39  perev
* Jan: fix disk #9 sticking out of FGMO volume
*
* Revision 1.3  2008/10/16 02:57:24  perev
* HITS for FGSC recovery
*
* Revision 1.2  2008/10/13 00:24:06  perev
* upgr16 divisions of disks added
*
* Revision 1.1  2008/09/25 03:05:44  perev
* upgr16 (Jan)
* many of the volumes below were later changed
* The single triple GEM assembly in GEANT as follows, in this order:
* Front support disk:
C  * 0.25mm FR4, rho=1.91 g/cm3, composition as  G10                      1165
C  * 5.00mm Nomex,0.048 g/cm3                                             1166
C  * 0.25mm FR4, rho=1.91 g/cm3                                           1167
* Equivalent to 3GEMs+HV layer + readout layer:
C  * 3.0mm ArC02_70_30  (active volume)                                   1169
C  * 0.2mm Cu, rho=1.16 g/cm3  (stretch ~13 x and reduced density)        1170
C  * 0.3mm Kapton, rho=1.02g/cm3  ( use 0.7 density to account for holes  1171
C )                                                                       1171
C  * 0.2mm FR4, rho=1.19g/cm3, strech factor=1.6 ???                      1172
C  * 5.8mm ArC02_70_30  (passive drift volume)                            1173
C  * 2 rings of FR4 at inner/outer radii, 1 cm wide, thickness=9.5 cm     1174
C  * 4 bars at quadrant boundaries ????                                   1175
C  * the above values are old , only approximate                          1176
* Total depth=15.0mm
*
* Revision 1.4  2007/11/13 21:29:45  perev
* material ALKAP fixed
*
* Revision 1.3  2007/02/23 21:16:37  potekhin
* Replacing a dummy version (a placeholder) of the FGT (formerly known
* as IGT) code with a piece that supposedly works, and has 6 GEM disks.
* All the structures and volumes were renames to comply with the new
* naming convention (i.e. FGT, not IGT anymore).
*
* Revision 1.2  2007/02/13 20:41:03  potekhin
* Remove the hit declaration just to make sure
* there is no confusion downstream -- this version
* if for material balance puprpose only.
*
* Revision 1.1  2007/02/08 00:01:47  potekhin
* First cut of the "new" FGT which is identical to the previously
* used IGT. Will soon be replaced by a newer version.
*
