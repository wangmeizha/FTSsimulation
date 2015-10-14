C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE FGTDGEO3                                                 1
C FGTDGEO3 FORWARD GEM TRACKING DETECTOR FOR 2012 , 2013                  1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'FGTDGEO3'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'FORWARD GEM TRACKING DETECTOR FOR 2012 , 2013'/      1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C CREATED 12/20/2011                                                      1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 CREATED /'12/20/2011'/                                 2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C AUTHOR JAN BALEWSKI MIT, WEI-MING ZHANG KSU, WILLIE LEIGHT MIT (MATERI  2
C AL MIXES)                                                               2
      CHARACTER*40 AUTHOR /'JAN BALEWSKI MIT, WEI-MING ZHANG KSU, WILLIE  3
     * LEIGHT MIT (MATERIAL MIXES)'/                                      3
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  3
C - Advanced Geant interface                                              4
      CHARACTER*20 AG_MODULE,AG_TITLE,AG_EXNAME,AG_PARLIST,AG_CHDIR,      4
     *AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM                       4
      CHARACTER*4 AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,         4
     *AG_OPTION,AG_ORT,AG_MARK                                            4
      INTEGER AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,         4
     *AG_IMED,AG_IMAT,AG_IFIELD, AG_NLMAT,AG_IERROR,AG_NWBUF,AG_NPAR,     4
     *AG_ISTATUS,AG_IROT,AG_NBITS,AG_SERIAL, AG_ATTRIBUTE(6),AG_WORK,     4
     *AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,AG_JDU,AG_ORTI, AG_NDIV,    4
     *AG_IAXIS,AG_NDVMAX,AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,          4
     *AG_ENDSCR, AG_IDTYPE,AG_ISET,AG_IDET,AG_ISVOL,AG_LEVEL,AG_IRESER,   4
     *AG_IGEOM,AG_STANDALONE, AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,     4
     *AG_LDETU,AG_NPDIV,AG_NPDV,AG_NZ,AG_NCOPY, AG_IDEBU,AG_IGRAP,        4
     *AG_IHIST,AG_IMFLD,AG_ISIMU,AG_ENDSAVE,AG_CODE,AG_TRKTYP, AG_MODE,   4
     *AG_ECODE,AG_PDG,IPRIN, AG_ALPHAN,AG_ALPHAK(10),IGOR                 4
      REAL AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,AG_STMIN,     4
     *AG_DENS,AG_RADL,AG_ABSL, AG_THETAX,AG_PHIX,AG_THETAY,AG_PHIY,       4
     *AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ, AG_ALPHAL(10),        4
     *AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ,AG_TYPE, AG_DX, AG_DX1, AG_DX2,       4
     *AG_DY, AG_DY1, AG_DY2, AG_DMAXMS, AG_TWIST, AG_THET, AG_THE1,       4
     *AG_THE2, AG_PHI, AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2,       4
     *AG_RMIN, AG_RMAX, AG_RMN, AG_RMX, AG_ZI , AG_RMN1, AG_RMN2,         4
     *AG_RMX1, AG_RMX2, AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,     4
     *AG_DPHI, AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,       4
     *AG_STEP, AG_C0, AG_PAR, AG_AA, AG_ZZ, AG_WW, AG_LX, AG_LY, AG_LZ,   4
     *AG_HX, AG_HY, AG_HZ, AG_P1, AG_P2, AG_STACK,AG_UBUF,AG_XHMAX,       4
     *AG_YHMAX,AG_ZHMAX,AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN, AG_CHARGE,     4
     *AG_MASS,AG_TLIFE,AG_BRATIO                                          4
      PARAMETER (AG_LSTACK=130,AG_NWUHIT=10,AG_NWUVOL=3,AG_MAGIC=         4
     *-696969,AG_LDETU=250)                                               4
      COMMON/AGCGLOB/ AG_MODULE,AG_CHDIR,AG_LEVEL,AG_IDTYPE,AG_IERROR,    4
     *AG_STANDALONE,IPRIN, AG_IPRIN,AG_IGEOM,AG_IDEBU,AG_IGRAP,AG_IHIST,  4
     *AG_IMFLD,AG_ISIMU                                                   4
C Inherited variables a: saved during internal calls                      4
C b: reset at the entry in a block and then saved                         4
      COMMON/AGCPARA/ AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,        4
     *AG_ISHAPE, AG_IMED, AG_IMAT, AG_IFIELD, AG_FIELDM, AG_TMAXFD,       4
     *AG_STEMAX, AG_DEEMAX, AG_EPSIL, AG_STMIN, AG_DENS, AG_RADL,         4
     *AG_ABSL, AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1, AG_DY2, AG_RMN1,     4
     *AG_RMN2, AG_RMX1, AG_RMX2, AG_THET, AG_THE1, AG_THE2, AG_PHI,       4
     *AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2, AG_H1, AG_BL1,         4
     *AG_TL1, AG_H2, AG_BL2, AG_TL2, AG_RMIN, AG_RMAX, AG_DPHI,           4
     *AG_NPDIV, AG_NZ, AG_DZ, AG_TWIS, AG_LX, AG_LY, AG_LZ, AG_HX,        4
     *AG_HY, AG_HZ, AG_A, AG_ZA, AG_W, AG_NLMAT, AG_WORK, AG_SEEN,        4
     *AG_LSTY, AG_LWID, AG_COLO, AG_FILL, AG_SERIAL, AG_ISVOL,            4
     *AG_ISTATUS, AG_ZI(16), AG_RMN(16), AG_RMX(16), AG_VOLUME,           4
     *AG_MOTHER, AG_SHAPE, AG_CNICK, AG_ENDSAVE, AG_RESET1, AG_THETAX,    4
     *AG_PHIX,AG_THETAY,AG_PHIY,AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,      4
     *AG_ALFAZ, AG_X,AG_Y,AG_Z, AG_STEP,AG_C0,AG_NDIV,AG_IAXIS,           4
     *AG_NDVMAX,AG_ORTI,AG_NCOPY, AG_ALPHAN,AG_ALPHAK,AG_ALPHAL,          4
     *AG_RESET2, AG_KONLY, AG_ORT, AG_MARK                                4
C local variables valid inside same block                                 4
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       4
     *AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  4
     *AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         4
     *AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    4
     *AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          4
     *AG_OPTION                                                           4
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,        4
     *AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                             4
      COMMON/AGCSTAC/ AG_STACK(AG_LSTACK,15)                              4
C temporarly                                                              4
      EQUIVALENCE (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),           4
     *(AG_ALPHAX,AG_ALFAX), (AG_ALPHAY,AG_ALFAY),(AG_ALPHAZ,AG_ALFAZ),    4
     *(AG_TWIST,AG_TWIS),(AG_NPDIV,AG_NPDV),  (AG_P1,AG_HX),(AG_P2,       4
     *AG_HY)                                                              4
      REAL MIL_P,INCH_P                                                   4
      PARAMETER (INCH_P=2.54,MIL_P=0.00254)                               4
C - GEANT description of logical units                                    4
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            4
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      4
      COMMON/GCMAIL/ CHMAIL                                               4
      CHARACTER*132 CHMAIL                                                4
C - Natural constants (or considered as such)                             4
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   4
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        4
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       4
      INTEGER BLOCK_FGTM                                                  4
      INTEGER BLOCK_FGTD                                                  10
      INTEGER BLOCK_FGTQ                                                  10
      INTEGER BLOCK_FGTH                                                  10
      INTEGER BLOCK_FGVN                                                  10
      INTEGER BLOCK_FGZC                                                  10
      INTEGER BLOCK_FGVG                                                  10
      INTEGER BLOCK_FGVR                                                  10
      INTEGER BLOCK_FGVE                                                  10
      INTEGER BLOCK_FGQA                                                  10
      INTEGER BLOCK_FGQB                                                  10
      INTEGER BLOCK_FGQC                                                  10
      INTEGER BLOCK_FGQD                                                  10
      INTEGER BLOCK_FGQE                                                  10
      INTEGER BLOCK_FGQF                                                  10
      INTEGER BLOCK_FGXA                                                  10
      INTEGER BLOCK_FGXB                                                  10
      INTEGER BLOCK_FGXC                                                  10
      INTEGER BLOCK_FGXD                                                  10
      INTEGER BLOCK_FGWA                                                  10
      INTEGER BLOCK_FGWB                                                  10
      INTEGER BLOCK_FGWC                                                  10
      INTEGER BLOCK_FGWD                                                  10
      INTEGER BLOCK_FGWE                                                  10
      INTEGER BLOCK_FGCT                                                  10
      INTEGER BLOCK_FGCN                                                  10
      INTEGER BLOCK_FGCM                                                  10
      REAL *4 COOLTUBEINR/10.25/                                          11
      REAL *4 COOLTUBEDZ/150/                                             13
      REAL *4 DISKINR/10.35/                                              15
      REAL *4 DISKOUTR/39.4/                                              17
      REAL *4 FLATOUTR/37.0/                                              19
      REAL *4 FLATPERPANG/31.0/                                           21
      REAL *4 QUADTILTANG/-15.0/                                          23
      REAL *4 GRIDARCBIGR/29.15/                                          25
      REAL *4 GRIDARCSMALLR/20.63/                                        27
      REAL *4 GRIDRAYLONGPHI/60./                                         29
      REAL *4 GRIDRAYSHORTPHI/30./                                        31
      REAL *4 GRIDWIDTH/0.2/                                              33
      REAL *4 FRAMEWIDTH/1.15/                                            35
      REAL *4 FRAMEXYOFF/0.05/                                            37
      REAL *4 FRAMEDZ/0.198/                                              39
      REAL *4 VOLNOMEXDZ/1.288/                                           41
      REAL *4 VOLSENSDZ/0.312/                                            43
      REAL *4 VOLGEMDZ/0.612/                                             45
      REAL *4 VOLREADDZ/0.246/                                            47
      REAL *4 VOLELECDZ/4.3/                                              49
      REAL *4 ELECWIDTH/0.16/                                             51
      REAL *4 ELECADY/1.4/                                                53
      REAL *4 ELECCDY/4.5/                                                55
      REAL *4 ELECDANG/9./                                                57
      REAL *4 ELECELEN/22.5/                                              59
      REAL CENTERZ,FGTLENZ,DISKLENZ, FLATANG1,FLATANG2,QUAD,DISK, XX,     62
     *DISKZ, K,ZZFGZ, XXFGX,XXFGQ, XXFGZ                                  62
C -                                                                       64
      REAL FGGG_FGSTCONFIG                                                64
      EQUIVALENCE (BANK_FGGG(3),FGGG_FGSTCONFIG,RE_FGGG_FGSTCONFIG),      64
     *(NAMES_FGGG(1),NFGGG_FGSTCONFIG)                                    64
      EQUIVALENCE (COMMEN_FGGG(1),CFGGG_FGSTCONFIG),(MAP_FGGG(1,1),       64
     *MFGGG_FGSTCONFIG)                                                   64
      CHARACTER CFGGG_FGSTCONFIG*40/' '/                                  64
      CHARACTER NFGGG_FGSTCONFIG*12/'R.FGSTCONFIG'/                       64
      INTEGER MFGGG_FGSTCONFIG(2)/1,1/                                    64
      REAL RE_FGGG_FGSTCONFIG                                             64
C -                                                                       64
      INTEGER LEN_FGGG,LLEN_FGGG,LINK_FGGG                                64
      PARAMETER (LEN_FGGG=3)                                              64
      PARAMETER (LLEN_FGGG=1)                                             64
      INTEGER FLAG_FGGG/0/,MAP_FGGG(2,LLEN_FGGG),IDNUM_FGGG(10)           64
      CHARACTER NAMES_FGGG(LLEN_FGGG)*12,COMMEN_FGGG(LLEN_FGGG)*40        64
      DATA IDNUM_FGGG/10*0/                                               64
      REAL BANK_FGGG(LEN_FGGG)                                            64
      CHARACTER*60 BPATH_FGGG/' '/                                        64
      SAVE FLAG_FGGG,MAP_FGGG,IDNUM_FGGG,NAMES_FGGG,COMMEN_FGGG,          64
     *BPATH_FGGG                                                          64
      DATA LINK_FGGG/0/,BANK_FGGG/LEN_FGGG*0/                             64
C *                                                                       64
C -                                                                       65
      REAL FGST_CONFIG                                                    65
      EQUIVALENCE (BANK_FGST(3),FGST_CONFIG,RE_FGST_CONFIG),              65
     *(NAMES_FGST(1),NFGST_CONFIG)                                        65
      EQUIVALENCE (COMMEN_FGST(1),CFGST_CONFIG),(MAP_FGST(1,1),           65
     *MFGST_CONFIG)                                                       65
      CHARACTER CFGST_CONFIG*40/' '/                                      65
      CHARACTER NFGST_CONFIG*12/'R.CONFIG'/                               65
      INTEGER MFGST_CONFIG(2)/1,1/                                        65
      REAL RE_FGST_CONFIG                                                 65
C -                                                                       65
      INTEGER FGST_NDISK                                                  65
      EQUIVALENCE (BANK_FGST(4),FGST_NDISK,RE_FGST_NDISK),                65
     *(NAMES_FGST(2),NFGST_NDISK)                                         65
      EQUIVALENCE (COMMEN_FGST(2),CFGST_NDISK),(MAP_FGST(1,2),            65
     *MFGST_NDISK)                                                        65
      CHARACTER CFGST_NDISK*40/' '/                                       65
      CHARACTER NFGST_NDISK*12/'I.NDISK'/                                 65
      INTEGER MFGST_NDISK(2)/1,1/                                         65
      REAL RE_FGST_NDISK                                                  65
C -                                                                       65
      INTEGER FGST_NQUAD                                                  65
      EQUIVALENCE (BANK_FGST(5),FGST_NQUAD,RE_FGST_NQUAD),                65
     *(NAMES_FGST(3),NFGST_NQUAD)                                         65
      EQUIVALENCE (COMMEN_FGST(3),CFGST_NQUAD),(MAP_FGST(1,3),            65
     *MFGST_NQUAD)                                                        65
      CHARACTER CFGST_NQUAD*40/' '/                                       65
      CHARACTER NFGST_NQUAD*12/'I.NQUAD'/                                 65
      INTEGER MFGST_NQUAD(2)/1,1/                                         65
      REAL RE_FGST_NQUAD                                                  65
C -                                                                       65
      REAL FGST_ZDISCA(6)                                                 65
      EQUIVALENCE (BANK_FGST(6),FGST_ZDISCA,RE_FGST_ZDISCA),              65
     *(NAMES_FGST(4),NFGST_ZDISCA)                                        65
      EQUIVALENCE (COMMEN_FGST(4),CFGST_ZDISCA),(MAP_FGST(1,4),           65
     *MFGST_ZDISCA)                                                       65
      CHARACTER CFGST_ZDISCA*40/' '/                                      65
      CHARACTER NFGST_ZDISCA*12/'R.ZDISCA'/                               65
      INTEGER MFGST_ZDISCA(2)/6,1/                                        65
      REAL RE_FGST_ZDISCA                                                 65
C -                                                                       65
      REAL FGST_COOL1STSHIFT                                              65
      EQUIVALENCE (BANK_FGST(12),FGST_COOL1STSHIFT,                       65
     *RE_FGST_COOL1STSHIFT),(NAMES_FGST(5),NFGST_COOL1STSHIFT)            65
      EQUIVALENCE (COMMEN_FGST(5),CFGST_COOL1STSHIFT),(MAP_FGST(1,5),     65
     *MFGST_COOL1STSHIFT)                                                 65
      CHARACTER CFGST_COOL1STSHIFT*40/' '/                                65
      CHARACTER NFGST_COOL1STSHIFT*12/'R.COOL1STSHIFT'/                   65
      INTEGER MFGST_COOL1STSHIFT(2)/1,1/                                  65
      REAL RE_FGST_COOL1STSHIFT                                           65
C -                                                                       65
      INTEGER LEN_FGST,LLEN_FGST,LINK_FGST                                65
      PARAMETER (LEN_FGST=12)                                             65
      PARAMETER (LLEN_FGST=5)                                             65
      INTEGER FLAG_FGST/0/,MAP_FGST(2,LLEN_FGST),IDNUM_FGST(10)           65
      CHARACTER NAMES_FGST(LLEN_FGST)*12,COMMEN_FGST(LLEN_FGST)*40        65
      DATA IDNUM_FGST/10*0/                                               65
      REAL BANK_FGST(LEN_FGST)                                            65
      CHARACTER*60 BPATH_FGST/' '/                                        65
      SAVE FLAG_FGST,MAP_FGST,IDNUM_FGST,NAMES_FGST,COMMEN_FGST,          65
     *BPATH_FGST                                                          65
      DATA LINK_FGST/0/,BANK_FGST/LEN_FGST*0/                             65
C *                                                                       65
C *                                                                       66
      CALL AGSBEGM(MODULE,IPRIN)                                          66
      IF (IPRIN.LT.0) RETURN                                              66
C Filling bank FGGG - FGT GEOMETRY CONTROL STRUCTURE                      66
      BANK_TITLE='FGT GEOMETRY CONTROL STRUCTURE'                         66
      BPATH_FGGG='FGGG*'                                                  66
      IDNUM_FGGG(1)=0                                                     67
C SELECTION OF NO. OF DISKS AND QUADRANTS                                 67
      FGGG_FGSTCONFIG=1.0                                                 67
      CFGGG_FGSTCONFIG='SELECTION OF NO. OF DISKS AND QUADRANTS'          67
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FGGG',    68
     *BPATH_FGGG,IDNUM_FGGG,1, MAP_FGGG,NAMES_FGGG,COMMEN_FGGG,           68
     *BANK_FGGG,LEN_FGGG,LLEN_FGGG,LINK_FGGG,FLAG_FGGG)                   68
C *                                                                       68
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FGGG',    68
     *BPATH_FGGG,IDNUM_FGGG,1, MAP_FGGG,NAMES_FGGG,COMMEN_FGGG,           68
     *BANK_FGGG,LEN_FGGG,LLEN_FGGG,LINK_FGGG,FLAG_FGGG)                   68
C *                                                                       69
      CALL AGSBEGM(MODULE,IPRIN)                                          69
      IF (IPRIN.LT.0) RETURN                                              69
C Filling bank FGST - FGT GEOMETRY FOR YEAR 2012                          69
      BANK_TITLE='FGT GEOMETRY FOR YEAR 2012'                             69
      BPATH_FGST='FGST*'                                                  69
      IDNUM_FGST(1)=0                                                     70
C VERSIONING OF THE FGST GEOMETRY DATA                                    70
      FGST_CONFIG=1.0                                                     70
      CFGST_CONFIG='VERSIONING OF THE FGST GEOMETRY DATA'                 70
C NUMBER OF DISKS                                                         71
      FGST_NDISK=6                                                        71
      CFGST_NDISK='NUMBER OF DISKS'                                       71
C NUMBER QUADRANTS IN A DISKS                                             72
      FGST_NQUAD=4                                                        72
      CFGST_NQUAD='NUMBER QUADRANTS IN A DISKS'                           72
C Z-CENTER OF DISK SENSTIVE VOLUME FROM CENTER IN STAR , AS MEASURED IN   74
C 2011-1                                                                  74
      FGST_ZDISCA(1)=67.3990                                              74
      FGST_ZDISCA(2)=77.8765                                              74
      FGST_ZDISCA(3)=87.0840                                              74
      FGST_ZDISCA(4)=97.4821                                              74
      FGST_ZDISCA(5)=108.9121                                             74
      FGST_ZDISCA(6)=118.9927                                             74
      CFGST_ZDISCA='Z-CENTER OF DISK SENSTIVE VOLUME FROM CENTER IN STAR  74
     * , AS MEASURED IN 2011-1'                                           74
C A GLOBAL OFFSET TO ACCOUNT FOR ~1CM SHIFT OF COOLING RING               75
      FGST_COOL1STSHIFT=0                                                 75
      CFGST_COOL1STSHIFT='A GLOBAL OFFSET TO ACCOUNT FOR ~1CM SHIFT OF C  75
     *OOLING RING'                                                        75
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FGST',    76
     *BPATH_FGST,IDNUM_FGST,1, MAP_FGST,NAMES_FGST,COMMEN_FGST,           76
     *BANK_FGST,LEN_FGST,LLEN_FGST,LINK_FGST,FLAG_FGST)                   76
C *                                                                       76
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FGST',    76
     *BPATH_FGST,IDNUM_FGST,1, MAP_FGST,NAMES_FGST,COMMEN_FGST,           76
     *BANK_FGST,LEN_FGST,LLEN_FGST,LINK_FGST,FLAG_FGST)                   76
C *                                                                       77
      CALL AGSBEGM(MODULE,IPRIN)                                          77
      IF (IPRIN.LT.0) RETURN                                              77
C Filling bank FGST - FGT GEOMETRY FOR YEAR 2012 WITH 1CM OFFSET ACCOUNT  77
C ED                                                                      77
      BANK_TITLE='FGT GEOMETRY FOR YEAR 2012 WITH 1CM OFFSET ACCOUNTED'   77
      BPATH_FGST='FGST*'                                                  77
      IDNUM_FGST(1)=0                                                     78
C REVISED FGT GEOMETRY FOR Y2012                                          78
      FGST_CONFIG=1.1                                                     78
      CFGST_CONFIG='REVISED FGT GEOMETRY FOR Y2012'                       78
C A GLOBAL OFFSET TO ACCOUNT FOR ~1CM SHIFT OF COOLING RING               79
      FGST_COOL1STSHIFT=1.0                                               79
      CFGST_COOL1STSHIFT='A GLOBAL OFFSET TO ACCOUNT FOR ~1CM SHIFT OF C  79
     *OOLING RING'                                                        79
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FGST',    80
     *BPATH_FGST,IDNUM_FGST,1, MAP_FGST,NAMES_FGST,COMMEN_FGST,           80
     *BANK_FGST,LEN_FGST,LLEN_FGST,LINK_FGST,FLAG_FGST)                   80
C *                                                                       80
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FGST',    80
     *BPATH_FGST,IDNUM_FGST,1, MAP_FGST,NAMES_FGST,COMMEN_FGST,           80
     *BANK_FGST,LEN_FGST,LLEN_FGST,LINK_FGST,FLAG_FGST)                   80
C *                                                                       81
      CALL AGSBEGM(MODULE,IPRIN)                                          81
      IF (IPRIN.LT.0) RETURN                                              81
C Filling bank FGST - FGT GEOMETRY WITH SIX FULL DISKS Y2013. USING Y201  81
C 2 Z-POSITIONS.                                                          81
      BANK_TITLE='FGT GEOMETRY WITH SIX FULL DISKS Y2013. USING Y2012 Z-  81
     *POSITIONS.'                                                         81
      BPATH_FGST='FGST*'                                                  81
      IDNUM_FGST(1)=0                                                     82
C VERSIONING OF THE FGST GEOMETRY DATA                                    82
      FGST_CONFIG=2.0                                                     82
      CFGST_CONFIG='VERSIONING OF THE FGST GEOMETRY DATA'                 82
C NUMBER OF DISKS                                                         83
      FGST_NDISK=6                                                        83
      CFGST_NDISK='NUMBER OF DISKS'                                       83
C NUMBER QUADRANTS IN A DISKS                                             84
      FGST_NQUAD=4                                                        84
      CFGST_NQUAD='NUMBER QUADRANTS IN A DISKS'                           84
C Z-CENTER OF DISK SENSTIVE VOLUME FROM CENTER IN STAR , AS MEASURED IN   86
C 2011-1                                                                  86
      FGST_ZDISCA(1)=67.3990                                              86
      FGST_ZDISCA(2)=77.8765                                              86
      FGST_ZDISCA(3)=87.0840                                              86
      FGST_ZDISCA(4)=97.4821                                              86
      FGST_ZDISCA(5)=108.9121                                             86
      FGST_ZDISCA(6)=118.9927                                             86
      CFGST_ZDISCA='Z-CENTER OF DISK SENSTIVE VOLUME FROM CENTER IN STAR  86
     * , AS MEASURED IN 2011-1'                                           86
C A GLOBAL OFFSET TO ACCOUNT FOR 1CM SHIFT OF COOLING RING                87
      FGST_COOL1STSHIFT=1.0                                               87
      CFGST_COOL1STSHIFT='A GLOBAL OFFSET TO ACCOUNT FOR 1CM SHIFT OF CO  87
     *OLING RING'                                                         87
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FGST',    88
     *BPATH_FGST,IDNUM_FGST,1, MAP_FGST,NAMES_FGST,COMMEN_FGST,           88
     *BANK_FGST,LEN_FGST,LLEN_FGST,LINK_FGST,FLAG_FGST)                   88
C *                                                                       88
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FGST',    88
     *BPATH_FGST,IDNUM_FGST,1, MAP_FGST,NAMES_FGST,COMMEN_FGST,           88
     *BANK_FGST,LEN_FGST,LLEN_FGST,LINK_FGST,FLAG_FGST)                   88
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.281 <===                     89
      CALL AGSRESET                                                       89
      AG_A = 28.08                                                        89
      AG_Z = 14                                                           89
      AG_W = 0.281                                                        89
      AG_TITLE = 'SI'                                                     89
      AG_EXNAME = 'COMPONENT'                                             89
      AG_PARLIST = 'A_Z_W'                                                89
      CALL AXCOMPONENT                                                    89
C *                                                                       89
C Geant COMPONENT : O ===> A=16 Z=8 W=0.467 <===                          89
      CALL AGSRESET                                                       90
      AG_A = 16                                                           90
      AG_Z = 8                                                            90
      AG_W = 0.467                                                        90
      AG_TITLE = 'O'                                                      90
      AG_EXNAME = 'COMPONENT'                                             90
      AG_PARLIST = 'A_Z_W'                                                90
      CALL AXCOMPONENT                                                    90
C *                                                                       90
C Geant COMPONENT : C ===> A=12 Z=6 W=0.220 <===                          90
      CALL AGSRESET                                                       91
      AG_A = 12                                                           91
      AG_Z = 6                                                            91
      AG_W = 0.220                                                        91
      AG_TITLE = 'C'                                                      91
      AG_EXNAME = 'COMPONENT'                                             91
      AG_PARLIST = 'A_Z_W'                                                91
      CALL AXCOMPONENT                                                    91
C *                                                                       91
C Geant COMPONENT : H ===> A=1 Z=1 W=0.032 <===                           91
      CALL AGSRESET                                                       92
      AG_A = 1                                                            92
      AG_Z = 1                                                            92
      AG_W = 0.032                                                        92
      AG_TITLE = 'H'                                                      92
      AG_EXNAME = 'COMPONENT'                                             92
      AG_PARLIST = 'A_Z_W'                                                92
      CALL AXCOMPONENT                                                    92
C *                                                                       92
C Geant MIXTURE : FR4 ===> DENS=1.80 <===                                 92
      CALL AGSRESET                                                       93
      AG_DENS = 1.80                                                      93
      AG_TITLE = 'FR4'                                                    93
      AG_EXNAME = 'MIXTURE'                                               93
      AG_PARLIST = 'DENS'                                                 93
      CALL AXMIXTURE                                                      93
C *                                                                       93
C Geant COMPONENT : O ===> A=16 Z=8 W=0.062 <===                          93
      CALL AGSRESET                                                       94
      AG_A = 16                                                           94
      AG_Z = 8                                                            94
      AG_W = 0.062                                                        94
      AG_TITLE = 'O'                                                      94
      AG_EXNAME = 'COMPONENT'                                             94
      AG_PARLIST = 'A_Z_W'                                                94
      CALL AXCOMPONENT                                                    94
C *                                                                       94
C Geant COMPONENT : C ===> A=12 Z=6 W=0.892 <===                          94
      CALL AGSRESET                                                       95
      AG_A = 12                                                           95
      AG_Z = 6                                                            95
      AG_W = 0.892                                                        95
      AG_TITLE = 'C'                                                      95
      AG_EXNAME = 'COMPONENT'                                             95
      AG_PARLIST = 'A_Z_W'                                                95
      CALL AXCOMPONENT                                                    95
C *                                                                       95
C Geant COMPONENT : H ===> A=1 Z=1 W=0.019 <===                           95
      CALL AGSRESET                                                       96
      AG_A = 1                                                            96
      AG_Z = 1                                                            96
      AG_W = 0.019                                                        96
      AG_TITLE = 'H'                                                      96
      AG_EXNAME = 'COMPONENT'                                             96
      AG_PARLIST = 'A_Z_W'                                                96
      CALL AXCOMPONENT                                                    96
C *                                                                       96
C Geant COMPONENT : CL ===> A=35.5 Z=17 W=0.027 <===                      96
      CALL AGSRESET                                                       97
      AG_A = 35.5                                                         97
      AG_Z = 17                                                           97
      AG_W = 0.027                                                        97
      AG_TITLE = 'CL'                                                     97
      AG_EXNAME = 'COMPONENT'                                             97
      AG_PARLIST = 'A_Z_W'                                                97
      CALL AXCOMPONENT                                                    97
C *                                                                       97
C Geant MIXTURE : CFRPMIX ===> DENS=1.78 <===                             97
      CALL AGSRESET                                                       98
      AG_DENS = 1.78                                                      98
      AG_TITLE = 'CFRPMIX'                                                98
      AG_EXNAME = 'MIXTURE'                                               98
      AG_PARLIST = 'DENS'                                                 98
      CALL AXMIXTURE                                                      98
C *                                                                       98
C Geant COMPONENT : O ===> A=16 Z=8 W=0.142 <===                          98
      CALL AGSRESET                                                       99
      AG_A = 16                                                           99
      AG_Z = 8                                                            99
      AG_W = 0.142                                                        99
      AG_TITLE = 'O'                                                      99
      AG_EXNAME = 'COMPONENT'                                             99
      AG_PARLIST = 'A_Z_W'                                                99
      CALL AXCOMPONENT                                                    99
C *                                                                       99
C Geant COMPONENT : C ===> A=12 Z=6 W=0.637 <===                          99
      CALL AGSRESET                                                       100
      AG_A = 12                                                           100
      AG_Z = 6                                                            100
      AG_W = 0.637                                                        100
      AG_TITLE = 'C'                                                      100
      AG_EXNAME = 'COMPONENT'                                             100
      AG_PARLIST = 'A_Z_W'                                                100
      CALL AXCOMPONENT                                                    100
C *                                                                       100
C Geant COMPONENT : H ===> A=1 Z=1 W=0.097 <===                           100
      CALL AGSRESET                                                       101
      AG_A = 1                                                            101
      AG_Z = 1                                                            101
      AG_W = 0.097                                                        101
      AG_TITLE = 'H'                                                      101
      AG_EXNAME = 'COMPONENT'                                             101
      AG_PARLIST = 'A_Z_W'                                                101
      CALL AXCOMPONENT                                                    101
C *                                                                       101
C Geant COMPONENT : N ===> A=14 Z=7 W=0.124 <===                          101
      CALL AGSRESET                                                       102
      AG_A = 14                                                           102
      AG_Z = 7                                                            102
      AG_W = 0.124                                                        102
      AG_TITLE = 'N'                                                      102
      AG_EXNAME = 'COMPONENT'                                             102
      AG_PARLIST = 'A_Z_W'                                                102
      CALL AXCOMPONENT                                                    102
C *                                                                       102
C Geant MIXTURE : NYLON ===> DENS=1.15 <===                               102
      CALL AGSRESET                                                       103
      AG_DENS = 1.15                                                      103
      AG_TITLE = 'NYLON'                                                  103
      AG_EXNAME = 'MIXTURE'                                               103
      AG_PARLIST = 'DENS'                                                 103
      CALL AXMIXTURE                                                      103
C *                                                                       103
C Geant COMPONENT : O ===> A=16 Z=8 W=0.129 <===                          103
      CALL AGSRESET                                                       104
      AG_A = 16                                                           104
      AG_Z = 8                                                            104
      AG_W = 0.129                                                        104
      AG_TITLE = 'O'                                                      104
      AG_EXNAME = 'COMPONENT'                                             104
      AG_PARLIST = 'A_Z_W'                                                104
      CALL AXCOMPONENT                                                    104
C *                                                                       104
C Geant COMPONENT : C ===> A=12 Z=6 W=0.579 <===                          104
      CALL AGSRESET                                                       105
      AG_A = 12                                                           105
      AG_Z = 6                                                            105
      AG_W = 0.579                                                        105
      AG_TITLE = 'C'                                                      105
      AG_EXNAME = 'COMPONENT'                                             105
      AG_PARLIST = 'A_Z_W'                                                105
      CALL AXCOMPONENT                                                    105
C *                                                                       105
C Geant COMPONENT : H ===> A=1 Z=1 W=0.088 <===                           105
      CALL AGSRESET                                                       106
      AG_A = 1                                                            106
      AG_Z = 1                                                            106
      AG_W = 0.088                                                        106
      AG_TITLE = 'H'                                                      106
      AG_EXNAME = 'COMPONENT'                                             106
      AG_PARLIST = 'A_Z_W'                                                106
      CALL AXCOMPONENT                                                    106
C *                                                                       106
C Geant COMPONENT : N ===> A=14 Z=7 W=0.112 <===                          106
      CALL AGSRESET                                                       107
      AG_A = 14                                                           107
      AG_Z = 7                                                            107
      AG_W = 0.112                                                        107
      AG_TITLE = 'N'                                                      107
      AG_EXNAME = 'COMPONENT'                                             107
      AG_PARLIST = 'A_Z_W'                                                107
      CALL AXCOMPONENT                                                    107
C *                                                                       107
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.092 <===                        107
      CALL AGSRESET                                                       108
      AG_A = 27                                                           108
      AG_Z = 13                                                           108
      AG_W = 0.092                                                        108
      AG_TITLE = 'AL'                                                     108
      AG_EXNAME = 'COMPONENT'                                             108
      AG_PARLIST = 'A_Z_W'                                                108
      CALL AXCOMPONENT                                                    108
C *                                                                       108
C Geant MIXTURE : COOLMIX ===> DENS=1.214 <===                            108
      CALL AGSRESET                                                       109
      AG_DENS = 1.214                                                     109
      AG_TITLE = 'COOLMIX'                                                109
      AG_EXNAME = 'MIXTURE'                                               109
      AG_PARLIST = 'DENS'                                                 109
      CALL AXMIXTURE                                                      109
C *                                                                       109
C Geant COMPONENT : O ===> A=16 Z=8 W=0.090 <===                          109
      CALL AGSRESET                                                       110
      AG_A = 16                                                           110
      AG_Z = 8                                                            110
      AG_W = 0.090                                                        110
      AG_TITLE = 'O'                                                      110
      AG_EXNAME = 'COMPONENT'                                             110
      AG_PARLIST = 'A_Z_W'                                                110
      CALL AXCOMPONENT                                                    110
C *                                                                       110
C Geant COMPONENT : C ===> A=12 Z=6 W=0.796 <===                          110
      CALL AGSRESET                                                       111
      AG_A = 12                                                           111
      AG_Z = 6                                                            111
      AG_W = 0.796                                                        111
      AG_TITLE = 'C'                                                      111
      AG_EXNAME = 'COMPONENT'                                             111
      AG_PARLIST = 'A_Z_W'                                                111
      CALL AXCOMPONENT                                                    111
C *                                                                       111
C Geant COMPONENT : H ===> A=1 Z=1 W=0.023 <===                           111
      CALL AGSRESET                                                       112
      AG_A = 1                                                            112
      AG_Z = 1                                                            112
      AG_W = 0.023                                                        112
      AG_TITLE = 'H'                                                      112
      AG_EXNAME = 'COMPONENT'                                             112
      AG_PARLIST = 'A_Z_W'                                                112
      CALL AXCOMPONENT                                                    112
C *                                                                       112
C Geant COMPONENT : CL ===> A=35.5 Z=17 W=0.019 <===                      112
      CALL AGSRESET                                                       113
      AG_A = 35.5                                                         113
      AG_Z = 17                                                           113
      AG_W = 0.019                                                        113
      AG_TITLE = 'CL'                                                     113
      AG_EXNAME = 'COMPONENT'                                             113
      AG_PARLIST = 'A_Z_W'                                                113
      CALL AXCOMPONENT                                                    113
C *                                                                       113
C Geant COMPONENT : N ===> A=14 Z=7 W=0.025 <===                          113
      CALL AGSRESET                                                       114
      AG_A = 14                                                           114
      AG_Z = 7                                                            114
      AG_W = 0.025                                                        114
      AG_TITLE = 'N'                                                      114
      AG_EXNAME = 'COMPONENT'                                             114
      AG_PARLIST = 'A_Z_W'                                                114
      CALL AXCOMPONENT                                                    114
C *                                                                       114
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.002 <===                        114
      CALL AGSRESET                                                       115
      AG_A = 27                                                           115
      AG_Z = 13                                                           115
      AG_W = 0.002                                                        115
      AG_TITLE = 'AL'                                                     115
      AG_EXNAME = 'COMPONENT'                                             115
      AG_PARLIST = 'A_Z_W'                                                115
      CALL AXCOMPONENT                                                    115
C *                                                                       115
C Geant COMPONENT : CU ===> A=63.5 Z=29 W=0.039 <===                      115
      CALL AGSRESET                                                       116
      AG_A = 63.5                                                         116
      AG_Z = 29                                                           116
      AG_W = 0.039                                                        116
      AG_TITLE = 'CU'                                                     116
      AG_EXNAME = 'COMPONENT'                                             116
      AG_PARLIST = 'A_Z_W'                                                116
      CALL AXCOMPONENT                                                    116
C *                                                                       116
C Geant COMPONENT : AR ===> A=39.9 Z=18 W=0.006 <===                      116
      CALL AGSRESET                                                       117
      AG_A = 39.9                                                         117
      AG_Z = 18                                                           117
      AG_W = 0.006                                                        117
      AG_TITLE = 'AR'                                                     117
      AG_EXNAME = 'COMPONENT'                                             117
      AG_PARLIST = 'A_Z_W'                                                117
      CALL AXCOMPONENT                                                    117
C *                                                                       117
C Geant MIXTURE : NOMEXMIX ===> DENS=0.090 <===                           117
      CALL AGSRESET                                                       118
      AG_DENS = 0.090                                                     118
      AG_TITLE = 'NOMEXMIX'                                               118
      AG_EXNAME = 'MIXTURE'                                               118
      AG_PARLIST = 'DENS'                                                 118
      CALL AXMIXTURE                                                      118
C *                                                                       118
C Geant COMPONENT : O ===> A=16 Z=8 W=0.095 <===                          118
      CALL AGSRESET                                                       119
      AG_A = 16                                                           119
      AG_Z = 8                                                            119
      AG_W = 0.095                                                        119
      AG_TITLE = 'O'                                                      119
      AG_EXNAME = 'COMPONENT'                                             119
      AG_PARLIST = 'A_Z_W'                                                119
      CALL AXCOMPONENT                                                    119
C *                                                                       119
C Geant COMPONENT : C ===> A=12 Z=6 W=0.301 <===                          119
      CALL AGSRESET                                                       120
      AG_A = 12                                                           120
      AG_Z = 6                                                            120
      AG_W = 0.301                                                        120
      AG_TITLE = 'C'                                                      120
      AG_EXNAME = 'COMPONENT'                                             120
      AG_PARLIST = 'A_Z_W'                                                120
      CALL AXCOMPONENT                                                    120
C *                                                                       120
C Geant COMPONENT : H ===> A=1 Z=1 W=0.011 <===                           120
      CALL AGSRESET                                                       121
      AG_A = 1                                                            121
      AG_Z = 1                                                            121
      AG_W = 0.011                                                        121
      AG_TITLE = 'H'                                                      121
      AG_EXNAME = 'COMPONENT'                                             121
      AG_PARLIST = 'A_Z_W'                                                121
      CALL AXCOMPONENT                                                    121
C *                                                                       121
C Geant COMPONENT : N ===> A=14 Z=7 W=0.032 <===                          121
      CALL AGSRESET                                                       122
      AG_A = 14                                                           122
      AG_Z = 7                                                            122
      AG_W = 0.032                                                        122
      AG_TITLE = 'N'                                                      122
      AG_EXNAME = 'COMPONENT'                                             122
      AG_PARLIST = 'A_Z_W'                                                122
      CALL AXCOMPONENT                                                    122
C *                                                                       122
C Geant COMPONENT : CU ===> A=63.5 Z=29 W=0.547 <===                      122
      CALL AGSRESET                                                       123
      AG_A = 63.5                                                         123
      AG_Z = 29                                                           123
      AG_W = 0.547                                                        123
      AG_TITLE = 'CU'                                                     123
      AG_EXNAME = 'COMPONENT'                                             123
      AG_PARLIST = 'A_Z_W'                                                123
      CALL AXCOMPONENT                                                    123
C *                                                                       123
C Geant COMPONENT : AR ===> A=39.9 Z=18 W=0.014 <===                      123
      CALL AGSRESET                                                       124
      AG_A = 39.9                                                         124
      AG_Z = 18                                                           124
      AG_W = 0.014                                                        124
      AG_TITLE = 'AR'                                                     124
      AG_EXNAME = 'COMPONENT'                                             124
      AG_PARLIST = 'A_Z_W'                                                124
      CALL AXCOMPONENT                                                    124
C *                                                                       124
C Geant MIXTURE : GEMMIX ===> DENS=0.079 <===                             124
      CALL AGSRESET                                                       125
      AG_DENS = 0.079                                                     125
      AG_TITLE = 'GEMMIX'                                                 125
      AG_EXNAME = 'MIXTURE'                                               125
      AG_PARLIST = 'DENS'                                                 125
      CALL AXMIXTURE                                                      125
C *                                                                       125
C Geant COMPONENT : O ===> A=16 Z=8 W=0.160 <===                          125
      CALL AGSRESET                                                       126
      AG_A = 16                                                           126
      AG_Z = 8                                                            126
      AG_W = 0.160                                                        126
      AG_TITLE = 'O'                                                      126
      AG_EXNAME = 'COMPONENT'                                             126
      AG_PARLIST = 'A_Z_W'                                                126
      CALL AXCOMPONENT                                                    126
C *                                                                       126
C Geant COMPONENT : C ===> A=12 Z=6 W=0.508 <===                          126
      CALL AGSRESET                                                       127
      AG_A = 12                                                           127
      AG_Z = 6                                                            127
      AG_W = 0.508                                                        127
      AG_TITLE = 'C'                                                      127
      AG_EXNAME = 'COMPONENT'                                             127
      AG_PARLIST = 'A_Z_W'                                                127
      CALL AXCOMPONENT                                                    127
C *                                                                       127
C Geant COMPONENT : H ===> A=1 Z=1 W=0.020 <===                           127
      CALL AGSRESET                                                       128
      AG_A = 1                                                            128
      AG_Z = 1                                                            128
      AG_W = 0.020                                                        128
      AG_TITLE = 'H'                                                      128
      AG_EXNAME = 'COMPONENT'                                             128
      AG_PARLIST = 'A_Z_W'                                                128
      CALL AXCOMPONENT                                                    128
C *                                                                       128
C Geant COMPONENT : N ===> A=14 Z=7 W=0.051 <===                          128
      CALL AGSRESET                                                       129
      AG_A = 14                                                           129
      AG_Z = 7                                                            129
      AG_W = 0.051                                                        129
      AG_TITLE = 'N'                                                      129
      AG_EXNAME = 'COMPONENT'                                             129
      AG_PARLIST = 'A_Z_W'                                                129
      CALL AXCOMPONENT                                                    129
C *                                                                       129
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.003 <===                        129
      CALL AGSRESET                                                       130
      AG_A = 27                                                           130
      AG_Z = 13                                                           130
      AG_W = 0.003                                                        130
      AG_TITLE = 'AL'                                                     130
      AG_EXNAME = 'COMPONENT'                                             130
      AG_PARLIST = 'A_Z_W'                                                130
      CALL AXCOMPONENT                                                    130
C *                                                                       130
C Geant COMPONENT : CU ===> A=63.5 Z=29 W=0.255 <===                      130
      CALL AGSRESET                                                       131
      AG_A = 63.5                                                         131
      AG_Z = 29                                                           131
      AG_W = 0.255                                                        131
      AG_TITLE = 'CU'                                                     131
      AG_EXNAME = 'COMPONENT'                                             131
      AG_PARLIST = 'A_Z_W'                                                131
      CALL AXCOMPONENT                                                    131
C *                                                                       131
C Geant COMPONENT : AR ===> A=39.9 Z=18 W=0.003 <===                      131
      CALL AGSRESET                                                       132
      AG_A = 39.9                                                         132
      AG_Z = 18                                                           132
      AG_W = 0.003                                                        132
      AG_TITLE = 'AR'                                                     132
      AG_EXNAME = 'COMPONENT'                                             132
      AG_PARLIST = 'A_Z_W'                                                132
      CALL AXCOMPONENT                                                    132
C *                                                                       132
C Geant MIXTURE : READMIX ===> DENS=0.356 <===                            132
      CALL AGSRESET                                                       133
      AG_DENS = 0.356                                                     133
      AG_TITLE = 'READMIX'                                                133
      AG_EXNAME = 'MIXTURE'                                               133
      AG_PARLIST = 'DENS'                                                 133
      CALL AXMIXTURE                                                      133
C *                                                                       133
C Geant COMPONENT : SI ===> A=28.1 Z=14 W=0.191 <===                      133
      CALL AGSRESET                                                       134
      AG_A = 28.1                                                         134
      AG_Z = 14                                                           134
      AG_W = 0.191                                                        134
      AG_TITLE = 'SI'                                                     134
      AG_EXNAME = 'COMPONENT'                                             134
      AG_PARLIST = 'A_Z_W'                                                134
      CALL AXCOMPONENT                                                    134
C *                                                                       134
C Geant COMPONENT : O ===> A=16 Z=8 W=0.339 <===                          134
      CALL AGSRESET                                                       135
      AG_A = 16                                                           135
      AG_Z = 8                                                            135
      AG_W = 0.339                                                        135
      AG_TITLE = 'O'                                                      135
      AG_EXNAME = 'COMPONENT'                                             135
      AG_PARLIST = 'A_Z_W'                                                135
      CALL AXCOMPONENT                                                    135
C *                                                                       135
C Geant COMPONENT : C ===> A=12 Z=6 W=0.213 <===                          135
      CALL AGSRESET                                                       136
      AG_A = 12                                                           136
      AG_Z = 6                                                            136
      AG_W = 0.213                                                        136
      AG_TITLE = 'C'                                                      136
      AG_EXNAME = 'COMPONENT'                                             136
      AG_PARLIST = 'A_Z_W'                                                136
      CALL AXCOMPONENT                                                    136
C *                                                                       136
C Geant COMPONENT : H ===> A=1 Z=1 W=0.028 <===                           136
      CALL AGSRESET                                                       137
      AG_A = 1                                                            137
      AG_Z = 1                                                            137
      AG_W = 0.028                                                        137
      AG_TITLE = 'H'                                                      137
      AG_EXNAME = 'COMPONENT'                                             137
      AG_PARLIST = 'A_Z_W'                                                137
      CALL AXCOMPONENT                                                    137
C *                                                                       137
C Geant COMPONENT : CL ===> A=35.5 Z=17 W=0.009 <===                      137
      CALL AGSRESET                                                       138
      AG_A = 35.5                                                         138
      AG_Z = 17                                                           138
      AG_W = 0.009                                                        138
      AG_TITLE = 'CL'                                                     138
      AG_EXNAME = 'COMPONENT'                                             138
      AG_PARLIST = 'A_Z_W'                                                138
      CALL AXCOMPONENT                                                    138
C *                                                                       138
C Geant COMPONENT : CU ===> A=63.5 Z=29 W=0.122 <===                      138
      CALL AGSRESET                                                       139
      AG_A = 63.5                                                         139
      AG_Z = 29                                                           139
      AG_W = 0.122                                                        139
      AG_TITLE = 'CU'                                                     139
      AG_EXNAME = 'COMPONENT'                                             139
      AG_PARLIST = 'A_Z_W'                                                139
      CALL AXCOMPONENT                                                    139
C *                                                                       139
C Geant COMPONENT : PB ===> A=207 Z=82 W=0.039 <===                       139
      CALL AGSRESET                                                       140
      AG_A = 207                                                          140
      AG_Z = 82                                                           140
      AG_W = 0.039                                                        140
      AG_TITLE = 'PB'                                                     140
      AG_EXNAME = 'COMPONENT'                                             140
      AG_PARLIST = 'A_Z_W'                                                140
      CALL AXCOMPONENT                                                    140
C *                                                                       140
C Geant COMPONENT : SN ===> A=118.7 Z=50 W=0.059 <===                     140
      CALL AGSRESET                                                       141
      AG_A = 118.7                                                        141
      AG_Z = 50                                                           141
      AG_W = 0.059                                                        141
      AG_TITLE = 'SN'                                                     141
      AG_EXNAME = 'COMPONENT'                                             141
      AG_PARLIST = 'A_Z_W'                                                141
      CALL AXCOMPONENT                                                    141
C *                                                                       141
C Geant MIXTURE : APVMIX ===> DENS=2.535 <===                             141
      CALL AGSRESET                                                       142
      AG_DENS = 2.535                                                     142
      AG_TITLE = 'APVMIX'                                                 142
      AG_EXNAME = 'MIXTURE'                                               142
      AG_PARLIST = 'DENS'                                                 142
      CALL AXMIXTURE                                                      142
C *                                                                       142
C Geant COMPONENT : SI ===> A=28.1 Z=14 W=0.113 <===                      142
      CALL AGSRESET                                                       143
      AG_A = 28.1                                                         143
      AG_Z = 14                                                           143
      AG_W = 0.113                                                        143
      AG_TITLE = 'SI'                                                     143
      AG_EXNAME = 'COMPONENT'                                             143
      AG_PARLIST = 'A_Z_W'                                                143
      CALL AXCOMPONENT                                                    143
C *                                                                       143
C Geant COMPONENT : O ===> A=16 Z=8 W=0.241 <===                          143
      CALL AGSRESET                                                       144
      AG_A = 16                                                           144
      AG_Z = 8                                                            144
      AG_W = 0.241                                                        144
      AG_TITLE = 'O'                                                      144
      AG_EXNAME = 'COMPONENT'                                             144
      AG_PARLIST = 'A_Z_W'                                                144
      CALL AXCOMPONENT                                                    144
C *                                                                       144
C Geant COMPONENT : C ===> A=12 Z=6 W=0.248 <===                          144
      CALL AGSRESET                                                       145
      AG_A = 12                                                           145
      AG_Z = 6                                                            145
      AG_W = 0.248                                                        145
      AG_TITLE = 'C'                                                      145
      AG_EXNAME = 'COMPONENT'                                             145
      AG_PARLIST = 'A_Z_W'                                                145
      CALL AXCOMPONENT                                                    145
C *                                                                       145
C Geant COMPONENT : H ===> A=1 Z=1 W=0.023 <===                           145
      CALL AGSRESET                                                       146
      AG_A = 1                                                            146
      AG_Z = 1                                                            146
      AG_W = 0.023                                                        146
      AG_TITLE = 'H'                                                      146
      AG_EXNAME = 'COMPONENT'                                             146
      AG_PARLIST = 'A_Z_W'                                                146
      CALL AXCOMPONENT                                                    146
C *                                                                       146
C Geant COMPONENT : CU ===> A=63.5 Z=29 W=0.109 <===                      146
      CALL AGSRESET                                                       147
      AG_A = 63.5                                                         147
      AG_Z = 29                                                           147
      AG_W = 0.109                                                        147
      AG_TITLE = 'CU'                                                     147
      AG_EXNAME = 'COMPONENT'                                             147
      AG_PARLIST = 'A_Z_W'                                                147
      CALL AXCOMPONENT                                                    147
C *                                                                       147
C Geant COMPONENT : FE ===> A=55.8 Z=26 W=0.019 <===                      147
      CALL AGSRESET                                                       148
      AG_A = 55.8                                                         148
      AG_Z = 26                                                           148
      AG_W = 0.019                                                        148
      AG_TITLE = 'FE'                                                     148
      AG_EXNAME = 'COMPONENT'                                             148
      AG_PARLIST = 'A_Z_W'                                                148
      CALL AXCOMPONENT                                                    148
C *                                                                       148
C Geant COMPONENT : CR ===> A=52.0 Z=24 W=0.008 <===                      148
      CALL AGSRESET                                                       149
      AG_A = 52.0                                                         149
      AG_Z = 24                                                           149
      AG_W = 0.008                                                        149
      AG_TITLE = 'CR'                                                     149
      AG_EXNAME = 'COMPONENT'                                             149
      AG_PARLIST = 'A_Z_W'                                                149
      CALL AXCOMPONENT                                                    149
C *                                                                       149
C Geant COMPONENT : PB ===> A=207 Z=82 W=0.096 <===                       149
      CALL AGSRESET                                                       150
      AG_A = 207                                                          150
      AG_Z = 82                                                           150
      AG_W = 0.096                                                        150
      AG_TITLE = 'PB'                                                     150
      AG_EXNAME = 'COMPONENT'                                             150
      AG_PARLIST = 'A_Z_W'                                                150
      CALL AXCOMPONENT                                                    150
C *                                                                       150
C Geant COMPONENT : SN ===> A=118.7 Z=50 W=0.143 <===                     150
      CALL AGSRESET                                                       151
      AG_A = 118.7                                                        151
      AG_Z = 50                                                           151
      AG_W = 0.143                                                        151
      AG_TITLE = 'SN'                                                     151
      AG_EXNAME = 'COMPONENT'                                             151
      AG_PARLIST = 'A_Z_W'                                                151
      CALL AXCOMPONENT                                                    151
C *                                                                       151
C Geant MIXTURE : HVMIX ===> DENS=3.009 <===                              151
      CALL AGSRESET                                                       152
      AG_DENS = 3.009                                                     152
      AG_TITLE = 'HVMIX'                                                  152
      AG_EXNAME = 'MIXTURE'                                               152
      AG_PARLIST = 'DENS'                                                 152
      CALL AXMIXTURE                                                      152
C *                                                                       152
C Geant COMPONENT : SI ===> A=28.1 Z=14 W=0.122 <===                      152
      CALL AGSRESET                                                       153
      AG_A = 28.1                                                         153
      AG_Z = 14                                                           153
      AG_W = 0.122                                                        153
      AG_TITLE = 'SI'                                                     153
      AG_EXNAME = 'COMPONENT'                                             153
      AG_PARLIST = 'A_Z_W'                                                153
      CALL AXCOMPONENT                                                    153
C *                                                                       153
C Geant COMPONENT : O ===> A=16 Z=8 W=0.225 <===                          153
      CALL AGSRESET                                                       154
      AG_A = 16                                                           154
      AG_Z = 8                                                            154
      AG_W = 0.225                                                        154
      AG_TITLE = 'O'                                                      154
      AG_EXNAME = 'COMPONENT'                                             154
      AG_PARLIST = 'A_Z_W'                                                154
      CALL AXCOMPONENT                                                    154
C *                                                                       154
C Geant COMPONENT : C ===> A=12 Z=6 W=0.297 <===                          154
      CALL AGSRESET                                                       155
      AG_A = 12                                                           155
      AG_Z = 6                                                            155
      AG_W = 0.297                                                        155
      AG_TITLE = 'C'                                                      155
      AG_EXNAME = 'COMPONENT'                                             155
      AG_PARLIST = 'A_Z_W'                                                155
      CALL AXCOMPONENT                                                    155
C *                                                                       155
C Geant COMPONENT : H ===> A=1 Z=1 W=0.023 <===                           155
      CALL AGSRESET                                                       156
      AG_A = 1                                                            156
      AG_Z = 1                                                            156
      AG_W = 0.023                                                        156
      AG_TITLE = 'H'                                                      156
      AG_EXNAME = 'COMPONENT'                                             156
      AG_PARLIST = 'A_Z_W'                                                156
      CALL AXCOMPONENT                                                    156
C *                                                                       156
C Geant COMPONENT : N ===> A=14 Z=7 W=0.009 <===                          156
      CALL AGSRESET                                                       157
      AG_A = 14                                                           157
      AG_Z = 7                                                            157
      AG_W = 0.009                                                        157
      AG_TITLE = 'N'                                                      157
      AG_EXNAME = 'COMPONENT'                                             157
      AG_PARLIST = 'A_Z_W'                                                157
      CALL AXCOMPONENT                                                    157
C *                                                                       157
C Geant COMPONENT : CU ===> A=63.5 Z=29 W=0.045 <===                      157
      CALL AGSRESET                                                       158
      AG_A = 63.5                                                         158
      AG_Z = 29                                                           158
      AG_W = 0.045                                                        158
      AG_TITLE = 'CU'                                                     158
      AG_EXNAME = 'COMPONENT'                                             158
      AG_PARLIST = 'A_Z_W'                                                158
      CALL AXCOMPONENT                                                    158
C *                                                                       158
C Geant COMPONENT : FE ===> A=55.8 Z=26 W=0.055 <===                      158
      CALL AGSRESET                                                       159
      AG_A = 55.8                                                         159
      AG_Z = 26                                                           159
      AG_W = 0.055                                                        159
      AG_TITLE = 'FE'                                                     159
      AG_EXNAME = 'COMPONENT'                                             159
      AG_PARLIST = 'A_Z_W'                                                159
      CALL AXCOMPONENT                                                    159
C *                                                                       159
C Geant COMPONENT : NI ===> A=58.7 Z=28 W=0.022 <===                      159
      CALL AGSRESET                                                       160
      AG_A = 58.7                                                         160
      AG_Z = 28                                                           160
      AG_W = 0.022                                                        160
      AG_TITLE = 'NI'                                                     160
      AG_EXNAME = 'COMPONENT'                                             160
      AG_PARLIST = 'A_Z_W'                                                160
      CALL AXCOMPONENT                                                    160
C *                                                                       160
C Geant COMPONENT : PB ===> A=207 Z=82 W=0.081 <===                       160
      CALL AGSRESET                                                       161
      AG_A = 207                                                          161
      AG_Z = 82                                                           161
      AG_W = 0.081                                                        161
      AG_TITLE = 'PB'                                                     161
      AG_EXNAME = 'COMPONENT'                                             161
      AG_PARLIST = 'A_Z_W'                                                161
      CALL AXCOMPONENT                                                    161
C *                                                                       161
C Geant COMPONENT : SN ===> A=118.7 Z=50 W=0.121 <===                     161
      CALL AGSRESET                                                       162
      AG_A = 118.7                                                        162
      AG_Z = 50                                                           162
      AG_W = 0.121                                                        162
      AG_TITLE = 'SN'                                                     162
      AG_EXNAME = 'COMPONENT'                                             162
      AG_PARLIST = 'A_Z_W'                                                162
      CALL AXCOMPONENT                                                    162
C *                                                                       162
C Geant MIXTURE : CONMIX ===> DENS=1.816 <===                             162
      CALL AGSRESET                                                       163
      AG_DENS = 1.816                                                     163
      AG_TITLE = 'CONMIX'                                                 163
      AG_EXNAME = 'MIXTURE'                                               163
      AG_PARLIST = 'DENS'                                                 163
      CALL AXMIXTURE                                                      163
C *                                                                       163
C Geant COMPONENT : SI ===> A=28.1 Z=14 W=0.141 <===                      163
      CALL AGSRESET                                                       164
      AG_A = 28.1                                                         164
      AG_Z = 14                                                           164
      AG_W = 0.141                                                        164
      AG_TITLE = 'SI'                                                     164
      AG_EXNAME = 'COMPONENT'                                             164
      AG_PARLIST = 'A_Z_W'                                                164
      CALL AXCOMPONENT                                                    164
C *                                                                       164
C Geant COMPONENT : O ===> A=16 Z=8 W=0.196 <===                          164
      CALL AGSRESET                                                       165
      AG_A = 16                                                           165
      AG_Z = 8                                                            165
      AG_W = 0.196                                                        165
      AG_TITLE = 'O'                                                      165
      AG_EXNAME = 'COMPONENT'                                             165
      AG_PARLIST = 'A_Z_W'                                                165
      CALL AXCOMPONENT                                                    165
C *                                                                       165
C Geant COMPONENT : C ===> A=12 Z=6 W=0.419 <===                          165
      CALL AGSRESET                                                       166
      AG_A = 12                                                           166
      AG_Z = 6                                                            166
      AG_W = 0.419                                                        166
      AG_TITLE = 'C'                                                      166
      AG_EXNAME = 'COMPONENT'                                             166
      AG_PARLIST = 'A_Z_W'                                                166
      CALL AXCOMPONENT                                                    166
C *                                                                       166
C Geant COMPONENT : H ===> A=1 Z=1 W=0.008 <===                           166
      CALL AGSRESET                                                       167
      AG_A = 1                                                            167
      AG_Z = 1                                                            167
      AG_W = 0.008                                                        167
      AG_TITLE = 'H'                                                      167
      AG_EXNAME = 'COMPONENT'                                             167
      AG_PARLIST = 'A_Z_W'                                                167
      CALL AXCOMPONENT                                                    167
C *                                                                       167
C Geant COMPONENT : CU ===> A=63.5 Z=29 W=0.156 <===                      167
      CALL AGSRESET                                                       168
      AG_A = 63.5                                                         168
      AG_Z = 29                                                           168
      AG_W = 0.156                                                        168
      AG_TITLE = 'CU'                                                     168
      AG_EXNAME = 'COMPONENT'                                             168
      AG_PARLIST = 'A_Z_W'                                                168
      CALL AXCOMPONENT                                                    168
C *                                                                       168
C Geant COMPONENT : PB ===> A=207 Z=82 W=0.032 <===                       168
      CALL AGSRESET                                                       169
      AG_A = 207                                                          169
      AG_Z = 82                                                           169
      AG_W = 0.032                                                        169
      AG_TITLE = 'PB'                                                     169
      AG_EXNAME = 'COMPONENT'                                             169
      AG_PARLIST = 'A_Z_W'                                                169
      CALL AXCOMPONENT                                                    169
C *                                                                       169
C Geant COMPONENT : SN ===> A=118.7 Z=50 W=0.048 <===                     169
      CALL AGSRESET                                                       170
      AG_A = 118.7                                                        170
      AG_Z = 50                                                           170
      AG_W = 0.048                                                        170
      AG_TITLE = 'SN'                                                     170
      AG_EXNAME = 'COMPONENT'                                             170
      AG_PARLIST = 'A_Z_W'                                                170
      CALL AXCOMPONENT                                                    170
C *                                                                       170
C Geant MIXTURE : TERMMIX ===> DENS=2.55 <===                             170
      CALL AGSRESET                                                       171
      AG_DENS = 2.55                                                      171
      AG_TITLE = 'TERMMIX'                                                171
      AG_EXNAME = 'MIXTURE'                                               171
      AG_PARLIST = 'DENS'                                                 171
      CALL AXMIXTURE                                                      171
C *                                                                       171
C Geant COMPONENT : CU ===> A=63.5 Z=29 W=0.63 <===                       171
      CALL AGSRESET                                                       172
      AG_A = 63.5                                                         172
      AG_Z = 29                                                           172
      AG_W = 0.63                                                         172
      AG_TITLE = 'CU'                                                     172
      AG_EXNAME = 'COMPONENT'                                             172
      AG_PARLIST = 'A_Z_W'                                                172
      CALL AXCOMPONENT                                                    172
C *                                                                       172
C Geant COMPONENT : ZN ===> A=65.4 Z=30 W=0.37 <===                       172
      CALL AGSRESET                                                       173
      AG_A = 65.4                                                         173
      AG_Z = 30                                                           173
      AG_W = 0.37                                                         173
      AG_TITLE = 'ZN'                                                     173
      AG_EXNAME = 'COMPONENT'                                             173
      AG_PARLIST = 'A_Z_W'                                                173
      CALL AXCOMPONENT                                                    173
C *                                                                       173
C Geant MIXTURE : BRASS ===> DENS=8.4 <===                                173
      CALL AGSRESET                                                       174
      AG_DENS = 8.4                                                       174
      AG_TITLE = 'BRASS'                                                  174
      AG_EXNAME = 'MIXTURE'                                               174
      AG_PARLIST = 'DENS'                                                 174
      CALL AXMIXTURE                                                      174
C *                                                                       174
C Geant COMPONENT : AR ===> A=39.95 Z=18. W=0.700 <===                    174
      CALL AGSRESET                                                       175
      AG_A = 39.95                                                        175
      AG_Z = 18.                                                          175
      AG_W = 0.700                                                        175
      AG_TITLE = 'AR'                                                     175
      AG_EXNAME = 'COMPONENT'                                             175
      AG_PARLIST = 'A_Z_W'                                                175
      CALL AXCOMPONENT                                                    175
C *                                                                       175
C Geant COMPONENT : O ===> A=16. Z=8. W=0.218 <===                        175
      CALL AGSRESET                                                       176
      AG_A = 16.                                                          176
      AG_Z = 8.                                                           176
      AG_W = 0.218                                                        176
      AG_TITLE = 'O'                                                      176
      AG_EXNAME = 'COMPONENT'                                             176
      AG_PARLIST = 'A_Z_W'                                                176
      CALL AXCOMPONENT                                                    176
C *                                                                       176
C Geant COMPONENT : C ===> A=12.01 Z=6. W=0.082 <===                      176
      CALL AGSRESET                                                       177
      AG_A = 12.01                                                        177
      AG_Z = 6.                                                           177
      AG_W = 0.082                                                        177
      AG_TITLE = 'C'                                                      177
      AG_EXNAME = 'COMPONENT'                                             177
      AG_PARLIST = 'A_Z_W'                                                177
      CALL AXCOMPONENT                                                    177
C *                                                                       177
C Geant MIXTURE : ARCO2MIX ===> ISVOL=1 DENS=0.0018015 <===               177
      CALL AGSRESET                                                       178
      AG_ISVOL = 1                                                        178
      AG_DENS = 0.0018015                                                 178
      AG_TITLE = 'ARCO2MIX'                                               178
      AG_EXNAME = 'MIXTURE'                                               178
      AG_PARLIST = 'ISVOL_DENS'                                           178
      CALL AXMIXTURE                                                      178
C *                                                                       178
C use default FGGG bank                                                   178
      BPATH_FGGG='FGGG*'                                                  179
      IDNUM_FGGG(1)=0                                                     179
      CALL AGDGETP('FGTDGEO3','*',-999,'FGGG',' ',BANK_FGGG, BPATH_FGGG,  179
     *IDNUM_FGGG,1,MAP_FGGG,NAMES_FGGG,BANK_FGGG,LEN_FGGG,LLEN_FGGG,      179
     *LINK_FGGG,FLAG_FGGG)                                                179
C use FGST bank                                                           180
      BPATH_FGST='FGST*'                                                  180
      IDNUM_FGST(1)=0                                                     180
      FGST_CONFIG=FGGG_FGSTCONFIG                                         180
      CALL AGDGETP('FGTDGEO3','*',-999,'FGST',NFGST_CONFIG,               180
     *RE_FGST_CONFIG,BPATH_FGST,IDNUM_FGST,1,MAP_FGST,NAMES_FGST,         180
     *BANK_FGST,LEN_FGST,LLEN_FGST,LINK_FGST,FLAG_FGST)                   180
      DISKLENZ=VOLNOMEXDZ+VOLSENSDZ+VOLGEMDZ+VOLREADDZ+VOLELECDZ          181
      FGTLENZ = COOLTUBEDZ                                                182
      CENTERZ = FGST_ZDISCA(1) +FGTLENZ/2.0 -( VOLNOMEXDZ+VOLSENSDZ/2.)   183
     *- FGST_COOL1STSHIFT                                                 183
      XX=ACOS(FLATOUTR/DISKOUTR)                                          184
      FLATANG1=FLATPERPANG-XX/DEGRAD                                      185
      FLATANG2=FLATPERPANG+XX/DEGRAD                                      186
      CALL AGSBEGM(MODULE,IPRIN)                                          187
C Check IPRIN<0                                                           187
      IF (IPRIN.LT.0) RETURN                                              187
C Create FGTM :                                                           187
C Call To Local Procedure FGTM                                            187
      ASSIGN 5020 TO BLOCK_FGTM                                           187
      GO TO  5010                                                         187
5020  CONTINUE                                                            187
C *                                                                       187
C Geant POSITION : FGTM ===> MOTHER='IDSM' Z=CENTERZ <===                 188
      CALL AGSRESET                                                       188
      AG_MOTHER = 'IDSM'                                                  188
      AG_Z = CENTERZ                                                      188
      AG_TITLE = 'FGTM'                                                   188
      AG_EXNAME = 'POSITION'                                              188
      AG_PARLIST = 'MOTHER_Z'                                             188
      CALL AXPOSITION                                                     188
C *                                                                       188
C ----------------------------------------------------------------------  188
C ------------ FGTM                                                       188
      CALL AGSENDM                                                        190
      RETURN                                                              190
C Begin Local Procedure FGTM                                              190
C FGTM MOTHER VOLUME FOR FGT ASSEMBLY                                     190
5010  CONTINUE                                                            190
         AG_TITLE='FGTM'                                                  190
         AG_MODULE=MODULE                                                 190
         AG_MARK='V00'                                                    190
         CALL AGSPUSH                                                     190
         AG_MODULE=MODULE                                                 190
C    *                                                                    190
C    Geant ATTRIBUTE : FGTM ===> SEEN=0 COLO=1 <===                       191
         CALL AGSRESET                                                    191
         AG_SEEN = 0                                                      191
         AG_COLO = 1                                                      191
         AG_TITLE = 'FGTM'                                                191
         AG_EXNAME = 'ATTRIBUTE'                                          191
         AG_PARLIST = 'SEEN_COLO'                                         191
         CALL AXATTRIBUTE                                                 191
C    *                                                                    191
C    Geant MATERIAL : AIR ===> inherited arguments used <===              191
         CALL AGSRESET                                                    192
         AG_TITLE = 'AIR'                                                 192
         AG_EXNAME = 'MATERIAL'                                           192
         AG_PARLIST = 'NONE'                                              192
         CALL AXMATERIAL                                                  192
C    *                                                                    192
C    Geant SHAPE : TUBE ===> RMIN=COOLTUBEINR RMAX=DISKOUTR, DZ=FGTLENZ/  192
C    2.0 <===                                                             192
         CALL AGSRESET                                                    194
         AG_RMIN = COOLTUBEINR                                            194
         AG_RMAX = DISKOUTR                                               194
         AG_DZ = FGTLENZ/2.0                                              194
         AG_TITLE = 'TUBE'                                                194
         AG_EXNAME = 'SHAPE'                                              194
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      194
         CALL AXSHAPE                                                     194
C    *                                                                    194
C    Create FGCT :                                                        195
C    Call To Local Procedure FGCT                                         195
         ASSIGN 5040 TO BLOCK_FGCT                                        195
         GO TO  5030                                                      195
5040     CONTINUE                                                         195
C    *                                                                    195
C    Geant POSITION : FGCT ===> inherited arguments used <===             196
         CALL AGSRESET                                                    196
         AG_TITLE = 'FGCT'                                                196
         AG_EXNAME = 'POSITION'                                           196
         AG_PARLIST = 'NONE'                                              196
         CALL AXPOSITION                                                  196
C    *                                                                    196
C    Create FGCN :                                                        197
C    Call To Local Procedure FGCN                                         197
         ASSIGN 5060 TO BLOCK_FGCN                                        197
         GO TO  5050                                                      197
5060     CONTINUE                                                         197
C    *                                                                    197
C    Geant POSITION : FGCN ===> Z=-FGTLENZ/2.0+0.64/2. <===               198
         CALL AGSRESET                                                    198
         AG_Z = -FGTLENZ/2.0+0.64/2.                                      198
         AG_TITLE = 'FGCN'                                                198
         AG_EXNAME = 'POSITION'                                           198
         AG_PARLIST = 'Z'                                                 198
         CALL AXPOSITION                                                  198
C    *                                                                    198
C    *                                                                    198
         DISK=1                                                           199
            GO TO 5073                                                    199
5071        DISK=DISK+1                                                   199
5073        IF(DISK-(FGST_NDISK).GT.0) GO TO 5072                         199
            DISKZ=-FGTLENZ/2.+1.0 +DISKLENZ/2. + FGST_ZDISCA(DISK)-       200
     *      FGST_ZDISCA(1) + FGST_COOL1STSHIFT                            200
C       Check (FGST_CONFIG.EQ.2.0).OR.(DISK.LE.1)                         201
            IF ((FGST_CONFIG.EQ.2.0).OR.(DISK.LE.1)) THEN                 201
C       Create FGTD :                                                     202
C       Call To Local Procedure FGTD                                      202
            ASSIGN 5090 TO BLOCK_FGTD                                     202
            GO TO  5080                                                   202
5090        CONTINUE                                                      202
C       *                                                                 202
C       Geant POSITION : FGTD ===> Z=DISKZ ALPHAZ=QUADTILTANG NCOPY=DISK  203
C        <===                                                             203
            CALL AGSRESET                                                 203
            AG_Z = DISKZ                                                  203
            AG_ALPHAN=AG_ALPHAN+1                                         203
            AG_ALPHAK(AG_ALPHAN)=3                                        203
            AG_ALPHAL(AG_ALPHAN)=QUADTILTANG                              203
            AG_NCOPY = DISK                                               203
            AG_TITLE = 'FGTD'                                             203
            AG_EXNAME = 'POSITION'                                        203
            AG_PARLIST = 'Z_ALPHAZ_NCOPY'                                 203
            CALL AXPOSITION                                               203
C       *                                                                 203
            ELSE                                                          203
C       Create FGTH :                                                     205
C       Call To Local Procedure FGTH                                      205
            ASSIGN 5110 TO BLOCK_FGTH                                     205
            GO TO  5100                                                   205
5110        CONTINUE                                                      205
C       *                                                                 205
C       Geant POSITION : FGTH ===> Z=DISKZ ALPHAZ=QUADTILTANG NCOPY=DISK  206
C        <===                                                             206
            CALL AGSRESET                                                 206
            AG_Z = DISKZ                                                  206
            AG_ALPHAN=AG_ALPHAN+1                                         206
            AG_ALPHAK(AG_ALPHAN)=3                                        206
            AG_ALPHAL(AG_ALPHAN)=QUADTILTANG                              206
            AG_NCOPY = DISK                                               206
            AG_TITLE = 'FGTH'                                             206
            AG_EXNAME = 'POSITION'                                        206
            AG_PARLIST = 'Z_ALPHAZ_NCOPY'                                 206
            CALL AXPOSITION                                               206
C       *                                                                 206
            ENDIF                                                         206
         GO TO 5071                                                       208
5072     CONTINUE                                                         208
C    *                                                                    209
         DISK=2                                                           209
            GO TO 5123                                                    209
5121        DISK=DISK+1                                                   209
5123        IF(DISK-(FGST_NDISK).GT.0) GO TO 5122                         209
            DISKZ=-FGTLENZ/2. + 1.0 + FGST_ZDISCA(DISK)- FGST_ZDISCA(1)   210
     *      -1.28/2.                                                      210
C       Create FGCM :                                                     211
C       Call To Local Procedure FGCM                                      211
            ASSIGN 5140 TO BLOCK_FGCM                                     211
            GO TO  5130                                                   211
5140        CONTINUE                                                      211
C       *                                                                 211
C       Geant POSITION : FGCM ===> Z=DISKZ <===                           212
            CALL AGSRESET                                                 212
            AG_Z = DISKZ                                                  212
            AG_TITLE = 'FGCM'                                             212
            AG_EXNAME = 'POSITION'                                        212
            AG_PARLIST = 'Z'                                              212
            CALL AXPOSITION                                               212
C       *                                                                 212
         GO TO 5121                                                       213
5122     CONTINUE                                                         213
5011  CALL AGSPOPB                                                        214
      GO TO BLOCK_FGTM, (5010 ,5020)                                      214
C End of Local Procedure                                                  214
C ----------------------------------------------------------------------  215
C ------------ FGCT                                                       215
C Begin Local Procedure FGCT                                              216
C FGCT INNER COOLING TUBE                                                 216
5030  CONTINUE                                                            216
         AG_TITLE='FGCT'                                                  216
         AG_MODULE=MODULE                                                 216
         AG_MARK='V00'                                                    216
         CALL AGSPUSH                                                     216
         AG_MODULE=MODULE                                                 216
C    *                                                                    216
C    Geant ATTRIBUTE : FGCT ===> SEEN=1 COLO=6 <===                       217
         CALL AGSRESET                                                    217
         AG_SEEN = 1                                                      217
         AG_COLO = 6                                                      217
         AG_TITLE = 'FGCT'                                                217
         AG_EXNAME = 'ATTRIBUTE'                                          217
         AG_PARLIST = 'SEEN_COLO'                                         217
         CALL AXATTRIBUTE                                                 217
C    *                                                                    217
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          217
         CALL AGSRESET                                                    218
         AG_TITLE = 'CFRPMIX'                                             218
         AG_EXNAME = 'MATERIAL'                                           218
         AG_PARLIST = 'NONE'                                              218
         CALL AXMATERIAL                                                  218
C    *                                                                    218
C    Geant SHAPE : TUBE ===> RMIN=COOLTUBEINR RMAX=COOLTUBEINR+0.05, DZ=  218
C    COOLTUBEDZ/2. <===                                                   218
         CALL AGSRESET                                                    220
         AG_RMIN = COOLTUBEINR                                            220
         AG_RMAX = COOLTUBEINR+0.05                                       220
         AG_DZ = COOLTUBEDZ/2.                                            220
         AG_TITLE = 'TUBE'                                                220
         AG_EXNAME = 'SHAPE'                                              220
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      220
         CALL AXSHAPE                                                     220
C    *                                                                    220
5031  CALL AGSPOPB                                                        221
      GO TO BLOCK_FGCT, (5030 ,5040)                                      221
C End of Local Procedure                                                  221
C ----------------------------------------------------------------------  222
C ------------ FGCN                                                       222
C Begin Local Procedure FGCN                                              223
C FGCN NYLON 1ST RING                                                     223
5050  CONTINUE                                                            223
         AG_TITLE='FGCN'                                                  223
         AG_MODULE=MODULE                                                 223
         AG_MARK='V00'                                                    223
         CALL AGSPUSH                                                     223
         AG_MODULE=MODULE                                                 223
C    *                                                                    223
C    Geant ATTRIBUTE : FGCN ===> SEEN=1 COLO=6 <===                       224
         CALL AGSRESET                                                    224
         AG_SEEN = 1                                                      224
         AG_COLO = 6                                                      224
         AG_TITLE = 'FGCN'                                                224
         AG_EXNAME = 'ATTRIBUTE'                                          224
         AG_PARLIST = 'SEEN_COLO'                                         224
         CALL AXATTRIBUTE                                                 224
C    *                                                                    224
C    Geant MATERIAL : NYLON ===> inherited arguments used <===            224
         CALL AGSRESET                                                    225
         AG_TITLE = 'NYLON'                                               225
         AG_EXNAME = 'MATERIAL'                                           225
         AG_PARLIST = 'NONE'                                              225
         CALL AXMATERIAL                                                  225
C    *                                                                    225
C    Geant SHAPE : TUBE ===> RMIN=10.35 RMAX=11.5 DZ=0.64/2. <===         225
         CALL AGSRESET                                                    226
         AG_RMIN = 10.35                                                  226
         AG_RMAX = 11.5                                                   226
         AG_DZ = 0.64/2.                                                  226
         AG_TITLE = 'TUBE'                                                226
         AG_EXNAME = 'SHAPE'                                              226
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      226
         CALL AXSHAPE                                                     226
C    *                                                                    226
5051  CALL AGSPOPB                                                        227
      GO TO BLOCK_FGCN, (5050 ,5060)                                      227
C End of Local Procedure                                                  227
C ----------------------------------------------------------------------  228
C ------------ FGCM                                                       228
C Begin Local Procedure FGCM                                              229
C FGCM NYLON AND AL RING                                                  229
5130  CONTINUE                                                            229
         AG_TITLE='FGCM'                                                  229
         AG_MODULE=MODULE                                                 229
         AG_MARK='V00'                                                    229
         CALL AGSPUSH                                                     229
         AG_MODULE=MODULE                                                 229
C    *                                                                    229
C    Geant ATTRIBUTE : FGCM ===> SEEN=1 COLO=6 <===                       230
         CALL AGSRESET                                                    230
         AG_SEEN = 1                                                      230
         AG_COLO = 6                                                      230
         AG_TITLE = 'FGCM'                                                230
         AG_EXNAME = 'ATTRIBUTE'                                          230
         AG_PARLIST = 'SEEN_COLO'                                         230
         CALL AXATTRIBUTE                                                 230
C    *                                                                    230
C    Geant MATERIAL : COOLMIX ===> inherited arguments used <===          230
         CALL AGSRESET                                                    231
         AG_TITLE = 'COOLMIX'                                             231
         AG_EXNAME = 'MATERIAL'                                           231
         AG_PARLIST = 'NONE'                                              231
         CALL AXMATERIAL                                                  231
C    *                                                                    231
C    Geant SHAPE : TUBE ===> RMIN=10.35 RMAX=11.54 DZ=1.28/2. <===        231
         CALL AGSRESET                                                    232
         AG_RMIN = 10.35                                                  232
         AG_RMAX = 11.54                                                  232
         AG_DZ = 1.28/2.                                                  232
         AG_TITLE = 'TUBE'                                                232
         AG_EXNAME = 'SHAPE'                                              232
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      232
         CALL AXSHAPE                                                     232
C    *                                                                    232
5131  CALL AGSPOPB                                                        233
      GO TO BLOCK_FGCM, (5130 ,5140)                                      233
C End of Local Procedure                                                  233
C ----------------------------------------------------------------------  234
C ------------ FGTD                                                       234
C Begin Local Procedure FGTD                                              235
C FGTD MOTHER VOLUME FOR FGT DISK                                         235
5080  CONTINUE                                                            235
         AG_TITLE='FGTD'                                                  235
         AG_MODULE=MODULE                                                 235
         AG_MARK='V00'                                                    235
         CALL AGSPUSH                                                     235
         AG_MODULE=MODULE                                                 235
C    *                                                                    235
C    Geant ATTRIBUTE : FGTD ===> SEEN=1 COLO=5 <===                       236
         CALL AGSRESET                                                    236
         AG_SEEN = 1                                                      236
         AG_COLO = 5                                                      236
         AG_TITLE = 'FGTD'                                                236
         AG_EXNAME = 'ATTRIBUTE'                                          236
         AG_PARLIST = 'SEEN_COLO'                                         236
         CALL AXATTRIBUTE                                                 236
C    *                                                                    236
C    Geant MATERIAL : AIR ===> inherited arguments used <===              236
         CALL AGSRESET                                                    237
         AG_TITLE = 'AIR'                                                 237
         AG_EXNAME = 'MATERIAL'                                           237
         AG_PARLIST = 'NONE'                                              237
         CALL AXMATERIAL                                                  237
C    *                                                                    237
C    Geant SHAPE : TUBE ===> RMIN=DISKINR RMAX=DISKOUTR DZ=DISKLENZ/2.0   237
C    <===                                                                 237
         CALL AGSRESET                                                    238
         AG_RMIN = DISKINR                                                238
         AG_RMAX = DISKOUTR                                               238
         AG_DZ = DISKLENZ/2.0                                             238
         AG_TITLE = 'TUBE'                                                238
         AG_EXNAME = 'SHAPE'                                              238
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      238
         CALL AXSHAPE                                                     238
C    *                                                                    238
C    *                                                                    238
         QUAD=1                                                           239
            GO TO 5153                                                    239
5151        QUAD=QUAD+1                                                   239
5153        IF(QUAD-(FGST_NQUAD).GT.0) GO TO 5152                         239
C       Create FGTQ :                                                     240
C       Call To Local Procedure FGTQ                                      240
            ASSIGN 5170 TO BLOCK_FGTQ                                     240
            GO TO  5160                                                   240
5170        CONTINUE                                                      240
C       *                                                                 240
C       Geant POSITION : FGTQ ===> Z=0. ALPHAZ=(1-QUAD)*90. <===          241
            CALL AGSRESET                                                 241
            AG_Z = 0.                                                     241
            AG_ALPHAN=AG_ALPHAN+1                                         241
            AG_ALPHAK(AG_ALPHAN)=3                                        241
            AG_ALPHAL(AG_ALPHAN)=(1-QUAD)*90.                             241
            AG_TITLE = 'FGTQ'                                             241
            AG_EXNAME = 'POSITION'                                        241
            AG_PARLIST = 'Z_ALPHAZ'                                       241
            CALL AXPOSITION                                               241
C       *                                                                 241
         GO TO 5151                                                       242
5152     CONTINUE                                                         242
5081  CALL AGSPOPB                                                        243
      GO TO BLOCK_FGTD, (5080 ,5090)                                      243
C End of Local Procedure                                                  243
C ----------------------------------------------------------------------  244
C ------------ FGTH                                                       244
C Begin Local Procedure FGTH                                              245
C FGTH MOTHER VOLUME FOR FGT DISK                                         245
5100  CONTINUE                                                            245
         AG_TITLE='FGTH'                                                  245
         AG_MODULE=MODULE                                                 245
         AG_MARK='V00'                                                    245
         CALL AGSPUSH                                                     245
         AG_MODULE=MODULE                                                 245
C    *                                                                    245
C    Geant ATTRIBUTE : FGTH ===> SEEN=1 COLO=7 <===                       246
         CALL AGSRESET                                                    246
         AG_SEEN = 1                                                      246
         AG_COLO = 7                                                      246
         AG_TITLE = 'FGTH'                                                246
         AG_EXNAME = 'ATTRIBUTE'                                          246
         AG_PARLIST = 'SEEN_COLO'                                         246
         CALL AXATTRIBUTE                                                 246
C    *                                                                    246
C    Geant MATERIAL : AIR ===> inherited arguments used <===              246
         CALL AGSRESET                                                    247
         AG_TITLE = 'AIR'                                                 247
         AG_EXNAME = 'MATERIAL'                                           247
         AG_PARLIST = 'NONE'                                              247
         CALL AXMATERIAL                                                  247
C    *                                                                    247
C    Geant SHAPE : TUBE ===> RMIN=DISKINR RMAX=DISKOUTR DZ=DISKLENZ/2.0   247
C    <===                                                                 247
         CALL AGSRESET                                                    248
         AG_RMIN = DISKINR                                                248
         AG_RMAX = DISKOUTR                                               248
         AG_DZ = DISKLENZ/2.0                                             248
         AG_TITLE = 'TUBE'                                                248
         AG_EXNAME = 'SHAPE'                                              248
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      248
         CALL AXSHAPE                                                     248
C    *                                                                    248
C    *                                                                    248
         QUAD=1                                                           249
            GO TO 5183                                                    249
5181        QUAD=QUAD+1                                                   249
5183        IF(QUAD-(2).GT.0) GO TO 5182                                  249
C       Create FGTQ :                                                     250
C       Call To Local Procedure FGTQ                                      250
            ASSIGN 5190 TO BLOCK_FGTQ                                     250
            GO TO  5160                                                   250
5190        CONTINUE                                                      250
C       *                                                                 250
C       Geant POSITION : FGTQ ===> Z=0. ALPHAZ=(1-QUAD)*90. <===          251
            CALL AGSRESET                                                 251
            AG_Z = 0.                                                     251
            AG_ALPHAN=AG_ALPHAN+1                                         251
            AG_ALPHAK(AG_ALPHAN)=3                                        251
            AG_ALPHAL(AG_ALPHAN)=(1-QUAD)*90.                             251
            AG_TITLE = 'FGTQ'                                             251
            AG_EXNAME = 'POSITION'                                        251
            AG_PARLIST = 'Z_ALPHAZ'                                       251
            CALL AXPOSITION                                               251
C       *                                                                 251
         GO TO 5181                                                       252
5182     CONTINUE                                                         252
5101  CALL AGSPOPB                                                        253
      GO TO BLOCK_FGTH, (5100 ,5110)                                      253
C End of Local Procedure                                                  253
C ----------------------------------------------------------------------  254
C ------------ FGTQ                                                       254
C Begin Local Procedure FGTQ                                              255
C FGTQ QUADRANT                                                           255
5160  CONTINUE                                                            255
         AG_TITLE='FGTQ'                                                  255
         AG_MODULE=MODULE                                                 255
         AG_MARK='V00'                                                    255
         CALL AGSPUSH                                                     255
         AG_MODULE=MODULE                                                 255
C    *                                                                    255
C    Geant ATTRIBUTE : FGTQ ===> SEEN=0 COLO=4 <===                       256
         CALL AGSRESET                                                    256
         AG_SEEN = 0                                                      256
         AG_COLO = 4                                                      256
         AG_TITLE = 'FGTQ'                                                256
         AG_EXNAME = 'ATTRIBUTE'                                          256
         AG_PARLIST = 'SEEN_COLO'                                         256
         CALL AXATTRIBUTE                                                 256
C    *                                                                    256
C    Geant MATERIAL : AIR ===> inherited arguments used <===              256
         CALL AGSRESET                                                    257
         AG_TITLE = 'AIR'                                                 257
         AG_EXNAME = 'MATERIAL'                                           257
         AG_PARLIST = 'NONE'                                              257
         CALL AXMATERIAL                                                  257
C    *                                                                    257
C    Geant SHAPE : TUBS ===> RMIN=DISKINR RMAX=DISKOUTR PHI1=0., PHI2=90  257
C    . DZ=DISKLENZ/2. <===                                                257
         CALL AGSRESET                                                    259
         AG_RMIN = DISKINR                                                259
         AG_RMAX = DISKOUTR                                               259
         AG_PHI1 = 0.                                                     259
         AG_PHI2 = 90.                                                    259
         AG_DZ = DISKLENZ/2.                                              259
         AG_TITLE = 'TUBS'                                                259
         AG_EXNAME = 'SHAPE'                                              259
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            259
         CALL AXSHAPE                                                     259
C    *                                                                    259
C    Create FGVN :                                                        260
C    Call To Local Procedure FGVN                                         260
         ASSIGN 5210 TO BLOCK_FGVN                                        260
         GO TO  5200                                                      260
5210     CONTINUE                                                         260
C    *                                                                    260
C    Geant POSITION : FGVN ===> Z=-DISKLENZ/2.+VOLNOMEXDZ/2. <===         261
         CALL AGSRESET                                                    261
         AG_Z = -DISKLENZ/2.+VOLNOMEXDZ/2.                                261
         AG_TITLE = 'FGVN'                                                261
         AG_EXNAME = 'POSITION'                                           261
         AG_PARLIST = 'Z'                                                 261
         CALL AXPOSITION                                                  261
C    *                                                                    261
C    Create FGZC :                                                        262
C    Call To Local Procedure FGZC                                         262
         ASSIGN 5230 TO BLOCK_FGZC                                        262
         GO TO  5220                                                      262
5230     CONTINUE                                                         262
C    *                                                                    262
C    Geant POSITION : FGZC ===> Z=-DISKLENZ/2.+VOLNOMEXDZ+VOLSENSDZ/2. <  263
C    ===                                                                  263
         CALL AGSRESET                                                    263
         AG_Z = -DISKLENZ/2.+VOLNOMEXDZ+VOLSENSDZ/2.                      263
         AG_TITLE = 'FGZC'                                                263
         AG_EXNAME = 'POSITION'                                           263
         AG_PARLIST = 'Z'                                                 263
         CALL AXPOSITION                                                  263
C    *                                                                    263
C    Create FGVG :                                                        264
C    Call To Local Procedure FGVG                                         264
         ASSIGN 5250 TO BLOCK_FGVG                                        264
         GO TO  5240                                                      264
5250     CONTINUE                                                         264
C    *                                                                    264
C    Geant POSITION : FGVG ===> Z=-DISKLENZ/2.+VOLNOMEXDZ+VOLSENSDZ+VOLG  265
C    EMDZ/2. <===                                                         265
         CALL AGSRESET                                                    265
         AG_Z = -DISKLENZ/2.+VOLNOMEXDZ+VOLSENSDZ+VOLGEMDZ/2.             265
         AG_TITLE = 'FGVG'                                                265
         AG_EXNAME = 'POSITION'                                           265
         AG_PARLIST = 'Z'                                                 265
         CALL AXPOSITION                                                  265
C    *                                                                    265
C    Create FGVR :                                                        266
C    Call To Local Procedure FGVR                                         266
         ASSIGN 5270 TO BLOCK_FGVR                                        266
         GO TO  5260                                                      266
5270     CONTINUE                                                         266
C    *                                                                    266
C    Geant POSITION : FGVR ===> Z=-DISKLENZ/2.+VOLNOMEXDZ+VOLSENSDZ+VOLG  267
C    EMDZ+VOLREADDZ/2. <===                                               267
         CALL AGSRESET                                                    267
         AG_Z = -DISKLENZ/2.+VOLNOMEXDZ+VOLSENSDZ+VOLGEMDZ+VOLREADDZ/2.   267
         AG_TITLE = 'FGVR'                                                267
         AG_EXNAME = 'POSITION'                                           267
         AG_PARLIST = 'Z'                                                 267
         CALL AXPOSITION                                                  267
C    *                                                                    267
C    Create FGVE :                                                        268
C    Call To Local Procedure FGVE                                         268
         ASSIGN 5290 TO BLOCK_FGVE                                        268
         GO TO  5280                                                      268
5290     CONTINUE                                                         268
C    *                                                                    268
C    Geant POSITION : FGVE ===> Z=-DISKLENZ/2.+VOLNOMEXDZ+VOLSENSDZ+VOLG  270
C    EMDZ+VOLREADDZ+VOLELECDZ/2. <===                                     270
         CALL AGSRESET                                                    270
         AG_Z = -DISKLENZ/                                                270
     *   2.+VOLNOMEXDZ+VOLSENSDZ+VOLGEMDZ+VOLREADDZ+VOLELECDZ/2.          270
         AG_TITLE = 'FGVE'                                                270
         AG_EXNAME = 'POSITION'                                           270
         AG_PARLIST = 'Z'                                                 270
         CALL AXPOSITION                                                  270
C    *                                                                    270
5161  CALL AGSPOPB                                                        271
      GO TO BLOCK_FGTQ, (5160 ,5170,5190)                                 271
C End of Local Procedure                                                  271
C ----------------------------------------------------------------------  272
C ------------ FGVN                                                       272
C Begin Local Procedure FGVN                                              273
C FGVN QUAD NOMEX MIXTURE VOLUME                                          273
5200  CONTINUE                                                            273
         AG_TITLE='FGVN'                                                  273
         AG_MODULE=MODULE                                                 273
         AG_MARK='V00'                                                    273
         CALL AGSPUSH                                                     273
         AG_MODULE=MODULE                                                 273
C    *                                                                    273
C    Geant ATTRIBUTE : FGVN ===> SEEN=0 COLO=5 <===                       274
         CALL AGSRESET                                                    274
         AG_SEEN = 0                                                      274
         AG_COLO = 5                                                      274
         AG_TITLE = 'FGVN'                                                274
         AG_EXNAME = 'ATTRIBUTE'                                          274
         AG_PARLIST = 'SEEN_COLO'                                         274
         CALL AXATTRIBUTE                                                 274
C    *                                                                    274
C    Geant MATERIAL : NOMEXMIX ===> inherited arguments used <===         274
         CALL AGSRESET                                                    275
         AG_TITLE = 'NOMEXMIX'                                            275
         AG_EXNAME = 'MATERIAL'                                           275
         AG_PARLIST = 'NONE'                                              275
         CALL AXMATERIAL                                                  275
C    *                                                                    275
C    Geant SHAPE : TUBS ===> RMIN=DISKINR RMAX=DISKOUTR PHI1=0., PHI2=90  275
C    . DZ=VOLNOMEXDZ/2. <===                                              275
         CALL AGSRESET                                                    277
         AG_RMIN = DISKINR                                                277
         AG_RMAX = DISKOUTR                                               277
         AG_PHI1 = 0.                                                     277
         AG_PHI2 = 90.                                                    277
         AG_DZ = VOLNOMEXDZ/2.                                            277
         AG_TITLE = 'TUBS'                                                277
         AG_EXNAME = 'SHAPE'                                              277
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            277
         CALL AXSHAPE                                                     277
C    *                                                                    277
C    *                                                                    277
         K=0                                                              278
            GO TO 5303                                                    278
5301        K=K+1                                                         278
5303        IF(K-(2).GT.0) GO TO 5302                                     278
            ZZFGZ=(-VOLNOMEXDZ+FRAMEDZ)/2.                                279
C       Check K.EQ.1                                                      280
            IF (K.EQ.1) THEN                                              280
            ZZFGZ=(VOLNOMEXDZ-FRAMEDZ)/2.-0.007                           281
            ENDIF                                                         282
C       Check K.EQ.2                                                      283
            IF (K.EQ.2) THEN                                              283
            ZZFGZ=(VOLNOMEXDZ-FRAMEDZ)/2.-0.014 - FRAMEDZ                 284
            ENDIF                                                         285
C       Create FGQA :                                                     286
C       Call To Local Procedure FGQA                                      286
            ASSIGN 5320 TO BLOCK_FGQA                                     286
            GO TO  5310                                                   286
5320        CONTINUE                                                      286
C       *                                                                 286
C       Geant POSITION : FGQA ===> X=FRAMEWIDTH/2.+FRAMEXYOFF Y=(DISKOUT  287
C       R+DISKINR)/2. Z=ZZFGZ <===                                        287
            CALL AGSRESET                                                 287
            AG_X = FRAMEWIDTH/2.+FRAMEXYOFF                               287
            AG_Y = (DISKOUTR+DISKINR)/2.                                  287
            AG_Z = ZZFGZ                                                  287
            AG_TITLE = 'FGQA'                                             287
            AG_EXNAME = 'POSITION'                                        287
            AG_PARLIST = 'X_Y_Z'                                          287
            CALL AXPOSITION                                               287
C       *                                                                 287
C       Create FGQB :                                                     288
C       Call To Local Procedure FGQB                                      288
            ASSIGN 5340 TO BLOCK_FGQB                                     288
            GO TO  5330                                                   288
5340        CONTINUE                                                      288
C       *                                                                 288
C       Geant POSITION : FGQB ===> Z=ZZFGZ <===                           289
            CALL AGSRESET                                                 289
            AG_Z = ZZFGZ                                                  289
            AG_TITLE = 'FGQB'                                             289
            AG_EXNAME = 'POSITION'                                        289
            AG_PARLIST = 'Z'                                              289
            CALL AXPOSITION                                               289
C       *                                                                 289
C       Create FGQC :                                                     290
C       Call To Local Procedure FGQC                                      290
            ASSIGN 5360 TO BLOCK_FGQC                                     290
            GO TO  5350                                                   290
5360        CONTINUE                                                      290
C       *                                                                 290
C       Geant POSITION : FGQC ===> X=(DISKOUTR+DISKINR)/2. Y=FRAMEWIDTH/  291
C       2.+FRAMEXYOFF Z=ZZFGZ <===                                        291
            CALL AGSRESET                                                 291
            AG_X = (DISKOUTR+DISKINR)/2.                                  291
            AG_Y = FRAMEWIDTH/2.+FRAMEXYOFF                               291
            AG_Z = ZZFGZ                                                  291
            AG_TITLE = 'FGQC'                                             291
            AG_EXNAME = 'POSITION'                                        291
            AG_PARLIST = 'X_Y_Z'                                          291
            CALL AXPOSITION                                               291
C       *                                                                 291
C       Create FGQD :                                                     292
C       Call To Local Procedure FGQD                                      292
            ASSIGN 5380 TO BLOCK_FGQD                                     292
            GO TO  5370                                                   292
5380        CONTINUE                                                      292
C       *                                                                 292
C       Geant POSITION : FGQD ===> Z=ZZFGZ <===                           293
            CALL AGSRESET                                                 293
            AG_Z = ZZFGZ                                                  293
            AG_TITLE = 'FGQD'                                             293
            AG_EXNAME = 'POSITION'                                        293
            AG_PARLIST = 'Z'                                              293
            CALL AXPOSITION                                               293
C       *                                                                 293
C       Create FGQE :                                                     294
C       Call To Local Procedure FGQE                                      294
            ASSIGN 5400 TO BLOCK_FGQE                                     294
            GO TO  5390                                                   294
5400        CONTINUE                                                      294
C       *                                                                 294
C       Geant POSITION : FGQE ===> X=(FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPE  297
C       RPANG*DEGRAD) Y=(FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD)  297
C        Z=ZZFGZ ALPHAZ=FLATPERPANG-90. <===                              297
            CALL AGSRESET                                                 297
            AG_X = (FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPERPANG*DEGRAD)       297
            AG_Y = (FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD)       297
            AG_Z = ZZFGZ                                                  297
            AG_ALPHAN=AG_ALPHAN+1                                         297
            AG_ALPHAK(AG_ALPHAN)=3                                        297
            AG_ALPHAL(AG_ALPHAN)=FLATPERPANG-90.                          297
            AG_TITLE = 'FGQE'                                             297
            AG_EXNAME = 'POSITION'                                        297
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   297
            CALL AXPOSITION                                               297
C       *                                                                 297
C       Create FGQF :                                                     298
C       Call To Local Procedure FGQF                                      298
            ASSIGN 5420 TO BLOCK_FGQF                                     298
            GO TO  5410                                                   298
5420        CONTINUE                                                      298
C       *                                                                 298
C       Geant POSITION : FGQF ===> Z=ZZFGZ <===                           299
            CALL AGSRESET                                                 299
            AG_Z = ZZFGZ                                                  299
            AG_TITLE = 'FGQF'                                             299
            AG_EXNAME = 'POSITION'                                        299
            AG_PARLIST = 'Z'                                              299
            CALL AXPOSITION                                               299
C       *                                                                 299
         GO TO 5301                                                       300
5302     CONTINUE                                                         300
5201  CALL AGSPOPB                                                        301
      GO TO BLOCK_FGVN, (5200 ,5210)                                      301
C End of Local Procedure                                                  301
C ----------------------------------------------------------------------  302
C ------------ FGZC                                                       302
C Begin Local Procedure FGZC                                              303
C FGZC SENSITIVE VOLUME                                                   303
5220  CONTINUE                                                            303
         AG_TITLE='FGZC'                                                  303
         AG_MODULE=MODULE                                                 303
         AG_MARK='V00'                                                    303
         CALL AGSPUSH                                                     303
         AG_MODULE=MODULE                                                 303
C    *                                                                    303
C    Geant ATTRIBUTE : FGZC ===> SEEN=1 COLO=1 <===                       304
         CALL AGSRESET                                                    304
         AG_SEEN = 1                                                      304
         AG_COLO = 1                                                      304
         AG_TITLE = 'FGZC'                                                304
         AG_EXNAME = 'ATTRIBUTE'                                          304
         AG_PARLIST = 'SEEN_COLO'                                         304
         CALL AXATTRIBUTE                                                 304
C    *                                                                    304
C    Geant MATERIAL : ARCO2MIX ===> inherited arguments used <===         304
         CALL AGSRESET                                                    305
         AG_TITLE = 'ARCO2MIX'                                            305
         AG_EXNAME = 'MATERIAL'                                           305
         AG_PARLIST = 'NONE'                                              305
         CALL AXMATERIAL                                                  305
C    *                                                                    305
C    Geant MATERIAL : SENSITIVE_FGT_GAS ===> ISVOL=1 <===                 305
         CALL AGSRESET                                                    306
         AG_ISVOL = 1                                                     306
         AG_TITLE = 'SENSITIVE_FGT_GAS'                                   306
         AG_EXNAME = 'MATERIAL'                                           306
         AG_PARLIST = 'ISVOL'                                             306
         CALL AXMATERIAL                                                  306
C    *                                                                    306
C    Geant SHAPE : TUBS ===> RMIN=DISKINR RMAX=DISKOUTR PHI1=0., PHI2=90  306
C    . DZ=VOLSENSDZ/2. <===                                               306
         CALL AGSRESET                                                    308
         AG_RMIN = DISKINR                                                308
         AG_RMAX = DISKOUTR                                               308
         AG_PHI1 = 0.                                                     308
         AG_PHI2 = 90.                                                    308
         AG_DZ = VOLSENSDZ/2.                                             308
         AG_TITLE = 'TUBS'                                                308
         AG_EXNAME = 'SHAPE'                                              308
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            308
         CALL AXSHAPE                                                     308
C    *                                                                    308
C    Defining Hits: ZZ:0.001:S YY:0.001: XX:0.001: PTOT:16:(0,100) CX:10  308
C    : CY:10: CZ:10:, SLENG:16:(0,500) TOF:16:(0,1.0E-6) STEP:0.01: ELOS  308
C    S:16:(0,0.001) PX:16:(-250.0,+250.0), PY:16:(-250.0,+250.0) PZ:16:(  308
C    -250.0,+250.0)                                                       308
         AG_TITLE = 'FGZC'                                                311
         AG_EXNAME = 'HITS'                                               311
         AG_TYPE=AG_MAGIC                                                 311
         CALL AGSDETV('FGZC')                                             311
C    *                                                                    311
         AG_PARLIST='ZZ'                                                  311
         AG_TITLE = 'DETH'                                                311
         AG_NBITS = AG_MAGIC                                              311
         AG_BIN=0.001                                                     311
         AG_OPTION='S'                                                    311
         AG_FHMIN = AG_MAGIC                                              311
         AG_FHMAX = AG_MAGIC                                              311
         CALL AGSDETH (0)                                                 311
C    *                                                                    311
         AG_PARLIST='YY'                                                  311
         AG_TITLE = 'DETH'                                                311
         AG_NBITS = AG_MAGIC                                              311
         AG_BIN=0.001                                                     311
         AG_FHMIN=AG_MAGIC                                                311
         AG_FHMAX=AG_MAGIC                                                311
         AG_OPTION='*'                                                    311
         CALL AGSDETH (0)                                                 311
C    *                                                                    311
         AG_PARLIST='XX'                                                  311
         AG_TITLE = 'DETH'                                                311
         AG_NBITS = AG_MAGIC                                              311
         AG_BIN=0.001                                                     311
         AG_FHMIN=AG_MAGIC                                                311
         AG_FHMAX=AG_MAGIC                                                311
         AG_OPTION='*'                                                    311
         CALL AGSDETH (0)                                                 311
C    *                                                                    311
         AG_PARLIST='PTOT'                                                311
         AG_TITLE = 'DETH'                                                311
         AG_NBITS = 16                                                    311
         AG_BIN=AG_MAGIC                                                  311
         AG_OPTION='*'                                                    311
         AG_FHMIN = 0                                                     311
         AG_FHMAX = 100                                                   311
         CALL AGSDETH (0)                                                 311
C    *                                                                    311
         AG_PARLIST='CX'                                                  311
         AG_TITLE = 'DETH'                                                311
         AG_NBITS = 10                                                    311
         AG_BIN=AG_MAGIC                                                  311
         AG_FHMIN=AG_MAGIC                                                311
         AG_FHMAX=AG_MAGIC                                                311
         AG_OPTION='*'                                                    311
         CALL AGSDETH (0)                                                 311
C    *                                                                    311
         AG_PARLIST='CY'                                                  311
         AG_TITLE = 'DETH'                                                311
         AG_NBITS = 10                                                    311
         AG_BIN=AG_MAGIC                                                  311
         AG_FHMIN=AG_MAGIC                                                311
         AG_FHMAX=AG_MAGIC                                                311
         AG_OPTION='*'                                                    311
         CALL AGSDETH (0)                                                 311
C    *                                                                    311
         AG_PARLIST='CZ'                                                  311
         AG_TITLE = 'DETH'                                                311
         AG_NBITS = 10                                                    311
         AG_BIN=AG_MAGIC                                                  311
         AG_OPTION='*'                                                    311
         AG_FHMIN = AG_MAGIC                                              311
         AG_FHMAX = AG_MAGIC                                              311
         CALL AGSDETH (0)                                                 311
C    *                                                                    311
         AG_PARLIST='SLENG'                                               311
         AG_TITLE = 'DETH'                                                311
         AG_NBITS = 16                                                    311
         AG_BIN=AG_MAGIC                                                  311
         AG_OPTION='*'                                                    311
         AG_FHMIN = 0                                                     311
         AG_FHMAX = 500                                                   311
         CALL AGSDETH (0)                                                 311
C    *                                                                    311
         AG_PARLIST='TOF'                                                 311
         AG_TITLE = 'DETH'                                                311
         AG_NBITS = 16                                                    311
         AG_BIN=AG_MAGIC                                                  311
         AG_OPTION='*'                                                    311
         AG_FHMIN = 0                                                     311
         AG_FHMAX = 1.0E-6                                                311
         CALL AGSDETH (0)                                                 311
C    *                                                                    311
         AG_PARLIST='STEP'                                                311
         AG_TITLE = 'DETH'                                                311
         AG_NBITS = AG_MAGIC                                              311
         AG_BIN=0.01                                                      311
         AG_FHMIN=AG_MAGIC                                                311
         AG_FHMAX=AG_MAGIC                                                311
         AG_OPTION='*'                                                    311
         CALL AGSDETH (0)                                                 311
C    *                                                                    311
         AG_PARLIST='ELOSS'                                               311
         AG_TITLE = 'DETH'                                                311
         AG_NBITS = 16                                                    311
         AG_BIN=AG_MAGIC                                                  311
         AG_OPTION='*'                                                    311
         AG_FHMIN = 0                                                     311
         AG_FHMAX = 0.001                                                 311
         CALL AGSDETH (0)                                                 311
C    *                                                                    311
         AG_PARLIST='PX'                                                  311
         AG_TITLE = 'DETH'                                                311
         AG_NBITS = 16                                                    311
         AG_BIN=AG_MAGIC                                                  311
         AG_OPTION='*'                                                    311
         AG_FHMIN = -250.0                                                311
         AG_FHMAX = +250.0                                                311
         CALL AGSDETH (0)                                                 311
C    *                                                                    311
         AG_PARLIST='PY'                                                  311
         AG_TITLE = 'DETH'                                                311
         AG_NBITS = 16                                                    311
         AG_BIN=AG_MAGIC                                                  311
         AG_OPTION='*'                                                    311
         AG_FHMIN = -250.0                                                311
         AG_FHMAX = +250.0                                                311
         CALL AGSDETH (0)                                                 311
C    *                                                                    311
         AG_PARLIST='PZ'                                                  311
         AG_TITLE = 'DETH'                                                311
         AG_NBITS = 16                                                    311
         AG_BIN=AG_MAGIC                                                  311
         AG_OPTION='*'                                                    311
         AG_FHMIN = -250.0                                                311
         AG_FHMAX = +250.0                                                311
         CALL AGSDETH (0)                                                 311
C    *                                                                    311
         CALL AGSDETU('FGZC')                                             311
C    *                                                                    311
C    *                                                                    312
         K=0                                                              312
            GO TO 5433                                                    312
5431        K=K+1                                                         312
5433        IF(K-(1).GT.0) GO TO 5432                                     312
            ZZFGZ=(-VOLSENSDZ+FRAMEDZ)/2. * (1.-2.*K)                     313
C       Create FGQA :                                                     314
C       Call To Local Procedure FGQA                                      314
            ASSIGN 5440 TO BLOCK_FGQA                                     314
            GO TO  5310                                                   314
5440        CONTINUE                                                      314
C       *                                                                 314
C       Geant POSITION : FGQA ===> X=FRAMEWIDTH/2.+FRAMEXYOFF Y=(DISKOUT  315
C       R+DISKINR)/2. Z=ZZFGZ <===                                        315
            CALL AGSRESET                                                 315
            AG_X = FRAMEWIDTH/2.+FRAMEXYOFF                               315
            AG_Y = (DISKOUTR+DISKINR)/2.                                  315
            AG_Z = ZZFGZ                                                  315
            AG_TITLE = 'FGQA'                                             315
            AG_EXNAME = 'POSITION'                                        315
            AG_PARLIST = 'X_Y_Z'                                          315
            CALL AXPOSITION                                               315
C       *                                                                 315
C       Create FGQB :                                                     316
C       Call To Local Procedure FGQB                                      316
            ASSIGN 5450 TO BLOCK_FGQB                                     316
            GO TO  5330                                                   316
5450        CONTINUE                                                      316
C       *                                                                 316
C       Geant POSITION : FGQB ===> Z=ZZFGZ <===                           317
            CALL AGSRESET                                                 317
            AG_Z = ZZFGZ                                                  317
            AG_TITLE = 'FGQB'                                             317
            AG_EXNAME = 'POSITION'                                        317
            AG_PARLIST = 'Z'                                              317
            CALL AXPOSITION                                               317
C       *                                                                 317
C       Create FGQC :                                                     318
C       Call To Local Procedure FGQC                                      318
            ASSIGN 5460 TO BLOCK_FGQC                                     318
            GO TO  5350                                                   318
5460        CONTINUE                                                      318
C       *                                                                 318
C       Geant POSITION : FGQC ===> X=(DISKOUTR+DISKINR)/2. Y=FRAMEWIDTH/  319
C       2.+FRAMEXYOFF Z=ZZFGZ <===                                        319
            CALL AGSRESET                                                 319
            AG_X = (DISKOUTR+DISKINR)/2.                                  319
            AG_Y = FRAMEWIDTH/2.+FRAMEXYOFF                               319
            AG_Z = ZZFGZ                                                  319
            AG_TITLE = 'FGQC'                                             319
            AG_EXNAME = 'POSITION'                                        319
            AG_PARLIST = 'X_Y_Z'                                          319
            CALL AXPOSITION                                               319
C       *                                                                 319
C       Create FGQD :                                                     320
C       Call To Local Procedure FGQD                                      320
            ASSIGN 5470 TO BLOCK_FGQD                                     320
            GO TO  5370                                                   320
5470        CONTINUE                                                      320
C       *                                                                 320
C       Geant POSITION : FGQD ===> Z=ZZFGZ <===                           321
            CALL AGSRESET                                                 321
            AG_Z = ZZFGZ                                                  321
            AG_TITLE = 'FGQD'                                             321
            AG_EXNAME = 'POSITION'                                        321
            AG_PARLIST = 'Z'                                              321
            CALL AXPOSITION                                               321
C       *                                                                 321
C       Create FGQE :                                                     322
C       Call To Local Procedure FGQE                                      322
            ASSIGN 5480 TO BLOCK_FGQE                                     322
            GO TO  5390                                                   322
5480        CONTINUE                                                      322
C       *                                                                 322
C       Geant POSITION : FGQE ===> X=(FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPE  325
C       RPANG*DEGRAD) Y=(FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD)  325
C        Z=ZZFGZ ALPHAZ=FLATPERPANG-90. <===                              325
            CALL AGSRESET                                                 325
            AG_X = (FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPERPANG*DEGRAD)       325
            AG_Y = (FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD)       325
            AG_Z = ZZFGZ                                                  325
            AG_ALPHAN=AG_ALPHAN+1                                         325
            AG_ALPHAK(AG_ALPHAN)=3                                        325
            AG_ALPHAL(AG_ALPHAN)=FLATPERPANG-90.                          325
            AG_TITLE = 'FGQE'                                             325
            AG_EXNAME = 'POSITION'                                        325
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   325
            CALL AXPOSITION                                               325
C       *                                                                 325
C       Create FGQF :                                                     326
C       Call To Local Procedure FGQF                                      326
            ASSIGN 5490 TO BLOCK_FGQF                                     326
            GO TO  5410                                                   326
5490        CONTINUE                                                      326
C       *                                                                 326
C       Geant POSITION : FGQF ===> Z=ZZFGZ <===                           327
            CALL AGSRESET                                                 327
            AG_Z = ZZFGZ                                                  327
            AG_TITLE = 'FGQF'                                             327
            AG_EXNAME = 'POSITION'                                        327
            AG_PARLIST = 'Z'                                              327
            CALL AXPOSITION                                               327
C       *                                                                 327
C       Create FGXA :                                                     328
C       Call To Local Procedure FGXA                                      328
            ASSIGN 5510 TO BLOCK_FGXA                                     328
            GO TO  5500                                                   328
5510        CONTINUE                                                      328
C       *                                                                 328
C       Geant POSITION : FGXA ===> Z=ZZFGZ <===                           329
            CALL AGSRESET                                                 329
            AG_Z = ZZFGZ                                                  329
            AG_TITLE = 'FGXA'                                             329
            AG_EXNAME = 'POSITION'                                        329
            AG_PARLIST = 'Z'                                              329
            CALL AXPOSITION                                               329
C       *                                                                 329
C       Create FGXB :                                                     330
C       Call To Local Procedure FGXB                                      330
            ASSIGN 5530 TO BLOCK_FGXB                                     330
            GO TO  5520                                                   330
5530        CONTINUE                                                      330
C       *                                                                 330
C       Geant POSITION : FGXB ===> Z=ZZFGZ <===                           331
            CALL AGSRESET                                                 331
            AG_Z = ZZFGZ                                                  331
            AG_TITLE = 'FGXB'                                             331
            AG_EXNAME = 'POSITION'                                        331
            AG_PARLIST = 'Z'                                              331
            CALL AXPOSITION                                               331
C       *                                                                 331
C       Create FGXC :                                                     332
C       Call To Local Procedure FGXC                                      332
            ASSIGN 5550 TO BLOCK_FGXC                                     332
            GO TO  5540                                                   332
5550        CONTINUE                                                      332
C       *                                                                 332
            XXFGZ=(DISKOUTR+DISKINR)/2.                                   333
C       Geant POSITION : FGXC ===> X=XXFGZ*COS(GRIDRAYLONGPHI*DEGRAD) Y=  335
C       XXFGZ*SIN(GRIDRAYLONGPHI*DEGRAD) Z=ZZFGZ ALPHAZ=GRIDRAYLONGPHI <  335
C       ===                                                               335
            CALL AGSRESET                                                 335
            AG_X = XXFGZ*COS(GRIDRAYLONGPHI*DEGRAD)                       335
            AG_Y = XXFGZ*SIN(GRIDRAYLONGPHI*DEGRAD)                       335
            AG_Z = ZZFGZ                                                  335
            AG_ALPHAN=AG_ALPHAN+1                                         335
            AG_ALPHAK(AG_ALPHAN)=3                                        335
            AG_ALPHAL(AG_ALPHAN)=GRIDRAYLONGPHI                           335
            AG_TITLE = 'FGXC'                                             335
            AG_EXNAME = 'POSITION'                                        335
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   335
            CALL AXPOSITION                                               335
C       *                                                                 335
C       Create FGXD :                                                     336
C       Call To Local Procedure FGXD                                      336
            ASSIGN 5570 TO BLOCK_FGXD                                     336
            GO TO  5560                                                   336
5570        CONTINUE                                                      336
C       *                                                                 336
            XXFGZ=(FLATOUTR+DISKINR)/2.                                   337
C       Geant POSITION : FGXD ===> X=XXFGZ*COS(GRIDRAYSHORTPHI*DEGRAD) Y  339
C       =XXFGZ*SIN(GRIDRAYSHORTPHI*DEGRAD) Z=ZZFGZ ALPHAZ=GRIDRAYSHORTPH  339
C       I <===                                                            339
            CALL AGSRESET                                                 339
            AG_X = XXFGZ*COS(GRIDRAYSHORTPHI*DEGRAD)                      339
            AG_Y = XXFGZ*SIN(GRIDRAYSHORTPHI*DEGRAD)                      339
            AG_Z = ZZFGZ                                                  339
            AG_ALPHAN=AG_ALPHAN+1                                         339
            AG_ALPHAK(AG_ALPHAN)=3                                        339
            AG_ALPHAL(AG_ALPHAN)=GRIDRAYSHORTPHI                          339
            AG_TITLE = 'FGXD'                                             339
            AG_EXNAME = 'POSITION'                                        339
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   339
            CALL AXPOSITION                                               339
C       *                                                                 339
         GO TO 5431                                                       340
5432     CONTINUE                                                         340
5221  CALL AGSPOPB                                                        341
      GO TO BLOCK_FGZC, (5220 ,5230)                                      341
C End of Local Procedure                                                  341
C ----------------------------------------------------------------------  342
C ------------ FGVG                                                       342
C Begin Local Procedure FGVG                                              343
C FGVG QUAD 3GEM MIXTURE VOLUME                                           343
5240  CONTINUE                                                            343
         AG_TITLE='FGVG'                                                  343
         AG_MODULE=MODULE                                                 343
         AG_MARK='V00'                                                    343
         CALL AGSPUSH                                                     343
         AG_MODULE=MODULE                                                 343
C    *                                                                    343
C    Geant ATTRIBUTE : FGVG ===> SEEN=0 COLO=6 <===                       344
         CALL AGSRESET                                                    344
         AG_SEEN = 0                                                      344
         AG_COLO = 6                                                      344
         AG_TITLE = 'FGVG'                                                344
         AG_EXNAME = 'ATTRIBUTE'                                          344
         AG_PARLIST = 'SEEN_COLO'                                         344
         CALL AXATTRIBUTE                                                 344
C    *                                                                    344
C    Geant MATERIAL : GEMMIX ===> inherited arguments used <===           344
         CALL AGSRESET                                                    345
         AG_TITLE = 'GEMMIX'                                              345
         AG_EXNAME = 'MATERIAL'                                           345
         AG_PARLIST = 'NONE'                                              345
         CALL AXMATERIAL                                                  345
C    *                                                                    345
C    Geant SHAPE : TUBS ===> RMIN=DISKINR RMAX=DISKOUTR PHI1=0., PHI2=90  345
C    . DZ=VOLGEMDZ/2. <===                                                345
         CALL AGSRESET                                                    347
         AG_RMIN = DISKINR                                                347
         AG_RMAX = DISKOUTR                                               347
         AG_PHI1 = 0.                                                     347
         AG_PHI2 = 90.                                                    347
         AG_DZ = VOLGEMDZ/2.                                              347
         AG_TITLE = 'TUBS'                                                347
         AG_EXNAME = 'SHAPE'                                              347
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            347
         CALL AXSHAPE                                                     347
C    *                                                                    347
C    *                                                                    347
         K=0                                                              348
            GO TO 5583                                                    348
5581        K=K+1                                                         348
5583        IF(K-(2).GT.0) GO TO 5582                                     348
            ZZFGZ=(-VOLGEMDZ+FRAMEDZ-0.01)/2. * (-1+K)                    349
C       Create FGQA :                                                     350
C       Call To Local Procedure FGQA                                      350
            ASSIGN 5590 TO BLOCK_FGQA                                     350
            GO TO  5310                                                   350
5590        CONTINUE                                                      350
C       *                                                                 350
C       Geant POSITION : FGQA ===> X=FRAMEWIDTH/2.+FRAMEXYOFF Y=(DISKOUT  351
C       R+DISKINR)/2. Z=ZZFGZ <===                                        351
            CALL AGSRESET                                                 351
            AG_X = FRAMEWIDTH/2.+FRAMEXYOFF                               351
            AG_Y = (DISKOUTR+DISKINR)/2.                                  351
            AG_Z = ZZFGZ                                                  351
            AG_TITLE = 'FGQA'                                             351
            AG_EXNAME = 'POSITION'                                        351
            AG_PARLIST = 'X_Y_Z'                                          351
            CALL AXPOSITION                                               351
C       *                                                                 351
C       Create FGQB :                                                     352
C       Call To Local Procedure FGQB                                      352
            ASSIGN 5600 TO BLOCK_FGQB                                     352
            GO TO  5330                                                   352
5600        CONTINUE                                                      352
C       *                                                                 352
C       Geant POSITION : FGQB ===> Z=ZZFGZ <===                           353
            CALL AGSRESET                                                 353
            AG_Z = ZZFGZ                                                  353
            AG_TITLE = 'FGQB'                                             353
            AG_EXNAME = 'POSITION'                                        353
            AG_PARLIST = 'Z'                                              353
            CALL AXPOSITION                                               353
C       *                                                                 353
C       Create FGQC :                                                     354
C       Call To Local Procedure FGQC                                      354
            ASSIGN 5610 TO BLOCK_FGQC                                     354
            GO TO  5350                                                   354
5610        CONTINUE                                                      354
C       *                                                                 354
C       Geant POSITION : FGQC ===> X=(DISKOUTR+DISKINR)/2. Y=FRAMEWIDTH/  355
C       2.+FRAMEXYOFF Z=ZZFGZ <===                                        355
            CALL AGSRESET                                                 355
            AG_X = (DISKOUTR+DISKINR)/2.                                  355
            AG_Y = FRAMEWIDTH/2.+FRAMEXYOFF                               355
            AG_Z = ZZFGZ                                                  355
            AG_TITLE = 'FGQC'                                             355
            AG_EXNAME = 'POSITION'                                        355
            AG_PARLIST = 'X_Y_Z'                                          355
            CALL AXPOSITION                                               355
C       *                                                                 355
C       Create FGQD :                                                     356
C       Call To Local Procedure FGQD                                      356
            ASSIGN 5620 TO BLOCK_FGQD                                     356
            GO TO  5370                                                   356
5620        CONTINUE                                                      356
C       *                                                                 356
C       Geant POSITION : FGQD ===> Z=ZZFGZ <===                           357
            CALL AGSRESET                                                 357
            AG_Z = ZZFGZ                                                  357
            AG_TITLE = 'FGQD'                                             357
            AG_EXNAME = 'POSITION'                                        357
            AG_PARLIST = 'Z'                                              357
            CALL AXPOSITION                                               357
C       *                                                                 357
C       Create FGQE :                                                     358
C       Call To Local Procedure FGQE                                      358
            ASSIGN 5630 TO BLOCK_FGQE                                     358
            GO TO  5390                                                   358
5630        CONTINUE                                                      358
C       *                                                                 358
C       Geant POSITION : FGQE ===> X=(FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPE  361
C       RPANG*DEGRAD) Y=(FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD)  361
C        Z=ZZFGZ ALPHAZ=FLATPERPANG-90. <===                              361
            CALL AGSRESET                                                 361
            AG_X = (FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPERPANG*DEGRAD)       361
            AG_Y = (FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD)       361
            AG_Z = ZZFGZ                                                  361
            AG_ALPHAN=AG_ALPHAN+1                                         361
            AG_ALPHAK(AG_ALPHAN)=3                                        361
            AG_ALPHAL(AG_ALPHAN)=FLATPERPANG-90.                          361
            AG_TITLE = 'FGQE'                                             361
            AG_EXNAME = 'POSITION'                                        361
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   361
            CALL AXPOSITION                                               361
C       *                                                                 361
C       Create FGQF :                                                     362
C       Call To Local Procedure FGQF                                      362
            ASSIGN 5640 TO BLOCK_FGQF                                     362
            GO TO  5410                                                   362
5640        CONTINUE                                                      362
C       *                                                                 362
C       Geant POSITION : FGQF ===> Z=ZZFGZ <===                           363
            CALL AGSRESET                                                 363
            AG_Z = ZZFGZ                                                  363
            AG_TITLE = 'FGQF'                                             363
            AG_EXNAME = 'POSITION'                                        363
            AG_PARLIST = 'Z'                                              363
            CALL AXPOSITION                                               363
C       *                                                                 363
C       Create FGXA :                                                     364
C       Call To Local Procedure FGXA                                      364
            ASSIGN 5650 TO BLOCK_FGXA                                     364
            GO TO  5500                                                   364
5650        CONTINUE                                                      364
C       *                                                                 364
C       Geant POSITION : FGXA ===> Z=ZZFGZ <===                           365
            CALL AGSRESET                                                 365
            AG_Z = ZZFGZ                                                  365
            AG_TITLE = 'FGXA'                                             365
            AG_EXNAME = 'POSITION'                                        365
            AG_PARLIST = 'Z'                                              365
            CALL AXPOSITION                                               365
C       *                                                                 365
C       Create FGXB :                                                     366
C       Call To Local Procedure FGXB                                      366
            ASSIGN 5660 TO BLOCK_FGXB                                     366
            GO TO  5520                                                   366
5660        CONTINUE                                                      366
C       *                                                                 366
C       Geant POSITION : FGXB ===> Z=ZZFGZ <===                           367
            CALL AGSRESET                                                 367
            AG_Z = ZZFGZ                                                  367
            AG_TITLE = 'FGXB'                                             367
            AG_EXNAME = 'POSITION'                                        367
            AG_PARLIST = 'Z'                                              367
            CALL AXPOSITION                                               367
C       *                                                                 367
C       Create FGXC :                                                     368
C       Call To Local Procedure FGXC                                      368
            ASSIGN 5670 TO BLOCK_FGXC                                     368
            GO TO  5540                                                   368
5670        CONTINUE                                                      368
C       *                                                                 368
            XXFGZ=(DISKOUTR+DISKINR)/2.                                   369
C       Geant POSITION : FGXC ===> X=XXFGZ*COS(GRIDRAYLONGPHI*DEGRAD) Y=  371
C       XXFGZ*SIN(GRIDRAYLONGPHI*DEGRAD) Z=ZZFGZ ALPHAZ=GRIDRAYLONGPHI <  371
C       ===                                                               371
            CALL AGSRESET                                                 371
            AG_X = XXFGZ*COS(GRIDRAYLONGPHI*DEGRAD)                       371
            AG_Y = XXFGZ*SIN(GRIDRAYLONGPHI*DEGRAD)                       371
            AG_Z = ZZFGZ                                                  371
            AG_ALPHAN=AG_ALPHAN+1                                         371
            AG_ALPHAK(AG_ALPHAN)=3                                        371
            AG_ALPHAL(AG_ALPHAN)=GRIDRAYLONGPHI                           371
            AG_TITLE = 'FGXC'                                             371
            AG_EXNAME = 'POSITION'                                        371
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   371
            CALL AXPOSITION                                               371
C       *                                                                 371
C       Create FGXD :                                                     372
C       Call To Local Procedure FGXD                                      372
            ASSIGN 5680 TO BLOCK_FGXD                                     372
            GO TO  5560                                                   372
5680        CONTINUE                                                      372
C       *                                                                 372
            XXFGZ=(FLATOUTR+DISKINR)/2.                                   373
C       Geant POSITION : FGXD ===> X=XXFGZ*COS(GRIDRAYSHORTPHI*DEGRAD) Y  375
C       =XXFGZ*SIN(GRIDRAYSHORTPHI*DEGRAD) Z=ZZFGZ ALPHAZ=GRIDRAYSHORTPH  375
C       I <===                                                            375
            CALL AGSRESET                                                 375
            AG_X = XXFGZ*COS(GRIDRAYSHORTPHI*DEGRAD)                      375
            AG_Y = XXFGZ*SIN(GRIDRAYSHORTPHI*DEGRAD)                      375
            AG_Z = ZZFGZ                                                  375
            AG_ALPHAN=AG_ALPHAN+1                                         375
            AG_ALPHAK(AG_ALPHAN)=3                                        375
            AG_ALPHAL(AG_ALPHAN)=GRIDRAYSHORTPHI                          375
            AG_TITLE = 'FGXD'                                             375
            AG_EXNAME = 'POSITION'                                        375
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   375
            CALL AXPOSITION                                               375
C       *                                                                 375
         GO TO 5581                                                       376
5582     CONTINUE                                                         376
5241  CALL AGSPOPB                                                        377
      GO TO BLOCK_FGVG, (5240 ,5250)                                      377
C End of Local Procedure                                                  377
C ----------------------------------------------------------------------  378
C ------------ FGVR                                                       378
C Begin Local Procedure FGVR                                              379
C FGVR QUAD READOUT MIXTURE VOLUME                                        379
5260  CONTINUE                                                            379
         AG_TITLE='FGVR'                                                  379
         AG_MODULE=MODULE                                                 379
         AG_MARK='V00'                                                    379
         CALL AGSPUSH                                                     379
         AG_MODULE=MODULE                                                 379
C    *                                                                    379
C    Geant ATTRIBUTE : FGVR ===> SEEN=0 COLO=5 <===                       380
         CALL AGSRESET                                                    380
         AG_SEEN = 0                                                      380
         AG_COLO = 5                                                      380
         AG_TITLE = 'FGVR'                                                380
         AG_EXNAME = 'ATTRIBUTE'                                          380
         AG_PARLIST = 'SEEN_COLO'                                         380
         CALL AXATTRIBUTE                                                 380
C    *                                                                    380
C    Geant MATERIAL : READMIX ===> inherited arguments used <===          380
         CALL AGSRESET                                                    381
         AG_TITLE = 'READMIX'                                             381
         AG_EXNAME = 'MATERIAL'                                           381
         AG_PARLIST = 'NONE'                                              381
         CALL AXMATERIAL                                                  381
C    *                                                                    381
C    Geant SHAPE : TUBS ===> RMIN=DISKINR RMAX=DISKOUTR PHI1=0., PHI2=90  381
C    . DZ=VOLREADDZ/2. <===                                               381
         CALL AGSRESET                                                    383
         AG_RMIN = DISKINR                                                383
         AG_RMAX = DISKOUTR                                               383
         AG_PHI1 = 0.                                                     383
         AG_PHI2 = 90.                                                    383
         AG_DZ = VOLREADDZ/2.                                             383
         AG_TITLE = 'TUBS'                                                383
         AG_EXNAME = 'SHAPE'                                              383
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            383
         CALL AXSHAPE                                                     383
C    *                                                                    383
         ZZFGZ= - VOLREADDZ/2. + FRAMEDZ/1.5                              383
C    Create FGQA :                                                        385
C    Call To Local Procedure FGQA                                         385
         ASSIGN 5690 TO BLOCK_FGQA                                        385
         GO TO  5310                                                      385
5690     CONTINUE                                                         385
C    *                                                                    385
C    Geant POSITION : FGQA ===> X=FRAMEWIDTH/2.+FRAMEXYOFF Y=(DISKOUTR+D  386
C    ISKINR)/2. Z=ZZFGZ <===                                              386
         CALL AGSRESET                                                    386
         AG_X = FRAMEWIDTH/2.+FRAMEXYOFF                                  386
         AG_Y = (DISKOUTR+DISKINR)/2.                                     386
         AG_Z = ZZFGZ                                                     386
         AG_TITLE = 'FGQA'                                                386
         AG_EXNAME = 'POSITION'                                           386
         AG_PARLIST = 'X_Y_Z'                                             386
         CALL AXPOSITION                                                  386
C    *                                                                    386
C    Create FGQB :                                                        387
C    Call To Local Procedure FGQB                                         387
         ASSIGN 5700 TO BLOCK_FGQB                                        387
         GO TO  5330                                                      387
5700     CONTINUE                                                         387
C    *                                                                    387
C    Geant POSITION : FGQB ===> Z=ZZFGZ <===                              388
         CALL AGSRESET                                                    388
         AG_Z = ZZFGZ                                                     388
         AG_TITLE = 'FGQB'                                                388
         AG_EXNAME = 'POSITION'                                           388
         AG_PARLIST = 'Z'                                                 388
         CALL AXPOSITION                                                  388
C    *                                                                    388
C    Create FGQC :                                                        389
C    Call To Local Procedure FGQC                                         389
         ASSIGN 5710 TO BLOCK_FGQC                                        389
         GO TO  5350                                                      389
5710     CONTINUE                                                         389
C    *                                                                    389
C    Geant POSITION : FGQC ===> X=(DISKOUTR+DISKINR)/2. Y=FRAMEWIDTH/2.+  390
C    FRAMEXYOFF Z=ZZFGZ <===                                              390
         CALL AGSRESET                                                    390
         AG_X = (DISKOUTR+DISKINR)/2.                                     390
         AG_Y = FRAMEWIDTH/2.+FRAMEXYOFF                                  390
         AG_Z = ZZFGZ                                                     390
         AG_TITLE = 'FGQC'                                                390
         AG_EXNAME = 'POSITION'                                           390
         AG_PARLIST = 'X_Y_Z'                                             390
         CALL AXPOSITION                                                  390
C    *                                                                    390
C    Create FGQD :                                                        391
C    Call To Local Procedure FGQD                                         391
         ASSIGN 5720 TO BLOCK_FGQD                                        391
         GO TO  5370                                                      391
5720     CONTINUE                                                         391
C    *                                                                    391
C    Geant POSITION : FGQD ===> Z=ZZFGZ <===                              392
         CALL AGSRESET                                                    392
         AG_Z = ZZFGZ                                                     392
         AG_TITLE = 'FGQD'                                                392
         AG_EXNAME = 'POSITION'                                           392
         AG_PARLIST = 'Z'                                                 392
         CALL AXPOSITION                                                  392
C    *                                                                    392
C    Create FGQE :                                                        393
C    Call To Local Procedure FGQE                                         393
         ASSIGN 5730 TO BLOCK_FGQE                                        393
         GO TO  5390                                                      393
5730     CONTINUE                                                         393
C    *                                                                    393
C    Geant POSITION : FGQE ===> X=(FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPERPA  396
C    NG*DEGRAD) Y=(FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD) Z=ZZF  396
C    GZ ALPHAZ=FLATPERPANG-90. <===                                       396
         CALL AGSRESET                                                    396
         AG_X = (FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPERPANG*DEGRAD)          396
         AG_Y = (FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD)          396
         AG_Z = ZZFGZ                                                     396
         AG_ALPHAN=AG_ALPHAN+1                                            396
         AG_ALPHAK(AG_ALPHAN)=3                                           396
         AG_ALPHAL(AG_ALPHAN)=FLATPERPANG-90.                             396
         AG_TITLE = 'FGQE'                                                396
         AG_EXNAME = 'POSITION'                                           396
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      396
         CALL AXPOSITION                                                  396
C    *                                                                    396
C    Create FGQF :                                                        397
C    Call To Local Procedure FGQF                                         397
         ASSIGN 5740 TO BLOCK_FGQF                                        397
         GO TO  5410                                                      397
5740     CONTINUE                                                         397
C    *                                                                    397
C    Geant POSITION : FGQF ===> Z=ZZFGZ <===                              398
         CALL AGSRESET                                                    398
         AG_Z = ZZFGZ                                                     398
         AG_TITLE = 'FGQF'                                                398
         AG_EXNAME = 'POSITION'                                           398
         AG_PARLIST = 'Z'                                                 398
         CALL AXPOSITION                                                  398
C    *                                                                    398
5261  CALL AGSPOPB                                                        399
      GO TO BLOCK_FGVR, (5260 ,5270)                                      399
C End of Local Procedure                                                  399
C ----------------------------------------------------------------------  400
C ------------ FGVE                                                       400
C Begin Local Procedure FGVE                                              401
C FGVE QUAD ELECTRONICS VOLUME                                            401
5280  CONTINUE                                                            401
         AG_TITLE='FGVE'                                                  401
         AG_MODULE=MODULE                                                 401
         AG_MARK='V00'                                                    401
         CALL AGSPUSH                                                     401
         AG_MODULE=MODULE                                                 401
C    *                                                                    401
C    Geant ATTRIBUTE : FGVE ===> SEEN=0 COLO=7 <===                       402
         CALL AGSRESET                                                    402
         AG_SEEN = 0                                                      402
         AG_COLO = 7                                                      402
         AG_TITLE = 'FGVE'                                                402
         AG_EXNAME = 'ATTRIBUTE'                                          402
         AG_PARLIST = 'SEEN_COLO'                                         402
         CALL AXATTRIBUTE                                                 402
C    *                                                                    402
C    Geant MATERIAL : AIR ===> inherited arguments used <===              402
         CALL AGSRESET                                                    403
         AG_TITLE = 'AIR'                                                 403
         AG_EXNAME = 'MATERIAL'                                           403
         AG_PARLIST = 'NONE'                                              403
         CALL AXMATERIAL                                                  403
C    *                                                                    403
C    Geant SHAPE : TUBS ===> RMIN=DISKINR RMAX=DISKOUTR PHI1=0., PHI2=90  403
C    . DZ=VOLELECDZ/2. <===                                               403
         CALL AGSRESET                                                    405
         AG_RMIN = DISKINR                                                405
         AG_RMAX = DISKOUTR                                               405
         AG_PHI1 = 0.                                                     405
         AG_PHI2 = 90.                                                    405
         AG_DZ = VOLELECDZ/2.                                             405
         AG_TITLE = 'TUBS'                                                405
         AG_EXNAME = 'SHAPE'                                              405
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            405
         CALL AXSHAPE                                                     405
C    *                                                                    405
C    Create FGWA :                                                        406
C    Call To Local Procedure FGWA                                         406
         ASSIGN 5760 TO BLOCK_FGWA                                        406
         GO TO  5750                                                      406
5760     CONTINUE                                                         406
C    *                                                                    406
C    Geant POSITION : FGWA ===> X=DISKINR+FRAMEWIDTH*0.7 Y=ELECADY/2. <=  407
C    ==                                                                   407
         CALL AGSRESET                                                    407
         AG_X = DISKINR+FRAMEWIDTH*0.7                                    407
         AG_Y = ELECADY/2.                                                407
         AG_TITLE = 'FGWA'                                                407
         AG_EXNAME = 'POSITION'                                           407
         AG_PARLIST = 'X_Y'                                               407
         CALL AXPOSITION                                                  407
C    *                                                                    407
C    Create FGWA :                                                        408
C    Call To Local Procedure FGWA                                         408
         ASSIGN 5770 TO BLOCK_FGWA                                        408
         GO TO  5750                                                      408
5770     CONTINUE                                                         408
C    *                                                                    408
C    Geant POSITION : FGWA ===> X=ELECADY/2. Y=DISKINR+FRAMEWIDTH*0.7 AL  409
C    PHAZ=90. <===                                                        409
         CALL AGSRESET                                                    409
         AG_X = ELECADY/2.                                                409
         AG_Y = DISKINR+FRAMEWIDTH*0.7                                    409
         AG_ALPHAN=AG_ALPHAN+1                                            409
         AG_ALPHAK(AG_ALPHAN)=3                                           409
         AG_ALPHAL(AG_ALPHAN)=90.                                         409
         AG_TITLE = 'FGWA'                                                409
         AG_EXNAME = 'POSITION'                                           409
         AG_PARLIST = 'X_Y_ALPHAZ'                                        409
         CALL AXPOSITION                                                  409
C    *                                                                    409
C    Create FGWB :                                                        410
C    Call To Local Procedure FGWB                                         410
         ASSIGN 5790 TO BLOCK_FGWB                                        410
         GO TO  5780                                                      410
5790     CONTINUE                                                         410
C    *                                                                    410
C    Geant POSITION : FGWB ===> X=(DISKOUTR+DISKINR)/2. Y=FRAMEXYOFF+ELE  411
C    CWIDTH/2. <===                                                       411
         CALL AGSRESET                                                    411
         AG_X = (DISKOUTR+DISKINR)/2.                                     411
         AG_Y = FRAMEXYOFF+ELECWIDTH/2.                                   411
         AG_TITLE = 'FGWB'                                                411
         AG_EXNAME = 'POSITION'                                           411
         AG_PARLIST = 'X_Y'                                               411
         CALL AXPOSITION                                                  411
C    *                                                                    411
C    Create FGWB :                                                        412
C    Call To Local Procedure FGWB                                         412
         ASSIGN 5800 TO BLOCK_FGWB                                        412
         GO TO  5780                                                      412
5800     CONTINUE                                                         412
C    *                                                                    412
C    Geant POSITION : FGWB ===> X=FRAMEXYOFF+ELECWIDTH/2. Y=(DISKOUTR+DI  413
C    SKINR)/2. ALPHAZ=90. <===                                            413
         CALL AGSRESET                                                    413
         AG_X = FRAMEXYOFF+ELECWIDTH/2.                                   413
         AG_Y = (DISKOUTR+DISKINR)/2.                                     413
         AG_ALPHAN=AG_ALPHAN+1                                            413
         AG_ALPHAK(AG_ALPHAN)=3                                           413
         AG_ALPHAL(AG_ALPHAN)=90.                                         413
         AG_TITLE = 'FGWB'                                                413
         AG_EXNAME = 'POSITION'                                           413
         AG_PARLIST = 'X_Y_ALPHAZ'                                        413
         CALL AXPOSITION                                                  413
C    *                                                                    413
C    Create FGWC :                                                        414
C    Call To Local Procedure FGWC                                         414
         ASSIGN 5820 TO BLOCK_FGWC                                        414
         GO TO  5810                                                      414
5820     CONTINUE                                                         414
C    *                                                                    414
C    Geant POSITION : FGWC ===> X=DISKOUTR-FRAMEWIDTH*0.7 Y=ELECCDY/2. <  415
C    ===                                                                  415
         CALL AGSRESET                                                    415
         AG_X = DISKOUTR-FRAMEWIDTH*0.7                                   415
         AG_Y = ELECCDY/2.                                                415
         AG_TITLE = 'FGWC'                                                415
         AG_EXNAME = 'POSITION'                                           415
         AG_PARLIST = 'X_Y'                                               415
         CALL AXPOSITION                                                  415
C    *                                                                    415
C    Create FGWC :                                                        416
C    Call To Local Procedure FGWC                                         416
         ASSIGN 5830 TO BLOCK_FGWC                                        416
         GO TO  5810                                                      416
5830     CONTINUE                                                         416
C    *                                                                    416
C    Geant POSITION : FGWC ===> X=ELECCDY/2. Y=DISKOUTR-FRAMEWIDTH*0.7 A  417
C    LPHAZ=90. <===                                                       417
         CALL AGSRESET                                                    417
         AG_X = ELECCDY/2.                                                417
         AG_Y = DISKOUTR-FRAMEWIDTH*0.7                                   417
         AG_ALPHAN=AG_ALPHAN+1                                            417
         AG_ALPHAK(AG_ALPHAN)=3                                           417
         AG_ALPHAL(AG_ALPHAN)=90.                                         417
         AG_TITLE = 'FGWC'                                                417
         AG_EXNAME = 'POSITION'                                           417
         AG_PARLIST = 'X_Y_ALPHAZ'                                        417
         CALL AXPOSITION                                                  417
C    *                                                                    417
C    Create FGWD :                                                        418
C    Call To Local Procedure FGWD                                         418
         ASSIGN 5850 TO BLOCK_FGWD                                        418
         GO TO  5840                                                      418
5850     CONTINUE                                                         418
C    *                                                                    418
C    Geant POSITION : FGWD ===> X=(DISKOUTR-FRAMEWIDTH/2.)*COS(ELECDANG*  420
C    DEGRAD) Y=(DISKOUTR-FRAMEWIDTH/2.)*SIN(ELECDANG*DEGRAD) ALPHAX=90.   420
C    <===                                                                 420
         CALL AGSRESET                                                    420
         AG_X = (DISKOUTR-FRAMEWIDTH/2.)*COS(ELECDANG*DEGRAD)             420
         AG_Y = (DISKOUTR-FRAMEWIDTH/2.)*SIN(ELECDANG*DEGRAD)             420
         AG_ALPHAN=AG_ALPHAN+1                                            420
         AG_ALPHAK(AG_ALPHAN)=1                                           420
         AG_ALPHAL(AG_ALPHAN)=90.                                         420
         AG_TITLE = 'FGWD'                                                420
         AG_EXNAME = 'POSITION'                                           420
         AG_PARLIST = 'X_Y_ALPHAX'                                        420
         CALL AXPOSITION                                                  420
C    *                                                                    420
C    Create FGWD :                                                        421
C    Call To Local Procedure FGWD                                         421
         ASSIGN 5860 TO BLOCK_FGWD                                        421
         GO TO  5840                                                      421
5860     CONTINUE                                                         421
C    *                                                                    421
C    Geant POSITION : FGWD ===> X=(DISKOUTR-FRAMEWIDTH/2.)*SIN(ELECDANG*  423
C    DEGRAD) Y=(DISKOUTR-FRAMEWIDTH/2.)*COS(ELECDANG*DEGRAD) ALPHAX=90.   423
C    ALPHAZ=90. <===                                                      423
         CALL AGSRESET                                                    423
         AG_X = (DISKOUTR-FRAMEWIDTH/2.)*SIN(ELECDANG*DEGRAD)             423
         AG_Y = (DISKOUTR-FRAMEWIDTH/2.)*COS(ELECDANG*DEGRAD)             423
         AG_ALPHAN=AG_ALPHAN+1                                            423
         AG_ALPHAK(AG_ALPHAN)=1                                           423
         AG_ALPHAL(AG_ALPHAN)=90.                                         423
         AG_ALPHAN=AG_ALPHAN+1                                            423
         AG_ALPHAK(AG_ALPHAN)=3                                           423
         AG_ALPHAL(AG_ALPHAN)=90.                                         423
         AG_TITLE = 'FGWD'                                                423
         AG_EXNAME = 'POSITION'                                           423
         AG_PARLIST = 'X_Y_ALPHAX_ALPHAZ'                                 423
         CALL AXPOSITION                                                  423
C    *                                                                    423
C    Create FGWE :                                                        424
C    Call To Local Procedure FGWE                                         424
         ASSIGN 5880 TO BLOCK_FGWE                                        424
         GO TO  5870                                                      424
5880     CONTINUE                                                         424
C    *                                                                    424
C    Geant POSITION : FGWE ===> X=(FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPERPA  426
C    NG*DEGRAD) Y=(FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD) ALPHA  426
C    Z=FLATPERPANG-90. <===                                               426
         CALL AGSRESET                                                    426
         AG_X = (FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPERPANG*DEGRAD)          426
         AG_Y = (FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD)          426
         AG_ALPHAN=AG_ALPHAN+1                                            426
         AG_ALPHAK(AG_ALPHAN)=3                                           426
         AG_ALPHAL(AG_ALPHAN)=FLATPERPANG-90.                             426
         AG_TITLE = 'FGWE'                                                426
         AG_EXNAME = 'POSITION'                                           426
         AG_PARLIST = 'X_Y_ALPHAZ'                                        426
         CALL AXPOSITION                                                  426
C    *                                                                    426
5281  CALL AGSPOPB                                                        427
      GO TO BLOCK_FGVE, (5280 ,5290)                                      427
C End of Local Procedure                                                  427
C ----------------------------------------------------------------------  428
C ------------ FGQA                                                       428
C Begin Local Procedure FGQA                                              429
C FGQA A-SPACER FRAME                                                     429
5310  CONTINUE                                                            429
         AG_TITLE='FGQA'                                                  429
         AG_MODULE=MODULE                                                 429
         AG_MARK='V00'                                                    429
         CALL AGSPUSH                                                     429
         AG_MODULE=MODULE                                                 429
C    *                                                                    429
C    Geant ATTRIBUTE : FGQA ===> SEEN=1 COLO=4 <===                       430
         CALL AGSRESET                                                    430
         AG_SEEN = 1                                                      430
         AG_COLO = 4                                                      430
         AG_TITLE = 'FGQA'                                                430
         AG_EXNAME = 'ATTRIBUTE'                                          430
         AG_PARLIST = 'SEEN_COLO'                                         430
         CALL AXATTRIBUTE                                                 430
C    *                                                                    430
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              430
         CALL AGSRESET                                                    431
         AG_TITLE = 'FR4'                                                 431
         AG_EXNAME = 'MATERIAL'                                           431
         AG_PARLIST = 'NONE'                                              431
         CALL AXMATERIAL                                                  431
C    *                                                                    431
C    Geant SHAPE : BOX ===> DX=FRAMEWIDTH/2. DY=(DISKOUTR-DISKINR-FRAMEW  431
C    IDTH)/2., DZ=FRAMEDZ/2. <===                                         431
         CALL AGSRESET                                                    433
         AG_DX = FRAMEWIDTH/2.                                            433
         AG_DY = (DISKOUTR-DISKINR-FRAMEWIDTH)/2.                         433
         AG_DZ = FRAMEDZ/2.                                               433
         AG_TITLE = 'BOX'                                                 433
         AG_EXNAME = 'SHAPE'                                              433
         AG_PARLIST = 'DX_DY_DZ'                                          433
         CALL AXSHAPE                                                     433
C    *                                                                    433
5311  CALL AGSPOPB                                                        434
      GO TO BLOCK_FGQA, (5310 ,5320,5440,5590,5690)                       434
C End of Local Procedure                                                  434
C ----------------------------------------------------------------------  435
C ------------ FGQB                                                       435
C Begin Local Procedure FGQB                                              436
C FGQB B-SPACER FRAME, ARC                                                436
5330  CONTINUE                                                            436
         AG_TITLE='FGQB'                                                  436
         AG_MODULE=MODULE                                                 436
         AG_MARK='V00'                                                    436
         CALL AGSPUSH                                                     436
         AG_MODULE=MODULE                                                 436
C    *                                                                    436
C    Geant ATTRIBUTE : FGQB ===> SEEN=1 COLO=4 <===                       437
         CALL AGSRESET                                                    437
         AG_SEEN = 1                                                      437
         AG_COLO = 4                                                      437
         AG_TITLE = 'FGQB'                                                437
         AG_EXNAME = 'ATTRIBUTE'                                          437
         AG_PARLIST = 'SEEN_COLO'                                         437
         CALL AXATTRIBUTE                                                 437
C    *                                                                    437
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              437
         CALL AGSRESET                                                    438
         AG_TITLE = 'FR4'                                                 438
         AG_EXNAME = 'MATERIAL'                                           438
         AG_PARLIST = 'NONE'                                              438
         CALL AXMATERIAL                                                  438
C    *                                                                    438
         XXFGQ=FRAMEXYOFF/DISKINR/DEGRAD                                  438
C    Geant SHAPE : TUBS ===> RMIN=DISKINR RMAX=DISKINR+FRAMEWIDTH, PHI1=  441
C    XXFGQ PHI2=90.-XXFGQ DZ=FRAMEDZ/2. <===                              441
         CALL AGSRESET                                                    441
         AG_RMIN = DISKINR                                                441
         AG_RMAX = DISKINR+FRAMEWIDTH                                     441
         AG_PHI1 = XXFGQ                                                  441
         AG_PHI2 = 90.-XXFGQ                                              441
         AG_DZ = FRAMEDZ/2.                                               441
         AG_TITLE = 'TUBS'                                                441
         AG_EXNAME = 'SHAPE'                                              441
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            441
         CALL AXSHAPE                                                     441
C    *                                                                    441
5331  CALL AGSPOPB                                                        442
      GO TO BLOCK_FGQB, (5330 ,5340,5450,5600,5700)                       442
C End of Local Procedure                                                  442
C ----------------------------------------------------------------------  443
C ------------ FGQC                                                       443
C Begin Local Procedure FGQC                                              444
C FGQC C-SPACER FRAME, BAR                                                444
5350  CONTINUE                                                            444
         AG_TITLE='FGQC'                                                  444
         AG_MODULE=MODULE                                                 444
         AG_MARK='V00'                                                    444
         CALL AGSPUSH                                                     444
         AG_MODULE=MODULE                                                 444
C    *                                                                    444
C    Geant ATTRIBUTE : FGQC ===> SEEN=1 COLO=4 <===                       445
         CALL AGSRESET                                                    445
         AG_SEEN = 1                                                      445
         AG_COLO = 4                                                      445
         AG_TITLE = 'FGQC'                                                445
         AG_EXNAME = 'ATTRIBUTE'                                          445
         AG_PARLIST = 'SEEN_COLO'                                         445
         CALL AXATTRIBUTE                                                 445
C    *                                                                    445
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              445
         CALL AGSRESET                                                    446
         AG_TITLE = 'FR4'                                                 446
         AG_EXNAME = 'MATERIAL'                                           446
         AG_PARLIST = 'NONE'                                              446
         CALL AXMATERIAL                                                  446
C    *                                                                    446
C    Geant SHAPE : BOX ===> DX=(DISKOUTR-DISKINR-FRAMEWIDTH)/2., DY=FRAM  446
C    EWIDTH/2. DZ=FRAMEDZ/2. <===                                         446
         CALL AGSRESET                                                    448
         AG_DX = (DISKOUTR-DISKINR-FRAMEWIDTH)/2.                         448
         AG_DY = FRAMEWIDTH/2.                                            448
         AG_DZ = FRAMEDZ/2.                                               448
         AG_TITLE = 'BOX'                                                 448
         AG_EXNAME = 'SHAPE'                                              448
         AG_PARLIST = 'DX_DY_DZ'                                          448
         CALL AXSHAPE                                                     448
C    *                                                                    448
5351  CALL AGSPOPB                                                        449
      GO TO BLOCK_FGQC, (5350 ,5360,5460,5610,5710)                       449
C End of Local Procedure                                                  449
C ----------------------------------------------------------------------  450
C ------------ FGQD                                                       450
C Begin Local Procedure FGQD                                              451
C FGQD D-SPACER FRAME, ARC                                                451
5370  CONTINUE                                                            451
         AG_TITLE='FGQD'                                                  451
         AG_MODULE=MODULE                                                 451
         AG_MARK='V00'                                                    451
         CALL AGSPUSH                                                     451
         AG_MODULE=MODULE                                                 451
C    *                                                                    451
C    Geant ATTRIBUTE : FGQD ===> SEEN=1 COLO=4 <===                       452
         CALL AGSRESET                                                    452
         AG_SEEN = 1                                                      452
         AG_COLO = 4                                                      452
         AG_TITLE = 'FGQD'                                                452
         AG_EXNAME = 'ATTRIBUTE'                                          452
         AG_PARLIST = 'SEEN_COLO'                                         452
         CALL AXATTRIBUTE                                                 452
C    *                                                                    452
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              452
         CALL AGSRESET                                                    453
         AG_TITLE = 'FR4'                                                 453
         AG_EXNAME = 'MATERIAL'                                           453
         AG_PARLIST = 'NONE'                                              453
         CALL AXMATERIAL                                                  453
C    *                                                                    453
         XXFGQ=FRAMEXYOFF/DISKOUTR/DEGRAD                                 453
C    Geant SHAPE : TUBS ===> RMIN=DISKOUTR-FRAMEWIDTH RMAX=DISKOUTR, PHI  456
C    1=XXFGQ PHI2=FLATANG1 DZ=FRAMEDZ/2. <===                             456
         CALL AGSRESET                                                    456
         AG_RMIN = DISKOUTR-FRAMEWIDTH                                    456
         AG_RMAX = DISKOUTR                                               456
         AG_PHI1 = XXFGQ                                                  456
         AG_PHI2 = FLATANG1                                               456
         AG_DZ = FRAMEDZ/2.                                               456
         AG_TITLE = 'TUBS'                                                456
         AG_EXNAME = 'SHAPE'                                              456
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            456
         CALL AXSHAPE                                                     456
C    *                                                                    456
5371  CALL AGSPOPB                                                        457
      GO TO BLOCK_FGQD, (5370 ,5380,5470,5620,5720)                       457
C End of Local Procedure                                                  457
C ----------------------------------------------------------------------  458
C ------------ FGQE                                                       458
C Begin Local Procedure FGQE                                              459
C FGQE E-SPACER FRAME, FLAT                                               459
5390  CONTINUE                                                            459
         AG_TITLE='FGQE'                                                  459
         AG_MODULE=MODULE                                                 459
         AG_MARK='V00'                                                    459
         CALL AGSPUSH                                                     459
         AG_MODULE=MODULE                                                 459
C    *                                                                    459
C    Geant ATTRIBUTE : FGQE ===> SEEN=1 COLO=4 <===                       460
         CALL AGSRESET                                                    460
         AG_SEEN = 1                                                      460
         AG_COLO = 4                                                      460
         AG_TITLE = 'FGQE'                                                460
         AG_EXNAME = 'ATTRIBUTE'                                          460
         AG_PARLIST = 'SEEN_COLO'                                         460
         CALL AXATTRIBUTE                                                 460
C    *                                                                    460
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              460
         CALL AGSRESET                                                    461
         AG_TITLE = 'FR4'                                                 461
         AG_EXNAME = 'MATERIAL'                                           461
         AG_PARLIST = 'NONE'                                              461
         CALL AXMATERIAL                                                  461
C    *                                                                    461
C    Geant SHAPE : BOX ===> DX=DISKOUTR*SIN((FLATANG2-FLATANG1)*DEGRAD/2  461
C    .), DY=FRAMEWIDTH/2. DZ=FRAMEDZ/2. <===                              461
         CALL AGSRESET                                                    463
         AG_DX = DISKOUTR*SIN((FLATANG2-FLATANG1)*DEGRAD/2.)              463
         AG_DY = FRAMEWIDTH/2.                                            463
         AG_DZ = FRAMEDZ/2.                                               463
         AG_TITLE = 'BOX'                                                 463
         AG_EXNAME = 'SHAPE'                                              463
         AG_PARLIST = 'DX_DY_DZ'                                          463
         CALL AXSHAPE                                                     463
C    *                                                                    463
5391  CALL AGSPOPB                                                        464
      GO TO BLOCK_FGQE, (5390 ,5400,5480,5630,5730)                       464
C End of Local Procedure                                                  464
C ----------------------------------------------------------------------  465
C ------------ FGQF                                                       465
C Begin Local Procedure FGQF                                              466
C FGQF F-SPACER FRAME, ARC                                                466
5410  CONTINUE                                                            466
         AG_TITLE='FGQF'                                                  466
         AG_MODULE=MODULE                                                 466
         AG_MARK='V00'                                                    466
         CALL AGSPUSH                                                     466
         AG_MODULE=MODULE                                                 466
C    *                                                                    466
C    Geant ATTRIBUTE : FGQF ===> SEEN=1 COLO=4 <===                       467
         CALL AGSRESET                                                    467
         AG_SEEN = 1                                                      467
         AG_COLO = 4                                                      467
         AG_TITLE = 'FGQF'                                                467
         AG_EXNAME = 'ATTRIBUTE'                                          467
         AG_PARLIST = 'SEEN_COLO'                                         467
         CALL AXATTRIBUTE                                                 467
C    *                                                                    467
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              467
         CALL AGSRESET                                                    468
         AG_TITLE = 'FR4'                                                 468
         AG_EXNAME = 'MATERIAL'                                           468
         AG_PARLIST = 'NONE'                                              468
         CALL AXMATERIAL                                                  468
C    *                                                                    468
         XXFGQ=FRAMEXYOFF/DISKOUTR/DEGRAD                                 468
C    Geant SHAPE : TUBS ===> RMIN=DISKOUTR-FRAMEWIDTH RMAX=DISKOUTR, PHI  471
C    1=FLATANG2 PHI2=90.-XXFGQ DZ=FRAMEDZ/2. <===                         471
         CALL AGSRESET                                                    471
         AG_RMIN = DISKOUTR-FRAMEWIDTH                                    471
         AG_RMAX = DISKOUTR                                               471
         AG_PHI1 = FLATANG2                                               471
         AG_PHI2 = 90.-XXFGQ                                              471
         AG_DZ = FRAMEDZ/2.                                               471
         AG_TITLE = 'TUBS'                                                471
         AG_EXNAME = 'SHAPE'                                              471
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            471
         CALL AXSHAPE                                                     471
C    *                                                                    471
5411  CALL AGSPOPB                                                        472
      GO TO BLOCK_FGQF, (5410 ,5420,5490,5640,5740)                       472
C End of Local Procedure                                                  472
C ----------------------------------------------------------------------  473
C ------------ FGXA                                                       473
C Begin Local Procedure FGXA                                              474
C FGXA A-GRID, ARC                                                        474
5500  CONTINUE                                                            474
         AG_TITLE='FGXA'                                                  474
         AG_MODULE=MODULE                                                 474
         AG_MARK='V00'                                                    474
         CALL AGSPUSH                                                     474
         AG_MODULE=MODULE                                                 474
C    *                                                                    474
C    Geant ATTRIBUTE : FGXB ===> SEEN=1 COLO=3 <===                       475
         CALL AGSRESET                                                    475
         AG_SEEN = 1                                                      475
         AG_COLO = 3                                                      475
         AG_TITLE = 'FGXB'                                                475
         AG_EXNAME = 'ATTRIBUTE'                                          475
         AG_PARLIST = 'SEEN_COLO'                                         475
         CALL AXATTRIBUTE                                                 475
C    *                                                                    475
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              475
         CALL AGSRESET                                                    476
         AG_TITLE = 'FR4'                                                 476
         AG_EXNAME = 'MATERIAL'                                           476
         AG_PARLIST = 'NONE'                                              476
         CALL AXMATERIAL                                                  476
C    *                                                                    476
         XXFGX=FRAMEXYOFF/GRIDARCBIGR/DEGRAD                              476
C    Geant SHAPE : TUBS ===> RMIN=GRIDARCBIGR RMAX=GRIDARCBIGR+GRIDWIDTH  479
C    , PHI1=XXFGX PHI2=90.-XXFGX DZ=FRAMEDZ/2. <===                       479
         CALL AGSRESET                                                    479
         AG_RMIN = GRIDARCBIGR                                            479
         AG_RMAX = GRIDARCBIGR+GRIDWIDTH                                  479
         AG_PHI1 = XXFGX                                                  479
         AG_PHI2 = 90.-XXFGX                                              479
         AG_DZ = FRAMEDZ/2.                                               479
         AG_TITLE = 'TUBS'                                                479
         AG_EXNAME = 'SHAPE'                                              479
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            479
         CALL AXSHAPE                                                     479
C    *                                                                    479
5501  CALL AGSPOPB                                                        480
      GO TO BLOCK_FGXA, (5500 ,5510,5650)                                 480
C End of Local Procedure                                                  480
C ----------------------------------------------------------------------  481
C ------------ FGXB                                                       481
C Begin Local Procedure FGXB                                              482
C FGXB B-GRID, ARC                                                        482
5520  CONTINUE                                                            482
         AG_TITLE='FGXB'                                                  482
         AG_MODULE=MODULE                                                 482
         AG_MARK='V00'                                                    482
         CALL AGSPUSH                                                     482
         AG_MODULE=MODULE                                                 482
C    *                                                                    482
C    Geant ATTRIBUTE : FGXB ===> SEEN=1 COLO=3 <===                       483
         CALL AGSRESET                                                    483
         AG_SEEN = 1                                                      483
         AG_COLO = 3                                                      483
         AG_TITLE = 'FGXB'                                                483
         AG_EXNAME = 'ATTRIBUTE'                                          483
         AG_PARLIST = 'SEEN_COLO'                                         483
         CALL AXATTRIBUTE                                                 483
C    *                                                                    483
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              483
         CALL AGSRESET                                                    484
         AG_TITLE = 'FR4'                                                 484
         AG_EXNAME = 'MATERIAL'                                           484
         AG_PARLIST = 'NONE'                                              484
         CALL AXMATERIAL                                                  484
C    *                                                                    484
         XXFGX=FRAMEXYOFF/GRIDARCSMALLR/DEGRAD                            484
C    Geant SHAPE : TUBS ===> RMIN=GRIDARCSMALLR RMAX=GRIDARCSMALLR+GRIDW  488
C    IDTH, PHI1=XXFGX PHI2=90.-XXFGX, DZ=FRAMEDZ/2. <===                  488
         CALL AGSRESET                                                    488
         AG_RMIN = GRIDARCSMALLR                                          488
         AG_RMAX = GRIDARCSMALLR+GRIDWIDTH                                488
         AG_PHI1 = XXFGX                                                  488
         AG_PHI2 = 90.-XXFGX                                              488
         AG_DZ = FRAMEDZ/2.                                               488
         AG_TITLE = 'TUBS'                                                488
         AG_EXNAME = 'SHAPE'                                              488
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            488
         CALL AXSHAPE                                                     488
C    *                                                                    488
5521  CALL AGSPOPB                                                        489
      GO TO BLOCK_FGXB, (5520 ,5530,5660)                                 489
C End of Local Procedure                                                  489
C ----------------------------------------------------------------------  490
C ------------ FGXC                                                       490
C Begin Local Procedure FGXC                                              491
C FGXC C-GRID, RAY                                                        491
5540  CONTINUE                                                            491
         AG_TITLE='FGXC'                                                  491
         AG_MODULE=MODULE                                                 491
         AG_MARK='V00'                                                    491
         CALL AGSPUSH                                                     491
         AG_MODULE=MODULE                                                 491
C    *                                                                    491
C    Geant ATTRIBUTE : FGXC ===> SEEN=1 COLO=3 <===                       492
         CALL AGSRESET                                                    492
         AG_SEEN = 1                                                      492
         AG_COLO = 3                                                      492
         AG_TITLE = 'FGXC'                                                492
         AG_EXNAME = 'ATTRIBUTE'                                          492
         AG_PARLIST = 'SEEN_COLO'                                         492
         CALL AXATTRIBUTE                                                 492
C    *                                                                    492
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              492
         CALL AGSRESET                                                    493
         AG_TITLE = 'FR4'                                                 493
         AG_EXNAME = 'MATERIAL'                                           493
         AG_PARLIST = 'NONE'                                              493
         CALL AXMATERIAL                                                  493
C    *                                                                    493
C    Geant SHAPE : BOX ===> DX=(DISKOUTR-DISKINR-FRAMEWIDTH)/2., DY=GRID  493
C    WIDTH/2. DZ=FRAMEDZ/2. <===                                          493
         CALL AGSRESET                                                    495
         AG_DX = (DISKOUTR-DISKINR-FRAMEWIDTH)/2.                         495
         AG_DY = GRIDWIDTH/2.                                             495
         AG_DZ = FRAMEDZ/2.                                               495
         AG_TITLE = 'BOX'                                                 495
         AG_EXNAME = 'SHAPE'                                              495
         AG_PARLIST = 'DX_DY_DZ'                                          495
         CALL AXSHAPE                                                     495
C    *                                                                    495
5541  CALL AGSPOPB                                                        496
      GO TO BLOCK_FGXC, (5540 ,5550,5670)                                 496
C End of Local Procedure                                                  496
C ----------------------------------------------------------------------  497
C ------------ FGXD                                                       497
C Begin Local Procedure FGXD                                              498
C FGXD D-GRID, RAY                                                        498
5560  CONTINUE                                                            498
         AG_TITLE='FGXD'                                                  498
         AG_MODULE=MODULE                                                 498
         AG_MARK='V00'                                                    498
         CALL AGSPUSH                                                     498
         AG_MODULE=MODULE                                                 498
C    *                                                                    498
C    Geant ATTRIBUTE : FGXD ===> SEEN=1 COLO=3 <===                       499
         CALL AGSRESET                                                    499
         AG_SEEN = 1                                                      499
         AG_COLO = 3                                                      499
         AG_TITLE = 'FGXD'                                                499
         AG_EXNAME = 'ATTRIBUTE'                                          499
         AG_PARLIST = 'SEEN_COLO'                                         499
         CALL AXATTRIBUTE                                                 499
C    *                                                                    499
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              499
         CALL AGSRESET                                                    500
         AG_TITLE = 'FR4'                                                 500
         AG_EXNAME = 'MATERIAL'                                           500
         AG_PARLIST = 'NONE'                                              500
         CALL AXMATERIAL                                                  500
C    *                                                                    500
C    Geant SHAPE : BOX ===> DX=(FLATOUTR-DISKINR-FRAMEWIDTH)/2., DY=GRID  500
C    WIDTH/2. DZ=FRAMEDZ/2. <===                                          500
         CALL AGSRESET                                                    502
         AG_DX = (FLATOUTR-DISKINR-FRAMEWIDTH)/2.                         502
         AG_DY = GRIDWIDTH/2.                                             502
         AG_DZ = FRAMEDZ/2.                                               502
         AG_TITLE = 'BOX'                                                 502
         AG_EXNAME = 'SHAPE'                                              502
         AG_PARLIST = 'DX_DY_DZ'                                          502
         CALL AXSHAPE                                                     502
C    *                                                                    502
5561  CALL AGSPOPB                                                        503
      GO TO BLOCK_FGXD, (5560 ,5570,5680)                                 503
C End of Local Procedure                                                  503
C ----------------------------------------------------------------------  504
C ------------ FGWA                                                       504
C Begin Local Procedure FGWA                                              505
C FGWA A-ELEC, BAR                                                        505
5750  CONTINUE                                                            505
         AG_TITLE='FGWA'                                                  505
         AG_MODULE=MODULE                                                 505
         AG_MARK='V00'                                                    505
         CALL AGSPUSH                                                     505
         AG_MODULE=MODULE                                                 505
C    *                                                                    505
C    Geant ATTRIBUTE : FGWA ===> SEEN=1 COLO=1 <===                       506
         CALL AGSRESET                                                    506
         AG_SEEN = 1                                                      506
         AG_COLO = 1                                                      506
         AG_TITLE = 'FGWA'                                                506
         AG_EXNAME = 'ATTRIBUTE'                                          506
         AG_PARLIST = 'SEEN_COLO'                                         506
         CALL AXATTRIBUTE                                                 506
C    *                                                                    506
C    Geant MATERIAL : TERMMIX ===> inherited arguments used <===          506
         CALL AGSRESET                                                    507
         AG_TITLE = 'TERMMIX'                                             507
         AG_EXNAME = 'MATERIAL'                                           507
         AG_PARLIST = 'NONE'                                              507
         CALL AXMATERIAL                                                  507
C    *                                                                    507
C    Geant SHAPE : BOX ===> DX=0.3/2. DY=ELECADY/2. DZ=2.7/2. <===        507
         CALL AGSRESET                                                    508
         AG_DX = 0.3/2.                                                   508
         AG_DY = ELECADY/2.                                               508
         AG_DZ = 2.7/2.                                                   508
         AG_TITLE = 'BOX'                                                 508
         AG_EXNAME = 'SHAPE'                                              508
         AG_PARLIST = 'DX_DY_DZ'                                          508
         CALL AXSHAPE                                                     508
C    *                                                                    508
5751  CALL AGSPOPB                                                        509
      GO TO BLOCK_FGWA, (5750 ,5760,5770)                                 509
C End of Local Procedure                                                  509
C ----------------------------------------------------------------------  510
C ------------ FGWB                                                       510
C Begin Local Procedure FGWB                                              511
C FGWB APV BOARD                                                          511
5780  CONTINUE                                                            511
         AG_TITLE='FGWB'                                                  511
         AG_MODULE=MODULE                                                 511
         AG_MARK='V00'                                                    511
         CALL AGSPUSH                                                     511
         AG_MODULE=MODULE                                                 511
C    *                                                                    511
C    Geant ATTRIBUTE : FGWB ===> SEEN=1 COLO=1 <===                       512
         CALL AGSRESET                                                    512
         AG_SEEN = 1                                                      512
         AG_COLO = 1                                                      512
         AG_TITLE = 'FGWB'                                                512
         AG_EXNAME = 'ATTRIBUTE'                                          512
         AG_PARLIST = 'SEEN_COLO'                                         512
         CALL AXATTRIBUTE                                                 512
C    *                                                                    512
C    Geant MATERIAL : APVMIX ===> inherited arguments used <===           512
         CALL AGSRESET                                                    513
         AG_TITLE = 'APVMIX'                                              513
         AG_EXNAME = 'MATERIAL'                                           513
         AG_PARLIST = 'NONE'                                              513
         CALL AXMATERIAL                                                  513
C    *                                                                    513
C    Geant SHAPE : BOX ===> DX=25.92/2. DY=ELECWIDTH/2. DZ=4.04/2. <===   513
         CALL AGSRESET                                                    514
         AG_DX = 25.92/2.                                                 514
         AG_DY = ELECWIDTH/2.                                             514
         AG_DZ = 4.04/2.                                                  514
         AG_TITLE = 'BOX'                                                 514
         AG_EXNAME = 'SHAPE'                                              514
         AG_PARLIST = 'DX_DY_DZ'                                          514
         CALL AXSHAPE                                                     514
C    *                                                                    514
5781  CALL AGSPOPB                                                        515
      GO TO BLOCK_FGWB, (5780 ,5790,5800)                                 515
C End of Local Procedure                                                  515
C ----------------------------------------------------------------------  516
C ------------ FGWC                                                       516
C Begin Local Procedure FGWC                                              517
C FGWC INTERCONNECT BOARD WIDER                                           517
5810  CONTINUE                                                            517
         AG_TITLE='FGWC'                                                  517
         AG_MODULE=MODULE                                                 517
         AG_MARK='V00'                                                    517
         CALL AGSPUSH                                                     517
         AG_MODULE=MODULE                                                 517
C    *                                                                    517
C    Geant ATTRIBUTE : FGWC ===> SEEN=1 COLO=1 <===                       518
         CALL AGSRESET                                                    518
         AG_SEEN = 1                                                      518
         AG_COLO = 1                                                      518
         AG_TITLE = 'FGWC'                                                518
         AG_EXNAME = 'ATTRIBUTE'                                          518
         AG_PARLIST = 'SEEN_COLO'                                         518
         CALL AXATTRIBUTE                                                 518
C    *                                                                    518
C    Geant MATERIAL : CONMIX ===> inherited arguments used <===           518
         CALL AGSRESET                                                    519
         AG_TITLE = 'CONMIX'                                              519
         AG_EXNAME = 'MATERIAL'                                           519
         AG_PARLIST = 'NONE'                                              519
         CALL AXMATERIAL                                                  519
C    *                                                                    519
C    Geant SHAPE : BOX ===> DX=0.3/2. DY=ELECCDY/2. DZ=3.15/2. <===       519
         CALL AGSRESET                                                    520
         AG_DX = 0.3/2.                                                   520
         AG_DY = ELECCDY/2.                                               520
         AG_DZ = 3.15/2.                                                  520
         AG_TITLE = 'BOX'                                                 520
         AG_EXNAME = 'SHAPE'                                              520
         AG_PARLIST = 'DX_DY_DZ'                                          520
         CALL AXSHAPE                                                     520
C    *                                                                    520
5811  CALL AGSPOPB                                                        521
      GO TO BLOCK_FGWC, (5810 ,5820,5830)                                 521
C End of Local Procedure                                                  521
C ----------------------------------------------------------------------  522
C ------------ FGWD                                                       522
C Begin Local Procedure FGWD                                              523
C FGWD GAS FEED CONNECTION                                                523
5840  CONTINUE                                                            523
         AG_TITLE='FGWD'                                                  523
         AG_MODULE=MODULE                                                 523
         AG_MARK='V00'                                                    523
         CALL AGSPUSH                                                     523
         AG_MODULE=MODULE                                                 523
C    *                                                                    523
C    Geant ATTRIBUTE : FGWD ===> SEEN=1 COLO=1 <===                       524
         CALL AGSRESET                                                    524
         AG_SEEN = 1                                                      524
         AG_COLO = 1                                                      524
         AG_TITLE = 'FGWD'                                                524
         AG_EXNAME = 'ATTRIBUTE'                                          524
         AG_PARLIST = 'SEEN_COLO'                                         524
         CALL AXATTRIBUTE                                                 524
C    *                                                                    524
C    Geant MATERIAL : BRASS ===> inherited arguments used <===            524
         CALL AGSRESET                                                    525
         AG_TITLE = 'BRASS'                                               525
         AG_EXNAME = 'MATERIAL'                                           525
         AG_PARLIST = 'NONE'                                              525
         CALL AXMATERIAL                                                  525
C    *                                                                    525
C    Geant SHAPE : TUBE ===> RMIN=0.2 RMAX=0.5 DZ=2.1/2. <===             525
         CALL AGSRESET                                                    526
         AG_RMIN = 0.2                                                    526
         AG_RMAX = 0.5                                                    526
         AG_DZ = 2.1/2.                                                   526
         AG_TITLE = 'TUBE'                                                526
         AG_EXNAME = 'SHAPE'                                              526
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      526
         CALL AXSHAPE                                                     526
C    *                                                                    526
5841  CALL AGSPOPB                                                        527
      GO TO BLOCK_FGWD, (5840 ,5850,5860)                                 527
C End of Local Procedure                                                  527
C ----------------------------------------------------------------------  528
C ------------ FGWE                                                       528
C Begin Local Procedure FGWE                                              529
C FGWE HV BOARD                                                           529
5870  CONTINUE                                                            529
         AG_TITLE='FGWE'                                                  529
         AG_MODULE=MODULE                                                 529
         AG_MARK='V00'                                                    529
         CALL AGSPUSH                                                     529
         AG_MODULE=MODULE                                                 529
C    *                                                                    529
C    Geant ATTRIBUTE : FGWE ===> SEEN=1 COLO=1 <===                       530
         CALL AGSRESET                                                    530
         AG_SEEN = 1                                                      530
         AG_COLO = 1                                                      530
         AG_TITLE = 'FGWE'                                                530
         AG_EXNAME = 'ATTRIBUTE'                                          530
         AG_PARLIST = 'SEEN_COLO'                                         530
         CALL AXATTRIBUTE                                                 530
C    *                                                                    530
C    Geant MATERIAL : HVMIX ===> inherited arguments used <===            530
         CALL AGSRESET                                                    531
         AG_TITLE = 'HVMIX'                                               531
         AG_EXNAME = 'MATERIAL'                                           531
         AG_PARLIST = 'NONE'                                              531
         CALL AXMATERIAL                                                  531
C    *                                                                    531
C    Geant SHAPE : BOX ===> DX=ELECELEN/2. DY=ELECWIDTH/2., DZ=VOLELECDZ  531
C    /2. <===                                                             531
         CALL AGSRESET                                                    533
         AG_DX = ELECELEN/2.                                              533
         AG_DY = ELECWIDTH/2.                                             533
         AG_DZ = VOLELECDZ/2.                                             533
         AG_TITLE = 'BOX'                                                 533
         AG_EXNAME = 'SHAPE'                                              533
         AG_PARLIST = 'DX_DY_DZ'                                          533
         CALL AXSHAPE                                                     533
C    *                                                                    533
5871  CALL AGSPOPB                                                        534
      GO TO BLOCK_FGWE, (5870 ,5880)                                      534
C End of Local Procedure                                                  534
      WRITE(LOUT,5890)'===>>  [End AgML Module FGTDGEO3]  <<==='          534
5890  FORMAT(A32,/,/)                                                     534
C MODULE FGTDGEO3                                                         536
      END                                                                 536
