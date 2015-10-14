C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE FGTDGEOV                                                 1
C FGTDGEOV VERY FORWARD GEM TRACKING DETECTOR FOR ESTAR ERA               1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'FGTDGEOV'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'VERY FORWARD GEM TRACKING DETECTOR FOR ESTAR ERA'/   1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C CREATED 08/2012                                                         1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 CREATED /'08/2012'/                                    2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C AUTHOR JAN BALEWSKI MIT, WEI-MING ZHANG KSU, WILLIE LEIGHT MIT (MATERI  2
C AL MIXES), ANSELM VOSSEN CEEM: EXTENSION TO 12 DISCS                    2
      CHARACTER*40 AUTHOR /'JAN BALEWSKI MIT, WEI-MING ZHANG KSU, WILLIE  4
     * LEIGHT MIT (MATERIAL MIXES), ANSELM VOSSEN CEEM: EXTENSION TO 12   4
     *DISCS'/                                                             4
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  4
C - Advanced Geant interface                                              5
      CHARACTER*20 AG_MODULE,AG_TITLE,AG_EXNAME,AG_PARLIST,AG_CHDIR,      5
     *AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM                       5
      CHARACTER*4 AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,         5
     *AG_OPTION,AG_ORT,AG_MARK                                            5
      INTEGER AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,         5
     *AG_IMED,AG_IMAT,AG_IFIELD, AG_NLMAT,AG_IERROR,AG_NWBUF,AG_NPAR,     5
     *AG_ISTATUS,AG_IROT,AG_NBITS,AG_SERIAL, AG_ATTRIBUTE(6),AG_WORK,     5
     *AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,AG_JDU,AG_ORTI, AG_NDIV,    5
     *AG_IAXIS,AG_NDVMAX,AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,          5
     *AG_ENDSCR, AG_IDTYPE,AG_ISET,AG_IDET,AG_ISVOL,AG_LEVEL,AG_IRESER,   5
     *AG_IGEOM,AG_STANDALONE, AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,     5
     *AG_LDETU,AG_NPDIV,AG_NPDV,AG_NZ,AG_NCOPY, AG_IDEBU,AG_IGRAP,        5
     *AG_IHIST,AG_IMFLD,AG_ISIMU,AG_ENDSAVE,AG_CODE,AG_TRKTYP, AG_MODE,   5
     *AG_ECODE,AG_PDG,IPRIN, AG_ALPHAN,AG_ALPHAK(10),IGOR                 5
      REAL AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,AG_STMIN,     5
     *AG_DENS,AG_RADL,AG_ABSL, AG_THETAX,AG_PHIX,AG_THETAY,AG_PHIY,       5
     *AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ, AG_ALPHAL(10),        5
     *AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ,AG_TYPE, AG_DX, AG_DX1, AG_DX2,       5
     *AG_DY, AG_DY1, AG_DY2, AG_DMAXMS, AG_TWIST, AG_THET, AG_THE1,       5
     *AG_THE2, AG_PHI, AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2,       5
     *AG_RMIN, AG_RMAX, AG_RMN, AG_RMX, AG_ZI , AG_RMN1, AG_RMN2,         5
     *AG_RMX1, AG_RMX2, AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,     5
     *AG_DPHI, AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,       5
     *AG_STEP, AG_C0, AG_PAR, AG_AA, AG_ZZ, AG_WW, AG_LX, AG_LY, AG_LZ,   5
     *AG_HX, AG_HY, AG_HZ, AG_P1, AG_P2, AG_STACK,AG_UBUF,AG_XHMAX,       5
     *AG_YHMAX,AG_ZHMAX,AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN, AG_CHARGE,     5
     *AG_MASS,AG_TLIFE,AG_BRATIO                                          5
      PARAMETER (AG_LSTACK=130,AG_NWUHIT=10,AG_NWUVOL=3,AG_MAGIC=         5
     *-696969,AG_LDETU=250)                                               5
      COMMON/AGCGLOB/ AG_MODULE,AG_CHDIR,AG_LEVEL,AG_IDTYPE,AG_IERROR,    5
     *AG_STANDALONE,IPRIN, AG_IPRIN,AG_IGEOM,AG_IDEBU,AG_IGRAP,AG_IHIST,  5
     *AG_IMFLD,AG_ISIMU                                                   5
C Inherited variables a: saved during internal calls                      5
C b: reset at the entry in a block and then saved                         5
      COMMON/AGCPARA/ AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,        5
     *AG_ISHAPE, AG_IMED, AG_IMAT, AG_IFIELD, AG_FIELDM, AG_TMAXFD,       5
     *AG_STEMAX, AG_DEEMAX, AG_EPSIL, AG_STMIN, AG_DENS, AG_RADL,         5
     *AG_ABSL, AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1, AG_DY2, AG_RMN1,     5
     *AG_RMN2, AG_RMX1, AG_RMX2, AG_THET, AG_THE1, AG_THE2, AG_PHI,       5
     *AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2, AG_H1, AG_BL1,         5
     *AG_TL1, AG_H2, AG_BL2, AG_TL2, AG_RMIN, AG_RMAX, AG_DPHI,           5
     *AG_NPDIV, AG_NZ, AG_DZ, AG_TWIS, AG_LX, AG_LY, AG_LZ, AG_HX,        5
     *AG_HY, AG_HZ, AG_A, AG_ZA, AG_W, AG_NLMAT, AG_WORK, AG_SEEN,        5
     *AG_LSTY, AG_LWID, AG_COLO, AG_FILL, AG_SERIAL, AG_ISVOL,            5
     *AG_ISTATUS, AG_ZI(16), AG_RMN(16), AG_RMX(16), AG_VOLUME,           5
     *AG_MOTHER, AG_SHAPE, AG_CNICK, AG_ENDSAVE, AG_RESET1, AG_THETAX,    5
     *AG_PHIX,AG_THETAY,AG_PHIY,AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,      5
     *AG_ALFAZ, AG_X,AG_Y,AG_Z, AG_STEP,AG_C0,AG_NDIV,AG_IAXIS,           5
     *AG_NDVMAX,AG_ORTI,AG_NCOPY, AG_ALPHAN,AG_ALPHAK,AG_ALPHAL,          5
     *AG_RESET2, AG_KONLY, AG_ORT, AG_MARK                                5
C local variables valid inside same block                                 5
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       5
     *AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  5
     *AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         5
     *AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    5
     *AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          5
     *AG_OPTION                                                           5
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,        5
     *AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                             5
      COMMON/AGCSTAC/ AG_STACK(AG_LSTACK,15)                              5
C temporarly                                                              5
      EQUIVALENCE (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),           5
     *(AG_ALPHAX,AG_ALFAX), (AG_ALPHAY,AG_ALFAY),(AG_ALPHAZ,AG_ALFAZ),    5
     *(AG_TWIST,AG_TWIS),(AG_NPDIV,AG_NPDV),  (AG_P1,AG_HX),(AG_P2,       5
     *AG_HY)                                                              5
      REAL MIL_P,INCH_P                                                   5
      PARAMETER (INCH_P=2.54,MIL_P=0.00254)                               5
C - GEANT description of logical units                                    5
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            5
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      5
      COMMON/GCMAIL/ CHMAIL                                               5
      CHARACTER*132 CHMAIL                                                5
C - Natural constants (or considered as such)                             5
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   5
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        5
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       5
      INTEGER BLOCK_FGTM                                                  5
      INTEGER BLOCK_FGTD                                                  12
      INTEGER BLOCK_FGTQ                                                  12
      INTEGER BLOCK_FGVN                                                  12
      INTEGER BLOCK_FGZC                                                  12
      INTEGER BLOCK_FGVG                                                  12
      INTEGER BLOCK_FGVR                                                  12
      INTEGER BLOCK_FGVE                                                  12
      INTEGER BLOCK_FGQA                                                  12
      INTEGER BLOCK_FGQB                                                  12
      INTEGER BLOCK_FGQC                                                  12
      INTEGER BLOCK_FGQD                                                  12
      INTEGER BLOCK_FGQE                                                  12
      INTEGER BLOCK_FGQF                                                  12
      INTEGER BLOCK_FGXA                                                  12
      INTEGER BLOCK_FGXB                                                  12
      INTEGER BLOCK_FGXC                                                  12
      INTEGER BLOCK_FGXD                                                  12
      INTEGER BLOCK_FGWA                                                  12
      INTEGER BLOCK_FGWB                                                  12
      INTEGER BLOCK_FGWC                                                  12
      INTEGER BLOCK_FGWD                                                  12
      INTEGER BLOCK_FGWE                                                  12
      INTEGER BLOCK_FGCT                                                  12
      INTEGER BLOCK_FGCN                                                  12
      INTEGER BLOCK_FGCM                                                  12
      INTEGER BLOCK_FGDB                                                  12
      INTEGER BLOCK_FTQB                                                  12
      INTEGER BLOCK_FVNB                                                  12
      INTEGER BLOCK_FZCB                                                  12
      INTEGER BLOCK_FVGB                                                  12
      INTEGER BLOCK_FVRB                                                  12
      INTEGER BLOCK_FVEB                                                  12
      INTEGER BLOCK_FXCB                                                  12
      INTEGER BLOCK_FXDB                                                  12
      INTEGER BLOCK_FQAB                                                  12
      INTEGER BLOCK_FQBB                                                  12
      INTEGER BLOCK_FQCB                                                  12
      INTEGER BLOCK_FWEB                                                  12
      REAL *4 COOLTUBEINR/10.25/                                          13
      REAL *4 COOLTUBEINRT/7.0/                                           15
      REAL *4 COOLTUBEDZ/150/                                             17
      REAL *4 DISKINR/10.35/                                              19
      REAL *4 DISKINRT/7.0/                                               21
      REAL *4 DISKOUTR/39.4/                                              23
      REAL *4 FLATOUTR/37.0/                                              25
      REAL *4 FLATPERPANG/31.0/                                           27
      REAL *4 QUADTILTANG/-15.0/                                          29
      REAL *4 GRIDARCBIGR/29.15/                                          31
      REAL *4 GRIDARCSMALLR/20.63/                                        33
      REAL *4 GRIDRAYLONGPHI/60./                                         35
      REAL *4 GRIDRAYSHORTPHI/30./                                        37
      REAL *4 GRIDWIDTH/0.2/                                              39
      REAL *4 FRAMEWIDTH/1.15/                                            41
      REAL *4 FRAMEXYOFF/0.05/                                            43
      REAL *4 FRAMEDZ/0.198/                                              45
      REAL *4 VOLNOMEXDZ/1.288/                                           47
      REAL *4 VOLSENSDZ/0.312/                                            49
      REAL *4 VOLGEMDZ/0.612/                                             51
      REAL *4 VOLREADDZ/0.246/                                            53
      REAL *4 VOLELECDZ/4.3/                                              55
      REAL *4 ELECWIDTH/0.16/                                             57
      REAL *4 ELECADY/1.4/                                                59
      REAL *4 ELECCDY/4.5/                                                61
      REAL *4 ELECDANG/9./                                                63
      REAL *4 ELECELEN/22.5/                                              65
      REAL *4 ELECELENT/14.0/                                             67
      REAL CENTERZ,FGTLENZ,DISKLENZ, FLATANG1,FLATANG2,QUAD,DISK, XX,     70
     *DISKZ, K,ZZFGZ, XXFGX,XXFGQ, XXFGZ                                  70
C -                                                                       72
      REAL FGGG_FGSTCONFIG                                                72
      EQUIVALENCE (BANK_FGGG(3),FGGG_FGSTCONFIG,RE_FGGG_FGSTCONFIG),      72
     *(NAMES_FGGG(1),NFGGG_FGSTCONFIG)                                    72
      EQUIVALENCE (COMMEN_FGGG(1),CFGGG_FGSTCONFIG),(MAP_FGGG(1,1),       72
     *MFGGG_FGSTCONFIG)                                                   72
      CHARACTER CFGGG_FGSTCONFIG*40/' '/                                  72
      CHARACTER NFGGG_FGSTCONFIG*12/'R.FGSTCONFIG'/                       72
      INTEGER MFGGG_FGSTCONFIG(2)/1,1/                                    72
      REAL RE_FGGG_FGSTCONFIG                                             72
C -                                                                       72
      INTEGER LEN_FGGG,LLEN_FGGG,LINK_FGGG                                72
      PARAMETER (LEN_FGGG=3)                                              72
      PARAMETER (LLEN_FGGG=1)                                             72
      INTEGER FLAG_FGGG/0/,MAP_FGGG(2,LLEN_FGGG),IDNUM_FGGG(10)           72
      CHARACTER NAMES_FGGG(LLEN_FGGG)*12,COMMEN_FGGG(LLEN_FGGG)*40        72
      DATA IDNUM_FGGG/10*0/                                               72
      REAL BANK_FGGG(LEN_FGGG)                                            72
      CHARACTER*60 BPATH_FGGG/' '/                                        72
      SAVE FLAG_FGGG,MAP_FGGG,IDNUM_FGGG,NAMES_FGGG,COMMEN_FGGG,          72
     *BPATH_FGGG                                                          72
      DATA LINK_FGGG/0/,BANK_FGGG/LEN_FGGG*0/                             72
C *                                                                       72
C -                                                                       73
      REAL FGST_CONFIG                                                    73
      EQUIVALENCE (BANK_FGST(3),FGST_CONFIG,RE_FGST_CONFIG),              73
     *(NAMES_FGST(1),NFGST_CONFIG)                                        73
      EQUIVALENCE (COMMEN_FGST(1),CFGST_CONFIG),(MAP_FGST(1,1),           73
     *MFGST_CONFIG)                                                       73
      CHARACTER CFGST_CONFIG*40/' '/                                      73
      CHARACTER NFGST_CONFIG*12/'R.CONFIG'/                               73
      INTEGER MFGST_CONFIG(2)/1,1/                                        73
      REAL RE_FGST_CONFIG                                                 73
C -                                                                       73
      REAL FGST_NDISK                                                     73
      EQUIVALENCE (BANK_FGST(4),FGST_NDISK,RE_FGST_NDISK),                73
     *(NAMES_FGST(2),NFGST_NDISK)                                         73
      EQUIVALENCE (COMMEN_FGST(2),CFGST_NDISK),(MAP_FGST(1,2),            73
     *MFGST_NDISK)                                                        73
      CHARACTER CFGST_NDISK*40/' '/                                       73
      CHARACTER NFGST_NDISK*12/'R.NDISK'/                                 73
      INTEGER MFGST_NDISK(2)/1,1/                                         73
      REAL RE_FGST_NDISK                                                  73
C -                                                                       73
      INTEGER FGST_NQUAD                                                  73
      EQUIVALENCE (BANK_FGST(5),FGST_NQUAD,RE_FGST_NQUAD),                73
     *(NAMES_FGST(3),NFGST_NQUAD)                                         73
      EQUIVALENCE (COMMEN_FGST(3),CFGST_NQUAD),(MAP_FGST(1,3),            73
     *MFGST_NQUAD)                                                        73
      CHARACTER CFGST_NQUAD*40/' '/                                       73
      CHARACTER NFGST_NQUAD*12/'I.NQUAD'/                                 73
      INTEGER MFGST_NQUAD(2)/1,1/                                         73
      REAL RE_FGST_NQUAD                                                  73
C -                                                                       73
      REAL FGST_ZDISCA(12)                                                73
      EQUIVALENCE (BANK_FGST(6),FGST_ZDISCA,RE_FGST_ZDISCA),              73
     *(NAMES_FGST(4),NFGST_ZDISCA)                                        73
      EQUIVALENCE (COMMEN_FGST(4),CFGST_ZDISCA),(MAP_FGST(1,4),           73
     *MFGST_ZDISCA)                                                       73
      CHARACTER CFGST_ZDISCA*40/' '/                                      73
      CHARACTER NFGST_ZDISCA*12/'R.ZDISCA'/                               73
      INTEGER MFGST_ZDISCA(2)/12,1/                                       73
      REAL RE_FGST_ZDISCA                                                 73
C -                                                                       73
      REAL FGST_CTUBEDZ                                                   73
      EQUIVALENCE (BANK_FGST(18),FGST_CTUBEDZ,RE_FGST_CTUBEDZ),           73
     *(NAMES_FGST(5),NFGST_CTUBEDZ)                                       73
      EQUIVALENCE (COMMEN_FGST(5),CFGST_CTUBEDZ),(MAP_FGST(1,5),          73
     *MFGST_CTUBEDZ)                                                      73
      CHARACTER CFGST_CTUBEDZ*40/' '/                                     73
      CHARACTER NFGST_CTUBEDZ*12/'R.CTUBEDZ'/                             73
      INTEGER MFGST_CTUBEDZ(2)/1,1/                                       73
      REAL RE_FGST_CTUBEDZ                                                73
C -                                                                       73
      INTEGER LEN_FGST,LLEN_FGST,LINK_FGST                                73
      PARAMETER (LEN_FGST=18)                                             73
      PARAMETER (LLEN_FGST=5)                                             73
      INTEGER FLAG_FGST/0/,MAP_FGST(2,LLEN_FGST),IDNUM_FGST(10)           73
      CHARACTER NAMES_FGST(LLEN_FGST)*12,COMMEN_FGST(LLEN_FGST)*40        73
      DATA IDNUM_FGST/10*0/                                               73
      REAL BANK_FGST(LEN_FGST)                                            73
      CHARACTER*60 BPATH_FGST/' '/                                        73
      SAVE FLAG_FGST,MAP_FGST,IDNUM_FGST,NAMES_FGST,COMMEN_FGST,          73
     *BPATH_FGST                                                          73
      DATA LINK_FGST/0/,BANK_FGST/LEN_FGST*0/                             73
C *                                                                       73
C *                                                                       74
      CALL AGSBEGM(MODULE,IPRIN)                                          74
      IF (IPRIN.LT.0) RETURN                                              74
C Filling bank FGGG - FGT GEOMETRY CONTROL STRUCTURE                      74
      BANK_TITLE='FGT GEOMETRY CONTROL STRUCTURE'                         74
      BPATH_FGGG='FGGG*'                                                  74
      IDNUM_FGGG(1)=0                                                     75
C SELECTION OF NO. OF DISKS AND QUADRANTS                                 75
      FGGG_FGSTCONFIG=1.0                                                 75
      CFGGG_FGSTCONFIG='SELECTION OF NO. OF DISKS AND QUADRANTS'          75
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FGGG',    76
     *BPATH_FGGG,IDNUM_FGGG,1, MAP_FGGG,NAMES_FGGG,COMMEN_FGGG,           76
     *BANK_FGGG,LEN_FGGG,LLEN_FGGG,LINK_FGGG,FLAG_FGGG)                   76
C *                                                                       76
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FGGG',    76
     *BPATH_FGGG,IDNUM_FGGG,1, MAP_FGGG,NAMES_FGGG,COMMEN_FGGG,           76
     *BANK_FGGG,LEN_FGGG,LLEN_FGGG,LINK_FGGG,FLAG_FGGG)                   76
C *                                                                       77
      CALL AGSBEGM(MODULE,IPRIN)                                          77
      IF (IPRIN.LT.0) RETURN                                              77
C Filling bank FGST - FGT GEOMETRY FOR YEAR 2012                          77
      BANK_TITLE='FGT GEOMETRY FOR YEAR 2012'                             77
      BPATH_FGST='FGST*'                                                  77
      IDNUM_FGST(1)=0                                                     78
C VERSIONING OF THE FGST GEOMETRY DATA                                    78
      FGST_CONFIG=3.0                                                     78
      CFGST_CONFIG='VERSIONING OF THE FGST GEOMETRY DATA'                 78
C NUMBER OF DISKS                                                         79
      FGST_NDISK=6                                                        79
      CFGST_NDISK='NUMBER OF DISKS'                                       79
C NUMBER QUADRANTS IN A DISKS                                             80
      FGST_NQUAD=4                                                        80
      CFGST_NQUAD='NUMBER QUADRANTS IN A DISKS'                           80
C Z-CENTER OF DISK SENSTIVE VOLUME FROM CENTER IN STAR , AS MEASURED IN   82
C 2011-1                                                                  82
      FGST_ZDISCA(1)=67.3990                                              82
      FGST_ZDISCA(2)=77.8765                                              82
      FGST_ZDISCA(3)=87.0840                                              82
      FGST_ZDISCA(4)=97.4821                                              82
      FGST_ZDISCA(5)=108.9121                                             82
      FGST_ZDISCA(6)=118.9927                                             82
      CFGST_ZDISCA='Z-CENTER OF DISK SENSTIVE VOLUME FROM CENTER IN STAR  82
     * , AS MEASURED IN 2011-1'                                           82
C COOLING TUBE DZ                                                         83
      FGST_CTUBEDZ=150.0                                                  83
      CFGST_CTUBEDZ='COOLING TUBE DZ'                                     83
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FGST',    84
     *BPATH_FGST,IDNUM_FGST,1, MAP_FGST,NAMES_FGST,COMMEN_FGST,           84
     *BANK_FGST,LEN_FGST,LLEN_FGST,LINK_FGST,FLAG_FGST)                   84
C *                                                                       84
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FGST',    84
     *BPATH_FGST,IDNUM_FGST,1, MAP_FGST,NAMES_FGST,COMMEN_FGST,           84
     *BANK_FGST,LEN_FGST,LLEN_FGST,LINK_FGST,FLAG_FGST)                   84
C *                                                                       85
      CALL AGSBEGM(MODULE,IPRIN)                                          85
      IF (IPRIN.LT.0) RETURN                                              85
C Filling bank FGST - FGT GEOMETRY WITH ONE QUADRANT IN ONE DISK          85
      BANK_TITLE='FGT GEOMETRY WITH ONE QUADRANT IN ONE DISK'             85
      BPATH_FGST='FGST*'                                                  85
      IDNUM_FGST(1)=0                                                     86
C VERSIONING OF THE FGST GEOMETRY DATA                                    86
      FGST_CONFIG=2.0                                                     86
      CFGST_CONFIG='VERSIONING OF THE FGST GEOMETRY DATA'                 86
C NUMBER OF DISKS                                                         87
      FGST_NDISK=6                                                        87
      CFGST_NDISK='NUMBER OF DISKS'                                       87
C NUMBER QUADRANTS IN A DISKS                                             88
      FGST_NQUAD=4                                                        88
      CFGST_NQUAD='NUMBER QUADRANTS IN A DISKS'                           88
C Z-CENTER OF DISK SENSTIVE VOLUME FROM CENTER IN STAR , I                89
      FGST_ZDISCA(1)=70.                                                  89
      FGST_ZDISCA(2)=80.                                                  89
      FGST_ZDISCA(3)=90.                                                  89
      FGST_ZDISCA(4)=100.                                                 89
      FGST_ZDISCA(5)=110.                                                 89
      FGST_ZDISCA(6)=120.                                                 89
      CFGST_ZDISCA='Z-CENTER OF DISK SENSTIVE VOLUME FROM CENTER IN STAR  89
     * , I'                                                               89
C COOLING TUBE DZ                                                         90
      FGST_CTUBEDZ=150.0                                                  90
      CFGST_CTUBEDZ='COOLING TUBE DZ'                                     90
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FGST',    91
     *BPATH_FGST,IDNUM_FGST,1, MAP_FGST,NAMES_FGST,COMMEN_FGST,           91
     *BANK_FGST,LEN_FGST,LLEN_FGST,LINK_FGST,FLAG_FGST)                   91
C *                                                                       91
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FGST',    91
     *BPATH_FGST,IDNUM_FGST,1, MAP_FGST,NAMES_FGST,COMMEN_FGST,           91
     *BANK_FGST,LEN_FGST,LLEN_FGST,LINK_FGST,FLAG_FGST)                   91
C *                                                                       92
      CALL AGSBEGM(MODULE,IPRIN)                                          92
      IF (IPRIN.LT.0) RETURN                                              92
C Filling bank FGST - FGT GEOMETRY FOR YEAR 2014                          92
      BANK_TITLE='FGT GEOMETRY FOR YEAR 2014'                             92
      BPATH_FGST='FGST*'                                                  92
      IDNUM_FGST(1)=0                                                     93
C VERSIONING OF THE FGST GEOMETRY DATA                                    93
      FGST_CONFIG=1.0                                                     93
      CFGST_CONFIG='VERSIONING OF THE FGST GEOMETRY DATA'                 93
C NUMBER OF DISKS                                                         94
      FGST_NDISK=12                                                       94
      CFGST_NDISK='NUMBER OF DISKS'                                       94
C NUMBER QUADRANTS IN A DISKS                                             95
      FGST_NQUAD=4                                                        95
      CFGST_NQUAD='NUMBER QUADRANTS IN A DISKS'                           95
C Z-CENTER OF DISK SENSTIVE VOLUME FROM CENTER IN STAR , CTUBEDZ = 150.0  99
C  ! COOLING TUBE DZ                                                      99
      FGST_ZDISCA(1)=67.3990                                              99
      FGST_ZDISCA(2)=77.8765                                              99
      FGST_ZDISCA(3)=87.0840                                              99
      FGST_ZDISCA(4)=97.4821                                              99
      FGST_ZDISCA(5)=108.9121                                             99
      FGST_ZDISCA(6)=118.9927                                             99
      FGST_ZDISCA(7)=130.0000                                             99
      FGST_ZDISCA(8)=140.0000                                             99
      FGST_ZDISCA(9)=150.0000                                             99
      FGST_ZDISCA(10)=170.0000                                            99
      FGST_ZDISCA(11)=190.0000                                            99
      FGST_ZDISCA(12)=200.0000                                            99
      CFGST_ZDISCA='Z-CENTER OF DISK SENSTIVE VOLUME FROM CENTER IN STAR  99
     * , CTUBEDZ = 150.0 ! COOLING TUBE DZ'                               99
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FGST',    100
     *BPATH_FGST,IDNUM_FGST,1, MAP_FGST,NAMES_FGST,COMMEN_FGST,           100
     *BANK_FGST,LEN_FGST,LLEN_FGST,LINK_FGST,FLAG_FGST)                   100
C *                                                                       100
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FGST',    100
     *BPATH_FGST,IDNUM_FGST,1, MAP_FGST,NAMES_FGST,COMMEN_FGST,           100
     *BANK_FGST,LEN_FGST,LLEN_FGST,LINK_FGST,FLAG_FGST)                   100
C *                                                                       101
      CALL AGSBEGM(MODULE,IPRIN)                                          101
      IF (IPRIN.LT.0) RETURN                                              101
C Filling bank FGST - FGT GEOMETRY FOR YEAR 2014                          101
      BANK_TITLE='FGT GEOMETRY FOR YEAR 2014'                             101
      BPATH_FGST='FGST*'                                                  101
      IDNUM_FGST(1)=0                                                     102
C VERSIONING OF THE FGST GEOMETRY DATA                                    102
      FGST_CONFIG=4.0                                                     102
      CFGST_CONFIG='VERSIONING OF THE FGST GEOMETRY DATA'                 102
C NUMBER OF DISKS                                                         103
      FGST_NDISK=6                                                        103
      CFGST_NDISK='NUMBER OF DISKS'                                       103
C NUMBER QUADRANTS IN A DISKS                                             104
      FGST_NQUAD=4                                                        104
      CFGST_NQUAD='NUMBER QUADRANTS IN A DISKS'                           104
C Z-CENTER                                                                106
      FGST_ZDISCA(1)=93.500                                               106
      FGST_ZDISCA(2)=100.000                                              106
      FGST_ZDISCA(3)=193.500                                              106
      FGST_ZDISCA(4)=200.000                                              106
      FGST_ZDISCA(5)=293.500                                              106
      FGST_ZDISCA(6)=300.00                                               106
      CFGST_ZDISCA='Z-CENTER'                                             106
C COOLING TUBE DZ                                                         107
      FGST_CTUBEDZ=270.0                                                  107
      CFGST_CTUBEDZ='COOLING TUBE DZ'                                     107
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FGST',    108
     *BPATH_FGST,IDNUM_FGST,1, MAP_FGST,NAMES_FGST,COMMEN_FGST,           108
     *BANK_FGST,LEN_FGST,LLEN_FGST,LINK_FGST,FLAG_FGST)                   108
C *                                                                       108
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FGST',    108
     *BPATH_FGST,IDNUM_FGST,1, MAP_FGST,NAMES_FGST,COMMEN_FGST,           108
     *BANK_FGST,LEN_FGST,LLEN_FGST,LINK_FGST,FLAG_FGST)                   108
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.281 <===                     109
      CALL AGSRESET                                                       109
      AG_A = 28.08                                                        109
      AG_Z = 14                                                           109
      AG_W = 0.281                                                        109
      AG_TITLE = 'SI'                                                     109
      AG_EXNAME = 'COMPONENT'                                             109
      AG_PARLIST = 'A_Z_W'                                                109
      CALL AXCOMPONENT                                                    109
C *                                                                       109
C Geant COMPONENT : O ===> A=16 Z=8 W=0.467 <===                          109
      CALL AGSRESET                                                       110
      AG_A = 16                                                           110
      AG_Z = 8                                                            110
      AG_W = 0.467                                                        110
      AG_TITLE = 'O'                                                      110
      AG_EXNAME = 'COMPONENT'                                             110
      AG_PARLIST = 'A_Z_W'                                                110
      CALL AXCOMPONENT                                                    110
C *                                                                       110
C Geant COMPONENT : C ===> A=12 Z=6 W=0.220 <===                          110
      CALL AGSRESET                                                       111
      AG_A = 12                                                           111
      AG_Z = 6                                                            111
      AG_W = 0.220                                                        111
      AG_TITLE = 'C'                                                      111
      AG_EXNAME = 'COMPONENT'                                             111
      AG_PARLIST = 'A_Z_W'                                                111
      CALL AXCOMPONENT                                                    111
C *                                                                       111
C Geant COMPONENT : H ===> A=1 Z=1 W=0.032 <===                           111
      CALL AGSRESET                                                       112
      AG_A = 1                                                            112
      AG_Z = 1                                                            112
      AG_W = 0.032                                                        112
      AG_TITLE = 'H'                                                      112
      AG_EXNAME = 'COMPONENT'                                             112
      AG_PARLIST = 'A_Z_W'                                                112
      CALL AXCOMPONENT                                                    112
C *                                                                       112
C Geant MIXTURE : FR4 ===> DENS=1.80 <===                                 112
      CALL AGSRESET                                                       113
      AG_DENS = 1.80                                                      113
      AG_TITLE = 'FR4'                                                    113
      AG_EXNAME = 'MIXTURE'                                               113
      AG_PARLIST = 'DENS'                                                 113
      CALL AXMIXTURE                                                      113
C *                                                                       113
C Geant COMPONENT : O ===> A=16 Z=8 W=0.062 <===                          113
      CALL AGSRESET                                                       114
      AG_A = 16                                                           114
      AG_Z = 8                                                            114
      AG_W = 0.062                                                        114
      AG_TITLE = 'O'                                                      114
      AG_EXNAME = 'COMPONENT'                                             114
      AG_PARLIST = 'A_Z_W'                                                114
      CALL AXCOMPONENT                                                    114
C *                                                                       114
C Geant COMPONENT : C ===> A=12 Z=6 W=0.892 <===                          114
      CALL AGSRESET                                                       115
      AG_A = 12                                                           115
      AG_Z = 6                                                            115
      AG_W = 0.892                                                        115
      AG_TITLE = 'C'                                                      115
      AG_EXNAME = 'COMPONENT'                                             115
      AG_PARLIST = 'A_Z_W'                                                115
      CALL AXCOMPONENT                                                    115
C *                                                                       115
C Geant COMPONENT : H ===> A=1 Z=1 W=0.019 <===                           115
      CALL AGSRESET                                                       116
      AG_A = 1                                                            116
      AG_Z = 1                                                            116
      AG_W = 0.019                                                        116
      AG_TITLE = 'H'                                                      116
      AG_EXNAME = 'COMPONENT'                                             116
      AG_PARLIST = 'A_Z_W'                                                116
      CALL AXCOMPONENT                                                    116
C *                                                                       116
C Geant COMPONENT : CL ===> A=35.5 Z=17 W=0.027 <===                      116
      CALL AGSRESET                                                       117
      AG_A = 35.5                                                         117
      AG_Z = 17                                                           117
      AG_W = 0.027                                                        117
      AG_TITLE = 'CL'                                                     117
      AG_EXNAME = 'COMPONENT'                                             117
      AG_PARLIST = 'A_Z_W'                                                117
      CALL AXCOMPONENT                                                    117
C *                                                                       117
C Geant MIXTURE : CFRPMIX ===> DENS=1.78 <===                             117
      CALL AGSRESET                                                       118
      AG_DENS = 1.78                                                      118
      AG_TITLE = 'CFRPMIX'                                                118
      AG_EXNAME = 'MIXTURE'                                               118
      AG_PARLIST = 'DENS'                                                 118
      CALL AXMIXTURE                                                      118
C *                                                                       118
C Geant COMPONENT : O ===> A=16 Z=8 W=0.142 <===                          118
      CALL AGSRESET                                                       119
      AG_A = 16                                                           119
      AG_Z = 8                                                            119
      AG_W = 0.142                                                        119
      AG_TITLE = 'O'                                                      119
      AG_EXNAME = 'COMPONENT'                                             119
      AG_PARLIST = 'A_Z_W'                                                119
      CALL AXCOMPONENT                                                    119
C *                                                                       119
C Geant COMPONENT : C ===> A=12 Z=6 W=0.637 <===                          119
      CALL AGSRESET                                                       120
      AG_A = 12                                                           120
      AG_Z = 6                                                            120
      AG_W = 0.637                                                        120
      AG_TITLE = 'C'                                                      120
      AG_EXNAME = 'COMPONENT'                                             120
      AG_PARLIST = 'A_Z_W'                                                120
      CALL AXCOMPONENT                                                    120
C *                                                                       120
C Geant COMPONENT : H ===> A=1 Z=1 W=0.097 <===                           120
      CALL AGSRESET                                                       121
      AG_A = 1                                                            121
      AG_Z = 1                                                            121
      AG_W = 0.097                                                        121
      AG_TITLE = 'H'                                                      121
      AG_EXNAME = 'COMPONENT'                                             121
      AG_PARLIST = 'A_Z_W'                                                121
      CALL AXCOMPONENT                                                    121
C *                                                                       121
C Geant COMPONENT : N ===> A=14 Z=7 W=0.124 <===                          121
      CALL AGSRESET                                                       122
      AG_A = 14                                                           122
      AG_Z = 7                                                            122
      AG_W = 0.124                                                        122
      AG_TITLE = 'N'                                                      122
      AG_EXNAME = 'COMPONENT'                                             122
      AG_PARLIST = 'A_Z_W'                                                122
      CALL AXCOMPONENT                                                    122
C *                                                                       122
C Geant MIXTURE : NYLON ===> DENS=1.15 <===                               122
      CALL AGSRESET                                                       123
      AG_DENS = 1.15                                                      123
      AG_TITLE = 'NYLON'                                                  123
      AG_EXNAME = 'MIXTURE'                                               123
      AG_PARLIST = 'DENS'                                                 123
      CALL AXMIXTURE                                                      123
C *                                                                       123
C Geant COMPONENT : O ===> A=16 Z=8 W=0.129 <===                          123
      CALL AGSRESET                                                       124
      AG_A = 16                                                           124
      AG_Z = 8                                                            124
      AG_W = 0.129                                                        124
      AG_TITLE = 'O'                                                      124
      AG_EXNAME = 'COMPONENT'                                             124
      AG_PARLIST = 'A_Z_W'                                                124
      CALL AXCOMPONENT                                                    124
C *                                                                       124
C Geant COMPONENT : C ===> A=12 Z=6 W=0.579 <===                          124
      CALL AGSRESET                                                       125
      AG_A = 12                                                           125
      AG_Z = 6                                                            125
      AG_W = 0.579                                                        125
      AG_TITLE = 'C'                                                      125
      AG_EXNAME = 'COMPONENT'                                             125
      AG_PARLIST = 'A_Z_W'                                                125
      CALL AXCOMPONENT                                                    125
C *                                                                       125
C Geant COMPONENT : H ===> A=1 Z=1 W=0.088 <===                           125
      CALL AGSRESET                                                       126
      AG_A = 1                                                            126
      AG_Z = 1                                                            126
      AG_W = 0.088                                                        126
      AG_TITLE = 'H'                                                      126
      AG_EXNAME = 'COMPONENT'                                             126
      AG_PARLIST = 'A_Z_W'                                                126
      CALL AXCOMPONENT                                                    126
C *                                                                       126
C Geant COMPONENT : N ===> A=14 Z=7 W=0.112 <===                          126
      CALL AGSRESET                                                       127
      AG_A = 14                                                           127
      AG_Z = 7                                                            127
      AG_W = 0.112                                                        127
      AG_TITLE = 'N'                                                      127
      AG_EXNAME = 'COMPONENT'                                             127
      AG_PARLIST = 'A_Z_W'                                                127
      CALL AXCOMPONENT                                                    127
C *                                                                       127
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.092 <===                        127
      CALL AGSRESET                                                       128
      AG_A = 27                                                           128
      AG_Z = 13                                                           128
      AG_W = 0.092                                                        128
      AG_TITLE = 'AL'                                                     128
      AG_EXNAME = 'COMPONENT'                                             128
      AG_PARLIST = 'A_Z_W'                                                128
      CALL AXCOMPONENT                                                    128
C *                                                                       128
C Geant MIXTURE : COOLMIX ===> DENS=1.214 <===                            128
      CALL AGSRESET                                                       129
      AG_DENS = 1.214                                                     129
      AG_TITLE = 'COOLMIX'                                                129
      AG_EXNAME = 'MIXTURE'                                               129
      AG_PARLIST = 'DENS'                                                 129
      CALL AXMIXTURE                                                      129
C *                                                                       129
C Geant COMPONENT : O ===> A=16 Z=8 W=0.090 <===                          129
      CALL AGSRESET                                                       130
      AG_A = 16                                                           130
      AG_Z = 8                                                            130
      AG_W = 0.090                                                        130
      AG_TITLE = 'O'                                                      130
      AG_EXNAME = 'COMPONENT'                                             130
      AG_PARLIST = 'A_Z_W'                                                130
      CALL AXCOMPONENT                                                    130
C *                                                                       130
C Geant COMPONENT : C ===> A=12 Z=6 W=0.796 <===                          130
      CALL AGSRESET                                                       131
      AG_A = 12                                                           131
      AG_Z = 6                                                            131
      AG_W = 0.796                                                        131
      AG_TITLE = 'C'                                                      131
      AG_EXNAME = 'COMPONENT'                                             131
      AG_PARLIST = 'A_Z_W'                                                131
      CALL AXCOMPONENT                                                    131
C *                                                                       131
C Geant COMPONENT : H ===> A=1 Z=1 W=0.023 <===                           131
      CALL AGSRESET                                                       132
      AG_A = 1                                                            132
      AG_Z = 1                                                            132
      AG_W = 0.023                                                        132
      AG_TITLE = 'H'                                                      132
      AG_EXNAME = 'COMPONENT'                                             132
      AG_PARLIST = 'A_Z_W'                                                132
      CALL AXCOMPONENT                                                    132
C *                                                                       132
C Geant COMPONENT : CL ===> A=35.5 Z=17 W=0.019 <===                      132
      CALL AGSRESET                                                       133
      AG_A = 35.5                                                         133
      AG_Z = 17                                                           133
      AG_W = 0.019                                                        133
      AG_TITLE = 'CL'                                                     133
      AG_EXNAME = 'COMPONENT'                                             133
      AG_PARLIST = 'A_Z_W'                                                133
      CALL AXCOMPONENT                                                    133
C *                                                                       133
C Geant COMPONENT : N ===> A=14 Z=7 W=0.025 <===                          133
      CALL AGSRESET                                                       134
      AG_A = 14                                                           134
      AG_Z = 7                                                            134
      AG_W = 0.025                                                        134
      AG_TITLE = 'N'                                                      134
      AG_EXNAME = 'COMPONENT'                                             134
      AG_PARLIST = 'A_Z_W'                                                134
      CALL AXCOMPONENT                                                    134
C *                                                                       134
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.002 <===                        134
      CALL AGSRESET                                                       135
      AG_A = 27                                                           135
      AG_Z = 13                                                           135
      AG_W = 0.002                                                        135
      AG_TITLE = 'AL'                                                     135
      AG_EXNAME = 'COMPONENT'                                             135
      AG_PARLIST = 'A_Z_W'                                                135
      CALL AXCOMPONENT                                                    135
C *                                                                       135
C Geant COMPONENT : CU ===> A=63.5 Z=29 W=0.039 <===                      135
      CALL AGSRESET                                                       136
      AG_A = 63.5                                                         136
      AG_Z = 29                                                           136
      AG_W = 0.039                                                        136
      AG_TITLE = 'CU'                                                     136
      AG_EXNAME = 'COMPONENT'                                             136
      AG_PARLIST = 'A_Z_W'                                                136
      CALL AXCOMPONENT                                                    136
C *                                                                       136
C Geant COMPONENT : AR ===> A=39.9 Z=18 W=0.006 <===                      136
      CALL AGSRESET                                                       137
      AG_A = 39.9                                                         137
      AG_Z = 18                                                           137
      AG_W = 0.006                                                        137
      AG_TITLE = 'AR'                                                     137
      AG_EXNAME = 'COMPONENT'                                             137
      AG_PARLIST = 'A_Z_W'                                                137
      CALL AXCOMPONENT                                                    137
C *                                                                       137
C Geant MIXTURE : NOMEXMIX ===> DENS=0.090 <===                           137
      CALL AGSRESET                                                       138
      AG_DENS = 0.090                                                     138
      AG_TITLE = 'NOMEXMIX'                                               138
      AG_EXNAME = 'MIXTURE'                                               138
      AG_PARLIST = 'DENS'                                                 138
      CALL AXMIXTURE                                                      138
C *                                                                       138
C Geant COMPONENT : O ===> A=16 Z=8 W=0.095 <===                          138
      CALL AGSRESET                                                       139
      AG_A = 16                                                           139
      AG_Z = 8                                                            139
      AG_W = 0.095                                                        139
      AG_TITLE = 'O'                                                      139
      AG_EXNAME = 'COMPONENT'                                             139
      AG_PARLIST = 'A_Z_W'                                                139
      CALL AXCOMPONENT                                                    139
C *                                                                       139
C Geant COMPONENT : C ===> A=12 Z=6 W=0.301 <===                          139
      CALL AGSRESET                                                       140
      AG_A = 12                                                           140
      AG_Z = 6                                                            140
      AG_W = 0.301                                                        140
      AG_TITLE = 'C'                                                      140
      AG_EXNAME = 'COMPONENT'                                             140
      AG_PARLIST = 'A_Z_W'                                                140
      CALL AXCOMPONENT                                                    140
C *                                                                       140
C Geant COMPONENT : H ===> A=1 Z=1 W=0.011 <===                           140
      CALL AGSRESET                                                       141
      AG_A = 1                                                            141
      AG_Z = 1                                                            141
      AG_W = 0.011                                                        141
      AG_TITLE = 'H'                                                      141
      AG_EXNAME = 'COMPONENT'                                             141
      AG_PARLIST = 'A_Z_W'                                                141
      CALL AXCOMPONENT                                                    141
C *                                                                       141
C Geant COMPONENT : N ===> A=14 Z=7 W=0.032 <===                          141
      CALL AGSRESET                                                       142
      AG_A = 14                                                           142
      AG_Z = 7                                                            142
      AG_W = 0.032                                                        142
      AG_TITLE = 'N'                                                      142
      AG_EXNAME = 'COMPONENT'                                             142
      AG_PARLIST = 'A_Z_W'                                                142
      CALL AXCOMPONENT                                                    142
C *                                                                       142
C Geant COMPONENT : CU ===> A=63.5 Z=29 W=0.547 <===                      142
      CALL AGSRESET                                                       143
      AG_A = 63.5                                                         143
      AG_Z = 29                                                           143
      AG_W = 0.547                                                        143
      AG_TITLE = 'CU'                                                     143
      AG_EXNAME = 'COMPONENT'                                             143
      AG_PARLIST = 'A_Z_W'                                                143
      CALL AXCOMPONENT                                                    143
C *                                                                       143
C Geant COMPONENT : AR ===> A=39.9 Z=18 W=0.014 <===                      143
      CALL AGSRESET                                                       144
      AG_A = 39.9                                                         144
      AG_Z = 18                                                           144
      AG_W = 0.014                                                        144
      AG_TITLE = 'AR'                                                     144
      AG_EXNAME = 'COMPONENT'                                             144
      AG_PARLIST = 'A_Z_W'                                                144
      CALL AXCOMPONENT                                                    144
C *                                                                       144
C Geant MIXTURE : GEMMIX ===> DENS=0.079 <===                             144
      CALL AGSRESET                                                       145
      AG_DENS = 0.079                                                     145
      AG_TITLE = 'GEMMIX'                                                 145
      AG_EXNAME = 'MIXTURE'                                               145
      AG_PARLIST = 'DENS'                                                 145
      CALL AXMIXTURE                                                      145
C *                                                                       145
C Geant COMPONENT : O ===> A=16 Z=8 W=0.160 <===                          145
      CALL AGSRESET                                                       146
      AG_A = 16                                                           146
      AG_Z = 8                                                            146
      AG_W = 0.160                                                        146
      AG_TITLE = 'O'                                                      146
      AG_EXNAME = 'COMPONENT'                                             146
      AG_PARLIST = 'A_Z_W'                                                146
      CALL AXCOMPONENT                                                    146
C *                                                                       146
C Geant COMPONENT : C ===> A=12 Z=6 W=0.508 <===                          146
      CALL AGSRESET                                                       147
      AG_A = 12                                                           147
      AG_Z = 6                                                            147
      AG_W = 0.508                                                        147
      AG_TITLE = 'C'                                                      147
      AG_EXNAME = 'COMPONENT'                                             147
      AG_PARLIST = 'A_Z_W'                                                147
      CALL AXCOMPONENT                                                    147
C *                                                                       147
C Geant COMPONENT : H ===> A=1 Z=1 W=0.020 <===                           147
      CALL AGSRESET                                                       148
      AG_A = 1                                                            148
      AG_Z = 1                                                            148
      AG_W = 0.020                                                        148
      AG_TITLE = 'H'                                                      148
      AG_EXNAME = 'COMPONENT'                                             148
      AG_PARLIST = 'A_Z_W'                                                148
      CALL AXCOMPONENT                                                    148
C *                                                                       148
C Geant COMPONENT : N ===> A=14 Z=7 W=0.051 <===                          148
      CALL AGSRESET                                                       149
      AG_A = 14                                                           149
      AG_Z = 7                                                            149
      AG_W = 0.051                                                        149
      AG_TITLE = 'N'                                                      149
      AG_EXNAME = 'COMPONENT'                                             149
      AG_PARLIST = 'A_Z_W'                                                149
      CALL AXCOMPONENT                                                    149
C *                                                                       149
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.003 <===                        149
      CALL AGSRESET                                                       150
      AG_A = 27                                                           150
      AG_Z = 13                                                           150
      AG_W = 0.003                                                        150
      AG_TITLE = 'AL'                                                     150
      AG_EXNAME = 'COMPONENT'                                             150
      AG_PARLIST = 'A_Z_W'                                                150
      CALL AXCOMPONENT                                                    150
C *                                                                       150
C Geant COMPONENT : CU ===> A=63.5 Z=29 W=0.255 <===                      150
      CALL AGSRESET                                                       151
      AG_A = 63.5                                                         151
      AG_Z = 29                                                           151
      AG_W = 0.255                                                        151
      AG_TITLE = 'CU'                                                     151
      AG_EXNAME = 'COMPONENT'                                             151
      AG_PARLIST = 'A_Z_W'                                                151
      CALL AXCOMPONENT                                                    151
C *                                                                       151
C Geant COMPONENT : AR ===> A=39.9 Z=18 W=0.003 <===                      151
      CALL AGSRESET                                                       152
      AG_A = 39.9                                                         152
      AG_Z = 18                                                           152
      AG_W = 0.003                                                        152
      AG_TITLE = 'AR'                                                     152
      AG_EXNAME = 'COMPONENT'                                             152
      AG_PARLIST = 'A_Z_W'                                                152
      CALL AXCOMPONENT                                                    152
C *                                                                       152
C Geant MIXTURE : READMIX ===> DENS=0.356 <===                            152
      CALL AGSRESET                                                       153
      AG_DENS = 0.356                                                     153
      AG_TITLE = 'READMIX'                                                153
      AG_EXNAME = 'MIXTURE'                                               153
      AG_PARLIST = 'DENS'                                                 153
      CALL AXMIXTURE                                                      153
C *                                                                       153
C Geant COMPONENT : SI ===> A=28.1 Z=14 W=0.191 <===                      153
      CALL AGSRESET                                                       154
      AG_A = 28.1                                                         154
      AG_Z = 14                                                           154
      AG_W = 0.191                                                        154
      AG_TITLE = 'SI'                                                     154
      AG_EXNAME = 'COMPONENT'                                             154
      AG_PARLIST = 'A_Z_W'                                                154
      CALL AXCOMPONENT                                                    154
C *                                                                       154
C Geant COMPONENT : O ===> A=16 Z=8 W=0.339 <===                          154
      CALL AGSRESET                                                       155
      AG_A = 16                                                           155
      AG_Z = 8                                                            155
      AG_W = 0.339                                                        155
      AG_TITLE = 'O'                                                      155
      AG_EXNAME = 'COMPONENT'                                             155
      AG_PARLIST = 'A_Z_W'                                                155
      CALL AXCOMPONENT                                                    155
C *                                                                       155
C Geant COMPONENT : C ===> A=12 Z=6 W=0.213 <===                          155
      CALL AGSRESET                                                       156
      AG_A = 12                                                           156
      AG_Z = 6                                                            156
      AG_W = 0.213                                                        156
      AG_TITLE = 'C'                                                      156
      AG_EXNAME = 'COMPONENT'                                             156
      AG_PARLIST = 'A_Z_W'                                                156
      CALL AXCOMPONENT                                                    156
C *                                                                       156
C Geant COMPONENT : H ===> A=1 Z=1 W=0.028 <===                           156
      CALL AGSRESET                                                       157
      AG_A = 1                                                            157
      AG_Z = 1                                                            157
      AG_W = 0.028                                                        157
      AG_TITLE = 'H'                                                      157
      AG_EXNAME = 'COMPONENT'                                             157
      AG_PARLIST = 'A_Z_W'                                                157
      CALL AXCOMPONENT                                                    157
C *                                                                       157
C Geant COMPONENT : CL ===> A=35.5 Z=17 W=0.009 <===                      157
      CALL AGSRESET                                                       158
      AG_A = 35.5                                                         158
      AG_Z = 17                                                           158
      AG_W = 0.009                                                        158
      AG_TITLE = 'CL'                                                     158
      AG_EXNAME = 'COMPONENT'                                             158
      AG_PARLIST = 'A_Z_W'                                                158
      CALL AXCOMPONENT                                                    158
C *                                                                       158
C Geant COMPONENT : CU ===> A=63.5 Z=29 W=0.122 <===                      158
      CALL AGSRESET                                                       159
      AG_A = 63.5                                                         159
      AG_Z = 29                                                           159
      AG_W = 0.122                                                        159
      AG_TITLE = 'CU'                                                     159
      AG_EXNAME = 'COMPONENT'                                             159
      AG_PARLIST = 'A_Z_W'                                                159
      CALL AXCOMPONENT                                                    159
C *                                                                       159
C Geant COMPONENT : PB ===> A=207 Z=82 W=0.039 <===                       159
      CALL AGSRESET                                                       160
      AG_A = 207                                                          160
      AG_Z = 82                                                           160
      AG_W = 0.039                                                        160
      AG_TITLE = 'PB'                                                     160
      AG_EXNAME = 'COMPONENT'                                             160
      AG_PARLIST = 'A_Z_W'                                                160
      CALL AXCOMPONENT                                                    160
C *                                                                       160
C Geant COMPONENT : SN ===> A=118.7 Z=50 W=0.059 <===                     160
      CALL AGSRESET                                                       161
      AG_A = 118.7                                                        161
      AG_Z = 50                                                           161
      AG_W = 0.059                                                        161
      AG_TITLE = 'SN'                                                     161
      AG_EXNAME = 'COMPONENT'                                             161
      AG_PARLIST = 'A_Z_W'                                                161
      CALL AXCOMPONENT                                                    161
C *                                                                       161
C Geant MIXTURE : APVMIX ===> DENS=2.535 <===                             161
      CALL AGSRESET                                                       162
      AG_DENS = 2.535                                                     162
      AG_TITLE = 'APVMIX'                                                 162
      AG_EXNAME = 'MIXTURE'                                               162
      AG_PARLIST = 'DENS'                                                 162
      CALL AXMIXTURE                                                      162
C *                                                                       162
C Geant COMPONENT : SI ===> A=28.1 Z=14 W=0.113 <===                      162
      CALL AGSRESET                                                       163
      AG_A = 28.1                                                         163
      AG_Z = 14                                                           163
      AG_W = 0.113                                                        163
      AG_TITLE = 'SI'                                                     163
      AG_EXNAME = 'COMPONENT'                                             163
      AG_PARLIST = 'A_Z_W'                                                163
      CALL AXCOMPONENT                                                    163
C *                                                                       163
C Geant COMPONENT : O ===> A=16 Z=8 W=0.241 <===                          163
      CALL AGSRESET                                                       164
      AG_A = 16                                                           164
      AG_Z = 8                                                            164
      AG_W = 0.241                                                        164
      AG_TITLE = 'O'                                                      164
      AG_EXNAME = 'COMPONENT'                                             164
      AG_PARLIST = 'A_Z_W'                                                164
      CALL AXCOMPONENT                                                    164
C *                                                                       164
C Geant COMPONENT : C ===> A=12 Z=6 W=0.248 <===                          164
      CALL AGSRESET                                                       165
      AG_A = 12                                                           165
      AG_Z = 6                                                            165
      AG_W = 0.248                                                        165
      AG_TITLE = 'C'                                                      165
      AG_EXNAME = 'COMPONENT'                                             165
      AG_PARLIST = 'A_Z_W'                                                165
      CALL AXCOMPONENT                                                    165
C *                                                                       165
C Geant COMPONENT : H ===> A=1 Z=1 W=0.023 <===                           165
      CALL AGSRESET                                                       166
      AG_A = 1                                                            166
      AG_Z = 1                                                            166
      AG_W = 0.023                                                        166
      AG_TITLE = 'H'                                                      166
      AG_EXNAME = 'COMPONENT'                                             166
      AG_PARLIST = 'A_Z_W'                                                166
      CALL AXCOMPONENT                                                    166
C *                                                                       166
C Geant COMPONENT : CU ===> A=63.5 Z=29 W=0.109 <===                      166
      CALL AGSRESET                                                       167
      AG_A = 63.5                                                         167
      AG_Z = 29                                                           167
      AG_W = 0.109                                                        167
      AG_TITLE = 'CU'                                                     167
      AG_EXNAME = 'COMPONENT'                                             167
      AG_PARLIST = 'A_Z_W'                                                167
      CALL AXCOMPONENT                                                    167
C *                                                                       167
C Geant COMPONENT : FE ===> A=55.8 Z=26 W=0.019 <===                      167
      CALL AGSRESET                                                       168
      AG_A = 55.8                                                         168
      AG_Z = 26                                                           168
      AG_W = 0.019                                                        168
      AG_TITLE = 'FE'                                                     168
      AG_EXNAME = 'COMPONENT'                                             168
      AG_PARLIST = 'A_Z_W'                                                168
      CALL AXCOMPONENT                                                    168
C *                                                                       168
C Geant COMPONENT : CR ===> A=52.0 Z=24 W=0.008 <===                      168
      CALL AGSRESET                                                       169
      AG_A = 52.0                                                         169
      AG_Z = 24                                                           169
      AG_W = 0.008                                                        169
      AG_TITLE = 'CR'                                                     169
      AG_EXNAME = 'COMPONENT'                                             169
      AG_PARLIST = 'A_Z_W'                                                169
      CALL AXCOMPONENT                                                    169
C *                                                                       169
C Geant COMPONENT : PB ===> A=207 Z=82 W=0.096 <===                       169
      CALL AGSRESET                                                       170
      AG_A = 207                                                          170
      AG_Z = 82                                                           170
      AG_W = 0.096                                                        170
      AG_TITLE = 'PB'                                                     170
      AG_EXNAME = 'COMPONENT'                                             170
      AG_PARLIST = 'A_Z_W'                                                170
      CALL AXCOMPONENT                                                    170
C *                                                                       170
C Geant COMPONENT : SN ===> A=118.7 Z=50 W=0.143 <===                     170
      CALL AGSRESET                                                       171
      AG_A = 118.7                                                        171
      AG_Z = 50                                                           171
      AG_W = 0.143                                                        171
      AG_TITLE = 'SN'                                                     171
      AG_EXNAME = 'COMPONENT'                                             171
      AG_PARLIST = 'A_Z_W'                                                171
      CALL AXCOMPONENT                                                    171
C *                                                                       171
C Geant MIXTURE : HVMIX ===> DENS=3.009 <===                              171
      CALL AGSRESET                                                       172
      AG_DENS = 3.009                                                     172
      AG_TITLE = 'HVMIX'                                                  172
      AG_EXNAME = 'MIXTURE'                                               172
      AG_PARLIST = 'DENS'                                                 172
      CALL AXMIXTURE                                                      172
C *                                                                       172
C Geant COMPONENT : SI ===> A=28.1 Z=14 W=0.122 <===                      172
      CALL AGSRESET                                                       173
      AG_A = 28.1                                                         173
      AG_Z = 14                                                           173
      AG_W = 0.122                                                        173
      AG_TITLE = 'SI'                                                     173
      AG_EXNAME = 'COMPONENT'                                             173
      AG_PARLIST = 'A_Z_W'                                                173
      CALL AXCOMPONENT                                                    173
C *                                                                       173
C Geant COMPONENT : O ===> A=16 Z=8 W=0.225 <===                          173
      CALL AGSRESET                                                       174
      AG_A = 16                                                           174
      AG_Z = 8                                                            174
      AG_W = 0.225                                                        174
      AG_TITLE = 'O'                                                      174
      AG_EXNAME = 'COMPONENT'                                             174
      AG_PARLIST = 'A_Z_W'                                                174
      CALL AXCOMPONENT                                                    174
C *                                                                       174
C Geant COMPONENT : C ===> A=12 Z=6 W=0.297 <===                          174
      CALL AGSRESET                                                       175
      AG_A = 12                                                           175
      AG_Z = 6                                                            175
      AG_W = 0.297                                                        175
      AG_TITLE = 'C'                                                      175
      AG_EXNAME = 'COMPONENT'                                             175
      AG_PARLIST = 'A_Z_W'                                                175
      CALL AXCOMPONENT                                                    175
C *                                                                       175
C Geant COMPONENT : H ===> A=1 Z=1 W=0.023 <===                           175
      CALL AGSRESET                                                       176
      AG_A = 1                                                            176
      AG_Z = 1                                                            176
      AG_W = 0.023                                                        176
      AG_TITLE = 'H'                                                      176
      AG_EXNAME = 'COMPONENT'                                             176
      AG_PARLIST = 'A_Z_W'                                                176
      CALL AXCOMPONENT                                                    176
C *                                                                       176
C Geant COMPONENT : N ===> A=14 Z=7 W=0.009 <===                          176
      CALL AGSRESET                                                       177
      AG_A = 14                                                           177
      AG_Z = 7                                                            177
      AG_W = 0.009                                                        177
      AG_TITLE = 'N'                                                      177
      AG_EXNAME = 'COMPONENT'                                             177
      AG_PARLIST = 'A_Z_W'                                                177
      CALL AXCOMPONENT                                                    177
C *                                                                       177
C Geant COMPONENT : CU ===> A=63.5 Z=29 W=0.045 <===                      177
      CALL AGSRESET                                                       178
      AG_A = 63.5                                                         178
      AG_Z = 29                                                           178
      AG_W = 0.045                                                        178
      AG_TITLE = 'CU'                                                     178
      AG_EXNAME = 'COMPONENT'                                             178
      AG_PARLIST = 'A_Z_W'                                                178
      CALL AXCOMPONENT                                                    178
C *                                                                       178
C Geant COMPONENT : FE ===> A=55.8 Z=26 W=0.055 <===                      178
      CALL AGSRESET                                                       179
      AG_A = 55.8                                                         179
      AG_Z = 26                                                           179
      AG_W = 0.055                                                        179
      AG_TITLE = 'FE'                                                     179
      AG_EXNAME = 'COMPONENT'                                             179
      AG_PARLIST = 'A_Z_W'                                                179
      CALL AXCOMPONENT                                                    179
C *                                                                       179
C Geant COMPONENT : NI ===> A=58.7 Z=28 W=0.022 <===                      179
      CALL AGSRESET                                                       180
      AG_A = 58.7                                                         180
      AG_Z = 28                                                           180
      AG_W = 0.022                                                        180
      AG_TITLE = 'NI'                                                     180
      AG_EXNAME = 'COMPONENT'                                             180
      AG_PARLIST = 'A_Z_W'                                                180
      CALL AXCOMPONENT                                                    180
C *                                                                       180
C Geant COMPONENT : PB ===> A=207 Z=82 W=0.081 <===                       180
      CALL AGSRESET                                                       181
      AG_A = 207                                                          181
      AG_Z = 82                                                           181
      AG_W = 0.081                                                        181
      AG_TITLE = 'PB'                                                     181
      AG_EXNAME = 'COMPONENT'                                             181
      AG_PARLIST = 'A_Z_W'                                                181
      CALL AXCOMPONENT                                                    181
C *                                                                       181
C Geant COMPONENT : SN ===> A=118.7 Z=50 W=0.121 <===                     181
      CALL AGSRESET                                                       182
      AG_A = 118.7                                                        182
      AG_Z = 50                                                           182
      AG_W = 0.121                                                        182
      AG_TITLE = 'SN'                                                     182
      AG_EXNAME = 'COMPONENT'                                             182
      AG_PARLIST = 'A_Z_W'                                                182
      CALL AXCOMPONENT                                                    182
C *                                                                       182
C Geant MIXTURE : CONMIX ===> DENS=1.816 <===                             182
      CALL AGSRESET                                                       183
      AG_DENS = 1.816                                                     183
      AG_TITLE = 'CONMIX'                                                 183
      AG_EXNAME = 'MIXTURE'                                               183
      AG_PARLIST = 'DENS'                                                 183
      CALL AXMIXTURE                                                      183
C *                                                                       183
C Geant COMPONENT : SI ===> A=28.1 Z=14 W=0.141 <===                      183
      CALL AGSRESET                                                       184
      AG_A = 28.1                                                         184
      AG_Z = 14                                                           184
      AG_W = 0.141                                                        184
      AG_TITLE = 'SI'                                                     184
      AG_EXNAME = 'COMPONENT'                                             184
      AG_PARLIST = 'A_Z_W'                                                184
      CALL AXCOMPONENT                                                    184
C *                                                                       184
C Geant COMPONENT : O ===> A=16 Z=8 W=0.196 <===                          184
      CALL AGSRESET                                                       185
      AG_A = 16                                                           185
      AG_Z = 8                                                            185
      AG_W = 0.196                                                        185
      AG_TITLE = 'O'                                                      185
      AG_EXNAME = 'COMPONENT'                                             185
      AG_PARLIST = 'A_Z_W'                                                185
      CALL AXCOMPONENT                                                    185
C *                                                                       185
C Geant COMPONENT : C ===> A=12 Z=6 W=0.419 <===                          185
      CALL AGSRESET                                                       186
      AG_A = 12                                                           186
      AG_Z = 6                                                            186
      AG_W = 0.419                                                        186
      AG_TITLE = 'C'                                                      186
      AG_EXNAME = 'COMPONENT'                                             186
      AG_PARLIST = 'A_Z_W'                                                186
      CALL AXCOMPONENT                                                    186
C *                                                                       186
C Geant COMPONENT : H ===> A=1 Z=1 W=0.008 <===                           186
      CALL AGSRESET                                                       187
      AG_A = 1                                                            187
      AG_Z = 1                                                            187
      AG_W = 0.008                                                        187
      AG_TITLE = 'H'                                                      187
      AG_EXNAME = 'COMPONENT'                                             187
      AG_PARLIST = 'A_Z_W'                                                187
      CALL AXCOMPONENT                                                    187
C *                                                                       187
C Geant COMPONENT : CU ===> A=63.5 Z=29 W=0.156 <===                      187
      CALL AGSRESET                                                       188
      AG_A = 63.5                                                         188
      AG_Z = 29                                                           188
      AG_W = 0.156                                                        188
      AG_TITLE = 'CU'                                                     188
      AG_EXNAME = 'COMPONENT'                                             188
      AG_PARLIST = 'A_Z_W'                                                188
      CALL AXCOMPONENT                                                    188
C *                                                                       188
C Geant COMPONENT : PB ===> A=207 Z=82 W=0.032 <===                       188
      CALL AGSRESET                                                       189
      AG_A = 207                                                          189
      AG_Z = 82                                                           189
      AG_W = 0.032                                                        189
      AG_TITLE = 'PB'                                                     189
      AG_EXNAME = 'COMPONENT'                                             189
      AG_PARLIST = 'A_Z_W'                                                189
      CALL AXCOMPONENT                                                    189
C *                                                                       189
C Geant COMPONENT : SN ===> A=118.7 Z=50 W=0.048 <===                     189
      CALL AGSRESET                                                       190
      AG_A = 118.7                                                        190
      AG_Z = 50                                                           190
      AG_W = 0.048                                                        190
      AG_TITLE = 'SN'                                                     190
      AG_EXNAME = 'COMPONENT'                                             190
      AG_PARLIST = 'A_Z_W'                                                190
      CALL AXCOMPONENT                                                    190
C *                                                                       190
C Geant MIXTURE : TERMMIX ===> DENS=2.55 <===                             190
      CALL AGSRESET                                                       191
      AG_DENS = 2.55                                                      191
      AG_TITLE = 'TERMMIX'                                                191
      AG_EXNAME = 'MIXTURE'                                               191
      AG_PARLIST = 'DENS'                                                 191
      CALL AXMIXTURE                                                      191
C *                                                                       191
C Geant COMPONENT : CU ===> A=63.5 Z=29 W=0.63 <===                       191
      CALL AGSRESET                                                       192
      AG_A = 63.5                                                         192
      AG_Z = 29                                                           192
      AG_W = 0.63                                                         192
      AG_TITLE = 'CU'                                                     192
      AG_EXNAME = 'COMPONENT'                                             192
      AG_PARLIST = 'A_Z_W'                                                192
      CALL AXCOMPONENT                                                    192
C *                                                                       192
C Geant COMPONENT : ZN ===> A=65.4 Z=30 W=0.37 <===                       192
      CALL AGSRESET                                                       193
      AG_A = 65.4                                                         193
      AG_Z = 30                                                           193
      AG_W = 0.37                                                         193
      AG_TITLE = 'ZN'                                                     193
      AG_EXNAME = 'COMPONENT'                                             193
      AG_PARLIST = 'A_Z_W'                                                193
      CALL AXCOMPONENT                                                    193
C *                                                                       193
C Geant MIXTURE : BRASS ===> DENS=8.4 <===                                193
      CALL AGSRESET                                                       194
      AG_DENS = 8.4                                                       194
      AG_TITLE = 'BRASS'                                                  194
      AG_EXNAME = 'MIXTURE'                                               194
      AG_PARLIST = 'DENS'                                                 194
      CALL AXMIXTURE                                                      194
C *                                                                       194
C Geant COMPONENT : AR ===> A=39.95 Z=18. W=0.700 <===                    194
      CALL AGSRESET                                                       195
      AG_A = 39.95                                                        195
      AG_Z = 18.                                                          195
      AG_W = 0.700                                                        195
      AG_TITLE = 'AR'                                                     195
      AG_EXNAME = 'COMPONENT'                                             195
      AG_PARLIST = 'A_Z_W'                                                195
      CALL AXCOMPONENT                                                    195
C *                                                                       195
C Geant COMPONENT : O ===> A=16. Z=8. W=0.218 <===                        195
      CALL AGSRESET                                                       196
      AG_A = 16.                                                          196
      AG_Z = 8.                                                           196
      AG_W = 0.218                                                        196
      AG_TITLE = 'O'                                                      196
      AG_EXNAME = 'COMPONENT'                                             196
      AG_PARLIST = 'A_Z_W'                                                196
      CALL AXCOMPONENT                                                    196
C *                                                                       196
C Geant COMPONENT : C ===> A=12.01 Z=6. W=0.082 <===                      196
      CALL AGSRESET                                                       197
      AG_A = 12.01                                                        197
      AG_Z = 6.                                                           197
      AG_W = 0.082                                                        197
      AG_TITLE = 'C'                                                      197
      AG_EXNAME = 'COMPONENT'                                             197
      AG_PARLIST = 'A_Z_W'                                                197
      CALL AXCOMPONENT                                                    197
C *                                                                       197
C Geant MIXTURE : ARCO2MIX ===> ISVOL=1 DENS=0.0018015 <===               197
      CALL AGSRESET                                                       198
      AG_ISVOL = 1                                                        198
      AG_DENS = 0.0018015                                                 198
      AG_TITLE = 'ARCO2MIX'                                               198
      AG_EXNAME = 'MIXTURE'                                               198
      AG_PARLIST = 'ISVOL_DENS'                                           198
      CALL AXMIXTURE                                                      198
C *                                                                       198
C use default FGGG bank                                                   198
      BPATH_FGGG='FGGG*'                                                  199
      IDNUM_FGGG(1)=0                                                     199
      CALL AGDGETP('FGTDGEOV','*',-999,'FGGG',' ',BANK_FGGG, BPATH_FGGG,  199
     *IDNUM_FGGG,1,MAP_FGGG,NAMES_FGGG,BANK_FGGG,LEN_FGGG,LLEN_FGGG,      199
     *LINK_FGGG,FLAG_FGGG)                                                199
C use FGST bank                                                           200
      BPATH_FGST='FGST*'                                                  200
      IDNUM_FGST(1)=0                                                     200
      FGST_CONFIG=FGGG_FGSTCONFIG                                         200
      CALL AGDGETP('FGTDGEOV','*',-999,'FGST',NFGST_CONFIG,               200
     *RE_FGST_CONFIG,BPATH_FGST,IDNUM_FGST,1,MAP_FGST,NAMES_FGST,         200
     *BANK_FGST,LEN_FGST,LLEN_FGST,LINK_FGST,FLAG_FGST)                   200
      DISKLENZ=VOLNOMEXDZ+VOLSENSDZ+VOLGEMDZ+VOLREADDZ+VOLELECDZ          201
      COOLTUBEDZ = FGST_CTUBEDZ                                           202
      FGTLENZ = COOLTUBEDZ                                                203
      CENTERZ = FGST_ZDISCA(1) +FGTLENZ/2.0 -( VOLNOMEXDZ+VOLSENSDZ/2.)   204
      XX=ACOS(FLATOUTR/DISKOUTR)                                          205
      FLATANG1=FLATPERPANG-XX/DEGRAD                                      206
      FLATANG2=FLATPERPANG+XX/DEGRAD                                      207
      CALL AGSBEGM(MODULE,IPRIN)                                          208
C Check IPRIN<0                                                           208
      IF (IPRIN.LT.0) RETURN                                              208
C Create FGTM :                                                           208
C Call To Local Procedure FGTM                                            208
      ASSIGN 5020 TO BLOCK_FGTM                                           208
      GO TO  5010                                                         208
5020  CONTINUE                                                            208
C *                                                                       208
C Geant POSITION : FGTM ===> MOTHER='IDSM' Z=CENTERZ <===                 209
      CALL AGSRESET                                                       209
      AG_MOTHER = 'IDSM'                                                  209
      AG_Z = CENTERZ                                                      209
      AG_TITLE = 'FGTM'                                                   209
      AG_EXNAME = 'POSITION'                                              209
      AG_PARLIST = 'MOTHER_Z'                                             209
      CALL AXPOSITION                                                     209
C *                                                                       209
C ----------------------------------------------------------------------  209
C ------------ FGTM                                                       209
      CALL AGSENDM                                                        211
      RETURN                                                              211
C Begin Local Procedure FGTM                                              211
C FGTM MOTHER VOLUME FOR FGT ASSEMBLY                                     211
5010  CONTINUE                                                            211
         AG_TITLE='FGTM'                                                  211
         AG_MODULE=MODULE                                                 211
         AG_MARK='V00'                                                    211
         CALL AGSPUSH                                                     211
         AG_MODULE=MODULE                                                 211
C    *                                                                    211
C    Geant ATTRIBUTE : FGTM ===> SEEN=0 COLO=1 <===                       212
         CALL AGSRESET                                                    212
         AG_SEEN = 0                                                      212
         AG_COLO = 1                                                      212
         AG_TITLE = 'FGTM'                                                212
         AG_EXNAME = 'ATTRIBUTE'                                          212
         AG_PARLIST = 'SEEN_COLO'                                         212
         CALL AXATTRIBUTE                                                 212
C    *                                                                    212
C    Geant MATERIAL : AIR ===> inherited arguments used <===              212
         CALL AGSRESET                                                    213
         AG_TITLE = 'AIR'                                                 213
         AG_EXNAME = 'MATERIAL'                                           213
         AG_PARLIST = 'NONE'                                              213
         CALL AXMATERIAL                                                  213
C    *                                                                    213
C    Geant SHAPE : TUBE ===> RMIN=COOLTUBEINRT RMAX=DISKOUTR, DZ=FGTLENZ  213
C    /2.0 <===                                                            213
         CALL AGSRESET                                                    215
         AG_RMIN = COOLTUBEINRT                                           215
         AG_RMAX = DISKOUTR                                               215
         AG_DZ = FGTLENZ/2.0                                              215
         AG_TITLE = 'TUBE'                                                215
         AG_EXNAME = 'SHAPE'                                              215
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      215
         CALL AXSHAPE                                                     215
C    *                                                                    215
C    Create FGCT :                                                        216
C    Call To Local Procedure FGCT                                         216
         ASSIGN 5040 TO BLOCK_FGCT                                        216
         GO TO  5030                                                      216
5040     CONTINUE                                                         216
C    *                                                                    216
C    Geant POSITION : FGCT ===> inherited arguments used <===             217
         CALL AGSRESET                                                    217
         AG_TITLE = 'FGCT'                                                217
         AG_EXNAME = 'POSITION'                                           217
         AG_PARLIST = 'NONE'                                              217
         CALL AXPOSITION                                                  217
C    *                                                                    217
C    Create FGCN :                                                        218
C    Call To Local Procedure FGCN                                         218
         ASSIGN 5060 TO BLOCK_FGCN                                        218
         GO TO  5050                                                      218
5060     CONTINUE                                                         218
C    *                                                                    218
C    Geant POSITION : FGCN ===> Z=-FGTLENZ/2.0+0.64/2. <===               219
         CALL AGSRESET                                                    219
         AG_Z = -FGTLENZ/2.0+0.64/2.                                      219
         AG_TITLE = 'FGCN'                                                219
         AG_EXNAME = 'POSITION'                                           219
         AG_PARLIST = 'Z'                                                 219
         CALL AXPOSITION                                                  219
C    *                                                                    219
C    *                                                                    219
         DISK=1                                                           220
            GO TO 5073                                                    220
5071        DISK=DISK+1                                                   220
5073        IF(DISK-(FGST_NDISK).GT.0) GO TO 5072                         220
            DISKZ=-FGTLENZ/2.+1.0 +DISKLENZ/2. + FGST_ZDISCA(DISK)-       221
     *      FGST_ZDISCA(1)                                                221
C       Check (FGST_CONFIG.EQ.1.0).AND.(DISK.LE.6)                        222
            IF ((FGST_CONFIG.EQ.1.0).AND.(DISK.LE.6)) THEN                222
C       Create FGTD :                                                     223
C       Call To Local Procedure FGTD                                      223
            ASSIGN 5090 TO BLOCK_FGTD                                     223
            GO TO  5080                                                   223
5090        CONTINUE                                                      223
C       *                                                                 223
C       Geant POSITION : FGTD ===> Z=DISKZ ALPHAZ=QUADTILTANG <===        224
            CALL AGSRESET                                                 224
            AG_Z = DISKZ                                                  224
            AG_ALPHAN=AG_ALPHAN+1                                         224
            AG_ALPHAK(AG_ALPHAN)=3                                        224
            AG_ALPHAL(AG_ALPHAN)=QUADTILTANG                              224
            AG_TITLE = 'FGTD'                                             224
            AG_EXNAME = 'POSITION'                                        224
            AG_PARLIST = 'Z_ALPHAZ'                                       224
            CALL AXPOSITION                                               224
C       *                                                                 224
            ELSE                                                          224
C       Create FGDB :                                                     226
C       Call To Local Procedure FGDB                                      226
            ASSIGN 5110 TO BLOCK_FGDB                                     226
            GO TO  5100                                                   226
5110        CONTINUE                                                      226
C       *                                                                 226
C       Geant POSITION : FGDB ===> Z=DISKZ ALPHAZ=QUADTILTANG <===        227
            CALL AGSRESET                                                 227
            AG_Z = DISKZ                                                  227
            AG_ALPHAN=AG_ALPHAN+1                                         227
            AG_ALPHAK(AG_ALPHAN)=3                                        227
            AG_ALPHAL(AG_ALPHAN)=QUADTILTANG                              227
            AG_TITLE = 'FGDB'                                             227
            AG_EXNAME = 'POSITION'                                        227
            AG_PARLIST = 'Z_ALPHAZ'                                       227
            CALL AXPOSITION                                               227
C       *                                                                 227
            ENDIF                                                         227
         GO TO 5071                                                       229
5072     CONTINUE                                                         229
C    *                                                                    230
         DISK=2                                                           230
            GO TO 5123                                                    230
5121        DISK=DISK+1                                                   230
5123        IF(DISK-(FGST_NDISK).GT.0) GO TO 5122                         230
            DISKZ=-FGTLENZ/2. + 1.0 + FGST_ZDISCA(DISK)- FGST_ZDISCA(1)   231
     *      -1.28/2.                                                      231
C       Create FGCM :                                                     232
C       Call To Local Procedure FGCM                                      232
            ASSIGN 5140 TO BLOCK_FGCM                                     232
            GO TO  5130                                                   232
5140        CONTINUE                                                      232
C       *                                                                 232
C       Geant POSITION : FGCM ===> Z=DISKZ <===                           233
            CALL AGSRESET                                                 233
            AG_Z = DISKZ                                                  233
            AG_TITLE = 'FGCM'                                             233
            AG_EXNAME = 'POSITION'                                        233
            AG_PARLIST = 'Z'                                              233
            CALL AXPOSITION                                               233
C       *                                                                 233
         GO TO 5121                                                       234
5122     CONTINUE                                                         234
5011  CALL AGSPOPB                                                        235
      GO TO BLOCK_FGTM, (5010 ,5020)                                      235
C End of Local Procedure                                                  235
C ----------------------------------------------------------------------  236
C ------------ FGCT                                                       236
C Begin Local Procedure FGCT                                              237
C FGCT INNER COOLING TUBE                                                 237
5030  CONTINUE                                                            237
         AG_TITLE='FGCT'                                                  237
         AG_MODULE=MODULE                                                 237
         AG_MARK='V00'                                                    237
         CALL AGSPUSH                                                     237
         AG_MODULE=MODULE                                                 237
C    *                                                                    237
C    Geant ATTRIBUTE : FGCT ===> SEEN=1 COLO=6 <===                       238
         CALL AGSRESET                                                    238
         AG_SEEN = 1                                                      238
         AG_COLO = 6                                                      238
         AG_TITLE = 'FGCT'                                                238
         AG_EXNAME = 'ATTRIBUTE'                                          238
         AG_PARLIST = 'SEEN_COLO'                                         238
         CALL AXATTRIBUTE                                                 238
C    *                                                                    238
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          238
         CALL AGSRESET                                                    239
         AG_TITLE = 'CFRPMIX'                                             239
         AG_EXNAME = 'MATERIAL'                                           239
         AG_PARLIST = 'NONE'                                              239
         CALL AXMATERIAL                                                  239
C    *                                                                    239
C    Geant SHAPE : TUBE ===> RMIN=COOLTUBEINR RMAX=COOLTUBEINR+0.05, DZ=  239
C    COOLTUBEDZ/2. <===                                                   239
         CALL AGSRESET                                                    241
         AG_RMIN = COOLTUBEINR                                            241
         AG_RMAX = COOLTUBEINR+0.05                                       241
         AG_DZ = COOLTUBEDZ/2.                                            241
         AG_TITLE = 'TUBE'                                                241
         AG_EXNAME = 'SHAPE'                                              241
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      241
         CALL AXSHAPE                                                     241
C    *                                                                    241
5031  CALL AGSPOPB                                                        242
      GO TO BLOCK_FGCT, (5030 ,5040)                                      242
C End of Local Procedure                                                  242
C ----------------------------------------------------------------------  243
C ------------ FGCN                                                       243
C Begin Local Procedure FGCN                                              244
C FGCN NYLON 1ST RING                                                     244
5050  CONTINUE                                                            244
         AG_TITLE='FGCN'                                                  244
         AG_MODULE=MODULE                                                 244
         AG_MARK='V00'                                                    244
         CALL AGSPUSH                                                     244
         AG_MODULE=MODULE                                                 244
C    *                                                                    244
C    Geant ATTRIBUTE : FGCN ===> SEEN=1 COLO=6 <===                       245
         CALL AGSRESET                                                    245
         AG_SEEN = 1                                                      245
         AG_COLO = 6                                                      245
         AG_TITLE = 'FGCN'                                                245
         AG_EXNAME = 'ATTRIBUTE'                                          245
         AG_PARLIST = 'SEEN_COLO'                                         245
         CALL AXATTRIBUTE                                                 245
C    *                                                                    245
C    Geant MATERIAL : NYLON ===> inherited arguments used <===            245
         CALL AGSRESET                                                    246
         AG_TITLE = 'NYLON'                                               246
         AG_EXNAME = 'MATERIAL'                                           246
         AG_PARLIST = 'NONE'                                              246
         CALL AXMATERIAL                                                  246
C    *                                                                    246
C    Geant SHAPE : TUBE ===> RMIN=10.35 RMAX=11.5 DZ=0.64/2. <===         246
         CALL AGSRESET                                                    247
         AG_RMIN = 10.35                                                  247
         AG_RMAX = 11.5                                                   247
         AG_DZ = 0.64/2.                                                  247
         AG_TITLE = 'TUBE'                                                247
         AG_EXNAME = 'SHAPE'                                              247
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      247
         CALL AXSHAPE                                                     247
C    *                                                                    247
5051  CALL AGSPOPB                                                        248
      GO TO BLOCK_FGCN, (5050 ,5060)                                      248
C End of Local Procedure                                                  248
C ----------------------------------------------------------------------  249
C ------------ FGCM                                                       249
C Begin Local Procedure FGCM                                              250
C FGCM NYLON AND AL RING                                                  250
5130  CONTINUE                                                            250
         AG_TITLE='FGCM'                                                  250
         AG_MODULE=MODULE                                                 250
         AG_MARK='V00'                                                    250
         CALL AGSPUSH                                                     250
         AG_MODULE=MODULE                                                 250
C    *                                                                    250
C    Geant ATTRIBUTE : FGCM ===> SEEN=1 COLO=6 <===                       251
         CALL AGSRESET                                                    251
         AG_SEEN = 1                                                      251
         AG_COLO = 6                                                      251
         AG_TITLE = 'FGCM'                                                251
         AG_EXNAME = 'ATTRIBUTE'                                          251
         AG_PARLIST = 'SEEN_COLO'                                         251
         CALL AXATTRIBUTE                                                 251
C    *                                                                    251
C    Geant MATERIAL : COOLMIX ===> inherited arguments used <===          251
         CALL AGSRESET                                                    252
         AG_TITLE = 'COOLMIX'                                             252
         AG_EXNAME = 'MATERIAL'                                           252
         AG_PARLIST = 'NONE'                                              252
         CALL AXMATERIAL                                                  252
C    *                                                                    252
C    Geant SHAPE : TUBE ===> RMIN=10.35 RMAX=11.54 DZ=1.28/2. <===        252
         CALL AGSRESET                                                    253
         AG_RMIN = 10.35                                                  253
         AG_RMAX = 11.54                                                  253
         AG_DZ = 1.28/2.                                                  253
         AG_TITLE = 'TUBE'                                                253
         AG_EXNAME = 'SHAPE'                                              253
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      253
         CALL AXSHAPE                                                     253
C    *                                                                    253
5131  CALL AGSPOPB                                                        254
      GO TO BLOCK_FGCM, (5130 ,5140)                                      254
C End of Local Procedure                                                  254
C ----------------------------------------------------------------------  255
C ------------ FGTD                                                       255
C Begin Local Procedure FGTD                                              256
C FGTD MOTHER VOLUME FOR FGT DISK                                         256
5080  CONTINUE                                                            256
         AG_TITLE='FGTD'                                                  256
         AG_MODULE=MODULE                                                 256
         AG_MARK='V00'                                                    256
         CALL AGSPUSH                                                     256
         AG_MODULE=MODULE                                                 256
C    *                                                                    256
C    Geant ATTRIBUTE : FGTD ===> SEEN=1 COLO=5 <===                       257
         CALL AGSRESET                                                    257
         AG_SEEN = 1                                                      257
         AG_COLO = 5                                                      257
         AG_TITLE = 'FGTD'                                                257
         AG_EXNAME = 'ATTRIBUTE'                                          257
         AG_PARLIST = 'SEEN_COLO'                                         257
         CALL AXATTRIBUTE                                                 257
C    *                                                                    257
C    Geant MATERIAL : AIR ===> inherited arguments used <===              257
         CALL AGSRESET                                                    258
         AG_TITLE = 'AIR'                                                 258
         AG_EXNAME = 'MATERIAL'                                           258
         AG_PARLIST = 'NONE'                                              258
         CALL AXMATERIAL                                                  258
C    *                                                                    258
C    Geant SHAPE : TUBE ===> RMIN=DISKINR RMAX=DISKOUTR DZ=DISKLENZ/2.0   258
C    <===                                                                 258
         CALL AGSRESET                                                    259
         AG_RMIN = DISKINR                                                259
         AG_RMAX = DISKOUTR                                               259
         AG_DZ = DISKLENZ/2.0                                             259
         AG_TITLE = 'TUBE'                                                259
         AG_EXNAME = 'SHAPE'                                              259
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      259
         CALL AXSHAPE                                                     259
C    *                                                                    259
C    *                                                                    259
         QUAD=1                                                           260
            GO TO 5153                                                    260
5151        QUAD=QUAD+1                                                   260
5153        IF(QUAD-(FGST_NQUAD).GT.0) GO TO 5152                         260
C       Create FGTQ :                                                     261
C       Call To Local Procedure FGTQ                                      261
            ASSIGN 5170 TO BLOCK_FGTQ                                     261
            GO TO  5160                                                   261
5170        CONTINUE                                                      261
C       *                                                                 261
C       Geant POSITION : FGTQ ===> Z=0. ALPHAZ=(1-QUAD)*90. <===          262
            CALL AGSRESET                                                 262
            AG_Z = 0.                                                     262
            AG_ALPHAN=AG_ALPHAN+1                                         262
            AG_ALPHAK(AG_ALPHAN)=3                                        262
            AG_ALPHAL(AG_ALPHAN)=(1-QUAD)*90.                             262
            AG_TITLE = 'FGTQ'                                             262
            AG_EXNAME = 'POSITION'                                        262
            AG_PARLIST = 'Z_ALPHAZ'                                       262
            CALL AXPOSITION                                               262
C       *                                                                 262
         GO TO 5151                                                       263
5152     CONTINUE                                                         263
5081  CALL AGSPOPB                                                        264
      GO TO BLOCK_FGTD, (5080 ,5090)                                      264
C End of Local Procedure                                                  264
C ----------------------------------------------------------------------  265
C ------------ FGDB                                                       265
C Begin Local Procedure FGDB                                              266
C FGDB MOTHER VOLUME FOR FGT DISK                                         266
5100  CONTINUE                                                            266
         AG_TITLE='FGDB'                                                  266
         AG_MODULE=MODULE                                                 266
         AG_MARK='V00'                                                    266
         CALL AGSPUSH                                                     266
         AG_MODULE=MODULE                                                 266
C    *                                                                    266
C    Geant ATTRIBUTE : FGDB ===> SEEN=1 COLO=5 <===                       267
         CALL AGSRESET                                                    267
         AG_SEEN = 1                                                      267
         AG_COLO = 5                                                      267
         AG_TITLE = 'FGDB'                                                267
         AG_EXNAME = 'ATTRIBUTE'                                          267
         AG_PARLIST = 'SEEN_COLO'                                         267
         CALL AXATTRIBUTE                                                 267
C    *                                                                    267
C    Geant MATERIAL : AIR ===> inherited arguments used <===              267
         CALL AGSRESET                                                    268
         AG_TITLE = 'AIR'                                                 268
         AG_EXNAME = 'MATERIAL'                                           268
         AG_PARLIST = 'NONE'                                              268
         CALL AXMATERIAL                                                  268
C    *                                                                    268
C    Geant SHAPE : TUBE ===> RMIN=DISKINRT RMAX=DISKOUTR DZ=DISKLENZ/2.0  268
C     <===                                                                268
         CALL AGSRESET                                                    269
         AG_RMIN = DISKINRT                                               269
         AG_RMAX = DISKOUTR                                               269
         AG_DZ = DISKLENZ/2.0                                             269
         AG_TITLE = 'TUBE'                                                269
         AG_EXNAME = 'SHAPE'                                              269
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      269
         CALL AXSHAPE                                                     269
C    *                                                                    269
C    *                                                                    269
         QUAD=1                                                           270
            GO TO 5183                                                    270
5181        QUAD=QUAD+1                                                   270
5183        IF(QUAD-(FGST_NQUAD).GT.0) GO TO 5182                         270
C       Create FTQB :                                                     271
C       Call To Local Procedure FTQB                                      271
            ASSIGN 5200 TO BLOCK_FTQB                                     271
            GO TO  5190                                                   271
5200        CONTINUE                                                      271
C       *                                                                 271
C       Geant POSITION : FTQB ===> Z=0. ALPHAZ=(1-QUAD)*90. <===          272
            CALL AGSRESET                                                 272
            AG_Z = 0.                                                     272
            AG_ALPHAN=AG_ALPHAN+1                                         272
            AG_ALPHAK(AG_ALPHAN)=3                                        272
            AG_ALPHAL(AG_ALPHAN)=(1-QUAD)*90.                             272
            AG_TITLE = 'FTQB'                                             272
            AG_EXNAME = 'POSITION'                                        272
            AG_PARLIST = 'Z_ALPHAZ'                                       272
            CALL AXPOSITION                                               272
C       *                                                                 272
         GO TO 5181                                                       273
5182     CONTINUE                                                         273
5101  CALL AGSPOPB                                                        274
      GO TO BLOCK_FGDB, (5100 ,5110)                                      274
C End of Local Procedure                                                  274
C ----------------------------------------------------------------------  275
C ------------ FGTQ                                                       275
C Begin Local Procedure FGTQ                                              276
C FGTQ QUADRANT                                                           276
5160  CONTINUE                                                            276
         AG_TITLE='FGTQ'                                                  276
         AG_MODULE=MODULE                                                 276
         AG_MARK='V00'                                                    276
         CALL AGSPUSH                                                     276
         AG_MODULE=MODULE                                                 276
C    *                                                                    276
C    Geant ATTRIBUTE : FGTQ ===> SEEN=0 COLO=4 <===                       277
         CALL AGSRESET                                                    277
         AG_SEEN = 0                                                      277
         AG_COLO = 4                                                      277
         AG_TITLE = 'FGTQ'                                                277
         AG_EXNAME = 'ATTRIBUTE'                                          277
         AG_PARLIST = 'SEEN_COLO'                                         277
         CALL AXATTRIBUTE                                                 277
C    *                                                                    277
C    Geant MATERIAL : AIR ===> inherited arguments used <===              277
         CALL AGSRESET                                                    278
         AG_TITLE = 'AIR'                                                 278
         AG_EXNAME = 'MATERIAL'                                           278
         AG_PARLIST = 'NONE'                                              278
         CALL AXMATERIAL                                                  278
C    *                                                                    278
C    Geant SHAPE : TUBS ===> RMIN=DISKINR RMAX=DISKOUTR PHI1=0., PHI2=90  278
C    . DZ=DISKLENZ/2. <===                                                278
         CALL AGSRESET                                                    280
         AG_RMIN = DISKINR                                                280
         AG_RMAX = DISKOUTR                                               280
         AG_PHI1 = 0.                                                     280
         AG_PHI2 = 90.                                                    280
         AG_DZ = DISKLENZ/2.                                              280
         AG_TITLE = 'TUBS'                                                280
         AG_EXNAME = 'SHAPE'                                              280
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            280
         CALL AXSHAPE                                                     280
C    *                                                                    280
C    Create FGVN :                                                        281
C    Call To Local Procedure FGVN                                         281
         ASSIGN 5220 TO BLOCK_FGVN                                        281
         GO TO  5210                                                      281
5220     CONTINUE                                                         281
C    *                                                                    281
C    Geant POSITION : FGVN ===> Z=-DISKLENZ/2.+VOLNOMEXDZ/2. <===         282
         CALL AGSRESET                                                    282
         AG_Z = -DISKLENZ/2.+VOLNOMEXDZ/2.                                282
         AG_TITLE = 'FGVN'                                                282
         AG_EXNAME = 'POSITION'                                           282
         AG_PARLIST = 'Z'                                                 282
         CALL AXPOSITION                                                  282
C    *                                                                    282
C    Create FGZC :                                                        283
C    Call To Local Procedure FGZC                                         283
         ASSIGN 5240 TO BLOCK_FGZC                                        283
         GO TO  5230                                                      283
5240     CONTINUE                                                         283
C    *                                                                    283
C    Geant POSITION : FGZC ===> Z=-DISKLENZ/2.+VOLNOMEXDZ+VOLSENSDZ/2. <  284
C    ===                                                                  284
         CALL AGSRESET                                                    284
         AG_Z = -DISKLENZ/2.+VOLNOMEXDZ+VOLSENSDZ/2.                      284
         AG_TITLE = 'FGZC'                                                284
         AG_EXNAME = 'POSITION'                                           284
         AG_PARLIST = 'Z'                                                 284
         CALL AXPOSITION                                                  284
C    *                                                                    284
C    Create FGVG :                                                        285
C    Call To Local Procedure FGVG                                         285
         ASSIGN 5260 TO BLOCK_FGVG                                        285
         GO TO  5250                                                      285
5260     CONTINUE                                                         285
C    *                                                                    285
C    Geant POSITION : FGVG ===> Z=-DISKLENZ/2.+VOLNOMEXDZ+VOLSENSDZ+VOLG  286
C    EMDZ/2. <===                                                         286
         CALL AGSRESET                                                    286
         AG_Z = -DISKLENZ/2.+VOLNOMEXDZ+VOLSENSDZ+VOLGEMDZ/2.             286
         AG_TITLE = 'FGVG'                                                286
         AG_EXNAME = 'POSITION'                                           286
         AG_PARLIST = 'Z'                                                 286
         CALL AXPOSITION                                                  286
C    *                                                                    286
C    Create FGVR :                                                        287
C    Call To Local Procedure FGVR                                         287
         ASSIGN 5280 TO BLOCK_FGVR                                        287
         GO TO  5270                                                      287
5280     CONTINUE                                                         287
C    *                                                                    287
C    Geant POSITION : FGVR ===> Z=-DISKLENZ/2.+VOLNOMEXDZ+VOLSENSDZ+VOLG  288
C    EMDZ+VOLREADDZ/2. <===                                               288
         CALL AGSRESET                                                    288
         AG_Z = -DISKLENZ/2.+VOLNOMEXDZ+VOLSENSDZ+VOLGEMDZ+VOLREADDZ/2.   288
         AG_TITLE = 'FGVR'                                                288
         AG_EXNAME = 'POSITION'                                           288
         AG_PARLIST = 'Z'                                                 288
         CALL AXPOSITION                                                  288
C    *                                                                    288
C    Create FGVE :                                                        289
C    Call To Local Procedure FGVE                                         289
         ASSIGN 5300 TO BLOCK_FGVE                                        289
         GO TO  5290                                                      289
5300     CONTINUE                                                         289
C    *                                                                    289
C    Geant POSITION : FGVE ===> Z=-DISKLENZ/2.+VOLNOMEXDZ+VOLSENSDZ+VOLG  291
C    EMDZ+VOLREADDZ+VOLELECDZ/2. <===                                     291
         CALL AGSRESET                                                    291
         AG_Z = -DISKLENZ/                                                291
     *   2.+VOLNOMEXDZ+VOLSENSDZ+VOLGEMDZ+VOLREADDZ+VOLELECDZ/2.          291
         AG_TITLE = 'FGVE'                                                291
         AG_EXNAME = 'POSITION'                                           291
         AG_PARLIST = 'Z'                                                 291
         CALL AXPOSITION                                                  291
C    *                                                                    291
5161  CALL AGSPOPB                                                        292
      GO TO BLOCK_FGTQ, (5160 ,5170)                                      292
C End of Local Procedure                                                  292
C ----------------------------------------------------------------------  293
C ------------ FTQB                                                       293
C Begin Local Procedure FTQB                                              294
C FTQB QUADRANT                                                           294
5190  CONTINUE                                                            294
         AG_TITLE='FTQB'                                                  294
         AG_MODULE=MODULE                                                 294
         AG_MARK='V00'                                                    294
         CALL AGSPUSH                                                     294
         AG_MODULE=MODULE                                                 294
C    *                                                                    294
C    Geant ATTRIBUTE : FTQB ===> SEEN=0 COLO=4 <===                       295
         CALL AGSRESET                                                    295
         AG_SEEN = 0                                                      295
         AG_COLO = 4                                                      295
         AG_TITLE = 'FTQB'                                                295
         AG_EXNAME = 'ATTRIBUTE'                                          295
         AG_PARLIST = 'SEEN_COLO'                                         295
         CALL AXATTRIBUTE                                                 295
C    *                                                                    295
C    Geant MATERIAL : AIR ===> inherited arguments used <===              295
         CALL AGSRESET                                                    296
         AG_TITLE = 'AIR'                                                 296
         AG_EXNAME = 'MATERIAL'                                           296
         AG_PARLIST = 'NONE'                                              296
         CALL AXMATERIAL                                                  296
C    *                                                                    296
C    Geant SHAPE : TUBS ===> RMIN=DISKINRT RMAX=DISKOUTR PHI1=0., PHI2=9  296
C    0. DZ=DISKLENZ/2. <===                                               296
         CALL AGSRESET                                                    298
         AG_RMIN = DISKINRT                                               298
         AG_RMAX = DISKOUTR                                               298
         AG_PHI1 = 0.                                                     298
         AG_PHI2 = 90.                                                    298
         AG_DZ = DISKLENZ/2.                                              298
         AG_TITLE = 'TUBS'                                                298
         AG_EXNAME = 'SHAPE'                                              298
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            298
         CALL AXSHAPE                                                     298
C    *                                                                    298
C    Create FVNB :                                                        299
C    Call To Local Procedure FVNB                                         299
         ASSIGN 5320 TO BLOCK_FVNB                                        299
         GO TO  5310                                                      299
5320     CONTINUE                                                         299
C    *                                                                    299
C    Geant POSITION : FVNB ===> Z=-DISKLENZ/2.+VOLNOMEXDZ/2. <===         300
         CALL AGSRESET                                                    300
         AG_Z = -DISKLENZ/2.+VOLNOMEXDZ/2.                                300
         AG_TITLE = 'FVNB'                                                300
         AG_EXNAME = 'POSITION'                                           300
         AG_PARLIST = 'Z'                                                 300
         CALL AXPOSITION                                                  300
C    *                                                                    300
C    Create FZCB :                                                        301
C    Call To Local Procedure FZCB                                         301
         ASSIGN 5340 TO BLOCK_FZCB                                        301
         GO TO  5330                                                      301
5340     CONTINUE                                                         301
C    *                                                                    301
C    Geant POSITION : FZCB ===> Z=-DISKLENZ/2.+VOLNOMEXDZ+VOLSENSDZ/2. <  302
C    ===                                                                  302
         CALL AGSRESET                                                    302
         AG_Z = -DISKLENZ/2.+VOLNOMEXDZ+VOLSENSDZ/2.                      302
         AG_TITLE = 'FZCB'                                                302
         AG_EXNAME = 'POSITION'                                           302
         AG_PARLIST = 'Z'                                                 302
         CALL AXPOSITION                                                  302
C    *                                                                    302
C    Create FVGB :                                                        303
C    Call To Local Procedure FVGB                                         303
         ASSIGN 5360 TO BLOCK_FVGB                                        303
         GO TO  5350                                                      303
5360     CONTINUE                                                         303
C    *                                                                    303
C    Geant POSITION : FVGB ===> Z=-DISKLENZ/2.+VOLNOMEXDZ+VOLSENSDZ+VOLG  304
C    EMDZ/2. <===                                                         304
         CALL AGSRESET                                                    304
         AG_Z = -DISKLENZ/2.+VOLNOMEXDZ+VOLSENSDZ+VOLGEMDZ/2.             304
         AG_TITLE = 'FVGB'                                                304
         AG_EXNAME = 'POSITION'                                           304
         AG_PARLIST = 'Z'                                                 304
         CALL AXPOSITION                                                  304
C    *                                                                    304
C    Create FVRB :                                                        305
C    Call To Local Procedure FVRB                                         305
         ASSIGN 5380 TO BLOCK_FVRB                                        305
         GO TO  5370                                                      305
5380     CONTINUE                                                         305
C    *                                                                    305
C    Geant POSITION : FVRB ===> Z=-DISKLENZ/2.+VOLNOMEXDZ+VOLSENSDZ+VOLG  306
C    EMDZ+VOLREADDZ/2. <===                                               306
         CALL AGSRESET                                                    306
         AG_Z = -DISKLENZ/2.+VOLNOMEXDZ+VOLSENSDZ+VOLGEMDZ+VOLREADDZ/2.   306
         AG_TITLE = 'FVRB'                                                306
         AG_EXNAME = 'POSITION'                                           306
         AG_PARLIST = 'Z'                                                 306
         CALL AXPOSITION                                                  306
C    *                                                                    306
C    Create FVEB :                                                        307
C    Call To Local Procedure FVEB                                         307
         ASSIGN 5400 TO BLOCK_FVEB                                        307
         GO TO  5390                                                      307
5400     CONTINUE                                                         307
C    *                                                                    307
C    Geant POSITION : FVEB ===> Z=-DISKLENZ/2.+VOLNOMEXDZ+VOLSENSDZ+VOLG  309
C    EMDZ+VOLREADDZ+VOLELECDZ/2. <===                                     309
         CALL AGSRESET                                                    309
         AG_Z = -DISKLENZ/                                                309
     *   2.+VOLNOMEXDZ+VOLSENSDZ+VOLGEMDZ+VOLREADDZ+VOLELECDZ/2.          309
         AG_TITLE = 'FVEB'                                                309
         AG_EXNAME = 'POSITION'                                           309
         AG_PARLIST = 'Z'                                                 309
         CALL AXPOSITION                                                  309
C    *                                                                    309
5191  CALL AGSPOPB                                                        310
      GO TO BLOCK_FTQB, (5190 ,5200)                                      310
C End of Local Procedure                                                  310
C ----------------------------------------------------------------------  311
C ------------ FGVN                                                       311
C Begin Local Procedure FGVN                                              312
C FGVN QUAD NOMEX MIXTURE VOLUME                                          312
5210  CONTINUE                                                            312
         AG_TITLE='FGVN'                                                  312
         AG_MODULE=MODULE                                                 312
         AG_MARK='V00'                                                    312
         CALL AGSPUSH                                                     312
         AG_MODULE=MODULE                                                 312
C    *                                                                    312
C    Geant ATTRIBUTE : FGVN ===> SEEN=0 COLO=5 <===                       313
         CALL AGSRESET                                                    313
         AG_SEEN = 0                                                      313
         AG_COLO = 5                                                      313
         AG_TITLE = 'FGVN'                                                313
         AG_EXNAME = 'ATTRIBUTE'                                          313
         AG_PARLIST = 'SEEN_COLO'                                         313
         CALL AXATTRIBUTE                                                 313
C    *                                                                    313
C    Geant MATERIAL : NOMEXMIX ===> inherited arguments used <===         313
         CALL AGSRESET                                                    314
         AG_TITLE = 'NOMEXMIX'                                            314
         AG_EXNAME = 'MATERIAL'                                           314
         AG_PARLIST = 'NONE'                                              314
         CALL AXMATERIAL                                                  314
C    *                                                                    314
C    Geant SHAPE : TUBS ===> RMIN=DISKINR RMAX=DISKOUTR PHI1=0., PHI2=90  314
C    . DZ=VOLNOMEXDZ/2. <===                                              314
         CALL AGSRESET                                                    316
         AG_RMIN = DISKINR                                                316
         AG_RMAX = DISKOUTR                                               316
         AG_PHI1 = 0.                                                     316
         AG_PHI2 = 90.                                                    316
         AG_DZ = VOLNOMEXDZ/2.                                            316
         AG_TITLE = 'TUBS'                                                316
         AG_EXNAME = 'SHAPE'                                              316
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            316
         CALL AXSHAPE                                                     316
C    *                                                                    316
C    *                                                                    316
         K=0                                                              317
            GO TO 5413                                                    317
5411        K=K+1                                                         317
5413        IF(K-(2).GT.0) GO TO 5412                                     317
            ZZFGZ=(-VOLNOMEXDZ+FRAMEDZ)/2.                                318
C       Check K.EQ.1                                                      319
            IF (K.EQ.1) THEN                                              319
            ZZFGZ=(VOLNOMEXDZ-FRAMEDZ)/2.-0.007                           320
            ENDIF                                                         321
C       Check K.EQ.2                                                      322
            IF (K.EQ.2) THEN                                              322
            ZZFGZ=(VOLNOMEXDZ-FRAMEDZ)/2.-0.014 - FRAMEDZ                 323
            ENDIF                                                         324
C       Create FGQA :                                                     325
C       Call To Local Procedure FGQA                                      325
            ASSIGN 5430 TO BLOCK_FGQA                                     325
            GO TO  5420                                                   325
5430        CONTINUE                                                      325
C       *                                                                 325
C       Geant POSITION : FGQA ===> X=FRAMEWIDTH/2.+FRAMEXYOFF Y=(DISKOUT  326
C       R+DISKINR)/2. Z=ZZFGZ <===                                        326
            CALL AGSRESET                                                 326
            AG_X = FRAMEWIDTH/2.+FRAMEXYOFF                               326
            AG_Y = (DISKOUTR+DISKINR)/2.                                  326
            AG_Z = ZZFGZ                                                  326
            AG_TITLE = 'FGQA'                                             326
            AG_EXNAME = 'POSITION'                                        326
            AG_PARLIST = 'X_Y_Z'                                          326
            CALL AXPOSITION                                               326
C       *                                                                 326
C       Create FGQB :                                                     327
C       Call To Local Procedure FGQB                                      327
            ASSIGN 5450 TO BLOCK_FGQB                                     327
            GO TO  5440                                                   327
5450        CONTINUE                                                      327
C       *                                                                 327
C       Geant POSITION : FGQB ===> Z=ZZFGZ <===                           328
            CALL AGSRESET                                                 328
            AG_Z = ZZFGZ                                                  328
            AG_TITLE = 'FGQB'                                             328
            AG_EXNAME = 'POSITION'                                        328
            AG_PARLIST = 'Z'                                              328
            CALL AXPOSITION                                               328
C       *                                                                 328
C       Create FGQC :                                                     329
C       Call To Local Procedure FGQC                                      329
            ASSIGN 5470 TO BLOCK_FGQC                                     329
            GO TO  5460                                                   329
5470        CONTINUE                                                      329
C       *                                                                 329
C       Geant POSITION : FGQC ===> X=(DISKOUTR+DISKINR)/2. Y=FRAMEWIDTH/  330
C       2.+FRAMEXYOFF Z=ZZFGZ <===                                        330
            CALL AGSRESET                                                 330
            AG_X = (DISKOUTR+DISKINR)/2.                                  330
            AG_Y = FRAMEWIDTH/2.+FRAMEXYOFF                               330
            AG_Z = ZZFGZ                                                  330
            AG_TITLE = 'FGQC'                                             330
            AG_EXNAME = 'POSITION'                                        330
            AG_PARLIST = 'X_Y_Z'                                          330
            CALL AXPOSITION                                               330
C       *                                                                 330
C       Create FGQD :                                                     331
C       Call To Local Procedure FGQD                                      331
            ASSIGN 5490 TO BLOCK_FGQD                                     331
            GO TO  5480                                                   331
5490        CONTINUE                                                      331
C       *                                                                 331
C       Geant POSITION : FGQD ===> Z=ZZFGZ <===                           332
            CALL AGSRESET                                                 332
            AG_Z = ZZFGZ                                                  332
            AG_TITLE = 'FGQD'                                             332
            AG_EXNAME = 'POSITION'                                        332
            AG_PARLIST = 'Z'                                              332
            CALL AXPOSITION                                               332
C       *                                                                 332
C       Create FGQE :                                                     333
C       Call To Local Procedure FGQE                                      333
            ASSIGN 5510 TO BLOCK_FGQE                                     333
            GO TO  5500                                                   333
5510        CONTINUE                                                      333
C       *                                                                 333
C       Geant POSITION : FGQE ===> X=(FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPE  336
C       RPANG*DEGRAD) Y=(FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD)  336
C        Z=ZZFGZ ALPHAZ=FLATPERPANG-90. <===                              336
            CALL AGSRESET                                                 336
            AG_X = (FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPERPANG*DEGRAD)       336
            AG_Y = (FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD)       336
            AG_Z = ZZFGZ                                                  336
            AG_ALPHAN=AG_ALPHAN+1                                         336
            AG_ALPHAK(AG_ALPHAN)=3                                        336
            AG_ALPHAL(AG_ALPHAN)=FLATPERPANG-90.                          336
            AG_TITLE = 'FGQE'                                             336
            AG_EXNAME = 'POSITION'                                        336
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   336
            CALL AXPOSITION                                               336
C       *                                                                 336
C       Create FGQF :                                                     337
C       Call To Local Procedure FGQF                                      337
            ASSIGN 5530 TO BLOCK_FGQF                                     337
            GO TO  5520                                                   337
5530        CONTINUE                                                      337
C       *                                                                 337
C       Geant POSITION : FGQF ===> Z=ZZFGZ <===                           338
            CALL AGSRESET                                                 338
            AG_Z = ZZFGZ                                                  338
            AG_TITLE = 'FGQF'                                             338
            AG_EXNAME = 'POSITION'                                        338
            AG_PARLIST = 'Z'                                              338
            CALL AXPOSITION                                               338
C       *                                                                 338
         GO TO 5411                                                       339
5412     CONTINUE                                                         339
5211  CALL AGSPOPB                                                        340
      GO TO BLOCK_FGVN, (5210 ,5220)                                      340
C End of Local Procedure                                                  340
C ----------------------------------------------------------------------  341
C ------------ FVNB                                                       341
C Begin Local Procedure FVNB                                              342
C FVNB QUAD NOMEX MIXTURE VOLUME                                          342
5310  CONTINUE                                                            342
         AG_TITLE='FVNB'                                                  342
         AG_MODULE=MODULE                                                 342
         AG_MARK='V00'                                                    342
         CALL AGSPUSH                                                     342
         AG_MODULE=MODULE                                                 342
C    *                                                                    342
C    Geant ATTRIBUTE : FVNB ===> SEEN=0 COLO=5 <===                       343
         CALL AGSRESET                                                    343
         AG_SEEN = 0                                                      343
         AG_COLO = 5                                                      343
         AG_TITLE = 'FVNB'                                                343
         AG_EXNAME = 'ATTRIBUTE'                                          343
         AG_PARLIST = 'SEEN_COLO'                                         343
         CALL AXATTRIBUTE                                                 343
C    *                                                                    343
C    Geant MATERIAL : NOMEXMIX ===> inherited arguments used <===         343
         CALL AGSRESET                                                    344
         AG_TITLE = 'NOMEXMIX'                                            344
         AG_EXNAME = 'MATERIAL'                                           344
         AG_PARLIST = 'NONE'                                              344
         CALL AXMATERIAL                                                  344
C    *                                                                    344
C    Geant SHAPE : TUBS ===> RMIN=DISKINRT RMAX=DISKOUTR PHI1=0., PHI2=9  344
C    0. DZ=VOLNOMEXDZ/2. <===                                             344
         CALL AGSRESET                                                    346
         AG_RMIN = DISKINRT                                               346
         AG_RMAX = DISKOUTR                                               346
         AG_PHI1 = 0.                                                     346
         AG_PHI2 = 90.                                                    346
         AG_DZ = VOLNOMEXDZ/2.                                            346
         AG_TITLE = 'TUBS'                                                346
         AG_EXNAME = 'SHAPE'                                              346
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            346
         CALL AXSHAPE                                                     346
C    *                                                                    346
C    *                                                                    346
         K=0                                                              347
            GO TO 5543                                                    347
5541        K=K+1                                                         347
5543        IF(K-(2).GT.0) GO TO 5542                                     347
            ZZFGZ=(-VOLNOMEXDZ+FRAMEDZ)/2.                                348
C       Check K.EQ.1                                                      349
            IF (K.EQ.1) THEN                                              349
            ZZFGZ=(VOLNOMEXDZ-FRAMEDZ)/2.-0.007                           350
            ENDIF                                                         351
C       Check K.EQ.2                                                      352
            IF (K.EQ.2) THEN                                              352
            ZZFGZ=(VOLNOMEXDZ-FRAMEDZ)/2.-0.014 - FRAMEDZ                 353
            ENDIF                                                         354
C       Create FGQA :                                                     355
C       Call To Local Procedure FGQA                                      355
            ASSIGN 5550 TO BLOCK_FGQA                                     355
            GO TO  5420                                                   355
5550        CONTINUE                                                      355
C       *                                                                 355
C       Geant POSITION : FGQA ===> X=FRAMEWIDTH/2.+FRAMEXYOFF Y=(DISKOUT  356
C       R+DISKINRT)/2. Z=ZZFGZ <===                                       356
            CALL AGSRESET                                                 356
            AG_X = FRAMEWIDTH/2.+FRAMEXYOFF                               356
            AG_Y = (DISKOUTR+DISKINRT)/2.                                 356
            AG_Z = ZZFGZ                                                  356
            AG_TITLE = 'FGQA'                                             356
            AG_EXNAME = 'POSITION'                                        356
            AG_PARLIST = 'X_Y_Z'                                          356
            CALL AXPOSITION                                               356
C       *                                                                 356
C       Create FGQB :                                                     357
C       Call To Local Procedure FGQB                                      357
            ASSIGN 5560 TO BLOCK_FGQB                                     357
            GO TO  5440                                                   357
5560        CONTINUE                                                      357
C       *                                                                 357
C       Geant POSITION : FGQB ===> Z=ZZFGZ <===                           358
            CALL AGSRESET                                                 358
            AG_Z = ZZFGZ                                                  358
            AG_TITLE = 'FGQB'                                             358
            AG_EXNAME = 'POSITION'                                        358
            AG_PARLIST = 'Z'                                              358
            CALL AXPOSITION                                               358
C       *                                                                 358
C       Create FGQC :                                                     359
C       Call To Local Procedure FGQC                                      359
            ASSIGN 5570 TO BLOCK_FGQC                                     359
            GO TO  5460                                                   359
5570        CONTINUE                                                      359
C       *                                                                 359
C       Geant POSITION : FGQC ===> X=(DISKOUTR+DISKINRT)/2. Y=FRAMEWIDTH  360
C       /2.+FRAMEXYOFF Z=ZZFGZ <===                                       360
            CALL AGSRESET                                                 360
            AG_X = (DISKOUTR+DISKINRT)/2.                                 360
            AG_Y = FRAMEWIDTH/2.+FRAMEXYOFF                               360
            AG_Z = ZZFGZ                                                  360
            AG_TITLE = 'FGQC'                                             360
            AG_EXNAME = 'POSITION'                                        360
            AG_PARLIST = 'X_Y_Z'                                          360
            CALL AXPOSITION                                               360
C       *                                                                 360
C       Create FGQD :                                                     361
C       Call To Local Procedure FGQD                                      361
            ASSIGN 5580 TO BLOCK_FGQD                                     361
            GO TO  5480                                                   361
5580        CONTINUE                                                      361
C       *                                                                 361
C       Geant POSITION : FGQD ===> Z=ZZFGZ <===                           362
            CALL AGSRESET                                                 362
            AG_Z = ZZFGZ                                                  362
            AG_TITLE = 'FGQD'                                             362
            AG_EXNAME = 'POSITION'                                        362
            AG_PARLIST = 'Z'                                              362
            CALL AXPOSITION                                               362
C       *                                                                 362
C       Create FGQE :                                                     363
C       Call To Local Procedure FGQE                                      363
            ASSIGN 5590 TO BLOCK_FGQE                                     363
            GO TO  5500                                                   363
5590        CONTINUE                                                      363
C       *                                                                 363
C       Geant POSITION : FGQE ===> X=(FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPE  366
C       RPANG*DEGRAD) Y=(FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD)  366
C        Z=ZZFGZ ALPHAZ=FLATPERPANG-90. <===                              366
            CALL AGSRESET                                                 366
            AG_X = (FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPERPANG*DEGRAD)       366
            AG_Y = (FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD)       366
            AG_Z = ZZFGZ                                                  366
            AG_ALPHAN=AG_ALPHAN+1                                         366
            AG_ALPHAK(AG_ALPHAN)=3                                        366
            AG_ALPHAL(AG_ALPHAN)=FLATPERPANG-90.                          366
            AG_TITLE = 'FGQE'                                             366
            AG_EXNAME = 'POSITION'                                        366
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   366
            CALL AXPOSITION                                               366
C       *                                                                 366
C       Create FGQF :                                                     367
C       Call To Local Procedure FGQF                                      367
            ASSIGN 5600 TO BLOCK_FGQF                                     367
            GO TO  5520                                                   367
5600        CONTINUE                                                      367
C       *                                                                 367
C       Geant POSITION : FGQF ===> Z=ZZFGZ <===                           368
            CALL AGSRESET                                                 368
            AG_Z = ZZFGZ                                                  368
            AG_TITLE = 'FGQF'                                             368
            AG_EXNAME = 'POSITION'                                        368
            AG_PARLIST = 'Z'                                              368
            CALL AXPOSITION                                               368
C       *                                                                 368
         GO TO 5541                                                       369
5542     CONTINUE                                                         369
5311  CALL AGSPOPB                                                        370
      GO TO BLOCK_FVNB, (5310 ,5320)                                      370
C End of Local Procedure                                                  370
C ----------------------------------------------------------------------  371
C ------------ FGZC                                                       371
C Begin Local Procedure FGZC                                              372
C FGZC SENSITIVE VOLUME                                                   372
5230  CONTINUE                                                            372
         AG_TITLE='FGZC'                                                  372
         AG_MODULE=MODULE                                                 372
         AG_MARK='V00'                                                    372
         CALL AGSPUSH                                                     372
         AG_MODULE=MODULE                                                 372
C    *                                                                    372
C    Geant ATTRIBUTE : FGZC ===> SEEN=1 COLO=1 <===                       373
         CALL AGSRESET                                                    373
         AG_SEEN = 1                                                      373
         AG_COLO = 1                                                      373
         AG_TITLE = 'FGZC'                                                373
         AG_EXNAME = 'ATTRIBUTE'                                          373
         AG_PARLIST = 'SEEN_COLO'                                         373
         CALL AXATTRIBUTE                                                 373
C    *                                                                    373
C    Geant MATERIAL : ARCO2MIX ===> inherited arguments used <===         373
         CALL AGSRESET                                                    374
         AG_TITLE = 'ARCO2MIX'                                            374
         AG_EXNAME = 'MATERIAL'                                           374
         AG_PARLIST = 'NONE'                                              374
         CALL AXMATERIAL                                                  374
C    *                                                                    374
C    Geant MATERIAL : SENSITIVE_FGT_GAS ===> ISVOL=1 <===                 374
         CALL AGSRESET                                                    375
         AG_ISVOL = 1                                                     375
         AG_TITLE = 'SENSITIVE_FGT_GAS'                                   375
         AG_EXNAME = 'MATERIAL'                                           375
         AG_PARLIST = 'ISVOL'                                             375
         CALL AXMATERIAL                                                  375
C    *                                                                    375
C    Geant SHAPE : TUBS ===> RMIN=DISKINR RMAX=DISKOUTR PHI1=0., PHI2=90  375
C    . DZ=VOLSENSDZ/2. <===                                               375
         CALL AGSRESET                                                    377
         AG_RMIN = DISKINR                                                377
         AG_RMAX = DISKOUTR                                               377
         AG_PHI1 = 0.                                                     377
         AG_PHI2 = 90.                                                    377
         AG_DZ = VOLSENSDZ/2.                                             377
         AG_TITLE = 'TUBS'                                                377
         AG_EXNAME = 'SHAPE'                                              377
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            377
         CALL AXSHAPE                                                     377
C    *                                                                    377
C    Defining Hits: ZZ:0.001: YY:0.001: XX:0.001: PTOT:16:(0,100) CX:10:  377
C     CY:10: CZ:10:, SLENG:16:(0,500) TOF:16:(0,1.0E-6) STEP:0.01: ELOSS  377
C    :16:(0,0.001) PX:16:(-250.0,+250.0), PY:16:(-250.0,+250.0) PZ:16:(-  377
C    250.0,+250.0)                                                        377
         AG_TITLE = 'FGZC'                                                380
         AG_EXNAME = 'HITS'                                               380
         AG_TYPE=AG_MAGIC                                                 380
         CALL AGSDETV('FGZC')                                             380
C    *                                                                    380
         AG_PARLIST='ZZ'                                                  380
         AG_TITLE = 'DETH'                                                380
         AG_NBITS = AG_MAGIC                                              380
         AG_BIN=0.001                                                     380
         AG_FHMIN=AG_MAGIC                                                380
         AG_FHMAX=AG_MAGIC                                                380
         AG_OPTION='*'                                                    380
         CALL AGSDETH (0)                                                 380
C    *                                                                    380
         AG_PARLIST='YY'                                                  380
         AG_TITLE = 'DETH'                                                380
         AG_NBITS = AG_MAGIC                                              380
         AG_BIN=0.001                                                     380
         AG_FHMIN=AG_MAGIC                                                380
         AG_FHMAX=AG_MAGIC                                                380
         AG_OPTION='*'                                                    380
         CALL AGSDETH (0)                                                 380
C    *                                                                    380
         AG_PARLIST='XX'                                                  380
         AG_TITLE = 'DETH'                                                380
         AG_NBITS = AG_MAGIC                                              380
         AG_BIN=0.001                                                     380
         AG_FHMIN=AG_MAGIC                                                380
         AG_FHMAX=AG_MAGIC                                                380
         AG_OPTION='*'                                                    380
         CALL AGSDETH (0)                                                 380
C    *                                                                    380
         AG_PARLIST='PTOT'                                                380
         AG_TITLE = 'DETH'                                                380
         AG_NBITS = 16                                                    380
         AG_BIN=AG_MAGIC                                                  380
         AG_OPTION='*'                                                    380
         AG_FHMIN = 0                                                     380
         AG_FHMAX = 100                                                   380
         CALL AGSDETH (0)                                                 380
C    *                                                                    380
         AG_PARLIST='CX'                                                  380
         AG_TITLE = 'DETH'                                                380
         AG_NBITS = 10                                                    380
         AG_BIN=AG_MAGIC                                                  380
         AG_FHMIN=AG_MAGIC                                                380
         AG_FHMAX=AG_MAGIC                                                380
         AG_OPTION='*'                                                    380
         CALL AGSDETH (0)                                                 380
C    *                                                                    380
         AG_PARLIST='CY'                                                  380
         AG_TITLE = 'DETH'                                                380
         AG_NBITS = 10                                                    380
         AG_BIN=AG_MAGIC                                                  380
         AG_FHMIN=AG_MAGIC                                                380
         AG_FHMAX=AG_MAGIC                                                380
         AG_OPTION='*'                                                    380
         CALL AGSDETH (0)                                                 380
C    *                                                                    380
         AG_PARLIST='CZ'                                                  380
         AG_TITLE = 'DETH'                                                380
         AG_NBITS = 10                                                    380
         AG_BIN=AG_MAGIC                                                  380
         AG_OPTION='*'                                                    380
         AG_FHMIN = AG_MAGIC                                              380
         AG_FHMAX = AG_MAGIC                                              380
         CALL AGSDETH (0)                                                 380
C    *                                                                    380
         AG_PARLIST='SLENG'                                               380
         AG_TITLE = 'DETH'                                                380
         AG_NBITS = 16                                                    380
         AG_BIN=AG_MAGIC                                                  380
         AG_OPTION='*'                                                    380
         AG_FHMIN = 0                                                     380
         AG_FHMAX = 500                                                   380
         CALL AGSDETH (0)                                                 380
C    *                                                                    380
         AG_PARLIST='TOF'                                                 380
         AG_TITLE = 'DETH'                                                380
         AG_NBITS = 16                                                    380
         AG_BIN=AG_MAGIC                                                  380
         AG_OPTION='*'                                                    380
         AG_FHMIN = 0                                                     380
         AG_FHMAX = 1.0E-6                                                380
         CALL AGSDETH (0)                                                 380
C    *                                                                    380
         AG_PARLIST='STEP'                                                380
         AG_TITLE = 'DETH'                                                380
         AG_NBITS = AG_MAGIC                                              380
         AG_BIN=0.01                                                      380
         AG_FHMIN=AG_MAGIC                                                380
         AG_FHMAX=AG_MAGIC                                                380
         AG_OPTION='*'                                                    380
         CALL AGSDETH (0)                                                 380
C    *                                                                    380
         AG_PARLIST='ELOSS'                                               380
         AG_TITLE = 'DETH'                                                380
         AG_NBITS = 16                                                    380
         AG_BIN=AG_MAGIC                                                  380
         AG_OPTION='*'                                                    380
         AG_FHMIN = 0                                                     380
         AG_FHMAX = 0.001                                                 380
         CALL AGSDETH (0)                                                 380
C    *                                                                    380
         AG_PARLIST='PX'                                                  380
         AG_TITLE = 'DETH'                                                380
         AG_NBITS = 16                                                    380
         AG_BIN=AG_MAGIC                                                  380
         AG_OPTION='*'                                                    380
         AG_FHMIN = -250.0                                                380
         AG_FHMAX = +250.0                                                380
         CALL AGSDETH (0)                                                 380
C    *                                                                    380
         AG_PARLIST='PY'                                                  380
         AG_TITLE = 'DETH'                                                380
         AG_NBITS = 16                                                    380
         AG_BIN=AG_MAGIC                                                  380
         AG_OPTION='*'                                                    380
         AG_FHMIN = -250.0                                                380
         AG_FHMAX = +250.0                                                380
         CALL AGSDETH (0)                                                 380
C    *                                                                    380
         AG_PARLIST='PZ'                                                  380
         AG_TITLE = 'DETH'                                                380
         AG_NBITS = 16                                                    380
         AG_BIN=AG_MAGIC                                                  380
         AG_OPTION='*'                                                    380
         AG_FHMIN = -250.0                                                380
         AG_FHMAX = +250.0                                                380
         CALL AGSDETH (0)                                                 380
C    *                                                                    380
         CALL AGSDETU('FGZC')                                             380
C    *                                                                    380
C    *                                                                    381
         K=0                                                              381
            GO TO 5613                                                    381
5611        K=K+1                                                         381
5613        IF(K-(1).GT.0) GO TO 5612                                     381
            ZZFGZ=(-VOLSENSDZ+FRAMEDZ)/2. * (1.-2.*K)                     382
C       Create FGQA :                                                     383
C       Call To Local Procedure FGQA                                      383
            ASSIGN 5620 TO BLOCK_FGQA                                     383
            GO TO  5420                                                   383
5620        CONTINUE                                                      383
C       *                                                                 383
C       Geant POSITION : FGQA ===> X=FRAMEWIDTH/2.+FRAMEXYOFF Y=(DISKOUT  384
C       R+DISKINR)/2. Z=ZZFGZ <===                                        384
            CALL AGSRESET                                                 384
            AG_X = FRAMEWIDTH/2.+FRAMEXYOFF                               384
            AG_Y = (DISKOUTR+DISKINR)/2.                                  384
            AG_Z = ZZFGZ                                                  384
            AG_TITLE = 'FGQA'                                             384
            AG_EXNAME = 'POSITION'                                        384
            AG_PARLIST = 'X_Y_Z'                                          384
            CALL AXPOSITION                                               384
C       *                                                                 384
C       Create FGQB :                                                     385
C       Call To Local Procedure FGQB                                      385
            ASSIGN 5630 TO BLOCK_FGQB                                     385
            GO TO  5440                                                   385
5630        CONTINUE                                                      385
C       *                                                                 385
C       Geant POSITION : FGQB ===> Z=ZZFGZ <===                           386
            CALL AGSRESET                                                 386
            AG_Z = ZZFGZ                                                  386
            AG_TITLE = 'FGQB'                                             386
            AG_EXNAME = 'POSITION'                                        386
            AG_PARLIST = 'Z'                                              386
            CALL AXPOSITION                                               386
C       *                                                                 386
C       Create FGQC :                                                     387
C       Call To Local Procedure FGQC                                      387
            ASSIGN 5640 TO BLOCK_FGQC                                     387
            GO TO  5460                                                   387
5640        CONTINUE                                                      387
C       *                                                                 387
C       Geant POSITION : FGQC ===> X=(DISKOUTR+DISKINR)/2. Y=FRAMEWIDTH/  388
C       2.+FRAMEXYOFF Z=ZZFGZ <===                                        388
            CALL AGSRESET                                                 388
            AG_X = (DISKOUTR+DISKINR)/2.                                  388
            AG_Y = FRAMEWIDTH/2.+FRAMEXYOFF                               388
            AG_Z = ZZFGZ                                                  388
            AG_TITLE = 'FGQC'                                             388
            AG_EXNAME = 'POSITION'                                        388
            AG_PARLIST = 'X_Y_Z'                                          388
            CALL AXPOSITION                                               388
C       *                                                                 388
C       Create FGQD :                                                     389
C       Call To Local Procedure FGQD                                      389
            ASSIGN 5650 TO BLOCK_FGQD                                     389
            GO TO  5480                                                   389
5650        CONTINUE                                                      389
C       *                                                                 389
C       Geant POSITION : FGQD ===> Z=ZZFGZ <===                           390
            CALL AGSRESET                                                 390
            AG_Z = ZZFGZ                                                  390
            AG_TITLE = 'FGQD'                                             390
            AG_EXNAME = 'POSITION'                                        390
            AG_PARLIST = 'Z'                                              390
            CALL AXPOSITION                                               390
C       *                                                                 390
C       Create FGQE :                                                     391
C       Call To Local Procedure FGQE                                      391
            ASSIGN 5660 TO BLOCK_FGQE                                     391
            GO TO  5500                                                   391
5660        CONTINUE                                                      391
C       *                                                                 391
C       Geant POSITION : FGQE ===> X=(FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPE  394
C       RPANG*DEGRAD) Y=(FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD)  394
C        Z=ZZFGZ ALPHAZ=FLATPERPANG-90. <===                              394
            CALL AGSRESET                                                 394
            AG_X = (FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPERPANG*DEGRAD)       394
            AG_Y = (FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD)       394
            AG_Z = ZZFGZ                                                  394
            AG_ALPHAN=AG_ALPHAN+1                                         394
            AG_ALPHAK(AG_ALPHAN)=3                                        394
            AG_ALPHAL(AG_ALPHAN)=FLATPERPANG-90.                          394
            AG_TITLE = 'FGQE'                                             394
            AG_EXNAME = 'POSITION'                                        394
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   394
            CALL AXPOSITION                                               394
C       *                                                                 394
C       Create FGQF :                                                     395
C       Call To Local Procedure FGQF                                      395
            ASSIGN 5670 TO BLOCK_FGQF                                     395
            GO TO  5520                                                   395
5670        CONTINUE                                                      395
C       *                                                                 395
C       Geant POSITION : FGQF ===> Z=ZZFGZ <===                           396
            CALL AGSRESET                                                 396
            AG_Z = ZZFGZ                                                  396
            AG_TITLE = 'FGQF'                                             396
            AG_EXNAME = 'POSITION'                                        396
            AG_PARLIST = 'Z'                                              396
            CALL AXPOSITION                                               396
C       *                                                                 396
C       Create FGXA :                                                     397
C       Call To Local Procedure FGXA                                      397
            ASSIGN 5690 TO BLOCK_FGXA                                     397
            GO TO  5680                                                   397
5690        CONTINUE                                                      397
C       *                                                                 397
C       Geant POSITION : FGXA ===> Z=ZZFGZ <===                           398
            CALL AGSRESET                                                 398
            AG_Z = ZZFGZ                                                  398
            AG_TITLE = 'FGXA'                                             398
            AG_EXNAME = 'POSITION'                                        398
            AG_PARLIST = 'Z'                                              398
            CALL AXPOSITION                                               398
C       *                                                                 398
C       Create FGXB :                                                     399
C       Call To Local Procedure FGXB                                      399
            ASSIGN 5710 TO BLOCK_FGXB                                     399
            GO TO  5700                                                   399
5710        CONTINUE                                                      399
C       *                                                                 399
C       Geant POSITION : FGXB ===> Z=ZZFGZ <===                           400
            CALL AGSRESET                                                 400
            AG_Z = ZZFGZ                                                  400
            AG_TITLE = 'FGXB'                                             400
            AG_EXNAME = 'POSITION'                                        400
            AG_PARLIST = 'Z'                                              400
            CALL AXPOSITION                                               400
C       *                                                                 400
C       Create FGXC :                                                     401
C       Call To Local Procedure FGXC                                      401
            ASSIGN 5730 TO BLOCK_FGXC                                     401
            GO TO  5720                                                   401
5730        CONTINUE                                                      401
C       *                                                                 401
            XXFGZ=(DISKOUTR+DISKINR)/2.                                   402
C       Geant POSITION : FGXC ===> X=XXFGZ*COS(GRIDRAYLONGPHI*DEGRAD) Y=  404
C       XXFGZ*SIN(GRIDRAYLONGPHI*DEGRAD) Z=ZZFGZ ALPHAZ=GRIDRAYLONGPHI <  404
C       ===                                                               404
            CALL AGSRESET                                                 404
            AG_X = XXFGZ*COS(GRIDRAYLONGPHI*DEGRAD)                       404
            AG_Y = XXFGZ*SIN(GRIDRAYLONGPHI*DEGRAD)                       404
            AG_Z = ZZFGZ                                                  404
            AG_ALPHAN=AG_ALPHAN+1                                         404
            AG_ALPHAK(AG_ALPHAN)=3                                        404
            AG_ALPHAL(AG_ALPHAN)=GRIDRAYLONGPHI                           404
            AG_TITLE = 'FGXC'                                             404
            AG_EXNAME = 'POSITION'                                        404
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   404
            CALL AXPOSITION                                               404
C       *                                                                 404
C       Create FGXD :                                                     405
C       Call To Local Procedure FGXD                                      405
            ASSIGN 5750 TO BLOCK_FGXD                                     405
            GO TO  5740                                                   405
5750        CONTINUE                                                      405
C       *                                                                 405
            XXFGZ=(FLATOUTR+DISKINR)/2.                                   406
C       Geant POSITION : FGXD ===> X=XXFGZ*COS(GRIDRAYSHORTPHI*DEGRAD) Y  408
C       =XXFGZ*SIN(GRIDRAYSHORTPHI*DEGRAD) Z=ZZFGZ ALPHAZ=GRIDRAYSHORTPH  408
C       I <===                                                            408
            CALL AGSRESET                                                 408
            AG_X = XXFGZ*COS(GRIDRAYSHORTPHI*DEGRAD)                      408
            AG_Y = XXFGZ*SIN(GRIDRAYSHORTPHI*DEGRAD)                      408
            AG_Z = ZZFGZ                                                  408
            AG_ALPHAN=AG_ALPHAN+1                                         408
            AG_ALPHAK(AG_ALPHAN)=3                                        408
            AG_ALPHAL(AG_ALPHAN)=GRIDRAYSHORTPHI                          408
            AG_TITLE = 'FGXD'                                             408
            AG_EXNAME = 'POSITION'                                        408
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   408
            CALL AXPOSITION                                               408
C       *                                                                 408
         GO TO 5611                                                       409
5612     CONTINUE                                                         409
5231  CALL AGSPOPB                                                        410
      GO TO BLOCK_FGZC, (5230 ,5240)                                      410
C End of Local Procedure                                                  410
C ----------------------------------------------------------------------  411
C ------------ FZCB                                                       411
C Begin Local Procedure FZCB                                              412
C FZCB SENSITIVE VOLUME                                                   412
5330  CONTINUE                                                            412
         AG_TITLE='FZCB'                                                  412
         AG_MODULE=MODULE                                                 412
         AG_MARK='V00'                                                    412
         CALL AGSPUSH                                                     412
         AG_MODULE=MODULE                                                 412
C    *                                                                    412
C    Geant ATTRIBUTE : FZCB ===> SEEN=1 COLO=1 <===                       413
         CALL AGSRESET                                                    413
         AG_SEEN = 1                                                      413
         AG_COLO = 1                                                      413
         AG_TITLE = 'FZCB'                                                413
         AG_EXNAME = 'ATTRIBUTE'                                          413
         AG_PARLIST = 'SEEN_COLO'                                         413
         CALL AXATTRIBUTE                                                 413
C    *                                                                    413
C    Geant MATERIAL : ARCO2MIX ===> inherited arguments used <===         413
         CALL AGSRESET                                                    414
         AG_TITLE = 'ARCO2MIX'                                            414
         AG_EXNAME = 'MATERIAL'                                           414
         AG_PARLIST = 'NONE'                                              414
         CALL AXMATERIAL                                                  414
C    *                                                                    414
C    Geant MATERIAL : SENSITIVE_FGT_GAS ===> ISVOL=1 <===                 414
         CALL AGSRESET                                                    415
         AG_ISVOL = 1                                                     415
         AG_TITLE = 'SENSITIVE_FGT_GAS'                                   415
         AG_EXNAME = 'MATERIAL'                                           415
         AG_PARLIST = 'ISVOL'                                             415
         CALL AXMATERIAL                                                  415
C    *                                                                    415
C    Geant SHAPE : TUBS ===> RMIN=DISKINRT RMAX=DISKOUTR PHI1=0., PHI2=9  415
C    0. DZ=VOLSENSDZ/2. <===                                              415
         CALL AGSRESET                                                    417
         AG_RMIN = DISKINRT                                               417
         AG_RMAX = DISKOUTR                                               417
         AG_PHI1 = 0.                                                     417
         AG_PHI2 = 90.                                                    417
         AG_DZ = VOLSENSDZ/2.                                             417
         AG_TITLE = 'TUBS'                                                417
         AG_EXNAME = 'SHAPE'                                              417
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            417
         CALL AXSHAPE                                                     417
C    *                                                                    417
C    Defining Hits: ZZ:0.001: YY:0.001: XX:0.001: PTOT:16:(0,100) CX:10:  417
C     CY:10: CZ:10:, SLENG:16:(0,500) TOF:16:(0,1.0E-6) STEP:0.01: ELOSS  417
C    :16:(0,0.001) PX:16:(-250.0,+250.0), PY:16:(-250.0,+250.0) PZ:16:(-  417
C    250.0,+250.0)                                                        417
         AG_TITLE = 'FZCB'                                                420
         AG_EXNAME = 'HITS'                                               420
         AG_TYPE=AG_MAGIC                                                 420
         CALL AGSDETV('FZCB')                                             420
C    *                                                                    420
         AG_PARLIST='ZZ'                                                  420
         AG_TITLE = 'DETH'                                                420
         AG_NBITS = AG_MAGIC                                              420
         AG_BIN=0.001                                                     420
         AG_FHMIN=AG_MAGIC                                                420
         AG_FHMAX=AG_MAGIC                                                420
         AG_OPTION='*'                                                    420
         CALL AGSDETH (0)                                                 420
C    *                                                                    420
         AG_PARLIST='YY'                                                  420
         AG_TITLE = 'DETH'                                                420
         AG_NBITS = AG_MAGIC                                              420
         AG_BIN=0.001                                                     420
         AG_FHMIN=AG_MAGIC                                                420
         AG_FHMAX=AG_MAGIC                                                420
         AG_OPTION='*'                                                    420
         CALL AGSDETH (0)                                                 420
C    *                                                                    420
         AG_PARLIST='XX'                                                  420
         AG_TITLE = 'DETH'                                                420
         AG_NBITS = AG_MAGIC                                              420
         AG_BIN=0.001                                                     420
         AG_FHMIN=AG_MAGIC                                                420
         AG_FHMAX=AG_MAGIC                                                420
         AG_OPTION='*'                                                    420
         CALL AGSDETH (0)                                                 420
C    *                                                                    420
         AG_PARLIST='PTOT'                                                420
         AG_TITLE = 'DETH'                                                420
         AG_NBITS = 16                                                    420
         AG_BIN=AG_MAGIC                                                  420
         AG_OPTION='*'                                                    420
         AG_FHMIN = 0                                                     420
         AG_FHMAX = 100                                                   420
         CALL AGSDETH (0)                                                 420
C    *                                                                    420
         AG_PARLIST='CX'                                                  420
         AG_TITLE = 'DETH'                                                420
         AG_NBITS = 10                                                    420
         AG_BIN=AG_MAGIC                                                  420
         AG_FHMIN=AG_MAGIC                                                420
         AG_FHMAX=AG_MAGIC                                                420
         AG_OPTION='*'                                                    420
         CALL AGSDETH (0)                                                 420
C    *                                                                    420
         AG_PARLIST='CY'                                                  420
         AG_TITLE = 'DETH'                                                420
         AG_NBITS = 10                                                    420
         AG_BIN=AG_MAGIC                                                  420
         AG_FHMIN=AG_MAGIC                                                420
         AG_FHMAX=AG_MAGIC                                                420
         AG_OPTION='*'                                                    420
         CALL AGSDETH (0)                                                 420
C    *                                                                    420
         AG_PARLIST='CZ'                                                  420
         AG_TITLE = 'DETH'                                                420
         AG_NBITS = 10                                                    420
         AG_BIN=AG_MAGIC                                                  420
         AG_OPTION='*'                                                    420
         AG_FHMIN = AG_MAGIC                                              420
         AG_FHMAX = AG_MAGIC                                              420
         CALL AGSDETH (0)                                                 420
C    *                                                                    420
         AG_PARLIST='SLENG'                                               420
         AG_TITLE = 'DETH'                                                420
         AG_NBITS = 16                                                    420
         AG_BIN=AG_MAGIC                                                  420
         AG_OPTION='*'                                                    420
         AG_FHMIN = 0                                                     420
         AG_FHMAX = 500                                                   420
         CALL AGSDETH (0)                                                 420
C    *                                                                    420
         AG_PARLIST='TOF'                                                 420
         AG_TITLE = 'DETH'                                                420
         AG_NBITS = 16                                                    420
         AG_BIN=AG_MAGIC                                                  420
         AG_OPTION='*'                                                    420
         AG_FHMIN = 0                                                     420
         AG_FHMAX = 1.0E-6                                                420
         CALL AGSDETH (0)                                                 420
C    *                                                                    420
         AG_PARLIST='STEP'                                                420
         AG_TITLE = 'DETH'                                                420
         AG_NBITS = AG_MAGIC                                              420
         AG_BIN=0.01                                                      420
         AG_FHMIN=AG_MAGIC                                                420
         AG_FHMAX=AG_MAGIC                                                420
         AG_OPTION='*'                                                    420
         CALL AGSDETH (0)                                                 420
C    *                                                                    420
         AG_PARLIST='ELOSS'                                               420
         AG_TITLE = 'DETH'                                                420
         AG_NBITS = 16                                                    420
         AG_BIN=AG_MAGIC                                                  420
         AG_OPTION='*'                                                    420
         AG_FHMIN = 0                                                     420
         AG_FHMAX = 0.001                                                 420
         CALL AGSDETH (0)                                                 420
C    *                                                                    420
         AG_PARLIST='PX'                                                  420
         AG_TITLE = 'DETH'                                                420
         AG_NBITS = 16                                                    420
         AG_BIN=AG_MAGIC                                                  420
         AG_OPTION='*'                                                    420
         AG_FHMIN = -250.0                                                420
         AG_FHMAX = +250.0                                                420
         CALL AGSDETH (0)                                                 420
C    *                                                                    420
         AG_PARLIST='PY'                                                  420
         AG_TITLE = 'DETH'                                                420
         AG_NBITS = 16                                                    420
         AG_BIN=AG_MAGIC                                                  420
         AG_OPTION='*'                                                    420
         AG_FHMIN = -250.0                                                420
         AG_FHMAX = +250.0                                                420
         CALL AGSDETH (0)                                                 420
C    *                                                                    420
         AG_PARLIST='PZ'                                                  420
         AG_TITLE = 'DETH'                                                420
         AG_NBITS = 16                                                    420
         AG_BIN=AG_MAGIC                                                  420
         AG_OPTION='*'                                                    420
         AG_FHMIN = -250.0                                                420
         AG_FHMAX = +250.0                                                420
         CALL AGSDETH (0)                                                 420
C    *                                                                    420
         CALL AGSDETU('FZCB')                                             420
C    *                                                                    420
C    *                                                                    421
         K=0                                                              421
            GO TO 5763                                                    421
5761        K=K+1                                                         421
5763        IF(K-(1).GT.0) GO TO 5762                                     421
            ZZFGZ=(-VOLSENSDZ+FRAMEDZ)/2. * (1.-2.*K)                     422
C       Create FGQA :                                                     423
C       Call To Local Procedure FGQA                                      423
            ASSIGN 5770 TO BLOCK_FGQA                                     423
            GO TO  5420                                                   423
5770        CONTINUE                                                      423
C       *                                                                 423
C       Geant POSITION : FGQA ===> X=FRAMEWIDTH/2.+FRAMEXYOFF Y=(DISKOUT  424
C       R+DISKINRT)/2. Z=ZZFGZ <===                                       424
            CALL AGSRESET                                                 424
            AG_X = FRAMEWIDTH/2.+FRAMEXYOFF                               424
            AG_Y = (DISKOUTR+DISKINRT)/2.                                 424
            AG_Z = ZZFGZ                                                  424
            AG_TITLE = 'FGQA'                                             424
            AG_EXNAME = 'POSITION'                                        424
            AG_PARLIST = 'X_Y_Z'                                          424
            CALL AXPOSITION                                               424
C       *                                                                 424
C       Create FGQB :                                                     425
C       Call To Local Procedure FGQB                                      425
            ASSIGN 5780 TO BLOCK_FGQB                                     425
            GO TO  5440                                                   425
5780        CONTINUE                                                      425
C       *                                                                 425
C       Geant POSITION : FGQB ===> Z=ZZFGZ <===                           426
            CALL AGSRESET                                                 426
            AG_Z = ZZFGZ                                                  426
            AG_TITLE = 'FGQB'                                             426
            AG_EXNAME = 'POSITION'                                        426
            AG_PARLIST = 'Z'                                              426
            CALL AXPOSITION                                               426
C       *                                                                 426
C       Create FGQC :                                                     427
C       Call To Local Procedure FGQC                                      427
            ASSIGN 5790 TO BLOCK_FGQC                                     427
            GO TO  5460                                                   427
5790        CONTINUE                                                      427
C       *                                                                 427
C       Geant POSITION : FGQC ===> X=(DISKOUTR+DISKINRT)/2. Y=FRAMEWIDTH  428
C       /2.+FRAMEXYOFF Z=ZZFGZ <===                                       428
            CALL AGSRESET                                                 428
            AG_X = (DISKOUTR+DISKINRT)/2.                                 428
            AG_Y = FRAMEWIDTH/2.+FRAMEXYOFF                               428
            AG_Z = ZZFGZ                                                  428
            AG_TITLE = 'FGQC'                                             428
            AG_EXNAME = 'POSITION'                                        428
            AG_PARLIST = 'X_Y_Z'                                          428
            CALL AXPOSITION                                               428
C       *                                                                 428
C       Create FGQD :                                                     429
C       Call To Local Procedure FGQD                                      429
            ASSIGN 5800 TO BLOCK_FGQD                                     429
            GO TO  5480                                                   429
5800        CONTINUE                                                      429
C       *                                                                 429
C       Geant POSITION : FGQD ===> Z=ZZFGZ <===                           430
            CALL AGSRESET                                                 430
            AG_Z = ZZFGZ                                                  430
            AG_TITLE = 'FGQD'                                             430
            AG_EXNAME = 'POSITION'                                        430
            AG_PARLIST = 'Z'                                              430
            CALL AXPOSITION                                               430
C       *                                                                 430
C       Create FGQE :                                                     431
C       Call To Local Procedure FGQE                                      431
            ASSIGN 5810 TO BLOCK_FGQE                                     431
            GO TO  5500                                                   431
5810        CONTINUE                                                      431
C       *                                                                 431
C       Geant POSITION : FGQE ===> X=(FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPE  434
C       RPANG*DEGRAD) Y=(FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD)  434
C        Z=ZZFGZ ALPHAZ=FLATPERPANG-90. <===                              434
            CALL AGSRESET                                                 434
            AG_X = (FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPERPANG*DEGRAD)       434
            AG_Y = (FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD)       434
            AG_Z = ZZFGZ                                                  434
            AG_ALPHAN=AG_ALPHAN+1                                         434
            AG_ALPHAK(AG_ALPHAN)=3                                        434
            AG_ALPHAL(AG_ALPHAN)=FLATPERPANG-90.                          434
            AG_TITLE = 'FGQE'                                             434
            AG_EXNAME = 'POSITION'                                        434
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   434
            CALL AXPOSITION                                               434
C       *                                                                 434
C       Create FGQF :                                                     435
C       Call To Local Procedure FGQF                                      435
            ASSIGN 5820 TO BLOCK_FGQF                                     435
            GO TO  5520                                                   435
5820        CONTINUE                                                      435
C       *                                                                 435
C       Geant POSITION : FGQF ===> Z=ZZFGZ <===                           436
            CALL AGSRESET                                                 436
            AG_Z = ZZFGZ                                                  436
            AG_TITLE = 'FGQF'                                             436
            AG_EXNAME = 'POSITION'                                        436
            AG_PARLIST = 'Z'                                              436
            CALL AXPOSITION                                               436
C       *                                                                 436
C       Create FGXA :                                                     437
C       Call To Local Procedure FGXA                                      437
            ASSIGN 5830 TO BLOCK_FGXA                                     437
            GO TO  5680                                                   437
5830        CONTINUE                                                      437
C       *                                                                 437
C       Geant POSITION : FGXA ===> Z=ZZFGZ <===                           438
            CALL AGSRESET                                                 438
            AG_Z = ZZFGZ                                                  438
            AG_TITLE = 'FGXA'                                             438
            AG_EXNAME = 'POSITION'                                        438
            AG_PARLIST = 'Z'                                              438
            CALL AXPOSITION                                               438
C       *                                                                 438
C       Create FGXB :                                                     439
C       Call To Local Procedure FGXB                                      439
            ASSIGN 5840 TO BLOCK_FGXB                                     439
            GO TO  5700                                                   439
5840        CONTINUE                                                      439
C       *                                                                 439
C       Geant POSITION : FGXB ===> Z=ZZFGZ <===                           440
            CALL AGSRESET                                                 440
            AG_Z = ZZFGZ                                                  440
            AG_TITLE = 'FGXB'                                             440
            AG_EXNAME = 'POSITION'                                        440
            AG_PARLIST = 'Z'                                              440
            CALL AXPOSITION                                               440
C       *                                                                 440
C       Create FGXC :                                                     441
C       Call To Local Procedure FGXC                                      441
            ASSIGN 5850 TO BLOCK_FGXC                                     441
            GO TO  5720                                                   441
5850        CONTINUE                                                      441
C       *                                                                 441
            XXFGZ=(DISKOUTR+DISKINRT)/2.                                  442
C       Geant POSITION : FGXC ===> X=XXFGZ*COS(GRIDRAYLONGPHI*DEGRAD) Y=  444
C       XXFGZ*SIN(GRIDRAYLONGPHI*DEGRAD) Z=ZZFGZ ALPHAZ=GRIDRAYLONGPHI <  444
C       ===                                                               444
            CALL AGSRESET                                                 444
            AG_X = XXFGZ*COS(GRIDRAYLONGPHI*DEGRAD)                       444
            AG_Y = XXFGZ*SIN(GRIDRAYLONGPHI*DEGRAD)                       444
            AG_Z = ZZFGZ                                                  444
            AG_ALPHAN=AG_ALPHAN+1                                         444
            AG_ALPHAK(AG_ALPHAN)=3                                        444
            AG_ALPHAL(AG_ALPHAN)=GRIDRAYLONGPHI                           444
            AG_TITLE = 'FGXC'                                             444
            AG_EXNAME = 'POSITION'                                        444
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   444
            CALL AXPOSITION                                               444
C       *                                                                 444
C       Create FGXD :                                                     445
C       Call To Local Procedure FGXD                                      445
            ASSIGN 5860 TO BLOCK_FGXD                                     445
            GO TO  5740                                                   445
5860        CONTINUE                                                      445
C       *                                                                 445
            XXFGZ=(FLATOUTR+DISKINRT)/2.                                  446
C       Geant POSITION : FGXD ===> X=XXFGZ*COS(GRIDRAYSHORTPHI*DEGRAD) Y  448
C       =XXFGZ*SIN(GRIDRAYSHORTPHI*DEGRAD) Z=ZZFGZ ALPHAZ=GRIDRAYSHORTPH  448
C       I <===                                                            448
            CALL AGSRESET                                                 448
            AG_X = XXFGZ*COS(GRIDRAYSHORTPHI*DEGRAD)                      448
            AG_Y = XXFGZ*SIN(GRIDRAYSHORTPHI*DEGRAD)                      448
            AG_Z = ZZFGZ                                                  448
            AG_ALPHAN=AG_ALPHAN+1                                         448
            AG_ALPHAK(AG_ALPHAN)=3                                        448
            AG_ALPHAL(AG_ALPHAN)=GRIDRAYSHORTPHI                          448
            AG_TITLE = 'FGXD'                                             448
            AG_EXNAME = 'POSITION'                                        448
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   448
            CALL AXPOSITION                                               448
C       *                                                                 448
         GO TO 5761                                                       449
5762     CONTINUE                                                         449
5331  CALL AGSPOPB                                                        450
      GO TO BLOCK_FZCB, (5330 ,5340)                                      450
C End of Local Procedure                                                  450
C ----------------------------------------------------------------------  451
C ------------ FGVG                                                       451
C Begin Local Procedure FGVG                                              452
C FGVG QUAD 3GEM MIXTURE VOLUME                                           452
5250  CONTINUE                                                            452
         AG_TITLE='FGVG'                                                  452
         AG_MODULE=MODULE                                                 452
         AG_MARK='V00'                                                    452
         CALL AGSPUSH                                                     452
         AG_MODULE=MODULE                                                 452
C    *                                                                    452
C    Geant ATTRIBUTE : FGVG ===> SEEN=0 COLO=6 <===                       453
         CALL AGSRESET                                                    453
         AG_SEEN = 0                                                      453
         AG_COLO = 6                                                      453
         AG_TITLE = 'FGVG'                                                453
         AG_EXNAME = 'ATTRIBUTE'                                          453
         AG_PARLIST = 'SEEN_COLO'                                         453
         CALL AXATTRIBUTE                                                 453
C    *                                                                    453
C    Geant MATERIAL : GEMMIX ===> inherited arguments used <===           453
         CALL AGSRESET                                                    454
         AG_TITLE = 'GEMMIX'                                              454
         AG_EXNAME = 'MATERIAL'                                           454
         AG_PARLIST = 'NONE'                                              454
         CALL AXMATERIAL                                                  454
C    *                                                                    454
C    Geant SHAPE : TUBS ===> RMIN=DISKINR RMAX=DISKOUTR PHI1=0., PHI2=90  454
C    . DZ=VOLGEMDZ/2. <===                                                454
         CALL AGSRESET                                                    456
         AG_RMIN = DISKINR                                                456
         AG_RMAX = DISKOUTR                                               456
         AG_PHI1 = 0.                                                     456
         AG_PHI2 = 90.                                                    456
         AG_DZ = VOLGEMDZ/2.                                              456
         AG_TITLE = 'TUBS'                                                456
         AG_EXNAME = 'SHAPE'                                              456
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            456
         CALL AXSHAPE                                                     456
C    *                                                                    456
C    *                                                                    456
         K=0                                                              457
            GO TO 5873                                                    457
5871        K=K+1                                                         457
5873        IF(K-(2).GT.0) GO TO 5872                                     457
            ZZFGZ=(-VOLGEMDZ+FRAMEDZ-0.01)/2. * (-1+K)                    458
C       Create FGQA :                                                     459
C       Call To Local Procedure FGQA                                      459
            ASSIGN 5880 TO BLOCK_FGQA                                     459
            GO TO  5420                                                   459
5880        CONTINUE                                                      459
C       *                                                                 459
C       Geant POSITION : FGQA ===> X=FRAMEWIDTH/2.+FRAMEXYOFF Y=(DISKOUT  460
C       R+DISKINR)/2. Z=ZZFGZ <===                                        460
            CALL AGSRESET                                                 460
            AG_X = FRAMEWIDTH/2.+FRAMEXYOFF                               460
            AG_Y = (DISKOUTR+DISKINR)/2.                                  460
            AG_Z = ZZFGZ                                                  460
            AG_TITLE = 'FGQA'                                             460
            AG_EXNAME = 'POSITION'                                        460
            AG_PARLIST = 'X_Y_Z'                                          460
            CALL AXPOSITION                                               460
C       *                                                                 460
C       Create FGQB :                                                     461
C       Call To Local Procedure FGQB                                      461
            ASSIGN 5890 TO BLOCK_FGQB                                     461
            GO TO  5440                                                   461
5890        CONTINUE                                                      461
C       *                                                                 461
C       Geant POSITION : FGQB ===> Z=ZZFGZ <===                           462
            CALL AGSRESET                                                 462
            AG_Z = ZZFGZ                                                  462
            AG_TITLE = 'FGQB'                                             462
            AG_EXNAME = 'POSITION'                                        462
            AG_PARLIST = 'Z'                                              462
            CALL AXPOSITION                                               462
C       *                                                                 462
C       Create FGQC :                                                     463
C       Call To Local Procedure FGQC                                      463
            ASSIGN 5900 TO BLOCK_FGQC                                     463
            GO TO  5460                                                   463
5900        CONTINUE                                                      463
C       *                                                                 463
C       Geant POSITION : FGQC ===> X=(DISKOUTR+DISKINR)/2. Y=FRAMEWIDTH/  464
C       2.+FRAMEXYOFF Z=ZZFGZ <===                                        464
            CALL AGSRESET                                                 464
            AG_X = (DISKOUTR+DISKINR)/2.                                  464
            AG_Y = FRAMEWIDTH/2.+FRAMEXYOFF                               464
            AG_Z = ZZFGZ                                                  464
            AG_TITLE = 'FGQC'                                             464
            AG_EXNAME = 'POSITION'                                        464
            AG_PARLIST = 'X_Y_Z'                                          464
            CALL AXPOSITION                                               464
C       *                                                                 464
C       Create FGQD :                                                     465
C       Call To Local Procedure FGQD                                      465
            ASSIGN 5910 TO BLOCK_FGQD                                     465
            GO TO  5480                                                   465
5910        CONTINUE                                                      465
C       *                                                                 465
C       Geant POSITION : FGQD ===> Z=ZZFGZ <===                           466
            CALL AGSRESET                                                 466
            AG_Z = ZZFGZ                                                  466
            AG_TITLE = 'FGQD'                                             466
            AG_EXNAME = 'POSITION'                                        466
            AG_PARLIST = 'Z'                                              466
            CALL AXPOSITION                                               466
C       *                                                                 466
C       Create FGQE :                                                     467
C       Call To Local Procedure FGQE                                      467
            ASSIGN 5920 TO BLOCK_FGQE                                     467
            GO TO  5500                                                   467
5920        CONTINUE                                                      467
C       *                                                                 467
C       Geant POSITION : FGQE ===> X=(FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPE  470
C       RPANG*DEGRAD) Y=(FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD)  470
C        Z=ZZFGZ ALPHAZ=FLATPERPANG-90. <===                              470
            CALL AGSRESET                                                 470
            AG_X = (FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPERPANG*DEGRAD)       470
            AG_Y = (FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD)       470
            AG_Z = ZZFGZ                                                  470
            AG_ALPHAN=AG_ALPHAN+1                                         470
            AG_ALPHAK(AG_ALPHAN)=3                                        470
            AG_ALPHAL(AG_ALPHAN)=FLATPERPANG-90.                          470
            AG_TITLE = 'FGQE'                                             470
            AG_EXNAME = 'POSITION'                                        470
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   470
            CALL AXPOSITION                                               470
C       *                                                                 470
C       Create FGQF :                                                     471
C       Call To Local Procedure FGQF                                      471
            ASSIGN 5930 TO BLOCK_FGQF                                     471
            GO TO  5520                                                   471
5930        CONTINUE                                                      471
C       *                                                                 471
C       Geant POSITION : FGQF ===> Z=ZZFGZ <===                           472
            CALL AGSRESET                                                 472
            AG_Z = ZZFGZ                                                  472
            AG_TITLE = 'FGQF'                                             472
            AG_EXNAME = 'POSITION'                                        472
            AG_PARLIST = 'Z'                                              472
            CALL AXPOSITION                                               472
C       *                                                                 472
C       Create FGXA :                                                     473
C       Call To Local Procedure FGXA                                      473
            ASSIGN 5940 TO BLOCK_FGXA                                     473
            GO TO  5680                                                   473
5940        CONTINUE                                                      473
C       *                                                                 473
C       Geant POSITION : FGXA ===> Z=ZZFGZ <===                           474
            CALL AGSRESET                                                 474
            AG_Z = ZZFGZ                                                  474
            AG_TITLE = 'FGXA'                                             474
            AG_EXNAME = 'POSITION'                                        474
            AG_PARLIST = 'Z'                                              474
            CALL AXPOSITION                                               474
C       *                                                                 474
C       Create FGXB :                                                     475
C       Call To Local Procedure FGXB                                      475
            ASSIGN 5950 TO BLOCK_FGXB                                     475
            GO TO  5700                                                   475
5950        CONTINUE                                                      475
C       *                                                                 475
C       Geant POSITION : FGXB ===> Z=ZZFGZ <===                           476
            CALL AGSRESET                                                 476
            AG_Z = ZZFGZ                                                  476
            AG_TITLE = 'FGXB'                                             476
            AG_EXNAME = 'POSITION'                                        476
            AG_PARLIST = 'Z'                                              476
            CALL AXPOSITION                                               476
C       *                                                                 476
C       Create FGXC :                                                     477
C       Call To Local Procedure FGXC                                      477
            ASSIGN 5960 TO BLOCK_FGXC                                     477
            GO TO  5720                                                   477
5960        CONTINUE                                                      477
C       *                                                                 477
            XXFGZ=(DISKOUTR+DISKINR)/2.                                   478
C       Geant POSITION : FGXC ===> X=XXFGZ*COS(GRIDRAYLONGPHI*DEGRAD) Y=  480
C       XXFGZ*SIN(GRIDRAYLONGPHI*DEGRAD) Z=ZZFGZ ALPHAZ=GRIDRAYLONGPHI <  480
C       ===                                                               480
            CALL AGSRESET                                                 480
            AG_X = XXFGZ*COS(GRIDRAYLONGPHI*DEGRAD)                       480
            AG_Y = XXFGZ*SIN(GRIDRAYLONGPHI*DEGRAD)                       480
            AG_Z = ZZFGZ                                                  480
            AG_ALPHAN=AG_ALPHAN+1                                         480
            AG_ALPHAK(AG_ALPHAN)=3                                        480
            AG_ALPHAL(AG_ALPHAN)=GRIDRAYLONGPHI                           480
            AG_TITLE = 'FGXC'                                             480
            AG_EXNAME = 'POSITION'                                        480
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   480
            CALL AXPOSITION                                               480
C       *                                                                 480
C       Create FGXD :                                                     481
C       Call To Local Procedure FGXD                                      481
            ASSIGN 5970 TO BLOCK_FGXD                                     481
            GO TO  5740                                                   481
5970        CONTINUE                                                      481
C       *                                                                 481
            XXFGZ=(FLATOUTR+DISKINR)/2.                                   482
C       Geant POSITION : FGXD ===> X=XXFGZ*COS(GRIDRAYSHORTPHI*DEGRAD) Y  484
C       =XXFGZ*SIN(GRIDRAYSHORTPHI*DEGRAD) Z=ZZFGZ ALPHAZ=GRIDRAYSHORTPH  484
C       I <===                                                            484
            CALL AGSRESET                                                 484
            AG_X = XXFGZ*COS(GRIDRAYSHORTPHI*DEGRAD)                      484
            AG_Y = XXFGZ*SIN(GRIDRAYSHORTPHI*DEGRAD)                      484
            AG_Z = ZZFGZ                                                  484
            AG_ALPHAN=AG_ALPHAN+1                                         484
            AG_ALPHAK(AG_ALPHAN)=3                                        484
            AG_ALPHAL(AG_ALPHAN)=GRIDRAYSHORTPHI                          484
            AG_TITLE = 'FGXD'                                             484
            AG_EXNAME = 'POSITION'                                        484
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   484
            CALL AXPOSITION                                               484
C       *                                                                 484
         GO TO 5871                                                       485
5872     CONTINUE                                                         485
5251  CALL AGSPOPB                                                        486
      GO TO BLOCK_FGVG, (5250 ,5260)                                      486
C End of Local Procedure                                                  486
C ----------------------------------------------------------------------  487
C ------------ FVGB                                                       487
C Begin Local Procedure FVGB                                              488
C FVGB QUAD 3GEM MIXTURE VOLUME                                           488
5350  CONTINUE                                                            488
         AG_TITLE='FVGB'                                                  488
         AG_MODULE=MODULE                                                 488
         AG_MARK='V00'                                                    488
         CALL AGSPUSH                                                     488
         AG_MODULE=MODULE                                                 488
C    *                                                                    488
C    Geant ATTRIBUTE : FVGB ===> SEEN=0 COLO=6 <===                       489
         CALL AGSRESET                                                    489
         AG_SEEN = 0                                                      489
         AG_COLO = 6                                                      489
         AG_TITLE = 'FVGB'                                                489
         AG_EXNAME = 'ATTRIBUTE'                                          489
         AG_PARLIST = 'SEEN_COLO'                                         489
         CALL AXATTRIBUTE                                                 489
C    *                                                                    489
C    Geant MATERIAL : GEMMIX ===> inherited arguments used <===           489
         CALL AGSRESET                                                    490
         AG_TITLE = 'GEMMIX'                                              490
         AG_EXNAME = 'MATERIAL'                                           490
         AG_PARLIST = 'NONE'                                              490
         CALL AXMATERIAL                                                  490
C    *                                                                    490
C    Geant SHAPE : TUBS ===> RMIN=DISKINRT RMAX=DISKOUTR PHI1=0., PHI2=9  490
C    0. DZ=VOLGEMDZ/2. <===                                               490
         CALL AGSRESET                                                    492
         AG_RMIN = DISKINRT                                               492
         AG_RMAX = DISKOUTR                                               492
         AG_PHI1 = 0.                                                     492
         AG_PHI2 = 90.                                                    492
         AG_DZ = VOLGEMDZ/2.                                              492
         AG_TITLE = 'TUBS'                                                492
         AG_EXNAME = 'SHAPE'                                              492
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            492
         CALL AXSHAPE                                                     492
C    *                                                                    492
C    *                                                                    492
         K=0                                                              493
            GO TO 5983                                                    493
5981        K=K+1                                                         493
5983        IF(K-(2).GT.0) GO TO 5982                                     493
            ZZFGZ=(-VOLGEMDZ+FRAMEDZ-0.01)/2. * (-1+K)                    494
C       Create FGQA :                                                     495
C       Call To Local Procedure FGQA                                      495
            ASSIGN 5990 TO BLOCK_FGQA                                     495
            GO TO  5420                                                   495
5990        CONTINUE                                                      495
C       *                                                                 495
C       Geant POSITION : FGQA ===> X=FRAMEWIDTH/2.+FRAMEXYOFF Y=(DISKOUT  496
C       R+DISKINRT)/2. Z=ZZFGZ <===                                       496
            CALL AGSRESET                                                 496
            AG_X = FRAMEWIDTH/2.+FRAMEXYOFF                               496
            AG_Y = (DISKOUTR+DISKINRT)/2.                                 496
            AG_Z = ZZFGZ                                                  496
            AG_TITLE = 'FGQA'                                             496
            AG_EXNAME = 'POSITION'                                        496
            AG_PARLIST = 'X_Y_Z'                                          496
            CALL AXPOSITION                                               496
C       *                                                                 496
C       Create FGQB :                                                     497
C       Call To Local Procedure FGQB                                      497
            ASSIGN 6000 TO BLOCK_FGQB                                     497
            GO TO  5440                                                   497
6000        CONTINUE                                                      497
C       *                                                                 497
C       Geant POSITION : FGQB ===> Z=ZZFGZ <===                           498
            CALL AGSRESET                                                 498
            AG_Z = ZZFGZ                                                  498
            AG_TITLE = 'FGQB'                                             498
            AG_EXNAME = 'POSITION'                                        498
            AG_PARLIST = 'Z'                                              498
            CALL AXPOSITION                                               498
C       *                                                                 498
C       Create FGQC :                                                     499
C       Call To Local Procedure FGQC                                      499
            ASSIGN 6010 TO BLOCK_FGQC                                     499
            GO TO  5460                                                   499
6010        CONTINUE                                                      499
C       *                                                                 499
C       Geant POSITION : FGQC ===> X=(DISKOUTR+DISKINRT)/2. Y=FRAMEWIDTH  500
C       /2.+FRAMEXYOFF Z=ZZFGZ <===                                       500
            CALL AGSRESET                                                 500
            AG_X = (DISKOUTR+DISKINRT)/2.                                 500
            AG_Y = FRAMEWIDTH/2.+FRAMEXYOFF                               500
            AG_Z = ZZFGZ                                                  500
            AG_TITLE = 'FGQC'                                             500
            AG_EXNAME = 'POSITION'                                        500
            AG_PARLIST = 'X_Y_Z'                                          500
            CALL AXPOSITION                                               500
C       *                                                                 500
C       Create FGQD :                                                     501
C       Call To Local Procedure FGQD                                      501
            ASSIGN 6020 TO BLOCK_FGQD                                     501
            GO TO  5480                                                   501
6020        CONTINUE                                                      501
C       *                                                                 501
C       Geant POSITION : FGQD ===> Z=ZZFGZ <===                           502
            CALL AGSRESET                                                 502
            AG_Z = ZZFGZ                                                  502
            AG_TITLE = 'FGQD'                                             502
            AG_EXNAME = 'POSITION'                                        502
            AG_PARLIST = 'Z'                                              502
            CALL AXPOSITION                                               502
C       *                                                                 502
C       Create FGQE :                                                     503
C       Call To Local Procedure FGQE                                      503
            ASSIGN 6030 TO BLOCK_FGQE                                     503
            GO TO  5500                                                   503
6030        CONTINUE                                                      503
C       *                                                                 503
C       Geant POSITION : FGQE ===> X=(FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPE  506
C       RPANG*DEGRAD) Y=(FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD)  506
C        Z=ZZFGZ ALPHAZ=FLATPERPANG-90. <===                              506
            CALL AGSRESET                                                 506
            AG_X = (FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPERPANG*DEGRAD)       506
            AG_Y = (FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD)       506
            AG_Z = ZZFGZ                                                  506
            AG_ALPHAN=AG_ALPHAN+1                                         506
            AG_ALPHAK(AG_ALPHAN)=3                                        506
            AG_ALPHAL(AG_ALPHAN)=FLATPERPANG-90.                          506
            AG_TITLE = 'FGQE'                                             506
            AG_EXNAME = 'POSITION'                                        506
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   506
            CALL AXPOSITION                                               506
C       *                                                                 506
C       Create FGQF :                                                     507
C       Call To Local Procedure FGQF                                      507
            ASSIGN 6040 TO BLOCK_FGQF                                     507
            GO TO  5520                                                   507
6040        CONTINUE                                                      507
C       *                                                                 507
C       Geant POSITION : FGQF ===> Z=ZZFGZ <===                           508
            CALL AGSRESET                                                 508
            AG_Z = ZZFGZ                                                  508
            AG_TITLE = 'FGQF'                                             508
            AG_EXNAME = 'POSITION'                                        508
            AG_PARLIST = 'Z'                                              508
            CALL AXPOSITION                                               508
C       *                                                                 508
C       Create FGXA :                                                     509
C       Call To Local Procedure FGXA                                      509
            ASSIGN 6050 TO BLOCK_FGXA                                     509
            GO TO  5680                                                   509
6050        CONTINUE                                                      509
C       *                                                                 509
C       Geant POSITION : FGXA ===> Z=ZZFGZ <===                           510
            CALL AGSRESET                                                 510
            AG_Z = ZZFGZ                                                  510
            AG_TITLE = 'FGXA'                                             510
            AG_EXNAME = 'POSITION'                                        510
            AG_PARLIST = 'Z'                                              510
            CALL AXPOSITION                                               510
C       *                                                                 510
C       Create FGXB :                                                     511
C       Call To Local Procedure FGXB                                      511
            ASSIGN 6060 TO BLOCK_FGXB                                     511
            GO TO  5700                                                   511
6060        CONTINUE                                                      511
C       *                                                                 511
C       Geant POSITION : FGXB ===> Z=ZZFGZ <===                           512
            CALL AGSRESET                                                 512
            AG_Z = ZZFGZ                                                  512
            AG_TITLE = 'FGXB'                                             512
            AG_EXNAME = 'POSITION'                                        512
            AG_PARLIST = 'Z'                                              512
            CALL AXPOSITION                                               512
C       *                                                                 512
C       Create FXCB :                                                     513
C       Call To Local Procedure FXCB                                      513
            ASSIGN 6080 TO BLOCK_FXCB                                     513
            GO TO  6070                                                   513
6080        CONTINUE                                                      513
C       *                                                                 513
            XXFGZ=(DISKOUTR+DISKINRT)/2.                                  514
C       Geant POSITION : FXCB ===> X=XXFGZ*COS(GRIDRAYLONGPHI*DEGRAD) Y=  516
C       XXFGZ*SIN(GRIDRAYLONGPHI*DEGRAD) Z=ZZFGZ ALPHAZ=GRIDRAYLONGPHI <  516
C       ===                                                               516
            CALL AGSRESET                                                 516
            AG_X = XXFGZ*COS(GRIDRAYLONGPHI*DEGRAD)                       516
            AG_Y = XXFGZ*SIN(GRIDRAYLONGPHI*DEGRAD)                       516
            AG_Z = ZZFGZ                                                  516
            AG_ALPHAN=AG_ALPHAN+1                                         516
            AG_ALPHAK(AG_ALPHAN)=3                                        516
            AG_ALPHAL(AG_ALPHAN)=GRIDRAYLONGPHI                           516
            AG_TITLE = 'FXCB'                                             516
            AG_EXNAME = 'POSITION'                                        516
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   516
            CALL AXPOSITION                                               516
C       *                                                                 516
C       Create FXDB :                                                     517
C       Call To Local Procedure FXDB                                      517
            ASSIGN 6100 TO BLOCK_FXDB                                     517
            GO TO  6090                                                   517
6100        CONTINUE                                                      517
C       *                                                                 517
            XXFGZ=(FLATOUTR+DISKINRT)/2.                                  518
C       Geant POSITION : FXDB ===> X=XXFGZ*COS(GRIDRAYSHORTPHI*DEGRAD) Y  520
C       =XXFGZ*SIN(GRIDRAYSHORTPHI*DEGRAD) Z=ZZFGZ ALPHAZ=GRIDRAYSHORTPH  520
C       I <===                                                            520
            CALL AGSRESET                                                 520
            AG_X = XXFGZ*COS(GRIDRAYSHORTPHI*DEGRAD)                      520
            AG_Y = XXFGZ*SIN(GRIDRAYSHORTPHI*DEGRAD)                      520
            AG_Z = ZZFGZ                                                  520
            AG_ALPHAN=AG_ALPHAN+1                                         520
            AG_ALPHAK(AG_ALPHAN)=3                                        520
            AG_ALPHAL(AG_ALPHAN)=GRIDRAYSHORTPHI                          520
            AG_TITLE = 'FXDB'                                             520
            AG_EXNAME = 'POSITION'                                        520
            AG_PARLIST = 'X_Y_Z_ALPHAZ'                                   520
            CALL AXPOSITION                                               520
C       *                                                                 520
         GO TO 5981                                                       521
5982     CONTINUE                                                         521
5351  CALL AGSPOPB                                                        522
      GO TO BLOCK_FVGB, (5350 ,5360)                                      522
C End of Local Procedure                                                  522
C ----------------------------------------------------------------------  523
C ------------ FGVR                                                       523
C Begin Local Procedure FGVR                                              524
C FGVR QUAD READOUT MIXTURE VOLUME                                        524
5270  CONTINUE                                                            524
         AG_TITLE='FGVR'                                                  524
         AG_MODULE=MODULE                                                 524
         AG_MARK='V00'                                                    524
         CALL AGSPUSH                                                     524
         AG_MODULE=MODULE                                                 524
C    *                                                                    524
C    Geant ATTRIBUTE : FGVR ===> SEEN=0 COLO=5 <===                       525
         CALL AGSRESET                                                    525
         AG_SEEN = 0                                                      525
         AG_COLO = 5                                                      525
         AG_TITLE = 'FGVR'                                                525
         AG_EXNAME = 'ATTRIBUTE'                                          525
         AG_PARLIST = 'SEEN_COLO'                                         525
         CALL AXATTRIBUTE                                                 525
C    *                                                                    525
C    Geant MATERIAL : READMIX ===> inherited arguments used <===          525
         CALL AGSRESET                                                    526
         AG_TITLE = 'READMIX'                                             526
         AG_EXNAME = 'MATERIAL'                                           526
         AG_PARLIST = 'NONE'                                              526
         CALL AXMATERIAL                                                  526
C    *                                                                    526
C    Geant SHAPE : TUBS ===> RMIN=DISKINR RMAX=DISKOUTR PHI1=0., PHI2=90  526
C    . DZ=VOLREADDZ/2. <===                                               526
         CALL AGSRESET                                                    528
         AG_RMIN = DISKINR                                                528
         AG_RMAX = DISKOUTR                                               528
         AG_PHI1 = 0.                                                     528
         AG_PHI2 = 90.                                                    528
         AG_DZ = VOLREADDZ/2.                                             528
         AG_TITLE = 'TUBS'                                                528
         AG_EXNAME = 'SHAPE'                                              528
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            528
         CALL AXSHAPE                                                     528
C    *                                                                    528
         ZZFGZ= - VOLREADDZ/2. + FRAMEDZ/1.5                              528
C    Create FGQA :                                                        530
C    Call To Local Procedure FGQA                                         530
         ASSIGN 6110 TO BLOCK_FGQA                                        530
         GO TO  5420                                                      530
6110     CONTINUE                                                         530
C    *                                                                    530
C    Geant POSITION : FGQA ===> X=FRAMEWIDTH/2.+FRAMEXYOFF Y=(DISKOUTR+D  531
C    ISKINR)/2. Z=ZZFGZ <===                                              531
         CALL AGSRESET                                                    531
         AG_X = FRAMEWIDTH/2.+FRAMEXYOFF                                  531
         AG_Y = (DISKOUTR+DISKINR)/2.                                     531
         AG_Z = ZZFGZ                                                     531
         AG_TITLE = 'FGQA'                                                531
         AG_EXNAME = 'POSITION'                                           531
         AG_PARLIST = 'X_Y_Z'                                             531
         CALL AXPOSITION                                                  531
C    *                                                                    531
C    Create FGQB :                                                        532
C    Call To Local Procedure FGQB                                         532
         ASSIGN 6120 TO BLOCK_FGQB                                        532
         GO TO  5440                                                      532
6120     CONTINUE                                                         532
C    *                                                                    532
C    Geant POSITION : FGQB ===> Z=ZZFGZ <===                              533
         CALL AGSRESET                                                    533
         AG_Z = ZZFGZ                                                     533
         AG_TITLE = 'FGQB'                                                533
         AG_EXNAME = 'POSITION'                                           533
         AG_PARLIST = 'Z'                                                 533
         CALL AXPOSITION                                                  533
C    *                                                                    533
C    Create FGQC :                                                        534
C    Call To Local Procedure FGQC                                         534
         ASSIGN 6130 TO BLOCK_FGQC                                        534
         GO TO  5460                                                      534
6130     CONTINUE                                                         534
C    *                                                                    534
C    Geant POSITION : FGQC ===> X=(DISKOUTR+DISKINR)/2. Y=FRAMEWIDTH/2.+  535
C    FRAMEXYOFF Z=ZZFGZ <===                                              535
         CALL AGSRESET                                                    535
         AG_X = (DISKOUTR+DISKINR)/2.                                     535
         AG_Y = FRAMEWIDTH/2.+FRAMEXYOFF                                  535
         AG_Z = ZZFGZ                                                     535
         AG_TITLE = 'FGQC'                                                535
         AG_EXNAME = 'POSITION'                                           535
         AG_PARLIST = 'X_Y_Z'                                             535
         CALL AXPOSITION                                                  535
C    *                                                                    535
C    Create FGQD :                                                        536
C    Call To Local Procedure FGQD                                         536
         ASSIGN 6140 TO BLOCK_FGQD                                        536
         GO TO  5480                                                      536
6140     CONTINUE                                                         536
C    *                                                                    536
C    Geant POSITION : FGQD ===> Z=ZZFGZ <===                              537
         CALL AGSRESET                                                    537
         AG_Z = ZZFGZ                                                     537
         AG_TITLE = 'FGQD'                                                537
         AG_EXNAME = 'POSITION'                                           537
         AG_PARLIST = 'Z'                                                 537
         CALL AXPOSITION                                                  537
C    *                                                                    537
C    Create FGQE :                                                        538
C    Call To Local Procedure FGQE                                         538
         ASSIGN 6150 TO BLOCK_FGQE                                        538
         GO TO  5500                                                      538
6150     CONTINUE                                                         538
C    *                                                                    538
C    Geant POSITION : FGQE ===> X=(FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPERPA  541
C    NG*DEGRAD) Y=(FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD) Z=ZZF  541
C    GZ ALPHAZ=FLATPERPANG-90. <===                                       541
         CALL AGSRESET                                                    541
         AG_X = (FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPERPANG*DEGRAD)          541
         AG_Y = (FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD)          541
         AG_Z = ZZFGZ                                                     541
         AG_ALPHAN=AG_ALPHAN+1                                            541
         AG_ALPHAK(AG_ALPHAN)=3                                           541
         AG_ALPHAL(AG_ALPHAN)=FLATPERPANG-90.                             541
         AG_TITLE = 'FGQE'                                                541
         AG_EXNAME = 'POSITION'                                           541
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      541
         CALL AXPOSITION                                                  541
C    *                                                                    541
C    Create FGQF :                                                        542
C    Call To Local Procedure FGQF                                         542
         ASSIGN 6160 TO BLOCK_FGQF                                        542
         GO TO  5520                                                      542
6160     CONTINUE                                                         542
C    *                                                                    542
C    Geant POSITION : FGQF ===> Z=ZZFGZ <===                              543
         CALL AGSRESET                                                    543
         AG_Z = ZZFGZ                                                     543
         AG_TITLE = 'FGQF'                                                543
         AG_EXNAME = 'POSITION'                                           543
         AG_PARLIST = 'Z'                                                 543
         CALL AXPOSITION                                                  543
C    *                                                                    543
5271  CALL AGSPOPB                                                        544
      GO TO BLOCK_FGVR, (5270 ,5280)                                      544
C End of Local Procedure                                                  544
C ----------------------------------------------------------------------  545
C ------------ FVRB                                                       545
C Begin Local Procedure FVRB                                              546
C FVRB QUAD READOUT MIXTURE VOLUME                                        546
5370  CONTINUE                                                            546
         AG_TITLE='FVRB'                                                  546
         AG_MODULE=MODULE                                                 546
         AG_MARK='V00'                                                    546
         CALL AGSPUSH                                                     546
         AG_MODULE=MODULE                                                 546
C    *                                                                    546
C    Geant ATTRIBUTE : FVRB ===> SEEN=0 COLO=5 <===                       547
         CALL AGSRESET                                                    547
         AG_SEEN = 0                                                      547
         AG_COLO = 5                                                      547
         AG_TITLE = 'FVRB'                                                547
         AG_EXNAME = 'ATTRIBUTE'                                          547
         AG_PARLIST = 'SEEN_COLO'                                         547
         CALL AXATTRIBUTE                                                 547
C    *                                                                    547
C    Geant MATERIAL : READMIX ===> inherited arguments used <===          547
         CALL AGSRESET                                                    548
         AG_TITLE = 'READMIX'                                             548
         AG_EXNAME = 'MATERIAL'                                           548
         AG_PARLIST = 'NONE'                                              548
         CALL AXMATERIAL                                                  548
C    *                                                                    548
C    Geant SHAPE : TUBS ===> RMIN=DISKINRT RMAX=DISKOUTR PHI1=0., PHI2=9  548
C    0. DZ=VOLREADDZ/2. <===                                              548
         CALL AGSRESET                                                    550
         AG_RMIN = DISKINRT                                               550
         AG_RMAX = DISKOUTR                                               550
         AG_PHI1 = 0.                                                     550
         AG_PHI2 = 90.                                                    550
         AG_DZ = VOLREADDZ/2.                                             550
         AG_TITLE = 'TUBS'                                                550
         AG_EXNAME = 'SHAPE'                                              550
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            550
         CALL AXSHAPE                                                     550
C    *                                                                    550
         ZZFGZ= - VOLREADDZ/2. + FRAMEDZ/1.5                              550
C    Create FQAB :                                                        552
C    Call To Local Procedure FQAB                                         552
         ASSIGN 6180 TO BLOCK_FQAB                                        552
         GO TO  6170                                                      552
6180     CONTINUE                                                         552
C    *                                                                    552
C    Geant POSITION : FQAB ===> X=FRAMEWIDTH/2.+FRAMEXYOFF Y=(DISKOUTR+D  553
C    ISKINRT)/2. Z=ZZFGZ <===                                             553
         CALL AGSRESET                                                    553
         AG_X = FRAMEWIDTH/2.+FRAMEXYOFF                                  553
         AG_Y = (DISKOUTR+DISKINRT)/2.                                    553
         AG_Z = ZZFGZ                                                     553
         AG_TITLE = 'FQAB'                                                553
         AG_EXNAME = 'POSITION'                                           553
         AG_PARLIST = 'X_Y_Z'                                             553
         CALL AXPOSITION                                                  553
C    *                                                                    553
C    Create FQBB :                                                        554
C    Call To Local Procedure FQBB                                         554
         ASSIGN 6200 TO BLOCK_FQBB                                        554
         GO TO  6190                                                      554
6200     CONTINUE                                                         554
C    *                                                                    554
C    Geant POSITION : FQBB ===> Z=ZZFGZ <===                              555
         CALL AGSRESET                                                    555
         AG_Z = ZZFGZ                                                     555
         AG_TITLE = 'FQBB'                                                555
         AG_EXNAME = 'POSITION'                                           555
         AG_PARLIST = 'Z'                                                 555
         CALL AXPOSITION                                                  555
C    *                                                                    555
C    Create FQCB :                                                        556
C    Call To Local Procedure FQCB                                         556
         ASSIGN 6220 TO BLOCK_FQCB                                        556
         GO TO  6210                                                      556
6220     CONTINUE                                                         556
C    *                                                                    556
C    Geant POSITION : FQCB ===> X=(DISKOUTR+DISKINRT)/2. Y=FRAMEWIDTH/2.  557
C    +FRAMEXYOFF Z=ZZFGZ <===                                             557
         CALL AGSRESET                                                    557
         AG_X = (DISKOUTR+DISKINRT)/2.                                    557
         AG_Y = FRAMEWIDTH/2.+FRAMEXYOFF                                  557
         AG_Z = ZZFGZ                                                     557
         AG_TITLE = 'FQCB'                                                557
         AG_EXNAME = 'POSITION'                                           557
         AG_PARLIST = 'X_Y_Z'                                             557
         CALL AXPOSITION                                                  557
C    *                                                                    557
C    Create FGQD :                                                        558
C    Call To Local Procedure FGQD                                         558
         ASSIGN 6230 TO BLOCK_FGQD                                        558
         GO TO  5480                                                      558
6230     CONTINUE                                                         558
C    *                                                                    558
C    Geant POSITION : FGQD ===> Z=ZZFGZ <===                              559
         CALL AGSRESET                                                    559
         AG_Z = ZZFGZ                                                     559
         AG_TITLE = 'FGQD'                                                559
         AG_EXNAME = 'POSITION'                                           559
         AG_PARLIST = 'Z'                                                 559
         CALL AXPOSITION                                                  559
C    *                                                                    559
C    Create FGQE :                                                        560
C    Call To Local Procedure FGQE                                         560
         ASSIGN 6240 TO BLOCK_FGQE                                        560
         GO TO  5500                                                      560
6240     CONTINUE                                                         560
C    *                                                                    560
C    Geant POSITION : FGQE ===> X=(FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPERPA  563
C    NG*DEGRAD) Y=(FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD) Z=ZZF  563
C    GZ ALPHAZ=FLATPERPANG-90. <===                                       563
         CALL AGSRESET                                                    563
         AG_X = (FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPERPANG*DEGRAD)          563
         AG_Y = (FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD)          563
         AG_Z = ZZFGZ                                                     563
         AG_ALPHAN=AG_ALPHAN+1                                            563
         AG_ALPHAK(AG_ALPHAN)=3                                           563
         AG_ALPHAL(AG_ALPHAN)=FLATPERPANG-90.                             563
         AG_TITLE = 'FGQE'                                                563
         AG_EXNAME = 'POSITION'                                           563
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      563
         CALL AXPOSITION                                                  563
C    *                                                                    563
C    Create FGQF :                                                        564
C    Call To Local Procedure FGQF                                         564
         ASSIGN 6250 TO BLOCK_FGQF                                        564
         GO TO  5520                                                      564
6250     CONTINUE                                                         564
C    *                                                                    564
C    Geant POSITION : FGQF ===> Z=ZZFGZ <===                              565
         CALL AGSRESET                                                    565
         AG_Z = ZZFGZ                                                     565
         AG_TITLE = 'FGQF'                                                565
         AG_EXNAME = 'POSITION'                                           565
         AG_PARLIST = 'Z'                                                 565
         CALL AXPOSITION                                                  565
C    *                                                                    565
5371  CALL AGSPOPB                                                        566
      GO TO BLOCK_FVRB, (5370 ,5380)                                      566
C End of Local Procedure                                                  566
C ----------------------------------------------------------------------  567
C ------------ FGVE                                                       567
C Begin Local Procedure FGVE                                              568
C FGVE QUAD ELECTRONICS VOLUME                                            568
5290  CONTINUE                                                            568
         AG_TITLE='FGVE'                                                  568
         AG_MODULE=MODULE                                                 568
         AG_MARK='V00'                                                    568
         CALL AGSPUSH                                                     568
         AG_MODULE=MODULE                                                 568
C    *                                                                    568
C    Geant ATTRIBUTE : FGVE ===> SEEN=0 COLO=7 <===                       569
         CALL AGSRESET                                                    569
         AG_SEEN = 0                                                      569
         AG_COLO = 7                                                      569
         AG_TITLE = 'FGVE'                                                569
         AG_EXNAME = 'ATTRIBUTE'                                          569
         AG_PARLIST = 'SEEN_COLO'                                         569
         CALL AXATTRIBUTE                                                 569
C    *                                                                    569
C    Geant MATERIAL : AIR ===> inherited arguments used <===              569
         CALL AGSRESET                                                    570
         AG_TITLE = 'AIR'                                                 570
         AG_EXNAME = 'MATERIAL'                                           570
         AG_PARLIST = 'NONE'                                              570
         CALL AXMATERIAL                                                  570
C    *                                                                    570
C    Geant SHAPE : TUBS ===> RMIN=DISKINR RMAX=DISKOUTR PHI1=0., PHI2=90  570
C    . DZ=VOLELECDZ/2. <===                                               570
         CALL AGSRESET                                                    572
         AG_RMIN = DISKINR                                                572
         AG_RMAX = DISKOUTR                                               572
         AG_PHI1 = 0.                                                     572
         AG_PHI2 = 90.                                                    572
         AG_DZ = VOLELECDZ/2.                                             572
         AG_TITLE = 'TUBS'                                                572
         AG_EXNAME = 'SHAPE'                                              572
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            572
         CALL AXSHAPE                                                     572
C    *                                                                    572
C    Create FGWA :                                                        573
C    Call To Local Procedure FGWA                                         573
         ASSIGN 6270 TO BLOCK_FGWA                                        573
         GO TO  6260                                                      573
6270     CONTINUE                                                         573
C    *                                                                    573
C    Geant POSITION : FGWA ===> X=DISKINR+FRAMEWIDTH*0.7 Y=ELECADY/2. <=  574
C    ==                                                                   574
         CALL AGSRESET                                                    574
         AG_X = DISKINR+FRAMEWIDTH*0.7                                    574
         AG_Y = ELECADY/2.                                                574
         AG_TITLE = 'FGWA'                                                574
         AG_EXNAME = 'POSITION'                                           574
         AG_PARLIST = 'X_Y'                                               574
         CALL AXPOSITION                                                  574
C    *                                                                    574
C    Create FGWA :                                                        575
C    Call To Local Procedure FGWA                                         575
         ASSIGN 6280 TO BLOCK_FGWA                                        575
         GO TO  6260                                                      575
6280     CONTINUE                                                         575
C    *                                                                    575
C    Geant POSITION : FGWA ===> X=ELECADY/2. Y=DISKINR+FRAMEWIDTH*0.7 AL  576
C    PHAZ=90. <===                                                        576
         CALL AGSRESET                                                    576
         AG_X = ELECADY/2.                                                576
         AG_Y = DISKINR+FRAMEWIDTH*0.7                                    576
         AG_ALPHAN=AG_ALPHAN+1                                            576
         AG_ALPHAK(AG_ALPHAN)=3                                           576
         AG_ALPHAL(AG_ALPHAN)=90.                                         576
         AG_TITLE = 'FGWA'                                                576
         AG_EXNAME = 'POSITION'                                           576
         AG_PARLIST = 'X_Y_ALPHAZ'                                        576
         CALL AXPOSITION                                                  576
C    *                                                                    576
C    Create FGWB :                                                        577
C    Call To Local Procedure FGWB                                         577
         ASSIGN 6300 TO BLOCK_FGWB                                        577
         GO TO  6290                                                      577
6300     CONTINUE                                                         577
C    *                                                                    577
C    Geant POSITION : FGWB ===> X=(DISKOUTR+DISKINR)/2. Y=FRAMEXYOFF+ELE  578
C    CWIDTH/2. <===                                                       578
         CALL AGSRESET                                                    578
         AG_X = (DISKOUTR+DISKINR)/2.                                     578
         AG_Y = FRAMEXYOFF+ELECWIDTH/2.                                   578
         AG_TITLE = 'FGWB'                                                578
         AG_EXNAME = 'POSITION'                                           578
         AG_PARLIST = 'X_Y'                                               578
         CALL AXPOSITION                                                  578
C    *                                                                    578
C    Create FGWB :                                                        579
C    Call To Local Procedure FGWB                                         579
         ASSIGN 6310 TO BLOCK_FGWB                                        579
         GO TO  6290                                                      579
6310     CONTINUE                                                         579
C    *                                                                    579
C    Geant POSITION : FGWB ===> X=FRAMEXYOFF+ELECWIDTH/2. Y=(DISKOUTR+DI  580
C    SKINR)/2. ALPHAZ=90. <===                                            580
         CALL AGSRESET                                                    580
         AG_X = FRAMEXYOFF+ELECWIDTH/2.                                   580
         AG_Y = (DISKOUTR+DISKINR)/2.                                     580
         AG_ALPHAN=AG_ALPHAN+1                                            580
         AG_ALPHAK(AG_ALPHAN)=3                                           580
         AG_ALPHAL(AG_ALPHAN)=90.                                         580
         AG_TITLE = 'FGWB'                                                580
         AG_EXNAME = 'POSITION'                                           580
         AG_PARLIST = 'X_Y_ALPHAZ'                                        580
         CALL AXPOSITION                                                  580
C    *                                                                    580
C    Create FGWC :                                                        581
C    Call To Local Procedure FGWC                                         581
         ASSIGN 6330 TO BLOCK_FGWC                                        581
         GO TO  6320                                                      581
6330     CONTINUE                                                         581
C    *                                                                    581
C    Geant POSITION : FGWC ===> X=DISKOUTR-FRAMEWIDTH*0.7 Y=ELECCDY/2. <  582
C    ===                                                                  582
         CALL AGSRESET                                                    582
         AG_X = DISKOUTR-FRAMEWIDTH*0.7                                   582
         AG_Y = ELECCDY/2.                                                582
         AG_TITLE = 'FGWC'                                                582
         AG_EXNAME = 'POSITION'                                           582
         AG_PARLIST = 'X_Y'                                               582
         CALL AXPOSITION                                                  582
C    *                                                                    582
C    Create FGWC :                                                        583
C    Call To Local Procedure FGWC                                         583
         ASSIGN 6340 TO BLOCK_FGWC                                        583
         GO TO  6320                                                      583
6340     CONTINUE                                                         583
C    *                                                                    583
C    Geant POSITION : FGWC ===> X=ELECCDY/2. Y=DISKOUTR-FRAMEWIDTH*0.7 A  584
C    LPHAZ=90. <===                                                       584
         CALL AGSRESET                                                    584
         AG_X = ELECCDY/2.                                                584
         AG_Y = DISKOUTR-FRAMEWIDTH*0.7                                   584
         AG_ALPHAN=AG_ALPHAN+1                                            584
         AG_ALPHAK(AG_ALPHAN)=3                                           584
         AG_ALPHAL(AG_ALPHAN)=90.                                         584
         AG_TITLE = 'FGWC'                                                584
         AG_EXNAME = 'POSITION'                                           584
         AG_PARLIST = 'X_Y_ALPHAZ'                                        584
         CALL AXPOSITION                                                  584
C    *                                                                    584
C    Create FGWD :                                                        585
C    Call To Local Procedure FGWD                                         585
         ASSIGN 6360 TO BLOCK_FGWD                                        585
         GO TO  6350                                                      585
6360     CONTINUE                                                         585
C    *                                                                    585
C    Geant POSITION : FGWD ===> X=(DISKOUTR-FRAMEWIDTH/2.)*COS(ELECDANG*  587
C    DEGRAD) Y=(DISKOUTR-FRAMEWIDTH/2.)*SIN(ELECDANG*DEGRAD) ALPHAX=90.   587
C    <===                                                                 587
         CALL AGSRESET                                                    587
         AG_X = (DISKOUTR-FRAMEWIDTH/2.)*COS(ELECDANG*DEGRAD)             587
         AG_Y = (DISKOUTR-FRAMEWIDTH/2.)*SIN(ELECDANG*DEGRAD)             587
         AG_ALPHAN=AG_ALPHAN+1                                            587
         AG_ALPHAK(AG_ALPHAN)=1                                           587
         AG_ALPHAL(AG_ALPHAN)=90.                                         587
         AG_TITLE = 'FGWD'                                                587
         AG_EXNAME = 'POSITION'                                           587
         AG_PARLIST = 'X_Y_ALPHAX'                                        587
         CALL AXPOSITION                                                  587
C    *                                                                    587
C    Create FGWD :                                                        588
C    Call To Local Procedure FGWD                                         588
         ASSIGN 6370 TO BLOCK_FGWD                                        588
         GO TO  6350                                                      588
6370     CONTINUE                                                         588
C    *                                                                    588
C    Geant POSITION : FGWD ===> X=(DISKOUTR-FRAMEWIDTH/2.)*SIN(ELECDANG*  590
C    DEGRAD) Y=(DISKOUTR-FRAMEWIDTH/2.)*COS(ELECDANG*DEGRAD) ALPHAX=90.   590
C    ALPHAZ=90. <===                                                      590
         CALL AGSRESET                                                    590
         AG_X = (DISKOUTR-FRAMEWIDTH/2.)*SIN(ELECDANG*DEGRAD)             590
         AG_Y = (DISKOUTR-FRAMEWIDTH/2.)*COS(ELECDANG*DEGRAD)             590
         AG_ALPHAN=AG_ALPHAN+1                                            590
         AG_ALPHAK(AG_ALPHAN)=1                                           590
         AG_ALPHAL(AG_ALPHAN)=90.                                         590
         AG_ALPHAN=AG_ALPHAN+1                                            590
         AG_ALPHAK(AG_ALPHAN)=3                                           590
         AG_ALPHAL(AG_ALPHAN)=90.                                         590
         AG_TITLE = 'FGWD'                                                590
         AG_EXNAME = 'POSITION'                                           590
         AG_PARLIST = 'X_Y_ALPHAX_ALPHAZ'                                 590
         CALL AXPOSITION                                                  590
C    *                                                                    590
C    Create FGWE :                                                        591
C    Call To Local Procedure FGWE                                         591
         ASSIGN 6390 TO BLOCK_FGWE                                        591
         GO TO  6380                                                      591
6390     CONTINUE                                                         591
C    *                                                                    591
C    Geant POSITION : FGWE ===> X=(FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPERPA  593
C    NG*DEGRAD) Y=(FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD) ALPHA  593
C    Z=FLATPERPANG-90. <===                                               593
         CALL AGSRESET                                                    593
         AG_X = (FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPERPANG*DEGRAD)          593
         AG_Y = (FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD)          593
         AG_ALPHAN=AG_ALPHAN+1                                            593
         AG_ALPHAK(AG_ALPHAN)=3                                           593
         AG_ALPHAL(AG_ALPHAN)=FLATPERPANG-90.                             593
         AG_TITLE = 'FGWE'                                                593
         AG_EXNAME = 'POSITION'                                           593
         AG_PARLIST = 'X_Y_ALPHAZ'                                        593
         CALL AXPOSITION                                                  593
C    *                                                                    593
5291  CALL AGSPOPB                                                        594
      GO TO BLOCK_FGVE, (5290 ,5300)                                      594
C End of Local Procedure                                                  594
C ----------------------------------------------------------------------  595
C ------------ FVEB                                                       595
C Begin Local Procedure FVEB                                              596
C FVEB QUAD ELECTRONICS VOLUME                                            596
5390  CONTINUE                                                            596
         AG_TITLE='FVEB'                                                  596
         AG_MODULE=MODULE                                                 596
         AG_MARK='V00'                                                    596
         CALL AGSPUSH                                                     596
         AG_MODULE=MODULE                                                 596
C    *                                                                    596
C    Geant ATTRIBUTE : FVEB ===> SEEN=0 COLO=7 <===                       597
         CALL AGSRESET                                                    597
         AG_SEEN = 0                                                      597
         AG_COLO = 7                                                      597
         AG_TITLE = 'FVEB'                                                597
         AG_EXNAME = 'ATTRIBUTE'                                          597
         AG_PARLIST = 'SEEN_COLO'                                         597
         CALL AXATTRIBUTE                                                 597
C    *                                                                    597
C    Geant MATERIAL : AIR ===> inherited arguments used <===              597
         CALL AGSRESET                                                    598
         AG_TITLE = 'AIR'                                                 598
         AG_EXNAME = 'MATERIAL'                                           598
         AG_PARLIST = 'NONE'                                              598
         CALL AXMATERIAL                                                  598
C    *                                                                    598
C    Geant SHAPE : TUBS ===> RMIN=DISKINRT RMAX=DISKOUTR PHI1=0., PHI2=9  598
C    0. DZ=VOLELECDZ/2. <===                                              598
         CALL AGSRESET                                                    600
         AG_RMIN = DISKINRT                                               600
         AG_RMAX = DISKOUTR                                               600
         AG_PHI1 = 0.                                                     600
         AG_PHI2 = 90.                                                    600
         AG_DZ = VOLELECDZ/2.                                             600
         AG_TITLE = 'TUBS'                                                600
         AG_EXNAME = 'SHAPE'                                              600
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            600
         CALL AXSHAPE                                                     600
C    *                                                                    600
C    Create FGWA :                                                        601
C    Call To Local Procedure FGWA                                         601
         ASSIGN 6400 TO BLOCK_FGWA                                        601
         GO TO  6260                                                      601
6400     CONTINUE                                                         601
C    *                                                                    601
C    Geant POSITION : FGWA ===> X=DISKINRT+FRAMEWIDTH*0.7 Y=ELECADY/2. <  602
C    ===                                                                  602
         CALL AGSRESET                                                    602
         AG_X = DISKINRT+FRAMEWIDTH*0.7                                   602
         AG_Y = ELECADY/2.                                                602
         AG_TITLE = 'FGWA'                                                602
         AG_EXNAME = 'POSITION'                                           602
         AG_PARLIST = 'X_Y'                                               602
         CALL AXPOSITION                                                  602
C    *                                                                    602
C    Create FGWA :                                                        603
C    Call To Local Procedure FGWA                                         603
         ASSIGN 6410 TO BLOCK_FGWA                                        603
         GO TO  6260                                                      603
6410     CONTINUE                                                         603
C    *                                                                    603
C    Geant POSITION : FGWA ===> X=ELECADY/2. Y=DISKINRT+FRAMEWIDTH*0.7 A  604
C    LPHAZ=90. <===                                                       604
         CALL AGSRESET                                                    604
         AG_X = ELECADY/2.                                                604
         AG_Y = DISKINRT+FRAMEWIDTH*0.7                                   604
         AG_ALPHAN=AG_ALPHAN+1                                            604
         AG_ALPHAK(AG_ALPHAN)=3                                           604
         AG_ALPHAL(AG_ALPHAN)=90.                                         604
         AG_TITLE = 'FGWA'                                                604
         AG_EXNAME = 'POSITION'                                           604
         AG_PARLIST = 'X_Y_ALPHAZ'                                        604
         CALL AXPOSITION                                                  604
C    *                                                                    604
C    Create FGWB :                                                        605
C    Call To Local Procedure FGWB                                         605
         ASSIGN 6420 TO BLOCK_FGWB                                        605
         GO TO  6290                                                      605
6420     CONTINUE                                                         605
C    *                                                                    605
C    Geant POSITION : FGWB ===> X=(DISKOUTR+DISKINRT)/2. Y=FRAMEXYOFF+EL  606
C    ECWIDTH/2. <===                                                      606
         CALL AGSRESET                                                    606
         AG_X = (DISKOUTR+DISKINRT)/2.                                    606
         AG_Y = FRAMEXYOFF+ELECWIDTH/2.                                   606
         AG_TITLE = 'FGWB'                                                606
         AG_EXNAME = 'POSITION'                                           606
         AG_PARLIST = 'X_Y'                                               606
         CALL AXPOSITION                                                  606
C    *                                                                    606
C    Create FGWB :                                                        607
C    Call To Local Procedure FGWB                                         607
         ASSIGN 6430 TO BLOCK_FGWB                                        607
         GO TO  6290                                                      607
6430     CONTINUE                                                         607
C    *                                                                    607
C    Geant POSITION : FGWB ===> X=FRAMEXYOFF+ELECWIDTH/2. Y=(DISKOUTR+DI  608
C    SKINRT)/2. ALPHAZ=90. <===                                           608
         CALL AGSRESET                                                    608
         AG_X = FRAMEXYOFF+ELECWIDTH/2.                                   608
         AG_Y = (DISKOUTR+DISKINRT)/2.                                    608
         AG_ALPHAN=AG_ALPHAN+1                                            608
         AG_ALPHAK(AG_ALPHAN)=3                                           608
         AG_ALPHAL(AG_ALPHAN)=90.                                         608
         AG_TITLE = 'FGWB'                                                608
         AG_EXNAME = 'POSITION'                                           608
         AG_PARLIST = 'X_Y_ALPHAZ'                                        608
         CALL AXPOSITION                                                  608
C    *                                                                    608
C    Create FGWC :                                                        609
C    Call To Local Procedure FGWC                                         609
         ASSIGN 6440 TO BLOCK_FGWC                                        609
         GO TO  6320                                                      609
6440     CONTINUE                                                         609
C    *                                                                    609
C    Geant POSITION : FGWC ===> X=DISKOUTR-FRAMEWIDTH*0.7 Y=ELECCDY/2. <  610
C    ===                                                                  610
         CALL AGSRESET                                                    610
         AG_X = DISKOUTR-FRAMEWIDTH*0.7                                   610
         AG_Y = ELECCDY/2.                                                610
         AG_TITLE = 'FGWC'                                                610
         AG_EXNAME = 'POSITION'                                           610
         AG_PARLIST = 'X_Y'                                               610
         CALL AXPOSITION                                                  610
C    *                                                                    610
C    Create FGWC :                                                        611
C    Call To Local Procedure FGWC                                         611
         ASSIGN 6450 TO BLOCK_FGWC                                        611
         GO TO  6320                                                      611
6450     CONTINUE                                                         611
C    *                                                                    611
C    Geant POSITION : FGWC ===> X=ELECCDY/2. Y=DISKOUTR-FRAMEWIDTH*0.7 A  612
C    LPHAZ=90. <===                                                       612
         CALL AGSRESET                                                    612
         AG_X = ELECCDY/2.                                                612
         AG_Y = DISKOUTR-FRAMEWIDTH*0.7                                   612
         AG_ALPHAN=AG_ALPHAN+1                                            612
         AG_ALPHAK(AG_ALPHAN)=3                                           612
         AG_ALPHAL(AG_ALPHAN)=90.                                         612
         AG_TITLE = 'FGWC'                                                612
         AG_EXNAME = 'POSITION'                                           612
         AG_PARLIST = 'X_Y_ALPHAZ'                                        612
         CALL AXPOSITION                                                  612
C    *                                                                    612
C    Create FGWD :                                                        613
C    Call To Local Procedure FGWD                                         613
         ASSIGN 6460 TO BLOCK_FGWD                                        613
         GO TO  6350                                                      613
6460     CONTINUE                                                         613
C    *                                                                    613
C    Geant POSITION : FGWD ===> X=(DISKOUTR-FRAMEWIDTH/2.)*COS(ELECDANG*  615
C    DEGRAD) Y=(DISKOUTR-FRAMEWIDTH/2.)*SIN(ELECDANG*DEGRAD) ALPHAX=90.   615
C    <===                                                                 615
         CALL AGSRESET                                                    615
         AG_X = (DISKOUTR-FRAMEWIDTH/2.)*COS(ELECDANG*DEGRAD)             615
         AG_Y = (DISKOUTR-FRAMEWIDTH/2.)*SIN(ELECDANG*DEGRAD)             615
         AG_ALPHAN=AG_ALPHAN+1                                            615
         AG_ALPHAK(AG_ALPHAN)=1                                           615
         AG_ALPHAL(AG_ALPHAN)=90.                                         615
         AG_TITLE = 'FGWD'                                                615
         AG_EXNAME = 'POSITION'                                           615
         AG_PARLIST = 'X_Y_ALPHAX'                                        615
         CALL AXPOSITION                                                  615
C    *                                                                    615
C    Create FGWD :                                                        616
C    Call To Local Procedure FGWD                                         616
         ASSIGN 6470 TO BLOCK_FGWD                                        616
         GO TO  6350                                                      616
6470     CONTINUE                                                         616
C    *                                                                    616
C    Geant POSITION : FGWD ===> X=(DISKOUTR-FRAMEWIDTH/2.)*SIN(ELECDANG*  618
C    DEGRAD) Y=(DISKOUTR-FRAMEWIDTH/2.)*COS(ELECDANG*DEGRAD) ALPHAX=90.   618
C    ALPHAZ=90. <===                                                      618
         CALL AGSRESET                                                    618
         AG_X = (DISKOUTR-FRAMEWIDTH/2.)*SIN(ELECDANG*DEGRAD)             618
         AG_Y = (DISKOUTR-FRAMEWIDTH/2.)*COS(ELECDANG*DEGRAD)             618
         AG_ALPHAN=AG_ALPHAN+1                                            618
         AG_ALPHAK(AG_ALPHAN)=1                                           618
         AG_ALPHAL(AG_ALPHAN)=90.                                         618
         AG_ALPHAN=AG_ALPHAN+1                                            618
         AG_ALPHAK(AG_ALPHAN)=3                                           618
         AG_ALPHAL(AG_ALPHAN)=90.                                         618
         AG_TITLE = 'FGWD'                                                618
         AG_EXNAME = 'POSITION'                                           618
         AG_PARLIST = 'X_Y_ALPHAX_ALPHAZ'                                 618
         CALL AXPOSITION                                                  618
C    *                                                                    618
C    Create FWEB :                                                        619
C    Call To Local Procedure FWEB                                         619
         ASSIGN 6490 TO BLOCK_FWEB                                        619
         GO TO  6480                                                      619
6490     CONTINUE                                                         619
C    *                                                                    619
C    Geant POSITION : FWEB ===> X=(FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPERPA  621
C    NG*DEGRAD) Y=(FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD) ALPHA  621
C    Z=FLATPERPANG-90. <===                                               621
         CALL AGSRESET                                                    621
         AG_X = (FLATOUTR-FRAMEWIDTH/2.)*COS(FLATPERPANG*DEGRAD)          621
         AG_Y = (FLATOUTR-FRAMEWIDTH/2.)*SIN(FLATPERPANG*DEGRAD)          621
         AG_ALPHAN=AG_ALPHAN+1                                            621
         AG_ALPHAK(AG_ALPHAN)=3                                           621
         AG_ALPHAL(AG_ALPHAN)=FLATPERPANG-90.                             621
         AG_TITLE = 'FWEB'                                                621
         AG_EXNAME = 'POSITION'                                           621
         AG_PARLIST = 'X_Y_ALPHAZ'                                        621
         CALL AXPOSITION                                                  621
C    *                                                                    621
5391  CALL AGSPOPB                                                        622
      GO TO BLOCK_FVEB, (5390 ,5400)                                      622
C End of Local Procedure                                                  622
C ----------------------------------------------------------------------  623
C ------------ FGQA                                                       623
C Begin Local Procedure FGQA                                              624
C FGQA A-SPACER FRAME                                                     624
5420  CONTINUE                                                            624
         AG_TITLE='FGQA'                                                  624
         AG_MODULE=MODULE                                                 624
         AG_MARK='V00'                                                    624
         CALL AGSPUSH                                                     624
         AG_MODULE=MODULE                                                 624
C    *                                                                    624
C    Geant ATTRIBUTE : FGQA ===> SEEN=1 COLO=4 <===                       625
         CALL AGSRESET                                                    625
         AG_SEEN = 1                                                      625
         AG_COLO = 4                                                      625
         AG_TITLE = 'FGQA'                                                625
         AG_EXNAME = 'ATTRIBUTE'                                          625
         AG_PARLIST = 'SEEN_COLO'                                         625
         CALL AXATTRIBUTE                                                 625
C    *                                                                    625
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              625
         CALL AGSRESET                                                    626
         AG_TITLE = 'FR4'                                                 626
         AG_EXNAME = 'MATERIAL'                                           626
         AG_PARLIST = 'NONE'                                              626
         CALL AXMATERIAL                                                  626
C    *                                                                    626
C    Geant SHAPE : BOX ===> DX=FRAMEWIDTH/2. DY=(DISKOUTR-DISKINR-FRAMEW  626
C    IDTH)/2., DZ=FRAMEDZ/2. <===                                         626
         CALL AGSRESET                                                    628
         AG_DX = FRAMEWIDTH/2.                                            628
         AG_DY = (DISKOUTR-DISKINR-FRAMEWIDTH)/2.                         628
         AG_DZ = FRAMEDZ/2.                                               628
         AG_TITLE = 'BOX'                                                 628
         AG_EXNAME = 'SHAPE'                                              628
         AG_PARLIST = 'DX_DY_DZ'                                          628
         CALL AXSHAPE                                                     628
C    *                                                                    628
5421  CALL AGSPOPB                                                        629
      GO TO BLOCK_FGQA, (5420 ,5430,5550,5620,5770,5880,5990,6110)        629
C End of Local Procedure                                                  629
C ----------------------------------------------------------------------  630
C ------------ FQAB                                                       630
C Begin Local Procedure FQAB                                              631
C FQAB A-SPACER FRAME                                                     631
6170  CONTINUE                                                            631
         AG_TITLE='FQAB'                                                  631
         AG_MODULE=MODULE                                                 631
         AG_MARK='V00'                                                    631
         CALL AGSPUSH                                                     631
         AG_MODULE=MODULE                                                 631
C    *                                                                    631
C    Geant ATTRIBUTE : FQAB ===> SEEN=1 COLO=4 <===                       632
         CALL AGSRESET                                                    632
         AG_SEEN = 1                                                      632
         AG_COLO = 4                                                      632
         AG_TITLE = 'FQAB'                                                632
         AG_EXNAME = 'ATTRIBUTE'                                          632
         AG_PARLIST = 'SEEN_COLO'                                         632
         CALL AXATTRIBUTE                                                 632
C    *                                                                    632
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              632
         CALL AGSRESET                                                    633
         AG_TITLE = 'FR4'                                                 633
         AG_EXNAME = 'MATERIAL'                                           633
         AG_PARLIST = 'NONE'                                              633
         CALL AXMATERIAL                                                  633
C    *                                                                    633
C    Geant SHAPE : BOX ===> DX=FRAMEWIDTH/2. DY=(DISKOUTR-DISKINRT-FRAME  633
C    WIDTH)/2., DZ=FRAMEDZ/2. <===                                        633
         CALL AGSRESET                                                    635
         AG_DX = FRAMEWIDTH/2.                                            635
         AG_DY = (DISKOUTR-DISKINRT-FRAMEWIDTH)/2.                        635
         AG_DZ = FRAMEDZ/2.                                               635
         AG_TITLE = 'BOX'                                                 635
         AG_EXNAME = 'SHAPE'                                              635
         AG_PARLIST = 'DX_DY_DZ'                                          635
         CALL AXSHAPE                                                     635
C    *                                                                    635
6171  CALL AGSPOPB                                                        636
      GO TO BLOCK_FQAB, (6170 ,6180)                                      636
C End of Local Procedure                                                  636
C ----------------------------------------------------------------------  637
C ------------ FGQB                                                       637
C Begin Local Procedure FGQB                                              638
C FGQB B-SPACER FRAME, ARC                                                638
5440  CONTINUE                                                            638
         AG_TITLE='FGQB'                                                  638
         AG_MODULE=MODULE                                                 638
         AG_MARK='V00'                                                    638
         CALL AGSPUSH                                                     638
         AG_MODULE=MODULE                                                 638
C    *                                                                    638
C    Geant ATTRIBUTE : FGQB ===> SEEN=1 COLO=4 <===                       639
         CALL AGSRESET                                                    639
         AG_SEEN = 1                                                      639
         AG_COLO = 4                                                      639
         AG_TITLE = 'FGQB'                                                639
         AG_EXNAME = 'ATTRIBUTE'                                          639
         AG_PARLIST = 'SEEN_COLO'                                         639
         CALL AXATTRIBUTE                                                 639
C    *                                                                    639
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              639
         CALL AGSRESET                                                    640
         AG_TITLE = 'FR4'                                                 640
         AG_EXNAME = 'MATERIAL'                                           640
         AG_PARLIST = 'NONE'                                              640
         CALL AXMATERIAL                                                  640
C    *                                                                    640
         XXFGQ=FRAMEXYOFF/DISKINR/DEGRAD                                  640
C    Geant SHAPE : TUBS ===> RMIN=DISKINR RMAX=DISKINR+FRAMEWIDTH, PHI1=  643
C    XXFGQ PHI2=90.-XXFGQ DZ=FRAMEDZ/2. <===                              643
         CALL AGSRESET                                                    643
         AG_RMIN = DISKINR                                                643
         AG_RMAX = DISKINR+FRAMEWIDTH                                     643
         AG_PHI1 = XXFGQ                                                  643
         AG_PHI2 = 90.-XXFGQ                                              643
         AG_DZ = FRAMEDZ/2.                                               643
         AG_TITLE = 'TUBS'                                                643
         AG_EXNAME = 'SHAPE'                                              643
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            643
         CALL AXSHAPE                                                     643
C    *                                                                    643
5441  CALL AGSPOPB                                                        644
      GO TO BLOCK_FGQB, (5440 ,5450,5560,5630,5780,5890,6000,6120)        644
C End of Local Procedure                                                  644
C ----------------------------------------------------------------------  645
C ------------ FQBB                                                       645
C Begin Local Procedure FQBB                                              646
C FQBB B-SPACER FRAME, ARC                                                646
6190  CONTINUE                                                            646
         AG_TITLE='FQBB'                                                  646
         AG_MODULE=MODULE                                                 646
         AG_MARK='V00'                                                    646
         CALL AGSPUSH                                                     646
         AG_MODULE=MODULE                                                 646
C    *                                                                    646
C    Geant ATTRIBUTE : FQBB ===> SEEN=1 COLO=4 <===                       647
         CALL AGSRESET                                                    647
         AG_SEEN = 1                                                      647
         AG_COLO = 4                                                      647
         AG_TITLE = 'FQBB'                                                647
         AG_EXNAME = 'ATTRIBUTE'                                          647
         AG_PARLIST = 'SEEN_COLO'                                         647
         CALL AXATTRIBUTE                                                 647
C    *                                                                    647
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              647
         CALL AGSRESET                                                    648
         AG_TITLE = 'FR4'                                                 648
         AG_EXNAME = 'MATERIAL'                                           648
         AG_PARLIST = 'NONE'                                              648
         CALL AXMATERIAL                                                  648
C    *                                                                    648
         XXFGQ=FRAMEXYOFF/DISKINRT/DEGRAD                                 648
C    Geant SHAPE : TUBS ===> RMIN=DISKINRT RMAX=DISKINRT+FRAMEWIDTH, PHI  651
C    1=XXFGQ PHI2=90.-XXFGQ DZ=FRAMEDZ/2. <===                            651
         CALL AGSRESET                                                    651
         AG_RMIN = DISKINRT                                               651
         AG_RMAX = DISKINRT+FRAMEWIDTH                                    651
         AG_PHI1 = XXFGQ                                                  651
         AG_PHI2 = 90.-XXFGQ                                              651
         AG_DZ = FRAMEDZ/2.                                               651
         AG_TITLE = 'TUBS'                                                651
         AG_EXNAME = 'SHAPE'                                              651
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            651
         CALL AXSHAPE                                                     651
C    *                                                                    651
6191  CALL AGSPOPB                                                        652
      GO TO BLOCK_FQBB, (6190 ,6200)                                      652
C End of Local Procedure                                                  652
C ----------------------------------------------------------------------  653
C ------------ FGQC                                                       653
C Begin Local Procedure FGQC                                              654
C FGQC C-SPACER FRAME, BAR                                                654
5460  CONTINUE                                                            654
         AG_TITLE='FGQC'                                                  654
         AG_MODULE=MODULE                                                 654
         AG_MARK='V00'                                                    654
         CALL AGSPUSH                                                     654
         AG_MODULE=MODULE                                                 654
C    *                                                                    654
C    Geant ATTRIBUTE : FGQC ===> SEEN=1 COLO=4 <===                       655
         CALL AGSRESET                                                    655
         AG_SEEN = 1                                                      655
         AG_COLO = 4                                                      655
         AG_TITLE = 'FGQC'                                                655
         AG_EXNAME = 'ATTRIBUTE'                                          655
         AG_PARLIST = 'SEEN_COLO'                                         655
         CALL AXATTRIBUTE                                                 655
C    *                                                                    655
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              655
         CALL AGSRESET                                                    656
         AG_TITLE = 'FR4'                                                 656
         AG_EXNAME = 'MATERIAL'                                           656
         AG_PARLIST = 'NONE'                                              656
         CALL AXMATERIAL                                                  656
C    *                                                                    656
C    Geant SHAPE : BOX ===> DX=(DISKOUTR-DISKINR-FRAMEWIDTH)/2., DY=FRAM  656
C    EWIDTH/2. DZ=FRAMEDZ/2. <===                                         656
         CALL AGSRESET                                                    658
         AG_DX = (DISKOUTR-DISKINR-FRAMEWIDTH)/2.                         658
         AG_DY = FRAMEWIDTH/2.                                            658
         AG_DZ = FRAMEDZ/2.                                               658
         AG_TITLE = 'BOX'                                                 658
         AG_EXNAME = 'SHAPE'                                              658
         AG_PARLIST = 'DX_DY_DZ'                                          658
         CALL AXSHAPE                                                     658
C    *                                                                    658
5461  CALL AGSPOPB                                                        659
      GO TO BLOCK_FGQC, (5460 ,5470,5570,5640,5790,5900,6010,6130)        659
C End of Local Procedure                                                  659
C ----------------------------------------------------------------------  660
C ------------ FQCB                                                       660
C Begin Local Procedure FQCB                                              661
C FQCB C-SPACER FRAME, BAR                                                661
6210  CONTINUE                                                            661
         AG_TITLE='FQCB'                                                  661
         AG_MODULE=MODULE                                                 661
         AG_MARK='V00'                                                    661
         CALL AGSPUSH                                                     661
         AG_MODULE=MODULE                                                 661
C    *                                                                    661
C    Geant ATTRIBUTE : FQCB ===> SEEN=1 COLO=4 <===                       662
         CALL AGSRESET                                                    662
         AG_SEEN = 1                                                      662
         AG_COLO = 4                                                      662
         AG_TITLE = 'FQCB'                                                662
         AG_EXNAME = 'ATTRIBUTE'                                          662
         AG_PARLIST = 'SEEN_COLO'                                         662
         CALL AXATTRIBUTE                                                 662
C    *                                                                    662
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              662
         CALL AGSRESET                                                    663
         AG_TITLE = 'FR4'                                                 663
         AG_EXNAME = 'MATERIAL'                                           663
         AG_PARLIST = 'NONE'                                              663
         CALL AXMATERIAL                                                  663
C    *                                                                    663
C    Geant SHAPE : BOX ===> DX=(DISKOUTR-DISKINRT-FRAMEWIDTH)/2., DY=FRA  663
C    MEWIDTH/2. DZ=FRAMEDZ/2. <===                                        663
         CALL AGSRESET                                                    665
         AG_DX = (DISKOUTR-DISKINRT-FRAMEWIDTH)/2.                        665
         AG_DY = FRAMEWIDTH/2.                                            665
         AG_DZ = FRAMEDZ/2.                                               665
         AG_TITLE = 'BOX'                                                 665
         AG_EXNAME = 'SHAPE'                                              665
         AG_PARLIST = 'DX_DY_DZ'                                          665
         CALL AXSHAPE                                                     665
C    *                                                                    665
6211  CALL AGSPOPB                                                        666
      GO TO BLOCK_FQCB, (6210 ,6220)                                      666
C End of Local Procedure                                                  666
C ----------------------------------------------------------------------  667
C ------------ FGQD                                                       667
C Begin Local Procedure FGQD                                              668
C FGQD D-SPACER FRAME, ARC                                                668
5480  CONTINUE                                                            668
         AG_TITLE='FGQD'                                                  668
         AG_MODULE=MODULE                                                 668
         AG_MARK='V00'                                                    668
         CALL AGSPUSH                                                     668
         AG_MODULE=MODULE                                                 668
C    *                                                                    668
C    Geant ATTRIBUTE : FGQD ===> SEEN=1 COLO=4 <===                       669
         CALL AGSRESET                                                    669
         AG_SEEN = 1                                                      669
         AG_COLO = 4                                                      669
         AG_TITLE = 'FGQD'                                                669
         AG_EXNAME = 'ATTRIBUTE'                                          669
         AG_PARLIST = 'SEEN_COLO'                                         669
         CALL AXATTRIBUTE                                                 669
C    *                                                                    669
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              669
         CALL AGSRESET                                                    670
         AG_TITLE = 'FR4'                                                 670
         AG_EXNAME = 'MATERIAL'                                           670
         AG_PARLIST = 'NONE'                                              670
         CALL AXMATERIAL                                                  670
C    *                                                                    670
         XXFGQ=FRAMEXYOFF/DISKOUTR/DEGRAD                                 670
C    Geant SHAPE : TUBS ===> RMIN=DISKOUTR-FRAMEWIDTH RMAX=DISKOUTR, PHI  673
C    1=XXFGQ PHI2=FLATANG1 DZ=FRAMEDZ/2. <===                             673
         CALL AGSRESET                                                    673
         AG_RMIN = DISKOUTR-FRAMEWIDTH                                    673
         AG_RMAX = DISKOUTR                                               673
         AG_PHI1 = XXFGQ                                                  673
         AG_PHI2 = FLATANG1                                               673
         AG_DZ = FRAMEDZ/2.                                               673
         AG_TITLE = 'TUBS'                                                673
         AG_EXNAME = 'SHAPE'                                              673
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            673
         CALL AXSHAPE                                                     673
C    *                                                                    673
5481  CALL AGSPOPB                                                        674
      GO TO BLOCK_FGQD, (5480 ,5490,5580,5650,5800,5910,6020,6140,6230)   674
C End of Local Procedure                                                  674
C ----------------------------------------------------------------------  675
C ------------ FGQE                                                       675
C Begin Local Procedure FGQE                                              676
C FGQE E-SPACER FRAME, FLAT                                               676
5500  CONTINUE                                                            676
         AG_TITLE='FGQE'                                                  676
         AG_MODULE=MODULE                                                 676
         AG_MARK='V00'                                                    676
         CALL AGSPUSH                                                     676
         AG_MODULE=MODULE                                                 676
C    *                                                                    676
C    Geant ATTRIBUTE : FGQE ===> SEEN=1 COLO=4 <===                       677
         CALL AGSRESET                                                    677
         AG_SEEN = 1                                                      677
         AG_COLO = 4                                                      677
         AG_TITLE = 'FGQE'                                                677
         AG_EXNAME = 'ATTRIBUTE'                                          677
         AG_PARLIST = 'SEEN_COLO'                                         677
         CALL AXATTRIBUTE                                                 677
C    *                                                                    677
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              677
         CALL AGSRESET                                                    678
         AG_TITLE = 'FR4'                                                 678
         AG_EXNAME = 'MATERIAL'                                           678
         AG_PARLIST = 'NONE'                                              678
         CALL AXMATERIAL                                                  678
C    *                                                                    678
C    Geant SHAPE : BOX ===> DX=DISKOUTR*SIN((FLATANG2-FLATANG1)*DEGRAD/2  678
C    .), DY=FRAMEWIDTH/2. DZ=FRAMEDZ/2. <===                              678
         CALL AGSRESET                                                    680
         AG_DX = DISKOUTR*SIN((FLATANG2-FLATANG1)*DEGRAD/2.)              680
         AG_DY = FRAMEWIDTH/2.                                            680
         AG_DZ = FRAMEDZ/2.                                               680
         AG_TITLE = 'BOX'                                                 680
         AG_EXNAME = 'SHAPE'                                              680
         AG_PARLIST = 'DX_DY_DZ'                                          680
         CALL AXSHAPE                                                     680
C    *                                                                    680
5501  CALL AGSPOPB                                                        681
      GO TO BLOCK_FGQE, (5500 ,5510,5590,5660,5810,5920,6030,6150,6240)   681
C End of Local Procedure                                                  681
C ----------------------------------------------------------------------  682
C ------------ FGQF                                                       682
C Begin Local Procedure FGQF                                              683
C FGQF F-SPACER FRAME, ARC                                                683
5520  CONTINUE                                                            683
         AG_TITLE='FGQF'                                                  683
         AG_MODULE=MODULE                                                 683
         AG_MARK='V00'                                                    683
         CALL AGSPUSH                                                     683
         AG_MODULE=MODULE                                                 683
C    *                                                                    683
C    Geant ATTRIBUTE : FGQF ===> SEEN=1 COLO=4 <===                       684
         CALL AGSRESET                                                    684
         AG_SEEN = 1                                                      684
         AG_COLO = 4                                                      684
         AG_TITLE = 'FGQF'                                                684
         AG_EXNAME = 'ATTRIBUTE'                                          684
         AG_PARLIST = 'SEEN_COLO'                                         684
         CALL AXATTRIBUTE                                                 684
C    *                                                                    684
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              684
         CALL AGSRESET                                                    685
         AG_TITLE = 'FR4'                                                 685
         AG_EXNAME = 'MATERIAL'                                           685
         AG_PARLIST = 'NONE'                                              685
         CALL AXMATERIAL                                                  685
C    *                                                                    685
         XXFGQ=FRAMEXYOFF/DISKOUTR/DEGRAD                                 685
C    Geant SHAPE : TUBS ===> RMIN=DISKOUTR-FRAMEWIDTH RMAX=DISKOUTR, PHI  688
C    1=FLATANG2 PHI2=90.-XXFGQ DZ=FRAMEDZ/2. <===                         688
         CALL AGSRESET                                                    688
         AG_RMIN = DISKOUTR-FRAMEWIDTH                                    688
         AG_RMAX = DISKOUTR                                               688
         AG_PHI1 = FLATANG2                                               688
         AG_PHI2 = 90.-XXFGQ                                              688
         AG_DZ = FRAMEDZ/2.                                               688
         AG_TITLE = 'TUBS'                                                688
         AG_EXNAME = 'SHAPE'                                              688
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            688
         CALL AXSHAPE                                                     688
C    *                                                                    688
5521  CALL AGSPOPB                                                        689
      GO TO BLOCK_FGQF, (5520 ,5530,5600,5670,5820,5930,6040,6160,6250)   689
C End of Local Procedure                                                  689
C ----------------------------------------------------------------------  690
C ------------ FGXA                                                       690
C Begin Local Procedure FGXA                                              691
C FGXA A-GRID, ARC                                                        691
5680  CONTINUE                                                            691
         AG_TITLE='FGXA'                                                  691
         AG_MODULE=MODULE                                                 691
         AG_MARK='V00'                                                    691
         CALL AGSPUSH                                                     691
         AG_MODULE=MODULE                                                 691
C    *                                                                    691
C    Geant ATTRIBUTE : FGXB ===> SEEN=1 COLO=3 <===                       692
         CALL AGSRESET                                                    692
         AG_SEEN = 1                                                      692
         AG_COLO = 3                                                      692
         AG_TITLE = 'FGXB'                                                692
         AG_EXNAME = 'ATTRIBUTE'                                          692
         AG_PARLIST = 'SEEN_COLO'                                         692
         CALL AXATTRIBUTE                                                 692
C    *                                                                    692
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              692
         CALL AGSRESET                                                    693
         AG_TITLE = 'FR4'                                                 693
         AG_EXNAME = 'MATERIAL'                                           693
         AG_PARLIST = 'NONE'                                              693
         CALL AXMATERIAL                                                  693
C    *                                                                    693
         XXFGX=FRAMEXYOFF/GRIDARCBIGR/DEGRAD                              693
C    Geant SHAPE : TUBS ===> RMIN=GRIDARCBIGR RMAX=GRIDARCBIGR+GRIDWIDTH  696
C    , PHI1=XXFGX PHI2=90.-XXFGX DZ=FRAMEDZ/2. <===                       696
         CALL AGSRESET                                                    696
         AG_RMIN = GRIDARCBIGR                                            696
         AG_RMAX = GRIDARCBIGR+GRIDWIDTH                                  696
         AG_PHI1 = XXFGX                                                  696
         AG_PHI2 = 90.-XXFGX                                              696
         AG_DZ = FRAMEDZ/2.                                               696
         AG_TITLE = 'TUBS'                                                696
         AG_EXNAME = 'SHAPE'                                              696
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            696
         CALL AXSHAPE                                                     696
C    *                                                                    696
5681  CALL AGSPOPB                                                        697
      GO TO BLOCK_FGXA, (5680 ,5690,5830,5940,6050)                       697
C End of Local Procedure                                                  697
C ----------------------------------------------------------------------  698
C ------------ FGXB                                                       698
C Begin Local Procedure FGXB                                              699
C FGXB B-GRID, ARC                                                        699
5700  CONTINUE                                                            699
         AG_TITLE='FGXB'                                                  699
         AG_MODULE=MODULE                                                 699
         AG_MARK='V00'                                                    699
         CALL AGSPUSH                                                     699
         AG_MODULE=MODULE                                                 699
C    *                                                                    699
C    Geant ATTRIBUTE : FGXB ===> SEEN=1 COLO=3 <===                       700
         CALL AGSRESET                                                    700
         AG_SEEN = 1                                                      700
         AG_COLO = 3                                                      700
         AG_TITLE = 'FGXB'                                                700
         AG_EXNAME = 'ATTRIBUTE'                                          700
         AG_PARLIST = 'SEEN_COLO'                                         700
         CALL AXATTRIBUTE                                                 700
C    *                                                                    700
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              700
         CALL AGSRESET                                                    701
         AG_TITLE = 'FR4'                                                 701
         AG_EXNAME = 'MATERIAL'                                           701
         AG_PARLIST = 'NONE'                                              701
         CALL AXMATERIAL                                                  701
C    *                                                                    701
         XXFGX=FRAMEXYOFF/GRIDARCSMALLR/DEGRAD                            701
C    Geant SHAPE : TUBS ===> RMIN=GRIDARCSMALLR RMAX=GRIDARCSMALLR+GRIDW  705
C    IDTH, PHI1=XXFGX PHI2=90.-XXFGX, DZ=FRAMEDZ/2. <===                  705
         CALL AGSRESET                                                    705
         AG_RMIN = GRIDARCSMALLR                                          705
         AG_RMAX = GRIDARCSMALLR+GRIDWIDTH                                705
         AG_PHI1 = XXFGX                                                  705
         AG_PHI2 = 90.-XXFGX                                              705
         AG_DZ = FRAMEDZ/2.                                               705
         AG_TITLE = 'TUBS'                                                705
         AG_EXNAME = 'SHAPE'                                              705
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            705
         CALL AXSHAPE                                                     705
C    *                                                                    705
5701  CALL AGSPOPB                                                        706
      GO TO BLOCK_FGXB, (5700 ,5710,5840,5950,6060)                       706
C End of Local Procedure                                                  706
C ----------------------------------------------------------------------  707
C ------------ FGXC                                                       707
C Begin Local Procedure FGXC                                              708
C FGXC C-GRID, RAY                                                        708
5720  CONTINUE                                                            708
         AG_TITLE='FGXC'                                                  708
         AG_MODULE=MODULE                                                 708
         AG_MARK='V00'                                                    708
         CALL AGSPUSH                                                     708
         AG_MODULE=MODULE                                                 708
C    *                                                                    708
C    Geant ATTRIBUTE : FGXC ===> SEEN=1 COLO=3 <===                       709
         CALL AGSRESET                                                    709
         AG_SEEN = 1                                                      709
         AG_COLO = 3                                                      709
         AG_TITLE = 'FGXC'                                                709
         AG_EXNAME = 'ATTRIBUTE'                                          709
         AG_PARLIST = 'SEEN_COLO'                                         709
         CALL AXATTRIBUTE                                                 709
C    *                                                                    709
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              709
         CALL AGSRESET                                                    710
         AG_TITLE = 'FR4'                                                 710
         AG_EXNAME = 'MATERIAL'                                           710
         AG_PARLIST = 'NONE'                                              710
         CALL AXMATERIAL                                                  710
C    *                                                                    710
C    Geant SHAPE : BOX ===> DX=(DISKOUTR-DISKINR-FRAMEWIDTH)/2., DY=GRID  710
C    WIDTH/2. DZ=FRAMEDZ/2. <===                                          710
         CALL AGSRESET                                                    712
         AG_DX = (DISKOUTR-DISKINR-FRAMEWIDTH)/2.                         712
         AG_DY = GRIDWIDTH/2.                                             712
         AG_DZ = FRAMEDZ/2.                                               712
         AG_TITLE = 'BOX'                                                 712
         AG_EXNAME = 'SHAPE'                                              712
         AG_PARLIST = 'DX_DY_DZ'                                          712
         CALL AXSHAPE                                                     712
C    *                                                                    712
5721  CALL AGSPOPB                                                        713
      GO TO BLOCK_FGXC, (5720 ,5730,5850,5960)                            713
C End of Local Procedure                                                  713
C ----------------------------------------------------------------------  714
C ------------ FXCB                                                       714
C Begin Local Procedure FXCB                                              715
C FXCB C-GRID, RAY                                                        715
6070  CONTINUE                                                            715
         AG_TITLE='FXCB'                                                  715
         AG_MODULE=MODULE                                                 715
         AG_MARK='V00'                                                    715
         CALL AGSPUSH                                                     715
         AG_MODULE=MODULE                                                 715
C    *                                                                    715
C    Geant ATTRIBUTE : FXCB ===> SEEN=1 COLO=3 <===                       716
         CALL AGSRESET                                                    716
         AG_SEEN = 1                                                      716
         AG_COLO = 3                                                      716
         AG_TITLE = 'FXCB'                                                716
         AG_EXNAME = 'ATTRIBUTE'                                          716
         AG_PARLIST = 'SEEN_COLO'                                         716
         CALL AXATTRIBUTE                                                 716
C    *                                                                    716
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              716
         CALL AGSRESET                                                    717
         AG_TITLE = 'FR4'                                                 717
         AG_EXNAME = 'MATERIAL'                                           717
         AG_PARLIST = 'NONE'                                              717
         CALL AXMATERIAL                                                  717
C    *                                                                    717
C    Geant SHAPE : BOX ===> DX=(DISKOUTR-DISKINRT-FRAMEWIDTH)/2., DY=GRI  717
C    DWIDTH/2. DZ=FRAMEDZ/2. <===                                         717
         CALL AGSRESET                                                    719
         AG_DX = (DISKOUTR-DISKINRT-FRAMEWIDTH)/2.                        719
         AG_DY = GRIDWIDTH/2.                                             719
         AG_DZ = FRAMEDZ/2.                                               719
         AG_TITLE = 'BOX'                                                 719
         AG_EXNAME = 'SHAPE'                                              719
         AG_PARLIST = 'DX_DY_DZ'                                          719
         CALL AXSHAPE                                                     719
C    *                                                                    719
6071  CALL AGSPOPB                                                        720
      GO TO BLOCK_FXCB, (6070 ,6080)                                      720
C End of Local Procedure                                                  720
C ----------------------------------------------------------------------  721
C ------------ FGXD                                                       721
C Begin Local Procedure FGXD                                              722
C FGXD D-GRID, RAY                                                        722
5740  CONTINUE                                                            722
         AG_TITLE='FGXD'                                                  722
         AG_MODULE=MODULE                                                 722
         AG_MARK='V00'                                                    722
         CALL AGSPUSH                                                     722
         AG_MODULE=MODULE                                                 722
C    *                                                                    722
C    Geant ATTRIBUTE : FGXD ===> SEEN=1 COLO=3 <===                       723
         CALL AGSRESET                                                    723
         AG_SEEN = 1                                                      723
         AG_COLO = 3                                                      723
         AG_TITLE = 'FGXD'                                                723
         AG_EXNAME = 'ATTRIBUTE'                                          723
         AG_PARLIST = 'SEEN_COLO'                                         723
         CALL AXATTRIBUTE                                                 723
C    *                                                                    723
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              723
         CALL AGSRESET                                                    724
         AG_TITLE = 'FR4'                                                 724
         AG_EXNAME = 'MATERIAL'                                           724
         AG_PARLIST = 'NONE'                                              724
         CALL AXMATERIAL                                                  724
C    *                                                                    724
C    Geant SHAPE : BOX ===> DX=(FLATOUTR-DISKINR-FRAMEWIDTH)/2., DY=GRID  724
C    WIDTH/2. DZ=FRAMEDZ/2. <===                                          724
         CALL AGSRESET                                                    726
         AG_DX = (FLATOUTR-DISKINR-FRAMEWIDTH)/2.                         726
         AG_DY = GRIDWIDTH/2.                                             726
         AG_DZ = FRAMEDZ/2.                                               726
         AG_TITLE = 'BOX'                                                 726
         AG_EXNAME = 'SHAPE'                                              726
         AG_PARLIST = 'DX_DY_DZ'                                          726
         CALL AXSHAPE                                                     726
C    *                                                                    726
5741  CALL AGSPOPB                                                        727
      GO TO BLOCK_FGXD, (5740 ,5750,5860,5970)                            727
C End of Local Procedure                                                  727
C ----------------------------------------------------------------------  728
C ------------ FXDB                                                       728
C Begin Local Procedure FXDB                                              729
C FXDB D-GRID, RAY                                                        729
6090  CONTINUE                                                            729
         AG_TITLE='FXDB'                                                  729
         AG_MODULE=MODULE                                                 729
         AG_MARK='V00'                                                    729
         CALL AGSPUSH                                                     729
         AG_MODULE=MODULE                                                 729
C    *                                                                    729
C    Geant ATTRIBUTE : FXDB ===> SEEN=1 COLO=3 <===                       730
         CALL AGSRESET                                                    730
         AG_SEEN = 1                                                      730
         AG_COLO = 3                                                      730
         AG_TITLE = 'FXDB'                                                730
         AG_EXNAME = 'ATTRIBUTE'                                          730
         AG_PARLIST = 'SEEN_COLO'                                         730
         CALL AXATTRIBUTE                                                 730
C    *                                                                    730
C    Geant MATERIAL : FR4 ===> inherited arguments used <===              730
         CALL AGSRESET                                                    731
         AG_TITLE = 'FR4'                                                 731
         AG_EXNAME = 'MATERIAL'                                           731
         AG_PARLIST = 'NONE'                                              731
         CALL AXMATERIAL                                                  731
C    *                                                                    731
C    Geant SHAPE : BOX ===> DX=(FLATOUTR-DISKINRT-FRAMEWIDTH)/2., DY=GRI  731
C    DWIDTH/2. DZ=FRAMEDZ/2. <===                                         731
         CALL AGSRESET                                                    733
         AG_DX = (FLATOUTR-DISKINRT-FRAMEWIDTH)/2.                        733
         AG_DY = GRIDWIDTH/2.                                             733
         AG_DZ = FRAMEDZ/2.                                               733
         AG_TITLE = 'BOX'                                                 733
         AG_EXNAME = 'SHAPE'                                              733
         AG_PARLIST = 'DX_DY_DZ'                                          733
         CALL AXSHAPE                                                     733
C    *                                                                    733
6091  CALL AGSPOPB                                                        734
      GO TO BLOCK_FXDB, (6090 ,6100)                                      734
C End of Local Procedure                                                  734
C ----------------------------------------------------------------------  735
C ------------ FGWA                                                       735
C Begin Local Procedure FGWA                                              736
C FGWA A-ELEC, BAR                                                        736
6260  CONTINUE                                                            736
         AG_TITLE='FGWA'                                                  736
         AG_MODULE=MODULE                                                 736
         AG_MARK='V00'                                                    736
         CALL AGSPUSH                                                     736
         AG_MODULE=MODULE                                                 736
C    *                                                                    736
C    Geant ATTRIBUTE : FGWA ===> SEEN=1 COLO=1 <===                       737
         CALL AGSRESET                                                    737
         AG_SEEN = 1                                                      737
         AG_COLO = 1                                                      737
         AG_TITLE = 'FGWA'                                                737
         AG_EXNAME = 'ATTRIBUTE'                                          737
         AG_PARLIST = 'SEEN_COLO'                                         737
         CALL AXATTRIBUTE                                                 737
C    *                                                                    737
C    Geant MATERIAL : TERMMIX ===> inherited arguments used <===          737
         CALL AGSRESET                                                    738
         AG_TITLE = 'TERMMIX'                                             738
         AG_EXNAME = 'MATERIAL'                                           738
         AG_PARLIST = 'NONE'                                              738
         CALL AXMATERIAL                                                  738
C    *                                                                    738
C    Geant SHAPE : BOX ===> DX=0.3/2. DY=ELECADY/2. DZ=2.7/2. <===        738
         CALL AGSRESET                                                    739
         AG_DX = 0.3/2.                                                   739
         AG_DY = ELECADY/2.                                               739
         AG_DZ = 2.7/2.                                                   739
         AG_TITLE = 'BOX'                                                 739
         AG_EXNAME = 'SHAPE'                                              739
         AG_PARLIST = 'DX_DY_DZ'                                          739
         CALL AXSHAPE                                                     739
C    *                                                                    739
6261  CALL AGSPOPB                                                        740
      GO TO BLOCK_FGWA, (6260 ,6270,6280,6400,6410)                       740
C End of Local Procedure                                                  740
C ----------------------------------------------------------------------  741
C ------------ FGWB                                                       741
C Begin Local Procedure FGWB                                              742
C FGWB APV BOARD                                                          742
6290  CONTINUE                                                            742
         AG_TITLE='FGWB'                                                  742
         AG_MODULE=MODULE                                                 742
         AG_MARK='V00'                                                    742
         CALL AGSPUSH                                                     742
         AG_MODULE=MODULE                                                 742
C    *                                                                    742
C    Geant ATTRIBUTE : FGWB ===> SEEN=1 COLO=1 <===                       743
         CALL AGSRESET                                                    743
         AG_SEEN = 1                                                      743
         AG_COLO = 1                                                      743
         AG_TITLE = 'FGWB'                                                743
         AG_EXNAME = 'ATTRIBUTE'                                          743
         AG_PARLIST = 'SEEN_COLO'                                         743
         CALL AXATTRIBUTE                                                 743
C    *                                                                    743
C    Geant MATERIAL : APVMIX ===> inherited arguments used <===           743
         CALL AGSRESET                                                    744
         AG_TITLE = 'APVMIX'                                              744
         AG_EXNAME = 'MATERIAL'                                           744
         AG_PARLIST = 'NONE'                                              744
         CALL AXMATERIAL                                                  744
C    *                                                                    744
C    Geant SHAPE : BOX ===> DX=25.92/2. DY=ELECWIDTH/2. DZ=4.04/2. <===   744
         CALL AGSRESET                                                    745
         AG_DX = 25.92/2.                                                 745
         AG_DY = ELECWIDTH/2.                                             745
         AG_DZ = 4.04/2.                                                  745
         AG_TITLE = 'BOX'                                                 745
         AG_EXNAME = 'SHAPE'                                              745
         AG_PARLIST = 'DX_DY_DZ'                                          745
         CALL AXSHAPE                                                     745
C    *                                                                    745
6291  CALL AGSPOPB                                                        746
      GO TO BLOCK_FGWB, (6290 ,6300,6310,6420,6430)                       746
C End of Local Procedure                                                  746
C ----------------------------------------------------------------------  747
C ------------ FGWC                                                       747
C Begin Local Procedure FGWC                                              748
C FGWC INTERCONNECT BOARD WIDER                                           748
6320  CONTINUE                                                            748
         AG_TITLE='FGWC'                                                  748
         AG_MODULE=MODULE                                                 748
         AG_MARK='V00'                                                    748
         CALL AGSPUSH                                                     748
         AG_MODULE=MODULE                                                 748
C    *                                                                    748
C    Geant ATTRIBUTE : FGWC ===> SEEN=1 COLO=1 <===                       749
         CALL AGSRESET                                                    749
         AG_SEEN = 1                                                      749
         AG_COLO = 1                                                      749
         AG_TITLE = 'FGWC'                                                749
         AG_EXNAME = 'ATTRIBUTE'                                          749
         AG_PARLIST = 'SEEN_COLO'                                         749
         CALL AXATTRIBUTE                                                 749
C    *                                                                    749
C    Geant MATERIAL : CONMIX ===> inherited arguments used <===           749
         CALL AGSRESET                                                    750
         AG_TITLE = 'CONMIX'                                              750
         AG_EXNAME = 'MATERIAL'                                           750
         AG_PARLIST = 'NONE'                                              750
         CALL AXMATERIAL                                                  750
C    *                                                                    750
C    Geant SHAPE : BOX ===> DX=0.3/2. DY=ELECCDY/2. DZ=3.15/2. <===       750
         CALL AGSRESET                                                    751
         AG_DX = 0.3/2.                                                   751
         AG_DY = ELECCDY/2.                                               751
         AG_DZ = 3.15/2.                                                  751
         AG_TITLE = 'BOX'                                                 751
         AG_EXNAME = 'SHAPE'                                              751
         AG_PARLIST = 'DX_DY_DZ'                                          751
         CALL AXSHAPE                                                     751
C    *                                                                    751
6321  CALL AGSPOPB                                                        752
      GO TO BLOCK_FGWC, (6320 ,6330,6340,6440,6450)                       752
C End of Local Procedure                                                  752
C ----------------------------------------------------------------------  753
C ------------ FGWD                                                       753
C Begin Local Procedure FGWD                                              754
C FGWD GAS FEED CONNECTION                                                754
6350  CONTINUE                                                            754
         AG_TITLE='FGWD'                                                  754
         AG_MODULE=MODULE                                                 754
         AG_MARK='V00'                                                    754
         CALL AGSPUSH                                                     754
         AG_MODULE=MODULE                                                 754
C    *                                                                    754
C    Geant ATTRIBUTE : FGWD ===> SEEN=1 COLO=1 <===                       755
         CALL AGSRESET                                                    755
         AG_SEEN = 1                                                      755
         AG_COLO = 1                                                      755
         AG_TITLE = 'FGWD'                                                755
         AG_EXNAME = 'ATTRIBUTE'                                          755
         AG_PARLIST = 'SEEN_COLO'                                         755
         CALL AXATTRIBUTE                                                 755
C    *                                                                    755
C    Geant MATERIAL : BRASS ===> inherited arguments used <===            755
         CALL AGSRESET                                                    756
         AG_TITLE = 'BRASS'                                               756
         AG_EXNAME = 'MATERIAL'                                           756
         AG_PARLIST = 'NONE'                                              756
         CALL AXMATERIAL                                                  756
C    *                                                                    756
C    Geant SHAPE : TUBE ===> RMIN=0.2 RMAX=0.5 DZ=2.1/2. <===             756
         CALL AGSRESET                                                    757
         AG_RMIN = 0.2                                                    757
         AG_RMAX = 0.5                                                    757
         AG_DZ = 2.1/2.                                                   757
         AG_TITLE = 'TUBE'                                                757
         AG_EXNAME = 'SHAPE'                                              757
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      757
         CALL AXSHAPE                                                     757
C    *                                                                    757
6351  CALL AGSPOPB                                                        758
      GO TO BLOCK_FGWD, (6350 ,6360,6370,6460,6470)                       758
C End of Local Procedure                                                  758
C ----------------------------------------------------------------------  759
C ------------ FGWE                                                       759
C Begin Local Procedure FGWE                                              760
C FGWE HV BOARD                                                           760
6380  CONTINUE                                                            760
         AG_TITLE='FGWE'                                                  760
         AG_MODULE=MODULE                                                 760
         AG_MARK='V00'                                                    760
         CALL AGSPUSH                                                     760
         AG_MODULE=MODULE                                                 760
C    *                                                                    760
C    Geant ATTRIBUTE : FGWE ===> SEEN=1 COLO=1 <===                       761
         CALL AGSRESET                                                    761
         AG_SEEN = 1                                                      761
         AG_COLO = 1                                                      761
         AG_TITLE = 'FGWE'                                                761
         AG_EXNAME = 'ATTRIBUTE'                                          761
         AG_PARLIST = 'SEEN_COLO'                                         761
         CALL AXATTRIBUTE                                                 761
C    *                                                                    761
C    Geant MATERIAL : HVMIX ===> inherited arguments used <===            761
         CALL AGSRESET                                                    762
         AG_TITLE = 'HVMIX'                                               762
         AG_EXNAME = 'MATERIAL'                                           762
         AG_PARLIST = 'NONE'                                              762
         CALL AXMATERIAL                                                  762
C    *                                                                    762
C    Geant SHAPE : BOX ===> DX=ELECELEN/2. DY=ELECWIDTH/2., DZ=VOLELECDZ  762
C    /2. <===                                                             762
         CALL AGSRESET                                                    764
         AG_DX = ELECELEN/2.                                              764
         AG_DY = ELECWIDTH/2.                                             764
         AG_DZ = VOLELECDZ/2.                                             764
         AG_TITLE = 'BOX'                                                 764
         AG_EXNAME = 'SHAPE'                                              764
         AG_PARLIST = 'DX_DY_DZ'                                          764
         CALL AXSHAPE                                                     764
C    *                                                                    764
6381  CALL AGSPOPB                                                        765
      GO TO BLOCK_FGWE, (6380 ,6390)                                      765
C End of Local Procedure                                                  765
C ----------------------------------------------------------------------  766
C ------------ FWEB                                                       766
C Begin Local Procedure FWEB                                              767
C FWEB HV BOARD                                                           767
6480  CONTINUE                                                            767
         AG_TITLE='FWEB'                                                  767
         AG_MODULE=MODULE                                                 767
         AG_MARK='V00'                                                    767
         CALL AGSPUSH                                                     767
         AG_MODULE=MODULE                                                 767
C    *                                                                    767
C    Geant ATTRIBUTE : FWEB ===> SEEN=1 COLO=1 <===                       768
         CALL AGSRESET                                                    768
         AG_SEEN = 1                                                      768
         AG_COLO = 1                                                      768
         AG_TITLE = 'FWEB'                                                768
         AG_EXNAME = 'ATTRIBUTE'                                          768
         AG_PARLIST = 'SEEN_COLO'                                         768
         CALL AXATTRIBUTE                                                 768
C    *                                                                    768
C    Geant MATERIAL : HVMIX ===> inherited arguments used <===            768
         CALL AGSRESET                                                    769
         AG_TITLE = 'HVMIX'                                               769
         AG_EXNAME = 'MATERIAL'                                           769
         AG_PARLIST = 'NONE'                                              769
         CALL AXMATERIAL                                                  769
C    *                                                                    769
C    Geant SHAPE : BOX ===> DX=ELECELENT/2. DY=ELECWIDTH/2., DZ=VOLELECD  769
C    Z/2. <===                                                            769
         CALL AGSRESET                                                    771
         AG_DX = ELECELENT/2.                                             771
         AG_DY = ELECWIDTH/2.                                             771
         AG_DZ = VOLELECDZ/2.                                             771
         AG_TITLE = 'BOX'                                                 771
         AG_EXNAME = 'SHAPE'                                              771
         AG_PARLIST = 'DX_DY_DZ'                                          771
         CALL AXSHAPE                                                     771
C    *                                                                    771
6481  CALL AGSPOPB                                                        772
      GO TO BLOCK_FWEB, (6480 ,6490)                                      772
C End of Local Procedure                                                  772
      WRITE(LOUT,6500)'===>>  [End AgML Module FGTDGEOV]  <<==='          772
6500  FORMAT(A32,/,/)                                                     772
C MODULE FGTDGEOV                                                         774
      END                                                                 774
