C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE PIXLGEO4                                                 1
C PIXLGEO4 PIXEL DETECTOR GEOMETRY                                        1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'PIXLGEO4'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'PIXEL DETECTOR GEOMETRY'/                            1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C AUTHOR JB                                                               1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'JB'/                                          2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED DEC.20 2011                                                     2
      CHARACTER*40 CREATED /'DEC.20 2011'/                                3
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
      INTEGER BLOCK_PXMO                                                  4
      INTEGER BLOCK_PXLA                                                  30
      INTEGER BLOCK_PXRB                                                  30
      INTEGER BLOCK_PXLB                                                  30
      INTEGER BLOCK_PXIB                                                  30
      INTEGER BLOCK_PXTR                                                  30
      INTEGER BLOCK_PXTM                                                  30
      INTEGER BLOCK_PXTL                                                  30
      INTEGER BLOCK_PXTJ                                                  30
      INTEGER BLOCK_PXCA                                                  30
      INTEGER BLOCK_PXCB                                                  30
      INTEGER BLOCK_PXCC                                                  30
      INTEGER BLOCK_PXCD                                                  30
      INTEGER BLOCK_PXCE                                                  30
      INTEGER BLOCK_PXCF                                                  30
      INTEGER BLOCK_PXCG                                                  30
      INTEGER BLOCK_PXCH                                                  30
      INTEGER BLOCK_PLAC                                                  30
      INTEGER BLOCK_SIFR                                                  30
      INTEGER BLOCK_SIFL                                                  30
      INTEGER BLOCK_GLUA                                                  30
      INTEGER BLOCK_ALCA                                                  30
      INTEGER BLOCK_GLUB                                                  30
      INTEGER BLOCK_CFBK                                                  30
      INTEGER BLOCK_GLUC                                                  30
      INTEGER BLOCK_LADR                                                  30
      REAL *4 HALFLADRIGHT/2.50864/                                       31
      REAL *4 HALFLADBOTTOM/0.73364/                                      33
      REAL *4 HALFLADLEFT/2.38864/                                        35
      REAL *4 HALFLADTHK/0.0135/                                          37
      REAL *4 HALFLADZLEN/19.59/                                          39
      REAL *4 HALFLADTOPR/0.623359/                                       41
      REAL *4 HALFLADTOPM/0.680382/                                       43
      REAL *4 HALFLADTOPL/0.661607/                                       45
      REAL *4 HALFLADMID/0.0384251/                                       47
      REAL *4 ARCTMIN/0.123/                                              49
      REAL *4 ARCTMAX/0.15/                                               51
      REAL *4 ARCTBMIN/0.15/                                              53
      REAL *4 ARCTBMAX/0.177/                                             55
      REAL *4 ARCBMIN/0.173/                                              57
      REAL *4 ARCBMAX/0.2/                                                59
      REAL *4 HALFPIXR/0.96/                                              61
      REAL *4 HALFPIXTHK/0.0025/                                          63
      REAL *4 HALFPIXZ/10/                                                65
      INTEGER *4 NSECTOR /10/                                             67
      REAL *4 MINRADSEC/2.35/                                             69
      REAL *4 MAXRADSEC/9.5/                                              71
      REAL *4 MAXZSEC/21/                                                 73
      REAL *4 MINRVOL/2.3/                                                75
      REAL *4 MAXRVOL/10/                                                 77
      REAL *4 MAXZVOL/26/                                                 79
      REAL *4 HALFMAXZ/20.0/                                              81
      REAL *4 MINPHISEC/88.0/                                             83
      REAL *4 MAXPHISEC/128.0/                                            85
      REAL *4 HALFPIXPRL/0.015/                                           87
      REAL *4 HALFPIXTHKPL/0.0025/                                        89
      REAL *4 HALFPIXPZL/10/                                              91
      REAL *4 HALFPIXPRR/0.169/                                           93
      REAL *4 HALFPIXTHKPR/0.0025/                                        95
      REAL *4 HALFPIXPZR/10/                                              97
      REAL *4 HALFPIXGA/1.144/                                            99
      REAL *4 HALFPIXTHKGA/0.0025/                                        101
      REAL *4 HALFPIXZGA/10/                                              103
      REAL *4 HALFPIXGB/0.9775/                                           105
      REAL *4 HALFPIXTHKGB/0.0025/                                        107
      REAL *4 HALFPIXZGB/15.325/                                          109
      REAL *4 HALFPIXAC/1.2215/                                           111
      REAL *4 HALFPIXTHKAC/0.0085/                                        113
      REAL *4 HALFPIXZAC/15.325/                                          115
      REAL *4 HALFPIXCB/0.9775/                                           117
      REAL *4 HALFPIXTHKCB/0.02/                                          119
      REAL *4 HALFPIXZCB/15.325/                                          121
      REAL *4 HALFPIXGC/0.72/                                             123
      REAL *4 HALFPIXTHKGC/0.005/                                         125
      REAL *4 HALFPIXZGC/15.325/                                          127
      REAL *4 GLOBOFFZ/-5.015/                                            129
      REAL *4 XPOS,YPOS,ZPOS,ANGLE,ANGLEPOS ,SECTOR                       131
      REAL *4 LADBOXX, LADBOXY, LADBOXZ                                   133
      INTEGER *4 DEBUG                                                    135
C -                                                                       137
      REAL PIXL_VERSION                                                   137
      EQUIVALENCE (BANK_PIXL(3),PIXL_VERSION,RE_PIXL_VERSION),            137
     *(NAMES_PIXL(1),NPIXL_VERSION)                                       137
      EQUIVALENCE (COMMEN_PIXL(1),CPIXL_VERSION),(MAP_PIXL(1,1),          137
     *MPIXL_VERSION)                                                      137
      CHARACTER CPIXL_VERSION*40/' '/                                     137
      CHARACTER NPIXL_VERSION*12/'R.VERSION'/                             137
      INTEGER MPIXL_VERSION(2)/1,1/                                       137
      REAL RE_PIXL_VERSION                                                137
C -                                                                       137
      INTEGER PIXL_SECTOR                                                 137
      EQUIVALENCE (BANK_PIXL(4),PIXL_SECTOR,RE_PIXL_SECTOR),              137
     *(NAMES_PIXL(2),NPIXL_SECTOR)                                        137
      EQUIVALENCE (COMMEN_PIXL(2),CPIXL_SECTOR),(MAP_PIXL(1,2),           137
     *MPIXL_SECTOR)                                                       137
      CHARACTER CPIXL_SECTOR*40/' '/                                      137
      CHARACTER NPIXL_SECTOR*12/'I.SECTOR'/                               137
      INTEGER MPIXL_SECTOR(2)/1,1/                                        137
      REAL RE_PIXL_SECTOR                                                 137
C -                                                                       137
      REAL PIXL_PHINUM(10)                                                137
      EQUIVALENCE (BANK_PIXL(5),PIXL_PHINUM,RE_PIXL_PHINUM),              137
     *(NAMES_PIXL(3),NPIXL_PHINUM)                                        137
      EQUIVALENCE (COMMEN_PIXL(3),CPIXL_PHINUM),(MAP_PIXL(1,3),           137
     *MPIXL_PHINUM)                                                       137
      CHARACTER CPIXL_PHINUM*40/' '/                                      137
      CHARACTER NPIXL_PHINUM*12/'R.PHINUM'/                               137
      INTEGER MPIXL_PHINUM(2)/10,1/                                       137
      REAL RE_PIXL_PHINUM                                                 137
C -                                                                       137
      INTEGER LEN_PIXL,LLEN_PIXL,LINK_PIXL                                137
      PARAMETER (LEN_PIXL=14)                                             137
      PARAMETER (LLEN_PIXL=3)                                             137
      INTEGER FLAG_PIXL/0/,MAP_PIXL(2,LLEN_PIXL),IDNUM_PIXL(10)           137
      CHARACTER NAMES_PIXL(LLEN_PIXL)*12,COMMEN_PIXL(LLEN_PIXL)*40        137
      DATA IDNUM_PIXL/10*0/                                               137
      REAL BANK_PIXL(LEN_PIXL)                                            137
      CHARACTER*60 BPATH_PIXL/' '/                                        137
      SAVE FLAG_PIXL,MAP_PIXL,IDNUM_PIXL,NAMES_PIXL,COMMEN_PIXL,          137
     *BPATH_PIXL                                                          137
      DATA LINK_PIXL/0/,BANK_PIXL/LEN_PIXL*0/                             137
C *                                                                       137
C -                                                                       138
      INTEGER PARO_VERSION                                                138
      EQUIVALENCE (BANK_PARO(3),PARO_VERSION,RE_PARO_VERSION),            138
     *(NAMES_PARO(1),NPARO_VERSION)                                       138
      EQUIVALENCE (COMMEN_PARO(1),CPARO_VERSION),(MAP_PARO(1,1),          138
     *MPARO_VERSION)                                                      138
      CHARACTER CPARO_VERSION*40/' '/                                     138
      CHARACTER NPARO_VERSION*12/'I.VERSION'/                             138
      INTEGER MPARO_VERSION(2)/1,1/                                       138
      REAL RE_PARO_VERSION                                                138
C -                                                                       138
      REAL PARO_DX                                                        138
      EQUIVALENCE (BANK_PARO(4),PARO_DX,RE_PARO_DX),(NAMES_PARO(2),       138
     *NPARO_DX)                                                           138
      EQUIVALENCE (COMMEN_PARO(2),CPARO_DX),(MAP_PARO(1,2),MPARO_DX)      138
      CHARACTER CPARO_DX*40/' '/                                          138
      CHARACTER NPARO_DX*12/'R.DX'/                                       138
      INTEGER MPARO_DX(2)/1,1/                                            138
      REAL RE_PARO_DX                                                     138
C -                                                                       138
      REAL PARO_DY                                                        138
      EQUIVALENCE (BANK_PARO(5),PARO_DY,RE_PARO_DY),(NAMES_PARO(3),       138
     *NPARO_DY)                                                           138
      EQUIVALENCE (COMMEN_PARO(3),CPARO_DY),(MAP_PARO(1,3),MPARO_DY)      138
      CHARACTER CPARO_DY*40/' '/                                          138
      CHARACTER NPARO_DY*12/'R.DY'/                                       138
      INTEGER MPARO_DY(2)/1,1/                                            138
      REAL RE_PARO_DY                                                     138
C -                                                                       138
      REAL PARO_DZ                                                        138
      EQUIVALENCE (BANK_PARO(6),PARO_DZ,RE_PARO_DZ),(NAMES_PARO(4),       138
     *NPARO_DZ)                                                           138
      EQUIVALENCE (COMMEN_PARO(4),CPARO_DZ),(MAP_PARO(1,4),MPARO_DZ)      138
      CHARACTER CPARO_DZ*40/' '/                                          138
      CHARACTER NPARO_DZ*12/'R.DZ'/                                       138
      INTEGER MPARO_DZ(2)/1,1/                                            138
      REAL RE_PARO_DZ                                                     138
C -                                                                       138
      REAL PARO_MULT                                                      138
      EQUIVALENCE (BANK_PARO(7),PARO_MULT,RE_PARO_MULT),(NAMES_PARO(5),   138
     *NPARO_MULT)                                                         138
      EQUIVALENCE (COMMEN_PARO(5),CPARO_MULT),(MAP_PARO(1,5),             138
     *MPARO_MULT)                                                         138
      CHARACTER CPARO_MULT*40/' '/                                        138
      CHARACTER NPARO_MULT*12/'R.MULT'/                                   138
      INTEGER MPARO_MULT(2)/1,1/                                          138
      REAL RE_PARO_MULT                                                   138
C -                                                                       138
      INTEGER PARO_SEC(10)                                                138
      EQUIVALENCE (BANK_PARO(8),PARO_SEC,RE_PARO_SEC),(NAMES_PARO(6),     138
     *NPARO_SEC)                                                          138
      EQUIVALENCE (COMMEN_PARO(6),CPARO_SEC),(MAP_PARO(1,6),MPARO_SEC)    138
      CHARACTER CPARO_SEC*40/' '/                                         138
      CHARACTER NPARO_SEC*12/'I.SEC'/                                     138
      INTEGER MPARO_SEC(2)/10,1/                                          138
      REAL RE_PARO_SEC                                                    138
C -                                                                       138
      INTEGER LEN_PARO,LLEN_PARO,LINK_PARO                                138
      PARAMETER (LEN_PARO=17)                                             138
      PARAMETER (LLEN_PARO=6)                                             138
      INTEGER FLAG_PARO/0/,MAP_PARO(2,LLEN_PARO),IDNUM_PARO(10)           138
      CHARACTER NAMES_PARO(LLEN_PARO)*12,COMMEN_PARO(LLEN_PARO)*40        138
      DATA IDNUM_PARO/10*0/                                               138
      REAL BANK_PARO(LEN_PARO)                                            138
      CHARACTER*60 BPATH_PARO/' '/                                        138
      SAVE FLAG_PARO,MAP_PARO,IDNUM_PARO,NAMES_PARO,COMMEN_PARO,          138
     *BPATH_PARO                                                          138
      DATA LINK_PARO/0/,BANK_PARO/LEN_PARO*0/                             138
C *                                                                       138
C -                                                                       139
      INTEGER PARA_VERSION                                                139
      EQUIVALENCE (BANK_PARA(3),PARA_VERSION,RE_PARA_VERSION),            139
     *(NAMES_PARA(1),NPARA_VERSION)                                       139
      EQUIVALENCE (COMMEN_PARA(1),CPARA_VERSION),(MAP_PARA(1,1),          139
     *MPARA_VERSION)                                                      139
      CHARACTER CPARA_VERSION*40/' '/                                     139
      CHARACTER NPARA_VERSION*12/'I.VERSION'/                             139
      INTEGER MPARA_VERSION(2)/1,1/                                       139
      REAL RE_PARA_VERSION                                                139
C -                                                                       139
      REAL PARA_DA                                                        139
      EQUIVALENCE (BANK_PARA(4),PARA_DA,RE_PARA_DA),(NAMES_PARA(2),       139
     *NPARA_DA)                                                           139
      EQUIVALENCE (COMMEN_PARA(2),CPARA_DA),(MAP_PARA(1,2),MPARA_DA)      139
      CHARACTER CPARA_DA*40/' '/                                          139
      CHARACTER NPARA_DA*12/'R.DA'/                                       139
      INTEGER MPARA_DA(2)/1,1/                                            139
      REAL RE_PARA_DA                                                     139
C -                                                                       139
      REAL PARA_DB                                                        139
      EQUIVALENCE (BANK_PARA(5),PARA_DB,RE_PARA_DB),(NAMES_PARA(3),       139
     *NPARA_DB)                                                           139
      EQUIVALENCE (COMMEN_PARA(3),CPARA_DB),(MAP_PARA(1,3),MPARA_DB)      139
      CHARACTER CPARA_DB*40/' '/                                          139
      CHARACTER NPARA_DB*12/'R.DB'/                                       139
      INTEGER MPARA_DB(2)/1,1/                                            139
      REAL RE_PARA_DB                                                     139
C -                                                                       139
      REAL PARA_DG                                                        139
      EQUIVALENCE (BANK_PARA(6),PARA_DG,RE_PARA_DG),(NAMES_PARA(4),       139
     *NPARA_DG)                                                           139
      EQUIVALENCE (COMMEN_PARA(4),CPARA_DG),(MAP_PARA(1,4),MPARA_DG)      139
      CHARACTER CPARA_DG*40/' '/                                          139
      CHARACTER NPARA_DG*12/'R.DG'/                                       139
      INTEGER MPARA_DG(2)/1,1/                                            139
      REAL RE_PARA_DG                                                     139
C -                                                                       139
      REAL PARA_MULT                                                      139
      EQUIVALENCE (BANK_PARA(7),PARA_MULT,RE_PARA_MULT),(NAMES_PARA(5),   139
     *NPARA_MULT)                                                         139
      EQUIVALENCE (COMMEN_PARA(5),CPARA_MULT),(MAP_PARA(1,5),             139
     *MPARA_MULT)                                                         139
      CHARACTER CPARA_MULT*40/' '/                                        139
      CHARACTER NPARA_MULT*12/'R.MULT'/                                   139
      INTEGER MPARA_MULT(2)/1,1/                                          139
      REAL RE_PARA_MULT                                                   139
C -                                                                       139
      INTEGER PARA_SEC(10)                                                139
      EQUIVALENCE (BANK_PARA(8),PARA_SEC,RE_PARA_SEC),(NAMES_PARA(6),     139
     *NPARA_SEC)                                                          139
      EQUIVALENCE (COMMEN_PARA(6),CPARA_SEC),(MAP_PARA(1,6),MPARA_SEC)    139
      CHARACTER CPARA_SEC*40/' '/                                         139
      CHARACTER NPARA_SEC*12/'I.SEC'/                                     139
      INTEGER MPARA_SEC(2)/10,1/                                          139
      REAL RE_PARA_SEC                                                    139
C -                                                                       139
      INTEGER LEN_PARA,LLEN_PARA,LINK_PARA                                139
      PARAMETER (LEN_PARA=17)                                             139
      PARAMETER (LLEN_PARA=6)                                             139
      INTEGER FLAG_PARA/0/,MAP_PARA(2,LLEN_PARA),IDNUM_PARA(10)           139
      CHARACTER NAMES_PARA(LLEN_PARA)*12,COMMEN_PARA(LLEN_PARA)*40        139
      DATA IDNUM_PARA/10*0/                                               139
      REAL BANK_PARA(LEN_PARA)                                            139
      CHARACTER*60 BPATH_PARA/' '/                                        139
      SAVE FLAG_PARA,MAP_PARA,IDNUM_PARA,NAMES_PARA,COMMEN_PARA,          139
     *BPATH_PARA                                                          139
      DATA LINK_PARA/0/,BANK_PARA/LEN_PARA*0/                             139
C *                                                                       139
C *                                                                       140
      CALL AGSBEGM(MODULE,IPRIN)                                          140
      IF (IPRIN.LT.0) RETURN                                              140
C Filling bank PIXL - GENERAL PIXEL DETECTOR PARAMTERS                    140
      BANK_TITLE='GENERAL PIXEL DETECTOR PARAMTERS'                       140
      BPATH_PIXL='PIXL*'                                                  140
      IDNUM_PIXL(1)=0                                                     141
C NONE                                                                    141
      PIXL_VERSION=1.0                                                    141
      CPIXL_VERSION='NONE'                                                141
C NUMBER OF SECTORS                                                       142
      PIXL_SECTOR=10                                                      142
      CPIXL_SECTOR='NUMBER OF SECTORS'                                    142
C PHI VALUES FOR EACH SECTOR, STARTING FROM 1, 2 .. 10                    144
      PIXL_PHINUM(1)=0                                                    144
      PIXL_PHINUM(2)=324                                                  144
      PIXL_PHINUM(3)=288                                                  144
      PIXL_PHINUM(4)=252                                                  144
      PIXL_PHINUM(5)=216                                                  144
      PIXL_PHINUM(6)=180                                                  144
      PIXL_PHINUM(7)=144                                                  144
      PIXL_PHINUM(8)=108                                                  144
      PIXL_PHINUM(9)=72                                                   144
      PIXL_PHINUM(10)=36                                                  144
      CPIXL_PHINUM='PHI VALUES FOR EACH SECTOR, STARTING FROM 1, 2 .. 10  144
     *'                                                                   144
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXL',    145
     *BPATH_PIXL,IDNUM_PIXL,1, MAP_PIXL,NAMES_PIXL,COMMEN_PIXL,           145
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   145
C *                                                                       145
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXL',    145
     *BPATH_PIXL,IDNUM_PIXL,1, MAP_PIXL,NAMES_PIXL,COMMEN_PIXL,           145
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   145
C *                                                                       146
      CALL AGSBEGM(MODULE,IPRIN)                                          146
      IF (IPRIN.LT.0) RETURN                                              146
C Filling bank PIXL - JOINED PROTOTYPE 1-2-10                             146
      BANK_TITLE='JOINED PROTOTYPE 1-2-10'                                146
      BPATH_PIXL='PIXL*'                                                  146
      IDNUM_PIXL(1)=0                                                     147
C NONE                                                                    147
      PIXL_VERSION=2.0                                                    147
      CPIXL_VERSION='NONE'                                                147
C NUMBER OF SECTORS                                                       148
      PIXL_SECTOR=3                                                       148
      CPIXL_SECTOR='NUMBER OF SECTORS'                                    148
C PHI VALUES FOR EACH SECTOR, STARTING FROM 1, 2 .. 10                    149
      PIXL_PHINUM(1)=0                                                    149
      PIXL_PHINUM(2)=324                                                  149
      PIXL_PHINUM(3)=36                                                   149
      CPIXL_PHINUM='PHI VALUES FOR EACH SECTOR, STARTING FROM 1, 2 .. 10  149
     *'                                                                   149
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXL',    150
     *BPATH_PIXL,IDNUM_PIXL,1, MAP_PIXL,NAMES_PIXL,COMMEN_PIXL,           150
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   150
C *                                                                       150
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXL',    150
     *BPATH_PIXL,IDNUM_PIXL,1, MAP_PIXL,NAMES_PIXL,COMMEN_PIXL,           150
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   150
C *                                                                       151
      CALL AGSBEGM(MODULE,IPRIN)                                          151
      IF (IPRIN.LT.0) RETURN                                              151
C Filling bank PIXL - MERCEDES PROTOTYPE 1-5-7                            151
      BANK_TITLE='MERCEDES PROTOTYPE 1-5-7'                               151
      BPATH_PIXL='PIXL*'                                                  151
      IDNUM_PIXL(1)=0                                                     152
C NONE                                                                    152
      PIXL_VERSION=3.0                                                    152
      CPIXL_VERSION='NONE'                                                152
C NUMBER OF SECTORS                                                       153
      PIXL_SECTOR=3                                                       153
      CPIXL_SECTOR='NUMBER OF SECTORS'                                    153
C PHI VALUES FOR EACH SECTOR, STARTING FROM 1, 2 .. 10                    154
      PIXL_PHINUM(1)=0                                                    154
      PIXL_PHINUM(2)=216                                                  154
      PIXL_PHINUM(3)=144                                                  154
      CPIXL_PHINUM='PHI VALUES FOR EACH SECTOR, STARTING FROM 1, 2 .. 10  154
     *'                                                                   154
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXL',    155
     *BPATH_PIXL,IDNUM_PIXL,1, MAP_PIXL,NAMES_PIXL,COMMEN_PIXL,           155
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   155
C *                                                                       155
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXL',    155
     *BPATH_PIXL,IDNUM_PIXL,1, MAP_PIXL,NAMES_PIXL,COMMEN_PIXL,           155
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   155
C *                                                                       156
      CALL AGSBEGM(MODULE,IPRIN)                                          156
      IF (IPRIN.LT.0) RETURN                                              156
C Filling bank PIXL - MERCEDES PROTOTYPE 1-4-8                            156
      BANK_TITLE='MERCEDES PROTOTYPE 1-4-8'                               156
      BPATH_PIXL='PIXL*'                                                  156
      IDNUM_PIXL(1)=0                                                     157
C NONE                                                                    157
      PIXL_VERSION=4.0                                                    157
      CPIXL_VERSION='NONE'                                                157
C NUMBER OF SECTORS                                                       158
      PIXL_SECTOR=3                                                       158
      CPIXL_SECTOR='NUMBER OF SECTORS'                                    158
C PHI VALUES FOR EACH SECTOR, STARTING FROM 1, 2 .. 10                    159
      PIXL_PHINUM(1)=0                                                    159
      PIXL_PHINUM(2)=252                                                  159
      PIXL_PHINUM(3)=108                                                  159
      CPIXL_PHINUM='PHI VALUES FOR EACH SECTOR, STARTING FROM 1, 2 .. 10  159
     *'                                                                   159
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXL',    160
     *BPATH_PIXL,IDNUM_PIXL,1, MAP_PIXL,NAMES_PIXL,COMMEN_PIXL,           160
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   160
C *                                                                       160
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXL',    160
     *BPATH_PIXL,IDNUM_PIXL,1, MAP_PIXL,NAMES_PIXL,COMMEN_PIXL,           160
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   160
C *                                                                       161
      CALL AGSBEGM(MODULE,IPRIN)                                          161
      IF (IPRIN.LT.0) RETURN                                              161
C Filling bank PIXL - MERCEDES PROTOTYPE 1-4-7                            161
      BANK_TITLE='MERCEDES PROTOTYPE 1-4-7'                               161
      BPATH_PIXL='PIXL*'                                                  161
      IDNUM_PIXL(1)=0                                                     162
C NONE                                                                    162
      PIXL_VERSION=5.0                                                    162
      CPIXL_VERSION='NONE'                                                162
C NUMBER OF SECTORS                                                       163
      PIXL_SECTOR=3                                                       163
      CPIXL_SECTOR='NUMBER OF SECTORS'                                    163
C PHI VALUES FOR EACH SECTOR, STARTING FROM 1, 2 .. 10                    164
      PIXL_PHINUM(1)=0                                                    164
      PIXL_PHINUM(2)=252                                                  164
      PIXL_PHINUM(3)=144                                                  164
      CPIXL_PHINUM='PHI VALUES FOR EACH SECTOR, STARTING FROM 1, 2 .. 10  164
     *'                                                                   164
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXL',    165
     *BPATH_PIXL,IDNUM_PIXL,1, MAP_PIXL,NAMES_PIXL,COMMEN_PIXL,           165
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   165
C *                                                                       165
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PIXL',    165
     *BPATH_PIXL,IDNUM_PIXL,1, MAP_PIXL,NAMES_PIXL,COMMEN_PIXL,           165
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   165
C *                                                                       166
      CALL AGSBEGM(MODULE,IPRIN)                                          166
      IF (IPRIN.LT.0) RETURN                                              166
C Filling bank PARO - DEFAULT                                             166
      BANK_TITLE='DEFAULT'                                                166
      BPATH_PARO='PARO*'                                                  166
      IDNUM_PARO(1)=0                                                     167
C NONE                                                                    167
      PARO_VERSION=0                                                      167
      CPARO_VERSION='NONE'                                                167
C NO SHIFT                                                                168
      PARO_DX=0                                                           168
      CPARO_DX='NO SHIFT'                                                 168
C NO SHIFT                                                                169
      PARO_DY=0                                                           169
      CPARO_DY='NO SHIFT'                                                 169
C NO SHIFT                                                                170
      PARO_DZ=0                                                           170
      CPARO_DZ='NO SHIFT'                                                 170
C NO SHIFT                                                                171
      PARO_MULT=0                                                         171
      CPARO_MULT='NO SHIFT'                                               171
C 1..10                                                                   172
      PARO_SEC(1)=0                                                       172
      PARO_SEC(2)=0                                                       172
      PARO_SEC(3)=0                                                       172
      PARO_SEC(4)=0                                                       172
      PARO_SEC(5)=0                                                       172
      PARO_SEC(6)=0                                                       172
      PARO_SEC(7)=0                                                       172
      PARO_SEC(8)=0                                                       172
      PARO_SEC(9)=0                                                       172
      PARO_SEC(10)=0                                                      172
      CPARO_SEC='1..10'                                                   172
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARO',    173
     *BPATH_PARO,IDNUM_PARO,1, MAP_PARO,NAMES_PARO,COMMEN_PARO,           173
     *BANK_PARO,LEN_PARO,LLEN_PARO,LINK_PARO,FLAG_PARO)                   173
C *                                                                       173
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARO',    173
     *BPATH_PARO,IDNUM_PARO,1, MAP_PARO,NAMES_PARO,COMMEN_PARO,           173
     *BANK_PARO,LEN_PARO,LLEN_PARO,LINK_PARO,FLAG_PARO)                   173
C *                                                                       174
      CALL AGSBEGM(MODULE,IPRIN)                                          174
      IF (IPRIN.LT.0) RETURN                                              174
C Filling bank PARO - SHIFTZ                                              174
      BANK_TITLE='SHIFTZ'                                                 174
      BPATH_PARO='PARO*'                                                  174
      IDNUM_PARO(1)=0                                                     175
C NONE                                                                    175
      PARO_VERSION=1                                                      175
      CPARO_VERSION='NONE'                                                175
C SHIFT                                                                   176
      PARO_DX=0                                                           176
      CPARO_DX='SHIFT'                                                    176
C NO SHIFT                                                                177
      PARO_DY=0                                                           177
      CPARO_DY='NO SHIFT'                                                 177
C NO SHIFT                                                                178
      PARO_DZ=1                                                           178
      CPARO_DZ='NO SHIFT'                                                 178
C MAX IS 5 CM                                                             179
      PARO_MULT=5                                                         179
      CPARO_MULT='MAX IS 5 CM'                                            179
C 1..10                                                                   180
      PARO_SEC(1)=1                                                       180
      PARO_SEC(2)=0                                                       180
      PARO_SEC(3)=0                                                       180
      PARO_SEC(4)=0                                                       180
      PARO_SEC(5)=0                                                       180
      PARO_SEC(6)=0                                                       180
      PARO_SEC(7)=0                                                       180
      PARO_SEC(8)=0                                                       180
      PARO_SEC(9)=0                                                       180
      PARO_SEC(10)=0                                                      180
      CPARO_SEC='1..10'                                                   180
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARO',    181
     *BPATH_PARO,IDNUM_PARO,1, MAP_PARO,NAMES_PARO,COMMEN_PARO,           181
     *BANK_PARO,LEN_PARO,LLEN_PARO,LINK_PARO,FLAG_PARO)                   181
C *                                                                       181
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARO',    181
     *BPATH_PARO,IDNUM_PARO,1, MAP_PARO,NAMES_PARO,COMMEN_PARO,           181
     *BANK_PARO,LEN_PARO,LLEN_PARO,LINK_PARO,FLAG_PARO)                   181
C *                                                                       182
      CALL AGSBEGM(MODULE,IPRIN)                                          182
      IF (IPRIN.LT.0) RETURN                                              182
C Filling bank PARA - DEFAULT                                             182
      BANK_TITLE='DEFAULT'                                                182
      BPATH_PARA='PARA*'                                                  182
      IDNUM_PARA(1)=0                                                     183
C NONE                                                                    183
      PARA_VERSION=0                                                      183
      CPARA_VERSION='NONE'                                                183
C NO ROTATION                                                             184
      PARA_DA=0                                                           184
      CPARA_DA='NO ROTATION'                                              184
C NO ROTATION                                                             185
      PARA_DB=0                                                           185
      CPARA_DB='NO ROTATION'                                              185
C NO ROTATION                                                             186
      PARA_DG=0                                                           186
      CPARA_DG='NO ROTATION'                                              186
C NO ROTATION                                                             187
      PARA_MULT=0                                                         187
      CPARA_MULT='NO ROTATION'                                            187
C 1..10                                                                   188
      PARA_SEC(1)=0                                                       188
      PARA_SEC(2)=0                                                       188
      PARA_SEC(3)=0                                                       188
      PARA_SEC(4)=0                                                       188
      PARA_SEC(5)=0                                                       188
      PARA_SEC(6)=0                                                       188
      PARA_SEC(7)=0                                                       188
      PARA_SEC(8)=0                                                       188
      PARA_SEC(9)=0                                                       188
      PARA_SEC(10)=0                                                      188
      CPARA_SEC='1..10'                                                   188
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARA',    189
     *BPATH_PARA,IDNUM_PARA,1, MAP_PARA,NAMES_PARA,COMMEN_PARA,           189
     *BANK_PARA,LEN_PARA,LLEN_PARA,LINK_PARA,FLAG_PARA)                   189
C *                                                                       189
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARA',    189
     *BPATH_PARA,IDNUM_PARA,1, MAP_PARA,NAMES_PARA,COMMEN_PARA,           189
     *BANK_PARA,LEN_PARA,LLEN_PARA,LINK_PARA,FLAG_PARA)                   189
C *                                                                       190
      CALL AGSBEGM(MODULE,IPRIN)                                          190
      IF (IPRIN.LT.0) RETURN                                              190
C Filling bank PARA - ROTATIONG                                           190
      BANK_TITLE='ROTATIONG'                                              190
      BPATH_PARA='PARA*'                                                  190
      IDNUM_PARA(1)=0                                                     191
C NONE                                                                    191
      PARA_VERSION=1                                                      191
      CPARA_VERSION='NONE'                                                191
C NO ROT.                                                                 192
      PARA_DA=0                                                           192
      CPARA_DA='NO ROT.'                                                  192
C NO ROT.                                                                 193
      PARA_DB=0                                                           193
      CPARA_DB='NO ROT.'                                                  193
C ROT.                                                                    194
      PARA_DG=5                                                           194
      CPARA_DG='ROT.'                                                     194
C MAX IS 5 DEG                                                            195
      PARA_MULT=1                                                         195
      CPARA_MULT='MAX IS 5 DEG'                                           195
C 1..10                                                                   196
      PARA_SEC(1)=1                                                       196
      PARA_SEC(2)=0                                                       196
      PARA_SEC(3)=0                                                       196
      PARA_SEC(4)=0                                                       196
      PARA_SEC(5)=0                                                       196
      PARA_SEC(6)=0                                                       196
      PARA_SEC(7)=0                                                       196
      PARA_SEC(8)=0                                                       196
      PARA_SEC(9)=0                                                       196
      PARA_SEC(10)=0                                                      196
      CPARA_SEC='1..10'                                                   196
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARA',    197
     *BPATH_PARA,IDNUM_PARA,1, MAP_PARA,NAMES_PARA,COMMEN_PARA,           197
     *BANK_PARA,LEN_PARA,LLEN_PARA,LINK_PARA,FLAG_PARA)                   197
C *                                                                       197
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PARA',    197
     *BPATH_PARA,IDNUM_PARA,1, MAP_PARA,NAMES_PARA,COMMEN_PARA,           197
     *BANK_PARA,LEN_PARA,LLEN_PARA,LINK_PARA,FLAG_PARA)                   197
C Geant COMPONENT : O ===> A=16 Z=8 W=0.164 <===                          198
      CALL AGSRESET                                                       198
      AG_A = 16                                                           198
      AG_Z = 8                                                            198
      AG_W = 0.164                                                        198
      AG_TITLE = 'O'                                                      198
      AG_EXNAME = 'COMPONENT'                                             198
      AG_PARLIST = 'A_Z_W'                                                198
      CALL AXCOMPONENT                                                    198
C *                                                                       198
C Geant COMPONENT : C ===> A=12 Z=6 W=0.763 <===                          198
      CALL AGSRESET                                                       199
      AG_A = 12                                                           199
      AG_Z = 6                                                            199
      AG_W = 0.763                                                        199
      AG_TITLE = 'C'                                                      199
      AG_EXNAME = 'COMPONENT'                                             199
      AG_PARLIST = 'A_Z_W'                                                199
      CALL AXCOMPONENT                                                    199
C *                                                                       199
C Geant COMPONENT : H ===> A=1 Z=1 W=0.073 <===                           199
      CALL AGSRESET                                                       200
      AG_A = 1                                                            200
      AG_Z = 1                                                            200
      AG_W = 0.073                                                        200
      AG_TITLE = 'H'                                                      200
      AG_EXNAME = 'COMPONENT'                                             200
      AG_PARLIST = 'A_Z_W'                                                200
      CALL AXCOMPONENT                                                    200
C *                                                                       200
C Geant MIXTURE : GLUMIX ===> DENS=1.2 <===                               200
      CALL AGSRESET                                                       201
      AG_DENS = 1.2                                                       201
      AG_TITLE = 'GLUMIX'                                                 201
      AG_EXNAME = 'MIXTURE'                                               201
      AG_PARLIST = 'DENS'                                                 201
      CALL AXMIXTURE                                                      201
C *                                                                       201
C Geant MATERIAL : CFRPMIX ===> A=12 Z=6 DENS=1.3 RADL=23.9 <===          201
      CALL AGSRESET                                                       202
      AG_A = 12                                                           202
      AG_Z = 6                                                            202
      AG_DENS = 1.3                                                       202
      AG_RADL = 23.9                                                      202
      AG_TITLE = 'CFRPMIX'                                                202
      AG_EXNAME = 'MATERIAL'                                              202
      AG_PARLIST = 'A_Z_DENS_RADL'                                        202
      CALL AXMATERIAL                                                     202
C *                                                                       202
C Geant MATERIAL : CBMIX ===> A=12 Z=6 DENS=1.3 RADL=68 <===              202
      CALL AGSRESET                                                       203
      AG_A = 12                                                           203
      AG_Z = 6                                                            203
      AG_DENS = 1.3                                                       203
      AG_RADL = 68                                                        203
      AG_TITLE = 'CBMIX'                                                  203
      AG_EXNAME = 'MATERIAL'                                              203
      AG_PARLIST = 'A_Z_DENS_RADL'                                        203
      CALL AXMATERIAL                                                     203
C *                                                                       203
C Geant MATERIAL : ALCAMIX ===> A=26.98 Z=13 DENS=2.7 RADL=23.7 <===      203
      CALL AGSRESET                                                       204
      AG_A = 26.98                                                        204
      AG_Z = 13                                                           204
      AG_DENS = 2.7                                                       204
      AG_RADL = 23.7                                                      204
      AG_TITLE = 'ALCAMIX'                                                204
      AG_EXNAME = 'MATERIAL'                                              204
      AG_PARLIST = 'A_Z_DENS_RADL'                                        204
      CALL AXMATERIAL                                                     204
C *                                                                       204
C use PIXL bank                                                           204
      BPATH_PIXL='PIXL*'                                                  205
      IDNUM_PIXL(1)=0                                                     205
      PIXL_VERSION=1.0                                                    205
      CALL AGDGETP('PIXLGEO4','*',-999,'PIXL',NPIXL_VERSION,              205
     *RE_PIXL_VERSION,BPATH_PIXL,IDNUM_PIXL,1,MAP_PIXL,NAMES_PIXL,        205
     *BANK_PIXL,LEN_PIXL,LLEN_PIXL,LINK_PIXL,FLAG_PIXL)                   205
C use PARO bank                                                           206
      BPATH_PARO='PARO*'                                                  206
      IDNUM_PARO(1)=0                                                     206
      PARO_VERSION=0                                                      206
      CALL AGDGETP('PIXLGEO4','*',-999,'PARO',NPARO_VERSION,              206
     *RE_PARO_VERSION,BPATH_PARO,IDNUM_PARO,1,MAP_PARO,NAMES_PARO,        206
     *BANK_PARO,LEN_PARO,LLEN_PARO,LINK_PARO,FLAG_PARO)                   206
C use PARA bank                                                           207
      BPATH_PARA='PARA*'                                                  207
      IDNUM_PARA(1)=0                                                     207
      PARA_VERSION=0                                                      207
      CALL AGDGETP('PIXLGEO4','*',-999,'PARA',NPARA_VERSION,              207
     *RE_PARA_VERSION,BPATH_PARA,IDNUM_PARA,1,MAP_PARA,NAMES_PARA,        207
     *BANK_PARA,LEN_PARA,LLEN_PARA,LINK_PARA,FLAG_PARA)                   207
      DEBUG = 0                                                           208
      CALL AGSBEGM(MODULE,IPRIN)                                          209
C Check IPRIN<0                                                           209
      IF (IPRIN.LT.0) RETURN                                              209
C Create PXMO :                                                           209
C Call To Local Procedure PXMO                                            209
      ASSIGN 5020 TO BLOCK_PXMO                                           209
      GO TO  5010                                                         209
5020  CONTINUE                                                            209
C *                                                                       209
C Geant POSITION : PXMO ===> MOTHER='IDSM' <===                           210
      CALL AGSRESET                                                       210
      AG_MOTHER = 'IDSM'                                                  210
      AG_TITLE = 'PXMO'                                                   210
      AG_EXNAME = 'POSITION'                                              210
      AG_PARLIST = 'MOTHER'                                               210
      CALL AXPOSITION                                                     210
C *                                                                       210
C ----------------------------------------------------------------------  210
C ------------ PXMO                                                       210
      CALL AGSENDM                                                        212
      RETURN                                                              212
C Begin Local Procedure PXMO                                              212
C PXMO MAIN VOLUME IN THE AGML TUTORIAL GEOMETRY                          212
5010  CONTINUE                                                            212
         AG_TITLE='PXMO'                                                  212
         AG_MODULE=MODULE                                                 212
         AG_MARK='V00'                                                    212
         CALL AGSPUSH                                                     212
         AG_MODULE=MODULE                                                 212
C    *                                                                    212
C    Geant MATERIAL : AIR ===> inherited arguments used <===              213
         CALL AGSRESET                                                    213
         AG_TITLE = 'AIR'                                                 213
         AG_EXNAME = 'MATERIAL'                                           213
         AG_PARLIST = 'NONE'                                              213
         CALL AXMATERIAL                                                  213
C    *                                                                    213
C    Geant ATTRIBUTE : PXMO ===> SEEN=0 COLO=6 <===                       213
         CALL AGSRESET                                                    214
         AG_SEEN = 0                                                      214
         AG_COLO = 6                                                      214
         AG_TITLE = 'PXMO'                                                214
         AG_EXNAME = 'ATTRIBUTE'                                          214
         AG_PARLIST = 'SEEN_COLO'                                         214
         CALL AXATTRIBUTE                                                 214
C    *                                                                    214
C    Geant SHAPE : TUBE ===> RMIN=MINRVOL RMAX=MAXRVOL DZ=MAXZVOL <===    214
         CALL AGSRESET                                                    215
         AG_RMIN = MINRVOL                                                215
         AG_RMAX = MAXRVOL                                                215
         AG_DZ = MAXZVOL                                                  215
         AG_TITLE = 'TUBE'                                                215
         AG_EXNAME = 'SHAPE'                                              215
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      215
         CALL AXSHAPE                                                     215
C    *                                                                    215
C    *                                                                    215
         SECTOR=1                                                         216
            GO TO 5033                                                    216
5031        SECTOR=SECTOR+1                                               216
5033        IF(SECTOR-(PIXL_SECTOR).GT.0) GO TO 5032                      216
            ANGLE = PIXL_PHINUM(SECTOR)                                   217
            ANGLEPOS=ANGLE*(PI/180.0)                                     218
            XPOS=.00001*SIN(ANGLEPOS) +                                   219
     *      (PARO_DX*PARO_MULT)*PARO_SEC(SECTOR)                          219
            YPOS=.00001*COS(ANGLEPOS) +                                   220
     *      (PARO_DY*PARO_MULT)*PARO_SEC(SECTOR)                          220
            ZPOS =GLOBOFFZ + (PARO_DZ*PARO_MULT)*PARO_SEC(SECTOR)         221
C       Create PXLA :                                                     222
C       Call To Local Procedure PXLA                                      222
            ASSIGN 5050 TO BLOCK_PXLA                                     222
            GO TO  5040                                                   222
5050        CONTINUE                                                      222
C       *                                                                 222
C       Check DEBUG==1                                                    223
            IF (DEBUG.EQ.1) THEN                                          223
            WRITE(LOUT,5060)ANGLE                                         223
5060        FORMAT('angle(deg)=', F6.3)                                   223
            WRITE(LOUT,5070)SECTOR                                        224
5070        FORMAT('sector=', F6.2)                                       224
            WRITE(LOUT,5080)XPOS*1000000                                  225
5080        FORMAT('xpos=', F6.2)                                         225
            WRITE(LOUT,5090)YPOS*1000000                                  226
5090        FORMAT('ypos=', F6.2)                                         226
            WRITE(LOUT,5100)ZPOS                                          227
5100        FORMAT('zpos=', F6.2)                                         227
            WRITE(LOUT,5110)-1*(PARA_DG*PARA_MULT)*PARA_SEC(SECTOR)       228
5110        FORMAT('alphaZ=', F6.2)                                       228
            WRITE(LOUT,5120)-1*(PARA_DA*PARA_MULT)*PARA_SEC(SECTOR)       229
5120        FORMAT('alphaX=', F6.2)                                       229
            WRITE(LOUT,5130)-1*(PARA_DB*PARA_MULT)*PARA_SEC(SECTOR)       230
5130        FORMAT('alphaY=', F6.2)                                       230
            ENDIF                                                         232
C       Geant POSITION : PXLA ===> MOTHER='PXMO' X=XPOS Y=YPOS Z=ZPOS AL  236
C       PHAZ=-1*ANGLE-(PARA_DG*PARA_MULT*PARA_SEC(SECTOR)) ALPHAX=-1*(PA  236
C       RA_DA*PARA_MULT)*PARA_SEC(SECTOR) ALPHAY=-1*(PARA_DB*PARA_MULT)*  236
C       PARA_SEC(SECTOR) KONLY='MANY' <===                                236
            CALL AGSRESET                                                 236
            AG_MOTHER = 'PXMO'                                            236
            AG_X = XPOS                                                   236
            AG_Y = YPOS                                                   236
            AG_Z = ZPOS                                                   236
            AG_ALPHAN=AG_ALPHAN+1                                         236
            AG_ALPHAK(AG_ALPHAN)=3                                        236
            AG_ALPHAL(AG_ALPHAN)=                                         236
     *      -1*ANGLE-(PARA_DG*PARA_MULT*PARA_SEC(SECTOR))                 236
            AG_ALPHAN=AG_ALPHAN+1                                         236
            AG_ALPHAK(AG_ALPHAN)=1                                        236
            AG_ALPHAL(AG_ALPHAN)=                                         236
     *      -1*(PARA_DA*PARA_MULT)*PARA_SEC(SECTOR)                       236
            AG_ALPHAN=AG_ALPHAN+1                                         236
            AG_ALPHAK(AG_ALPHAN)=2                                        236
            AG_ALPHAL(AG_ALPHAN)=                                         236
     *      -1*(PARA_DB*PARA_MULT)*PARA_SEC(SECTOR)                       236
            AG_KONLY = 'MANY'                                             236
            AG_TITLE = 'PXLA'                                             236
            AG_EXNAME = 'POSITION'                                        236
            AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAZ_ALPHAX_ALPHAY_KONLY'        236
            CALL AXPOSITION                                               236
C       *                                                                 236
         GO TO 5031                                                       237
5032     CONTINUE                                                         237
5011  CALL AGSPOPB                                                        238
      GO TO BLOCK_PXMO, (5010 ,5020)                                      238
C End of Local Procedure                                                  238
C ----------------------------------------------------------------------  239
C ------------ PXLA                                                       239
C Begin Local Procedure PXLA                                              240
C PXLA PIXEL SECTOR                                                       240
5040  CONTINUE                                                            240
         AG_TITLE='PXLA'                                                  240
         AG_MODULE=MODULE                                                 240
         AG_MARK='V00'                                                    240
         CALL AGSPUSH                                                     240
         AG_MODULE=MODULE                                                 240
C    *                                                                    240
C    Geant MATERIAL : AIR ===> inherited arguments used <===              241
         CALL AGSRESET                                                    241
         AG_TITLE = 'AIR'                                                 241
         AG_EXNAME = 'MATERIAL'                                           241
         AG_PARLIST = 'NONE'                                              241
         CALL AXMATERIAL                                                  241
C    *                                                                    241
C    Geant ATTRIBUTE : PXLA ===> SEEN=0 COLO=1 <===                       241
         CALL AGSRESET                                                    242
         AG_SEEN = 0                                                      242
         AG_COLO = 1                                                      242
         AG_TITLE = 'PXLA'                                                242
         AG_EXNAME = 'ATTRIBUTE'                                          242
         AG_PARLIST = 'SEEN_COLO'                                         242
         CALL AXATTRIBUTE                                                 242
C    *                                                                    242
C    Geant SHAPE : TUBS ===> RMIN=MINRADSEC RMAX=MAXRADSEC, PHI1=MINPHIS  242
C    EC PHI2=MAXPHISEC DZ=HALFMAXZ <===                                   242
         CALL AGSRESET                                                    244
         AG_RMIN = MINRADSEC                                              244
         AG_RMAX = MAXRADSEC                                              244
         AG_PHI1 = MINPHISEC                                              244
         AG_PHI2 = MAXPHISEC                                              244
         AG_DZ = HALFMAXZ                                                 244
         AG_TITLE = 'TUBS'                                                244
         AG_EXNAME = 'SHAPE'                                              244
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            244
         CALL AXSHAPE                                                     244
C    *                                                                    244
C    Create PXRB :                                                        245
C    Call To Local Procedure PXRB                                         245
         ASSIGN 5150 TO BLOCK_PXRB                                        245
         GO TO  5140                                                      245
5150     CONTINUE                                                         245
C    *                                                                    245
C    Geant POSITION : PXRB ===> X=-0.112 Y=5.26084 Z=-2.67 <===           246
         CALL AGSRESET                                                    246
         AG_X = -0.112                                                    246
         AG_Y = 5.26084                                                   246
         AG_Z = -2.67                                                     246
         AG_TITLE = 'PXRB'                                                246
         AG_EXNAME = 'POSITION'                                           246
         AG_PARLIST = 'X_Y_Z'                                             246
         CALL AXPOSITION                                                  246
C    *                                                                    246
C    Create PXLB :                                                        247
C    Call To Local Procedure PXLB                                         247
         ASSIGN 5170 TO BLOCK_PXLB                                        247
         GO TO  5160                                                      247
5170     CONTINUE                                                         247
C    *                                                                    247
C    Geant POSITION : PXLB ===> X=-3.2696 Y=4.69076 Z=-2.67 ALPHAZ=216 <  248
C    ===                                                                  248
         CALL AGSRESET                                                    248
         AG_X = -3.2696                                                   248
         AG_Y = 4.69076                                                   248
         AG_Z = -2.67                                                     248
         AG_ALPHAN=AG_ALPHAN+1                                            248
         AG_ALPHAK(AG_ALPHAN)=3                                           248
         AG_ALPHAL(AG_ALPHAN)=216                                         248
         AG_TITLE = 'PXLB'                                                248
         AG_EXNAME = 'POSITION'                                           248
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      248
         CALL AXPOSITION                                                  248
C    *                                                                    248
C    Create PXIB :                                                        249
C    Call To Local Procedure PXIB                                         249
         ASSIGN 5190 TO BLOCK_PXIB                                        249
         GO TO  5180                                                      249
5190     CONTINUE                                                         249
C    *                                                                    249
C    Geant POSITION : PXIB ===> X=-0.989152 Y=2.64767 Z=-2.67 ALPHAZ=267  250
C    .396-270 <===                                                        250
         CALL AGSRESET                                                    250
         AG_X = -0.989152                                                 250
         AG_Y = 2.64767                                                   250
         AG_Z = -2.67                                                     250
         AG_ALPHAN=AG_ALPHAN+1                                            250
         AG_ALPHAK(AG_ALPHAN)=3                                           250
         AG_ALPHAL(AG_ALPHAN)=267.396-270                                 250
         AG_TITLE = 'PXIB'                                                250
         AG_EXNAME = 'POSITION'                                           250
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      250
         CALL AXPOSITION                                                  250
C    *                                                                    250
C    Create PXTR :                                                        251
C    Call To Local Procedure PXTR                                         251
         ASSIGN 5210 TO BLOCK_PXTR                                        251
         GO TO  5200                                                      251
5210     CONTINUE                                                         251
C    *                                                                    251
C    Geant POSITION : PXTR ===> X=-0.839768 Y=8.01401 Z=-2.67 ALPHAZ=79.  252
C    963-90 <===                                                          252
         CALL AGSRESET                                                    252
         AG_X = -0.839768                                                 252
         AG_Y = 8.01401                                                   252
         AG_Z = -2.67                                                     252
         AG_ALPHAN=AG_ALPHAN+1                                            252
         AG_ALPHAK(AG_ALPHAN)=3                                           252
         AG_ALPHAL(AG_ALPHAN)=79.963-90                                   252
         AG_TITLE = 'PXTR'                                                252
         AG_EXNAME = 'POSITION'                                           252
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      252
         CALL AXPOSITION                                                  252
C    *                                                                    252
C    Create PXTM :                                                        253
C    Call To Local Procedure PXTM                                         253
         ASSIGN 5230 TO BLOCK_PXTM                                        253
         GO TO  5220                                                      253
5230     CONTINUE                                                         253
C    *                                                                    253
C    Geant POSITION : PXTM ===> X=-2.43063 Y=7.66624 Z=-2.67 ALPHAZ=91.9  254
C    63-90 <===                                                           254
         CALL AGSRESET                                                    254
         AG_X = -2.43063                                                  254
         AG_Y = 7.66624                                                   254
         AG_Z = -2.67                                                     254
         AG_ALPHAN=AG_ALPHAN+1                                            254
         AG_ALPHAK(AG_ALPHAN)=3                                           254
         AG_ALPHAL(AG_ALPHAN)=91.963-90                                   254
         AG_TITLE = 'PXTM'                                                254
         AG_EXNAME = 'POSITION'                                           254
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      254
         CALL AXPOSITION                                                  254
C    *                                                                    254
C    Create PXTL :                                                        255
C    Call To Local Procedure PXTL                                         255
         ASSIGN 5250 TO BLOCK_PXTL                                        255
         GO TO  5240                                                      255
5250     CONTINUE                                                         255
C    *                                                                    255
C    Geant POSITION : PXTL ===> X=-3.9532 Y=6.99789 Z=-2.67 ALPHAZ=103.6  256
C    93-90 <===                                                           256
         CALL AGSRESET                                                    256
         AG_X = -3.9532                                                   256
         AG_Y = 6.99789                                                   256
         AG_Z = -2.67                                                     256
         AG_ALPHAN=AG_ALPHAN+1                                            256
         AG_ALPHAK(AG_ALPHAN)=3                                           256
         AG_ALPHAL(AG_ALPHAN)=103.693-90                                  256
         AG_TITLE = 'PXTL'                                                256
         AG_EXNAME = 'POSITION'                                           256
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      256
         CALL AXPOSITION                                                  256
C    *                                                                    256
C    Create PXTJ :                                                        257
C    Call To Local Procedure PXTJ                                         257
         ASSIGN 5270 TO BLOCK_PXTJ                                        257
         GO TO  5260                                                      257
5270     CONTINUE                                                         257
C    *                                                                    257
C    Geant POSITION : PXTJ ===> X=-3.21706 Y=7.41467 Z=-2.67 ALPHAZ=203.  258
C    54-270 <===                                                          258
         CALL AGSRESET                                                    258
         AG_X = -3.21706                                                  258
         AG_Y = 7.41467                                                   258
         AG_Z = -2.67                                                     258
         AG_ALPHAN=AG_ALPHAN+1                                            258
         AG_ALPHAK(AG_ALPHAN)=3                                           258
         AG_ALPHAL(AG_ALPHAN)=203.54-270                                  258
         AG_TITLE = 'PXTJ'                                                258
         AG_EXNAME = 'POSITION'                                           258
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      258
         CALL AXPOSITION                                                  258
C    *                                                                    258
C    Create PXTJ :                                                        259
C    Call To Local Procedure PXTJ                                         259
         ASSIGN 5280 TO BLOCK_PXTJ                                        259
         GO TO  5260                                                      259
5280     CONTINUE                                                         259
C    *                                                                    259
C    Geant POSITION : PXTJ ===> X=-1.60516 Y=7.92151 Z=-2.67 ALPHAZ=191.  260
C    54-270 <===                                                          260
         CALL AGSRESET                                                    260
         AG_X = -1.60516                                                  260
         AG_Y = 7.92151                                                   260
         AG_Z = -2.67                                                     260
         AG_ALPHAN=AG_ALPHAN+1                                            260
         AG_ALPHAK(AG_ALPHAN)=3                                           260
         AG_ALPHAL(AG_ALPHAN)=191.54-270                                  260
         AG_TITLE = 'PXTJ'                                                260
         AG_EXNAME = 'POSITION'                                           260
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      260
         CALL AXPOSITION                                                  260
C    *                                                                    260
C    Create PXCA :                                                        261
C    Call To Local Procedure PXCA                                         261
         ASSIGN 5300 TO BLOCK_PXCA                                        261
         GO TO  5290                                                      261
5300     CONTINUE                                                         261
C    *                                                                    261
C    Geant POSITION : PXCA ===> X=-0.25 Y=2.7522 Z=-2.67 <===             262
         CALL AGSRESET                                                    262
         AG_X = -0.25                                                     262
         AG_Y = 2.7522                                                    262
         AG_Z = -2.67                                                     262
         AG_TITLE = 'PXCA'                                                262
         AG_EXNAME = 'POSITION'                                           262
         AG_PARLIST = 'X_Y_Z'                                             262
         CALL AXPOSITION                                                  262
C    *                                                                    262
C    Create PXCC :                                                        263
C    Call To Local Procedure PXCC                                         263
         ASSIGN 5320 TO BLOCK_PXCC                                        263
         GO TO  5310                                                      263
5320     CONTINUE                                                         263
C    *                                                                    263
C    Geant POSITION : PXCC ===> X=-0.25 Y=7.76948 Z=-2.67 <===            264
         CALL AGSRESET                                                    264
         AG_X = -0.25                                                     264
         AG_Y = 7.76948                                                   264
         AG_Z = -2.67                                                     264
         AG_TITLE = 'PXCC'                                                264
         AG_EXNAME = 'POSITION'                                           264
         AG_PARLIST = 'X_Y_Z'                                             264
         CALL AXPOSITION                                                  264
C    *                                                                    264
C    Create PXCD :                                                        265
C    Call To Local Procedure PXCD                                         265
         ASSIGN 5340 TO BLOCK_PXCD                                        265
         GO TO  5330                                                      265
5340     CONTINUE                                                         265
C    *                                                                    265
C    Geant POSITION : PXCD ===> X=-1.47764 Y=7.98676 Z=-2.67 <===         266
         CALL AGSRESET                                                    266
         AG_X = -1.47764                                                  266
         AG_Y = 7.98676                                                   266
         AG_Z = -2.67                                                     266
         AG_TITLE = 'PXCD'                                                266
         AG_EXNAME = 'POSITION'                                           266
         AG_PARLIST = 'X_Y_Z'                                             266
         CALL AXPOSITION                                                  266
C    *                                                                    266
C    Create PXCE :                                                        267
C    Call To Local Procedure PXCE                                         267
         ASSIGN 5360 TO BLOCK_PXCE                                        267
         GO TO  5350                                                      267
5360     CONTINUE                                                         267
C    *                                                                    267
C    Geant POSITION : PXCE ===> X=-1.7562 Y=7.85145 Z=-2.67 <===          268
         CALL AGSRESET                                                    268
         AG_X = -1.7562                                                   268
         AG_Y = 7.85145                                                   268
         AG_Z = -2.67                                                     268
         AG_TITLE = 'PXCE'                                                268
         AG_EXNAME = 'POSITION'                                           268
         AG_PARLIST = 'X_Y_Z'                                             268
         CALL AXPOSITION                                                  268
C    *                                                                    268
C    Create PXCF :                                                        269
C    Call To Local Procedure PXCF                                         269
         ASSIGN 5380 TO BLOCK_PXCF                                        269
         GO TO  5370                                                      269
5380     CONTINUE                                                         269
C    *                                                                    269
C    Geant POSITION : PXCF ===> X=-3.10589 Y=7.50501 Z=-2.67 <===         270
         CALL AGSRESET                                                    270
         AG_X = -3.10589                                                  270
         AG_Y = 7.50501                                                   270
         AG_Z = -2.67                                                     270
         AG_TITLE = 'PXCF'                                                270
         AG_EXNAME = 'POSITION'                                           270
         AG_PARLIST = 'X_Y_Z'                                             270
         CALL AXPOSITION                                                  270
C    *                                                                    270
C    Create PXCG :                                                        271
C    Call To Local Procedure PXCG                                         271
         ASSIGN 5400 TO BLOCK_PXCG                                        271
         GO TO  5390                                                      271
5400     CONTINUE                                                         271
C    *                                                                    271
C    Geant POSITION : PXCG ===> X=-3.35023 Y=7.31474 Z=-2.67 <===         272
         CALL AGSRESET                                                    272
         AG_X = -3.35023                                                  272
         AG_Y = 7.31474                                                   272
         AG_Z = -2.67                                                     272
         AG_TITLE = 'PXCG'                                                272
         AG_EXNAME = 'POSITION'                                           272
         AG_PARLIST = 'X_Y_Z'                                             272
         CALL AXPOSITION                                                  272
C    *                                                                    272
C    Create PXCH :                                                        273
C    Call To Local Procedure PXCH                                         273
         ASSIGN 5420 TO BLOCK_PXCH                                        273
         GO TO  5410                                                      273
5420     CONTINUE                                                         273
C    *                                                                    273
C    Geant POSITION : PXCH ===> X=-4.56196 Y=6.70432 Z=-2.67 <===         274
         CALL AGSRESET                                                    274
         AG_X = -4.56196                                                  274
         AG_Y = 6.70432                                                   274
         AG_Z = -2.67                                                     274
         AG_TITLE = 'PXCH'                                                274
         AG_EXNAME = 'POSITION'                                           274
         AG_PARLIST = 'X_Y_Z'                                             274
         CALL AXPOSITION                                                  274
C    *                                                                    274
C    Create PXCB :                                                        275
C    Call To Local Procedure PXCB                                         275
         ASSIGN 5440 TO BLOCK_PXCB                                        275
         GO TO  5430                                                      275
5440     CONTINUE                                                         275
C    *                                                                    275
C    Geant POSITION : PXCB ===> X=-1.71349 Y=2.86881 Z=-2.67 <===         276
         CALL AGSRESET                                                    276
         AG_X = -1.71349                                                  276
         AG_Y = 2.86881                                                   276
         AG_Z = -2.67                                                     276
         AG_TITLE = 'PXCB'                                                276
         AG_EXNAME = 'POSITION'                                           276
         AG_PARLIST = 'X_Y_Z'                                             276
         CALL AXPOSITION                                                  276
C    *                                                                    276
C    Create LADR :                                                        277
C    Call To Local Procedure LADR                                         277
         ASSIGN 5460 TO BLOCK_LADR                                        277
         GO TO  5450                                                      277
5460     CONTINUE                                                         277
C    *                                                                    277
C    Geant POSITION : LADR ===> X=-1.35685 Y=8.1624 Z=0 ALPHAZ=169.963 <  278
C    ===                                                                  278
         CALL AGSRESET                                                    278
         AG_X = -1.35685                                                  278
         AG_Y = 8.1624                                                    278
         AG_Z = 0                                                         278
         AG_ALPHAN=AG_ALPHAN+1                                            278
         AG_ALPHAK(AG_ALPHAN)=3                                           278
         AG_ALPHAL(AG_ALPHAN)=169.963                                     278
         AG_TITLE = 'LADR'                                                278
         AG_EXNAME = 'POSITION'                                           278
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      278
         CALL AXPOSITION                                                  278
C    *                                                                    278
C    Create LADR :                                                        279
C    Call To Local Procedure LADR                                         279
         ASSIGN 5470 TO BLOCK_LADR                                        279
         GO TO  5450                                                      279
5470     CONTINUE                                                         279
C    *                                                                    279
C    Geant POSITION : LADR ===> X=-3.02426 Y=7.70193 Z=0 ALPHAZ=181.963   280
C    <===                                                                 280
         CALL AGSRESET                                                    280
         AG_X = -3.02426                                                  280
         AG_Y = 7.70193                                                   280
         AG_Z = 0                                                         280
         AG_ALPHAN=AG_ALPHAN+1                                            280
         AG_ALPHAK(AG_ALPHAN)=3                                           280
         AG_ALPHAL(AG_ALPHAN)=181.963                                     280
         AG_TITLE = 'LADR'                                                280
         AG_EXNAME = 'POSITION'                                           280
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      280
         CALL AXPOSITION                                                  280
C    *                                                                    280
C    Create LADR :                                                        281
C    Call To Local Procedure LADR                                         281
         ASSIGN 5480 TO BLOCK_LADR                                        281
         GO TO  5450                                                      281
5480     CONTINUE                                                         281
C    *                                                                    281
C    Geant POSITION : LADR ===> X=-4.55949 Y=6.90484 Z=0 ALPHAZ=193.963   282
C    <===                                                                 282
         CALL AGSRESET                                                    282
         AG_X = -4.55949                                                  282
         AG_Y = 6.90484                                                   282
         AG_Z = 0                                                         282
         AG_ALPHAN=AG_ALPHAN+1                                            282
         AG_ALPHAK(AG_ALPHAN)=3                                           282
         AG_ALPHAL(AG_ALPHAN)=193.963                                     282
         AG_TITLE = 'LADR'                                                282
         AG_EXNAME = 'POSITION'                                           282
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      282
         CALL AXPOSITION                                                  282
C    *                                                                    282
C    Create LADR :                                                        283
C    Call To Local Procedure LADR                                         283
         ASSIGN 5490 TO BLOCK_LADR                                        283
         GO TO  5450                                                      283
5490     CONTINUE                                                         283
C    *                                                                    283
C    Geant POSITION : LADR ===> X=-0.612167 Y=2.57447 Z=0 ALPHAZ=357.396  284
C     <===                                                                284
         CALL AGSRESET                                                    284
         AG_X = -0.612167                                                 284
         AG_Y = 2.57447                                                   284
         AG_Z = 0                                                         284
         AG_ALPHAN=AG_ALPHAN+1                                            284
         AG_ALPHAK(AG_ALPHAN)=3                                           284
         AG_ALPHAL(AG_ALPHAN)=357.396                                     284
         AG_TITLE = 'LADR'                                                284
         AG_EXNAME = 'POSITION'                                           284
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      284
         CALL AXPOSITION                                                  284
C    *                                                                    284
5041  CALL AGSPOPB                                                        285
      GO TO BLOCK_PXLA, (5040 ,5050)                                      285
C End of Local Procedure                                                  285
C ----------------------------------------------------------------------  286
C ------------ PXRB                                                       286
C Begin Local Procedure PXRB                                              287
C PXRB SECTOR RIGHT SIDE                                                  287
5140  CONTINUE                                                            287
         AG_TITLE='PXRB'                                                  287
         AG_MODULE=MODULE                                                 287
         AG_MARK='V00'                                                    287
         CALL AGSPUSH                                                     287
         AG_MODULE=MODULE                                                 287
C    *                                                                    287
C    Geant ATTRIBUTE : PXRB ===> SEEN=1 COLO=4 <===                       288
         CALL AGSRESET                                                    288
         AG_SEEN = 1                                                      288
         AG_COLO = 4                                                      288
         AG_TITLE = 'PXRB'                                                288
         AG_EXNAME = 'ATTRIBUTE'                                          288
         AG_PARLIST = 'SEEN_COLO'                                         288
         CALL AXATTRIBUTE                                                 288
C    *                                                                    288
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          288
         CALL AGSRESET                                                    289
         AG_TITLE = 'CFRPMIX'                                             289
         AG_EXNAME = 'MATERIAL'                                           289
         AG_PARLIST = 'NONE'                                              289
         CALL AXMATERIAL                                                  289
C    *                                                                    289
C    Geant SHAPE : BOX ===> DX=HALFLADTHK DY=HALFLADRIGHT, DZ=HALFLADZLE  289
C    N <===                                                               289
         CALL AGSRESET                                                    291
         AG_DX = HALFLADTHK                                               291
         AG_DY = HALFLADRIGHT                                             291
         AG_DZ = HALFLADZLEN                                              291
         AG_TITLE = 'BOX'                                                 291
         AG_EXNAME = 'SHAPE'                                              291
         AG_PARLIST = 'DX_DY_DZ'                                          291
         CALL AXSHAPE                                                     291
C    *                                                                    291
5141  CALL AGSPOPB                                                        292
      GO TO BLOCK_PXRB, (5140 ,5150)                                      292
C End of Local Procedure                                                  292
C ----------------------------------------------------------------------  293
C ------------ PXLB                                                       293
C Begin Local Procedure PXLB                                              294
C PXLB SECTOR LEFT SIDE                                                   294
5160  CONTINUE                                                            294
         AG_TITLE='PXLB'                                                  294
         AG_MODULE=MODULE                                                 294
         AG_MARK='V00'                                                    294
         CALL AGSPUSH                                                     294
         AG_MODULE=MODULE                                                 294
C    *                                                                    294
C    Geant ATTRIBUTE : PXLB ===> SEEN=1 COLO=4 <===                       295
         CALL AGSRESET                                                    295
         AG_SEEN = 1                                                      295
         AG_COLO = 4                                                      295
         AG_TITLE = 'PXLB'                                                295
         AG_EXNAME = 'ATTRIBUTE'                                          295
         AG_PARLIST = 'SEEN_COLO'                                         295
         CALL AXATTRIBUTE                                                 295
C    *                                                                    295
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          295
         CALL AGSRESET                                                    296
         AG_TITLE = 'CFRPMIX'                                             296
         AG_EXNAME = 'MATERIAL'                                           296
         AG_PARLIST = 'NONE'                                              296
         CALL AXMATERIAL                                                  296
C    *                                                                    296
C    Geant SHAPE : BOX ===> DX=HALFLADTHK DY=HALFLADLEFT DZ=HALFLADZLEN   296
C    <===                                                                 296
         CALL AGSRESET                                                    297
         AG_DX = HALFLADTHK                                               297
         AG_DY = HALFLADLEFT                                              297
         AG_DZ = HALFLADZLEN                                              297
         AG_TITLE = 'BOX'                                                 297
         AG_EXNAME = 'SHAPE'                                              297
         AG_PARLIST = 'DX_DY_DZ'                                          297
         CALL AXSHAPE                                                     297
C    *                                                                    297
5161  CALL AGSPOPB                                                        298
      GO TO BLOCK_PXLB, (5160 ,5170)                                      298
C End of Local Procedure                                                  298
C ----------------------------------------------------------------------  299
C ------------ PXIB                                                       299
C Begin Local Procedure PXIB                                              300
C PXIB SECTOR BOTTOM SIDE                                                 300
5180  CONTINUE                                                            300
         AG_TITLE='PXIB'                                                  300
         AG_MODULE=MODULE                                                 300
         AG_MARK='V00'                                                    300
         CALL AGSPUSH                                                     300
         AG_MODULE=MODULE                                                 300
C    *                                                                    300
C    Geant ATTRIBUTE : PXIB ===> SEEN=1 COLO=4 <===                       301
         CALL AGSRESET                                                    301
         AG_SEEN = 1                                                      301
         AG_COLO = 4                                                      301
         AG_TITLE = 'PXIB'                                                301
         AG_EXNAME = 'ATTRIBUTE'                                          301
         AG_PARLIST = 'SEEN_COLO'                                         301
         CALL AXATTRIBUTE                                                 301
C    *                                                                    301
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          301
         CALL AGSRESET                                                    302
         AG_TITLE = 'CFRPMIX'                                             302
         AG_EXNAME = 'MATERIAL'                                           302
         AG_PARLIST = 'NONE'                                              302
         CALL AXMATERIAL                                                  302
C    *                                                                    302
C    Geant SHAPE : BOX ===> DX=HALFLADBOTTOM DY=HALFLADTHK, DZ=HALFLADZL  302
C    EN <===                                                              302
         CALL AGSRESET                                                    304
         AG_DX = HALFLADBOTTOM                                            304
         AG_DY = HALFLADTHK                                               304
         AG_DZ = HALFLADZLEN                                              304
         AG_TITLE = 'BOX'                                                 304
         AG_EXNAME = 'SHAPE'                                              304
         AG_PARLIST = 'DX_DY_DZ'                                          304
         CALL AXSHAPE                                                     304
C    *                                                                    304
5181  CALL AGSPOPB                                                        305
      GO TO BLOCK_PXIB, (5180 ,5190)                                      305
C End of Local Procedure                                                  305
C ----------------------------------------------------------------------  306
C ------------ PXTR                                                       306
C Begin Local Procedure PXTR                                              307
C PXTR PART OF SECTOR RIGHT                                               307
5200  CONTINUE                                                            307
         AG_TITLE='PXTR'                                                  307
         AG_MODULE=MODULE                                                 307
         AG_MARK='V00'                                                    307
         CALL AGSPUSH                                                     307
         AG_MODULE=MODULE                                                 307
C    *                                                                    307
C    Geant ATTRIBUTE : PXTR ===> SEEN=1 COLO=4 <===                       308
         CALL AGSRESET                                                    308
         AG_SEEN = 1                                                      308
         AG_COLO = 4                                                      308
         AG_TITLE = 'PXTR'                                                308
         AG_EXNAME = 'ATTRIBUTE'                                          308
         AG_PARLIST = 'SEEN_COLO'                                         308
         CALL AXATTRIBUTE                                                 308
C    *                                                                    308
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          308
         CALL AGSRESET                                                    309
         AG_TITLE = 'CFRPMIX'                                             309
         AG_EXNAME = 'MATERIAL'                                           309
         AG_PARLIST = 'NONE'                                              309
         CALL AXMATERIAL                                                  309
C    *                                                                    309
C    Geant SHAPE : BOX ===> DX=HALFLADTOPR DY=HALFLADTHK DZ=HALFLADZLEN   309
C    <===                                                                 309
         CALL AGSRESET                                                    310
         AG_DX = HALFLADTOPR                                              310
         AG_DY = HALFLADTHK                                               310
         AG_DZ = HALFLADZLEN                                              310
         AG_TITLE = 'BOX'                                                 310
         AG_EXNAME = 'SHAPE'                                              310
         AG_PARLIST = 'DX_DY_DZ'                                          310
         CALL AXSHAPE                                                     310
C    *                                                                    310
5201  CALL AGSPOPB                                                        311
      GO TO BLOCK_PXTR, (5200 ,5210)                                      311
C End of Local Procedure                                                  311
C ----------------------------------------------------------------------  312
C ------------ PXTM                                                       312
C Begin Local Procedure PXTM                                              313
C PXTM PART OF SECTOR MIDDLE                                              313
5220  CONTINUE                                                            313
         AG_TITLE='PXTM'                                                  313
         AG_MODULE=MODULE                                                 313
         AG_MARK='V00'                                                    313
         CALL AGSPUSH                                                     313
         AG_MODULE=MODULE                                                 313
C    *                                                                    313
C    Geant ATTRIBUTE : PXTM ===> SEEN=1 COLO=4 <===                       314
         CALL AGSRESET                                                    314
         AG_SEEN = 1                                                      314
         AG_COLO = 4                                                      314
         AG_TITLE = 'PXTM'                                                314
         AG_EXNAME = 'ATTRIBUTE'                                          314
         AG_PARLIST = 'SEEN_COLO'                                         314
         CALL AXATTRIBUTE                                                 314
C    *                                                                    314
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          314
         CALL AGSRESET                                                    315
         AG_TITLE = 'CFRPMIX'                                             315
         AG_EXNAME = 'MATERIAL'                                           315
         AG_PARLIST = 'NONE'                                              315
         CALL AXMATERIAL                                                  315
C    *                                                                    315
C    Geant SHAPE : BOX ===> DX=HALFLADTOPM DY=HALFLADTHK DZ=HALFLADZLEN   315
C    <===                                                                 315
         CALL AGSRESET                                                    316
         AG_DX = HALFLADTOPM                                              316
         AG_DY = HALFLADTHK                                               316
         AG_DZ = HALFLADZLEN                                              316
         AG_TITLE = 'BOX'                                                 316
         AG_EXNAME = 'SHAPE'                                              316
         AG_PARLIST = 'DX_DY_DZ'                                          316
         CALL AXSHAPE                                                     316
C    *                                                                    316
5221  CALL AGSPOPB                                                        317
      GO TO BLOCK_PXTM, (5220 ,5230)                                      317
C End of Local Procedure                                                  317
C ----------------------------------------------------------------------  318
C ------------ PXTL                                                       318
C Begin Local Procedure PXTL                                              319
C PXTL PART OF SECTOR RIGHT                                               319
5240  CONTINUE                                                            319
         AG_TITLE='PXTL'                                                  319
         AG_MODULE=MODULE                                                 319
         AG_MARK='V00'                                                    319
         CALL AGSPUSH                                                     319
         AG_MODULE=MODULE                                                 319
C    *                                                                    319
C    Geant ATTRIBUTE : PXTL ===> SEEN=1 COLO=4 <===                       320
         CALL AGSRESET                                                    320
         AG_SEEN = 1                                                      320
         AG_COLO = 4                                                      320
         AG_TITLE = 'PXTL'                                                320
         AG_EXNAME = 'ATTRIBUTE'                                          320
         AG_PARLIST = 'SEEN_COLO'                                         320
         CALL AXATTRIBUTE                                                 320
C    *                                                                    320
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          320
         CALL AGSRESET                                                    321
         AG_TITLE = 'CFRPMIX'                                             321
         AG_EXNAME = 'MATERIAL'                                           321
         AG_PARLIST = 'NONE'                                              321
         CALL AXMATERIAL                                                  321
C    *                                                                    321
C    Geant SHAPE : BOX ===> DX=HALFLADTOPL DY=HALFLADTHK DZ=HALFLADZLEN   321
C    <===                                                                 321
         CALL AGSRESET                                                    322
         AG_DX = HALFLADTOPL                                              322
         AG_DY = HALFLADTHK                                               322
         AG_DZ = HALFLADZLEN                                              322
         AG_TITLE = 'BOX'                                                 322
         AG_EXNAME = 'SHAPE'                                              322
         AG_PARLIST = 'DX_DY_DZ'                                          322
         CALL AXSHAPE                                                     322
C    *                                                                    322
5241  CALL AGSPOPB                                                        323
      GO TO BLOCK_PXTL, (5240 ,5250)                                      323
C End of Local Procedure                                                  323
C ----------------------------------------------------------------------  324
C ------------ PXTJ                                                       324
C Begin Local Procedure PXTJ                                              325
C PXTJ PART JOINING 2 PARTS OF TOP SECTOR                                 325
5260  CONTINUE                                                            325
         AG_TITLE='PXTJ'                                                  325
         AG_MODULE=MODULE                                                 325
         AG_MARK='V00'                                                    325
         CALL AGSPUSH                                                     325
         AG_MODULE=MODULE                                                 325
C    *                                                                    325
C    Geant ATTRIBUTE : PXTJ ===> SEEN=1 COLO=4 <===                       326
         CALL AGSRESET                                                    326
         AG_SEEN = 1                                                      326
         AG_COLO = 4                                                      326
         AG_TITLE = 'PXTJ'                                                326
         AG_EXNAME = 'ATTRIBUTE'                                          326
         AG_PARLIST = 'SEEN_COLO'                                         326
         CALL AXATTRIBUTE                                                 326
C    *                                                                    326
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          326
         CALL AGSRESET                                                    327
         AG_TITLE = 'CFRPMIX'                                             327
         AG_EXNAME = 'MATERIAL'                                           327
         AG_PARLIST = 'NONE'                                              327
         CALL AXMATERIAL                                                  327
C    *                                                                    327
C    Geant SHAPE : BOX ===> DX=HALFLADMID DY=HALFLADTHK DZ=HALFLADZLEN <  327
C    ===                                                                  327
         CALL AGSRESET                                                    328
         AG_DX = HALFLADMID                                               328
         AG_DY = HALFLADTHK                                               328
         AG_DZ = HALFLADZLEN                                              328
         AG_TITLE = 'BOX'                                                 328
         AG_EXNAME = 'SHAPE'                                              328
         AG_PARLIST = 'DX_DY_DZ'                                          328
         CALL AXSHAPE                                                     328
C    *                                                                    328
5261  CALL AGSPOPB                                                        329
      GO TO BLOCK_PXTJ, (5260 ,5270,5280)                                 329
C End of Local Procedure                                                  329
C ----------------------------------------------------------------------  330
C ------------ PXCA                                                       330
C Begin Local Procedure PXCA                                              331
C PXCA ARC BOTTOM RIGHT                                                   331
5290  CONTINUE                                                            331
         AG_TITLE='PXCA'                                                  331
         AG_MODULE=MODULE                                                 331
         AG_MARK='V00'                                                    331
         CALL AGSPUSH                                                     331
         AG_MODULE=MODULE                                                 331
C    *                                                                    331
C    Geant ATTRIBUTE : PXCA ===> SEEN=1 COLO=4 <===                       332
         CALL AGSRESET                                                    332
         AG_SEEN = 1                                                      332
         AG_COLO = 4                                                      332
         AG_TITLE = 'PXCA'                                                332
         AG_EXNAME = 'ATTRIBUTE'                                          332
         AG_PARLIST = 'SEEN_COLO'                                         332
         CALL AXATTRIBUTE                                                 332
C    *                                                                    332
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          332
         CALL AGSRESET                                                    333
         AG_TITLE = 'CFRPMIX'                                             333
         AG_EXNAME = 'MATERIAL'                                           333
         AG_PARLIST = 'NONE'                                              333
         CALL AXMATERIAL                                                  333
C    *                                                                    333
C    Geant SHAPE : TUBS ===> RMIN=ARCTMIN RMAX=ARCTMAX PHI1=267.396, PHI  333
C    2=360 DZ=HALFLADZLEN <===                                            333
         CALL AGSRESET                                                    335
         AG_RMIN = ARCTMIN                                                335
         AG_RMAX = ARCTMAX                                                335
         AG_PHI1 = 267.396                                                335
         AG_PHI2 = 360                                                    335
         AG_DZ = HALFLADZLEN                                              335
         AG_TITLE = 'TUBS'                                                335
         AG_EXNAME = 'SHAPE'                                              335
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            335
         CALL AXSHAPE                                                     335
C    *                                                                    335
5291  CALL AGSPOPB                                                        336
      GO TO BLOCK_PXCA, (5290 ,5300)                                      336
C End of Local Procedure                                                  336
C ----------------------------------------------------------------------  337
C ------------ PXCC                                                       337
C Begin Local Procedure PXCC                                              338
C PXCC ARC TOP RIGHT                                                      338
5310  CONTINUE                                                            338
         AG_TITLE='PXCC'                                                  338
         AG_MODULE=MODULE                                                 338
         AG_MARK='V00'                                                    338
         CALL AGSPUSH                                                     338
         AG_MODULE=MODULE                                                 338
C    *                                                                    338
C    Geant ATTRIBUTE : PXCC ===> SEEN=1 COLO=4 <===                       339
         CALL AGSRESET                                                    339
         AG_SEEN = 1                                                      339
         AG_COLO = 4                                                      339
         AG_TITLE = 'PXCC'                                                339
         AG_EXNAME = 'ATTRIBUTE'                                          339
         AG_PARLIST = 'SEEN_COLO'                                         339
         CALL AXATTRIBUTE                                                 339
C    *                                                                    339
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          339
         CALL AGSRESET                                                    340
         AG_TITLE = 'CFRPMIX'                                             340
         AG_EXNAME = 'MATERIAL'                                           340
         AG_PARLIST = 'NONE'                                              340
         CALL AXMATERIAL                                                  340
C    *                                                                    340
C    Geant SHAPE : TUBS ===> RMIN=ARCTMIN RMAX=ARCTMAX PHI1=0, PHI2=79.9  340
C    63 DZ=HALFLADZLEN <===                                               340
         CALL AGSRESET                                                    342
         AG_RMIN = ARCTMIN                                                342
         AG_RMAX = ARCTMAX                                                342
         AG_PHI1 = 0                                                      342
         AG_PHI2 = 79.963                                                 342
         AG_DZ = HALFLADZLEN                                              342
         AG_TITLE = 'TUBS'                                                342
         AG_EXNAME = 'SHAPE'                                              342
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            342
         CALL AXSHAPE                                                     342
C    *                                                                    342
5311  CALL AGSPOPB                                                        343
      GO TO BLOCK_PXCC, (5310 ,5320)                                      343
C End of Local Procedure                                                  343
C ----------------------------------------------------------------------  344
C ------------ PXCD                                                       344
C Begin Local Procedure PXCD                                              345
C PXCD ARC TOP 23                                                         345
5330  CONTINUE                                                            345
         AG_TITLE='PXCD'                                                  345
         AG_MODULE=MODULE                                                 345
         AG_MARK='V00'                                                    345
         CALL AGSPUSH                                                     345
         AG_MODULE=MODULE                                                 345
C    *                                                                    345
C    Geant ATTRIBUTE : PXCD ===> SEEN=1 COLO=4 <===                       346
         CALL AGSRESET                                                    346
         AG_SEEN = 1                                                      346
         AG_COLO = 4                                                      346
         AG_TITLE = 'PXCD'                                                346
         AG_EXNAME = 'ATTRIBUTE'                                          346
         AG_PARLIST = 'SEEN_COLO'                                         346
         CALL AXATTRIBUTE                                                 346
C    *                                                                    346
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          346
         CALL AGSRESET                                                    347
         AG_TITLE = 'CFRPMIX'                                             347
         AG_EXNAME = 'MATERIAL'                                           347
         AG_PARLIST = 'NONE'                                              347
         CALL AXMATERIAL                                                  347
C    *                                                                    347
C    Geant SHAPE : TUBS ===> RMIN=ARCTMIN RMAX=ARCTMAX PHI1=79.963, PHI2  347
C    =191.54 DZ=HALFLADZLEN <===                                          347
         CALL AGSRESET                                                    349
         AG_RMIN = ARCTMIN                                                349
         AG_RMAX = ARCTMAX                                                349
         AG_PHI1 = 79.963                                                 349
         AG_PHI2 = 191.54                                                 349
         AG_DZ = HALFLADZLEN                                              349
         AG_TITLE = 'TUBS'                                                349
         AG_EXNAME = 'SHAPE'                                              349
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            349
         CALL AXSHAPE                                                     349
C    *                                                                    349
5331  CALL AGSPOPB                                                        350
      GO TO BLOCK_PXCD, (5330 ,5340)                                      350
C End of Local Procedure                                                  350
C ----------------------------------------------------------------------  351
C ------------ PXCE                                                       351
C Begin Local Procedure PXCE                                              352
C PXCE ARC BOTTOM 23                                                      352
5350  CONTINUE                                                            352
         AG_TITLE='PXCE'                                                  352
         AG_MODULE=MODULE                                                 352
         AG_MARK='V00'                                                    352
         CALL AGSPUSH                                                     352
         AG_MODULE=MODULE                                                 352
C    *                                                                    352
C    Geant ATTRIBUTE : PXCE ===> SEEN=1 COLO=4 <===                       353
         CALL AGSRESET                                                    353
         AG_SEEN = 1                                                      353
         AG_COLO = 4                                                      353
         AG_TITLE = 'PXCE'                                                353
         AG_EXNAME = 'ATTRIBUTE'                                          353
         AG_PARLIST = 'SEEN_COLO'                                         353
         CALL AXATTRIBUTE                                                 353
C    *                                                                    353
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          353
         CALL AGSRESET                                                    354
         AG_TITLE = 'CFRPMIX'                                             354
         AG_EXNAME = 'MATERIAL'                                           354
         AG_PARLIST = 'NONE'                                              354
         CALL AXMATERIAL                                                  354
C    *                                                                    354
C    Geant SHAPE : TUBS ===> RMIN=ARCTBMIN RMAX=ARCTBMAX PHI1=271.963, P  354
C    HI2=371.54 DZ=HALFLADZLEN <===                                       354
         CALL AGSRESET                                                    356
         AG_RMIN = ARCTBMIN                                               356
         AG_RMAX = ARCTBMAX                                               356
         AG_PHI1 = 271.963                                                356
         AG_PHI2 = 371.54                                                 356
         AG_DZ = HALFLADZLEN                                              356
         AG_TITLE = 'TUBS'                                                356
         AG_EXNAME = 'SHAPE'                                              356
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            356
         CALL AXSHAPE                                                     356
C    *                                                                    356
5351  CALL AGSPOPB                                                        357
      GO TO BLOCK_PXCE, (5350 ,5360)                                      357
C End of Local Procedure                                                  357
C ----------------------------------------------------------------------  358
C ------------ PXCF                                                       358
C Begin Local Procedure PXCF                                              359
C PXCF ARC TOP 12                                                         359
5370  CONTINUE                                                            359
         AG_TITLE='PXCF'                                                  359
         AG_MODULE=MODULE                                                 359
         AG_MARK='V00'                                                    359
         CALL AGSPUSH                                                     359
         AG_MODULE=MODULE                                                 359
C    *                                                                    359
C    Geant ATTRIBUTE : PXCF ===> SEEN=1 COLO=4 <===                       360
         CALL AGSRESET                                                    360
         AG_SEEN = 1                                                      360
         AG_COLO = 4                                                      360
         AG_TITLE = 'PXCF'                                                360
         AG_EXNAME = 'ATTRIBUTE'                                          360
         AG_PARLIST = 'SEEN_COLO'                                         360
         CALL AXATTRIBUTE                                                 360
C    *                                                                    360
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          360
         CALL AGSRESET                                                    361
         AG_TITLE = 'CFRPMIX'                                             361
         AG_EXNAME = 'MATERIAL'                                           361
         AG_PARLIST = 'NONE'                                              361
         CALL AXMATERIAL                                                  361
C    *                                                                    361
C    Geant SHAPE : TUBS ===> RMIN=ARCTMIN RMAX=ARCTMAX PHI1=91.963, PHI2  361
C    =203.54 DZ=HALFLADZLEN <===                                          361
         CALL AGSRESET                                                    363
         AG_RMIN = ARCTMIN                                                363
         AG_RMAX = ARCTMAX                                                363
         AG_PHI1 = 91.963                                                 363
         AG_PHI2 = 203.54                                                 363
         AG_DZ = HALFLADZLEN                                              363
         AG_TITLE = 'TUBS'                                                363
         AG_EXNAME = 'SHAPE'                                              363
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            363
         CALL AXSHAPE                                                     363
C    *                                                                    363
5371  CALL AGSPOPB                                                        364
      GO TO BLOCK_PXCF, (5370 ,5380)                                      364
C End of Local Procedure                                                  364
C ----------------------------------------------------------------------  365
C ------------ PXCG                                                       365
C Begin Local Procedure PXCG                                              366
C PXCG ARC BOTTOM 12                                                      366
5390  CONTINUE                                                            366
         AG_TITLE='PXCG'                                                  366
         AG_MODULE=MODULE                                                 366
         AG_MARK='V00'                                                    366
         CALL AGSPUSH                                                     366
         AG_MODULE=MODULE                                                 366
C    *                                                                    366
C    Geant ATTRIBUTE : PXCG ===> SEEN=1 COLO=4 <===                       367
         CALL AGSRESET                                                    367
         AG_SEEN = 1                                                      367
         AG_COLO = 4                                                      367
         AG_TITLE = 'PXCG'                                                367
         AG_EXNAME = 'ATTRIBUTE'                                          367
         AG_PARLIST = 'SEEN_COLO'                                         367
         CALL AXATTRIBUTE                                                 367
C    *                                                                    367
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          367
         CALL AGSRESET                                                    368
         AG_TITLE = 'CFRPMIX'                                             368
         AG_EXNAME = 'MATERIAL'                                           368
         AG_PARLIST = 'NONE'                                              368
         CALL AXMATERIAL                                                  368
C    *                                                                    368
C    Geant SHAPE : TUBS ===> RMIN=ARCTBMIN RMAX=ARCTBMAX PHI1=283.963, P  368
C    HI2=383.54 DZ=HALFLADZLEN <===                                       368
         CALL AGSRESET                                                    370
         AG_RMIN = ARCTBMIN                                               370
         AG_RMAX = ARCTBMAX                                               370
         AG_PHI1 = 283.963                                                370
         AG_PHI2 = 383.54                                                 370
         AG_DZ = HALFLADZLEN                                              370
         AG_TITLE = 'TUBS'                                                370
         AG_EXNAME = 'SHAPE'                                              370
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            370
         CALL AXSHAPE                                                     370
C    *                                                                    370
5391  CALL AGSPOPB                                                        371
      GO TO BLOCK_PXCG, (5390 ,5400)                                      371
C End of Local Procedure                                                  371
C ----------------------------------------------------------------------  372
C ------------ PXCH                                                       372
C Begin Local Procedure PXCH                                              373
C PXCH ARC TOP LEFT                                                       373
5410  CONTINUE                                                            373
         AG_TITLE='PXCH'                                                  373
         AG_MODULE=MODULE                                                 373
         AG_MARK='V00'                                                    373
         CALL AGSPUSH                                                     373
         AG_MODULE=MODULE                                                 373
C    *                                                                    373
C    Geant ATTRIBUTE : PXCH ===> SEEN=1 COLO=4 <===                       374
         CALL AGSRESET                                                    374
         AG_SEEN = 1                                                      374
         AG_COLO = 4                                                      374
         AG_TITLE = 'PXCH'                                                374
         AG_EXNAME = 'ATTRIBUTE'                                          374
         AG_PARLIST = 'SEEN_COLO'                                         374
         CALL AXATTRIBUTE                                                 374
C    *                                                                    374
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          374
         CALL AGSRESET                                                    375
         AG_TITLE = 'CFRPMIX'                                             375
         AG_EXNAME = 'MATERIAL'                                           375
         AG_PARLIST = 'NONE'                                              375
         CALL AXMATERIAL                                                  375
C    *                                                                    375
C    Geant SHAPE : TUBS ===> RMIN=ARCTMIN RMAX=ARCTMAX PHI1=103.963, PHI  375
C    2=216 DZ=HALFLADZLEN <===                                            375
         CALL AGSRESET                                                    377
         AG_RMIN = ARCTMIN                                                377
         AG_RMAX = ARCTMAX                                                377
         AG_PHI1 = 103.963                                                377
         AG_PHI2 = 216                                                    377
         AG_DZ = HALFLADZLEN                                              377
         AG_TITLE = 'TUBS'                                                377
         AG_EXNAME = 'SHAPE'                                              377
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            377
         CALL AXSHAPE                                                     377
C    *                                                                    377
5411  CALL AGSPOPB                                                        378
      GO TO BLOCK_PXCH, (5410 ,5420)                                      378
C End of Local Procedure                                                  378
C ----------------------------------------------------------------------  379
C ------------ PXCB                                                       379
C Begin Local Procedure PXCB                                              380
C PXCB ARC BOTTOM LEFT                                                    380
5430  CONTINUE                                                            380
         AG_TITLE='PXCB'                                                  380
         AG_MODULE=MODULE                                                 380
         AG_MARK='V00'                                                    380
         CALL AGSPUSH                                                     380
         AG_MODULE=MODULE                                                 380
C    *                                                                    380
C    Geant ATTRIBUTE : PXCB ===> SEEN=1 COLO=4 <===                       381
         CALL AGSRESET                                                    381
         AG_SEEN = 1                                                      381
         AG_COLO = 4                                                      381
         AG_TITLE = 'PXCB'                                                381
         AG_EXNAME = 'ATTRIBUTE'                                          381
         AG_PARLIST = 'SEEN_COLO'                                         381
         CALL AXATTRIBUTE                                                 381
C    *                                                                    381
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          381
         CALL AGSRESET                                                    382
         AG_TITLE = 'CFRPMIX'                                             382
         AG_EXNAME = 'MATERIAL'                                           382
         AG_PARLIST = 'NONE'                                              382
         CALL AXMATERIAL                                                  382
C    *                                                                    382
C    Geant SHAPE : TUBS ===> RMIN=ARCBMIN RMAX=ARCBMAX PHI1=216, PHI2=26  382
C    7.396 DZ=HALFLADZLEN <===                                            382
         CALL AGSRESET                                                    384
         AG_RMIN = ARCBMIN                                                384
         AG_RMAX = ARCBMAX                                                384
         AG_PHI1 = 216                                                    384
         AG_PHI2 = 267.396                                                384
         AG_DZ = HALFLADZLEN                                              384
         AG_TITLE = 'TUBS'                                                384
         AG_EXNAME = 'SHAPE'                                              384
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            384
         CALL AXSHAPE                                                     384
C    *                                                                    384
5431  CALL AGSPOPB                                                        385
      GO TO BLOCK_PXCB, (5430 ,5440)                                      385
C End of Local Procedure                                                  385
C ----------------------------------------------------------------------  386
C ------------ LADR                                                       386
C Begin Local Procedure LADR                                              387
C LADR FIRST LADDER PROTOTYPE                                             387
5450  CONTINUE                                                            387
         AG_TITLE='LADR'                                                  387
         AG_MODULE=MODULE                                                 387
         AG_MARK='V00'                                                    387
         CALL AGSPUSH                                                     387
         AG_MODULE=MODULE                                                 387
C    *                                                                    387
C    Geant MATERIAL : AIR ===> inherited arguments used <===              388
         CALL AGSRESET                                                    388
         AG_TITLE = 'AIR'                                                 388
         AG_EXNAME = 'MATERIAL'                                           388
         AG_PARLIST = 'NONE'                                              388
         CALL AXMATERIAL                                                  388
C    *                                                                    388
C    Geant ATTRIBUTE : LADR ===> SEEN=0 COLO=4 <===                       388
         CALL AGSRESET                                                    389
         AG_SEEN = 0                                                      389
         AG_COLO = 4                                                      389
         AG_TITLE = 'LADR'                                                389
         AG_EXNAME = 'ATTRIBUTE'                                          389
         AG_PARLIST = 'SEEN_COLO'                                         389
         CALL AXATTRIBUTE                                                 389
C    *                                                                    389
         LADBOXX = HALFPIXAC + .1                                         389
         LADBOXY = HALFPIXTHK + HALFPIXTHKGA + HALFPIXTHKAC +             392
     *   HALFPIXTHKGB + HALFPIXTHKCB + HALFPIXTHKGC + .01                 392
         LADBOXZ = HALFPIXZAC + .1                                        393
C    Geant SHAPE : BOX ===> DX=LADBOXX DY=LADBOXY DZ=LADBOXZ <===         394
         CALL AGSRESET                                                    394
         AG_DX = LADBOXX                                                  394
         AG_DY = LADBOXY                                                  394
         AG_DZ = LADBOXZ                                                  394
         AG_TITLE = 'BOX'                                                 394
         AG_EXNAME = 'SHAPE'                                              394
         AG_PARLIST = 'DX_DY_DZ'                                          394
         CALL AXSHAPE                                                     394
C    *                                                                    394
C    Create PLAC :                                                        395
C    Call To Local Procedure PLAC                                         395
         ASSIGN 5510 TO BLOCK_PLAC                                        395
         GO TO  5500                                                      395
5510     CONTINUE                                                         395
C    *                                                                    395
C    Geant POSITION : PLAC ===> X=-0.2315 Y=-0.0415 Z=5.015 <===          396
         CALL AGSRESET                                                    396
         AG_X = -0.2315                                                   396
         AG_Y = -0.0415                                                   396
         AG_Z = 5.015                                                     396
         AG_TITLE = 'PLAC'                                                396
         AG_EXNAME = 'POSITION'                                           396
         AG_PARLIST = 'X_Y_Z'                                             396
         CALL AXPOSITION                                                  396
C    *                                                                    396
C    Create SIFL :                                                        397
C    Call To Local Procedure SIFL                                         397
         ASSIGN 5530 TO BLOCK_SIFL                                        397
         GO TO  5520                                                      397
5530     CONTINUE                                                         397
C    *                                                                    397
C    Geant POSITION : SIFL ===> X=-1.2065 Y=-0.0415 Z=5.015 <===          398
         CALL AGSRESET                                                    398
         AG_X = -1.2065                                                   398
         AG_Y = -0.0415                                                   398
         AG_Z = 5.015                                                     398
         AG_TITLE = 'SIFL'                                                398
         AG_EXNAME = 'POSITION'                                           398
         AG_PARLIST = 'X_Y_Z'                                             398
         CALL AXPOSITION                                                  398
C    *                                                                    398
C    Create SIFR :                                                        399
C    Call To Local Procedure SIFR                                         399
         ASSIGN 5550 TO BLOCK_SIFR                                        399
         GO TO  5540                                                      399
5550     CONTINUE                                                         399
C    *                                                                    399
C    Geant POSITION : SIFR ===> X=0.8975 Y=-0.0415 Z=5.015 <===           400
         CALL AGSRESET                                                    400
         AG_X = 0.8975                                                    400
         AG_Y = -0.0415                                                   400
         AG_Z = 5.015                                                     400
         AG_TITLE = 'SIFR'                                                400
         AG_EXNAME = 'POSITION'                                           400
         AG_PARLIST = 'X_Y_Z'                                             400
         CALL AXPOSITION                                                  400
C    *                                                                    400
C    Create GLUA :                                                        401
C    Call To Local Procedure GLUA                                         401
         ASSIGN 5570 TO BLOCK_GLUA                                        401
         GO TO  5560                                                      401
5570     CONTINUE                                                         401
C    *                                                                    401
C    Geant POSITION : GLUA ===> X=-0.0775 Y=-0.0365 Z=5.015 <===          402
         CALL AGSRESET                                                    402
         AG_X = -0.0775                                                   402
         AG_Y = -0.0365                                                   402
         AG_Z = 5.015                                                     402
         AG_TITLE = 'GLUA'                                                402
         AG_EXNAME = 'POSITION'                                           402
         AG_PARLIST = 'X_Y_Z'                                             402
         CALL AXPOSITION                                                  402
C    *                                                                    402
C    Create ALCA :                                                        403
C    Call To Local Procedure ALCA                                         403
         ASSIGN 5590 TO BLOCK_ALCA                                        403
         GO TO  5580                                                      403
5590     CONTINUE                                                         403
C    *                                                                    403
C    Geant POSITION : ALCA ===> X=0 Y=-0.0255 Z=0 <===                    404
         CALL AGSRESET                                                    404
         AG_X = 0                                                         404
         AG_Y = -0.0255                                                   404
         AG_Z = 0                                                         404
         AG_TITLE = 'ALCA'                                                404
         AG_EXNAME = 'POSITION'                                           404
         AG_PARLIST = 'X_Y_Z'                                             404
         CALL AXPOSITION                                                  404
C    *                                                                    404
C    Create GLUB :                                                        405
C    Call To Local Procedure GLUB                                         405
         ASSIGN 5610 TO BLOCK_GLUB                                        405
         GO TO  5600                                                      405
5610     CONTINUE                                                         405
C    *                                                                    405
C    Geant POSITION : GLUB ===> X=-0.244 Y=-0.0145 Z=0 <===               406
         CALL AGSRESET                                                    406
         AG_X = -0.244                                                    406
         AG_Y = -0.0145                                                   406
         AG_Z = 0                                                         406
         AG_TITLE = 'GLUB'                                                406
         AG_EXNAME = 'POSITION'                                           406
         AG_PARLIST = 'X_Y_Z'                                             406
         CALL AXPOSITION                                                  406
C    *                                                                    406
C    Create CFBK :                                                        407
C    Call To Local Procedure CFBK                                         407
         ASSIGN 5630 TO BLOCK_CFBK                                        407
         GO TO  5620                                                      407
5630     CONTINUE                                                         407
C    *                                                                    407
C    Geant POSITION : CFBK ===> X=-0.244 Y=0.008 Z=0 <===                 408
         CALL AGSRESET                                                    408
         AG_X = -0.244                                                    408
         AG_Y = 0.008                                                     408
         AG_Z = 0                                                         408
         AG_TITLE = 'CFBK'                                                408
         AG_EXNAME = 'POSITION'                                           408
         AG_PARLIST = 'X_Y_Z'                                             408
         CALL AXPOSITION                                                  408
C    *                                                                    408
C    Create GLUC :                                                        409
C    Call To Local Procedure GLUC                                         409
         ASSIGN 5650 TO BLOCK_GLUC                                        409
         GO TO  5640                                                      409
5650     CONTINUE                                                         409
C    *                                                                    409
C    Geant POSITION : GLUC ===> X=-0.5015 Y=0.033 Z=0 <===                410
         CALL AGSRESET                                                    410
         AG_X = -0.5015                                                   410
         AG_Y = 0.033                                                     410
         AG_Z = 0                                                         410
         AG_TITLE = 'GLUC'                                                410
         AG_EXNAME = 'POSITION'                                           410
         AG_PARLIST = 'X_Y_Z'                                             410
         CALL AXPOSITION                                                  410
C    *                                                                    410
5451  CALL AGSPOPB                                                        411
      GO TO BLOCK_LADR, (5450 ,5460,5470,5480,5490)                       411
C End of Local Procedure                                                  411
C ----------------------------------------------------------------------  412
C ------------ SIFL                                                       412
C Begin Local Procedure SIFL                                              413
C SIFL PASSIVE SILICON LAYER                                              413
5520  CONTINUE                                                            413
         AG_TITLE='SIFL'                                                  413
         AG_MODULE=MODULE                                                 413
         AG_MARK='V00'                                                    413
         CALL AGSPUSH                                                     413
         AG_MODULE=MODULE                                                 413
C    *                                                                    413
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          414
         CALL AGSRESET                                                    414
         AG_TITLE = 'SILICON'                                             414
         AG_EXNAME = 'MATERIAL'                                           414
         AG_PARLIST = 'NONE'                                              414
         CALL AXMATERIAL                                                  414
C    *                                                                    414
C    Geant MATERIAL : PASSIVESILICON ===> ISVOL=0 <===                    414
         CALL AGSRESET                                                    415
         AG_ISVOL = 0                                                     415
         AG_TITLE = 'PASSIVESILICON'                                      415
         AG_EXNAME = 'MATERIAL'                                           415
         AG_PARLIST = 'ISVOL'                                             415
         CALL AXMATERIAL                                                  415
C    *                                                                    415
C    Geant ATTRIBUTE : SIFL ===> SEEN=1 COLO=2 <===                       415
         CALL AGSRESET                                                    416
         AG_SEEN = 1                                                      416
         AG_COLO = 2                                                      416
         AG_TITLE = 'SIFL'                                                416
         AG_EXNAME = 'ATTRIBUTE'                                          416
         AG_PARLIST = 'SEEN_COLO'                                         416
         CALL AXATTRIBUTE                                                 416
C    *                                                                    416
C    Geant SHAPE : BOX ===> DX=HALFPIXPRL DY=HALFPIXTHKPL, DZ=HALFPIXPZL  416
C     <===                                                                416
         CALL AGSRESET                                                    418
         AG_DX = HALFPIXPRL                                               418
         AG_DY = HALFPIXTHKPL                                             418
         AG_DZ = HALFPIXPZL                                               418
         AG_TITLE = 'BOX'                                                 418
         AG_EXNAME = 'SHAPE'                                              418
         AG_PARLIST = 'DX_DY_DZ'                                          418
         CALL AXSHAPE                                                     418
C    *                                                                    418
5521  CALL AGSPOPB                                                        419
      GO TO BLOCK_SIFL, (5520 ,5530)                                      419
C End of Local Procedure                                                  419
C ----------------------------------------------------------------------  420
C ------------ SIFR                                                       420
C Begin Local Procedure SIFR                                              421
C SIFR PASSIVE SILICON LAYER                                              421
5540  CONTINUE                                                            421
         AG_TITLE='SIFR'                                                  421
         AG_MODULE=MODULE                                                 421
         AG_MARK='V00'                                                    421
         CALL AGSPUSH                                                     421
         AG_MODULE=MODULE                                                 421
C    *                                                                    421
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          422
         CALL AGSRESET                                                    422
         AG_TITLE = 'SILICON'                                             422
         AG_EXNAME = 'MATERIAL'                                           422
         AG_PARLIST = 'NONE'                                              422
         CALL AXMATERIAL                                                  422
C    *                                                                    422
C    Geant MATERIAL : PASSIVESILICON ===> ISVOL=0 <===                    422
         CALL AGSRESET                                                    423
         AG_ISVOL = 0                                                     423
         AG_TITLE = 'PASSIVESILICON'                                      423
         AG_EXNAME = 'MATERIAL'                                           423
         AG_PARLIST = 'ISVOL'                                             423
         CALL AXMATERIAL                                                  423
C    *                                                                    423
C    Geant ATTRIBUTE : SIFR ===> SEEN=1 COLO=2 <===                       423
         CALL AGSRESET                                                    424
         AG_SEEN = 1                                                      424
         AG_COLO = 2                                                      424
         AG_TITLE = 'SIFR'                                                424
         AG_EXNAME = 'ATTRIBUTE'                                          424
         AG_PARLIST = 'SEEN_COLO'                                         424
         CALL AXATTRIBUTE                                                 424
C    *                                                                    424
C    Geant SHAPE : BOX ===> DX=HALFPIXPRR DY=HALFPIXTHKPR, DZ=HALFPIXPZR  424
C     <===                                                                424
         CALL AGSRESET                                                    426
         AG_DX = HALFPIXPRR                                               426
         AG_DY = HALFPIXTHKPR                                             426
         AG_DZ = HALFPIXPZR                                               426
         AG_TITLE = 'BOX'                                                 426
         AG_EXNAME = 'SHAPE'                                              426
         AG_PARLIST = 'DX_DY_DZ'                                          426
         CALL AXSHAPE                                                     426
C    *                                                                    426
5541  CALL AGSPOPB                                                        427
      GO TO BLOCK_SIFR, (5540 ,5550)                                      427
C End of Local Procedure                                                  427
C ----------------------------------------------------------------------  428
C ------------ ALCA                                                       428
C Begin Local Procedure ALCA                                              429
C ALCA ALUMINIUM CABLE                                                    429
5580  CONTINUE                                                            429
         AG_TITLE='ALCA'                                                  429
         AG_MODULE=MODULE                                                 429
         AG_MARK='V00'                                                    429
         CALL AGSPUSH                                                     429
         AG_MODULE=MODULE                                                 429
C    *                                                                    429
C    Geant MATERIAL : ALCAMIX ===> inherited arguments used <===          430
         CALL AGSRESET                                                    430
         AG_TITLE = 'ALCAMIX'                                             430
         AG_EXNAME = 'MATERIAL'                                           430
         AG_PARLIST = 'NONE'                                              430
         CALL AXMATERIAL                                                  430
C    *                                                                    430
C    Geant ATTRIBUTE : ALCA ===> SEEN=1 COLO=3 <===                       430
         CALL AGSRESET                                                    431
         AG_SEEN = 1                                                      431
         AG_COLO = 3                                                      431
         AG_TITLE = 'ALCA'                                                431
         AG_EXNAME = 'ATTRIBUTE'                                          431
         AG_PARLIST = 'SEEN_COLO'                                         431
         CALL AXATTRIBUTE                                                 431
C    *                                                                    431
C    Geant SHAPE : BOX ===> DX=HALFPIXAC DY=HALFPIXTHKAC DZ=HALFPIXZAC <  431
C    ===                                                                  431
         CALL AGSRESET                                                    432
         AG_DX = HALFPIXAC                                                432
         AG_DY = HALFPIXTHKAC                                             432
         AG_DZ = HALFPIXZAC                                               432
         AG_TITLE = 'BOX'                                                 432
         AG_EXNAME = 'SHAPE'                                              432
         AG_PARLIST = 'DX_DY_DZ'                                          432
         CALL AXSHAPE                                                     432
C    *                                                                    432
5581  CALL AGSPOPB                                                        433
      GO TO BLOCK_ALCA, (5580 ,5590)                                      433
C End of Local Procedure                                                  433
C ----------------------------------------------------------------------  434
C ------------ GLUA                                                       434
C Begin Local Procedure GLUA                                              435
C GLUA GLUE LAYER A                                                       435
5560  CONTINUE                                                            435
         AG_TITLE='GLUA'                                                  435
         AG_MODULE=MODULE                                                 435
         AG_MARK='V00'                                                    435
         CALL AGSPUSH                                                     435
         AG_MODULE=MODULE                                                 435
C    *                                                                    435
C    Geant MATERIAL : GLUMIX ===> inherited arguments used <===           436
         CALL AGSRESET                                                    436
         AG_TITLE = 'GLUMIX'                                              436
         AG_EXNAME = 'MATERIAL'                                           436
         AG_PARLIST = 'NONE'                                              436
         CALL AXMATERIAL                                                  436
C    *                                                                    436
C    Geant ATTRIBUTE : GLUA ===> SEEN=1 COLO=5 <===                       436
         CALL AGSRESET                                                    437
         AG_SEEN = 1                                                      437
         AG_COLO = 5                                                      437
         AG_TITLE = 'GLUA'                                                437
         AG_EXNAME = 'ATTRIBUTE'                                          437
         AG_PARLIST = 'SEEN_COLO'                                         437
         CALL AXATTRIBUTE                                                 437
C    *                                                                    437
C    Geant SHAPE : BOX ===> DX=HALFPIXGA DY=HALFPIXTHKGA DZ=HALFPIXZGA <  437
C    ===                                                                  437
         CALL AGSRESET                                                    438
         AG_DX = HALFPIXGA                                                438
         AG_DY = HALFPIXTHKGA                                             438
         AG_DZ = HALFPIXZGA                                               438
         AG_TITLE = 'BOX'                                                 438
         AG_EXNAME = 'SHAPE'                                              438
         AG_PARLIST = 'DX_DY_DZ'                                          438
         CALL AXSHAPE                                                     438
C    *                                                                    438
5561  CALL AGSPOPB                                                        439
      GO TO BLOCK_GLUA, (5560 ,5570)                                      439
C End of Local Procedure                                                  439
C ----------------------------------------------------------------------  440
C ------------ GLUB                                                       440
C Begin Local Procedure GLUB                                              441
C GLUB GLUE LAYER B                                                       441
5600  CONTINUE                                                            441
         AG_TITLE='GLUB'                                                  441
         AG_MODULE=MODULE                                                 441
         AG_MARK='V00'                                                    441
         CALL AGSPUSH                                                     441
         AG_MODULE=MODULE                                                 441
C    *                                                                    441
C    Geant MATERIAL : GLUMIX ===> inherited arguments used <===           442
         CALL AGSRESET                                                    442
         AG_TITLE = 'GLUMIX'                                              442
         AG_EXNAME = 'MATERIAL'                                           442
         AG_PARLIST = 'NONE'                                              442
         CALL AXMATERIAL                                                  442
C    *                                                                    442
C    Geant ATTRIBUTE : GLUB ===> SEEN=1 COLO=5 <===                       442
         CALL AGSRESET                                                    443
         AG_SEEN = 1                                                      443
         AG_COLO = 5                                                      443
         AG_TITLE = 'GLUB'                                                443
         AG_EXNAME = 'ATTRIBUTE'                                          443
         AG_PARLIST = 'SEEN_COLO'                                         443
         CALL AXATTRIBUTE                                                 443
C    *                                                                    443
C    Geant SHAPE : BOX ===> DX=HALFPIXGB DY=HALFPIXTHKGB DZ=HALFPIXZGB <  443
C    ===                                                                  443
         CALL AGSRESET                                                    444
         AG_DX = HALFPIXGB                                                444
         AG_DY = HALFPIXTHKGB                                             444
         AG_DZ = HALFPIXZGB                                               444
         AG_TITLE = 'BOX'                                                 444
         AG_EXNAME = 'SHAPE'                                              444
         AG_PARLIST = 'DX_DY_DZ'                                          444
         CALL AXSHAPE                                                     444
C    *                                                                    444
5601  CALL AGSPOPB                                                        445
      GO TO BLOCK_GLUB, (5600 ,5610)                                      445
C End of Local Procedure                                                  445
C ----------------------------------------------------------------------  446
C ------------ CFBK                                                       446
C Begin Local Procedure CFBK                                              447
C CFBK CARBON FIBER BACKING                                               447
5620  CONTINUE                                                            447
         AG_TITLE='CFBK'                                                  447
         AG_MODULE=MODULE                                                 447
         AG_MARK='V00'                                                    447
         CALL AGSPUSH                                                     447
         AG_MODULE=MODULE                                                 447
C    *                                                                    447
C    Geant MATERIAL : CBMIX ===> inherited arguments used <===            448
         CALL AGSRESET                                                    448
         AG_TITLE = 'CBMIX'                                               448
         AG_EXNAME = 'MATERIAL'                                           448
         AG_PARLIST = 'NONE'                                              448
         CALL AXMATERIAL                                                  448
C    *                                                                    448
C    Geant ATTRIBUTE : CFBK ===> SEEN=1 COLO=3 <===                       448
         CALL AGSRESET                                                    449
         AG_SEEN = 1                                                      449
         AG_COLO = 3                                                      449
         AG_TITLE = 'CFBK'                                                449
         AG_EXNAME = 'ATTRIBUTE'                                          449
         AG_PARLIST = 'SEEN_COLO'                                         449
         CALL AXATTRIBUTE                                                 449
C    *                                                                    449
C    Geant SHAPE : BOX ===> DX=HALFPIXCB DY=HALFPIXTHKCB DZ=HALFPIXZCB <  449
C    ===                                                                  449
         CALL AGSRESET                                                    450
         AG_DX = HALFPIXCB                                                450
         AG_DY = HALFPIXTHKCB                                             450
         AG_DZ = HALFPIXZCB                                               450
         AG_TITLE = 'BOX'                                                 450
         AG_EXNAME = 'SHAPE'                                              450
         AG_PARLIST = 'DX_DY_DZ'                                          450
         CALL AXSHAPE                                                     450
C    *                                                                    450
5621  CALL AGSPOPB                                                        451
      GO TO BLOCK_CFBK, (5620 ,5630)                                      451
C End of Local Procedure                                                  451
C ----------------------------------------------------------------------  452
C ------------ GLUC                                                       452
C Begin Local Procedure GLUC                                              453
C GLUC GLUE LAYER C                                                       453
5640  CONTINUE                                                            453
         AG_TITLE='GLUC'                                                  453
         AG_MODULE=MODULE                                                 453
         AG_MARK='V00'                                                    453
         CALL AGSPUSH                                                     453
         AG_MODULE=MODULE                                                 453
C    *                                                                    453
C    Geant MATERIAL : GLUMIX ===> inherited arguments used <===           454
         CALL AGSRESET                                                    454
         AG_TITLE = 'GLUMIX'                                              454
         AG_EXNAME = 'MATERIAL'                                           454
         AG_PARLIST = 'NONE'                                              454
         CALL AXMATERIAL                                                  454
C    *                                                                    454
C    Geant ATTRIBUTE : GLUC ===> SEEN=1 COLO=5 <===                       454
         CALL AGSRESET                                                    455
         AG_SEEN = 1                                                      455
         AG_COLO = 5                                                      455
         AG_TITLE = 'GLUC'                                                455
         AG_EXNAME = 'ATTRIBUTE'                                          455
         AG_PARLIST = 'SEEN_COLO'                                         455
         CALL AXATTRIBUTE                                                 455
C    *                                                                    455
C    Geant SHAPE : BOX ===> DX=HALFPIXGC DY=HALFPIXTHKGC DZ=HALFPIXZGC <  455
C    ===                                                                  455
         CALL AGSRESET                                                    456
         AG_DX = HALFPIXGC                                                456
         AG_DY = HALFPIXTHKGC                                             456
         AG_DZ = HALFPIXZGC                                               456
         AG_TITLE = 'BOX'                                                 456
         AG_EXNAME = 'SHAPE'                                              456
         AG_PARLIST = 'DX_DY_DZ'                                          456
         CALL AXSHAPE                                                     456
C    *                                                                    456
5641  CALL AGSPOPB                                                        457
      GO TO BLOCK_GLUC, (5640 ,5650)                                      457
C End of Local Procedure                                                  457
C ----------------------------------------------------------------------  458
C ------------ PLAC                                                       458
C Begin Local Procedure PLAC                                              459
C PLAC ACTIVE SILICON TOP SECTOR                                          459
5500  CONTINUE                                                            459
         AG_TITLE='PLAC'                                                  459
         AG_MODULE=MODULE                                                 459
         AG_MARK='V00'                                                    459
         CALL AGSPUSH                                                     459
         AG_MODULE=MODULE                                                 459
C    *                                                                    459
C    Geant ATTRIBUTE : PLAC ===> SEEN=1 COLO=6 <===                       460
         CALL AGSRESET                                                    460
         AG_SEEN = 1                                                      460
         AG_COLO = 6                                                      460
         AG_TITLE = 'PLAC'                                                460
         AG_EXNAME = 'ATTRIBUTE'                                          460
         AG_PARLIST = 'SEEN_COLO'                                         460
         CALL AXATTRIBUTE                                                 460
C    *                                                                    460
C    Geant MATERIAL : SILICON ===> inherited arguments used <===          460
         CALL AGSRESET                                                    461
         AG_TITLE = 'SILICON'                                             461
         AG_EXNAME = 'MATERIAL'                                           461
         AG_PARLIST = 'NONE'                                              461
         CALL AXMATERIAL                                                  461
C    *                                                                    461
C    Geant MATERIAL : SSENSITIVE ===> ISVOL=1 <===                        461
         CALL AGSRESET                                                    462
         AG_ISVOL = 1                                                     462
         AG_TITLE = 'SSENSITIVE'                                          462
         AG_EXNAME = 'MATERIAL'                                           462
         AG_PARLIST = 'ISVOL'                                             462
         CALL AXMATERIAL                                                  462
C    *                                                                    462
C    Geant SHAPE : BOX ===> DX=HALFPIXR DY=HALFPIXTHK DZ=HALFPIXZ <===    462
         CALL AGSRESET                                                    463
         AG_DX = HALFPIXR                                                 463
         AG_DY = HALFPIXTHK                                               463
         AG_DZ = HALFPIXZ                                                 463
         AG_TITLE = 'BOX'                                                 463
         AG_EXNAME = 'SHAPE'                                              463
         AG_PARLIST = 'DX_DY_DZ'                                          463
         CALL AXSHAPE                                                     463
C    *                                                                    463
C    Defining Hits: Z:.00001:S Y:.00001: X:.00001: PTOT:16:(0,100) CX:10  463
C    : CY:10: CZ:10:, SLENG:16:(0,500) TOF:16: STEP:.01: ELOSS:16:        463
         AG_TITLE = 'PLAC'                                                465
         AG_EXNAME = 'HITS'                                               465
         AG_TYPE=AG_MAGIC                                                 465
         CALL AGSDETV('PLAC')                                             465
C    *                                                                    465
         AG_PARLIST='Z'                                                   465
         AG_TITLE = 'DETH'                                                465
         AG_NBITS = AG_MAGIC                                              465
         AG_BIN=.00001                                                    465
         AG_OPTION='S'                                                    465
         AG_FHMIN = AG_MAGIC                                              465
         AG_FHMAX = AG_MAGIC                                              465
         CALL AGSDETH (0)                                                 465
C    *                                                                    465
         AG_PARLIST='Y'                                                   465
         AG_TITLE = 'DETH'                                                465
         AG_NBITS = AG_MAGIC                                              465
         AG_BIN=.00001                                                    465
         AG_FHMIN=AG_MAGIC                                                465
         AG_FHMAX=AG_MAGIC                                                465
         AG_OPTION='*'                                                    465
         CALL AGSDETH (0)                                                 465
C    *                                                                    465
         AG_PARLIST='X'                                                   465
         AG_TITLE = 'DETH'                                                465
         AG_NBITS = AG_MAGIC                                              465
         AG_BIN=.00001                                                    465
         AG_FHMIN=AG_MAGIC                                                465
         AG_FHMAX=AG_MAGIC                                                465
         AG_OPTION='*'                                                    465
         CALL AGSDETH (0)                                                 465
C    *                                                                    465
         AG_PARLIST='PTOT'                                                465
         AG_TITLE = 'DETH'                                                465
         AG_NBITS = 16                                                    465
         AG_BIN=AG_MAGIC                                                  465
         AG_OPTION='*'                                                    465
         AG_FHMIN = 0                                                     465
         AG_FHMAX = 100                                                   465
         CALL AGSDETH (0)                                                 465
C    *                                                                    465
         AG_PARLIST='CX'                                                  465
         AG_TITLE = 'DETH'                                                465
         AG_NBITS = 10                                                    465
         AG_BIN=AG_MAGIC                                                  465
         AG_FHMIN=AG_MAGIC                                                465
         AG_FHMAX=AG_MAGIC                                                465
         AG_OPTION='*'                                                    465
         CALL AGSDETH (0)                                                 465
C    *                                                                    465
         AG_PARLIST='CY'                                                  465
         AG_TITLE = 'DETH'                                                465
         AG_NBITS = 10                                                    465
         AG_BIN=AG_MAGIC                                                  465
         AG_FHMIN=AG_MAGIC                                                465
         AG_FHMAX=AG_MAGIC                                                465
         AG_OPTION='*'                                                    465
         CALL AGSDETH (0)                                                 465
C    *                                                                    465
         AG_PARLIST='CZ'                                                  465
         AG_TITLE = 'DETH'                                                465
         AG_NBITS = 10                                                    465
         AG_BIN=AG_MAGIC                                                  465
         AG_OPTION='*'                                                    465
         AG_FHMIN = AG_MAGIC                                              465
         AG_FHMAX = AG_MAGIC                                              465
         CALL AGSDETH (0)                                                 465
C    *                                                                    465
         AG_PARLIST='SLENG'                                               465
         AG_TITLE = 'DETH'                                                465
         AG_NBITS = 16                                                    465
         AG_BIN=AG_MAGIC                                                  465
         AG_OPTION='*'                                                    465
         AG_FHMIN = 0                                                     465
         AG_FHMAX = 500                                                   465
         CALL AGSDETH (0)                                                 465
C    *                                                                    465
         AG_PARLIST='TOF'                                                 465
         AG_TITLE = 'DETH'                                                465
         AG_NBITS = 16                                                    465
         AG_BIN=AG_MAGIC                                                  465
         AG_FHMIN=AG_MAGIC                                                465
         AG_FHMAX=AG_MAGIC                                                465
         AG_OPTION='*'                                                    465
         CALL AGSDETH (0)                                                 465
C    *                                                                    465
         AG_PARLIST='STEP'                                                465
         AG_TITLE = 'DETH'                                                465
         AG_NBITS = AG_MAGIC                                              465
         AG_BIN=.01                                                       465
         AG_FHMIN=AG_MAGIC                                                465
         AG_FHMAX=AG_MAGIC                                                465
         AG_OPTION='*'                                                    465
         CALL AGSDETH (0)                                                 465
C    *                                                                    465
         AG_PARLIST='ELOSS'                                               465
         AG_TITLE = 'DETH'                                                465
         AG_NBITS = 16                                                    465
         AG_BIN=AG_MAGIC                                                  465
         AG_FHMIN=AG_MAGIC                                                465
         AG_FHMAX=AG_MAGIC                                                465
         AG_OPTION='*'                                                    465
         CALL AGSDETH (0)                                                 465
C    *                                                                    465
         CALL AGSDETU('PLAC')                                             465
C    *                                                                    465
5501  CALL AGSPOPB                                                        466
      GO TO BLOCK_PLAC, (5500 ,5510)                                      466
C End of Local Procedure                                                  466
      WRITE(LOUT,5660)'===>>  [End AgML Module PixlGeo4]  <<==='          466
5660  FORMAT(A32,/,/)                                                     466
C MODULE PIXLGEO4                                                         468
      END                                                                 468
