C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE TPCXGEO1                                                 1
C TPCXGEO1 IS TPC INNER PADROW UPGRADE MODULE                             1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'TPCXGEO1'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'IS TPC INNER PADROW UPGRADE MODULE'/                 1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C AUTHOR HAO QUI                                                          1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'HAO QUI'/                                     2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED MARCH 2012                                                      2
      CHARACTER*40 CREATED /'MARCH 2012'/                                 3
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
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            5
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      5
      COMMON/GCMAIL/ CHMAIL                                               5
      CHARACTER*132 CHMAIL                                                5
C - Natural constants (or considered as such)                             5
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   6
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        6
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       6
      CHARACTER*4 VOLUNAME                                                6
      REAL TSAS_ZOFFSET/0.0/, TSGT_ZOFFSET/0.0/                           9
      INTEGER BLOCK_TPCE                                                  16
      INTEGER BLOCK_TPCM                                                  16
      INTEGER BLOCK_TIFC                                                  16
      INTEGER BLOCK_TIKA                                                  16
      INTEGER BLOCK_TINX                                                  16
      INTEGER BLOCK_TIAD                                                  16
      INTEGER BLOCK_TOFC                                                  16
      INTEGER BLOCK_TOFS                                                  16
      INTEGER BLOCK_TOKA                                                  16
      INTEGER BLOCK_TONX                                                  16
      INTEGER BLOCK_TOAD                                                  16
      INTEGER BLOCK_TOIG                                                  16
      INTEGER BLOCK_TOHA                                                  16
      INTEGER BLOCK_TSWH                                                  16
      INTEGER BLOCK_TSAW                                                  16
      INTEGER BLOCK_TSAS                                                  16
      INTEGER BLOCK_TWAS                                                  16
      INTEGER BLOCK_FEEA                                                  16
      INTEGER BLOCK_FEEP                                                  16
      INTEGER BLOCK_FEER                                                  16
      INTEGER BLOCK_FEEI                                                  16
      INTEGER BLOCK_TALS                                                  16
      INTEGER BLOCK_TSGT                                                  16
      INTEGER BLOCK_TWBT                                                  16
      INTEGER BLOCK_TWRI                                                  16
      INTEGER BLOCK_TWRG                                                  16
      INTEGER BLOCK_TWRC                                                  16
      INTEGER BLOCK_TWTR                                                  16
      INTEGER BLOCK_TWMR                                                  16
      INTEGER BLOCK_TRDO                                                  16
      INTEGER BLOCK_TBRW                                                  16
      INTEGER BLOCK_TWRB                                                  16
      INTEGER BLOCK_TCOO                                                  16
      INTEGER BLOCK_TCAB                                                  16
      INTEGER BLOCK_TRDC                                                  16
      INTEGER BLOCK_TRIB                                                  16
      INTEGER BLOCK_TWIR                                                  16
      INTEGER BLOCK_THOL                                                  16
      INTEGER BLOCK_FEES                                                  16
      INTEGER BLOCK_TPGV                                                  16
      INTEGER BLOCK_TPSS                                                  16
      INTEGER BLOCK_TPAD                                                  16
      INTEGER KINNER,KOUTER                                               17
      PARAMETER (KINNER=0)                                                19
      PARAMETER (KOUTER=1)                                                20
      REAL INCH,CM                                                        21
      PARAMETER (INCH = 2.54)                                             23
      PARAMETER (CM = 1.)                                                 24
      INTEGER KBLACK,KRED,KGREEN,KBLUE,KYELLOW,KVIOLET,KLIGHTBLUE         25
      INTEGER KBBOX,KPARA,KTRD1,ISHAPE                                    27
      PARAMETER (KBLACK=1)                                                29
      PARAMETER (KRED=2)                                                  30
      PARAMETER (KGREEN=3)                                                31
      PARAMETER (KBLUE=4)                                                 32
      PARAMETER (KYELLOW=5)                                               33
      PARAMETER (KVIOLET=6)                                               34
      PARAMETER (KLIGHTBLUE=7)                                            35
      PARAMETER (KBBOX=0)                                                 36
      PARAMETER (KPARA=1)                                                 37
      PARAMETER (KTRD1=2)                                                 38
      INTEGER *4 MYCOUNT/0/                                               39
      REAL SIND,COSD,TAND                                                 41
      REAL COS15,SIN15,TAN15                                              43
      PARAMETER (COS15 = .965925826289068312)                             45
      PARAMETER (SIN15 = .258819045102520739)                             46
      PARAMETER (TAN15 = .267949192431122696)                             47
      INTEGER I,J,ISECANG,NCOUNT/0/,JTRIB,KASE                            48
      REAL X,X0,X1,X2,DX,DX1,DX2,XC                                       50
      REAL Y,Y0,Y1,Y2,DY,DY1,DY2                                          52
      REAL Z,Z0,Z1,Z2,DZ,DZ1,DZ2                                          54
      REAL ZBEG,ZDED,ZPMT,ZEND                                            56
      REAL R,R0,R1,R2,DR                                                  58
      REAL ALPHA,BETA                                                     60
      REAL XW,DXW,YW,DYW,ZW,DZW,DYDX                                      62
      REAL DRSECTORSHIFT/0/                                               64
      REAL MYDX,MYDY,MYDZ,MYALPH,MYTHET,MYPHI,MYPAR(20)                   66
      REAL TOCSIR,TOCSOR,TOKAIR,TOKAOR,TONXIR,TONXOR,TOADIR,TOADOR,       68
     *TOIGIR,TOIGOR                                                       68
      REAL TOALIR,TOALOR,TOHAIR,TOHAOR,TOALIR2,TOALOR2,TOFCIR,TOFCOR,     70
     *TOFSIR,TOFSOR                                                       70
      REAL TIFCIR,TIALIR,TIALOR,TIKAIR,TIKAOR,TINXIR,TINXOR,TIADIR,       72
     *TIADOR,TIFCOR                                                       72
      REAL TPGVLENG,TOFCLENG,TPGVZ                                        74
      INTEGER I_SEC,I_ROW                                                 76
      EXTERNAL TPADSTEP                                                   78
      EXTERNAL TPAISTEP                                                   79
      EXTERNAL TPAOSTEP                                                   80
      EXTERNAL TPCELASER                                                  81
      REAL TPGVIR                                                         82
      REAL DEL                                                            84
      REAL ZWHEEL1,ZTOFCEND,ZTIFCEND,ZTIFCFLANGEBEGIN                     86
      REAL QWE(10)                                                        88
      REAL XHOLES(0:15)                                                   90
      REAL WIDTH1IOF(2),WIDTH2IOF(2),HEIGHTIOF(2),DEPTHIOF(2)             92
      INTEGER INOUT                                                       94
      REAL ZZZSA(2)                                                       96
      REAL RMNSA(0:1)                                                     98
      REAL RMXSA(0:1)                                                     100
      DATA ZZZSA/207.4,219.1/                                             102
      DATA RMNSA/51,121.07/                                               103
      DATA RMXSA/121.07,194/                                              104
      REAL ZZZWA(2)                                                       105
      REAL RMNWA(0:1)                                                     107
      REAL RMXWA(0:1)                                                     109
      DATA ZZZWA/211.4,229.2/                                             111
      DATA RMNWA/54,121/                                                  112
      DATA RMXWA/121,189/                                                 113
      REAL ZZZRDO(2)                                                      114
      REAL RMNRDO(2)                                                      116
      REAL RMXRDO(2)                                                      118
      DATA ZZZRDO/232.,252./                                              120
      DATA RMNRDO/88.,88./                                                121
      DATA RMXRDO/198.,198./                                              122
      REAL MYCOS,MYRMIN,MYRMAX,MYG1,MYG2                                  123
      REAL DRDOCOOLING(0:7)                                               125
      DATA DRDOCOOLING/ 16.0, 16.0, 16.0, 16.7, 15.0, 3.5, 14.5, 0.0/     127
      REAL RCOOLINGTUBE/123456789/                                        128
      CHARACTER*4 MYSHA                                                   130
      INTEGER ICOO,ICAB,JCOO,IRIB,ITALS,JTALS                             132
C -                                                                       140
      REAL TPCG_VERSION                                                   140
      EQUIVALENCE (BANK_TPCG(3),TPCG_VERSION,RE_TPCG_VERSION),            140
     *(NAMES_TPCG(1),NTPCG_VERSION)                                       140
      EQUIVALENCE (COMMEN_TPCG(1),CTPCG_VERSION),(MAP_TPCG(1,1),          140
     *MTPCG_VERSION)                                                      140
      CHARACTER CTPCG_VERSION*40/' '/                                     140
      CHARACTER NTPCG_VERSION*12/'R.VERSION'/                             140
      INTEGER MTPCG_VERSION(2)/1,1/                                       140
      REAL RE_TPCG_VERSION                                                140
C -                                                                       140
      REAL TPCG_RMIN                                                      140
      EQUIVALENCE (BANK_TPCG(4),TPCG_RMIN,RE_TPCG_RMIN),(NAMES_TPCG(2),   140
     *NTPCG_RMIN)                                                         140
      EQUIVALENCE (COMMEN_TPCG(2),CTPCG_RMIN),(MAP_TPCG(1,2),             140
     *MTPCG_RMIN)                                                         140
      CHARACTER CTPCG_RMIN*40/' '/                                        140
      CHARACTER NTPCG_RMIN*12/'R.RMIN'/                                   140
      INTEGER MTPCG_RMIN(2)/1,1/                                          140
      REAL RE_TPCG_RMIN                                                   140
C -                                                                       140
      REAL TPCG_RMAX                                                      140
      EQUIVALENCE (BANK_TPCG(5),TPCG_RMAX,RE_TPCG_RMAX),(NAMES_TPCG(3),   140
     *NTPCG_RMAX)                                                         140
      EQUIVALENCE (COMMEN_TPCG(3),CTPCG_RMAX),(MAP_TPCG(1,3),             140
     *MTPCG_RMAX)                                                         140
      CHARACTER CTPCG_RMAX*40/' '/                                        140
      CHARACTER NTPCG_RMAX*12/'R.RMAX'/                                   140
      INTEGER MTPCG_RMAX(2)/1,1/                                          140
      REAL RE_TPCG_RMAX                                                   140
C -                                                                       140
      REAL TPCG_RMINIFC                                                   140
      EQUIVALENCE (BANK_TPCG(6),TPCG_RMINIFC,RE_TPCG_RMINIFC),            140
     *(NAMES_TPCG(4),NTPCG_RMINIFC)                                       140
      EQUIVALENCE (COMMEN_TPCG(4),CTPCG_RMINIFC),(MAP_TPCG(1,4),          140
     *MTPCG_RMINIFC)                                                      140
      CHARACTER CTPCG_RMINIFC*40/' '/                                     140
      CHARACTER NTPCG_RMINIFC*12/'R.RMINIFC'/                             140
      INTEGER MTPCG_RMINIFC(2)/1,1/                                       140
      REAL RE_TPCG_RMINIFC                                                140
C -                                                                       140
      REAL TPCG_LENGTHT                                                   140
      EQUIVALENCE (BANK_TPCG(7),TPCG_LENGTHT,RE_TPCG_LENGTHT),            140
     *(NAMES_TPCG(5),NTPCG_LENGTHT)                                       140
      EQUIVALENCE (COMMEN_TPCG(5),CTPCG_LENGTHT),(MAP_TPCG(1,5),          140
     *MTPCG_LENGTHT)                                                      140
      CHARACTER CTPCG_LENGTHT*40/' '/                                     140
      CHARACTER NTPCG_LENGTHT*12/'R.LENGTHT'/                             140
      INTEGER MTPCG_LENGTHT(2)/1,1/                                       140
      REAL RE_TPCG_LENGTHT                                                140
C -                                                                       140
      REAL TPCG_LENGTH                                                    140
      EQUIVALENCE (BANK_TPCG(8),TPCG_LENGTH,RE_TPCG_LENGTH),              140
     *(NAMES_TPCG(6),NTPCG_LENGTH)                                        140
      EQUIVALENCE (COMMEN_TPCG(6),CTPCG_LENGTH),(MAP_TPCG(1,6),           140
     *MTPCG_LENGTH)                                                       140
      CHARACTER CTPCG_LENGTH*40/' '/                                      140
      CHARACTER NTPCG_LENGTH*12/'R.LENGTH'/                               140
      INTEGER MTPCG_LENGTH(2)/1,1/                                        140
      REAL RE_TPCG_LENGTH                                                 140
C -                                                                       140
      REAL TPCG_LENGTHW                                                   140
      EQUIVALENCE (BANK_TPCG(9),TPCG_LENGTHW,RE_TPCG_LENGTHW),            140
     *(NAMES_TPCG(7),NTPCG_LENGTHW)                                       140
      EQUIVALENCE (COMMEN_TPCG(7),CTPCG_LENGTHW),(MAP_TPCG(1,7),          140
     *MTPCG_LENGTHW)                                                      140
      CHARACTER CTPCG_LENGTHW*40/' '/                                     140
      CHARACTER NTPCG_LENGTHW*12/'R.LENGTHW'/                             140
      INTEGER MTPCG_LENGTHW(2)/1,1/                                       140
      REAL RE_TPCG_LENGTHW                                                140
C -                                                                       140
      REAL TPCG_LENGTHV                                                   140
      EQUIVALENCE (BANK_TPCG(10),TPCG_LENGTHV,RE_TPCG_LENGTHV),           140
     *(NAMES_TPCG(8),NTPCG_LENGTHV)                                       140
      EQUIVALENCE (COMMEN_TPCG(8),CTPCG_LENGTHV),(MAP_TPCG(1,8),          140
     *MTPCG_LENGTHV)                                                      140
      CHARACTER CTPCG_LENGTHV*40/' '/                                     140
      CHARACTER NTPCG_LENGTHV*12/'R.LENGTHV'/                             140
      INTEGER MTPCG_LENGTHV(2)/1,1/                                       140
      REAL RE_TPCG_LENGTHV                                                140
C -                                                                       140
      REAL TPCG_DZENVELOP                                                 140
      EQUIVALENCE (BANK_TPCG(11),TPCG_DZENVELOP,RE_TPCG_DZENVELOP),       140
     *(NAMES_TPCG(9),NTPCG_DZENVELOP)                                     140
      EQUIVALENCE (COMMEN_TPCG(9),CTPCG_DZENVELOP),(MAP_TPCG(1,9),        140
     *MTPCG_DZENVELOP)                                                    140
      CHARACTER CTPCG_DZENVELOP*40/' '/                                   140
      CHARACTER NTPCG_DZENVELOP*12/'R.DZENVELOP'/                         140
      INTEGER MTPCG_DZENVELOP(2)/1,1/                                     140
      REAL RE_TPCG_DZENVELOP                                              140
C -                                                                       140
      REAL TPCG_WHEELIR                                                   140
      EQUIVALENCE (BANK_TPCG(12),TPCG_WHEELIR,RE_TPCG_WHEELIR),           140
     *(NAMES_TPCG(10),NTPCG_WHEELIR)                                      140
      EQUIVALENCE (COMMEN_TPCG(10),CTPCG_WHEELIR),(MAP_TPCG(1,10),        140
     *MTPCG_WHEELIR)                                                      140
      CHARACTER CTPCG_WHEELIR*40/' '/                                     140
      CHARACTER NTPCG_WHEELIR*12/'R.WHEELIR'/                             140
      INTEGER MTPCG_WHEELIR(2)/1,1/                                       140
      REAL RE_TPCG_WHEELIR                                                140
C -                                                                       140
      REAL TPCG_WHEELR0                                                   140
      EQUIVALENCE (BANK_TPCG(13),TPCG_WHEELR0,RE_TPCG_WHEELR0),           140
     *(NAMES_TPCG(11),NTPCG_WHEELR0)                                      140
      EQUIVALENCE (COMMEN_TPCG(11),CTPCG_WHEELR0),(MAP_TPCG(1,11),        140
     *MTPCG_WHEELR0)                                                      140
      CHARACTER CTPCG_WHEELR0*40/' '/                                     140
      CHARACTER NTPCG_WHEELR0*12/'R.WHEELR0'/                             140
      INTEGER MTPCG_WHEELR0(2)/1,1/                                       140
      REAL RE_TPCG_WHEELR0                                                140
C -                                                                       140
      REAL TPCG_WHEELR1                                                   140
      EQUIVALENCE (BANK_TPCG(14),TPCG_WHEELR1,RE_TPCG_WHEELR1),           140
     *(NAMES_TPCG(12),NTPCG_WHEELR1)                                      140
      EQUIVALENCE (COMMEN_TPCG(12),CTPCG_WHEELR1),(MAP_TPCG(1,12),        140
     *MTPCG_WHEELR1)                                                      140
      CHARACTER CTPCG_WHEELR1*40/' '/                                     140
      CHARACTER NTPCG_WHEELR1*12/'R.WHEELR1'/                             140
      INTEGER MTPCG_WHEELR1(2)/1,1/                                       140
      REAL RE_TPCG_WHEELR1                                                140
C -                                                                       140
      REAL TPCG_WHEELR2                                                   140
      EQUIVALENCE (BANK_TPCG(15),TPCG_WHEELR2,RE_TPCG_WHEELR2),           140
     *(NAMES_TPCG(13),NTPCG_WHEELR2)                                      140
      EQUIVALENCE (COMMEN_TPCG(13),CTPCG_WHEELR2),(MAP_TPCG(1,13),        140
     *MTPCG_WHEELR2)                                                      140
      CHARACTER CTPCG_WHEELR2*40/' '/                                     140
      CHARACTER NTPCG_WHEELR2*12/'R.WHEELR2'/                             140
      INTEGER MTPCG_WHEELR2(2)/1,1/                                       140
      REAL RE_TPCG_WHEELR2                                                140
C -                                                                       140
      REAL TPCG_WHEELTOTALRIBWIDTH                                        140
      EQUIVALENCE (BANK_TPCG(16),TPCG_WHEELTOTALRIBWIDTH,                 140
     *RE_TPCG_WHEELTOTALRIBWIDTH),(NAMES_TPCG(14),                        140
     *NTPCG_WHEELTOTALRIBWIDTH)                                           140
      EQUIVALENCE (COMMEN_TPCG(14),CTPCG_WHEELTOTALRIBWIDTH),             140
     *(MAP_TPCG(1,14),MTPCG_WHEELTOTALRIBWIDTH)                           140
      CHARACTER CTPCG_WHEELTOTALRIBWIDTH*40/' '/                          140
      CHARACTER NTPCG_WHEELTOTALRIBWIDTH*12/'R.WHEELTOTALRIBWIDTH'/       140
      INTEGER MTPCG_WHEELTOTALRIBWIDTH(2)/1,1/                            140
      REAL RE_TPCG_WHEELTOTALRIBWIDTH                                     140
C -                                                                       140
      REAL TPCG_WHEELRIBWIDTH                                             140
      EQUIVALENCE (BANK_TPCG(17),TPCG_WHEELRIBWIDTH,                      140
     *RE_TPCG_WHEELRIBWIDTH),(NAMES_TPCG(15),NTPCG_WHEELRIBWIDTH)         140
      EQUIVALENCE (COMMEN_TPCG(15),CTPCG_WHEELRIBWIDTH),(MAP_TPCG(1,15),  140
     *MTPCG_WHEELRIBWIDTH)                                                140
      CHARACTER CTPCG_WHEELRIBWIDTH*40/' '/                               140
      CHARACTER NTPCG_WHEELRIBWIDTH*12/'R.WHEELRIBWIDTH'/                 140
      INTEGER MTPCG_WHEELRIBWIDTH(2)/1,1/                                 140
      REAL RE_TPCG_WHEELRIBWIDTH                                          140
C -                                                                       140
      REAL TPCG_WHEELRIBHEIGHT                                            140
      EQUIVALENCE (BANK_TPCG(18),TPCG_WHEELRIBHEIGHT,                     140
     *RE_TPCG_WHEELRIBHEIGHT),(NAMES_TPCG(16),NTPCG_WHEELRIBHEIGHT)       140
      EQUIVALENCE (COMMEN_TPCG(16),CTPCG_WHEELRIBHEIGHT),(MAP_TPCG(1,     140
     *16),MTPCG_WHEELRIBHEIGHT)                                           140
      CHARACTER CTPCG_WHEELRIBHEIGHT*40/' '/                              140
      CHARACTER NTPCG_WHEELRIBHEIGHT*12/'R.WHEELRIBHEIGHT'/               140
      INTEGER MTPCG_WHEELRIBHEIGHT(2)/1,1/                                140
      REAL RE_TPCG_WHEELRIBHEIGHT                                         140
C -                                                                       140
      REAL TPCG_WHEELBOXDX                                                140
      EQUIVALENCE (BANK_TPCG(19),TPCG_WHEELBOXDX,RE_TPCG_WHEELBOXDX),     140
     *(NAMES_TPCG(17),NTPCG_WHEELBOXDX)                                   140
      EQUIVALENCE (COMMEN_TPCG(17),CTPCG_WHEELBOXDX),(MAP_TPCG(1,17),     140
     *MTPCG_WHEELBOXDX)                                                   140
      CHARACTER CTPCG_WHEELBOXDX*40/' '/                                  140
      CHARACTER NTPCG_WHEELBOXDX*12/'R.WHEELBOXDX'/                       140
      INTEGER MTPCG_WHEELBOXDX(2)/1,1/                                    140
      REAL RE_TPCG_WHEELBOXDX                                             140
C -                                                                       140
      REAL TPCG_WHEELBOXDY                                                140
      EQUIVALENCE (BANK_TPCG(20),TPCG_WHEELBOXDY,RE_TPCG_WHEELBOXDY),     140
     *(NAMES_TPCG(18),NTPCG_WHEELBOXDY)                                   140
      EQUIVALENCE (COMMEN_TPCG(18),CTPCG_WHEELBOXDY),(MAP_TPCG(1,18),     140
     *MTPCG_WHEELBOXDY)                                                   140
      CHARACTER CTPCG_WHEELBOXDY*40/' '/                                  140
      CHARACTER NTPCG_WHEELBOXDY*12/'R.WHEELBOXDY'/                       140
      INTEGER MTPCG_WHEELBOXDY(2)/1,1/                                    140
      REAL RE_TPCG_WHEELBOXDY                                             140
C -                                                                       140
      REAL TPCG_WHEELOR1                                                  140
      EQUIVALENCE (BANK_TPCG(21),TPCG_WHEELOR1,RE_TPCG_WHEELOR1),         140
     *(NAMES_TPCG(19),NTPCG_WHEELOR1)                                     140
      EQUIVALENCE (COMMEN_TPCG(19),CTPCG_WHEELOR1),(MAP_TPCG(1,19),       140
     *MTPCG_WHEELOR1)                                                     140
      CHARACTER CTPCG_WHEELOR1*40/' '/                                    140
      CHARACTER NTPCG_WHEELOR1*12/'R.WHEELOR1'/                           140
      INTEGER MTPCG_WHEELOR1(2)/1,1/                                      140
      REAL RE_TPCG_WHEELOR1                                               140
C -                                                                       140
      REAL TPCG_WHEELOR                                                   140
      EQUIVALENCE (BANK_TPCG(22),TPCG_WHEELOR,RE_TPCG_WHEELOR),           140
     *(NAMES_TPCG(20),NTPCG_WHEELOR)                                      140
      EQUIVALENCE (COMMEN_TPCG(20),CTPCG_WHEELOR),(MAP_TPCG(1,20),        140
     *MTPCG_WHEELOR)                                                      140
      CHARACTER CTPCG_WHEELOR*40/' '/                                     140
      CHARACTER NTPCG_WHEELOR*12/'R.WHEELOR'/                             140
      INTEGER MTPCG_WHEELOR(2)/1,1/                                       140
      REAL RE_TPCG_WHEELOR                                                140
C -                                                                       140
      REAL TPCG_WHEELTHK1                                                 140
      EQUIVALENCE (BANK_TPCG(23),TPCG_WHEELTHK1,RE_TPCG_WHEELTHK1),       140
     *(NAMES_TPCG(21),NTPCG_WHEELTHK1)                                    140
      EQUIVALENCE (COMMEN_TPCG(21),CTPCG_WHEELTHK1),(MAP_TPCG(1,21),      140
     *MTPCG_WHEELTHK1)                                                    140
      CHARACTER CTPCG_WHEELTHK1*40/' '/                                   140
      CHARACTER NTPCG_WHEELTHK1*12/'R.WHEELTHK1'/                         140
      INTEGER MTPCG_WHEELTHK1(2)/1,1/                                     140
      REAL RE_TPCG_WHEELTHK1                                              140
C -                                                                       140
      REAL TPCG_WHEELTHK                                                  140
      EQUIVALENCE (BANK_TPCG(24),TPCG_WHEELTHK,RE_TPCG_WHEELTHK),         140
     *(NAMES_TPCG(22),NTPCG_WHEELTHK)                                     140
      EQUIVALENCE (COMMEN_TPCG(22),CTPCG_WHEELTHK),(MAP_TPCG(1,22),       140
     *MTPCG_WHEELTHK)                                                     140
      CHARACTER CTPCG_WHEELTHK*40/' '/                                    140
      CHARACTER NTPCG_WHEELTHK*12/'R.WHEELTHK'/                           140
      INTEGER MTPCG_WHEELTHK(2)/1,1/                                      140
      REAL RE_TPCG_WHEELTHK                                               140
C -                                                                       140
      REAL TPCG_SENGASOR                                                  140
      EQUIVALENCE (BANK_TPCG(25),TPCG_SENGASOR,RE_TPCG_SENGASOR),         140
     *(NAMES_TPCG(23),NTPCG_SENGASOR)                                     140
      EQUIVALENCE (COMMEN_TPCG(23),CTPCG_SENGASOR),(MAP_TPCG(1,23),       140
     *MTPCG_SENGASOR)                                                     140
      CHARACTER CTPCG_SENGASOR*40/' '/                                    140
      CHARACTER NTPCG_SENGASOR*12/'R.SENGASOR'/                           140
      INTEGER MTPCG_SENGASOR(2)/1,1/                                      140
      REAL RE_TPCG_SENGASOR                                               140
C -                                                                       140
      REAL TPCG_MEMBTHK                                                   140
      EQUIVALENCE (BANK_TPCG(26),TPCG_MEMBTHK,RE_TPCG_MEMBTHK),           140
     *(NAMES_TPCG(24),NTPCG_MEMBTHK)                                      140
      EQUIVALENCE (COMMEN_TPCG(24),CTPCG_MEMBTHK),(MAP_TPCG(1,24),        140
     *MTPCG_MEMBTHK)                                                      140
      CHARACTER CTPCG_MEMBTHK*40/' '/                                     140
      CHARACTER NTPCG_MEMBTHK*12/'R.MEMBTHK'/                             140
      INTEGER MTPCG_MEMBTHK(2)/1,1/                                       140
      REAL RE_TPCG_MEMBTHK                                                140
C -                                                                       140
      REAL TPCG_TOCSDR                                                    140
      EQUIVALENCE (BANK_TPCG(27),TPCG_TOCSDR,RE_TPCG_TOCSDR),             140
     *(NAMES_TPCG(25),NTPCG_TOCSDR)                                       140
      EQUIVALENCE (COMMEN_TPCG(25),CTPCG_TOCSDR),(MAP_TPCG(1,25),         140
     *MTPCG_TOCSDR)                                                       140
      CHARACTER CTPCG_TOCSDR*40/' '/                                      140
      CHARACTER NTPCG_TOCSDR*12/'R.TOCSDR'/                               140
      INTEGER MTPCG_TOCSDR(2)/1,1/                                        140
      REAL RE_TPCG_TOCSDR                                                 140
C -                                                                       140
      REAL TPCG_TOKADR                                                    140
      EQUIVALENCE (BANK_TPCG(28),TPCG_TOKADR,RE_TPCG_TOKADR),             140
     *(NAMES_TPCG(26),NTPCG_TOKADR)                                       140
      EQUIVALENCE (COMMEN_TPCG(26),CTPCG_TOKADR),(MAP_TPCG(1,26),         140
     *MTPCG_TOKADR)                                                       140
      CHARACTER CTPCG_TOKADR*40/' '/                                      140
      CHARACTER NTPCG_TOKADR*12/'R.TOKADR'/                               140
      INTEGER MTPCG_TOKADR(2)/1,1/                                        140
      REAL RE_TPCG_TOKADR                                                 140
C -                                                                       140
      REAL TPCG_TONXDR                                                    140
      EQUIVALENCE (BANK_TPCG(29),TPCG_TONXDR,RE_TPCG_TONXDR),             140
     *(NAMES_TPCG(27),NTPCG_TONXDR)                                       140
      EQUIVALENCE (COMMEN_TPCG(27),CTPCG_TONXDR),(MAP_TPCG(1,27),         140
     *MTPCG_TONXDR)                                                       140
      CHARACTER CTPCG_TONXDR*40/' '/                                      140
      CHARACTER NTPCG_TONXDR*12/'R.TONXDR'/                               140
      INTEGER MTPCG_TONXDR(2)/1,1/                                        140
      REAL RE_TPCG_TONXDR                                                 140
C -                                                                       140
      REAL TPCG_TOADDR                                                    140
      EQUIVALENCE (BANK_TPCG(30),TPCG_TOADDR,RE_TPCG_TOADDR),             140
     *(NAMES_TPCG(28),NTPCG_TOADDR)                                       140
      EQUIVALENCE (COMMEN_TPCG(28),CTPCG_TOADDR),(MAP_TPCG(1,28),         140
     *MTPCG_TOADDR)                                                       140
      CHARACTER CTPCG_TOADDR*40/' '/                                      140
      CHARACTER NTPCG_TOADDR*12/'R.TOADDR'/                               140
      INTEGER MTPCG_TOADDR(2)/1,1/                                        140
      REAL RE_TPCG_TOADDR                                                 140
C -                                                                       140
      REAL TPCG_TOIGDR                                                    140
      EQUIVALENCE (BANK_TPCG(31),TPCG_TOIGDR,RE_TPCG_TOIGDR),             140
     *(NAMES_TPCG(29),NTPCG_TOIGDR)                                       140
      EQUIVALENCE (COMMEN_TPCG(29),CTPCG_TOIGDR),(MAP_TPCG(1,29),         140
     *MTPCG_TOIGDR)                                                       140
      CHARACTER CTPCG_TOIGDR*40/' '/                                      140
      CHARACTER NTPCG_TOIGDR*12/'R.TOIGDR'/                               140
      INTEGER MTPCG_TOIGDR(2)/1,1/                                        140
      REAL RE_TPCG_TOIGDR                                                 140
C -                                                                       140
      REAL TPCG_TOALDR                                                    140
      EQUIVALENCE (BANK_TPCG(32),TPCG_TOALDR,RE_TPCG_TOALDR),             140
     *(NAMES_TPCG(30),NTPCG_TOALDR)                                       140
      EQUIVALENCE (COMMEN_TPCG(30),CTPCG_TOALDR),(MAP_TPCG(1,30),         140
     *MTPCG_TOALDR)                                                       140
      CHARACTER CTPCG_TOALDR*40/' '/                                      140
      CHARACTER NTPCG_TOALDR*12/'R.TOALDR'/                               140
      INTEGER MTPCG_TOALDR(2)/1,1/                                        140
      REAL RE_TPCG_TOALDR                                                 140
C -                                                                       140
      REAL TPCG_TOHADR                                                    140
      EQUIVALENCE (BANK_TPCG(33),TPCG_TOHADR,RE_TPCG_TOHADR),             140
     *(NAMES_TPCG(31),NTPCG_TOHADR)                                       140
      EQUIVALENCE (COMMEN_TPCG(31),CTPCG_TOHADR),(MAP_TPCG(1,31),         140
     *MTPCG_TOHADR)                                                       140
      CHARACTER CTPCG_TOHADR*40/' '/                                      140
      CHARACTER NTPCG_TOHADR*12/'R.TOHADR'/                               140
      INTEGER MTPCG_TOHADR(2)/1,1/                                        140
      REAL RE_TPCG_TOHADR                                                 140
C -                                                                       140
      REAL TPCG_TIADDR                                                    140
      EQUIVALENCE (BANK_TPCG(34),TPCG_TIADDR,RE_TPCG_TIADDR),             140
     *(NAMES_TPCG(32),NTPCG_TIADDR)                                       140
      EQUIVALENCE (COMMEN_TPCG(32),CTPCG_TIADDR),(MAP_TPCG(1,32),         140
     *MTPCG_TIADDR)                                                       140
      CHARACTER CTPCG_TIADDR*40/' '/                                      140
      CHARACTER NTPCG_TIADDR*12/'R.TIADDR'/                               140
      INTEGER MTPCG_TIADDR(2)/1,1/                                        140
      REAL RE_TPCG_TIADDR                                                 140
C -                                                                       140
      REAL TPCG_TINXDR                                                    140
      EQUIVALENCE (BANK_TPCG(35),TPCG_TINXDR,RE_TPCG_TINXDR),             140
     *(NAMES_TPCG(33),NTPCG_TINXDR)                                       140
      EQUIVALENCE (COMMEN_TPCG(33),CTPCG_TINXDR),(MAP_TPCG(1,33),         140
     *MTPCG_TINXDR)                                                       140
      CHARACTER CTPCG_TINXDR*40/' '/                                      140
      CHARACTER NTPCG_TINXDR*12/'R.TINXDR'/                               140
      INTEGER MTPCG_TINXDR(2)/1,1/                                        140
      REAL RE_TPCG_TINXDR                                                 140
C -                                                                       140
      REAL TPCG_TIKADR                                                    140
      EQUIVALENCE (BANK_TPCG(36),TPCG_TIKADR,RE_TPCG_TIKADR),             140
     *(NAMES_TPCG(34),NTPCG_TIKADR)                                       140
      EQUIVALENCE (COMMEN_TPCG(34),CTPCG_TIKADR),(MAP_TPCG(1,34),         140
     *MTPCG_TIKADR)                                                       140
      CHARACTER CTPCG_TIKADR*40/' '/                                      140
      CHARACTER NTPCG_TIKADR*12/'R.TIKADR'/                               140
      INTEGER MTPCG_TIKADR(2)/1,1/                                        140
      REAL RE_TPCG_TIKADR                                                 140
C -                                                                       140
      REAL TPCG_TIALDR                                                    140
      EQUIVALENCE (BANK_TPCG(37),TPCG_TIALDR,RE_TPCG_TIALDR),             140
     *(NAMES_TPCG(35),NTPCG_TIALDR)                                       140
      EQUIVALENCE (COMMEN_TPCG(35),CTPCG_TIALDR),(MAP_TPCG(1,35),         140
     *MTPCG_TIALDR)                                                       140
      CHARACTER CTPCG_TIALDR*40/' '/                                      140
      CHARACTER NTPCG_TIALDR*12/'R.TIALDR'/                               140
      INTEGER MTPCG_TIALDR(2)/1,1/                                        140
      REAL RE_TPCG_TIALDR                                                 140
C -                                                                       140
      REAL TPCG_TIFCRF                                                    140
      EQUIVALENCE (BANK_TPCG(38),TPCG_TIFCRF,RE_TPCG_TIFCRF),             140
     *(NAMES_TPCG(36),NTPCG_TIFCRF)                                       140
      EQUIVALENCE (COMMEN_TPCG(36),CTPCG_TIFCRF),(MAP_TPCG(1,36),         140
     *MTPCG_TIFCRF)                                                       140
      CHARACTER CTPCG_TIFCRF*40/' '/                                      140
      CHARACTER NTPCG_TIFCRF*12/'R.TIFCRF'/                               140
      INTEGER MTPCG_TIFCRF(2)/1,1/                                        140
      REAL RE_TPCG_TIFCRF                                                 140
C -                                                                       140
      REAL TPCG_TIFCDRT                                                   140
      EQUIVALENCE (BANK_TPCG(39),TPCG_TIFCDRT,RE_TPCG_TIFCDRT),           140
     *(NAMES_TPCG(37),NTPCG_TIFCDRT)                                      140
      EQUIVALENCE (COMMEN_TPCG(37),CTPCG_TIFCDRT),(MAP_TPCG(1,37),        140
     *MTPCG_TIFCDRT)                                                      140
      CHARACTER CTPCG_TIFCDRT*40/' '/                                     140
      CHARACTER NTPCG_TIFCDRT*12/'R.TIFCDRT'/                             140
      INTEGER MTPCG_TIFCDRT(2)/1,1/                                       140
      REAL RE_TPCG_TIFCDRT                                                140
C -                                                                       140
      REAL TPCG_DZYF1                                                     140
      EQUIVALENCE (BANK_TPCG(40),TPCG_DZYF1,RE_TPCG_DZYF1),               140
     *(NAMES_TPCG(38),NTPCG_DZYF1)                                        140
      EQUIVALENCE (COMMEN_TPCG(38),CTPCG_DZYF1),(MAP_TPCG(1,38),          140
     *MTPCG_DZYF1)                                                        140
      CHARACTER CTPCG_DZYF1*40/' '/                                       140
      CHARACTER NTPCG_DZYF1*12/'R.DZYF1'/                                 140
      INTEGER MTPCG_DZYF1(2)/1,1/                                         140
      REAL RE_TPCG_DZYF1                                                  140
C -                                                                       140
      REAL TPCG_DZYF2                                                     140
      EQUIVALENCE (BANK_TPCG(41),TPCG_DZYF2,RE_TPCG_DZYF2),               140
     *(NAMES_TPCG(39),NTPCG_DZYF2)                                        140
      EQUIVALENCE (COMMEN_TPCG(39),CTPCG_DZYF2),(MAP_TPCG(1,39),          140
     *MTPCG_DZYF2)                                                        140
      CHARACTER CTPCG_DZYF2*40/' '/                                       140
      CHARACTER NTPCG_DZYF2*12/'R.DZYF2'/                                 140
      INTEGER MTPCG_DZYF2(2)/1,1/                                         140
      REAL RE_TPCG_DZYF2                                                  140
C -                                                                       140
      REAL TPCG_DZYF3                                                     140
      EQUIVALENCE (BANK_TPCG(42),TPCG_DZYF3,RE_TPCG_DZYF3),               140
     *(NAMES_TPCG(40),NTPCG_DZYF3)                                        140
      EQUIVALENCE (COMMEN_TPCG(40),CTPCG_DZYF3),(MAP_TPCG(1,40),          140
     *MTPCG_DZYF3)                                                        140
      CHARACTER CTPCG_DZYF3*40/' '/                                       140
      CHARACTER NTPCG_DZYF3*12/'R.DZYF3'/                                 140
      INTEGER MTPCG_DZYF3(2)/1,1/                                         140
      REAL RE_TPCG_DZYF3                                                  140
C -                                                                       140
      REAL TPCG_PADPLANETHICKNESS                                         140
      EQUIVALENCE (BANK_TPCG(43),TPCG_PADPLANETHICKNESS,                  140
     *RE_TPCG_PADPLANETHICKNESS),(NAMES_TPCG(41),                         140
     *NTPCG_PADPLANETHICKNESS)                                            140
      EQUIVALENCE (COMMEN_TPCG(41),CTPCG_PADPLANETHICKNESS),(MAP_TPCG(1,  140
     *41),MTPCG_PADPLANETHICKNESS)                                        140
      CHARACTER CTPCG_PADPLANETHICKNESS*40/' '/                           140
      CHARACTER NTPCG_PADPLANETHICKNESS*12/'R.PADPLANETHICKNESS'/         140
      INTEGER MTPCG_PADPLANETHICKNESS(2)/1,1/                             140
      REAL RE_TPCG_PADPLANETHICKNESS                                      140
C -                                                                       140
      REAL TPCG_DGATEGROUND                                               140
      EQUIVALENCE (BANK_TPCG(44),TPCG_DGATEGROUND,RE_TPCG_DGATEGROUND),   140
     *(NAMES_TPCG(42),NTPCG_DGATEGROUND)                                  140
      EQUIVALENCE (COMMEN_TPCG(42),CTPCG_DGATEGROUND),(MAP_TPCG(1,42),    140
     *MTPCG_DGATEGROUND)                                                  140
      CHARACTER CTPCG_DGATEGROUND*40/' '/                                 140
      CHARACTER NTPCG_DGATEGROUND*12/'R.DGATEGROUND'/                     140
      INTEGER MTPCG_DGATEGROUND(2)/1,1/                                   140
      REAL RE_TPCG_DGATEGROUND                                            140
C -                                                                       140
      REAL TPCG_WIREMOUNTWIDTH                                            140
      EQUIVALENCE (BANK_TPCG(45),TPCG_WIREMOUNTWIDTH,                     140
     *RE_TPCG_WIREMOUNTWIDTH),(NAMES_TPCG(43),NTPCG_WIREMOUNTWIDTH)       140
      EQUIVALENCE (COMMEN_TPCG(43),CTPCG_WIREMOUNTWIDTH),(MAP_TPCG(1,     140
     *43),MTPCG_WIREMOUNTWIDTH)                                           140
      CHARACTER CTPCG_WIREMOUNTWIDTH*40/' '/                              140
      CHARACTER NTPCG_WIREMOUNTWIDTH*12/'R.WIREMOUNTWIDTH'/               140
      INTEGER MTPCG_WIREMOUNTWIDTH(2)/1,1/                                140
      REAL RE_TPCG_WIREMOUNTWIDTH                                         140
C -                                                                       140
      REAL TPCG_WIREMOUNTHEIGHT                                           140
      EQUIVALENCE (BANK_TPCG(46),TPCG_WIREMOUNTHEIGHT,                    140
     *RE_TPCG_WIREMOUNTHEIGHT),(NAMES_TPCG(44),NTPCG_WIREMOUNTHEIGHT)     140
      EQUIVALENCE (COMMEN_TPCG(44),CTPCG_WIREMOUNTHEIGHT),(MAP_TPCG(1,    140
     *44),MTPCG_WIREMOUNTHEIGHT)                                          140
      CHARACTER CTPCG_WIREMOUNTHEIGHT*40/' '/                             140
      CHARACTER NTPCG_WIREMOUNTHEIGHT*12/'R.WIREMOUNTHEIGHT'/             140
      INTEGER MTPCG_WIREMOUNTHEIGHT(2)/1,1/                               140
      REAL RE_TPCG_WIREMOUNTHEIGHT                                        140
C -                                                                       140
      REAL TPCG_DXRDO                                                     140
      EQUIVALENCE (BANK_TPCG(47),TPCG_DXRDO,RE_TPCG_DXRDO),               140
     *(NAMES_TPCG(45),NTPCG_DXRDO)                                        140
      EQUIVALENCE (COMMEN_TPCG(45),CTPCG_DXRDO),(MAP_TPCG(1,45),          140
     *MTPCG_DXRDO)                                                        140
      CHARACTER CTPCG_DXRDO*40/' '/                                       140
      CHARACTER NTPCG_DXRDO*12/'R.DXRDO'/                                 140
      INTEGER MTPCG_DXRDO(2)/1,1/                                         140
      REAL RE_TPCG_DXRDO                                                  140
C -                                                                       140
      REAL TPCG_DYRDO                                                     140
      EQUIVALENCE (BANK_TPCG(48),TPCG_DYRDO,RE_TPCG_DYRDO),               140
     *(NAMES_TPCG(46),NTPCG_DYRDO)                                        140
      EQUIVALENCE (COMMEN_TPCG(46),CTPCG_DYRDO),(MAP_TPCG(1,46),          140
     *MTPCG_DYRDO)                                                        140
      CHARACTER CTPCG_DYRDO*40/' '/                                       140
      CHARACTER NTPCG_DYRDO*12/'R.DYRDO'/                                 140
      INTEGER MTPCG_DYRDO(2)/1,1/                                         140
      REAL RE_TPCG_DYRDO                                                  140
C -                                                                       140
      REAL TPCG_DZRDO                                                     140
      EQUIVALENCE (BANK_TPCG(49),TPCG_DZRDO,RE_TPCG_DZRDO),               140
     *(NAMES_TPCG(47),NTPCG_DZRDO)                                        140
      EQUIVALENCE (COMMEN_TPCG(47),CTPCG_DZRDO),(MAP_TPCG(1,47),          140
     *MTPCG_DZRDO)                                                        140
      CHARACTER CTPCG_DZRDO*40/' '/                                       140
      CHARACTER NTPCG_DZRDO*12/'R.DZRDO'/                                 140
      INTEGER MTPCG_DZRDO(2)/1,1/                                         140
      REAL RE_TPCG_DZRDO                                                  140
C -                                                                       140
      REAL TPCG_ZRDO                                                      140
      EQUIVALENCE (BANK_TPCG(50),TPCG_ZRDO,RE_TPCG_ZRDO),                 140
     *(NAMES_TPCG(48),NTPCG_ZRDO)                                         140
      EQUIVALENCE (COMMEN_TPCG(48),CTPCG_ZRDO),(MAP_TPCG(1,48),           140
     *MTPCG_ZRDO)                                                         140
      CHARACTER CTPCG_ZRDO*40/' '/                                        140
      CHARACTER NTPCG_ZRDO*12/'R.ZRDO'/                                   140
      INTEGER MTPCG_ZRDO(2)/1,1/                                          140
      REAL RE_TPCG_ZRDO                                                   140
C -                                                                       140
      REAL TPCG_HEIGTUBE                                                  140
      EQUIVALENCE (BANK_TPCG(51),TPCG_HEIGTUBE,RE_TPCG_HEIGTUBE),         140
     *(NAMES_TPCG(49),NTPCG_HEIGTUBE)                                     140
      EQUIVALENCE (COMMEN_TPCG(49),CTPCG_HEIGTUBE),(MAP_TPCG(1,49),       140
     *MTPCG_HEIGTUBE)                                                     140
      CHARACTER CTPCG_HEIGTUBE*40/' '/                                    140
      CHARACTER NTPCG_HEIGTUBE*12/'R.HEIGTUBE'/                           140
      INTEGER MTPCG_HEIGTUBE(2)/1,1/                                      140
      REAL RE_TPCG_HEIGTUBE                                               140
C -                                                                       140
      REAL TPCG_WIDTUBE                                                   140
      EQUIVALENCE (BANK_TPCG(52),TPCG_WIDTUBE,RE_TPCG_WIDTUBE),           140
     *(NAMES_TPCG(50),NTPCG_WIDTUBE)                                      140
      EQUIVALENCE (COMMEN_TPCG(50),CTPCG_WIDTUBE),(MAP_TPCG(1,50),        140
     *MTPCG_WIDTUBE)                                                      140
      CHARACTER CTPCG_WIDTUBE*40/' '/                                     140
      CHARACTER NTPCG_WIDTUBE*12/'R.WIDTUBE'/                             140
      INTEGER MTPCG_WIDTUBE(2)/1,1/                                       140
      REAL RE_TPCG_WIDTUBE                                                140
C -                                                                       140
      REAL TPCG_RDOCOOLINGDX                                              140
      EQUIVALENCE (BANK_TPCG(53),TPCG_RDOCOOLINGDX,                       140
     *RE_TPCG_RDOCOOLINGDX),(NAMES_TPCG(51),NTPCG_RDOCOOLINGDX)           140
      EQUIVALENCE (COMMEN_TPCG(51),CTPCG_RDOCOOLINGDX),(MAP_TPCG(1,51),   140
     *MTPCG_RDOCOOLINGDX)                                                 140
      CHARACTER CTPCG_RDOCOOLINGDX*40/' '/                                140
      CHARACTER NTPCG_RDOCOOLINGDX*12/'R.RDOCOOLINGDX'/                   140
      INTEGER MTPCG_RDOCOOLINGDX(2)/1,1/                                  140
      REAL RE_TPCG_RDOCOOLINGDX                                           140
C -                                                                       140
      REAL TPCG_RDOCOOLINGDY                                              140
      EQUIVALENCE (BANK_TPCG(54),TPCG_RDOCOOLINGDY,                       140
     *RE_TPCG_RDOCOOLINGDY),(NAMES_TPCG(52),NTPCG_RDOCOOLINGDY)           140
      EQUIVALENCE (COMMEN_TPCG(52),CTPCG_RDOCOOLINGDY),(MAP_TPCG(1,52),   140
     *MTPCG_RDOCOOLINGDY)                                                 140
      CHARACTER CTPCG_RDOCOOLINGDY*40/' '/                                140
      CHARACTER NTPCG_RDOCOOLINGDY*12/'R.RDOCOOLINGDY'/                   140
      INTEGER MTPCG_RDOCOOLINGDY(2)/1,1/                                  140
      REAL RE_TPCG_RDOCOOLINGDY                                           140
C -                                                                       140
      REAL TPCG_RDOCOOLINGDZ                                              140
      EQUIVALENCE (BANK_TPCG(55),TPCG_RDOCOOLINGDZ,                       140
     *RE_TPCG_RDOCOOLINGDZ),(NAMES_TPCG(53),NTPCG_RDOCOOLINGDZ)           140
      EQUIVALENCE (COMMEN_TPCG(53),CTPCG_RDOCOOLINGDZ),(MAP_TPCG(1,53),   140
     *MTPCG_RDOCOOLINGDZ)                                                 140
      CHARACTER CTPCG_RDOCOOLINGDZ*40/' '/                                140
      CHARACTER NTPCG_RDOCOOLINGDZ*12/'R.RDOCOOLINGDZ'/                   140
      INTEGER MTPCG_RDOCOOLINGDZ(2)/1,1/                                  140
      REAL RE_TPCG_RDOCOOLINGDZ                                           140
C -                                                                       140
      REAL TPCG_ZGATINGGRID                                               140
      EQUIVALENCE (BANK_TPCG(56),TPCG_ZGATINGGRID,RE_TPCG_ZGATINGGRID),   140
     *(NAMES_TPCG(54),NTPCG_ZGATINGGRID)                                  140
      EQUIVALENCE (COMMEN_TPCG(54),CTPCG_ZGATINGGRID),(MAP_TPCG(1,54),    140
     *MTPCG_ZGATINGGRID)                                                  140
      CHARACTER CTPCG_ZGATINGGRID*40/' '/                                 140
      CHARACTER NTPCG_ZGATINGGRID*12/'R.ZGATINGGRID'/                     140
      INTEGER MTPCG_ZGATINGGRID(2)/1,1/                                   140
      REAL RE_TPCG_ZGATINGGRID                                            140
C -                                                                       140
      REAL TPCG_ZGROUNDGRID                                               140
      EQUIVALENCE (BANK_TPCG(57),TPCG_ZGROUNDGRID,RE_TPCG_ZGROUNDGRID),   140
     *(NAMES_TPCG(55),NTPCG_ZGROUNDGRID)                                  140
      EQUIVALENCE (COMMEN_TPCG(55),CTPCG_ZGROUNDGRID),(MAP_TPCG(1,55),    140
     *MTPCG_ZGROUNDGRID)                                                  140
      CHARACTER CTPCG_ZGROUNDGRID*40/' '/                                 140
      CHARACTER NTPCG_ZGROUNDGRID*12/'R.ZGROUNDGRID'/                     140
      INTEGER MTPCG_ZGROUNDGRID(2)/1,1/                                   140
      REAL RE_TPCG_ZGROUNDGRID                                            140
C -                                                                       140
      REAL TPCG_DEADZONE                                                  140
      EQUIVALENCE (BANK_TPCG(58),TPCG_DEADZONE,RE_TPCG_DEADZONE),         140
     *(NAMES_TPCG(56),NTPCG_DEADZONE)                                     140
      EQUIVALENCE (COMMEN_TPCG(56),CTPCG_DEADZONE),(MAP_TPCG(1,56),       140
     *MTPCG_DEADZONE)                                                     140
      CHARACTER CTPCG_DEADZONE*40/' '/                                    140
      CHARACTER NTPCG_DEADZONE*12/'R.DEADZONE'/                           140
      INTEGER MTPCG_DEADZONE(2)/1,1/                                      140
      REAL RE_TPCG_DEADZONE                                               140
C -                                                                       140
      REAL TPCG_DANODE(2)                                                 140
      EQUIVALENCE (BANK_TPCG(59),TPCG_DANODE,RE_TPCG_DANODE),             140
     *(NAMES_TPCG(57),NTPCG_DANODE)                                       140
      EQUIVALENCE (COMMEN_TPCG(57),CTPCG_DANODE),(MAP_TPCG(1,57),         140
     *MTPCG_DANODE)                                                       140
      CHARACTER CTPCG_DANODE*40/' '/                                      140
      CHARACTER NTPCG_DANODE*12/'R.DANODE'/                               140
      INTEGER MTPCG_DANODE(2)/2,1/                                        140
      REAL RE_TPCG_DANODE                                                 140
C -                                                                       140
      INTEGER LEN_TPCG,LLEN_TPCG,LINK_TPCG                                140
      PARAMETER (LEN_TPCG=60)                                             140
      PARAMETER (LLEN_TPCG=57)                                            140
      INTEGER FLAG_TPCG/0/,MAP_TPCG(2,LLEN_TPCG),IDNUM_TPCG(10)           140
      CHARACTER NAMES_TPCG(LLEN_TPCG)*12,COMMEN_TPCG(LLEN_TPCG)*40        140
      DATA IDNUM_TPCG/10*0/                                               140
      REAL BANK_TPCG(LEN_TPCG)                                            140
      CHARACTER*60 BPATH_TPCG/' '/                                        140
      SAVE FLAG_TPCG,MAP_TPCG,IDNUM_TPCG,NAMES_TPCG,COMMEN_TPCG,          140
     *BPATH_TPCG                                                          140
      DATA LINK_TPCG/0/,BANK_TPCG/LEN_TPCG*0/                             140
C *                                                                       140
C -                                                                       141
      REAL TPRS_SEC                                                       141
      EQUIVALENCE (BANK_TPRS(3),TPRS_SEC,RE_TPRS_SEC),(NAMES_TPRS(1),     141
     *NTPRS_SEC)                                                          141
      EQUIVALENCE (COMMEN_TPRS(1),CTPRS_SEC),(MAP_TPRS(1,1),MTPRS_SEC)    141
      CHARACTER CTPRS_SEC*40/' '/                                         141
      CHARACTER NTPRS_SEC*12/'R.SEC'/                                     141
      INTEGER MTPRS_SEC(2)/1,1/                                           141
      REAL RE_TPRS_SEC                                                    141
C -                                                                       141
      REAL TPRS_NROW                                                      141
      EQUIVALENCE (BANK_TPRS(4),TPRS_NROW,RE_TPRS_NROW),(NAMES_TPRS(2),   141
     *NTPRS_NROW)                                                         141
      EQUIVALENCE (COMMEN_TPRS(2),CTPRS_NROW),(MAP_TPRS(1,2),             141
     *MTPRS_NROW)                                                         141
      CHARACTER CTPRS_NROW*40/' '/                                        141
      CHARACTER NTPRS_NROW*12/'R.NROW'/                                   141
      INTEGER MTPRS_NROW(2)/1,1/                                          141
      REAL RE_TPRS_NROW                                                   141
C -                                                                       141
      REAL TPRS_PITCH                                                     141
      EQUIVALENCE (BANK_TPRS(5),TPRS_PITCH,RE_TPRS_PITCH),                141
     *(NAMES_TPRS(3),NTPRS_PITCH)                                         141
      EQUIVALENCE (COMMEN_TPRS(3),CTPRS_PITCH),(MAP_TPRS(1,3),            141
     *MTPRS_PITCH)                                                        141
      CHARACTER CTPRS_PITCH*40/' '/                                       141
      CHARACTER NTPRS_PITCH*12/'R.PITCH'/                                 141
      INTEGER MTPRS_PITCH(2)/1,1/                                         141
      REAL RE_TPRS_PITCH                                                  141
C -                                                                       141
      REAL TPRS_WIDTH                                                     141
      EQUIVALENCE (BANK_TPRS(6),TPRS_WIDTH,RE_TPRS_WIDTH),                141
     *(NAMES_TPRS(4),NTPRS_WIDTH)                                         141
      EQUIVALENCE (COMMEN_TPRS(4),CTPRS_WIDTH),(MAP_TPRS(1,4),            141
     *MTPRS_WIDTH)                                                        141
      CHARACTER CTPRS_WIDTH*40/' '/                                       141
      CHARACTER NTPRS_WIDTH*12/'R.WIDTH'/                                 141
      INTEGER MTPRS_WIDTH(2)/1,1/                                         141
      REAL RE_TPRS_WIDTH                                                  141
C -                                                                       141
      REAL TPRS_DANODE                                                    141
      EQUIVALENCE (BANK_TPRS(7),TPRS_DANODE,RE_TPRS_DANODE),              141
     *(NAMES_TPRS(5),NTPRS_DANODE)                                        141
      EQUIVALENCE (COMMEN_TPRS(5),CTPRS_DANODE),(MAP_TPRS(1,5),           141
     *MTPRS_DANODE)                                                       141
      CHARACTER CTPRS_DANODE*40/' '/                                      141
      CHARACTER NTPRS_DANODE*12/'R.DANODE'/                               141
      INTEGER MTPRS_DANODE(2)/1,1/                                        141
      REAL RE_TPRS_DANODE                                                 141
C -                                                                       141
      REAL TPRS_RPADS(32)                                                 141
      EQUIVALENCE (BANK_TPRS(8),TPRS_RPADS,RE_TPRS_RPADS),                141
     *(NAMES_TPRS(6),NTPRS_RPADS)                                         141
      EQUIVALENCE (COMMEN_TPRS(6),CTPRS_RPADS),(MAP_TPRS(1,6),            141
     *MTPRS_RPADS)                                                        141
      CHARACTER CTPRS_RPADS*40/' '/                                       141
      CHARACTER NTPRS_RPADS*12/'R.RPADS'/                                 141
      INTEGER MTPRS_RPADS(2)/32,1/                                        141
      REAL RE_TPRS_RPADS                                                  141
C -                                                                       141
      REAL TPRS_NPADS(32)                                                 141
      EQUIVALENCE (BANK_TPRS(40),TPRS_NPADS,RE_TPRS_NPADS),               141
     *(NAMES_TPRS(7),NTPRS_NPADS)                                         141
      EQUIVALENCE (COMMEN_TPRS(7),CTPRS_NPADS),(MAP_TPRS(1,7),            141
     *MTPRS_NPADS)                                                        141
      CHARACTER CTPRS_NPADS*40/' '/                                       141
      CHARACTER NTPRS_NPADS*12/'R.NPADS'/                                 141
      INTEGER MTPRS_NPADS(2)/32,1/                                        141
      REAL RE_TPRS_NPADS                                                  141
C -                                                                       141
      INTEGER LEN_TPRS,LLEN_TPRS,LINK_TPRS                                141
      PARAMETER (LEN_TPRS=71)                                             141
      PARAMETER (LLEN_TPRS=7)                                             141
      INTEGER FLAG_TPRS/0/,MAP_TPRS(2,LLEN_TPRS),IDNUM_TPRS(10)           141
      CHARACTER NAMES_TPRS(LLEN_TPRS)*12,COMMEN_TPRS(LLEN_TPRS)*40        141
      DATA IDNUM_TPRS/10*0/                                               141
      REAL BANK_TPRS(LEN_TPRS)                                            141
      CHARACTER*60 BPATH_TPRS/' '/                                        141
      SAVE FLAG_TPRS,MAP_TPRS,IDNUM_TPRS,NAMES_TPRS,COMMEN_TPRS,          141
     *BPATH_TPRS                                                          141
      DATA LINK_TPRS/0/,BANK_TPRS/LEN_TPRS*0/                             141
C *                                                                       141
C -                                                                       143
      REAL TECW_SEC                                                       143
      EQUIVALENCE (BANK_TECW(3),TECW_SEC,RE_TECW_SEC),(NAMES_TECW(1),     143
     *NTECW_SEC)                                                          143
      EQUIVALENCE (COMMEN_TECW(1),CTECW_SEC),(MAP_TECW(1,1),MTECW_SEC)    143
      CHARACTER CTECW_SEC*40/' '/                                         143
      CHARACTER NTECW_SEC*12/'R.SEC'/                                     143
      INTEGER MTECW_SEC(2)/1,1/                                           143
      REAL RE_TECW_SEC                                                    143
C -                                                                       143
      REAL TECW_GAPWIDI                                                   143
      EQUIVALENCE (BANK_TECW(4),TECW_GAPWIDI,RE_TECW_GAPWIDI),            143
     *(NAMES_TECW(2),NTECW_GAPWIDI)                                       143
      EQUIVALENCE (COMMEN_TECW(2),CTECW_GAPWIDI),(MAP_TECW(1,2),          143
     *MTECW_GAPWIDI)                                                      143
      CHARACTER CTECW_GAPWIDI*40/' '/                                     143
      CHARACTER NTECW_GAPWIDI*12/'R.GAPWIDI'/                             143
      INTEGER MTECW_GAPWIDI(2)/1,1/                                       143
      REAL RE_TECW_GAPWIDI                                                143
C -                                                                       143
      REAL TECW_GAPWIDO                                                   143
      EQUIVALENCE (BANK_TECW(5),TECW_GAPWIDO,RE_TECW_GAPWIDO),            143
     *(NAMES_TECW(3),NTECW_GAPWIDO)                                       143
      EQUIVALENCE (COMMEN_TECW(3),CTECW_GAPWIDO),(MAP_TECW(1,3),          143
     *MTECW_GAPWIDO)                                                      143
      CHARACTER CTECW_GAPWIDO*40/' '/                                     143
      CHARACTER NTECW_GAPWIDO*12/'R.GAPWIDO'/                             143
      INTEGER MTECW_GAPWIDO(2)/1,1/                                       143
      REAL RE_TECW_GAPWIDO                                                143
C -                                                                       143
      REAL TECW_GAPHEIT                                                   143
      EQUIVALENCE (BANK_TECW(6),TECW_GAPHEIT,RE_TECW_GAPHEIT),            143
     *(NAMES_TECW(4),NTECW_GAPHEIT)                                       143
      EQUIVALENCE (COMMEN_TECW(4),CTECW_GAPHEIT),(MAP_TECW(1,4),          143
     *MTECW_GAPHEIT)                                                      143
      CHARACTER CTECW_GAPHEIT*40/' '/                                     143
      CHARACTER NTECW_GAPHEIT*12/'R.GAPHEIT'/                             143
      INTEGER MTECW_GAPHEIT(2)/1,1/                                       143
      REAL RE_TECW_GAPHEIT                                                143
C -                                                                       143
      REAL TECW_GAPRAD                                                    143
      EQUIVALENCE (BANK_TECW(7),TECW_GAPRAD,RE_TECW_GAPRAD),              143
     *(NAMES_TECW(5),NTECW_GAPRAD)                                        143
      EQUIVALENCE (COMMEN_TECW(5),CTECW_GAPRAD),(MAP_TECW(1,5),           143
     *MTECW_GAPRAD)                                                       143
      CHARACTER CTECW_GAPRAD*40/' '/                                      143
      CHARACTER NTECW_GAPRAD*12/'R.GAPRAD'/                               143
      INTEGER MTECW_GAPRAD(2)/1,1/                                        143
      REAL RE_TECW_GAPRAD                                                 143
C -                                                                       143
      REAL TECW_INWIDTH                                                   143
      EQUIVALENCE (BANK_TECW(8),TECW_INWIDTH,RE_TECW_INWIDTH),            143
     *(NAMES_TECW(6),NTECW_INWIDTH)                                       143
      EQUIVALENCE (COMMEN_TECW(6),CTECW_INWIDTH),(MAP_TECW(1,6),          143
     *MTECW_INWIDTH)                                                      143
      CHARACTER CTECW_INWIDTH*40/' '/                                     143
      CHARACTER NTECW_INWIDTH*12/'R.INWIDTH'/                             143
      INTEGER MTECW_INWIDTH(2)/1,1/                                       143
      REAL RE_TECW_INWIDTH                                                143
C -                                                                       143
      REAL TECW_OUWIDTH                                                   143
      EQUIVALENCE (BANK_TECW(9),TECW_OUWIDTH,RE_TECW_OUWIDTH),            143
     *(NAMES_TECW(7),NTECW_OUWIDTH)                                       143
      EQUIVALENCE (COMMEN_TECW(7),CTECW_OUWIDTH),(MAP_TECW(1,7),          143
     *MTECW_OUWIDTH)                                                      143
      CHARACTER CTECW_OUWIDTH*40/' '/                                     143
      CHARACTER NTECW_OUWIDTH*12/'R.OUWIDTH'/                             143
      INTEGER MTECW_OUWIDTH(2)/1,1/                                       143
      REAL RE_TECW_OUWIDTH                                                143
C -                                                                       143
      REAL TECW_WIDTHLIP                                                  143
      EQUIVALENCE (BANK_TECW(10),TECW_WIDTHLIP,RE_TECW_WIDTHLIP),         143
     *(NAMES_TECW(8),NTECW_WIDTHLIP)                                      143
      EQUIVALENCE (COMMEN_TECW(8),CTECW_WIDTHLIP),(MAP_TECW(1,8),         143
     *MTECW_WIDTHLIP)                                                     143
      CHARACTER CTECW_WIDTHLIP*40/' '/                                    143
      CHARACTER NTECW_WIDTHLIP*12/'R.WIDTHLIP'/                           143
      INTEGER MTECW_WIDTHLIP(2)/1,1/                                      143
      REAL RE_TECW_WIDTHLIP                                               143
C -                                                                       143
      REAL TECW_NORIBS                                                    143
      EQUIVALENCE (BANK_TECW(11),TECW_NORIBS,RE_TECW_NORIBS),             143
     *(NAMES_TECW(9),NTECW_NORIBS)                                        143
      EQUIVALENCE (COMMEN_TECW(9),CTECW_NORIBS),(MAP_TECW(1,9),           143
     *MTECW_NORIBS)                                                       143
      CHARACTER CTECW_NORIBS*40/' '/                                      143
      CHARACTER NTECW_NORIBS*12/'R.NORIBS'/                               143
      INTEGER MTECW_NORIBS(2)/1,1/                                        143
      REAL RE_TECW_NORIBS                                                 143
C -                                                                       143
      REAL TECW_ZSTEPRIB                                                  143
      EQUIVALENCE (BANK_TECW(12),TECW_ZSTEPRIB,RE_TECW_ZSTEPRIB),         143
     *(NAMES_TECW(10),NTECW_ZSTEPRIB)                                     143
      EQUIVALENCE (COMMEN_TECW(10),CTECW_ZSTEPRIB),(MAP_TECW(1,10),       143
     *MTECW_ZSTEPRIB)                                                     143
      CHARACTER CTECW_ZSTEPRIB*40/' '/                                    143
      CHARACTER NTECW_ZSTEPRIB*12/'R.ZSTEPRIB'/                           143
      INTEGER MTECW_ZSTEPRIB(2)/1,1/                                      143
      REAL RE_TECW_ZSTEPRIB                                               143
C -                                                                       143
      REAL TECW_WIDTHRIB                                                  143
      EQUIVALENCE (BANK_TECW(13),TECW_WIDTHRIB,RE_TECW_WIDTHRIB),         143
     *(NAMES_TECW(11),NTECW_WIDTHRIB)                                     143
      EQUIVALENCE (COMMEN_TECW(11),CTECW_WIDTHRIB),(MAP_TECW(1,11),       143
     *MTECW_WIDTHRIB)                                                     143
      CHARACTER CTECW_WIDTHRIB*40/' '/                                    143
      CHARACTER NTECW_WIDTHRIB*12/'R.WIDTHRIB'/                           143
      INTEGER MTECW_WIDTHRIB(2)/1,1/                                      143
      REAL RE_TECW_WIDTHRIB                                               143
C -                                                                       143
      REAL TECW_HEIGHT                                                    143
      EQUIVALENCE (BANK_TECW(14),TECW_HEIGHT,RE_TECW_HEIGHT),             143
     *(NAMES_TECW(12),NTECW_HEIGHT)                                       143
      EQUIVALENCE (COMMEN_TECW(12),CTECW_HEIGHT),(MAP_TECW(1,12),         143
     *MTECW_HEIGHT)                                                       143
      CHARACTER CTECW_HEIGHT*40/' '/                                      143
      CHARACTER NTECW_HEIGHT*12/'R.HEIGHT'/                               143
      INTEGER MTECW_HEIGHT(2)/1,1/                                        143
      REAL RE_TECW_HEIGHT                                                 143
C -                                                                       143
      REAL TECW_THICK                                                     143
      EQUIVALENCE (BANK_TECW(15),TECW_THICK,RE_TECW_THICK),               143
     *(NAMES_TECW(13),NTECW_THICK)                                        143
      EQUIVALENCE (COMMEN_TECW(13),CTECW_THICK),(MAP_TECW(1,13),          143
     *MTECW_THICK)                                                        143
      CHARACTER CTECW_THICK*40/' '/                                       143
      CHARACTER NTECW_THICK*12/'R.THICK'/                                 143
      INTEGER MTECW_THICK(2)/1,1/                                         143
      REAL RE_TECW_THICK                                                  143
C -                                                                       143
      REAL TECW_THICKAL                                                   143
      EQUIVALENCE (BANK_TECW(16),TECW_THICKAL,RE_TECW_THICKAL),           143
     *(NAMES_TECW(14),NTECW_THICKAL)                                      143
      EQUIVALENCE (COMMEN_TECW(14),CTECW_THICKAL),(MAP_TECW(1,14),        143
     *MTECW_THICKAL)                                                      143
      CHARACTER CTECW_THICKAL*40/' '/                                     143
      CHARACTER NTECW_THICKAL*12/'R.THICKAL'/                             143
      INTEGER MTECW_THICKAL(2)/1,1/                                       143
      REAL RE_TECW_THICKAL                                                143
C -                                                                       143
      REAL TECW_RMIN                                                      143
      EQUIVALENCE (BANK_TECW(17),TECW_RMIN,RE_TECW_RMIN),                 143
     *(NAMES_TECW(15),NTECW_RMIN)                                         143
      EQUIVALENCE (COMMEN_TECW(15),CTECW_RMIN),(MAP_TECW(1,15),           143
     *MTECW_RMIN)                                                         143
      CHARACTER CTECW_RMIN*40/' '/                                        143
      CHARACTER NTECW_RMIN*12/'R.RMIN'/                                   143
      INTEGER MTECW_RMIN(2)/1,1/                                          143
      REAL RE_TECW_RMIN                                                   143
C -                                                                       143
      REAL TECW_RCENTER                                                   143
      EQUIVALENCE (BANK_TECW(18),TECW_RCENTER,RE_TECW_RCENTER),           143
     *(NAMES_TECW(16),NTECW_RCENTER)                                      143
      EQUIVALENCE (COMMEN_TECW(16),CTECW_RCENTER),(MAP_TECW(1,16),        143
     *MTECW_RCENTER)                                                      143
      CHARACTER CTECW_RCENTER*40/' '/                                     143
      CHARACTER NTECW_RCENTER*12/'R.RCENTER'/                             143
      INTEGER MTECW_RCENTER(2)/1,1/                                       143
      REAL RE_TECW_RCENTER                                                143
C -                                                                       143
      REAL TECW_HOLEDX                                                    143
      EQUIVALENCE (BANK_TECW(19),TECW_HOLEDX,RE_TECW_HOLEDX),             143
     *(NAMES_TECW(17),NTECW_HOLEDX)                                       143
      EQUIVALENCE (COMMEN_TECW(17),CTECW_HOLEDX),(MAP_TECW(1,17),         143
     *MTECW_HOLEDX)                                                       143
      CHARACTER CTECW_HOLEDX*40/' '/                                      143
      CHARACTER NTECW_HOLEDX*12/'R.HOLEDX'/                               143
      INTEGER MTECW_HOLEDX(2)/1,1/                                        143
      REAL RE_TECW_HOLEDX                                                 143
C -                                                                       143
      REAL TECW_HOLEDY                                                    143
      EQUIVALENCE (BANK_TECW(20),TECW_HOLEDY,RE_TECW_HOLEDY),             143
     *(NAMES_TECW(18),NTECW_HOLEDY)                                       143
      EQUIVALENCE (COMMEN_TECW(18),CTECW_HOLEDY),(MAP_TECW(1,18),         143
     *MTECW_HOLEDY)                                                       143
      CHARACTER CTECW_HOLEDY*40/' '/                                      143
      CHARACTER NTECW_HOLEDY*12/'R.HOLEDY'/                               143
      INTEGER MTECW_HOLEDY(2)/1,1/                                        143
      REAL RE_TECW_HOLEDY                                                 143
C -                                                                       143
      INTEGER LEN_TECW,LLEN_TECW,LINK_TECW                                143
      PARAMETER (LEN_TECW=20)                                             143
      PARAMETER (LLEN_TECW=18)                                            143
      INTEGER FLAG_TECW/0/,MAP_TECW(2,LLEN_TECW),IDNUM_TECW(10)           143
      CHARACTER NAMES_TECW(LLEN_TECW)*12,COMMEN_TECW(LLEN_TECW)*40        143
      DATA IDNUM_TECW/10*0/                                               143
      REAL BANK_TECW(LEN_TECW)                                            143
      CHARACTER*60 BPATH_TECW/' '/                                        143
      SAVE FLAG_TECW,MAP_TECW,IDNUM_TECW,NAMES_TECW,COMMEN_TECW,          143
     *BPATH_TECW                                                          143
      DATA LINK_TECW/0/,BANK_TECW/LEN_TECW*0/                             143
C *                                                                       143
C -                                                                       144
      REAL TPCR_RDOVTHK                                                   144
      EQUIVALENCE (BANK_TPCR(3),TPCR_RDOVTHK,RE_TPCR_RDOVTHK),            144
     *(NAMES_TPCR(1),NTPCR_RDOVTHK)                                       144
      EQUIVALENCE (COMMEN_TPCR(1),CTPCR_RDOVTHK),(MAP_TPCR(1,1),          144
     *MTPCR_RDOVTHK)                                                      144
      CHARACTER CTPCR_RDOVTHK*40/' '/                                     144
      CHARACTER NTPCR_RDOVTHK*12/'R.RDOVTHK'/                             144
      INTEGER MTPCR_RDOVTHK(2)/1,1/                                       144
      REAL RE_TPCR_RDOVTHK                                                144
C -                                                                       144
      REAL TPCR_RDOTHK                                                    144
      EQUIVALENCE (BANK_TPCR(4),TPCR_RDOTHK,RE_TPCR_RDOTHK),              144
     *(NAMES_TPCR(2),NTPCR_RDOTHK)                                        144
      EQUIVALENCE (COMMEN_TPCR(2),CTPCR_RDOTHK),(MAP_TPCR(1,2),           144
     *MTPCR_RDOTHK)                                                       144
      CHARACTER CTPCR_RDOTHK*40/' '/                                      144
      CHARACTER NTPCR_RDOTHK*12/'R.RDOTHK'/                               144
      INTEGER MTPCR_RDOTHK(2)/1,1/                                        144
      REAL RE_TPCR_RDOTHK                                                 144
C -                                                                       144
      REAL TPCR_RDOLEN                                                    144
      EQUIVALENCE (BANK_TPCR(5),TPCR_RDOLEN,RE_TPCR_RDOLEN),              144
     *(NAMES_TPCR(3),NTPCR_RDOLEN)                                        144
      EQUIVALENCE (COMMEN_TPCR(3),CTPCR_RDOLEN),(MAP_TPCR(1,3),           144
     *MTPCR_RDOLEN)                                                       144
      CHARACTER CTPCR_RDOLEN*40/' '/                                      144
      CHARACTER NTPCR_RDOLEN*12/'R.RDOLEN'/                               144
      INTEGER MTPCR_RDOLEN(2)/1,1/                                        144
      REAL RE_TPCR_RDOLEN                                                 144
C -                                                                       144
      REAL TPCR_NRDOBRD                                                   144
      EQUIVALENCE (BANK_TPCR(6),TPCR_NRDOBRD,RE_TPCR_NRDOBRD),            144
     *(NAMES_TPCR(4),NTPCR_NRDOBRD)                                       144
      EQUIVALENCE (COMMEN_TPCR(4),CTPCR_NRDOBRD),(MAP_TPCR(1,4),          144
     *MTPCR_NRDOBRD)                                                      144
      CHARACTER CTPCR_NRDOBRD*40/' '/                                     144
      CHARACTER NTPCR_NRDOBRD*12/'R.NRDOBRD'/                             144
      INTEGER MTPCR_NRDOBRD(2)/1,1/                                       144
      REAL RE_TPCR_NRDOBRD                                                144
C -                                                                       144
      REAL TPCR_RDOHT(9)                                                  144
      EQUIVALENCE (BANK_TPCR(7),TPCR_RDOHT,RE_TPCR_RDOHT),                144
     *(NAMES_TPCR(5),NTPCR_RDOHT)                                         144
      EQUIVALENCE (COMMEN_TPCR(5),CTPCR_RDOHT),(MAP_TPCR(1,5),            144
     *MTPCR_RDOHT)                                                        144
      CHARACTER CTPCR_RDOHT*40/' '/                                       144
      CHARACTER NTPCR_RDOHT*12/'R.RDOHT'/                                 144
      INTEGER MTPCR_RDOHT(2)/9,1/                                         144
      REAL RE_TPCR_RDOHT                                                  144
C -                                                                       144
      INTEGER LEN_TPCR,LLEN_TPCR,LINK_TPCR                                144
      PARAMETER (LEN_TPCR=15)                                             144
      PARAMETER (LLEN_TPCR=5)                                             144
      INTEGER FLAG_TPCR/0/,MAP_TPCR(2,LLEN_TPCR),IDNUM_TPCR(10)           144
      CHARACTER NAMES_TPCR(LLEN_TPCR)*12,COMMEN_TPCR(LLEN_TPCR)*40        144
      DATA IDNUM_TPCR/10*0/                                               144
      REAL BANK_TPCR(LEN_TPCR)                                            144
      CHARACTER*60 BPATH_TPCR/' '/                                        144
      SAVE FLAG_TPCR,MAP_TPCR,IDNUM_TPCR,NAMES_TPCR,COMMEN_TPCR,          144
     *BPATH_TPCR                                                          144
      DATA LINK_TPCR/0/,BANK_TPCR/LEN_TPCR*0/                             144
C *                                                                       144
C -                                                                       146
      REAL TFEE_VERS                                                      146
      EQUIVALENCE (BANK_TFEE(3),TFEE_VERS,RE_TFEE_VERS),(NAMES_TFEE(1),   146
     *NTFEE_VERS)                                                         146
      EQUIVALENCE (COMMEN_TFEE(1),CTFEE_VERS),(MAP_TFEE(1,1),             146
     *MTFEE_VERS)                                                         146
      CHARACTER CTFEE_VERS*40/' '/                                        146
      CHARACTER NTFEE_VERS*12/'R.VERS'/                                   146
      INTEGER MTFEE_VERS(2)/1,1/                                          146
      REAL RE_TFEE_VERS                                                   146
C -                                                                       146
      REAL TFEE_CARDDX                                                    146
      EQUIVALENCE (BANK_TFEE(4),TFEE_CARDDX,RE_TFEE_CARDDX),              146
     *(NAMES_TFEE(2),NTFEE_CARDDX)                                        146
      EQUIVALENCE (COMMEN_TFEE(2),CTFEE_CARDDX),(MAP_TFEE(1,2),           146
     *MTFEE_CARDDX)                                                       146
      CHARACTER CTFEE_CARDDX*40/' '/                                      146
      CHARACTER NTFEE_CARDDX*12/'R.CARDDX'/                               146
      INTEGER MTFEE_CARDDX(2)/1,1/                                        146
      REAL RE_TFEE_CARDDX                                                 146
C -                                                                       146
      REAL TFEE_CARDDY                                                    146
      EQUIVALENCE (BANK_TFEE(5),TFEE_CARDDY,RE_TFEE_CARDDY),              146
     *(NAMES_TFEE(3),NTFEE_CARDDY)                                        146
      EQUIVALENCE (COMMEN_TFEE(3),CTFEE_CARDDY),(MAP_TFEE(1,3),           146
     *MTFEE_CARDDY)                                                       146
      CHARACTER CTFEE_CARDDY*40/' '/                                      146
      CHARACTER NTFEE_CARDDY*12/'R.CARDDY'/                               146
      INTEGER MTFEE_CARDDY(2)/1,1/                                        146
      REAL RE_TFEE_CARDDY                                                 146
C -                                                                       146
      REAL TFEE_CARDDZ                                                    146
      EQUIVALENCE (BANK_TFEE(6),TFEE_CARDDZ,RE_TFEE_CARDDZ),              146
     *(NAMES_TFEE(4),NTFEE_CARDDZ)                                        146
      EQUIVALENCE (COMMEN_TFEE(4),CTFEE_CARDDZ),(MAP_TFEE(1,4),           146
     *MTFEE_CARDDZ)                                                       146
      CHARACTER CTFEE_CARDDZ*40/' '/                                      146
      CHARACTER NTFEE_CARDDZ*12/'R.CARDDZ'/                               146
      INTEGER MTFEE_CARDDZ(2)/1,1/                                        146
      REAL RE_TFEE_CARDDZ                                                 146
C -                                                                       146
      REAL TFEE_PLATEDX                                                   146
      EQUIVALENCE (BANK_TFEE(7),TFEE_PLATEDX,RE_TFEE_PLATEDX),            146
     *(NAMES_TFEE(5),NTFEE_PLATEDX)                                       146
      EQUIVALENCE (COMMEN_TFEE(5),CTFEE_PLATEDX),(MAP_TFEE(1,5),          146
     *MTFEE_PLATEDX)                                                      146
      CHARACTER CTFEE_PLATEDX*40/' '/                                     146
      CHARACTER NTFEE_PLATEDX*12/'R.PLATEDX'/                             146
      INTEGER MTFEE_PLATEDX(2)/1,1/                                       146
      REAL RE_TFEE_PLATEDX                                                146
C -                                                                       146
      REAL TFEE_PLATEDY                                                   146
      EQUIVALENCE (BANK_TFEE(8),TFEE_PLATEDY,RE_TFEE_PLATEDY),            146
     *(NAMES_TFEE(6),NTFEE_PLATEDY)                                       146
      EQUIVALENCE (COMMEN_TFEE(6),CTFEE_PLATEDY),(MAP_TFEE(1,6),          146
     *MTFEE_PLATEDY)                                                      146
      CHARACTER CTFEE_PLATEDY*40/' '/                                     146
      CHARACTER NTFEE_PLATEDY*12/'R.PLATEDY'/                             146
      INTEGER MTFEE_PLATEDY(2)/1,1/                                       146
      REAL RE_TFEE_PLATEDY                                                146
C -                                                                       146
      REAL TFEE_PLATEDZ                                                   146
      EQUIVALENCE (BANK_TFEE(9),TFEE_PLATEDZ,RE_TFEE_PLATEDZ),            146
     *(NAMES_TFEE(7),NTFEE_PLATEDZ)                                       146
      EQUIVALENCE (COMMEN_TFEE(7),CTFEE_PLATEDZ),(MAP_TFEE(1,7),          146
     *MTFEE_PLATEDZ)                                                      146
      CHARACTER CTFEE_PLATEDZ*40/' '/                                     146
      CHARACTER NTFEE_PLATEDZ*12/'R.PLATEDZ'/                             146
      INTEGER MTFEE_PLATEDZ(2)/1,1/                                       146
      REAL RE_TFEE_PLATEDZ                                                146
C -                                                                       146
      REAL TFEE_ASSEMBLYTHICKNESS                                         146
      EQUIVALENCE (BANK_TFEE(10),TFEE_ASSEMBLYTHICKNESS,                  146
     *RE_TFEE_ASSEMBLYTHICKNESS),(NAMES_TFEE(8),                          146
     *NTFEE_ASSEMBLYTHICKNESS)                                            146
      EQUIVALENCE (COMMEN_TFEE(8),CTFEE_ASSEMBLYTHICKNESS),(MAP_TFEE(1,   146
     *8),MTFEE_ASSEMBLYTHICKNESS)                                         146
      CHARACTER CTFEE_ASSEMBLYTHICKNESS*40/' '/                           146
      CHARACTER NTFEE_ASSEMBLYTHICKNESS*12/'R.ASSEMBLYTHICKNESS'/         146
      INTEGER MTFEE_ASSEMBLYTHICKNESS(2)/1,1/                             146
      REAL RE_TFEE_ASSEMBLYTHICKNESS                                      146
C -                                                                       146
      REAL TFEE_RIBDX                                                     146
      EQUIVALENCE (BANK_TFEE(11),TFEE_RIBDX,RE_TFEE_RIBDX),               146
     *(NAMES_TFEE(9),NTFEE_RIBDX)                                         146
      EQUIVALENCE (COMMEN_TFEE(9),CTFEE_RIBDX),(MAP_TFEE(1,9),            146
     *MTFEE_RIBDX)                                                        146
      CHARACTER CTFEE_RIBDX*40/' '/                                       146
      CHARACTER NTFEE_RIBDX*12/'R.RIBDX'/                                 146
      INTEGER MTFEE_RIBDX(2)/1,1/                                         146
      REAL RE_TFEE_RIBDX                                                  146
C -                                                                       146
      REAL TFEE_RIBDY                                                     146
      EQUIVALENCE (BANK_TFEE(12),TFEE_RIBDY,RE_TFEE_RIBDY),               146
     *(NAMES_TFEE(10),NTFEE_RIBDY)                                        146
      EQUIVALENCE (COMMEN_TFEE(10),CTFEE_RIBDY),(MAP_TFEE(1,10),          146
     *MTFEE_RIBDY)                                                        146
      CHARACTER CTFEE_RIBDY*40/' '/                                       146
      CHARACTER NTFEE_RIBDY*12/'R.RIBDY'/                                 146
      INTEGER MTFEE_RIBDY(2)/1,1/                                         146
      REAL RE_TFEE_RIBDY                                                  146
C -                                                                       146
      REAL TFEE_RIBDZ                                                     146
      EQUIVALENCE (BANK_TFEE(13),TFEE_RIBDZ,RE_TFEE_RIBDZ),               146
     *(NAMES_TFEE(11),NTFEE_RIBDZ)                                        146
      EQUIVALENCE (COMMEN_TFEE(11),CTFEE_RIBDZ),(MAP_TFEE(1,11),          146
     *MTFEE_RIBDZ)                                                        146
      CHARACTER CTFEE_RIBDZ*40/' '/                                       146
      CHARACTER NTFEE_RIBDZ*12/'R.RIBDZ'/                                 146
      INTEGER MTFEE_RIBDZ(2)/1,1/                                         146
      REAL RE_TFEE_RIBDZ                                                  146
C -                                                                       146
      REAL TFEE_ASS(3)                                                    146
      EQUIVALENCE (BANK_TFEE(14),TFEE_ASS,RE_TFEE_ASS),(NAMES_TFEE(12),   146
     *NTFEE_ASS)                                                          146
      EQUIVALENCE (COMMEN_TFEE(12),CTFEE_ASS),(MAP_TFEE(1,12),            146
     *MTFEE_ASS)                                                          146
      CHARACTER CTFEE_ASS*40/' '/                                         146
      CHARACTER NTFEE_ASS*12/'R.ASS'/                                     146
      INTEGER MTFEE_ASS(2)/3,1/                                           146
      REAL RE_TFEE_ASS                                                    146
C -                                                                       146
      INTEGER LEN_TFEE,LLEN_TFEE,LINK_TFEE                                146
      PARAMETER (LEN_TFEE=16)                                             146
      PARAMETER (LLEN_TFEE=12)                                            146
      INTEGER FLAG_TFEE/0/,MAP_TFEE(2,LLEN_TFEE),IDNUM_TFEE(10)           146
      CHARACTER NAMES_TFEE(LLEN_TFEE)*12,COMMEN_TFEE(LLEN_TFEE)*40        146
      DATA IDNUM_TFEE/10*0/                                               146
      REAL BANK_TFEE(LEN_TFEE)                                            146
      CHARACTER*60 BPATH_TFEE/' '/                                        146
      SAVE FLAG_TFEE,MAP_TFEE,IDNUM_TFEE,NAMES_TFEE,COMMEN_TFEE,          146
     *BPATH_TFEE                                                          146
      DATA LINK_TFEE/0/,BANK_TFEE/LEN_TFEE*0/                             146
C *                                                                       146
C -                                                                       147
      REAL RIBS_VERSION                                                   147
      EQUIVALENCE (BANK_RIBS(3),RIBS_VERSION,RE_RIBS_VERSION),            147
     *(NAMES_RIBS(1),NRIBS_VERSION)                                       147
      EQUIVALENCE (COMMEN_RIBS(1),CRIBS_VERSION),(MAP_RIBS(1,1),          147
     *MRIBS_VERSION)                                                      147
      CHARACTER CRIBS_VERSION*40/' '/                                     147
      CHARACTER NRIBS_VERSION*12/'R.VERSION'/                             147
      INTEGER MRIBS_VERSION(2)/1,1/                                       147
      REAL RE_RIBS_VERSION                                                147
C -                                                                       147
      REAL RIBS_INOUT                                                     147
      EQUIVALENCE (BANK_RIBS(4),RIBS_INOUT,RE_RIBS_INOUT),                147
     *(NAMES_RIBS(2),NRIBS_INOUT)                                         147
      EQUIVALENCE (COMMEN_RIBS(2),CRIBS_INOUT),(MAP_RIBS(1,2),            147
     *MRIBS_INOUT)                                                        147
      CHARACTER CRIBS_INOUT*40/' '/                                       147
      CHARACTER NRIBS_INOUT*12/'R.INOUT'/                                 147
      INTEGER MRIBS_INOUT(2)/1,1/                                         147
      REAL RE_RIBS_INOUT                                                  147
C -                                                                       147
      REAL RIBS_NUM                                                       147
      EQUIVALENCE (BANK_RIBS(5),RIBS_NUM,RE_RIBS_NUM),(NAMES_RIBS(3),     147
     *NRIBS_NUM)                                                          147
      EQUIVALENCE (COMMEN_RIBS(3),CRIBS_NUM),(MAP_RIBS(1,3),MRIBS_NUM)    147
      CHARACTER CRIBS_NUM*40/' '/                                         147
      CHARACTER NRIBS_NUM*12/'R.NUM'/                                     147
      INTEGER MRIBS_NUM(2)/1,1/                                           147
      REAL RE_RIBS_NUM                                                    147
C -                                                                       147
      REAL RIBS_X(16)                                                     147
      EQUIVALENCE (BANK_RIBS(6),RIBS_X,RE_RIBS_X),(NAMES_RIBS(4),         147
     *NRIBS_X)                                                            147
      EQUIVALENCE (COMMEN_RIBS(4),CRIBS_X),(MAP_RIBS(1,4),MRIBS_X)        147
      CHARACTER CRIBS_X*40/' '/                                           147
      CHARACTER NRIBS_X*12/'R.X'/                                         147
      INTEGER MRIBS_X(2)/16,1/                                            147
      REAL RE_RIBS_X                                                      147
C -                                                                       147
      REAL RIBS_DX(16)                                                    147
      EQUIVALENCE (BANK_RIBS(22),RIBS_DX,RE_RIBS_DX),(NAMES_RIBS(5),      147
     *NRIBS_DX)                                                           147
      EQUIVALENCE (COMMEN_RIBS(5),CRIBS_DX),(MAP_RIBS(1,5),MRIBS_DX)      147
      CHARACTER CRIBS_DX*40/' '/                                          147
      CHARACTER NRIBS_DX*12/'R.DX'/                                       147
      INTEGER MRIBS_DX(2)/16,1/                                           147
      REAL RE_RIBS_DX                                                     147
C -                                                                       147
      REAL RIBS_Y(16)                                                     147
      EQUIVALENCE (BANK_RIBS(38),RIBS_Y,RE_RIBS_Y),(NAMES_RIBS(6),        147
     *NRIBS_Y)                                                            147
      EQUIVALENCE (COMMEN_RIBS(6),CRIBS_Y),(MAP_RIBS(1,6),MRIBS_Y)        147
      CHARACTER CRIBS_Y*40/' '/                                           147
      CHARACTER NRIBS_Y*12/'R.Y'/                                         147
      INTEGER MRIBS_Y(2)/16,1/                                            147
      REAL RE_RIBS_Y                                                      147
C -                                                                       147
      REAL RIBS_DY(16)                                                    147
      EQUIVALENCE (BANK_RIBS(54),RIBS_DY,RE_RIBS_DY),(NAMES_RIBS(7),      147
     *NRIBS_DY)                                                           147
      EQUIVALENCE (COMMEN_RIBS(7),CRIBS_DY),(MAP_RIBS(1,7),MRIBS_DY)      147
      CHARACTER CRIBS_DY*40/' '/                                          147
      CHARACTER NRIBS_DY*12/'R.DY'/                                       147
      INTEGER MRIBS_DY(2)/16,1/                                           147
      REAL RE_RIBS_DY                                                     147
C -                                                                       147
      INTEGER LEN_RIBS,LLEN_RIBS,LINK_RIBS                                147
      PARAMETER (LEN_RIBS=69)                                             147
      PARAMETER (LLEN_RIBS=7)                                             147
      INTEGER FLAG_RIBS/0/,MAP_RIBS(2,LLEN_RIBS),IDNUM_RIBS(10)           147
      CHARACTER NAMES_RIBS(LLEN_RIBS)*12,COMMEN_RIBS(LLEN_RIBS)*40        147
      DATA IDNUM_RIBS/10*0/                                               147
      REAL BANK_RIBS(LEN_RIBS)                                            147
      CHARACTER*60 BPATH_RIBS/' '/                                        147
      SAVE FLAG_RIBS,MAP_RIBS,IDNUM_RIBS,NAMES_RIBS,COMMEN_RIBS,          147
     *BPATH_RIBS                                                          147
      DATA LINK_RIBS/0/,BANK_RIBS/LEN_RIBS*0/                             147
C *                                                                       147
C -                                                                       148
      INTEGER HOLZ_INOUT                                                  148
      EQUIVALENCE (BANK_HOLZ(3),HOLZ_INOUT,RE_HOLZ_INOUT),                148
     *(NAMES_HOLZ(1),NHOLZ_INOUT)                                         148
      EQUIVALENCE (COMMEN_HOLZ(1),CHOLZ_INOUT),(MAP_HOLZ(1,1),            148
     *MHOLZ_INOUT)                                                        148
      CHARACTER CHOLZ_INOUT*40/' '/                                       148
      CHARACTER NHOLZ_INOUT*12/'I.INOUT'/                                 148
      INTEGER MHOLZ_INOUT(2)/1,1/                                         148
      REAL RE_HOLZ_INOUT                                                  148
C -                                                                       148
      INTEGER HOLZ_NUMBEROFROWS                                           148
      EQUIVALENCE (BANK_HOLZ(4),HOLZ_NUMBEROFROWS,RE_HOLZ_NUMBEROFROWS),  148
     *(NAMES_HOLZ(2),NHOLZ_NUMBEROFROWS)                                  148
      EQUIVALENCE (COMMEN_HOLZ(2),CHOLZ_NUMBEROFROWS),(MAP_HOLZ(1,2),     148
     *MHOLZ_NUMBEROFROWS)                                                 148
      CHARACTER CHOLZ_NUMBEROFROWS*40/' '/                                148
      CHARACTER NHOLZ_NUMBEROFROWS*12/'I.NUMBEROFROWS'/                   148
      INTEGER MHOLZ_NUMBEROFROWS(2)/1,1/                                  148
      REAL RE_HOLZ_NUMBEROFROWS                                           148
C -                                                                       148
      INTEGER HOLZ_NUMBERPERROW(16)                                       148
      EQUIVALENCE (BANK_HOLZ(5),HOLZ_NUMBERPERROW,RE_HOLZ_NUMBERPERROW),  148
     *(NAMES_HOLZ(3),NHOLZ_NUMBERPERROW)                                  148
      EQUIVALENCE (COMMEN_HOLZ(3),CHOLZ_NUMBERPERROW),(MAP_HOLZ(1,3),     148
     *MHOLZ_NUMBERPERROW)                                                 148
      CHARACTER CHOLZ_NUMBERPERROW*40/' '/                                148
      CHARACTER NHOLZ_NUMBERPERROW*12/'I.NUMBERPERROW'/                   148
      INTEGER MHOLZ_NUMBERPERROW(2)/16,1/                                 148
      REAL RE_HOLZ_NUMBERPERROW                                           148
C -                                                                       148
      REAL HOLZ_YHOLES(160)                                               148
      EQUIVALENCE (BANK_HOLZ(21),HOLZ_YHOLES,RE_HOLZ_YHOLES),             148
     *(NAMES_HOLZ(4),NHOLZ_YHOLES)                                        148
      EQUIVALENCE (COMMEN_HOLZ(4),CHOLZ_YHOLES),(MAP_HOLZ(1,4),           148
     *MHOLZ_YHOLES)                                                       148
      CHARACTER CHOLZ_YHOLES*40/' '/                                      148
      CHARACTER NHOLZ_YHOLES*12/'R.YHOLES'/                               148
      INTEGER MHOLZ_YHOLES(2)/160,1/                                      148
      REAL RE_HOLZ_YHOLES                                                 148
C -                                                                       148
      INTEGER LEN_HOLZ,LLEN_HOLZ,LINK_HOLZ                                148
      PARAMETER (LEN_HOLZ=180)                                            148
      PARAMETER (LLEN_HOLZ=4)                                             148
      INTEGER FLAG_HOLZ/0/,MAP_HOLZ(2,LLEN_HOLZ),IDNUM_HOLZ(10)           148
      CHARACTER NAMES_HOLZ(LLEN_HOLZ)*12,COMMEN_HOLZ(LLEN_HOLZ)*40        148
      DATA IDNUM_HOLZ/10*0/                                               148
      REAL BANK_HOLZ(LEN_HOLZ)                                            148
      CHARACTER*60 BPATH_HOLZ/' '/                                        148
      SAVE FLAG_HOLZ,MAP_HOLZ,IDNUM_HOLZ,NAMES_HOLZ,COMMEN_HOLZ,          148
     *BPATH_HOLZ                                                          148
      DATA LINK_HOLZ/0/,BANK_HOLZ/LEN_HOLZ*0/                             148
C *                                                                       148
C -                                                                       149
      INTEGER COOL_INOUT                                                  149
      EQUIVALENCE (BANK_COOL(3),COOL_INOUT,RE_COOL_INOUT),                149
     *(NAMES_COOL(1),NCOOL_INOUT)                                         149
      EQUIVALENCE (COMMEN_COOL(1),CCOOL_INOUT),(MAP_COOL(1,1),            149
     *MCOOL_INOUT)                                                        149
      CHARACTER CCOOL_INOUT*40/' '/                                       149
      CHARACTER NCOOL_INOUT*12/'I.INOUT'/                                 149
      INTEGER MCOOL_INOUT(2)/1,1/                                         149
      REAL RE_COOL_INOUT                                                  149
C -                                                                       149
      REAL COOL_TUBELENGTH(16)                                            149
      EQUIVALENCE (BANK_COOL(4),COOL_TUBELENGTH,RE_COOL_TUBELENGTH),      149
     *(NAMES_COOL(2),NCOOL_TUBELENGTH)                                    149
      EQUIVALENCE (COMMEN_COOL(2),CCOOL_TUBELENGTH),(MAP_COOL(1,2),       149
     *MCOOL_TUBELENGTH)                                                   149
      CHARACTER CCOOL_TUBELENGTH*40/' '/                                  149
      CHARACTER NCOOL_TUBELENGTH*12/'R.TUBELENGTH'/                       149
      INTEGER MCOOL_TUBELENGTH(2)/16,1/                                   149
      REAL RE_COOL_TUBELENGTH                                             149
C -                                                                       149
      INTEGER LEN_COOL,LLEN_COOL,LINK_COOL                                149
      PARAMETER (LEN_COOL=19)                                             149
      PARAMETER (LLEN_COOL=2)                                             149
      INTEGER FLAG_COOL/0/,MAP_COOL(2,LLEN_COOL),IDNUM_COOL(10)           149
      CHARACTER NAMES_COOL(LLEN_COOL)*12,COMMEN_COOL(LLEN_COOL)*40        149
      DATA IDNUM_COOL/10*0/                                               149
      REAL BANK_COOL(LEN_COOL)                                            149
      CHARACTER*60 BPATH_COOL/' '/                                        149
      SAVE FLAG_COOL,MAP_COOL,IDNUM_COOL,NAMES_COOL,COMMEN_COOL,          149
     *BPATH_COOL                                                          149
      DATA LINK_COOL/0/,BANK_COOL/LEN_COOL*0/                             149
C *                                                                       149
C *                                                                       150
      CALL AGSBEGM(MODULE,IPRIN)                                          150
      IF (IPRIN.LT.0) RETURN                                              150
C Filling bank TPCG - // TPC BASIC DIMENSIONS                             150
      BANK_TITLE='// TPC BASIC DIMENSIONS'                                150
      BPATH_TPCG='TPCG*'                                                  150
      IDNUM_TPCG(1)=0                                                     151
C VERSION 10: OFFSET TO INDICATE NEW LINE OF TPC                          151
      TPCG_VERSION=10                                                     151
      CTPCG_VERSION='VERSION 10: OFFSET TO INDICATE NEW LINE OF TPC'      151
C // RMIN => TPC ENVELOPE INNER RADIUS                                    152
      TPCG_RMIN=46.107                                                    152
      CTPCG_RMIN='// RMIN => TPC ENVELOPE INNER RADIUS'                   152
C // RMAX => TPC ENVELOPE OUTER RADIUS                                    153
      TPCG_RMAX=206.75/COS15                                              153
      CTPCG_RMAX='// RMAX => TPC ENVELOPE OUTER RADIUS'                   153
C // RMINIFC => INNER RADIUS TPC IFC : A.LEBEDEV MEASUREMENT 10/16/08     154
      TPCG_RMINIFC=46.6                                                   154
      CTPCG_RMINIFC='// RMINIFC => INNER RADIUS TPC IFC : A.LEBEDEV MEAS  154
     *UREMENT 10/16/08'                                                   154
C // LENGTHT => TPC FULL LENGTH UP TO FRONT OF EEMC                       155
      TPCG_LENGTHT=2*271.0                                                155
      CTPCG_LENGTHT='// LENGTHT => TPC FULL LENGTH UP TO FRONT OF EEMC'   155
C // LENGTH => TPC FULL LENGTH INCLUDING RDOS                             156
      TPCG_LENGTH=2*259.685                                               156
      CTPCG_LENGTH='// LENGTH => TPC FULL LENGTH INCLUDING RDOS'          156
C // LENGTHW => TPC LENGTH INCLUDING WHEEL                                157
      TPCG_LENGTHW=2*229.71                                               157
      CTPCG_LENGTHW='// LENGTHW => TPC LENGTH INCLUDING WHEEL'            157
C // LENGTHV => TPC GAS VOLUME LENGTH                                     158
      TPCG_LENGTHV=2*210.00                                               158
      CTPCG_LENGTHV='// LENGTHV => TPC GAS VOLUME LENGTH'                 158
C // TPCE ENVELOP DZ                                                      159
      TPCG_DZENVELOP=268.                                                 159
      CTPCG_DZENVELOP='// TPCE ENVELOP DZ'                                159
C // 49.60 WHEELIR => SUPPORT WHEEL INNER RADIUS                          160
      TPCG_WHEELIR=38.620*INCH/2                                          160
      CTPCG_WHEELIR='// 49.60 WHEELIR => SUPPORT WHEEL INNER RADIUS'      160
C // WHEELR0 => DISTANCE FROM IP OF END OF INNER CYLINDRICAL PART         161
      TPCG_WHEELR0=21.500*INCH                                            161
      CTPCG_WHEELR0='// WHEELR0 => DISTANCE FROM IP OF END OF INNER CYLI  161
     *NDRICAL PART'                                                       161
C // WHEELR1 => DISTANCE FROM IP OF MIDDLE RIB                            162
      TPCG_WHEELR1=47.867*INCH                                            162
      CTPCG_WHEELR1='// WHEELR1 => DISTANCE FROM IP OF MIDDLE RIB'        162
C // WHEELR2 => DISTANCE FROM IP OF UPPER RIB                             163
      TPCG_WHEELR2=76.093*INCH                                            163
      CTPCG_WHEELR2='// WHEELR2 => DISTANCE FROM IP OF UPPER RIB'         163
C // WHEELTOTALRIBWIDTH => TOTAL RIB WIDTH (A.LEBEDEV MEASUREMENT)        164
      TPCG_WHEELTOTALRIBWIDTH=7.6                                         164
      CTPCG_WHEELTOTALRIBWIDTH='// WHEELTOTALRIBWIDTH => TOTAL RIB WIDTH  164
     * (A.LEBEDEV MEASUREMENT)'                                           164
C // WHEELRIBWIDTH => RIB PLATE WIDTH (A.LEBEDEV MEASUREMENT)             165
      TPCG_WHEELRIBWIDTH=1.9                                              165
      CTPCG_WHEELRIBWIDTH='// WHEELRIBWIDTH => RIB PLATE WIDTH (A.LEBEDE  165
     *V MEASUREMENT)'                                                     165
C // WHEELRIBHEIGHT => RIB PLATE HEIGHT (A.LEBEDEV MEASUREMENT)           166
      TPCG_WHEELRIBHEIGHT=1.3                                             166
      CTPCG_WHEELRIBHEIGHT='// WHEELRIBHEIGHT => RIB PLATE HEIGHT (A.LEB  166
     *EDEV MEASUREMENT)'                                                  166
C // WHEELBOXDX (A.LEBEDEV MEASUREMENT)                                   167
      TPCG_WHEELBOXDX=4.7                                                 167
      CTPCG_WHEELBOXDX='// WHEELBOXDX (A.LEBEDEV MEASUREMENT)'            167
C // WHEELBOXDY (A.LEBEDEV MEASUREMENT)                                   168
      TPCG_WHEELBOXDY=7.6                                                 168
      CTPCG_WHEELBOXDY='// WHEELBOXDY (A.LEBEDEV MEASUREMENT)'            168
C // WHEELOR1 => @ TOFC                                                   169
      TPCG_WHEELOR1=201.0                                                 169
      CTPCG_WHEELOR1='// WHEELOR1 => @ TOFC'                              169
C // WHEELOR => SUPPORT WHEEL OUTER RADIUS                                170
      TPCG_WHEELOR=206.75                                                 170
      CTPCG_WHEELOR='// WHEELOR => SUPPORT WHEEL OUTER RADIUS'            170
C // WHEELTHK1                                                            171
      TPCG_WHEELTHK1=5.72                                                 171
      CTPCG_WHEELTHK1='// WHEELTHK1'                                      171
C // WHEELTHK => SUPPORT WHEEL LENGTH                                     172
      TPCG_WHEELTHK=11.43                                                 172
      CTPCG_WHEELTHK='// WHEELTHK => SUPPORT WHEEL LENGTH'                172
C // SENGASOR => TPC SENSITIVE GAS OUTER RADIUS                           173
      TPCG_SENGASOR=200                                                   173
      CTPCG_SENGASOR='// SENGASOR => TPC SENSITIVE GAS OUTER RADIUS'      173
C // MEMBTHK => CENTRAL MEMBRANE THICKNESS                                174
      TPCG_MEMBTHK=0.00762                                                174
      CTPCG_MEMBTHK='// MEMBTHK => CENTRAL MEMBRANE THICKNESS'            174
C // TOCSDR => OUTER COPPER THICKNESS: NIM A499 (2003) 659-678 TABLE 2    175
      TPCG_TOCSDR=0.013                                                   175
      CTPCG_TOCSDR='// TOCSDR => OUTER COPPER THICKNESS: NIM A499 (2003)  175
     * 659-678 TABLE 2'                                                   175
C // TOKADR => OUTER KAPTON THICKNESS                                     176
      TPCG_TOKADR=0.015                                                   176
      CTPCG_TOKADR='// TOKADR => OUTER KAPTON THICKNESS'                  176
C // TONXDR => OUTER NOMEX THICKNESS                                      177
      TPCG_TONXDR=0.953                                                   177
      CTPCG_TONXDR='// TONXDR => OUTER NOMEX THICKNESS'                   177
C // TOADDR => OUTER ADHESIVE THICKNESS                                   178
      TPCG_TOADDR=0.05                                                    178
      CTPCG_TOADDR='// TOADDR => OUTER ADHESIVE THICKNESS'                178
C // TOIGDR => OUTER ISOLATING GAS THICKNESS                              179
      TPCG_TOIGDR=5.70                                                    179
      CTPCG_TOIGDR='// TOIGDR => OUTER ISOLATING GAS THICKNESS'           179
C // TOALDR => OUTER ALUMINUM THICKNESS (FOR BOTH LAYERS)                 180
      TPCG_TOALDR=0.40                                                    180
      CTPCG_TOALDR='// TOALDR => OUTER ALUMINUM THICKNESS (FOR BOTH LAYE  180
     *RS)'                                                                180
C // TOHADR => OUTER HONEYCOMB AL THICKNESS                               181
      TPCG_TOHADR=0.60                                                    181
      CTPCG_TOHADR='// TOHADR => OUTER HONEYCOMB AL THICKNESS'            181
C // TIADDR => INNER ADHESIVE LAYER THICKNESS: NIM A499 (2003) 659-678 T  182
C ABLE 2                                                                  182
      TPCG_TIADDR=0.080                                                   182
      CTPCG_TIADDR='// TIADDR => INNER ADHESIVE LAYER THICKNESS: NIM A49  182
     *9 (2003) 659-678 TABLE 2'                                           182
C // TINXDR => INNER NOMEX STRUCTUREURE THICKNESS                         183
      TPCG_TINXDR=1.270                                                   183
      CTPCG_TINXDR='// TINXDR => INNER NOMEX STRUCTUREURE THICKNESS'      183
C // TIKADR => INNER KAPTON LAYER THICKNESS                               184
      TPCG_TIKADR=0.015                                                   184
      CTPCG_TIKADR='// TIKADR => INNER KAPTON LAYER THICKNESS'            184
C // TIALDR => INNER ALUMINUM LAYER THICKNESS                             185
      TPCG_TIALDR=0.004                                                   185
      CTPCG_TIALDR='// TIALDR => INNER ALUMINUM LAYER THICKNESS'          185
C // TIFCRF => OUTER RADIUS OF IFC FLANGE                                 186
      TPCG_TIFCRF=51.7                                                    186
      CTPCG_TIFCRF='// TIFCRF => OUTER RADIUS OF IFC FLANGE'              186
C // TIFCDRT => TOLERANCE BETWEEN FLANGE AND WHEEL                        187
      TPCG_TIFCDRT=0.1                                                    187
      CTPCG_TIFCDRT='// TIFCDRT => TOLERANCE BETWEEN FLANGE AND WHEEL'    187
C // DZ OF YF1                                                            188
      TPCG_DZYF1=221.162                                                  188
      CTPCG_DZYF1='// DZ OF YF1'                                          188
C // DZ OF YF2                                                            189
      TPCG_DZYF2=211.9                                                    189
      CTPCG_DZYF2='// DZ OF YF2'                                          189
C // DZ OF YF3                                                            190
      TPCG_DZYF3=208.02                                                   190
      CTPCG_DZYF3='// DZ OF YF3'                                          190
C // 1.82 MM DRAWING 24A0314 & 24A0304                                    191
      TPCG_PADPLANETHICKNESS=0.182                                        191
      CTPCG_PADPLANETHICKNESS='// 1.82 MM DRAWING 24A0314 & 24A0304'      191
C //                                                                      192
      TPCG_DGATEGROUND=0.6                                                192
      CTPCG_DGATEGROUND='//'                                              192
C //                                                                      193
      TPCG_WIREMOUNTWIDTH=5*((0.130+0.1376)/2)*INCH                       193
      CTPCG_WIREMOUNTWIDTH='//'                                           193
C // DRAWING 24A0434                                                      194
      TPCG_WIREMOUNTHEIGHT=1.340*INCH                                     194
      CTPCG_WIREMOUNTHEIGHT='// DRAWING 24A0434'                          194
C // A.LEBEDEV 1 RDO = 5 LB                                               195
      TPCG_DXRDO=1.75/2                                                   195
      CTPCG_DXRDO='// A.LEBEDEV 1 RDO = 5 LB'                             195
C //                                                                      196
      TPCG_DYRDO=45./2                                                    196
      CTPCG_DYRDO='//'                                                    196
C //                                                                      197
      TPCG_DZRDO=17.0/2                                                   197
      CTPCG_DZRDO='//'                                                    197
C //                                                                      198
      TPCG_ZRDO=TPCG_LENGTHW/2 + 20.0                                     198
      CTPCG_ZRDO='//'                                                     198
C // X COOLING TUBE DRAWING 24A0801B                                      199
      TPCG_HEIGTUBE=0.703*INCH                                            199
      CTPCG_HEIGTUBE='// X COOLING TUBE DRAWING 24A0801B'                 199
C // Z MIXTURE TPCE_WATER_PIPE => RHO = 2.32155 G/CM**3                   200
      TPCG_WIDTUBE=0.500*INCH                                             200
      CTPCG_WIDTUBE='// Z MIXTURE TPCE_WATER_PIPE => RHO = 2.32155 G/CM*  200
     **3'                                                                 200
C //                                                                      201
      TPCG_RDOCOOLINGDX=(38.0 + 9.0 + 58.0)/2                             201
      CTPCG_RDOCOOLINGDX='//'                                             201
C //                                                                      202
      TPCG_RDOCOOLINGDY=1.25                                              202
      CTPCG_RDOCOOLINGDY='//'                                             202
C //                                                                      203
      TPCG_RDOCOOLINGDZ=2.50                                              203
      CTPCG_RDOCOOLINGDZ='//'                                             203
C // INNER/OUTER ANODE WIDTH                                              204
      TPCG_DANODE(1)=0.2                                                  204
      TPCG_DANODE(2)=0.4                                                  204
      CTPCG_DANODE='// INNER/OUTER ANODE WIDTH'                           204
C //                                                                      205
      TPCG_ZGATINGGRID=TPCG_LENGTHV/2                                     205
     *-(TPCG_DGATEGROUND+2*TPCG_DANODE(2))                                205
      CTPCG_ZGATINGGRID='//'                                              205
C //                                                                      206
      TPCG_ZGROUNDGRID=TPCG_ZGATINGGRID+TPCG_DGATEGROUND                  206
      CTPCG_ZGROUNDGRID='//'                                              206
C // DEAD ZONE BEFORE GATINGGRID. NO HITS THERE                           207
      TPCG_DEADZONE=12.                                                   207
      CTPCG_DEADZONE='// DEAD ZONE BEFORE GATINGGRID. NO HITS THERE'      207
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCG',    208
     *BPATH_TPCG,IDNUM_TPCG,1, MAP_TPCG,NAMES_TPCG,COMMEN_TPCG,           208
     *BANK_TPCG,LEN_TPCG,LLEN_TPCG,LINK_TPCG,FLAG_TPCG)                   208
C *                                                                       208
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCG',    208
     *BPATH_TPCG,IDNUM_TPCG,1, MAP_TPCG,NAMES_TPCG,COMMEN_TPCG,           208
     *BANK_TPCG,LEN_TPCG,LLEN_TPCG,LINK_TPCG,FLAG_TPCG)                   208
C use default TPCG bank                                                   209
      BPATH_TPCG='TPCG*'                                                  209
      IDNUM_TPCG(1)=0                                                     209
      CALL AGDGETP('TPCXGEO1','*',-999,'TPCG',' ',BANK_TPCG, BPATH_TPCG,  209
     *IDNUM_TPCG,1,MAP_TPCG,NAMES_TPCG,BANK_TPCG,LEN_TPCG,LLEN_TPCG,      209
     *LINK_TPCG,FLAG_TPCG)                                                209
C *                                                                       210
      CALL AGSBEGM(MODULE,IPRIN)                                          210
      IF (IPRIN.LT.0) RETURN                                              210
C Filling bank TPRS - // SECTOR OF PADROWS                                210
      BANK_TITLE='// SECTOR OF PADROWS'                                   210
      BPATH_TPRS='TPRS*'                                                  210
      IDNUM_TPRS(1)=0                                                     211
C // SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                              211
      TPRS_SEC=1                                                          211
      CTPRS_SEC='// SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'              211
C // NUMBER OF PADROWS IN THE SECTOR                                      212
      TPRS_NROW=32                                                        212
      CTPRS_NROW='// NUMBER OF PADROWS IN THE SECTOR'                     212
C // TPC PADROW PITCH WIDTH                                               213
      TPRS_PITCH=0.67                                                     213
      CTPRS_PITCH='// TPC PADROW PITCH WIDTH'                             213
C // TPC PADROW THICKNESS                                                 214
      TPRS_WIDTH=1.95                                                     214
      CTPRS_WIDTH='// TPC PADROW THICKNESS'                               214
C // DISTANCE TO ANODE WIRE FROM PAD PLANE                                215
      TPRS_DANODE=0.4                                                     215
      CTPRS_DANODE='// DISTANCE TO ANODE WIRE FROM PAD PLANE'             215
C // NUMBER OF PADS IN ROW                                                219
      TPRS_NPADS(1)=42                                                    219
      TPRS_NPADS(2)=44                                                    219
      TPRS_NPADS(3)=46                                                    219
      TPRS_NPADS(4)=48                                                    219
      TPRS_NPADS(5)=50                                                    219
      TPRS_NPADS(6)=50                                                    219
      TPRS_NPADS(7)=52                                                    219
      TPRS_NPADS(8)=54                                                    219
      TPRS_NPADS(9)=56                                                    219
      TPRS_NPADS(10)=58                                                   219
      TPRS_NPADS(11)=58                                                   219
      TPRS_NPADS(12)=60                                                   219
      TPRS_NPADS(13)=62                                                   219
      TPRS_NPADS(14)=64                                                   219
      TPRS_NPADS(15)=66                                                   219
      TPRS_NPADS(16)=66                                                   219
      TPRS_NPADS(17)=68                                                   219
      TPRS_NPADS(18)=70                                                   219
      TPRS_NPADS(19)=72                                                   219
      TPRS_NPADS(20)=74                                                   219
      TPRS_NPADS(21)=74                                                   219
      TPRS_NPADS(22)=76                                                   219
      TPRS_NPADS(23)=78                                                   219
      TPRS_NPADS(24)=80                                                   219
      TPRS_NPADS(25)=82                                                   219
      TPRS_NPADS(26)=82                                                   219
      TPRS_NPADS(27)=84                                                   219
      TPRS_NPADS(28)=86                                                   219
      TPRS_NPADS(29)=88                                                   219
      TPRS_NPADS(30)=90                                                   219
      TPRS_NPADS(31)=90                                                   219
      TPRS_NPADS(32)=92                                                   219
      CTPRS_NPADS='// NUMBER OF PADS IN ROW'                              219
C // TPC PADROW RADII                                                     225
      TPRS_RPADS(1)=57.2                                                  225
      TPRS_RPADS(2)=59.2                                                  225
      TPRS_RPADS(3)=61.2                                                  225
      TPRS_RPADS(4)=63.2                                                  225
      TPRS_RPADS(5)=65.2                                                  225
      TPRS_RPADS(6)=67.2                                                  225
      TPRS_RPADS(7)=69.2                                                  225
      TPRS_RPADS(8)=71.2                                                  225
      TPRS_RPADS(9)=73.2                                                  225
      TPRS_RPADS(10)=75.2                                                 225
      TPRS_RPADS(11)=77.2                                                 225
      TPRS_RPADS(12)=79.2                                                 225
      TPRS_RPADS(13)=81.2                                                 225
      TPRS_RPADS(14)=83.2                                                 225
      TPRS_RPADS(15)=85.2                                                 225
      TPRS_RPADS(16)=87.2                                                 225
      TPRS_RPADS(17)=89.2                                                 225
      TPRS_RPADS(18)=91.2                                                 225
      TPRS_RPADS(19)=93.2                                                 225
      TPRS_RPADS(20)=95.2                                                 225
      TPRS_RPADS(21)=97.2                                                 225
      TPRS_RPADS(22)=99.2                                                 225
      TPRS_RPADS(23)=101.2                                                225
      TPRS_RPADS(24)=103.2                                                225
      TPRS_RPADS(25)=105.2                                                225
      TPRS_RPADS(26)=107.2                                                225
      TPRS_RPADS(27)=109.2                                                225
      TPRS_RPADS(28)=111.2                                                225
      TPRS_RPADS(29)=113.2                                                225
      TPRS_RPADS(30)=115.2                                                225
      TPRS_RPADS(31)=117.2                                                225
      TPRS_RPADS(32)=119.2                                                225
      CTPRS_RPADS='// TPC PADROW RADII'                                   225
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    226
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           226
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   226
C *                                                                       226
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    226
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           226
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   226
C *                                                                       227
      CALL AGSBEGM(MODULE,IPRIN)                                          227
      IF (IPRIN.LT.0) RETURN                                              227
C Filling bank TPRS - // SECTOR OF PADROWS                                227
      BANK_TITLE='// SECTOR OF PADROWS'                                   227
      BPATH_TPRS='TPRS*'                                                  227
      IDNUM_TPRS(1)=0                                                     228
C // SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                              228
      TPRS_SEC=2                                                          228
      CTPRS_SEC='// SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'              228
C // NUMBER OF PADROWS IN OUTER SECTOR                                    229
      TPRS_NROW=32                                                        229
      CTPRS_NROW='// NUMBER OF PADROWS IN OUTER SECTOR'                   229
C // OUTER TPC PADROW PITCH WIDTH                                         230
      TPRS_PITCH=0.67                                                     230
      CTPRS_PITCH='// OUTER TPC PADROW PITCH WIDTH'                       230
C // OUTER TPC PADROW THICKNESS                                           231
      TPRS_WIDTH=1.95                                                     231
      CTPRS_WIDTH='// OUTER TPC PADROW THICKNESS'                         231
C // DISTANCE TO ANODE WIRE FROM PAD PLANE                                232
      TPRS_DANODE=0.4                                                     232
      CTPRS_DANODE='// DISTANCE TO ANODE WIRE FROM PAD PLANE'             232
C // NUMBER OF PADS IN ROW                                                236
      TPRS_NPADS(1)=98                                                    236
      TPRS_NPADS(2)=100                                                   236
      TPRS_NPADS(3)=102                                                   236
      TPRS_NPADS(4)=104                                                   236
      TPRS_NPADS(5)=106                                                   236
      TPRS_NPADS(6)=106                                                   236
      TPRS_NPADS(7)=108                                                   236
      TPRS_NPADS(8)=110                                                   236
      TPRS_NPADS(9)=112                                                   236
      TPRS_NPADS(10)=112                                                  236
      TPRS_NPADS(11)=114                                                  236
      TPRS_NPADS(12)=116                                                  236
      TPRS_NPADS(13)=118                                                  236
      TPRS_NPADS(14)=120                                                  236
      TPRS_NPADS(15)=122                                                  236
      TPRS_NPADS(16)=122                                                  236
      TPRS_NPADS(17)=124                                                  236
      TPRS_NPADS(18)=126                                                  236
      TPRS_NPADS(19)=128                                                  236
      TPRS_NPADS(20)=128                                                  236
      TPRS_NPADS(21)=130                                                  236
      TPRS_NPADS(22)=132                                                  236
      TPRS_NPADS(23)=134                                                  236
      TPRS_NPADS(24)=136                                                  236
      TPRS_NPADS(25)=138                                                  236
      TPRS_NPADS(26)=138                                                  236
      TPRS_NPADS(27)=140                                                  236
      TPRS_NPADS(28)=142                                                  236
      TPRS_NPADS(29)=144                                                  236
      TPRS_NPADS(30)=144                                                  236
      TPRS_NPADS(31)=144                                                  236
      TPRS_NPADS(32)=144                                                  236
      CTPRS_NPADS='// NUMBER OF PADS IN ROW'                              236
C // TPC PADROW RADII                                                     244
      TPRS_RPADS(1)=127.195                                               244
      TPRS_RPADS(2)=129.195                                               244
      TPRS_RPADS(3)=131.195                                               244
      TPRS_RPADS(4)=133.195                                               244
      TPRS_RPADS(5)=135.195                                               244
      TPRS_RPADS(6)=137.195                                               244
      TPRS_RPADS(7)=139.195                                               244
      TPRS_RPADS(8)=141.195                                               244
      TPRS_RPADS(9)=143.195                                               244
      TPRS_RPADS(10)=145.195                                              244
      TPRS_RPADS(11)=147.195                                              244
      TPRS_RPADS(12)=149.195                                              244
      TPRS_RPADS(13)=151.195                                              244
      TPRS_RPADS(14)=153.195                                              244
      TPRS_RPADS(15)=155.195                                              244
      TPRS_RPADS(16)=157.195                                              244
      TPRS_RPADS(17)=159.195                                              244
      TPRS_RPADS(18)=161.195                                              244
      TPRS_RPADS(19)=163.195                                              244
      TPRS_RPADS(20)=165.195                                              244
      TPRS_RPADS(21)=167.195                                              244
      TPRS_RPADS(22)=169.195                                              244
      TPRS_RPADS(23)=171.195                                              244
      TPRS_RPADS(24)=173.195                                              244
      TPRS_RPADS(25)=175.195                                              244
      TPRS_RPADS(26)=177.195                                              244
      TPRS_RPADS(27)=179.195                                              244
      TPRS_RPADS(28)=181.195                                              244
      TPRS_RPADS(29)=183.195                                              244
      TPRS_RPADS(30)=185.195                                              244
      TPRS_RPADS(31)=187.195                                              244
      TPRS_RPADS(32)=189.195                                              244
      CTPRS_RPADS='// TPC PADROW RADII'                                   244
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    245
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           245
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   245
C *                                                                       245
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    245
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           245
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   245
C use default TPRS bank                                                   246
      BPATH_TPRS='TPRS*'                                                  246
      IDNUM_TPRS(1)=0                                                     246
      CALL AGDGETP('TPCXGEO1','*',-999,'TPRS',' ',BANK_TPRS, BPATH_TPRS,  246
     *IDNUM_TPRS,1,MAP_TPRS,NAMES_TPRS,BANK_TPRS,LEN_TPRS,LLEN_TPRS,      246
     *LINK_TPRS,FLAG_TPRS)                                                246
C *                                                                       247
      CALL AGSBEGM(MODULE,IPRIN)                                          247
      IF (IPRIN.LT.0) RETURN                                              247
C Filling bank TECW - // EC TRAPEZOID AND SUPPORT WHEEL                   247
      BANK_TITLE='// EC TRAPEZOID AND SUPPORT WHEEL'                      247
      BPATH_TECW='TECW*'                                                  247
      IDNUM_TECW(1)=0                                                     248
C // SEC => SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                       248
      TECW_SEC=1                                                          248
      CTECW_SEC='// SEC => SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'       248
C // GAPWIDI => AIR IN SUPPORT WHEEL - INNER WIDTH                        249
      TECW_GAPWIDI=2.* 10.91                                              249
      CTECW_GAPWIDI='// GAPWIDI => AIR IN SUPPORT WHEEL - INNER WIDTH'    249
C // GAPWIDO => AIR IN SUPPORT WHEEL - OUTER WIDTH                        250
      TECW_GAPWIDO=2.* 27.56                                              250
      CTECW_GAPWIDO='// GAPWIDO => AIR IN SUPPORT WHEEL - OUTER WIDTH'    250
C // GAPHEIT => AIR IN SUPPORT WHEEL - HEIGHT (DR)                        251
      TECW_GAPHEIT=62.15                                                  251
      CTECW_GAPHEIT='// GAPHEIT => AIR IN SUPPORT WHEEL - HEIGHT (DR)'    251
C // GAPRAD => AIR IN SUPPORT WHEEL - CENTER RADIUS                       252
      TECW_GAPRAD=87.0                                                    252
      CTECW_GAPRAD='// GAPRAD => AIR IN SUPPORT WHEEL - CENTER RADIUS'    252
C //2.* 13.75, !// INWIDTH => SECTOR WIDTH AT INNER RADIUS                253
      TECW_INWIDTH=10.829*INCH                                            253
      CTECW_INWIDTH='//2.* 13.75, !// INWIDTH => SECTOR WIDTH AT INNER R  253
     *ADIUS'                                                              253
C //2.* 32.38, !// OUWIDTH => SECTOR WIDTH AT OUTER RADIUS                254
      TECW_OUWIDTH=25.498*INCH                                            254
      CTECW_OUWIDTH='//2.* 32.38, !// OUWIDTH => SECTOR WIDTH AT OUTER R  254
     *ADIUS'                                                              254
C // NORIBS                                                               255
      TECW_NORIBS=11                                                      255
      CTECW_NORIBS='// NORIBS'                                            255
C // ZSTEPRIB                                                             256
      TECW_ZSTEPRIB=3.150*INCH                                            256
      CTECW_ZSTEPRIB='// ZSTEPRIB'                                        256
C // WIDTHRIB => SIDE RIB WIDTH                                           257
      TECW_WIDTHRIB=1.47*COS(15./180*ACOS(-1.))*INCH                      257
      CTECW_WIDTHRIB='// WIDTHRIB => SIDE RIB WIDTH'                      257
C // DRAWING 24A3685B 27.77*INCH FROM DRAWING 24A0325, !// => 69.527 =>   258
C (WIRE PLANE) 69.                                                        258
      TECW_HEIGHT=27.373*INCH                                             258
      CTECW_HEIGHT='// DRAWING 24A3685B 27.77*INCH FROM DRAWING 24A0325,  258
     * !// => 69.527 => (WIRE PLANE) 69.'                                 258
C // 24A3685B 3.219*INCH !// THICK => SECTOR THICKNESS                    259
      TECW_THICK=3.300*INCH                                               259
      CTECW_THICK='// 24A3685B 3.219*INCH !// THICK => SECTOR             259
     *THICKNESS'                                                          259
C //                                                                      260
      TECW_THICKAL=0.375*INCH                                             260
      CTECW_THICKAL='//'                                                  260
C // RMIN => MINIMUM DISTANCE TO BEAM LINE (WIRE PLANE)                   261
      TECW_RMIN=51.905                                                    261
      CTECW_RMIN='// RMIN => MINIMUM DISTANCE TO BEAM LINE (WIRE          261
     *PLANE)'                                                             261
C // RCENTER => SECTOR CENTER RADIUS (SET BY PRECISION HOLES)             262
      TECW_RCENTER=86.669                                                 262
      CTECW_RCENTER='// RCENTER => SECTOR CENTER RADIUS (SET BY PRECISIO  262
     *N HOLES)'                                                           262
C // = 0.9525 CM                                                          263
      TECW_HOLEDX=0.750/2*INCH                                            263
      CTECW_HOLEDX='// = 0.9525 CM'                                       263
C // = 3.33375 CM                                                         264
      TECW_HOLEDY=2.625/2*INCH                                            264
      CTECW_HOLEDY='// = 3.33375 CM'                                      264
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TECW',    265
     *BPATH_TECW,IDNUM_TECW,1, MAP_TECW,NAMES_TECW,COMMEN_TECW,           265
     *BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)                   265
C *                                                                       265
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TECW',    265
     *BPATH_TECW,IDNUM_TECW,1, MAP_TECW,NAMES_TECW,COMMEN_TECW,           265
     *BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)                   265
C *                                                                       266
      CALL AGSBEGM(MODULE,IPRIN)                                          266
      IF (IPRIN.LT.0) RETURN                                              266
C Filling bank TECW - // EC TRAPEZOID AND SUPPORT WHEEL                   266
      BANK_TITLE='// EC TRAPEZOID AND SUPPORT WHEEL'                      266
      BPATH_TECW='TECW*'                                                  266
      IDNUM_TECW(1)=0                                                     267
C // SEC => SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                       267
      TECW_SEC=2                                                          267
      CTECW_SEC='// SEC => SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'       267
C // GAPWIDI => AIR IN SUPPORT WHEEL - INNER WIDTH                        268
      TECW_GAPWIDI=2.* 28.92                                              268
      CTECW_GAPWIDI='// GAPWIDI => AIR IN SUPPORT WHEEL - INNER WIDTH'    268
C // GAPWIDO => AIR IN SUPPORT WHEEL - OUTER WIDTH                        269
      TECW_GAPWIDO=2.* 46.74                                              269
      CTECW_GAPWIDO='// GAPWIDO => AIR IN SUPPORT WHEEL - OUTER WIDTH'    269
C // GAPHEIT => AIR IN SUPPORT WHEEL - HEIGHT (DR)                        270
      TECW_GAPHEIT=65.0                                                   270
      CTECW_GAPHEIT='// GAPHEIT => AIR IN SUPPORT WHEEL - HEIGHT (DR)'    270
C // AGAPRAD => IR IN SUPPORT WHEEL - RADIUS                              271
      TECW_GAPRAD=158.0                                                   271
      CTECW_GAPRAD='// AGAPRAD => IR IN SUPPORT WHEEL - RADIUS'           271
C //64.92, !// INWIDTH => SECTOR WIDTH AT INNER RADIUS                    272
      TECW_INWIDTH=25.561*INCH                                            272
      CTECW_INWIDTH='//64.92, !// INWIDTH => SECTOR WIDTH AT INNER RADIU  272
     *S'                                                                  272
C //103.25, !// OUWIDTH => SECTOR WIDTH AT OUTER RADIUS                   273
      TECW_OUWIDTH=40.649*INCH                                            273
      CTECW_OUWIDTH='//103.25, !// OUWIDTH => SECTOR WIDTH AT OUTER RADI  273
     *US'                                                                 273
C // = 1.242060 WIDTHLIP => WIDTH OF LIP                                  274
      TECW_WIDTHLIP=((25.561-24.583)+(40.649-39.671))/4*INCH              274
      CTECW_WIDTHLIP='// = 1.242060 WIDTHLIP => WIDTH OF LIP'             274
C // NORIBS                                                               275
      TECW_NORIBS=14                                                      275
      CTECW_NORIBS='// NORIBS'                                            275
C // ZSTEPRIB                                                             276
      TECW_ZSTEPRIB=3.150*INCH                                            276
      CTECW_ZSTEPRIB='// ZSTEPRIB'                                        276
C // WIDTHRIB = > SIDE RIB WIDTH                                          277
      TECW_WIDTHRIB=1.47*COS(15./180*ACOS(-1.))*INCH                      277
      CTECW_WIDTHRIB='// WIDTHRIB = > SIDE RIB WIDTH'                     277
C // 71.514 (WIRE PLANE) => 71.51, !// HEIGHT => SECTOR RADIAL HEIGHT     278
      TECW_HEIGHT=28.155*INCH                                             278
      CTECW_HEIGHT='// 71.514 (WIRE PLANE) => 71.51, !// HEIGHT => SECTO  278
     *R RADIAL HEIGHT'                                                    278
C // 24A3925G THICK => SECTOR THICKNESS                                   279
      TECW_THICK=3.140*INCH                                               279
      CTECW_THICK='// 24A3925G THICK => SECTOR THICKNESS'                 279
C //                                                                      280
      TECW_THICKAL=0.375*INCH                                             280
      CTECW_THICKAL='//'                                                  280
C // RMIN => MINIMUM DISTANCE TO BEAM LINE (WIRE PLANE)                   281
      TECW_RMIN=121.732                                                   281
      CTECW_RMIN='// RMIN => MINIMUM DISTANCE TO BEAM LINE (WIRE          281
     *PLANE)'                                                             281
C // RCENTER => SECTOR CENTER RADIUS (SET BY PRECISION HOLES)             282
      TECW_RCENTER=157.488                                                282
      CTECW_RCENTER='// RCENTER => SECTOR CENTER RADIUS (SET BY PRECISIO  282
     *N HOLES)'                                                           282
C //                                                                      283
      TECW_HOLEDX=0.750/2*INCH                                            283
      CTECW_HOLEDX='//'                                                   283
C //                                                                      284
      TECW_HOLEDY=2.500/2*INCH                                            284
      CTECW_HOLEDY='//'                                                   284
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TECW',    285
     *BPATH_TECW,IDNUM_TECW,1, MAP_TECW,NAMES_TECW,COMMEN_TECW,           285
     *BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)                   285
C *                                                                       285
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TECW',    285
     *BPATH_TECW,IDNUM_TECW,1, MAP_TECW,NAMES_TECW,COMMEN_TECW,           285
     *BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)                   285
C *                                                                       286
      CALL AGSBEGM(MODULE,IPRIN)                                          286
      IF (IPRIN.LT.0) RETURN                                              286
C Filling bank TPCR - // VOLUME FOR TPC READOUT BOARDS                    286
      BANK_TITLE='// VOLUME FOR TPC READOUT BOARDS'                       286
      BPATH_TPCR='TPCR*'                                                  286
      IDNUM_TPCR(1)=0                                                     287
C // LENGTH OF RDO VOLUME                                                 287
      TPCR_RDOVTHK=30.                                                    287
      CTPCR_RDOVTHK='// LENGTH OF RDO VOLUME'                             287
C // THICKNESS OF RDO CARD                                                288
      TPCR_RDOTHK=.25                                                     288
      CTPCR_RDOTHK='// THICKNESS OF RDO CARD'                             288
C // CARD LENGTH ALONG BEAM DIRECTION                                     289
      TPCR_RDOLEN=27                                                      289
      CTPCR_RDOLEN='// CARD LENGTH ALONG BEAM DIRECTION'                  289
C // NUMBER OF RDO BOARDS                                                 290
      TPCR_NRDOBRD=9                                                      290
      CTPCR_NRDOBRD='// NUMBER OF RDO BOARDS'                             290
C // RADIAL POS OF RDO BOARDS                                             292
      TPCR_RDOHT(1)=60.0                                                  292
      TPCR_RDOHT(2)=74.0                                                  292
      TPCR_RDOHT(3)=84.0                                                  292
      TPCR_RDOHT(4)=101.0                                                 292
      TPCR_RDOHT(5)=106.0                                                 292
      TPCR_RDOHT(6)=126.0                                                 292
      TPCR_RDOHT(7)=146.0                                                 292
      TPCR_RDOHT(8)=166.0                                                 292
      TPCR_RDOHT(9)=186.0                                                 292
      CTPCR_RDOHT='// RADIAL POS OF RDO BOARDS'                           292
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCR',    293
     *BPATH_TPCR,IDNUM_TPCR,1, MAP_TPCR,NAMES_TPCR,COMMEN_TPCR,           293
     *BANK_TPCR,LEN_TPCR,LLEN_TPCR,LINK_TPCR,FLAG_TPCR)                   293
C *                                                                       293
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCR',    293
     *BPATH_TPCR,IDNUM_TPCR,1, MAP_TPCR,NAMES_TPCR,COMMEN_TPCR,           293
     *BANK_TPCR,LEN_TPCR,LLEN_TPCR,LINK_TPCR,FLAG_TPCR)                   293
C *                                                                       294
      CALL AGSBEGM(MODULE,IPRIN)                                          294
      IF (IPRIN.LT.0) RETURN                                              294
C Filling bank TFEE - // FRONTENDELECTRONICS                              294
      BANK_TITLE='// FRONTENDELECTRONICS'                                 294
      BPATH_TFEE='TFEE*'                                                  294
      IDNUM_TFEE(1)=0                                                     295
C // VERSION OF FRONTENDELECTRONICS                                       295
      TFEE_VERS=1                                                         295
      CTFEE_VERS='// VERSION OF FRONTENDELECTRONICS'                      295
C // 1.47 SCALE FACTOR ACCOUNT FOR 9 LB OF CABLES                         296
      TFEE_CARDDX=1.47*0.110*INCH/2                                       296
      CTFEE_CARDDX='// 1.47 SCALE FACTOR ACCOUNT FOR 9 LB OF CABLES'      296
C //...                                                                   297
      TFEE_CARDDY=2.900*INCH/2                                            297
      CTFEE_CARDDY='//...'                                                297
C //...                                                                   298
      TFEE_CARDDZ=7.000*INCH/2                                            298
      CTFEE_CARDDZ='//...'                                                298
C //...                                                                   299
      TFEE_PLATEDX=0.110*INCH/2                                           299
      CTFEE_PLATEDX='//...'                                               299
C //...                                                                   300
      TFEE_PLATEDY=1.480*INCH                                             300
      CTFEE_PLATEDY='//...'                                               300
C //...                                                                   301
      TFEE_PLATEDZ=4.650*INCH/2                                           301
      CTFEE_PLATEDZ='//...'                                               301
C //...                                                                   302
      TFEE_ASSEMBLYTHICKNESS=TFEE_CARDDX + TFEE_PLATEDX                   302
      CTFEE_ASSEMBLYTHICKNESS='//...'                                     302
C //...                                                                   303
      TFEE_RIBDX=0.820*INCH/2 - 2* TFEE_CARDDX                            303
      CTFEE_RIBDX='//...'                                                 303
C //...                                                                   304
      TFEE_RIBDY=2.900*INCH/2 - 2* TFEE_CARDDX                            304
      CTFEE_RIBDY='//...'                                                 304
C //...                                                                   305
      TFEE_RIBDZ=TFEE_CARDDX                                              305
      CTFEE_RIBDZ='//...'                                                 305
C // FEE ASSEMBLY BRIK SIZE                                               306
      TFEE_ASS(1)=1.6                                                     306
      TFEE_ASS(2)=4                                                       306
      TFEE_ASS(3)=10                                                      306
      CTFEE_ASS='// FEE ASSEMBLY BRIK SIZE'                               306
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TFEE',    307
     *BPATH_TFEE,IDNUM_TFEE,1, MAP_TFEE,NAMES_TFEE,COMMEN_TFEE,           307
     *BANK_TFEE,LEN_TFEE,LLEN_TFEE,LINK_TFEE,FLAG_TFEE)                   307
C *                                                                       307
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TFEE',    307
     *BPATH_TFEE,IDNUM_TFEE,1, MAP_TFEE,NAMES_TFEE,COMMEN_TFEE,           307
     *BANK_TFEE,LEN_TFEE,LLEN_TFEE,LINK_TFEE,FLAG_TFEE)                   307
C use TECW bank                                                           308
      BPATH_TECW='TECW*'                                                  308
      IDNUM_TECW(1)=0                                                     308
      TECW_SEC=KINNER+1                                                   308
      CALL AGDGETP('TPCXGEO1','*',-999,'TECW',NTECW_SEC,RE_TECW_SEC,      308
     *BPATH_TECW,IDNUM_TECW,1,MAP_TECW,NAMES_TECW,BANK_TECW,LEN_TECW,     308
     *LLEN_TECW,LINK_TECW,FLAG_TECW)                                      308
C *                                                                       309
      CALL AGSBEGM(MODULE,IPRIN)                                          309
      IF (IPRIN.LT.0) RETURN                                              309
C Filling bank RIBS - // RIB STRUCTURE / INNER DRAWING 24A3925G           309
      BANK_TITLE='// RIB STRUCTURE / INNER DRAWING 24A3925G'              309
      BPATH_RIBS='RIBS*'                                                  309
      IDNUM_RIBS(1)=0                                                     310
C // VERSION NUMBER                                                       310
      RIBS_VERSION=KINNER                                                 310
      CRIBS_VERSION='// VERSION NUMBER'                                   310
C // SELCTOR                                                              311
      RIBS_INOUT=KINNER                                                   311
      CRIBS_INOUT='// SELCTOR'                                            311
C // NUMBER OF ELEMENTS                                                   312
      RIBS_NUM=11                                                         312
      CRIBS_NUM='// NUMBER OF ELEMENTS'                                   312
C // X                                                                    315
      RIBS_X(1)=0.000                                                     315
      RIBS_X(2)=4.261                                                     315
      RIBS_X(3)=10.561                                                    315
      RIBS_X(4)=16.861                                                    315
      RIBS_X(5)=23.161                                                    315
      RIBS_X(6)=25.950                                                    315
      RIBS_X(7)=3.726                                                     315
      RIBS_X(8)=3.726                                                     315
      RIBS_X(9)=10.026                                                    315
      RIBS_X(10)=10.026                                                   315
      RIBS_X(11)=25.480                                                   315
      CRIBS_X='// X'                                                      315
C // Y                                                                    318
      RIBS_Y(1)=0.000                                                     318
      RIBS_Y(2)=0.000                                                     318
      RIBS_Y(3)=0.000                                                     318
      RIBS_Y(4)=0.000                                                     318
      RIBS_Y(5)=0.000                                                     318
      RIBS_Y(6)=0.000                                                     318
      RIBS_Y(7)=-5.645                                                    318
      RIBS_Y(8)=5.645                                                     318
      RIBS_Y(9)=-3.895                                                    318
      RIBS_Y(10)=3.895                                                    318
      RIBS_Y(11)=0.000                                                    318
      CRIBS_Y='// Y'                                                      318
C // DX ... LAST=2*(25.950-(1.050+2.515+21.915))                          329
      RIBS_DX(1)=1.470                                                    329
      RIBS_DX(2)=0.375                                                    329
      RIBS_DX(3)=0.375                                                    329
      RIBS_DX(4)=0.375                                                    329
      RIBS_DX(5)=0.375                                                    329
      RIBS_DX(6)=TECW_HEIGHT/INCH-25.950                                  329
      RIBS_DX(7)=1.070                                                    329
      RIBS_DX(8)=1.070                                                    329
      RIBS_DX(9)=1.070                                                    329
      RIBS_DX(10)=1.070                                                   329
      RIBS_DX(11)=0.940                                                   329
      CRIBS_DX='// DX ... LAST=2*(25.950-(1.050+2.515+21.915))'           329
C // DY                                                                   331
      RIBS_DY(1)=0.                                                       331
      RIBS_DY(2)=0.                                                       331
      RIBS_DY(3)=0.                                                       331
      RIBS_DY(4)=0.                                                       331
      RIBS_DY(5)=0.                                                       331
      RIBS_DY(6)=0.                                                       331
      RIBS_DY(7)=0.750                                                    331
      RIBS_DY(8)=0.750                                                    331
      RIBS_DY(9)=0.750                                                    331
      RIBS_DY(10)=0.750                                                   331
      RIBS_DY(11)=2.0                                                     331
      CRIBS_DY='// DY'                                                    331
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RIBS',    332
     *BPATH_RIBS,IDNUM_RIBS,1, MAP_RIBS,NAMES_RIBS,COMMEN_RIBS,           332
     *BANK_RIBS,LEN_RIBS,LLEN_RIBS,LINK_RIBS,FLAG_RIBS)                   332
C *                                                                       332
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RIBS',    332
     *BPATH_RIBS,IDNUM_RIBS,1, MAP_RIBS,NAMES_RIBS,COMMEN_RIBS,           332
     *BANK_RIBS,LEN_RIBS,LLEN_RIBS,LINK_RIBS,FLAG_RIBS)                   332
C use TECW bank                                                           333
      BPATH_TECW='TECW*'                                                  333
      IDNUM_TECW(1)=0                                                     333
      TECW_SEC=KOUTER+1                                                   333
      CALL AGDGETP('TPCXGEO1','*',-999,'TECW',NTECW_SEC,RE_TECW_SEC,      333
     *BPATH_TECW,IDNUM_TECW,1,MAP_TECW,NAMES_TECW,BANK_TECW,LEN_TECW,     333
     *LLEN_TECW,LINK_TECW,FLAG_TECW)                                      333
C *                                                                       334
      CALL AGSBEGM(MODULE,IPRIN)                                          334
      IF (IPRIN.LT.0) RETURN                                              334
C Filling bank RIBS - // RIB STRUCTURE / OUTER DRAWING 24A3925G           334
      BANK_TITLE='// RIB STRUCTURE / OUTER DRAWING 24A3925G'              334
      BPATH_RIBS='RIBS*'                                                  334
      IDNUM_RIBS(1)=0                                                     335
C // VERSION NUMBER                                                       335
      RIBS_VERSION=KOUTER                                                 335
      CRIBS_VERSION='// VERSION NUMBER'                                   335
C // SELCTOR                                                              336
      RIBS_INOUT=KOUTER                                                   336
      CRIBS_INOUT='// SELCTOR'                                            336
C // NUMBER OF ELEMENTS                                                   337
      RIBS_NUM=14                                                         337
      CRIBS_NUM='// NUMBER OF ELEMENTS'                                   337
C // X                                                                    342
      RIBS_X(1)=0.280                                                     342
      RIBS_X(2)=4.261                                                     342
      RIBS_X(3)=7.410                                                     342
      RIBS_X(4)=10.560                                                    342
      RIBS_X(5)=13.710                                                    342
      RIBS_X(6)=16.859                                                    342
      RIBS_X(7)=20.009                                                    342
      RIBS_X(8)=23.158                                                    342
      RIBS_X(9)=26.309                                                    342
      RIBS_X(10)=6.023                                                    342
      RIBS_X(11)=6.023                                                    342
      RIBS_X(12)=12.3225                                                  342
      RIBS_X(13)=15.472                                                   342
      RIBS_X(14)=26.309-1.575/2                                           342
      CRIBS_X='// X'                                                      342
C // Y                                                                    346
      RIBS_Y(1)=0                                                         346
      RIBS_Y(2)=0                                                         346
      RIBS_Y(3)=0                                                         346
      RIBS_Y(4)=0                                                         346
      RIBS_Y(5)=0                                                         346
      RIBS_Y(6)=0                                                         346
      RIBS_Y(7)=0                                                         346
      RIBS_Y(8)=0                                                         346
      RIBS_Y(9)=0                                                         346
      RIBS_Y(10)=17.250/2                                                 346
      RIBS_Y(11)=-17.250/2                                                346
      RIBS_Y(12)=0                                                        346
      RIBS_Y(13)=0                                                        346
      RIBS_Y(14)=0                                                        346
      CRIBS_Y='// Y'                                                      346
C // DX                                                                   349
      RIBS_DX(1)=1.268                                                    349
      RIBS_DX(2)=0.375                                                    349
      RIBS_DX(3)=0.375                                                    349
      RIBS_DX(4)=0.375                                                    349
      RIBS_DX(5)=0.375                                                    349
      RIBS_DX(6)=0.375                                                    349
      RIBS_DX(7)=0.375                                                    349
      RIBS_DX(8)=0.375                                                    349
      RIBS_DX(9)=1.566                                                    349
      RIBS_DX(10)=2.774                                                   349
      RIBS_DX(11)=2.774                                                   349
      RIBS_DX(12)=2.775                                                   349
      RIBS_DX(13)=2.774                                                   349
      RIBS_DX(14)=1.575                                                   349
      CRIBS_DX='// DX'                                                    349
C // DY                                                                   352
      RIBS_DY(1)=0                                                        352
      RIBS_DY(2)=0                                                        352
      RIBS_DY(3)=0                                                        352
      RIBS_DY(4)=0                                                        352
      RIBS_DY(5)=0                                                        352
      RIBS_DY(6)=0                                                        352
      RIBS_DY(7)=0                                                        352
      RIBS_DY(8)=0                                                        352
      RIBS_DY(9)=0                                                        352
      RIBS_DY(10)=1.250                                                   352
      RIBS_DY(11)=1.250                                                   352
      RIBS_DY(12)=0.375                                                   352
      RIBS_DY(13)=0.375                                                   352
      RIBS_DY(14)=1.5680                                                  352
      CRIBS_DY='// DY'                                                    352
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RIBS',    353
     *BPATH_RIBS,IDNUM_RIBS,1, MAP_RIBS,NAMES_RIBS,COMMEN_RIBS,           353
     *BANK_RIBS,LEN_RIBS,LLEN_RIBS,LINK_RIBS,FLAG_RIBS)                   353
C *                                                                       353
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RIBS',    353
     *BPATH_RIBS,IDNUM_RIBS,1, MAP_RIBS,NAMES_RIBS,COMMEN_RIBS,           353
     *BANK_RIBS,LEN_RIBS,LLEN_RIBS,LINK_RIBS,FLAG_RIBS)                   353
C *                                                                       354
      CALL AGSBEGM(MODULE,IPRIN)                                          354
      IF (IPRIN.LT.0) RETURN                                              354
C Filling bank HOLZ - // HOLES PER ROW INNER                              354
      BANK_TITLE='// HOLES PER ROW INNER'                                 354
      BPATH_HOLZ='HOLZ*'                                                  354
      IDNUM_HOLZ(1)=0                                                     355
C // SELECTOR                                                             355
      HOLZ_INOUT=KINNER                                                   355
      CHOLZ_INOUT='// SELECTOR'                                           355
C // NUMBER OF ROWS                                                       356
      HOLZ_NUMBEROFROWS=15                                                356
      CHOLZ_NUMBEROFROWS='// NUMBER OF ROWS'                              356
C // N/ROW                                                                358
      HOLZ_NUMBERPERROW(1)=6                                              358
      HOLZ_NUMBERPERROW(2)=5                                              358
      HOLZ_NUMBERPERROW(3)=5                                              358
      HOLZ_NUMBERPERROW(4)=2                                              358
      HOLZ_NUMBERPERROW(5)=5                                              358
      HOLZ_NUMBERPERROW(6)=4                                              358
      HOLZ_NUMBERPERROW(7)=4                                              358
      HOLZ_NUMBERPERROW(8)=4                                              358
      HOLZ_NUMBERPERROW(9)=4                                              358
      HOLZ_NUMBERPERROW(10)=3                                             358
      HOLZ_NUMBERPERROW(11)=3                                             358
      HOLZ_NUMBERPERROW(12)=3                                             358
      HOLZ_NUMBERPERROW(13)=3                                             358
      HOLZ_NUMBERPERROW(14)=2                                             358
      HOLZ_NUMBERPERROW(15)=2                                             358
      HOLZ_NUMBERPERROW(16)=0                                             358
      CHOLZ_NUMBERPERROW='// N/ROW'                                       358
C // Y HOLES                                                              430
      HOLZ_YHOLES(1)=3.180*(-2.5+0)                                       430
      HOLZ_YHOLES(2)=3.180*(-2.5+1)                                       430
      HOLZ_YHOLES(3)=3.180*(-2.5+2)                                       430
      HOLZ_YHOLES(4)=3.180*(-2.5+3)                                       430
      HOLZ_YHOLES(5)=3.180*(-2.5+4)                                       430
      HOLZ_YHOLES(6)=3.180*(-2.5+5)                                       430
      HOLZ_YHOLES(7)=3.180*(-2.5+6)                                       430
      HOLZ_YHOLES(8)=3.180*(-2.5+7)                                       430
      HOLZ_YHOLES(9)=3.180*(-2.5+8)                                       430
      HOLZ_YHOLES(10)=3.764*(-2.0+0)                                      430
      HOLZ_YHOLES(11)=3.764*(-2.0+1)                                      430
      HOLZ_YHOLES(12)=3.764*(-2.0+2)                                      430
      HOLZ_YHOLES(13)=3.764*(-2.0+3)                                      430
      HOLZ_YHOLES(14)=3.764*(-2.0+4)                                      430
      HOLZ_YHOLES(15)=3.764*(-2.0+5)                                      430
      HOLZ_YHOLES(16)=3.764*(-2.0+6)                                      430
      HOLZ_YHOLES(17)=3.764*(-2.0+7)                                      430
      HOLZ_YHOLES(18)=3.764*(-2.0+8)                                      430
      HOLZ_YHOLES(19)=3.553*(-2.0+0)                                      430
      HOLZ_YHOLES(20)=3.553*(-2.0+1)                                      430
      HOLZ_YHOLES(21)=3.553*(-2.0+2)                                      430
      HOLZ_YHOLES(22)=3.553*(-2.0+3)                                      430
      HOLZ_YHOLES(23)=3.553*(-2.0+4)                                      430
      HOLZ_YHOLES(24)=3.553*(-2.0+5)                                      430
      HOLZ_YHOLES(25)=3.553*(-2.0+6)                                      430
      HOLZ_YHOLES(26)=3.553*(-2.0+7)                                      430
      HOLZ_YHOLES(27)=3.553*(-2.0+8)                                      430
      HOLZ_YHOLES(28)=13.368*(-0.5+0)                                     430
      HOLZ_YHOLES(29)=13.368*(-0.5+1)                                     430
      HOLZ_YHOLES(30)=13.368*(-0.5+2)                                     430
      HOLZ_YHOLES(31)=13.368*(-0.5+3)                                     430
      HOLZ_YHOLES(32)=13.368*(-0.5+4)                                     430
      HOLZ_YHOLES(33)=13.368*(-0.5+5)                                     430
      HOLZ_YHOLES(34)=13.368*(-0.5+6)                                     430
      HOLZ_YHOLES(35)=13.368*(-0.5+7)                                     430
      HOLZ_YHOLES(36)=13.368*(-0.5+8)                                     430
      HOLZ_YHOLES(37)=3.131*(-2.0+0)                                      430
      HOLZ_YHOLES(38)=3.131*(-2.0+1)                                      430
      HOLZ_YHOLES(39)=3.131*(-2.0+2)                                      430
      HOLZ_YHOLES(40)=3.131*(-2.0+3)                                      430
      HOLZ_YHOLES(41)=3.131*(-2.0+4)                                      430
      HOLZ_YHOLES(42)=3.131*(-2.0+5)                                      430
      HOLZ_YHOLES(43)=3.131*(-2.0+6)                                      430
      HOLZ_YHOLES(44)=3.131*(-2.0+7)                                      430
      HOLZ_YHOLES(45)=3.131*(-2.0+8)                                      430
      HOLZ_YHOLES(46)=-3.892 - 1.948                                      430
      HOLZ_YHOLES(47)=-1.948                                              430
      HOLZ_YHOLES(48)=+1.948                                              430
      HOLZ_YHOLES(49)=3.892 + 1.948                                       430
      HOLZ_YHOLES(50)=0.000                                               430
      HOLZ_YHOLES(51)=0.000                                               430
      HOLZ_YHOLES(52)=0.000                                               430
      HOLZ_YHOLES(53)=0.000                                               430
      HOLZ_YHOLES(54)=0.000                                               430
      HOLZ_YHOLES(55)=-3.611 - 1.807                                      430
      HOLZ_YHOLES(56)=-1.807                                              430
      HOLZ_YHOLES(57)=+1.807                                              430
      HOLZ_YHOLES(58)=3.611 + 1.807                                       430
      HOLZ_YHOLES(59)=0.000                                               430
      HOLZ_YHOLES(60)=0.000                                               430
      HOLZ_YHOLES(61)=0.000                                               430
      HOLZ_YHOLES(62)=0.000                                               430
      HOLZ_YHOLES(63)=0.000                                               430
      HOLZ_YHOLES(64)=-3.281 - 1.7145                                     430
      HOLZ_YHOLES(65)=-1.7145                                             430
      HOLZ_YHOLES(66)=+1.7145                                             430
      HOLZ_YHOLES(67)=3.281 + 1.7145                                      430
      HOLZ_YHOLES(68)=0.000                                               430
      HOLZ_YHOLES(69)=0.000                                               430
      HOLZ_YHOLES(70)=0.000                                               430
      HOLZ_YHOLES(71)=0.000                                               430
      HOLZ_YHOLES(72)=0.000                                               430
      HOLZ_YHOLES(73)=3.150*(-1.5+0)                                      430
      HOLZ_YHOLES(74)=3.150*(-1.5+1)                                      430
      HOLZ_YHOLES(75)=3.150*(-1.5+2)                                      430
      HOLZ_YHOLES(76)=3.150*(-1.5+3)                                      430
      HOLZ_YHOLES(77)=3.150*(-1.5+4)                                      430
      HOLZ_YHOLES(78)=3.150*(-1.5+5)                                      430
      HOLZ_YHOLES(79)=3.150*(-1.5+6)                                      430
      HOLZ_YHOLES(80)=3.150*(-1.5+7)                                      430
      HOLZ_YHOLES(81)=3.150*(-1.5+8)                                      430
      HOLZ_YHOLES(82)=4.152*(-1.0+0)                                      430
      HOLZ_YHOLES(83)=4.152*(-1.0+1)                                      430
      HOLZ_YHOLES(84)=4.152*(-1.0+2)                                      430
      HOLZ_YHOLES(85)=4.152*(-1.0+3)                                      430
      HOLZ_YHOLES(86)=4.152*(-1.0+4)                                      430
      HOLZ_YHOLES(87)=4.152*(-1.0+5)                                      430
      HOLZ_YHOLES(88)=4.152*(-1.0+6)                                      430
      HOLZ_YHOLES(89)=4.152*(-1.0+7)                                      430
      HOLZ_YHOLES(90)=4.152*(-1.0+8)                                      430
      HOLZ_YHOLES(91)=3.730*(-1.0+0)                                      430
      HOLZ_YHOLES(92)=3.730*(-1.0+1)                                      430
      HOLZ_YHOLES(93)=3.730*(-1.0+2)                                      430
      HOLZ_YHOLES(94)=3.730*(-1.0+3)                                      430
      HOLZ_YHOLES(95)=3.730*(-1.0+4)                                      430
      HOLZ_YHOLES(96)=3.730*(-1.0+5)                                      430
      HOLZ_YHOLES(97)=3.730*(-1.0+6)                                      430
      HOLZ_YHOLES(98)=3.730*(-1.0+7)                                      430
      HOLZ_YHOLES(99)=3.730*(-1.0+8)                                      430
      HOLZ_YHOLES(100)=3.308*(-1.0+0)                                     430
      HOLZ_YHOLES(101)=3.308*(-1.0+1)                                     430
      HOLZ_YHOLES(102)=3.308*(-1.0+2)                                     430
      HOLZ_YHOLES(103)=3.308*(-1.0+3)                                     430
      HOLZ_YHOLES(104)=3.308*(-1.0+4)                                     430
      HOLZ_YHOLES(105)=3.308*(-1.0+5)                                     430
      HOLZ_YHOLES(106)=3.308*(-1.0+6)                                     430
      HOLZ_YHOLES(107)=3.308*(-1.0+7)                                     430
      HOLZ_YHOLES(108)=3.308*(-1.0+8)                                     430
      HOLZ_YHOLES(109)=3.075*(-1.0+0)                                     430
      HOLZ_YHOLES(110)=3.075*(-1.0+1)                                     430
      HOLZ_YHOLES(111)=3.075*(-1.0+2)                                     430
      HOLZ_YHOLES(112)=3.075*(-1.0+3)                                     430
      HOLZ_YHOLES(113)=3.075*(-1.0+4)                                     430
      HOLZ_YHOLES(114)=3.075*(-1.0+5)                                     430
      HOLZ_YHOLES(115)=3.075*(-1.0+6)                                     430
      HOLZ_YHOLES(116)=3.075*(-1.0+7)                                     430
      HOLZ_YHOLES(117)=3.075*(-1.0+8)                                     430
      HOLZ_YHOLES(118)=4.927*(-0.5+0)                                     430
      HOLZ_YHOLES(119)=4.927*(-0.5+1)                                     430
      HOLZ_YHOLES(120)=4.927*(-0.5+2)                                     430
      HOLZ_YHOLES(121)=4.927*(-0.5+3)                                     430
      HOLZ_YHOLES(122)=4.927*(-0.5+4)                                     430
      HOLZ_YHOLES(123)=4.927*(-0.5+5)                                     430
      HOLZ_YHOLES(124)=4.927*(-0.5+6)                                     430
      HOLZ_YHOLES(125)=4.927*(-0.5+7)                                     430
      HOLZ_YHOLES(126)=4.927*(-0.5+8)                                     430
      HOLZ_YHOLES(127)=4.083*(-0.5+0)                                     430
      HOLZ_YHOLES(128)=4.083*(-0.5+1)                                     430
      HOLZ_YHOLES(129)=4.083*(-0.5+2)                                     430
      HOLZ_YHOLES(130)=4.083*(-0.5+3)                                     430
      HOLZ_YHOLES(131)=4.083*(-0.5+4)                                     430
      HOLZ_YHOLES(132)=4.083*(-0.5+5)                                     430
      HOLZ_YHOLES(133)=4.083*(-0.5+6)                                     430
      HOLZ_YHOLES(134)=4.083*(-0.5+7)                                     430
      HOLZ_YHOLES(135)=4.083*(-0.5+8)                                     430
      HOLZ_YHOLES(136)=0.000                                              430
      HOLZ_YHOLES(137)=0.000                                              430
      HOLZ_YHOLES(138)=0.000                                              430
      HOLZ_YHOLES(139)=0.000                                              430
      HOLZ_YHOLES(140)=0.000                                              430
      HOLZ_YHOLES(141)=0.000                                              430
      HOLZ_YHOLES(142)=0.000                                              430
      HOLZ_YHOLES(143)=0.000                                              430
      HOLZ_YHOLES(144)=0.000                                              430
      CHOLZ_YHOLES='// Y HOLES'                                           430
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'HOLZ',    431
     *BPATH_HOLZ,IDNUM_HOLZ,1, MAP_HOLZ,NAMES_HOLZ,COMMEN_HOLZ,           431
     *BANK_HOLZ,LEN_HOLZ,LLEN_HOLZ,LINK_HOLZ,FLAG_HOLZ)                   431
C *                                                                       431
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'HOLZ',    431
     *BPATH_HOLZ,IDNUM_HOLZ,1, MAP_HOLZ,NAMES_HOLZ,COMMEN_HOLZ,           431
     *BANK_HOLZ,LEN_HOLZ,LLEN_HOLZ,LINK_HOLZ,FLAG_HOLZ)                   431
C *                                                                       432
      CALL AGSBEGM(MODULE,IPRIN)                                          432
      IF (IPRIN.LT.0) RETURN                                              432
C Filling bank HOLZ - // HOLES PER ROW OUTER                              432
      BANK_TITLE='// HOLES PER ROW OUTER'                                 432
      BPATH_HOLZ='HOLZ*'                                                  432
      IDNUM_HOLZ(1)=0                                                     433
C // SELECTOR                                                             433
      HOLZ_INOUT=KOUTER                                                   433
      CHOLZ_INOUT='// SELECTOR'                                           433
C // NUMBER OF ROWS                                                       434
      HOLZ_NUMBEROFROWS=16                                                434
      CHOLZ_NUMBEROFROWS='// NUMBER OF ROWS'                              434
C // N/ROW                                                                436
      HOLZ_NUMBERPERROW(1)=9                                              436
      HOLZ_NUMBERPERROW(2)=9                                              436
      HOLZ_NUMBERPERROW(3)=9                                              436
      HOLZ_NUMBERPERROW(4)=9                                              436
      HOLZ_NUMBERPERROW(5)=9                                              436
      HOLZ_NUMBERPERROW(6)=8                                              436
      HOLZ_NUMBERPERROW(7)=8                                              436
      HOLZ_NUMBERPERROW(8)=8                                              436
      HOLZ_NUMBERPERROW(9)=8                                              436
      HOLZ_NUMBERPERROW(10)=8                                             436
      HOLZ_NUMBERPERROW(11)=7                                             436
      HOLZ_NUMBERPERROW(12)=7                                             436
      HOLZ_NUMBERPERROW(13)=7                                             436
      HOLZ_NUMBERPERROW(14)=7                                             436
      HOLZ_NUMBERPERROW(15)=7                                             436
      HOLZ_NUMBERPERROW(16)=6                                             436
      CHOLZ_NUMBERPERROW='// N/ROW'                                       436
C // Y HOLES                                                              472
      HOLZ_YHOLES(1)=-15.025                                              472
      HOLZ_YHOLES(2)=-11.606                                              472
      HOLZ_YHOLES(3)=-8.177                                               472
      HOLZ_YHOLES(4)=-4.220                                               472
      HOLZ_YHOLES(5)=0                                                    472
      HOLZ_YHOLES(6)=4.220                                                472
      HOLZ_YHOLES(7)=8.177                                                472
      HOLZ_YHOLES(8)=11.606                                               472
      HOLZ_YHOLES(9)=15.025                                               472
      HOLZ_YHOLES(10)=-15.025                                             472
      HOLZ_YHOLES(11)=-11.606                                             472
      HOLZ_YHOLES(12)=-8.177                                              472
      HOLZ_YHOLES(13)=-4.220                                              472
      HOLZ_YHOLES(14)=0                                                   472
      HOLZ_YHOLES(15)=4.220                                               472
      HOLZ_YHOLES(16)=8.177                                               472
      HOLZ_YHOLES(17)=11.606                                              472
      HOLZ_YHOLES(18)=15.025                                              472
      HOLZ_YHOLES(19)=-14.243                                             472
      HOLZ_YHOLES(20)=-11.078                                             472
      HOLZ_YHOLES(21)=-6.330                                              472
      HOLZ_YHOLES(22)=-3.165                                              472
      HOLZ_YHOLES(23)=0                                                   472
      HOLZ_YHOLES(24)=3.165                                               472
      HOLZ_YHOLES(25)=6.330                                               472
      HOLZ_YHOLES(26)=11.078                                              472
      HOLZ_YHOLES(27)=14.243                                              472
      HOLZ_YHOLES(28)=-14.243                                             472
      HOLZ_YHOLES(29)=-11.078                                             472
      HOLZ_YHOLES(30)=-6.330                                              472
      HOLZ_YHOLES(31)=-3.165                                              472
      HOLZ_YHOLES(32)=0                                                   472
      HOLZ_YHOLES(33)=3.165                                               472
      HOLZ_YHOLES(34)=6.330                                               472
      HOLZ_YHOLES(35)=11.078                                              472
      HOLZ_YHOLES(36)=14.243                                              472
      HOLZ_YHOLES(37)=-13.452                                             472
      HOLZ_YHOLES(38)=-10.023                                             472
      HOLZ_YHOLES(39)=-6.330                                              472
      HOLZ_YHOLES(40)=-3.165                                              472
      HOLZ_YHOLES(41)=0                                                   472
      HOLZ_YHOLES(42)=3.165                                               472
      HOLZ_YHOLES(43)=6.330                                               472
      HOLZ_YHOLES(44)=10.023                                              472
      HOLZ_YHOLES(45)=13.452                                              472
      HOLZ_YHOLES(46)=-13.452                                             472
      HOLZ_YHOLES(47)=-10.023                                             472
      HOLZ_YHOLES(48)=-6.330                                              472
      HOLZ_YHOLES(49)=-3.165                                              472
      HOLZ_YHOLES(50)=3.165                                               472
      HOLZ_YHOLES(51)=6.330                                               472
      HOLZ_YHOLES(52)=10.023                                              472
      HOLZ_YHOLES(53)=13.452                                              472
      HOLZ_YHOLES(54)=0                                                   472
      HOLZ_YHOLES(55)=-12.660                                             472
      HOLZ_YHOLES(56)=-9.495                                              472
      HOLZ_YHOLES(57)=-6.330                                              472
      HOLZ_YHOLES(58)=-2.110                                              472
      HOLZ_YHOLES(59)=2.110                                               472
      HOLZ_YHOLES(60)=6.330                                               472
      HOLZ_YHOLES(61)=9.495                                               472
      HOLZ_YHOLES(62)=12.660                                              472
      HOLZ_YHOLES(63)=0                                                   472
      HOLZ_YHOLES(64)=-12.660                                             472
      HOLZ_YHOLES(65)=-9.495                                              472
      HOLZ_YHOLES(66)=-6.330                                              472
      HOLZ_YHOLES(67)=-2.110                                              472
      HOLZ_YHOLES(68)=2.110                                               472
      HOLZ_YHOLES(69)=6.330                                               472
      HOLZ_YHOLES(70)=9.495                                               472
      HOLZ_YHOLES(71)=12.660                                              472
      HOLZ_YHOLES(72)=0                                                   472
      HOLZ_YHOLES(73)=-11.870                                             472
      HOLZ_YHOLES(74)=-8.440                                              472
      HOLZ_YHOLES(75)=-5.275                                              472
      HOLZ_YHOLES(76)=-2.110                                              472
      HOLZ_YHOLES(77)=2.110                                               472
      HOLZ_YHOLES(78)=5.275                                               472
      HOLZ_YHOLES(79)=8.440                                               472
      HOLZ_YHOLES(80)=11.870                                              472
      HOLZ_YHOLES(81)=0                                                   472
      HOLZ_YHOLES(82)=-11.870                                             472
      HOLZ_YHOLES(83)=-8.440                                              472
      HOLZ_YHOLES(84)=-5.275                                              472
      HOLZ_YHOLES(85)=-2.110                                              472
      HOLZ_YHOLES(86)=2.110                                               472
      HOLZ_YHOLES(87)=5.275                                               472
      HOLZ_YHOLES(88)=8.440                                               472
      HOLZ_YHOLES(89)=11.870                                              472
      HOLZ_YHOLES(90)=0                                                   472
      HOLZ_YHOLES(91)=-11.078                                             472
      HOLZ_YHOLES(92)=-7.649                                              472
      HOLZ_YHOLES(93)=-4.220                                              472
      HOLZ_YHOLES(94)=0                                                   472
      HOLZ_YHOLES(95)=4.220                                               472
      HOLZ_YHOLES(96)=7.649                                               472
      HOLZ_YHOLES(97)=11.078                                              472
      HOLZ_YHOLES(98)=0                                                   472
      HOLZ_YHOLES(99)=0                                                   472
      HOLZ_YHOLES(100)=-11.078                                            472
      HOLZ_YHOLES(101)=-7.649                                             472
      HOLZ_YHOLES(102)=-4.220                                             472
      HOLZ_YHOLES(103)=0                                                  472
      HOLZ_YHOLES(104)=4.220                                              472
      HOLZ_YHOLES(105)=7.649                                              472
      HOLZ_YHOLES(106)=11.078                                             472
      HOLZ_YHOLES(107)=0                                                  472
      HOLZ_YHOLES(108)=0                                                  472
      HOLZ_YHOLES(109)=-10.022                                            472
      HOLZ_YHOLES(110)=-6.858                                             472
      HOLZ_YHOLES(111)=-3.429                                             472
      HOLZ_YHOLES(112)=0                                                  472
      HOLZ_YHOLES(113)=3.429                                              472
      HOLZ_YHOLES(114)=6.858                                              472
      HOLZ_YHOLES(115)=10.022                                             472
      HOLZ_YHOLES(116)=0                                                  472
      HOLZ_YHOLES(117)=0                                                  472
      HOLZ_YHOLES(118)=-10.022                                            472
      HOLZ_YHOLES(119)=-6.858                                             472
      HOLZ_YHOLES(120)=-3.429                                             472
      HOLZ_YHOLES(121)=0                                                  472
      HOLZ_YHOLES(122)=3.429                                              472
      HOLZ_YHOLES(123)=6.858                                              472
      HOLZ_YHOLES(124)=10.022                                             472
      HOLZ_YHOLES(125)=0                                                  472
      HOLZ_YHOLES(126)=0                                                  472
      HOLZ_YHOLES(127)=-9.495                                             472
      HOLZ_YHOLES(128)=-6.330                                             472
      HOLZ_YHOLES(129)=-3.165                                             472
      HOLZ_YHOLES(130)=0                                                  472
      HOLZ_YHOLES(131)=3.165                                              472
      HOLZ_YHOLES(132)=6.330                                              472
      HOLZ_YHOLES(133)=9.495                                              472
      HOLZ_YHOLES(134)=0                                                  472
      HOLZ_YHOLES(135)=0                                                  472
      HOLZ_YHOLES(136)=-9.495                                             472
      HOLZ_YHOLES(137)=-6.330                                             472
      HOLZ_YHOLES(138)=-3.165                                             472
      HOLZ_YHOLES(139)=3.165                                              472
      HOLZ_YHOLES(140)=6.330                                              472
      HOLZ_YHOLES(141)=9.495                                              472
      HOLZ_YHOLES(142)=0                                                  472
      HOLZ_YHOLES(143)=0                                                  472
      HOLZ_YHOLES(144)=0                                                  472
      CHOLZ_YHOLES='// Y HOLES'                                           472
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'HOLZ',    473
     *BPATH_HOLZ,IDNUM_HOLZ,1, MAP_HOLZ,NAMES_HOLZ,COMMEN_HOLZ,           473
     *BANK_HOLZ,LEN_HOLZ,LLEN_HOLZ,LINK_HOLZ,FLAG_HOLZ)                   473
C *                                                                       473
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'HOLZ',    473
     *BPATH_HOLZ,IDNUM_HOLZ,1, MAP_HOLZ,NAMES_HOLZ,COMMEN_HOLZ,           473
     *BANK_HOLZ,LEN_HOLZ,LLEN_HOLZ,LINK_HOLZ,FLAG_HOLZ)                   473
C *                                                                       474
      CALL AGSBEGM(MODULE,IPRIN)                                          474
      IF (IPRIN.LT.0) RETURN                                              474
C Filling bank COOL - // COOLING TUBE LENGTHS / INNER                     474
      BANK_TITLE='// COOLING TUBE LENGTHS / INNER'                        474
      BPATH_COOL='COOL*'                                                  474
      IDNUM_COOL(1)=0                                                     475
C // INNER VS OUTER                                                       475
      COOL_INOUT=KINNER                                                   475
      CCOOL_INOUT='// INNER VS OUTER'                                     475
C // TUBE LENGTHS                                                         479
      COOL_TUBELENGTH(1)=19.300                                           479
      COOL_TUBELENGTH(2)=18.454                                           479
      COOL_TUBELENGTH(3)=17.610                                           479
      COOL_TUBELENGTH(4)=16.766                                           479
      COOL_TUBELENGTH(5)=15.922                                           479
      COOL_TUBELENGTH(6)=15.078                                           479
      COOL_TUBELENGTH(7)=14.235                                           479
      COOL_TUBELENGTH(8)=13.390                                           479
      COOL_TUBELENGTH(9)=12.546                                           479
      COOL_TUBELENGTH(10)=11.704                                          479
      COOL_TUBELENGTH(11)=10.860                                          479
      COOL_TUBELENGTH(12)=10.016                                          479
      COOL_TUBELENGTH(13)=9.172                                           479
      COOL_TUBELENGTH(14)=8.327                                           479
      COOL_TUBELENGTH(15)=7.483                                           479
      COOL_TUBELENGTH(16)=0.000                                           479
      CCOOL_TUBELENGTH='// TUBE LENGTHS'                                  479
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'COOL',    480
     *BPATH_COOL,IDNUM_COOL,1, MAP_COOL,NAMES_COOL,COMMEN_COOL,           480
     *BANK_COOL,LEN_COOL,LLEN_COOL,LINK_COOL,FLAG_COOL)                   480
C *                                                                       480
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'COOL',    480
     *BPATH_COOL,IDNUM_COOL,1, MAP_COOL,NAMES_COOL,COMMEN_COOL,           480
     *BANK_COOL,LEN_COOL,LLEN_COOL,LINK_COOL,FLAG_COOL)                   480
C *                                                                       481
      CALL AGSBEGM(MODULE,IPRIN)                                          481
      IF (IPRIN.LT.0) RETURN                                              481
C Filling bank COOL - // COOLING TUBE LENGTHS / OUTER                     481
      BANK_TITLE='// COOLING TUBE LENGTHS / OUTER'                        481
      BPATH_COOL='COOL*'                                                  481
      IDNUM_COOL(1)=0                                                     482
C // INNER VS OUTER                                                       482
      COOL_INOUT=KOUTER                                                   482
      CCOOL_INOUT='// INNER VS OUTER'                                     482
C // TUBE LENGTHS                                                         486
      COOL_TUBELENGTH(1)=33.620                                           486
      COOL_TUBELENGTH(2)=33.605                                           486
      COOL_TUBELENGTH(3)=32.761                                           486
      COOL_TUBELENGTH(4)=31.917                                           486
      COOL_TUBELENGTH(5)=31.073                                           486
      COOL_TUBELENGTH(6)=30.229                                           486
      COOL_TUBELENGTH(7)=29.385                                           486
      COOL_TUBELENGTH(8)=28.541                                           486
      COOL_TUBELENGTH(9)=27.697                                           486
      COOL_TUBELENGTH(10)=26.853                                          486
      COOL_TUBELENGTH(11)=26.009                                          486
      COOL_TUBELENGTH(12)=25.165                                          486
      COOL_TUBELENGTH(13)=24.321                                          486
      COOL_TUBELENGTH(14)=23.478                                          486
      COOL_TUBELENGTH(15)=22.634                                          486
      COOL_TUBELENGTH(16)=21.790                                          486
      CCOOL_TUBELENGTH='// TUBE LENGTHS'                                  486
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'COOL',    487
     *BPATH_COOL,IDNUM_COOL,1, MAP_COOL,NAMES_COOL,COMMEN_COOL,           487
     *BANK_COOL,LEN_COOL,LLEN_COOL,LINK_COOL,FLAG_COOL)                   487
C *                                                                       487
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'COOL',    487
     *BPATH_COOL,IDNUM_COOL,1, MAP_COOL,NAMES_COOL,COMMEN_COOL,           487
     *BANK_COOL,LEN_COOL,LLEN_COOL,LINK_COOL,FLAG_COOL)                   487
C use default TFEE bank                                                   488
      BPATH_TFEE='TFEE*'                                                  488
      IDNUM_TFEE(1)=0                                                     488
      CALL AGDGETP('TPCXGEO1','*',-999,'TFEE',' ',BANK_TFEE, BPATH_TFEE,  488
     *IDNUM_TFEE,1,MAP_TFEE,NAMES_TFEE,BANK_TFEE,LEN_TFEE,LLEN_TFEE,      488
     *LINK_TFEE,FLAG_TFEE)                                                488
C use default TECW bank                                                   489
      BPATH_TECW='TECW*'                                                  489
      IDNUM_TECW(1)=0                                                     489
      CALL AGDGETP('TPCXGEO1','*',-999,'TECW',' ',BANK_TECW, BPATH_TECW,  489
     *IDNUM_TECW,1,MAP_TECW,NAMES_TECW,BANK_TECW,LEN_TECW,LLEN_TECW,      489
     *LINK_TECW,FLAG_TECW)                                                489
C use default TPCR bank                                                   490
      BPATH_TPCR='TPCR*'                                                  490
      IDNUM_TPCR(1)=0                                                     490
      CALL AGDGETP('TPCXGEO1','*',-999,'TPCR',' ',BANK_TPCR, BPATH_TPCR,  490
     *IDNUM_TPCR,1,MAP_TPCR,NAMES_TPCR,BANK_TPCR,LEN_TPCR,LLEN_TPCR,      490
     *LINK_TPCR,FLAG_TPCR)                                                490
C use default TPCG bank                                                   491
      BPATH_TPCG='TPCG*'                                                  491
      IDNUM_TPCG(1)=0                                                     491
      CALL AGDGETP('TPCXGEO1','*',-999,'TPCG',' ',BANK_TPCG, BPATH_TPCG,  491
     *IDNUM_TPCG,1,MAP_TPCG,NAMES_TPCG,BANK_TPCG,LEN_TPCG,LLEN_TPCG,      491
     *LINK_TPCG,FLAG_TPCG)                                                491
C GAS PLUS ENDCAPS                                                        492
      TOFCLENG = TPCG_LENGTH-2*TPCG_WHEELTHK-2*TPCR_RDOVTHK               492
C ACTIVE GAS                                                              493
      TPGVLENG = (TOFCLENG-TPCG_MEMBTHK)/2                                493
C ACTIVE GAS                                                              494
      TPGVLENG = (TPCG_LENGTHV-TPCG_MEMBTHK)/2                            494
      TOCSIR = TPCG_SENGASOR                                              495
      TOCSOR = TOCSIR + TPCG_TOCSDR                                       496
      TOKAIR = TOCSOR                                                     496
      TOKAOR = TOKAIR + TPCG_TOKADR                                       497
      TONXIR = TOKAOR                                                     497
      TONXOR = TONXIR + TPCG_TONXDR                                       498
      TOADIR = TONXOR                                                     498
      TOADOR = TOADIR + TPCG_TOADDR                                       499
      TOIGIR = TOADOR                                                     499
      TOIGOR = TOIGIR + TPCG_TOIGDR                                       500
      TOALIR = TOIGOR                                                     500
      TOALOR = TOALIR + TPCG_TOALDR/2.                                    501
      TOHAIR = TOALOR                                                     501
      TOHAOR = TOHAIR + TPCG_TOHADR                                       502
      TOALIR2= TOHAOR                                                     502
      TOALOR2= TOALIR2+ TPCG_TOALDR/2.                                    503
      TOFCIR = TOCSIR                                                     503
      TOFCOR = TOALOR2                                                    504
      TOFSIR = TOCSIR                                                     504
      TOFSOR = TOADOR                                                     505
      TIFCIR = TPCG_RMINIFC                                               506
      TIALIR = TIFCIR                                                     506
      TIALOR = TIALIR + TPCG_TIALDR/2.                                    507
      TIKAIR = TIALOR                                                     507
      TIKAOR = TIKAIR + TPCG_TIKADR                                       508
      TINXIR = TIKAOR                                                     508
      TINXOR = TINXIR + TPCG_TINXDR                                       509
      TIADIR = TINXOR                                                     509
      TIADOR = TIADIR + TPCG_TIADDR                                       510
      TIFCOR = TIADOR + TPCG_TIALDR/2.                                    511
      TPGVIR = TIFCOR                                                     511
C // TPC GAS INNER RADIUS                                                 512
      DEL = TPCG_RMAX-TOFCOR                                              513
      WRITE(LOUT,5010)TPCG_VERSION                                        513
5010  FORMAT(' TPCE: config=',F9.3)                                       515
      WIDTH1IOF(1)=7.661*INCH                                             516
      WIDTH1IOF(2)=22.287*INCH                                            516
      WIDTH2IOF(1)=20.552*INCH                                            517
      WIDTH2IOF(2)=35.422*INCH                                            517
      HEIGHTIOF(1)=24.222*INCH                                            518
      HEIGHTIOF(2)=24.840*INCH                                            518
      DEPTHIOF(1)=1.800*INCH                                              519
      DEPTHIOF(2)=1.800*INCH                                              519
C Geant COMPONENT : C ===> A=12 Z=6 W=5 <===                              520
      CALL AGSRESET                                                       520
      AG_A = 12                                                           520
      AG_Z = 6                                                            520
      AG_W = 5                                                            520
      AG_TITLE = 'C'                                                      520
      AG_EXNAME = 'COMPONENT'                                             520
      AG_PARLIST = 'A_Z_W'                                                520
      CALL AXCOMPONENT                                                    520
C *                                                                       520
C Geant COMPONENT : H ===> A=1 Z=1 W=8 <===                               520
      CALL AGSRESET                                                       521
      AG_A = 1                                                            521
      AG_Z = 1                                                            521
      AG_W = 8                                                            521
      AG_TITLE = 'H'                                                      521
      AG_EXNAME = 'COMPONENT'                                             521
      AG_PARLIST = 'A_Z_W'                                                521
      CALL AXCOMPONENT                                                    521
C *                                                                       521
C Geant COMPONENT : O ===> A=16 Z=8 W=2 <===                              521
      CALL AGSRESET                                                       522
      AG_A = 16                                                           522
      AG_Z = 8                                                            522
      AG_W = 2                                                            522
      AG_TITLE = 'O'                                                      522
      AG_EXNAME = 'COMPONENT'                                             522
      AG_PARLIST = 'A_Z_W'                                                522
      CALL AXCOMPONENT                                                    522
C *                                                                       522
C Geant MIXTURE : NOMEX ===> DENS=0.064 <===                              522
      CALL AGSRESET                                                       523
      AG_DENS = 0.064                                                     523
      AG_TITLE = 'NOMEX'                                                  523
      AG_EXNAME = 'MIXTURE'                                               523
      AG_PARLIST = 'DENS'                                                 523
      CALL AXMIXTURE                                                      523
C *                                                                       523
C Geant COMPONENT : C ===> A=12 Z=6 W=0.625 <===                          523
      CALL AGSRESET                                                       524
      AG_A = 12                                                           524
      AG_Z = 6                                                            524
      AG_W = 0.625                                                        524
      AG_TITLE = 'C'                                                      524
      AG_EXNAME = 'COMPONENT'                                             524
      AG_PARLIST = 'A_Z_W'                                                524
      CALL AXCOMPONENT                                                    524
C *                                                                       524
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4166667E-01 <===                   524
      CALL AGSRESET                                                       525
      AG_A = 1                                                            525
      AG_Z = 1                                                            525
      AG_W = 0.4166667E-01                                                525
      AG_TITLE = 'H'                                                      525
      AG_EXNAME = 'COMPONENT'                                             525
      AG_PARLIST = 'A_Z_W'                                                525
      CALL AXCOMPONENT                                                    525
C *                                                                       525
C Geant COMPONENT : O ===> A=16 Z=8 W=0.3333333 <===                      525
      CALL AGSRESET                                                       526
      AG_A = 16                                                           526
      AG_Z = 8                                                            526
      AG_W = 0.3333333                                                    526
      AG_TITLE = 'O'                                                      526
      AG_EXNAME = 'COMPONENT'                                             526
      AG_PARLIST = 'A_Z_W'                                                526
      CALL AXCOMPONENT                                                    526
C *                                                                       526
C Geant MIXTURE : ADHESIVE ===> DENS=1.2 <===                             526
      CALL AGSRESET                                                       527
      AG_DENS = 1.2                                                       527
      AG_TITLE = 'ADHESIVE'                                               527
      AG_EXNAME = 'MIXTURE'                                               527
      AG_PARLIST = 'DENS'                                                 527
      CALL AXMIXTURE                                                      527
C *                                                                       527
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.0105 <===                       527
      CALL AGSRESET                                                       528
      AG_A = 27                                                           528
      AG_Z = 13                                                           528
      AG_W = 0.0105                                                       528
      AG_TITLE = 'AL'                                                     528
      AG_EXNAME = 'COMPONENT'                                             528
      AG_PARLIST = 'A_Z_W'                                                528
      CALL AXCOMPONENT                                                    528
C *                                                                       528
C Geant COMPONENT : N ===> A=14 Z=7 W=0.7395 <===                         528
      CALL AGSRESET                                                       529
      AG_A = 14                                                           529
      AG_Z = 7                                                            529
      AG_W = 0.7395                                                       529
      AG_TITLE = 'N'                                                      529
      AG_EXNAME = 'COMPONENT'                                             529
      AG_PARLIST = 'A_Z_W'                                                529
      CALL AXCOMPONENT                                                    529
C *                                                                       529
C Geant COMPONENT : ADHESIVE ===> A=9 Z=4.5 W=0.2500 <===                 529
      CALL AGSRESET                                                       530
      AG_A = 9                                                            530
      AG_Z = 4.5                                                          530
      AG_W = 0.2500                                                       530
      AG_TITLE = 'ADHESIVE'                                               530
      AG_EXNAME = 'COMPONENT'                                             530
      AG_PARLIST = 'A_Z_W'                                                530
      CALL AXCOMPONENT                                                    530
C *                                                                       530
C Geant MIXTURE : AL_HONEYCOMB ===> DENS=0.282 <===                       530
      CALL AGSRESET                                                       531
      AG_DENS = 0.282                                                     531
      AG_TITLE = 'AL_HONEYCOMB'                                           531
      AG_EXNAME = 'MIXTURE'                                               531
      AG_PARLIST = 'DENS'                                                 531
      CALL AXMIXTURE                                                      531
C *                                                                       531
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             531
      CALL AGSRESET                                                       532
      AG_A = 28.08                                                        532
      AG_Z = 14                                                           532
      AG_W = 0.6*1*28./60.                                                532
      AG_TITLE = 'SI'                                                     532
      AG_EXNAME = 'COMPONENT'                                             532
      AG_PARLIST = 'A_Z_W'                                                532
      CALL AXCOMPONENT                                                    532
C *                                                                       532
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  532
C =                                                                       532
      CALL AGSRESET                                                       533
      AG_A = 16                                                           533
      AG_Z = 8                                                            533
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               533
      AG_TITLE = 'O'                                                      533
      AG_EXNAME = 'COMPONENT'                                             533
      AG_PARLIST = 'A_Z_W'                                                533
      CALL AXCOMPONENT                                                    533
C *                                                                       533
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 533
      CALL AGSRESET                                                       534
      AG_A = 12                                                           534
      AG_Z = 6                                                            534
      AG_W = 0.4*8*12./174.                                               534
      AG_TITLE = 'C'                                                      534
      AG_EXNAME = 'COMPONENT'                                             534
      AG_PARLIST = 'A_Z_W'                                                534
      CALL AXCOMPONENT                                                    534
C *                                                                       534
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  534
      CALL AGSRESET                                                       535
      AG_A = 1                                                            535
      AG_Z = 1                                                            535
      AG_W = 0.4*14*1./174.                                               535
      AG_TITLE = 'H'                                                      535
      AG_EXNAME = 'COMPONENT'                                             535
      AG_PARLIST = 'A_Z_W'                                                535
      CALL AXCOMPONENT                                                    535
C *                                                                       535
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  535
      CALL AGSRESET                                                       536
      AG_DENS = 1.7                                                       536
      AG_TITLE = 'G10'                                                    536
      AG_EXNAME = 'MIXTURE'                                               536
      AG_PARLIST = 'DENS'                                                 536
      CALL AXMIXTURE                                                      536
C *                                                                       536
C Geant COMPONENT : H ===> A=1 Z=1 W=1.06546299984001044E-02 <===         536
      CALL AGSRESET                                                       537
      AG_A = 1                                                            537
      AG_Z = 1                                                            537
      AG_W = 1.06546299984001044E-02                                      537
      AG_TITLE = 'H'                                                      537
      AG_EXNAME = 'COMPONENT'                                             537
      AG_PARLIST = 'A_Z_W'                                                537
      CALL AXCOMPONENT                                                    537
C *                                                                       537
C Geant COMPONENT : O ===> A=16 Z=8 W=8.52370399872008355E-02 <===        537
      CALL AGSRESET                                                       538
      AG_A = 16                                                           538
      AG_Z = 8                                                            538
      AG_W = 8.52370399872008355E-02                                      538
      AG_TITLE = 'O'                                                      538
      AG_EXNAME = 'COMPONENT'                                             538
      AG_PARLIST = 'A_Z_W'                                                538
      CALL AXCOMPONENT                                                    538
C *                                                                       538
C Geant COMPONENT : AL ===> A=27 Z=14 W=9.04108330014399053E-01 <===      538
      CALL AGSRESET                                                       539
      AG_A = 27                                                           539
      AG_Z = 14                                                           539
      AG_W = 9.04108330014399053E-01                                      539
      AG_TITLE = 'AL'                                                     539
      AG_EXNAME = 'COMPONENT'                                             539
      AG_PARLIST = 'A_Z_W'                                                539
      CALL AXCOMPONENT                                                    539
C *                                                                       539
C Geant MIXTURE : WATER_PIPE ===> DENS=2.32155 <===                       539
      CALL AGSRESET                                                       540
      AG_DENS = 2.32155                                                   540
      AG_TITLE = 'WATER_PIPE'                                             540
      AG_EXNAME = 'MIXTURE'                                               540
      AG_PARLIST = 'DENS'                                                 540
      CALL AXMIXTURE                                                      540
C *                                                                       540
C Geant COMPONENT : CU ===> A=63.54 Z=29 W=0.586 <===                     540
      CALL AGSRESET                                                       541
      AG_A = 63.54                                                        541
      AG_Z = 29                                                           541
      AG_W = 0.586                                                        541
      AG_TITLE = 'CU'                                                     541
      AG_EXNAME = 'COMPONENT'                                             541
      AG_PARLIST = 'A_Z_W'                                                541
      CALL AXCOMPONENT                                                    541
C *                                                                       541
C Geant COMPONENT : C ===> A=12.01 Z=6 W=0.259 <===                       541
      CALL AGSRESET                                                       542
      AG_A = 12.01                                                        542
      AG_Z = 6                                                            542
      AG_W = 0.259                                                        542
      AG_TITLE = 'C'                                                      542
      AG_EXNAME = 'COMPONENT'                                             542
      AG_PARLIST = 'A_Z_W'                                                542
      CALL AXCOMPONENT                                                    542
C *                                                                       542
C Geant COMPONENT : O ===> A=15.999 Z=8 W=0.138 <===                      542
      CALL AGSRESET                                                       543
      AG_A = 15.999                                                       543
      AG_Z = 8                                                            543
      AG_W = 0.138                                                        543
      AG_TITLE = 'O'                                                      543
      AG_EXNAME = 'COMPONENT'                                             543
      AG_PARLIST = 'A_Z_W'                                                543
      CALL AXCOMPONENT                                                    543
C *                                                                       543
C Geant COMPONENT : H ===> A=1.00794 Z=1 W=0.017 <===                     543
      CALL AGSRESET                                                       544
      AG_A = 1.00794                                                      544
      AG_Z = 1                                                            544
      AG_W = 0.017                                                        544
      AG_TITLE = 'H'                                                      544
      AG_EXNAME = 'COMPONENT'                                             544
      AG_PARLIST = 'A_Z_W'                                                544
      CALL AXCOMPONENT                                                    544
C *                                                                       544
C Geant MIXTURE : CABLES ===> DENS=2.68 <===                              544
      CALL AGSRESET                                                       545
      AG_DENS = 2.68                                                      545
      AG_TITLE = 'CABLES'                                                 545
      AG_EXNAME = 'MIXTURE'                                               545
      AG_PARLIST = 'DENS'                                                 545
      CALL AXMIXTURE                                                      545
C *                                                                       545
C Geant COMPONENT : AR ===> A=40 Z=18 W=9 <===                            545
      CALL AGSRESET                                                       546
      AG_A = 40                                                           546
      AG_Z = 18                                                           546
      AG_W = 9                                                            546
      AG_TITLE = 'AR'                                                     546
      AG_EXNAME = 'COMPONENT'                                             546
      AG_PARLIST = 'A_Z_W'                                                546
      CALL AXCOMPONENT                                                    546
C *                                                                       546
C Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                              546
      CALL AGSRESET                                                       547
      AG_A = 12                                                           547
      AG_Z = 6                                                            547
      AG_W = 1                                                            547
      AG_TITLE = 'C'                                                      547
      AG_EXNAME = 'COMPONENT'                                             547
      AG_PARLIST = 'A_Z_W'                                                547
      CALL AXCOMPONENT                                                    547
C *                                                                       547
C Geant COMPONENT : H ===> A=1 Z=1 W=4 <===                               547
      CALL AGSRESET                                                       548
      AG_A = 1                                                            548
      AG_Z = 1                                                            548
      AG_W = 4                                                            548
      AG_TITLE = 'H'                                                      548
      AG_EXNAME = 'COMPONENT'                                             548
      AG_PARLIST = 'A_Z_W'                                                548
      CALL AXCOMPONENT                                                    548
C *                                                                       548
C Geant MIXTURE : P10 ===> DENS=0.154053E-02 <===                         548
      CALL AGSRESET                                                       549
      AG_DENS = 0.154053E-02                                              549
      AG_TITLE = 'P10'                                                    549
      AG_EXNAME = 'MIXTURE'                                               549
      AG_PARLIST = 'DENS'                                                 549
      CALL AXMIXTURE                                                      549
C *                                                                       549
      CALL AGSBEGM(MODULE,IPRIN)                                          549
C Check IPRIN<0                                                           550
      IF (IPRIN.LT.0) RETURN                                              550
C Create TPCE :                                                           550
C Call To Local Procedure TPCE                                            550
      ASSIGN 5030 TO BLOCK_TPCE                                           550
      GO TO  5020                                                         550
5030  CONTINUE                                                            550
C *                                                                       550
C Geant POSITION : TPCE ===> MOTHER='CAVE' <===                           551
      CALL AGSRESET                                                       551
      AG_MOTHER = 'CAVE'                                                  551
      AG_TITLE = 'TPCE'                                                   551
      AG_EXNAME = 'POSITION'                                              551
      AG_PARLIST = 'MOTHER'                                               551
      CALL AXPOSITION                                                     551
C *                                                                       551
C ----------------------------------------------------------------------  551
C ------------ TPCE                                                       551
      CALL AGSENDM                                                        553
      RETURN                                                              553
C Begin Local Procedure TPCE                                              553
C TPCE IS THE TPC ENVELOPE                                                553
5020  CONTINUE                                                            553
         AG_TITLE='TPCE'                                                  553
         AG_MODULE=MODULE                                                 553
         AG_MARK='V00'                                                    553
         CALL AGSPUSH                                                     553
         AG_MODULE=MODULE                                                 553
C    *                                                                    553
C    Geant MATERIAL : AIR ===> inherited arguments used <===              554
         CALL AGSRESET                                                    554
         AG_TITLE = 'AIR'                                                 554
         AG_EXNAME = 'MATERIAL'                                           554
         AG_PARLIST = 'NONE'                                              554
         CALL AXMATERIAL                                                  554
C    *                                                                    554
C    Geant MEDIUM : STANDARD ===> IFIELD=1 FIELDM=20 TMAXFD=20 EPSIL=0.0  554
C    1 STEMAX=10 DEEMAX=0.248853, STMIN=1.15055 ISVOL=0 <===              554
         CALL AGSRESET                                                    556
         AG_IFIELD = 1                                                    556
         AG_FIELDM = 20                                                   556
         AG_TMAXFD = 20                                                   556
         AG_EPSIL = 0.01                                                  556
         AG_STEMAX = 10                                                   556
         AG_DEEMAX = 0.248853                                             556
         AG_STMIN = 1.15055                                               556
         AG_ISVOL = 0                                                     556
         AG_TITLE = 'STANDARD'                                            556
         AG_EXNAME = 'MEDIUM'                                             556
         AG_PARLIST = 'IFIELD_FIELDM_TMAXFD_EPSIL_STEMAX_DEEMAX_STMIN_IS  556
     *VOL'                                                                556
         CALL AXMEDIUM                                                    556
C    *                                                                    556
C    Geant ATTRIBUTE : TPCE ===> SEEN=0 COLO=KRED <===                    556
         CALL AGSRESET                                                    557
         AG_SEEN = 0                                                      557
         AG_COLO = KRED                                                   557
         AG_TITLE = 'TPCE'                                                557
         AG_EXNAME = 'ATTRIBUTE'                                          557
         AG_PARLIST = 'SEEN_COLO'                                         557
         CALL AXATTRIBUTE                                                 557
C    *                                                                    557
C    Geant SHAPE : TUBE ===> RMIN=TPCG_RMIN RMAX=TPCG_RMAX, DZ=TPCG_DZEN  557
C    VELOP <===                                                           557
         CALL AGSRESET                                                    559
         AG_RMIN = TPCG_RMIN                                              559
         AG_RMAX = TPCG_RMAX                                              559
         AG_DZ = TPCG_DZENVELOP                                           559
         AG_TITLE = 'TUBE'                                                559
         AG_EXNAME = 'SHAPE'                                              559
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      559
         CALL AXSHAPE                                                     559
C    *                                                                    559
C    Check IPRIN>=1                                                       559
         IF (IPRIN.GE.1) WRITE(LOUT,5040)AG_IVOLUME,AG_VOLUME             559
5040     FORMAT('Creating volume ',I4,2X,A4)                              561
         TPGVZ =(TPCG_MEMBTHK + TPGVLENG)/2                               562
         ZWHEEL1 = TPCG_LENGTHW/2 - TPCG_WHEELTHK                         562
         ZTOFCEND = TPCG_LENGTHW/2 - TPCG_WHEELTHK1                       563
         ZTIFCEND = TPCG_LENGTHW/2 + 0.97                                 564
         ZTIFCFLANGEBEGIN = ZTIFCEND - 1*INCH                             565
C    Create TPGV :                                                        567
C    Call To Local Procedure TPGV                                         567
         ASSIGN 5060 TO BLOCK_TPGV                                        567
         GO TO  5050                                                      567
5060     CONTINUE                                                         567
C    *                                                                    567
C    Geant POSITION : TPGV ===> Z=+TPGVZ THETAX=90 PHIX=75 THETAY=90 PHI  569
C    Y=-15 THETAZ=0 PHIZ=0 KONLY='MANY' <===                              569
         CALL AGSRESET                                                    569
         AG_Z = +TPGVZ                                                    569
         AG_THETAX = 90                                                   569
         AG_PHIX = 75                                                     569
         AG_THETAY = 90                                                   569
         AG_PHIY = -15                                                    569
         AG_THETAZ = 0                                                    569
         AG_PHIZ = 0                                                      569
         AG_KONLY = 'MANY'                                                569
         AG_TITLE = 'TPGV'                                                569
         AG_EXNAME = 'POSITION'                                           569
         AG_PARLIST = 'Z_THETAX_PHIX_THETAY_PHIY_THETAZ_PHIZ_KONLY'       569
         CALL AXPOSITION                                                  569
C    *                                                                    569
C    Geant POSITION : TPGV ===> Z=-TPGVZ THETAX=90 PHIX=105 THETAY=90 PH  569
C    IY=195 THETAZ=180 PHIZ=0 KONLY='MANY' <===                           569
         CALL AGSRESET                                                    571
         AG_Z = -TPGVZ                                                    571
         AG_THETAX = 90                                                   571
         AG_PHIX = 105                                                    571
         AG_THETAY = 90                                                   571
         AG_PHIY = 195                                                    571
         AG_THETAZ = 180                                                  571
         AG_PHIZ = 0                                                      571
         AG_KONLY = 'MANY'                                                571
         AG_TITLE = 'TPGV'                                                571
         AG_EXNAME = 'POSITION'                                           571
         AG_PARLIST = 'Z_THETAX_PHIX_THETAY_PHIY_THETAZ_PHIZ_KONLY'       571
         CALL AXPOSITION                                                  571
C    *                                                                    571
C    Create TPCM :                                                        572
C    Call To Local Procedure TPCM                                         572
         ASSIGN 5080 TO BLOCK_TPCM                                        572
         GO TO  5070                                                      572
5080     CONTINUE                                                         572
C    *                                                                    572
C    Geant POSITION : TPCM ===> inherited arguments used <===             573
         CALL AGSRESET                                                    573
         AG_TITLE = 'TPCM'                                                573
         AG_EXNAME = 'POSITION'                                           573
         AG_PARLIST = 'NONE'                                              573
         CALL AXPOSITION                                                  573
C    *                                                                    573
C    Create TIFC :                                                        574
C    Call To Local Procedure TIFC                                         574
         ASSIGN 5100 TO BLOCK_TIFC                                        574
         GO TO  5090                                                      574
5100     CONTINUE                                                         574
C    *                                                                    574
C    Geant POSITION : TIFC ===> inherited arguments used <===             575
         CALL AGSRESET                                                    575
         AG_TITLE = 'TIFC'                                                575
         AG_EXNAME = 'POSITION'                                           575
         AG_PARLIST = 'NONE'                                              575
         CALL AXPOSITION                                                  575
C    *                                                                    575
C    Create TOFC :                                                        576
C    Call To Local Procedure TOFC                                         576
         ASSIGN 5120 TO BLOCK_TOFC                                        576
         GO TO  5110                                                      576
5120     CONTINUE                                                         576
C    *                                                                    576
C    Geant POSITION : TOFC ===> inherited arguments used <===             577
         CALL AGSRESET                                                    577
         AG_TITLE = 'TOFC'                                                577
         AG_EXNAME = 'POSITION'                                           577
         AG_PARLIST = 'NONE'                                              577
         CALL AXPOSITION                                                  577
C    *                                                                    577
C    *                                                                    577
         ISECANG=0                                                        578
            GOTO 5133                                                     578
5131        ISECANG=ISECANG+(30)                                          578
5133        IF((30)*(ISECANG-(360-30)).GT.0) GO TO 5132                   578
C       Create TSWH :                                                     579
C       Call To Local Procedure TSWH                                      579
            ASSIGN 5150 TO BLOCK_TSWH                                     579
            GO TO  5140                                                   579
5150        CONTINUE                                                      579
C       *                                                                 579
C       Geant POSITION : TSWH ===> ALPHAZ=ISECANG KONLY='MANY' <===       580
            CALL AGSRESET                                                 580
            AG_ALPHAN=AG_ALPHAN+1                                         580
            AG_ALPHAK(AG_ALPHAN)=3                                        580
            AG_ALPHAL(AG_ALPHAN)=ISECANG                                  580
            AG_KONLY = 'MANY'                                             580
            AG_TITLE = 'TSWH'                                             580
            AG_EXNAME = 'POSITION'                                        580
            AG_PARLIST = 'ALPHAZ_KONLY'                                   580
            CALL AXPOSITION                                               580
C       *                                                                 580
C       Create TSWH :                                                     581
C       Call To Local Procedure TSWH                                      581
            ASSIGN 5160 TO BLOCK_TSWH                                     581
            GO TO  5140                                                   581
5160        CONTINUE                                                      581
C       *                                                                 581
C       Geant POSITION : TSWH ===> ORT=XY-Z ALPHAZ=ISECANG KONLY='MANY'   582
C       <===                                                              582
            CALL AGSRESET                                                 582
            AG_ALPHAN=AG_ALPHAN+1                                         582
            AG_ALPHAK(AG_ALPHAN)=IGOR('XY-Z')                             582
            AG_ALPHAN=AG_ALPHAN+1                                         582
            AG_ALPHAK(AG_ALPHAN)=3                                        582
            AG_ALPHAL(AG_ALPHAN)=ISECANG                                  582
            AG_KONLY = 'MANY'                                             582
            AG_TITLE = 'TSWH'                                             582
            AG_EXNAME = 'POSITION'                                        582
            AG_PARLIST = 'ORT_ALPHAZ_KONLY'                               582
            CALL AXPOSITION                                               582
C       *                                                                 582
         GO TO 5131                                                       583
5132     CONTINUE                                                         583
5021  CALL AGSPOPB                                                        584
      GO TO BLOCK_TPCE, (5020 ,5030)                                      584
C End of Local Procedure                                                  584
C ----------------------------------------------------------------------  585
C ------------ TPCM                                                       585
C Begin Local Procedure TPCM                                              586
C TPCM IS THE CENTRAL MEMBRANE PLACED IN TPC                              586
5070  CONTINUE                                                            586
         AG_TITLE='TPCM'                                                  586
         AG_MODULE=MODULE                                                 586
         AG_MARK='V00'                                                    586
         CALL AGSPUSH                                                     586
         AG_MODULE=MODULE                                                 586
C    *                                                                    586
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            587
         CALL AGSRESET                                                    587
         AG_TITLE = 'MYLAR'                                               587
         AG_EXNAME = 'MATERIAL'                                           587
         AG_PARLIST = 'NONE'                                              587
         CALL AXMATERIAL                                                  587
C    *                                                                    587
C    Geant MEDIUM : TPCM_MYLAR ===> IFIELD=1 FIELDM=20 TMAXFD=20 EPSIL=0  587
C    .01 STEMAX=10 DEEMAX=0.212557, STMIN=0.0311385 ISVOL=0 <===          587
         CALL AGSRESET                                                    589
         AG_IFIELD = 1                                                    589
         AG_FIELDM = 20                                                   589
         AG_TMAXFD = 20                                                   589
         AG_EPSIL = 0.01                                                  589
         AG_STEMAX = 10                                                   589
         AG_DEEMAX = 0.212557                                             589
         AG_STMIN = 0.0311385                                             589
         AG_ISVOL = 0                                                     589
         AG_TITLE = 'TPCM_MYLAR'                                          589
         AG_EXNAME = 'MEDIUM'                                             589
         AG_PARLIST = 'IFIELD_FIELDM_TMAXFD_EPSIL_STEMAX_DEEMAX_STMIN_IS  589
     *VOL'                                                                589
         CALL AXMEDIUM                                                    589
C    *                                                                    589
C    Geant ATTRIBUTE : TPCM ===> SEEN=1 COLO=KBLUE <===                   589
         CALL AGSRESET                                                    590
         AG_SEEN = 1                                                      590
         AG_COLO = KBLUE                                                  590
         AG_TITLE = 'TPCM'                                                590
         AG_EXNAME = 'ATTRIBUTE'                                          590
         AG_PARLIST = 'SEEN_COLO'                                         590
         CALL AXATTRIBUTE                                                 590
C    *                                                                    590
C    Geant SHAPE : TUBE ===> RMIN=TPGVIR RMAX=TPCG_SENGASOR, DZ=TPCG_MEM  590
C    BTHK/2 <===                                                          590
         CALL AGSRESET                                                    592
         AG_RMIN = TPGVIR                                                 592
         AG_RMAX = TPCG_SENGASOR                                          592
         AG_DZ = TPCG_MEMBTHK/2                                           592
         AG_TITLE = 'TUBE'                                                592
         AG_EXNAME = 'SHAPE'                                              592
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      592
         CALL AXSHAPE                                                     592
C    *                                                                    592
5071  CALL AGSPOPB                                                        593
      GO TO BLOCK_TPCM, (5070 ,5080)                                      593
C End of Local Procedure                                                  593
C ----------------------------------------------------------------------  594
C ------------ TIFC                                                       594
C Begin Local Procedure TIFC                                              595
C TIFC DEFINES THE INNER FIELD CAGE PLACED IN TPC                         595
5090  CONTINUE                                                            595
         AG_TITLE='TIFC'                                                  595
         AG_MODULE=MODULE                                                 595
         AG_MARK='V00'                                                    595
         CALL AGSPUSH                                                     595
         AG_MODULE=MODULE                                                 595
C    *                                                                    595
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        596
         CALL AGSRESET                                                    596
         AG_TITLE = 'ALUMINIUM'                                           596
         AG_EXNAME = 'MATERIAL'                                           596
         AG_PARLIST = 'NONE'                                              596
         CALL AXMATERIAL                                                  596
C    *                                                                    596
C    Geant MEDIUM : TIFC_ALUMINIUM ===> IFIELD=1 FIELDM=20 TMAXFD=20 EPS  596
C    IL=0.01 STEMAX=10 DEEMAX=0.18296, STMIN=0.0334422 ISVOL=0 <===       596
         CALL AGSRESET                                                    598
         AG_IFIELD = 1                                                    598
         AG_FIELDM = 20                                                   598
         AG_TMAXFD = 20                                                   598
         AG_EPSIL = 0.01                                                  598
         AG_STEMAX = 10                                                   598
         AG_DEEMAX = 0.18296                                              598
         AG_STMIN = 0.0334422                                             598
         AG_ISVOL = 0                                                     598
         AG_TITLE = 'TIFC_ALUMINIUM'                                      598
         AG_EXNAME = 'MEDIUM'                                             598
         AG_PARLIST = 'IFIELD_FIELDM_TMAXFD_EPSIL_STEMAX_DEEMAX_STMIN_IS  598
     *VOL'                                                                598
         CALL AXMEDIUM                                                    598
C    *                                                                    598
C    Geant ATTRIBUTE : TIFC ===> SEEN=1 COLO=KBLUE <===                   598
         CALL AGSRESET                                                    599
         AG_SEEN = 1                                                      599
         AG_COLO = KBLUE                                                  599
         AG_TITLE = 'TIFC'                                                599
         AG_EXNAME = 'ATTRIBUTE'                                          599
         AG_PARLIST = 'SEEN_COLO'                                         599
         CALL AXATTRIBUTE                                                 599
C    *                                                                    599
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=6 ZI={-ZTIFCEND, -ZTIFCF  599
C    LANGEBEGIN,-ZTIFCFLANGEBEGIN,ZTIFCFLANGEBEGIN, ZTIFCFLANGEBEGIN,ZTI  599
C    FCEND} RMN={TIFCIR, TIFCIR,TIFCIR,TIFCIR,TIFCIR,TIFCIR} RMX={TPCG_T  599
C    IFCRF, TPCG_TIFCRF,TIFCOR,TIFCOR,TPCG_TIFCRF, TPCG_TIFCRF} <===      599
         CALL AGSRESET                                                    605
         AG_PHI1 = 0                                                      605
         AG_DPHI = 360                                                    605
         AG_NZ = 6                                                        605
         AG_ZI(1)=-ZTIFCEND                                               605
         AG_ZI(2)=-ZTIFCFLANGEBEGIN                                       605
         AG_ZI(3)=-ZTIFCFLANGEBEGIN                                       605
         AG_ZI(4)=ZTIFCFLANGEBEGIN                                        605
         AG_ZI(5)=ZTIFCFLANGEBEGIN                                        605
         AG_ZI(6)=ZTIFCEND                                                605
         AG_RMN(1)=TIFCIR                                                 605
         AG_RMN(2)=TIFCIR                                                 605
         AG_RMN(3)=TIFCIR                                                 605
         AG_RMN(4)=TIFCIR                                                 605
         AG_RMN(5)=TIFCIR                                                 605
         AG_RMN(6)=TIFCIR                                                 605
         AG_RMX(1)=TPCG_TIFCRF                                            605
         AG_RMX(2)=TPCG_TIFCRF                                            605
         AG_RMX(3)=TIFCOR                                                 605
         AG_RMX(4)=TIFCOR                                                 605
         AG_RMX(5)=TPCG_TIFCRF                                            605
         AG_RMX(6)=TPCG_TIFCRF                                            605
         AG_TITLE = 'PCON'                                                605
         AG_EXNAME = 'SHAPE'                                              605
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           605
         CALL AXSHAPE                                                     605
C    *                                                                    605
C    Create TIKA :                                                        606
C    Call To Local Procedure TIKA                                         606
         ASSIGN 5180 TO BLOCK_TIKA                                        606
         GO TO  5170                                                      606
5180     CONTINUE                                                         606
C    *                                                                    606
C    Geant POSITION : TIKA ===> inherited arguments used <===             607
         CALL AGSRESET                                                    607
         AG_TITLE = 'TIKA'                                                607
         AG_EXNAME = 'POSITION'                                           607
         AG_PARLIST = 'NONE'                                              607
         CALL AXPOSITION                                                  607
C    *                                                                    607
C    Create TINX :                                                        608
C    Call To Local Procedure TINX                                         608
         ASSIGN 5200 TO BLOCK_TINX                                        608
         GO TO  5190                                                      608
5200     CONTINUE                                                         608
C    *                                                                    608
C    Geant POSITION : TINX ===> inherited arguments used <===             609
         CALL AGSRESET                                                    609
         AG_TITLE = 'TINX'                                                609
         AG_EXNAME = 'POSITION'                                           609
         AG_PARLIST = 'NONE'                                              609
         CALL AXPOSITION                                                  609
C    *                                                                    609
C    Create TIAD :                                                        610
C    Call To Local Procedure TIAD                                         610
         ASSIGN 5220 TO BLOCK_TIAD                                        610
         GO TO  5210                                                      610
5220     CONTINUE                                                         610
C    *                                                                    610
C    Geant POSITION : TIAD ===> inherited arguments used <===             611
         CALL AGSRESET                                                    611
         AG_TITLE = 'TIAD'                                                611
         AG_EXNAME = 'POSITION'                                           611
         AG_PARLIST = 'NONE'                                              611
         CALL AXPOSITION                                                  611
C    *                                                                    611
5091  CALL AGSPOPB                                                        612
      GO TO BLOCK_TIFC, (5090 ,5100)                                      612
C End of Local Procedure                                                  612
C ----------------------------------------------------------------------  613
C ------------ TIKA                                                       613
C Begin Local Procedure TIKA                                              614
C TIKA IS THE KAPTON FILM OF THE INNER FIELD CAGE                         614
5170  CONTINUE                                                            614
         AG_TITLE='TIKA'                                                  614
         AG_MODULE=MODULE                                                 614
         AG_MARK='V00'                                                    614
         CALL AGSPUSH                                                     614
         AG_MODULE=MODULE                                                 614
C    *                                                                    614
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            615
         CALL AGSRESET                                                    615
         AG_TITLE = 'MYLAR'                                               615
         AG_EXNAME = 'MATERIAL'                                           615
         AG_PARLIST = 'NONE'                                              615
         CALL AXMATERIAL                                                  615
C    *                                                                    615
C    Geant MEDIUM : TIKA_MYLAR ===> IFIELD=1 FIELDM=20 TMAXFD=20 EPSIL=0  615
C    .01 STEMAX=10 DEEMAX=0.212557, STMIN=0.0311385 ISVOL=0 <===          615
         CALL AGSRESET                                                    617
         AG_IFIELD = 1                                                    617
         AG_FIELDM = 20                                                   617
         AG_TMAXFD = 20                                                   617
         AG_EPSIL = 0.01                                                  617
         AG_STEMAX = 10                                                   617
         AG_DEEMAX = 0.212557                                             617
         AG_STMIN = 0.0311385                                             617
         AG_ISVOL = 0                                                     617
         AG_TITLE = 'TIKA_MYLAR'                                          617
         AG_EXNAME = 'MEDIUM'                                             617
         AG_PARLIST = 'IFIELD_FIELDM_TMAXFD_EPSIL_STEMAX_DEEMAX_STMIN_IS  617
     *VOL'                                                                617
         CALL AXMEDIUM                                                    617
C    *                                                                    617
C    Geant ATTRIBUTE : TIKA ===> SEEN=1 COLO=KVIOLET <===                 617
         CALL AGSRESET                                                    618
         AG_SEEN = 1                                                      618
         AG_COLO = KVIOLET                                                618
         AG_TITLE = 'TIKA'                                                618
         AG_EXNAME = 'ATTRIBUTE'                                          618
         AG_PARLIST = 'SEEN_COLO'                                         618
         CALL AXATTRIBUTE                                                 618
C    *                                                                    618
C    Geant SHAPE : TUBE ===> RMIN=TIKAIR RMAX=TIKAOR DZ=TPCG_DZYF1 <===   618
         CALL AGSRESET                                                    619
         AG_RMIN = TIKAIR                                                 619
         AG_RMAX = TIKAOR                                                 619
         AG_DZ = TPCG_DZYF1                                               619
         AG_TITLE = 'TUBE'                                                619
         AG_EXNAME = 'SHAPE'                                              619
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      619
         CALL AXSHAPE                                                     619
C    *                                                                    619
5171  CALL AGSPOPB                                                        620
      GO TO BLOCK_TIKA, (5170 ,5180)                                      620
C End of Local Procedure                                                  620
C ----------------------------------------------------------------------  621
C ------------ TINX                                                       621
C Begin Local Procedure TINX                                              622
C TINX IS THE INNER NOMEX STRUCTUREURE                                    622
5190  CONTINUE                                                            622
         AG_TITLE='TINX'                                                  622
         AG_MODULE=MODULE                                                 622
         AG_MARK='V00'                                                    622
         CALL AGSPUSH                                                     622
         AG_MODULE=MODULE                                                 622
C    *                                                                    622
C    Geant MATERIAL : NOMEX ===> inherited arguments used <===            623
         CALL AGSRESET                                                    623
         AG_TITLE = 'NOMEX'                                               623
         AG_EXNAME = 'MATERIAL'                                           623
         AG_PARLIST = 'NONE'                                              623
         CALL AXMATERIAL                                                  623
C    *                                                                    623
C    Geant MEDIUM : TINX_NOMEX ===> IFIELD=1 FIELDM=20 TMAXFD=20 EPSIL=0  623
C    .01 STEMAX=10 DEEMAX=0.242015, STMIN=135061 ISVOL=0 <===             623
         CALL AGSRESET                                                    625
         AG_IFIELD = 1                                                    625
         AG_FIELDM = 20                                                   625
         AG_TMAXFD = 20                                                   625
         AG_EPSIL = 0.01                                                  625
         AG_STEMAX = 10                                                   625
         AG_DEEMAX = 0.242015                                             625
         AG_STMIN = 135061                                                625
         AG_ISVOL = 0                                                     625
         AG_TITLE = 'TINX_NOMEX'                                          625
         AG_EXNAME = 'MEDIUM'                                             625
         AG_PARLIST = 'IFIELD_FIELDM_TMAXFD_EPSIL_STEMAX_DEEMAX_STMIN_IS  625
     *VOL'                                                                625
         CALL AXMEDIUM                                                    625
C    *                                                                    625
C    Geant ATTRIBUTE : TINX ===> SEEN=1 COLO=KYELLOW <===                 625
         CALL AGSRESET                                                    626
         AG_SEEN = 1                                                      626
         AG_COLO = KYELLOW                                                626
         AG_TITLE = 'TINX'                                                626
         AG_EXNAME = 'ATTRIBUTE'                                          626
         AG_PARLIST = 'SEEN_COLO'                                         626
         CALL AXATTRIBUTE                                                 626
C    *                                                                    626
C    Geant SHAPE : TUBE ===> RMIN=TINXIR RMAX=TINXOR DZ=TPCG_DZYF1 <===   626
         CALL AGSRESET                                                    627
         AG_RMIN = TINXIR                                                 627
         AG_RMAX = TINXOR                                                 627
         AG_DZ = TPCG_DZYF1                                               627
         AG_TITLE = 'TUBE'                                                627
         AG_EXNAME = 'SHAPE'                                              627
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      627
         CALL AXSHAPE                                                     627
C    *                                                                    627
5191  CALL AGSPOPB                                                        628
      GO TO BLOCK_TINX, (5190 ,5200)                                      628
C End of Local Procedure                                                  628
C ----------------------------------------------------------------------  629
C ------------ TIAD                                                       629
C Begin Local Procedure TIAD                                              630
C TIAD THE INNER ADHESIVE STRUCTUREURE                                    630
5210  CONTINUE                                                            630
         AG_TITLE='TIAD'                                                  630
         AG_MODULE=MODULE                                                 630
         AG_MARK='V00'                                                    630
         CALL AGSPUSH                                                     630
         AG_MODULE=MODULE                                                 630
C    *                                                                    630
C    Geant MATERIAL : ADHESIVE ===> inherited arguments used <===         631
         CALL AGSRESET                                                    631
         AG_TITLE = 'ADHESIVE'                                            631
         AG_EXNAME = 'MATERIAL'                                           631
         AG_PARLIST = 'NONE'                                              631
         CALL AXMATERIAL                                                  631
C    *                                                                    631
C    Geant MEDIUM : TIAD_ADHESIVE ===> IFIELD=1 FIELDM=20 TMAXFD=20 EPSI  631
C    L=0.01 STEMAX=10 DEEMAX=0.21521, STMIN=0.033412 ISVOL=0 <===         631
         CALL AGSRESET                                                    633
         AG_IFIELD = 1                                                    633
         AG_FIELDM = 20                                                   633
         AG_TMAXFD = 20                                                   633
         AG_EPSIL = 0.01                                                  633
         AG_STEMAX = 10                                                   633
         AG_DEEMAX = 0.21521                                              633
         AG_STMIN = 0.033412                                              633
         AG_ISVOL = 0                                                     633
         AG_TITLE = 'TIAD_ADHESIVE'                                       633
         AG_EXNAME = 'MEDIUM'                                             633
         AG_PARLIST = 'IFIELD_FIELDM_TMAXFD_EPSIL_STEMAX_DEEMAX_STMIN_IS  633
     *VOL'                                                                633
         CALL AXMEDIUM                                                    633
C    *                                                                    633
C    Geant ATTRIBUTE : TIKA ===> SEEN=1 COLO=KVIOLET <===                 633
         CALL AGSRESET                                                    634
         AG_SEEN = 1                                                      634
         AG_COLO = KVIOLET                                                634
         AG_TITLE = 'TIKA'                                                634
         AG_EXNAME = 'ATTRIBUTE'                                          634
         AG_PARLIST = 'SEEN_COLO'                                         634
         CALL AXATTRIBUTE                                                 634
C    *                                                                    634
C    Geant SHAPE : TUBE ===> RMIN=TIADIR RMAX=TIADOR DZ=TPCG_DZYF1 <===   634
         CALL AGSRESET                                                    635
         AG_RMIN = TIADIR                                                 635
         AG_RMAX = TIADOR                                                 635
         AG_DZ = TPCG_DZYF1                                               635
         AG_TITLE = 'TUBE'                                                635
         AG_EXNAME = 'SHAPE'                                              635
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      635
         CALL AXSHAPE                                                     635
C    *                                                                    635
5211  CALL AGSPOPB                                                        636
      GO TO BLOCK_TIAD, (5210 ,5220)                                      636
C End of Local Procedure                                                  636
C ----------------------------------------------------------------------  637
C ------------ TOFC                                                       637
C Begin Local Procedure TOFC                                              638
C TOFC DEFINES OUTER FIELD CAGE - FILL IT WITH INSULATING GAS ALREADY     638
5110  CONTINUE                                                            638
         AG_TITLE='TOFC'                                                  638
         AG_MODULE=MODULE                                                 638
         AG_MARK='V00'                                                    638
         CALL AGSPUSH                                                     638
         AG_MODULE=MODULE                                                 638
C    *                                                                    638
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        639
         CALL AGSRESET                                                    639
         AG_TITLE = 'ALUMINIUM'                                           639
         AG_EXNAME = 'MATERIAL'                                           639
         AG_PARLIST = 'NONE'                                              639
         CALL AXMATERIAL                                                  639
C    *                                                                    639
C    Geant MEDIUM : TOFC_ALUMINIUM ===> IFIELD=1 FIELDM=20 TMAXFD=20 EPS  639
C    IL=0.01 STEMAX=10 DEEMAX=0.18296, STMIN=0.0334422 ISVOL=0 <===       639
         CALL AGSRESET                                                    641
         AG_IFIELD = 1                                                    641
         AG_FIELDM = 20                                                   641
         AG_TMAXFD = 20                                                   641
         AG_EPSIL = 0.01                                                  641
         AG_STEMAX = 10                                                   641
         AG_DEEMAX = 0.18296                                              641
         AG_STMIN = 0.0334422                                             641
         AG_ISVOL = 0                                                     641
         AG_TITLE = 'TOFC_ALUMINIUM'                                      641
         AG_EXNAME = 'MEDIUM'                                             641
         AG_PARLIST = 'IFIELD_FIELDM_TMAXFD_EPSIL_STEMAX_DEEMAX_STMIN_IS  641
     *VOL'                                                                641
         CALL AXMEDIUM                                                    641
C    *                                                                    641
C    Geant ATTRIBUTE : TOFC ===> SEEN=1 COLO=KGREEN <===                  641
         CALL AGSRESET                                                    642
         AG_SEEN = 1                                                      642
         AG_COLO = KGREEN                                                 642
         AG_TITLE = 'TOFC'                                                642
         AG_EXNAME = 'ATTRIBUTE'                                          642
         AG_PARLIST = 'SEEN_COLO'                                         642
         CALL AXATTRIBUTE                                                 642
C    *                                                                    642
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=6 ZI={-ZTOFCEND, -ZWHEEL  642
C    1,-ZWHEEL1,ZWHEEL1,ZWHEEL1,ZTOFCEND}, RMN={TPCG_WHEELOR1,TPCG_WHEEL  642
C    OR1,TOFCIR, TOFCIR,TPCG_WHEELOR1,TPCG_WHEELOR1} RMX={TPCG_WHEELOR,   642
C    TPCG_WHEELOR,TOFCOR,TOFCOR,TPCG_WHEELOR, TPCG_WHEELOR} <===          642
         CALL AGSRESET                                                    648
         AG_PHI1 = 0                                                      648
         AG_DPHI = 360                                                    648
         AG_NZ = 6                                                        648
         AG_ZI(1)=-ZTOFCEND                                               648
         AG_ZI(2)=-ZWHEEL1                                                648
         AG_ZI(3)=-ZWHEEL1                                                648
         AG_ZI(4)=ZWHEEL1                                                 648
         AG_ZI(5)=ZWHEEL1                                                 648
         AG_ZI(6)=ZTOFCEND                                                648
         AG_RMN(1)=TPCG_WHEELOR1                                          648
         AG_RMN(2)=TPCG_WHEELOR1                                          648
         AG_RMN(3)=TOFCIR                                                 648
         AG_RMN(4)=TOFCIR                                                 648
         AG_RMN(5)=TPCG_WHEELOR1                                          648
         AG_RMN(6)=TPCG_WHEELOR1                                          648
         AG_RMX(1)=TPCG_WHEELOR                                           648
         AG_RMX(2)=TPCG_WHEELOR                                           648
         AG_RMX(3)=TOFCOR                                                 648
         AG_RMX(4)=TOFCOR                                                 648
         AG_RMX(5)=TPCG_WHEELOR                                           648
         AG_RMX(6)=TPCG_WHEELOR                                           648
         AG_TITLE = 'PCON'                                                648
         AG_EXNAME = 'SHAPE'                                              648
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           648
         CALL AXSHAPE                                                     648
C    *                                                                    648
C    Create TOAD :                                                        649
C    Call To Local Procedure TOAD                                         649
         ASSIGN 5240 TO BLOCK_TOAD                                        649
         GO TO  5230                                                      649
5240     CONTINUE                                                         649
C    *                                                                    649
C    Geant POSITION : TOAD ===> inherited arguments used <===             650
         CALL AGSRESET                                                    650
         AG_TITLE = 'TOAD'                                                650
         AG_EXNAME = 'POSITION'                                           650
         AG_PARLIST = 'NONE'                                              650
         CALL AXPOSITION                                                  650
C    *                                                                    650
C    Create TOFS :                                                        651
C    Call To Local Procedure TOFS                                         651
         ASSIGN 5260 TO BLOCK_TOFS                                        651
         GO TO  5250                                                      651
5260     CONTINUE                                                         651
C    *                                                                    651
C    Geant POSITION : TOFS ===> inherited arguments used <===             652
         CALL AGSRESET                                                    652
         AG_TITLE = 'TOFS'                                                652
         AG_EXNAME = 'POSITION'                                           652
         AG_PARLIST = 'NONE'                                              652
         CALL AXPOSITION                                                  652
C    *                                                                    652
C    Create TOKA :                                                        653
C    Call To Local Procedure TOKA                                         653
         ASSIGN 5280 TO BLOCK_TOKA                                        653
         GO TO  5270                                                      653
5280     CONTINUE                                                         653
C    *                                                                    653
C    Geant POSITION : TOKA ===> inherited arguments used <===             654
         CALL AGSRESET                                                    654
         AG_TITLE = 'TOKA'                                                654
         AG_EXNAME = 'POSITION'                                           654
         AG_PARLIST = 'NONE'                                              654
         CALL AXPOSITION                                                  654
C    *                                                                    654
C    Create TONX :                                                        655
C    Call To Local Procedure TONX                                         655
         ASSIGN 5300 TO BLOCK_TONX                                        655
         GO TO  5290                                                      655
5300     CONTINUE                                                         655
C    *                                                                    655
C    Geant POSITION : TONX ===> inherited arguments used <===             656
         CALL AGSRESET                                                    656
         AG_TITLE = 'TONX'                                                656
         AG_EXNAME = 'POSITION'                                           656
         AG_PARLIST = 'NONE'                                              656
         CALL AXPOSITION                                                  656
C    *                                                                    656
C    Create TOIG :                                                        657
C    Call To Local Procedure TOIG                                         657
         ASSIGN 5320 TO BLOCK_TOIG                                        657
         GO TO  5310                                                      657
5320     CONTINUE                                                         657
C    *                                                                    657
C    Geant POSITION : TOIG ===> inherited arguments used <===             658
         CALL AGSRESET                                                    658
         AG_TITLE = 'TOIG'                                                658
         AG_EXNAME = 'POSITION'                                           658
         AG_PARLIST = 'NONE'                                              658
         CALL AXPOSITION                                                  658
C    *                                                                    658
C    Create TOHA :                                                        659
C    Call To Local Procedure TOHA                                         659
         ASSIGN 5340 TO BLOCK_TOHA                                        659
         GO TO  5330                                                      659
5340     CONTINUE                                                         659
C    *                                                                    659
C    Geant POSITION : TOHA ===> inherited arguments used <===             660
         CALL AGSRESET                                                    660
         AG_TITLE = 'TOHA'                                                660
         AG_EXNAME = 'POSITION'                                           660
         AG_PARLIST = 'NONE'                                              660
         CALL AXPOSITION                                                  660
C    *                                                                    660
5111  CALL AGSPOPB                                                        661
      GO TO BLOCK_TOFC, (5110 ,5120)                                      661
C End of Local Procedure                                                  661
C ----------------------------------------------------------------------  662
C ------------ TOFS                                                       662
C Begin Local Procedure TOFS                                              663
C TOFS IS THE OUTER FIELD CAGE STRUCTUREURE                               663
5250  CONTINUE                                                            663
         AG_TITLE='TOFS'                                                  663
         AG_MODULE=MODULE                                                 663
         AG_MARK='V00'                                                    663
         CALL AGSPUSH                                                     663
         AG_MODULE=MODULE                                                 663
C    *                                                                    663
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           664
         CALL AGSRESET                                                    664
         AG_TITLE = 'COPPER'                                              664
         AG_EXNAME = 'MATERIAL'                                           664
         AG_PARLIST = 'NONE'                                              664
         CALL AXMATERIAL                                                  664
C    *                                                                    664
C    Geant MEDIUM : TOFS_COPPER ===> IFIELD=1 FIELDM=20 TMAXFD=20 EPSIL=  664
C    0.01 STEMAX=10 DEEMAX=0.25, STMIN=0.0281386 ISVOL=0 <===             664
         CALL AGSRESET                                                    666
         AG_IFIELD = 1                                                    666
         AG_FIELDM = 20                                                   666
         AG_TMAXFD = 20                                                   666
         AG_EPSIL = 0.01                                                  666
         AG_STEMAX = 10                                                   666
         AG_DEEMAX = 0.25                                                 666
         AG_STMIN = 0.0281386                                             666
         AG_ISVOL = 0                                                     666
         AG_TITLE = 'TOFS_COPPER'                                         666
         AG_EXNAME = 'MEDIUM'                                             666
         AG_PARLIST = 'IFIELD_FIELDM_TMAXFD_EPSIL_STEMAX_DEEMAX_STMIN_IS  666
     *VOL'                                                                666
         CALL AXMEDIUM                                                    666
C    *                                                                    666
C    Geant ATTRIBUTE : TOFS ===> SEEN=1 COLO=KBLUE <===                   666
         CALL AGSRESET                                                    667
         AG_SEEN = 1                                                      667
         AG_COLO = KBLUE                                                  667
         AG_TITLE = 'TOFS'                                                667
         AG_EXNAME = 'ATTRIBUTE'                                          667
         AG_PARLIST = 'SEEN_COLO'                                         667
         CALL AXATTRIBUTE                                                 667
C    *                                                                    667
C    Geant SHAPE : TUBE ===> RMIN=TOCSIR RMAX=TOCSOR DZ=TPCG_DZYF2 <===   667
         CALL AGSRESET                                                    668
         AG_RMIN = TOCSIR                                                 668
         AG_RMAX = TOCSOR                                                 668
         AG_DZ = TPCG_DZYF2                                               668
         AG_TITLE = 'TUBE'                                                668
         AG_EXNAME = 'SHAPE'                                              668
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      668
         CALL AXSHAPE                                                     668
C    *                                                                    668
5251  CALL AGSPOPB                                                        669
      GO TO BLOCK_TOFS, (5250 ,5260)                                      669
C End of Local Procedure                                                  669
C ----------------------------------------------------------------------  670
C ------------ TOKA                                                       670
C Begin Local Procedure TOKA                                              671
C TOKA IS MYLAR LAYER                                                     671
5270  CONTINUE                                                            671
         AG_TITLE='TOKA'                                                  671
         AG_MODULE=MODULE                                                 671
         AG_MARK='V00'                                                    671
         CALL AGSPUSH                                                     671
         AG_MODULE=MODULE                                                 671
C    *                                                                    671
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            672
         CALL AGSRESET                                                    672
         AG_TITLE = 'MYLAR'                                               672
         AG_EXNAME = 'MATERIAL'                                           672
         AG_PARLIST = 'NONE'                                              672
         CALL AXMATERIAL                                                  672
C    *                                                                    672
C    Geant MEDIUM : TOKA_MYLAR ===> IFIELD=1 FIELDM=20 TMAXFD=20 EPSIL=0  672
C    .01 STEMAX=10 DEEMAX=0.212557, STMIN=0.0311385 ISVOL=0 <===          672
         CALL AGSRESET                                                    674
         AG_IFIELD = 1                                                    674
         AG_FIELDM = 20                                                   674
         AG_TMAXFD = 20                                                   674
         AG_EPSIL = 0.01                                                  674
         AG_STEMAX = 10                                                   674
         AG_DEEMAX = 0.212557                                             674
         AG_STMIN = 0.0311385                                             674
         AG_ISVOL = 0                                                     674
         AG_TITLE = 'TOKA_MYLAR'                                          674
         AG_EXNAME = 'MEDIUM'                                             674
         AG_PARLIST = 'IFIELD_FIELDM_TMAXFD_EPSIL_STEMAX_DEEMAX_STMIN_IS  674
     *VOL'                                                                674
         CALL AXMEDIUM                                                    674
C    *                                                                    674
C    Geant ATTRIBUTE : TOKA ===> SEEN=1 COLO=KGREEN <===                  674
         CALL AGSRESET                                                    675
         AG_SEEN = 1                                                      675
         AG_COLO = KGREEN                                                 675
         AG_TITLE = 'TOKA'                                                675
         AG_EXNAME = 'ATTRIBUTE'                                          675
         AG_PARLIST = 'SEEN_COLO'                                         675
         CALL AXATTRIBUTE                                                 675
C    *                                                                    675
C    Geant SHAPE : TUBE ===> RMIN=TOKAIR RMAX=TOKAOR DZ=TPCG_DZYF2 <===   675
         CALL AGSRESET                                                    676
         AG_RMIN = TOKAIR                                                 676
         AG_RMAX = TOKAOR                                                 676
         AG_DZ = TPCG_DZYF2                                               676
         AG_TITLE = 'TUBE'                                                676
         AG_EXNAME = 'SHAPE'                                              676
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      676
         CALL AXSHAPE                                                     676
C    *                                                                    676
5271  CALL AGSPOPB                                                        677
      GO TO BLOCK_TOKA, (5270 ,5280)                                      677
C End of Local Procedure                                                  677
C ----------------------------------------------------------------------  678
C ------------ TONX                                                       678
C Begin Local Procedure TONX                                              679
C TONX IS NOMEX SUPPORT                                                   679
5290  CONTINUE                                                            679
         AG_TITLE='TONX'                                                  679
         AG_MODULE=MODULE                                                 679
         AG_MARK='V00'                                                    679
         CALL AGSPUSH                                                     679
         AG_MODULE=MODULE                                                 679
C    *                                                                    679
C    Geant MATERIAL : NOMEX ===> inherited arguments used <===            680
         CALL AGSRESET                                                    680
         AG_TITLE = 'NOMEX'                                               680
         AG_EXNAME = 'MATERIAL'                                           680
         AG_PARLIST = 'NONE'                                              680
         CALL AXMATERIAL                                                  680
C    *                                                                    680
C    Geant MEDIUM : TONX_NOMEX ===> IFIELD=1 FIELDM=20 TMAXFD=20 EPSIL=0  680
C    .01 STEMAX=10 DEEMAX=0.242015, STMIN=0.135061 ISVOL=0 <===           680
         CALL AGSRESET                                                    682
         AG_IFIELD = 1                                                    682
         AG_FIELDM = 20                                                   682
         AG_TMAXFD = 20                                                   682
         AG_EPSIL = 0.01                                                  682
         AG_STEMAX = 10                                                   682
         AG_DEEMAX = 0.242015                                             682
         AG_STMIN = 0.135061                                              682
         AG_ISVOL = 0                                                     682
         AG_TITLE = 'TONX_NOMEX'                                          682
         AG_EXNAME = 'MEDIUM'                                             682
         AG_PARLIST = 'IFIELD_FIELDM_TMAXFD_EPSIL_STEMAX_DEEMAX_STMIN_IS  682
     *VOL'                                                                682
         CALL AXMEDIUM                                                    682
C    *                                                                    682
C    Geant ATTRIBUTE : TONX ===> SEEN=1 COLO=KRED <===                    682
         CALL AGSRESET                                                    683
         AG_SEEN = 1                                                      683
         AG_COLO = KRED                                                   683
         AG_TITLE = 'TONX'                                                683
         AG_EXNAME = 'ATTRIBUTE'                                          683
         AG_PARLIST = 'SEEN_COLO'                                         683
         CALL AXATTRIBUTE                                                 683
C    *                                                                    683
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=6 ZI={-TPCG_DZYF2, -TPCG  683
C    _DZYF3,-TPCG_DZYF3,TPCG_DZYF3,TPCG_DZYF3, TPCG_DZYF2} RMN={TONXOR-0  683
C    .2,TONXOR-0.2, TONXIR,TONXIR,TONXOR-0.2,TONXOR-0.2} RMX={TONXOR, TO  683
C    NXOR,TONXOR,TONXOR,TONXOR,TONXOR} <===                               683
         CALL AGSRESET                                                    688
         AG_PHI1 = 0                                                      688
         AG_DPHI = 360                                                    688
         AG_NZ = 6                                                        688
         AG_ZI(1)=-TPCG_DZYF2                                             688
         AG_ZI(2)=-TPCG_DZYF3                                             688
         AG_ZI(3)=-TPCG_DZYF3                                             688
         AG_ZI(4)=TPCG_DZYF3                                              688
         AG_ZI(5)=TPCG_DZYF3                                              688
         AG_ZI(6)=TPCG_DZYF2                                              688
         AG_RMN(1)=TONXOR-0.2                                             688
         AG_RMN(2)=TONXOR-0.2                                             688
         AG_RMN(3)=TONXIR                                                 688
         AG_RMN(4)=TONXIR                                                 688
         AG_RMN(5)=TONXOR-0.2                                             688
         AG_RMN(6)=TONXOR-0.2                                             688
         AG_RMX(1)=TONXOR                                                 688
         AG_RMX(2)=TONXOR                                                 688
         AG_RMX(3)=TONXOR                                                 688
         AG_RMX(4)=TONXOR                                                 688
         AG_RMX(5)=TONXOR                                                 688
         AG_RMX(6)=TONXOR                                                 688
         AG_TITLE = 'PCON'                                                688
         AG_EXNAME = 'SHAPE'                                              688
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           688
         CALL AXSHAPE                                                     688
C    *                                                                    688
5291  CALL AGSPOPB                                                        689
      GO TO BLOCK_TONX, (5290 ,5300)                                      689
C End of Local Procedure                                                  689
C ----------------------------------------------------------------------  690
C ------------ TOAD                                                       690
C Begin Local Procedure TOAD                                              691
C TOAD IS ADHESIVE LAYER                                                  691
5230  CONTINUE                                                            691
         AG_TITLE='TOAD'                                                  691
         AG_MODULE=MODULE                                                 691
         AG_MARK='V00'                                                    691
         CALL AGSPUSH                                                     691
         AG_MODULE=MODULE                                                 691
C    *                                                                    691
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            692
         CALL AGSRESET                                                    692
         AG_TITLE = 'MYLAR'                                               692
         AG_EXNAME = 'MATERIAL'                                           692
         AG_PARLIST = 'NONE'                                              692
         CALL AXMATERIAL                                                  692
C    *                                                                    692
C    Geant MEDIUM : TOAD_MYLAR ===> IFIELD=1 FIELDM=20 TMAXFD=20 EPSIL=0  692
C    .01 STEMAX=10 DEEMAX=0.212557, STMIN=0.0311385 ISVOL=0 <===          692
         CALL AGSRESET                                                    694
         AG_IFIELD = 1                                                    694
         AG_FIELDM = 20                                                   694
         AG_TMAXFD = 20                                                   694
         AG_EPSIL = 0.01                                                  694
         AG_STEMAX = 10                                                   694
         AG_DEEMAX = 0.212557                                             694
         AG_STMIN = 0.0311385                                             694
         AG_ISVOL = 0                                                     694
         AG_TITLE = 'TOAD_MYLAR'                                          694
         AG_EXNAME = 'MEDIUM'                                             694
         AG_PARLIST = 'IFIELD_FIELDM_TMAXFD_EPSIL_STEMAX_DEEMAX_STMIN_IS  694
     *VOL'                                                                694
         CALL AXMEDIUM                                                    694
C    *                                                                    694
C    Geant ATTRIBUTE : TOAD ===> SEEN=1 COLO=KLIGHTBLUE <===              694
         CALL AGSRESET                                                    695
         AG_SEEN = 1                                                      695
         AG_COLO = KLIGHTBLUE                                             695
         AG_TITLE = 'TOAD'                                                695
         AG_EXNAME = 'ATTRIBUTE'                                          695
         AG_PARLIST = 'SEEN_COLO'                                         695
         CALL AXATTRIBUTE                                                 695
C    *                                                                    695
C    Geant SHAPE : TUBE ===> RMIN=TOADIR RMAX=TOADOR DZ=TPCG_DZYF2 <===   695
         CALL AGSRESET                                                    696
         AG_RMIN = TOADIR                                                 696
         AG_RMAX = TOADOR                                                 696
         AG_DZ = TPCG_DZYF2                                               696
         AG_TITLE = 'TUBE'                                                696
         AG_EXNAME = 'SHAPE'                                              696
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      696
         CALL AXSHAPE                                                     696
C    *                                                                    696
5231  CALL AGSPOPB                                                        697
      GO TO BLOCK_TOAD, (5230 ,5240)                                      697
C End of Local Procedure                                                  697
C ----------------------------------------------------------------------  698
C ------------ TOIG                                                       698
C Begin Local Procedure TOIG                                              699
C TOIG IS INSULATING GAS (NITROGEN)                                       699
5310  CONTINUE                                                            699
         AG_TITLE='TOIG'                                                  699
         AG_MODULE=MODULE                                                 699
         AG_MARK='V00'                                                    699
         CALL AGSPUSH                                                     699
         AG_MODULE=MODULE                                                 699
C    *                                                                    699
C    Geant MATERIAL : NITROGEN_GAS ===> inherited arguments used <===     700
         CALL AGSRESET                                                    700
         AG_TITLE = 'NITROGEN_GAS'                                        700
         AG_EXNAME = 'MATERIAL'                                           700
         AG_PARLIST = 'NONE'                                              700
         CALL AXMATERIAL                                                  700
C    *                                                                    700
C    Geant MEDIUM : TOIG_NITROGEN_GAS ===> IFIELD=1 FIELDM=20 TMAXFD=20   700
C    EPSIL=0.01 STEMAX=10, DEEMAX=0.248893 STMIN=1.14554 ISVOL=0 <===     700
         CALL AGSRESET                                                    702
         AG_IFIELD = 1                                                    702
         AG_FIELDM = 20                                                   702
         AG_TMAXFD = 20                                                   702
         AG_EPSIL = 0.01                                                  702
         AG_STEMAX = 10                                                   702
         AG_DEEMAX = 0.248893                                             702
         AG_STMIN = 1.14554                                               702
         AG_ISVOL = 0                                                     702
         AG_TITLE = 'TOIG_NITROGEN_GAS'                                   702
         AG_EXNAME = 'MEDIUM'                                             702
         AG_PARLIST = 'IFIELD_FIELDM_TMAXFD_EPSIL_STEMAX_DEEMAX_STMIN_IS  702
     *VOL'                                                                702
         CALL AXMEDIUM                                                    702
C    *                                                                    702
C    Geant ATTRIBUTE : TOAD ===> SEEN=1 COLO=KLIGHTBLUE <===              702
         CALL AGSRESET                                                    703
         AG_SEEN = 1                                                      703
         AG_COLO = KLIGHTBLUE                                             703
         AG_TITLE = 'TOAD'                                                703
         AG_EXNAME = 'ATTRIBUTE'                                          703
         AG_PARLIST = 'SEEN_COLO'                                         703
         CALL AXATTRIBUTE                                                 703
C    *                                                                    703
C    Geant SHAPE : TUBE ===> RMIN=TOIGIR RMAX=TOIGOR DZ=TPCG_DZYF2 <===   703
         CALL AGSRESET                                                    704
         AG_RMIN = TOIGIR                                                 704
         AG_RMAX = TOIGOR                                                 704
         AG_DZ = TPCG_DZYF2                                               704
         AG_TITLE = 'TUBE'                                                704
         AG_EXNAME = 'SHAPE'                                              704
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      704
         CALL AXSHAPE                                                     704
C    *                                                                    704
5311  CALL AGSPOPB                                                        705
      GO TO BLOCK_TOIG, (5310 ,5320)                                      705
C End of Local Procedure                                                  705
C ----------------------------------------------------------------------  706
C ------------ TOHA                                                       706
C Begin Local Procedure TOHA                                              707
C TOHA GAS CONTAINMENT VESSEL (AL) + HA                                   707
5330  CONTINUE                                                            707
         AG_TITLE='TOHA'                                                  707
         AG_MODULE=MODULE                                                 707
         AG_MARK='V00'                                                    707
         CALL AGSPUSH                                                     707
         AG_MODULE=MODULE                                                 707
C    *                                                                    707
C    Geant MATERIAL : AL_HONEYCOMB ===> inherited arguments used <===     708
         CALL AGSRESET                                                    708
         AG_TITLE = 'AL_HONEYCOMB'                                        708
         AG_EXNAME = 'MATERIAL'                                           708
         AG_PARLIST = 'NONE'                                              708
         CALL AXMATERIAL                                                  708
C    *                                                                    708
C    Geant MEDIUM : TOHA_AL_HONEYCOMB ===> IFIELD=1 FIELDM=20 TMAXFD=20   708
C    EPSIL=0.01 STEMAX=10, DEEMAX=0.23328 STMIN=0.0707689 ISVOL=0 <===    708
         CALL AGSRESET                                                    710
         AG_IFIELD = 1                                                    710
         AG_FIELDM = 20                                                   710
         AG_TMAXFD = 20                                                   710
         AG_EPSIL = 0.01                                                  710
         AG_STEMAX = 10                                                   710
         AG_DEEMAX = 0.23328                                              710
         AG_STMIN = 0.0707689                                             710
         AG_ISVOL = 0                                                     710
         AG_TITLE = 'TOHA_AL_HONEYCOMB'                                   710
         AG_EXNAME = 'MEDIUM'                                             710
         AG_PARLIST = 'IFIELD_FIELDM_TMAXFD_EPSIL_STEMAX_DEEMAX_STMIN_IS  710
     *VOL'                                                                710
         CALL AXMEDIUM                                                    710
C    *                                                                    710
C    Geant ATTRIBUTE : TOHA ===> SEEN=1 COLO=KVIOLET <===                 710
         CALL AGSRESET                                                    711
         AG_SEEN = 1                                                      711
         AG_COLO = KVIOLET                                                711
         AG_TITLE = 'TOHA'                                                711
         AG_EXNAME = 'ATTRIBUTE'                                          711
         AG_PARLIST = 'SEEN_COLO'                                         711
         CALL AXATTRIBUTE                                                 711
C    *                                                                    711
C    Geant SHAPE : TUBE ===> RMIN=TOHAIR RMAX=TOHAOR DZ=TPCG_DZYF3 <===   711
         CALL AGSRESET                                                    712
         AG_RMIN = TOHAIR                                                 712
         AG_RMAX = TOHAOR                                                 712
         AG_DZ = TPCG_DZYF3                                               712
         AG_TITLE = 'TUBE'                                                712
         AG_EXNAME = 'SHAPE'                                              712
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      712
         CALL AXSHAPE                                                     712
C    *                                                                    712
5331  CALL AGSPOPB                                                        713
      GO TO BLOCK_TOHA, (5330 ,5340)                                      713
C End of Local Procedure                                                  713
C ----------------------------------------------------------------------  714
C ------------ TSWH                                                       714
C Begin Local Procedure TSWH                                              715
C TSWH TPCSECTORWHOLE IS SECTOR AS WHOLE                                  715
5140  CONTINUE                                                            715
         AG_TITLE='TSWH'                                                  715
         AG_MODULE=MODULE                                                 715
         AG_MARK='V00'                                                    715
         CALL AGSPUSH                                                     715
         AG_MODULE=MODULE                                                 715
C    *                                                                    715
C    Geant MEDIUM : TSWH_STANDARD ===> DEEMAX=0.248853 STMIN=1.15055 <==  716
C    =                                                                    716
         CALL AGSRESET                                                    716
         AG_DEEMAX = 0.248853                                             716
         AG_STMIN = 1.15055                                               716
         AG_TITLE = 'TSWH_STANDARD'                                       716
         AG_EXNAME = 'MEDIUM'                                             716
         AG_PARLIST = 'DEEMAX_STMIN'                                      716
         CALL AXMEDIUM                                                    716
C    *                                                                    716
C    Geant ATTRIBUTE : TSWH ===> SEEN=1 COLO=KVIOLET <===                 716
         CALL AGSRESET                                                    717
         AG_SEEN = 1                                                      717
         AG_COLO = KVIOLET                                                717
         AG_TITLE = 'TSWH'                                                717
         AG_EXNAME = 'ATTRIBUTE'                                          717
         AG_PARLIST = 'SEEN_COLO'                                         717
         CALL AXATTRIBUTE                                                 717
C    *                                                                    717
C    Geant SHAPE : PCON ===> PHI1=-15 DPHI=30 NZ=8 ZI={TPCG_MEMBTHK/2, Z  717
C    WHEEL1,ZWHEEL1,ZTOFCEND,ZTOFCEND, ZTIFCFLANGEBEGIN,ZTIFCFLANGEBEGIN  717
C    ,TPCG_DZENVELOP}, RMN={TIFCOR,TIFCOR,TIFCOR,TIFCOR,TIFCOR, TPCG_TIF  717
C    CRF+TPCG_TIFCDRT,TPCG_TIFCRF+TPCG_TIFCDRT, TPCG_TIFCRF+TPCG_TIFCDRT  717
C    } RMX={TOFCIR,TOFCIR, TPCG_WHEELOR1,TPCG_WHEELOR1,TPCG_WHEELOR, TPC  717
C    G_WHEELOR,TPCG_WHEELOR,TPCG_WHEELOR} <===                            717
         CALL AGSRESET                                                    725
         AG_PHI1 = -15                                                    725
         AG_DPHI = 30                                                     725
         AG_NZ = 8                                                        725
         AG_ZI(1)=TPCG_MEMBTHK/2                                          725
         AG_ZI(2)=ZWHEEL1                                                 725
         AG_ZI(3)=ZWHEEL1                                                 725
         AG_ZI(4)=ZTOFCEND                                                725
         AG_ZI(5)=ZTOFCEND                                                725
         AG_ZI(6)=ZTIFCFLANGEBEGIN                                        725
         AG_ZI(7)=ZTIFCFLANGEBEGIN                                        725
         AG_ZI(8)=TPCG_DZENVELOP                                          725
         AG_RMN(1)=TIFCOR                                                 725
         AG_RMN(2)=TIFCOR                                                 725
         AG_RMN(3)=TIFCOR                                                 725
         AG_RMN(4)=TIFCOR                                                 725
         AG_RMN(5)=TIFCOR                                                 725
         AG_RMN(6)=TPCG_TIFCRF+TPCG_TIFCDRT                               725
         AG_RMN(7)=TPCG_TIFCRF+TPCG_TIFCDRT                               725
         AG_RMN(8)=TPCG_TIFCRF+TPCG_TIFCDRT                               725
         AG_RMX(1)=TOFCIR                                                 725
         AG_RMX(2)=TOFCIR                                                 725
         AG_RMX(3)=TPCG_WHEELOR1                                          725
         AG_RMX(4)=TPCG_WHEELOR1                                          725
         AG_RMX(5)=TPCG_WHEELOR                                           725
         AG_RMX(6)=TPCG_WHEELOR                                           725
         AG_RMX(7)=TPCG_WHEELOR                                           725
         AG_RMX(8)=TPCG_WHEELOR                                           725
         AG_TITLE = 'PCON'                                                725
         AG_EXNAME = 'SHAPE'                                              725
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           725
         CALL AXSHAPE                                                     725
C    *                                                                    725
C    Create TSAW :                                                        726
C    Call To Local Procedure TSAW                                         726
         ASSIGN 5360 TO BLOCK_TSAW                                        726
         GO TO  5350                                                      726
5360     CONTINUE                                                         726
C    *                                                                    726
C    Geant POSITION : TSAW ===> KONLY='MANY' <===                         727
         CALL AGSRESET                                                    727
         AG_KONLY = 'MANY'                                                727
         AG_TITLE = 'TSAW'                                                727
         AG_EXNAME = 'POSITION'                                           727
         AG_PARLIST = 'KONLY'                                             727
         CALL AXPOSITION                                                  727
C    *                                                                    727
5141  CALL AGSPOPB                                                        728
      GO TO BLOCK_TSWH, (5140 ,5150,5160)                                 728
C End of Local Procedure                                                  728
C ----------------------------------------------------------------------  729
C ------------ TSAW                                                       729
C Begin Local Procedure TSAW                                              730
C TSAW TPCSECTORANDWHEEL                                                  730
5350  CONTINUE                                                            730
         AG_TITLE='TSAW'                                                  730
         AG_MODULE=MODULE                                                 730
         AG_MARK='V00'                                                    730
         CALL AGSPUSH                                                     730
         AG_MODULE=MODULE                                                 730
C    *                                                                    730
C    Geant MEDIUM : STANDARD ===> DEEMAX=0.248853 STMIN=1.15055 <===      731
         CALL AGSRESET                                                    731
         AG_DEEMAX = 0.248853                                             731
         AG_STMIN = 1.15055                                               731
         AG_TITLE = 'STANDARD'                                            731
         AG_EXNAME = 'MEDIUM'                                             731
         AG_PARLIST = 'DEEMAX_STMIN'                                      731
         CALL AXMEDIUM                                                    731
C    *                                                                    731
C    Geant ATTRIBUTE : TSAW ===> SEEN=1 COLO=KYELLOW <===                 731
         CALL AGSRESET                                                    732
         AG_SEEN = 1                                                      732
         AG_COLO = KYELLOW                                                732
         AG_TITLE = 'TSAW'                                                732
         AG_EXNAME = 'ATTRIBUTE'                                          732
         AG_PARLIST = 'SEEN_COLO'                                         732
         CALL AXATTRIBUTE                                                 732
C    *                                                                    732
C    Geant SHAPE : PCON ===> PHI1=-15 DPHI=30 NZ=2 ZI={204, TPCG_DZENVEL  732
C    OP-TPCG_MEMBTHK/2} RMN={48,48} RMX={208, 208} <===                   732
         CALL AGSRESET                                                    735
         AG_PHI1 = -15                                                    735
         AG_DPHI = 30                                                     735
         AG_NZ = 2                                                        735
         AG_ZI(1)=204                                                     735
         AG_ZI(2)=TPCG_DZENVELOP-TPCG_MEMBTHK/2                           735
         AG_RMN(1)=48                                                     735
         AG_RMN(2)=48                                                     735
         AG_RMX(1)=208                                                    735
         AG_RMX(2)=208                                                    735
         AG_TITLE = 'PCON'                                                735
         AG_EXNAME = 'SHAPE'                                              735
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           735
         CALL AXSHAPE                                                     735
C    *                                                                    735
         DX2 = 1.470                                                      735
         DX = 0                                                           737
C    use TECW bank                                                        738
         BPATH_TECW='TECW*'                                               738
         IDNUM_TECW(1)=0                                                  738
         TECW_SEC=1                                                       738
         CALL AGDGETP('TPCXGEO1','*',-999,'TECW',NTECW_SEC,RE_TECW_SEC,   738
     *   BPATH_TECW,IDNUM_TECW,1,MAP_TECW,NAMES_TECW,BANK_TECW,LEN_TECW,  738
     *   LLEN_TECW,LINK_TECW,FLAG_TECW)                                   738
C    *                                                                    739
         INOUT=KINNER                                                     739
            GO TO 5373                                                    739
5371        INOUT=INOUT+1                                                 739
5373        IF(INOUT-(KOUTER).GT.0) GO TO 5372                            739
            MYCOUNT = 0                                                   739
C       use COOL bank                                                     741
            BPATH_COOL='COOL*'                                            741
            IDNUM_COOL(1)=0                                               741
            COOL_INOUT=INOUT                                              741
            CALL AGDGETP('TPCXGEO1','*',-999,'COOL',NCOOL_INOUT,          741
     *      RE_COOL_INOUT,BPATH_COOL,IDNUM_COOL,1,MAP_COOL,NAMES_COOL,    741
     *      BANK_COOL,LEN_COOL,LLEN_COOL,LINK_COOL,FLAG_COOL)             741
C       use RIBS bank                                                     742
            BPATH_RIBS='RIBS*'                                            742
            IDNUM_RIBS(1)=0                                               742
            RIBS_INOUT=INOUT                                              742
            CALL AGDGETP('TPCXGEO1','*',-999,'RIBS',NRIBS_INOUT,          742
     *      RE_RIBS_INOUT,BPATH_RIBS,IDNUM_RIBS,1,MAP_RIBS,NAMES_RIBS,    742
     *      BANK_RIBS,LEN_RIBS,LLEN_RIBS,LINK_RIBS,FLAG_RIBS)             742
C       use HOLZ bank                                                     743
            BPATH_HOLZ='HOLZ*'                                            743
            IDNUM_HOLZ(1)=0                                               743
            HOLZ_INOUT=INOUT                                              743
            CALL AGDGETP('TPCXGEO1','*',-999,'HOLZ',NHOLZ_INOUT,          743
     *      RE_HOLZ_INOUT,BPATH_HOLZ,IDNUM_HOLZ,1,MAP_HOLZ,NAMES_HOLZ,    743
     *      BANK_HOLZ,LEN_HOLZ,LLEN_HOLZ,LINK_HOLZ,FLAG_HOLZ)             743
C       use TECW bank                                                     744
            BPATH_TECW='TECW*'                                            744
            IDNUM_TECW(1)=0                                               744
            TECW_SEC=(INOUT+1)                                            744
            CALL AGDGETP('TPCXGEO1','*',-999,'TECW',NTECW_SEC,            744
     *      RE_TECW_SEC,BPATH_TECW,IDNUM_TECW,1,MAP_TECW,NAMES_TECW,      744
     *      BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)             744
            Z2 = ZWHEEL1                                                  745
            Z1 = Z2 - TECW_THICK                                          746
            Z = (Z1 + TPCG_MEMBTHK/2)/2                                   747
            DZ = (Z1 - TPCG_MEMBTHK/2)/2                                  748
            X1 = -TECW_HEIGHT/2                                           749
            X2 = TECW_HEIGHT/2                                            750
            Y1 = TECW_INWIDTH/2                                           751
            Y2 = TECW_OUWIDTH/2                                           752
            X0 = (Y1*X2-Y2*X1)/(X2-X1)                                    753
            DYDX = (Y2 - Y1)/(X2 - X1)                                    754
            ALPHA = 180./PI*ATAN(DYDX)                                    755
            R1 = Y1/DYDX                                                  756
            R2 = Y2/DYDX                                                  757
            DRSECTORSHIFT = TECW_RMIN - R1                                758
C       Check INOUT==KINNER                                               759
            IF (INOUT.EQ.KINNER) THEN                                     759
            XHOLES(0) = R2 - 1.988*INCH                                   760
C       *                                                                 761
            I=1                                                           761
               GO TO 5383                                                 761
5381           I=I+1                                                      761
5383           IF(I-(HOLZ_NUMBEROFROWS-1).GT.0) GO TO 5382                761
               XHOLES(I) = XHOLES(I-1) - 1.575*INCH                       762
            GO TO 5381                                                    763
5382        CONTINUE                                                      763
            ELSE                                                          764
C       *                                                                 765
            I=0                                                           765
               GO TO 5393                                                 765
5391           I=I+1                                                      765
5393           IF(I-(HOLZ_NUMBEROFROWS - 1).GT.0) GO TO 5392              765
               XHOLES(I) = R2 - (1.988 + 1.575*I)*INCH                    766
            GO TO 5391                                                    767
5392        CONTINUE                                                      767
            ENDIF                                                         768
C       Create TSAS :                                                     769
C       Call To Local Procedure TSAS                                      769
            ASSIGN 5410 TO BLOCK_TSAS                                     769
            GO TO  5400                                                   769
5410        CONTINUE                                                      769
C       *                                                                 769
C       Geant POSITION : TSAS ===> Z=TSAS_ZOFFSET KONLY='MANY' <===       770
            CALL AGSRESET                                                 770
            AG_Z = TSAS_ZOFFSET                                           770
            AG_KONLY = 'MANY'                                             770
            AG_TITLE = 'TSAS'                                             770
            AG_EXNAME = 'POSITION'                                        770
            AG_PARLIST = 'Z_KONLY'                                        770
            CALL AXPOSITION                                               770
C       *                                                                 770
C       Create TWAS :                                                     771
C       Call To Local Procedure TWAS                                      771
            ASSIGN 5430 TO BLOCK_TWAS                                     771
            GO TO  5420                                                   771
5430        CONTINUE                                                      771
C       *                                                                 771
C       Geant POSITION : TWAS ===> KONLY='MANY' <===                      772
            CALL AGSRESET                                                 772
            AG_KONLY = 'MANY'                                             772
            AG_TITLE = 'TWAS'                                             772
            AG_EXNAME = 'POSITION'                                        772
            AG_PARLIST = 'KONLY'                                          772
            CALL AXPOSITION                                               772
C       *                                                                 772
C       Check INOUT==0                                                    772
            IF (INOUT.EQ.0) THEN                                          773
C       Create TBRW :                                                     774
C       Call To Local Procedure TBRW                                      774
            ASSIGN 5450 TO BLOCK_TBRW                                     774
            GO TO  5440                                                   774
5450        CONTINUE                                                      774
C       *                                                                 774
C       Geant POSITION : TBRW ===> X=TPCG_WHEELR1 Z=ZWHEEL1+TPCG_WHEELTH  775
C       K/2 <===                                                          775
            CALL AGSRESET                                                 775
            AG_X = TPCG_WHEELR1                                           775
            AG_Z = ZWHEEL1+TPCG_WHEELTHK/2                                775
            AG_TITLE = 'TBRW'                                             775
            AG_EXNAME = 'POSITION'                                        775
            AG_PARLIST = 'X_Z'                                            775
            CALL AXPOSITION                                               775
C       *                                                                 775
            ENDIF                                                         775
            X1 = TPCG_WHEELR0*COS15                                       777
            X2 = TPCG_WHEELR2                                             778
            DZ = TPCG_WHEELRIBHEIGHT/2                                    779
            Z = ZWHEEL1 + DZ                                              780
            DX = (X2 - X1)/2                                              781
            DY = TPCG_WHEELTOTALRIBWIDTH/4                                782
            X = (X1 + X2)/2                                               783
            Y = (X *TAN15 - DY)*(2*INOUT-1)                               784
            MYDX=DY                                                       784
            MYDY=DX                                                       784
            MYDZ=DZ                                                       784
            MYALPH=-(1-2*INOUT)*15                                        785
C       Create TWRB :                                                     786
C       Call To Local Procedure TWRB                                      786
            ASSIGN 5470 TO BLOCK_TWRB                                     786
            GO TO  5460                                                   786
5470        CONTINUE                                                      786
C       *                                                                 786
C       Geant POSITION : TWRB ===> X=X Y=Y Z=Z ORT=YX-Z <===              787
            CALL AGSRESET                                                 787
            AG_X = X                                                      787
            AG_Y = Y                                                      787
            AG_Z = Z                                                      787
            AG_ALPHAN=AG_ALPHAN+1                                         787
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             787
            AG_TITLE = 'TWRB'                                             787
            AG_EXNAME = 'POSITION'                                        787
            AG_PARLIST = 'X_Y_Z_ORT'                                      787
            CALL AXPOSITION                                               787
C       *                                                                 787
            DZ = TPCG_WHEELTHK/2                                          787
            Z = ZWHEEL1 + DZ                                              789
            DY = TPCG_WHEELRIBWIDTH/4                                     790
            X = (X1 + X2)/2                                               791
            Y = (X *TAN15 - DY)*(2*INOUT-1)                               792
            MYDX=DY                                                       792
            MYDY=DX                                                       792
            MYDZ=DZ                                                       792
            MYALPH=-(1-2*INOUT)*15                                        793
C       Create TWRB :                                                     794
C       Call To Local Procedure TWRB                                      794
            ASSIGN 5480 TO BLOCK_TWRB                                     794
            GO TO  5460                                                   794
5480        CONTINUE                                                      794
C       *                                                                 794
C       Geant POSITION : TWRB ===> X=X Y=Y Z=Z ORT=YX-Z <===              795
            CALL AGSRESET                                                 795
            AG_X = X                                                      795
            AG_Y = Y                                                      795
            AG_Z = Z                                                      795
            AG_ALPHAN=AG_ALPHAN+1                                         795
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             795
            AG_TITLE = 'TWRB'                                             795
            AG_EXNAME = 'POSITION'                                        795
            AG_PARLIST = 'X_Y_Z_ORT'                                      795
            CALL AXPOSITION                                               795
C       *                                                                 795
         GO TO 5371                                                       796
5372     CONTINUE                                                         796
C    *                                                                    797
         ALPHA=-15                                                        797
            GOTO 5493                                                     797
5491        ALPHA=ALPHA+(30)                                              797
5493        IF((30)*(ALPHA-(15)).GT.0) GO TO 5492                         797
C       *                                                                 798
            J=0                                                           798
               GO TO 5503                                                 798
5501           J=J+1                                                      798
5503           IF(J-(3).GT.0) GO TO 5502                                  798
C          Check (J==0)                                                   799
               IF ((J.EQ.0)) THEN                                         799
               R = TPCG_WHEELR2 - 1.5/8.5*(TPCG_WHEELR2 - TPCG_WHEELR1)   800
               ENDIF                                                      801
C          Check (J==1)                                                   802
               IF ((J.EQ.1)) THEN                                         802
               R = TPCG_WHEELR2 - 6.5/8.5*(TPCG_WHEELR2 - TPCG_WHEELR1)   803
               ENDIF                                                      804
C          Check (J==2)                                                   805
               IF ((J.EQ.2)) THEN                                         805
               R = TPCG_WHEELR1 - 2.0/8.5*(TPCG_WHEELR1 - TPCG_WHEELR0)   806
               ENDIF                                                      807
C          Check (J==3)                                                   808
               IF ((J.EQ.3)) THEN                                         808
               R = TPCG_WHEELR1 - 7.0/8.5*(TPCG_WHEELR1 - TPCG_WHEELR0)   809
               ENDIF                                                      810
               X = R                                                      811
               Y = R *TAND(ALPHA)                                         812
C          Geant POSITION : TBRW ===> X=X Y=Y Z=ZWHEEL1+TPCG_WHEELTHK/2   813
C          ALPHAZ=ALPHA <===                                              813
               CALL AGSRESET                                              813
               AG_X = X                                                   813
               AG_Y = Y                                                   813
               AG_Z = ZWHEEL1+TPCG_WHEELTHK/2                             813
               AG_ALPHAN=AG_ALPHAN+1                                      813
               AG_ALPHAK(AG_ALPHAN)=3                                     813
               AG_ALPHAL(AG_ALPHAN)=ALPHA                                 813
               AG_TITLE = 'TBRW'                                          813
               AG_EXNAME = 'POSITION'                                     813
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                813
               CALL AXPOSITION                                            813
C          *                                                              813
               MYCOUNT=MYCOUNT+1                                          813
            GO TO 5501                                                    815
5502        CONTINUE                                                      815
            QWE(1) = -QWE(1)                                              816
            QWE(2) = -QWE(2)                                              817
         GO TO 5491                                                       818
5492     CONTINUE                                                         818
C    Create TWBT :                                                        819
C    Call To Local Procedure TWBT                                         819
         ASSIGN 5520 TO BLOCK_TWBT                                        819
         GO TO  5510                                                      819
5520     CONTINUE                                                         819
C    *                                                                    819
C    Geant POSITION : TWBT ===> inherited arguments used <===             820
         CALL AGSRESET                                                    820
         AG_TITLE = 'TWBT'                                                820
         AG_EXNAME = 'POSITION'                                           820
         AG_PARLIST = 'NONE'                                              820
         CALL AXPOSITION                                                  820
C    *                                                                    820
C    Create TWRI :                                                        821
C    Call To Local Procedure TWRI                                         821
         ASSIGN 5540 TO BLOCK_TWRI                                        821
         GO TO  5530                                                      821
5540     CONTINUE                                                         821
C    *                                                                    821
C    Geant POSITION : TWRI ===> KONLY='MANY' <===                         822
         CALL AGSRESET                                                    822
         AG_KONLY = 'MANY'                                                822
         AG_TITLE = 'TWRI'                                                822
         AG_EXNAME = 'POSITION'                                           822
         AG_PARLIST = 'KONLY'                                             822
         CALL AXPOSITION                                                  822
C    *                                                                    822
C    Create TWTR :                                                        823
C    Call To Local Procedure TWTR                                         823
         ASSIGN 5560 TO BLOCK_TWTR                                        823
         GO TO  5550                                                      823
5560     CONTINUE                                                         823
C    *                                                                    823
C    Geant POSITION : TWTR ===> inherited arguments used <===             824
         CALL AGSRESET                                                    824
         AG_TITLE = 'TWTR'                                                824
         AG_EXNAME = 'POSITION'                                           824
         AG_PARLIST = 'NONE'                                              824
         CALL AXPOSITION                                                  824
C    *                                                                    824
C    Create TWMR :                                                        825
C    Call To Local Procedure TWMR                                         825
         ASSIGN 5580 TO BLOCK_TWMR                                        825
         GO TO  5570                                                      825
5580     CONTINUE                                                         825
C    *                                                                    825
C    Geant POSITION : TWMR ===> inherited arguments used <===             826
         CALL AGSRESET                                                    826
         AG_TITLE = 'TWMR'                                                826
         AG_EXNAME = 'POSITION'                                           826
         AG_PARLIST = 'NONE'                                              826
         CALL AXPOSITION                                                  826
C    *                                                                    826
C    Create TRDO :                                                        827
C    Call To Local Procedure TRDO                                         827
         ASSIGN 5600 TO BLOCK_TRDO                                        827
         GO TO  5590                                                      827
5600     CONTINUE                                                         827
C    *                                                                    827
C    Geant POSITION : TRDO ===> KONLY='MANY' <===                         828
         CALL AGSRESET                                                    828
         AG_KONLY = 'MANY'                                                828
         AG_TITLE = 'TRDO'                                                828
         AG_EXNAME = 'POSITION'                                           828
         AG_PARLIST = 'KONLY'                                             828
         CALL AXPOSITION                                                  828
C    *                                                                    828
         RCOOLINGTUBE = TPCG_WHEELR2 - TPCG_HEIGTUBE - 2*TPCG_DXRDO       828
C    *                                                                    830
         ICOO=0                                                           830
            GO TO 5613                                                    830
5611        ICOO=ICOO+1                                                   830
5613        IF(ICOO-(7).GT.0) GO TO 5612                                  830
            DZ = TPCG_HEIGTUBE*CM/2                                       831
            DY = TPCG_WIDTUBE *CM/2                                       832
            Z = RCOOLINGTUBE                                              833
            DX1 = (Z-DZ)*TAN15 - 2*TPCG_RDOCOOLINGDY                      834
            DX2 = (Z+DZ)*TAN15 - 2*TPCG_RDOCOOLINGDY                      835
            ISHAPE = KTRD1                                                835
            MYPAR(1)=DX1                                                  837
            MYPAR(2)=DX2                                                  837
            MYPAR(3)=DY                                                   837
            MYPAR(4)=DZ                                                   837
C       Create TCOO :                                                     838
C       Call To Local Procedure TCOO                                      838
            ASSIGN 5630 TO BLOCK_TCOO                                     838
            GO TO  5620                                                   838
5630        CONTINUE                                                      838
C       *                                                                 838
C       Geant POSITION : TCOO ===> X=Z Z=(TPCG_ZRDO-2*DY) ORT=YZX <===    839
            CALL AGSRESET                                                 839
            AG_X = Z                                                      839
            AG_Z = (TPCG_ZRDO-2*DY)                                       839
            AG_ALPHAN=AG_ALPHAN+1                                         839
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              839
            AG_TITLE = 'TCOO'                                             839
            AG_EXNAME = 'POSITION'                                        839
            AG_PARLIST = 'X_Z_ORT'                                        839
            CALL AXPOSITION                                               839
C       *                                                                 839
            RCOOLINGTUBE=RCOOLINGTUBE-DRDOCOOLING(ICOO)                   839
         GO TO 5611                                                       841
5612     CONTINUE                                                         841
         DZ = 4.5                                                         842
         DY = 4.5/2                                                       843
         Z = TPCG_ZRDO + TPCG_DZRDO + DZ                                  844
         X1 = TPCG_WHEELR0                                                845
         X2 = TPCG_WHEELR2                                                846
         DX =(X2 - X1)/2                                                  847
         X =(X1 + X2)/2                                                   848
C    *                                                                    849
         ICAB=0                                                           849
            GO TO 5643                                                    849
5641        ICAB=ICAB+1                                                   849
5643        IF(ICAB-(1).GT.0) GO TO 5642                                  849
            Y = - (X *TAN15 - DY) * (1 - 2*ICAB)                          850
            MYPAR(1)=DY                                                   851
            MYPAR(2)=DX                                                   851
            MYPAR(3)=DZ                                                   851
            MYPAR(4)=-15*(1-2*ICAB)                                       851
            MYPAR(5)=0                                                    851
            MYPAR(6)=0                                                    851
C       Create TCAB :                                                     852
C       Call To Local Procedure TCAB                                      852
            ASSIGN 5660 TO BLOCK_TCAB                                     852
            GO TO  5650                                                   852
5660        CONTINUE                                                      852
C       *                                                                 852
C       Geant POSITION : TCAB ===> X=X Y=Y Z=Z ORT=YX-Z <===              853
            CALL AGSRESET                                                 853
            AG_X = X                                                      853
            AG_Y = Y                                                      853
            AG_Z = Z                                                      853
            AG_ALPHAN=AG_ALPHAN+1                                         853
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             853
            AG_TITLE = 'TCAB'                                             853
            AG_EXNAME = 'POSITION'                                        853
            AG_PARLIST = 'X_Y_Z_ORT'                                      853
            CALL AXPOSITION                                               853
C       *                                                                 853
         GO TO 5641                                                       854
5642     CONTINUE                                                         854
5351  CALL AGSPOPB                                                        855
      GO TO BLOCK_TSAW, (5350 ,5360)                                      855
C End of Local Procedure                                                  855
C ----------------------------------------------------------------------  856
C ------------ TSAS                                                       856
C Begin Local Procedure TSAS                                              857
C TSAS TPCINNERSECTORASSEMBLY & TPCOUTERSECTORASSEMBLY TSAS AND TSA1      857
5400  CONTINUE                                                            857
         AG_TITLE='TSAS'                                                  857
         AG_MODULE=MODULE                                                 857
         AG_MARK='V00'                                                    857
         CALL AGSPUSH                                                     857
         AG_MODULE=MODULE                                                 857
C    *                                                                    857
C    Geant MEDIUM : TSAS_STANDARD ===> DEEMAX=0.248853 STMIN=1.15055 <==  858
C    =                                                                    858
         CALL AGSRESET                                                    858
         AG_DEEMAX = 0.248853                                             858
         AG_STMIN = 1.15055                                               858
         AG_TITLE = 'TSAS_STANDARD'                                       858
         AG_EXNAME = 'MEDIUM'                                             858
         AG_PARLIST = 'DEEMAX_STMIN'                                      858
         CALL AXMEDIUM                                                    858
C    *                                                                    858
C    Geant SHAPE : PGON ===> PHI1=-15 DPHI=30 NPDIV=1 NZ=2, ZI={ZZZSA(1)  858
C    ,ZZZSA(2)} RMN={RMNSA(INOUT),RMNSA(INOUT)}, RMX={RMXSA(INOUT),RMXSA  858
C    (INOUT)} <===                                                        858
         CALL AGSRESET                                                    861
         AG_PHI1 = -15                                                    861
         AG_DPHI = 30                                                     861
         AG_NPDIV = 1                                                     861
         AG_NZ = 2                                                        861
         AG_ZI(1)=ZZZSA(1)                                                861
         AG_ZI(2)=ZZZSA(2)                                                861
         AG_RMN(1)=RMNSA(INOUT)                                           861
         AG_RMN(2)=RMNSA(INOUT)                                           861
         AG_RMX(1)=RMXSA(INOUT)                                           861
         AG_RMX(2)=RMXSA(INOUT)                                           861
         AG_TITLE = 'PGON'                                                861
         AG_EXNAME = 'SHAPE'                                              861
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     861
         CALL AXSHAPE                                                     861
C    *                                                                    861
C    Geant ATTRIBUTE : TSAS ===> SEEN=0 <===                              861
         CALL AGSRESET                                                    862
         AG_SEEN = 0                                                      862
         AG_TITLE = 'TSAS'                                                862
         AG_EXNAME = 'ATTRIBUTE'                                          862
         AG_PARLIST = 'SEEN'                                              862
         CALL AXATTRIBUTE                                                 862
C    *                                                                    862
C    Create TALS :                                                        863
C    Call To Local Procedure TALS                                         863
         ASSIGN 5680 TO BLOCK_TALS                                        863
         GO TO  5670                                                      863
5680     CONTINUE                                                         863
C    *                                                                    863
C    Geant POSITION : TALS ===> inherited arguments used <===             864
         CALL AGSRESET                                                    864
         AG_TITLE = 'TALS'                                                864
         AG_EXNAME = 'POSITION'                                           864
         AG_PARLIST = 'NONE'                                              864
         CALL AXPOSITION                                                  864
C    *                                                                    864
C    Create TSGT :                                                        865
C    Call To Local Procedure TSGT                                         865
         ASSIGN 5700 TO BLOCK_TSGT                                        865
         GO TO  5690                                                      865
5700     CONTINUE                                                         865
C    *                                                                    865
C    Geant POSITION : TSGT ===> Z=TSGT_ZOFFSET <===                       866
         CALL AGSRESET                                                    866
         AG_Z = TSGT_ZOFFSET                                              866
         AG_TITLE = 'TSGT'                                                866
         AG_EXNAME = 'POSITION'                                           866
         AG_PARLIST = 'Z'                                                 866
         CALL AXPOSITION                                                  866
C    *                                                                    866
         DY =(R2 - R1)/2                                                  866
         Y =(R2 + R1)/2                                                   868
         DX = TECW_WIDTHRIB/2                                             869
         DZ =(TECW_THICK - TPCG_PADPLANETHICKNESS - TECW_THICKAL)/2       870
         Z = Z2 - DZ                                                      871
         DXW = TPCG_WIREMOUNTWIDTH/COS15/2                                872
         DZW = TPCG_WIREMOUNTHEIGHT/2                                     873
         XW = 0                                                           874
C    *                                                                    875
         IRIB=0                                                           875
            GO TO 5713                                                    875
5711        IRIB=IRIB+1                                                   875
5713        IF(IRIB-(1).GT.0) GO TO 5712                                  875
            ISHAPE = KPARA                                                875
            MYPAR(1)=DX                                                   877
            MYPAR(2)=DY                                                   877
            MYPAR(3)=DZ                                                   877
            MYPAR(4)=-(1-2*IRIB)*ALPHA                                    877
C       Create TRIB :                                                     878
C       Call To Local Procedure TRIB                                      878
            ASSIGN 5730 TO BLOCK_TRIB                                     878
            GO TO  5720                                                   878
5730        CONTINUE                                                      878
C       *                                                                 878
            X = ((Y1 + Y2)/2 - DX)*(1-2*IRIB)                             879
C       Geant POSITION : TRIB ===> X=Y Y=-X Z=Z ORT=YX-Z <===             880
            CALL AGSRESET                                                 880
            AG_X = Y                                                      880
            AG_Y = -X                                                     880
            AG_Z = Z                                                      880
            AG_ALPHAN=AG_ALPHAN+1                                         880
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             880
            AG_TITLE = 'TRIB'                                             880
            AG_EXNAME = 'POSITION'                                        880
            AG_PARLIST = 'X_Y_Z_ORT'                                      880
            CALL AXPOSITION                                               880
C       *                                                                 880
            XW = ((Y1 + Y2)/2 - DXW)*(1-2*IRIB)                           880
            MYPAR(1)=DXW                                                  882
            MYPAR(2)=DY                                                   882
            MYPAR(3)=DZW                                                  882
            MYPAR(4)=-(1-2*IRIB)*ALPHA                                    882
            MYPAR(5)=0                                                    882
            MYPAR(6)=0                                                    882
C       Create TWIR :                                                     883
C       Call To Local Procedure TWIR                                      883
            ASSIGN 5750 TO BLOCK_TWIR                                     883
            GO TO  5740                                                   883
5750        CONTINUE                                                      883
C       *                                                                 883
            ZW = Z1 - DZW/2                                               884
C       Geant POSITION : TWIR ===> X=Y Y=-XW Z=ZW ORT=YX-Z <===           885
            CALL AGSRESET                                                 885
            AG_X = Y                                                      885
            AG_Y = -XW                                                    885
            AG_Z = ZW                                                     885
            AG_ALPHAN=AG_ALPHAN+1                                         885
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             885
            AG_TITLE = 'TWIR'                                             885
            AG_EXNAME = 'POSITION'                                        885
            AG_PARLIST = 'X_Y_Z_ORT'                                      885
            CALL AXPOSITION                                               885
C       *                                                                 885
         GO TO 5711                                                       886
5712     CONTINUE                                                         886
C    *                                                                    887
         IRIB=0                                                           887
            GO TO 5763                                                    887
5761        IRIB=IRIB+1                                                   887
5763        IF(IRIB-(TECW_NORIBS-1).GT.0) GO TO 5762                      887
            R = R2 - RIBS_X (IRIB+1) * INCH                               888
            Y = RIBS_Y (IRIB+1) * INCH                                    889
            DR = RIBS_DX (IRIB+1) * INCH                                  890
            DY = RIBS_DY (IRIB+1) * INCH                                  891
C       Check DY<1.E-7                                                    892
            IF (DY.LT.1.E-7) THEN                                         892
            DX2 = R *DYDX - TECW_WIDTHRIB - 2*DXW                         892
C       TECW_WIDTHLIP                                                     893
            DX1 = (R - DR)*DYDX - TECW_WIDTHRIB - 2*DXW                   893
C       TECW_WIDTHLIP                                                     894
            XC = R - TECW_ZSTEPRIB/2                                      895
            ISHAPE = KTRD1                                                895
            MYPAR(1)=DX1                                                  897
            MYPAR(2)=DX2                                                  897
            MYPAR(3)=DZ                                                   897
            MYPAR(4)=DR/2                                                 897
C       Create TRIB :                                                     898
C       Call To Local Procedure TRIB                                      898
            ASSIGN 5770 TO BLOCK_TRIB                                     898
            GO TO  5720                                                   898
5770        CONTINUE                                                      898
C       *                                                                 898
C       Geant POSITION : TRIB ===> X=R-DR/2 Y=0 Z=Z2-DZ ORT=YZX <===      899
            CALL AGSRESET                                                 899
            AG_X = R-DR/2                                                 899
            AG_Y = 0                                                      899
            AG_Z = Z2-DZ                                                  899
            AG_ALPHAN=AG_ALPHAN+1                                         899
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              899
            AG_TITLE = 'TRIB'                                             899
            AG_EXNAME = 'POSITION'                                        899
            AG_PARLIST = 'X_Y_Z_ORT'                                      899
            CALL AXPOSITION                                               899
C       *                                                                 899
            ELSE                                                          899
            ISHAPE = KBBOX                                                900
            MYPAR(1)=DR/2                                                 902
            MYPAR(2)=DY/2                                                 902
            MYPAR(3)=DZ                                                   902
C       Create TRIB :                                                     903
C       Call To Local Procedure TRIB                                      903
            ASSIGN 5780 TO BLOCK_TRIB                                     903
            GO TO  5720                                                   903
5780        CONTINUE                                                      903
C       *                                                                 903
C       Geant POSITION : TRIB ===> X=R Y=Y Z=Z2-DZ <===                   904
            CALL AGSRESET                                                 904
            AG_X = R                                                      904
            AG_Y = Y                                                      904
            AG_Z = Z2-DZ                                                  904
            AG_TITLE = 'TRIB'                                             904
            AG_EXNAME = 'POSITION'                                        904
            AG_PARLIST = 'X_Y_Z'                                          904
            CALL AXPOSITION                                               904
C       *                                                                 904
            ENDIF                                                         904
         GO TO 5761                                                       906
5762     CONTINUE                                                         906
5401  CALL AGSPOPB                                                        907
      GO TO BLOCK_TSAS, (5400 ,5410)                                      907
C End of Local Procedure                                                  907
C ----------------------------------------------------------------------  908
C ------------ TWAS                                                       908
C Begin Local Procedure TWAS                                              909
C TWAS TPCWHEELINNERASSEMBLY & TPCWHEELOUTERASSEMBLY TWAS AND TWA1        909
5420  CONTINUE                                                            909
         AG_TITLE='TWAS'                                                  909
         AG_MODULE=MODULE                                                 909
         AG_MARK='V00'                                                    909
         CALL AGSPUSH                                                     909
         AG_MODULE=MODULE                                                 909
C    *                                                                    909
C    Geant MEDIUM : TWAS_STANDARD ===> DEEMAX=0.248853 STMIN=1.15055 <==  910
C    =                                                                    910
         CALL AGSRESET                                                    910
         AG_DEEMAX = 0.248853                                             910
         AG_STMIN = 1.15055                                               910
         AG_TITLE = 'TWAS_STANDARD'                                       910
         AG_EXNAME = 'MEDIUM'                                             910
         AG_PARLIST = 'DEEMAX_STMIN'                                      910
         CALL AXMEDIUM                                                    910
C    *                                                                    910
C    Geant SHAPE : PGON ===> PHI1=-15 DPHI=30 NPDIV=1 NZ=2, ZI={ZZZWA(1)  910
C    ,ZZZWA(2)} RMN={RMNWA(INOUT),RMNWA(INOUT)}, RMX={RMXWA(INOUT),RMXWA  910
C    (INOUT)} <===                                                        910
         CALL AGSRESET                                                    913
         AG_PHI1 = -15                                                    913
         AG_DPHI = 30                                                     913
         AG_NPDIV = 1                                                     913
         AG_NZ = 2                                                        913
         AG_ZI(1)=ZZZWA(1)                                                913
         AG_ZI(2)=ZZZWA(2)                                                913
         AG_RMN(1)=RMNWA(INOUT)                                           913
         AG_RMN(2)=RMNWA(INOUT)                                           913
         AG_RMX(1)=RMXWA(INOUT)                                           913
         AG_RMX(2)=RMXWA(INOUT)                                           913
         AG_TITLE = 'PGON'                                                913
         AG_EXNAME = 'SHAPE'                                              913
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     913
         CALL AXSHAPE                                                     913
C    *                                                                    913
C    Geant ATTRIBUTE : TWAS ===> SEEN=0 <===                              913
         CALL AGSRESET                                                    914
         AG_SEEN = 0                                                      914
         AG_TITLE = 'TWAS'                                                914
         AG_EXNAME = 'ATTRIBUTE'                                          914
         AG_PARLIST = 'SEEN'                                              914
         CALL AXATTRIBUTE                                                 914
C    *                                                                    914
         DX = TPCG_WIDTUBE*CM/2                                           914
         DZ = DEPTHIOF(INOUT + 1)/2                                       916
         DY = HEIGHTIOF(INOUT + 1)/2                                      917
         Y1 = - DY                                                        918
         Y2 = DY                                                          919
         X1 = WIDTH1IOF(INOUT + 1)/2 - DX                                 920
         X2 = WIDTH2IOF(INOUT + 1)/2 - DX                                 921
         X0 =(Y1*X2 - Y2*X1)/(Y2 - Y1)                                    922
         DYDX =(X2 - X1)/(Y2 - Y1)                                        923
         BETA = 180./PI*ATAN(DYDX)                                        924
C    Check (INOUT==KOUTER)                                                925
         IF ((INOUT.EQ.KOUTER)) THEN                                      925
         DY=DY-0.7                                                        926
         ENDIF                                                            927
         Y = XHOLES(0) - DY                                               928
         X =(X1 + X2)/2                                                   929
C    *                                                                    930
         ICOO=0                                                           930
            GO TO 5793                                                    930
5791        ICOO=ICOO+1                                                   930
5793        IF(ICOO-(1).GT.0) GO TO 5792                                  930
            MYPAR(1)=DX                                                   931
            MYPAR(2)=DY                                                   931
            MYPAR(3)=DZ                                                   931
            MYPAR(4)=-(1-2*ICOO)*BETA                                     931
            MYPAR(5)=0                                                    931
            MYPAR(6)=0                                                    931
            ISHAPE = KPARA                                                931
C       Create TCOO :                                                     933
C       Call To Local Procedure TCOO                                      933
            ASSIGN 5800 TO BLOCK_TCOO                                     933
            GO TO  5620                                                   933
5800        CONTINUE                                                      933
C       *                                                                 933
C       Geant POSITION : TCOO ===> X=Y Y=X*(2*ICOO-1) Z=ZWHEEL1+DZ ORT=Y  934
C       X-Z <===                                                          934
            CALL AGSRESET                                                 934
            AG_X = Y                                                      934
            AG_Y = X*(2*ICOO-1)                                           934
            AG_Z = ZWHEEL1+DZ                                             934
            AG_ALPHAN=AG_ALPHAN+1                                         934
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             934
            AG_TITLE = 'TCOO'                                             934
            AG_EXNAME = 'POSITION'                                        934
            AG_PARLIST = 'X_Y_Z_ORT'                                      934
            CALL AXPOSITION                                               934
C       *                                                                 934
         GO TO 5791                                                       935
5792     CONTINUE                                                         935
C    *                                                                    936
         ICOO=0                                                           936
            GO TO 5813                                                    936
5811        ICOO=ICOO+1                                                   936
5813        IF(ICOO-(HOLZ_NUMBEROFROWS - 1).GT.0) GO TO 5812              936
            X = XHOLES(ICOO)                                              936
C       // CM                                                             937
            DX = TPCG_HEIGTUBE*CM/2                                       938
            DZ = TPCG_WIDTUBE*CM/2                                        939
            DY = COOL_TUBELENGTH( ICOO+1 )*INCH / 2                       940
C       Check INOUT==KINNER                                               941
            IF (INOUT.EQ.KINNER) THEN                                     941
            DY2 = DY + 2*DYDX*DX                                          942
            DY1 = DY                                                      942
            ELSE                                                          944
            DY2 = DY                                                      945
            DY1 = DY - 2*DYDX*DX                                          946
            ENDIF                                                         947
            ISHAPE = KTRD1                                                947
            MYPAR(1)=DY1                                                  949
            MYPAR(2)=DY2                                                  949
            MYPAR(3)=DZ                                                   949
            MYPAR(4)=DX                                                   949
            MYPAR(11) = X-DX-TFEE_ASSEMBLYTHICKNESS/2+DRSECTORSHIFT       950
            MYPAR(13) = ZWHEEL1+DZ                                        951
C       Create TCOO :                                                     952
C       Call To Local Procedure TCOO                                      952
            ASSIGN 5820 TO BLOCK_TCOO                                     952
            GO TO  5620                                                   952
5820        CONTINUE                                                      952
C       *                                                                 952
C       Geant POSITION : TCOO ===> X=MYPAR(11) Z=MYPAR(13) ORT=YZX <===   953
            CALL AGSRESET                                                 953
            AG_X = MYPAR(11)                                              953
            AG_Z = MYPAR(13)                                              953
            AG_ALPHAN=AG_ALPHAN+1                                         953
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              953
            AG_TITLE = 'TCOO'                                             953
            AG_EXNAME = 'POSITION'                                        953
            AG_PARLIST = 'X_Z_ORT'                                        953
            CALL AXPOSITION                                               953
C       *                                                                 953
C       *                                                                 953
            JCOO=0                                                        954
               GO TO 5833                                                 954
5831           JCOO=JCOO+1                                                954
5833           IF(JCOO-(HOLZ_NUMBERPERROW(ICOO+1) - 1).GT.0) GO TO 5832   954
               Y = HOLZ_YHOLES( 9*ICOO + JCOO + 1 )*INCH                  955
C          Create FEEA :                                                  956
C          Call To Local Procedure FEEA                                   956
               ASSIGN 5850 TO BLOCK_FEEA                                  956
               GO TO  5840                                                956
5850           CONTINUE                                                   956
C          *                                                              956
C          Geant POSITION : FEEA ===> X=X+TFEE_ASSEMBLYTHICKNESS/2+DRSEC  958
C          TORSHIFT Y=Y Z=ZWHEEL1+2*DZ+TFEE_RIBDZ KONLY='MANY' <===       958
               CALL AGSRESET                                              958
               AG_X = X+TFEE_ASSEMBLYTHICKNESS/2+DRSECTORSHIFT            958
               AG_Y = Y                                                   958
               AG_Z = ZWHEEL1+2*DZ+TFEE_RIBDZ                             958
               AG_KONLY = 'MANY'                                          958
               AG_TITLE = 'FEEA'                                          958
               AG_EXNAME = 'POSITION'                                     958
               AG_PARLIST = 'X_Y_Z_KONLY'                                 958
               CALL AXPOSITION                                            958
C          *                                                              958
            GO TO 5831                                                    959
5832        CONTINUE                                                      959
         GO TO 5811                                                       960
5812     CONTINUE                                                         960
5421  CALL AGSPOPB                                                        961
      GO TO BLOCK_TWAS, (5420 ,5430)                                      961
C End of Local Procedure                                                  961
C ----------------------------------------------------------------------  962
C ------------ FEEA                                                       962
C Begin Local Procedure FEEA                                              963
C FEEA TGEOVOLUMEASSEMBLY(FEE)                                            963
5840  CONTINUE                                                            963
         AG_TITLE='FEEA'                                                  963
         AG_MODULE=MODULE                                                 963
         AG_MARK='V00'                                                    963
         CALL AGSPUSH                                                     963
         AG_MODULE=MODULE                                                 963
C    *                                                                    963
C    Geant MEDIUM : FEEA_STD ===> DEEMAX=0.248853 STMIN=1.15055 <===      964
         CALL AGSRESET                                                    964
         AG_DEEMAX = 0.248853                                             964
         AG_STMIN = 1.15055                                               964
         AG_TITLE = 'FEEA_STD'                                            964
         AG_EXNAME = 'MEDIUM'                                             964
         AG_PARLIST = 'DEEMAX_STMIN'                                      964
         CALL AXMEDIUM                                                    964
C    *                                                                    964
C    Geant ATTRIBUTE : FEEA ===> SEEN=1 COLO=KGREEN <===                  964
         CALL AGSRESET                                                    965
         AG_SEEN = 1                                                      965
         AG_COLO = KGREEN                                                 965
         AG_TITLE = 'FEEA'                                                965
         AG_EXNAME = 'ATTRIBUTE'                                          965
         AG_PARLIST = 'SEEN_COLO'                                         965
         CALL AXATTRIBUTE                                                 965
C    *                                                                    965
C    Geant SHAPE : BOX ===> DX=TFEE_ASS(1) DY=TFEE_ASS(2), DZ=TFEE_ASS(3  965
C    ) <===                                                               965
         CALL AGSRESET                                                    967
         AG_DX = TFEE_ASS(1)                                              967
         AG_DY = TFEE_ASS(2)                                              967
         AG_DZ = TFEE_ASS(3)                                              967
         AG_TITLE = 'BOX'                                                 967
         AG_EXNAME = 'SHAPE'                                              967
         AG_PARLIST = 'DX_DY_DZ'                                          967
         CALL AXSHAPE                                                     967
C    *                                                                    967
C    Geant ATTRIBUTE : FEEA ===> SEEN=1 COLO=42 <===                      967
         CALL AGSRESET                                                    968
         AG_SEEN = 1                                                      968
         AG_COLO = 42                                                     968
         AG_TITLE = 'FEEA'                                                968
         AG_EXNAME = 'ATTRIBUTE'                                          968
         AG_PARLIST = 'SEEN_COLO'                                         968
         CALL AXATTRIBUTE                                                 968
C    *                                                                    968
C    Create FEEP :                                                        969
C    Call To Local Procedure FEEP                                         969
         ASSIGN 5870 TO BLOCK_FEEP                                        969
         GO TO  5860                                                      969
5870     CONTINUE                                                         969
C    *                                                                    969
C    Geant POSITION : FEEP ===> inherited arguments used <===             970
         CALL AGSRESET                                                    970
         AG_TITLE = 'FEEP'                                                970
         AG_EXNAME = 'POSITION'                                           970
         AG_PARLIST = 'NONE'                                              970
         CALL AXPOSITION                                                  970
C    *                                                                    970
C    Create FEER :                                                        971
C    Call To Local Procedure FEER                                         971
         ASSIGN 5890 TO BLOCK_FEER                                        971
         GO TO  5880                                                      971
5890     CONTINUE                                                         971
C    *                                                                    971
C    Geant POSITION : FEER ===> X=-TFEE_RIBDX-TFEE_CARDDX <===            972
         CALL AGSRESET                                                    972
         AG_X = -TFEE_RIBDX-TFEE_CARDDX                                   972
         AG_TITLE = 'FEER'                                                972
         AG_EXNAME = 'POSITION'                                           972
         AG_PARLIST = 'X'                                                 972
         CALL AXPOSITION                                                  972
C    *                                                                    972
C    Create FEEI :                                                        973
C    Call To Local Procedure FEEI                                         973
         ASSIGN 5910 TO BLOCK_FEEI                                        973
         GO TO  5900                                                      973
5910     CONTINUE                                                         973
C    *                                                                    973
C    Geant POSITION : FEEI ===> X=2*TFEE_CARDDX Z=0.5 <===                974
         CALL AGSRESET                                                    974
         AG_X = 2*TFEE_CARDDX                                             974
         AG_Z = 0.5                                                       974
         AG_TITLE = 'FEEI'                                                974
         AG_EXNAME = 'POSITION'                                           974
         AG_PARLIST = 'X_Z'                                               974
         CALL AXPOSITION                                                  974
C    *                                                                    974
5841  CALL AGSPOPB                                                        975
      GO TO BLOCK_FEEA, (5840 ,5850)                                      975
C End of Local Procedure                                                  975
C ----------------------------------------------------------------------  976
C ------------ FEEP                                                       976
C Begin Local Procedure FEEP                                              977
C FEEP FEEPLATE                                                           977
5860  CONTINUE                                                            977
         AG_TITLE='FEEP'                                                  977
         AG_MODULE=MODULE                                                 977
         AG_MARK='V00'                                                    977
         CALL AGSPUSH                                                     977
         AG_MODULE=MODULE                                                 977
C    *                                                                    977
C    Geant ATTRIBUTE : FEEP ===> SEEN=1 COLO=KRED <===                    978
         CALL AGSRESET                                                    978
         AG_SEEN = 1                                                      978
         AG_COLO = KRED                                                   978
         AG_TITLE = 'FEEP'                                                978
         AG_EXNAME = 'ATTRIBUTE'                                          978
         AG_PARLIST = 'SEEN_COLO'                                         978
         CALL AXATTRIBUTE                                                 978
C    *                                                                    978
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        978
         CALL AGSRESET                                                    979
         AG_TITLE = 'ALUMINIUM'                                           979
         AG_EXNAME = 'MATERIAL'                                           979
         AG_PARLIST = 'NONE'                                              979
         CALL AXMATERIAL                                                  979
C    *                                                                    979
C    Geant MEDIUM : FEEP_ALUMINIUM ===> DEEMAX=0.18296 STMIN=0.0334422 <  979
C    ===                                                                  979
         CALL AGSRESET                                                    980
         AG_DEEMAX = 0.18296                                              980
         AG_STMIN = 0.0334422                                             980
         AG_TITLE = 'FEEP_ALUMINIUM'                                      980
         AG_EXNAME = 'MEDIUM'                                             980
         AG_PARLIST = 'DEEMAX_STMIN'                                      980
         CALL AXMEDIUM                                                    980
C    *                                                                    980
C    Geant SHAPE : BOX ===> DX=TFEE_PLATEDX DY=TFEE_PLATEDY, DZ=TFEE_PLA  980
C    TEDZ <===                                                            980
         CALL AGSRESET                                                    982
         AG_DX = TFEE_PLATEDX                                             982
         AG_DY = TFEE_PLATEDY                                             982
         AG_DZ = TFEE_PLATEDZ                                             982
         AG_TITLE = 'BOX'                                                 982
         AG_EXNAME = 'SHAPE'                                              982
         AG_PARLIST = 'DX_DY_DZ'                                          982
         CALL AXSHAPE                                                     982
C    *                                                                    982
5861  CALL AGSPOPB                                                        983
      GO TO BLOCK_FEEP, (5860 ,5870)                                      983
C End of Local Procedure                                                  983
C ----------------------------------------------------------------------  984
C ------------ FEER                                                       984
C Begin Local Procedure FEER                                              985
C FEER FEERIB                                                             985
5880  CONTINUE                                                            985
         AG_TITLE='FEER'                                                  985
         AG_MODULE=MODULE                                                 985
         AG_MARK='V00'                                                    985
         CALL AGSPUSH                                                     985
         AG_MODULE=MODULE                                                 985
C    *                                                                    985
C    Geant ATTRIBUTE : FEER ===> SEEN=1 COLO=1 <===                       986
         CALL AGSRESET                                                    986
         AG_SEEN = 1                                                      986
         AG_COLO = 1                                                      986
         AG_TITLE = 'FEER'                                                986
         AG_EXNAME = 'ATTRIBUTE'                                          986
         AG_PARLIST = 'SEEN_COLO'                                         986
         CALL AXATTRIBUTE                                                 986
C    *                                                                    986
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        986
         CALL AGSRESET                                                    987
         AG_TITLE = 'ALUMINIUM'                                           987
         AG_EXNAME = 'MATERIAL'                                           987
         AG_PARLIST = 'NONE'                                              987
         CALL AXMATERIAL                                                  987
C    *                                                                    987
C    Geant MEDIUM : FEER_ALUMINIUM ===> DEEMAX=0.18296 STMIN=0.0334422 <  987
C    ===                                                                  987
         CALL AGSRESET                                                    988
         AG_DEEMAX = 0.18296                                              988
         AG_STMIN = 0.0334422                                             988
         AG_TITLE = 'FEER_ALUMINIUM'                                      988
         AG_EXNAME = 'MEDIUM'                                             988
         AG_PARLIST = 'DEEMAX_STMIN'                                      988
         CALL AXMEDIUM                                                    988
C    *                                                                    988
C    Geant SHAPE : BOX ===> DX=TFEE_RIBDX DY=TFEE_RIBDY DZ=TFEE_RIBDZ <=  988
C    ==                                                                   988
         CALL AGSRESET                                                    989
         AG_DX = TFEE_RIBDX                                               989
         AG_DY = TFEE_RIBDY                                               989
         AG_DZ = TFEE_RIBDZ                                               989
         AG_TITLE = 'BOX'                                                 989
         AG_EXNAME = 'SHAPE'                                              989
         AG_PARLIST = 'DX_DY_DZ'                                          989
         CALL AXSHAPE                                                     989
C    *                                                                    989
5881  CALL AGSPOPB                                                        990
      GO TO BLOCK_FEER, (5880 ,5890)                                      990
C End of Local Procedure                                                  990
C ----------------------------------------------------------------------  991
C ------------ FEEI                                                       991
C Begin Local Procedure FEEI                                              992
C FEEI FEEITSELF                                                          992
5900  CONTINUE                                                            992
         AG_TITLE='FEEI'                                                  992
         AG_MODULE=MODULE                                                 992
         AG_MARK='V00'                                                    992
         CALL AGSPUSH                                                     992
         AG_MODULE=MODULE                                                 992
C    *                                                                    992
C    Geant ATTRIBUTE : FEEI ===> SEEN=1 COLO=KVIOLET <===                 993
         CALL AGSRESET                                                    993
         AG_SEEN = 1                                                      993
         AG_COLO = KVIOLET                                                993
         AG_TITLE = 'FEEI'                                                993
         AG_EXNAME = 'ATTRIBUTE'                                          993
         AG_PARLIST = 'SEEN_COLO'                                         993
         CALL AXATTRIBUTE                                                 993
C    *                                                                    993
C    Geant MATERIAL : G10 ===> inherited arguments used <===              993
         CALL AGSRESET                                                    994
         AG_TITLE = 'G10'                                                 994
         AG_EXNAME = 'MATERIAL'                                           994
         AG_PARLIST = 'NONE'                                              994
         CALL AXMATERIAL                                                  994
C    *                                                                    994
C    Geant MEDIUM : FEEI_G10 ===> DEEMAX=0.20313 STMIN=0.0333806 <===     994
         CALL AGSRESET                                                    995
         AG_DEEMAX = 0.20313                                              995
         AG_STMIN = 0.0333806                                             995
         AG_TITLE = 'FEEI_G10'                                            995
         AG_EXNAME = 'MEDIUM'                                             995
         AG_PARLIST = 'DEEMAX_STMIN'                                      995
         CALL AXMEDIUM                                                    995
C    *                                                                    995
C    Geant SHAPE : BOX ===> DX=TFEE_CARDDX DY=TFEE_CARDDY, DZ=TFEE_CARDD  995
C    Z <===                                                               995
         CALL AGSRESET                                                    997
         AG_DX = TFEE_CARDDX                                              997
         AG_DY = TFEE_CARDDY                                              997
         AG_DZ = TFEE_CARDDZ                                              997
         AG_TITLE = 'BOX'                                                 997
         AG_EXNAME = 'SHAPE'                                              997
         AG_PARLIST = 'DX_DY_DZ'                                          997
         CALL AXSHAPE                                                     997
C    *                                                                    997
5901  CALL AGSPOPB                                                        998
      GO TO BLOCK_FEEI, (5900 ,5910)                                      998
C End of Local Procedure                                                  998
C ----------------------------------------------------------------------  999
C ------------ TALS                                                       999
C Begin Local Procedure TALS                                              1000
C TALS TPCSECTORALSUPPORT THE SECTOR G10 ALLIMINIUM SUPPORT               1000
5670  CONTINUE                                                            1000
         AG_TITLE='TALS'                                                  1000
         AG_MODULE=MODULE                                                 1000
         AG_MARK='V00'                                                    1000
         CALL AGSPUSH                                                     1000
         AG_MODULE=MODULE                                                 1000
C    *                                                                    1000
C    Geant ATTRIBUTE : TALS ===> SEEN=1 COLO=KYELLOW <===                 1001
         CALL AGSRESET                                                    1001
         AG_SEEN = 1                                                      1001
         AG_COLO = KYELLOW                                                1001
         AG_TITLE = 'TALS'                                                1001
         AG_EXNAME = 'ATTRIBUTE'                                          1001
         AG_PARLIST = 'SEEN_COLO'                                         1001
         CALL AXATTRIBUTE                                                 1001
C    *                                                                    1001
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1001
         CALL AGSRESET                                                    1002
         AG_TITLE = 'ALUMINIUM'                                           1002
         AG_EXNAME = 'MATERIAL'                                           1002
         AG_PARLIST = 'NONE'                                              1002
         CALL AXMATERIAL                                                  1002
C    *                                                                    1002
C    Geant MEDIUM : TALS_ALUMINIUM ===> DEEMAX=0.18296 STMIN=0.0334422 <  1002
C    ===                                                                  1002
         CALL AGSRESET                                                    1003
         AG_DEEMAX = 0.18296                                              1003
         AG_STMIN = 0.0334422                                             1003
         AG_TITLE = 'TALS_ALUMINIUM'                                      1003
         AG_EXNAME = 'MEDIUM'                                             1003
         AG_PARLIST = 'DEEMAX_STMIN'                                      1003
         CALL AXMEDIUM                                                    1003
C    *                                                                    1003
C    Geant SHAPE : PGON ===> PHI1=-ALPHA DPHI=2*ALPHA NPDIV=1, NZ=2 ZI={  1003
C    Z1+TPCG_PADPLANETHICKNESS,Z1+TPCG_PADPLANETHICKNESS+TECW_THICKAL},   1003
C    RMN={R1,R1}, RMX={R2,R2} <===                                        1003
         CALL AGSRESET                                                    1007
         AG_PHI1 = -ALPHA                                                 1007
         AG_DPHI = 2*ALPHA                                                1007
         AG_NPDIV = 1                                                     1007
         AG_NZ = 2                                                        1007
         AG_ZI(1)=Z1+TPCG_PADPLANETHICKNESS                               1007
         AG_ZI(2)=Z1+TPCG_PADPLANETHICKNESS+TECW_THICKAL                  1007
         AG_RMN(1)=R1                                                     1007
         AG_RMN(2)=R1                                                     1007
         AG_RMX(1)=R2                                                     1007
         AG_RMX(2)=R2                                                     1007
         AG_TITLE = 'PGON'                                                1007
         AG_EXNAME = 'SHAPE'                                              1007
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1007
         CALL AXSHAPE                                                     1007
C    *                                                                    1007
         MYPAR(1)= TECW_HOLEDX                                            1007
         MYPAR(2)= TECW_HOLEDY                                            1009
         MYPAR(3)= TECW_THICKAL/2                                         1010
         MYPAR(13)= ZWHEEL1 - TECW_THICK + TPCG_PADPLANETHICKNESS +       1011
     *   TECW_THICKAL/2                                                   1011
C    *                                                                    1012
         ITALS=0                                                          1012
            GO TO 5923                                                    1012
5921        ITALS=ITALS+1                                                 1012
5923        IF(ITALS-(HOLZ_NUMBEROFROWS - 1).GT.0) GO TO 5922             1012
            MYPAR(11) = XHOLES(ITALS)                                     1012
C       // CM                                                             1013
C       *                                                                 1014
            JTALS=0                                                       1014
               GO TO 5933                                                 1014
5931           JTALS=JTALS+1                                              1014
5933           IF(JTALS-(HOLZ_NUMBERPERROW( ITALS+1 ) - 1).GT.0) GO TO    1014
     *         5932                                                       1014
               MYPAR(12) = HOLZ_YHOLES( 9*ITALS+JTALS+1) *INCH            1015
C          Create THOL :                                                  1016
C          Call To Local Procedure THOL                                   1016
               ASSIGN 5950 TO BLOCK_THOL                                  1016
               GO TO  5940                                                1016
5950           CONTINUE                                                   1016
C          *                                                              1016
C          Geant POSITION : THOL ===> X=MYPAR(11) Y=MYPAR(12) Z=MYPAR(13  1017
C          ) <===                                                         1017
               CALL AGSRESET                                              1017
               AG_X = MYPAR(11)                                           1017
               AG_Y = MYPAR(12)                                           1017
               AG_Z = MYPAR(13)                                           1017
               AG_TITLE = 'THOL'                                          1017
               AG_EXNAME = 'POSITION'                                     1017
               AG_PARLIST = 'X_Y_Z'                                       1017
               CALL AXPOSITION                                            1017
C          *                                                              1017
            GO TO 5931                                                    1018
5932        CONTINUE                                                      1018
         GO TO 5921                                                       1019
5922     CONTINUE                                                         1019
5671  CALL AGSPOPB                                                        1020
      GO TO BLOCK_TALS, (5670 ,5680)                                      1020
C End of Local Procedure                                                  1020
C ----------------------------------------------------------------------  1021
C ------------ TSGT                                                       1021
C Begin Local Procedure TSGT                                              1022
C TSGT TPCSECTORG10 THE SECTOR G10                                        1022
5690  CONTINUE                                                            1022
         AG_TITLE='TSGT'                                                  1022
         AG_MODULE=MODULE                                                 1022
         AG_MARK='V00'                                                    1022
         CALL AGSPUSH                                                     1022
         AG_MODULE=MODULE                                                 1022
C    *                                                                    1022
C    Geant ATTRIBUTE : TSGT ===> SEEN=1 COLO=KBLUE <===                   1023
         CALL AGSRESET                                                    1023
         AG_SEEN = 1                                                      1023
         AG_COLO = KBLUE                                                  1023
         AG_TITLE = 'TSGT'                                                1023
         AG_EXNAME = 'ATTRIBUTE'                                          1023
         AG_PARLIST = 'SEEN_COLO'                                         1023
         CALL AXATTRIBUTE                                                 1023
C    *                                                                    1023
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1023
         CALL AGSRESET                                                    1024
         AG_TITLE = 'G10'                                                 1024
         AG_EXNAME = 'MATERIAL'                                           1024
         AG_PARLIST = 'NONE'                                              1024
         CALL AXMATERIAL                                                  1024
C    *                                                                    1024
C    Geant MEDIUM : TSGT_G10 ===> DEEMAX=0.20313 STMIN=0.0333806 <===     1024
         CALL AGSRESET                                                    1025
         AG_DEEMAX = 0.20313                                              1025
         AG_STMIN = 0.0333806                                             1025
         AG_TITLE = 'TSGT_G10'                                            1025
         AG_EXNAME = 'MEDIUM'                                             1025
         AG_PARLIST = 'DEEMAX_STMIN'                                      1025
         CALL AXMEDIUM                                                    1025
C    *                                                                    1025
C    Geant SHAPE : PGON ===> PHI1=-ALPHA DPHI=2*ALPHA NPDIV=1, NZ=2 ZI={  1025
C    Z1,Z1+TPCG_PADPLANETHICKNESS} RMN={R1, R1} RMX={R2,R2} <===          1025
         CALL AGSRESET                                                    1028
         AG_PHI1 = -ALPHA                                                 1028
         AG_DPHI = 2*ALPHA                                                1028
         AG_NPDIV = 1                                                     1028
         AG_NZ = 2                                                        1028
         AG_ZI(1)=Z1                                                      1028
         AG_ZI(2)=Z1+TPCG_PADPLANETHICKNESS                               1028
         AG_RMN(1)=R1                                                     1028
         AG_RMN(2)=R1                                                     1028
         AG_RMX(1)=R2                                                     1028
         AG_RMX(2)=R2                                                     1028
         AG_TITLE = 'PGON'                                                1028
         AG_EXNAME = 'SHAPE'                                              1028
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1028
         CALL AXSHAPE                                                     1028
C    *                                                                    1028
5691  CALL AGSPOPB                                                        1029
      GO TO BLOCK_TSGT, (5690 ,5700)                                      1029
C End of Local Procedure                                                  1029
C ----------------------------------------------------------------------  1030
C ------------ TWBT                                                       1030
C Begin Local Procedure TWBT                                              1031
C TWBT //WHEELBOTTOM                                                      1031
5510  CONTINUE                                                            1031
         AG_TITLE='TWBT'                                                  1031
         AG_MODULE=MODULE                                                 1031
         AG_MARK='V00'                                                    1031
         CALL AGSPUSH                                                     1031
         AG_MODULE=MODULE                                                 1031
C    *                                                                    1031
C    Geant ATTRIBUTE : TWBT ===> SEEN=1 COLO=KGREEN <===                  1032
         CALL AGSRESET                                                    1032
         AG_SEEN = 1                                                      1032
         AG_COLO = KGREEN                                                 1032
         AG_TITLE = 'TWBT'                                                1032
         AG_EXNAME = 'ATTRIBUTE'                                          1032
         AG_PARLIST = 'SEEN_COLO'                                         1032
         CALL AXATTRIBUTE                                                 1032
C    *                                                                    1032
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1032
         CALL AGSRESET                                                    1033
         AG_TITLE = 'ALUMINIUM'                                           1033
         AG_EXNAME = 'MATERIAL'                                           1033
         AG_PARLIST = 'NONE'                                              1033
         CALL AXMATERIAL                                                  1033
C    *                                                                    1033
C    Geant MEDIUM : TWBT_ALUMINIUM ===> DEEMAX=0.18296 STMIN=0.0334422 <  1033
C    ===                                                                  1033
         CALL AGSRESET                                                    1034
         AG_DEEMAX = 0.18296                                              1034
         AG_STMIN = 0.0334422                                             1034
         AG_TITLE = 'TWBT_ALUMINIUM'                                      1034
         AG_EXNAME = 'MEDIUM'                                             1034
         AG_PARLIST = 'DEEMAX_STMIN'                                      1034
         CALL AXMEDIUM                                                    1034
C    *                                                                    1034
C    Geant SHAPE : PCON ===> PHI1=-15 DPHI=30 NZ=4 ZI={ZWHEEL1, ZWHEEL1+  1034
C    3.500*INCH,ZWHEEL1+3.500*INCH,TPCG_LENGTHW/2}, RMN={TPCG_WHEELIR,TP  1034
C    CG_WHEELIR, TPCG_WHEELIR+3.6,TPCG_WHEELIR+3.6} RMX={TPCG_WHEELR0, T  1034
C    PCG_WHEELR0,TPCG_WHEELR0,TPCG_WHEELR0} <===                          1034
         CALL AGSRESET                                                    1039
         AG_PHI1 = -15                                                    1039
         AG_DPHI = 30                                                     1039
         AG_NZ = 4                                                        1039
         AG_ZI(1)=ZWHEEL1                                                 1039
         AG_ZI(2)=ZWHEEL1+3.500*INCH                                      1039
         AG_ZI(3)=ZWHEEL1+3.500*INCH                                      1039
         AG_ZI(4)=TPCG_LENGTHW/2                                          1039
         AG_RMN(1)=TPCG_WHEELIR                                           1039
         AG_RMN(2)=TPCG_WHEELIR                                           1039
         AG_RMN(3)=TPCG_WHEELIR+3.6                                       1039
         AG_RMN(4)=TPCG_WHEELIR+3.6                                       1039
         AG_RMX(1)=TPCG_WHEELR0                                           1039
         AG_RMX(2)=TPCG_WHEELR0                                           1039
         AG_RMX(3)=TPCG_WHEELR0                                           1039
         AG_RMX(4)=TPCG_WHEELR0                                           1039
         AG_TITLE = 'PCON'                                                1039
         AG_EXNAME = 'SHAPE'                                              1039
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1039
         CALL AXSHAPE                                                     1039
C    *                                                                    1039
5511  CALL AGSPOPB                                                        1040
      GO TO BLOCK_TWBT, (5510 ,5520)                                      1040
C End of Local Procedure                                                  1040
C ----------------------------------------------------------------------  1041
C ------------ TWRI                                                       1041
C Begin Local Procedure TWRI                                              1042
C TWRI WHEELOUTERRING                                                     1042
5530  CONTINUE                                                            1042
         AG_TITLE='TWRI'                                                  1042
         AG_MODULE=MODULE                                                 1042
         AG_MARK='V00'                                                    1042
         CALL AGSPUSH                                                     1042
         AG_MODULE=MODULE                                                 1042
C    *                                                                    1042
C    Geant MEDIUM : TWRI_STANDARD ===> STMIN=1.15055 <===                 1043
         CALL AGSRESET                                                    1043
         AG_STMIN = 1.15055                                               1043
         AG_TITLE = 'TWRI_STANDARD'                                       1043
         AG_EXNAME = 'MEDIUM'                                             1043
         AG_PARLIST = 'STMIN'                                             1043
         CALL AXMEDIUM                                                    1043
C    *                                                                    1043
C    Geant SHAPE : PCON ===> PHI1=-15 DPHI=30 NZ=4 ZI={ZWHEEL1, ZTOFCEND  1043
C    ,ZTOFCEND,TPCG_LENGTHW/2} RMN={TPCG_WHEELR2*COS15, TPCG_WHEELR2*COS  1043
C    15,TPCG_WHEELR2*COS15, TPCG_WHEELR2*COS15} RMX={TPCG_WHEELOR1, TPCG  1043
C    _WHEELOR1,TPCG_WHEELOR,TPCG_WHEELOR} <===                            1043
         CALL AGSRESET                                                    1048
         AG_PHI1 = -15                                                    1048
         AG_DPHI = 30                                                     1048
         AG_NZ = 4                                                        1048
         AG_ZI(1)=ZWHEEL1                                                 1048
         AG_ZI(2)=ZTOFCEND                                                1048
         AG_ZI(3)=ZTOFCEND                                                1048
         AG_ZI(4)=TPCG_LENGTHW/2                                          1048
         AG_RMN(1)=TPCG_WHEELR2*COS15                                     1048
         AG_RMN(2)=TPCG_WHEELR2*COS15                                     1048
         AG_RMN(3)=TPCG_WHEELR2*COS15                                     1048
         AG_RMN(4)=TPCG_WHEELR2*COS15                                     1048
         AG_RMX(1)=TPCG_WHEELOR1                                          1048
         AG_RMX(2)=TPCG_WHEELOR1                                          1048
         AG_RMX(3)=TPCG_WHEELOR                                           1048
         AG_RMX(4)=TPCG_WHEELOR                                           1048
         AG_TITLE = 'PCON'                                                1048
         AG_EXNAME = 'SHAPE'                                              1048
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1048
         CALL AXSHAPE                                                     1048
C    *                                                                    1048
C    Geant MEDIUM : TWRI_SOMETHING ===> DEEMAX=0.248853 STMIN=1.15055 <=  1048
C    ==                                                                   1048
         CALL AGSRESET                                                    1049
         AG_DEEMAX = 0.248853                                             1049
         AG_STMIN = 1.15055                                               1049
         AG_TITLE = 'TWRI_SOMETHING'                                      1049
         AG_EXNAME = 'MEDIUM'                                             1049
         AG_PARLIST = 'DEEMAX_STMIN'                                      1049
         CALL AXMEDIUM                                                    1049
C    *                                                                    1049
C    Geant ATTRIBUTE : TWRI ===> SEEN=0 COLO=KGREEN <===                  1049
         CALL AGSRESET                                                    1050
         AG_SEEN = 0                                                      1050
         AG_COLO = KGREEN                                                 1050
         AG_TITLE = 'TWRI'                                                1050
         AG_EXNAME = 'ATTRIBUTE'                                          1050
         AG_PARLIST = 'SEEN_COLO'                                         1050
         CALL AXATTRIBUTE                                                 1050
C    *                                                                    1050
C    Create TWRC :                                                        1051
C    Call To Local Procedure TWRC                                         1051
         ASSIGN 5970 TO BLOCK_TWRC                                        1051
         GO TO  5960                                                      1051
5970     CONTINUE                                                         1051
C    *                                                                    1051
C    Geant POSITION : TWRC ===> inherited arguments used <===             1052
         CALL AGSRESET                                                    1052
         AG_TITLE = 'TWRC'                                                1052
         AG_EXNAME = 'POSITION'                                           1052
         AG_PARLIST = 'NONE'                                              1052
         CALL AXPOSITION                                                  1052
C    *                                                                    1052
         MYCOS = COS15                                                    1052
         MYRMIN=TPCG_WHEELR2                                              1052
         MYRMAX = TPCG_WHEELOR1                                           1053
         MYG1 = MYRMIN*COS15                                              1054
         NCOUNT = 0                                                       1055
C    *                                                                    1056
C       Loop here                                                         1056
5981        IF(MYG2.GE.MYRMIN) GO TO 5982                                 1056
            NCOUNT=NCOUNT+1                                               1056
            MYG2 = MYRMIN                                                 1058
C       Check (MYG2>MYRMAX*MYCOS)                                         1059
            IF ((MYG2.GT.MYRMAX*MYCOS)) THEN                              1059
            MYG2 = MYRMAX*MYCOS                                           1060
            ENDIF                                                         1061
            ALPHA = ACOS(MYCOS)/PI*180                                    1062
C       Create TWRG :                                                     1063
C       Call To Local Procedure TWRG                                      1063
            ASSIGN 6000 TO BLOCK_TWRG                                     1063
            GO TO  5990                                                   1063
6000        CONTINUE                                                      1063
C       *                                                                 1063
C       Geant POSITION : TWRG ===> inherited arguments used <===          1064
            CALL AGSRESET                                                 1064
            AG_TITLE = 'TWRG'                                             1064
            AG_EXNAME = 'POSITION'                                        1064
            AG_PARLIST = 'NONE'                                           1064
            CALL AXPOSITION                                               1064
C       *                                                                 1064
            MYCOS = MYG2/MYRMIN                                           1064
            MYG1=MYG2                                                     1065
C       Check (NCOUNT>10)                                                 1066
            IF ((NCOUNT.GT.10)) THEN                                      1066
            GO TO 5982                                                    1067
            ENDIF                                                         1068
         GO TO 5981                                                       1069
5982     CONTINUE                                                         1069
5531  CALL AGSPOPB                                                        1070
      GO TO BLOCK_TWRI, (5530 ,5540)                                      1070
C End of Local Procedure                                                  1070
C ----------------------------------------------------------------------  1071
C ------------ TWRG                                                       1071
C Begin Local Procedure TWRG                                              1072
C TWRG WHEELOUTERRING PGON PART                                           1072
5990  CONTINUE                                                            1072
         AG_TITLE='TWRG'                                                  1072
         AG_MODULE=MODULE                                                 1072
         AG_MARK='V00'                                                    1072
         CALL AGSPUSH                                                     1072
         AG_MODULE=MODULE                                                 1072
C    *                                                                    1072
C    Geant ATTRIBUTE : TWRG ===> SEEN=1 COLO=KGREEN <===                  1073
         CALL AGSRESET                                                    1073
         AG_SEEN = 1                                                      1073
         AG_COLO = KGREEN                                                 1073
         AG_TITLE = 'TWRG'                                                1073
         AG_EXNAME = 'ATTRIBUTE'                                          1073
         AG_PARLIST = 'SEEN_COLO'                                         1073
         CALL AXATTRIBUTE                                                 1073
C    *                                                                    1073
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1073
         CALL AGSRESET                                                    1074
         AG_TITLE = 'ALUMINIUM'                                           1074
         AG_EXNAME = 'MATERIAL'                                           1074
         AG_PARLIST = 'NONE'                                              1074
         CALL AXMATERIAL                                                  1074
C    *                                                                    1074
C    Geant MEDIUM : TWRG_ALUMINIUM ===> DEEMAX=0.18296 STMIN=0.0334422 <  1074
C    ===                                                                  1074
         CALL AGSRESET                                                    1075
         AG_DEEMAX = 0.18296                                              1075
         AG_STMIN = 0.0334422                                             1075
         AG_TITLE = 'TWRG_ALUMINIUM'                                      1075
         AG_EXNAME = 'MEDIUM'                                             1075
         AG_PARLIST = 'DEEMAX_STMIN'                                      1075
         CALL AXMEDIUM                                                    1075
C    *                                                                    1075
C    Geant SHAPE : PGON ===> PHI1=-ALPHA DPHI=ALPHA*2 NPDIV=1, NZ=2 ZI={  1075
C    ZWHEEL1,TPCG_LENGTHW/2} RMN={MYG1, MYG1} RMX={MYG2,MYG2} <===        1075
         CALL AGSRESET                                                    1078
         AG_PHI1 = -ALPHA                                                 1078
         AG_DPHI = ALPHA*2                                                1078
         AG_NPDIV = 1                                                     1078
         AG_NZ = 2                                                        1078
         AG_ZI(1)=ZWHEEL1                                                 1078
         AG_ZI(2)=TPCG_LENGTHW/2                                          1078
         AG_RMN(1)=MYG1                                                   1078
         AG_RMN(2)=MYG1                                                   1078
         AG_RMX(1)=MYG2                                                   1078
         AG_RMX(2)=MYG2                                                   1078
         AG_TITLE = 'PGON'                                                1078
         AG_EXNAME = 'SHAPE'                                              1078
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1078
         CALL AXSHAPE                                                     1078
C    *                                                                    1078
5991  CALL AGSPOPB                                                        1079
      GO TO BLOCK_TWRG, (5990 ,6000)                                      1079
C End of Local Procedure                                                  1079
C ----------------------------------------------------------------------  1080
C ------------ TWRC                                                       1080
C Begin Local Procedure TWRC                                              1081
C TWRC WHEELOUTERRING PCON PART                                           1081
5960  CONTINUE                                                            1081
         AG_TITLE='TWRC'                                                  1081
         AG_MODULE=MODULE                                                 1081
         AG_MARK='V00'                                                    1081
         CALL AGSPUSH                                                     1081
         AG_MODULE=MODULE                                                 1081
C    *                                                                    1081
C    Geant ATTRIBUTE : TWRC ===> SEEN=1 COLO=KGREEN <===                  1082
         CALL AGSRESET                                                    1082
         AG_SEEN = 1                                                      1082
         AG_COLO = KGREEN                                                 1082
         AG_TITLE = 'TWRC'                                                1082
         AG_EXNAME = 'ATTRIBUTE'                                          1082
         AG_PARLIST = 'SEEN_COLO'                                         1082
         CALL AXATTRIBUTE                                                 1082
C    *                                                                    1082
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1082
         CALL AGSRESET                                                    1083
         AG_TITLE = 'ALUMINIUM'                                           1083
         AG_EXNAME = 'MATERIAL'                                           1083
         AG_PARLIST = 'NONE'                                              1083
         CALL AXMATERIAL                                                  1083
C    *                                                                    1083
C    Geant MEDIUM : TWRC_ALUMINIUM ===> DEEMAX=0.18296 STMIN=0.0334422 <  1083
C    ===                                                                  1083
         CALL AGSRESET                                                    1084
         AG_DEEMAX = 0.18296                                              1084
         AG_STMIN = 0.0334422                                             1084
         AG_TITLE = 'TWRC_ALUMINIUM'                                      1084
         AG_EXNAME = 'MEDIUM'                                             1084
         AG_PARLIST = 'DEEMAX_STMIN'                                      1084
         CALL AXMEDIUM                                                    1084
C    *                                                                    1084
C    Geant SHAPE : PCON ===> PHI1=-15 DPHI=30 NZ=4 ZI={ZWHEEL1, ZTOFCEND  1084
C    ,ZTOFCEND,TPCG_LENGTHW/2} RMN={TPCG_WHEELR2/COS15, TPCG_WHEELR2/COS  1084
C    15,TPCG_WHEELR2/COS15, TPCG_WHEELR2/COS15} RMX={TPCG_WHEELOR1, TPCG  1084
C    _WHEELOR1,TPCG_WHEELOR,TPCG_WHEELOR} <===                            1084
         CALL AGSRESET                                                    1089
         AG_PHI1 = -15                                                    1089
         AG_DPHI = 30                                                     1089
         AG_NZ = 4                                                        1089
         AG_ZI(1)=ZWHEEL1                                                 1089
         AG_ZI(2)=ZTOFCEND                                                1089
         AG_ZI(3)=ZTOFCEND                                                1089
         AG_ZI(4)=TPCG_LENGTHW/2                                          1089
         AG_RMN(1)=TPCG_WHEELR2/COS15                                     1089
         AG_RMN(2)=TPCG_WHEELR2/COS15                                     1089
         AG_RMN(3)=TPCG_WHEELR2/COS15                                     1089
         AG_RMN(4)=TPCG_WHEELR2/COS15                                     1089
         AG_RMX(1)=TPCG_WHEELOR1                                          1089
         AG_RMX(2)=TPCG_WHEELOR1                                          1089
         AG_RMX(3)=TPCG_WHEELOR                                           1089
         AG_RMX(4)=TPCG_WHEELOR                                           1089
         AG_TITLE = 'PCON'                                                1089
         AG_EXNAME = 'SHAPE'                                              1089
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1089
         CALL AXSHAPE                                                     1089
C    *                                                                    1089
5961  CALL AGSPOPB                                                        1090
      GO TO BLOCK_TWRC, (5960 ,5970)                                      1090
C End of Local Procedure                                                  1090
C ----------------------------------------------------------------------  1091
C ------------ TWTR                                                       1091
C Begin Local Procedure TWTR                                              1092
C TWTR TPCWHEELTOPRIB                                                     1092
5550  CONTINUE                                                            1092
         AG_TITLE='TWTR'                                                  1092
         AG_MODULE=MODULE                                                 1092
         AG_MARK='V00'                                                    1092
         CALL AGSPUSH                                                     1092
         AG_MODULE=MODULE                                                 1092
C    *                                                                    1092
C    Geant ATTRIBUTE : TWTR ===> SEEN=1 COLO=KBLUE <===                   1093
         CALL AGSRESET                                                    1093
         AG_SEEN = 1                                                      1093
         AG_COLO = KBLUE                                                  1093
         AG_TITLE = 'TWTR'                                                1093
         AG_EXNAME = 'ATTRIBUTE'                                          1093
         AG_PARLIST = 'SEEN_COLO'                                         1093
         CALL AXATTRIBUTE                                                 1093
C    *                                                                    1093
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1093
         CALL AGSRESET                                                    1094
         AG_TITLE = 'ALUMINIUM'                                           1094
         AG_EXNAME = 'MATERIAL'                                           1094
         AG_PARLIST = 'NONE'                                              1094
         CALL AXMATERIAL                                                  1094
C    *                                                                    1094
C    Geant MEDIUM : TWTR_ALUMINIUM ===> DEEMAX=0.18296 STMIN=0.0334422 <  1094
C    ===                                                                  1094
         CALL AGSRESET                                                    1095
         AG_DEEMAX = 0.18296                                              1095
         AG_STMIN = 0.0334422                                             1095
         AG_TITLE = 'TWTR_ALUMINIUM'                                      1095
         AG_EXNAME = 'MEDIUM'                                             1095
         AG_PARLIST = 'DEEMAX_STMIN'                                      1095
         CALL AXMEDIUM                                                    1095
C    *                                                                    1095
         MYG1=TPCG_WHEELR2 - (TPCG_WHEELTOTALRIBWIDTH -                   1095
     *   TPCG_WHEELRIBWIDTH)/2                                            1095
C    Geant SHAPE : PGON ===> PHI1=-15 DPHI=30 NPDIV=1 NZ=2, ZI={ZWHEEL1,  1099
C    ZWHEEL1+TPCG_WHEELRIBHEIGHT} RMN={MYG1, MYG1} RMX={TPCG_WHEELR2,TPC  1099
C    G_WHEELR2} <===                                                      1099
         CALL AGSRESET                                                    1099
         AG_PHI1 = -15                                                    1099
         AG_DPHI = 30                                                     1099
         AG_NPDIV = 1                                                     1099
         AG_NZ = 2                                                        1099
         AG_ZI(1)=ZWHEEL1                                                 1099
         AG_ZI(2)=ZWHEEL1+TPCG_WHEELRIBHEIGHT                             1099
         AG_RMN(1)=MYG1                                                   1099
         AG_RMN(2)=MYG1                                                   1099
         AG_RMX(1)=TPCG_WHEELR2                                           1099
         AG_RMX(2)=TPCG_WHEELR2                                           1099
         AG_TITLE = 'PGON'                                                1099
         AG_EXNAME = 'SHAPE'                                              1099
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1099
         CALL AXSHAPE                                                     1099
C    *                                                                    1099
5551  CALL AGSPOPB                                                        1100
      GO TO BLOCK_TWTR, (5550 ,5560)                                      1100
C End of Local Procedure                                                  1100
C ----------------------------------------------------------------------  1101
C ------------ TWMR                                                       1101
C Begin Local Procedure TWMR                                              1102
C TWMR TPCWHEELMIDDLERIB                                                  1102
5570  CONTINUE                                                            1102
         AG_TITLE='TWMR'                                                  1102
         AG_MODULE=MODULE                                                 1102
         AG_MARK='V00'                                                    1102
         CALL AGSPUSH                                                     1102
         AG_MODULE=MODULE                                                 1102
C    *                                                                    1102
C    Geant ATTRIBUTE : TWMR ===> SEEN=1 COLO=KYELLOW <===                 1103
         CALL AGSRESET                                                    1103
         AG_SEEN = 1                                                      1103
         AG_COLO = KYELLOW                                                1103
         AG_TITLE = 'TWMR'                                                1103
         AG_EXNAME = 'ATTRIBUTE'                                          1103
         AG_PARLIST = 'SEEN_COLO'                                         1103
         CALL AXATTRIBUTE                                                 1103
C    *                                                                    1103
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1103
         CALL AGSRESET                                                    1104
         AG_TITLE = 'ALUMINIUM'                                           1104
         AG_EXNAME = 'MATERIAL'                                           1104
         AG_PARLIST = 'NONE'                                              1104
         CALL AXMATERIAL                                                  1104
C    *                                                                    1104
C    Geant MEDIUM : TWMR_ALUMINIUM ===> DEEMAX=0.18296 STMIN=0.0334422 <  1104
C    ===                                                                  1104
         CALL AGSRESET                                                    1105
         AG_DEEMAX = 0.18296                                              1105
         AG_STMIN = 0.0334422                                             1105
         AG_TITLE = 'TWMR_ALUMINIUM'                                      1105
         AG_EXNAME = 'MEDIUM'                                             1105
         AG_PARLIST = 'DEEMAX_STMIN'                                      1105
         CALL AXMEDIUM                                                    1105
C    *                                                                    1105
         MYG1=TPCG_WHEELR2 - (TPCG_WHEELTOTALRIBWIDTH -                   1105
     *   TPCG_WHEELRIBWIDTH)/2                                            1105
C    Geant SHAPE : PGON ===> PHI1=-15 DPHI=30 NPDIV=1 NZ=4, ZI={ZWHEEL1,  1115
C    ZWHEEL1+TPCG_WHEELRIBHEIGHT,ZWHEEL1+TPCG_WHEELRIBHEIGHT, TPCG_LENGT  1115
C    HW/2} RMN={TPCG_WHEELR1-TPCG_WHEELTOTALRIBWIDTH/2, TPCG_WHEELR1-TPC  1115
C    G_WHEELTOTALRIBWIDTH/2, TPCG_WHEELR1-TPCG_WHEELRIBWIDTH/2, TPCG_WHE  1115
C    ELR1-TPCG_WHEELRIBWIDTH/2}, RMX={TPCG_WHEELR1+TPCG_WHEELTOTALRIBWID  1115
C    TH/2, TPCG_WHEELR1+TPCG_WHEELTOTALRIBWIDTH/2, TPCG_WHEELR1+TPCG_WHE  1115
C    ELRIBWIDTH/2,TPCG_WHEELR1+TPCG_WHEELRIBWIDTH/2} <===                 1115
         CALL AGSRESET                                                    1115
         AG_PHI1 = -15                                                    1115
         AG_DPHI = 30                                                     1115
         AG_NPDIV = 1                                                     1115
         AG_NZ = 4                                                        1115
         AG_ZI(1)=ZWHEEL1                                                 1115
         AG_ZI(2)=ZWHEEL1+TPCG_WHEELRIBHEIGHT                             1115
         AG_ZI(3)=ZWHEEL1+TPCG_WHEELRIBHEIGHT                             1115
         AG_ZI(4)=TPCG_LENGTHW/2                                          1115
         AG_RMN(1)=TPCG_WHEELR1-TPCG_WHEELTOTALRIBWIDTH/2                 1115
         AG_RMN(2)=TPCG_WHEELR1-TPCG_WHEELTOTALRIBWIDTH/2                 1115
         AG_RMN(3)=TPCG_WHEELR1-TPCG_WHEELRIBWIDTH/2                      1115
         AG_RMN(4)=TPCG_WHEELR1-TPCG_WHEELRIBWIDTH/2                      1115
         AG_RMX(1)=TPCG_WHEELR1+TPCG_WHEELTOTALRIBWIDTH/2                 1115
         AG_RMX(2)=TPCG_WHEELR1+TPCG_WHEELTOTALRIBWIDTH/2                 1115
         AG_RMX(3)=TPCG_WHEELR1+TPCG_WHEELRIBWIDTH/2                      1115
         AG_RMX(4)=TPCG_WHEELR1+TPCG_WHEELRIBWIDTH/2                      1115
         AG_TITLE = 'PGON'                                                1115
         AG_EXNAME = 'SHAPE'                                              1115
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1115
         CALL AXSHAPE                                                     1115
C    *                                                                    1115
5571  CALL AGSPOPB                                                        1116
      GO TO BLOCK_TWMR, (5570 ,5580)                                      1116
C End of Local Procedure                                                  1116
C ----------------------------------------------------------------------  1117
C ------------ TRDO                                                       1117
C Begin Local Procedure TRDO                                              1118
C TRDO TPCRDOASSEMBLY                                                     1118
5590  CONTINUE                                                            1118
         AG_TITLE='TRDO'                                                  1118
         AG_MODULE=MODULE                                                 1118
         AG_MARK='V00'                                                    1118
         CALL AGSPUSH                                                     1118
         AG_MODULE=MODULE                                                 1118
C    *                                                                    1118
C    Geant ATTRIBUTE : TRDO ===> SEEN=0 <===                              1119
         CALL AGSRESET                                                    1119
         AG_SEEN = 0                                                      1119
         AG_TITLE = 'TRDO'                                                1119
         AG_EXNAME = 'ATTRIBUTE'                                          1119
         AG_PARLIST = 'SEEN'                                              1119
         CALL AXATTRIBUTE                                                 1119
C    *                                                                    1119
C    Geant ATTRIBUTE : TRDO ===> SEEN=1 COLO=KYELLOW <===                 1119
         CALL AGSRESET                                                    1120
         AG_SEEN = 1                                                      1120
         AG_COLO = KYELLOW                                                1120
         AG_TITLE = 'TRDO'                                                1120
         AG_EXNAME = 'ATTRIBUTE'                                          1120
         AG_PARLIST = 'SEEN_COLO'                                         1120
         CALL AXATTRIBUTE                                                 1120
C    *                                                                    1120
C    Geant MEDIUM : TRDO_WHATEVER ===> DEEMAX=0.248853 STMIN=1.15055 <==  1120
C    =                                                                    1120
         CALL AGSRESET                                                    1121
         AG_DEEMAX = 0.248853                                             1121
         AG_STMIN = 1.15055                                               1121
         AG_TITLE = 'TRDO_WHATEVER'                                       1121
         AG_EXNAME = 'MEDIUM'                                             1121
         AG_PARLIST = 'DEEMAX_STMIN'                                      1121
         CALL AXMEDIUM                                                    1121
C    *                                                                    1121
C    Geant SHAPE : PGON ===> PHI1=-15 DPHI=30 NPDIV=1 NZ=2, ZI={ZZZRDO(1  1121
C    ),ZZZRDO(2)} RMN={RMNRDO(1),RMNRDO(2)}, RMX={RMXRDO(1),RMXRDO(2)} <  1121
C    ===                                                                  1121
         CALL AGSRESET                                                    1124
         AG_PHI1 = -15                                                    1124
         AG_DPHI = 30                                                     1124
         AG_NPDIV = 1                                                     1124
         AG_NZ = 2                                                        1124
         AG_ZI(1)=ZZZRDO(1)                                               1124
         AG_ZI(2)=ZZZRDO(2)                                               1124
         AG_RMN(1)=RMNRDO(1)                                              1124
         AG_RMN(2)=RMNRDO(2)                                              1124
         AG_RMX(1)=RMXRDO(1)                                              1124
         AG_RMX(2)=RMXRDO(2)                                              1124
         AG_TITLE = 'PGON'                                                1124
         AG_EXNAME = 'SHAPE'                                              1124
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1124
         CALL AXSHAPE                                                     1124
C    *                                                                    1124
         ISHAPE = KPARA                                                   1124
         MYPAR(1)=TPCG_RDOCOOLINGDY                                       1126
         MYPAR(2)=TPCG_RDOCOOLINGDX                                       1126
         MYPAR(3)=TPCG_RDOCOOLINGDZ                                       1126
         MYPAR(4)=15                                                      1126
         X = TPCG_WHEELR2 - TPCG_RDOCOOLINGDX                             1127
         Y = X*TAN15 - TPCG_RDOCOOLINGDY                                  1128
         Z = TPCG_ZRDO - 2*DY                                             1129
C    Create TCOO :                                                        1130
C    Call To Local Procedure TCOO                                         1130
         ASSIGN 6010 TO BLOCK_TCOO                                        1130
         GO TO  5620                                                      1130
6010     CONTINUE                                                         1130
C    *                                                                    1130
C    Geant POSITION : TCOO ===> X=X Y=Y Z=Z ORT=YX-Z <===                 1131
         CALL AGSRESET                                                    1131
         AG_X = X                                                         1131
         AG_Y = Y                                                         1131
         AG_Z = Z                                                         1131
         AG_ALPHAN=AG_ALPHAN+1                                            1131
         AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                                1131
         AG_TITLE = 'TCOO'                                                1131
         AG_EXNAME = 'POSITION'                                           1131
         AG_PARLIST = 'X_Y_Z_ORT'                                         1131
         CALL AXPOSITION                                                  1131
C    *                                                                    1131
         ISHAPE = KPARA                                                   1131
         MYPAR(1)=TPCG_RDOCOOLINGDY                                       1133
         MYPAR(2)=TPCG_RDOCOOLINGDX                                       1133
         MYPAR(3)=TPCG_RDOCOOLINGDZ                                       1133
         MYPAR(4)=-15                                                     1133
         X = TPCG_WHEELR2 - TPCG_RDOCOOLINGDX                             1134
         Y = X*TAN15 - TPCG_RDOCOOLINGDY                                  1135
         Z = TPCG_ZRDO - 2*DY                                             1136
C    Create TCOO :                                                        1137
C    Call To Local Procedure TCOO                                         1137
         ASSIGN 6020 TO BLOCK_TCOO                                        1137
         GO TO  5620                                                      1137
6020     CONTINUE                                                         1137
C    *                                                                    1137
C    Geant POSITION : TCOO ===> X=X Y=-Y Z=Z ORT=YX-Z <===                1138
         CALL AGSRESET                                                    1138
         AG_X = X                                                         1138
         AG_Y = -Y                                                        1138
         AG_Z = Z                                                         1138
         AG_ALPHAN=AG_ALPHAN+1                                            1138
         AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                                1138
         AG_TITLE = 'TCOO'                                                1138
         AG_EXNAME = 'POSITION'                                           1138
         AG_PARLIST = 'X_Y_Z_ORT'                                         1138
         CALL AXPOSITION                                                  1138
C    *                                                                    1138
         RCOOLINGTUBE = TPCG_WHEELR2 - TPCG_HEIGTUBE - 2*TPCG_DXRDO       1138
C    *                                                                    1140
         ICOO=0                                                           1140
            GO TO 6033                                                    1140
6031        ICOO=ICOO+1                                                   1140
6033        IF(ICOO-(7).GT.0) GO TO 6032                                  1140
            DZ = TPCG_HEIGTUBE*CM/2                                       1141
            DY = TPCG_WIDTUBE*CM/2                                        1142
            Z = RCOOLINGTUBE                                              1143
            DX1 = (Z-DZ)*TAN15 - 2*TPCG_RDOCOOLINGDY                      1144
            DX2 = (Z+DZ)*TAN15 - 2*TPCG_RDOCOOLINGDY                      1145
            ISHAPE = KTRD1                                                1145
            MYPAR(1)=DX1                                                  1147
            MYPAR(2)=DX2                                                  1147
            MYPAR(3)=DY                                                   1147
            MYPAR(4)=DZ                                                   1147
            MYPAR(11) = Z                                                 1147
            MYPAR(13) = TPCG_ZRDO - 2*DY                                  1148
C       Create TCOO :                                                     1149
C       Call To Local Procedure TCOO                                      1149
            ASSIGN 6040 TO BLOCK_TCOO                                     1149
            GO TO  5620                                                   1149
6040        CONTINUE                                                      1149
C       *                                                                 1149
C       Geant POSITION : TCOO ===> X=MYPAR(11) Y=0 Z=MYPAR(13) ORT=YZX <  1150
C       ===                                                               1150
            CALL AGSRESET                                                 1150
            AG_X = MYPAR(11)                                              1150
            AG_Y = 0                                                      1150
            AG_Z = MYPAR(13)                                              1150
            AG_ALPHAN=AG_ALPHAN+1                                         1150
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              1150
            AG_TITLE = 'TCOO'                                             1150
            AG_EXNAME = 'POSITION'                                        1150
            AG_PARLIST = 'X_Y_Z_ORT'                                      1150
            CALL AXPOSITION                                               1150
C       *                                                                 1150
C       Check (ICOO .NE. 0 .AND. ICOO .NE. 6)                             1150
            IF ((ICOO .NE. 0 .AND. ICOO .NE. 6)) THEN                     1151
            MYPAR(11) = Z + DZ + TPCG_DXRDO                               1151
            MYPAR(13) = TPCG_ZRDO-TPCG_DZRDO                              1153
C       Create TRDC :                                                     1154
C       Call To Local Procedure TRDC                                      1154
            ASSIGN 6060 TO BLOCK_TRDC                                     1154
            GO TO  6050                                                   1154
6060        CONTINUE                                                      1154
C       *                                                                 1154
C       Geant POSITION : TRDC ===> X=MYPAR(11) Y=0 Z=MYPAR(13) <===       1155
            CALL AGSRESET                                                 1155
            AG_X = MYPAR(11)                                              1155
            AG_Y = 0                                                      1155
            AG_Z = MYPAR(13)                                              1155
            AG_TITLE = 'TRDC'                                             1155
            AG_EXNAME = 'POSITION'                                        1155
            AG_PARLIST = 'X_Y_Z'                                          1155
            CALL AXPOSITION                                               1155
C       *                                                                 1155
            ENDIF                                                         1155
            RCOOLINGTUBE=RCOOLINGTUBE-DRDOCOOLING(ICOO)                   1157
         GO TO 6031                                                       1158
6032     CONTINUE                                                         1158
5591  CALL AGSPOPB                                                        1159
      GO TO BLOCK_TRDO, (5590 ,5600)                                      1159
C End of Local Procedure                                                  1159
C ----------------------------------------------------------------------  1160
C ------------ TBRW                                                       1160
C Begin Local Procedure TBRW                                              1161
C TBRW WHEELRIBBOX                                                        1161
5440  CONTINUE                                                            1161
         AG_TITLE='TBRW'                                                  1161
         AG_MODULE=MODULE                                                 1161
         AG_MARK='V00'                                                    1161
         CALL AGSPUSH                                                     1161
         AG_MODULE=MODULE                                                 1161
C    *                                                                    1161
C    Geant ATTRIBUTE : TBRW ===> SEEN=1 COLO=KVIOLET <===                 1162
         CALL AGSRESET                                                    1162
         AG_SEEN = 1                                                      1162
         AG_COLO = KVIOLET                                                1162
         AG_TITLE = 'TBRW'                                                1162
         AG_EXNAME = 'ATTRIBUTE'                                          1162
         AG_PARLIST = 'SEEN_COLO'                                         1162
         CALL AXATTRIBUTE                                                 1162
C    *                                                                    1162
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1162
         CALL AGSRESET                                                    1163
         AG_TITLE = 'ALUMINIUM'                                           1163
         AG_EXNAME = 'MATERIAL'                                           1163
         AG_PARLIST = 'NONE'                                              1163
         CALL AXMATERIAL                                                  1163
C    *                                                                    1163
C    Geant MEDIUM : TBRW_ALUMINIUM ===> DEEMAX=0.18296 STMIN=0.0334422 <  1163
C    ===                                                                  1163
         CALL AGSRESET                                                    1164
         AG_DEEMAX = 0.18296                                              1164
         AG_STMIN = 0.0334422                                             1164
         AG_TITLE = 'TBRW_ALUMINIUM'                                      1164
         AG_EXNAME = 'MEDIUM'                                             1164
         AG_PARLIST = 'DEEMAX_STMIN'                                      1164
         CALL AXMEDIUM                                                    1164
C    *                                                                    1164
C    Geant SHAPE : BOX ===> DX=TPCG_WHEELBOXDY/2 DY=TPCG_WHEELBOXDX/2, D  1164
C    Z=TPCG_WHEELTHK/2 <===                                               1164
         CALL AGSRESET                                                    1166
         AG_DX = TPCG_WHEELBOXDY/2                                        1166
         AG_DY = TPCG_WHEELBOXDX/2                                        1166
         AG_DZ = TPCG_WHEELTHK/2                                          1166
         AG_TITLE = 'BOX'                                                 1166
         AG_EXNAME = 'SHAPE'                                              1166
         AG_PARLIST = 'DX_DY_DZ'                                          1166
         CALL AXSHAPE                                                     1166
C    *                                                                    1166
5441  CALL AGSPOPB                                                        1167
      GO TO BLOCK_TBRW, (5440 ,5450)                                      1167
C End of Local Procedure                                                  1167
C ----------------------------------------------------------------------  1168
C ------------ TWRB                                                       1168
C Begin Local Procedure TWRB                                              1169
C TWRB TPCWHEELRIB                                                        1169
5460  CONTINUE                                                            1169
         AG_TITLE='TWRB'                                                  1169
         AG_MODULE=MODULE                                                 1169
         AG_MARK='V00'                                                    1169
         CALL AGSPUSH                                                     1169
         AG_MODULE=MODULE                                                 1169
C    *                                                                    1169
C    Geant ATTRIBUTE : TWRB ===> SEEN=1 COLO=KYELLOW <===                 1170
         CALL AGSRESET                                                    1170
         AG_SEEN = 1                                                      1170
         AG_COLO = KYELLOW                                                1170
         AG_TITLE = 'TWRB'                                                1170
         AG_EXNAME = 'ATTRIBUTE'                                          1170
         AG_PARLIST = 'SEEN_COLO'                                         1170
         CALL AXATTRIBUTE                                                 1170
C    *                                                                    1170
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1170
         CALL AGSRESET                                                    1171
         AG_TITLE = 'ALUMINIUM'                                           1171
         AG_EXNAME = 'MATERIAL'                                           1171
         AG_PARLIST = 'NONE'                                              1171
         CALL AXMATERIAL                                                  1171
C    *                                                                    1171
C    Geant MEDIUM : TWRB_ALUMINIUM ===> DEEMAX=0.18296 STMIN=0.0334422 <  1171
C    ===                                                                  1171
         CALL AGSRESET                                                    1172
         AG_DEEMAX = 0.18296                                              1172
         AG_STMIN = 0.0334422                                             1172
         AG_TITLE = 'TWRB_ALUMINIUM'                                      1172
         AG_EXNAME = 'MEDIUM'                                             1172
         AG_PARLIST = 'DEEMAX_STMIN'                                      1172
         CALL AXMEDIUM                                                    1172
C    *                                                                    1172
C    Geant SHAPE : PARA ===> DX=MYDX DY=MYDY DZ=MYDZ ALPH=MYALPH, THET=0  1172
C     PHI=0 <===                                                          1172
         CALL AGSRESET                                                    1174
         AG_DX = MYDX                                                     1174
         AG_DY = MYDY                                                     1174
         AG_DZ = MYDZ                                                     1174
         AG_ALPH = MYALPH                                                 1174
         AG_THET = 0                                                      1174
         AG_PHI = 0                                                       1174
         AG_TITLE = 'PARA'                                                1174
         AG_EXNAME = 'SHAPE'                                              1174
         AG_PARLIST = 'DX_DY_DZ_ALPH_THET_PHI'                            1174
         CALL AXSHAPE                                                     1174
C    *                                                                    1174
5461  CALL AGSPOPB                                                        1175
      GO TO BLOCK_TWRB, (5460 ,5470,5480)                                 1175
C End of Local Procedure                                                  1175
C ----------------------------------------------------------------------  1176
C ------------ TCOO                                                       1176
C Begin Local Procedure TCOO                                              1177
C TCOO COOLINGTUBE                                                        1177
5620  CONTINUE                                                            1177
         AG_TITLE='TCOO'                                                  1177
         AG_MODULE=MODULE                                                 1177
         AG_MARK='V00'                                                    1177
         CALL AGSPUSH                                                     1177
         AG_MODULE=MODULE                                                 1177
C    *                                                                    1177
C    Geant ATTRIBUTE : TCOO ===> SEEN=1 COLO=KRED <===                    1178
         CALL AGSRESET                                                    1178
         AG_SEEN = 1                                                      1178
         AG_COLO = KRED                                                   1178
         AG_TITLE = 'TCOO'                                                1178
         AG_EXNAME = 'ATTRIBUTE'                                          1178
         AG_PARLIST = 'SEEN_COLO'                                         1178
         CALL AXATTRIBUTE                                                 1178
C    *                                                                    1178
C    Geant MATERIAL : WATER_PIPE ===> inherited arguments used <===       1178
         CALL AGSRESET                                                    1179
         AG_TITLE = 'WATER_PIPE'                                          1179
         AG_EXNAME = 'MATERIAL'                                           1179
         AG_PARLIST = 'NONE'                                              1179
         CALL AXMATERIAL                                                  1179
C    *                                                                    1179
C    Geant MEDIUM : TCOO_H2O_PIPE ===> DEEMAX=0.184746 STMIN=0.0348298 <  1179
C    ===                                                                  1179
         CALL AGSRESET                                                    1180
         AG_DEEMAX = 0.184746                                             1180
         AG_STMIN = 0.0348298                                             1180
         AG_TITLE = 'TCOO_H2O_PIPE'                                       1180
         AG_EXNAME = 'MEDIUM'                                             1180
         AG_PARLIST = 'DEEMAX_STMIN'                                      1180
         CALL AXMEDIUM                                                    1180
C    *                                                                    1180
C    Check ISHAPE==KPARA                                                  1180
         IF (ISHAPE.EQ.KPARA) THEN                                        1181
C    Geant SHAPE : PARA ===> DX=MYPAR(1) DY=MYPAR(2) DZ=MYPAR(3), ALPH=M  1183
C    YPAR(4) THET=0 PHI=0 <===                                            1183
         CALL AGSRESET                                                    1183
         AG_DX = MYPAR(1)                                                 1183
         AG_DY = MYPAR(2)                                                 1183
         AG_DZ = MYPAR(3)                                                 1183
         AG_ALPH = MYPAR(4)                                               1183
         AG_THET = 0                                                      1183
         AG_PHI = 0                                                       1183
         AG_TITLE = 'PARA'                                                1183
         AG_EXNAME = 'SHAPE'                                              1183
         AG_PARLIST = 'DX_DY_DZ_ALPH_THET_PHI'                            1183
         CALL AXSHAPE                                                     1183
C    *                                                                    1183
         ENDIF                                                            1183
C    Check ISHAPE==KTRD1                                                  1185
         IF (ISHAPE.EQ.KTRD1) THEN                                        1185
C    Geant SHAPE : TRD1 ===> DX1=MYPAR(1) DX2=MYPAR(2) DY=MYPAR(3), DZ=M  1187
C    YPAR(4) <===                                                         1187
         CALL AGSRESET                                                    1187
         AG_DX1 = MYPAR(1)                                                1187
         AG_DX2 = MYPAR(2)                                                1187
         AG_DY = MYPAR(3)                                                 1187
         AG_DZ = MYPAR(4)                                                 1187
         AG_TITLE = 'TRD1'                                                1187
         AG_EXNAME = 'SHAPE'                                              1187
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     1187
         CALL AXSHAPE                                                     1187
C    *                                                                    1187
         ENDIF                                                            1187
5621  CALL AGSPOPB                                                        1189
      GO TO BLOCK_TCOO, (5620 ,5630,5800,5820,6010,6020,6040)             1189
C End of Local Procedure                                                  1189
C ----------------------------------------------------------------------  1190
C ------------ TCAB                                                       1190
C Begin Local Procedure TCAB                                              1191
C TCAB CABLES                                                             1191
5650  CONTINUE                                                            1191
         AG_TITLE='TCAB'                                                  1191
         AG_MODULE=MODULE                                                 1191
         AG_MARK='V00'                                                    1191
         CALL AGSPUSH                                                     1191
         AG_MODULE=MODULE                                                 1191
C    *                                                                    1191
C    Geant ATTRIBUTE : TCAB ===> SEEN=1 COLO=KGREEN <===                  1192
         CALL AGSRESET                                                    1192
         AG_SEEN = 1                                                      1192
         AG_COLO = KGREEN                                                 1192
         AG_TITLE = 'TCAB'                                                1192
         AG_EXNAME = 'ATTRIBUTE'                                          1192
         AG_PARLIST = 'SEEN_COLO'                                         1192
         CALL AXATTRIBUTE                                                 1192
C    *                                                                    1192
C    Geant MATERIAL : CABLES ===> inherited arguments used <===           1192
         CALL AGSRESET                                                    1193
         AG_TITLE = 'CABLES'                                              1193
         AG_EXNAME = 'MATERIAL'                                           1193
         AG_PARLIST = 'NONE'                                              1193
         CALL AXMATERIAL                                                  1193
C    *                                                                    1193
C    Geant MEDIUM : TCAB_CABLES ===> DEEMAX=0.172513 STMIN=0.0388322 <==  1193
C    =                                                                    1193
         CALL AGSRESET                                                    1194
         AG_DEEMAX = 0.172513                                             1194
         AG_STMIN = 0.0388322                                             1194
         AG_TITLE = 'TCAB_CABLES'                                         1194
         AG_EXNAME = 'MEDIUM'                                             1194
         AG_PARLIST = 'DEEMAX_STMIN'                                      1194
         CALL AXMEDIUM                                                    1194
C    *                                                                    1194
C    Geant SHAPE : PARA ===> DX=MYPAR(1) DY=MYPAR(2) DZ=MYPAR(3), ALPH=M  1194
C    YPAR(4) THET=0 PHI=0 <===                                            1194
         CALL AGSRESET                                                    1196
         AG_DX = MYPAR(1)                                                 1196
         AG_DY = MYPAR(2)                                                 1196
         AG_DZ = MYPAR(3)                                                 1196
         AG_ALPH = MYPAR(4)                                               1196
         AG_THET = 0                                                      1196
         AG_PHI = 0                                                       1196
         AG_TITLE = 'PARA'                                                1196
         AG_EXNAME = 'SHAPE'                                              1196
         AG_PARLIST = 'DX_DY_DZ_ALPH_THET_PHI'                            1196
         CALL AXSHAPE                                                     1196
C    *                                                                    1196
5651  CALL AGSPOPB                                                        1197
      GO TO BLOCK_TCAB, (5650 ,5660)                                      1197
C End of Local Procedure                                                  1197
C ----------------------------------------------------------------------  1198
C ------------ TRDC                                                       1198
C Begin Local Procedure TRDC                                              1199
C TRDC RDOCARD                                                            1199
6050  CONTINUE                                                            1199
         AG_TITLE='TRDC'                                                  1199
         AG_MODULE=MODULE                                                 1199
         AG_MARK='V00'                                                    1199
         CALL AGSPUSH                                                     1199
         AG_MODULE=MODULE                                                 1199
C    *                                                                    1199
C    Geant ATTRIBUTE : TRDC ===> SEEN=1 COLO=KBLUE <===                   1200
         CALL AGSRESET                                                    1200
         AG_SEEN = 1                                                      1200
         AG_COLO = KBLUE                                                  1200
         AG_TITLE = 'TRDC'                                                1200
         AG_EXNAME = 'ATTRIBUTE'                                          1200
         AG_PARLIST = 'SEEN_COLO'                                         1200
         CALL AXATTRIBUTE                                                 1200
C    *                                                                    1200
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1200
         CALL AGSRESET                                                    1201
         AG_TITLE = 'G10'                                                 1201
         AG_EXNAME = 'MATERIAL'                                           1201
         AG_PARLIST = 'NONE'                                              1201
         CALL AXMATERIAL                                                  1201
C    *                                                                    1201
C    Geant MEDIUM : TRDC_G10 ===> DEEMAX=0.20313 STMIN=0.0333806 <===     1201
         CALL AGSRESET                                                    1202
         AG_DEEMAX = 0.20313                                              1202
         AG_STMIN = 0.0333806                                             1202
         AG_TITLE = 'TRDC_G10'                                            1202
         AG_EXNAME = 'MEDIUM'                                             1202
         AG_PARLIST = 'DEEMAX_STMIN'                                      1202
         CALL AXMEDIUM                                                    1202
C    *                                                                    1202
C    Geant SHAPE : BOX ===> DX=TPCG_DXRDO DY=TPCG_DYRDO DZ=TPCG_DZRDO <=  1202
C    ==                                                                   1202
         CALL AGSRESET                                                    1203
         AG_DX = TPCG_DXRDO                                               1203
         AG_DY = TPCG_DYRDO                                               1203
         AG_DZ = TPCG_DZRDO                                               1203
         AG_TITLE = 'BOX'                                                 1203
         AG_EXNAME = 'SHAPE'                                              1203
         AG_PARLIST = 'DX_DY_DZ'                                          1203
         CALL AXSHAPE                                                     1203
C    *                                                                    1203
6051  CALL AGSPOPB                                                        1204
      GO TO BLOCK_TRDC, (6050 ,6060)                                      1204
C End of Local Procedure                                                  1204
C ----------------------------------------------------------------------  1205
C ------------ TRIB                                                       1205
C Begin Local Procedure TRIB                                              1206
C TRIB TPC RIBS                                                           1206
5720  CONTINUE                                                            1206
         AG_TITLE='TRIB'                                                  1206
         AG_MODULE=MODULE                                                 1206
         AG_MARK='V00'                                                    1206
         CALL AGSPUSH                                                     1206
         AG_MODULE=MODULE                                                 1206
C    *                                                                    1206
C    Geant ATTRIBUTE : TRIB ===> SEEN=1 COLO=KRED <===                    1207
         CALL AGSRESET                                                    1207
         AG_SEEN = 1                                                      1207
         AG_COLO = KRED                                                   1207
         AG_TITLE = 'TRIB'                                                1207
         AG_EXNAME = 'ATTRIBUTE'                                          1207
         AG_PARLIST = 'SEEN_COLO'                                         1207
         CALL AXATTRIBUTE                                                 1207
C    *                                                                    1207
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1207
         CALL AGSRESET                                                    1208
         AG_TITLE = 'ALUMINIUM'                                           1208
         AG_EXNAME = 'MATERIAL'                                           1208
         AG_PARLIST = 'NONE'                                              1208
         CALL AXMATERIAL                                                  1208
C    *                                                                    1208
C    Geant MEDIUM : TRIB_ALUMINIUM ===> DEEMAX=0.18296 STMIN=0.0334422 <  1208
C    ===                                                                  1208
         CALL AGSRESET                                                    1209
         AG_DEEMAX = 0.18296                                              1209
         AG_STMIN = 0.0334422                                             1209
         AG_TITLE = 'TRIB_ALUMINIUM'                                      1209
         AG_EXNAME = 'MEDIUM'                                             1209
         AG_PARLIST = 'DEEMAX_STMIN'                                      1209
         CALL AXMEDIUM                                                    1209
C    *                                                                    1209
C    Check ISHAPE==KPARA                                                  1209
         IF (ISHAPE.EQ.KPARA) THEN                                        1210
C    Geant SHAPE : PARA ===> DX=MYPAR(1) DY=MYPAR(2) DZ=MYPAR(3), ALPH=M  1212
C    YPAR(4) THET=0 PHI=0 <===                                            1212
         CALL AGSRESET                                                    1212
         AG_DX = MYPAR(1)                                                 1212
         AG_DY = MYPAR(2)                                                 1212
         AG_DZ = MYPAR(3)                                                 1212
         AG_ALPH = MYPAR(4)                                               1212
         AG_THET = 0                                                      1212
         AG_PHI = 0                                                       1212
         AG_TITLE = 'PARA'                                                1212
         AG_EXNAME = 'SHAPE'                                              1212
         AG_PARLIST = 'DX_DY_DZ_ALPH_THET_PHI'                            1212
         CALL AXSHAPE                                                     1212
C    *                                                                    1212
         ELSEIF (ISHAPE.EQ.KTRD1) THEN                                    1212
C    Geant SHAPE : TRD1 ===> DX1=MYPAR(1) DX2=MYPAR(2) DY=MYPAR(3), DZ=M  1215
C    YPAR(4) <===                                                         1215
         CALL AGSRESET                                                    1215
         AG_DX1 = MYPAR(1)                                                1215
         AG_DX2 = MYPAR(2)                                                1215
         AG_DY = MYPAR(3)                                                 1215
         AG_DZ = MYPAR(4)                                                 1215
         AG_TITLE = 'TRD1'                                                1215
         AG_EXNAME = 'SHAPE'                                              1215
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     1215
         CALL AXSHAPE                                                     1215
C    *                                                                    1215
         ELSEIF (ISHAPE.EQ.KBBOX) THEN                                    1215
C    Geant SHAPE : BOX ===> DX=MYPAR(1) DY=MYPAR(2) DZ=MYPAR(3) <===      1217
         CALL AGSRESET                                                    1217
         AG_DX = MYPAR(1)                                                 1217
         AG_DY = MYPAR(2)                                                 1217
         AG_DZ = MYPAR(3)                                                 1217
         AG_TITLE = 'BOX'                                                 1217
         AG_EXNAME = 'SHAPE'                                              1217
         AG_PARLIST = 'DX_DY_DZ'                                          1217
         CALL AXSHAPE                                                     1217
C    *                                                                    1217
         ENDIF                                                            1217
5721  CALL AGSPOPB                                                        1219
      GO TO BLOCK_TRIB, (5720 ,5730,5770,5780)                            1219
C End of Local Procedure                                                  1219
C ----------------------------------------------------------------------  1220
C ------------ TWIR                                                       1220
C Begin Local Procedure TWIR                                              1221
C TWIR WIREMOUNT                                                          1221
5740  CONTINUE                                                            1221
         AG_TITLE='TWIR'                                                  1221
         AG_MODULE=MODULE                                                 1221
         AG_MARK='V00'                                                    1221
         CALL AGSPUSH                                                     1221
         AG_MODULE=MODULE                                                 1221
C    *                                                                    1221
C    Geant ATTRIBUTE : TWIR ===> SEEN=1 COLO=KYELLOW <===                 1222
         CALL AGSRESET                                                    1222
         AG_SEEN = 1                                                      1222
         AG_COLO = KYELLOW                                                1222
         AG_TITLE = 'TWIR'                                                1222
         AG_EXNAME = 'ATTRIBUTE'                                          1222
         AG_PARLIST = 'SEEN_COLO'                                         1222
         CALL AXATTRIBUTE                                                 1222
C    *                                                                    1222
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1222
         CALL AGSRESET                                                    1223
         AG_TITLE = 'G10'                                                 1223
         AG_EXNAME = 'MATERIAL'                                           1223
         AG_PARLIST = 'NONE'                                              1223
         CALL AXMATERIAL                                                  1223
C    *                                                                    1223
C    Geant MEDIUM : TWIR_G10 ===> DEEMAX=0.20313 STMIN=0.0333806 <===     1223
         CALL AGSRESET                                                    1224
         AG_DEEMAX = 0.20313                                              1224
         AG_STMIN = 0.0333806                                             1224
         AG_TITLE = 'TWIR_G10'                                            1224
         AG_EXNAME = 'MEDIUM'                                             1224
         AG_PARLIST = 'DEEMAX_STMIN'                                      1224
         CALL AXMEDIUM                                                    1224
C    *                                                                    1224
C    Geant SHAPE : PARA ===> DX=MYPAR(1) DY=MYPAR(2) DZ=MYPAR(3), ALPH=M  1224
C    YPAR(4) THET=0 PHI=0 <===                                            1224
         CALL AGSRESET                                                    1226
         AG_DX = MYPAR(1)                                                 1226
         AG_DY = MYPAR(2)                                                 1226
         AG_DZ = MYPAR(3)                                                 1226
         AG_ALPH = MYPAR(4)                                               1226
         AG_THET = 0                                                      1226
         AG_PHI = 0                                                       1226
         AG_TITLE = 'PARA'                                                1226
         AG_EXNAME = 'SHAPE'                                              1226
         AG_PARLIST = 'DX_DY_DZ_ALPH_THET_PHI'                            1226
         CALL AXSHAPE                                                     1226
C    *                                                                    1226
5741  CALL AGSPOPB                                                        1227
      GO TO BLOCK_TWIR, (5740 ,5750)                                      1227
C End of Local Procedure                                                  1227
C ----------------------------------------------------------------------  1228
C ------------ THOL                                                       1228
C Begin Local Procedure THOL                                              1229
C THOL THE SECTOR HOLES FOR FEE                                           1229
5940  CONTINUE                                                            1229
         AG_TITLE='THOL'                                                  1229
         AG_MODULE=MODULE                                                 1229
         AG_MARK='V00'                                                    1229
         CALL AGSPUSH                                                     1229
         AG_MODULE=MODULE                                                 1229
C    *                                                                    1229
C    Geant ATTRIBUTE : THOL ===> SEEN=1 COLO=KGREEN <===                  1230
         CALL AGSRESET                                                    1230
         AG_SEEN = 1                                                      1230
         AG_COLO = KGREEN                                                 1230
         AG_TITLE = 'THOL'                                                1230
         AG_EXNAME = 'ATTRIBUTE'                                          1230
         AG_PARLIST = 'SEEN_COLO'                                         1230
         CALL AXATTRIBUTE                                                 1230
C    *                                                                    1230
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1230
         CALL AGSRESET                                                    1231
         AG_TITLE = 'AIR'                                                 1231
         AG_EXNAME = 'MATERIAL'                                           1231
         AG_PARLIST = 'NONE'                                              1231
         CALL AXMATERIAL                                                  1231
C    *                                                                    1231
C    Geant MEDIUM : THOL_AIR ===> DEEMAX=0.248853 STMIN=1.15055 <===      1231
         CALL AGSRESET                                                    1232
         AG_DEEMAX = 0.248853                                             1232
         AG_STMIN = 1.15055                                               1232
         AG_TITLE = 'THOL_AIR'                                            1232
         AG_EXNAME = 'MEDIUM'                                             1232
         AG_PARLIST = 'DEEMAX_STMIN'                                      1232
         CALL AXMEDIUM                                                    1232
C    *                                                                    1232
C    Geant SHAPE : BOX ===> DX=MYPAR(1) DY=MYPAR(2) DZ=MYPAR(3) <===      1232
         CALL AGSRESET                                                    1233
         AG_DX = MYPAR(1)                                                 1233
         AG_DY = MYPAR(2)                                                 1233
         AG_DZ = MYPAR(3)                                                 1233
         AG_TITLE = 'BOX'                                                 1233
         AG_EXNAME = 'SHAPE'                                              1233
         AG_PARLIST = 'DX_DY_DZ'                                          1233
         CALL AXSHAPE                                                     1233
C    *                                                                    1233
C    Create FEES :                                                        1234
C    Call To Local Procedure FEES                                         1234
         ASSIGN 6080 TO BLOCK_FEES                                        1234
         GO TO  6070                                                      1234
6080     CONTINUE                                                         1234
C    *                                                                    1234
C    Geant POSITION : FEES ===> inherited arguments used <===             1235
         CALL AGSRESET                                                    1235
         AG_TITLE = 'FEES'                                                1235
         AG_EXNAME = 'POSITION'                                           1235
         AG_PARLIST = 'NONE'                                              1235
         CALL AXPOSITION                                                  1235
C    *                                                                    1235
5941  CALL AGSPOPB                                                        1236
      GO TO BLOCK_THOL, (5940 ,5950)                                      1236
C End of Local Procedure                                                  1236
C ----------------------------------------------------------------------  1237
C ------------ FEES                                                       1237
C Begin Local Procedure FEES                                              1238
C FEES TFEESOCKET                                                         1238
6070  CONTINUE                                                            1238
         AG_TITLE='FEES'                                                  1238
         AG_MODULE=MODULE                                                 1238
         AG_MARK='V00'                                                    1238
         CALL AGSPUSH                                                     1238
         AG_MODULE=MODULE                                                 1238
C    *                                                                    1238
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1239
         CALL AGSRESET                                                    1239
         AG_TITLE = 'G10'                                                 1239
         AG_EXNAME = 'MATERIAL'                                           1239
         AG_PARLIST = 'NONE'                                              1239
         CALL AXMATERIAL                                                  1239
C    *                                                                    1239
C    Geant MEDIUM : FEES_G10 ===> DEEMAX=0.20313 STMIN=0.0333806 <===     1239
         CALL AGSRESET                                                    1240
         AG_DEEMAX = 0.20313                                              1240
         AG_STMIN = 0.0333806                                             1240
         AG_TITLE = 'FEES_G10'                                            1240
         AG_EXNAME = 'MEDIUM'                                             1240
         AG_PARLIST = 'DEEMAX_STMIN'                                      1240
         CALL AXMEDIUM                                                    1240
C    *                                                                    1240
C    Geant ATTRIBUTE : FEES ===> SEEN=1 COLO=KBLUE <===                   1240
         CALL AGSRESET                                                    1241
         AG_SEEN = 1                                                      1241
         AG_COLO = KBLUE                                                  1241
         AG_TITLE = 'FEES'                                                1241
         AG_EXNAME = 'ATTRIBUTE'                                          1241
         AG_PARLIST = 'SEEN_COLO'                                         1241
         CALL AXATTRIBUTE                                                 1241
C    *                                                                    1241
C    Geant SHAPE : BOX ===> DX=6.2/20 DY=57./20 DZ=5.8/20 <===            1241
         CALL AGSRESET                                                    1242
         AG_DX = 6.2/20                                                   1242
         AG_DY = 57./20                                                   1242
         AG_DZ = 5.8/20                                                   1242
         AG_TITLE = 'BOX'                                                 1242
         AG_EXNAME = 'SHAPE'                                              1242
         AG_PARLIST = 'DX_DY_DZ'                                          1242
         CALL AXSHAPE                                                     1242
C    *                                                                    1242
6071  CALL AGSPOPB                                                        1243
      GO TO BLOCK_FEES, (6070 ,6080)                                      1243
C End of Local Procedure                                                  1243
C ----------------------------------------------------------------------  1244
C ------------ TPGV                                                       1244
C Begin Local Procedure TPGV                                              1245
C TPGV IS THE GAS VOLUME PLACED IN TPC                                    1245
5050  CONTINUE                                                            1245
         AG_TITLE='TPGV'                                                  1245
         AG_MODULE=MODULE                                                 1245
         AG_MARK='V00'                                                    1245
         CALL AGSPUSH                                                     1245
         AG_MODULE=MODULE                                                 1245
C    *                                                                    1245
C    Geant ATTRIBUTE : TPGV ===> SEEN=1 COLO=KRED <===                    1246
         CALL AGSRESET                                                    1246
         AG_SEEN = 1                                                      1246
         AG_COLO = KRED                                                   1246
         AG_TITLE = 'TPGV'                                                1246
         AG_EXNAME = 'ATTRIBUTE'                                          1246
         AG_PARLIST = 'SEEN_COLO'                                         1246
         CALL AXATTRIBUTE                                                 1246
C    *                                                                    1246
C    Geant MATERIAL : P10 ===> inherited arguments used <===              1246
         CALL AGSRESET                                                    1247
         AG_TITLE = 'P10'                                                 1247
         AG_EXNAME = 'MATERIAL'                                           1247
         AG_PARLIST = 'NONE'                                              1247
         CALL AXMATERIAL                                                  1247
C    *                                                                    1247
C    Geant MEDIUM : TPGV_P10 ===> DEEMAX=0.248246 STMIN=1.60594 <===      1247
         CALL AGSRESET                                                    1248
         AG_DEEMAX = 0.248246                                             1248
         AG_STMIN = 1.60594                                               1248
         AG_TITLE = 'TPGV_P10'                                            1248
         AG_EXNAME = 'MEDIUM'                                             1248
         AG_PARLIST = 'DEEMAX_STMIN'                                      1248
         CALL AXMEDIUM                                                    1248
C    *                                                                    1248
C    Geant SHAPE : TUBE ===> RMIN=TPGVIR RMAX=TPCG_SENGASOR, DZ=TPGVLENG  1248
C    /2 <===                                                              1248
         CALL AGSRESET                                                    1250
         AG_RMIN = TPGVIR                                                 1250
         AG_RMAX = TPCG_SENGASOR                                          1250
         AG_DZ = TPGVLENG/2                                               1250
         AG_TITLE = 'TUBE'                                                1250
         AG_EXNAME = 'SHAPE'                                              1250
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1250
         CALL AXSHAPE                                                     1250
C    *                                                                    1250
C    *                                                                    1250
         ISECANG=15                                                       1251
            GOTO 6093                                                     1251
6091        ISECANG=ISECANG+(30)                                          1251
6093        IF((30)*(ISECANG-(360-15)).GT.0) GO TO 6092                   1251
C       Create TPSS :                                                     1252
C       Call To Local Procedure TPSS                                      1252
            ASSIGN 6110 TO BLOCK_TPSS                                     1252
            GO TO  6100                                                   1252
6110        CONTINUE                                                      1252
C       *                                                                 1252
C       Geant POSITION : TPSS ===> ALPHAZ=ISECANG KONLY='MANY' <===       1253
            CALL AGSRESET                                                 1253
            AG_ALPHAN=AG_ALPHAN+1                                         1253
            AG_ALPHAK(AG_ALPHAN)=3                                        1253
            AG_ALPHAL(AG_ALPHAN)=ISECANG                                  1253
            AG_KONLY = 'MANY'                                             1253
            AG_TITLE = 'TPSS'                                             1253
            AG_EXNAME = 'POSITION'                                        1253
            AG_PARLIST = 'ALPHAZ_KONLY'                                   1253
            CALL AXPOSITION                                               1253
C       *                                                                 1253
         GO TO 6091                                                       1254
6092     CONTINUE                                                         1254
5051  CALL AGSPOPB                                                        1255
      GO TO BLOCK_TPGV, (5050 ,5060)                                      1255
C End of Local Procedure                                                  1255
C ----------------------------------------------------------------------  1256
C ------------ TPSS                                                       1256
C Begin Local Procedure TPSS                                              1257
C TPSS IS A DIVISION OF GAS VOLUME CORRESPONDING TO A SUPERSECTORS        1257
6100  CONTINUE                                                            1257
         AG_TITLE='TPSS'                                                  1257
         AG_MODULE=MODULE                                                 1257
         AG_MARK='V00'                                                    1257
         CALL AGSPUSH                                                     1257
         AG_MODULE=MODULE                                                 1257
C    *                                                                    1257
C    Geant ATTRIBUTE : TPSS ===> SEEN=1 COLO=KBLUE <===                   1258
         CALL AGSRESET                                                    1258
         AG_SEEN = 1                                                      1258
         AG_COLO = KBLUE                                                  1258
         AG_TITLE = 'TPSS'                                                1258
         AG_EXNAME = 'ATTRIBUTE'                                          1258
         AG_PARLIST = 'SEEN_COLO'                                         1258
         CALL AXATTRIBUTE                                                 1258
C    *                                                                    1258
C    Geant MEDIUM : TPSS_WHATEVER ===> DEEMAX=0.248246 STMIN=1.60594 <==  1258
C    =                                                                    1258
         CALL AGSRESET                                                    1259
         AG_DEEMAX = 0.248246                                             1259
         AG_STMIN = 1.60594                                               1259
         AG_TITLE = 'TPSS_WHATEVER'                                       1259
         AG_EXNAME = 'MEDIUM'                                             1259
         AG_PARLIST = 'DEEMAX_STMIN'                                      1259
         CALL AXMEDIUM                                                    1259
C    *                                                                    1259
C    Geant SHAPE : TUBS ===> PHI1=-15 PHI2=15 <===                        1259
         CALL AGSRESET                                                    1260
         AG_PHI1 = -15                                                    1260
         AG_PHI2 = 15                                                     1260
         AG_TITLE = 'TUBS'                                                1260
         AG_EXNAME = 'SHAPE'                                              1260
         AG_PARLIST = 'PHI1_PHI2'                                         1260
         CALL AXSHAPE                                                     1260
C    *                                                                    1260
C    *                                                                    1260
         KASE=1                                                           1261
            GO TO 6123                                                    1261
6121        KASE=KASE+1                                                   1261
6123        IF(KASE-(2).GT.0) GO TO 6122                                  1261
C       *                                                                 1262
            I_SEC=1                                                       1262
               GO TO 6133                                                 1262
6131           I_SEC=I_SEC+1                                              1262
6133           IF(I_SEC-(2).GT.0) GO TO 6132                              1262
C          use TPRS bank                                                  1263
               BPATH_TPRS='TPRS*'                                         1263
               IDNUM_TPRS(1)=0                                            1263
               TPRS_SEC=I_SEC                                             1263
               CALL AGDGETP('TPCXGEO1','*',-999,'TPRS',NTPRS_SEC,         1263
     *         RE_TPRS_SEC,BPATH_TPRS,IDNUM_TPRS,1,MAP_TPRS,NAMES_TPRS,   1263
     *         BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)          1263
C          use TECW bank                                                  1264
               BPATH_TECW='TECW*'                                         1264
               IDNUM_TECW(1)=0                                            1264
               TECW_SEC=I_SEC                                             1264
               CALL AGDGETP('TPCXGEO1','*',-999,'TECW',NTECW_SEC,         1264
     *         RE_TECW_SEC,BPATH_TECW,IDNUM_TECW,1,MAP_TECW,NAMES_TECW,   1264
     *         BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)          1264
               ZWHEEL1 = TPCG_LENGTHW/2 - TPCG_WHEELTHK                   1265
               ZBEG = TPCG_MEMBTHK/2                                      1266
               ZEND = TPCG_ZGROUNDGRID + TPRS_DANODE*2                    1267
               ZDED = TPCG_ZGATINGGRID - TPCG_DEADZONE                    1268
               ZPMT = TPCG_ZGATINGGRID                                    1269
               DX = TPRS_WIDTH/2                                          1270
C          Check KASE==1                                                  1271
               IF (KASE.EQ.1) THEN                                        1271
               DZ = (ZDED - ZBEG)/2                                       1271
               Z = (ZDED + ZBEG)/2 - TPGVZ                                1273
               ENDIF                                                      1274
C          Check KASE==2                                                  1275
               IF (KASE.EQ.2) THEN                                        1275
               DZ = (ZEND - ZPMT)/2                                       1275
               Z = (ZEND + ZPMT)/2 - TPGVZ                                1277
               ENDIF                                                      1278
C          *                                                              1279
               I_ROW=1                                                    1279
                  GO TO 6143                                              1279
6141              I_ROW=I_ROW+1                                           1279
6143              IF(I_ROW-(NINT(TPRS_NROW)).GT.0) GO TO 6142             1279
C             Check (I_ROW==1)                                            1280
                  IF ((I_ROW.EQ.1)) THEN                                  1280
                  DY=TPRS_NPADS(I_ROW)*TPRS_PITCH/2                       1281
                  X=TPRS_RPADS(I_ROW)-TPRS_WIDTH                          1282
C             Create TPAD :                                               1283
C             Call To Local Procedure TPAD                                1283
                  ASSIGN 6160 TO BLOCK_TPAD                               1283
                  GO TO  6150                                             1283
6160              CONTINUE                                                1283
C             *                                                           1283
C             Geant POSITION : TPAD ===> X=X Z=Z DZ=DZ DX=DX DY=DY <===   1284
                  CALL AGSRESET                                           1284
                  AG_X = X                                                1284
                  AG_Z = Z                                                1284
                  AG_DZ = DZ                                              1284
                  AG_DX = DX                                              1284
                  AG_DY = DY                                              1284
                  AG_TITLE = 'TPAD'                                       1284
                  AG_EXNAME = 'POSITION'                                  1284
                  AG_PARLIST = 'X_Z_DZ_DX_DY'                             1284
                  CALL AXPOSITION                                         1284
C             *                                                           1284
                  ENDIF                                                   1284
                  DY=TPRS_NPADS(I_ROW)*TPRS_PITCH/2                       1286
                  X=TPRS_RPADS(I_ROW)                                     1287
C             Create TPAD :                                               1288
C             Call To Local Procedure TPAD                                1288
                  ASSIGN 6170 TO BLOCK_TPAD                               1288
                  GO TO  6150                                             1288
6170              CONTINUE                                                1288
C             *                                                           1288
C             Geant POSITION : TPAD ===> X=X Z=Z DZ=DZ DX=DX DY=DY <===   1289
                  CALL AGSRESET                                           1289
                  AG_X = X                                                1289
                  AG_Z = Z                                                1289
                  AG_DZ = DZ                                              1289
                  AG_DX = DX                                              1289
                  AG_DY = DY                                              1289
                  AG_TITLE = 'TPAD'                                       1289
                  AG_EXNAME = 'POSITION'                                  1289
                  AG_PARLIST = 'X_Z_DZ_DX_DY'                             1289
                  CALL AXPOSITION                                         1289
C             *                                                           1289
C             Check (I_ROW==NINT(TPRS_NROW))                              1289
                  IF ((I_ROW.EQ.NINT(TPRS_NROW))) THEN                    1290
                  X=TPRS_RPADS(I_ROW)+TPRS_WIDTH                          1291
                  DY=TPRS_NPADS(I_ROW)*TPRS_PITCH/2                       1292
C             Create TPAD :                                               1293
C             Call To Local Procedure TPAD                                1293
                  ASSIGN 6180 TO BLOCK_TPAD                               1293
                  GO TO  6150                                             1293
6180              CONTINUE                                                1293
C             *                                                           1293
C             Geant POSITION : TPAD ===> X=X Z=Z DZ=DZ DX=DX DY=DY <===   1294
                  CALL AGSRESET                                           1294
                  AG_X = X                                                1294
                  AG_Z = Z                                                1294
                  AG_DZ = DZ                                              1294
                  AG_DX = DX                                              1294
                  AG_DY = DY                                              1294
                  AG_TITLE = 'TPAD'                                       1294
                  AG_EXNAME = 'POSITION'                                  1294
                  AG_PARLIST = 'X_Z_DZ_DX_DY'                             1294
                  CALL AXPOSITION                                         1294
C             *                                                           1294
                  ENDIF                                                   1294
               GO TO 6141                                                 1296
6142           CONTINUE                                                   1296
            GO TO 6131                                                    1297
6132        CONTINUE                                                      1297
         GO TO 6121                                                       1298
6122     CONTINUE                                                         1298
6101  CALL AGSPOPB                                                        1299
      GO TO BLOCK_TPSS, (6100 ,6110)                                      1299
C End of Local Procedure                                                  1299
C ----------------------------------------------------------------------  1300
C ------------ TPAD                                                       1300
C Begin Local Procedure TPAD                                              1301
C TPAD IS A REAL PADROW WITH DIMENSIONS DEFINED AT POSITIONING TIME       1301
6150  CONTINUE                                                            1301
         AG_TITLE='TPAD'                                                  1301
         AG_MODULE=MODULE                                                 1301
         AG_MARK='V00'                                                    1301
         CALL AGSPUSH                                                     1301
         AG_MODULE=MODULE                                                 1301
C    *                                                                    1301
C    *                                                                    1302
C       Check TPRS_WIDTH<1.5                                              1302
            IF (TPRS_WIDTH.LT.1.5) THEN                                   1302
C       Geant ATTRIBUTE : TPAD ===> SEEN=1 COLO=2 SERIAL=1 <===           1303
            CALL AGSRESET                                                 1303
            AG_SEEN = 1                                                   1303
            AG_COLO = 2                                                   1303
            AG_SERIAL = 1                                                 1303
            AG_TITLE = 'TPAD'                                             1303
            AG_EXNAME = 'ATTRIBUTE'                                       1303
            AG_PARLIST = 'SEEN_COLO_SERIAL'                               1303
            CALL AXATTRIBUTE                                              1303
C       *                                                                 1303
         END IF                                                           1304
C    *                                                                    1305
C       Check TPRS_WIDTH>1.5                                              1305
            IF (TPRS_WIDTH.GT.1.5) THEN                                   1305
C       Geant ATTRIBUTE : TPAD ===> SEEN=1 COLO=2 SERIAL=2 <===           1306
            CALL AGSRESET                                                 1306
            AG_SEEN = 1                                                   1306
            AG_COLO = 2                                                   1306
            AG_SERIAL = 2                                                 1306
            AG_TITLE = 'TPAD'                                             1306
            AG_EXNAME = 'ATTRIBUTE'                                       1306
            AG_PARLIST = 'SEEN_COLO_SERIAL'                               1306
            CALL AXATTRIBUTE                                              1306
C       *                                                                 1306
         END IF                                                           1307
C    Geant MATERIAL : P10 ===> inherited arguments used <===              1308
         CALL AGSRESET                                                    1308
         AG_TITLE = 'P10'                                                 1308
         AG_EXNAME = 'MATERIAL'                                           1308
         AG_PARLIST = 'NONE'                                              1308
         CALL AXMATERIAL                                                  1308
C    *                                                                    1308
C    Geant MATERIAL : SENSITIVE_GAS ===> ISVOL=1 STEMAX=2.5*TPRS_WIDTH <  1308
C    ===                                                                  1308
         CALL AGSRESET                                                    1309
         AG_ISVOL = 1                                                     1309
         AG_STEMAX = 2.5*TPRS_WIDTH                                       1309
         AG_TITLE = 'SENSITIVE_GAS'                                       1309
         AG_EXNAME = 'MATERIAL'                                           1309
         AG_PARLIST = 'ISVOL_STEMAX'                                      1309
         CALL AXMATERIAL                                                  1309
C    *                                                                    1309
C    Geant MEDIUM : TPAD_P10 ===> STEMAX=2.5*TPRS_WIDTH DEEMAX=0.0017536  1309
C    7 STMIN=0.0249056 ISVOL=1 <===                                       1309
         CALL AGSRESET                                                    1310
         AG_STEMAX = 2.5*TPRS_WIDTH                                       1310
         AG_DEEMAX = 0.00175367                                           1310
         AG_STMIN = 0.0249056                                             1310
         AG_ISVOL = 1                                                     1310
         AG_TITLE = 'TPAD_P10'                                            1310
         AG_EXNAME = 'MEDIUM'                                             1310
         AG_PARLIST = 'STEMAX_DEEMAX_STMIN_ISVOL'                         1310
         CALL AXMEDIUM                                                    1310
C    *                                                                    1310
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           1310
         CALL AGSRESET                                                    1311
         AG_DX = 0                                                        1311
         AG_DY = 0                                                        1311
         AG_DZ = 0                                                        1311
         AG_TITLE = 'BOX'                                                 1311
         AG_EXNAME = 'SHAPE'                                              1311
         AG_PARLIST = 'DX_DY_DZ'                                          1311
         CALL AXSHAPE                                                     1311
C    *                                                                    1311
         CALL GSTPAR(AG_IMED, 'CUTGAM',1E-4)                              1311
         CALL GSTPAR(AG_IMED, 'CUTELE',1E-4)                              1313
         CALL GSTPAR(AG_IMED, 'DCUTE',1E-4)                               1314
         CALL GSTPAR(AG_IMED, 'DCUTM',1E-4)                               1315
         CALL GSTPAR(AG_IMED, 'STRA', 1.)                                 1316
C    Defining Hits: Z:0.0005:S Y:0.0005: X:0.0005: CX:10: CY:10: CZ:10:   1318
C    LPTOT:18:(-3,2), SLENG:0.1:(0,800) TOF:16:(0,1.0E-6) LGAM:16:(-2,+2  1318
C    ) STEP:11:(0,10) USER:21:(-0.01,0.01)                                1318
         AG_TITLE = 'TPAD'                                                1318
         AG_EXNAME = 'HITS'                                               1318
         AG_TYPE=AG_MAGIC                                                 1318
         CALL AGSDETV('TPAD')                                             1318
C    *                                                                    1318
         AG_PARLIST='Z'                                                   1318
         AG_TITLE = 'DETH'                                                1318
         AG_NBITS = AG_MAGIC                                              1318
         AG_BIN=0.0005                                                    1318
         AG_OPTION='S'                                                    1318
         AG_FHMIN = AG_MAGIC                                              1318
         AG_FHMAX = AG_MAGIC                                              1318
         CALL AGSDETH (0)                                                 1318
C    *                                                                    1318
         AG_PARLIST='Y'                                                   1318
         AG_TITLE = 'DETH'                                                1318
         AG_NBITS = AG_MAGIC                                              1318
         AG_BIN=0.0005                                                    1318
         AG_FHMIN=AG_MAGIC                                                1318
         AG_FHMAX=AG_MAGIC                                                1318
         AG_OPTION='*'                                                    1318
         CALL AGSDETH (0)                                                 1318
C    *                                                                    1318
         AG_PARLIST='X'                                                   1318
         AG_TITLE = 'DETH'                                                1318
         AG_NBITS = AG_MAGIC                                              1318
         AG_BIN=0.0005                                                    1318
         AG_FHMIN=AG_MAGIC                                                1318
         AG_FHMAX=AG_MAGIC                                                1318
         AG_OPTION='*'                                                    1318
         CALL AGSDETH (0)                                                 1318
C    *                                                                    1318
         AG_PARLIST='CX'                                                  1318
         AG_TITLE = 'DETH'                                                1318
         AG_NBITS = 10                                                    1318
         AG_BIN=AG_MAGIC                                                  1318
         AG_FHMIN=AG_MAGIC                                                1318
         AG_FHMAX=AG_MAGIC                                                1318
         AG_OPTION='*'                                                    1318
         CALL AGSDETH (0)                                                 1318
C    *                                                                    1318
         AG_PARLIST='CY'                                                  1318
         AG_TITLE = 'DETH'                                                1318
         AG_NBITS = 10                                                    1318
         AG_BIN=AG_MAGIC                                                  1318
         AG_FHMIN=AG_MAGIC                                                1318
         AG_FHMAX=AG_MAGIC                                                1318
         AG_OPTION='*'                                                    1318
         CALL AGSDETH (0)                                                 1318
C    *                                                                    1318
         AG_PARLIST='CZ'                                                  1318
         AG_TITLE = 'DETH'                                                1318
         AG_NBITS = 10                                                    1318
         AG_BIN=AG_MAGIC                                                  1318
         AG_FHMIN=AG_MAGIC                                                1318
         AG_FHMAX=AG_MAGIC                                                1318
         AG_OPTION='*'                                                    1318
         CALL AGSDETH (0)                                                 1318
C    *                                                                    1318
         AG_PARLIST='LPTOT'                                               1318
         AG_TITLE = 'DETH'                                                1318
         AG_NBITS = 18                                                    1318
         AG_BIN=AG_MAGIC                                                  1318
         AG_OPTION='*'                                                    1318
         AG_FHMIN = -3                                                    1318
         AG_FHMAX = 2                                                     1318
         CALL AGSDETH (0)                                                 1318
C    *                                                                    1318
         AG_PARLIST='SLENG'                                               1318
         AG_TITLE = 'DETH'                                                1318
         AG_NBITS = AG_MAGIC                                              1318
         AG_BIN=0.1                                                       1318
         AG_OPTION='*'                                                    1318
         AG_FHMIN = 0                                                     1318
         AG_FHMAX = 800                                                   1318
         CALL AGSDETH (0)                                                 1318
C    *                                                                    1318
         AG_PARLIST='TOF'                                                 1318
         AG_TITLE = 'DETH'                                                1318
         AG_NBITS = 16                                                    1318
         AG_BIN=AG_MAGIC                                                  1318
         AG_OPTION='*'                                                    1318
         AG_FHMIN = 0                                                     1318
         AG_FHMAX = 1.0E-6                                                1318
         CALL AGSDETH (0)                                                 1318
C    *                                                                    1318
         AG_PARLIST='LGAM'                                                1318
         AG_TITLE = 'DETH'                                                1318
         AG_NBITS = 16                                                    1318
         AG_BIN=AG_MAGIC                                                  1318
         AG_OPTION='*'                                                    1318
         AG_FHMIN = -2                                                    1318
         AG_FHMAX = +2                                                    1318
         CALL AGSDETH (0)                                                 1318
C    *                                                                    1318
         AG_PARLIST='STEP'                                                1318
         AG_TITLE = 'DETH'                                                1318
         AG_NBITS = 11                                                    1318
         AG_BIN=AG_MAGIC                                                  1318
         AG_OPTION='*'                                                    1318
         AG_FHMIN = 0                                                     1318
         AG_FHMAX = 10                                                    1318
         CALL AGSDETH (0)                                                 1318
C    *                                                                    1318
         AG_PARLIST='USER'                                                1318
         AG_TITLE = 'DETH'                                                1318
         AG_NBITS = 21                                                    1318
         AG_BIN=AG_MAGIC                                                  1318
         AG_OPTION='*'                                                    1318
         AG_FHMIN = -0.01                                                 1318
         AG_FHMAX = 0.01                                                  1318
         CALL AGSDETH (JUMPAD(TPADSTEP))                                  1318
C    *                                                                    1318
         CALL AGSDETU('TPAD')                                             1318
C    *                                                                    1318
6151  CALL AGSPOPB                                                        1319
      GO TO BLOCK_TPAD, (6150 ,6160,6170,6180)                            1319
C End of Local Procedure                                                  1319
      WRITE(LOUT,6190)'===>>  [End AgML Module TpcxGeo1]  <<==='          1319
6190  FORMAT(A32,/,/)                                                     1319
C MODULE TPCXGEO1                                                         1321
      END                                                                 1321
