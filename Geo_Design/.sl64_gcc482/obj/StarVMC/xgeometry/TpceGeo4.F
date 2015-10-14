C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE TPCEGEO4                                                 1
C TPCEGEO4 IS THE UPDATED TPC                                             1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'TPCEGEO4'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'IS THE UPDATED TPC'/                                 1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C AUTHOR JASON WEBB (ADAPTED FROM V3A)                                    1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'JASON WEBB (ADAPTED FROM V3A)'/               2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED 3/10/14                                                         2
      CHARACTER*40 CREATED /'3/10/14'/                                    3
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
      INTEGER BLOCK_TPMV                                                  16
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
      REAL ZBEG,ZDED,ZPMT,ZEND,FACT                                       56
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
C -                                                                       134
      REAL TPCC_VERSION                                                   134
      EQUIVALENCE (BANK_TPCC(3),TPCC_VERSION,RE_TPCC_VERSION),            134
     *(NAMES_TPCC(1),NTPCC_VERSION)                                       134
      EQUIVALENCE (COMMEN_TPCC(1),CTPCC_VERSION),(MAP_TPCC(1,1),          134
     *MTPCC_VERSION)                                                      134
      CHARACTER CTPCC_VERSION*40/' '/                                     134
      CHARACTER NTPCC_VERSION*12/'R.VERSION'/                             134
      INTEGER MTPCC_VERSION(2)/1,1/                                       134
      REAL RE_TPCC_VERSION                                                134
C -                                                                       134
      INTEGER LEN_TPCC,LLEN_TPCC,LINK_TPCC                                134
      PARAMETER (LEN_TPCC=3)                                              134
      PARAMETER (LLEN_TPCC=1)                                             134
      INTEGER FLAG_TPCC/0/,MAP_TPCC(2,LLEN_TPCC),IDNUM_TPCC(10)           134
      CHARACTER NAMES_TPCC(LLEN_TPCC)*12,COMMEN_TPCC(LLEN_TPCC)*40        134
      DATA IDNUM_TPCC/10*0/                                               134
      REAL BANK_TPCC(LEN_TPCC)                                            134
      CHARACTER*60 BPATH_TPCC/' '/                                        134
      SAVE FLAG_TPCC,MAP_TPCC,IDNUM_TPCC,NAMES_TPCC,COMMEN_TPCC,          134
     *BPATH_TPCC                                                          134
      DATA LINK_TPCC/0/,BANK_TPCC/LEN_TPCC*0/                             134
C *                                                                       134
C -                                                                       141
      REAL TPCG_VERSION                                                   141
      EQUIVALENCE (BANK_TPCG(3),TPCG_VERSION,RE_TPCG_VERSION),            141
     *(NAMES_TPCG(1),NTPCG_VERSION)                                       141
      EQUIVALENCE (COMMEN_TPCG(1),CTPCG_VERSION),(MAP_TPCG(1,1),          141
     *MTPCG_VERSION)                                                      141
      CHARACTER CTPCG_VERSION*40/' '/                                     141
      CHARACTER NTPCG_VERSION*12/'R.VERSION'/                             141
      INTEGER MTPCG_VERSION(2)/1,1/                                       141
      REAL RE_TPCG_VERSION                                                141
C -                                                                       141
      REAL TPCG_RMIN                                                      141
      EQUIVALENCE (BANK_TPCG(4),TPCG_RMIN,RE_TPCG_RMIN),(NAMES_TPCG(2),   141
     *NTPCG_RMIN)                                                         141
      EQUIVALENCE (COMMEN_TPCG(2),CTPCG_RMIN),(MAP_TPCG(1,2),             141
     *MTPCG_RMIN)                                                         141
      CHARACTER CTPCG_RMIN*40/' '/                                        141
      CHARACTER NTPCG_RMIN*12/'R.RMIN'/                                   141
      INTEGER MTPCG_RMIN(2)/1,1/                                          141
      REAL RE_TPCG_RMIN                                                   141
C -                                                                       141
      REAL TPCG_RMAX                                                      141
      EQUIVALENCE (BANK_TPCG(5),TPCG_RMAX,RE_TPCG_RMAX),(NAMES_TPCG(3),   141
     *NTPCG_RMAX)                                                         141
      EQUIVALENCE (COMMEN_TPCG(3),CTPCG_RMAX),(MAP_TPCG(1,3),             141
     *MTPCG_RMAX)                                                         141
      CHARACTER CTPCG_RMAX*40/' '/                                        141
      CHARACTER NTPCG_RMAX*12/'R.RMAX'/                                   141
      INTEGER MTPCG_RMAX(2)/1,1/                                          141
      REAL RE_TPCG_RMAX                                                   141
C -                                                                       141
      REAL TPCG_RMINIFC                                                   141
      EQUIVALENCE (BANK_TPCG(6),TPCG_RMINIFC,RE_TPCG_RMINIFC),            141
     *(NAMES_TPCG(4),NTPCG_RMINIFC)                                       141
      EQUIVALENCE (COMMEN_TPCG(4),CTPCG_RMINIFC),(MAP_TPCG(1,4),          141
     *MTPCG_RMINIFC)                                                      141
      CHARACTER CTPCG_RMINIFC*40/' '/                                     141
      CHARACTER NTPCG_RMINIFC*12/'R.RMINIFC'/                             141
      INTEGER MTPCG_RMINIFC(2)/1,1/                                       141
      REAL RE_TPCG_RMINIFC                                                141
C -                                                                       141
      REAL TPCG_LENGTHT                                                   141
      EQUIVALENCE (BANK_TPCG(7),TPCG_LENGTHT,RE_TPCG_LENGTHT),            141
     *(NAMES_TPCG(5),NTPCG_LENGTHT)                                       141
      EQUIVALENCE (COMMEN_TPCG(5),CTPCG_LENGTHT),(MAP_TPCG(1,5),          141
     *MTPCG_LENGTHT)                                                      141
      CHARACTER CTPCG_LENGTHT*40/' '/                                     141
      CHARACTER NTPCG_LENGTHT*12/'R.LENGTHT'/                             141
      INTEGER MTPCG_LENGTHT(2)/1,1/                                       141
      REAL RE_TPCG_LENGTHT                                                141
C -                                                                       141
      REAL TPCG_LENGTH                                                    141
      EQUIVALENCE (BANK_TPCG(8),TPCG_LENGTH,RE_TPCG_LENGTH),              141
     *(NAMES_TPCG(6),NTPCG_LENGTH)                                        141
      EQUIVALENCE (COMMEN_TPCG(6),CTPCG_LENGTH),(MAP_TPCG(1,6),           141
     *MTPCG_LENGTH)                                                       141
      CHARACTER CTPCG_LENGTH*40/' '/                                      141
      CHARACTER NTPCG_LENGTH*12/'R.LENGTH'/                               141
      INTEGER MTPCG_LENGTH(2)/1,1/                                        141
      REAL RE_TPCG_LENGTH                                                 141
C -                                                                       141
      REAL TPCG_LENGTHW                                                   141
      EQUIVALENCE (BANK_TPCG(9),TPCG_LENGTHW,RE_TPCG_LENGTHW),            141
     *(NAMES_TPCG(7),NTPCG_LENGTHW)                                       141
      EQUIVALENCE (COMMEN_TPCG(7),CTPCG_LENGTHW),(MAP_TPCG(1,7),          141
     *MTPCG_LENGTHW)                                                      141
      CHARACTER CTPCG_LENGTHW*40/' '/                                     141
      CHARACTER NTPCG_LENGTHW*12/'R.LENGTHW'/                             141
      INTEGER MTPCG_LENGTHW(2)/1,1/                                       141
      REAL RE_TPCG_LENGTHW                                                141
C -                                                                       141
      REAL TPCG_LENGTHV                                                   141
      EQUIVALENCE (BANK_TPCG(10),TPCG_LENGTHV,RE_TPCG_LENGTHV),           141
     *(NAMES_TPCG(8),NTPCG_LENGTHV)                                       141
      EQUIVALENCE (COMMEN_TPCG(8),CTPCG_LENGTHV),(MAP_TPCG(1,8),          141
     *MTPCG_LENGTHV)                                                      141
      CHARACTER CTPCG_LENGTHV*40/' '/                                     141
      CHARACTER NTPCG_LENGTHV*12/'R.LENGTHV'/                             141
      INTEGER MTPCG_LENGTHV(2)/1,1/                                       141
      REAL RE_TPCG_LENGTHV                                                141
C -                                                                       141
      REAL TPCG_DZENVELOP                                                 141
      EQUIVALENCE (BANK_TPCG(11),TPCG_DZENVELOP,RE_TPCG_DZENVELOP),       141
     *(NAMES_TPCG(9),NTPCG_DZENVELOP)                                     141
      EQUIVALENCE (COMMEN_TPCG(9),CTPCG_DZENVELOP),(MAP_TPCG(1,9),        141
     *MTPCG_DZENVELOP)                                                    141
      CHARACTER CTPCG_DZENVELOP*40/' '/                                   141
      CHARACTER NTPCG_DZENVELOP*12/'R.DZENVELOP'/                         141
      INTEGER MTPCG_DZENVELOP(2)/1,1/                                     141
      REAL RE_TPCG_DZENVELOP                                              141
C -                                                                       141
      REAL TPCG_WHEELIR                                                   141
      EQUIVALENCE (BANK_TPCG(12),TPCG_WHEELIR,RE_TPCG_WHEELIR),           141
     *(NAMES_TPCG(10),NTPCG_WHEELIR)                                      141
      EQUIVALENCE (COMMEN_TPCG(10),CTPCG_WHEELIR),(MAP_TPCG(1,10),        141
     *MTPCG_WHEELIR)                                                      141
      CHARACTER CTPCG_WHEELIR*40/' '/                                     141
      CHARACTER NTPCG_WHEELIR*12/'R.WHEELIR'/                             141
      INTEGER MTPCG_WHEELIR(2)/1,1/                                       141
      REAL RE_TPCG_WHEELIR                                                141
C -                                                                       141
      REAL TPCG_WHEELR0                                                   141
      EQUIVALENCE (BANK_TPCG(13),TPCG_WHEELR0,RE_TPCG_WHEELR0),           141
     *(NAMES_TPCG(11),NTPCG_WHEELR0)                                      141
      EQUIVALENCE (COMMEN_TPCG(11),CTPCG_WHEELR0),(MAP_TPCG(1,11),        141
     *MTPCG_WHEELR0)                                                      141
      CHARACTER CTPCG_WHEELR0*40/' '/                                     141
      CHARACTER NTPCG_WHEELR0*12/'R.WHEELR0'/                             141
      INTEGER MTPCG_WHEELR0(2)/1,1/                                       141
      REAL RE_TPCG_WHEELR0                                                141
C -                                                                       141
      REAL TPCG_WHEELR1                                                   141
      EQUIVALENCE (BANK_TPCG(14),TPCG_WHEELR1,RE_TPCG_WHEELR1),           141
     *(NAMES_TPCG(12),NTPCG_WHEELR1)                                      141
      EQUIVALENCE (COMMEN_TPCG(12),CTPCG_WHEELR1),(MAP_TPCG(1,12),        141
     *MTPCG_WHEELR1)                                                      141
      CHARACTER CTPCG_WHEELR1*40/' '/                                     141
      CHARACTER NTPCG_WHEELR1*12/'R.WHEELR1'/                             141
      INTEGER MTPCG_WHEELR1(2)/1,1/                                       141
      REAL RE_TPCG_WHEELR1                                                141
C -                                                                       141
      REAL TPCG_WHEELR2                                                   141
      EQUIVALENCE (BANK_TPCG(15),TPCG_WHEELR2,RE_TPCG_WHEELR2),           141
     *(NAMES_TPCG(13),NTPCG_WHEELR2)                                      141
      EQUIVALENCE (COMMEN_TPCG(13),CTPCG_WHEELR2),(MAP_TPCG(1,13),        141
     *MTPCG_WHEELR2)                                                      141
      CHARACTER CTPCG_WHEELR2*40/' '/                                     141
      CHARACTER NTPCG_WHEELR2*12/'R.WHEELR2'/                             141
      INTEGER MTPCG_WHEELR2(2)/1,1/                                       141
      REAL RE_TPCG_WHEELR2                                                141
C -                                                                       141
      REAL TPCG_WHEELTOTALRIBWIDTH                                        141
      EQUIVALENCE (BANK_TPCG(16),TPCG_WHEELTOTALRIBWIDTH,                 141
     *RE_TPCG_WHEELTOTALRIBWIDTH),(NAMES_TPCG(14),                        141
     *NTPCG_WHEELTOTALRIBWIDTH)                                           141
      EQUIVALENCE (COMMEN_TPCG(14),CTPCG_WHEELTOTALRIBWIDTH),             141
     *(MAP_TPCG(1,14),MTPCG_WHEELTOTALRIBWIDTH)                           141
      CHARACTER CTPCG_WHEELTOTALRIBWIDTH*40/' '/                          141
      CHARACTER NTPCG_WHEELTOTALRIBWIDTH*12/'R.WHEELTOTALRIBWIDTH'/       141
      INTEGER MTPCG_WHEELTOTALRIBWIDTH(2)/1,1/                            141
      REAL RE_TPCG_WHEELTOTALRIBWIDTH                                     141
C -                                                                       141
      REAL TPCG_WHEELRIBWIDTH                                             141
      EQUIVALENCE (BANK_TPCG(17),TPCG_WHEELRIBWIDTH,                      141
     *RE_TPCG_WHEELRIBWIDTH),(NAMES_TPCG(15),NTPCG_WHEELRIBWIDTH)         141
      EQUIVALENCE (COMMEN_TPCG(15),CTPCG_WHEELRIBWIDTH),(MAP_TPCG(1,15),  141
     *MTPCG_WHEELRIBWIDTH)                                                141
      CHARACTER CTPCG_WHEELRIBWIDTH*40/' '/                               141
      CHARACTER NTPCG_WHEELRIBWIDTH*12/'R.WHEELRIBWIDTH'/                 141
      INTEGER MTPCG_WHEELRIBWIDTH(2)/1,1/                                 141
      REAL RE_TPCG_WHEELRIBWIDTH                                          141
C -                                                                       141
      REAL TPCG_WHEELRIBHEIGHT                                            141
      EQUIVALENCE (BANK_TPCG(18),TPCG_WHEELRIBHEIGHT,                     141
     *RE_TPCG_WHEELRIBHEIGHT),(NAMES_TPCG(16),NTPCG_WHEELRIBHEIGHT)       141
      EQUIVALENCE (COMMEN_TPCG(16),CTPCG_WHEELRIBHEIGHT),(MAP_TPCG(1,     141
     *16),MTPCG_WHEELRIBHEIGHT)                                           141
      CHARACTER CTPCG_WHEELRIBHEIGHT*40/' '/                              141
      CHARACTER NTPCG_WHEELRIBHEIGHT*12/'R.WHEELRIBHEIGHT'/               141
      INTEGER MTPCG_WHEELRIBHEIGHT(2)/1,1/                                141
      REAL RE_TPCG_WHEELRIBHEIGHT                                         141
C -                                                                       141
      REAL TPCG_WHEELBOXDX                                                141
      EQUIVALENCE (BANK_TPCG(19),TPCG_WHEELBOXDX,RE_TPCG_WHEELBOXDX),     141
     *(NAMES_TPCG(17),NTPCG_WHEELBOXDX)                                   141
      EQUIVALENCE (COMMEN_TPCG(17),CTPCG_WHEELBOXDX),(MAP_TPCG(1,17),     141
     *MTPCG_WHEELBOXDX)                                                   141
      CHARACTER CTPCG_WHEELBOXDX*40/' '/                                  141
      CHARACTER NTPCG_WHEELBOXDX*12/'R.WHEELBOXDX'/                       141
      INTEGER MTPCG_WHEELBOXDX(2)/1,1/                                    141
      REAL RE_TPCG_WHEELBOXDX                                             141
C -                                                                       141
      REAL TPCG_WHEELBOXDY                                                141
      EQUIVALENCE (BANK_TPCG(20),TPCG_WHEELBOXDY,RE_TPCG_WHEELBOXDY),     141
     *(NAMES_TPCG(18),NTPCG_WHEELBOXDY)                                   141
      EQUIVALENCE (COMMEN_TPCG(18),CTPCG_WHEELBOXDY),(MAP_TPCG(1,18),     141
     *MTPCG_WHEELBOXDY)                                                   141
      CHARACTER CTPCG_WHEELBOXDY*40/' '/                                  141
      CHARACTER NTPCG_WHEELBOXDY*12/'R.WHEELBOXDY'/                       141
      INTEGER MTPCG_WHEELBOXDY(2)/1,1/                                    141
      REAL RE_TPCG_WHEELBOXDY                                             141
C -                                                                       141
      REAL TPCG_WHEELOR1                                                  141
      EQUIVALENCE (BANK_TPCG(21),TPCG_WHEELOR1,RE_TPCG_WHEELOR1),         141
     *(NAMES_TPCG(19),NTPCG_WHEELOR1)                                     141
      EQUIVALENCE (COMMEN_TPCG(19),CTPCG_WHEELOR1),(MAP_TPCG(1,19),       141
     *MTPCG_WHEELOR1)                                                     141
      CHARACTER CTPCG_WHEELOR1*40/' '/                                    141
      CHARACTER NTPCG_WHEELOR1*12/'R.WHEELOR1'/                           141
      INTEGER MTPCG_WHEELOR1(2)/1,1/                                      141
      REAL RE_TPCG_WHEELOR1                                               141
C -                                                                       141
      REAL TPCG_WHEELOR                                                   141
      EQUIVALENCE (BANK_TPCG(22),TPCG_WHEELOR,RE_TPCG_WHEELOR),           141
     *(NAMES_TPCG(20),NTPCG_WHEELOR)                                      141
      EQUIVALENCE (COMMEN_TPCG(20),CTPCG_WHEELOR),(MAP_TPCG(1,20),        141
     *MTPCG_WHEELOR)                                                      141
      CHARACTER CTPCG_WHEELOR*40/' '/                                     141
      CHARACTER NTPCG_WHEELOR*12/'R.WHEELOR'/                             141
      INTEGER MTPCG_WHEELOR(2)/1,1/                                       141
      REAL RE_TPCG_WHEELOR                                                141
C -                                                                       141
      REAL TPCG_WHEELTHK1                                                 141
      EQUIVALENCE (BANK_TPCG(23),TPCG_WHEELTHK1,RE_TPCG_WHEELTHK1),       141
     *(NAMES_TPCG(21),NTPCG_WHEELTHK1)                                    141
      EQUIVALENCE (COMMEN_TPCG(21),CTPCG_WHEELTHK1),(MAP_TPCG(1,21),      141
     *MTPCG_WHEELTHK1)                                                    141
      CHARACTER CTPCG_WHEELTHK1*40/' '/                                   141
      CHARACTER NTPCG_WHEELTHK1*12/'R.WHEELTHK1'/                         141
      INTEGER MTPCG_WHEELTHK1(2)/1,1/                                     141
      REAL RE_TPCG_WHEELTHK1                                              141
C -                                                                       141
      REAL TPCG_WHEELTHK                                                  141
      EQUIVALENCE (BANK_TPCG(24),TPCG_WHEELTHK,RE_TPCG_WHEELTHK),         141
     *(NAMES_TPCG(22),NTPCG_WHEELTHK)                                     141
      EQUIVALENCE (COMMEN_TPCG(22),CTPCG_WHEELTHK),(MAP_TPCG(1,22),       141
     *MTPCG_WHEELTHK)                                                     141
      CHARACTER CTPCG_WHEELTHK*40/' '/                                    141
      CHARACTER NTPCG_WHEELTHK*12/'R.WHEELTHK'/                           141
      INTEGER MTPCG_WHEELTHK(2)/1,1/                                      141
      REAL RE_TPCG_WHEELTHK                                               141
C -                                                                       141
      REAL TPCG_SENGASOR                                                  141
      EQUIVALENCE (BANK_TPCG(25),TPCG_SENGASOR,RE_TPCG_SENGASOR),         141
     *(NAMES_TPCG(23),NTPCG_SENGASOR)                                     141
      EQUIVALENCE (COMMEN_TPCG(23),CTPCG_SENGASOR),(MAP_TPCG(1,23),       141
     *MTPCG_SENGASOR)                                                     141
      CHARACTER CTPCG_SENGASOR*40/' '/                                    141
      CHARACTER NTPCG_SENGASOR*12/'R.SENGASOR'/                           141
      INTEGER MTPCG_SENGASOR(2)/1,1/                                      141
      REAL RE_TPCG_SENGASOR                                               141
C -                                                                       141
      REAL TPCG_MEMBTHK                                                   141
      EQUIVALENCE (BANK_TPCG(26),TPCG_MEMBTHK,RE_TPCG_MEMBTHK),           141
     *(NAMES_TPCG(24),NTPCG_MEMBTHK)                                      141
      EQUIVALENCE (COMMEN_TPCG(24),CTPCG_MEMBTHK),(MAP_TPCG(1,24),        141
     *MTPCG_MEMBTHK)                                                      141
      CHARACTER CTPCG_MEMBTHK*40/' '/                                     141
      CHARACTER NTPCG_MEMBTHK*12/'R.MEMBTHK'/                             141
      INTEGER MTPCG_MEMBTHK(2)/1,1/                                       141
      REAL RE_TPCG_MEMBTHK                                                141
C -                                                                       141
      REAL TPCG_TOCSDR                                                    141
      EQUIVALENCE (BANK_TPCG(27),TPCG_TOCSDR,RE_TPCG_TOCSDR),             141
     *(NAMES_TPCG(25),NTPCG_TOCSDR)                                       141
      EQUIVALENCE (COMMEN_TPCG(25),CTPCG_TOCSDR),(MAP_TPCG(1,25),         141
     *MTPCG_TOCSDR)                                                       141
      CHARACTER CTPCG_TOCSDR*40/' '/                                      141
      CHARACTER NTPCG_TOCSDR*12/'R.TOCSDR'/                               141
      INTEGER MTPCG_TOCSDR(2)/1,1/                                        141
      REAL RE_TPCG_TOCSDR                                                 141
C -                                                                       141
      REAL TPCG_TOKADR                                                    141
      EQUIVALENCE (BANK_TPCG(28),TPCG_TOKADR,RE_TPCG_TOKADR),             141
     *(NAMES_TPCG(26),NTPCG_TOKADR)                                       141
      EQUIVALENCE (COMMEN_TPCG(26),CTPCG_TOKADR),(MAP_TPCG(1,26),         141
     *MTPCG_TOKADR)                                                       141
      CHARACTER CTPCG_TOKADR*40/' '/                                      141
      CHARACTER NTPCG_TOKADR*12/'R.TOKADR'/                               141
      INTEGER MTPCG_TOKADR(2)/1,1/                                        141
      REAL RE_TPCG_TOKADR                                                 141
C -                                                                       141
      REAL TPCG_TONXDR                                                    141
      EQUIVALENCE (BANK_TPCG(29),TPCG_TONXDR,RE_TPCG_TONXDR),             141
     *(NAMES_TPCG(27),NTPCG_TONXDR)                                       141
      EQUIVALENCE (COMMEN_TPCG(27),CTPCG_TONXDR),(MAP_TPCG(1,27),         141
     *MTPCG_TONXDR)                                                       141
      CHARACTER CTPCG_TONXDR*40/' '/                                      141
      CHARACTER NTPCG_TONXDR*12/'R.TONXDR'/                               141
      INTEGER MTPCG_TONXDR(2)/1,1/                                        141
      REAL RE_TPCG_TONXDR                                                 141
C -                                                                       141
      REAL TPCG_TOADDR                                                    141
      EQUIVALENCE (BANK_TPCG(30),TPCG_TOADDR,RE_TPCG_TOADDR),             141
     *(NAMES_TPCG(28),NTPCG_TOADDR)                                       141
      EQUIVALENCE (COMMEN_TPCG(28),CTPCG_TOADDR),(MAP_TPCG(1,28),         141
     *MTPCG_TOADDR)                                                       141
      CHARACTER CTPCG_TOADDR*40/' '/                                      141
      CHARACTER NTPCG_TOADDR*12/'R.TOADDR'/                               141
      INTEGER MTPCG_TOADDR(2)/1,1/                                        141
      REAL RE_TPCG_TOADDR                                                 141
C -                                                                       141
      REAL TPCG_TOIGDR                                                    141
      EQUIVALENCE (BANK_TPCG(31),TPCG_TOIGDR,RE_TPCG_TOIGDR),             141
     *(NAMES_TPCG(29),NTPCG_TOIGDR)                                       141
      EQUIVALENCE (COMMEN_TPCG(29),CTPCG_TOIGDR),(MAP_TPCG(1,29),         141
     *MTPCG_TOIGDR)                                                       141
      CHARACTER CTPCG_TOIGDR*40/' '/                                      141
      CHARACTER NTPCG_TOIGDR*12/'R.TOIGDR'/                               141
      INTEGER MTPCG_TOIGDR(2)/1,1/                                        141
      REAL RE_TPCG_TOIGDR                                                 141
C -                                                                       141
      REAL TPCG_TOALDR                                                    141
      EQUIVALENCE (BANK_TPCG(32),TPCG_TOALDR,RE_TPCG_TOALDR),             141
     *(NAMES_TPCG(30),NTPCG_TOALDR)                                       141
      EQUIVALENCE (COMMEN_TPCG(30),CTPCG_TOALDR),(MAP_TPCG(1,30),         141
     *MTPCG_TOALDR)                                                       141
      CHARACTER CTPCG_TOALDR*40/' '/                                      141
      CHARACTER NTPCG_TOALDR*12/'R.TOALDR'/                               141
      INTEGER MTPCG_TOALDR(2)/1,1/                                        141
      REAL RE_TPCG_TOALDR                                                 141
C -                                                                       141
      REAL TPCG_TOHADR                                                    141
      EQUIVALENCE (BANK_TPCG(33),TPCG_TOHADR,RE_TPCG_TOHADR),             141
     *(NAMES_TPCG(31),NTPCG_TOHADR)                                       141
      EQUIVALENCE (COMMEN_TPCG(31),CTPCG_TOHADR),(MAP_TPCG(1,31),         141
     *MTPCG_TOHADR)                                                       141
      CHARACTER CTPCG_TOHADR*40/' '/                                      141
      CHARACTER NTPCG_TOHADR*12/'R.TOHADR'/                               141
      INTEGER MTPCG_TOHADR(2)/1,1/                                        141
      REAL RE_TPCG_TOHADR                                                 141
C -                                                                       141
      REAL TPCG_TIADDR                                                    141
      EQUIVALENCE (BANK_TPCG(34),TPCG_TIADDR,RE_TPCG_TIADDR),             141
     *(NAMES_TPCG(32),NTPCG_TIADDR)                                       141
      EQUIVALENCE (COMMEN_TPCG(32),CTPCG_TIADDR),(MAP_TPCG(1,32),         141
     *MTPCG_TIADDR)                                                       141
      CHARACTER CTPCG_TIADDR*40/' '/                                      141
      CHARACTER NTPCG_TIADDR*12/'R.TIADDR'/                               141
      INTEGER MTPCG_TIADDR(2)/1,1/                                        141
      REAL RE_TPCG_TIADDR                                                 141
C -                                                                       141
      REAL TPCG_TINXDR                                                    141
      EQUIVALENCE (BANK_TPCG(35),TPCG_TINXDR,RE_TPCG_TINXDR),             141
     *(NAMES_TPCG(33),NTPCG_TINXDR)                                       141
      EQUIVALENCE (COMMEN_TPCG(33),CTPCG_TINXDR),(MAP_TPCG(1,33),         141
     *MTPCG_TINXDR)                                                       141
      CHARACTER CTPCG_TINXDR*40/' '/                                      141
      CHARACTER NTPCG_TINXDR*12/'R.TINXDR'/                               141
      INTEGER MTPCG_TINXDR(2)/1,1/                                        141
      REAL RE_TPCG_TINXDR                                                 141
C -                                                                       141
      REAL TPCG_TIKADR                                                    141
      EQUIVALENCE (BANK_TPCG(36),TPCG_TIKADR,RE_TPCG_TIKADR),             141
     *(NAMES_TPCG(34),NTPCG_TIKADR)                                       141
      EQUIVALENCE (COMMEN_TPCG(34),CTPCG_TIKADR),(MAP_TPCG(1,34),         141
     *MTPCG_TIKADR)                                                       141
      CHARACTER CTPCG_TIKADR*40/' '/                                      141
      CHARACTER NTPCG_TIKADR*12/'R.TIKADR'/                               141
      INTEGER MTPCG_TIKADR(2)/1,1/                                        141
      REAL RE_TPCG_TIKADR                                                 141
C -                                                                       141
      REAL TPCG_TIALDR                                                    141
      EQUIVALENCE (BANK_TPCG(37),TPCG_TIALDR,RE_TPCG_TIALDR),             141
     *(NAMES_TPCG(35),NTPCG_TIALDR)                                       141
      EQUIVALENCE (COMMEN_TPCG(35),CTPCG_TIALDR),(MAP_TPCG(1,35),         141
     *MTPCG_TIALDR)                                                       141
      CHARACTER CTPCG_TIALDR*40/' '/                                      141
      CHARACTER NTPCG_TIALDR*12/'R.TIALDR'/                               141
      INTEGER MTPCG_TIALDR(2)/1,1/                                        141
      REAL RE_TPCG_TIALDR                                                 141
C -                                                                       141
      REAL TPCG_TIFCRF                                                    141
      EQUIVALENCE (BANK_TPCG(38),TPCG_TIFCRF,RE_TPCG_TIFCRF),             141
     *(NAMES_TPCG(36),NTPCG_TIFCRF)                                       141
      EQUIVALENCE (COMMEN_TPCG(36),CTPCG_TIFCRF),(MAP_TPCG(1,36),         141
     *MTPCG_TIFCRF)                                                       141
      CHARACTER CTPCG_TIFCRF*40/' '/                                      141
      CHARACTER NTPCG_TIFCRF*12/'R.TIFCRF'/                               141
      INTEGER MTPCG_TIFCRF(2)/1,1/                                        141
      REAL RE_TPCG_TIFCRF                                                 141
C -                                                                       141
      REAL TPCG_TIFCDRT                                                   141
      EQUIVALENCE (BANK_TPCG(39),TPCG_TIFCDRT,RE_TPCG_TIFCDRT),           141
     *(NAMES_TPCG(37),NTPCG_TIFCDRT)                                      141
      EQUIVALENCE (COMMEN_TPCG(37),CTPCG_TIFCDRT),(MAP_TPCG(1,37),        141
     *MTPCG_TIFCDRT)                                                      141
      CHARACTER CTPCG_TIFCDRT*40/' '/                                     141
      CHARACTER NTPCG_TIFCDRT*12/'R.TIFCDRT'/                             141
      INTEGER MTPCG_TIFCDRT(2)/1,1/                                       141
      REAL RE_TPCG_TIFCDRT                                                141
C -                                                                       141
      REAL TPCG_DZYF1                                                     141
      EQUIVALENCE (BANK_TPCG(40),TPCG_DZYF1,RE_TPCG_DZYF1),               141
     *(NAMES_TPCG(38),NTPCG_DZYF1)                                        141
      EQUIVALENCE (COMMEN_TPCG(38),CTPCG_DZYF1),(MAP_TPCG(1,38),          141
     *MTPCG_DZYF1)                                                        141
      CHARACTER CTPCG_DZYF1*40/' '/                                       141
      CHARACTER NTPCG_DZYF1*12/'R.DZYF1'/                                 141
      INTEGER MTPCG_DZYF1(2)/1,1/                                         141
      REAL RE_TPCG_DZYF1                                                  141
C -                                                                       141
      REAL TPCG_DZYF2                                                     141
      EQUIVALENCE (BANK_TPCG(41),TPCG_DZYF2,RE_TPCG_DZYF2),               141
     *(NAMES_TPCG(39),NTPCG_DZYF2)                                        141
      EQUIVALENCE (COMMEN_TPCG(39),CTPCG_DZYF2),(MAP_TPCG(1,39),          141
     *MTPCG_DZYF2)                                                        141
      CHARACTER CTPCG_DZYF2*40/' '/                                       141
      CHARACTER NTPCG_DZYF2*12/'R.DZYF2'/                                 141
      INTEGER MTPCG_DZYF2(2)/1,1/                                         141
      REAL RE_TPCG_DZYF2                                                  141
C -                                                                       141
      REAL TPCG_DZYF3                                                     141
      EQUIVALENCE (BANK_TPCG(42),TPCG_DZYF3,RE_TPCG_DZYF3),               141
     *(NAMES_TPCG(40),NTPCG_DZYF3)                                        141
      EQUIVALENCE (COMMEN_TPCG(40),CTPCG_DZYF3),(MAP_TPCG(1,40),          141
     *MTPCG_DZYF3)                                                        141
      CHARACTER CTPCG_DZYF3*40/' '/                                       141
      CHARACTER NTPCG_DZYF3*12/'R.DZYF3'/                                 141
      INTEGER MTPCG_DZYF3(2)/1,1/                                         141
      REAL RE_TPCG_DZYF3                                                  141
C -                                                                       141
      REAL TPCG_PADPLANETHICKNESS                                         141
      EQUIVALENCE (BANK_TPCG(43),TPCG_PADPLANETHICKNESS,                  141
     *RE_TPCG_PADPLANETHICKNESS),(NAMES_TPCG(41),                         141
     *NTPCG_PADPLANETHICKNESS)                                            141
      EQUIVALENCE (COMMEN_TPCG(41),CTPCG_PADPLANETHICKNESS),(MAP_TPCG(1,  141
     *41),MTPCG_PADPLANETHICKNESS)                                        141
      CHARACTER CTPCG_PADPLANETHICKNESS*40/' '/                           141
      CHARACTER NTPCG_PADPLANETHICKNESS*12/'R.PADPLANETHICKNESS'/         141
      INTEGER MTPCG_PADPLANETHICKNESS(2)/1,1/                             141
      REAL RE_TPCG_PADPLANETHICKNESS                                      141
C -                                                                       141
      REAL TPCG_DGATEGROUND                                               141
      EQUIVALENCE (BANK_TPCG(44),TPCG_DGATEGROUND,RE_TPCG_DGATEGROUND),   141
     *(NAMES_TPCG(42),NTPCG_DGATEGROUND)                                  141
      EQUIVALENCE (COMMEN_TPCG(42),CTPCG_DGATEGROUND),(MAP_TPCG(1,42),    141
     *MTPCG_DGATEGROUND)                                                  141
      CHARACTER CTPCG_DGATEGROUND*40/' '/                                 141
      CHARACTER NTPCG_DGATEGROUND*12/'R.DGATEGROUND'/                     141
      INTEGER MTPCG_DGATEGROUND(2)/1,1/                                   141
      REAL RE_TPCG_DGATEGROUND                                            141
C -                                                                       141
      REAL TPCG_WIREMOUNTWIDTH                                            141
      EQUIVALENCE (BANK_TPCG(45),TPCG_WIREMOUNTWIDTH,                     141
     *RE_TPCG_WIREMOUNTWIDTH),(NAMES_TPCG(43),NTPCG_WIREMOUNTWIDTH)       141
      EQUIVALENCE (COMMEN_TPCG(43),CTPCG_WIREMOUNTWIDTH),(MAP_TPCG(1,     141
     *43),MTPCG_WIREMOUNTWIDTH)                                           141
      CHARACTER CTPCG_WIREMOUNTWIDTH*40/' '/                              141
      CHARACTER NTPCG_WIREMOUNTWIDTH*12/'R.WIREMOUNTWIDTH'/               141
      INTEGER MTPCG_WIREMOUNTWIDTH(2)/1,1/                                141
      REAL RE_TPCG_WIREMOUNTWIDTH                                         141
C -                                                                       141
      REAL TPCG_WIREMOUNTHEIGHT                                           141
      EQUIVALENCE (BANK_TPCG(46),TPCG_WIREMOUNTHEIGHT,                    141
     *RE_TPCG_WIREMOUNTHEIGHT),(NAMES_TPCG(44),NTPCG_WIREMOUNTHEIGHT)     141
      EQUIVALENCE (COMMEN_TPCG(44),CTPCG_WIREMOUNTHEIGHT),(MAP_TPCG(1,    141
     *44),MTPCG_WIREMOUNTHEIGHT)                                          141
      CHARACTER CTPCG_WIREMOUNTHEIGHT*40/' '/                             141
      CHARACTER NTPCG_WIREMOUNTHEIGHT*12/'R.WIREMOUNTHEIGHT'/             141
      INTEGER MTPCG_WIREMOUNTHEIGHT(2)/1,1/                               141
      REAL RE_TPCG_WIREMOUNTHEIGHT                                        141
C -                                                                       141
      REAL TPCG_DXRDO                                                     141
      EQUIVALENCE (BANK_TPCG(47),TPCG_DXRDO,RE_TPCG_DXRDO),               141
     *(NAMES_TPCG(45),NTPCG_DXRDO)                                        141
      EQUIVALENCE (COMMEN_TPCG(45),CTPCG_DXRDO),(MAP_TPCG(1,45),          141
     *MTPCG_DXRDO)                                                        141
      CHARACTER CTPCG_DXRDO*40/' '/                                       141
      CHARACTER NTPCG_DXRDO*12/'R.DXRDO'/                                 141
      INTEGER MTPCG_DXRDO(2)/1,1/                                         141
      REAL RE_TPCG_DXRDO                                                  141
C -                                                                       141
      REAL TPCG_DYRDO                                                     141
      EQUIVALENCE (BANK_TPCG(48),TPCG_DYRDO,RE_TPCG_DYRDO),               141
     *(NAMES_TPCG(46),NTPCG_DYRDO)                                        141
      EQUIVALENCE (COMMEN_TPCG(46),CTPCG_DYRDO),(MAP_TPCG(1,46),          141
     *MTPCG_DYRDO)                                                        141
      CHARACTER CTPCG_DYRDO*40/' '/                                       141
      CHARACTER NTPCG_DYRDO*12/'R.DYRDO'/                                 141
      INTEGER MTPCG_DYRDO(2)/1,1/                                         141
      REAL RE_TPCG_DYRDO                                                  141
C -                                                                       141
      REAL TPCG_DZRDO                                                     141
      EQUIVALENCE (BANK_TPCG(49),TPCG_DZRDO,RE_TPCG_DZRDO),               141
     *(NAMES_TPCG(47),NTPCG_DZRDO)                                        141
      EQUIVALENCE (COMMEN_TPCG(47),CTPCG_DZRDO),(MAP_TPCG(1,47),          141
     *MTPCG_DZRDO)                                                        141
      CHARACTER CTPCG_DZRDO*40/' '/                                       141
      CHARACTER NTPCG_DZRDO*12/'R.DZRDO'/                                 141
      INTEGER MTPCG_DZRDO(2)/1,1/                                         141
      REAL RE_TPCG_DZRDO                                                  141
C -                                                                       141
      REAL TPCG_ZRDO                                                      141
      EQUIVALENCE (BANK_TPCG(50),TPCG_ZRDO,RE_TPCG_ZRDO),                 141
     *(NAMES_TPCG(48),NTPCG_ZRDO)                                         141
      EQUIVALENCE (COMMEN_TPCG(48),CTPCG_ZRDO),(MAP_TPCG(1,48),           141
     *MTPCG_ZRDO)                                                         141
      CHARACTER CTPCG_ZRDO*40/' '/                                        141
      CHARACTER NTPCG_ZRDO*12/'R.ZRDO'/                                   141
      INTEGER MTPCG_ZRDO(2)/1,1/                                          141
      REAL RE_TPCG_ZRDO                                                   141
C -                                                                       141
      REAL TPCG_HEIGTUBE                                                  141
      EQUIVALENCE (BANK_TPCG(51),TPCG_HEIGTUBE,RE_TPCG_HEIGTUBE),         141
     *(NAMES_TPCG(49),NTPCG_HEIGTUBE)                                     141
      EQUIVALENCE (COMMEN_TPCG(49),CTPCG_HEIGTUBE),(MAP_TPCG(1,49),       141
     *MTPCG_HEIGTUBE)                                                     141
      CHARACTER CTPCG_HEIGTUBE*40/' '/                                    141
      CHARACTER NTPCG_HEIGTUBE*12/'R.HEIGTUBE'/                           141
      INTEGER MTPCG_HEIGTUBE(2)/1,1/                                      141
      REAL RE_TPCG_HEIGTUBE                                               141
C -                                                                       141
      REAL TPCG_WIDTUBE                                                   141
      EQUIVALENCE (BANK_TPCG(52),TPCG_WIDTUBE,RE_TPCG_WIDTUBE),           141
     *(NAMES_TPCG(50),NTPCG_WIDTUBE)                                      141
      EQUIVALENCE (COMMEN_TPCG(50),CTPCG_WIDTUBE),(MAP_TPCG(1,50),        141
     *MTPCG_WIDTUBE)                                                      141
      CHARACTER CTPCG_WIDTUBE*40/' '/                                     141
      CHARACTER NTPCG_WIDTUBE*12/'R.WIDTUBE'/                             141
      INTEGER MTPCG_WIDTUBE(2)/1,1/                                       141
      REAL RE_TPCG_WIDTUBE                                                141
C -                                                                       141
      REAL TPCG_RDOCOOLINGDX                                              141
      EQUIVALENCE (BANK_TPCG(53),TPCG_RDOCOOLINGDX,                       141
     *RE_TPCG_RDOCOOLINGDX),(NAMES_TPCG(51),NTPCG_RDOCOOLINGDX)           141
      EQUIVALENCE (COMMEN_TPCG(51),CTPCG_RDOCOOLINGDX),(MAP_TPCG(1,51),   141
     *MTPCG_RDOCOOLINGDX)                                                 141
      CHARACTER CTPCG_RDOCOOLINGDX*40/' '/                                141
      CHARACTER NTPCG_RDOCOOLINGDX*12/'R.RDOCOOLINGDX'/                   141
      INTEGER MTPCG_RDOCOOLINGDX(2)/1,1/                                  141
      REAL RE_TPCG_RDOCOOLINGDX                                           141
C -                                                                       141
      REAL TPCG_RDOCOOLINGDY                                              141
      EQUIVALENCE (BANK_TPCG(54),TPCG_RDOCOOLINGDY,                       141
     *RE_TPCG_RDOCOOLINGDY),(NAMES_TPCG(52),NTPCG_RDOCOOLINGDY)           141
      EQUIVALENCE (COMMEN_TPCG(52),CTPCG_RDOCOOLINGDY),(MAP_TPCG(1,52),   141
     *MTPCG_RDOCOOLINGDY)                                                 141
      CHARACTER CTPCG_RDOCOOLINGDY*40/' '/                                141
      CHARACTER NTPCG_RDOCOOLINGDY*12/'R.RDOCOOLINGDY'/                   141
      INTEGER MTPCG_RDOCOOLINGDY(2)/1,1/                                  141
      REAL RE_TPCG_RDOCOOLINGDY                                           141
C -                                                                       141
      REAL TPCG_RDOCOOLINGDZ                                              141
      EQUIVALENCE (BANK_TPCG(55),TPCG_RDOCOOLINGDZ,                       141
     *RE_TPCG_RDOCOOLINGDZ),(NAMES_TPCG(53),NTPCG_RDOCOOLINGDZ)           141
      EQUIVALENCE (COMMEN_TPCG(53),CTPCG_RDOCOOLINGDZ),(MAP_TPCG(1,53),   141
     *MTPCG_RDOCOOLINGDZ)                                                 141
      CHARACTER CTPCG_RDOCOOLINGDZ*40/' '/                                141
      CHARACTER NTPCG_RDOCOOLINGDZ*12/'R.RDOCOOLINGDZ'/                   141
      INTEGER MTPCG_RDOCOOLINGDZ(2)/1,1/                                  141
      REAL RE_TPCG_RDOCOOLINGDZ                                           141
C -                                                                       141
      REAL TPCG_ZGATINGGRID                                               141
      EQUIVALENCE (BANK_TPCG(56),TPCG_ZGATINGGRID,RE_TPCG_ZGATINGGRID),   141
     *(NAMES_TPCG(54),NTPCG_ZGATINGGRID)                                  141
      EQUIVALENCE (COMMEN_TPCG(54),CTPCG_ZGATINGGRID),(MAP_TPCG(1,54),    141
     *MTPCG_ZGATINGGRID)                                                  141
      CHARACTER CTPCG_ZGATINGGRID*40/' '/                                 141
      CHARACTER NTPCG_ZGATINGGRID*12/'R.ZGATINGGRID'/                     141
      INTEGER MTPCG_ZGATINGGRID(2)/1,1/                                   141
      REAL RE_TPCG_ZGATINGGRID                                            141
C -                                                                       141
      REAL TPCG_ZGROUNDGRID                                               141
      EQUIVALENCE (BANK_TPCG(57),TPCG_ZGROUNDGRID,RE_TPCG_ZGROUNDGRID),   141
     *(NAMES_TPCG(55),NTPCG_ZGROUNDGRID)                                  141
      EQUIVALENCE (COMMEN_TPCG(55),CTPCG_ZGROUNDGRID),(MAP_TPCG(1,55),    141
     *MTPCG_ZGROUNDGRID)                                                  141
      CHARACTER CTPCG_ZGROUNDGRID*40/' '/                                 141
      CHARACTER NTPCG_ZGROUNDGRID*12/'R.ZGROUNDGRID'/                     141
      INTEGER MTPCG_ZGROUNDGRID(2)/1,1/                                   141
      REAL RE_TPCG_ZGROUNDGRID                                            141
C -                                                                       141
      REAL TPCG_DEADZONE                                                  141
      EQUIVALENCE (BANK_TPCG(58),TPCG_DEADZONE,RE_TPCG_DEADZONE),         141
     *(NAMES_TPCG(56),NTPCG_DEADZONE)                                     141
      EQUIVALENCE (COMMEN_TPCG(56),CTPCG_DEADZONE),(MAP_TPCG(1,56),       141
     *MTPCG_DEADZONE)                                                     141
      CHARACTER CTPCG_DEADZONE*40/' '/                                    141
      CHARACTER NTPCG_DEADZONE*12/'R.DEADZONE'/                           141
      INTEGER MTPCG_DEADZONE(2)/1,1/                                      141
      REAL RE_TPCG_DEADZONE                                               141
C -                                                                       141
      REAL TPCG_DANODE(2)                                                 141
      EQUIVALENCE (BANK_TPCG(59),TPCG_DANODE,RE_TPCG_DANODE),             141
     *(NAMES_TPCG(57),NTPCG_DANODE)                                       141
      EQUIVALENCE (COMMEN_TPCG(57),CTPCG_DANODE),(MAP_TPCG(1,57),         141
     *MTPCG_DANODE)                                                       141
      CHARACTER CTPCG_DANODE*40/' '/                                      141
      CHARACTER NTPCG_DANODE*12/'R.DANODE'/                               141
      INTEGER MTPCG_DANODE(2)/2,1/                                        141
      REAL RE_TPCG_DANODE                                                 141
C -                                                                       141
      REAL TPCG_TOFMAX                                                    141
      EQUIVALENCE (BANK_TPCG(61),TPCG_TOFMAX,RE_TPCG_TOFMAX),             141
     *(NAMES_TPCG(58),NTPCG_TOFMAX)                                       141
      EQUIVALENCE (COMMEN_TPCG(58),CTPCG_TOFMAX),(MAP_TPCG(1,58),         141
     *MTPCG_TOFMAX)                                                       141
      CHARACTER CTPCG_TOFMAX*40/' '/                                      141
      CHARACTER NTPCG_TOFMAX*12/'R.TOFMAX'/                               141
      INTEGER MTPCG_TOFMAX(2)/1,1/                                        141
      REAL RE_TPCG_TOFMAX                                                 141
C -                                                                       141
      INTEGER LEN_TPCG,LLEN_TPCG,LINK_TPCG                                141
      PARAMETER (LEN_TPCG=61)                                             141
      PARAMETER (LLEN_TPCG=58)                                            141
      INTEGER FLAG_TPCG/0/,MAP_TPCG(2,LLEN_TPCG),IDNUM_TPCG(10)           141
      CHARACTER NAMES_TPCG(LLEN_TPCG)*12,COMMEN_TPCG(LLEN_TPCG)*40        141
      DATA IDNUM_TPCG/10*0/                                               141
      REAL BANK_TPCG(LEN_TPCG)                                            141
      CHARACTER*60 BPATH_TPCG/' '/                                        141
      SAVE FLAG_TPCG,MAP_TPCG,IDNUM_TPCG,NAMES_TPCG,COMMEN_TPCG,          141
     *BPATH_TPCG                                                          141
      DATA LINK_TPCG/0/,BANK_TPCG/LEN_TPCG*0/                             141
C *                                                                       141
C -                                                                       142
      REAL TPRS_SEC                                                       142
      EQUIVALENCE (BANK_TPRS(3),TPRS_SEC,RE_TPRS_SEC),(NAMES_TPRS(1),     142
     *NTPRS_SEC)                                                          142
      EQUIVALENCE (COMMEN_TPRS(1),CTPRS_SEC),(MAP_TPRS(1,1),MTPRS_SEC)    142
      CHARACTER CTPRS_SEC*40/' '/                                         142
      CHARACTER NTPRS_SEC*12/'R.SEC'/                                     142
      INTEGER MTPRS_SEC(2)/1,1/                                           142
      REAL RE_TPRS_SEC                                                    142
C -                                                                       142
      REAL TPRS_NROW                                                      142
      EQUIVALENCE (BANK_TPRS(4),TPRS_NROW,RE_TPRS_NROW),(NAMES_TPRS(2),   142
     *NTPRS_NROW)                                                         142
      EQUIVALENCE (COMMEN_TPRS(2),CTPRS_NROW),(MAP_TPRS(1,2),             142
     *MTPRS_NROW)                                                         142
      CHARACTER CTPRS_NROW*40/' '/                                        142
      CHARACTER NTPRS_NROW*12/'R.NROW'/                                   142
      INTEGER MTPRS_NROW(2)/1,1/                                          142
      REAL RE_TPRS_NROW                                                   142
C -                                                                       142
      REAL TPRS_PITCH                                                     142
      EQUIVALENCE (BANK_TPRS(5),TPRS_PITCH,RE_TPRS_PITCH),                142
     *(NAMES_TPRS(3),NTPRS_PITCH)                                         142
      EQUIVALENCE (COMMEN_TPRS(3),CTPRS_PITCH),(MAP_TPRS(1,3),            142
     *MTPRS_PITCH)                                                        142
      CHARACTER CTPRS_PITCH*40/' '/                                       142
      CHARACTER NTPRS_PITCH*12/'R.PITCH'/                                 142
      INTEGER MTPRS_PITCH(2)/1,1/                                         142
      REAL RE_TPRS_PITCH                                                  142
C -                                                                       142
      REAL TPRS_WIDTH                                                     142
      EQUIVALENCE (BANK_TPRS(6),TPRS_WIDTH,RE_TPRS_WIDTH),                142
     *(NAMES_TPRS(4),NTPRS_WIDTH)                                         142
      EQUIVALENCE (COMMEN_TPRS(4),CTPRS_WIDTH),(MAP_TPRS(1,4),            142
     *MTPRS_WIDTH)                                                        142
      CHARACTER CTPRS_WIDTH*40/' '/                                       142
      CHARACTER NTPRS_WIDTH*12/'R.WIDTH'/                                 142
      INTEGER MTPRS_WIDTH(2)/1,1/                                         142
      REAL RE_TPRS_WIDTH                                                  142
C -                                                                       142
      REAL TPRS_SUPER                                                     142
      EQUIVALENCE (BANK_TPRS(7),TPRS_SUPER,RE_TPRS_SUPER),                142
     *(NAMES_TPRS(5),NTPRS_SUPER)                                         142
      EQUIVALENCE (COMMEN_TPRS(5),CTPRS_SUPER),(MAP_TPRS(1,5),            142
     *MTPRS_SUPER)                                                        142
      CHARACTER CTPRS_SUPER*40/' '/                                       142
      CHARACTER NTPRS_SUPER*12/'R.SUPER'/                                 142
      INTEGER MTPRS_SUPER(2)/1,1/                                         142
      REAL RE_TPRS_SUPER                                                  142
C -                                                                       142
      REAL TPRS_DANODE                                                    142
      EQUIVALENCE (BANK_TPRS(8),TPRS_DANODE,RE_TPRS_DANODE),              142
     *(NAMES_TPRS(6),NTPRS_DANODE)                                        142
      EQUIVALENCE (COMMEN_TPRS(6),CTPRS_DANODE),(MAP_TPRS(1,6),           142
     *MTPRS_DANODE)                                                       142
      CHARACTER CTPRS_DANODE*40/' '/                                      142
      CHARACTER NTPRS_DANODE*12/'R.DANODE'/                               142
      INTEGER MTPRS_DANODE(2)/1,1/                                        142
      REAL RE_TPRS_DANODE                                                 142
C -                                                                       142
      REAL TPRS_RPADS(40)                                                 142
      EQUIVALENCE (BANK_TPRS(9),TPRS_RPADS,RE_TPRS_RPADS),                142
     *(NAMES_TPRS(7),NTPRS_RPADS)                                         142
      EQUIVALENCE (COMMEN_TPRS(7),CTPRS_RPADS),(MAP_TPRS(1,7),            142
     *MTPRS_RPADS)                                                        142
      CHARACTER CTPRS_RPADS*40/' '/                                       142
      CHARACTER NTPRS_RPADS*12/'R.RPADS'/                                 142
      INTEGER MTPRS_RPADS(2)/40,1/                                        142
      REAL RE_TPRS_RPADS                                                  142
C -                                                                       142
      REAL TPRS_NPADS(40)                                                 142
      EQUIVALENCE (BANK_TPRS(49),TPRS_NPADS,RE_TPRS_NPADS),               142
     *(NAMES_TPRS(8),NTPRS_NPADS)                                         142
      EQUIVALENCE (COMMEN_TPRS(8),CTPRS_NPADS),(MAP_TPRS(1,8),            142
     *MTPRS_NPADS)                                                        142
      CHARACTER CTPRS_NPADS*40/' '/                                       142
      CHARACTER NTPRS_NPADS*12/'R.NPADS'/                                 142
      INTEGER MTPRS_NPADS(2)/40,1/                                        142
      REAL RE_TPRS_NPADS                                                  142
C -                                                                       142
      INTEGER LEN_TPRS,LLEN_TPRS,LINK_TPRS                                142
      PARAMETER (LEN_TPRS=88)                                             142
      PARAMETER (LLEN_TPRS=8)                                             142
      INTEGER FLAG_TPRS/0/,MAP_TPRS(2,LLEN_TPRS),IDNUM_TPRS(10)           142
      CHARACTER NAMES_TPRS(LLEN_TPRS)*12,COMMEN_TPRS(LLEN_TPRS)*40        142
      DATA IDNUM_TPRS/10*0/                                               142
      REAL BANK_TPRS(LEN_TPRS)                                            142
      CHARACTER*60 BPATH_TPRS/' '/                                        142
      SAVE FLAG_TPRS,MAP_TPRS,IDNUM_TPRS,NAMES_TPRS,COMMEN_TPRS,          142
     *BPATH_TPRS                                                          142
      DATA LINK_TPRS/0/,BANK_TPRS/LEN_TPRS*0/                             142
C *                                                                       142
C -                                                                       144
      REAL TECW_SEC                                                       144
      EQUIVALENCE (BANK_TECW(3),TECW_SEC,RE_TECW_SEC),(NAMES_TECW(1),     144
     *NTECW_SEC)                                                          144
      EQUIVALENCE (COMMEN_TECW(1),CTECW_SEC),(MAP_TECW(1,1),MTECW_SEC)    144
      CHARACTER CTECW_SEC*40/' '/                                         144
      CHARACTER NTECW_SEC*12/'R.SEC'/                                     144
      INTEGER MTECW_SEC(2)/1,1/                                           144
      REAL RE_TECW_SEC                                                    144
C -                                                                       144
      REAL TECW_GAPWIDI                                                   144
      EQUIVALENCE (BANK_TECW(4),TECW_GAPWIDI,RE_TECW_GAPWIDI),            144
     *(NAMES_TECW(2),NTECW_GAPWIDI)                                       144
      EQUIVALENCE (COMMEN_TECW(2),CTECW_GAPWIDI),(MAP_TECW(1,2),          144
     *MTECW_GAPWIDI)                                                      144
      CHARACTER CTECW_GAPWIDI*40/' '/                                     144
      CHARACTER NTECW_GAPWIDI*12/'R.GAPWIDI'/                             144
      INTEGER MTECW_GAPWIDI(2)/1,1/                                       144
      REAL RE_TECW_GAPWIDI                                                144
C -                                                                       144
      REAL TECW_GAPWIDO                                                   144
      EQUIVALENCE (BANK_TECW(5),TECW_GAPWIDO,RE_TECW_GAPWIDO),            144
     *(NAMES_TECW(3),NTECW_GAPWIDO)                                       144
      EQUIVALENCE (COMMEN_TECW(3),CTECW_GAPWIDO),(MAP_TECW(1,3),          144
     *MTECW_GAPWIDO)                                                      144
      CHARACTER CTECW_GAPWIDO*40/' '/                                     144
      CHARACTER NTECW_GAPWIDO*12/'R.GAPWIDO'/                             144
      INTEGER MTECW_GAPWIDO(2)/1,1/                                       144
      REAL RE_TECW_GAPWIDO                                                144
C -                                                                       144
      REAL TECW_GAPHEIT                                                   144
      EQUIVALENCE (BANK_TECW(6),TECW_GAPHEIT,RE_TECW_GAPHEIT),            144
     *(NAMES_TECW(4),NTECW_GAPHEIT)                                       144
      EQUIVALENCE (COMMEN_TECW(4),CTECW_GAPHEIT),(MAP_TECW(1,4),          144
     *MTECW_GAPHEIT)                                                      144
      CHARACTER CTECW_GAPHEIT*40/' '/                                     144
      CHARACTER NTECW_GAPHEIT*12/'R.GAPHEIT'/                             144
      INTEGER MTECW_GAPHEIT(2)/1,1/                                       144
      REAL RE_TECW_GAPHEIT                                                144
C -                                                                       144
      REAL TECW_GAPRAD                                                    144
      EQUIVALENCE (BANK_TECW(7),TECW_GAPRAD,RE_TECW_GAPRAD),              144
     *(NAMES_TECW(5),NTECW_GAPRAD)                                        144
      EQUIVALENCE (COMMEN_TECW(5),CTECW_GAPRAD),(MAP_TECW(1,5),           144
     *MTECW_GAPRAD)                                                       144
      CHARACTER CTECW_GAPRAD*40/' '/                                      144
      CHARACTER NTECW_GAPRAD*12/'R.GAPRAD'/                               144
      INTEGER MTECW_GAPRAD(2)/1,1/                                        144
      REAL RE_TECW_GAPRAD                                                 144
C -                                                                       144
      REAL TECW_INWIDTH                                                   144
      EQUIVALENCE (BANK_TECW(8),TECW_INWIDTH,RE_TECW_INWIDTH),            144
     *(NAMES_TECW(6),NTECW_INWIDTH)                                       144
      EQUIVALENCE (COMMEN_TECW(6),CTECW_INWIDTH),(MAP_TECW(1,6),          144
     *MTECW_INWIDTH)                                                      144
      CHARACTER CTECW_INWIDTH*40/' '/                                     144
      CHARACTER NTECW_INWIDTH*12/'R.INWIDTH'/                             144
      INTEGER MTECW_INWIDTH(2)/1,1/                                       144
      REAL RE_TECW_INWIDTH                                                144
C -                                                                       144
      REAL TECW_OUWIDTH                                                   144
      EQUIVALENCE (BANK_TECW(9),TECW_OUWIDTH,RE_TECW_OUWIDTH),            144
     *(NAMES_TECW(7),NTECW_OUWIDTH)                                       144
      EQUIVALENCE (COMMEN_TECW(7),CTECW_OUWIDTH),(MAP_TECW(1,7),          144
     *MTECW_OUWIDTH)                                                      144
      CHARACTER CTECW_OUWIDTH*40/' '/                                     144
      CHARACTER NTECW_OUWIDTH*12/'R.OUWIDTH'/                             144
      INTEGER MTECW_OUWIDTH(2)/1,1/                                       144
      REAL RE_TECW_OUWIDTH                                                144
C -                                                                       144
      REAL TECW_WIDTHLIP                                                  144
      EQUIVALENCE (BANK_TECW(10),TECW_WIDTHLIP,RE_TECW_WIDTHLIP),         144
     *(NAMES_TECW(8),NTECW_WIDTHLIP)                                      144
      EQUIVALENCE (COMMEN_TECW(8),CTECW_WIDTHLIP),(MAP_TECW(1,8),         144
     *MTECW_WIDTHLIP)                                                     144
      CHARACTER CTECW_WIDTHLIP*40/' '/                                    144
      CHARACTER NTECW_WIDTHLIP*12/'R.WIDTHLIP'/                           144
      INTEGER MTECW_WIDTHLIP(2)/1,1/                                      144
      REAL RE_TECW_WIDTHLIP                                               144
C -                                                                       144
      REAL TECW_NORIBS                                                    144
      EQUIVALENCE (BANK_TECW(11),TECW_NORIBS,RE_TECW_NORIBS),             144
     *(NAMES_TECW(9),NTECW_NORIBS)                                        144
      EQUIVALENCE (COMMEN_TECW(9),CTECW_NORIBS),(MAP_TECW(1,9),           144
     *MTECW_NORIBS)                                                       144
      CHARACTER CTECW_NORIBS*40/' '/                                      144
      CHARACTER NTECW_NORIBS*12/'R.NORIBS'/                               144
      INTEGER MTECW_NORIBS(2)/1,1/                                        144
      REAL RE_TECW_NORIBS                                                 144
C -                                                                       144
      REAL TECW_ZSTEPRIB                                                  144
      EQUIVALENCE (BANK_TECW(12),TECW_ZSTEPRIB,RE_TECW_ZSTEPRIB),         144
     *(NAMES_TECW(10),NTECW_ZSTEPRIB)                                     144
      EQUIVALENCE (COMMEN_TECW(10),CTECW_ZSTEPRIB),(MAP_TECW(1,10),       144
     *MTECW_ZSTEPRIB)                                                     144
      CHARACTER CTECW_ZSTEPRIB*40/' '/                                    144
      CHARACTER NTECW_ZSTEPRIB*12/'R.ZSTEPRIB'/                           144
      INTEGER MTECW_ZSTEPRIB(2)/1,1/                                      144
      REAL RE_TECW_ZSTEPRIB                                               144
C -                                                                       144
      REAL TECW_WIDTHRIB                                                  144
      EQUIVALENCE (BANK_TECW(13),TECW_WIDTHRIB,RE_TECW_WIDTHRIB),         144
     *(NAMES_TECW(11),NTECW_WIDTHRIB)                                     144
      EQUIVALENCE (COMMEN_TECW(11),CTECW_WIDTHRIB),(MAP_TECW(1,11),       144
     *MTECW_WIDTHRIB)                                                     144
      CHARACTER CTECW_WIDTHRIB*40/' '/                                    144
      CHARACTER NTECW_WIDTHRIB*12/'R.WIDTHRIB'/                           144
      INTEGER MTECW_WIDTHRIB(2)/1,1/                                      144
      REAL RE_TECW_WIDTHRIB                                               144
C -                                                                       144
      REAL TECW_HEIGHT                                                    144
      EQUIVALENCE (BANK_TECW(14),TECW_HEIGHT,RE_TECW_HEIGHT),             144
     *(NAMES_TECW(12),NTECW_HEIGHT)                                       144
      EQUIVALENCE (COMMEN_TECW(12),CTECW_HEIGHT),(MAP_TECW(1,12),         144
     *MTECW_HEIGHT)                                                       144
      CHARACTER CTECW_HEIGHT*40/' '/                                      144
      CHARACTER NTECW_HEIGHT*12/'R.HEIGHT'/                               144
      INTEGER MTECW_HEIGHT(2)/1,1/                                        144
      REAL RE_TECW_HEIGHT                                                 144
C -                                                                       144
      REAL TECW_THICK                                                     144
      EQUIVALENCE (BANK_TECW(15),TECW_THICK,RE_TECW_THICK),               144
     *(NAMES_TECW(13),NTECW_THICK)                                        144
      EQUIVALENCE (COMMEN_TECW(13),CTECW_THICK),(MAP_TECW(1,13),          144
     *MTECW_THICK)                                                        144
      CHARACTER CTECW_THICK*40/' '/                                       144
      CHARACTER NTECW_THICK*12/'R.THICK'/                                 144
      INTEGER MTECW_THICK(2)/1,1/                                         144
      REAL RE_TECW_THICK                                                  144
C -                                                                       144
      REAL TECW_THICKAL                                                   144
      EQUIVALENCE (BANK_TECW(16),TECW_THICKAL,RE_TECW_THICKAL),           144
     *(NAMES_TECW(14),NTECW_THICKAL)                                      144
      EQUIVALENCE (COMMEN_TECW(14),CTECW_THICKAL),(MAP_TECW(1,14),        144
     *MTECW_THICKAL)                                                      144
      CHARACTER CTECW_THICKAL*40/' '/                                     144
      CHARACTER NTECW_THICKAL*12/'R.THICKAL'/                             144
      INTEGER MTECW_THICKAL(2)/1,1/                                       144
      REAL RE_TECW_THICKAL                                                144
C -                                                                       144
      REAL TECW_RMIN                                                      144
      EQUIVALENCE (BANK_TECW(17),TECW_RMIN,RE_TECW_RMIN),                 144
     *(NAMES_TECW(15),NTECW_RMIN)                                         144
      EQUIVALENCE (COMMEN_TECW(15),CTECW_RMIN),(MAP_TECW(1,15),           144
     *MTECW_RMIN)                                                         144
      CHARACTER CTECW_RMIN*40/' '/                                        144
      CHARACTER NTECW_RMIN*12/'R.RMIN'/                                   144
      INTEGER MTECW_RMIN(2)/1,1/                                          144
      REAL RE_TECW_RMIN                                                   144
C -                                                                       144
      REAL TECW_RCENTER                                                   144
      EQUIVALENCE (BANK_TECW(18),TECW_RCENTER,RE_TECW_RCENTER),           144
     *(NAMES_TECW(16),NTECW_RCENTER)                                      144
      EQUIVALENCE (COMMEN_TECW(16),CTECW_RCENTER),(MAP_TECW(1,16),        144
     *MTECW_RCENTER)                                                      144
      CHARACTER CTECW_RCENTER*40/' '/                                     144
      CHARACTER NTECW_RCENTER*12/'R.RCENTER'/                             144
      INTEGER MTECW_RCENTER(2)/1,1/                                       144
      REAL RE_TECW_RCENTER                                                144
C -                                                                       144
      REAL TECW_HOLEDX                                                    144
      EQUIVALENCE (BANK_TECW(19),TECW_HOLEDX,RE_TECW_HOLEDX),             144
     *(NAMES_TECW(17),NTECW_HOLEDX)                                       144
      EQUIVALENCE (COMMEN_TECW(17),CTECW_HOLEDX),(MAP_TECW(1,17),         144
     *MTECW_HOLEDX)                                                       144
      CHARACTER CTECW_HOLEDX*40/' '/                                      144
      CHARACTER NTECW_HOLEDX*12/'R.HOLEDX'/                               144
      INTEGER MTECW_HOLEDX(2)/1,1/                                        144
      REAL RE_TECW_HOLEDX                                                 144
C -                                                                       144
      REAL TECW_HOLEDY                                                    144
      EQUIVALENCE (BANK_TECW(20),TECW_HOLEDY,RE_TECW_HOLEDY),             144
     *(NAMES_TECW(18),NTECW_HOLEDY)                                       144
      EQUIVALENCE (COMMEN_TECW(18),CTECW_HOLEDY),(MAP_TECW(1,18),         144
     *MTECW_HOLEDY)                                                       144
      CHARACTER CTECW_HOLEDY*40/' '/                                      144
      CHARACTER NTECW_HOLEDY*12/'R.HOLEDY'/                               144
      INTEGER MTECW_HOLEDY(2)/1,1/                                        144
      REAL RE_TECW_HOLEDY                                                 144
C -                                                                       144
      INTEGER LEN_TECW,LLEN_TECW,LINK_TECW                                144
      PARAMETER (LEN_TECW=20)                                             144
      PARAMETER (LLEN_TECW=18)                                            144
      INTEGER FLAG_TECW/0/,MAP_TECW(2,LLEN_TECW),IDNUM_TECW(10)           144
      CHARACTER NAMES_TECW(LLEN_TECW)*12,COMMEN_TECW(LLEN_TECW)*40        144
      DATA IDNUM_TECW/10*0/                                               144
      REAL BANK_TECW(LEN_TECW)                                            144
      CHARACTER*60 BPATH_TECW/' '/                                        144
      SAVE FLAG_TECW,MAP_TECW,IDNUM_TECW,NAMES_TECW,COMMEN_TECW,          144
     *BPATH_TECW                                                          144
      DATA LINK_TECW/0/,BANK_TECW/LEN_TECW*0/                             144
C *                                                                       144
C -                                                                       145
      REAL TPCR_RDOVTHK                                                   145
      EQUIVALENCE (BANK_TPCR(3),TPCR_RDOVTHK,RE_TPCR_RDOVTHK),            145
     *(NAMES_TPCR(1),NTPCR_RDOVTHK)                                       145
      EQUIVALENCE (COMMEN_TPCR(1),CTPCR_RDOVTHK),(MAP_TPCR(1,1),          145
     *MTPCR_RDOVTHK)                                                      145
      CHARACTER CTPCR_RDOVTHK*40/' '/                                     145
      CHARACTER NTPCR_RDOVTHK*12/'R.RDOVTHK'/                             145
      INTEGER MTPCR_RDOVTHK(2)/1,1/                                       145
      REAL RE_TPCR_RDOVTHK                                                145
C -                                                                       145
      REAL TPCR_RDOTHK                                                    145
      EQUIVALENCE (BANK_TPCR(4),TPCR_RDOTHK,RE_TPCR_RDOTHK),              145
     *(NAMES_TPCR(2),NTPCR_RDOTHK)                                        145
      EQUIVALENCE (COMMEN_TPCR(2),CTPCR_RDOTHK),(MAP_TPCR(1,2),           145
     *MTPCR_RDOTHK)                                                       145
      CHARACTER CTPCR_RDOTHK*40/' '/                                      145
      CHARACTER NTPCR_RDOTHK*12/'R.RDOTHK'/                               145
      INTEGER MTPCR_RDOTHK(2)/1,1/                                        145
      REAL RE_TPCR_RDOTHK                                                 145
C -                                                                       145
      REAL TPCR_RDOLEN                                                    145
      EQUIVALENCE (BANK_TPCR(5),TPCR_RDOLEN,RE_TPCR_RDOLEN),              145
     *(NAMES_TPCR(3),NTPCR_RDOLEN)                                        145
      EQUIVALENCE (COMMEN_TPCR(3),CTPCR_RDOLEN),(MAP_TPCR(1,3),           145
     *MTPCR_RDOLEN)                                                       145
      CHARACTER CTPCR_RDOLEN*40/' '/                                      145
      CHARACTER NTPCR_RDOLEN*12/'R.RDOLEN'/                               145
      INTEGER MTPCR_RDOLEN(2)/1,1/                                        145
      REAL RE_TPCR_RDOLEN                                                 145
C -                                                                       145
      REAL TPCR_NRDOBRD                                                   145
      EQUIVALENCE (BANK_TPCR(6),TPCR_NRDOBRD,RE_TPCR_NRDOBRD),            145
     *(NAMES_TPCR(4),NTPCR_NRDOBRD)                                       145
      EQUIVALENCE (COMMEN_TPCR(4),CTPCR_NRDOBRD),(MAP_TPCR(1,4),          145
     *MTPCR_NRDOBRD)                                                      145
      CHARACTER CTPCR_NRDOBRD*40/' '/                                     145
      CHARACTER NTPCR_NRDOBRD*12/'R.NRDOBRD'/                             145
      INTEGER MTPCR_NRDOBRD(2)/1,1/                                       145
      REAL RE_TPCR_NRDOBRD                                                145
C -                                                                       145
      REAL TPCR_RDOHT(9)                                                  145
      EQUIVALENCE (BANK_TPCR(7),TPCR_RDOHT,RE_TPCR_RDOHT),                145
     *(NAMES_TPCR(5),NTPCR_RDOHT)                                         145
      EQUIVALENCE (COMMEN_TPCR(5),CTPCR_RDOHT),(MAP_TPCR(1,5),            145
     *MTPCR_RDOHT)                                                        145
      CHARACTER CTPCR_RDOHT*40/' '/                                       145
      CHARACTER NTPCR_RDOHT*12/'R.RDOHT'/                                 145
      INTEGER MTPCR_RDOHT(2)/9,1/                                         145
      REAL RE_TPCR_RDOHT                                                  145
C -                                                                       145
      INTEGER LEN_TPCR,LLEN_TPCR,LINK_TPCR                                145
      PARAMETER (LEN_TPCR=15)                                             145
      PARAMETER (LLEN_TPCR=5)                                             145
      INTEGER FLAG_TPCR/0/,MAP_TPCR(2,LLEN_TPCR),IDNUM_TPCR(10)           145
      CHARACTER NAMES_TPCR(LLEN_TPCR)*12,COMMEN_TPCR(LLEN_TPCR)*40        145
      DATA IDNUM_TPCR/10*0/                                               145
      REAL BANK_TPCR(LEN_TPCR)                                            145
      CHARACTER*60 BPATH_TPCR/' '/                                        145
      SAVE FLAG_TPCR,MAP_TPCR,IDNUM_TPCR,NAMES_TPCR,COMMEN_TPCR,          145
     *BPATH_TPCR                                                          145
      DATA LINK_TPCR/0/,BANK_TPCR/LEN_TPCR*0/                             145
C *                                                                       145
C -                                                                       147
      REAL TFEE_VERS                                                      147
      EQUIVALENCE (BANK_TFEE(3),TFEE_VERS,RE_TFEE_VERS),(NAMES_TFEE(1),   147
     *NTFEE_VERS)                                                         147
      EQUIVALENCE (COMMEN_TFEE(1),CTFEE_VERS),(MAP_TFEE(1,1),             147
     *MTFEE_VERS)                                                         147
      CHARACTER CTFEE_VERS*40/' '/                                        147
      CHARACTER NTFEE_VERS*12/'R.VERS'/                                   147
      INTEGER MTFEE_VERS(2)/1,1/                                          147
      REAL RE_TFEE_VERS                                                   147
C -                                                                       147
      REAL TFEE_CARDDX                                                    147
      EQUIVALENCE (BANK_TFEE(4),TFEE_CARDDX,RE_TFEE_CARDDX),              147
     *(NAMES_TFEE(2),NTFEE_CARDDX)                                        147
      EQUIVALENCE (COMMEN_TFEE(2),CTFEE_CARDDX),(MAP_TFEE(1,2),           147
     *MTFEE_CARDDX)                                                       147
      CHARACTER CTFEE_CARDDX*40/' '/                                      147
      CHARACTER NTFEE_CARDDX*12/'R.CARDDX'/                               147
      INTEGER MTFEE_CARDDX(2)/1,1/                                        147
      REAL RE_TFEE_CARDDX                                                 147
C -                                                                       147
      REAL TFEE_CARDDY                                                    147
      EQUIVALENCE (BANK_TFEE(5),TFEE_CARDDY,RE_TFEE_CARDDY),              147
     *(NAMES_TFEE(3),NTFEE_CARDDY)                                        147
      EQUIVALENCE (COMMEN_TFEE(3),CTFEE_CARDDY),(MAP_TFEE(1,3),           147
     *MTFEE_CARDDY)                                                       147
      CHARACTER CTFEE_CARDDY*40/' '/                                      147
      CHARACTER NTFEE_CARDDY*12/'R.CARDDY'/                               147
      INTEGER MTFEE_CARDDY(2)/1,1/                                        147
      REAL RE_TFEE_CARDDY                                                 147
C -                                                                       147
      REAL TFEE_CARDDZ                                                    147
      EQUIVALENCE (BANK_TFEE(6),TFEE_CARDDZ,RE_TFEE_CARDDZ),              147
     *(NAMES_TFEE(4),NTFEE_CARDDZ)                                        147
      EQUIVALENCE (COMMEN_TFEE(4),CTFEE_CARDDZ),(MAP_TFEE(1,4),           147
     *MTFEE_CARDDZ)                                                       147
      CHARACTER CTFEE_CARDDZ*40/' '/                                      147
      CHARACTER NTFEE_CARDDZ*12/'R.CARDDZ'/                               147
      INTEGER MTFEE_CARDDZ(2)/1,1/                                        147
      REAL RE_TFEE_CARDDZ                                                 147
C -                                                                       147
      REAL TFEE_PLATEDX                                                   147
      EQUIVALENCE (BANK_TFEE(7),TFEE_PLATEDX,RE_TFEE_PLATEDX),            147
     *(NAMES_TFEE(5),NTFEE_PLATEDX)                                       147
      EQUIVALENCE (COMMEN_TFEE(5),CTFEE_PLATEDX),(MAP_TFEE(1,5),          147
     *MTFEE_PLATEDX)                                                      147
      CHARACTER CTFEE_PLATEDX*40/' '/                                     147
      CHARACTER NTFEE_PLATEDX*12/'R.PLATEDX'/                             147
      INTEGER MTFEE_PLATEDX(2)/1,1/                                       147
      REAL RE_TFEE_PLATEDX                                                147
C -                                                                       147
      REAL TFEE_PLATEDY                                                   147
      EQUIVALENCE (BANK_TFEE(8),TFEE_PLATEDY,RE_TFEE_PLATEDY),            147
     *(NAMES_TFEE(6),NTFEE_PLATEDY)                                       147
      EQUIVALENCE (COMMEN_TFEE(6),CTFEE_PLATEDY),(MAP_TFEE(1,6),          147
     *MTFEE_PLATEDY)                                                      147
      CHARACTER CTFEE_PLATEDY*40/' '/                                     147
      CHARACTER NTFEE_PLATEDY*12/'R.PLATEDY'/                             147
      INTEGER MTFEE_PLATEDY(2)/1,1/                                       147
      REAL RE_TFEE_PLATEDY                                                147
C -                                                                       147
      REAL TFEE_PLATEDZ                                                   147
      EQUIVALENCE (BANK_TFEE(9),TFEE_PLATEDZ,RE_TFEE_PLATEDZ),            147
     *(NAMES_TFEE(7),NTFEE_PLATEDZ)                                       147
      EQUIVALENCE (COMMEN_TFEE(7),CTFEE_PLATEDZ),(MAP_TFEE(1,7),          147
     *MTFEE_PLATEDZ)                                                      147
      CHARACTER CTFEE_PLATEDZ*40/' '/                                     147
      CHARACTER NTFEE_PLATEDZ*12/'R.PLATEDZ'/                             147
      INTEGER MTFEE_PLATEDZ(2)/1,1/                                       147
      REAL RE_TFEE_PLATEDZ                                                147
C -                                                                       147
      REAL TFEE_ASSEMBLYTHICKNESS                                         147
      EQUIVALENCE (BANK_TFEE(10),TFEE_ASSEMBLYTHICKNESS,                  147
     *RE_TFEE_ASSEMBLYTHICKNESS),(NAMES_TFEE(8),                          147
     *NTFEE_ASSEMBLYTHICKNESS)                                            147
      EQUIVALENCE (COMMEN_TFEE(8),CTFEE_ASSEMBLYTHICKNESS),(MAP_TFEE(1,   147
     *8),MTFEE_ASSEMBLYTHICKNESS)                                         147
      CHARACTER CTFEE_ASSEMBLYTHICKNESS*40/' '/                           147
      CHARACTER NTFEE_ASSEMBLYTHICKNESS*12/'R.ASSEMBLYTHICKNESS'/         147
      INTEGER MTFEE_ASSEMBLYTHICKNESS(2)/1,1/                             147
      REAL RE_TFEE_ASSEMBLYTHICKNESS                                      147
C -                                                                       147
      REAL TFEE_RIBDX                                                     147
      EQUIVALENCE (BANK_TFEE(11),TFEE_RIBDX,RE_TFEE_RIBDX),               147
     *(NAMES_TFEE(9),NTFEE_RIBDX)                                         147
      EQUIVALENCE (COMMEN_TFEE(9),CTFEE_RIBDX),(MAP_TFEE(1,9),            147
     *MTFEE_RIBDX)                                                        147
      CHARACTER CTFEE_RIBDX*40/' '/                                       147
      CHARACTER NTFEE_RIBDX*12/'R.RIBDX'/                                 147
      INTEGER MTFEE_RIBDX(2)/1,1/                                         147
      REAL RE_TFEE_RIBDX                                                  147
C -                                                                       147
      REAL TFEE_RIBDY                                                     147
      EQUIVALENCE (BANK_TFEE(12),TFEE_RIBDY,RE_TFEE_RIBDY),               147
     *(NAMES_TFEE(10),NTFEE_RIBDY)                                        147
      EQUIVALENCE (COMMEN_TFEE(10),CTFEE_RIBDY),(MAP_TFEE(1,10),          147
     *MTFEE_RIBDY)                                                        147
      CHARACTER CTFEE_RIBDY*40/' '/                                       147
      CHARACTER NTFEE_RIBDY*12/'R.RIBDY'/                                 147
      INTEGER MTFEE_RIBDY(2)/1,1/                                         147
      REAL RE_TFEE_RIBDY                                                  147
C -                                                                       147
      REAL TFEE_RIBDZ                                                     147
      EQUIVALENCE (BANK_TFEE(13),TFEE_RIBDZ,RE_TFEE_RIBDZ),               147
     *(NAMES_TFEE(11),NTFEE_RIBDZ)                                        147
      EQUIVALENCE (COMMEN_TFEE(11),CTFEE_RIBDZ),(MAP_TFEE(1,11),          147
     *MTFEE_RIBDZ)                                                        147
      CHARACTER CTFEE_RIBDZ*40/' '/                                       147
      CHARACTER NTFEE_RIBDZ*12/'R.RIBDZ'/                                 147
      INTEGER MTFEE_RIBDZ(2)/1,1/                                         147
      REAL RE_TFEE_RIBDZ                                                  147
C -                                                                       147
      REAL TFEE_ASS(3)                                                    147
      EQUIVALENCE (BANK_TFEE(14),TFEE_ASS,RE_TFEE_ASS),(NAMES_TFEE(12),   147
     *NTFEE_ASS)                                                          147
      EQUIVALENCE (COMMEN_TFEE(12),CTFEE_ASS),(MAP_TFEE(1,12),            147
     *MTFEE_ASS)                                                          147
      CHARACTER CTFEE_ASS*40/' '/                                         147
      CHARACTER NTFEE_ASS*12/'R.ASS'/                                     147
      INTEGER MTFEE_ASS(2)/3,1/                                           147
      REAL RE_TFEE_ASS                                                    147
C -                                                                       147
      INTEGER LEN_TFEE,LLEN_TFEE,LINK_TFEE                                147
      PARAMETER (LEN_TFEE=16)                                             147
      PARAMETER (LLEN_TFEE=12)                                            147
      INTEGER FLAG_TFEE/0/,MAP_TFEE(2,LLEN_TFEE),IDNUM_TFEE(10)           147
      CHARACTER NAMES_TFEE(LLEN_TFEE)*12,COMMEN_TFEE(LLEN_TFEE)*40        147
      DATA IDNUM_TFEE/10*0/                                               147
      REAL BANK_TFEE(LEN_TFEE)                                            147
      CHARACTER*60 BPATH_TFEE/' '/                                        147
      SAVE FLAG_TFEE,MAP_TFEE,IDNUM_TFEE,NAMES_TFEE,COMMEN_TFEE,          147
     *BPATH_TFEE                                                          147
      DATA LINK_TFEE/0/,BANK_TFEE/LEN_TFEE*0/                             147
C *                                                                       147
C -                                                                       148
      REAL RIBS_VERSION                                                   148
      EQUIVALENCE (BANK_RIBS(3),RIBS_VERSION,RE_RIBS_VERSION),            148
     *(NAMES_RIBS(1),NRIBS_VERSION)                                       148
      EQUIVALENCE (COMMEN_RIBS(1),CRIBS_VERSION),(MAP_RIBS(1,1),          148
     *MRIBS_VERSION)                                                      148
      CHARACTER CRIBS_VERSION*40/' '/                                     148
      CHARACTER NRIBS_VERSION*12/'R.VERSION'/                             148
      INTEGER MRIBS_VERSION(2)/1,1/                                       148
      REAL RE_RIBS_VERSION                                                148
C -                                                                       148
      REAL RIBS_INOUT                                                     148
      EQUIVALENCE (BANK_RIBS(4),RIBS_INOUT,RE_RIBS_INOUT),                148
     *(NAMES_RIBS(2),NRIBS_INOUT)                                         148
      EQUIVALENCE (COMMEN_RIBS(2),CRIBS_INOUT),(MAP_RIBS(1,2),            148
     *MRIBS_INOUT)                                                        148
      CHARACTER CRIBS_INOUT*40/' '/                                       148
      CHARACTER NRIBS_INOUT*12/'R.INOUT'/                                 148
      INTEGER MRIBS_INOUT(2)/1,1/                                         148
      REAL RE_RIBS_INOUT                                                  148
C -                                                                       148
      REAL RIBS_NUM                                                       148
      EQUIVALENCE (BANK_RIBS(5),RIBS_NUM,RE_RIBS_NUM),(NAMES_RIBS(3),     148
     *NRIBS_NUM)                                                          148
      EQUIVALENCE (COMMEN_RIBS(3),CRIBS_NUM),(MAP_RIBS(1,3),MRIBS_NUM)    148
      CHARACTER CRIBS_NUM*40/' '/                                         148
      CHARACTER NRIBS_NUM*12/'R.NUM'/                                     148
      INTEGER MRIBS_NUM(2)/1,1/                                           148
      REAL RE_RIBS_NUM                                                    148
C -                                                                       148
      REAL RIBS_X(16)                                                     148
      EQUIVALENCE (BANK_RIBS(6),RIBS_X,RE_RIBS_X),(NAMES_RIBS(4),         148
     *NRIBS_X)                                                            148
      EQUIVALENCE (COMMEN_RIBS(4),CRIBS_X),(MAP_RIBS(1,4),MRIBS_X)        148
      CHARACTER CRIBS_X*40/' '/                                           148
      CHARACTER NRIBS_X*12/'R.X'/                                         148
      INTEGER MRIBS_X(2)/16,1/                                            148
      REAL RE_RIBS_X                                                      148
C -                                                                       148
      REAL RIBS_DX(16)                                                    148
      EQUIVALENCE (BANK_RIBS(22),RIBS_DX,RE_RIBS_DX),(NAMES_RIBS(5),      148
     *NRIBS_DX)                                                           148
      EQUIVALENCE (COMMEN_RIBS(5),CRIBS_DX),(MAP_RIBS(1,5),MRIBS_DX)      148
      CHARACTER CRIBS_DX*40/' '/                                          148
      CHARACTER NRIBS_DX*12/'R.DX'/                                       148
      INTEGER MRIBS_DX(2)/16,1/                                           148
      REAL RE_RIBS_DX                                                     148
C -                                                                       148
      REAL RIBS_Y(16)                                                     148
      EQUIVALENCE (BANK_RIBS(38),RIBS_Y,RE_RIBS_Y),(NAMES_RIBS(6),        148
     *NRIBS_Y)                                                            148
      EQUIVALENCE (COMMEN_RIBS(6),CRIBS_Y),(MAP_RIBS(1,6),MRIBS_Y)        148
      CHARACTER CRIBS_Y*40/' '/                                           148
      CHARACTER NRIBS_Y*12/'R.Y'/                                         148
      INTEGER MRIBS_Y(2)/16,1/                                            148
      REAL RE_RIBS_Y                                                      148
C -                                                                       148
      REAL RIBS_DY(16)                                                    148
      EQUIVALENCE (BANK_RIBS(54),RIBS_DY,RE_RIBS_DY),(NAMES_RIBS(7),      148
     *NRIBS_DY)                                                           148
      EQUIVALENCE (COMMEN_RIBS(7),CRIBS_DY),(MAP_RIBS(1,7),MRIBS_DY)      148
      CHARACTER CRIBS_DY*40/' '/                                          148
      CHARACTER NRIBS_DY*12/'R.DY'/                                       148
      INTEGER MRIBS_DY(2)/16,1/                                           148
      REAL RE_RIBS_DY                                                     148
C -                                                                       148
      INTEGER LEN_RIBS,LLEN_RIBS,LINK_RIBS                                148
      PARAMETER (LEN_RIBS=69)                                             148
      PARAMETER (LLEN_RIBS=7)                                             148
      INTEGER FLAG_RIBS/0/,MAP_RIBS(2,LLEN_RIBS),IDNUM_RIBS(10)           148
      CHARACTER NAMES_RIBS(LLEN_RIBS)*12,COMMEN_RIBS(LLEN_RIBS)*40        148
      DATA IDNUM_RIBS/10*0/                                               148
      REAL BANK_RIBS(LEN_RIBS)                                            148
      CHARACTER*60 BPATH_RIBS/' '/                                        148
      SAVE FLAG_RIBS,MAP_RIBS,IDNUM_RIBS,NAMES_RIBS,COMMEN_RIBS,          148
     *BPATH_RIBS                                                          148
      DATA LINK_RIBS/0/,BANK_RIBS/LEN_RIBS*0/                             148
C *                                                                       148
C -                                                                       149
      INTEGER HOLZ_INOUT                                                  149
      EQUIVALENCE (BANK_HOLZ(3),HOLZ_INOUT,RE_HOLZ_INOUT),                149
     *(NAMES_HOLZ(1),NHOLZ_INOUT)                                         149
      EQUIVALENCE (COMMEN_HOLZ(1),CHOLZ_INOUT),(MAP_HOLZ(1,1),            149
     *MHOLZ_INOUT)                                                        149
      CHARACTER CHOLZ_INOUT*40/' '/                                       149
      CHARACTER NHOLZ_INOUT*12/'I.INOUT'/                                 149
      INTEGER MHOLZ_INOUT(2)/1,1/                                         149
      REAL RE_HOLZ_INOUT                                                  149
C -                                                                       149
      INTEGER HOLZ_NUMBEROFROWS                                           149
      EQUIVALENCE (BANK_HOLZ(4),HOLZ_NUMBEROFROWS,RE_HOLZ_NUMBEROFROWS),  149
     *(NAMES_HOLZ(2),NHOLZ_NUMBEROFROWS)                                  149
      EQUIVALENCE (COMMEN_HOLZ(2),CHOLZ_NUMBEROFROWS),(MAP_HOLZ(1,2),     149
     *MHOLZ_NUMBEROFROWS)                                                 149
      CHARACTER CHOLZ_NUMBEROFROWS*40/' '/                                149
      CHARACTER NHOLZ_NUMBEROFROWS*12/'I.NUMBEROFROWS'/                   149
      INTEGER MHOLZ_NUMBEROFROWS(2)/1,1/                                  149
      REAL RE_HOLZ_NUMBEROFROWS                                           149
C -                                                                       149
      INTEGER HOLZ_NUMBERPERROW(16)                                       149
      EQUIVALENCE (BANK_HOLZ(5),HOLZ_NUMBERPERROW,RE_HOLZ_NUMBERPERROW),  149
     *(NAMES_HOLZ(3),NHOLZ_NUMBERPERROW)                                  149
      EQUIVALENCE (COMMEN_HOLZ(3),CHOLZ_NUMBERPERROW),(MAP_HOLZ(1,3),     149
     *MHOLZ_NUMBERPERROW)                                                 149
      CHARACTER CHOLZ_NUMBERPERROW*40/' '/                                149
      CHARACTER NHOLZ_NUMBERPERROW*12/'I.NUMBERPERROW'/                   149
      INTEGER MHOLZ_NUMBERPERROW(2)/16,1/                                 149
      REAL RE_HOLZ_NUMBERPERROW                                           149
C -                                                                       149
      REAL HOLZ_YHOLES(160)                                               149
      EQUIVALENCE (BANK_HOLZ(21),HOLZ_YHOLES,RE_HOLZ_YHOLES),             149
     *(NAMES_HOLZ(4),NHOLZ_YHOLES)                                        149
      EQUIVALENCE (COMMEN_HOLZ(4),CHOLZ_YHOLES),(MAP_HOLZ(1,4),           149
     *MHOLZ_YHOLES)                                                       149
      CHARACTER CHOLZ_YHOLES*40/' '/                                      149
      CHARACTER NHOLZ_YHOLES*12/'R.YHOLES'/                               149
      INTEGER MHOLZ_YHOLES(2)/160,1/                                      149
      REAL RE_HOLZ_YHOLES                                                 149
C -                                                                       149
      INTEGER LEN_HOLZ,LLEN_HOLZ,LINK_HOLZ                                149
      PARAMETER (LEN_HOLZ=180)                                            149
      PARAMETER (LLEN_HOLZ=4)                                             149
      INTEGER FLAG_HOLZ/0/,MAP_HOLZ(2,LLEN_HOLZ),IDNUM_HOLZ(10)           149
      CHARACTER NAMES_HOLZ(LLEN_HOLZ)*12,COMMEN_HOLZ(LLEN_HOLZ)*40        149
      DATA IDNUM_HOLZ/10*0/                                               149
      REAL BANK_HOLZ(LEN_HOLZ)                                            149
      CHARACTER*60 BPATH_HOLZ/' '/                                        149
      SAVE FLAG_HOLZ,MAP_HOLZ,IDNUM_HOLZ,NAMES_HOLZ,COMMEN_HOLZ,          149
     *BPATH_HOLZ                                                          149
      DATA LINK_HOLZ/0/,BANK_HOLZ/LEN_HOLZ*0/                             149
C *                                                                       149
C -                                                                       150
      INTEGER COOL_INOUT                                                  150
      EQUIVALENCE (BANK_COOL(3),COOL_INOUT,RE_COOL_INOUT),                150
     *(NAMES_COOL(1),NCOOL_INOUT)                                         150
      EQUIVALENCE (COMMEN_COOL(1),CCOOL_INOUT),(MAP_COOL(1,1),            150
     *MCOOL_INOUT)                                                        150
      CHARACTER CCOOL_INOUT*40/' '/                                       150
      CHARACTER NCOOL_INOUT*12/'I.INOUT'/                                 150
      INTEGER MCOOL_INOUT(2)/1,1/                                         150
      REAL RE_COOL_INOUT                                                  150
C -                                                                       150
      REAL COOL_TUBELENGTH(16)                                            150
      EQUIVALENCE (BANK_COOL(4),COOL_TUBELENGTH,RE_COOL_TUBELENGTH),      150
     *(NAMES_COOL(2),NCOOL_TUBELENGTH)                                    150
      EQUIVALENCE (COMMEN_COOL(2),CCOOL_TUBELENGTH),(MAP_COOL(1,2),       150
     *MCOOL_TUBELENGTH)                                                   150
      CHARACTER CCOOL_TUBELENGTH*40/' '/                                  150
      CHARACTER NCOOL_TUBELENGTH*12/'R.TUBELENGTH'/                       150
      INTEGER MCOOL_TUBELENGTH(2)/16,1/                                   150
      REAL RE_COOL_TUBELENGTH                                             150
C -                                                                       150
      INTEGER LEN_COOL,LLEN_COOL,LINK_COOL                                150
      PARAMETER (LEN_COOL=19)                                             150
      PARAMETER (LLEN_COOL=2)                                             150
      INTEGER FLAG_COOL/0/,MAP_COOL(2,LLEN_COOL),IDNUM_COOL(10)           150
      CHARACTER NAMES_COOL(LLEN_COOL)*12,COMMEN_COOL(LLEN_COOL)*40        150
      DATA IDNUM_COOL/10*0/                                               150
      REAL BANK_COOL(LEN_COOL)                                            150
      CHARACTER*60 BPATH_COOL/' '/                                        150
      SAVE FLAG_COOL,MAP_COOL,IDNUM_COOL,NAMES_COOL,COMMEN_COOL,          150
     *BPATH_COOL                                                          150
      DATA LINK_COOL/0/,BANK_COOL/LEN_COOL*0/                             150
C *                                                                       150
C *                                                                       151
      CALL AGSBEGM(MODULE,IPRIN)                                          151
      IF (IPRIN.LT.0) RETURN                                              151
C Filling bank TPCC - SELECTED TPCG VERSION SUBVERSION                    151
      BANK_TITLE='SELECTED TPCG VERSION SUBVERSION'                       151
      BPATH_TPCC='TPCC*'                                                  151
      IDNUM_TPCC(1)=0                                                     152
C DEFAULT IS 3.0                                                          152
      TPCC_VERSION=3.0                                                    152
      CTPCC_VERSION='DEFAULT IS 3.0'                                      152
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCC',    153
     *BPATH_TPCC,IDNUM_TPCC,1, MAP_TPCC,NAMES_TPCC,COMMEN_TPCC,           153
     *BANK_TPCC,LEN_TPCC,LLEN_TPCC,LINK_TPCC,FLAG_TPCC)                   153
C *                                                                       153
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCC',    153
     *BPATH_TPCC,IDNUM_TPCC,1, MAP_TPCC,NAMES_TPCC,COMMEN_TPCC,           153
     *BANK_TPCC,LEN_TPCC,LLEN_TPCC,LINK_TPCC,FLAG_TPCC)                   153
C *                                                                       154
      CALL AGSBEGM(MODULE,IPRIN)                                          154
      IF (IPRIN.LT.0) RETURN                                              154
C Filling bank TPCG - // TPC BASIC DIMENSIONS                             154
      BANK_TITLE='// TPC BASIC DIMENSIONS'                                154
      BPATH_TPCG='TPCG*'                                                  154
      IDNUM_TPCG(1)=0                                                     155
C // VERSION => CURRENT VERSION                                           155
      TPCG_VERSION=3.0                                                    155
      CTPCG_VERSION='// VERSION => CURRENT VERSION'                       155
C // RMIN => TPC ENVELOPE INNER RADIUS                                    156
      TPCG_RMIN=46.107                                                    156
      CTPCG_RMIN='// RMIN => TPC ENVELOPE INNER RADIUS'                   156
C // RMAX => TPC ENVELOPE OUTER RADIUS                                    157
      TPCG_RMAX=206.75/COS15                                              157
      CTPCG_RMAX='// RMAX => TPC ENVELOPE OUTER RADIUS'                   157
C // RMINIFC => INNER RADIUS TPC IFC : A.LEBEDEV MEASUREMENT 10/16/08     158
      TPCG_RMINIFC=46.6                                                   158
      CTPCG_RMINIFC='// RMINIFC => INNER RADIUS TPC IFC : A.LEBEDEV MEAS  158
     *UREMENT 10/16/08'                                                   158
C // LENGTHT => TPC FULL LENGTH UP TO FRONT OF EEMC                       159
      TPCG_LENGTHT=2*271.0                                                159
      CTPCG_LENGTHT='// LENGTHT => TPC FULL LENGTH UP TO FRONT OF EEMC'   159
C // LENGTH => TPC FULL LENGTH INCLUDING RDOS                             160
      TPCG_LENGTH=2*259.685                                               160
      CTPCG_LENGTH='// LENGTH => TPC FULL LENGTH INCLUDING RDOS'          160
C // LENGTHW => TPC LENGTH INCLUDING WHEEL                                161
      TPCG_LENGTHW=2*229.71                                               161
      CTPCG_LENGTHW='// LENGTHW => TPC LENGTH INCLUDING WHEEL'            161
C // LENGTHV => TPC GAS VOLUME LENGTH                                     162
      TPCG_LENGTHV=2*210.00                                               162
      CTPCG_LENGTHV='// LENGTHV => TPC GAS VOLUME LENGTH'                 162
C // TPCE ENVELOP DZ                                                      163
      TPCG_DZENVELOP=268.                                                 163
      CTPCG_DZENVELOP='// TPCE ENVELOP DZ'                                163
C // 49.60 WHEELIR => SUPPORT WHEEL INNER RADIUS                          164
      TPCG_WHEELIR=38.620*INCH/2                                          164
      CTPCG_WHEELIR='// 49.60 WHEELIR => SUPPORT WHEEL INNER RADIUS'      164
C // WHEELR0 => DISTANCE FROM IP OF END OF INNER CYLINDRICAL PART         165
      TPCG_WHEELR0=21.500*INCH                                            165
      CTPCG_WHEELR0='// WHEELR0 => DISTANCE FROM IP OF END OF INNER CYLI  165
     *NDRICAL PART'                                                       165
C // WHEELR1 => DISTANCE FROM IP OF MIDDLE RIB                            166
      TPCG_WHEELR1=47.867*INCH                                            166
      CTPCG_WHEELR1='// WHEELR1 => DISTANCE FROM IP OF MIDDLE RIB'        166
C // WHEELR2 => DISTANCE FROM IP OF UPPER RIB                             167
      TPCG_WHEELR2=76.093*INCH                                            167
      CTPCG_WHEELR2='// WHEELR2 => DISTANCE FROM IP OF UPPER RIB'         167
C // WHEELTOTALRIBWIDTH => TOTAL RIB WIDTH (A.LEBEDEV MEASUREMENT)        168
      TPCG_WHEELTOTALRIBWIDTH=7.6                                         168
      CTPCG_WHEELTOTALRIBWIDTH='// WHEELTOTALRIBWIDTH => TOTAL RIB WIDTH  168
     * (A.LEBEDEV MEASUREMENT)'                                           168
C // WHEELRIBWIDTH => RIB PLATE WIDTH (A.LEBEDEV MEASUREMENT)             169
      TPCG_WHEELRIBWIDTH=1.9                                              169
      CTPCG_WHEELRIBWIDTH='// WHEELRIBWIDTH => RIB PLATE WIDTH (A.LEBEDE  169
     *V MEASUREMENT)'                                                     169
C // WHEELRIBHEIGHT => RIB PLATE HEIGHT (A.LEBEDEV MEASUREMENT)           170
      TPCG_WHEELRIBHEIGHT=1.3                                             170
      CTPCG_WHEELRIBHEIGHT='// WHEELRIBHEIGHT => RIB PLATE HEIGHT (A.LEB  170
     *EDEV MEASUREMENT)'                                                  170
C // WHEELBOXDX (A.LEBEDEV MEASUREMENT)                                   171
      TPCG_WHEELBOXDX=4.7                                                 171
      CTPCG_WHEELBOXDX='// WHEELBOXDX (A.LEBEDEV MEASUREMENT)'            171
C // WHEELBOXDY (A.LEBEDEV MEASUREMENT)                                   172
      TPCG_WHEELBOXDY=7.6                                                 172
      CTPCG_WHEELBOXDY='// WHEELBOXDY (A.LEBEDEV MEASUREMENT)'            172
C // WHEELOR1 => @ TOFC                                                   173
      TPCG_WHEELOR1=201.0                                                 173
      CTPCG_WHEELOR1='// WHEELOR1 => @ TOFC'                              173
C // WHEELOR => SUPPORT WHEEL OUTER RADIUS                                174
      TPCG_WHEELOR=206.75                                                 174
      CTPCG_WHEELOR='// WHEELOR => SUPPORT WHEEL OUTER RADIUS'            174
C // WHEELTHK1                                                            175
      TPCG_WHEELTHK1=5.72                                                 175
      CTPCG_WHEELTHK1='// WHEELTHK1'                                      175
C // WHEELTHK => SUPPORT WHEEL LENGTH                                     176
      TPCG_WHEELTHK=11.43                                                 176
      CTPCG_WHEELTHK='// WHEELTHK => SUPPORT WHEEL LENGTH'                176
C // SENGASOR => TPC SENSITIVE GAS OUTER RADIUS                           177
      TPCG_SENGASOR=200                                                   177
      CTPCG_SENGASOR='// SENGASOR => TPC SENSITIVE GAS OUTER RADIUS'      177
C // MEMBTHK => CENTRAL MEMBRANE THICKNESS                                178
      TPCG_MEMBTHK=0.00762                                                178
      CTPCG_MEMBTHK='// MEMBTHK => CENTRAL MEMBRANE THICKNESS'            178
C // TOCSDR => OUTER COPPER THICKNESS: NIM A499 (2003) 659-678 TABLE 2    179
      TPCG_TOCSDR=0.013                                                   179
      CTPCG_TOCSDR='// TOCSDR => OUTER COPPER THICKNESS: NIM A499 (2003)  179
     * 659-678 TABLE 2'                                                   179
C // TOKADR => OUTER KAPTON THICKNESS                                     180
      TPCG_TOKADR=0.015                                                   180
      CTPCG_TOKADR='// TOKADR => OUTER KAPTON THICKNESS'                  180
C // TONXDR => OUTER NOMEX THICKNESS                                      181
      TPCG_TONXDR=0.953                                                   181
      CTPCG_TONXDR='// TONXDR => OUTER NOMEX THICKNESS'                   181
C // TOADDR => OUTER ADHESIVE THICKNESS                                   182
      TPCG_TOADDR=0.05                                                    182
      CTPCG_TOADDR='// TOADDR => OUTER ADHESIVE THICKNESS'                182
C // TOIGDR => OUTER ISOLATING GAS THICKNESS                              183
      TPCG_TOIGDR=5.70                                                    183
      CTPCG_TOIGDR='// TOIGDR => OUTER ISOLATING GAS THICKNESS'           183
C // TOALDR => OUTER ALUMINUM THICKNESS (FOR BOTH LAYERS)                 184
      TPCG_TOALDR=0.40                                                    184
      CTPCG_TOALDR='// TOALDR => OUTER ALUMINUM THICKNESS (FOR BOTH LAYE  184
     *RS)'                                                                184
C // TOHADR => OUTER HONEYCOMB AL THICKNESS                               185
      TPCG_TOHADR=0.60                                                    185
      CTPCG_TOHADR='// TOHADR => OUTER HONEYCOMB AL THICKNESS'            185
C // TIADDR => INNER ADHESIVE LAYER THICKNESS: NIM A499 (2003) 659-678 T  186
C ABLE 2                                                                  186
      TPCG_TIADDR=0.080                                                   186
      CTPCG_TIADDR='// TIADDR => INNER ADHESIVE LAYER THICKNESS: NIM A49  186
     *9 (2003) 659-678 TABLE 2'                                           186
C // TINXDR => INNER NOMEX STRUCTUREURE THICKNESS                         187
      TPCG_TINXDR=1.270                                                   187
      CTPCG_TINXDR='// TINXDR => INNER NOMEX STRUCTUREURE THICKNESS'      187
C // TIKADR => INNER KAPTON LAYER THICKNESS                               188
      TPCG_TIKADR=0.015                                                   188
      CTPCG_TIKADR='// TIKADR => INNER KAPTON LAYER THICKNESS'            188
C // TIALDR => INNER ALUMINUM LAYER THICKNESS                             189
      TPCG_TIALDR=0.004                                                   189
      CTPCG_TIALDR='// TIALDR => INNER ALUMINUM LAYER THICKNESS'          189
C // TIFCRF => OUTER RADIUS OF IFC FLANGE                                 190
      TPCG_TIFCRF=51.7                                                    190
      CTPCG_TIFCRF='// TIFCRF => OUTER RADIUS OF IFC FLANGE'              190
C // TIFCDRT => TOLERANCE BETWEEN FLANGE AND WHEEL                        191
      TPCG_TIFCDRT=0.1                                                    191
      CTPCG_TIFCDRT='// TIFCDRT => TOLERANCE BETWEEN FLANGE AND WHEEL'    191
C // DZ OF YF1                                                            192
      TPCG_DZYF1=221.162                                                  192
      CTPCG_DZYF1='// DZ OF YF1'                                          192
C // DZ OF YF2                                                            193
      TPCG_DZYF2=211.9                                                    193
      CTPCG_DZYF2='// DZ OF YF2'                                          193
C // DZ OF YF3                                                            194
      TPCG_DZYF3=208.02                                                   194
      CTPCG_DZYF3='// DZ OF YF3'                                          194
C // 1.82 MM DRAWING 24A0314 & 24A0304                                    195
      TPCG_PADPLANETHICKNESS=0.182                                        195
      CTPCG_PADPLANETHICKNESS='// 1.82 MM DRAWING 24A0314 & 24A0304'      195
C //                                                                      196
      TPCG_DGATEGROUND=0.6                                                196
      CTPCG_DGATEGROUND='//'                                              196
C //                                                                      197
      TPCG_WIREMOUNTWIDTH=5*((0.130+0.1376)/2)*INCH                       197
      CTPCG_WIREMOUNTWIDTH='//'                                           197
C // DRAWING 24A0434                                                      198
      TPCG_WIREMOUNTHEIGHT=1.340*INCH                                     198
      CTPCG_WIREMOUNTHEIGHT='// DRAWING 24A0434'                          198
C // A.LEBEDEV 1 RDO = 5 LB                                               199
      TPCG_DXRDO=1.75/2                                                   199
      CTPCG_DXRDO='// A.LEBEDEV 1 RDO = 5 LB'                             199
C //                                                                      200
      TPCG_DYRDO=45./2                                                    200
      CTPCG_DYRDO='//'                                                    200
C //                                                                      201
      TPCG_DZRDO=17.0/2                                                   201
      CTPCG_DZRDO='//'                                                    201
C //                                                                      202
      TPCG_ZRDO=TPCG_LENGTHW/2 + 20.0                                     202
      CTPCG_ZRDO='//'                                                     202
C // X COOLING TUBE DRAWING 24A0801B                                      203
      TPCG_HEIGTUBE=0.703*INCH                                            203
      CTPCG_HEIGTUBE='// X COOLING TUBE DRAWING 24A0801B'                 203
C // Z MIXTURE TPCE_WATER_PIPE => RHO = 2.32155 G/CM**3                   204
      TPCG_WIDTUBE=0.500*INCH                                             204
      CTPCG_WIDTUBE='// Z MIXTURE TPCE_WATER_PIPE => RHO = 2.32155 G/CM*  204
     **3'                                                                 204
C //                                                                      205
      TPCG_RDOCOOLINGDX=(38.0 + 9.0 + 58.0)/2                             205
      CTPCG_RDOCOOLINGDX='//'                                             205
C //                                                                      206
      TPCG_RDOCOOLINGDY=1.25                                              206
      CTPCG_RDOCOOLINGDY='//'                                             206
C //                                                                      207
      TPCG_RDOCOOLINGDZ=2.50                                              207
      CTPCG_RDOCOOLINGDZ='//'                                             207
C // INNER/OUTER ANODE WIDTH                                              208
      TPCG_DANODE(1)=0.2                                                  208
      TPCG_DANODE(2)=0.4                                                  208
      CTPCG_DANODE='// INNER/OUTER ANODE WIDTH'                           208
C //                                                                      209
      TPCG_ZGATINGGRID=TPCG_LENGTHV/2                                     209
     *-(TPCG_DGATEGROUND+2*TPCG_DANODE(2))                                209
      CTPCG_ZGATINGGRID='//'                                              209
C //                                                                      210
      TPCG_ZGROUNDGRID=TPCG_ZGATINGGRID+TPCG_DGATEGROUND                  210
      CTPCG_ZGROUNDGRID='//'                                              210
C // DEAD ZONE BEFORE GATINGGRID. NO HITS THERE                           211
      TPCG_DEADZONE=12.                                                   211
      CTPCG_DEADZONE='// DEAD ZONE BEFORE GATINGGRID. NO HITS THERE'      211
C // MAX TIME OF FLIGHT                                                   212
      TPCG_TOFMAX=1.0E-6                                                  212
      CTPCG_TOFMAX='// MAX TIME OF FLIGHT'                                212
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCG',    213
     *BPATH_TPCG,IDNUM_TPCG,1, MAP_TPCG,NAMES_TPCG,COMMEN_TPCG,           213
     *BANK_TPCG,LEN_TPCG,LLEN_TPCG,LINK_TPCG,FLAG_TPCG)                   213
C *                                                                       213
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCG',    213
     *BPATH_TPCG,IDNUM_TPCG,1, MAP_TPCG,NAMES_TPCG,COMMEN_TPCG,           213
     *BANK_TPCG,LEN_TPCG,LLEN_TPCG,LINK_TPCG,FLAG_TPCG)                   213
C *                                                                       214
      CALL AGSBEGM(MODULE,IPRIN)                                          214
      IF (IPRIN.LT.0) RETURN                                              214
C Filling bank TPCG - SUBVERSION 1                                        214
      BANK_TITLE='SUBVERSION 1'                                           214
      BPATH_TPCG='TPCG*'                                                  214
      IDNUM_TPCG(1)=0                                                     215
C SELECTOR                                                                215
      TPCG_VERSION=3.1                                                    215
      CTPCG_VERSION='SELECTOR'                                            215
C MAX ENVELOPE RADIUS                                                     216
      TPCG_RMAX=207.77                                                    216
      CTPCG_RMAX='MAX ENVELOPE RADIUS'                                    216
C EXTEND DEADZONE BEFORE GATING GRID                                      217
      TPCG_DEADZONE=14                                                    217
      CTPCG_DEADZONE='EXTEND DEADZONE BEFORE GATING GRID'                 217
C INCREASE MAXIMUM TOF FOR PARTICLES                                      218
      TPCG_TOFMAX=32.0E-6                                                 218
      CTPCG_TOFMAX='INCREASE MAXIMUM TOF FOR PARTICLES'                   218
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCG',    219
     *BPATH_TPCG,IDNUM_TPCG,1, MAP_TPCG,NAMES_TPCG,COMMEN_TPCG,           219
     *BANK_TPCG,LEN_TPCG,LLEN_TPCG,LINK_TPCG,FLAG_TPCG)                   219
C *                                                                       219
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCG',    219
     *BPATH_TPCG,IDNUM_TPCG,1, MAP_TPCG,NAMES_TPCG,COMMEN_TPCG,           219
     *BANK_TPCG,LEN_TPCG,LLEN_TPCG,LINK_TPCG,FLAG_TPCG)                   219
C use default TPCC bank                                                   220
      BPATH_TPCC='TPCC*'                                                  220
      IDNUM_TPCC(1)=0                                                     220
      CALL AGDGETP('TPCEGEO4','*',-999,'TPCC',' ',BANK_TPCC, BPATH_TPCC,  220
     *IDNUM_TPCC,1,MAP_TPCC,NAMES_TPCC,BANK_TPCC,LEN_TPCC,LLEN_TPCC,      220
     *LINK_TPCC,FLAG_TPCC)                                                220
C use TPCG bank                                                           221
      BPATH_TPCG='TPCG*'                                                  221
      IDNUM_TPCG(1)=0                                                     221
      TPCG_VERSION=TPCC_VERSION                                           221
      CALL AGDGETP('TPCEGEO4','*',-999,'TPCG',NTPCG_VERSION,              221
     *RE_TPCG_VERSION,BPATH_TPCG,IDNUM_TPCG,1,MAP_TPCG,NAMES_TPCG,        221
     *BANK_TPCG,LEN_TPCG,LLEN_TPCG,LINK_TPCG,FLAG_TPCG)                   221
C *                                                                       222
      CALL AGSBEGM(MODULE,IPRIN)                                          222
      IF (IPRIN.LT.0) RETURN                                              222
C Filling bank TPRS - // SECTOR OF PADROWS                                222
      BANK_TITLE='// SECTOR OF PADROWS'                                   222
      BPATH_TPRS='TPRS*'                                                  222
      IDNUM_TPRS(1)=0                                                     223
C // SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                              223
      TPRS_SEC=1                                                          223
      CTPRS_SEC='// SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'              223
C // NUMBER OF PADROWS IN THE SECTOR                                      224
      TPRS_NROW=13                                                        224
      CTPRS_NROW='// NUMBER OF PADROWS IN THE SECTOR'                     224
C // TPC PADROW PITCH WIDTH                                               225
      TPRS_PITCH=0.335                                                    225
      CTPRS_PITCH='// TPC PADROW PITCH WIDTH'                             225
C // TPC PADROW THICKNESS                                                 226
      TPRS_WIDTH=1.15                                                     226
      CTPRS_WIDTH='// TPC PADROW THICKNESS'                               226
C // NUMBER OF PADRAWS IN A SUPERPADROW                                   227
      TPRS_SUPER=3                                                        227
      CTPRS_SUPER='// NUMBER OF PADRAWS IN A SUPERPADROW'                 227
C // DISTANCE TO ANODE WIRE FROM PAD PLANE                                228
      TPRS_DANODE=0.2                                                     228
      CTPRS_DANODE='// DISTANCE TO ANODE WIRE FROM PAD PLANE'             228
C // NUMBER OF PADS IN ROW                                                230
      TPRS_NPADS(1)=88                                                    230
      TPRS_NPADS(2)=96                                                    230
      TPRS_NPADS(3)=104                                                   230
      TPRS_NPADS(4)=112                                                   230
      TPRS_NPADS(5)=118                                                   230
      TPRS_NPADS(6)=126                                                   230
      TPRS_NPADS(7)=134                                                   230
      TPRS_NPADS(8)=142                                                   230
      TPRS_NPADS(9)=150                                                   230
      TPRS_NPADS(10)=158                                                  230
      TPRS_NPADS(11)=166                                                  230
      TPRS_NPADS(12)=174                                                  230
      TPRS_NPADS(13)=182                                                  230
      CTPRS_NPADS='// NUMBER OF PADS IN ROW'                              230
C // TPC PADROW RADII                                                     233
      TPRS_RPADS(1)=60.0                                                  233
      TPRS_RPADS(2)=64.8                                                  233
      TPRS_RPADS(3)=69.6                                                  233
      TPRS_RPADS(4)=74.4                                                  233
      TPRS_RPADS(5)=79.2                                                  233
      TPRS_RPADS(6)=84.0                                                  233
      TPRS_RPADS(7)=88.8                                                  233
      TPRS_RPADS(8)=93.6                                                  233
      TPRS_RPADS(9)=98.8                                                  233
      TPRS_RPADS(10)=104.0                                                233
      TPRS_RPADS(11)=109.2                                                233
      TPRS_RPADS(12)=114.4                                                233
      TPRS_RPADS(13)=119.6                                                233
      CTPRS_RPADS='// TPC PADROW RADII'                                   233
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    234
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           234
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   234
C *                                                                       234
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    234
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           234
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   234
C *                                                                       235
      CALL AGSBEGM(MODULE,IPRIN)                                          235
      IF (IPRIN.LT.0) RETURN                                              235
C Filling bank TPRS - // SECTOR OF PADROWS                                235
      BANK_TITLE='// SECTOR OF PADROWS'                                   235
      BPATH_TPRS='TPRS*'                                                  235
      IDNUM_TPRS(1)=0                                                     236
C // SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                              236
      TPRS_SEC=2                                                          236
      CTPRS_SEC='// SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'              236
C // NUMBER OF PADROWS IN OUTER SECTOR                                    237
      TPRS_NROW=32                                                        237
      CTPRS_NROW='// NUMBER OF PADROWS IN OUTER SECTOR'                   237
C // OUTER TPC PADROW PITCH WIDTH                                         238
      TPRS_PITCH=0.67                                                     238
      CTPRS_PITCH='// OUTER TPC PADROW PITCH WIDTH'                       238
C // OUTER TPC PADROW THICKNESS                                           239
      TPRS_WIDTH=1.95                                                     239
      CTPRS_WIDTH='// OUTER TPC PADROW THICKNESS'                         239
C // NUMBER OF PADROWS IN A SUPERPADROW                                   240
      TPRS_SUPER=1                                                        240
      CTPRS_SUPER='// NUMBER OF PADROWS IN A SUPERPADROW'                 240
C // DISTANCE TO ANODE WIRE FROM PAD PLANE                                241
      TPRS_DANODE=0.4                                                     241
      CTPRS_DANODE='// DISTANCE TO ANODE WIRE FROM PAD PLANE'             241
C // NUMBER OF PADS IN ROW                                                245
      TPRS_NPADS(1)=98                                                    245
      TPRS_NPADS(2)=100                                                   245
      TPRS_NPADS(3)=102                                                   245
      TPRS_NPADS(4)=104                                                   245
      TPRS_NPADS(5)=106                                                   245
      TPRS_NPADS(6)=106                                                   245
      TPRS_NPADS(7)=108                                                   245
      TPRS_NPADS(8)=110                                                   245
      TPRS_NPADS(9)=112                                                   245
      TPRS_NPADS(10)=112                                                  245
      TPRS_NPADS(11)=114                                                  245
      TPRS_NPADS(12)=116                                                  245
      TPRS_NPADS(13)=118                                                  245
      TPRS_NPADS(14)=120                                                  245
      TPRS_NPADS(15)=122                                                  245
      TPRS_NPADS(16)=122                                                  245
      TPRS_NPADS(17)=124                                                  245
      TPRS_NPADS(18)=126                                                  245
      TPRS_NPADS(19)=128                                                  245
      TPRS_NPADS(20)=128                                                  245
      TPRS_NPADS(21)=130                                                  245
      TPRS_NPADS(22)=132                                                  245
      TPRS_NPADS(23)=134                                                  245
      TPRS_NPADS(24)=136                                                  245
      TPRS_NPADS(25)=138                                                  245
      TPRS_NPADS(26)=138                                                  245
      TPRS_NPADS(27)=140                                                  245
      TPRS_NPADS(28)=142                                                  245
      TPRS_NPADS(29)=144                                                  245
      TPRS_NPADS(30)=144                                                  245
      TPRS_NPADS(31)=144                                                  245
      TPRS_NPADS(32)=144                                                  245
      CTPRS_NPADS='// NUMBER OF PADS IN ROW'                              245
C // TPC PADROW RADII                                                     253
      TPRS_RPADS(1)=127.195                                               253
      TPRS_RPADS(2)=129.195                                               253
      TPRS_RPADS(3)=131.195                                               253
      TPRS_RPADS(4)=133.195                                               253
      TPRS_RPADS(5)=135.195                                               253
      TPRS_RPADS(6)=137.195                                               253
      TPRS_RPADS(7)=139.195                                               253
      TPRS_RPADS(8)=141.195                                               253
      TPRS_RPADS(9)=143.195                                               253
      TPRS_RPADS(10)=145.195                                              253
      TPRS_RPADS(11)=147.195                                              253
      TPRS_RPADS(12)=149.195                                              253
      TPRS_RPADS(13)=151.195                                              253
      TPRS_RPADS(14)=153.195                                              253
      TPRS_RPADS(15)=155.195                                              253
      TPRS_RPADS(16)=157.195                                              253
      TPRS_RPADS(17)=159.195                                              253
      TPRS_RPADS(18)=161.195                                              253
      TPRS_RPADS(19)=163.195                                              253
      TPRS_RPADS(20)=165.195                                              253
      TPRS_RPADS(21)=167.195                                              253
      TPRS_RPADS(22)=169.195                                              253
      TPRS_RPADS(23)=171.195                                              253
      TPRS_RPADS(24)=173.195                                              253
      TPRS_RPADS(25)=175.195                                              253
      TPRS_RPADS(26)=177.195                                              253
      TPRS_RPADS(27)=179.195                                              253
      TPRS_RPADS(28)=181.195                                              253
      TPRS_RPADS(29)=183.195                                              253
      TPRS_RPADS(30)=185.195                                              253
      TPRS_RPADS(31)=187.195                                              253
      TPRS_RPADS(32)=189.195                                              253
      CTPRS_RPADS='// TPC PADROW RADII'                                   253
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    254
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           254
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   254
C *                                                                       254
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    254
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           254
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   254
C use default TPRS bank                                                   255
      BPATH_TPRS='TPRS*'                                                  255
      IDNUM_TPRS(1)=0                                                     255
      CALL AGDGETP('TPCEGEO4','*',-999,'TPRS',' ',BANK_TPRS, BPATH_TPRS,  255
     *IDNUM_TPRS,1,MAP_TPRS,NAMES_TPRS,BANK_TPRS,LEN_TPRS,LLEN_TPRS,      255
     *LINK_TPRS,FLAG_TPRS)                                                255
C *                                                                       256
      CALL AGSBEGM(MODULE,IPRIN)                                          256
      IF (IPRIN.LT.0) RETURN                                              256
C Filling bank TECW - // EC TRAPEZOID AND SUPPORT WHEEL                   256
      BANK_TITLE='// EC TRAPEZOID AND SUPPORT WHEEL'                      256
      BPATH_TECW='TECW*'                                                  256
      IDNUM_TECW(1)=0                                                     257
C // SEC => SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                       257
      TECW_SEC=1                                                          257
      CTECW_SEC='// SEC => SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'       257
C // GAPWIDI => AIR IN SUPPORT WHEEL - INNER WIDTH                        258
      TECW_GAPWIDI=2.* 10.91                                              258
      CTECW_GAPWIDI='// GAPWIDI => AIR IN SUPPORT WHEEL - INNER WIDTH'    258
C // GAPWIDO => AIR IN SUPPORT WHEEL - OUTER WIDTH                        259
      TECW_GAPWIDO=2.* 27.56                                              259
      CTECW_GAPWIDO='// GAPWIDO => AIR IN SUPPORT WHEEL - OUTER WIDTH'    259
C // GAPHEIT => AIR IN SUPPORT WHEEL - HEIGHT (DR)                        260
      TECW_GAPHEIT=62.15                                                  260
      CTECW_GAPHEIT='// GAPHEIT => AIR IN SUPPORT WHEEL - HEIGHT (DR)'    260
C // GAPRAD => AIR IN SUPPORT WHEEL - CENTER RADIUS                       261
      TECW_GAPRAD=87.0                                                    261
      CTECW_GAPRAD='// GAPRAD => AIR IN SUPPORT WHEEL - CENTER RADIUS'    261
C //2.* 13.75, !// INWIDTH => SECTOR WIDTH AT INNER RADIUS                262
      TECW_INWIDTH=10.829*INCH                                            262
      CTECW_INWIDTH='//2.* 13.75, !// INWIDTH => SECTOR WIDTH AT INNER R  262
     *ADIUS'                                                              262
C //2.* 32.38, !// OUWIDTH => SECTOR WIDTH AT OUTER RADIUS                263
      TECW_OUWIDTH=25.498*INCH                                            263
      CTECW_OUWIDTH='//2.* 32.38, !// OUWIDTH => SECTOR WIDTH AT OUTER R  263
     *ADIUS'                                                              263
C // NORIBS                                                               264
      TECW_NORIBS=11                                                      264
      CTECW_NORIBS='// NORIBS'                                            264
C // ZSTEPRIB                                                             265
      TECW_ZSTEPRIB=3.150*INCH                                            265
      CTECW_ZSTEPRIB='// ZSTEPRIB'                                        265
C // WIDTHRIB => SIDE RIB WIDTH                                           266
      TECW_WIDTHRIB=1.47*COS(15./180*ACOS(-1.))*INCH                      266
      CTECW_WIDTHRIB='// WIDTHRIB => SIDE RIB WIDTH'                      266
C // DRAWING 24A3685B 27.77*INCH FROM DRAWING 24A0325, !// => 69.527 =>   267
C (WIRE PLANE) 69.                                                        267
      TECW_HEIGHT=27.373*INCH                                             267
      CTECW_HEIGHT='// DRAWING 24A3685B 27.77*INCH FROM DRAWING 24A0325,  267
     * !// => 69.527 => (WIRE PLANE) 69.'                                 267
C // 24A3685B 3.219*INCH !// THICK => SECTOR THICKNESS                    268
      TECW_THICK=3.300*INCH                                               268
      CTECW_THICK='// 24A3685B 3.219*INCH !// THICK => SECTOR             268
     *THICKNESS'                                                          268
C //                                                                      269
      TECW_THICKAL=0.375*INCH                                             269
      CTECW_THICKAL='//'                                                  269
C // RMIN => MINIMUM DISTANCE TO BEAM LINE (WIRE PLANE)                   270
      TECW_RMIN=51.905                                                    270
      CTECW_RMIN='// RMIN => MINIMUM DISTANCE TO BEAM LINE (WIRE          270
     *PLANE)'                                                             270
C // RCENTER => SECTOR CENTER RADIUS (SET BY PRECISION HOLES)             271
      TECW_RCENTER=86.669                                                 271
      CTECW_RCENTER='// RCENTER => SECTOR CENTER RADIUS (SET BY PRECISIO  271
     *N HOLES)'                                                           271
C // = 0.9525 CM                                                          272
      TECW_HOLEDX=0.750/2*INCH                                            272
      CTECW_HOLEDX='// = 0.9525 CM'                                       272
C // = 3.33375 CM                                                         273
      TECW_HOLEDY=2.625/2*INCH                                            273
      CTECW_HOLEDY='// = 3.33375 CM'                                      273
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TECW',    274
     *BPATH_TECW,IDNUM_TECW,1, MAP_TECW,NAMES_TECW,COMMEN_TECW,           274
     *BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)                   274
C *                                                                       274
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TECW',    274
     *BPATH_TECW,IDNUM_TECW,1, MAP_TECW,NAMES_TECW,COMMEN_TECW,           274
     *BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)                   274
C *                                                                       275
      CALL AGSBEGM(MODULE,IPRIN)                                          275
      IF (IPRIN.LT.0) RETURN                                              275
C Filling bank TECW - // EC TRAPEZOID AND SUPPORT WHEEL                   275
      BANK_TITLE='// EC TRAPEZOID AND SUPPORT WHEEL'                      275
      BPATH_TECW='TECW*'                                                  275
      IDNUM_TECW(1)=0                                                     276
C // SEC => SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                       276
      TECW_SEC=2                                                          276
      CTECW_SEC='// SEC => SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'       276
C // GAPWIDI => AIR IN SUPPORT WHEEL - INNER WIDTH                        277
      TECW_GAPWIDI=2.* 28.92                                              277
      CTECW_GAPWIDI='// GAPWIDI => AIR IN SUPPORT WHEEL - INNER WIDTH'    277
C // GAPWIDO => AIR IN SUPPORT WHEEL - OUTER WIDTH                        278
      TECW_GAPWIDO=2.* 46.74                                              278
      CTECW_GAPWIDO='// GAPWIDO => AIR IN SUPPORT WHEEL - OUTER WIDTH'    278
C // GAPHEIT => AIR IN SUPPORT WHEEL - HEIGHT (DR)                        279
      TECW_GAPHEIT=65.0                                                   279
      CTECW_GAPHEIT='// GAPHEIT => AIR IN SUPPORT WHEEL - HEIGHT (DR)'    279
C // AGAPRAD => IR IN SUPPORT WHEEL - RADIUS                              280
      TECW_GAPRAD=158.0                                                   280
      CTECW_GAPRAD='// AGAPRAD => IR IN SUPPORT WHEEL - RADIUS'           280
C //64.92, !// INWIDTH => SECTOR WIDTH AT INNER RADIUS                    281
      TECW_INWIDTH=25.561*INCH                                            281
      CTECW_INWIDTH='//64.92, !// INWIDTH => SECTOR WIDTH AT INNER RADIU  281
     *S'                                                                  281
C //103.25, !// OUWIDTH => SECTOR WIDTH AT OUTER RADIUS                   282
      TECW_OUWIDTH=40.649*INCH                                            282
      CTECW_OUWIDTH='//103.25, !// OUWIDTH => SECTOR WIDTH AT OUTER RADI  282
     *US'                                                                 282
C // = 1.242060 WIDTHLIP => WIDTH OF LIP                                  283
      TECW_WIDTHLIP=((25.561-24.583)+(40.649-39.671))/4*INCH              283
      CTECW_WIDTHLIP='// = 1.242060 WIDTHLIP => WIDTH OF LIP'             283
C // NORIBS                                                               284
      TECW_NORIBS=14                                                      284
      CTECW_NORIBS='// NORIBS'                                            284
C // ZSTEPRIB                                                             285
      TECW_ZSTEPRIB=3.150*INCH                                            285
      CTECW_ZSTEPRIB='// ZSTEPRIB'                                        285
C // WIDTHRIB = > SIDE RIB WIDTH                                          286
      TECW_WIDTHRIB=1.47*COS(15./180*ACOS(-1.))*INCH                      286
      CTECW_WIDTHRIB='// WIDTHRIB = > SIDE RIB WIDTH'                     286
C // 71.514 (WIRE PLANE) => 71.51, !// HEIGHT => SECTOR RADIAL HEIGHT     287
      TECW_HEIGHT=28.155*INCH                                             287
      CTECW_HEIGHT='// 71.514 (WIRE PLANE) => 71.51, !// HEIGHT => SECTO  287
     *R RADIAL HEIGHT'                                                    287
C // 24A3925G THICK => SECTOR THICKNESS                                   288
      TECW_THICK=3.140*INCH                                               288
      CTECW_THICK='// 24A3925G THICK => SECTOR THICKNESS'                 288
C //                                                                      289
      TECW_THICKAL=0.375*INCH                                             289
      CTECW_THICKAL='//'                                                  289
C // RMIN => MINIMUM DISTANCE TO BEAM LINE (WIRE PLANE)                   290
      TECW_RMIN=121.732                                                   290
      CTECW_RMIN='// RMIN => MINIMUM DISTANCE TO BEAM LINE (WIRE          290
     *PLANE)'                                                             290
C // RCENTER => SECTOR CENTER RADIUS (SET BY PRECISION HOLES)             291
      TECW_RCENTER=157.488                                                291
      CTECW_RCENTER='// RCENTER => SECTOR CENTER RADIUS (SET BY PRECISIO  291
     *N HOLES)'                                                           291
C //                                                                      292
      TECW_HOLEDX=0.750/2*INCH                                            292
      CTECW_HOLEDX='//'                                                   292
C //                                                                      293
      TECW_HOLEDY=2.500/2*INCH                                            293
      CTECW_HOLEDY='//'                                                   293
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TECW',    294
     *BPATH_TECW,IDNUM_TECW,1, MAP_TECW,NAMES_TECW,COMMEN_TECW,           294
     *BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)                   294
C *                                                                       294
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TECW',    294
     *BPATH_TECW,IDNUM_TECW,1, MAP_TECW,NAMES_TECW,COMMEN_TECW,           294
     *BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)                   294
C *                                                                       295
      CALL AGSBEGM(MODULE,IPRIN)                                          295
      IF (IPRIN.LT.0) RETURN                                              295
C Filling bank TPCR - // VOLUME FOR TPC READOUT BOARDS                    295
      BANK_TITLE='// VOLUME FOR TPC READOUT BOARDS'                       295
      BPATH_TPCR='TPCR*'                                                  295
      IDNUM_TPCR(1)=0                                                     296
C // LENGTH OF RDO VOLUME                                                 296
      TPCR_RDOVTHK=30.                                                    296
      CTPCR_RDOVTHK='// LENGTH OF RDO VOLUME'                             296
C // THICKNESS OF RDO CARD                                                297
      TPCR_RDOTHK=.25                                                     297
      CTPCR_RDOTHK='// THICKNESS OF RDO CARD'                             297
C // CARD LENGTH ALONG BEAM DIRECTION                                     298
      TPCR_RDOLEN=27                                                      298
      CTPCR_RDOLEN='// CARD LENGTH ALONG BEAM DIRECTION'                  298
C // NUMBER OF RDO BOARDS                                                 299
      TPCR_NRDOBRD=9                                                      299
      CTPCR_NRDOBRD='// NUMBER OF RDO BOARDS'                             299
C // RADIAL POS OF RDO BOARDS                                             301
      TPCR_RDOHT(1)=60.0                                                  301
      TPCR_RDOHT(2)=74.0                                                  301
      TPCR_RDOHT(3)=84.0                                                  301
      TPCR_RDOHT(4)=101.0                                                 301
      TPCR_RDOHT(5)=106.0                                                 301
      TPCR_RDOHT(6)=126.0                                                 301
      TPCR_RDOHT(7)=146.0                                                 301
      TPCR_RDOHT(8)=166.0                                                 301
      TPCR_RDOHT(9)=186.0                                                 301
      CTPCR_RDOHT='// RADIAL POS OF RDO BOARDS'                           301
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCR',    302
     *BPATH_TPCR,IDNUM_TPCR,1, MAP_TPCR,NAMES_TPCR,COMMEN_TPCR,           302
     *BANK_TPCR,LEN_TPCR,LLEN_TPCR,LINK_TPCR,FLAG_TPCR)                   302
C *                                                                       302
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCR',    302
     *BPATH_TPCR,IDNUM_TPCR,1, MAP_TPCR,NAMES_TPCR,COMMEN_TPCR,           302
     *BANK_TPCR,LEN_TPCR,LLEN_TPCR,LINK_TPCR,FLAG_TPCR)                   302
C *                                                                       303
      CALL AGSBEGM(MODULE,IPRIN)                                          303
      IF (IPRIN.LT.0) RETURN                                              303
C Filling bank TFEE - // FRONTENDELECTRONICS                              303
      BANK_TITLE='// FRONTENDELECTRONICS'                                 303
      BPATH_TFEE='TFEE*'                                                  303
      IDNUM_TFEE(1)=0                                                     304
C // VERSION OF FRONTENDELECTRONICS                                       304
      TFEE_VERS=1                                                         304
      CTFEE_VERS='// VERSION OF FRONTENDELECTRONICS'                      304
C // 1.47 SCALE FACTOR ACCOUNT FOR 9 LB OF CABLES                         305
      TFEE_CARDDX=1.47*0.110*INCH/2                                       305
      CTFEE_CARDDX='// 1.47 SCALE FACTOR ACCOUNT FOR 9 LB OF CABLES'      305
C //...                                                                   306
      TFEE_CARDDY=2.900*INCH/2                                            306
      CTFEE_CARDDY='//...'                                                306
C //...                                                                   307
      TFEE_CARDDZ=7.000*INCH/2                                            307
      CTFEE_CARDDZ='//...'                                                307
C //...                                                                   308
      TFEE_PLATEDX=0.110*INCH/2                                           308
      CTFEE_PLATEDX='//...'                                               308
C //...                                                                   309
      TFEE_PLATEDY=1.480*INCH                                             309
      CTFEE_PLATEDY='//...'                                               309
C //...                                                                   310
      TFEE_PLATEDZ=4.650*INCH/2                                           310
      CTFEE_PLATEDZ='//...'                                               310
C //...                                                                   311
      TFEE_ASSEMBLYTHICKNESS=TFEE_CARDDX + TFEE_PLATEDX                   311
      CTFEE_ASSEMBLYTHICKNESS='//...'                                     311
C //...                                                                   312
      TFEE_RIBDX=0.820*INCH/2 - 2* TFEE_CARDDX                            312
      CTFEE_RIBDX='//...'                                                 312
C //...                                                                   313
      TFEE_RIBDY=2.900*INCH/2 - 2* TFEE_CARDDX                            313
      CTFEE_RIBDY='//...'                                                 313
C //...                                                                   314
      TFEE_RIBDZ=TFEE_CARDDX                                              314
      CTFEE_RIBDZ='//...'                                                 314
C // FEE ASSEMBLY BRIK SIZE                                               315
      TFEE_ASS(1)=1.6                                                     315
      TFEE_ASS(2)=4                                                       315
      TFEE_ASS(3)=10                                                      315
      CTFEE_ASS='// FEE ASSEMBLY BRIK SIZE'                               315
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TFEE',    316
     *BPATH_TFEE,IDNUM_TFEE,1, MAP_TFEE,NAMES_TFEE,COMMEN_TFEE,           316
     *BANK_TFEE,LEN_TFEE,LLEN_TFEE,LINK_TFEE,FLAG_TFEE)                   316
C *                                                                       316
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TFEE',    316
     *BPATH_TFEE,IDNUM_TFEE,1, MAP_TFEE,NAMES_TFEE,COMMEN_TFEE,           316
     *BANK_TFEE,LEN_TFEE,LLEN_TFEE,LINK_TFEE,FLAG_TFEE)                   316
C use TECW bank                                                           317
      BPATH_TECW='TECW*'                                                  317
      IDNUM_TECW(1)=0                                                     317
      TECW_SEC=KINNER+1                                                   317
      CALL AGDGETP('TPCEGEO4','*',-999,'TECW',NTECW_SEC,RE_TECW_SEC,      317
     *BPATH_TECW,IDNUM_TECW,1,MAP_TECW,NAMES_TECW,BANK_TECW,LEN_TECW,     317
     *LLEN_TECW,LINK_TECW,FLAG_TECW)                                      317
C *                                                                       318
      CALL AGSBEGM(MODULE,IPRIN)                                          318
      IF (IPRIN.LT.0) RETURN                                              318
C Filling bank RIBS - // RIB STRUCTURE / INNER DRAWING 24A3925G           318
      BANK_TITLE='// RIB STRUCTURE / INNER DRAWING 24A3925G'              318
      BPATH_RIBS='RIBS*'                                                  318
      IDNUM_RIBS(1)=0                                                     319
C // VERSION NUMBER                                                       319
      RIBS_VERSION=KINNER                                                 319
      CRIBS_VERSION='// VERSION NUMBER'                                   319
C // SELCTOR                                                              320
      RIBS_INOUT=KINNER                                                   320
      CRIBS_INOUT='// SELCTOR'                                            320
C // NUMBER OF ELEMENTS                                                   321
      RIBS_NUM=11                                                         321
      CRIBS_NUM='// NUMBER OF ELEMENTS'                                   321
C // X                                                                    324
      RIBS_X(1)=0.000                                                     324
      RIBS_X(2)=4.261                                                     324
      RIBS_X(3)=10.561                                                    324
      RIBS_X(4)=16.861                                                    324
      RIBS_X(5)=23.161                                                    324
      RIBS_X(6)=25.950                                                    324
      RIBS_X(7)=3.726                                                     324
      RIBS_X(8)=3.726                                                     324
      RIBS_X(9)=10.026                                                    324
      RIBS_X(10)=10.026                                                   324
      RIBS_X(11)=25.480                                                   324
      CRIBS_X='// X'                                                      324
C // Y                                                                    327
      RIBS_Y(1)=0.000                                                     327
      RIBS_Y(2)=0.000                                                     327
      RIBS_Y(3)=0.000                                                     327
      RIBS_Y(4)=0.000                                                     327
      RIBS_Y(5)=0.000                                                     327
      RIBS_Y(6)=0.000                                                     327
      RIBS_Y(7)=-5.645                                                    327
      RIBS_Y(8)=5.645                                                     327
      RIBS_Y(9)=-3.895                                                    327
      RIBS_Y(10)=3.895                                                    327
      RIBS_Y(11)=0.000                                                    327
      CRIBS_Y='// Y'                                                      327
C // DX ... LAST=2*(25.950-(1.050+2.515+21.915))                          338
      RIBS_DX(1)=1.470                                                    338
      RIBS_DX(2)=0.375                                                    338
      RIBS_DX(3)=0.375                                                    338
      RIBS_DX(4)=0.375                                                    338
      RIBS_DX(5)=0.375                                                    338
      RIBS_DX(6)=TECW_HEIGHT/INCH-25.950                                  338
      RIBS_DX(7)=1.070                                                    338
      RIBS_DX(8)=1.070                                                    338
      RIBS_DX(9)=1.070                                                    338
      RIBS_DX(10)=1.070                                                   338
      RIBS_DX(11)=0.940                                                   338
      CRIBS_DX='// DX ... LAST=2*(25.950-(1.050+2.515+21.915))'           338
C // DY                                                                   340
      RIBS_DY(1)=0.                                                       340
      RIBS_DY(2)=0.                                                       340
      RIBS_DY(3)=0.                                                       340
      RIBS_DY(4)=0.                                                       340
      RIBS_DY(5)=0.                                                       340
      RIBS_DY(6)=0.                                                       340
      RIBS_DY(7)=0.750                                                    340
      RIBS_DY(8)=0.750                                                    340
      RIBS_DY(9)=0.750                                                    340
      RIBS_DY(10)=0.750                                                   340
      RIBS_DY(11)=2.0                                                     340
      CRIBS_DY='// DY'                                                    340
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RIBS',    341
     *BPATH_RIBS,IDNUM_RIBS,1, MAP_RIBS,NAMES_RIBS,COMMEN_RIBS,           341
     *BANK_RIBS,LEN_RIBS,LLEN_RIBS,LINK_RIBS,FLAG_RIBS)                   341
C *                                                                       341
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RIBS',    341
     *BPATH_RIBS,IDNUM_RIBS,1, MAP_RIBS,NAMES_RIBS,COMMEN_RIBS,           341
     *BANK_RIBS,LEN_RIBS,LLEN_RIBS,LINK_RIBS,FLAG_RIBS)                   341
C use TECW bank                                                           342
      BPATH_TECW='TECW*'                                                  342
      IDNUM_TECW(1)=0                                                     342
      TECW_SEC=KOUTER+1                                                   342
      CALL AGDGETP('TPCEGEO4','*',-999,'TECW',NTECW_SEC,RE_TECW_SEC,      342
     *BPATH_TECW,IDNUM_TECW,1,MAP_TECW,NAMES_TECW,BANK_TECW,LEN_TECW,     342
     *LLEN_TECW,LINK_TECW,FLAG_TECW)                                      342
C *                                                                       343
      CALL AGSBEGM(MODULE,IPRIN)                                          343
      IF (IPRIN.LT.0) RETURN                                              343
C Filling bank RIBS - // RIB STRUCTURE / OUTER DRAWING 24A3925G           343
      BANK_TITLE='// RIB STRUCTURE / OUTER DRAWING 24A3925G'              343
      BPATH_RIBS='RIBS*'                                                  343
      IDNUM_RIBS(1)=0                                                     344
C // VERSION NUMBER                                                       344
      RIBS_VERSION=KOUTER                                                 344
      CRIBS_VERSION='// VERSION NUMBER'                                   344
C // SELCTOR                                                              345
      RIBS_INOUT=KOUTER                                                   345
      CRIBS_INOUT='// SELCTOR'                                            345
C // NUMBER OF ELEMENTS                                                   346
      RIBS_NUM=14                                                         346
      CRIBS_NUM='// NUMBER OF ELEMENTS'                                   346
C // X                                                                    351
      RIBS_X(1)=0.280                                                     351
      RIBS_X(2)=4.261                                                     351
      RIBS_X(3)=7.410                                                     351
      RIBS_X(4)=10.560                                                    351
      RIBS_X(5)=13.710                                                    351
      RIBS_X(6)=16.859                                                    351
      RIBS_X(7)=20.009                                                    351
      RIBS_X(8)=23.158                                                    351
      RIBS_X(9)=26.309                                                    351
      RIBS_X(10)=6.023                                                    351
      RIBS_X(11)=6.023                                                    351
      RIBS_X(12)=12.3225                                                  351
      RIBS_X(13)=15.472                                                   351
      RIBS_X(14)=26.309-1.575/2                                           351
      CRIBS_X='// X'                                                      351
C // Y                                                                    355
      RIBS_Y(1)=0                                                         355
      RIBS_Y(2)=0                                                         355
      RIBS_Y(3)=0                                                         355
      RIBS_Y(4)=0                                                         355
      RIBS_Y(5)=0                                                         355
      RIBS_Y(6)=0                                                         355
      RIBS_Y(7)=0                                                         355
      RIBS_Y(8)=0                                                         355
      RIBS_Y(9)=0                                                         355
      RIBS_Y(10)=17.250/2                                                 355
      RIBS_Y(11)=-17.250/2                                                355
      RIBS_Y(12)=0                                                        355
      RIBS_Y(13)=0                                                        355
      RIBS_Y(14)=0                                                        355
      CRIBS_Y='// Y'                                                      355
C // DX                                                                   358
      RIBS_DX(1)=1.268                                                    358
      RIBS_DX(2)=0.375                                                    358
      RIBS_DX(3)=0.375                                                    358
      RIBS_DX(4)=0.375                                                    358
      RIBS_DX(5)=0.375                                                    358
      RIBS_DX(6)=0.375                                                    358
      RIBS_DX(7)=0.375                                                    358
      RIBS_DX(8)=0.375                                                    358
      RIBS_DX(9)=1.566                                                    358
      RIBS_DX(10)=2.774                                                   358
      RIBS_DX(11)=2.774                                                   358
      RIBS_DX(12)=2.775                                                   358
      RIBS_DX(13)=2.774                                                   358
      RIBS_DX(14)=1.575                                                   358
      CRIBS_DX='// DX'                                                    358
C // DY                                                                   361
      RIBS_DY(1)=0                                                        361
      RIBS_DY(2)=0                                                        361
      RIBS_DY(3)=0                                                        361
      RIBS_DY(4)=0                                                        361
      RIBS_DY(5)=0                                                        361
      RIBS_DY(6)=0                                                        361
      RIBS_DY(7)=0                                                        361
      RIBS_DY(8)=0                                                        361
      RIBS_DY(9)=0                                                        361
      RIBS_DY(10)=1.250                                                   361
      RIBS_DY(11)=1.250                                                   361
      RIBS_DY(12)=0.375                                                   361
      RIBS_DY(13)=0.375                                                   361
      RIBS_DY(14)=1.5680                                                  361
      CRIBS_DY='// DY'                                                    361
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RIBS',    362
     *BPATH_RIBS,IDNUM_RIBS,1, MAP_RIBS,NAMES_RIBS,COMMEN_RIBS,           362
     *BANK_RIBS,LEN_RIBS,LLEN_RIBS,LINK_RIBS,FLAG_RIBS)                   362
C *                                                                       362
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RIBS',    362
     *BPATH_RIBS,IDNUM_RIBS,1, MAP_RIBS,NAMES_RIBS,COMMEN_RIBS,           362
     *BANK_RIBS,LEN_RIBS,LLEN_RIBS,LINK_RIBS,FLAG_RIBS)                   362
C *                                                                       363
      CALL AGSBEGM(MODULE,IPRIN)                                          363
      IF (IPRIN.LT.0) RETURN                                              363
C Filling bank HOLZ - // HOLES PER ROW INNER                              363
      BANK_TITLE='// HOLES PER ROW INNER'                                 363
      BPATH_HOLZ='HOLZ*'                                                  363
      IDNUM_HOLZ(1)=0                                                     364
C // SELECTOR                                                             364
      HOLZ_INOUT=KINNER                                                   364
      CHOLZ_INOUT='// SELECTOR'                                           364
C // NUMBER OF ROWS                                                       365
      HOLZ_NUMBEROFROWS=15                                                365
      CHOLZ_NUMBEROFROWS='// NUMBER OF ROWS'                              365
C // N/ROW                                                                367
      HOLZ_NUMBERPERROW(1)=6                                              367
      HOLZ_NUMBERPERROW(2)=5                                              367
      HOLZ_NUMBERPERROW(3)=5                                              367
      HOLZ_NUMBERPERROW(4)=2                                              367
      HOLZ_NUMBERPERROW(5)=5                                              367
      HOLZ_NUMBERPERROW(6)=4                                              367
      HOLZ_NUMBERPERROW(7)=4                                              367
      HOLZ_NUMBERPERROW(8)=4                                              367
      HOLZ_NUMBERPERROW(9)=4                                              367
      HOLZ_NUMBERPERROW(10)=3                                             367
      HOLZ_NUMBERPERROW(11)=3                                             367
      HOLZ_NUMBERPERROW(12)=3                                             367
      HOLZ_NUMBERPERROW(13)=3                                             367
      HOLZ_NUMBERPERROW(14)=2                                             367
      HOLZ_NUMBERPERROW(15)=2                                             367
      HOLZ_NUMBERPERROW(16)=0                                             367
      CHOLZ_NUMBERPERROW='// N/ROW'                                       367
C // Y HOLES                                                              439
      HOLZ_YHOLES(1)=3.180*(-2.5+0)                                       439
      HOLZ_YHOLES(2)=3.180*(-2.5+1)                                       439
      HOLZ_YHOLES(3)=3.180*(-2.5+2)                                       439
      HOLZ_YHOLES(4)=3.180*(-2.5+3)                                       439
      HOLZ_YHOLES(5)=3.180*(-2.5+4)                                       439
      HOLZ_YHOLES(6)=3.180*(-2.5+5)                                       439
      HOLZ_YHOLES(7)=3.180*(-2.5+6)                                       439
      HOLZ_YHOLES(8)=3.180*(-2.5+7)                                       439
      HOLZ_YHOLES(9)=3.180*(-2.5+8)                                       439
      HOLZ_YHOLES(10)=3.764*(-2.0+0)                                      439
      HOLZ_YHOLES(11)=3.764*(-2.0+1)                                      439
      HOLZ_YHOLES(12)=3.764*(-2.0+2)                                      439
      HOLZ_YHOLES(13)=3.764*(-2.0+3)                                      439
      HOLZ_YHOLES(14)=3.764*(-2.0+4)                                      439
      HOLZ_YHOLES(15)=3.764*(-2.0+5)                                      439
      HOLZ_YHOLES(16)=3.764*(-2.0+6)                                      439
      HOLZ_YHOLES(17)=3.764*(-2.0+7)                                      439
      HOLZ_YHOLES(18)=3.764*(-2.0+8)                                      439
      HOLZ_YHOLES(19)=3.553*(-2.0+0)                                      439
      HOLZ_YHOLES(20)=3.553*(-2.0+1)                                      439
      HOLZ_YHOLES(21)=3.553*(-2.0+2)                                      439
      HOLZ_YHOLES(22)=3.553*(-2.0+3)                                      439
      HOLZ_YHOLES(23)=3.553*(-2.0+4)                                      439
      HOLZ_YHOLES(24)=3.553*(-2.0+5)                                      439
      HOLZ_YHOLES(25)=3.553*(-2.0+6)                                      439
      HOLZ_YHOLES(26)=3.553*(-2.0+7)                                      439
      HOLZ_YHOLES(27)=3.553*(-2.0+8)                                      439
      HOLZ_YHOLES(28)=13.368*(-0.5+0)                                     439
      HOLZ_YHOLES(29)=13.368*(-0.5+1)                                     439
      HOLZ_YHOLES(30)=13.368*(-0.5+2)                                     439
      HOLZ_YHOLES(31)=13.368*(-0.5+3)                                     439
      HOLZ_YHOLES(32)=13.368*(-0.5+4)                                     439
      HOLZ_YHOLES(33)=13.368*(-0.5+5)                                     439
      HOLZ_YHOLES(34)=13.368*(-0.5+6)                                     439
      HOLZ_YHOLES(35)=13.368*(-0.5+7)                                     439
      HOLZ_YHOLES(36)=13.368*(-0.5+8)                                     439
      HOLZ_YHOLES(37)=3.131*(-2.0+0)                                      439
      HOLZ_YHOLES(38)=3.131*(-2.0+1)                                      439
      HOLZ_YHOLES(39)=3.131*(-2.0+2)                                      439
      HOLZ_YHOLES(40)=3.131*(-2.0+3)                                      439
      HOLZ_YHOLES(41)=3.131*(-2.0+4)                                      439
      HOLZ_YHOLES(42)=3.131*(-2.0+5)                                      439
      HOLZ_YHOLES(43)=3.131*(-2.0+6)                                      439
      HOLZ_YHOLES(44)=3.131*(-2.0+7)                                      439
      HOLZ_YHOLES(45)=3.131*(-2.0+8)                                      439
      HOLZ_YHOLES(46)=-3.892 - 1.948                                      439
      HOLZ_YHOLES(47)=-1.948                                              439
      HOLZ_YHOLES(48)=+1.948                                              439
      HOLZ_YHOLES(49)=3.892 + 1.948                                       439
      HOLZ_YHOLES(50)=0.000                                               439
      HOLZ_YHOLES(51)=0.000                                               439
      HOLZ_YHOLES(52)=0.000                                               439
      HOLZ_YHOLES(53)=0.000                                               439
      HOLZ_YHOLES(54)=0.000                                               439
      HOLZ_YHOLES(55)=-3.611 - 1.807                                      439
      HOLZ_YHOLES(56)=-1.807                                              439
      HOLZ_YHOLES(57)=+1.807                                              439
      HOLZ_YHOLES(58)=3.611 + 1.807                                       439
      HOLZ_YHOLES(59)=0.000                                               439
      HOLZ_YHOLES(60)=0.000                                               439
      HOLZ_YHOLES(61)=0.000                                               439
      HOLZ_YHOLES(62)=0.000                                               439
      HOLZ_YHOLES(63)=0.000                                               439
      HOLZ_YHOLES(64)=-3.281 - 1.7145                                     439
      HOLZ_YHOLES(65)=-1.7145                                             439
      HOLZ_YHOLES(66)=+1.7145                                             439
      HOLZ_YHOLES(67)=3.281 + 1.7145                                      439
      HOLZ_YHOLES(68)=0.000                                               439
      HOLZ_YHOLES(69)=0.000                                               439
      HOLZ_YHOLES(70)=0.000                                               439
      HOLZ_YHOLES(71)=0.000                                               439
      HOLZ_YHOLES(72)=0.000                                               439
      HOLZ_YHOLES(73)=3.150*(-1.5+0)                                      439
      HOLZ_YHOLES(74)=3.150*(-1.5+1)                                      439
      HOLZ_YHOLES(75)=3.150*(-1.5+2)                                      439
      HOLZ_YHOLES(76)=3.150*(-1.5+3)                                      439
      HOLZ_YHOLES(77)=3.150*(-1.5+4)                                      439
      HOLZ_YHOLES(78)=3.150*(-1.5+5)                                      439
      HOLZ_YHOLES(79)=3.150*(-1.5+6)                                      439
      HOLZ_YHOLES(80)=3.150*(-1.5+7)                                      439
      HOLZ_YHOLES(81)=3.150*(-1.5+8)                                      439
      HOLZ_YHOLES(82)=4.152*(-1.0+0)                                      439
      HOLZ_YHOLES(83)=4.152*(-1.0+1)                                      439
      HOLZ_YHOLES(84)=4.152*(-1.0+2)                                      439
      HOLZ_YHOLES(85)=4.152*(-1.0+3)                                      439
      HOLZ_YHOLES(86)=4.152*(-1.0+4)                                      439
      HOLZ_YHOLES(87)=4.152*(-1.0+5)                                      439
      HOLZ_YHOLES(88)=4.152*(-1.0+6)                                      439
      HOLZ_YHOLES(89)=4.152*(-1.0+7)                                      439
      HOLZ_YHOLES(90)=4.152*(-1.0+8)                                      439
      HOLZ_YHOLES(91)=3.730*(-1.0+0)                                      439
      HOLZ_YHOLES(92)=3.730*(-1.0+1)                                      439
      HOLZ_YHOLES(93)=3.730*(-1.0+2)                                      439
      HOLZ_YHOLES(94)=3.730*(-1.0+3)                                      439
      HOLZ_YHOLES(95)=3.730*(-1.0+4)                                      439
      HOLZ_YHOLES(96)=3.730*(-1.0+5)                                      439
      HOLZ_YHOLES(97)=3.730*(-1.0+6)                                      439
      HOLZ_YHOLES(98)=3.730*(-1.0+7)                                      439
      HOLZ_YHOLES(99)=3.730*(-1.0+8)                                      439
      HOLZ_YHOLES(100)=3.308*(-1.0+0)                                     439
      HOLZ_YHOLES(101)=3.308*(-1.0+1)                                     439
      HOLZ_YHOLES(102)=3.308*(-1.0+2)                                     439
      HOLZ_YHOLES(103)=3.308*(-1.0+3)                                     439
      HOLZ_YHOLES(104)=3.308*(-1.0+4)                                     439
      HOLZ_YHOLES(105)=3.308*(-1.0+5)                                     439
      HOLZ_YHOLES(106)=3.308*(-1.0+6)                                     439
      HOLZ_YHOLES(107)=3.308*(-1.0+7)                                     439
      HOLZ_YHOLES(108)=3.308*(-1.0+8)                                     439
      HOLZ_YHOLES(109)=3.075*(-1.0+0)                                     439
      HOLZ_YHOLES(110)=3.075*(-1.0+1)                                     439
      HOLZ_YHOLES(111)=3.075*(-1.0+2)                                     439
      HOLZ_YHOLES(112)=3.075*(-1.0+3)                                     439
      HOLZ_YHOLES(113)=3.075*(-1.0+4)                                     439
      HOLZ_YHOLES(114)=3.075*(-1.0+5)                                     439
      HOLZ_YHOLES(115)=3.075*(-1.0+6)                                     439
      HOLZ_YHOLES(116)=3.075*(-1.0+7)                                     439
      HOLZ_YHOLES(117)=3.075*(-1.0+8)                                     439
      HOLZ_YHOLES(118)=4.927*(-0.5+0)                                     439
      HOLZ_YHOLES(119)=4.927*(-0.5+1)                                     439
      HOLZ_YHOLES(120)=4.927*(-0.5+2)                                     439
      HOLZ_YHOLES(121)=4.927*(-0.5+3)                                     439
      HOLZ_YHOLES(122)=4.927*(-0.5+4)                                     439
      HOLZ_YHOLES(123)=4.927*(-0.5+5)                                     439
      HOLZ_YHOLES(124)=4.927*(-0.5+6)                                     439
      HOLZ_YHOLES(125)=4.927*(-0.5+7)                                     439
      HOLZ_YHOLES(126)=4.927*(-0.5+8)                                     439
      HOLZ_YHOLES(127)=4.083*(-0.5+0)                                     439
      HOLZ_YHOLES(128)=4.083*(-0.5+1)                                     439
      HOLZ_YHOLES(129)=4.083*(-0.5+2)                                     439
      HOLZ_YHOLES(130)=4.083*(-0.5+3)                                     439
      HOLZ_YHOLES(131)=4.083*(-0.5+4)                                     439
      HOLZ_YHOLES(132)=4.083*(-0.5+5)                                     439
      HOLZ_YHOLES(133)=4.083*(-0.5+6)                                     439
      HOLZ_YHOLES(134)=4.083*(-0.5+7)                                     439
      HOLZ_YHOLES(135)=4.083*(-0.5+8)                                     439
      HOLZ_YHOLES(136)=0.000                                              439
      HOLZ_YHOLES(137)=0.000                                              439
      HOLZ_YHOLES(138)=0.000                                              439
      HOLZ_YHOLES(139)=0.000                                              439
      HOLZ_YHOLES(140)=0.000                                              439
      HOLZ_YHOLES(141)=0.000                                              439
      HOLZ_YHOLES(142)=0.000                                              439
      HOLZ_YHOLES(143)=0.000                                              439
      HOLZ_YHOLES(144)=0.000                                              439
      CHOLZ_YHOLES='// Y HOLES'                                           439
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'HOLZ',    440
     *BPATH_HOLZ,IDNUM_HOLZ,1, MAP_HOLZ,NAMES_HOLZ,COMMEN_HOLZ,           440
     *BANK_HOLZ,LEN_HOLZ,LLEN_HOLZ,LINK_HOLZ,FLAG_HOLZ)                   440
C *                                                                       440
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'HOLZ',    440
     *BPATH_HOLZ,IDNUM_HOLZ,1, MAP_HOLZ,NAMES_HOLZ,COMMEN_HOLZ,           440
     *BANK_HOLZ,LEN_HOLZ,LLEN_HOLZ,LINK_HOLZ,FLAG_HOLZ)                   440
C *                                                                       441
      CALL AGSBEGM(MODULE,IPRIN)                                          441
      IF (IPRIN.LT.0) RETURN                                              441
C Filling bank HOLZ - // HOLES PER ROW OUTER                              441
      BANK_TITLE='// HOLES PER ROW OUTER'                                 441
      BPATH_HOLZ='HOLZ*'                                                  441
      IDNUM_HOLZ(1)=0                                                     442
C // SELECTOR                                                             442
      HOLZ_INOUT=KOUTER                                                   442
      CHOLZ_INOUT='// SELECTOR'                                           442
C // NUMBER OF ROWS                                                       443
      HOLZ_NUMBEROFROWS=16                                                443
      CHOLZ_NUMBEROFROWS='// NUMBER OF ROWS'                              443
C // N/ROW                                                                445
      HOLZ_NUMBERPERROW(1)=9                                              445
      HOLZ_NUMBERPERROW(2)=9                                              445
      HOLZ_NUMBERPERROW(3)=9                                              445
      HOLZ_NUMBERPERROW(4)=9                                              445
      HOLZ_NUMBERPERROW(5)=9                                              445
      HOLZ_NUMBERPERROW(6)=8                                              445
      HOLZ_NUMBERPERROW(7)=8                                              445
      HOLZ_NUMBERPERROW(8)=8                                              445
      HOLZ_NUMBERPERROW(9)=8                                              445
      HOLZ_NUMBERPERROW(10)=8                                             445
      HOLZ_NUMBERPERROW(11)=7                                             445
      HOLZ_NUMBERPERROW(12)=7                                             445
      HOLZ_NUMBERPERROW(13)=7                                             445
      HOLZ_NUMBERPERROW(14)=7                                             445
      HOLZ_NUMBERPERROW(15)=7                                             445
      HOLZ_NUMBERPERROW(16)=6                                             445
      CHOLZ_NUMBERPERROW='// N/ROW'                                       445
C // Y HOLES                                                              481
      HOLZ_YHOLES(1)=-15.025                                              481
      HOLZ_YHOLES(2)=-11.606                                              481
      HOLZ_YHOLES(3)=-8.177                                               481
      HOLZ_YHOLES(4)=-4.220                                               481
      HOLZ_YHOLES(5)=0                                                    481
      HOLZ_YHOLES(6)=4.220                                                481
      HOLZ_YHOLES(7)=8.177                                                481
      HOLZ_YHOLES(8)=11.606                                               481
      HOLZ_YHOLES(9)=15.025                                               481
      HOLZ_YHOLES(10)=-15.025                                             481
      HOLZ_YHOLES(11)=-11.606                                             481
      HOLZ_YHOLES(12)=-8.177                                              481
      HOLZ_YHOLES(13)=-4.220                                              481
      HOLZ_YHOLES(14)=0                                                   481
      HOLZ_YHOLES(15)=4.220                                               481
      HOLZ_YHOLES(16)=8.177                                               481
      HOLZ_YHOLES(17)=11.606                                              481
      HOLZ_YHOLES(18)=15.025                                              481
      HOLZ_YHOLES(19)=-14.243                                             481
      HOLZ_YHOLES(20)=-11.078                                             481
      HOLZ_YHOLES(21)=-6.330                                              481
      HOLZ_YHOLES(22)=-3.165                                              481
      HOLZ_YHOLES(23)=0                                                   481
      HOLZ_YHOLES(24)=3.165                                               481
      HOLZ_YHOLES(25)=6.330                                               481
      HOLZ_YHOLES(26)=11.078                                              481
      HOLZ_YHOLES(27)=14.243                                              481
      HOLZ_YHOLES(28)=-14.243                                             481
      HOLZ_YHOLES(29)=-11.078                                             481
      HOLZ_YHOLES(30)=-6.330                                              481
      HOLZ_YHOLES(31)=-3.165                                              481
      HOLZ_YHOLES(32)=0                                                   481
      HOLZ_YHOLES(33)=3.165                                               481
      HOLZ_YHOLES(34)=6.330                                               481
      HOLZ_YHOLES(35)=11.078                                              481
      HOLZ_YHOLES(36)=14.243                                              481
      HOLZ_YHOLES(37)=-13.452                                             481
      HOLZ_YHOLES(38)=-10.023                                             481
      HOLZ_YHOLES(39)=-6.330                                              481
      HOLZ_YHOLES(40)=-3.165                                              481
      HOLZ_YHOLES(41)=0                                                   481
      HOLZ_YHOLES(42)=3.165                                               481
      HOLZ_YHOLES(43)=6.330                                               481
      HOLZ_YHOLES(44)=10.023                                              481
      HOLZ_YHOLES(45)=13.452                                              481
      HOLZ_YHOLES(46)=-13.452                                             481
      HOLZ_YHOLES(47)=-10.023                                             481
      HOLZ_YHOLES(48)=-6.330                                              481
      HOLZ_YHOLES(49)=-3.165                                              481
      HOLZ_YHOLES(50)=3.165                                               481
      HOLZ_YHOLES(51)=6.330                                               481
      HOLZ_YHOLES(52)=10.023                                              481
      HOLZ_YHOLES(53)=13.452                                              481
      HOLZ_YHOLES(54)=0                                                   481
      HOLZ_YHOLES(55)=-12.660                                             481
      HOLZ_YHOLES(56)=-9.495                                              481
      HOLZ_YHOLES(57)=-6.330                                              481
      HOLZ_YHOLES(58)=-2.110                                              481
      HOLZ_YHOLES(59)=2.110                                               481
      HOLZ_YHOLES(60)=6.330                                               481
      HOLZ_YHOLES(61)=9.495                                               481
      HOLZ_YHOLES(62)=12.660                                              481
      HOLZ_YHOLES(63)=0                                                   481
      HOLZ_YHOLES(64)=-12.660                                             481
      HOLZ_YHOLES(65)=-9.495                                              481
      HOLZ_YHOLES(66)=-6.330                                              481
      HOLZ_YHOLES(67)=-2.110                                              481
      HOLZ_YHOLES(68)=2.110                                               481
      HOLZ_YHOLES(69)=6.330                                               481
      HOLZ_YHOLES(70)=9.495                                               481
      HOLZ_YHOLES(71)=12.660                                              481
      HOLZ_YHOLES(72)=0                                                   481
      HOLZ_YHOLES(73)=-11.870                                             481
      HOLZ_YHOLES(74)=-8.440                                              481
      HOLZ_YHOLES(75)=-5.275                                              481
      HOLZ_YHOLES(76)=-2.110                                              481
      HOLZ_YHOLES(77)=2.110                                               481
      HOLZ_YHOLES(78)=5.275                                               481
      HOLZ_YHOLES(79)=8.440                                               481
      HOLZ_YHOLES(80)=11.870                                              481
      HOLZ_YHOLES(81)=0                                                   481
      HOLZ_YHOLES(82)=-11.870                                             481
      HOLZ_YHOLES(83)=-8.440                                              481
      HOLZ_YHOLES(84)=-5.275                                              481
      HOLZ_YHOLES(85)=-2.110                                              481
      HOLZ_YHOLES(86)=2.110                                               481
      HOLZ_YHOLES(87)=5.275                                               481
      HOLZ_YHOLES(88)=8.440                                               481
      HOLZ_YHOLES(89)=11.870                                              481
      HOLZ_YHOLES(90)=0                                                   481
      HOLZ_YHOLES(91)=-11.078                                             481
      HOLZ_YHOLES(92)=-7.649                                              481
      HOLZ_YHOLES(93)=-4.220                                              481
      HOLZ_YHOLES(94)=0                                                   481
      HOLZ_YHOLES(95)=4.220                                               481
      HOLZ_YHOLES(96)=7.649                                               481
      HOLZ_YHOLES(97)=11.078                                              481
      HOLZ_YHOLES(98)=0                                                   481
      HOLZ_YHOLES(99)=0                                                   481
      HOLZ_YHOLES(100)=-11.078                                            481
      HOLZ_YHOLES(101)=-7.649                                             481
      HOLZ_YHOLES(102)=-4.220                                             481
      HOLZ_YHOLES(103)=0                                                  481
      HOLZ_YHOLES(104)=4.220                                              481
      HOLZ_YHOLES(105)=7.649                                              481
      HOLZ_YHOLES(106)=11.078                                             481
      HOLZ_YHOLES(107)=0                                                  481
      HOLZ_YHOLES(108)=0                                                  481
      HOLZ_YHOLES(109)=-10.022                                            481
      HOLZ_YHOLES(110)=-6.858                                             481
      HOLZ_YHOLES(111)=-3.429                                             481
      HOLZ_YHOLES(112)=0                                                  481
      HOLZ_YHOLES(113)=3.429                                              481
      HOLZ_YHOLES(114)=6.858                                              481
      HOLZ_YHOLES(115)=10.022                                             481
      HOLZ_YHOLES(116)=0                                                  481
      HOLZ_YHOLES(117)=0                                                  481
      HOLZ_YHOLES(118)=-10.022                                            481
      HOLZ_YHOLES(119)=-6.858                                             481
      HOLZ_YHOLES(120)=-3.429                                             481
      HOLZ_YHOLES(121)=0                                                  481
      HOLZ_YHOLES(122)=3.429                                              481
      HOLZ_YHOLES(123)=6.858                                              481
      HOLZ_YHOLES(124)=10.022                                             481
      HOLZ_YHOLES(125)=0                                                  481
      HOLZ_YHOLES(126)=0                                                  481
      HOLZ_YHOLES(127)=-9.495                                             481
      HOLZ_YHOLES(128)=-6.330                                             481
      HOLZ_YHOLES(129)=-3.165                                             481
      HOLZ_YHOLES(130)=0                                                  481
      HOLZ_YHOLES(131)=3.165                                              481
      HOLZ_YHOLES(132)=6.330                                              481
      HOLZ_YHOLES(133)=9.495                                              481
      HOLZ_YHOLES(134)=0                                                  481
      HOLZ_YHOLES(135)=0                                                  481
      HOLZ_YHOLES(136)=-9.495                                             481
      HOLZ_YHOLES(137)=-6.330                                             481
      HOLZ_YHOLES(138)=-3.165                                             481
      HOLZ_YHOLES(139)=3.165                                              481
      HOLZ_YHOLES(140)=6.330                                              481
      HOLZ_YHOLES(141)=9.495                                              481
      HOLZ_YHOLES(142)=0                                                  481
      HOLZ_YHOLES(143)=0                                                  481
      HOLZ_YHOLES(144)=0                                                  481
      CHOLZ_YHOLES='// Y HOLES'                                           481
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'HOLZ',    482
     *BPATH_HOLZ,IDNUM_HOLZ,1, MAP_HOLZ,NAMES_HOLZ,COMMEN_HOLZ,           482
     *BANK_HOLZ,LEN_HOLZ,LLEN_HOLZ,LINK_HOLZ,FLAG_HOLZ)                   482
C *                                                                       482
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'HOLZ',    482
     *BPATH_HOLZ,IDNUM_HOLZ,1, MAP_HOLZ,NAMES_HOLZ,COMMEN_HOLZ,           482
     *BANK_HOLZ,LEN_HOLZ,LLEN_HOLZ,LINK_HOLZ,FLAG_HOLZ)                   482
C *                                                                       483
      CALL AGSBEGM(MODULE,IPRIN)                                          483
      IF (IPRIN.LT.0) RETURN                                              483
C Filling bank COOL - // COOLING TUBE LENGTHS / INNER                     483
      BANK_TITLE='// COOLING TUBE LENGTHS / INNER'                        483
      BPATH_COOL='COOL*'                                                  483
      IDNUM_COOL(1)=0                                                     484
C // INNER VS OUTER                                                       484
      COOL_INOUT=KINNER                                                   484
      CCOOL_INOUT='// INNER VS OUTER'                                     484
C // TUBE LENGTHS                                                         488
      COOL_TUBELENGTH(1)=19.300                                           488
      COOL_TUBELENGTH(2)=18.454                                           488
      COOL_TUBELENGTH(3)=17.610                                           488
      COOL_TUBELENGTH(4)=16.766                                           488
      COOL_TUBELENGTH(5)=15.922                                           488
      COOL_TUBELENGTH(6)=15.078                                           488
      COOL_TUBELENGTH(7)=14.235                                           488
      COOL_TUBELENGTH(8)=13.390                                           488
      COOL_TUBELENGTH(9)=12.546                                           488
      COOL_TUBELENGTH(10)=11.704                                          488
      COOL_TUBELENGTH(11)=10.860                                          488
      COOL_TUBELENGTH(12)=10.016                                          488
      COOL_TUBELENGTH(13)=9.172                                           488
      COOL_TUBELENGTH(14)=8.327                                           488
      COOL_TUBELENGTH(15)=7.483                                           488
      COOL_TUBELENGTH(16)=0.000                                           488
      CCOOL_TUBELENGTH='// TUBE LENGTHS'                                  488
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'COOL',    489
     *BPATH_COOL,IDNUM_COOL,1, MAP_COOL,NAMES_COOL,COMMEN_COOL,           489
     *BANK_COOL,LEN_COOL,LLEN_COOL,LINK_COOL,FLAG_COOL)                   489
C *                                                                       489
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'COOL',    489
     *BPATH_COOL,IDNUM_COOL,1, MAP_COOL,NAMES_COOL,COMMEN_COOL,           489
     *BANK_COOL,LEN_COOL,LLEN_COOL,LINK_COOL,FLAG_COOL)                   489
C *                                                                       490
      CALL AGSBEGM(MODULE,IPRIN)                                          490
      IF (IPRIN.LT.0) RETURN                                              490
C Filling bank COOL - // COOLING TUBE LENGTHS / OUTER                     490
      BANK_TITLE='// COOLING TUBE LENGTHS / OUTER'                        490
      BPATH_COOL='COOL*'                                                  490
      IDNUM_COOL(1)=0                                                     491
C // INNER VS OUTER                                                       491
      COOL_INOUT=KOUTER                                                   491
      CCOOL_INOUT='// INNER VS OUTER'                                     491
C // TUBE LENGTHS                                                         495
      COOL_TUBELENGTH(1)=33.620                                           495
      COOL_TUBELENGTH(2)=33.605                                           495
      COOL_TUBELENGTH(3)=32.761                                           495
      COOL_TUBELENGTH(4)=31.917                                           495
      COOL_TUBELENGTH(5)=31.073                                           495
      COOL_TUBELENGTH(6)=30.229                                           495
      COOL_TUBELENGTH(7)=29.385                                           495
      COOL_TUBELENGTH(8)=28.541                                           495
      COOL_TUBELENGTH(9)=27.697                                           495
      COOL_TUBELENGTH(10)=26.853                                          495
      COOL_TUBELENGTH(11)=26.009                                          495
      COOL_TUBELENGTH(12)=25.165                                          495
      COOL_TUBELENGTH(13)=24.321                                          495
      COOL_TUBELENGTH(14)=23.478                                          495
      COOL_TUBELENGTH(15)=22.634                                          495
      COOL_TUBELENGTH(16)=21.790                                          495
      CCOOL_TUBELENGTH='// TUBE LENGTHS'                                  495
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'COOL',    496
     *BPATH_COOL,IDNUM_COOL,1, MAP_COOL,NAMES_COOL,COMMEN_COOL,           496
     *BANK_COOL,LEN_COOL,LLEN_COOL,LINK_COOL,FLAG_COOL)                   496
C *                                                                       496
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'COOL',    496
     *BPATH_COOL,IDNUM_COOL,1, MAP_COOL,NAMES_COOL,COMMEN_COOL,           496
     *BANK_COOL,LEN_COOL,LLEN_COOL,LINK_COOL,FLAG_COOL)                   496
C use default TFEE bank                                                   497
      BPATH_TFEE='TFEE*'                                                  497
      IDNUM_TFEE(1)=0                                                     497
      CALL AGDGETP('TPCEGEO4','*',-999,'TFEE',' ',BANK_TFEE, BPATH_TFEE,  497
     *IDNUM_TFEE,1,MAP_TFEE,NAMES_TFEE,BANK_TFEE,LEN_TFEE,LLEN_TFEE,      497
     *LINK_TFEE,FLAG_TFEE)                                                497
C use default TECW bank                                                   498
      BPATH_TECW='TECW*'                                                  498
      IDNUM_TECW(1)=0                                                     498
      CALL AGDGETP('TPCEGEO4','*',-999,'TECW',' ',BANK_TECW, BPATH_TECW,  498
     *IDNUM_TECW,1,MAP_TECW,NAMES_TECW,BANK_TECW,LEN_TECW,LLEN_TECW,      498
     *LINK_TECW,FLAG_TECW)                                                498
C use default TPCR bank                                                   499
      BPATH_TPCR='TPCR*'                                                  499
      IDNUM_TPCR(1)=0                                                     499
      CALL AGDGETP('TPCEGEO4','*',-999,'TPCR',' ',BANK_TPCR, BPATH_TPCR,  499
     *IDNUM_TPCR,1,MAP_TPCR,NAMES_TPCR,BANK_TPCR,LEN_TPCR,LLEN_TPCR,      499
     *LINK_TPCR,FLAG_TPCR)                                                499
C use default TPCC bank                                                   500
      BPATH_TPCC='TPCC*'                                                  500
      IDNUM_TPCC(1)=0                                                     500
      CALL AGDGETP('TPCEGEO4','*',-999,'TPCC',' ',BANK_TPCC, BPATH_TPCC,  500
     *IDNUM_TPCC,1,MAP_TPCC,NAMES_TPCC,BANK_TPCC,LEN_TPCC,LLEN_TPCC,      500
     *LINK_TPCC,FLAG_TPCC)                                                500
C use TPCG bank                                                           501
      BPATH_TPCG='TPCG*'                                                  501
      IDNUM_TPCG(1)=0                                                     501
      TPCG_VERSION=TPCC_VERSION                                           501
      CALL AGDGETP('TPCEGEO4','*',-999,'TPCG',NTPCG_VERSION,              501
     *RE_TPCG_VERSION,BPATH_TPCG,IDNUM_TPCG,1,MAP_TPCG,NAMES_TPCG,        501
     *BANK_TPCG,LEN_TPCG,LLEN_TPCG,LINK_TPCG,FLAG_TPCG)                   501
C GAS PLUS ENDCAPS                                                        502
      TOFCLENG = TPCG_LENGTH-2*TPCG_WHEELTHK-2*TPCR_RDOVTHK               502
C ACTIVE GAS                                                              503
      TPGVLENG = (TOFCLENG-TPCG_MEMBTHK)/2                                503
C ACTIVE GAS                                                              504
      TPGVLENG = (TPCG_LENGTHV-TPCG_MEMBTHK)/2                            504
      TOCSIR = TPCG_SENGASOR                                              505
      TOCSOR = TOCSIR + TPCG_TOCSDR                                       506
      TOKAIR = TOCSOR                                                     506
      TOKAOR = TOKAIR + TPCG_TOKADR                                       507
      TONXIR = TOKAOR                                                     507
      TONXOR = TONXIR + TPCG_TONXDR                                       508
      TOADIR = TONXOR                                                     508
      TOADOR = TOADIR + TPCG_TOADDR                                       509
      TOIGIR = TOADOR                                                     509
      TOIGOR = TOIGIR + TPCG_TOIGDR                                       510
      TOALIR = TOIGOR                                                     510
      TOALOR = TOALIR + TPCG_TOALDR/2.                                    511
      TOHAIR = TOALOR                                                     511
      TOHAOR = TOHAIR + TPCG_TOHADR                                       512
      TOALIR2= TOHAOR                                                     512
      TOALOR2= TOALIR2+ TPCG_TOALDR/2.                                    513
      TOFCIR = TOCSIR                                                     513
      TOFCOR = TOALOR2                                                    514
      TOFSIR = TOCSIR                                                     514
      TOFSOR = TOADOR                                                     515
      TIFCIR = TPCG_RMINIFC                                               516
      TIALIR = TIFCIR                                                     516
      TIALOR = TIALIR + TPCG_TIALDR/2.                                    517
      TIKAIR = TIALOR                                                     517
      TIKAOR = TIKAIR + TPCG_TIKADR                                       518
      TINXIR = TIKAOR                                                     518
      TINXOR = TINXIR + TPCG_TINXDR                                       519
      TIADIR = TINXOR                                                     519
      TIADOR = TIADIR + TPCG_TIADDR                                       520
      TIFCOR = TIADOR + TPCG_TIALDR/2.                                    521
      TPGVIR = TIFCOR                                                     521
C // TPC GAS INNER RADIUS                                                 522
      DEL = TPCG_RMAX-TOFCOR                                              523
      WRITE(LOUT,5010)TPCG_VERSION                                        523
5010  FORMAT(' TPCE: config=',F9.3)                                       525
      WIDTH1IOF(1)=7.661*INCH                                             526
      WIDTH1IOF(2)=22.287*INCH                                            526
      WIDTH2IOF(1)=20.552*INCH                                            527
      WIDTH2IOF(2)=35.422*INCH                                            527
      HEIGHTIOF(1)=24.222*INCH                                            528
      HEIGHTIOF(2)=24.840*INCH                                            528
      DEPTHIOF(1)=1.800*INCH                                              529
      DEPTHIOF(2)=1.800*INCH                                              529
C Geant COMPONENT : C ===> A=12 Z=6 W=5 <===                              530
      CALL AGSRESET                                                       530
      AG_A = 12                                                           530
      AG_Z = 6                                                            530
      AG_W = 5                                                            530
      AG_TITLE = 'C'                                                      530
      AG_EXNAME = 'COMPONENT'                                             530
      AG_PARLIST = 'A_Z_W'                                                530
      CALL AXCOMPONENT                                                    530
C *                                                                       530
C Geant COMPONENT : H ===> A=1 Z=1 W=8 <===                               530
      CALL AGSRESET                                                       531
      AG_A = 1                                                            531
      AG_Z = 1                                                            531
      AG_W = 8                                                            531
      AG_TITLE = 'H'                                                      531
      AG_EXNAME = 'COMPONENT'                                             531
      AG_PARLIST = 'A_Z_W'                                                531
      CALL AXCOMPONENT                                                    531
C *                                                                       531
C Geant COMPONENT : O ===> A=16 Z=8 W=2 <===                              531
      CALL AGSRESET                                                       532
      AG_A = 16                                                           532
      AG_Z = 8                                                            532
      AG_W = 2                                                            532
      AG_TITLE = 'O'                                                      532
      AG_EXNAME = 'COMPONENT'                                             532
      AG_PARLIST = 'A_Z_W'                                                532
      CALL AXCOMPONENT                                                    532
C *                                                                       532
C Geant MIXTURE : NOMEX ===> DENS=0.064 <===                              532
      CALL AGSRESET                                                       533
      AG_DENS = 0.064                                                     533
      AG_TITLE = 'NOMEX'                                                  533
      AG_EXNAME = 'MIXTURE'                                               533
      AG_PARLIST = 'DENS'                                                 533
      CALL AXMIXTURE                                                      533
C *                                                                       533
C Geant COMPONENT : C ===> A=12 Z=6 W=0.625 <===                          533
      CALL AGSRESET                                                       534
      AG_A = 12                                                           534
      AG_Z = 6                                                            534
      AG_W = 0.625                                                        534
      AG_TITLE = 'C'                                                      534
      AG_EXNAME = 'COMPONENT'                                             534
      AG_PARLIST = 'A_Z_W'                                                534
      CALL AXCOMPONENT                                                    534
C *                                                                       534
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4166667E-01 <===                   534
      CALL AGSRESET                                                       535
      AG_A = 1                                                            535
      AG_Z = 1                                                            535
      AG_W = 0.4166667E-01                                                535
      AG_TITLE = 'H'                                                      535
      AG_EXNAME = 'COMPONENT'                                             535
      AG_PARLIST = 'A_Z_W'                                                535
      CALL AXCOMPONENT                                                    535
C *                                                                       535
C Geant COMPONENT : O ===> A=16 Z=8 W=0.3333333 <===                      535
      CALL AGSRESET                                                       536
      AG_A = 16                                                           536
      AG_Z = 8                                                            536
      AG_W = 0.3333333                                                    536
      AG_TITLE = 'O'                                                      536
      AG_EXNAME = 'COMPONENT'                                             536
      AG_PARLIST = 'A_Z_W'                                                536
      CALL AXCOMPONENT                                                    536
C *                                                                       536
C Geant MIXTURE : ADHESIVE ===> DENS=1.2 <===                             536
      CALL AGSRESET                                                       537
      AG_DENS = 1.2                                                       537
      AG_TITLE = 'ADHESIVE'                                               537
      AG_EXNAME = 'MIXTURE'                                               537
      AG_PARLIST = 'DENS'                                                 537
      CALL AXMIXTURE                                                      537
C *                                                                       537
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.0105 <===                       537
      CALL AGSRESET                                                       538
      AG_A = 27                                                           538
      AG_Z = 13                                                           538
      AG_W = 0.0105                                                       538
      AG_TITLE = 'AL'                                                     538
      AG_EXNAME = 'COMPONENT'                                             538
      AG_PARLIST = 'A_Z_W'                                                538
      CALL AXCOMPONENT                                                    538
C *                                                                       538
C Geant COMPONENT : N ===> A=14 Z=7 W=0.7395 <===                         538
      CALL AGSRESET                                                       539
      AG_A = 14                                                           539
      AG_Z = 7                                                            539
      AG_W = 0.7395                                                       539
      AG_TITLE = 'N'                                                      539
      AG_EXNAME = 'COMPONENT'                                             539
      AG_PARLIST = 'A_Z_W'                                                539
      CALL AXCOMPONENT                                                    539
C *                                                                       539
C Geant COMPONENT : ADHESIVE ===> A=9 Z=4.5 W=0.2500 <===                 539
      CALL AGSRESET                                                       540
      AG_A = 9                                                            540
      AG_Z = 4.5                                                          540
      AG_W = 0.2500                                                       540
      AG_TITLE = 'ADHESIVE'                                               540
      AG_EXNAME = 'COMPONENT'                                             540
      AG_PARLIST = 'A_Z_W'                                                540
      CALL AXCOMPONENT                                                    540
C *                                                                       540
C Geant MIXTURE : AL_HONEYCOMB ===> DENS=0.282 <===                       540
      CALL AGSRESET                                                       541
      AG_DENS = 0.282                                                     541
      AG_TITLE = 'AL_HONEYCOMB'                                           541
      AG_EXNAME = 'MIXTURE'                                               541
      AG_PARLIST = 'DENS'                                                 541
      CALL AXMIXTURE                                                      541
C *                                                                       541
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             541
      CALL AGSRESET                                                       542
      AG_A = 28.08                                                        542
      AG_Z = 14                                                           542
      AG_W = 0.6*1*28./60.                                                542
      AG_TITLE = 'SI'                                                     542
      AG_EXNAME = 'COMPONENT'                                             542
      AG_PARLIST = 'A_Z_W'                                                542
      CALL AXCOMPONENT                                                    542
C *                                                                       542
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  542
C =                                                                       542
      CALL AGSRESET                                                       543
      AG_A = 16                                                           543
      AG_Z = 8                                                            543
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               543
      AG_TITLE = 'O'                                                      543
      AG_EXNAME = 'COMPONENT'                                             543
      AG_PARLIST = 'A_Z_W'                                                543
      CALL AXCOMPONENT                                                    543
C *                                                                       543
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 543
      CALL AGSRESET                                                       544
      AG_A = 12                                                           544
      AG_Z = 6                                                            544
      AG_W = 0.4*8*12./174.                                               544
      AG_TITLE = 'C'                                                      544
      AG_EXNAME = 'COMPONENT'                                             544
      AG_PARLIST = 'A_Z_W'                                                544
      CALL AXCOMPONENT                                                    544
C *                                                                       544
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  544
      CALL AGSRESET                                                       545
      AG_A = 1                                                            545
      AG_Z = 1                                                            545
      AG_W = 0.4*14*1./174.                                               545
      AG_TITLE = 'H'                                                      545
      AG_EXNAME = 'COMPONENT'                                             545
      AG_PARLIST = 'A_Z_W'                                                545
      CALL AXCOMPONENT                                                    545
C *                                                                       545
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  545
      CALL AGSRESET                                                       546
      AG_DENS = 1.7                                                       546
      AG_TITLE = 'G10'                                                    546
      AG_EXNAME = 'MIXTURE'                                               546
      AG_PARLIST = 'DENS'                                                 546
      CALL AXMIXTURE                                                      546
C *                                                                       546
C Geant COMPONENT : H ===> A=1 Z=1 W=1.06546299984001044E-02 <===         546
      CALL AGSRESET                                                       547
      AG_A = 1                                                            547
      AG_Z = 1                                                            547
      AG_W = 1.06546299984001044E-02                                      547
      AG_TITLE = 'H'                                                      547
      AG_EXNAME = 'COMPONENT'                                             547
      AG_PARLIST = 'A_Z_W'                                                547
      CALL AXCOMPONENT                                                    547
C *                                                                       547
C Geant COMPONENT : O ===> A=16 Z=8 W=8.52370399872008355E-02 <===        547
      CALL AGSRESET                                                       548
      AG_A = 16                                                           548
      AG_Z = 8                                                            548
      AG_W = 8.52370399872008355E-02                                      548
      AG_TITLE = 'O'                                                      548
      AG_EXNAME = 'COMPONENT'                                             548
      AG_PARLIST = 'A_Z_W'                                                548
      CALL AXCOMPONENT                                                    548
C *                                                                       548
C Geant COMPONENT : AL ===> A=27 Z=14 W=9.04108330014399053E-01 <===      548
      CALL AGSRESET                                                       549
      AG_A = 27                                                           549
      AG_Z = 14                                                           549
      AG_W = 9.04108330014399053E-01                                      549
      AG_TITLE = 'AL'                                                     549
      AG_EXNAME = 'COMPONENT'                                             549
      AG_PARLIST = 'A_Z_W'                                                549
      CALL AXCOMPONENT                                                    549
C *                                                                       549
C Geant MIXTURE : WATER_PIPE ===> DENS=2.32155 <===                       549
      CALL AGSRESET                                                       550
      AG_DENS = 2.32155                                                   550
      AG_TITLE = 'WATER_PIPE'                                             550
      AG_EXNAME = 'MIXTURE'                                               550
      AG_PARLIST = 'DENS'                                                 550
      CALL AXMIXTURE                                                      550
C *                                                                       550
C Geant COMPONENT : CU ===> A=63.54 Z=29 W=0.586 <===                     550
      CALL AGSRESET                                                       551
      AG_A = 63.54                                                        551
      AG_Z = 29                                                           551
      AG_W = 0.586                                                        551
      AG_TITLE = 'CU'                                                     551
      AG_EXNAME = 'COMPONENT'                                             551
      AG_PARLIST = 'A_Z_W'                                                551
      CALL AXCOMPONENT                                                    551
C *                                                                       551
C Geant COMPONENT : C ===> A=12.01 Z=6 W=0.259 <===                       551
      CALL AGSRESET                                                       552
      AG_A = 12.01                                                        552
      AG_Z = 6                                                            552
      AG_W = 0.259                                                        552
      AG_TITLE = 'C'                                                      552
      AG_EXNAME = 'COMPONENT'                                             552
      AG_PARLIST = 'A_Z_W'                                                552
      CALL AXCOMPONENT                                                    552
C *                                                                       552
C Geant COMPONENT : O ===> A=15.999 Z=8 W=0.138 <===                      552
      CALL AGSRESET                                                       553
      AG_A = 15.999                                                       553
      AG_Z = 8                                                            553
      AG_W = 0.138                                                        553
      AG_TITLE = 'O'                                                      553
      AG_EXNAME = 'COMPONENT'                                             553
      AG_PARLIST = 'A_Z_W'                                                553
      CALL AXCOMPONENT                                                    553
C *                                                                       553
C Geant COMPONENT : H ===> A=1.00794 Z=1 W=0.017 <===                     553
      CALL AGSRESET                                                       554
      AG_A = 1.00794                                                      554
      AG_Z = 1                                                            554
      AG_W = 0.017                                                        554
      AG_TITLE = 'H'                                                      554
      AG_EXNAME = 'COMPONENT'                                             554
      AG_PARLIST = 'A_Z_W'                                                554
      CALL AXCOMPONENT                                                    554
C *                                                                       554
C Geant MIXTURE : CABLES ===> DENS=2.68 <===                              554
      CALL AGSRESET                                                       555
      AG_DENS = 2.68                                                      555
      AG_TITLE = 'CABLES'                                                 555
      AG_EXNAME = 'MIXTURE'                                               555
      AG_PARLIST = 'DENS'                                                 555
      CALL AXMIXTURE                                                      555
C *                                                                       555
C Geant COMPONENT : AR ===> A=40 Z=18 W=9 <===                            555
      CALL AGSRESET                                                       556
      AG_A = 40                                                           556
      AG_Z = 18                                                           556
      AG_W = 9                                                            556
      AG_TITLE = 'AR'                                                     556
      AG_EXNAME = 'COMPONENT'                                             556
      AG_PARLIST = 'A_Z_W'                                                556
      CALL AXCOMPONENT                                                    556
C *                                                                       556
C Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                              556
      CALL AGSRESET                                                       557
      AG_A = 12                                                           557
      AG_Z = 6                                                            557
      AG_W = 1                                                            557
      AG_TITLE = 'C'                                                      557
      AG_EXNAME = 'COMPONENT'                                             557
      AG_PARLIST = 'A_Z_W'                                                557
      CALL AXCOMPONENT                                                    557
C *                                                                       557
C Geant COMPONENT : H ===> A=1 Z=1 W=4 <===                               557
      CALL AGSRESET                                                       558
      AG_A = 1                                                            558
      AG_Z = 1                                                            558
      AG_W = 4                                                            558
      AG_TITLE = 'H'                                                      558
      AG_EXNAME = 'COMPONENT'                                             558
      AG_PARLIST = 'A_Z_W'                                                558
      CALL AXCOMPONENT                                                    558
C *                                                                       558
C Geant MIXTURE : P10 ===> DENS=0.154053E-02 <===                         558
      CALL AGSRESET                                                       559
      AG_DENS = 0.154053E-02                                              559
      AG_TITLE = 'P10'                                                    559
      AG_EXNAME = 'MIXTURE'                                               559
      AG_PARLIST = 'DENS'                                                 559
      CALL AXMIXTURE                                                      559
C *                                                                       559
      CALL AGSBEGM(MODULE,IPRIN)                                          559
C Check IPRIN<0                                                           560
      IF (IPRIN.LT.0) RETURN                                              560
C Create TPCE :                                                           560
C Call To Local Procedure TPCE                                            560
      ASSIGN 5030 TO BLOCK_TPCE                                           560
      GO TO  5020                                                         560
5030  CONTINUE                                                            560
C *                                                                       560
C Geant POSITION : TPCE ===> MOTHER='CAVE' <===                           561
      CALL AGSRESET                                                       561
      AG_MOTHER = 'CAVE'                                                  561
      AG_TITLE = 'TPCE'                                                   561
      AG_EXNAME = 'POSITION'                                              561
      AG_PARLIST = 'MOTHER'                                               561
      CALL AXPOSITION                                                     561
C *                                                                       561
C ----------------------------------------------------------------------  561
C ------------ TPCE                                                       561
      CALL AGSENDM                                                        563
      RETURN                                                              563
C Begin Local Procedure TPCE                                              563
C TPCE IS THE TPC ENVELOPE                                                563
5020  CONTINUE                                                            563
         AG_TITLE='TPCE'                                                  563
         AG_MODULE=MODULE                                                 563
         AG_MARK='V00'                                                    563
         CALL AGSPUSH                                                     563
         AG_MODULE=MODULE                                                 563
C    *                                                                    563
C    Geant MATERIAL : AIR ===> inherited arguments used <===              564
         CALL AGSRESET                                                    564
         AG_TITLE = 'AIR'                                                 564
         AG_EXNAME = 'MATERIAL'                                           564
         AG_PARLIST = 'NONE'                                              564
         CALL AXMATERIAL                                                  564
C    *                                                                    564
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           564
         CALL AGSRESET                                                    565
         AG_TITLE = 'STANDARD'                                            565
         AG_EXNAME = 'MEDIUM'                                             565
         AG_PARLIST = 'NONE'                                              565
         CALL AXMEDIUM                                                    565
C    *                                                                    565
C    Geant ATTRIBUTE : TPCE ===> SEEN=0 COLO=KRED <===                    565
         CALL AGSRESET                                                    566
         AG_SEEN = 0                                                      566
         AG_COLO = KRED                                                   566
         AG_TITLE = 'TPCE'                                                566
         AG_EXNAME = 'ATTRIBUTE'                                          566
         AG_PARLIST = 'SEEN_COLO'                                         566
         CALL AXATTRIBUTE                                                 566
C    *                                                                    566
C    Geant SHAPE : TUBE ===> RMIN=TPCG_RMIN RMAX=TPCG_RMAX, DZ=TPCG_DZEN  566
C    VELOP <===                                                           566
         CALL AGSRESET                                                    568
         AG_RMIN = TPCG_RMIN                                              568
         AG_RMAX = TPCG_RMAX                                              568
         AG_DZ = TPCG_DZENVELOP                                           568
         AG_TITLE = 'TUBE'                                                568
         AG_EXNAME = 'SHAPE'                                              568
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      568
         CALL AXSHAPE                                                     568
C    *                                                                    568
C    Check IPRIN>=1                                                       568
         IF (IPRIN.GE.1) WRITE(LOUT,5040)AG_IVOLUME,AG_VOLUME             568
5040     FORMAT('Creating volume ',I4,2X,A4)                              570
         TPGVZ =(TPCG_MEMBTHK + TPGVLENG)/2                               571
         ZWHEEL1 = TPCG_LENGTHW/2 - TPCG_WHEELTHK                         571
         ZTOFCEND = TPCG_LENGTHW/2 - TPCG_WHEELTHK1                       572
         ZTIFCEND = TPCG_LENGTHW/2 + 0.97                                 573
         ZTIFCFLANGEBEGIN = ZTIFCEND - 1*INCH                             574
C    Create TPGV :                                                        576
C    Call To Local Procedure TPGV                                         576
         ASSIGN 5060 TO BLOCK_TPGV                                        576
         GO TO  5050                                                      576
5060     CONTINUE                                                         576
C    *                                                                    576
C    Geant POSITION : TPGV ===> THETAX=90 PHIX=75 THETAY=90 PHIY=-15 THE  577
C    TAZ=0 PHIZ=0 <===                                                    577
         CALL AGSRESET                                                    577
         AG_THETAX = 90                                                   577
         AG_PHIX = 75                                                     577
         AG_THETAY = 90                                                   577
         AG_PHIY = -15                                                    577
         AG_THETAZ = 0                                                    577
         AG_PHIZ = 0                                                      577
         AG_TITLE = 'TPGV'                                                577
         AG_EXNAME = 'POSITION'                                           577
         AG_PARLIST = 'THETAX_PHIX_THETAY_PHIY_THETAZ_PHIZ'               577
         CALL AXPOSITION                                                  577
C    *                                                                    577
C    Geant POSITION : TPGV ===> THETAX=90 PHIX=105 THETAY=90 PHIY=195 TH  577
C    ETAZ=180 PHIZ=0 <===                                                 577
         CALL AGSRESET                                                    578
         AG_THETAX = 90                                                   578
         AG_PHIX = 105                                                    578
         AG_THETAY = 90                                                   578
         AG_PHIY = 195                                                    578
         AG_THETAZ = 180                                                  578
         AG_PHIZ = 0                                                      578
         AG_TITLE = 'TPGV'                                                578
         AG_EXNAME = 'POSITION'                                           578
         AG_PARLIST = 'THETAX_PHIX_THETAY_PHIY_THETAZ_PHIZ'               578
         CALL AXPOSITION                                                  578
C    *                                                                    578
C    Create TPCM :                                                        579
C    Call To Local Procedure TPCM                                         579
         ASSIGN 5080 TO BLOCK_TPCM                                        579
         GO TO  5070                                                      579
5080     CONTINUE                                                         579
C    *                                                                    579
C    Geant POSITION : TPCM ===> inherited arguments used <===             580
         CALL AGSRESET                                                    580
         AG_TITLE = 'TPCM'                                                580
         AG_EXNAME = 'POSITION'                                           580
         AG_PARLIST = 'NONE'                                              580
         CALL AXPOSITION                                                  580
C    *                                                                    580
C    Create TIFC :                                                        581
C    Call To Local Procedure TIFC                                         581
         ASSIGN 5100 TO BLOCK_TIFC                                        581
         GO TO  5090                                                      581
5100     CONTINUE                                                         581
C    *                                                                    581
C    Geant POSITION : TIFC ===> inherited arguments used <===             582
         CALL AGSRESET                                                    582
         AG_TITLE = 'TIFC'                                                582
         AG_EXNAME = 'POSITION'                                           582
         AG_PARLIST = 'NONE'                                              582
         CALL AXPOSITION                                                  582
C    *                                                                    582
C    Create TOFC :                                                        583
C    Call To Local Procedure TOFC                                         583
         ASSIGN 5120 TO BLOCK_TOFC                                        583
         GO TO  5110                                                      583
5120     CONTINUE                                                         583
C    *                                                                    583
C    Geant POSITION : TOFC ===> inherited arguments used <===             584
         CALL AGSRESET                                                    584
         AG_TITLE = 'TOFC'                                                584
         AG_EXNAME = 'POSITION'                                           584
         AG_PARLIST = 'NONE'                                              584
         CALL AXPOSITION                                                  584
C    *                                                                    584
5021  CALL AGSPOPB                                                        585
      GO TO BLOCK_TPCE, (5020 ,5030)                                      585
C End of Local Procedure                                                  585
C ----------------------------------------------------------------------  586
C ------------ TPCM                                                       586
C Begin Local Procedure TPCM                                              587
C TPCM IS THE CENTRAL MEMBRANE PLACED IN TPC                              587
5070  CONTINUE                                                            587
         AG_TITLE='TPCM'                                                  587
         AG_MODULE=MODULE                                                 587
         AG_MARK='V00'                                                    587
         CALL AGSPUSH                                                     587
         AG_MODULE=MODULE                                                 587
C    *                                                                    587
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            588
         CALL AGSRESET                                                    588
         AG_TITLE = 'MYLAR'                                               588
         AG_EXNAME = 'MATERIAL'                                           588
         AG_PARLIST = 'NONE'                                              588
         CALL AXMATERIAL                                                  588
C    *                                                                    588
C    Geant ATTRIBUTE : TPCM ===> SEEN=1 COLO=KBLUE <===                   588
         CALL AGSRESET                                                    589
         AG_SEEN = 1                                                      589
         AG_COLO = KBLUE                                                  589
         AG_TITLE = 'TPCM'                                                589
         AG_EXNAME = 'ATTRIBUTE'                                          589
         AG_PARLIST = 'SEEN_COLO'                                         589
         CALL AXATTRIBUTE                                                 589
C    *                                                                    589
C    Geant SHAPE : TUBE ===> RMIN=TPGVIR RMAX=TPCG_SENGASOR, DZ=TPCG_MEM  589
C    BTHK/2 <===                                                          589
         CALL AGSRESET                                                    591
         AG_RMIN = TPGVIR                                                 591
         AG_RMAX = TPCG_SENGASOR                                          591
         AG_DZ = TPCG_MEMBTHK/2                                           591
         AG_TITLE = 'TUBE'                                                591
         AG_EXNAME = 'SHAPE'                                              591
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      591
         CALL AXSHAPE                                                     591
C    *                                                                    591
5071  CALL AGSPOPB                                                        592
      GO TO BLOCK_TPCM, (5070 ,5080)                                      592
C End of Local Procedure                                                  592
C ----------------------------------------------------------------------  593
C ------------ TIFC                                                       593
C Begin Local Procedure TIFC                                              594
C TIFC DEFINES THE INNER FIELD CAGE PLACED IN TPC                         594
5090  CONTINUE                                                            594
         AG_TITLE='TIFC'                                                  594
         AG_MODULE=MODULE                                                 594
         AG_MARK='V00'                                                    594
         CALL AGSPUSH                                                     594
         AG_MODULE=MODULE                                                 594
C    *                                                                    594
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        595
         CALL AGSRESET                                                    595
         AG_TITLE = 'ALUMINIUM'                                           595
         AG_EXNAME = 'MATERIAL'                                           595
         AG_PARLIST = 'NONE'                                              595
         CALL AXMATERIAL                                                  595
C    *                                                                    595
C    Geant ATTRIBUTE : TIFC ===> SEEN=1 COLO=KBLUE <===                   595
         CALL AGSRESET                                                    596
         AG_SEEN = 1                                                      596
         AG_COLO = KBLUE                                                  596
         AG_TITLE = 'TIFC'                                                596
         AG_EXNAME = 'ATTRIBUTE'                                          596
         AG_PARLIST = 'SEEN_COLO'                                         596
         CALL AXATTRIBUTE                                                 596
C    *                                                                    596
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=6 ZI={-ZTIFCEND, -ZTIFCF  596
C    LANGEBEGIN,-ZTIFCFLANGEBEGIN,ZTIFCFLANGEBEGIN, ZTIFCFLANGEBEGIN,ZTI  596
C    FCEND} RMN={TIFCIR, TIFCIR,TIFCIR,TIFCIR,TIFCIR,TIFCIR} RMX={TPCG_T  596
C    IFCRF, TPCG_TIFCRF,TIFCOR,TIFCOR,TPCG_TIFCRF, TPCG_TIFCRF} <===      596
         CALL AGSRESET                                                    602
         AG_PHI1 = 0                                                      602
         AG_DPHI = 360                                                    602
         AG_NZ = 6                                                        602
         AG_ZI(1)=-ZTIFCEND                                               602
         AG_ZI(2)=-ZTIFCFLANGEBEGIN                                       602
         AG_ZI(3)=-ZTIFCFLANGEBEGIN                                       602
         AG_ZI(4)=ZTIFCFLANGEBEGIN                                        602
         AG_ZI(5)=ZTIFCFLANGEBEGIN                                        602
         AG_ZI(6)=ZTIFCEND                                                602
         AG_RMN(1)=TIFCIR                                                 602
         AG_RMN(2)=TIFCIR                                                 602
         AG_RMN(3)=TIFCIR                                                 602
         AG_RMN(4)=TIFCIR                                                 602
         AG_RMN(5)=TIFCIR                                                 602
         AG_RMN(6)=TIFCIR                                                 602
         AG_RMX(1)=TPCG_TIFCRF                                            602
         AG_RMX(2)=TPCG_TIFCRF                                            602
         AG_RMX(3)=TIFCOR                                                 602
         AG_RMX(4)=TIFCOR                                                 602
         AG_RMX(5)=TPCG_TIFCRF                                            602
         AG_RMX(6)=TPCG_TIFCRF                                            602
         AG_TITLE = 'PCON'                                                602
         AG_EXNAME = 'SHAPE'                                              602
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           602
         CALL AXSHAPE                                                     602
C    *                                                                    602
C    Create TIKA :                                                        603
C    Call To Local Procedure TIKA                                         603
         ASSIGN 5140 TO BLOCK_TIKA                                        603
         GO TO  5130                                                      603
5140     CONTINUE                                                         603
C    *                                                                    603
C    Geant POSITION : TIKA ===> inherited arguments used <===             604
         CALL AGSRESET                                                    604
         AG_TITLE = 'TIKA'                                                604
         AG_EXNAME = 'POSITION'                                           604
         AG_PARLIST = 'NONE'                                              604
         CALL AXPOSITION                                                  604
C    *                                                                    604
C    Create TINX :                                                        605
C    Call To Local Procedure TINX                                         605
         ASSIGN 5160 TO BLOCK_TINX                                        605
         GO TO  5150                                                      605
5160     CONTINUE                                                         605
C    *                                                                    605
C    Geant POSITION : TINX ===> inherited arguments used <===             606
         CALL AGSRESET                                                    606
         AG_TITLE = 'TINX'                                                606
         AG_EXNAME = 'POSITION'                                           606
         AG_PARLIST = 'NONE'                                              606
         CALL AXPOSITION                                                  606
C    *                                                                    606
C    Create TIAD :                                                        607
C    Call To Local Procedure TIAD                                         607
         ASSIGN 5180 TO BLOCK_TIAD                                        607
         GO TO  5170                                                      607
5180     CONTINUE                                                         607
C    *                                                                    607
C    Geant POSITION : TIAD ===> inherited arguments used <===             608
         CALL AGSRESET                                                    608
         AG_TITLE = 'TIAD'                                                608
         AG_EXNAME = 'POSITION'                                           608
         AG_PARLIST = 'NONE'                                              608
         CALL AXPOSITION                                                  608
C    *                                                                    608
5091  CALL AGSPOPB                                                        609
      GO TO BLOCK_TIFC, (5090 ,5100)                                      609
C End of Local Procedure                                                  609
C ----------------------------------------------------------------------  610
C ------------ TIKA                                                       610
C Begin Local Procedure TIKA                                              611
C TIKA IS THE KAPTON FILM OF THE INNER FIELD CAGE                         611
5130  CONTINUE                                                            611
         AG_TITLE='TIKA'                                                  611
         AG_MODULE=MODULE                                                 611
         AG_MARK='V00'                                                    611
         CALL AGSPUSH                                                     611
         AG_MODULE=MODULE                                                 611
C    *                                                                    611
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            612
         CALL AGSRESET                                                    612
         AG_TITLE = 'MYLAR'                                               612
         AG_EXNAME = 'MATERIAL'                                           612
         AG_PARLIST = 'NONE'                                              612
         CALL AXMATERIAL                                                  612
C    *                                                                    612
C    Geant ATTRIBUTE : TIKA ===> SEEN=1 COLO=KVIOLET <===                 612
         CALL AGSRESET                                                    613
         AG_SEEN = 1                                                      613
         AG_COLO = KVIOLET                                                613
         AG_TITLE = 'TIKA'                                                613
         AG_EXNAME = 'ATTRIBUTE'                                          613
         AG_PARLIST = 'SEEN_COLO'                                         613
         CALL AXATTRIBUTE                                                 613
C    *                                                                    613
C    Geant SHAPE : TUBE ===> RMIN=TIKAIR RMAX=TIKAOR DZ=TPCG_DZYF1 <===   613
         CALL AGSRESET                                                    614
         AG_RMIN = TIKAIR                                                 614
         AG_RMAX = TIKAOR                                                 614
         AG_DZ = TPCG_DZYF1                                               614
         AG_TITLE = 'TUBE'                                                614
         AG_EXNAME = 'SHAPE'                                              614
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      614
         CALL AXSHAPE                                                     614
C    *                                                                    614
5131  CALL AGSPOPB                                                        615
      GO TO BLOCK_TIKA, (5130 ,5140)                                      615
C End of Local Procedure                                                  615
C ----------------------------------------------------------------------  616
C ------------ TINX                                                       616
C Begin Local Procedure TINX                                              617
C TINX IS THE INNER NOMEX STRUCTUREURE                                    617
5150  CONTINUE                                                            617
         AG_TITLE='TINX'                                                  617
         AG_MODULE=MODULE                                                 617
         AG_MARK='V00'                                                    617
         CALL AGSPUSH                                                     617
         AG_MODULE=MODULE                                                 617
C    *                                                                    617
C    Geant MATERIAL : NOMEX ===> inherited arguments used <===            618
         CALL AGSRESET                                                    618
         AG_TITLE = 'NOMEX'                                               618
         AG_EXNAME = 'MATERIAL'                                           618
         AG_PARLIST = 'NONE'                                              618
         CALL AXMATERIAL                                                  618
C    *                                                                    618
C    Geant ATTRIBUTE : TINX ===> SEEN=1 COLO=KYELLOW <===                 618
         CALL AGSRESET                                                    619
         AG_SEEN = 1                                                      619
         AG_COLO = KYELLOW                                                619
         AG_TITLE = 'TINX'                                                619
         AG_EXNAME = 'ATTRIBUTE'                                          619
         AG_PARLIST = 'SEEN_COLO'                                         619
         CALL AXATTRIBUTE                                                 619
C    *                                                                    619
C    Geant SHAPE : TUBE ===> RMIN=TINXIR RMAX=TINXOR DZ=TPCG_DZYF1 <===   619
         CALL AGSRESET                                                    620
         AG_RMIN = TINXIR                                                 620
         AG_RMAX = TINXOR                                                 620
         AG_DZ = TPCG_DZYF1                                               620
         AG_TITLE = 'TUBE'                                                620
         AG_EXNAME = 'SHAPE'                                              620
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      620
         CALL AXSHAPE                                                     620
C    *                                                                    620
5151  CALL AGSPOPB                                                        621
      GO TO BLOCK_TINX, (5150 ,5160)                                      621
C End of Local Procedure                                                  621
C ----------------------------------------------------------------------  622
C ------------ TIAD                                                       622
C Begin Local Procedure TIAD                                              623
C TIAD THE INNER ADHESIVE STRUCTUREURE                                    623
5170  CONTINUE                                                            623
         AG_TITLE='TIAD'                                                  623
         AG_MODULE=MODULE                                                 623
         AG_MARK='V00'                                                    623
         CALL AGSPUSH                                                     623
         AG_MODULE=MODULE                                                 623
C    *                                                                    623
C    Geant MATERIAL : ADHESIVE ===> inherited arguments used <===         624
         CALL AGSRESET                                                    624
         AG_TITLE = 'ADHESIVE'                                            624
         AG_EXNAME = 'MATERIAL'                                           624
         AG_PARLIST = 'NONE'                                              624
         CALL AXMATERIAL                                                  624
C    *                                                                    624
C    Geant ATTRIBUTE : TIKA ===> SEEN=1 COLO=KVIOLET <===                 624
         CALL AGSRESET                                                    625
         AG_SEEN = 1                                                      625
         AG_COLO = KVIOLET                                                625
         AG_TITLE = 'TIKA'                                                625
         AG_EXNAME = 'ATTRIBUTE'                                          625
         AG_PARLIST = 'SEEN_COLO'                                         625
         CALL AXATTRIBUTE                                                 625
C    *                                                                    625
C    Geant SHAPE : TUBE ===> RMIN=TIADIR RMAX=TIADOR DZ=TPCG_DZYF1 <===   625
         CALL AGSRESET                                                    626
         AG_RMIN = TIADIR                                                 626
         AG_RMAX = TIADOR                                                 626
         AG_DZ = TPCG_DZYF1                                               626
         AG_TITLE = 'TUBE'                                                626
         AG_EXNAME = 'SHAPE'                                              626
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      626
         CALL AXSHAPE                                                     626
C    *                                                                    626
5171  CALL AGSPOPB                                                        627
      GO TO BLOCK_TIAD, (5170 ,5180)                                      627
C End of Local Procedure                                                  627
C ----------------------------------------------------------------------  628
C ------------ TOFC                                                       628
C Begin Local Procedure TOFC                                              629
C TOFC DEFINES OUTER FIELD CAGE - FILL IT WITH INSULATING GAS ALREADY     629
5110  CONTINUE                                                            629
         AG_TITLE='TOFC'                                                  629
         AG_MODULE=MODULE                                                 629
         AG_MARK='V00'                                                    629
         CALL AGSPUSH                                                     629
         AG_MODULE=MODULE                                                 629
C    *                                                                    629
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        630
         CALL AGSRESET                                                    630
         AG_TITLE = 'ALUMINIUM'                                           630
         AG_EXNAME = 'MATERIAL'                                           630
         AG_PARLIST = 'NONE'                                              630
         CALL AXMATERIAL                                                  630
C    *                                                                    630
C    Geant ATTRIBUTE : TOFC ===> SEEN=1 COLO=KGREEN <===                  630
         CALL AGSRESET                                                    631
         AG_SEEN = 1                                                      631
         AG_COLO = KGREEN                                                 631
         AG_TITLE = 'TOFC'                                                631
         AG_EXNAME = 'ATTRIBUTE'                                          631
         AG_PARLIST = 'SEEN_COLO'                                         631
         CALL AXATTRIBUTE                                                 631
C    *                                                                    631
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=6 ZI={-ZTOFCEND, -ZWHEEL  631
C    1,-ZWHEEL1,ZWHEEL1,ZWHEEL1,ZTOFCEND}, RMN={TPCG_WHEELOR1,TPCG_WHEEL  631
C    OR1,TOFCIR, TOFCIR,TPCG_WHEELOR1,TPCG_WHEELOR1} RMX={TPCG_WHEELOR,   631
C    TPCG_WHEELOR,TOFCOR,TOFCOR,TPCG_WHEELOR, TPCG_WHEELOR} <===          631
         CALL AGSRESET                                                    637
         AG_PHI1 = 0                                                      637
         AG_DPHI = 360                                                    637
         AG_NZ = 6                                                        637
         AG_ZI(1)=-ZTOFCEND                                               637
         AG_ZI(2)=-ZWHEEL1                                                637
         AG_ZI(3)=-ZWHEEL1                                                637
         AG_ZI(4)=ZWHEEL1                                                 637
         AG_ZI(5)=ZWHEEL1                                                 637
         AG_ZI(6)=ZTOFCEND                                                637
         AG_RMN(1)=TPCG_WHEELOR1                                          637
         AG_RMN(2)=TPCG_WHEELOR1                                          637
         AG_RMN(3)=TOFCIR                                                 637
         AG_RMN(4)=TOFCIR                                                 637
         AG_RMN(5)=TPCG_WHEELOR1                                          637
         AG_RMN(6)=TPCG_WHEELOR1                                          637
         AG_RMX(1)=TPCG_WHEELOR                                           637
         AG_RMX(2)=TPCG_WHEELOR                                           637
         AG_RMX(3)=TOFCOR                                                 637
         AG_RMX(4)=TOFCOR                                                 637
         AG_RMX(5)=TPCG_WHEELOR                                           637
         AG_RMX(6)=TPCG_WHEELOR                                           637
         AG_TITLE = 'PCON'                                                637
         AG_EXNAME = 'SHAPE'                                              637
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           637
         CALL AXSHAPE                                                     637
C    *                                                                    637
C    Create TOAD :                                                        638
C    Call To Local Procedure TOAD                                         638
         ASSIGN 5200 TO BLOCK_TOAD                                        638
         GO TO  5190                                                      638
5200     CONTINUE                                                         638
C    *                                                                    638
C    Geant POSITION : TOAD ===> inherited arguments used <===             639
         CALL AGSRESET                                                    639
         AG_TITLE = 'TOAD'                                                639
         AG_EXNAME = 'POSITION'                                           639
         AG_PARLIST = 'NONE'                                              639
         CALL AXPOSITION                                                  639
C    *                                                                    639
C    Create TOFS :                                                        640
C    Call To Local Procedure TOFS                                         640
         ASSIGN 5220 TO BLOCK_TOFS                                        640
         GO TO  5210                                                      640
5220     CONTINUE                                                         640
C    *                                                                    640
C    Geant POSITION : TOFS ===> inherited arguments used <===             641
         CALL AGSRESET                                                    641
         AG_TITLE = 'TOFS'                                                641
         AG_EXNAME = 'POSITION'                                           641
         AG_PARLIST = 'NONE'                                              641
         CALL AXPOSITION                                                  641
C    *                                                                    641
C    Create TOKA :                                                        642
C    Call To Local Procedure TOKA                                         642
         ASSIGN 5240 TO BLOCK_TOKA                                        642
         GO TO  5230                                                      642
5240     CONTINUE                                                         642
C    *                                                                    642
C    Geant POSITION : TOKA ===> inherited arguments used <===             643
         CALL AGSRESET                                                    643
         AG_TITLE = 'TOKA'                                                643
         AG_EXNAME = 'POSITION'                                           643
         AG_PARLIST = 'NONE'                                              643
         CALL AXPOSITION                                                  643
C    *                                                                    643
C    Create TONX :                                                        644
C    Call To Local Procedure TONX                                         644
         ASSIGN 5260 TO BLOCK_TONX                                        644
         GO TO  5250                                                      644
5260     CONTINUE                                                         644
C    *                                                                    644
C    Geant POSITION : TONX ===> inherited arguments used <===             645
         CALL AGSRESET                                                    645
         AG_TITLE = 'TONX'                                                645
         AG_EXNAME = 'POSITION'                                           645
         AG_PARLIST = 'NONE'                                              645
         CALL AXPOSITION                                                  645
C    *                                                                    645
C    Create TOIG :                                                        646
C    Call To Local Procedure TOIG                                         646
         ASSIGN 5280 TO BLOCK_TOIG                                        646
         GO TO  5270                                                      646
5280     CONTINUE                                                         646
C    *                                                                    646
C    Geant POSITION : TOIG ===> inherited arguments used <===             647
         CALL AGSRESET                                                    647
         AG_TITLE = 'TOIG'                                                647
         AG_EXNAME = 'POSITION'                                           647
         AG_PARLIST = 'NONE'                                              647
         CALL AXPOSITION                                                  647
C    *                                                                    647
C    Create TOHA :                                                        648
C    Call To Local Procedure TOHA                                         648
         ASSIGN 5300 TO BLOCK_TOHA                                        648
         GO TO  5290                                                      648
5300     CONTINUE                                                         648
C    *                                                                    648
C    Geant POSITION : TOHA ===> inherited arguments used <===             649
         CALL AGSRESET                                                    649
         AG_TITLE = 'TOHA'                                                649
         AG_EXNAME = 'POSITION'                                           649
         AG_PARLIST = 'NONE'                                              649
         CALL AXPOSITION                                                  649
C    *                                                                    649
5111  CALL AGSPOPB                                                        650
      GO TO BLOCK_TOFC, (5110 ,5120)                                      650
C End of Local Procedure                                                  650
C ----------------------------------------------------------------------  651
C ------------ TOFS                                                       651
C Begin Local Procedure TOFS                                              652
C TOFS IS THE OUTER FIELD CAGE STRUCTUREURE                               652
5210  CONTINUE                                                            652
         AG_TITLE='TOFS'                                                  652
         AG_MODULE=MODULE                                                 652
         AG_MARK='V00'                                                    652
         CALL AGSPUSH                                                     652
         AG_MODULE=MODULE                                                 652
C    *                                                                    652
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           653
         CALL AGSRESET                                                    653
         AG_TITLE = 'COPPER'                                              653
         AG_EXNAME = 'MATERIAL'                                           653
         AG_PARLIST = 'NONE'                                              653
         CALL AXMATERIAL                                                  653
C    *                                                                    653
C    Geant MEDIUM : TOFS_COPPER ===> IFIELD=1 FIELDM=20 TMAXFD=20 EPSIL=  653
C    0.01 STEMAX=10 DEEMAX=0.25, STMIN=0.0281386 ISVOL=0 <===             653
         CALL AGSRESET                                                    655
         AG_IFIELD = 1                                                    655
         AG_FIELDM = 20                                                   655
         AG_TMAXFD = 20                                                   655
         AG_EPSIL = 0.01                                                  655
         AG_STEMAX = 10                                                   655
         AG_DEEMAX = 0.25                                                 655
         AG_STMIN = 0.0281386                                             655
         AG_ISVOL = 0                                                     655
         AG_TITLE = 'TOFS_COPPER'                                         655
         AG_EXNAME = 'MEDIUM'                                             655
         AG_PARLIST = 'IFIELD_FIELDM_TMAXFD_EPSIL_STEMAX_DEEMAX_STMIN_IS  655
     *VOL'                                                                655
         CALL AXMEDIUM                                                    655
C    *                                                                    655
C    Geant ATTRIBUTE : TOFS ===> SEEN=1 COLO=KBLUE <===                   655
         CALL AGSRESET                                                    656
         AG_SEEN = 1                                                      656
         AG_COLO = KBLUE                                                  656
         AG_TITLE = 'TOFS'                                                656
         AG_EXNAME = 'ATTRIBUTE'                                          656
         AG_PARLIST = 'SEEN_COLO'                                         656
         CALL AXATTRIBUTE                                                 656
C    *                                                                    656
C    Geant SHAPE : TUBE ===> RMIN=TOCSIR RMAX=TOCSOR DZ=TPCG_DZYF2 <===   656
         CALL AGSRESET                                                    657
         AG_RMIN = TOCSIR                                                 657
         AG_RMAX = TOCSOR                                                 657
         AG_DZ = TPCG_DZYF2                                               657
         AG_TITLE = 'TUBE'                                                657
         AG_EXNAME = 'SHAPE'                                              657
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      657
         CALL AXSHAPE                                                     657
C    *                                                                    657
5211  CALL AGSPOPB                                                        658
      GO TO BLOCK_TOFS, (5210 ,5220)                                      658
C End of Local Procedure                                                  658
C ----------------------------------------------------------------------  659
C ------------ TOKA                                                       659
C Begin Local Procedure TOKA                                              660
C TOKA IS MYLAR LAYER                                                     660
5230  CONTINUE                                                            660
         AG_TITLE='TOKA'                                                  660
         AG_MODULE=MODULE                                                 660
         AG_MARK='V00'                                                    660
         CALL AGSPUSH                                                     660
         AG_MODULE=MODULE                                                 660
C    *                                                                    660
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            661
         CALL AGSRESET                                                    661
         AG_TITLE = 'MYLAR'                                               661
         AG_EXNAME = 'MATERIAL'                                           661
         AG_PARLIST = 'NONE'                                              661
         CALL AXMATERIAL                                                  661
C    *                                                                    661
C    Geant ATTRIBUTE : TOKA ===> SEEN=1 COLO=KGREEN <===                  661
         CALL AGSRESET                                                    662
         AG_SEEN = 1                                                      662
         AG_COLO = KGREEN                                                 662
         AG_TITLE = 'TOKA'                                                662
         AG_EXNAME = 'ATTRIBUTE'                                          662
         AG_PARLIST = 'SEEN_COLO'                                         662
         CALL AXATTRIBUTE                                                 662
C    *                                                                    662
C    Geant SHAPE : TUBE ===> RMIN=TOKAIR RMAX=TOKAOR DZ=TPCG_DZYF2 <===   662
         CALL AGSRESET                                                    663
         AG_RMIN = TOKAIR                                                 663
         AG_RMAX = TOKAOR                                                 663
         AG_DZ = TPCG_DZYF2                                               663
         AG_TITLE = 'TUBE'                                                663
         AG_EXNAME = 'SHAPE'                                              663
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      663
         CALL AXSHAPE                                                     663
C    *                                                                    663
5231  CALL AGSPOPB                                                        664
      GO TO BLOCK_TOKA, (5230 ,5240)                                      664
C End of Local Procedure                                                  664
C ----------------------------------------------------------------------  665
C ------------ TONX                                                       665
C Begin Local Procedure TONX                                              666
C TONX IS NOMEX SUPPORT                                                   666
5250  CONTINUE                                                            666
         AG_TITLE='TONX'                                                  666
         AG_MODULE=MODULE                                                 666
         AG_MARK='V00'                                                    666
         CALL AGSPUSH                                                     666
         AG_MODULE=MODULE                                                 666
C    *                                                                    666
C    Geant MATERIAL : NOMEX ===> inherited arguments used <===            667
         CALL AGSRESET                                                    667
         AG_TITLE = 'NOMEX'                                               667
         AG_EXNAME = 'MATERIAL'                                           667
         AG_PARLIST = 'NONE'                                              667
         CALL AXMATERIAL                                                  667
C    *                                                                    667
C    Geant ATTRIBUTE : TONX ===> SEEN=1 COLO=KRED <===                    667
         CALL AGSRESET                                                    668
         AG_SEEN = 1                                                      668
         AG_COLO = KRED                                                   668
         AG_TITLE = 'TONX'                                                668
         AG_EXNAME = 'ATTRIBUTE'                                          668
         AG_PARLIST = 'SEEN_COLO'                                         668
         CALL AXATTRIBUTE                                                 668
C    *                                                                    668
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=6 ZI={-TPCG_DZYF2, -TPCG  668
C    _DZYF3,-TPCG_DZYF3,TPCG_DZYF3,TPCG_DZYF3, TPCG_DZYF2} RMN={TONXOR-0  668
C    .2,TONXOR-0.2, TONXIR,TONXIR,TONXOR-0.2,TONXOR-0.2} RMX={TONXOR, TO  668
C    NXOR,TONXOR,TONXOR,TONXOR,TONXOR} <===                               668
         CALL AGSRESET                                                    673
         AG_PHI1 = 0                                                      673
         AG_DPHI = 360                                                    673
         AG_NZ = 6                                                        673
         AG_ZI(1)=-TPCG_DZYF2                                             673
         AG_ZI(2)=-TPCG_DZYF3                                             673
         AG_ZI(3)=-TPCG_DZYF3                                             673
         AG_ZI(4)=TPCG_DZYF3                                              673
         AG_ZI(5)=TPCG_DZYF3                                              673
         AG_ZI(6)=TPCG_DZYF2                                              673
         AG_RMN(1)=TONXOR-0.2                                             673
         AG_RMN(2)=TONXOR-0.2                                             673
         AG_RMN(3)=TONXIR                                                 673
         AG_RMN(4)=TONXIR                                                 673
         AG_RMN(5)=TONXOR-0.2                                             673
         AG_RMN(6)=TONXOR-0.2                                             673
         AG_RMX(1)=TONXOR                                                 673
         AG_RMX(2)=TONXOR                                                 673
         AG_RMX(3)=TONXOR                                                 673
         AG_RMX(4)=TONXOR                                                 673
         AG_RMX(5)=TONXOR                                                 673
         AG_RMX(6)=TONXOR                                                 673
         AG_TITLE = 'PCON'                                                673
         AG_EXNAME = 'SHAPE'                                              673
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           673
         CALL AXSHAPE                                                     673
C    *                                                                    673
5251  CALL AGSPOPB                                                        674
      GO TO BLOCK_TONX, (5250 ,5260)                                      674
C End of Local Procedure                                                  674
C ----------------------------------------------------------------------  675
C ------------ TOAD                                                       675
C Begin Local Procedure TOAD                                              676
C TOAD IS ADHESIVE LAYER                                                  676
5190  CONTINUE                                                            676
         AG_TITLE='TOAD'                                                  676
         AG_MODULE=MODULE                                                 676
         AG_MARK='V00'                                                    676
         CALL AGSPUSH                                                     676
         AG_MODULE=MODULE                                                 676
C    *                                                                    676
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            677
         CALL AGSRESET                                                    677
         AG_TITLE = 'MYLAR'                                               677
         AG_EXNAME = 'MATERIAL'                                           677
         AG_PARLIST = 'NONE'                                              677
         CALL AXMATERIAL                                                  677
C    *                                                                    677
C    Geant ATTRIBUTE : TOAD ===> SEEN=1 COLO=KLIGHTBLUE <===              677
         CALL AGSRESET                                                    678
         AG_SEEN = 1                                                      678
         AG_COLO = KLIGHTBLUE                                             678
         AG_TITLE = 'TOAD'                                                678
         AG_EXNAME = 'ATTRIBUTE'                                          678
         AG_PARLIST = 'SEEN_COLO'                                         678
         CALL AXATTRIBUTE                                                 678
C    *                                                                    678
C    Geant SHAPE : TUBE ===> RMIN=TOADIR RMAX=TOADOR DZ=TPCG_DZYF2 <===   678
         CALL AGSRESET                                                    679
         AG_RMIN = TOADIR                                                 679
         AG_RMAX = TOADOR                                                 679
         AG_DZ = TPCG_DZYF2                                               679
         AG_TITLE = 'TUBE'                                                679
         AG_EXNAME = 'SHAPE'                                              679
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      679
         CALL AXSHAPE                                                     679
C    *                                                                    679
5191  CALL AGSPOPB                                                        680
      GO TO BLOCK_TOAD, (5190 ,5200)                                      680
C End of Local Procedure                                                  680
C ----------------------------------------------------------------------  681
C ------------ TOIG                                                       681
C Begin Local Procedure TOIG                                              682
C TOIG IS INSULATING GAS (NITROGEN)                                       682
5270  CONTINUE                                                            682
         AG_TITLE='TOIG'                                                  682
         AG_MODULE=MODULE                                                 682
         AG_MARK='V00'                                                    682
         CALL AGSPUSH                                                     682
         AG_MODULE=MODULE                                                 682
C    *                                                                    682
C    Geant MATERIAL : NITROGEN_GAS ===> inherited arguments used <===     683
         CALL AGSRESET                                                    683
         AG_TITLE = 'NITROGEN_GAS'                                        683
         AG_EXNAME = 'MATERIAL'                                           683
         AG_PARLIST = 'NONE'                                              683
         CALL AXMATERIAL                                                  683
C    *                                                                    683
C    Geant ATTRIBUTE : TOAD ===> SEEN=1 COLO=KLIGHTBLUE <===              683
         CALL AGSRESET                                                    684
         AG_SEEN = 1                                                      684
         AG_COLO = KLIGHTBLUE                                             684
         AG_TITLE = 'TOAD'                                                684
         AG_EXNAME = 'ATTRIBUTE'                                          684
         AG_PARLIST = 'SEEN_COLO'                                         684
         CALL AXATTRIBUTE                                                 684
C    *                                                                    684
C    Geant SHAPE : TUBE ===> RMIN=TOIGIR RMAX=TOIGOR DZ=TPCG_DZYF2 <===   684
         CALL AGSRESET                                                    685
         AG_RMIN = TOIGIR                                                 685
         AG_RMAX = TOIGOR                                                 685
         AG_DZ = TPCG_DZYF2                                               685
         AG_TITLE = 'TUBE'                                                685
         AG_EXNAME = 'SHAPE'                                              685
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      685
         CALL AXSHAPE                                                     685
C    *                                                                    685
5271  CALL AGSPOPB                                                        686
      GO TO BLOCK_TOIG, (5270 ,5280)                                      686
C End of Local Procedure                                                  686
C ----------------------------------------------------------------------  687
C ------------ TOHA                                                       687
C Begin Local Procedure TOHA                                              688
C TOHA GAS CONTAINMENT VESSEL (AL) + HA                                   688
5290  CONTINUE                                                            688
         AG_TITLE='TOHA'                                                  688
         AG_MODULE=MODULE                                                 688
         AG_MARK='V00'                                                    688
         CALL AGSPUSH                                                     688
         AG_MODULE=MODULE                                                 688
C    *                                                                    688
C    Geant MATERIAL : AL_HONEYCOMB ===> inherited arguments used <===     689
         CALL AGSRESET                                                    689
         AG_TITLE = 'AL_HONEYCOMB'                                        689
         AG_EXNAME = 'MATERIAL'                                           689
         AG_PARLIST = 'NONE'                                              689
         CALL AXMATERIAL                                                  689
C    *                                                                    689
C    Geant ATTRIBUTE : TOHA ===> SEEN=1 COLO=KVIOLET <===                 689
         CALL AGSRESET                                                    690
         AG_SEEN = 1                                                      690
         AG_COLO = KVIOLET                                                690
         AG_TITLE = 'TOHA'                                                690
         AG_EXNAME = 'ATTRIBUTE'                                          690
         AG_PARLIST = 'SEEN_COLO'                                         690
         CALL AXATTRIBUTE                                                 690
C    *                                                                    690
C    Geant SHAPE : TUBE ===> RMIN=TOHAIR RMAX=TOHAOR DZ=TPCG_DZYF3 <===   690
         CALL AGSRESET                                                    691
         AG_RMIN = TOHAIR                                                 691
         AG_RMAX = TOHAOR                                                 691
         AG_DZ = TPCG_DZYF3                                               691
         AG_TITLE = 'TUBE'                                                691
         AG_EXNAME = 'SHAPE'                                              691
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      691
         CALL AXSHAPE                                                     691
C    *                                                                    691
5291  CALL AGSPOPB                                                        692
      GO TO BLOCK_TOHA, (5290 ,5300)                                      692
C End of Local Procedure                                                  692
C ----------------------------------------------------------------------  693
C ------------ TPGV                                                       693
C Begin Local Procedure TPGV                                              694
C TPGV IS THE GAS VOLUME PLACED IN TPC                                    694
5050  CONTINUE                                                            694
         AG_TITLE='TPGV'                                                  694
         AG_MODULE=MODULE                                                 694
         AG_MARK='V00'                                                    694
         CALL AGSPUSH                                                     694
         AG_MODULE=MODULE                                                 694
C    *                                                                    694
C    Geant ATTRIBUTE : TPGV ===> SEEN=1 COLO=KRED <===                    695
         CALL AGSRESET                                                    695
         AG_SEEN = 1                                                      695
         AG_COLO = KRED                                                   695
         AG_TITLE = 'TPGV'                                                695
         AG_EXNAME = 'ATTRIBUTE'                                          695
         AG_PARLIST = 'SEEN_COLO'                                         695
         CALL AXATTRIBUTE                                                 695
C    *                                                                    695
C    Geant MATERIAL : P10 ===> inherited arguments used <===              695
         CALL AGSRESET                                                    696
         AG_TITLE = 'P10'                                                 696
         AG_EXNAME = 'MATERIAL'                                           696
         AG_PARLIST = 'NONE'                                              696
         CALL AXMATERIAL                                                  696
C    *                                                                    696
C    Geant SHAPE : PCON ===> PHI1=-15 DPHI=360 NZ=8 ZI={TPCG_MEMBTHK/2,   696
C    ZWHEEL1,ZWHEEL1,ZTOFCEND,ZTOFCEND, ZTIFCFLANGEBEGIN,ZTIFCFLANGEBEGI  696
C    N,TPCG_DZENVELOP}, RMN={TIFCOR,TIFCOR,TIFCOR,TIFCOR,TIFCOR, TPCG_TI  696
C    FCRF+TPCG_TIFCDRT,TPCG_TIFCRF+TPCG_TIFCDRT, TPCG_TIFCRF+TPCG_TIFCDR  696
C    T} RMX={TOFCIR, TOFCIR,TPCG_WHEELOR1,TPCG_WHEELOR1,TPCG_WHEELOR, TP  696
C    CG_WHEELOR,TPCG_WHEELOR,TPCG_WHEELOR} <===                           696
         CALL AGSRESET                                                    704
         AG_PHI1 = -15                                                    704
         AG_DPHI = 360                                                    704
         AG_NZ = 8                                                        704
         AG_ZI(1)=TPCG_MEMBTHK/2                                          704
         AG_ZI(2)=ZWHEEL1                                                 704
         AG_ZI(3)=ZWHEEL1                                                 704
         AG_ZI(4)=ZTOFCEND                                                704
         AG_ZI(5)=ZTOFCEND                                                704
         AG_ZI(6)=ZTIFCFLANGEBEGIN                                        704
         AG_ZI(7)=ZTIFCFLANGEBEGIN                                        704
         AG_ZI(8)=TPCG_DZENVELOP                                          704
         AG_RMN(1)=TIFCOR                                                 704
         AG_RMN(2)=TIFCOR                                                 704
         AG_RMN(3)=TIFCOR                                                 704
         AG_RMN(4)=TIFCOR                                                 704
         AG_RMN(5)=TIFCOR                                                 704
         AG_RMN(6)=TPCG_TIFCRF+TPCG_TIFCDRT                               704
         AG_RMN(7)=TPCG_TIFCRF+TPCG_TIFCDRT                               704
         AG_RMN(8)=TPCG_TIFCRF+TPCG_TIFCDRT                               704
         AG_RMX(1)=TOFCIR                                                 704
         AG_RMX(2)=TOFCIR                                                 704
         AG_RMX(3)=TPCG_WHEELOR1                                          704
         AG_RMX(4)=TPCG_WHEELOR1                                          704
         AG_RMX(5)=TPCG_WHEELOR                                           704
         AG_RMX(6)=TPCG_WHEELOR                                           704
         AG_RMX(7)=TPCG_WHEELOR                                           704
         AG_RMX(8)=TPCG_WHEELOR                                           704
         AG_TITLE = 'PCON'                                                704
         AG_EXNAME = 'SHAPE'                                              704
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           704
         CALL AXSHAPE                                                     704
C    *                                                                    704
C    *                                                                    704
         ISECANG=15                                                       705
            GOTO 5313                                                     705
5311        ISECANG=ISECANG+(30)                                          705
5313        IF((30)*(ISECANG-(360-15)).GT.0) GO TO 5312                   705
C       Create TPSS :                                                     706
C       Call To Local Procedure TPSS                                      706
            ASSIGN 5330 TO BLOCK_TPSS                                     706
            GO TO  5320                                                   706
5330        CONTINUE                                                      706
C       *                                                                 706
C       Geant POSITION : TPSS ===> ALPHAZ=ISECANG <===                    707
            CALL AGSRESET                                                 707
            AG_ALPHAN=AG_ALPHAN+1                                         707
            AG_ALPHAK(AG_ALPHAN)=3                                        707
            AG_ALPHAL(AG_ALPHAN)=ISECANG                                  707
            AG_TITLE = 'TPSS'                                             707
            AG_EXNAME = 'POSITION'                                        707
            AG_PARLIST = 'ALPHAZ'                                         707
            CALL AXPOSITION                                               707
C       *                                                                 707
         GO TO 5311                                                       708
5312     CONTINUE                                                         708
5051  CALL AGSPOPB                                                        709
      GO TO BLOCK_TPGV, (5050 ,5060)                                      709
C End of Local Procedure                                                  709
C ----------------------------------------------------------------------  710
C ------------ TPSS                                                       710
C Begin Local Procedure TPSS                                              711
C TPSS IS A DIVISION OF GAS VOLUME CORRESPONDING TO A SUPERSECTORS        711
5320  CONTINUE                                                            711
         AG_TITLE='TPSS'                                                  711
         AG_MODULE=MODULE                                                 711
         AG_MARK='V00'                                                    711
         CALL AGSPUSH                                                     711
         AG_MODULE=MODULE                                                 711
C    *                                                                    711
C    Geant ATTRIBUTE : TPSS ===> SEEN=1 COLO=KBLUE <===                   712
         CALL AGSRESET                                                    712
         AG_SEEN = 1                                                      712
         AG_COLO = KBLUE                                                  712
         AG_TITLE = 'TPSS'                                                712
         AG_EXNAME = 'ATTRIBUTE'                                          712
         AG_PARLIST = 'SEEN_COLO'                                         712
         CALL AXATTRIBUTE                                                 712
C    *                                                                    712
C    Geant SHAPE : PCON ===> PHI1=-15 DPHI=30 NZ=8 ZI={0,ZWHEEL1-TPCG_ME  712
C    MBTHK*0.5, ZWHEEL1-TPCG_MEMBTHK*0.5, ZTOFCEND-TPCG_MEMBTHK*0.5,ZTOF  712
C    CEND-TPCG_MEMBTHK*0.5, ZTIFCFLANGEBEGIN-TPCG_MEMBTHK*0.5, ZTIFCFLAN  712
C    GEBEGIN-TPCG_MEMBTHK*0.5,TPCG_DZENVELOP-TPCG_MEMBTHK*0.5}, RMN={TIF  712
C    COR,TIFCOR,TIFCOR, TIFCOR,TIFCOR,TPCG_TIFCRF+TPCG_TIFCDRT,TPCG_TIFC  712
C    RF+TPCG_TIFCDRT, TPCG_TIFCRF+TPCG_TIFCDRT}, RMX={TOFCIR,TOFCIR,TPCG  712
C    _WHEELOR1,TPCG_WHEELOR1, TPCG_WHEELOR,TPCG_WHEELOR,TPCG_WHEELOR,TPC  712
C    G_WHEELOR} <===                                                      712
         CALL AGSRESET                                                    722
         AG_PHI1 = -15                                                    722
         AG_DPHI = 30                                                     722
         AG_NZ = 8                                                        722
         AG_ZI(1)=0                                                       722
         AG_ZI(2)=ZWHEEL1-TPCG_MEMBTHK*0.5                                722
         AG_ZI(3)=ZWHEEL1-TPCG_MEMBTHK*0.5                                722
         AG_ZI(4)=ZTOFCEND-TPCG_MEMBTHK*0.5                               722
         AG_ZI(5)=ZTOFCEND-TPCG_MEMBTHK*0.5                               722
         AG_ZI(6)=ZTIFCFLANGEBEGIN-TPCG_MEMBTHK*0.5                       722
         AG_ZI(7)=ZTIFCFLANGEBEGIN-TPCG_MEMBTHK*0.5                       722
         AG_ZI(8)=TPCG_DZENVELOP-TPCG_MEMBTHK*0.5                         722
         AG_RMN(1)=TIFCOR                                                 722
         AG_RMN(2)=TIFCOR                                                 722
         AG_RMN(3)=TIFCOR                                                 722
         AG_RMN(4)=TIFCOR                                                 722
         AG_RMN(5)=TIFCOR                                                 722
         AG_RMN(6)=TPCG_TIFCRF+TPCG_TIFCDRT                               722
         AG_RMN(7)=TPCG_TIFCRF+TPCG_TIFCDRT                               722
         AG_RMN(8)=TPCG_TIFCRF+TPCG_TIFCDRT                               722
         AG_RMX(1)=TOFCIR                                                 722
         AG_RMX(2)=TOFCIR                                                 722
         AG_RMX(3)=TPCG_WHEELOR1                                          722
         AG_RMX(4)=TPCG_WHEELOR1                                          722
         AG_RMX(5)=TPCG_WHEELOR                                           722
         AG_RMX(6)=TPCG_WHEELOR                                           722
         AG_RMX(7)=TPCG_WHEELOR                                           722
         AG_RMX(8)=TPCG_WHEELOR                                           722
         AG_TITLE = 'PCON'                                                722
         AG_EXNAME = 'SHAPE'                                              722
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           722
         CALL AXSHAPE                                                     722
C    *                                                                    722
C    Create TPMV :                                                        723
C    Call To Local Procedure TPMV                                         723
         ASSIGN 5350 TO BLOCK_TPMV                                        723
         GO TO  5340                                                      723
5350     CONTINUE                                                         723
C    *                                                                    723
C    Geant POSITION : TPMV ===> KONLY='ONLY' <===                         724
         CALL AGSRESET                                                    724
         AG_KONLY = 'ONLY'                                                724
         AG_TITLE = 'TPMV'                                                724
         AG_EXNAME = 'POSITION'                                           724
         AG_PARLIST = 'KONLY'                                             724
         CALL AXPOSITION                                                  724
C    *                                                                    724
C    Create TSAW :                                                        725
C    Call To Local Procedure TSAW                                         725
         ASSIGN 5370 TO BLOCK_TSAW                                        725
         GO TO  5360                                                      725
5370     CONTINUE                                                         725
C    *                                                                    725
C    Geant POSITION : TSAW ===> KONLY='MANY' <===                         726
         CALL AGSRESET                                                    726
         AG_KONLY = 'MANY'                                                726
         AG_TITLE = 'TSAW'                                                726
         AG_EXNAME = 'POSITION'                                           726
         AG_PARLIST = 'KONLY'                                             726
         CALL AXPOSITION                                                  726
C    *                                                                    726
5321  CALL AGSPOPB                                                        727
      GO TO BLOCK_TPSS, (5320 ,5330)                                      727
C End of Local Procedure                                                  727
C ----------------------------------------------------------------------  728
C ------------ TSAW                                                       728
C Begin Local Procedure TSAW                                              729
C TSAW TPCSECTORANDWHEEL                                                  729
5360  CONTINUE                                                            729
         AG_TITLE='TSAW'                                                  729
         AG_MODULE=MODULE                                                 729
         AG_MARK='V00'                                                    729
         CALL AGSPUSH                                                     729
         AG_MODULE=MODULE                                                 729
C    *                                                                    729
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           730
         CALL AGSRESET                                                    730
         AG_TITLE = 'STANDARD'                                            730
         AG_EXNAME = 'MEDIUM'                                             730
         AG_PARLIST = 'NONE'                                              730
         CALL AXMEDIUM                                                    730
C    *                                                                    730
C    Geant ATTRIBUTE : TSAW ===> SEEN=1 COLO=KYELLOW <===                 730
         CALL AGSRESET                                                    731
         AG_SEEN = 1                                                      731
         AG_COLO = KYELLOW                                                731
         AG_TITLE = 'TSAW'                                                731
         AG_EXNAME = 'ATTRIBUTE'                                          731
         AG_PARLIST = 'SEEN_COLO'                                         731
         CALL AXATTRIBUTE                                                 731
C    *                                                                    731
C    Geant SHAPE : PCON ===> PHI1=-15 DPHI=30 NZ=10 ZI={210.90, 211.30,2  731
C    11.30,ZWHEEL1-TPCG_MEMBTHK*0.5,ZWHEEL1-TPCG_MEMBTHK*0.5, ZTOFCEND-T  731
C    PCG_MEMBTHK*0.5, ZTOFCEND-TPCG_MEMBTHK*0.5,ZTIFCFLANGEBEGIN-TPCG_ME  731
C    MBTHK*0.5, ZTIFCFLANGEBEGIN-TPCG_MEMBTHK*0.5, TPCG_DZENVELOP-TPCG_M  731
C    EMBTHK*0.5} RMN={TIFCOR, TIFCOR,TIFCOR,TIFCOR,TIFCOR,TIFCOR,TIFCOR,  731
C     TPCG_TIFCRF+TPCG_TIFCDRT,TPCG_TIFCRF+TPCG_TIFCDRT, TPCG_TIFCRF+TPC  731
C    G_TIFCDRT} RMX={TOFCIR, TOFCIR,TOFCIR,TOFCIR,TPCG_WHEELOR1,TPCG_WHE  731
C    ELOR1, TPCG_WHEELOR,TPCG_WHEELOR,TPCG_WHEELOR, TPCG_WHEELOR} <===    731
         CALL AGSRESET                                                    743
         AG_PHI1 = -15                                                    743
         AG_DPHI = 30                                                     743
         AG_NZ = 10                                                       743
         AG_ZI(1)=210.90                                                  743
         AG_ZI(2)=211.30                                                  743
         AG_ZI(3)=211.30                                                  743
         AG_ZI(4)=ZWHEEL1-TPCG_MEMBTHK*0.5                                743
         AG_ZI(5)=ZWHEEL1-TPCG_MEMBTHK*0.5                                743
         AG_ZI(6)=ZTOFCEND-TPCG_MEMBTHK*0.5                               743
         AG_ZI(7)=ZTOFCEND-TPCG_MEMBTHK*0.5                               743
         AG_ZI(8)=ZTIFCFLANGEBEGIN-TPCG_MEMBTHK*0.5                       743
         AG_ZI(9)=ZTIFCFLANGEBEGIN-TPCG_MEMBTHK*0.5                       743
         AG_ZI(10)=TPCG_DZENVELOP-TPCG_MEMBTHK*0.5                        743
         AG_RMN(1)=TIFCOR                                                 743
         AG_RMN(2)=TIFCOR                                                 743
         AG_RMN(3)=TIFCOR                                                 743
         AG_RMN(4)=TIFCOR                                                 743
         AG_RMN(5)=TIFCOR                                                 743
         AG_RMN(6)=TIFCOR                                                 743
         AG_RMN(7)=TIFCOR                                                 743
         AG_RMN(8)=TPCG_TIFCRF+TPCG_TIFCDRT                               743
         AG_RMN(9)=TPCG_TIFCRF+TPCG_TIFCDRT                               743
         AG_RMN(10)=TPCG_TIFCRF+TPCG_TIFCDRT                              743
         AG_RMX(1)=TOFCIR                                                 743
         AG_RMX(2)=TOFCIR                                                 743
         AG_RMX(3)=TOFCIR                                                 743
         AG_RMX(4)=TOFCIR                                                 743
         AG_RMX(5)=TPCG_WHEELOR1                                          743
         AG_RMX(6)=TPCG_WHEELOR1                                          743
         AG_RMX(7)=TPCG_WHEELOR                                           743
         AG_RMX(8)=TPCG_WHEELOR                                           743
         AG_RMX(9)=TPCG_WHEELOR                                           743
         AG_RMX(10)=TPCG_WHEELOR                                          743
         AG_TITLE = 'PCON'                                                743
         AG_EXNAME = 'SHAPE'                                              743
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           743
         CALL AXSHAPE                                                     743
C    *                                                                    743
         DX2 = 1.470                                                      743
         DX = 0                                                           745
C    use TECW bank                                                        746
         BPATH_TECW='TECW*'                                               746
         IDNUM_TECW(1)=0                                                  746
         TECW_SEC=1                                                       746
         CALL AGDGETP('TPCEGEO4','*',-999,'TECW',NTECW_SEC,RE_TECW_SEC,   746
     *   BPATH_TECW,IDNUM_TECW,1,MAP_TECW,NAMES_TECW,BANK_TECW,LEN_TECW,  746
     *   LLEN_TECW,LINK_TECW,FLAG_TECW)                                   746
C    *                                                                    747
         INOUT=KINNER                                                     747
            GO TO 5383                                                    747
5381        INOUT=INOUT+1                                                 747
5383        IF(INOUT-(KOUTER).GT.0) GO TO 5382                            747
            MYCOUNT = 0                                                   747
C       use COOL bank                                                     749
            BPATH_COOL='COOL*'                                            749
            IDNUM_COOL(1)=0                                               749
            COOL_INOUT=INOUT                                              749
            CALL AGDGETP('TPCEGEO4','*',-999,'COOL',NCOOL_INOUT,          749
     *      RE_COOL_INOUT,BPATH_COOL,IDNUM_COOL,1,MAP_COOL,NAMES_COOL,    749
     *      BANK_COOL,LEN_COOL,LLEN_COOL,LINK_COOL,FLAG_COOL)             749
C       use RIBS bank                                                     750
            BPATH_RIBS='RIBS*'                                            750
            IDNUM_RIBS(1)=0                                               750
            RIBS_INOUT=INOUT                                              750
            CALL AGDGETP('TPCEGEO4','*',-999,'RIBS',NRIBS_INOUT,          750
     *      RE_RIBS_INOUT,BPATH_RIBS,IDNUM_RIBS,1,MAP_RIBS,NAMES_RIBS,    750
     *      BANK_RIBS,LEN_RIBS,LLEN_RIBS,LINK_RIBS,FLAG_RIBS)             750
C       use HOLZ bank                                                     751
            BPATH_HOLZ='HOLZ*'                                            751
            IDNUM_HOLZ(1)=0                                               751
            HOLZ_INOUT=INOUT                                              751
            CALL AGDGETP('TPCEGEO4','*',-999,'HOLZ',NHOLZ_INOUT,          751
     *      RE_HOLZ_INOUT,BPATH_HOLZ,IDNUM_HOLZ,1,MAP_HOLZ,NAMES_HOLZ,    751
     *      BANK_HOLZ,LEN_HOLZ,LLEN_HOLZ,LINK_HOLZ,FLAG_HOLZ)             751
C       use TECW bank                                                     752
            BPATH_TECW='TECW*'                                            752
            IDNUM_TECW(1)=0                                               752
            TECW_SEC=(INOUT+1)                                            752
            CALL AGDGETP('TPCEGEO4','*',-999,'TECW',NTECW_SEC,            752
     *      RE_TECW_SEC,BPATH_TECW,IDNUM_TECW,1,MAP_TECW,NAMES_TECW,      752
     *      BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)             752
            Z2 = ZWHEEL1                                                  753
            Z1 = Z2 - TECW_THICK                                          754
            Z = (Z1 + TPCG_MEMBTHK/2)/2                                   755
            DZ = (Z1 - TPCG_MEMBTHK/2)/2                                  756
            X1 = -TECW_HEIGHT/2                                           757
            X2 = TECW_HEIGHT/2                                            758
            Y1 = TECW_INWIDTH/2                                           759
            Y2 = TECW_OUWIDTH/2                                           760
            X0 = (Y1*X2-Y2*X1)/(X2-X1)                                    761
            DYDX = (Y2 - Y1)/(X2 - X1)                                    762
            ALPHA = 180./PI*ATAN(DYDX)                                    763
            R1 = Y1/DYDX                                                  764
            R2 = Y2/DYDX                                                  765
            DRSECTORSHIFT = TECW_RMIN - R1                                766
C       Check INOUT==KINNER                                               767
            IF (INOUT.EQ.KINNER) THEN                                     767
            XHOLES(0) = R2 - 1.988*INCH                                   768
C       *                                                                 769
            I=1                                                           769
               GO TO 5393                                                 769
5391           I=I+1                                                      769
5393           IF(I-(HOLZ_NUMBEROFROWS-1).GT.0) GO TO 5392                769
               XHOLES(I) = XHOLES(I-1) - 1.575*INCH                       770
            GO TO 5391                                                    771
5392        CONTINUE                                                      771
            ELSE                                                          772
C       *                                                                 773
            I=0                                                           773
               GO TO 5403                                                 773
5401           I=I+1                                                      773
5403           IF(I-(HOLZ_NUMBEROFROWS - 1).GT.0) GO TO 5402              773
               XHOLES(I) = R2 - (1.988 + 1.575*I)*INCH                    774
            GO TO 5401                                                    775
5402        CONTINUE                                                      775
            ENDIF                                                         776
C       Create TSAS :                                                     777
C       Call To Local Procedure TSAS                                      777
            ASSIGN 5420 TO BLOCK_TSAS                                     777
            GO TO  5410                                                   777
5420        CONTINUE                                                      777
C       *                                                                 777
C       Geant POSITION : TSAS ===> Z=TSAS_ZOFFSET KONLY='MANY' <===       778
            CALL AGSRESET                                                 778
            AG_Z = TSAS_ZOFFSET                                           778
            AG_KONLY = 'MANY'                                             778
            AG_TITLE = 'TSAS'                                             778
            AG_EXNAME = 'POSITION'                                        778
            AG_PARLIST = 'Z_KONLY'                                        778
            CALL AXPOSITION                                               778
C       *                                                                 778
C       Create TWAS :                                                     779
C       Call To Local Procedure TWAS                                      779
            ASSIGN 5440 TO BLOCK_TWAS                                     779
            GO TO  5430                                                   779
5440        CONTINUE                                                      779
C       *                                                                 779
C       Geant POSITION : TWAS ===> KONLY='MANY' <===                      780
            CALL AGSRESET                                                 780
            AG_KONLY = 'MANY'                                             780
            AG_TITLE = 'TWAS'                                             780
            AG_EXNAME = 'POSITION'                                        780
            AG_PARLIST = 'KONLY'                                          780
            CALL AXPOSITION                                               780
C       *                                                                 780
C       Check INOUT==0                                                    780
            IF (INOUT.EQ.0) THEN                                          781
C       Create TBRW :                                                     782
C       Call To Local Procedure TBRW                                      782
            ASSIGN 5460 TO BLOCK_TBRW                                     782
            GO TO  5450                                                   782
5460        CONTINUE                                                      782
C       *                                                                 782
C       Geant POSITION : TBRW ===> X=TPCG_WHEELR1 Z=ZWHEEL1+TPCG_WHEELTH  783
C       K/2 <===                                                          783
            CALL AGSRESET                                                 783
            AG_X = TPCG_WHEELR1                                           783
            AG_Z = ZWHEEL1+TPCG_WHEELTHK/2                                783
            AG_TITLE = 'TBRW'                                             783
            AG_EXNAME = 'POSITION'                                        783
            AG_PARLIST = 'X_Z'                                            783
            CALL AXPOSITION                                               783
C       *                                                                 783
            ENDIF                                                         783
            X1 = TPCG_WHEELR0*COS15                                       785
            X2 = TPCG_WHEELR2                                             786
            DZ = TPCG_WHEELRIBHEIGHT/2                                    787
            Z = ZWHEEL1 + DZ                                              788
            DX = (X2 - X1)/2                                              789
            DY = TPCG_WHEELTOTALRIBWIDTH/4                                790
            X = (X1 + X2)/2                                               791
            Y = (X *TAN15 - DY)*(2*INOUT-1)                               792
            MYDX=DY                                                       792
            MYDY=DX                                                       792
            MYDZ=DZ                                                       792
            MYALPH=-(1-2*INOUT)*15                                        793
C       Create TWRB :                                                     794
C       Call To Local Procedure TWRB                                      794
            ASSIGN 5480 TO BLOCK_TWRB                                     794
            GO TO  5470                                                   794
5480        CONTINUE                                                      794
C       *                                                                 794
C       Geant POSITION : TWRB ===> X=X Y=Y Z=Z ORT=YX-Z KONLY='MANY' <==  795
C       =                                                                 795
            CALL AGSRESET                                                 795
            AG_X = X                                                      795
            AG_Y = Y                                                      795
            AG_Z = Z                                                      795
            AG_ALPHAN=AG_ALPHAN+1                                         795
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             795
            AG_KONLY = 'MANY'                                             795
            AG_TITLE = 'TWRB'                                             795
            AG_EXNAME = 'POSITION'                                        795
            AG_PARLIST = 'X_Y_Z_ORT_KONLY'                                795
            CALL AXPOSITION                                               795
C       *                                                                 795
            DZ = TPCG_WHEELTHK/2                                          795
            Z = ZWHEEL1 + DZ                                              797
            DY = TPCG_WHEELRIBWIDTH/4                                     798
            X = (X1 + X2)/2                                               799
            Y = (X *TAN15 - DY)*(2*INOUT-1)                               800
            MYDX=DY                                                       800
            MYDY=DX                                                       800
            MYDZ=DZ                                                       800
            MYALPH=-(1-2*INOUT)*15                                        801
C       Create TWRB :                                                     802
C       Call To Local Procedure TWRB                                      802
            ASSIGN 5490 TO BLOCK_TWRB                                     802
            GO TO  5470                                                   802
5490        CONTINUE                                                      802
C       *                                                                 802
C       Geant POSITION : TWRB ===> X=X Y=Y Z=Z ORT=YX-Z KONLY='MANY' <==  803
C       =                                                                 803
            CALL AGSRESET                                                 803
            AG_X = X                                                      803
            AG_Y = Y                                                      803
            AG_Z = Z                                                      803
            AG_ALPHAN=AG_ALPHAN+1                                         803
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             803
            AG_KONLY = 'MANY'                                             803
            AG_TITLE = 'TWRB'                                             803
            AG_EXNAME = 'POSITION'                                        803
            AG_PARLIST = 'X_Y_Z_ORT_KONLY'                                803
            CALL AXPOSITION                                               803
C       *                                                                 803
         GO TO 5381                                                       804
5382     CONTINUE                                                         804
C    *                                                                    805
         ALPHA=-15                                                        805
            GOTO 5503                                                     805
5501        ALPHA=ALPHA+(30)                                              805
5503        IF((30)*(ALPHA-(15)).GT.0) GO TO 5502                         805
C       *                                                                 806
            J=0                                                           806
               GO TO 5513                                                 806
5511           J=J+1                                                      806
5513           IF(J-(3).GT.0) GO TO 5512                                  806
C          Check (J==0)                                                   807
               IF ((J.EQ.0)) THEN                                         807
               R = TPCG_WHEELR2 - 1.5/8.5*(TPCG_WHEELR2 - TPCG_WHEELR1)   808
               ENDIF                                                      809
C          Check (J==1)                                                   810
               IF ((J.EQ.1)) THEN                                         810
               R = TPCG_WHEELR2 - 6.5/8.5*(TPCG_WHEELR2 - TPCG_WHEELR1)   811
               ENDIF                                                      812
C          Check (J==2)                                                   813
               IF ((J.EQ.2)) THEN                                         813
               R = TPCG_WHEELR1 - 2.0/8.5*(TPCG_WHEELR1 - TPCG_WHEELR0)   814
               ENDIF                                                      815
C          Check (J==3)                                                   816
               IF ((J.EQ.3)) THEN                                         816
               R = TPCG_WHEELR1 - 7.0/8.5*(TPCG_WHEELR1 - TPCG_WHEELR0)   817
               ENDIF                                                      818
               X = R                                                      819
               Y = R *TAND(ALPHA)                                         820
C          Geant POSITION : TBRW ===> X=X Y=Y Z=ZWHEEL1+TPCG_WHEELTHK/2   821
C          ALPHAZ=ALPHA <===                                              821
               CALL AGSRESET                                              821
               AG_X = X                                                   821
               AG_Y = Y                                                   821
               AG_Z = ZWHEEL1+TPCG_WHEELTHK/2                             821
               AG_ALPHAN=AG_ALPHAN+1                                      821
               AG_ALPHAK(AG_ALPHAN)=3                                     821
               AG_ALPHAL(AG_ALPHAN)=ALPHA                                 821
               AG_TITLE = 'TBRW'                                          821
               AG_EXNAME = 'POSITION'                                     821
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                821
               CALL AXPOSITION                                            821
C          *                                                              821
               MYCOUNT=MYCOUNT+1                                          821
            GO TO 5511                                                    823
5512        CONTINUE                                                      823
            QWE(1) = -QWE(1)                                              824
            QWE(2) = -QWE(2)                                              825
         GO TO 5501                                                       826
5502     CONTINUE                                                         826
C    Create TWBT :                                                        827
C    Call To Local Procedure TWBT                                         827
         ASSIGN 5530 TO BLOCK_TWBT                                        827
         GO TO  5520                                                      827
5530     CONTINUE                                                         827
C    *                                                                    827
C    Geant POSITION : TWBT ===> inherited arguments used <===             828
         CALL AGSRESET                                                    828
         AG_TITLE = 'TWBT'                                                828
         AG_EXNAME = 'POSITION'                                           828
         AG_PARLIST = 'NONE'                                              828
         CALL AXPOSITION                                                  828
C    *                                                                    828
C    Create TWRI :                                                        829
C    Call To Local Procedure TWRI                                         829
         ASSIGN 5550 TO BLOCK_TWRI                                        829
         GO TO  5540                                                      829
5550     CONTINUE                                                         829
C    *                                                                    829
C    Geant POSITION : TWRI ===> KONLY='MANY' <===                         830
         CALL AGSRESET                                                    830
         AG_KONLY = 'MANY'                                                830
         AG_TITLE = 'TWRI'                                                830
         AG_EXNAME = 'POSITION'                                           830
         AG_PARLIST = 'KONLY'                                             830
         CALL AXPOSITION                                                  830
C    *                                                                    830
C    Create TWTR :                                                        831
C    Call To Local Procedure TWTR                                         831
         ASSIGN 5570 TO BLOCK_TWTR                                        831
         GO TO  5560                                                      831
5570     CONTINUE                                                         831
C    *                                                                    831
C    Geant POSITION : TWTR ===> inherited arguments used <===             832
         CALL AGSRESET                                                    832
         AG_TITLE = 'TWTR'                                                832
         AG_EXNAME = 'POSITION'                                           832
         AG_PARLIST = 'NONE'                                              832
         CALL AXPOSITION                                                  832
C    *                                                                    832
C    Create TWMR :                                                        833
C    Call To Local Procedure TWMR                                         833
         ASSIGN 5590 TO BLOCK_TWMR                                        833
         GO TO  5580                                                      833
5590     CONTINUE                                                         833
C    *                                                                    833
C    Geant POSITION : TWMR ===> KONLY='MANY' <===                         834
         CALL AGSRESET                                                    834
         AG_KONLY = 'MANY'                                                834
         AG_TITLE = 'TWMR'                                                834
         AG_EXNAME = 'POSITION'                                           834
         AG_PARLIST = 'KONLY'                                             834
         CALL AXPOSITION                                                  834
C    *                                                                    834
C    Create TRDO :                                                        835
C    Call To Local Procedure TRDO                                         835
         ASSIGN 5610 TO BLOCK_TRDO                                        835
         GO TO  5600                                                      835
5610     CONTINUE                                                         835
C    *                                                                    835
C    Geant POSITION : TRDO ===> KONLY='MANY' <===                         836
         CALL AGSRESET                                                    836
         AG_KONLY = 'MANY'                                                836
         AG_TITLE = 'TRDO'                                                836
         AG_EXNAME = 'POSITION'                                           836
         AG_PARLIST = 'KONLY'                                             836
         CALL AXPOSITION                                                  836
C    *                                                                    836
         RCOOLINGTUBE = TPCG_WHEELR2 - TPCG_HEIGTUBE - 2*TPCG_DXRDO       836
C    *                                                                    838
         ICOO=0                                                           838
            GO TO 5623                                                    838
5621        ICOO=ICOO+1                                                   838
5623        IF(ICOO-(7).GT.0) GO TO 5622                                  838
            DZ = TPCG_HEIGTUBE*CM/2                                       839
            DY = TPCG_WIDTUBE *CM/2                                       840
            Z = RCOOLINGTUBE                                              841
            DX1 = (Z-DZ)*TAN15 - 2*TPCG_RDOCOOLINGDY                      842
            DX2 = (Z+DZ)*TAN15 - 2*TPCG_RDOCOOLINGDY                      843
            ISHAPE = KTRD1                                                843
            MYPAR(1)=DX1                                                  845
            MYPAR(2)=DX2                                                  845
            MYPAR(3)=DY                                                   845
            MYPAR(4)=DZ                                                   845
C       Create TCOO :                                                     846
C       Call To Local Procedure TCOO                                      846
            ASSIGN 5640 TO BLOCK_TCOO                                     846
            GO TO  5630                                                   846
5640        CONTINUE                                                      846
C       *                                                                 846
C       Geant POSITION : TCOO ===> X=Z Z=(TPCG_ZRDO-2*DY) ORT=YZX <===    847
            CALL AGSRESET                                                 847
            AG_X = Z                                                      847
            AG_Z = (TPCG_ZRDO-2*DY)                                       847
            AG_ALPHAN=AG_ALPHAN+1                                         847
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              847
            AG_TITLE = 'TCOO'                                             847
            AG_EXNAME = 'POSITION'                                        847
            AG_PARLIST = 'X_Z_ORT'                                        847
            CALL AXPOSITION                                               847
C       *                                                                 847
            RCOOLINGTUBE=RCOOLINGTUBE-DRDOCOOLING(ICOO)                   847
         GO TO 5621                                                       849
5622     CONTINUE                                                         849
         DZ = 4.5                                                         850
         DY = 4.5/2                                                       851
         Z = TPCG_ZRDO + TPCG_DZRDO + DZ                                  852
         X1 = TPCG_WHEELR0                                                853
         X2 = TPCG_WHEELR2                                                854
         DX =(X2 - X1)/2                                                  855
         X =(X1 + X2)/2                                                   856
C    *                                                                    857
         ICAB=0                                                           857
            GO TO 5653                                                    857
5651        ICAB=ICAB+1                                                   857
5653        IF(ICAB-(1).GT.0) GO TO 5652                                  857
            Y = - (X *TAN15 - DY) * (1 - 2*ICAB)                          858
            MYPAR(1)=DY                                                   859
            MYPAR(2)=DX                                                   859
            MYPAR(3)=DZ                                                   859
            MYPAR(4)=-15*(1-2*ICAB)                                       859
            MYPAR(5)=0                                                    859
            MYPAR(6)=0                                                    859
C       Create TCAB :                                                     860
C       Call To Local Procedure TCAB                                      860
            ASSIGN 5670 TO BLOCK_TCAB                                     860
            GO TO  5660                                                   860
5670        CONTINUE                                                      860
C       *                                                                 860
C       Geant POSITION : TCAB ===> X=X Y=Y Z=Z ORT=YX-Z <===              861
            CALL AGSRESET                                                 861
            AG_X = X                                                      861
            AG_Y = Y                                                      861
            AG_Z = Z                                                      861
            AG_ALPHAN=AG_ALPHAN+1                                         861
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             861
            AG_TITLE = 'TCAB'                                             861
            AG_EXNAME = 'POSITION'                                        861
            AG_PARLIST = 'X_Y_Z_ORT'                                      861
            CALL AXPOSITION                                               861
C       *                                                                 861
         GO TO 5651                                                       862
5652     CONTINUE                                                         862
5361  CALL AGSPOPB                                                        863
      GO TO BLOCK_TSAW, (5360 ,5370)                                      863
C End of Local Procedure                                                  863
C ----------------------------------------------------------------------  864
C ------------ TSAS                                                       864
C Begin Local Procedure TSAS                                              865
C TSAS TPCINNERSECTORASSEMBLY & TPCOUTERSECTORASSEMBLY TSAS AND TSA1      865
5410  CONTINUE                                                            865
         AG_TITLE='TSAS'                                                  865
         AG_MODULE=MODULE                                                 865
         AG_MARK='V00'                                                    865
         CALL AGSPUSH                                                     865
         AG_MODULE=MODULE                                                 865
C    *                                                                    865
C    Geant SHAPE : PGON ===> PHI1=-15 DPHI=30 NPDIV=1 NZ=2, ZI={ZZZSA(1)  868
C    ,ZZZSA(2)} RMN={RMNSA(INOUT),RMNSA(INOUT)}, RMX={RMXSA(INOUT),RMXSA  868
C    (INOUT)} <===                                                        868
         CALL AGSRESET                                                    868
         AG_PHI1 = -15                                                    868
         AG_DPHI = 30                                                     868
         AG_NPDIV = 1                                                     868
         AG_NZ = 2                                                        868
         AG_ZI(1)=ZZZSA(1)                                                868
         AG_ZI(2)=ZZZSA(2)                                                868
         AG_RMN(1)=RMNSA(INOUT)                                           868
         AG_RMN(2)=RMNSA(INOUT)                                           868
         AG_RMX(1)=RMXSA(INOUT)                                           868
         AG_RMX(2)=RMXSA(INOUT)                                           868
         AG_TITLE = 'PGON'                                                868
         AG_EXNAME = 'SHAPE'                                              868
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     868
         CALL AXSHAPE                                                     868
C    *                                                                    868
C    Geant ATTRIBUTE : TSAS ===> SEEN=0 <===                              868
         CALL AGSRESET                                                    869
         AG_SEEN = 0                                                      869
         AG_TITLE = 'TSAS'                                                869
         AG_EXNAME = 'ATTRIBUTE'                                          869
         AG_PARLIST = 'SEEN'                                              869
         CALL AXATTRIBUTE                                                 869
C    *                                                                    869
C    Create TALS :                                                        870
C    Call To Local Procedure TALS                                         870
         ASSIGN 5690 TO BLOCK_TALS                                        870
         GO TO  5680                                                      870
5690     CONTINUE                                                         870
C    *                                                                    870
C    Geant POSITION : TALS ===> inherited arguments used <===             871
         CALL AGSRESET                                                    871
         AG_TITLE = 'TALS'                                                871
         AG_EXNAME = 'POSITION'                                           871
         AG_PARLIST = 'NONE'                                              871
         CALL AXPOSITION                                                  871
C    *                                                                    871
C    Create TSGT :                                                        872
C    Call To Local Procedure TSGT                                         872
         ASSIGN 5710 TO BLOCK_TSGT                                        872
         GO TO  5700                                                      872
5710     CONTINUE                                                         872
C    *                                                                    872
C    Geant POSITION : TSGT ===> MOTHER='TPMV' Z=TSGT_ZOFFSET <===         873
         CALL AGSRESET                                                    873
         AG_MOTHER = 'TPMV'                                               873
         AG_Z = TSGT_ZOFFSET                                              873
         AG_TITLE = 'TSGT'                                                873
         AG_EXNAME = 'POSITION'                                           873
         AG_PARLIST = 'MOTHER_Z'                                          873
         CALL AXPOSITION                                                  873
C    *                                                                    873
         DY =(R2 - R1)/2                                                  873
         Y =(R2 + R1)/2                                                   875
         DX = TECW_WIDTHRIB/2                                             876
         DZ =(TECW_THICK - TPCG_PADPLANETHICKNESS - TECW_THICKAL)/2       877
         Z = Z2 - DZ                                                      878
         DXW = TPCG_WIREMOUNTWIDTH/COS15/2                                879
         DZW = TPCG_WIREMOUNTHEIGHT/2                                     880
         XW = 0                                                           881
C    *                                                                    882
         IRIB=0                                                           882
            GO TO 5723                                                    882
5721        IRIB=IRIB+1                                                   882
5723        IF(IRIB-(1).GT.0) GO TO 5722                                  882
            ISHAPE = KPARA                                                882
            MYPAR(1)=DX                                                   884
            MYPAR(2)=DY                                                   884
            MYPAR(3)=DZ                                                   884
            MYPAR(4)=-(1-2*IRIB)*ALPHA                                    884
C       Create TRIB :                                                     885
C       Call To Local Procedure TRIB                                      885
            ASSIGN 5740 TO BLOCK_TRIB                                     885
            GO TO  5730                                                   885
5740        CONTINUE                                                      885
C       *                                                                 885
            X = ((Y1 + Y2)/2 - DX)*(1-2*IRIB)                             886
C       Geant POSITION : TRIB ===> X=Y Y=-X Z=Z ORT=YX-Z <===             887
            CALL AGSRESET                                                 887
            AG_X = Y                                                      887
            AG_Y = -X                                                     887
            AG_Z = Z                                                      887
            AG_ALPHAN=AG_ALPHAN+1                                         887
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             887
            AG_TITLE = 'TRIB'                                             887
            AG_EXNAME = 'POSITION'                                        887
            AG_PARLIST = 'X_Y_Z_ORT'                                      887
            CALL AXPOSITION                                               887
C       *                                                                 887
            XW = ((Y1 + Y2)/2 - DXW)*(1-2*IRIB)                           887
            MYPAR(1)=DXW                                                  889
            MYPAR(2)=DY                                                   889
            MYPAR(3)=DZW                                                  889
            MYPAR(4)=-(1-2*IRIB)*ALPHA                                    889
            MYPAR(5)=0                                                    889
            MYPAR(6)=0                                                    889
C       Create TWIR :                                                     890
C       Call To Local Procedure TWIR                                      890
            ASSIGN 5760 TO BLOCK_TWIR                                     890
            GO TO  5750                                                   890
5760        CONTINUE                                                      890
C       *                                                                 890
            ZW = Z1 - DZW/2                                               891
C       Geant POSITION : TWIR ===> MOTHER='TPMV' X=Y Y=-XW Z=ZW ORT=YX-Z  892
C        KONLY='MANY' <===                                                892
            CALL AGSRESET                                                 892
            AG_MOTHER = 'TPMV'                                            892
            AG_X = Y                                                      892
            AG_Y = -XW                                                    892
            AG_Z = ZW                                                     892
            AG_ALPHAN=AG_ALPHAN+1                                         892
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             892
            AG_KONLY = 'MANY'                                             892
            AG_TITLE = 'TWIR'                                             892
            AG_EXNAME = 'POSITION'                                        892
            AG_PARLIST = 'MOTHER_X_Y_Z_ORT_KONLY'                         892
            CALL AXPOSITION                                               892
C       *                                                                 892
         GO TO 5721                                                       893
5722     CONTINUE                                                         893
C    *                                                                    894
         IRIB=0                                                           894
            GO TO 5773                                                    894
5771        IRIB=IRIB+1                                                   894
5773        IF(IRIB-(TECW_NORIBS-1).GT.0) GO TO 5772                      894
            R = R2 - RIBS_X (IRIB+1) * INCH                               895
            Y = RIBS_Y (IRIB+1) * INCH                                    896
            DR = RIBS_DX (IRIB+1) * INCH                                  897
            DY = RIBS_DY (IRIB+1) * INCH                                  898
C       Check DY<1.E-7                                                    899
            IF (DY.LT.1.E-7) THEN                                         899
            DX2 = R *DYDX - TECW_WIDTHRIB - 2*DXW                         899
C       TECW_WIDTHLIP                                                     900
            DX1 = (R - DR)*DYDX - TECW_WIDTHRIB - 2*DXW                   900
C       TECW_WIDTHLIP                                                     901
            XC = R - TECW_ZSTEPRIB/2                                      902
            ISHAPE = KTRD1                                                902
            MYPAR(1)=DX1                                                  904
            MYPAR(2)=DX2                                                  904
            MYPAR(3)=DZ                                                   904
            MYPAR(4)=DR/2                                                 904
C       Create TRIB :                                                     905
C       Call To Local Procedure TRIB                                      905
            ASSIGN 5780 TO BLOCK_TRIB                                     905
            GO TO  5730                                                   905
5780        CONTINUE                                                      905
C       *                                                                 905
C       Geant POSITION : TRIB ===> X=R-DR/2 Y=0 Z=Z2-DZ ORT=YZX <===      906
            CALL AGSRESET                                                 906
            AG_X = R-DR/2                                                 906
            AG_Y = 0                                                      906
            AG_Z = Z2-DZ                                                  906
            AG_ALPHAN=AG_ALPHAN+1                                         906
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              906
            AG_TITLE = 'TRIB'                                             906
            AG_EXNAME = 'POSITION'                                        906
            AG_PARLIST = 'X_Y_Z_ORT'                                      906
            CALL AXPOSITION                                               906
C       *                                                                 906
            ELSE                                                          906
            ISHAPE = KBBOX                                                907
            MYPAR(1)=DR/2                                                 909
            MYPAR(2)=DY/2                                                 909
            MYPAR(3)=DZ                                                   909
C       Create TRIB :                                                     910
C       Call To Local Procedure TRIB                                      910
            ASSIGN 5790 TO BLOCK_TRIB                                     910
            GO TO  5730                                                   910
5790        CONTINUE                                                      910
C       *                                                                 910
C       Geant POSITION : TRIB ===> X=R Y=Y Z=Z2-DZ <===                   911
            CALL AGSRESET                                                 911
            AG_X = R                                                      911
            AG_Y = Y                                                      911
            AG_Z = Z2-DZ                                                  911
            AG_TITLE = 'TRIB'                                             911
            AG_EXNAME = 'POSITION'                                        911
            AG_PARLIST = 'X_Y_Z'                                          911
            CALL AXPOSITION                                               911
C       *                                                                 911
            ENDIF                                                         911
         GO TO 5771                                                       913
5772     CONTINUE                                                         913
5411  CALL AGSPOPB                                                        914
      GO TO BLOCK_TSAS, (5410 ,5420)                                      914
C End of Local Procedure                                                  914
C ----------------------------------------------------------------------  915
C ------------ TWAS                                                       915
C Begin Local Procedure TWAS                                              916
C TWAS TPCWHEELINNERASSEMBLY & TPCWHEELOUTERASSEMBLY TWAS AND TWA1        916
5430  CONTINUE                                                            916
         AG_TITLE='TWAS'                                                  916
         AG_MODULE=MODULE                                                 916
         AG_MARK='V00'                                                    916
         CALL AGSPUSH                                                     916
         AG_MODULE=MODULE                                                 916
C    *                                                                    916
C    Geant ATTRIBUTE : TWAS ===> SEEN=0 <===                              917
         CALL AGSRESET                                                    917
         AG_SEEN = 0                                                      917
         AG_TITLE = 'TWAS'                                                917
         AG_EXNAME = 'ATTRIBUTE'                                          917
         AG_PARLIST = 'SEEN'                                              917
         CALL AXATTRIBUTE                                                 917
C    *                                                                    917
C    Geant SHAPE : PGON ===> PHI1=-15 DPHI=30 NPDIV=1 NZ=2, ZI={ZZZWA(1)  917
C    ,ZZZWA(2)} RMN={RMNWA(INOUT),RMNWA(INOUT)}, RMX={RMXWA(INOUT),RMXWA  917
C    (INOUT)} <===                                                        917
         CALL AGSRESET                                                    920
         AG_PHI1 = -15                                                    920
         AG_DPHI = 30                                                     920
         AG_NPDIV = 1                                                     920
         AG_NZ = 2                                                        920
         AG_ZI(1)=ZZZWA(1)                                                920
         AG_ZI(2)=ZZZWA(2)                                                920
         AG_RMN(1)=RMNWA(INOUT)                                           920
         AG_RMN(2)=RMNWA(INOUT)                                           920
         AG_RMX(1)=RMXWA(INOUT)                                           920
         AG_RMX(2)=RMXWA(INOUT)                                           920
         AG_TITLE = 'PGON'                                                920
         AG_EXNAME = 'SHAPE'                                              920
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     920
         CALL AXSHAPE                                                     920
C    *                                                                    920
         DX = TPCG_WIDTUBE*CM/2                                           920
         DZ = DEPTHIOF(INOUT + 1)/2                                       922
         DY = HEIGHTIOF(INOUT + 1)/2                                      923
         Y1 = - DY                                                        924
         Y2 = DY                                                          925
         X1 = WIDTH1IOF(INOUT + 1)/2 - DX                                 926
         X2 = WIDTH2IOF(INOUT + 1)/2 - DX                                 927
         X0 =(Y1*X2 - Y2*X1)/(Y2 - Y1)                                    928
         DYDX =(X2 - X1)/(Y2 - Y1)                                        929
         BETA = 180./PI*ATAN(DYDX)                                        930
C    Check (INOUT==KOUTER)                                                931
         IF ((INOUT.EQ.KOUTER)) THEN                                      931
         DY=DY-0.7                                                        932
         ENDIF                                                            933
         Y = XHOLES(0) - DY                                               934
         X =(X1 + X2)/2                                                   935
C    *                                                                    936
         ICOO=0                                                           936
            GO TO 5803                                                    936
5801        ICOO=ICOO+1                                                   936
5803        IF(ICOO-(1).GT.0) GO TO 5802                                  936
            MYPAR(1)=DX                                                   937
            MYPAR(2)=DY                                                   937
            MYPAR(3)=DZ                                                   937
            MYPAR(4)=-(1-2*ICOO)*BETA                                     937
            MYPAR(5)=0                                                    937
            MYPAR(6)=0                                                    937
            ISHAPE = KPARA                                                937
C       Create TCOO :                                                     939
C       Call To Local Procedure TCOO                                      939
            ASSIGN 5810 TO BLOCK_TCOO                                     939
            GO TO  5630                                                   939
5810        CONTINUE                                                      939
C       *                                                                 939
C       Geant POSITION : TCOO ===> X=Y Y=X*(2*ICOO-1) Z=ZWHEEL1+DZ ORT=Y  940
C       X-Z <===                                                          940
            CALL AGSRESET                                                 940
            AG_X = Y                                                      940
            AG_Y = X*(2*ICOO-1)                                           940
            AG_Z = ZWHEEL1+DZ                                             940
            AG_ALPHAN=AG_ALPHAN+1                                         940
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             940
            AG_TITLE = 'TCOO'                                             940
            AG_EXNAME = 'POSITION'                                        940
            AG_PARLIST = 'X_Y_Z_ORT'                                      940
            CALL AXPOSITION                                               940
C       *                                                                 940
         GO TO 5801                                                       941
5802     CONTINUE                                                         941
C    *                                                                    942
         ICOO=0                                                           942
            GO TO 5823                                                    942
5821        ICOO=ICOO+1                                                   942
5823        IF(ICOO-(HOLZ_NUMBEROFROWS - 1).GT.0) GO TO 5822              942
            X = XHOLES(ICOO)                                              942
C       // CM                                                             943
            DX = TPCG_HEIGTUBE*CM/2                                       944
            DZ = TPCG_WIDTUBE*CM/2                                        945
            DY = COOL_TUBELENGTH( ICOO+1 )*INCH / 2                       946
C       Check INOUT==KINNER                                               947
            IF (INOUT.EQ.KINNER) THEN                                     947
            DY2 = DY + 2*DYDX*DX                                          948
            DY1 = DY                                                      948
            ELSE                                                          950
            DY2 = DY                                                      951
            DY1 = DY - 2*DYDX*DX                                          952
            ENDIF                                                         953
            ISHAPE = KTRD1                                                953
            MYPAR(1)=DY1                                                  955
            MYPAR(2)=DY2                                                  955
            MYPAR(3)=DZ                                                   955
            MYPAR(4)=DX                                                   955
C       Create TCOO :                                                     956
C       Call To Local Procedure TCOO                                      956
            ASSIGN 5830 TO BLOCK_TCOO                                     956
            GO TO  5630                                                   956
5830        CONTINUE                                                      956
C       *                                                                 956
C       Geant POSITION : TCOO ===> X=DRSECTORSHIFT+X-DX-TFEE_ASSEMBLYTHI  958
C       CKNESS/2 Z=ZWHEEL1+DZ ORT=YZX <===                                958
            CALL AGSRESET                                                 958
            AG_X = DRSECTORSHIFT+X-DX-TFEE_ASSEMBLYTHICKNESS/2            958
            AG_Z = ZWHEEL1+DZ                                             958
            AG_ALPHAN=AG_ALPHAN+1                                         958
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              958
            AG_TITLE = 'TCOO'                                             958
            AG_EXNAME = 'POSITION'                                        958
            AG_PARLIST = 'X_Z_ORT'                                        958
            CALL AXPOSITION                                               958
C       *                                                                 958
C       *                                                                 958
            JCOO=0                                                        959
               GO TO 5843                                                 959
5841           JCOO=JCOO+1                                                959
5843           IF(JCOO-(HOLZ_NUMBERPERROW(ICOO+1) - 1).GT.0) GO TO 5842   959
               Y = HOLZ_YHOLES( 9*ICOO + JCOO + 1 )*INCH                  960
C          Create FEEA :                                                  961
C          Call To Local Procedure FEEA                                   961
               ASSIGN 5860 TO BLOCK_FEEA                                  961
               GO TO  5850                                                961
5860           CONTINUE                                                   961
C          *                                                              961
C          Geant POSITION : FEEA ===> X=X+TFEE_ASSEMBLYTHICKNESS/2+DRSEC  963
C          TORSHIFT Y=Y Z=ZWHEEL1+2*DZ+TFEE_RIBDZ KONLY='MANY' <===       963
               CALL AGSRESET                                              963
               AG_X = X+TFEE_ASSEMBLYTHICKNESS/2+DRSECTORSHIFT            963
               AG_Y = Y                                                   963
               AG_Z = ZWHEEL1+2*DZ+TFEE_RIBDZ                             963
               AG_KONLY = 'MANY'                                          963
               AG_TITLE = 'FEEA'                                          963
               AG_EXNAME = 'POSITION'                                     963
               AG_PARLIST = 'X_Y_Z_KONLY'                                 963
               CALL AXPOSITION                                            963
C          *                                                              963
            GO TO 5841                                                    964
5842        CONTINUE                                                      964
         GO TO 5821                                                       965
5822     CONTINUE                                                         965
5431  CALL AGSPOPB                                                        966
      GO TO BLOCK_TWAS, (5430 ,5440)                                      966
C End of Local Procedure                                                  966
C ----------------------------------------------------------------------  967
C ------------ FEEA                                                       967
C Begin Local Procedure FEEA                                              968
C FEEA TGEOVOLUMEASSEMBLY(FEE)                                            968
5850  CONTINUE                                                            968
         AG_TITLE='FEEA'                                                  968
         AG_MODULE=MODULE                                                 968
         AG_MARK='V00'                                                    968
         CALL AGSPUSH                                                     968
         AG_MODULE=MODULE                                                 968
C    *                                                                    968
C    Geant ATTRIBUTE : FEEA ===> SEEN=1 COLO=KGREEN <===                  969
         CALL AGSRESET                                                    969
         AG_SEEN = 1                                                      969
         AG_COLO = KGREEN                                                 969
         AG_TITLE = 'FEEA'                                                969
         AG_EXNAME = 'ATTRIBUTE'                                          969
         AG_PARLIST = 'SEEN_COLO'                                         969
         CALL AXATTRIBUTE                                                 969
C    *                                                                    969
C    Geant SHAPE : BOX ===> DX=TFEE_ASS(1) DY=TFEE_ASS(2), DZ=TFEE_ASS(3  969
C    ) <===                                                               969
         CALL AGSRESET                                                    971
         AG_DX = TFEE_ASS(1)                                              971
         AG_DY = TFEE_ASS(2)                                              971
         AG_DZ = TFEE_ASS(3)                                              971
         AG_TITLE = 'BOX'                                                 971
         AG_EXNAME = 'SHAPE'                                              971
         AG_PARLIST = 'DX_DY_DZ'                                          971
         CALL AXSHAPE                                                     971
C    *                                                                    971
C    Geant ATTRIBUTE : FEEA ===> SEEN=1 COLO=42 <===                      971
         CALL AGSRESET                                                    972
         AG_SEEN = 1                                                      972
         AG_COLO = 42                                                     972
         AG_TITLE = 'FEEA'                                                972
         AG_EXNAME = 'ATTRIBUTE'                                          972
         AG_PARLIST = 'SEEN_COLO'                                         972
         CALL AXATTRIBUTE                                                 972
C    *                                                                    972
C    Create FEEP :                                                        973
C    Call To Local Procedure FEEP                                         973
         ASSIGN 5880 TO BLOCK_FEEP                                        973
         GO TO  5870                                                      973
5880     CONTINUE                                                         973
C    *                                                                    973
C    Geant POSITION : FEEP ===> inherited arguments used <===             974
         CALL AGSRESET                                                    974
         AG_TITLE = 'FEEP'                                                974
         AG_EXNAME = 'POSITION'                                           974
         AG_PARLIST = 'NONE'                                              974
         CALL AXPOSITION                                                  974
C    *                                                                    974
C    Create FEER :                                                        975
C    Call To Local Procedure FEER                                         975
         ASSIGN 5900 TO BLOCK_FEER                                        975
         GO TO  5890                                                      975
5900     CONTINUE                                                         975
C    *                                                                    975
C    Geant POSITION : FEER ===> X=-TFEE_RIBDX-TFEE_CARDDX <===            976
         CALL AGSRESET                                                    976
         AG_X = -TFEE_RIBDX-TFEE_CARDDX                                   976
         AG_TITLE = 'FEER'                                                976
         AG_EXNAME = 'POSITION'                                           976
         AG_PARLIST = 'X'                                                 976
         CALL AXPOSITION                                                  976
C    *                                                                    976
C    Create FEEI :                                                        977
C    Call To Local Procedure FEEI                                         977
         ASSIGN 5920 TO BLOCK_FEEI                                        977
         GO TO  5910                                                      977
5920     CONTINUE                                                         977
C    *                                                                    977
C    Geant POSITION : FEEI ===> X=2*TFEE_CARDDX Z=0.5 <===                978
         CALL AGSRESET                                                    978
         AG_X = 2*TFEE_CARDDX                                             978
         AG_Z = 0.5                                                       978
         AG_TITLE = 'FEEI'                                                978
         AG_EXNAME = 'POSITION'                                           978
         AG_PARLIST = 'X_Z'                                               978
         CALL AXPOSITION                                                  978
C    *                                                                    978
5851  CALL AGSPOPB                                                        979
      GO TO BLOCK_FEEA, (5850 ,5860)                                      979
C End of Local Procedure                                                  979
C ----------------------------------------------------------------------  980
C ------------ FEEP                                                       980
C Begin Local Procedure FEEP                                              981
C FEEP FEEPLATE                                                           981
5870  CONTINUE                                                            981
         AG_TITLE='FEEP'                                                  981
         AG_MODULE=MODULE                                                 981
         AG_MARK='V00'                                                    981
         CALL AGSPUSH                                                     981
         AG_MODULE=MODULE                                                 981
C    *                                                                    981
C    Geant ATTRIBUTE : FEEP ===> SEEN=1 COLO=KRED <===                    982
         CALL AGSRESET                                                    982
         AG_SEEN = 1                                                      982
         AG_COLO = KRED                                                   982
         AG_TITLE = 'FEEP'                                                982
         AG_EXNAME = 'ATTRIBUTE'                                          982
         AG_PARLIST = 'SEEN_COLO'                                         982
         CALL AXATTRIBUTE                                                 982
C    *                                                                    982
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        982
         CALL AGSRESET                                                    983
         AG_TITLE = 'ALUMINIUM'                                           983
         AG_EXNAME = 'MATERIAL'                                           983
         AG_PARLIST = 'NONE'                                              983
         CALL AXMATERIAL                                                  983
C    *                                                                    983
C    Geant SHAPE : BOX ===> DX=TFEE_PLATEDX DY=TFEE_PLATEDY, DZ=TFEE_PLA  983
C    TEDZ <===                                                            983
         CALL AGSRESET                                                    985
         AG_DX = TFEE_PLATEDX                                             985
         AG_DY = TFEE_PLATEDY                                             985
         AG_DZ = TFEE_PLATEDZ                                             985
         AG_TITLE = 'BOX'                                                 985
         AG_EXNAME = 'SHAPE'                                              985
         AG_PARLIST = 'DX_DY_DZ'                                          985
         CALL AXSHAPE                                                     985
C    *                                                                    985
5871  CALL AGSPOPB                                                        986
      GO TO BLOCK_FEEP, (5870 ,5880)                                      986
C End of Local Procedure                                                  986
C ----------------------------------------------------------------------  987
C ------------ FEER                                                       987
C Begin Local Procedure FEER                                              988
C FEER FEERIB                                                             988
5890  CONTINUE                                                            988
         AG_TITLE='FEER'                                                  988
         AG_MODULE=MODULE                                                 988
         AG_MARK='V00'                                                    988
         CALL AGSPUSH                                                     988
         AG_MODULE=MODULE                                                 988
C    *                                                                    988
C    Geant ATTRIBUTE : FEER ===> SEEN=1 COLO=1 <===                       989
         CALL AGSRESET                                                    989
         AG_SEEN = 1                                                      989
         AG_COLO = 1                                                      989
         AG_TITLE = 'FEER'                                                989
         AG_EXNAME = 'ATTRIBUTE'                                          989
         AG_PARLIST = 'SEEN_COLO'                                         989
         CALL AXATTRIBUTE                                                 989
C    *                                                                    989
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        989
         CALL AGSRESET                                                    990
         AG_TITLE = 'ALUMINIUM'                                           990
         AG_EXNAME = 'MATERIAL'                                           990
         AG_PARLIST = 'NONE'                                              990
         CALL AXMATERIAL                                                  990
C    *                                                                    990
C    Geant SHAPE : BOX ===> DX=TFEE_RIBDX DY=TFEE_RIBDY DZ=TFEE_RIBDZ <=  990
C    ==                                                                   990
         CALL AGSRESET                                                    991
         AG_DX = TFEE_RIBDX                                               991
         AG_DY = TFEE_RIBDY                                               991
         AG_DZ = TFEE_RIBDZ                                               991
         AG_TITLE = 'BOX'                                                 991
         AG_EXNAME = 'SHAPE'                                              991
         AG_PARLIST = 'DX_DY_DZ'                                          991
         CALL AXSHAPE                                                     991
C    *                                                                    991
5891  CALL AGSPOPB                                                        992
      GO TO BLOCK_FEER, (5890 ,5900)                                      992
C End of Local Procedure                                                  992
C ----------------------------------------------------------------------  993
C ------------ FEEI                                                       993
C Begin Local Procedure FEEI                                              994
C FEEI FEEITSELF                                                          994
5910  CONTINUE                                                            994
         AG_TITLE='FEEI'                                                  994
         AG_MODULE=MODULE                                                 994
         AG_MARK='V00'                                                    994
         CALL AGSPUSH                                                     994
         AG_MODULE=MODULE                                                 994
C    *                                                                    994
C    Geant ATTRIBUTE : FEEI ===> SEEN=1 COLO=KVIOLET <===                 995
         CALL AGSRESET                                                    995
         AG_SEEN = 1                                                      995
         AG_COLO = KVIOLET                                                995
         AG_TITLE = 'FEEI'                                                995
         AG_EXNAME = 'ATTRIBUTE'                                          995
         AG_PARLIST = 'SEEN_COLO'                                         995
         CALL AXATTRIBUTE                                                 995
C    *                                                                    995
C    Geant MATERIAL : G10 ===> inherited arguments used <===              995
         CALL AGSRESET                                                    996
         AG_TITLE = 'G10'                                                 996
         AG_EXNAME = 'MATERIAL'                                           996
         AG_PARLIST = 'NONE'                                              996
         CALL AXMATERIAL                                                  996
C    *                                                                    996
C    Geant SHAPE : BOX ===> DX=TFEE_CARDDX DY=TFEE_CARDDY, DZ=TFEE_CARDD  996
C    Z <===                                                               996
         CALL AGSRESET                                                    998
         AG_DX = TFEE_CARDDX                                              998
         AG_DY = TFEE_CARDDY                                              998
         AG_DZ = TFEE_CARDDZ                                              998
         AG_TITLE = 'BOX'                                                 998
         AG_EXNAME = 'SHAPE'                                              998
         AG_PARLIST = 'DX_DY_DZ'                                          998
         CALL AXSHAPE                                                     998
C    *                                                                    998
5911  CALL AGSPOPB                                                        999
      GO TO BLOCK_FEEI, (5910 ,5920)                                      999
C End of Local Procedure                                                  999
C ----------------------------------------------------------------------  1000
C ------------ TALS                                                       1000
C Begin Local Procedure TALS                                              1001
C TALS TPCSECTORALSUPPORT THE SECTOR G10 ALLIMINIUM SUPPORT               1001
5680  CONTINUE                                                            1001
         AG_TITLE='TALS'                                                  1001
         AG_MODULE=MODULE                                                 1001
         AG_MARK='V00'                                                    1001
         CALL AGSPUSH                                                     1001
         AG_MODULE=MODULE                                                 1001
C    *                                                                    1001
C    Geant ATTRIBUTE : TALS ===> SEEN=1 COLO=KYELLOW <===                 1002
         CALL AGSRESET                                                    1002
         AG_SEEN = 1                                                      1002
         AG_COLO = KYELLOW                                                1002
         AG_TITLE = 'TALS'                                                1002
         AG_EXNAME = 'ATTRIBUTE'                                          1002
         AG_PARLIST = 'SEEN_COLO'                                         1002
         CALL AXATTRIBUTE                                                 1002
C    *                                                                    1002
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1002
         CALL AGSRESET                                                    1003
         AG_TITLE = 'ALUMINIUM'                                           1003
         AG_EXNAME = 'MATERIAL'                                           1003
         AG_PARLIST = 'NONE'                                              1003
         CALL AXMATERIAL                                                  1003
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
            GO TO 5933                                                    1012
5931        ITALS=ITALS+1                                                 1012
5933        IF(ITALS-(HOLZ_NUMBEROFROWS - 1).GT.0) GO TO 5932             1012
            MYPAR(11) = XHOLES(ITALS)                                     1012
C       // CM                                                             1013
C       *                                                                 1014
            JTALS=0                                                       1014
               GO TO 5943                                                 1014
5941           JTALS=JTALS+1                                              1014
5943           IF(JTALS-(HOLZ_NUMBERPERROW( ITALS+1 ) - 1).GT.0) GO TO    1014
     *         5942                                                       1014
               MYPAR(12) = HOLZ_YHOLES( 9*ITALS+JTALS+1) *INCH            1015
C          Create THOL :                                                  1016
C          Call To Local Procedure THOL                                   1016
               ASSIGN 5960 TO BLOCK_THOL                                  1016
               GO TO  5950                                                1016
5960           CONTINUE                                                   1016
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
            GO TO 5941                                                    1018
5942        CONTINUE                                                      1018
         GO TO 5931                                                       1019
5932     CONTINUE                                                         1019
5681  CALL AGSPOPB                                                        1020
      GO TO BLOCK_TALS, (5680 ,5690)                                      1020
C End of Local Procedure                                                  1020
C ----------------------------------------------------------------------  1021
C ------------ TSGT                                                       1021
C Begin Local Procedure TSGT                                              1022
C TSGT TPCSECTORG10 THE SECTOR G10                                        1022
5700  CONTINUE                                                            1022
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
C    Geant SHAPE : PGON ===> PHI1=-ALPHA DPHI=2*ALPHA NPDIV=1, NZ=2 ZI={  1024
C    Z1,Z1+TPCG_PADPLANETHICKNESS} RMN={R1, R1} RMX={R2,R2} <===          1024
         CALL AGSRESET                                                    1027
         AG_PHI1 = -ALPHA                                                 1027
         AG_DPHI = 2*ALPHA                                                1027
         AG_NPDIV = 1                                                     1027
         AG_NZ = 2                                                        1027
         AG_ZI(1)=Z1                                                      1027
         AG_ZI(2)=Z1+TPCG_PADPLANETHICKNESS                               1027
         AG_RMN(1)=R1                                                     1027
         AG_RMN(2)=R1                                                     1027
         AG_RMX(1)=R2                                                     1027
         AG_RMX(2)=R2                                                     1027
         AG_TITLE = 'PGON'                                                1027
         AG_EXNAME = 'SHAPE'                                              1027
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1027
         CALL AXSHAPE                                                     1027
C    *                                                                    1027
5701  CALL AGSPOPB                                                        1028
      GO TO BLOCK_TSGT, (5700 ,5710)                                      1028
C End of Local Procedure                                                  1028
C ----------------------------------------------------------------------  1029
C ------------ TWBT                                                       1029
C Begin Local Procedure TWBT                                              1030
C TWBT //WHEELBOTTOM                                                      1030
5520  CONTINUE                                                            1030
         AG_TITLE='TWBT'                                                  1030
         AG_MODULE=MODULE                                                 1030
         AG_MARK='V00'                                                    1030
         CALL AGSPUSH                                                     1030
         AG_MODULE=MODULE                                                 1030
C    *                                                                    1030
C    Geant ATTRIBUTE : TWBT ===> SEEN=1 COLO=KGREEN <===                  1031
         CALL AGSRESET                                                    1031
         AG_SEEN = 1                                                      1031
         AG_COLO = KGREEN                                                 1031
         AG_TITLE = 'TWBT'                                                1031
         AG_EXNAME = 'ATTRIBUTE'                                          1031
         AG_PARLIST = 'SEEN_COLO'                                         1031
         CALL AXATTRIBUTE                                                 1031
C    *                                                                    1031
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1031
         CALL AGSRESET                                                    1032
         AG_TITLE = 'ALUMINIUM'                                           1032
         AG_EXNAME = 'MATERIAL'                                           1032
         AG_PARLIST = 'NONE'                                              1032
         CALL AXMATERIAL                                                  1032
C    *                                                                    1032
C    Geant SHAPE : PCON ===> PHI1=-15 DPHI=30 NZ=4 ZI={ZWHEEL1, ZWHEEL1+  1032
C    3.500*INCH,ZWHEEL1+3.500*INCH,TPCG_LENGTHW/2}, RMN={TPCG_WHEELIR,TP  1032
C    CG_WHEELIR, TPCG_WHEELIR+3.6,TPCG_WHEELIR+3.6} RMX={TPCG_WHEELR0, T  1032
C    PCG_WHEELR0,TPCG_WHEELR0,TPCG_WHEELR0} <===                          1032
         CALL AGSRESET                                                    1037
         AG_PHI1 = -15                                                    1037
         AG_DPHI = 30                                                     1037
         AG_NZ = 4                                                        1037
         AG_ZI(1)=ZWHEEL1                                                 1037
         AG_ZI(2)=ZWHEEL1+3.500*INCH                                      1037
         AG_ZI(3)=ZWHEEL1+3.500*INCH                                      1037
         AG_ZI(4)=TPCG_LENGTHW/2                                          1037
         AG_RMN(1)=TPCG_WHEELIR                                           1037
         AG_RMN(2)=TPCG_WHEELIR                                           1037
         AG_RMN(3)=TPCG_WHEELIR+3.6                                       1037
         AG_RMN(4)=TPCG_WHEELIR+3.6                                       1037
         AG_RMX(1)=TPCG_WHEELR0                                           1037
         AG_RMX(2)=TPCG_WHEELR0                                           1037
         AG_RMX(3)=TPCG_WHEELR0                                           1037
         AG_RMX(4)=TPCG_WHEELR0                                           1037
         AG_TITLE = 'PCON'                                                1037
         AG_EXNAME = 'SHAPE'                                              1037
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1037
         CALL AXSHAPE                                                     1037
C    *                                                                    1037
5521  CALL AGSPOPB                                                        1038
      GO TO BLOCK_TWBT, (5520 ,5530)                                      1038
C End of Local Procedure                                                  1038
C ----------------------------------------------------------------------  1039
C ------------ TWRI                                                       1039
C Begin Local Procedure TWRI                                              1040
C TWRI WHEELOUTERRING                                                     1040
5540  CONTINUE                                                            1040
         AG_TITLE='TWRI'                                                  1040
         AG_MODULE=MODULE                                                 1040
         AG_MARK='V00'                                                    1040
         CALL AGSPUSH                                                     1040
         AG_MODULE=MODULE                                                 1040
C    *                                                                    1040
C    Geant SHAPE : PCON ===> PHI1=-15 DPHI=30 NZ=4 ZI={ZWHEEL1, ZTOFCEND  1045
C    ,ZTOFCEND,TPCG_LENGTHW/2} RMN={TPCG_WHEELR2*COS15, TPCG_WHEELR2*COS  1045
C    15,TPCG_WHEELR2*COS15, TPCG_WHEELR2*COS15} RMX={TPCG_WHEELOR1, TPCG  1045
C    _WHEELOR1,TPCG_WHEELOR,TPCG_WHEELOR} <===                            1045
         CALL AGSRESET                                                    1045
         AG_PHI1 = -15                                                    1045
         AG_DPHI = 30                                                     1045
         AG_NZ = 4                                                        1045
         AG_ZI(1)=ZWHEEL1                                                 1045
         AG_ZI(2)=ZTOFCEND                                                1045
         AG_ZI(3)=ZTOFCEND                                                1045
         AG_ZI(4)=TPCG_LENGTHW/2                                          1045
         AG_RMN(1)=TPCG_WHEELR2*COS15                                     1045
         AG_RMN(2)=TPCG_WHEELR2*COS15                                     1045
         AG_RMN(3)=TPCG_WHEELR2*COS15                                     1045
         AG_RMN(4)=TPCG_WHEELR2*COS15                                     1045
         AG_RMX(1)=TPCG_WHEELOR1                                          1045
         AG_RMX(2)=TPCG_WHEELOR1                                          1045
         AG_RMX(3)=TPCG_WHEELOR                                           1045
         AG_RMX(4)=TPCG_WHEELOR                                           1045
         AG_TITLE = 'PCON'                                                1045
         AG_EXNAME = 'SHAPE'                                              1045
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1045
         CALL AXSHAPE                                                     1045
C    *                                                                    1045
C    Geant MEDIUM : TWRI_SOMETHING ===> DEEMAX=0.248853 STMIN=1.15055 <=  1045
C    ==                                                                   1045
         CALL AGSRESET                                                    1046
         AG_DEEMAX = 0.248853                                             1046
         AG_STMIN = 1.15055                                               1046
         AG_TITLE = 'TWRI_SOMETHING'                                      1046
         AG_EXNAME = 'MEDIUM'                                             1046
         AG_PARLIST = 'DEEMAX_STMIN'                                      1046
         CALL AXMEDIUM                                                    1046
C    *                                                                    1046
C    Geant ATTRIBUTE : TWRI ===> SEEN=0 COLO=KGREEN <===                  1046
         CALL AGSRESET                                                    1047
         AG_SEEN = 0                                                      1047
         AG_COLO = KGREEN                                                 1047
         AG_TITLE = 'TWRI'                                                1047
         AG_EXNAME = 'ATTRIBUTE'                                          1047
         AG_PARLIST = 'SEEN_COLO'                                         1047
         CALL AXATTRIBUTE                                                 1047
C    *                                                                    1047
C    Create TWRC :                                                        1048
C    Call To Local Procedure TWRC                                         1048
         ASSIGN 5980 TO BLOCK_TWRC                                        1048
         GO TO  5970                                                      1048
5980     CONTINUE                                                         1048
C    *                                                                    1048
C    Geant POSITION : TWRC ===> inherited arguments used <===             1049
         CALL AGSRESET                                                    1049
         AG_TITLE = 'TWRC'                                                1049
         AG_EXNAME = 'POSITION'                                           1049
         AG_PARLIST = 'NONE'                                              1049
         CALL AXPOSITION                                                  1049
C    *                                                                    1049
         MYCOS = COS15                                                    1049
         MYRMIN=TPCG_WHEELR2                                              1049
         MYRMAX = TPCG_WHEELOR1                                           1050
         MYG1 = MYRMIN*COS15                                              1051
         NCOUNT = 0                                                       1052
C    *                                                                    1053
C       Loop here                                                         1053
5991        IF(MYG2.GE.MYRMIN) GO TO 5992                                 1053
            NCOUNT=NCOUNT+1                                               1053
            MYG2 = MYRMIN                                                 1055
C       Check (MYG2>MYRMAX*MYCOS)                                         1056
            IF ((MYG2.GT.MYRMAX*MYCOS)) THEN                              1056
            MYG2 = MYRMAX*MYCOS                                           1057
            ENDIF                                                         1058
            ALPHA = ACOS(MYCOS)*RADDEG                                    1058
C       Create TWRG :                                                     1060
C       Call To Local Procedure TWRG                                      1060
            ASSIGN 6010 TO BLOCK_TWRG                                     1060
            GO TO  6000                                                   1060
6010        CONTINUE                                                      1060
C       *                                                                 1060
C       Geant POSITION : TWRG ===> inherited arguments used <===          1061
            CALL AGSRESET                                                 1061
            AG_TITLE = 'TWRG'                                             1061
            AG_EXNAME = 'POSITION'                                        1061
            AG_PARLIST = 'NONE'                                           1061
            CALL AXPOSITION                                               1061
C       *                                                                 1061
            MYCOS = MYG2/MYRMIN                                           1061
            MYG1=MYG2                                                     1062
C       Check (NCOUNT>10)                                                 1063
            IF ((NCOUNT.GT.10)) THEN                                      1063
            GO TO 5992                                                    1064
            ENDIF                                                         1065
         GO TO 5991                                                       1066
5992     CONTINUE                                                         1066
5541  CALL AGSPOPB                                                        1067
      GO TO BLOCK_TWRI, (5540 ,5550)                                      1067
C End of Local Procedure                                                  1067
C ----------------------------------------------------------------------  1068
C ------------ TWRG                                                       1068
C Begin Local Procedure TWRG                                              1069
C TWRG WHEELOUTERRING PGON PART                                           1069
6000  CONTINUE                                                            1069
         AG_TITLE='TWRG'                                                  1069
         AG_MODULE=MODULE                                                 1069
         AG_MARK='V00'                                                    1069
         CALL AGSPUSH                                                     1069
         AG_MODULE=MODULE                                                 1069
C    *                                                                    1069
C    Geant ATTRIBUTE : TWRG ===> SEEN=1 COLO=KGREEN <===                  1070
         CALL AGSRESET                                                    1070
         AG_SEEN = 1                                                      1070
         AG_COLO = KGREEN                                                 1070
         AG_TITLE = 'TWRG'                                                1070
         AG_EXNAME = 'ATTRIBUTE'                                          1070
         AG_PARLIST = 'SEEN_COLO'                                         1070
         CALL AXATTRIBUTE                                                 1070
C    *                                                                    1070
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1070
         CALL AGSRESET                                                    1071
         AG_TITLE = 'ALUMINIUM'                                           1071
         AG_EXNAME = 'MATERIAL'                                           1071
         AG_PARLIST = 'NONE'                                              1071
         CALL AXMATERIAL                                                  1071
C    *                                                                    1071
C    Geant SHAPE : PGON ===> PHI1=-ALPHA DPHI=ALPHA*2 NPDIV=1, NZ=2 ZI={  1071
C    ZWHEEL1,TPCG_LENGTHW/2} RMN={MYG1, MYG1} RMX={MYG2,MYG2} <===        1071
         CALL AGSRESET                                                    1074
         AG_PHI1 = -ALPHA                                                 1074
         AG_DPHI = ALPHA*2                                                1074
         AG_NPDIV = 1                                                     1074
         AG_NZ = 2                                                        1074
         AG_ZI(1)=ZWHEEL1                                                 1074
         AG_ZI(2)=TPCG_LENGTHW/2                                          1074
         AG_RMN(1)=MYG1                                                   1074
         AG_RMN(2)=MYG1                                                   1074
         AG_RMX(1)=MYG2                                                   1074
         AG_RMX(2)=MYG2                                                   1074
         AG_TITLE = 'PGON'                                                1074
         AG_EXNAME = 'SHAPE'                                              1074
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1074
         CALL AXSHAPE                                                     1074
C    *                                                                    1074
6001  CALL AGSPOPB                                                        1075
      GO TO BLOCK_TWRG, (6000 ,6010)                                      1075
C End of Local Procedure                                                  1075
C ----------------------------------------------------------------------  1076
C ------------ TWRC                                                       1076
C Begin Local Procedure TWRC                                              1077
C TWRC WHEELOUTERRING PCON PART                                           1077
5970  CONTINUE                                                            1077
         AG_TITLE='TWRC'                                                  1077
         AG_MODULE=MODULE                                                 1077
         AG_MARK='V00'                                                    1077
         CALL AGSPUSH                                                     1077
         AG_MODULE=MODULE                                                 1077
C    *                                                                    1077
C    Geant ATTRIBUTE : TWRC ===> SEEN=1 COLO=KGREEN <===                  1078
         CALL AGSRESET                                                    1078
         AG_SEEN = 1                                                      1078
         AG_COLO = KGREEN                                                 1078
         AG_TITLE = 'TWRC'                                                1078
         AG_EXNAME = 'ATTRIBUTE'                                          1078
         AG_PARLIST = 'SEEN_COLO'                                         1078
         CALL AXATTRIBUTE                                                 1078
C    *                                                                    1078
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1078
         CALL AGSRESET                                                    1079
         AG_TITLE = 'ALUMINIUM'                                           1079
         AG_EXNAME = 'MATERIAL'                                           1079
         AG_PARLIST = 'NONE'                                              1079
         CALL AXMATERIAL                                                  1079
C    *                                                                    1079
C    Geant SHAPE : PCON ===> PHI1=-15 DPHI=30 NZ=4 ZI={ZWHEEL1, ZTOFCEND  1079
C    ,ZTOFCEND,TPCG_LENGTHW/2} RMN={TPCG_WHEELR2/COS15, TPCG_WHEELR2/COS  1079
C    15,TPCG_WHEELR2/COS15, TPCG_WHEELR2/COS15} RMX={TPCG_WHEELOR1, TPCG  1079
C    _WHEELOR1,TPCG_WHEELOR,TPCG_WHEELOR} <===                            1079
         CALL AGSRESET                                                    1084
         AG_PHI1 = -15                                                    1084
         AG_DPHI = 30                                                     1084
         AG_NZ = 4                                                        1084
         AG_ZI(1)=ZWHEEL1                                                 1084
         AG_ZI(2)=ZTOFCEND                                                1084
         AG_ZI(3)=ZTOFCEND                                                1084
         AG_ZI(4)=TPCG_LENGTHW/2                                          1084
         AG_RMN(1)=TPCG_WHEELR2/COS15                                     1084
         AG_RMN(2)=TPCG_WHEELR2/COS15                                     1084
         AG_RMN(3)=TPCG_WHEELR2/COS15                                     1084
         AG_RMN(4)=TPCG_WHEELR2/COS15                                     1084
         AG_RMX(1)=TPCG_WHEELOR1                                          1084
         AG_RMX(2)=TPCG_WHEELOR1                                          1084
         AG_RMX(3)=TPCG_WHEELOR                                           1084
         AG_RMX(4)=TPCG_WHEELOR                                           1084
         AG_TITLE = 'PCON'                                                1084
         AG_EXNAME = 'SHAPE'                                              1084
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1084
         CALL AXSHAPE                                                     1084
C    *                                                                    1084
5971  CALL AGSPOPB                                                        1085
      GO TO BLOCK_TWRC, (5970 ,5980)                                      1085
C End of Local Procedure                                                  1085
C ----------------------------------------------------------------------  1086
C ------------ TWTR                                                       1086
C Begin Local Procedure TWTR                                              1087
C TWTR TPCWHEELTOPRIB                                                     1087
5560  CONTINUE                                                            1087
         AG_TITLE='TWTR'                                                  1087
         AG_MODULE=MODULE                                                 1087
         AG_MARK='V00'                                                    1087
         CALL AGSPUSH                                                     1087
         AG_MODULE=MODULE                                                 1087
C    *                                                                    1087
C    Geant ATTRIBUTE : TWTR ===> SEEN=1 COLO=KBLUE <===                   1088
         CALL AGSRESET                                                    1088
         AG_SEEN = 1                                                      1088
         AG_COLO = KBLUE                                                  1088
         AG_TITLE = 'TWTR'                                                1088
         AG_EXNAME = 'ATTRIBUTE'                                          1088
         AG_PARLIST = 'SEEN_COLO'                                         1088
         CALL AXATTRIBUTE                                                 1088
C    *                                                                    1088
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1088
         CALL AGSRESET                                                    1089
         AG_TITLE = 'ALUMINIUM'                                           1089
         AG_EXNAME = 'MATERIAL'                                           1089
         AG_PARLIST = 'NONE'                                              1089
         CALL AXMATERIAL                                                  1089
C    *                                                                    1089
         MYG1=TPCG_WHEELR2 - (TPCG_WHEELTOTALRIBWIDTH -                   1089
     *   TPCG_WHEELRIBWIDTH)/2                                            1089
C    Geant SHAPE : PGON ===> PHI1=-15 DPHI=30 NPDIV=1 NZ=2, ZI={ZWHEEL1,  1093
C    ZWHEEL1+TPCG_WHEELRIBHEIGHT} RMN={MYG1, MYG1} RMX={TPCG_WHEELR2,TPC  1093
C    G_WHEELR2} <===                                                      1093
         CALL AGSRESET                                                    1093
         AG_PHI1 = -15                                                    1093
         AG_DPHI = 30                                                     1093
         AG_NPDIV = 1                                                     1093
         AG_NZ = 2                                                        1093
         AG_ZI(1)=ZWHEEL1                                                 1093
         AG_ZI(2)=ZWHEEL1+TPCG_WHEELRIBHEIGHT                             1093
         AG_RMN(1)=MYG1                                                   1093
         AG_RMN(2)=MYG1                                                   1093
         AG_RMX(1)=TPCG_WHEELR2                                           1093
         AG_RMX(2)=TPCG_WHEELR2                                           1093
         AG_TITLE = 'PGON'                                                1093
         AG_EXNAME = 'SHAPE'                                              1093
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1093
         CALL AXSHAPE                                                     1093
C    *                                                                    1093
5561  CALL AGSPOPB                                                        1094
      GO TO BLOCK_TWTR, (5560 ,5570)                                      1094
C End of Local Procedure                                                  1094
C ----------------------------------------------------------------------  1095
C ------------ TWMR                                                       1095
C Begin Local Procedure TWMR                                              1096
C TWMR TPCWHEELMIDDLERIB                                                  1096
5580  CONTINUE                                                            1096
         AG_TITLE='TWMR'                                                  1096
         AG_MODULE=MODULE                                                 1096
         AG_MARK='V00'                                                    1096
         CALL AGSPUSH                                                     1096
         AG_MODULE=MODULE                                                 1096
C    *                                                                    1096
C    Geant ATTRIBUTE : TWMR ===> SEEN=1 COLO=KYELLOW <===                 1097
         CALL AGSRESET                                                    1097
         AG_SEEN = 1                                                      1097
         AG_COLO = KYELLOW                                                1097
         AG_TITLE = 'TWMR'                                                1097
         AG_EXNAME = 'ATTRIBUTE'                                          1097
         AG_PARLIST = 'SEEN_COLO'                                         1097
         CALL AXATTRIBUTE                                                 1097
C    *                                                                    1097
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1097
         CALL AGSRESET                                                    1098
         AG_TITLE = 'ALUMINIUM'                                           1098
         AG_EXNAME = 'MATERIAL'                                           1098
         AG_PARLIST = 'NONE'                                              1098
         CALL AXMATERIAL                                                  1098
C    *                                                                    1098
         MYG1=TPCG_WHEELR2 - (TPCG_WHEELTOTALRIBWIDTH -                   1098
     *   TPCG_WHEELRIBWIDTH)/2                                            1098
C    Geant SHAPE : PGON ===> PHI1=-15 DPHI=30 NPDIV=1 NZ=4, ZI={ZWHEEL1,  1108
C    ZWHEEL1+TPCG_WHEELRIBHEIGHT,ZWHEEL1+TPCG_WHEELRIBHEIGHT, TPCG_LENGT  1108
C    HW/2} RMN={TPCG_WHEELR1-TPCG_WHEELTOTALRIBWIDTH/2, TPCG_WHEELR1-TPC  1108
C    G_WHEELTOTALRIBWIDTH/2, TPCG_WHEELR1-TPCG_WHEELRIBWIDTH/2, TPCG_WHE  1108
C    ELR1-TPCG_WHEELRIBWIDTH/2}, RMX={TPCG_WHEELR1+TPCG_WHEELTOTALRIBWID  1108
C    TH/2, TPCG_WHEELR1+TPCG_WHEELTOTALRIBWIDTH/2, TPCG_WHEELR1+TPCG_WHE  1108
C    ELRIBWIDTH/2,TPCG_WHEELR1+TPCG_WHEELRIBWIDTH/2} <===                 1108
         CALL AGSRESET                                                    1108
         AG_PHI1 = -15                                                    1108
         AG_DPHI = 30                                                     1108
         AG_NPDIV = 1                                                     1108
         AG_NZ = 4                                                        1108
         AG_ZI(1)=ZWHEEL1                                                 1108
         AG_ZI(2)=ZWHEEL1+TPCG_WHEELRIBHEIGHT                             1108
         AG_ZI(3)=ZWHEEL1+TPCG_WHEELRIBHEIGHT                             1108
         AG_ZI(4)=TPCG_LENGTHW/2                                          1108
         AG_RMN(1)=TPCG_WHEELR1-TPCG_WHEELTOTALRIBWIDTH/2                 1108
         AG_RMN(2)=TPCG_WHEELR1-TPCG_WHEELTOTALRIBWIDTH/2                 1108
         AG_RMN(3)=TPCG_WHEELR1-TPCG_WHEELRIBWIDTH/2                      1108
         AG_RMN(4)=TPCG_WHEELR1-TPCG_WHEELRIBWIDTH/2                      1108
         AG_RMX(1)=TPCG_WHEELR1+TPCG_WHEELTOTALRIBWIDTH/2                 1108
         AG_RMX(2)=TPCG_WHEELR1+TPCG_WHEELTOTALRIBWIDTH/2                 1108
         AG_RMX(3)=TPCG_WHEELR1+TPCG_WHEELRIBWIDTH/2                      1108
         AG_RMX(4)=TPCG_WHEELR1+TPCG_WHEELRIBWIDTH/2                      1108
         AG_TITLE = 'PGON'                                                1108
         AG_EXNAME = 'SHAPE'                                              1108
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1108
         CALL AXSHAPE                                                     1108
C    *                                                                    1108
5581  CALL AGSPOPB                                                        1109
      GO TO BLOCK_TWMR, (5580 ,5590)                                      1109
C End of Local Procedure                                                  1109
C ----------------------------------------------------------------------  1110
C ------------ TRDO                                                       1110
C Begin Local Procedure TRDO                                              1111
C TRDO TPCRDOASSEMBLY                                                     1111
5600  CONTINUE                                                            1111
         AG_TITLE='TRDO'                                                  1111
         AG_MODULE=MODULE                                                 1111
         AG_MARK='V00'                                                    1111
         CALL AGSPUSH                                                     1111
         AG_MODULE=MODULE                                                 1111
C    *                                                                    1111
C    Geant ATTRIBUTE : TRDO ===> SEEN=0 <===                              1112
         CALL AGSRESET                                                    1112
         AG_SEEN = 0                                                      1112
         AG_TITLE = 'TRDO'                                                1112
         AG_EXNAME = 'ATTRIBUTE'                                          1112
         AG_PARLIST = 'SEEN'                                              1112
         CALL AXATTRIBUTE                                                 1112
C    *                                                                    1112
C    Geant ATTRIBUTE : TRDO ===> SEEN=1 COLO=KYELLOW <===                 1112
         CALL AGSRESET                                                    1113
         AG_SEEN = 1                                                      1113
         AG_COLO = KYELLOW                                                1113
         AG_TITLE = 'TRDO'                                                1113
         AG_EXNAME = 'ATTRIBUTE'                                          1113
         AG_PARLIST = 'SEEN_COLO'                                         1113
         CALL AXATTRIBUTE                                                 1113
C    *                                                                    1113
C    Geant SHAPE : PGON ===> PHI1=-15 DPHI=30 NPDIV=1 NZ=2, ZI={ZZZRDO(1  1113
C    ),ZZZRDO(2)} RMN={RMNRDO(1),RMNRDO(2)}, RMX={RMXRDO(1),RMXRDO(2)} <  1113
C    ===                                                                  1113
         CALL AGSRESET                                                    1116
         AG_PHI1 = -15                                                    1116
         AG_DPHI = 30                                                     1116
         AG_NPDIV = 1                                                     1116
         AG_NZ = 2                                                        1116
         AG_ZI(1)=ZZZRDO(1)                                               1116
         AG_ZI(2)=ZZZRDO(2)                                               1116
         AG_RMN(1)=RMNRDO(1)                                              1116
         AG_RMN(2)=RMNRDO(2)                                              1116
         AG_RMX(1)=RMXRDO(1)                                              1116
         AG_RMX(2)=RMXRDO(2)                                              1116
         AG_TITLE = 'PGON'                                                1116
         AG_EXNAME = 'SHAPE'                                              1116
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1116
         CALL AXSHAPE                                                     1116
C    *                                                                    1116
         ISHAPE = KPARA                                                   1116
         MYPAR(1)=TPCG_RDOCOOLINGDY                                       1118
         MYPAR(2)=TPCG_RDOCOOLINGDX                                       1118
         MYPAR(3)=TPCG_RDOCOOLINGDZ                                       1118
         MYPAR(4)=15                                                      1118
         X = TPCG_WHEELR2 - TPCG_RDOCOOLINGDX                             1119
         Y = X*TAN15 - TPCG_RDOCOOLINGDY                                  1120
         Z = TPCG_ZRDO - 2*DY                                             1121
C    Create TCOO :                                                        1122
C    Call To Local Procedure TCOO                                         1122
         ASSIGN 6020 TO BLOCK_TCOO                                        1122
         GO TO  5630                                                      1122
6020     CONTINUE                                                         1122
C    *                                                                    1122
C    Geant POSITION : TCOO ===> X=X Y=Y Z=Z ORT=YX-Z <===                 1123
         CALL AGSRESET                                                    1123
         AG_X = X                                                         1123
         AG_Y = Y                                                         1123
         AG_Z = Z                                                         1123
         AG_ALPHAN=AG_ALPHAN+1                                            1123
         AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                                1123
         AG_TITLE = 'TCOO'                                                1123
         AG_EXNAME = 'POSITION'                                           1123
         AG_PARLIST = 'X_Y_Z_ORT'                                         1123
         CALL AXPOSITION                                                  1123
C    *                                                                    1123
         ISHAPE = KPARA                                                   1123
         MYPAR(1)=TPCG_RDOCOOLINGDY                                       1125
         MYPAR(2)=TPCG_RDOCOOLINGDX                                       1125
         MYPAR(3)=TPCG_RDOCOOLINGDZ                                       1125
         MYPAR(4)=-15                                                     1125
         X = TPCG_WHEELR2 - TPCG_RDOCOOLINGDX                             1126
         Y = X*TAN15 - TPCG_RDOCOOLINGDY                                  1127
         Z = TPCG_ZRDO - 2*DY                                             1128
C    Create TCOO :                                                        1129
C    Call To Local Procedure TCOO                                         1129
         ASSIGN 6030 TO BLOCK_TCOO                                        1129
         GO TO  5630                                                      1129
6030     CONTINUE                                                         1129
C    *                                                                    1129
C    Geant POSITION : TCOO ===> X=X Y=-Y Z=Z ORT=YX-Z <===                1130
         CALL AGSRESET                                                    1130
         AG_X = X                                                         1130
         AG_Y = -Y                                                        1130
         AG_Z = Z                                                         1130
         AG_ALPHAN=AG_ALPHAN+1                                            1130
         AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                                1130
         AG_TITLE = 'TCOO'                                                1130
         AG_EXNAME = 'POSITION'                                           1130
         AG_PARLIST = 'X_Y_Z_ORT'                                         1130
         CALL AXPOSITION                                                  1130
C    *                                                                    1130
         RCOOLINGTUBE = TPCG_WHEELR2 - TPCG_HEIGTUBE - 2*TPCG_DXRDO       1130
C    *                                                                    1132
         ICOO=0                                                           1132
            GO TO 6043                                                    1132
6041        ICOO=ICOO+1                                                   1132
6043        IF(ICOO-(7).GT.0) GO TO 6042                                  1132
            DZ = TPCG_HEIGTUBE*CM/2                                       1133
            DY = TPCG_WIDTUBE*CM/2                                        1134
            Z = RCOOLINGTUBE                                              1135
            DX1 = (Z-DZ)*TAN15 - 2*TPCG_RDOCOOLINGDY                      1136
            DX2 = (Z+DZ)*TAN15 - 2*TPCG_RDOCOOLINGDY                      1137
            ISHAPE = KTRD1                                                1137
            MYPAR(1)=DX1                                                  1139
            MYPAR(2)=DX2                                                  1139
            MYPAR(3)=DY                                                   1139
            MYPAR(4)=DZ                                                   1139
            MYPAR(11) = Z                                                 1139
            MYPAR(13) = TPCG_ZRDO - 2*DY                                  1140
C       Create TCOO :                                                     1141
C       Call To Local Procedure TCOO                                      1141
            ASSIGN 6050 TO BLOCK_TCOO                                     1141
            GO TO  5630                                                   1141
6050        CONTINUE                                                      1141
C       *                                                                 1141
C       Geant POSITION : TCOO ===> X=MYPAR(11) Y=0 Z=MYPAR(13) ORT=YZX <  1142
C       ===                                                               1142
            CALL AGSRESET                                                 1142
            AG_X = MYPAR(11)                                              1142
            AG_Y = 0                                                      1142
            AG_Z = MYPAR(13)                                              1142
            AG_ALPHAN=AG_ALPHAN+1                                         1142
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              1142
            AG_TITLE = 'TCOO'                                             1142
            AG_EXNAME = 'POSITION'                                        1142
            AG_PARLIST = 'X_Y_Z_ORT'                                      1142
            CALL AXPOSITION                                               1142
C       *                                                                 1142
C       Check (ICOO .NE. 0 .AND. ICOO .NE. 6)                             1142
            IF ((ICOO .NE. 0 .AND. ICOO .NE. 6)) THEN                     1143
            MYPAR(11) = Z + DZ + TPCG_DXRDO                               1143
            MYPAR(13) = TPCG_ZRDO-TPCG_DZRDO                              1145
C       Create TRDC :                                                     1146
C       Call To Local Procedure TRDC                                      1146
            ASSIGN 6070 TO BLOCK_TRDC                                     1146
            GO TO  6060                                                   1146
6070        CONTINUE                                                      1146
C       *                                                                 1146
C       Geant POSITION : TRDC ===> X=MYPAR(11) Y=0 Z=MYPAR(13) <===       1147
            CALL AGSRESET                                                 1147
            AG_X = MYPAR(11)                                              1147
            AG_Y = 0                                                      1147
            AG_Z = MYPAR(13)                                              1147
            AG_TITLE = 'TRDC'                                             1147
            AG_EXNAME = 'POSITION'                                        1147
            AG_PARLIST = 'X_Y_Z'                                          1147
            CALL AXPOSITION                                               1147
C       *                                                                 1147
            ENDIF                                                         1147
            RCOOLINGTUBE=RCOOLINGTUBE-DRDOCOOLING(ICOO)                   1149
         GO TO 6041                                                       1150
6042     CONTINUE                                                         1150
5601  CALL AGSPOPB                                                        1151
      GO TO BLOCK_TRDO, (5600 ,5610)                                      1151
C End of Local Procedure                                                  1151
C ----------------------------------------------------------------------  1152
C ------------ TBRW                                                       1152
C Begin Local Procedure TBRW                                              1153
C TBRW WHEELRIBBOX                                                        1153
5450  CONTINUE                                                            1153
         AG_TITLE='TBRW'                                                  1153
         AG_MODULE=MODULE                                                 1153
         AG_MARK='V00'                                                    1153
         CALL AGSPUSH                                                     1153
         AG_MODULE=MODULE                                                 1153
C    *                                                                    1153
C    Geant ATTRIBUTE : TBRW ===> SEEN=1 COLO=KVIOLET <===                 1154
         CALL AGSRESET                                                    1154
         AG_SEEN = 1                                                      1154
         AG_COLO = KVIOLET                                                1154
         AG_TITLE = 'TBRW'                                                1154
         AG_EXNAME = 'ATTRIBUTE'                                          1154
         AG_PARLIST = 'SEEN_COLO'                                         1154
         CALL AXATTRIBUTE                                                 1154
C    *                                                                    1154
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1154
         CALL AGSRESET                                                    1155
         AG_TITLE = 'ALUMINIUM'                                           1155
         AG_EXNAME = 'MATERIAL'                                           1155
         AG_PARLIST = 'NONE'                                              1155
         CALL AXMATERIAL                                                  1155
C    *                                                                    1155
C    Geant SHAPE : BOX ===> DX=TPCG_WHEELBOXDY/2 DY=TPCG_WHEELBOXDX/2, D  1155
C    Z=TPCG_WHEELTHK/2 <===                                               1155
         CALL AGSRESET                                                    1157
         AG_DX = TPCG_WHEELBOXDY/2                                        1157
         AG_DY = TPCG_WHEELBOXDX/2                                        1157
         AG_DZ = TPCG_WHEELTHK/2                                          1157
         AG_TITLE = 'BOX'                                                 1157
         AG_EXNAME = 'SHAPE'                                              1157
         AG_PARLIST = 'DX_DY_DZ'                                          1157
         CALL AXSHAPE                                                     1157
C    *                                                                    1157
5451  CALL AGSPOPB                                                        1158
      GO TO BLOCK_TBRW, (5450 ,5460)                                      1158
C End of Local Procedure                                                  1158
C ----------------------------------------------------------------------  1159
C ------------ TWRB                                                       1159
C Begin Local Procedure TWRB                                              1160
C TWRB TPCWHEELRIB                                                        1160
5470  CONTINUE                                                            1160
         AG_TITLE='TWRB'                                                  1160
         AG_MODULE=MODULE                                                 1160
         AG_MARK='V00'                                                    1160
         CALL AGSPUSH                                                     1160
         AG_MODULE=MODULE                                                 1160
C    *                                                                    1160
C    Geant ATTRIBUTE : TWRB ===> SEEN=1 COLO=KYELLOW <===                 1161
         CALL AGSRESET                                                    1161
         AG_SEEN = 1                                                      1161
         AG_COLO = KYELLOW                                                1161
         AG_TITLE = 'TWRB'                                                1161
         AG_EXNAME = 'ATTRIBUTE'                                          1161
         AG_PARLIST = 'SEEN_COLO'                                         1161
         CALL AXATTRIBUTE                                                 1161
C    *                                                                    1161
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1161
         CALL AGSRESET                                                    1162
         AG_TITLE = 'ALUMINIUM'                                           1162
         AG_EXNAME = 'MATERIAL'                                           1162
         AG_PARLIST = 'NONE'                                              1162
         CALL AXMATERIAL                                                  1162
C    *                                                                    1162
C    Geant SHAPE : PARA ===> DX=MYDX DY=MYDY DZ=MYDZ ALPH=MYALPH, THET=0  1162
C     PHI=0 <===                                                          1162
         CALL AGSRESET                                                    1164
         AG_DX = MYDX                                                     1164
         AG_DY = MYDY                                                     1164
         AG_DZ = MYDZ                                                     1164
         AG_ALPH = MYALPH                                                 1164
         AG_THET = 0                                                      1164
         AG_PHI = 0                                                       1164
         AG_TITLE = 'PARA'                                                1164
         AG_EXNAME = 'SHAPE'                                              1164
         AG_PARLIST = 'DX_DY_DZ_ALPH_THET_PHI'                            1164
         CALL AXSHAPE                                                     1164
C    *                                                                    1164
5471  CALL AGSPOPB                                                        1165
      GO TO BLOCK_TWRB, (5470 ,5480,5490)                                 1165
C End of Local Procedure                                                  1165
C ----------------------------------------------------------------------  1166
C ------------ TCOO                                                       1166
C Begin Local Procedure TCOO                                              1167
C TCOO COOLINGTUBE                                                        1167
5630  CONTINUE                                                            1167
         AG_TITLE='TCOO'                                                  1167
         AG_MODULE=MODULE                                                 1167
         AG_MARK='V00'                                                    1167
         CALL AGSPUSH                                                     1167
         AG_MODULE=MODULE                                                 1167
C    *                                                                    1167
C    Geant ATTRIBUTE : TCOO ===> SEEN=1 COLO=KRED <===                    1168
         CALL AGSRESET                                                    1168
         AG_SEEN = 1                                                      1168
         AG_COLO = KRED                                                   1168
         AG_TITLE = 'TCOO'                                                1168
         AG_EXNAME = 'ATTRIBUTE'                                          1168
         AG_PARLIST = 'SEEN_COLO'                                         1168
         CALL AXATTRIBUTE                                                 1168
C    *                                                                    1168
C    Geant MATERIAL : WATER_PIPE ===> inherited arguments used <===       1168
         CALL AGSRESET                                                    1169
         AG_TITLE = 'WATER_PIPE'                                          1169
         AG_EXNAME = 'MATERIAL'                                           1169
         AG_PARLIST = 'NONE'                                              1169
         CALL AXMATERIAL                                                  1169
C    *                                                                    1169
C    Check ISHAPE==KPARA                                                  1169
         IF (ISHAPE.EQ.KPARA) THEN                                        1170
C    Geant SHAPE : PARA ===> DX=MYPAR(1) DY=MYPAR(2) DZ=MYPAR(3), ALPH=M  1172
C    YPAR(4) THET=0 PHI=0 <===                                            1172
         CALL AGSRESET                                                    1172
         AG_DX = MYPAR(1)                                                 1172
         AG_DY = MYPAR(2)                                                 1172
         AG_DZ = MYPAR(3)                                                 1172
         AG_ALPH = MYPAR(4)                                               1172
         AG_THET = 0                                                      1172
         AG_PHI = 0                                                       1172
         AG_TITLE = 'PARA'                                                1172
         AG_EXNAME = 'SHAPE'                                              1172
         AG_PARLIST = 'DX_DY_DZ_ALPH_THET_PHI'                            1172
         CALL AXSHAPE                                                     1172
C    *                                                                    1172
         ENDIF                                                            1172
C    Check ISHAPE==KTRD1                                                  1174
         IF (ISHAPE.EQ.KTRD1) THEN                                        1174
C    Geant SHAPE : TRD1 ===> DX1=MYPAR(1) DX2=MYPAR(2) DY=MYPAR(3), DZ=M  1176
C    YPAR(4) <===                                                         1176
         CALL AGSRESET                                                    1176
         AG_DX1 = MYPAR(1)                                                1176
         AG_DX2 = MYPAR(2)                                                1176
         AG_DY = MYPAR(3)                                                 1176
         AG_DZ = MYPAR(4)                                                 1176
         AG_TITLE = 'TRD1'                                                1176
         AG_EXNAME = 'SHAPE'                                              1176
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     1176
         CALL AXSHAPE                                                     1176
C    *                                                                    1176
         ENDIF                                                            1176
5631  CALL AGSPOPB                                                        1178
      GO TO BLOCK_TCOO, (5630 ,5640,5810,5830,6020,6030,6050)             1178
C End of Local Procedure                                                  1178
C ----------------------------------------------------------------------  1179
C ------------ TCAB                                                       1179
C Begin Local Procedure TCAB                                              1180
C TCAB CABLES                                                             1180
5660  CONTINUE                                                            1180
         AG_TITLE='TCAB'                                                  1180
         AG_MODULE=MODULE                                                 1180
         AG_MARK='V00'                                                    1180
         CALL AGSPUSH                                                     1180
         AG_MODULE=MODULE                                                 1180
C    *                                                                    1180
C    Geant ATTRIBUTE : TCAB ===> SEEN=1 COLO=KGREEN <===                  1181
         CALL AGSRESET                                                    1181
         AG_SEEN = 1                                                      1181
         AG_COLO = KGREEN                                                 1181
         AG_TITLE = 'TCAB'                                                1181
         AG_EXNAME = 'ATTRIBUTE'                                          1181
         AG_PARLIST = 'SEEN_COLO'                                         1181
         CALL AXATTRIBUTE                                                 1181
C    *                                                                    1181
C    Geant MATERIAL : CABLES ===> inherited arguments used <===           1181
         CALL AGSRESET                                                    1182
         AG_TITLE = 'CABLES'                                              1182
         AG_EXNAME = 'MATERIAL'                                           1182
         AG_PARLIST = 'NONE'                                              1182
         CALL AXMATERIAL                                                  1182
C    *                                                                    1182
C    Geant SHAPE : PARA ===> DX=MYPAR(1) DY=MYPAR(2) DZ=MYPAR(3), ALPH=M  1182
C    YPAR(4) THET=0 PHI=0 <===                                            1182
         CALL AGSRESET                                                    1184
         AG_DX = MYPAR(1)                                                 1184
         AG_DY = MYPAR(2)                                                 1184
         AG_DZ = MYPAR(3)                                                 1184
         AG_ALPH = MYPAR(4)                                               1184
         AG_THET = 0                                                      1184
         AG_PHI = 0                                                       1184
         AG_TITLE = 'PARA'                                                1184
         AG_EXNAME = 'SHAPE'                                              1184
         AG_PARLIST = 'DX_DY_DZ_ALPH_THET_PHI'                            1184
         CALL AXSHAPE                                                     1184
C    *                                                                    1184
5661  CALL AGSPOPB                                                        1185
      GO TO BLOCK_TCAB, (5660 ,5670)                                      1185
C End of Local Procedure                                                  1185
C ----------------------------------------------------------------------  1186
C ------------ TRDC                                                       1186
C Begin Local Procedure TRDC                                              1187
C TRDC RDOCARD                                                            1187
6060  CONTINUE                                                            1187
         AG_TITLE='TRDC'                                                  1187
         AG_MODULE=MODULE                                                 1187
         AG_MARK='V00'                                                    1187
         CALL AGSPUSH                                                     1187
         AG_MODULE=MODULE                                                 1187
C    *                                                                    1187
C    Geant ATTRIBUTE : TRDC ===> SEEN=1 COLO=KBLUE <===                   1188
         CALL AGSRESET                                                    1188
         AG_SEEN = 1                                                      1188
         AG_COLO = KBLUE                                                  1188
         AG_TITLE = 'TRDC'                                                1188
         AG_EXNAME = 'ATTRIBUTE'                                          1188
         AG_PARLIST = 'SEEN_COLO'                                         1188
         CALL AXATTRIBUTE                                                 1188
C    *                                                                    1188
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1188
         CALL AGSRESET                                                    1189
         AG_TITLE = 'G10'                                                 1189
         AG_EXNAME = 'MATERIAL'                                           1189
         AG_PARLIST = 'NONE'                                              1189
         CALL AXMATERIAL                                                  1189
C    *                                                                    1189
C    Geant SHAPE : BOX ===> DX=TPCG_DXRDO DY=TPCG_DYRDO DZ=TPCG_DZRDO <=  1189
C    ==                                                                   1189
         CALL AGSRESET                                                    1190
         AG_DX = TPCG_DXRDO                                               1190
         AG_DY = TPCG_DYRDO                                               1190
         AG_DZ = TPCG_DZRDO                                               1190
         AG_TITLE = 'BOX'                                                 1190
         AG_EXNAME = 'SHAPE'                                              1190
         AG_PARLIST = 'DX_DY_DZ'                                          1190
         CALL AXSHAPE                                                     1190
C    *                                                                    1190
6061  CALL AGSPOPB                                                        1191
      GO TO BLOCK_TRDC, (6060 ,6070)                                      1191
C End of Local Procedure                                                  1191
C ----------------------------------------------------------------------  1192
C ------------ TRIB                                                       1192
C Begin Local Procedure TRIB                                              1193
C TRIB TPC RIBS                                                           1193
5730  CONTINUE                                                            1193
         AG_TITLE='TRIB'                                                  1193
         AG_MODULE=MODULE                                                 1193
         AG_MARK='V00'                                                    1193
         CALL AGSPUSH                                                     1193
         AG_MODULE=MODULE                                                 1193
C    *                                                                    1193
C    Geant ATTRIBUTE : TRIB ===> SEEN=1 COLO=KRED <===                    1194
         CALL AGSRESET                                                    1194
         AG_SEEN = 1                                                      1194
         AG_COLO = KRED                                                   1194
         AG_TITLE = 'TRIB'                                                1194
         AG_EXNAME = 'ATTRIBUTE'                                          1194
         AG_PARLIST = 'SEEN_COLO'                                         1194
         CALL AXATTRIBUTE                                                 1194
C    *                                                                    1194
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1194
         CALL AGSRESET                                                    1195
         AG_TITLE = 'ALUMINIUM'                                           1195
         AG_EXNAME = 'MATERIAL'                                           1195
         AG_PARLIST = 'NONE'                                              1195
         CALL AXMATERIAL                                                  1195
C    *                                                                    1195
C    Check ISHAPE==KPARA                                                  1195
         IF (ISHAPE.EQ.KPARA) THEN                                        1196
C    Geant SHAPE : PARA ===> DX=MYPAR(1) DY=MYPAR(2) DZ=MYPAR(3), ALPH=M  1198
C    YPAR(4) THET=0 PHI=0 <===                                            1198
         CALL AGSRESET                                                    1198
         AG_DX = MYPAR(1)                                                 1198
         AG_DY = MYPAR(2)                                                 1198
         AG_DZ = MYPAR(3)                                                 1198
         AG_ALPH = MYPAR(4)                                               1198
         AG_THET = 0                                                      1198
         AG_PHI = 0                                                       1198
         AG_TITLE = 'PARA'                                                1198
         AG_EXNAME = 'SHAPE'                                              1198
         AG_PARLIST = 'DX_DY_DZ_ALPH_THET_PHI'                            1198
         CALL AXSHAPE                                                     1198
C    *                                                                    1198
         ELSEIF (ISHAPE.EQ.KTRD1) THEN                                    1198
C    Geant SHAPE : TRD1 ===> DX1=MYPAR(1) DX2=MYPAR(2) DY=MYPAR(3), DZ=M  1201
C    YPAR(4) <===                                                         1201
         CALL AGSRESET                                                    1201
         AG_DX1 = MYPAR(1)                                                1201
         AG_DX2 = MYPAR(2)                                                1201
         AG_DY = MYPAR(3)                                                 1201
         AG_DZ = MYPAR(4)                                                 1201
         AG_TITLE = 'TRD1'                                                1201
         AG_EXNAME = 'SHAPE'                                              1201
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     1201
         CALL AXSHAPE                                                     1201
C    *                                                                    1201
         ELSEIF (ISHAPE.EQ.KBBOX) THEN                                    1201
C    Geant SHAPE : BOX ===> DX=MYPAR(1) DY=MYPAR(2) DZ=MYPAR(3) <===      1203
         CALL AGSRESET                                                    1203
         AG_DX = MYPAR(1)                                                 1203
         AG_DY = MYPAR(2)                                                 1203
         AG_DZ = MYPAR(3)                                                 1203
         AG_TITLE = 'BOX'                                                 1203
         AG_EXNAME = 'SHAPE'                                              1203
         AG_PARLIST = 'DX_DY_DZ'                                          1203
         CALL AXSHAPE                                                     1203
C    *                                                                    1203
         ENDIF                                                            1203
5731  CALL AGSPOPB                                                        1205
      GO TO BLOCK_TRIB, (5730 ,5740,5780,5790)                            1205
C End of Local Procedure                                                  1205
C ----------------------------------------------------------------------  1206
C ------------ TWIR                                                       1206
C Begin Local Procedure TWIR                                              1207
C TWIR MOUNT FOR PADROW WIRES                                             1207
5750  CONTINUE                                                            1207
         AG_TITLE='TWIR'                                                  1207
         AG_MODULE=MODULE                                                 1207
         AG_MARK='V00'                                                    1207
         CALL AGSPUSH                                                     1207
         AG_MODULE=MODULE                                                 1207
C    *                                                                    1207
C    Geant ATTRIBUTE : TWIR ===> SEEN=1 COLO=KYELLOW <===                 1208
         CALL AGSRESET                                                    1208
         AG_SEEN = 1                                                      1208
         AG_COLO = KYELLOW                                                1208
         AG_TITLE = 'TWIR'                                                1208
         AG_EXNAME = 'ATTRIBUTE'                                          1208
         AG_PARLIST = 'SEEN_COLO'                                         1208
         CALL AXATTRIBUTE                                                 1208
C    *                                                                    1208
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1208
         CALL AGSRESET                                                    1209
         AG_TITLE = 'G10'                                                 1209
         AG_EXNAME = 'MATERIAL'                                           1209
         AG_PARLIST = 'NONE'                                              1209
         CALL AXMATERIAL                                                  1209
C    *                                                                    1209
C    Geant SHAPE : PARA ===> DX=MYPAR(1) DY=MYPAR(2) DZ=MYPAR(3), ALPH=M  1209
C    YPAR(4) THET=0 PHI=0 <===                                            1209
         CALL AGSRESET                                                    1211
         AG_DX = MYPAR(1)                                                 1211
         AG_DY = MYPAR(2)                                                 1211
         AG_DZ = MYPAR(3)                                                 1211
         AG_ALPH = MYPAR(4)                                               1211
         AG_THET = 0                                                      1211
         AG_PHI = 0                                                       1211
         AG_TITLE = 'PARA'                                                1211
         AG_EXNAME = 'SHAPE'                                              1211
         AG_PARLIST = 'DX_DY_DZ_ALPH_THET_PHI'                            1211
         CALL AXSHAPE                                                     1211
C    *                                                                    1211
5751  CALL AGSPOPB                                                        1212
      GO TO BLOCK_TWIR, (5750 ,5760)                                      1212
C End of Local Procedure                                                  1212
C ----------------------------------------------------------------------  1213
C ------------ THOL                                                       1213
C Begin Local Procedure THOL                                              1214
C THOL THE SECTOR HOLES FOR FEE                                           1214
5950  CONTINUE                                                            1214
         AG_TITLE='THOL'                                                  1214
         AG_MODULE=MODULE                                                 1214
         AG_MARK='V00'                                                    1214
         CALL AGSPUSH                                                     1214
         AG_MODULE=MODULE                                                 1214
C    *                                                                    1214
C    Geant ATTRIBUTE : THOL ===> SEEN=1 COLO=KGREEN <===                  1215
         CALL AGSRESET                                                    1215
         AG_SEEN = 1                                                      1215
         AG_COLO = KGREEN                                                 1215
         AG_TITLE = 'THOL'                                                1215
         AG_EXNAME = 'ATTRIBUTE'                                          1215
         AG_PARLIST = 'SEEN_COLO'                                         1215
         CALL AXATTRIBUTE                                                 1215
C    *                                                                    1215
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1215
         CALL AGSRESET                                                    1216
         AG_TITLE = 'AIR'                                                 1216
         AG_EXNAME = 'MATERIAL'                                           1216
         AG_PARLIST = 'NONE'                                              1216
         CALL AXMATERIAL                                                  1216
C    *                                                                    1216
C    Geant SHAPE : BOX ===> DX=MYPAR(1) DY=MYPAR(2) DZ=MYPAR(3) <===      1216
         CALL AGSRESET                                                    1217
         AG_DX = MYPAR(1)                                                 1217
         AG_DY = MYPAR(2)                                                 1217
         AG_DZ = MYPAR(3)                                                 1217
         AG_TITLE = 'BOX'                                                 1217
         AG_EXNAME = 'SHAPE'                                              1217
         AG_PARLIST = 'DX_DY_DZ'                                          1217
         CALL AXSHAPE                                                     1217
C    *                                                                    1217
C    Create FEES :                                                        1218
C    Call To Local Procedure FEES                                         1218
         ASSIGN 6090 TO BLOCK_FEES                                        1218
         GO TO  6080                                                      1218
6090     CONTINUE                                                         1218
C    *                                                                    1218
C    Geant POSITION : FEES ===> inherited arguments used <===             1219
         CALL AGSRESET                                                    1219
         AG_TITLE = 'FEES'                                                1219
         AG_EXNAME = 'POSITION'                                           1219
         AG_PARLIST = 'NONE'                                              1219
         CALL AXPOSITION                                                  1219
C    *                                                                    1219
5951  CALL AGSPOPB                                                        1220
      GO TO BLOCK_THOL, (5950 ,5960)                                      1220
C End of Local Procedure                                                  1220
C ----------------------------------------------------------------------  1221
C ------------ FEES                                                       1221
C Begin Local Procedure FEES                                              1222
C FEES TFEESOCKET                                                         1222
6080  CONTINUE                                                            1222
         AG_TITLE='FEES'                                                  1222
         AG_MODULE=MODULE                                                 1222
         AG_MARK='V00'                                                    1222
         CALL AGSPUSH                                                     1222
         AG_MODULE=MODULE                                                 1222
C    *                                                                    1222
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1223
         CALL AGSRESET                                                    1223
         AG_TITLE = 'G10'                                                 1223
         AG_EXNAME = 'MATERIAL'                                           1223
         AG_PARLIST = 'NONE'                                              1223
         CALL AXMATERIAL                                                  1223
C    *                                                                    1223
C    Geant ATTRIBUTE : FEES ===> SEEN=1 COLO=KBLUE <===                   1223
         CALL AGSRESET                                                    1224
         AG_SEEN = 1                                                      1224
         AG_COLO = KBLUE                                                  1224
         AG_TITLE = 'FEES'                                                1224
         AG_EXNAME = 'ATTRIBUTE'                                          1224
         AG_PARLIST = 'SEEN_COLO'                                         1224
         CALL AXATTRIBUTE                                                 1224
C    *                                                                    1224
C    Geant SHAPE : BOX ===> DX=6.2/20 DY=57./20 DZ=5.8/20 <===            1224
         CALL AGSRESET                                                    1225
         AG_DX = 6.2/20                                                   1225
         AG_DY = 57./20                                                   1225
         AG_DZ = 5.8/20                                                   1225
         AG_TITLE = 'BOX'                                                 1225
         AG_EXNAME = 'SHAPE'                                              1225
         AG_PARLIST = 'DX_DY_DZ'                                          1225
         CALL AXSHAPE                                                     1225
C    *                                                                    1225
6081  CALL AGSPOPB                                                        1226
      GO TO BLOCK_FEES, (6080 ,6090)                                      1226
C End of Local Procedure                                                  1226
C ----------------------------------------------------------------------  1227
C ------------ TPMV                                                       1227
C Begin Local Procedure TPMV                                              1228
C TPMV IS THE TPAD MOTHER VOLUME                                          1228
5340  CONTINUE                                                            1228
         AG_TITLE='TPMV'                                                  1228
         AG_MODULE=MODULE                                                 1228
         AG_MARK='V00'                                                    1228
         CALL AGSPUSH                                                     1228
         AG_MODULE=MODULE                                                 1228
C    *                                                                    1228
C    Geant ATTRIBUTE : TPMV ===> SEEN=0 COLO=KBLUE <===                   1229
         CALL AGSRESET                                                    1229
         AG_SEEN = 0                                                      1229
         AG_COLO = KBLUE                                                  1229
         AG_TITLE = 'TPMV'                                                1229
         AG_EXNAME = 'ATTRIBUTE'                                          1229
         AG_PARLIST = 'SEEN_COLO'                                         1229
         CALL AXATTRIBUTE                                                 1229
C    *                                                                    1229
C    Geant SHAPE : PGON ===> PHI1=-15.00 DPHI=30.0 NPDIV=1, NZ=4 ZI={0.0  1229
C    ,210.9,210.9,211.3} RMN={TIFCOR, TIFCOR,121.0,121.0} RMX={TOFCIR*CO  1229
C    S15,TOFCIR*COS15, TOFCIR*COS15,TOFCIR*COS15} <===                    1229
         CALL AGSRESET                                                    1233
         AG_PHI1 = -15.00                                                 1233
         AG_DPHI = 30.0                                                   1233
         AG_NPDIV = 1                                                     1233
         AG_NZ = 4                                                        1233
         AG_ZI(1)=0.0                                                     1233
         AG_ZI(2)=210.9                                                   1233
         AG_ZI(3)=210.9                                                   1233
         AG_ZI(4)=211.3                                                   1233
         AG_RMN(1)=TIFCOR                                                 1233
         AG_RMN(2)=TIFCOR                                                 1233
         AG_RMN(3)=121.0                                                  1233
         AG_RMN(4)=121.0                                                  1233
         AG_RMX(1)=TOFCIR*COS15                                           1233
         AG_RMX(2)=TOFCIR*COS15                                           1233
         AG_RMX(3)=TOFCIR*COS15                                           1233
         AG_RMX(4)=TOFCIR*COS15                                           1233
         AG_TITLE = 'PGON'                                                1233
         AG_EXNAME = 'SHAPE'                                              1233
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1233
         CALL AXSHAPE                                                     1233
C    *                                                                    1233
C    *                                                                    1233
         KASE=1                                                           1234
            GO TO 6103                                                    1234
6101        KASE=KASE+1                                                   1234
6103        IF(KASE-(2).GT.0) GO TO 6102                                  1234
C       *                                                                 1235
            I_SEC=1                                                       1235
               GO TO 6113                                                 1235
6111           I_SEC=I_SEC+1                                              1235
6113           IF(I_SEC-(2).GT.0) GO TO 6112                              1235
C          use TPRS bank                                                  1236
               BPATH_TPRS='TPRS*'                                         1236
               IDNUM_TPRS(1)=0                                            1236
               TPRS_SEC=I_SEC                                             1236
               CALL AGDGETP('TPCEGEO4','*',-999,'TPRS',NTPRS_SEC,         1236
     *         RE_TPRS_SEC,BPATH_TPRS,IDNUM_TPRS,1,MAP_TPRS,NAMES_TPRS,   1236
     *         BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)          1236
C          use TECW bank                                                  1237
               BPATH_TECW='TECW*'                                         1237
               IDNUM_TECW(1)=0                                            1237
               TECW_SEC=I_SEC                                             1237
               CALL AGDGETP('TPCEGEO4','*',-999,'TECW',NTECW_SEC,         1237
     *         RE_TECW_SEC,BPATH_TECW,IDNUM_TECW,1,MAP_TECW,NAMES_TECW,   1237
     *         BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)          1237
               ZWHEEL1 = TPCG_LENGTHW/2 - TPCG_WHEELTHK                   1238
               ZBEG = TPCG_MEMBTHK/2                                      1239
               ZEND = TPCG_ZGROUNDGRID + TPRS_DANODE*2                    1240
               ZDED = TPCG_ZGATINGGRID - TPCG_DEADZONE                    1241
               ZPMT = TPCG_ZGATINGGRID                                    1242
               DX = TPRS_WIDTH/2                                          1243
C          Check KASE==1                                                  1244
               IF (KASE.EQ.1) THEN                                        1244
               DZ = (ZDED - ZBEG)/2                                       1244
               Z = (ZDED + ZBEG)/2                                        1245
               ENDIF                                                      1246
C          Check KASE==2                                                  1247
               IF (KASE.EQ.2) THEN                                        1247
               DZ = (ZEND - ZPMT)/2                                       1247
               Z = (ZEND + ZPMT)/2                                        1248
               ENDIF                                                      1249
C          *                                                              1250
               I_ROW=1                                                    1250
                  GO TO 6123                                              1250
6121              I_ROW=I_ROW+1                                           1250
6123              IF(I_ROW-(NINT(TPRS_NROW)).GT.0) GO TO 6122             1250
C             Check (NINT(TPRS_SUPER)==3|I_ROW==1)                        1251
                  IF ((NINT(TPRS_SUPER).EQ.3.OR.I_ROW.EQ.1)) THEN         1251
                  DY=TPRS_NPADS(I_ROW)*TPRS_PITCH/2                       1252
                  X=TPRS_RPADS(I_ROW)-TPRS_WIDTH                          1253
C             Create TPAD :                                               1254
C             Call To Local Procedure TPAD                                1254
                  ASSIGN 6140 TO BLOCK_TPAD                               1254
                  GO TO  6130                                             1254
6140              CONTINUE                                                1254
C             *                                                           1254
C             Geant POSITION : TPAD ===> X=X Z=Z DZ=DZ DX=DX DY=DY <===   1255
                  CALL AGSRESET                                           1255
                  AG_X = X                                                1255
                  AG_Z = Z                                                1255
                  AG_DZ = DZ                                              1255
                  AG_DX = DX                                              1255
                  AG_DY = DY                                              1255
                  AG_TITLE = 'TPAD'                                       1255
                  AG_EXNAME = 'POSITION'                                  1255
                  AG_PARLIST = 'X_Z_DZ_DX_DY'                             1255
                  CALL AXPOSITION                                         1255
C             *                                                           1255
                  ENDIF                                                   1255
                  DY=TPRS_NPADS(I_ROW)*TPRS_PITCH/2                       1257
                  X=TPRS_RPADS(I_ROW)                                     1258
C             Create TPAD :                                               1259
C             Call To Local Procedure TPAD                                1259
                  ASSIGN 6150 TO BLOCK_TPAD                               1259
                  GO TO  6130                                             1259
6150              CONTINUE                                                1259
C             *                                                           1259
C             Geant POSITION : TPAD ===> X=X Z=Z DZ=DZ DX=DX DY=DY <===   1260
                  CALL AGSRESET                                           1260
                  AG_X = X                                                1260
                  AG_Z = Z                                                1260
                  AG_DZ = DZ                                              1260
                  AG_DX = DX                                              1260
                  AG_DY = DY                                              1260
                  AG_TITLE = 'TPAD'                                       1260
                  AG_EXNAME = 'POSITION'                                  1260
                  AG_PARLIST = 'X_Z_DZ_DX_DY'                             1260
                  CALL AXPOSITION                                         1260
C             *                                                           1260
C             Check (NINT(TPRS_SUPER)==3|I_ROW==NINT(TPRS_NROW))          1260
                  IF                                                      1261
     *            ((NINT(TPRS_SUPER).EQ.3.OR.I_ROW.EQ.NINT(TPRS_NROW)))   1261
     *            THEN                                                    1261
                  X=TPRS_RPADS(I_ROW)+TPRS_WIDTH                          1262
                  DY=TPRS_NPADS(I_ROW)*TPRS_PITCH/2                       1263
C             Create TPAD :                                               1264
C             Call To Local Procedure TPAD                                1264
                  ASSIGN 6160 TO BLOCK_TPAD                               1264
                  GO TO  6130                                             1264
6160              CONTINUE                                                1264
C             *                                                           1264
C             Geant POSITION : TPAD ===> X=X Z=Z DZ=DZ DX=DX DY=DY <===   1265
                  CALL AGSRESET                                           1265
                  AG_X = X                                                1265
                  AG_Z = Z                                                1265
                  AG_DZ = DZ                                              1265
                  AG_DX = DX                                              1265
                  AG_DY = DY                                              1265
                  AG_TITLE = 'TPAD'                                       1265
                  AG_EXNAME = 'POSITION'                                  1265
                  AG_PARLIST = 'X_Z_DZ_DX_DY'                             1265
                  CALL AXPOSITION                                         1265
C             *                                                           1265
                  ENDIF                                                   1265
               GO TO 6121                                                 1267
6122           CONTINUE                                                   1267
            GO TO 6111                                                    1268
6112        CONTINUE                                                      1268
         GO TO 6101                                                       1269
6102     CONTINUE                                                         1269
5341  CALL AGSPOPB                                                        1270
      GO TO BLOCK_TPMV, (5340 ,5350)                                      1270
C End of Local Procedure                                                  1270
C ----------------------------------------------------------------------  1271
C ------------ TPAD                                                       1271
C Begin Local Procedure TPAD                                              1272
C TPAD IS A REAL PADROW WITH DIMENSIONS DEFINED AT POSITIONING TIME       1272
6130  CONTINUE                                                            1272
         AG_TITLE='TPAD'                                                  1272
         AG_MODULE=MODULE                                                 1272
         AG_MARK='V00'                                                    1272
         CALL AGSPUSH                                                     1272
         AG_MODULE=MODULE                                                 1272
C    *                                                                    1272
C    *                                                                    1273
C       Check TPRS_WIDTH<1.5                                              1273
            IF (TPRS_WIDTH.LT.1.5) THEN                                   1273
C       Geant ATTRIBUTE : TPAD ===> SEEN=1 COLO=2 SERIAL=1 <===           1274
            CALL AGSRESET                                                 1274
            AG_SEEN = 1                                                   1274
            AG_COLO = 2                                                   1274
            AG_SERIAL = 1                                                 1274
            AG_TITLE = 'TPAD'                                             1274
            AG_EXNAME = 'ATTRIBUTE'                                       1274
            AG_PARLIST = 'SEEN_COLO_SERIAL'                               1274
            CALL AXATTRIBUTE                                              1274
C       *                                                                 1274
         END IF                                                           1275
C    *                                                                    1276
C       Check TPRS_WIDTH>1.5                                              1276
            IF (TPRS_WIDTH.GT.1.5) THEN                                   1276
C       Geant ATTRIBUTE : TPAD ===> SEEN=1 COLO=2 SERIAL=2 <===           1277
            CALL AGSRESET                                                 1277
            AG_SEEN = 1                                                   1277
            AG_COLO = 2                                                   1277
            AG_SERIAL = 2                                                 1277
            AG_TITLE = 'TPAD'                                             1277
            AG_EXNAME = 'ATTRIBUTE'                                       1277
            AG_PARLIST = 'SEEN_COLO_SERIAL'                               1277
            CALL AXATTRIBUTE                                              1277
C       *                                                                 1277
         END IF                                                           1278
C    Geant MATERIAL : P10 ===> inherited arguments used <===              1279
         CALL AGSRESET                                                    1279
         AG_TITLE = 'P10'                                                 1279
         AG_EXNAME = 'MATERIAL'                                           1279
         AG_PARLIST = 'NONE'                                              1279
         CALL AXMATERIAL                                                  1279
C    *                                                                    1279
C    Geant MATERIAL : SENSITIVE_GAS ===> ISVOL=1 STEMAX=2.5*TPRS_WIDTH <  1279
C    ===                                                                  1279
         CALL AGSRESET                                                    1280
         AG_ISVOL = 1                                                     1280
         AG_STEMAX = 2.5*TPRS_WIDTH                                       1280
         AG_TITLE = 'SENSITIVE_GAS'                                       1280
         AG_EXNAME = 'MATERIAL'                                           1280
         AG_PARLIST = 'ISVOL_STEMAX'                                      1280
         CALL AXMATERIAL                                                  1280
C    *                                                                    1280
C    Geant MEDIUM : TPAD_P10 ===> STEMAX=2.5*TPRS_WIDTH DEEMAX=0.0017536  1280
C    7 STMIN=0.0249056 ISVOL=1 <===                                       1280
         CALL AGSRESET                                                    1281
         AG_STEMAX = 2.5*TPRS_WIDTH                                       1281
         AG_DEEMAX = 0.00175367                                           1281
         AG_STMIN = 0.0249056                                             1281
         AG_ISVOL = 1                                                     1281
         AG_TITLE = 'TPAD_P10'                                            1281
         AG_EXNAME = 'MEDIUM'                                             1281
         AG_PARLIST = 'STEMAX_DEEMAX_STMIN_ISVOL'                         1281
         CALL AXMEDIUM                                                    1281
C    *                                                                    1281
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           1281
         CALL AGSRESET                                                    1282
         AG_DX = 0                                                        1282
         AG_DY = 0                                                        1282
         AG_DZ = 0                                                        1282
         AG_TITLE = 'BOX'                                                 1282
         AG_EXNAME = 'SHAPE'                                              1282
         AG_PARLIST = 'DX_DY_DZ'                                          1282
         CALL AXSHAPE                                                     1282
C    *                                                                    1282
         CALL GSTPAR(AG_IMED, 'CUTGAM',1E-4)                              1282
         CALL GSTPAR(AG_IMED, 'CUTELE',1E-4)                              1284
         CALL GSTPAR(AG_IMED, 'DCUTE',1E-4)                               1285
         CALL GSTPAR(AG_IMED, 'DCUTM',1E-4)                               1286
         CALL GSTPAR(AG_IMED, 'STRA', 1.)                                 1287
C    Defining Hits: Z:0.0005:S Y:0.0005: X:0.0005: CX:10: CY:10: CZ:10:   1289
C    LPTOT:18:(-3,2), SLENG:0.1:(0,800) TOF:16:(0,TPCG_TOFMAX) LGAM:16:(  1289
C    -2,+2) STEP:11:(0,10) USER:21:(-0.01,0.01)                           1289
         AG_TITLE = 'TPAD'                                                1289
         AG_EXNAME = 'HITS'                                               1289
         AG_TYPE=AG_MAGIC                                                 1289
         CALL AGSDETV('TPAD')                                             1289
C    *                                                                    1289
         AG_PARLIST='Z'                                                   1289
         AG_TITLE = 'DETH'                                                1289
         AG_NBITS = AG_MAGIC                                              1289
         AG_BIN=0.0005                                                    1289
         AG_OPTION='S'                                                    1289
         AG_FHMIN = AG_MAGIC                                              1289
         AG_FHMAX = AG_MAGIC                                              1289
         CALL AGSDETH (0)                                                 1289
C    *                                                                    1289
         AG_PARLIST='Y'                                                   1289
         AG_TITLE = 'DETH'                                                1289
         AG_NBITS = AG_MAGIC                                              1289
         AG_BIN=0.0005                                                    1289
         AG_FHMIN=AG_MAGIC                                                1289
         AG_FHMAX=AG_MAGIC                                                1289
         AG_OPTION='*'                                                    1289
         CALL AGSDETH (0)                                                 1289
C    *                                                                    1289
         AG_PARLIST='X'                                                   1289
         AG_TITLE = 'DETH'                                                1289
         AG_NBITS = AG_MAGIC                                              1289
         AG_BIN=0.0005                                                    1289
         AG_FHMIN=AG_MAGIC                                                1289
         AG_FHMAX=AG_MAGIC                                                1289
         AG_OPTION='*'                                                    1289
         CALL AGSDETH (0)                                                 1289
C    *                                                                    1289
         AG_PARLIST='CX'                                                  1289
         AG_TITLE = 'DETH'                                                1289
         AG_NBITS = 10                                                    1289
         AG_BIN=AG_MAGIC                                                  1289
         AG_FHMIN=AG_MAGIC                                                1289
         AG_FHMAX=AG_MAGIC                                                1289
         AG_OPTION='*'                                                    1289
         CALL AGSDETH (0)                                                 1289
C    *                                                                    1289
         AG_PARLIST='CY'                                                  1289
         AG_TITLE = 'DETH'                                                1289
         AG_NBITS = 10                                                    1289
         AG_BIN=AG_MAGIC                                                  1289
         AG_FHMIN=AG_MAGIC                                                1289
         AG_FHMAX=AG_MAGIC                                                1289
         AG_OPTION='*'                                                    1289
         CALL AGSDETH (0)                                                 1289
C    *                                                                    1289
         AG_PARLIST='CZ'                                                  1289
         AG_TITLE = 'DETH'                                                1289
         AG_NBITS = 10                                                    1289
         AG_BIN=AG_MAGIC                                                  1289
         AG_FHMIN=AG_MAGIC                                                1289
         AG_FHMAX=AG_MAGIC                                                1289
         AG_OPTION='*'                                                    1289
         CALL AGSDETH (0)                                                 1289
C    *                                                                    1289
         AG_PARLIST='LPTOT'                                               1289
         AG_TITLE = 'DETH'                                                1289
         AG_NBITS = 18                                                    1289
         AG_BIN=AG_MAGIC                                                  1289
         AG_OPTION='*'                                                    1289
         AG_FHMIN = -3                                                    1289
         AG_FHMAX = 2                                                     1289
         CALL AGSDETH (0)                                                 1289
C    *                                                                    1289
         AG_PARLIST='SLENG'                                               1289
         AG_TITLE = 'DETH'                                                1289
         AG_NBITS = AG_MAGIC                                              1289
         AG_BIN=0.1                                                       1289
         AG_OPTION='*'                                                    1289
         AG_FHMIN = 0                                                     1289
         AG_FHMAX = 800                                                   1289
         CALL AGSDETH (0)                                                 1289
C    *                                                                    1289
         AG_PARLIST='TOF'                                                 1289
         AG_TITLE = 'DETH'                                                1289
         AG_NBITS = 16                                                    1289
         AG_BIN=AG_MAGIC                                                  1289
         AG_OPTION='*'                                                    1289
         AG_FHMIN = 0                                                     1289
         AG_FHMAX = TPCG_TOFMAX                                           1289
         CALL AGSDETH (0)                                                 1289
C    *                                                                    1289
         AG_PARLIST='LGAM'                                                1289
         AG_TITLE = 'DETH'                                                1289
         AG_NBITS = 16                                                    1289
         AG_BIN=AG_MAGIC                                                  1289
         AG_OPTION='*'                                                    1289
         AG_FHMIN = -2                                                    1289
         AG_FHMAX = +2                                                    1289
         CALL AGSDETH (0)                                                 1289
C    *                                                                    1289
         AG_PARLIST='STEP'                                                1289
         AG_TITLE = 'DETH'                                                1289
         AG_NBITS = 11                                                    1289
         AG_BIN=AG_MAGIC                                                  1289
         AG_OPTION='*'                                                    1289
         AG_FHMIN = 0                                                     1289
         AG_FHMAX = 10                                                    1289
         CALL AGSDETH (0)                                                 1289
C    *                                                                    1289
         AG_PARLIST='USER'                                                1289
         AG_TITLE = 'DETH'                                                1289
         AG_NBITS = 21                                                    1289
         AG_BIN=AG_MAGIC                                                  1289
         AG_OPTION='*'                                                    1289
         AG_FHMIN = -0.01                                                 1289
         AG_FHMAX = 0.01                                                  1289
         CALL AGSDETH (JUMPAD(TPADSTEP))                                  1289
C    *                                                                    1289
         CALL AGSDETU('TPAD')                                             1289
C    *                                                                    1289
6131  CALL AGSPOPB                                                        1290
      GO TO BLOCK_TPAD, (6130 ,6140,6150,6160)                            1290
C End of Local Procedure                                                  1290
      WRITE(LOUT,6170)'===>>  [End AgML Module tpcegeo4]  <<==='          1290
6170  FORMAT(A32,/,/)                                                     1290
C MODULE TPCEGEO4                                                         1292
      END                                                                 1292
