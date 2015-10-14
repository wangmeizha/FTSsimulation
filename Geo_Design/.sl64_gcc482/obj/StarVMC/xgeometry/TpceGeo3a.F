C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE TPCEGEO3A                                                1
C TPCEGEO3A IS THE UPDATED TPC                                            1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'TPCEGEO3A'/, BANK_TITLE/'*'/                           1
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
C AUTHOR DAVID UNDERWOOD CORRECTED BY MAXIM POTEKHIN, IMPLMENTED IN, TGE  1
C O BY YURI, TRANSLATED BACK TO AGSTAR BY VICTOR, AND CODE CLEANUP BY, J  1
C ASON                                                                    1
      DATA AG_I/0/,AG_J/0/                                                4
      CHARACTER*40 AUTHOR /'DAVID UNDERWOOD CORRECTED BY MAXIM POTEKHIN,  4
     * IMPLMENTED IN, TGEO BY YURI, TRANSLATED BACK TO AGSTAR BY VICTOR,  4
     * AND CODE CLEANUP BY, JASON'/                                       4
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  4
C CREATED DEC 7, 2005                                                     4
      CHARACTER*40 CREATED /'DEC 7, 2005'/                                5
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  5
C - Advanced Geant interface                                              6
      CHARACTER*20 AG_MODULE,AG_TITLE,AG_EXNAME,AG_PARLIST,AG_CHDIR,      6
     *AG_MATERIAL,AG_MIXTURE,AG_COMPONENT,AG_MEDIUM                       6
      CHARACTER*4 AG_VOLUME,AG_MOTHER,AG_SHAPE,AG_CNICK,AG_KONLY,         6
     *AG_OPTION,AG_ORT,AG_MARK                                            6
      INTEGER AG_BEGCOM,AG_IVOLUME,AG_IMOTHER,AG_IGNUM,AG_ISHAPE,         6
     *AG_IMED,AG_IMAT,AG_IFIELD, AG_NLMAT,AG_IERROR,AG_NWBUF,AG_NPAR,     6
     *AG_ISTATUS,AG_IROT,AG_NBITS,AG_SERIAL, AG_ATTRIBUTE(6),AG_WORK,     6
     *AG_SEEN,AG_LSTY,AG_LWID,AG_COLO,AG_FILL,AG_JDU,AG_ORTI, AG_NDIV,    6
     *AG_IAXIS,AG_NDVMAX,AG_IPRIN,AG_RESET1,AG_RESET2,AG_BEGSCR,          6
     *AG_ENDSCR, AG_IDTYPE,AG_ISET,AG_IDET,AG_ISVOL,AG_LEVEL,AG_IRESER,   6
     *AG_IGEOM,AG_STANDALONE, AG_LSTACK,AG_NWUHIT,AG_NWUVOL,AG_MAGIC,     6
     *AG_LDETU,AG_NPDIV,AG_NPDV,AG_NZ,AG_NCOPY, AG_IDEBU,AG_IGRAP,        6
     *AG_IHIST,AG_IMFLD,AG_ISIMU,AG_ENDSAVE,AG_CODE,AG_TRKTYP, AG_MODE,   6
     *AG_ECODE,AG_PDG,IPRIN, AG_ALPHAN,AG_ALPHAK(10),IGOR                 6
      REAL AG_FIELDM,AG_TMAXFD,AG_STEMAX,AG_DEEMAX,AG_EPSIL,AG_STMIN,     6
     *AG_DENS,AG_RADL,AG_ABSL, AG_THETAX,AG_PHIX,AG_THETAY,AG_PHIY,       6
     *AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,AG_ALFAZ, AG_ALPHAL(10),        6
     *AG_ALPHAX,AG_ALPHAY,AG_ALPHAZ,AG_TYPE, AG_DX, AG_DX1, AG_DX2,       6
     *AG_DY, AG_DY1, AG_DY2, AG_DMAXMS, AG_TWIST, AG_THET, AG_THE1,       6
     *AG_THE2, AG_PHI, AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2,       6
     *AG_RMIN, AG_RMAX, AG_RMN, AG_RMX, AG_ZI , AG_RMN1, AG_RMN2,         6
     *AG_RMX1, AG_RMX2, AG_H1, AG_H2, AG_BL1, AG_BL2, AG_TL1, AG_TL2,     6
     *AG_DPHI, AG_DZ, AG_TWIS, AG_X, AG_Y, AG_Z, AG_A, AG_ZA, AG_W,       6
     *AG_STEP, AG_C0, AG_PAR, AG_AA, AG_ZZ, AG_WW, AG_LX, AG_LY, AG_LZ,   6
     *AG_HX, AG_HY, AG_HZ, AG_P1, AG_P2, AG_STACK,AG_UBUF,AG_XHMAX,       6
     *AG_YHMAX,AG_ZHMAX,AG_RHMAX,AG_FHMAX,AG_FHMIN,AG_BIN, AG_CHARGE,     6
     *AG_MASS,AG_TLIFE,AG_BRATIO                                          6
      PARAMETER (AG_LSTACK=130,AG_NWUHIT=10,AG_NWUVOL=3,AG_MAGIC=         6
     *-696969,AG_LDETU=250)                                               6
      COMMON/AGCGLOB/ AG_MODULE,AG_CHDIR,AG_LEVEL,AG_IDTYPE,AG_IERROR,    6
     *AG_STANDALONE,IPRIN, AG_IPRIN,AG_IGEOM,AG_IDEBU,AG_IGRAP,AG_IHIST,  6
     *AG_IMFLD,AG_ISIMU                                                   6
C Inherited variables a: saved during internal calls                      6
C b: reset at the entry in a block and then saved                         6
      COMMON/AGCPARA/ AG_BEGCOM, AG_IVOLUME, AG_IMOTHER, AG_IGNUM,        6
     *AG_ISHAPE, AG_IMED, AG_IMAT, AG_IFIELD, AG_FIELDM, AG_TMAXFD,       6
     *AG_STEMAX, AG_DEEMAX, AG_EPSIL, AG_STMIN, AG_DENS, AG_RADL,         6
     *AG_ABSL, AG_DX, AG_DX1, AG_DX2, AG_DY, AG_DY1, AG_DY2, AG_RMN1,     6
     *AG_RMN2, AG_RMX1, AG_RMX2, AG_THET, AG_THE1, AG_THE2, AG_PHI,       6
     *AG_PHI1, AG_PHI2, AG_ALPH, AG_ALP1, AG_ALP2, AG_H1, AG_BL1,         6
     *AG_TL1, AG_H2, AG_BL2, AG_TL2, AG_RMIN, AG_RMAX, AG_DPHI,           6
     *AG_NPDIV, AG_NZ, AG_DZ, AG_TWIS, AG_LX, AG_LY, AG_LZ, AG_HX,        6
     *AG_HY, AG_HZ, AG_A, AG_ZA, AG_W, AG_NLMAT, AG_WORK, AG_SEEN,        6
     *AG_LSTY, AG_LWID, AG_COLO, AG_FILL, AG_SERIAL, AG_ISVOL,            6
     *AG_ISTATUS, AG_ZI(16), AG_RMN(16), AG_RMX(16), AG_VOLUME,           6
     *AG_MOTHER, AG_SHAPE, AG_CNICK, AG_ENDSAVE, AG_RESET1, AG_THETAX,    6
     *AG_PHIX,AG_THETAY,AG_PHIY,AG_THETAZ,AG_PHIZ,AG_ALFAX,AG_ALFAY,      6
     *AG_ALFAZ, AG_X,AG_Y,AG_Z, AG_STEP,AG_C0,AG_NDIV,AG_IAXIS,           6
     *AG_NDVMAX,AG_ORTI,AG_NCOPY, AG_ALPHAN,AG_ALPHAK,AG_ALPHAL,          6
     *AG_RESET2, AG_KONLY, AG_ORT, AG_MARK                                6
C local variables valid inside same block                                 6
      COMMON/AGCLOCA/ AG_BEGSCR,AG_UBUF(100),AG_PAR(100),AG_AA(20),       6
     *AG_ZZ(20),AG_WW(20),AG_NWBUF, AG_XHMAX,AG_YHMAX,AG_ZHMAX,AG_RHMAX,  6
     *AG_FHMAX,AG_FHMIN,AG_NBITS,AG_BIN,AG_TYPE, AG_IROT,AG_NPAR,         6
     *AG_ISET,AG_IDET,AG_JDU,AG_IRESER, AG_ENDSCR, AG_TITLE,AG_EXNAME,    6
     *AG_PARLIST,AG_MATERIAL,AG_MIXTURE, AG_COMPONENT,AG_MEDIUM,          6
     *AG_OPTION                                                           6
      COMMON/AGCPART/AG_CODE,AG_TRKTYP,AG_MASS,AG_CHARGE,AG_TLIFE,        6
     *AG_BRATIO(6),AG_MODE(6),AG_PDG,AG_ECODE                             6
      COMMON/AGCSTAC/ AG_STACK(AG_LSTACK,15)                              6
C temporarly                                                              6
      EQUIVALENCE (AG_ATTRIBUTE,AG_WORK),(AG_STEMAX,AG_DMAXMS),           6
     *(AG_ALPHAX,AG_ALFAX), (AG_ALPHAY,AG_ALFAY),(AG_ALPHAZ,AG_ALFAZ),    6
     *(AG_TWIST,AG_TWIS),(AG_NPDIV,AG_NPDV),  (AG_P1,AG_HX),(AG_P2,       6
     *AG_HY)                                                              6
      REAL MIL_P,INCH_P                                                   6
      PARAMETER (INCH_P=2.54,MIL_P=0.00254)                               6
C - GEANT description of logical units                                    6
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            7
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      7
      COMMON/GCMAIL/ CHMAIL                                               7
      CHARACTER*132 CHMAIL                                                7
C - Natural constants (or considered as such)                             7
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   8
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        8
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       8
      CHARACTER*4 VOLUNAME                                                8
      REAL TSAS_ZOFFSET/0.0/, TSGT_ZOFFSET/0.0/                           11
      INTEGER BLOCK_TPCE                                                  18
      INTEGER BLOCK_TPCM                                                  18
      INTEGER BLOCK_TIFC                                                  18
      INTEGER BLOCK_TIKA                                                  18
      INTEGER BLOCK_TINX                                                  18
      INTEGER BLOCK_TIAD                                                  18
      INTEGER BLOCK_TOFC                                                  18
      INTEGER BLOCK_TOFS                                                  18
      INTEGER BLOCK_TOKA                                                  18
      INTEGER BLOCK_TONX                                                  18
      INTEGER BLOCK_TOAD                                                  18
      INTEGER BLOCK_TOIG                                                  18
      INTEGER BLOCK_TOHA                                                  18
      INTEGER BLOCK_TSWH                                                  18
      INTEGER BLOCK_TSAW                                                  18
      INTEGER BLOCK_TSAS                                                  18
      INTEGER BLOCK_TWAS                                                  18
      INTEGER BLOCK_FEEA                                                  18
      INTEGER BLOCK_FEEP                                                  18
      INTEGER BLOCK_FEER                                                  18
      INTEGER BLOCK_FEEI                                                  18
      INTEGER BLOCK_TALS                                                  18
      INTEGER BLOCK_TSGT                                                  18
      INTEGER BLOCK_TWBT                                                  18
      INTEGER BLOCK_TWRI                                                  18
      INTEGER BLOCK_TWRG                                                  18
      INTEGER BLOCK_TWRC                                                  18
      INTEGER BLOCK_TWTR                                                  18
      INTEGER BLOCK_TWMR                                                  18
      INTEGER BLOCK_TRDO                                                  18
      INTEGER BLOCK_TBRW                                                  18
      INTEGER BLOCK_TWRB                                                  18
      INTEGER BLOCK_TCOO                                                  18
      INTEGER BLOCK_TCAB                                                  18
      INTEGER BLOCK_TRDC                                                  18
      INTEGER BLOCK_TRIB                                                  18
      INTEGER BLOCK_TWIR                                                  18
      INTEGER BLOCK_THOL                                                  18
      INTEGER BLOCK_FEES                                                  18
      INTEGER BLOCK_TPGV                                                  18
      INTEGER BLOCK_TPSS                                                  18
      INTEGER BLOCK_TPAD                                                  18
      INTEGER KINNER,KOUTER                                               19
      PARAMETER (KINNER=0)                                                21
      PARAMETER (KOUTER=1)                                                22
      REAL INCH,CM                                                        23
      PARAMETER (INCH = 2.54)                                             25
      PARAMETER (CM = 1.)                                                 26
      INTEGER KBLACK,KRED,KGREEN,KBLUE,KYELLOW,KVIOLET,KLIGHTBLUE         27
      INTEGER KBBOX,KPARA,KTRD1,ISHAPE                                    29
      PARAMETER (KBLACK=1)                                                31
      PARAMETER (KRED=2)                                                  32
      PARAMETER (KGREEN=3)                                                33
      PARAMETER (KBLUE=4)                                                 34
      PARAMETER (KYELLOW=5)                                               35
      PARAMETER (KVIOLET=6)                                               36
      PARAMETER (KLIGHTBLUE=7)                                            37
      PARAMETER (KBBOX=0)                                                 38
      PARAMETER (KPARA=1)                                                 39
      PARAMETER (KTRD1=2)                                                 40
      INTEGER *4 MYCOUNT/0/                                               41
      REAL SIND,COSD,TAND                                                 43
      REAL COS15,SIN15,TAN15                                              45
      PARAMETER (COS15 = .965925826289068312)                             47
      PARAMETER (SIN15 = .258819045102520739)                             48
      PARAMETER (TAN15 = .267949192431122696)                             49
      INTEGER I,J,ISECANG,NCOUNT/0/,JTRIB,KASE                            50
      REAL X,X0,X1,X2,DX,DX1,DX2,XC                                       52
      REAL Y,Y0,Y1,Y2,DY,DY1,DY2                                          54
      REAL Z,Z0,Z1,Z2,DZ,DZ1,DZ2                                          56
      REAL ZBEG,ZDED,ZPMT,ZEND                                            58
      REAL R,R0,R1,R2,DR                                                  60
      REAL ALPHA,BETA                                                     62
      REAL XW,DXW,YW,DYW,ZW,DZW,DYDX                                      64
      REAL DRSECTORSHIFT/0/                                               66
      REAL MYDX,MYDY,MYDZ,MYALPH,MYTHET,MYPHI,MYPAR(20)                   68
      REAL TOCSIR,TOCSOR,TOKAIR,TOKAOR,TONXIR,TONXOR,TOADIR,TOADOR,       70
     *TOIGIR,TOIGOR                                                       70
      REAL TOALIR,TOALOR,TOHAIR,TOHAOR,TOALIR2,TOALOR2,TOFCIR,TOFCOR,     72
     *TOFSIR,TOFSOR                                                       72
      REAL TIFCIR,TIALIR,TIALOR,TIKAIR,TIKAOR,TINXIR,TINXOR,TIADIR,       74
     *TIADOR,TIFCOR                                                       74
      REAL TPGVLENG,TOFCLENG,TPGVZ                                        76
      INTEGER I_SEC,I_ROW                                                 78
      EXTERNAL TPADSTEP                                                   80
      EXTERNAL TPAISTEP                                                   81
      EXTERNAL TPAOSTEP                                                   82
      EXTERNAL TPCELASER                                                  83
      REAL TPGVIR                                                         84
      REAL DEL                                                            86
      REAL ZWHEEL1,ZTOFCEND,ZTIFCEND,ZTIFCFLANGEBEGIN                     88
      REAL QWE(10)                                                        90
      REAL XHOLES(0:15)                                                   92
      REAL WIDTH1IOF(2),WIDTH2IOF(2),HEIGHTIOF(2),DEPTHIOF(2)             94
      INTEGER INOUT                                                       96
      REAL ZZZSA(2)                                                       98
      REAL RMNSA(0:1)                                                     100
      REAL RMXSA(0:1)                                                     102
      DATA ZZZSA/207.4,219.1/                                             104
      DATA RMNSA/51,121.07/                                               105
      DATA RMXSA/121.07,194/                                              106
      REAL ZZZWA(2)                                                       107
      REAL RMNWA(0:1)                                                     109
      REAL RMXWA(0:1)                                                     111
      DATA ZZZWA/211.4,229.2/                                             113
      DATA RMNWA/54,121/                                                  114
      DATA RMXWA/121,189/                                                 115
      REAL ZZZRDO(2)                                                      116
      REAL RMNRDO(2)                                                      118
      REAL RMXRDO(2)                                                      120
      DATA ZZZRDO/232.,252./                                              122
      DATA RMNRDO/88.,88./                                                123
      DATA RMXRDO/198.,198./                                              124
      REAL MYCOS,MYRMIN,MYRMAX,MYG1,MYG2                                  125
      REAL DRDOCOOLING(0:7)                                               127
      DATA DRDOCOOLING/ 16.0, 16.0, 16.0, 16.7, 15.0, 3.5, 14.5, 0.0/     129
      REAL RCOOLINGTUBE/123456789/                                        130
      CHARACTER*4 MYSHA                                                   132
      INTEGER ICOO,ICAB,JCOO,IRIB,ITALS,JTALS                             134
C -                                                                       136
      REAL TPCC_VERSION                                                   136
      EQUIVALENCE (BANK_TPCC(3),TPCC_VERSION,RE_TPCC_VERSION),            136
     *(NAMES_TPCC(1),NTPCC_VERSION)                                       136
      EQUIVALENCE (COMMEN_TPCC(1),CTPCC_VERSION),(MAP_TPCC(1,1),          136
     *MTPCC_VERSION)                                                      136
      CHARACTER CTPCC_VERSION*40/' '/                                     136
      CHARACTER NTPCC_VERSION*12/'R.VERSION'/                             136
      INTEGER MTPCC_VERSION(2)/1,1/                                       136
      REAL RE_TPCC_VERSION                                                136
C -                                                                       136
      INTEGER LEN_TPCC,LLEN_TPCC,LINK_TPCC                                136
      PARAMETER (LEN_TPCC=3)                                              136
      PARAMETER (LLEN_TPCC=1)                                             136
      INTEGER FLAG_TPCC/0/,MAP_TPCC(2,LLEN_TPCC),IDNUM_TPCC(10)           136
      CHARACTER NAMES_TPCC(LLEN_TPCC)*12,COMMEN_TPCC(LLEN_TPCC)*40        136
      DATA IDNUM_TPCC/10*0/                                               136
      REAL BANK_TPCC(LEN_TPCC)                                            136
      CHARACTER*60 BPATH_TPCC/' '/                                        136
      SAVE FLAG_TPCC,MAP_TPCC,IDNUM_TPCC,NAMES_TPCC,COMMEN_TPCC,          136
     *BPATH_TPCC                                                          136
      DATA LINK_TPCC/0/,BANK_TPCC/LEN_TPCC*0/                             136
C *                                                                       136
C -                                                                       143
      REAL TPCG_VERSION                                                   143
      EQUIVALENCE (BANK_TPCG(3),TPCG_VERSION,RE_TPCG_VERSION),            143
     *(NAMES_TPCG(1),NTPCG_VERSION)                                       143
      EQUIVALENCE (COMMEN_TPCG(1),CTPCG_VERSION),(MAP_TPCG(1,1),          143
     *MTPCG_VERSION)                                                      143
      CHARACTER CTPCG_VERSION*40/' '/                                     143
      CHARACTER NTPCG_VERSION*12/'R.VERSION'/                             143
      INTEGER MTPCG_VERSION(2)/1,1/                                       143
      REAL RE_TPCG_VERSION                                                143
C -                                                                       143
      REAL TPCG_RMIN                                                      143
      EQUIVALENCE (BANK_TPCG(4),TPCG_RMIN,RE_TPCG_RMIN),(NAMES_TPCG(2),   143
     *NTPCG_RMIN)                                                         143
      EQUIVALENCE (COMMEN_TPCG(2),CTPCG_RMIN),(MAP_TPCG(1,2),             143
     *MTPCG_RMIN)                                                         143
      CHARACTER CTPCG_RMIN*40/' '/                                        143
      CHARACTER NTPCG_RMIN*12/'R.RMIN'/                                   143
      INTEGER MTPCG_RMIN(2)/1,1/                                          143
      REAL RE_TPCG_RMIN                                                   143
C -                                                                       143
      REAL TPCG_RMAX                                                      143
      EQUIVALENCE (BANK_TPCG(5),TPCG_RMAX,RE_TPCG_RMAX),(NAMES_TPCG(3),   143
     *NTPCG_RMAX)                                                         143
      EQUIVALENCE (COMMEN_TPCG(3),CTPCG_RMAX),(MAP_TPCG(1,3),             143
     *MTPCG_RMAX)                                                         143
      CHARACTER CTPCG_RMAX*40/' '/                                        143
      CHARACTER NTPCG_RMAX*12/'R.RMAX'/                                   143
      INTEGER MTPCG_RMAX(2)/1,1/                                          143
      REAL RE_TPCG_RMAX                                                   143
C -                                                                       143
      REAL TPCG_RMINIFC                                                   143
      EQUIVALENCE (BANK_TPCG(6),TPCG_RMINIFC,RE_TPCG_RMINIFC),            143
     *(NAMES_TPCG(4),NTPCG_RMINIFC)                                       143
      EQUIVALENCE (COMMEN_TPCG(4),CTPCG_RMINIFC),(MAP_TPCG(1,4),          143
     *MTPCG_RMINIFC)                                                      143
      CHARACTER CTPCG_RMINIFC*40/' '/                                     143
      CHARACTER NTPCG_RMINIFC*12/'R.RMINIFC'/                             143
      INTEGER MTPCG_RMINIFC(2)/1,1/                                       143
      REAL RE_TPCG_RMINIFC                                                143
C -                                                                       143
      REAL TPCG_LENGTHT                                                   143
      EQUIVALENCE (BANK_TPCG(7),TPCG_LENGTHT,RE_TPCG_LENGTHT),            143
     *(NAMES_TPCG(5),NTPCG_LENGTHT)                                       143
      EQUIVALENCE (COMMEN_TPCG(5),CTPCG_LENGTHT),(MAP_TPCG(1,5),          143
     *MTPCG_LENGTHT)                                                      143
      CHARACTER CTPCG_LENGTHT*40/' '/                                     143
      CHARACTER NTPCG_LENGTHT*12/'R.LENGTHT'/                             143
      INTEGER MTPCG_LENGTHT(2)/1,1/                                       143
      REAL RE_TPCG_LENGTHT                                                143
C -                                                                       143
      REAL TPCG_LENGTH                                                    143
      EQUIVALENCE (BANK_TPCG(8),TPCG_LENGTH,RE_TPCG_LENGTH),              143
     *(NAMES_TPCG(6),NTPCG_LENGTH)                                        143
      EQUIVALENCE (COMMEN_TPCG(6),CTPCG_LENGTH),(MAP_TPCG(1,6),           143
     *MTPCG_LENGTH)                                                       143
      CHARACTER CTPCG_LENGTH*40/' '/                                      143
      CHARACTER NTPCG_LENGTH*12/'R.LENGTH'/                               143
      INTEGER MTPCG_LENGTH(2)/1,1/                                        143
      REAL RE_TPCG_LENGTH                                                 143
C -                                                                       143
      REAL TPCG_LENGTHW                                                   143
      EQUIVALENCE (BANK_TPCG(9),TPCG_LENGTHW,RE_TPCG_LENGTHW),            143
     *(NAMES_TPCG(7),NTPCG_LENGTHW)                                       143
      EQUIVALENCE (COMMEN_TPCG(7),CTPCG_LENGTHW),(MAP_TPCG(1,7),          143
     *MTPCG_LENGTHW)                                                      143
      CHARACTER CTPCG_LENGTHW*40/' '/                                     143
      CHARACTER NTPCG_LENGTHW*12/'R.LENGTHW'/                             143
      INTEGER MTPCG_LENGTHW(2)/1,1/                                       143
      REAL RE_TPCG_LENGTHW                                                143
C -                                                                       143
      REAL TPCG_LENGTHV                                                   143
      EQUIVALENCE (BANK_TPCG(10),TPCG_LENGTHV,RE_TPCG_LENGTHV),           143
     *(NAMES_TPCG(8),NTPCG_LENGTHV)                                       143
      EQUIVALENCE (COMMEN_TPCG(8),CTPCG_LENGTHV),(MAP_TPCG(1,8),          143
     *MTPCG_LENGTHV)                                                      143
      CHARACTER CTPCG_LENGTHV*40/' '/                                     143
      CHARACTER NTPCG_LENGTHV*12/'R.LENGTHV'/                             143
      INTEGER MTPCG_LENGTHV(2)/1,1/                                       143
      REAL RE_TPCG_LENGTHV                                                143
C -                                                                       143
      REAL TPCG_DZENVELOP                                                 143
      EQUIVALENCE (BANK_TPCG(11),TPCG_DZENVELOP,RE_TPCG_DZENVELOP),       143
     *(NAMES_TPCG(9),NTPCG_DZENVELOP)                                     143
      EQUIVALENCE (COMMEN_TPCG(9),CTPCG_DZENVELOP),(MAP_TPCG(1,9),        143
     *MTPCG_DZENVELOP)                                                    143
      CHARACTER CTPCG_DZENVELOP*40/' '/                                   143
      CHARACTER NTPCG_DZENVELOP*12/'R.DZENVELOP'/                         143
      INTEGER MTPCG_DZENVELOP(2)/1,1/                                     143
      REAL RE_TPCG_DZENVELOP                                              143
C -                                                                       143
      REAL TPCG_WHEELIR                                                   143
      EQUIVALENCE (BANK_TPCG(12),TPCG_WHEELIR,RE_TPCG_WHEELIR),           143
     *(NAMES_TPCG(10),NTPCG_WHEELIR)                                      143
      EQUIVALENCE (COMMEN_TPCG(10),CTPCG_WHEELIR),(MAP_TPCG(1,10),        143
     *MTPCG_WHEELIR)                                                      143
      CHARACTER CTPCG_WHEELIR*40/' '/                                     143
      CHARACTER NTPCG_WHEELIR*12/'R.WHEELIR'/                             143
      INTEGER MTPCG_WHEELIR(2)/1,1/                                       143
      REAL RE_TPCG_WHEELIR                                                143
C -                                                                       143
      REAL TPCG_WHEELR0                                                   143
      EQUIVALENCE (BANK_TPCG(13),TPCG_WHEELR0,RE_TPCG_WHEELR0),           143
     *(NAMES_TPCG(11),NTPCG_WHEELR0)                                      143
      EQUIVALENCE (COMMEN_TPCG(11),CTPCG_WHEELR0),(MAP_TPCG(1,11),        143
     *MTPCG_WHEELR0)                                                      143
      CHARACTER CTPCG_WHEELR0*40/' '/                                     143
      CHARACTER NTPCG_WHEELR0*12/'R.WHEELR0'/                             143
      INTEGER MTPCG_WHEELR0(2)/1,1/                                       143
      REAL RE_TPCG_WHEELR0                                                143
C -                                                                       143
      REAL TPCG_WHEELR1                                                   143
      EQUIVALENCE (BANK_TPCG(14),TPCG_WHEELR1,RE_TPCG_WHEELR1),           143
     *(NAMES_TPCG(12),NTPCG_WHEELR1)                                      143
      EQUIVALENCE (COMMEN_TPCG(12),CTPCG_WHEELR1),(MAP_TPCG(1,12),        143
     *MTPCG_WHEELR1)                                                      143
      CHARACTER CTPCG_WHEELR1*40/' '/                                     143
      CHARACTER NTPCG_WHEELR1*12/'R.WHEELR1'/                             143
      INTEGER MTPCG_WHEELR1(2)/1,1/                                       143
      REAL RE_TPCG_WHEELR1                                                143
C -                                                                       143
      REAL TPCG_WHEELR2                                                   143
      EQUIVALENCE (BANK_TPCG(15),TPCG_WHEELR2,RE_TPCG_WHEELR2),           143
     *(NAMES_TPCG(13),NTPCG_WHEELR2)                                      143
      EQUIVALENCE (COMMEN_TPCG(13),CTPCG_WHEELR2),(MAP_TPCG(1,13),        143
     *MTPCG_WHEELR2)                                                      143
      CHARACTER CTPCG_WHEELR2*40/' '/                                     143
      CHARACTER NTPCG_WHEELR2*12/'R.WHEELR2'/                             143
      INTEGER MTPCG_WHEELR2(2)/1,1/                                       143
      REAL RE_TPCG_WHEELR2                                                143
C -                                                                       143
      REAL TPCG_WHEELTOTALRIBWIDTH                                        143
      EQUIVALENCE (BANK_TPCG(16),TPCG_WHEELTOTALRIBWIDTH,                 143
     *RE_TPCG_WHEELTOTALRIBWIDTH),(NAMES_TPCG(14),                        143
     *NTPCG_WHEELTOTALRIBWIDTH)                                           143
      EQUIVALENCE (COMMEN_TPCG(14),CTPCG_WHEELTOTALRIBWIDTH),             143
     *(MAP_TPCG(1,14),MTPCG_WHEELTOTALRIBWIDTH)                           143
      CHARACTER CTPCG_WHEELTOTALRIBWIDTH*40/' '/                          143
      CHARACTER NTPCG_WHEELTOTALRIBWIDTH*12/'R.WHEELTOTALRIBWIDTH'/       143
      INTEGER MTPCG_WHEELTOTALRIBWIDTH(2)/1,1/                            143
      REAL RE_TPCG_WHEELTOTALRIBWIDTH                                     143
C -                                                                       143
      REAL TPCG_WHEELRIBWIDTH                                             143
      EQUIVALENCE (BANK_TPCG(17),TPCG_WHEELRIBWIDTH,                      143
     *RE_TPCG_WHEELRIBWIDTH),(NAMES_TPCG(15),NTPCG_WHEELRIBWIDTH)         143
      EQUIVALENCE (COMMEN_TPCG(15),CTPCG_WHEELRIBWIDTH),(MAP_TPCG(1,15),  143
     *MTPCG_WHEELRIBWIDTH)                                                143
      CHARACTER CTPCG_WHEELRIBWIDTH*40/' '/                               143
      CHARACTER NTPCG_WHEELRIBWIDTH*12/'R.WHEELRIBWIDTH'/                 143
      INTEGER MTPCG_WHEELRIBWIDTH(2)/1,1/                                 143
      REAL RE_TPCG_WHEELRIBWIDTH                                          143
C -                                                                       143
      REAL TPCG_WHEELRIBHEIGHT                                            143
      EQUIVALENCE (BANK_TPCG(18),TPCG_WHEELRIBHEIGHT,                     143
     *RE_TPCG_WHEELRIBHEIGHT),(NAMES_TPCG(16),NTPCG_WHEELRIBHEIGHT)       143
      EQUIVALENCE (COMMEN_TPCG(16),CTPCG_WHEELRIBHEIGHT),(MAP_TPCG(1,     143
     *16),MTPCG_WHEELRIBHEIGHT)                                           143
      CHARACTER CTPCG_WHEELRIBHEIGHT*40/' '/                              143
      CHARACTER NTPCG_WHEELRIBHEIGHT*12/'R.WHEELRIBHEIGHT'/               143
      INTEGER MTPCG_WHEELRIBHEIGHT(2)/1,1/                                143
      REAL RE_TPCG_WHEELRIBHEIGHT                                         143
C -                                                                       143
      REAL TPCG_WHEELBOXDX                                                143
      EQUIVALENCE (BANK_TPCG(19),TPCG_WHEELBOXDX,RE_TPCG_WHEELBOXDX),     143
     *(NAMES_TPCG(17),NTPCG_WHEELBOXDX)                                   143
      EQUIVALENCE (COMMEN_TPCG(17),CTPCG_WHEELBOXDX),(MAP_TPCG(1,17),     143
     *MTPCG_WHEELBOXDX)                                                   143
      CHARACTER CTPCG_WHEELBOXDX*40/' '/                                  143
      CHARACTER NTPCG_WHEELBOXDX*12/'R.WHEELBOXDX'/                       143
      INTEGER MTPCG_WHEELBOXDX(2)/1,1/                                    143
      REAL RE_TPCG_WHEELBOXDX                                             143
C -                                                                       143
      REAL TPCG_WHEELBOXDY                                                143
      EQUIVALENCE (BANK_TPCG(20),TPCG_WHEELBOXDY,RE_TPCG_WHEELBOXDY),     143
     *(NAMES_TPCG(18),NTPCG_WHEELBOXDY)                                   143
      EQUIVALENCE (COMMEN_TPCG(18),CTPCG_WHEELBOXDY),(MAP_TPCG(1,18),     143
     *MTPCG_WHEELBOXDY)                                                   143
      CHARACTER CTPCG_WHEELBOXDY*40/' '/                                  143
      CHARACTER NTPCG_WHEELBOXDY*12/'R.WHEELBOXDY'/                       143
      INTEGER MTPCG_WHEELBOXDY(2)/1,1/                                    143
      REAL RE_TPCG_WHEELBOXDY                                             143
C -                                                                       143
      REAL TPCG_WHEELOR1                                                  143
      EQUIVALENCE (BANK_TPCG(21),TPCG_WHEELOR1,RE_TPCG_WHEELOR1),         143
     *(NAMES_TPCG(19),NTPCG_WHEELOR1)                                     143
      EQUIVALENCE (COMMEN_TPCG(19),CTPCG_WHEELOR1),(MAP_TPCG(1,19),       143
     *MTPCG_WHEELOR1)                                                     143
      CHARACTER CTPCG_WHEELOR1*40/' '/                                    143
      CHARACTER NTPCG_WHEELOR1*12/'R.WHEELOR1'/                           143
      INTEGER MTPCG_WHEELOR1(2)/1,1/                                      143
      REAL RE_TPCG_WHEELOR1                                               143
C -                                                                       143
      REAL TPCG_WHEELOR                                                   143
      EQUIVALENCE (BANK_TPCG(22),TPCG_WHEELOR,RE_TPCG_WHEELOR),           143
     *(NAMES_TPCG(20),NTPCG_WHEELOR)                                      143
      EQUIVALENCE (COMMEN_TPCG(20),CTPCG_WHEELOR),(MAP_TPCG(1,20),        143
     *MTPCG_WHEELOR)                                                      143
      CHARACTER CTPCG_WHEELOR*40/' '/                                     143
      CHARACTER NTPCG_WHEELOR*12/'R.WHEELOR'/                             143
      INTEGER MTPCG_WHEELOR(2)/1,1/                                       143
      REAL RE_TPCG_WHEELOR                                                143
C -                                                                       143
      REAL TPCG_WHEELTHK1                                                 143
      EQUIVALENCE (BANK_TPCG(23),TPCG_WHEELTHK1,RE_TPCG_WHEELTHK1),       143
     *(NAMES_TPCG(21),NTPCG_WHEELTHK1)                                    143
      EQUIVALENCE (COMMEN_TPCG(21),CTPCG_WHEELTHK1),(MAP_TPCG(1,21),      143
     *MTPCG_WHEELTHK1)                                                    143
      CHARACTER CTPCG_WHEELTHK1*40/' '/                                   143
      CHARACTER NTPCG_WHEELTHK1*12/'R.WHEELTHK1'/                         143
      INTEGER MTPCG_WHEELTHK1(2)/1,1/                                     143
      REAL RE_TPCG_WHEELTHK1                                              143
C -                                                                       143
      REAL TPCG_WHEELTHK                                                  143
      EQUIVALENCE (BANK_TPCG(24),TPCG_WHEELTHK,RE_TPCG_WHEELTHK),         143
     *(NAMES_TPCG(22),NTPCG_WHEELTHK)                                     143
      EQUIVALENCE (COMMEN_TPCG(22),CTPCG_WHEELTHK),(MAP_TPCG(1,22),       143
     *MTPCG_WHEELTHK)                                                     143
      CHARACTER CTPCG_WHEELTHK*40/' '/                                    143
      CHARACTER NTPCG_WHEELTHK*12/'R.WHEELTHK'/                           143
      INTEGER MTPCG_WHEELTHK(2)/1,1/                                      143
      REAL RE_TPCG_WHEELTHK                                               143
C -                                                                       143
      REAL TPCG_SENGASOR                                                  143
      EQUIVALENCE (BANK_TPCG(25),TPCG_SENGASOR,RE_TPCG_SENGASOR),         143
     *(NAMES_TPCG(23),NTPCG_SENGASOR)                                     143
      EQUIVALENCE (COMMEN_TPCG(23),CTPCG_SENGASOR),(MAP_TPCG(1,23),       143
     *MTPCG_SENGASOR)                                                     143
      CHARACTER CTPCG_SENGASOR*40/' '/                                    143
      CHARACTER NTPCG_SENGASOR*12/'R.SENGASOR'/                           143
      INTEGER MTPCG_SENGASOR(2)/1,1/                                      143
      REAL RE_TPCG_SENGASOR                                               143
C -                                                                       143
      REAL TPCG_MEMBTHK                                                   143
      EQUIVALENCE (BANK_TPCG(26),TPCG_MEMBTHK,RE_TPCG_MEMBTHK),           143
     *(NAMES_TPCG(24),NTPCG_MEMBTHK)                                      143
      EQUIVALENCE (COMMEN_TPCG(24),CTPCG_MEMBTHK),(MAP_TPCG(1,24),        143
     *MTPCG_MEMBTHK)                                                      143
      CHARACTER CTPCG_MEMBTHK*40/' '/                                     143
      CHARACTER NTPCG_MEMBTHK*12/'R.MEMBTHK'/                             143
      INTEGER MTPCG_MEMBTHK(2)/1,1/                                       143
      REAL RE_TPCG_MEMBTHK                                                143
C -                                                                       143
      REAL TPCG_TOCSDR                                                    143
      EQUIVALENCE (BANK_TPCG(27),TPCG_TOCSDR,RE_TPCG_TOCSDR),             143
     *(NAMES_TPCG(25),NTPCG_TOCSDR)                                       143
      EQUIVALENCE (COMMEN_TPCG(25),CTPCG_TOCSDR),(MAP_TPCG(1,25),         143
     *MTPCG_TOCSDR)                                                       143
      CHARACTER CTPCG_TOCSDR*40/' '/                                      143
      CHARACTER NTPCG_TOCSDR*12/'R.TOCSDR'/                               143
      INTEGER MTPCG_TOCSDR(2)/1,1/                                        143
      REAL RE_TPCG_TOCSDR                                                 143
C -                                                                       143
      REAL TPCG_TOKADR                                                    143
      EQUIVALENCE (BANK_TPCG(28),TPCG_TOKADR,RE_TPCG_TOKADR),             143
     *(NAMES_TPCG(26),NTPCG_TOKADR)                                       143
      EQUIVALENCE (COMMEN_TPCG(26),CTPCG_TOKADR),(MAP_TPCG(1,26),         143
     *MTPCG_TOKADR)                                                       143
      CHARACTER CTPCG_TOKADR*40/' '/                                      143
      CHARACTER NTPCG_TOKADR*12/'R.TOKADR'/                               143
      INTEGER MTPCG_TOKADR(2)/1,1/                                        143
      REAL RE_TPCG_TOKADR                                                 143
C -                                                                       143
      REAL TPCG_TONXDR                                                    143
      EQUIVALENCE (BANK_TPCG(29),TPCG_TONXDR,RE_TPCG_TONXDR),             143
     *(NAMES_TPCG(27),NTPCG_TONXDR)                                       143
      EQUIVALENCE (COMMEN_TPCG(27),CTPCG_TONXDR),(MAP_TPCG(1,27),         143
     *MTPCG_TONXDR)                                                       143
      CHARACTER CTPCG_TONXDR*40/' '/                                      143
      CHARACTER NTPCG_TONXDR*12/'R.TONXDR'/                               143
      INTEGER MTPCG_TONXDR(2)/1,1/                                        143
      REAL RE_TPCG_TONXDR                                                 143
C -                                                                       143
      REAL TPCG_TOADDR                                                    143
      EQUIVALENCE (BANK_TPCG(30),TPCG_TOADDR,RE_TPCG_TOADDR),             143
     *(NAMES_TPCG(28),NTPCG_TOADDR)                                       143
      EQUIVALENCE (COMMEN_TPCG(28),CTPCG_TOADDR),(MAP_TPCG(1,28),         143
     *MTPCG_TOADDR)                                                       143
      CHARACTER CTPCG_TOADDR*40/' '/                                      143
      CHARACTER NTPCG_TOADDR*12/'R.TOADDR'/                               143
      INTEGER MTPCG_TOADDR(2)/1,1/                                        143
      REAL RE_TPCG_TOADDR                                                 143
C -                                                                       143
      REAL TPCG_TOIGDR                                                    143
      EQUIVALENCE (BANK_TPCG(31),TPCG_TOIGDR,RE_TPCG_TOIGDR),             143
     *(NAMES_TPCG(29),NTPCG_TOIGDR)                                       143
      EQUIVALENCE (COMMEN_TPCG(29),CTPCG_TOIGDR),(MAP_TPCG(1,29),         143
     *MTPCG_TOIGDR)                                                       143
      CHARACTER CTPCG_TOIGDR*40/' '/                                      143
      CHARACTER NTPCG_TOIGDR*12/'R.TOIGDR'/                               143
      INTEGER MTPCG_TOIGDR(2)/1,1/                                        143
      REAL RE_TPCG_TOIGDR                                                 143
C -                                                                       143
      REAL TPCG_TOALDR                                                    143
      EQUIVALENCE (BANK_TPCG(32),TPCG_TOALDR,RE_TPCG_TOALDR),             143
     *(NAMES_TPCG(30),NTPCG_TOALDR)                                       143
      EQUIVALENCE (COMMEN_TPCG(30),CTPCG_TOALDR),(MAP_TPCG(1,30),         143
     *MTPCG_TOALDR)                                                       143
      CHARACTER CTPCG_TOALDR*40/' '/                                      143
      CHARACTER NTPCG_TOALDR*12/'R.TOALDR'/                               143
      INTEGER MTPCG_TOALDR(2)/1,1/                                        143
      REAL RE_TPCG_TOALDR                                                 143
C -                                                                       143
      REAL TPCG_TOHADR                                                    143
      EQUIVALENCE (BANK_TPCG(33),TPCG_TOHADR,RE_TPCG_TOHADR),             143
     *(NAMES_TPCG(31),NTPCG_TOHADR)                                       143
      EQUIVALENCE (COMMEN_TPCG(31),CTPCG_TOHADR),(MAP_TPCG(1,31),         143
     *MTPCG_TOHADR)                                                       143
      CHARACTER CTPCG_TOHADR*40/' '/                                      143
      CHARACTER NTPCG_TOHADR*12/'R.TOHADR'/                               143
      INTEGER MTPCG_TOHADR(2)/1,1/                                        143
      REAL RE_TPCG_TOHADR                                                 143
C -                                                                       143
      REAL TPCG_TIADDR                                                    143
      EQUIVALENCE (BANK_TPCG(34),TPCG_TIADDR,RE_TPCG_TIADDR),             143
     *(NAMES_TPCG(32),NTPCG_TIADDR)                                       143
      EQUIVALENCE (COMMEN_TPCG(32),CTPCG_TIADDR),(MAP_TPCG(1,32),         143
     *MTPCG_TIADDR)                                                       143
      CHARACTER CTPCG_TIADDR*40/' '/                                      143
      CHARACTER NTPCG_TIADDR*12/'R.TIADDR'/                               143
      INTEGER MTPCG_TIADDR(2)/1,1/                                        143
      REAL RE_TPCG_TIADDR                                                 143
C -                                                                       143
      REAL TPCG_TINXDR                                                    143
      EQUIVALENCE (BANK_TPCG(35),TPCG_TINXDR,RE_TPCG_TINXDR),             143
     *(NAMES_TPCG(33),NTPCG_TINXDR)                                       143
      EQUIVALENCE (COMMEN_TPCG(33),CTPCG_TINXDR),(MAP_TPCG(1,33),         143
     *MTPCG_TINXDR)                                                       143
      CHARACTER CTPCG_TINXDR*40/' '/                                      143
      CHARACTER NTPCG_TINXDR*12/'R.TINXDR'/                               143
      INTEGER MTPCG_TINXDR(2)/1,1/                                        143
      REAL RE_TPCG_TINXDR                                                 143
C -                                                                       143
      REAL TPCG_TIKADR                                                    143
      EQUIVALENCE (BANK_TPCG(36),TPCG_TIKADR,RE_TPCG_TIKADR),             143
     *(NAMES_TPCG(34),NTPCG_TIKADR)                                       143
      EQUIVALENCE (COMMEN_TPCG(34),CTPCG_TIKADR),(MAP_TPCG(1,34),         143
     *MTPCG_TIKADR)                                                       143
      CHARACTER CTPCG_TIKADR*40/' '/                                      143
      CHARACTER NTPCG_TIKADR*12/'R.TIKADR'/                               143
      INTEGER MTPCG_TIKADR(2)/1,1/                                        143
      REAL RE_TPCG_TIKADR                                                 143
C -                                                                       143
      REAL TPCG_TIALDR                                                    143
      EQUIVALENCE (BANK_TPCG(37),TPCG_TIALDR,RE_TPCG_TIALDR),             143
     *(NAMES_TPCG(35),NTPCG_TIALDR)                                       143
      EQUIVALENCE (COMMEN_TPCG(35),CTPCG_TIALDR),(MAP_TPCG(1,35),         143
     *MTPCG_TIALDR)                                                       143
      CHARACTER CTPCG_TIALDR*40/' '/                                      143
      CHARACTER NTPCG_TIALDR*12/'R.TIALDR'/                               143
      INTEGER MTPCG_TIALDR(2)/1,1/                                        143
      REAL RE_TPCG_TIALDR                                                 143
C -                                                                       143
      REAL TPCG_TIFCRF                                                    143
      EQUIVALENCE (BANK_TPCG(38),TPCG_TIFCRF,RE_TPCG_TIFCRF),             143
     *(NAMES_TPCG(36),NTPCG_TIFCRF)                                       143
      EQUIVALENCE (COMMEN_TPCG(36),CTPCG_TIFCRF),(MAP_TPCG(1,36),         143
     *MTPCG_TIFCRF)                                                       143
      CHARACTER CTPCG_TIFCRF*40/' '/                                      143
      CHARACTER NTPCG_TIFCRF*12/'R.TIFCRF'/                               143
      INTEGER MTPCG_TIFCRF(2)/1,1/                                        143
      REAL RE_TPCG_TIFCRF                                                 143
C -                                                                       143
      REAL TPCG_TIFCDRT                                                   143
      EQUIVALENCE (BANK_TPCG(39),TPCG_TIFCDRT,RE_TPCG_TIFCDRT),           143
     *(NAMES_TPCG(37),NTPCG_TIFCDRT)                                      143
      EQUIVALENCE (COMMEN_TPCG(37),CTPCG_TIFCDRT),(MAP_TPCG(1,37),        143
     *MTPCG_TIFCDRT)                                                      143
      CHARACTER CTPCG_TIFCDRT*40/' '/                                     143
      CHARACTER NTPCG_TIFCDRT*12/'R.TIFCDRT'/                             143
      INTEGER MTPCG_TIFCDRT(2)/1,1/                                       143
      REAL RE_TPCG_TIFCDRT                                                143
C -                                                                       143
      REAL TPCG_DZYF1                                                     143
      EQUIVALENCE (BANK_TPCG(40),TPCG_DZYF1,RE_TPCG_DZYF1),               143
     *(NAMES_TPCG(38),NTPCG_DZYF1)                                        143
      EQUIVALENCE (COMMEN_TPCG(38),CTPCG_DZYF1),(MAP_TPCG(1,38),          143
     *MTPCG_DZYF1)                                                        143
      CHARACTER CTPCG_DZYF1*40/' '/                                       143
      CHARACTER NTPCG_DZYF1*12/'R.DZYF1'/                                 143
      INTEGER MTPCG_DZYF1(2)/1,1/                                         143
      REAL RE_TPCG_DZYF1                                                  143
C -                                                                       143
      REAL TPCG_DZYF2                                                     143
      EQUIVALENCE (BANK_TPCG(41),TPCG_DZYF2,RE_TPCG_DZYF2),               143
     *(NAMES_TPCG(39),NTPCG_DZYF2)                                        143
      EQUIVALENCE (COMMEN_TPCG(39),CTPCG_DZYF2),(MAP_TPCG(1,39),          143
     *MTPCG_DZYF2)                                                        143
      CHARACTER CTPCG_DZYF2*40/' '/                                       143
      CHARACTER NTPCG_DZYF2*12/'R.DZYF2'/                                 143
      INTEGER MTPCG_DZYF2(2)/1,1/                                         143
      REAL RE_TPCG_DZYF2                                                  143
C -                                                                       143
      REAL TPCG_DZYF3                                                     143
      EQUIVALENCE (BANK_TPCG(42),TPCG_DZYF3,RE_TPCG_DZYF3),               143
     *(NAMES_TPCG(40),NTPCG_DZYF3)                                        143
      EQUIVALENCE (COMMEN_TPCG(40),CTPCG_DZYF3),(MAP_TPCG(1,40),          143
     *MTPCG_DZYF3)                                                        143
      CHARACTER CTPCG_DZYF3*40/' '/                                       143
      CHARACTER NTPCG_DZYF3*12/'R.DZYF3'/                                 143
      INTEGER MTPCG_DZYF3(2)/1,1/                                         143
      REAL RE_TPCG_DZYF3                                                  143
C -                                                                       143
      REAL TPCG_PADPLANETHICKNESS                                         143
      EQUIVALENCE (BANK_TPCG(43),TPCG_PADPLANETHICKNESS,                  143
     *RE_TPCG_PADPLANETHICKNESS),(NAMES_TPCG(41),                         143
     *NTPCG_PADPLANETHICKNESS)                                            143
      EQUIVALENCE (COMMEN_TPCG(41),CTPCG_PADPLANETHICKNESS),(MAP_TPCG(1,  143
     *41),MTPCG_PADPLANETHICKNESS)                                        143
      CHARACTER CTPCG_PADPLANETHICKNESS*40/' '/                           143
      CHARACTER NTPCG_PADPLANETHICKNESS*12/'R.PADPLANETHICKNESS'/         143
      INTEGER MTPCG_PADPLANETHICKNESS(2)/1,1/                             143
      REAL RE_TPCG_PADPLANETHICKNESS                                      143
C -                                                                       143
      REAL TPCG_DGATEGROUND                                               143
      EQUIVALENCE (BANK_TPCG(44),TPCG_DGATEGROUND,RE_TPCG_DGATEGROUND),   143
     *(NAMES_TPCG(42),NTPCG_DGATEGROUND)                                  143
      EQUIVALENCE (COMMEN_TPCG(42),CTPCG_DGATEGROUND),(MAP_TPCG(1,42),    143
     *MTPCG_DGATEGROUND)                                                  143
      CHARACTER CTPCG_DGATEGROUND*40/' '/                                 143
      CHARACTER NTPCG_DGATEGROUND*12/'R.DGATEGROUND'/                     143
      INTEGER MTPCG_DGATEGROUND(2)/1,1/                                   143
      REAL RE_TPCG_DGATEGROUND                                            143
C -                                                                       143
      REAL TPCG_WIREMOUNTWIDTH                                            143
      EQUIVALENCE (BANK_TPCG(45),TPCG_WIREMOUNTWIDTH,                     143
     *RE_TPCG_WIREMOUNTWIDTH),(NAMES_TPCG(43),NTPCG_WIREMOUNTWIDTH)       143
      EQUIVALENCE (COMMEN_TPCG(43),CTPCG_WIREMOUNTWIDTH),(MAP_TPCG(1,     143
     *43),MTPCG_WIREMOUNTWIDTH)                                           143
      CHARACTER CTPCG_WIREMOUNTWIDTH*40/' '/                              143
      CHARACTER NTPCG_WIREMOUNTWIDTH*12/'R.WIREMOUNTWIDTH'/               143
      INTEGER MTPCG_WIREMOUNTWIDTH(2)/1,1/                                143
      REAL RE_TPCG_WIREMOUNTWIDTH                                         143
C -                                                                       143
      REAL TPCG_WIREMOUNTHEIGHT                                           143
      EQUIVALENCE (BANK_TPCG(46),TPCG_WIREMOUNTHEIGHT,                    143
     *RE_TPCG_WIREMOUNTHEIGHT),(NAMES_TPCG(44),NTPCG_WIREMOUNTHEIGHT)     143
      EQUIVALENCE (COMMEN_TPCG(44),CTPCG_WIREMOUNTHEIGHT),(MAP_TPCG(1,    143
     *44),MTPCG_WIREMOUNTHEIGHT)                                          143
      CHARACTER CTPCG_WIREMOUNTHEIGHT*40/' '/                             143
      CHARACTER NTPCG_WIREMOUNTHEIGHT*12/'R.WIREMOUNTHEIGHT'/             143
      INTEGER MTPCG_WIREMOUNTHEIGHT(2)/1,1/                               143
      REAL RE_TPCG_WIREMOUNTHEIGHT                                        143
C -                                                                       143
      REAL TPCG_DXRDO                                                     143
      EQUIVALENCE (BANK_TPCG(47),TPCG_DXRDO,RE_TPCG_DXRDO),               143
     *(NAMES_TPCG(45),NTPCG_DXRDO)                                        143
      EQUIVALENCE (COMMEN_TPCG(45),CTPCG_DXRDO),(MAP_TPCG(1,45),          143
     *MTPCG_DXRDO)                                                        143
      CHARACTER CTPCG_DXRDO*40/' '/                                       143
      CHARACTER NTPCG_DXRDO*12/'R.DXRDO'/                                 143
      INTEGER MTPCG_DXRDO(2)/1,1/                                         143
      REAL RE_TPCG_DXRDO                                                  143
C -                                                                       143
      REAL TPCG_DYRDO                                                     143
      EQUIVALENCE (BANK_TPCG(48),TPCG_DYRDO,RE_TPCG_DYRDO),               143
     *(NAMES_TPCG(46),NTPCG_DYRDO)                                        143
      EQUIVALENCE (COMMEN_TPCG(46),CTPCG_DYRDO),(MAP_TPCG(1,46),          143
     *MTPCG_DYRDO)                                                        143
      CHARACTER CTPCG_DYRDO*40/' '/                                       143
      CHARACTER NTPCG_DYRDO*12/'R.DYRDO'/                                 143
      INTEGER MTPCG_DYRDO(2)/1,1/                                         143
      REAL RE_TPCG_DYRDO                                                  143
C -                                                                       143
      REAL TPCG_DZRDO                                                     143
      EQUIVALENCE (BANK_TPCG(49),TPCG_DZRDO,RE_TPCG_DZRDO),               143
     *(NAMES_TPCG(47),NTPCG_DZRDO)                                        143
      EQUIVALENCE (COMMEN_TPCG(47),CTPCG_DZRDO),(MAP_TPCG(1,47),          143
     *MTPCG_DZRDO)                                                        143
      CHARACTER CTPCG_DZRDO*40/' '/                                       143
      CHARACTER NTPCG_DZRDO*12/'R.DZRDO'/                                 143
      INTEGER MTPCG_DZRDO(2)/1,1/                                         143
      REAL RE_TPCG_DZRDO                                                  143
C -                                                                       143
      REAL TPCG_ZRDO                                                      143
      EQUIVALENCE (BANK_TPCG(50),TPCG_ZRDO,RE_TPCG_ZRDO),                 143
     *(NAMES_TPCG(48),NTPCG_ZRDO)                                         143
      EQUIVALENCE (COMMEN_TPCG(48),CTPCG_ZRDO),(MAP_TPCG(1,48),           143
     *MTPCG_ZRDO)                                                         143
      CHARACTER CTPCG_ZRDO*40/' '/                                        143
      CHARACTER NTPCG_ZRDO*12/'R.ZRDO'/                                   143
      INTEGER MTPCG_ZRDO(2)/1,1/                                          143
      REAL RE_TPCG_ZRDO                                                   143
C -                                                                       143
      REAL TPCG_HEIGTUBE                                                  143
      EQUIVALENCE (BANK_TPCG(51),TPCG_HEIGTUBE,RE_TPCG_HEIGTUBE),         143
     *(NAMES_TPCG(49),NTPCG_HEIGTUBE)                                     143
      EQUIVALENCE (COMMEN_TPCG(49),CTPCG_HEIGTUBE),(MAP_TPCG(1,49),       143
     *MTPCG_HEIGTUBE)                                                     143
      CHARACTER CTPCG_HEIGTUBE*40/' '/                                    143
      CHARACTER NTPCG_HEIGTUBE*12/'R.HEIGTUBE'/                           143
      INTEGER MTPCG_HEIGTUBE(2)/1,1/                                      143
      REAL RE_TPCG_HEIGTUBE                                               143
C -                                                                       143
      REAL TPCG_WIDTUBE                                                   143
      EQUIVALENCE (BANK_TPCG(52),TPCG_WIDTUBE,RE_TPCG_WIDTUBE),           143
     *(NAMES_TPCG(50),NTPCG_WIDTUBE)                                      143
      EQUIVALENCE (COMMEN_TPCG(50),CTPCG_WIDTUBE),(MAP_TPCG(1,50),        143
     *MTPCG_WIDTUBE)                                                      143
      CHARACTER CTPCG_WIDTUBE*40/' '/                                     143
      CHARACTER NTPCG_WIDTUBE*12/'R.WIDTUBE'/                             143
      INTEGER MTPCG_WIDTUBE(2)/1,1/                                       143
      REAL RE_TPCG_WIDTUBE                                                143
C -                                                                       143
      REAL TPCG_RDOCOOLINGDX                                              143
      EQUIVALENCE (BANK_TPCG(53),TPCG_RDOCOOLINGDX,                       143
     *RE_TPCG_RDOCOOLINGDX),(NAMES_TPCG(51),NTPCG_RDOCOOLINGDX)           143
      EQUIVALENCE (COMMEN_TPCG(51),CTPCG_RDOCOOLINGDX),(MAP_TPCG(1,51),   143
     *MTPCG_RDOCOOLINGDX)                                                 143
      CHARACTER CTPCG_RDOCOOLINGDX*40/' '/                                143
      CHARACTER NTPCG_RDOCOOLINGDX*12/'R.RDOCOOLINGDX'/                   143
      INTEGER MTPCG_RDOCOOLINGDX(2)/1,1/                                  143
      REAL RE_TPCG_RDOCOOLINGDX                                           143
C -                                                                       143
      REAL TPCG_RDOCOOLINGDY                                              143
      EQUIVALENCE (BANK_TPCG(54),TPCG_RDOCOOLINGDY,                       143
     *RE_TPCG_RDOCOOLINGDY),(NAMES_TPCG(52),NTPCG_RDOCOOLINGDY)           143
      EQUIVALENCE (COMMEN_TPCG(52),CTPCG_RDOCOOLINGDY),(MAP_TPCG(1,52),   143
     *MTPCG_RDOCOOLINGDY)                                                 143
      CHARACTER CTPCG_RDOCOOLINGDY*40/' '/                                143
      CHARACTER NTPCG_RDOCOOLINGDY*12/'R.RDOCOOLINGDY'/                   143
      INTEGER MTPCG_RDOCOOLINGDY(2)/1,1/                                  143
      REAL RE_TPCG_RDOCOOLINGDY                                           143
C -                                                                       143
      REAL TPCG_RDOCOOLINGDZ                                              143
      EQUIVALENCE (BANK_TPCG(55),TPCG_RDOCOOLINGDZ,                       143
     *RE_TPCG_RDOCOOLINGDZ),(NAMES_TPCG(53),NTPCG_RDOCOOLINGDZ)           143
      EQUIVALENCE (COMMEN_TPCG(53),CTPCG_RDOCOOLINGDZ),(MAP_TPCG(1,53),   143
     *MTPCG_RDOCOOLINGDZ)                                                 143
      CHARACTER CTPCG_RDOCOOLINGDZ*40/' '/                                143
      CHARACTER NTPCG_RDOCOOLINGDZ*12/'R.RDOCOOLINGDZ'/                   143
      INTEGER MTPCG_RDOCOOLINGDZ(2)/1,1/                                  143
      REAL RE_TPCG_RDOCOOLINGDZ                                           143
C -                                                                       143
      REAL TPCG_ZGATINGGRID                                               143
      EQUIVALENCE (BANK_TPCG(56),TPCG_ZGATINGGRID,RE_TPCG_ZGATINGGRID),   143
     *(NAMES_TPCG(54),NTPCG_ZGATINGGRID)                                  143
      EQUIVALENCE (COMMEN_TPCG(54),CTPCG_ZGATINGGRID),(MAP_TPCG(1,54),    143
     *MTPCG_ZGATINGGRID)                                                  143
      CHARACTER CTPCG_ZGATINGGRID*40/' '/                                 143
      CHARACTER NTPCG_ZGATINGGRID*12/'R.ZGATINGGRID'/                     143
      INTEGER MTPCG_ZGATINGGRID(2)/1,1/                                   143
      REAL RE_TPCG_ZGATINGGRID                                            143
C -                                                                       143
      REAL TPCG_ZGROUNDGRID                                               143
      EQUIVALENCE (BANK_TPCG(57),TPCG_ZGROUNDGRID,RE_TPCG_ZGROUNDGRID),   143
     *(NAMES_TPCG(55),NTPCG_ZGROUNDGRID)                                  143
      EQUIVALENCE (COMMEN_TPCG(55),CTPCG_ZGROUNDGRID),(MAP_TPCG(1,55),    143
     *MTPCG_ZGROUNDGRID)                                                  143
      CHARACTER CTPCG_ZGROUNDGRID*40/' '/                                 143
      CHARACTER NTPCG_ZGROUNDGRID*12/'R.ZGROUNDGRID'/                     143
      INTEGER MTPCG_ZGROUNDGRID(2)/1,1/                                   143
      REAL RE_TPCG_ZGROUNDGRID                                            143
C -                                                                       143
      REAL TPCG_DEADZONE                                                  143
      EQUIVALENCE (BANK_TPCG(58),TPCG_DEADZONE,RE_TPCG_DEADZONE),         143
     *(NAMES_TPCG(56),NTPCG_DEADZONE)                                     143
      EQUIVALENCE (COMMEN_TPCG(56),CTPCG_DEADZONE),(MAP_TPCG(1,56),       143
     *MTPCG_DEADZONE)                                                     143
      CHARACTER CTPCG_DEADZONE*40/' '/                                    143
      CHARACTER NTPCG_DEADZONE*12/'R.DEADZONE'/                           143
      INTEGER MTPCG_DEADZONE(2)/1,1/                                      143
      REAL RE_TPCG_DEADZONE                                               143
C -                                                                       143
      REAL TPCG_DANODE(2)                                                 143
      EQUIVALENCE (BANK_TPCG(59),TPCG_DANODE,RE_TPCG_DANODE),             143
     *(NAMES_TPCG(57),NTPCG_DANODE)                                       143
      EQUIVALENCE (COMMEN_TPCG(57),CTPCG_DANODE),(MAP_TPCG(1,57),         143
     *MTPCG_DANODE)                                                       143
      CHARACTER CTPCG_DANODE*40/' '/                                      143
      CHARACTER NTPCG_DANODE*12/'R.DANODE'/                               143
      INTEGER MTPCG_DANODE(2)/2,1/                                        143
      REAL RE_TPCG_DANODE                                                 143
C -                                                                       143
      REAL TPCG_TOFMAX                                                    143
      EQUIVALENCE (BANK_TPCG(61),TPCG_TOFMAX,RE_TPCG_TOFMAX),             143
     *(NAMES_TPCG(58),NTPCG_TOFMAX)                                       143
      EQUIVALENCE (COMMEN_TPCG(58),CTPCG_TOFMAX),(MAP_TPCG(1,58),         143
     *MTPCG_TOFMAX)                                                       143
      CHARACTER CTPCG_TOFMAX*40/' '/                                      143
      CHARACTER NTPCG_TOFMAX*12/'R.TOFMAX'/                               143
      INTEGER MTPCG_TOFMAX(2)/1,1/                                        143
      REAL RE_TPCG_TOFMAX                                                 143
C -                                                                       143
      INTEGER LEN_TPCG,LLEN_TPCG,LINK_TPCG                                143
      PARAMETER (LEN_TPCG=61)                                             143
      PARAMETER (LLEN_TPCG=58)                                            143
      INTEGER FLAG_TPCG/0/,MAP_TPCG(2,LLEN_TPCG),IDNUM_TPCG(10)           143
      CHARACTER NAMES_TPCG(LLEN_TPCG)*12,COMMEN_TPCG(LLEN_TPCG)*40        143
      DATA IDNUM_TPCG/10*0/                                               143
      REAL BANK_TPCG(LEN_TPCG)                                            143
      CHARACTER*60 BPATH_TPCG/' '/                                        143
      SAVE FLAG_TPCG,MAP_TPCG,IDNUM_TPCG,NAMES_TPCG,COMMEN_TPCG,          143
     *BPATH_TPCG                                                          143
      DATA LINK_TPCG/0/,BANK_TPCG/LEN_TPCG*0/                             143
C *                                                                       143
C -                                                                       144
      REAL TPRS_SEC                                                       144
      EQUIVALENCE (BANK_TPRS(3),TPRS_SEC,RE_TPRS_SEC),(NAMES_TPRS(1),     144
     *NTPRS_SEC)                                                          144
      EQUIVALENCE (COMMEN_TPRS(1),CTPRS_SEC),(MAP_TPRS(1,1),MTPRS_SEC)    144
      CHARACTER CTPRS_SEC*40/' '/                                         144
      CHARACTER NTPRS_SEC*12/'R.SEC'/                                     144
      INTEGER MTPRS_SEC(2)/1,1/                                           144
      REAL RE_TPRS_SEC                                                    144
C -                                                                       144
      REAL TPRS_NROW                                                      144
      EQUIVALENCE (BANK_TPRS(4),TPRS_NROW,RE_TPRS_NROW),(NAMES_TPRS(2),   144
     *NTPRS_NROW)                                                         144
      EQUIVALENCE (COMMEN_TPRS(2),CTPRS_NROW),(MAP_TPRS(1,2),             144
     *MTPRS_NROW)                                                         144
      CHARACTER CTPRS_NROW*40/' '/                                        144
      CHARACTER NTPRS_NROW*12/'R.NROW'/                                   144
      INTEGER MTPRS_NROW(2)/1,1/                                          144
      REAL RE_TPRS_NROW                                                   144
C -                                                                       144
      REAL TPRS_PITCH                                                     144
      EQUIVALENCE (BANK_TPRS(5),TPRS_PITCH,RE_TPRS_PITCH),                144
     *(NAMES_TPRS(3),NTPRS_PITCH)                                         144
      EQUIVALENCE (COMMEN_TPRS(3),CTPRS_PITCH),(MAP_TPRS(1,3),            144
     *MTPRS_PITCH)                                                        144
      CHARACTER CTPRS_PITCH*40/' '/                                       144
      CHARACTER NTPRS_PITCH*12/'R.PITCH'/                                 144
      INTEGER MTPRS_PITCH(2)/1,1/                                         144
      REAL RE_TPRS_PITCH                                                  144
C -                                                                       144
      REAL TPRS_WIDTH                                                     144
      EQUIVALENCE (BANK_TPRS(6),TPRS_WIDTH,RE_TPRS_WIDTH),                144
     *(NAMES_TPRS(4),NTPRS_WIDTH)                                         144
      EQUIVALENCE (COMMEN_TPRS(4),CTPRS_WIDTH),(MAP_TPRS(1,4),            144
     *MTPRS_WIDTH)                                                        144
      CHARACTER CTPRS_WIDTH*40/' '/                                       144
      CHARACTER NTPRS_WIDTH*12/'R.WIDTH'/                                 144
      INTEGER MTPRS_WIDTH(2)/1,1/                                         144
      REAL RE_TPRS_WIDTH                                                  144
C -                                                                       144
      REAL TPRS_SUPER                                                     144
      EQUIVALENCE (BANK_TPRS(7),TPRS_SUPER,RE_TPRS_SUPER),                144
     *(NAMES_TPRS(5),NTPRS_SUPER)                                         144
      EQUIVALENCE (COMMEN_TPRS(5),CTPRS_SUPER),(MAP_TPRS(1,5),            144
     *MTPRS_SUPER)                                                        144
      CHARACTER CTPRS_SUPER*40/' '/                                       144
      CHARACTER NTPRS_SUPER*12/'R.SUPER'/                                 144
      INTEGER MTPRS_SUPER(2)/1,1/                                         144
      REAL RE_TPRS_SUPER                                                  144
C -                                                                       144
      REAL TPRS_DANODE                                                    144
      EQUIVALENCE (BANK_TPRS(8),TPRS_DANODE,RE_TPRS_DANODE),              144
     *(NAMES_TPRS(6),NTPRS_DANODE)                                        144
      EQUIVALENCE (COMMEN_TPRS(6),CTPRS_DANODE),(MAP_TPRS(1,6),           144
     *MTPRS_DANODE)                                                       144
      CHARACTER CTPRS_DANODE*40/' '/                                      144
      CHARACTER NTPRS_DANODE*12/'R.DANODE'/                               144
      INTEGER MTPRS_DANODE(2)/1,1/                                        144
      REAL RE_TPRS_DANODE                                                 144
C -                                                                       144
      REAL TPRS_RPADS(40)                                                 144
      EQUIVALENCE (BANK_TPRS(9),TPRS_RPADS,RE_TPRS_RPADS),                144
     *(NAMES_TPRS(7),NTPRS_RPADS)                                         144
      EQUIVALENCE (COMMEN_TPRS(7),CTPRS_RPADS),(MAP_TPRS(1,7),            144
     *MTPRS_RPADS)                                                        144
      CHARACTER CTPRS_RPADS*40/' '/                                       144
      CHARACTER NTPRS_RPADS*12/'R.RPADS'/                                 144
      INTEGER MTPRS_RPADS(2)/40,1/                                        144
      REAL RE_TPRS_RPADS                                                  144
C -                                                                       144
      REAL TPRS_NPADS(40)                                                 144
      EQUIVALENCE (BANK_TPRS(49),TPRS_NPADS,RE_TPRS_NPADS),               144
     *(NAMES_TPRS(8),NTPRS_NPADS)                                         144
      EQUIVALENCE (COMMEN_TPRS(8),CTPRS_NPADS),(MAP_TPRS(1,8),            144
     *MTPRS_NPADS)                                                        144
      CHARACTER CTPRS_NPADS*40/' '/                                       144
      CHARACTER NTPRS_NPADS*12/'R.NPADS'/                                 144
      INTEGER MTPRS_NPADS(2)/40,1/                                        144
      REAL RE_TPRS_NPADS                                                  144
C -                                                                       144
      INTEGER LEN_TPRS,LLEN_TPRS,LINK_TPRS                                144
      PARAMETER (LEN_TPRS=88)                                             144
      PARAMETER (LLEN_TPRS=8)                                             144
      INTEGER FLAG_TPRS/0/,MAP_TPRS(2,LLEN_TPRS),IDNUM_TPRS(10)           144
      CHARACTER NAMES_TPRS(LLEN_TPRS)*12,COMMEN_TPRS(LLEN_TPRS)*40        144
      DATA IDNUM_TPRS/10*0/                                               144
      REAL BANK_TPRS(LEN_TPRS)                                            144
      CHARACTER*60 BPATH_TPRS/' '/                                        144
      SAVE FLAG_TPRS,MAP_TPRS,IDNUM_TPRS,NAMES_TPRS,COMMEN_TPRS,          144
     *BPATH_TPRS                                                          144
      DATA LINK_TPRS/0/,BANK_TPRS/LEN_TPRS*0/                             144
C *                                                                       144
C -                                                                       146
      REAL TECW_SEC                                                       146
      EQUIVALENCE (BANK_TECW(3),TECW_SEC,RE_TECW_SEC),(NAMES_TECW(1),     146
     *NTECW_SEC)                                                          146
      EQUIVALENCE (COMMEN_TECW(1),CTECW_SEC),(MAP_TECW(1,1),MTECW_SEC)    146
      CHARACTER CTECW_SEC*40/' '/                                         146
      CHARACTER NTECW_SEC*12/'R.SEC'/                                     146
      INTEGER MTECW_SEC(2)/1,1/                                           146
      REAL RE_TECW_SEC                                                    146
C -                                                                       146
      REAL TECW_GAPWIDI                                                   146
      EQUIVALENCE (BANK_TECW(4),TECW_GAPWIDI,RE_TECW_GAPWIDI),            146
     *(NAMES_TECW(2),NTECW_GAPWIDI)                                       146
      EQUIVALENCE (COMMEN_TECW(2),CTECW_GAPWIDI),(MAP_TECW(1,2),          146
     *MTECW_GAPWIDI)                                                      146
      CHARACTER CTECW_GAPWIDI*40/' '/                                     146
      CHARACTER NTECW_GAPWIDI*12/'R.GAPWIDI'/                             146
      INTEGER MTECW_GAPWIDI(2)/1,1/                                       146
      REAL RE_TECW_GAPWIDI                                                146
C -                                                                       146
      REAL TECW_GAPWIDO                                                   146
      EQUIVALENCE (BANK_TECW(5),TECW_GAPWIDO,RE_TECW_GAPWIDO),            146
     *(NAMES_TECW(3),NTECW_GAPWIDO)                                       146
      EQUIVALENCE (COMMEN_TECW(3),CTECW_GAPWIDO),(MAP_TECW(1,3),          146
     *MTECW_GAPWIDO)                                                      146
      CHARACTER CTECW_GAPWIDO*40/' '/                                     146
      CHARACTER NTECW_GAPWIDO*12/'R.GAPWIDO'/                             146
      INTEGER MTECW_GAPWIDO(2)/1,1/                                       146
      REAL RE_TECW_GAPWIDO                                                146
C -                                                                       146
      REAL TECW_GAPHEIT                                                   146
      EQUIVALENCE (BANK_TECW(6),TECW_GAPHEIT,RE_TECW_GAPHEIT),            146
     *(NAMES_TECW(4),NTECW_GAPHEIT)                                       146
      EQUIVALENCE (COMMEN_TECW(4),CTECW_GAPHEIT),(MAP_TECW(1,4),          146
     *MTECW_GAPHEIT)                                                      146
      CHARACTER CTECW_GAPHEIT*40/' '/                                     146
      CHARACTER NTECW_GAPHEIT*12/'R.GAPHEIT'/                             146
      INTEGER MTECW_GAPHEIT(2)/1,1/                                       146
      REAL RE_TECW_GAPHEIT                                                146
C -                                                                       146
      REAL TECW_GAPRAD                                                    146
      EQUIVALENCE (BANK_TECW(7),TECW_GAPRAD,RE_TECW_GAPRAD),              146
     *(NAMES_TECW(5),NTECW_GAPRAD)                                        146
      EQUIVALENCE (COMMEN_TECW(5),CTECW_GAPRAD),(MAP_TECW(1,5),           146
     *MTECW_GAPRAD)                                                       146
      CHARACTER CTECW_GAPRAD*40/' '/                                      146
      CHARACTER NTECW_GAPRAD*12/'R.GAPRAD'/                               146
      INTEGER MTECW_GAPRAD(2)/1,1/                                        146
      REAL RE_TECW_GAPRAD                                                 146
C -                                                                       146
      REAL TECW_INWIDTH                                                   146
      EQUIVALENCE (BANK_TECW(8),TECW_INWIDTH,RE_TECW_INWIDTH),            146
     *(NAMES_TECW(6),NTECW_INWIDTH)                                       146
      EQUIVALENCE (COMMEN_TECW(6),CTECW_INWIDTH),(MAP_TECW(1,6),          146
     *MTECW_INWIDTH)                                                      146
      CHARACTER CTECW_INWIDTH*40/' '/                                     146
      CHARACTER NTECW_INWIDTH*12/'R.INWIDTH'/                             146
      INTEGER MTECW_INWIDTH(2)/1,1/                                       146
      REAL RE_TECW_INWIDTH                                                146
C -                                                                       146
      REAL TECW_OUWIDTH                                                   146
      EQUIVALENCE (BANK_TECW(9),TECW_OUWIDTH,RE_TECW_OUWIDTH),            146
     *(NAMES_TECW(7),NTECW_OUWIDTH)                                       146
      EQUIVALENCE (COMMEN_TECW(7),CTECW_OUWIDTH),(MAP_TECW(1,7),          146
     *MTECW_OUWIDTH)                                                      146
      CHARACTER CTECW_OUWIDTH*40/' '/                                     146
      CHARACTER NTECW_OUWIDTH*12/'R.OUWIDTH'/                             146
      INTEGER MTECW_OUWIDTH(2)/1,1/                                       146
      REAL RE_TECW_OUWIDTH                                                146
C -                                                                       146
      REAL TECW_WIDTHLIP                                                  146
      EQUIVALENCE (BANK_TECW(10),TECW_WIDTHLIP,RE_TECW_WIDTHLIP),         146
     *(NAMES_TECW(8),NTECW_WIDTHLIP)                                      146
      EQUIVALENCE (COMMEN_TECW(8),CTECW_WIDTHLIP),(MAP_TECW(1,8),         146
     *MTECW_WIDTHLIP)                                                     146
      CHARACTER CTECW_WIDTHLIP*40/' '/                                    146
      CHARACTER NTECW_WIDTHLIP*12/'R.WIDTHLIP'/                           146
      INTEGER MTECW_WIDTHLIP(2)/1,1/                                      146
      REAL RE_TECW_WIDTHLIP                                               146
C -                                                                       146
      REAL TECW_NORIBS                                                    146
      EQUIVALENCE (BANK_TECW(11),TECW_NORIBS,RE_TECW_NORIBS),             146
     *(NAMES_TECW(9),NTECW_NORIBS)                                        146
      EQUIVALENCE (COMMEN_TECW(9),CTECW_NORIBS),(MAP_TECW(1,9),           146
     *MTECW_NORIBS)                                                       146
      CHARACTER CTECW_NORIBS*40/' '/                                      146
      CHARACTER NTECW_NORIBS*12/'R.NORIBS'/                               146
      INTEGER MTECW_NORIBS(2)/1,1/                                        146
      REAL RE_TECW_NORIBS                                                 146
C -                                                                       146
      REAL TECW_ZSTEPRIB                                                  146
      EQUIVALENCE (BANK_TECW(12),TECW_ZSTEPRIB,RE_TECW_ZSTEPRIB),         146
     *(NAMES_TECW(10),NTECW_ZSTEPRIB)                                     146
      EQUIVALENCE (COMMEN_TECW(10),CTECW_ZSTEPRIB),(MAP_TECW(1,10),       146
     *MTECW_ZSTEPRIB)                                                     146
      CHARACTER CTECW_ZSTEPRIB*40/' '/                                    146
      CHARACTER NTECW_ZSTEPRIB*12/'R.ZSTEPRIB'/                           146
      INTEGER MTECW_ZSTEPRIB(2)/1,1/                                      146
      REAL RE_TECW_ZSTEPRIB                                               146
C -                                                                       146
      REAL TECW_WIDTHRIB                                                  146
      EQUIVALENCE (BANK_TECW(13),TECW_WIDTHRIB,RE_TECW_WIDTHRIB),         146
     *(NAMES_TECW(11),NTECW_WIDTHRIB)                                     146
      EQUIVALENCE (COMMEN_TECW(11),CTECW_WIDTHRIB),(MAP_TECW(1,11),       146
     *MTECW_WIDTHRIB)                                                     146
      CHARACTER CTECW_WIDTHRIB*40/' '/                                    146
      CHARACTER NTECW_WIDTHRIB*12/'R.WIDTHRIB'/                           146
      INTEGER MTECW_WIDTHRIB(2)/1,1/                                      146
      REAL RE_TECW_WIDTHRIB                                               146
C -                                                                       146
      REAL TECW_HEIGHT                                                    146
      EQUIVALENCE (BANK_TECW(14),TECW_HEIGHT,RE_TECW_HEIGHT),             146
     *(NAMES_TECW(12),NTECW_HEIGHT)                                       146
      EQUIVALENCE (COMMEN_TECW(12),CTECW_HEIGHT),(MAP_TECW(1,12),         146
     *MTECW_HEIGHT)                                                       146
      CHARACTER CTECW_HEIGHT*40/' '/                                      146
      CHARACTER NTECW_HEIGHT*12/'R.HEIGHT'/                               146
      INTEGER MTECW_HEIGHT(2)/1,1/                                        146
      REAL RE_TECW_HEIGHT                                                 146
C -                                                                       146
      REAL TECW_THICK                                                     146
      EQUIVALENCE (BANK_TECW(15),TECW_THICK,RE_TECW_THICK),               146
     *(NAMES_TECW(13),NTECW_THICK)                                        146
      EQUIVALENCE (COMMEN_TECW(13),CTECW_THICK),(MAP_TECW(1,13),          146
     *MTECW_THICK)                                                        146
      CHARACTER CTECW_THICK*40/' '/                                       146
      CHARACTER NTECW_THICK*12/'R.THICK'/                                 146
      INTEGER MTECW_THICK(2)/1,1/                                         146
      REAL RE_TECW_THICK                                                  146
C -                                                                       146
      REAL TECW_THICKAL                                                   146
      EQUIVALENCE (BANK_TECW(16),TECW_THICKAL,RE_TECW_THICKAL),           146
     *(NAMES_TECW(14),NTECW_THICKAL)                                      146
      EQUIVALENCE (COMMEN_TECW(14),CTECW_THICKAL),(MAP_TECW(1,14),        146
     *MTECW_THICKAL)                                                      146
      CHARACTER CTECW_THICKAL*40/' '/                                     146
      CHARACTER NTECW_THICKAL*12/'R.THICKAL'/                             146
      INTEGER MTECW_THICKAL(2)/1,1/                                       146
      REAL RE_TECW_THICKAL                                                146
C -                                                                       146
      REAL TECW_RMIN                                                      146
      EQUIVALENCE (BANK_TECW(17),TECW_RMIN,RE_TECW_RMIN),                 146
     *(NAMES_TECW(15),NTECW_RMIN)                                         146
      EQUIVALENCE (COMMEN_TECW(15),CTECW_RMIN),(MAP_TECW(1,15),           146
     *MTECW_RMIN)                                                         146
      CHARACTER CTECW_RMIN*40/' '/                                        146
      CHARACTER NTECW_RMIN*12/'R.RMIN'/                                   146
      INTEGER MTECW_RMIN(2)/1,1/                                          146
      REAL RE_TECW_RMIN                                                   146
C -                                                                       146
      REAL TECW_RCENTER                                                   146
      EQUIVALENCE (BANK_TECW(18),TECW_RCENTER,RE_TECW_RCENTER),           146
     *(NAMES_TECW(16),NTECW_RCENTER)                                      146
      EQUIVALENCE (COMMEN_TECW(16),CTECW_RCENTER),(MAP_TECW(1,16),        146
     *MTECW_RCENTER)                                                      146
      CHARACTER CTECW_RCENTER*40/' '/                                     146
      CHARACTER NTECW_RCENTER*12/'R.RCENTER'/                             146
      INTEGER MTECW_RCENTER(2)/1,1/                                       146
      REAL RE_TECW_RCENTER                                                146
C -                                                                       146
      REAL TECW_HOLEDX                                                    146
      EQUIVALENCE (BANK_TECW(19),TECW_HOLEDX,RE_TECW_HOLEDX),             146
     *(NAMES_TECW(17),NTECW_HOLEDX)                                       146
      EQUIVALENCE (COMMEN_TECW(17),CTECW_HOLEDX),(MAP_TECW(1,17),         146
     *MTECW_HOLEDX)                                                       146
      CHARACTER CTECW_HOLEDX*40/' '/                                      146
      CHARACTER NTECW_HOLEDX*12/'R.HOLEDX'/                               146
      INTEGER MTECW_HOLEDX(2)/1,1/                                        146
      REAL RE_TECW_HOLEDX                                                 146
C -                                                                       146
      REAL TECW_HOLEDY                                                    146
      EQUIVALENCE (BANK_TECW(20),TECW_HOLEDY,RE_TECW_HOLEDY),             146
     *(NAMES_TECW(18),NTECW_HOLEDY)                                       146
      EQUIVALENCE (COMMEN_TECW(18),CTECW_HOLEDY),(MAP_TECW(1,18),         146
     *MTECW_HOLEDY)                                                       146
      CHARACTER CTECW_HOLEDY*40/' '/                                      146
      CHARACTER NTECW_HOLEDY*12/'R.HOLEDY'/                               146
      INTEGER MTECW_HOLEDY(2)/1,1/                                        146
      REAL RE_TECW_HOLEDY                                                 146
C -                                                                       146
      INTEGER LEN_TECW,LLEN_TECW,LINK_TECW                                146
      PARAMETER (LEN_TECW=20)                                             146
      PARAMETER (LLEN_TECW=18)                                            146
      INTEGER FLAG_TECW/0/,MAP_TECW(2,LLEN_TECW),IDNUM_TECW(10)           146
      CHARACTER NAMES_TECW(LLEN_TECW)*12,COMMEN_TECW(LLEN_TECW)*40        146
      DATA IDNUM_TECW/10*0/                                               146
      REAL BANK_TECW(LEN_TECW)                                            146
      CHARACTER*60 BPATH_TECW/' '/                                        146
      SAVE FLAG_TECW,MAP_TECW,IDNUM_TECW,NAMES_TECW,COMMEN_TECW,          146
     *BPATH_TECW                                                          146
      DATA LINK_TECW/0/,BANK_TECW/LEN_TECW*0/                             146
C *                                                                       146
C -                                                                       147
      REAL TPCR_RDOVTHK                                                   147
      EQUIVALENCE (BANK_TPCR(3),TPCR_RDOVTHK,RE_TPCR_RDOVTHK),            147
     *(NAMES_TPCR(1),NTPCR_RDOVTHK)                                       147
      EQUIVALENCE (COMMEN_TPCR(1),CTPCR_RDOVTHK),(MAP_TPCR(1,1),          147
     *MTPCR_RDOVTHK)                                                      147
      CHARACTER CTPCR_RDOVTHK*40/' '/                                     147
      CHARACTER NTPCR_RDOVTHK*12/'R.RDOVTHK'/                             147
      INTEGER MTPCR_RDOVTHK(2)/1,1/                                       147
      REAL RE_TPCR_RDOVTHK                                                147
C -                                                                       147
      REAL TPCR_RDOTHK                                                    147
      EQUIVALENCE (BANK_TPCR(4),TPCR_RDOTHK,RE_TPCR_RDOTHK),              147
     *(NAMES_TPCR(2),NTPCR_RDOTHK)                                        147
      EQUIVALENCE (COMMEN_TPCR(2),CTPCR_RDOTHK),(MAP_TPCR(1,2),           147
     *MTPCR_RDOTHK)                                                       147
      CHARACTER CTPCR_RDOTHK*40/' '/                                      147
      CHARACTER NTPCR_RDOTHK*12/'R.RDOTHK'/                               147
      INTEGER MTPCR_RDOTHK(2)/1,1/                                        147
      REAL RE_TPCR_RDOTHK                                                 147
C -                                                                       147
      REAL TPCR_RDOLEN                                                    147
      EQUIVALENCE (BANK_TPCR(5),TPCR_RDOLEN,RE_TPCR_RDOLEN),              147
     *(NAMES_TPCR(3),NTPCR_RDOLEN)                                        147
      EQUIVALENCE (COMMEN_TPCR(3),CTPCR_RDOLEN),(MAP_TPCR(1,3),           147
     *MTPCR_RDOLEN)                                                       147
      CHARACTER CTPCR_RDOLEN*40/' '/                                      147
      CHARACTER NTPCR_RDOLEN*12/'R.RDOLEN'/                               147
      INTEGER MTPCR_RDOLEN(2)/1,1/                                        147
      REAL RE_TPCR_RDOLEN                                                 147
C -                                                                       147
      REAL TPCR_NRDOBRD                                                   147
      EQUIVALENCE (BANK_TPCR(6),TPCR_NRDOBRD,RE_TPCR_NRDOBRD),            147
     *(NAMES_TPCR(4),NTPCR_NRDOBRD)                                       147
      EQUIVALENCE (COMMEN_TPCR(4),CTPCR_NRDOBRD),(MAP_TPCR(1,4),          147
     *MTPCR_NRDOBRD)                                                      147
      CHARACTER CTPCR_NRDOBRD*40/' '/                                     147
      CHARACTER NTPCR_NRDOBRD*12/'R.NRDOBRD'/                             147
      INTEGER MTPCR_NRDOBRD(2)/1,1/                                       147
      REAL RE_TPCR_NRDOBRD                                                147
C -                                                                       147
      REAL TPCR_RDOHT(9)                                                  147
      EQUIVALENCE (BANK_TPCR(7),TPCR_RDOHT,RE_TPCR_RDOHT),                147
     *(NAMES_TPCR(5),NTPCR_RDOHT)                                         147
      EQUIVALENCE (COMMEN_TPCR(5),CTPCR_RDOHT),(MAP_TPCR(1,5),            147
     *MTPCR_RDOHT)                                                        147
      CHARACTER CTPCR_RDOHT*40/' '/                                       147
      CHARACTER NTPCR_RDOHT*12/'R.RDOHT'/                                 147
      INTEGER MTPCR_RDOHT(2)/9,1/                                         147
      REAL RE_TPCR_RDOHT                                                  147
C -                                                                       147
      INTEGER LEN_TPCR,LLEN_TPCR,LINK_TPCR                                147
      PARAMETER (LEN_TPCR=15)                                             147
      PARAMETER (LLEN_TPCR=5)                                             147
      INTEGER FLAG_TPCR/0/,MAP_TPCR(2,LLEN_TPCR),IDNUM_TPCR(10)           147
      CHARACTER NAMES_TPCR(LLEN_TPCR)*12,COMMEN_TPCR(LLEN_TPCR)*40        147
      DATA IDNUM_TPCR/10*0/                                               147
      REAL BANK_TPCR(LEN_TPCR)                                            147
      CHARACTER*60 BPATH_TPCR/' '/                                        147
      SAVE FLAG_TPCR,MAP_TPCR,IDNUM_TPCR,NAMES_TPCR,COMMEN_TPCR,          147
     *BPATH_TPCR                                                          147
      DATA LINK_TPCR/0/,BANK_TPCR/LEN_TPCR*0/                             147
C *                                                                       147
C -                                                                       149
      REAL TFEE_VERS                                                      149
      EQUIVALENCE (BANK_TFEE(3),TFEE_VERS,RE_TFEE_VERS),(NAMES_TFEE(1),   149
     *NTFEE_VERS)                                                         149
      EQUIVALENCE (COMMEN_TFEE(1),CTFEE_VERS),(MAP_TFEE(1,1),             149
     *MTFEE_VERS)                                                         149
      CHARACTER CTFEE_VERS*40/' '/                                        149
      CHARACTER NTFEE_VERS*12/'R.VERS'/                                   149
      INTEGER MTFEE_VERS(2)/1,1/                                          149
      REAL RE_TFEE_VERS                                                   149
C -                                                                       149
      REAL TFEE_CARDDX                                                    149
      EQUIVALENCE (BANK_TFEE(4),TFEE_CARDDX,RE_TFEE_CARDDX),              149
     *(NAMES_TFEE(2),NTFEE_CARDDX)                                        149
      EQUIVALENCE (COMMEN_TFEE(2),CTFEE_CARDDX),(MAP_TFEE(1,2),           149
     *MTFEE_CARDDX)                                                       149
      CHARACTER CTFEE_CARDDX*40/' '/                                      149
      CHARACTER NTFEE_CARDDX*12/'R.CARDDX'/                               149
      INTEGER MTFEE_CARDDX(2)/1,1/                                        149
      REAL RE_TFEE_CARDDX                                                 149
C -                                                                       149
      REAL TFEE_CARDDY                                                    149
      EQUIVALENCE (BANK_TFEE(5),TFEE_CARDDY,RE_TFEE_CARDDY),              149
     *(NAMES_TFEE(3),NTFEE_CARDDY)                                        149
      EQUIVALENCE (COMMEN_TFEE(3),CTFEE_CARDDY),(MAP_TFEE(1,3),           149
     *MTFEE_CARDDY)                                                       149
      CHARACTER CTFEE_CARDDY*40/' '/                                      149
      CHARACTER NTFEE_CARDDY*12/'R.CARDDY'/                               149
      INTEGER MTFEE_CARDDY(2)/1,1/                                        149
      REAL RE_TFEE_CARDDY                                                 149
C -                                                                       149
      REAL TFEE_CARDDZ                                                    149
      EQUIVALENCE (BANK_TFEE(6),TFEE_CARDDZ,RE_TFEE_CARDDZ),              149
     *(NAMES_TFEE(4),NTFEE_CARDDZ)                                        149
      EQUIVALENCE (COMMEN_TFEE(4),CTFEE_CARDDZ),(MAP_TFEE(1,4),           149
     *MTFEE_CARDDZ)                                                       149
      CHARACTER CTFEE_CARDDZ*40/' '/                                      149
      CHARACTER NTFEE_CARDDZ*12/'R.CARDDZ'/                               149
      INTEGER MTFEE_CARDDZ(2)/1,1/                                        149
      REAL RE_TFEE_CARDDZ                                                 149
C -                                                                       149
      REAL TFEE_PLATEDX                                                   149
      EQUIVALENCE (BANK_TFEE(7),TFEE_PLATEDX,RE_TFEE_PLATEDX),            149
     *(NAMES_TFEE(5),NTFEE_PLATEDX)                                       149
      EQUIVALENCE (COMMEN_TFEE(5),CTFEE_PLATEDX),(MAP_TFEE(1,5),          149
     *MTFEE_PLATEDX)                                                      149
      CHARACTER CTFEE_PLATEDX*40/' '/                                     149
      CHARACTER NTFEE_PLATEDX*12/'R.PLATEDX'/                             149
      INTEGER MTFEE_PLATEDX(2)/1,1/                                       149
      REAL RE_TFEE_PLATEDX                                                149
C -                                                                       149
      REAL TFEE_PLATEDY                                                   149
      EQUIVALENCE (BANK_TFEE(8),TFEE_PLATEDY,RE_TFEE_PLATEDY),            149
     *(NAMES_TFEE(6),NTFEE_PLATEDY)                                       149
      EQUIVALENCE (COMMEN_TFEE(6),CTFEE_PLATEDY),(MAP_TFEE(1,6),          149
     *MTFEE_PLATEDY)                                                      149
      CHARACTER CTFEE_PLATEDY*40/' '/                                     149
      CHARACTER NTFEE_PLATEDY*12/'R.PLATEDY'/                             149
      INTEGER MTFEE_PLATEDY(2)/1,1/                                       149
      REAL RE_TFEE_PLATEDY                                                149
C -                                                                       149
      REAL TFEE_PLATEDZ                                                   149
      EQUIVALENCE (BANK_TFEE(9),TFEE_PLATEDZ,RE_TFEE_PLATEDZ),            149
     *(NAMES_TFEE(7),NTFEE_PLATEDZ)                                       149
      EQUIVALENCE (COMMEN_TFEE(7),CTFEE_PLATEDZ),(MAP_TFEE(1,7),          149
     *MTFEE_PLATEDZ)                                                      149
      CHARACTER CTFEE_PLATEDZ*40/' '/                                     149
      CHARACTER NTFEE_PLATEDZ*12/'R.PLATEDZ'/                             149
      INTEGER MTFEE_PLATEDZ(2)/1,1/                                       149
      REAL RE_TFEE_PLATEDZ                                                149
C -                                                                       149
      REAL TFEE_ASSEMBLYTHICKNESS                                         149
      EQUIVALENCE (BANK_TFEE(10),TFEE_ASSEMBLYTHICKNESS,                  149
     *RE_TFEE_ASSEMBLYTHICKNESS),(NAMES_TFEE(8),                          149
     *NTFEE_ASSEMBLYTHICKNESS)                                            149
      EQUIVALENCE (COMMEN_TFEE(8),CTFEE_ASSEMBLYTHICKNESS),(MAP_TFEE(1,   149
     *8),MTFEE_ASSEMBLYTHICKNESS)                                         149
      CHARACTER CTFEE_ASSEMBLYTHICKNESS*40/' '/                           149
      CHARACTER NTFEE_ASSEMBLYTHICKNESS*12/'R.ASSEMBLYTHICKNESS'/         149
      INTEGER MTFEE_ASSEMBLYTHICKNESS(2)/1,1/                             149
      REAL RE_TFEE_ASSEMBLYTHICKNESS                                      149
C -                                                                       149
      REAL TFEE_RIBDX                                                     149
      EQUIVALENCE (BANK_TFEE(11),TFEE_RIBDX,RE_TFEE_RIBDX),               149
     *(NAMES_TFEE(9),NTFEE_RIBDX)                                         149
      EQUIVALENCE (COMMEN_TFEE(9),CTFEE_RIBDX),(MAP_TFEE(1,9),            149
     *MTFEE_RIBDX)                                                        149
      CHARACTER CTFEE_RIBDX*40/' '/                                       149
      CHARACTER NTFEE_RIBDX*12/'R.RIBDX'/                                 149
      INTEGER MTFEE_RIBDX(2)/1,1/                                         149
      REAL RE_TFEE_RIBDX                                                  149
C -                                                                       149
      REAL TFEE_RIBDY                                                     149
      EQUIVALENCE (BANK_TFEE(12),TFEE_RIBDY,RE_TFEE_RIBDY),               149
     *(NAMES_TFEE(10),NTFEE_RIBDY)                                        149
      EQUIVALENCE (COMMEN_TFEE(10),CTFEE_RIBDY),(MAP_TFEE(1,10),          149
     *MTFEE_RIBDY)                                                        149
      CHARACTER CTFEE_RIBDY*40/' '/                                       149
      CHARACTER NTFEE_RIBDY*12/'R.RIBDY'/                                 149
      INTEGER MTFEE_RIBDY(2)/1,1/                                         149
      REAL RE_TFEE_RIBDY                                                  149
C -                                                                       149
      REAL TFEE_RIBDZ                                                     149
      EQUIVALENCE (BANK_TFEE(13),TFEE_RIBDZ,RE_TFEE_RIBDZ),               149
     *(NAMES_TFEE(11),NTFEE_RIBDZ)                                        149
      EQUIVALENCE (COMMEN_TFEE(11),CTFEE_RIBDZ),(MAP_TFEE(1,11),          149
     *MTFEE_RIBDZ)                                                        149
      CHARACTER CTFEE_RIBDZ*40/' '/                                       149
      CHARACTER NTFEE_RIBDZ*12/'R.RIBDZ'/                                 149
      INTEGER MTFEE_RIBDZ(2)/1,1/                                         149
      REAL RE_TFEE_RIBDZ                                                  149
C -                                                                       149
      REAL TFEE_ASS(3)                                                    149
      EQUIVALENCE (BANK_TFEE(14),TFEE_ASS,RE_TFEE_ASS),(NAMES_TFEE(12),   149
     *NTFEE_ASS)                                                          149
      EQUIVALENCE (COMMEN_TFEE(12),CTFEE_ASS),(MAP_TFEE(1,12),            149
     *MTFEE_ASS)                                                          149
      CHARACTER CTFEE_ASS*40/' '/                                         149
      CHARACTER NTFEE_ASS*12/'R.ASS'/                                     149
      INTEGER MTFEE_ASS(2)/3,1/                                           149
      REAL RE_TFEE_ASS                                                    149
C -                                                                       149
      INTEGER LEN_TFEE,LLEN_TFEE,LINK_TFEE                                149
      PARAMETER (LEN_TFEE=16)                                             149
      PARAMETER (LLEN_TFEE=12)                                            149
      INTEGER FLAG_TFEE/0/,MAP_TFEE(2,LLEN_TFEE),IDNUM_TFEE(10)           149
      CHARACTER NAMES_TFEE(LLEN_TFEE)*12,COMMEN_TFEE(LLEN_TFEE)*40        149
      DATA IDNUM_TFEE/10*0/                                               149
      REAL BANK_TFEE(LEN_TFEE)                                            149
      CHARACTER*60 BPATH_TFEE/' '/                                        149
      SAVE FLAG_TFEE,MAP_TFEE,IDNUM_TFEE,NAMES_TFEE,COMMEN_TFEE,          149
     *BPATH_TFEE                                                          149
      DATA LINK_TFEE/0/,BANK_TFEE/LEN_TFEE*0/                             149
C *                                                                       149
C -                                                                       150
      REAL RIBS_VERSION                                                   150
      EQUIVALENCE (BANK_RIBS(3),RIBS_VERSION,RE_RIBS_VERSION),            150
     *(NAMES_RIBS(1),NRIBS_VERSION)                                       150
      EQUIVALENCE (COMMEN_RIBS(1),CRIBS_VERSION),(MAP_RIBS(1,1),          150
     *MRIBS_VERSION)                                                      150
      CHARACTER CRIBS_VERSION*40/' '/                                     150
      CHARACTER NRIBS_VERSION*12/'R.VERSION'/                             150
      INTEGER MRIBS_VERSION(2)/1,1/                                       150
      REAL RE_RIBS_VERSION                                                150
C -                                                                       150
      REAL RIBS_INOUT                                                     150
      EQUIVALENCE (BANK_RIBS(4),RIBS_INOUT,RE_RIBS_INOUT),                150
     *(NAMES_RIBS(2),NRIBS_INOUT)                                         150
      EQUIVALENCE (COMMEN_RIBS(2),CRIBS_INOUT),(MAP_RIBS(1,2),            150
     *MRIBS_INOUT)                                                        150
      CHARACTER CRIBS_INOUT*40/' '/                                       150
      CHARACTER NRIBS_INOUT*12/'R.INOUT'/                                 150
      INTEGER MRIBS_INOUT(2)/1,1/                                         150
      REAL RE_RIBS_INOUT                                                  150
C -                                                                       150
      REAL RIBS_NUM                                                       150
      EQUIVALENCE (BANK_RIBS(5),RIBS_NUM,RE_RIBS_NUM),(NAMES_RIBS(3),     150
     *NRIBS_NUM)                                                          150
      EQUIVALENCE (COMMEN_RIBS(3),CRIBS_NUM),(MAP_RIBS(1,3),MRIBS_NUM)    150
      CHARACTER CRIBS_NUM*40/' '/                                         150
      CHARACTER NRIBS_NUM*12/'R.NUM'/                                     150
      INTEGER MRIBS_NUM(2)/1,1/                                           150
      REAL RE_RIBS_NUM                                                    150
C -                                                                       150
      REAL RIBS_X(16)                                                     150
      EQUIVALENCE (BANK_RIBS(6),RIBS_X,RE_RIBS_X),(NAMES_RIBS(4),         150
     *NRIBS_X)                                                            150
      EQUIVALENCE (COMMEN_RIBS(4),CRIBS_X),(MAP_RIBS(1,4),MRIBS_X)        150
      CHARACTER CRIBS_X*40/' '/                                           150
      CHARACTER NRIBS_X*12/'R.X'/                                         150
      INTEGER MRIBS_X(2)/16,1/                                            150
      REAL RE_RIBS_X                                                      150
C -                                                                       150
      REAL RIBS_DX(16)                                                    150
      EQUIVALENCE (BANK_RIBS(22),RIBS_DX,RE_RIBS_DX),(NAMES_RIBS(5),      150
     *NRIBS_DX)                                                           150
      EQUIVALENCE (COMMEN_RIBS(5),CRIBS_DX),(MAP_RIBS(1,5),MRIBS_DX)      150
      CHARACTER CRIBS_DX*40/' '/                                          150
      CHARACTER NRIBS_DX*12/'R.DX'/                                       150
      INTEGER MRIBS_DX(2)/16,1/                                           150
      REAL RE_RIBS_DX                                                     150
C -                                                                       150
      REAL RIBS_Y(16)                                                     150
      EQUIVALENCE (BANK_RIBS(38),RIBS_Y,RE_RIBS_Y),(NAMES_RIBS(6),        150
     *NRIBS_Y)                                                            150
      EQUIVALENCE (COMMEN_RIBS(6),CRIBS_Y),(MAP_RIBS(1,6),MRIBS_Y)        150
      CHARACTER CRIBS_Y*40/' '/                                           150
      CHARACTER NRIBS_Y*12/'R.Y'/                                         150
      INTEGER MRIBS_Y(2)/16,1/                                            150
      REAL RE_RIBS_Y                                                      150
C -                                                                       150
      REAL RIBS_DY(16)                                                    150
      EQUIVALENCE (BANK_RIBS(54),RIBS_DY,RE_RIBS_DY),(NAMES_RIBS(7),      150
     *NRIBS_DY)                                                           150
      EQUIVALENCE (COMMEN_RIBS(7),CRIBS_DY),(MAP_RIBS(1,7),MRIBS_DY)      150
      CHARACTER CRIBS_DY*40/' '/                                          150
      CHARACTER NRIBS_DY*12/'R.DY'/                                       150
      INTEGER MRIBS_DY(2)/16,1/                                           150
      REAL RE_RIBS_DY                                                     150
C -                                                                       150
      INTEGER LEN_RIBS,LLEN_RIBS,LINK_RIBS                                150
      PARAMETER (LEN_RIBS=69)                                             150
      PARAMETER (LLEN_RIBS=7)                                             150
      INTEGER FLAG_RIBS/0/,MAP_RIBS(2,LLEN_RIBS),IDNUM_RIBS(10)           150
      CHARACTER NAMES_RIBS(LLEN_RIBS)*12,COMMEN_RIBS(LLEN_RIBS)*40        150
      DATA IDNUM_RIBS/10*0/                                               150
      REAL BANK_RIBS(LEN_RIBS)                                            150
      CHARACTER*60 BPATH_RIBS/' '/                                        150
      SAVE FLAG_RIBS,MAP_RIBS,IDNUM_RIBS,NAMES_RIBS,COMMEN_RIBS,          150
     *BPATH_RIBS                                                          150
      DATA LINK_RIBS/0/,BANK_RIBS/LEN_RIBS*0/                             150
C *                                                                       150
C -                                                                       151
      INTEGER HOLZ_INOUT                                                  151
      EQUIVALENCE (BANK_HOLZ(3),HOLZ_INOUT,RE_HOLZ_INOUT),                151
     *(NAMES_HOLZ(1),NHOLZ_INOUT)                                         151
      EQUIVALENCE (COMMEN_HOLZ(1),CHOLZ_INOUT),(MAP_HOLZ(1,1),            151
     *MHOLZ_INOUT)                                                        151
      CHARACTER CHOLZ_INOUT*40/' '/                                       151
      CHARACTER NHOLZ_INOUT*12/'I.INOUT'/                                 151
      INTEGER MHOLZ_INOUT(2)/1,1/                                         151
      REAL RE_HOLZ_INOUT                                                  151
C -                                                                       151
      INTEGER HOLZ_NUMBEROFROWS                                           151
      EQUIVALENCE (BANK_HOLZ(4),HOLZ_NUMBEROFROWS,RE_HOLZ_NUMBEROFROWS),  151
     *(NAMES_HOLZ(2),NHOLZ_NUMBEROFROWS)                                  151
      EQUIVALENCE (COMMEN_HOLZ(2),CHOLZ_NUMBEROFROWS),(MAP_HOLZ(1,2),     151
     *MHOLZ_NUMBEROFROWS)                                                 151
      CHARACTER CHOLZ_NUMBEROFROWS*40/' '/                                151
      CHARACTER NHOLZ_NUMBEROFROWS*12/'I.NUMBEROFROWS'/                   151
      INTEGER MHOLZ_NUMBEROFROWS(2)/1,1/                                  151
      REAL RE_HOLZ_NUMBEROFROWS                                           151
C -                                                                       151
      INTEGER HOLZ_NUMBERPERROW(16)                                       151
      EQUIVALENCE (BANK_HOLZ(5),HOLZ_NUMBERPERROW,RE_HOLZ_NUMBERPERROW),  151
     *(NAMES_HOLZ(3),NHOLZ_NUMBERPERROW)                                  151
      EQUIVALENCE (COMMEN_HOLZ(3),CHOLZ_NUMBERPERROW),(MAP_HOLZ(1,3),     151
     *MHOLZ_NUMBERPERROW)                                                 151
      CHARACTER CHOLZ_NUMBERPERROW*40/' '/                                151
      CHARACTER NHOLZ_NUMBERPERROW*12/'I.NUMBERPERROW'/                   151
      INTEGER MHOLZ_NUMBERPERROW(2)/16,1/                                 151
      REAL RE_HOLZ_NUMBERPERROW                                           151
C -                                                                       151
      REAL HOLZ_YHOLES(160)                                               151
      EQUIVALENCE (BANK_HOLZ(21),HOLZ_YHOLES,RE_HOLZ_YHOLES),             151
     *(NAMES_HOLZ(4),NHOLZ_YHOLES)                                        151
      EQUIVALENCE (COMMEN_HOLZ(4),CHOLZ_YHOLES),(MAP_HOLZ(1,4),           151
     *MHOLZ_YHOLES)                                                       151
      CHARACTER CHOLZ_YHOLES*40/' '/                                      151
      CHARACTER NHOLZ_YHOLES*12/'R.YHOLES'/                               151
      INTEGER MHOLZ_YHOLES(2)/160,1/                                      151
      REAL RE_HOLZ_YHOLES                                                 151
C -                                                                       151
      INTEGER LEN_HOLZ,LLEN_HOLZ,LINK_HOLZ                                151
      PARAMETER (LEN_HOLZ=180)                                            151
      PARAMETER (LLEN_HOLZ=4)                                             151
      INTEGER FLAG_HOLZ/0/,MAP_HOLZ(2,LLEN_HOLZ),IDNUM_HOLZ(10)           151
      CHARACTER NAMES_HOLZ(LLEN_HOLZ)*12,COMMEN_HOLZ(LLEN_HOLZ)*40        151
      DATA IDNUM_HOLZ/10*0/                                               151
      REAL BANK_HOLZ(LEN_HOLZ)                                            151
      CHARACTER*60 BPATH_HOLZ/' '/                                        151
      SAVE FLAG_HOLZ,MAP_HOLZ,IDNUM_HOLZ,NAMES_HOLZ,COMMEN_HOLZ,          151
     *BPATH_HOLZ                                                          151
      DATA LINK_HOLZ/0/,BANK_HOLZ/LEN_HOLZ*0/                             151
C *                                                                       151
C -                                                                       152
      INTEGER COOL_INOUT                                                  152
      EQUIVALENCE (BANK_COOL(3),COOL_INOUT,RE_COOL_INOUT),                152
     *(NAMES_COOL(1),NCOOL_INOUT)                                         152
      EQUIVALENCE (COMMEN_COOL(1),CCOOL_INOUT),(MAP_COOL(1,1),            152
     *MCOOL_INOUT)                                                        152
      CHARACTER CCOOL_INOUT*40/' '/                                       152
      CHARACTER NCOOL_INOUT*12/'I.INOUT'/                                 152
      INTEGER MCOOL_INOUT(2)/1,1/                                         152
      REAL RE_COOL_INOUT                                                  152
C -                                                                       152
      REAL COOL_TUBELENGTH(16)                                            152
      EQUIVALENCE (BANK_COOL(4),COOL_TUBELENGTH,RE_COOL_TUBELENGTH),      152
     *(NAMES_COOL(2),NCOOL_TUBELENGTH)                                    152
      EQUIVALENCE (COMMEN_COOL(2),CCOOL_TUBELENGTH),(MAP_COOL(1,2),       152
     *MCOOL_TUBELENGTH)                                                   152
      CHARACTER CCOOL_TUBELENGTH*40/' '/                                  152
      CHARACTER NCOOL_TUBELENGTH*12/'R.TUBELENGTH'/                       152
      INTEGER MCOOL_TUBELENGTH(2)/16,1/                                   152
      REAL RE_COOL_TUBELENGTH                                             152
C -                                                                       152
      INTEGER LEN_COOL,LLEN_COOL,LINK_COOL                                152
      PARAMETER (LEN_COOL=19)                                             152
      PARAMETER (LLEN_COOL=2)                                             152
      INTEGER FLAG_COOL/0/,MAP_COOL(2,LLEN_COOL),IDNUM_COOL(10)           152
      CHARACTER NAMES_COOL(LLEN_COOL)*12,COMMEN_COOL(LLEN_COOL)*40        152
      DATA IDNUM_COOL/10*0/                                               152
      REAL BANK_COOL(LEN_COOL)                                            152
      CHARACTER*60 BPATH_COOL/' '/                                        152
      SAVE FLAG_COOL,MAP_COOL,IDNUM_COOL,NAMES_COOL,COMMEN_COOL,          152
     *BPATH_COOL                                                          152
      DATA LINK_COOL/0/,BANK_COOL/LEN_COOL*0/                             152
C *                                                                       152
C *                                                                       153
      CALL AGSBEGM(MODULE,IPRIN)                                          153
      IF (IPRIN.LT.0) RETURN                                              153
C Filling bank TPCC - SELECTED TPCG VERSION SUBVERSION                    153
      BANK_TITLE='SELECTED TPCG VERSION SUBVERSION'                       153
      BPATH_TPCC='TPCC*'                                                  153
      IDNUM_TPCC(1)=0                                                     154
C DEFAULT IS 3.0                                                          154
      TPCC_VERSION=3.0                                                    154
      CTPCC_VERSION='DEFAULT IS 3.0'                                      154
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCC',    155
     *BPATH_TPCC,IDNUM_TPCC,1, MAP_TPCC,NAMES_TPCC,COMMEN_TPCC,           155
     *BANK_TPCC,LEN_TPCC,LLEN_TPCC,LINK_TPCC,FLAG_TPCC)                   155
C *                                                                       155
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCC',    155
     *BPATH_TPCC,IDNUM_TPCC,1, MAP_TPCC,NAMES_TPCC,COMMEN_TPCC,           155
     *BANK_TPCC,LEN_TPCC,LLEN_TPCC,LINK_TPCC,FLAG_TPCC)                   155
C *                                                                       156
      CALL AGSBEGM(MODULE,IPRIN)                                          156
      IF (IPRIN.LT.0) RETURN                                              156
C Filling bank TPCG - // TPC BASIC DIMENSIONS                             156
      BANK_TITLE='// TPC BASIC DIMENSIONS'                                156
      BPATH_TPCG='TPCG*'                                                  156
      IDNUM_TPCG(1)=0                                                     157
C // VERSION => CURRENT VERSION                                           157
      TPCG_VERSION=3.0                                                    157
      CTPCG_VERSION='// VERSION => CURRENT VERSION'                       157
C // RMIN => TPC ENVELOPE INNER RADIUS                                    158
      TPCG_RMIN=46.107                                                    158
      CTPCG_RMIN='// RMIN => TPC ENVELOPE INNER RADIUS'                   158
C // RMAX => TPC ENVELOPE OUTER RADIUS                                    159
      TPCG_RMAX=206.75/COS15                                              159
      CTPCG_RMAX='// RMAX => TPC ENVELOPE OUTER RADIUS'                   159
C // RMINIFC => INNER RADIUS TPC IFC : A.LEBEDEV MEASUREMENT 10/16/08     160
      TPCG_RMINIFC=46.6                                                   160
      CTPCG_RMINIFC='// RMINIFC => INNER RADIUS TPC IFC : A.LEBEDEV MEAS  160
     *UREMENT 10/16/08'                                                   160
C // LENGTHT => TPC FULL LENGTH UP TO FRONT OF EEMC                       161
      TPCG_LENGTHT=2*271.0                                                161
      CTPCG_LENGTHT='// LENGTHT => TPC FULL LENGTH UP TO FRONT OF EEMC'   161
C // LENGTH => TPC FULL LENGTH INCLUDING RDOS                             162
      TPCG_LENGTH=2*259.685                                               162
      CTPCG_LENGTH='// LENGTH => TPC FULL LENGTH INCLUDING RDOS'          162
C // LENGTHW => TPC LENGTH INCLUDING WHEEL                                163
      TPCG_LENGTHW=2*229.71                                               163
      CTPCG_LENGTHW='// LENGTHW => TPC LENGTH INCLUDING WHEEL'            163
C // LENGTHV => TPC GAS VOLUME LENGTH                                     164
      TPCG_LENGTHV=2*210.00                                               164
      CTPCG_LENGTHV='// LENGTHV => TPC GAS VOLUME LENGTH'                 164
C // TPCE ENVELOP DZ                                                      165
      TPCG_DZENVELOP=268.                                                 165
      CTPCG_DZENVELOP='// TPCE ENVELOP DZ'                                165
C // 49.60 WHEELIR => SUPPORT WHEEL INNER RADIUS                          166
      TPCG_WHEELIR=38.620*INCH/2                                          166
      CTPCG_WHEELIR='// 49.60 WHEELIR => SUPPORT WHEEL INNER RADIUS'      166
C // WHEELR0 => DISTANCE FROM IP OF END OF INNER CYLINDRICAL PART         167
      TPCG_WHEELR0=21.500*INCH                                            167
      CTPCG_WHEELR0='// WHEELR0 => DISTANCE FROM IP OF END OF INNER CYLI  167
     *NDRICAL PART'                                                       167
C // WHEELR1 => DISTANCE FROM IP OF MIDDLE RIB                            168
      TPCG_WHEELR1=47.867*INCH                                            168
      CTPCG_WHEELR1='// WHEELR1 => DISTANCE FROM IP OF MIDDLE RIB'        168
C // WHEELR2 => DISTANCE FROM IP OF UPPER RIB                             169
      TPCG_WHEELR2=76.093*INCH                                            169
      CTPCG_WHEELR2='// WHEELR2 => DISTANCE FROM IP OF UPPER RIB'         169
C // WHEELTOTALRIBWIDTH => TOTAL RIB WIDTH (A.LEBEDEV MEASUREMENT)        170
      TPCG_WHEELTOTALRIBWIDTH=7.6                                         170
      CTPCG_WHEELTOTALRIBWIDTH='// WHEELTOTALRIBWIDTH => TOTAL RIB WIDTH  170
     * (A.LEBEDEV MEASUREMENT)'                                           170
C // WHEELRIBWIDTH => RIB PLATE WIDTH (A.LEBEDEV MEASUREMENT)             171
      TPCG_WHEELRIBWIDTH=1.9                                              171
      CTPCG_WHEELRIBWIDTH='// WHEELRIBWIDTH => RIB PLATE WIDTH (A.LEBEDE  171
     *V MEASUREMENT)'                                                     171
C // WHEELRIBHEIGHT => RIB PLATE HEIGHT (A.LEBEDEV MEASUREMENT)           172
      TPCG_WHEELRIBHEIGHT=1.3                                             172
      CTPCG_WHEELRIBHEIGHT='// WHEELRIBHEIGHT => RIB PLATE HEIGHT (A.LEB  172
     *EDEV MEASUREMENT)'                                                  172
C // WHEELBOXDX (A.LEBEDEV MEASUREMENT)                                   173
      TPCG_WHEELBOXDX=4.7                                                 173
      CTPCG_WHEELBOXDX='// WHEELBOXDX (A.LEBEDEV MEASUREMENT)'            173
C // WHEELBOXDY (A.LEBEDEV MEASUREMENT)                                   174
      TPCG_WHEELBOXDY=7.6                                                 174
      CTPCG_WHEELBOXDY='// WHEELBOXDY (A.LEBEDEV MEASUREMENT)'            174
C // WHEELOR1 => @ TOFC                                                   175
      TPCG_WHEELOR1=201.0                                                 175
      CTPCG_WHEELOR1='// WHEELOR1 => @ TOFC'                              175
C // WHEELOR => SUPPORT WHEEL OUTER RADIUS                                176
      TPCG_WHEELOR=206.75                                                 176
      CTPCG_WHEELOR='// WHEELOR => SUPPORT WHEEL OUTER RADIUS'            176
C // WHEELTHK1                                                            177
      TPCG_WHEELTHK1=5.72                                                 177
      CTPCG_WHEELTHK1='// WHEELTHK1'                                      177
C // WHEELTHK => SUPPORT WHEEL LENGTH                                     178
      TPCG_WHEELTHK=11.43                                                 178
      CTPCG_WHEELTHK='// WHEELTHK => SUPPORT WHEEL LENGTH'                178
C // SENGASOR => TPC SENSITIVE GAS OUTER RADIUS                           179
      TPCG_SENGASOR=200                                                   179
      CTPCG_SENGASOR='// SENGASOR => TPC SENSITIVE GAS OUTER RADIUS'      179
C // MEMBTHK => CENTRAL MEMBRANE THICKNESS                                180
      TPCG_MEMBTHK=0.00762                                                180
      CTPCG_MEMBTHK='// MEMBTHK => CENTRAL MEMBRANE THICKNESS'            180
C // TOCSDR => OUTER COPPER THICKNESS: NIM A499 (2003) 659-678 TABLE 2    181
      TPCG_TOCSDR=0.013                                                   181
      CTPCG_TOCSDR='// TOCSDR => OUTER COPPER THICKNESS: NIM A499 (2003)  181
     * 659-678 TABLE 2'                                                   181
C // TOKADR => OUTER KAPTON THICKNESS                                     182
      TPCG_TOKADR=0.015                                                   182
      CTPCG_TOKADR='// TOKADR => OUTER KAPTON THICKNESS'                  182
C // TONXDR => OUTER NOMEX THICKNESS                                      183
      TPCG_TONXDR=0.953                                                   183
      CTPCG_TONXDR='// TONXDR => OUTER NOMEX THICKNESS'                   183
C // TOADDR => OUTER ADHESIVE THICKNESS                                   184
      TPCG_TOADDR=0.05                                                    184
      CTPCG_TOADDR='// TOADDR => OUTER ADHESIVE THICKNESS'                184
C // TOIGDR => OUTER ISOLATING GAS THICKNESS                              185
      TPCG_TOIGDR=5.70                                                    185
      CTPCG_TOIGDR='// TOIGDR => OUTER ISOLATING GAS THICKNESS'           185
C // TOALDR => OUTER ALUMINUM THICKNESS (FOR BOTH LAYERS)                 186
      TPCG_TOALDR=0.40                                                    186
      CTPCG_TOALDR='// TOALDR => OUTER ALUMINUM THICKNESS (FOR BOTH LAYE  186
     *RS)'                                                                186
C // TOHADR => OUTER HONEYCOMB AL THICKNESS                               187
      TPCG_TOHADR=0.60                                                    187
      CTPCG_TOHADR='// TOHADR => OUTER HONEYCOMB AL THICKNESS'            187
C // TIADDR => INNER ADHESIVE LAYER THICKNESS: NIM A499 (2003) 659-678 T  188
C ABLE 2                                                                  188
      TPCG_TIADDR=0.080                                                   188
      CTPCG_TIADDR='// TIADDR => INNER ADHESIVE LAYER THICKNESS: NIM A49  188
     *9 (2003) 659-678 TABLE 2'                                           188
C // TINXDR => INNER NOMEX STRUCTUREURE THICKNESS                         189
      TPCG_TINXDR=1.270                                                   189
      CTPCG_TINXDR='// TINXDR => INNER NOMEX STRUCTUREURE THICKNESS'      189
C // TIKADR => INNER KAPTON LAYER THICKNESS                               190
      TPCG_TIKADR=0.015                                                   190
      CTPCG_TIKADR='// TIKADR => INNER KAPTON LAYER THICKNESS'            190
C // TIALDR => INNER ALUMINUM LAYER THICKNESS                             191
      TPCG_TIALDR=0.004                                                   191
      CTPCG_TIALDR='// TIALDR => INNER ALUMINUM LAYER THICKNESS'          191
C // TIFCRF => OUTER RADIUS OF IFC FLANGE                                 192
      TPCG_TIFCRF=51.7                                                    192
      CTPCG_TIFCRF='// TIFCRF => OUTER RADIUS OF IFC FLANGE'              192
C // TIFCDRT => TOLERANCE BETWEEN FLANGE AND WHEEL                        193
      TPCG_TIFCDRT=0.1                                                    193
      CTPCG_TIFCDRT='// TIFCDRT => TOLERANCE BETWEEN FLANGE AND WHEEL'    193
C // DZ OF YF1                                                            194
      TPCG_DZYF1=221.162                                                  194
      CTPCG_DZYF1='// DZ OF YF1'                                          194
C // DZ OF YF2                                                            195
      TPCG_DZYF2=211.9                                                    195
      CTPCG_DZYF2='// DZ OF YF2'                                          195
C // DZ OF YF3                                                            196
      TPCG_DZYF3=208.02                                                   196
      CTPCG_DZYF3='// DZ OF YF3'                                          196
C // 1.82 MM DRAWING 24A0314 & 24A0304                                    197
      TPCG_PADPLANETHICKNESS=0.182                                        197
      CTPCG_PADPLANETHICKNESS='// 1.82 MM DRAWING 24A0314 & 24A0304'      197
C //                                                                      198
      TPCG_DGATEGROUND=0.6                                                198
      CTPCG_DGATEGROUND='//'                                              198
C //                                                                      199
      TPCG_WIREMOUNTWIDTH=5*((0.130+0.1376)/2)*INCH                       199
      CTPCG_WIREMOUNTWIDTH='//'                                           199
C // DRAWING 24A0434                                                      200
      TPCG_WIREMOUNTHEIGHT=1.340*INCH                                     200
      CTPCG_WIREMOUNTHEIGHT='// DRAWING 24A0434'                          200
C // A.LEBEDEV 1 RDO = 5 LB                                               201
      TPCG_DXRDO=1.75/2                                                   201
      CTPCG_DXRDO='// A.LEBEDEV 1 RDO = 5 LB'                             201
C //                                                                      202
      TPCG_DYRDO=45./2                                                    202
      CTPCG_DYRDO='//'                                                    202
C //                                                                      203
      TPCG_DZRDO=17.0/2                                                   203
      CTPCG_DZRDO='//'                                                    203
C //                                                                      204
      TPCG_ZRDO=TPCG_LENGTHW/2 + 20.0                                     204
      CTPCG_ZRDO='//'                                                     204
C // X COOLING TUBE DRAWING 24A0801B                                      205
      TPCG_HEIGTUBE=0.703*INCH                                            205
      CTPCG_HEIGTUBE='// X COOLING TUBE DRAWING 24A0801B'                 205
C // Z MIXTURE TPCE_WATER_PIPE => RHO = 2.32155 G/CM**3                   206
      TPCG_WIDTUBE=0.500*INCH                                             206
      CTPCG_WIDTUBE='// Z MIXTURE TPCE_WATER_PIPE => RHO = 2.32155 G/CM*  206
     **3'                                                                 206
C //                                                                      207
      TPCG_RDOCOOLINGDX=(38.0 + 9.0 + 58.0)/2                             207
      CTPCG_RDOCOOLINGDX='//'                                             207
C //                                                                      208
      TPCG_RDOCOOLINGDY=1.25                                              208
      CTPCG_RDOCOOLINGDY='//'                                             208
C //                                                                      209
      TPCG_RDOCOOLINGDZ=2.50                                              209
      CTPCG_RDOCOOLINGDZ='//'                                             209
C // INNER/OUTER ANODE WIDTH                                              210
      TPCG_DANODE(1)=0.2                                                  210
      TPCG_DANODE(2)=0.4                                                  210
      CTPCG_DANODE='// INNER/OUTER ANODE WIDTH'                           210
C //                                                                      211
      TPCG_ZGATINGGRID=TPCG_LENGTHV/2                                     211
     *-(TPCG_DGATEGROUND+2*TPCG_DANODE(2))                                211
      CTPCG_ZGATINGGRID='//'                                              211
C //                                                                      212
      TPCG_ZGROUNDGRID=TPCG_ZGATINGGRID+TPCG_DGATEGROUND                  212
      CTPCG_ZGROUNDGRID='//'                                              212
C // DEAD ZONE BEFORE GATINGGRID. NO HITS THERE                           213
      TPCG_DEADZONE=12.                                                   213
      CTPCG_DEADZONE='// DEAD ZONE BEFORE GATINGGRID. NO HITS THERE'      213
C // MAX TIME OF FLIGHT                                                   214
      TPCG_TOFMAX=1.0E-6                                                  214
      CTPCG_TOFMAX='// MAX TIME OF FLIGHT'                                214
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCG',    215
     *BPATH_TPCG,IDNUM_TPCG,1, MAP_TPCG,NAMES_TPCG,COMMEN_TPCG,           215
     *BANK_TPCG,LEN_TPCG,LLEN_TPCG,LINK_TPCG,FLAG_TPCG)                   215
C *                                                                       215
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCG',    215
     *BPATH_TPCG,IDNUM_TPCG,1, MAP_TPCG,NAMES_TPCG,COMMEN_TPCG,           215
     *BANK_TPCG,LEN_TPCG,LLEN_TPCG,LINK_TPCG,FLAG_TPCG)                   215
C *                                                                       216
      CALL AGSBEGM(MODULE,IPRIN)                                          216
      IF (IPRIN.LT.0) RETURN                                              216
C Filling bank TPCG - SUBVERSION 1                                        216
      BANK_TITLE='SUBVERSION 1'                                           216
      BPATH_TPCG='TPCG*'                                                  216
      IDNUM_TPCG(1)=0                                                     217
C SELECTOR                                                                217
      TPCG_VERSION=3.1                                                    217
      CTPCG_VERSION='SELECTOR'                                            217
C MAX ENVELOPE RADIUS                                                     218
      TPCG_RMAX=207.77                                                    218
      CTPCG_RMAX='MAX ENVELOPE RADIUS'                                    218
C EXTEND DEADZONE BEFORE GATING GRID                                      219
      TPCG_DEADZONE=14                                                    219
      CTPCG_DEADZONE='EXTEND DEADZONE BEFORE GATING GRID'                 219
C INCREASE MAXIMUM TOF FOR PARTICLES                                      220
      TPCG_TOFMAX=32.0E-6                                                 220
      CTPCG_TOFMAX='INCREASE MAXIMUM TOF FOR PARTICLES'                   220
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCG',    221
     *BPATH_TPCG,IDNUM_TPCG,1, MAP_TPCG,NAMES_TPCG,COMMEN_TPCG,           221
     *BANK_TPCG,LEN_TPCG,LLEN_TPCG,LINK_TPCG,FLAG_TPCG)                   221
C *                                                                       221
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCG',    221
     *BPATH_TPCG,IDNUM_TPCG,1, MAP_TPCG,NAMES_TPCG,COMMEN_TPCG,           221
     *BANK_TPCG,LEN_TPCG,LLEN_TPCG,LINK_TPCG,FLAG_TPCG)                   221
C use default TPCC bank                                                   222
      BPATH_TPCC='TPCC*'                                                  222
      IDNUM_TPCC(1)=0                                                     222
      CALL AGDGETP('TPCEGEO3A','*',-999,'TPCC',' ',BANK_TPCC,             222
     *BPATH_TPCC,IDNUM_TPCC,1,MAP_TPCC,NAMES_TPCC,BANK_TPCC,LEN_TPCC,     222
     *LLEN_TPCC,LINK_TPCC,FLAG_TPCC)                                      222
C use TPCG bank                                                           223
      BPATH_TPCG='TPCG*'                                                  223
      IDNUM_TPCG(1)=0                                                     223
      TPCG_VERSION=TPCC_VERSION                                           223
      CALL AGDGETP('TPCEGEO3A','*',-999,'TPCG',NTPCG_VERSION,             223
     *RE_TPCG_VERSION,BPATH_TPCG,IDNUM_TPCG,1,MAP_TPCG,NAMES_TPCG,        223
     *BANK_TPCG,LEN_TPCG,LLEN_TPCG,LINK_TPCG,FLAG_TPCG)                   223
C *                                                                       224
      CALL AGSBEGM(MODULE,IPRIN)                                          224
      IF (IPRIN.LT.0) RETURN                                              224
C Filling bank TPRS - // SECTOR OF PADROWS                                224
      BANK_TITLE='// SECTOR OF PADROWS'                                   224
      BPATH_TPRS='TPRS*'                                                  224
      IDNUM_TPRS(1)=0                                                     225
C // SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                              225
      TPRS_SEC=1                                                          225
      CTPRS_SEC='// SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'              225
C // NUMBER OF PADROWS IN THE SECTOR                                      226
      TPRS_NROW=13                                                        226
      CTPRS_NROW='// NUMBER OF PADROWS IN THE SECTOR'                     226
C // TPC PADROW PITCH WIDTH                                               227
      TPRS_PITCH=0.335                                                    227
      CTPRS_PITCH='// TPC PADROW PITCH WIDTH'                             227
C // TPC PADROW THICKNESS                                                 228
      TPRS_WIDTH=1.15                                                     228
      CTPRS_WIDTH='// TPC PADROW THICKNESS'                               228
C // NUMBER OF PADRAWS IN A SUPERPADROW                                   229
      TPRS_SUPER=3                                                        229
      CTPRS_SUPER='// NUMBER OF PADRAWS IN A SUPERPADROW'                 229
C // DISTANCE TO ANODE WIRE FROM PAD PLANE                                230
      TPRS_DANODE=0.2                                                     230
      CTPRS_DANODE='// DISTANCE TO ANODE WIRE FROM PAD PLANE'             230
C // NUMBER OF PADS IN ROW                                                232
      TPRS_NPADS(1)=88                                                    232
      TPRS_NPADS(2)=96                                                    232
      TPRS_NPADS(3)=104                                                   232
      TPRS_NPADS(4)=112                                                   232
      TPRS_NPADS(5)=118                                                   232
      TPRS_NPADS(6)=126                                                   232
      TPRS_NPADS(7)=134                                                   232
      TPRS_NPADS(8)=142                                                   232
      TPRS_NPADS(9)=150                                                   232
      TPRS_NPADS(10)=158                                                  232
      TPRS_NPADS(11)=166                                                  232
      TPRS_NPADS(12)=174                                                  232
      TPRS_NPADS(13)=182                                                  232
      CTPRS_NPADS='// NUMBER OF PADS IN ROW'                              232
C // TPC PADROW RADII                                                     235
      TPRS_RPADS(1)=60.0                                                  235
      TPRS_RPADS(2)=64.8                                                  235
      TPRS_RPADS(3)=69.6                                                  235
      TPRS_RPADS(4)=74.4                                                  235
      TPRS_RPADS(5)=79.2                                                  235
      TPRS_RPADS(6)=84.0                                                  235
      TPRS_RPADS(7)=88.8                                                  235
      TPRS_RPADS(8)=93.6                                                  235
      TPRS_RPADS(9)=98.8                                                  235
      TPRS_RPADS(10)=104.0                                                235
      TPRS_RPADS(11)=109.2                                                235
      TPRS_RPADS(12)=114.4                                                235
      TPRS_RPADS(13)=119.6                                                235
      CTPRS_RPADS='// TPC PADROW RADII'                                   235
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    236
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           236
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   236
C *                                                                       236
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    236
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           236
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   236
C *                                                                       237
      CALL AGSBEGM(MODULE,IPRIN)                                          237
      IF (IPRIN.LT.0) RETURN                                              237
C Filling bank TPRS - // SECTOR OF PADROWS                                237
      BANK_TITLE='// SECTOR OF PADROWS'                                   237
      BPATH_TPRS='TPRS*'                                                  237
      IDNUM_TPRS(1)=0                                                     238
C // SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                              238
      TPRS_SEC=2                                                          238
      CTPRS_SEC='// SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'              238
C // NUMBER OF PADROWS IN OUTER SECTOR                                    239
      TPRS_NROW=32                                                        239
      CTPRS_NROW='// NUMBER OF PADROWS IN OUTER SECTOR'                   239
C // OUTER TPC PADROW PITCH WIDTH                                         240
      TPRS_PITCH=0.67                                                     240
      CTPRS_PITCH='// OUTER TPC PADROW PITCH WIDTH'                       240
C // OUTER TPC PADROW THICKNESS                                           241
      TPRS_WIDTH=1.95                                                     241
      CTPRS_WIDTH='// OUTER TPC PADROW THICKNESS'                         241
C // NUMBER OF PADROWS IN A SUPERPADROW                                   242
      TPRS_SUPER=1                                                        242
      CTPRS_SUPER='// NUMBER OF PADROWS IN A SUPERPADROW'                 242
C // DISTANCE TO ANODE WIRE FROM PAD PLANE                                243
      TPRS_DANODE=0.4                                                     243
      CTPRS_DANODE='// DISTANCE TO ANODE WIRE FROM PAD PLANE'             243
C // NUMBER OF PADS IN ROW                                                247
      TPRS_NPADS(1)=98                                                    247
      TPRS_NPADS(2)=100                                                   247
      TPRS_NPADS(3)=102                                                   247
      TPRS_NPADS(4)=104                                                   247
      TPRS_NPADS(5)=106                                                   247
      TPRS_NPADS(6)=106                                                   247
      TPRS_NPADS(7)=108                                                   247
      TPRS_NPADS(8)=110                                                   247
      TPRS_NPADS(9)=112                                                   247
      TPRS_NPADS(10)=112                                                  247
      TPRS_NPADS(11)=114                                                  247
      TPRS_NPADS(12)=116                                                  247
      TPRS_NPADS(13)=118                                                  247
      TPRS_NPADS(14)=120                                                  247
      TPRS_NPADS(15)=122                                                  247
      TPRS_NPADS(16)=122                                                  247
      TPRS_NPADS(17)=124                                                  247
      TPRS_NPADS(18)=126                                                  247
      TPRS_NPADS(19)=128                                                  247
      TPRS_NPADS(20)=128                                                  247
      TPRS_NPADS(21)=130                                                  247
      TPRS_NPADS(22)=132                                                  247
      TPRS_NPADS(23)=134                                                  247
      TPRS_NPADS(24)=136                                                  247
      TPRS_NPADS(25)=138                                                  247
      TPRS_NPADS(26)=138                                                  247
      TPRS_NPADS(27)=140                                                  247
      TPRS_NPADS(28)=142                                                  247
      TPRS_NPADS(29)=144                                                  247
      TPRS_NPADS(30)=144                                                  247
      TPRS_NPADS(31)=144                                                  247
      TPRS_NPADS(32)=144                                                  247
      CTPRS_NPADS='// NUMBER OF PADS IN ROW'                              247
C // TPC PADROW RADII                                                     255
      TPRS_RPADS(1)=127.195                                               255
      TPRS_RPADS(2)=129.195                                               255
      TPRS_RPADS(3)=131.195                                               255
      TPRS_RPADS(4)=133.195                                               255
      TPRS_RPADS(5)=135.195                                               255
      TPRS_RPADS(6)=137.195                                               255
      TPRS_RPADS(7)=139.195                                               255
      TPRS_RPADS(8)=141.195                                               255
      TPRS_RPADS(9)=143.195                                               255
      TPRS_RPADS(10)=145.195                                              255
      TPRS_RPADS(11)=147.195                                              255
      TPRS_RPADS(12)=149.195                                              255
      TPRS_RPADS(13)=151.195                                              255
      TPRS_RPADS(14)=153.195                                              255
      TPRS_RPADS(15)=155.195                                              255
      TPRS_RPADS(16)=157.195                                              255
      TPRS_RPADS(17)=159.195                                              255
      TPRS_RPADS(18)=161.195                                              255
      TPRS_RPADS(19)=163.195                                              255
      TPRS_RPADS(20)=165.195                                              255
      TPRS_RPADS(21)=167.195                                              255
      TPRS_RPADS(22)=169.195                                              255
      TPRS_RPADS(23)=171.195                                              255
      TPRS_RPADS(24)=173.195                                              255
      TPRS_RPADS(25)=175.195                                              255
      TPRS_RPADS(26)=177.195                                              255
      TPRS_RPADS(27)=179.195                                              255
      TPRS_RPADS(28)=181.195                                              255
      TPRS_RPADS(29)=183.195                                              255
      TPRS_RPADS(30)=185.195                                              255
      TPRS_RPADS(31)=187.195                                              255
      TPRS_RPADS(32)=189.195                                              255
      CTPRS_RPADS='// TPC PADROW RADII'                                   255
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    256
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           256
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   256
C *                                                                       256
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    256
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           256
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   256
C use default TPRS bank                                                   257
      BPATH_TPRS='TPRS*'                                                  257
      IDNUM_TPRS(1)=0                                                     257
      CALL AGDGETP('TPCEGEO3A','*',-999,'TPRS',' ',BANK_TPRS,             257
     *BPATH_TPRS,IDNUM_TPRS,1,MAP_TPRS,NAMES_TPRS,BANK_TPRS,LEN_TPRS,     257
     *LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                                      257
C *                                                                       258
      CALL AGSBEGM(MODULE,IPRIN)                                          258
      IF (IPRIN.LT.0) RETURN                                              258
C Filling bank TECW - // EC TRAPEZOID AND SUPPORT WHEEL                   258
      BANK_TITLE='// EC TRAPEZOID AND SUPPORT WHEEL'                      258
      BPATH_TECW='TECW*'                                                  258
      IDNUM_TECW(1)=0                                                     259
C // SEC => SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                       259
      TECW_SEC=1                                                          259
      CTECW_SEC='// SEC => SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'       259
C // GAPWIDI => AIR IN SUPPORT WHEEL - INNER WIDTH                        260
      TECW_GAPWIDI=2.* 10.91                                              260
      CTECW_GAPWIDI='// GAPWIDI => AIR IN SUPPORT WHEEL - INNER WIDTH'    260
C // GAPWIDO => AIR IN SUPPORT WHEEL - OUTER WIDTH                        261
      TECW_GAPWIDO=2.* 27.56                                              261
      CTECW_GAPWIDO='// GAPWIDO => AIR IN SUPPORT WHEEL - OUTER WIDTH'    261
C // GAPHEIT => AIR IN SUPPORT WHEEL - HEIGHT (DR)                        262
      TECW_GAPHEIT=62.15                                                  262
      CTECW_GAPHEIT='// GAPHEIT => AIR IN SUPPORT WHEEL - HEIGHT (DR)'    262
C // GAPRAD => AIR IN SUPPORT WHEEL - CENTER RADIUS                       263
      TECW_GAPRAD=87.0                                                    263
      CTECW_GAPRAD='// GAPRAD => AIR IN SUPPORT WHEEL - CENTER RADIUS'    263
C //2.* 13.75, !// INWIDTH => SECTOR WIDTH AT INNER RADIUS                264
      TECW_INWIDTH=10.829*INCH                                            264
      CTECW_INWIDTH='//2.* 13.75, !// INWIDTH => SECTOR WIDTH AT INNER R  264
     *ADIUS'                                                              264
C //2.* 32.38, !// OUWIDTH => SECTOR WIDTH AT OUTER RADIUS                265
      TECW_OUWIDTH=25.498*INCH                                            265
      CTECW_OUWIDTH='//2.* 32.38, !// OUWIDTH => SECTOR WIDTH AT OUTER R  265
     *ADIUS'                                                              265
C // NORIBS                                                               266
      TECW_NORIBS=11                                                      266
      CTECW_NORIBS='// NORIBS'                                            266
C // ZSTEPRIB                                                             267
      TECW_ZSTEPRIB=3.150*INCH                                            267
      CTECW_ZSTEPRIB='// ZSTEPRIB'                                        267
C // WIDTHRIB => SIDE RIB WIDTH                                           268
      TECW_WIDTHRIB=1.47*COS(15./180*ACOS(-1.))*INCH                      268
      CTECW_WIDTHRIB='// WIDTHRIB => SIDE RIB WIDTH'                      268
C // DRAWING 24A3685B 27.77*INCH FROM DRAWING 24A0325, !// => 69.527 =>   269
C (WIRE PLANE) 69.                                                        269
      TECW_HEIGHT=27.373*INCH                                             269
      CTECW_HEIGHT='// DRAWING 24A3685B 27.77*INCH FROM DRAWING 24A0325,  269
     * !// => 69.527 => (WIRE PLANE) 69.'                                 269
C // 24A3685B 3.219*INCH !// THICK => SECTOR THICKNESS                    270
      TECW_THICK=3.300*INCH                                               270
      CTECW_THICK='// 24A3685B 3.219*INCH !// THICK => SECTOR             270
     *THICKNESS'                                                          270
C //                                                                      271
      TECW_THICKAL=0.375*INCH                                             271
      CTECW_THICKAL='//'                                                  271
C // RMIN => MINIMUM DISTANCE TO BEAM LINE (WIRE PLANE)                   272
      TECW_RMIN=51.905                                                    272
      CTECW_RMIN='// RMIN => MINIMUM DISTANCE TO BEAM LINE (WIRE          272
     *PLANE)'                                                             272
C // RCENTER => SECTOR CENTER RADIUS (SET BY PRECISION HOLES)             273
      TECW_RCENTER=86.669                                                 273
      CTECW_RCENTER='// RCENTER => SECTOR CENTER RADIUS (SET BY PRECISIO  273
     *N HOLES)'                                                           273
C // = 0.9525 CM                                                          274
      TECW_HOLEDX=0.750/2*INCH                                            274
      CTECW_HOLEDX='// = 0.9525 CM'                                       274
C // = 3.33375 CM                                                         275
      TECW_HOLEDY=2.625/2*INCH                                            275
      CTECW_HOLEDY='// = 3.33375 CM'                                      275
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TECW',    276
     *BPATH_TECW,IDNUM_TECW,1, MAP_TECW,NAMES_TECW,COMMEN_TECW,           276
     *BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)                   276
C *                                                                       276
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TECW',    276
     *BPATH_TECW,IDNUM_TECW,1, MAP_TECW,NAMES_TECW,COMMEN_TECW,           276
     *BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)                   276
C *                                                                       277
      CALL AGSBEGM(MODULE,IPRIN)                                          277
      IF (IPRIN.LT.0) RETURN                                              277
C Filling bank TECW - // EC TRAPEZOID AND SUPPORT WHEEL                   277
      BANK_TITLE='// EC TRAPEZOID AND SUPPORT WHEEL'                      277
      BPATH_TECW='TECW*'                                                  277
      IDNUM_TECW(1)=0                                                     278
C // SEC => SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                       278
      TECW_SEC=2                                                          278
      CTECW_SEC='// SEC => SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'       278
C // GAPWIDI => AIR IN SUPPORT WHEEL - INNER WIDTH                        279
      TECW_GAPWIDI=2.* 28.92                                              279
      CTECW_GAPWIDI='// GAPWIDI => AIR IN SUPPORT WHEEL - INNER WIDTH'    279
C // GAPWIDO => AIR IN SUPPORT WHEEL - OUTER WIDTH                        280
      TECW_GAPWIDO=2.* 46.74                                              280
      CTECW_GAPWIDO='// GAPWIDO => AIR IN SUPPORT WHEEL - OUTER WIDTH'    280
C // GAPHEIT => AIR IN SUPPORT WHEEL - HEIGHT (DR)                        281
      TECW_GAPHEIT=65.0                                                   281
      CTECW_GAPHEIT='// GAPHEIT => AIR IN SUPPORT WHEEL - HEIGHT (DR)'    281
C // AGAPRAD => IR IN SUPPORT WHEEL - RADIUS                              282
      TECW_GAPRAD=158.0                                                   282
      CTECW_GAPRAD='// AGAPRAD => IR IN SUPPORT WHEEL - RADIUS'           282
C //64.92, !// INWIDTH => SECTOR WIDTH AT INNER RADIUS                    283
      TECW_INWIDTH=25.561*INCH                                            283
      CTECW_INWIDTH='//64.92, !// INWIDTH => SECTOR WIDTH AT INNER RADIU  283
     *S'                                                                  283
C //103.25, !// OUWIDTH => SECTOR WIDTH AT OUTER RADIUS                   284
      TECW_OUWIDTH=40.649*INCH                                            284
      CTECW_OUWIDTH='//103.25, !// OUWIDTH => SECTOR WIDTH AT OUTER RADI  284
     *US'                                                                 284
C // = 1.242060 WIDTHLIP => WIDTH OF LIP                                  285
      TECW_WIDTHLIP=((25.561-24.583)+(40.649-39.671))/4*INCH              285
      CTECW_WIDTHLIP='// = 1.242060 WIDTHLIP => WIDTH OF LIP'             285
C // NORIBS                                                               286
      TECW_NORIBS=14                                                      286
      CTECW_NORIBS='// NORIBS'                                            286
C // ZSTEPRIB                                                             287
      TECW_ZSTEPRIB=3.150*INCH                                            287
      CTECW_ZSTEPRIB='// ZSTEPRIB'                                        287
C // WIDTHRIB = > SIDE RIB WIDTH                                          288
      TECW_WIDTHRIB=1.47*COS(15./180*ACOS(-1.))*INCH                      288
      CTECW_WIDTHRIB='// WIDTHRIB = > SIDE RIB WIDTH'                     288
C // 71.514 (WIRE PLANE) => 71.51, !// HEIGHT => SECTOR RADIAL HEIGHT     289
      TECW_HEIGHT=28.155*INCH                                             289
      CTECW_HEIGHT='// 71.514 (WIRE PLANE) => 71.51, !// HEIGHT => SECTO  289
     *R RADIAL HEIGHT'                                                    289
C // 24A3925G THICK => SECTOR THICKNESS                                   290
      TECW_THICK=3.140*INCH                                               290
      CTECW_THICK='// 24A3925G THICK => SECTOR THICKNESS'                 290
C //                                                                      291
      TECW_THICKAL=0.375*INCH                                             291
      CTECW_THICKAL='//'                                                  291
C // RMIN => MINIMUM DISTANCE TO BEAM LINE (WIRE PLANE)                   292
      TECW_RMIN=121.732                                                   292
      CTECW_RMIN='// RMIN => MINIMUM DISTANCE TO BEAM LINE (WIRE          292
     *PLANE)'                                                             292
C // RCENTER => SECTOR CENTER RADIUS (SET BY PRECISION HOLES)             293
      TECW_RCENTER=157.488                                                293
      CTECW_RCENTER='// RCENTER => SECTOR CENTER RADIUS (SET BY PRECISIO  293
     *N HOLES)'                                                           293
C //                                                                      294
      TECW_HOLEDX=0.750/2*INCH                                            294
      CTECW_HOLEDX='//'                                                   294
C //                                                                      295
      TECW_HOLEDY=2.500/2*INCH                                            295
      CTECW_HOLEDY='//'                                                   295
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TECW',    296
     *BPATH_TECW,IDNUM_TECW,1, MAP_TECW,NAMES_TECW,COMMEN_TECW,           296
     *BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)                   296
C *                                                                       296
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TECW',    296
     *BPATH_TECW,IDNUM_TECW,1, MAP_TECW,NAMES_TECW,COMMEN_TECW,           296
     *BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)                   296
C *                                                                       297
      CALL AGSBEGM(MODULE,IPRIN)                                          297
      IF (IPRIN.LT.0) RETURN                                              297
C Filling bank TPCR - // VOLUME FOR TPC READOUT BOARDS                    297
      BANK_TITLE='// VOLUME FOR TPC READOUT BOARDS'                       297
      BPATH_TPCR='TPCR*'                                                  297
      IDNUM_TPCR(1)=0                                                     298
C // LENGTH OF RDO VOLUME                                                 298
      TPCR_RDOVTHK=30.                                                    298
      CTPCR_RDOVTHK='// LENGTH OF RDO VOLUME'                             298
C // THICKNESS OF RDO CARD                                                299
      TPCR_RDOTHK=.25                                                     299
      CTPCR_RDOTHK='// THICKNESS OF RDO CARD'                             299
C // CARD LENGTH ALONG BEAM DIRECTION                                     300
      TPCR_RDOLEN=27                                                      300
      CTPCR_RDOLEN='// CARD LENGTH ALONG BEAM DIRECTION'                  300
C // NUMBER OF RDO BOARDS                                                 301
      TPCR_NRDOBRD=9                                                      301
      CTPCR_NRDOBRD='// NUMBER OF RDO BOARDS'                             301
C // RADIAL POS OF RDO BOARDS                                             303
      TPCR_RDOHT(1)=60.0                                                  303
      TPCR_RDOHT(2)=74.0                                                  303
      TPCR_RDOHT(3)=84.0                                                  303
      TPCR_RDOHT(4)=101.0                                                 303
      TPCR_RDOHT(5)=106.0                                                 303
      TPCR_RDOHT(6)=126.0                                                 303
      TPCR_RDOHT(7)=146.0                                                 303
      TPCR_RDOHT(8)=166.0                                                 303
      TPCR_RDOHT(9)=186.0                                                 303
      CTPCR_RDOHT='// RADIAL POS OF RDO BOARDS'                           303
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCR',    304
     *BPATH_TPCR,IDNUM_TPCR,1, MAP_TPCR,NAMES_TPCR,COMMEN_TPCR,           304
     *BANK_TPCR,LEN_TPCR,LLEN_TPCR,LINK_TPCR,FLAG_TPCR)                   304
C *                                                                       304
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCR',    304
     *BPATH_TPCR,IDNUM_TPCR,1, MAP_TPCR,NAMES_TPCR,COMMEN_TPCR,           304
     *BANK_TPCR,LEN_TPCR,LLEN_TPCR,LINK_TPCR,FLAG_TPCR)                   304
C *                                                                       305
      CALL AGSBEGM(MODULE,IPRIN)                                          305
      IF (IPRIN.LT.0) RETURN                                              305
C Filling bank TFEE - // FRONTENDELECTRONICS                              305
      BANK_TITLE='// FRONTENDELECTRONICS'                                 305
      BPATH_TFEE='TFEE*'                                                  305
      IDNUM_TFEE(1)=0                                                     306
C // VERSION OF FRONTENDELECTRONICS                                       306
      TFEE_VERS=1                                                         306
      CTFEE_VERS='// VERSION OF FRONTENDELECTRONICS'                      306
C // 1.47 SCALE FACTOR ACCOUNT FOR 9 LB OF CABLES                         307
      TFEE_CARDDX=1.47*0.110*INCH/2                                       307
      CTFEE_CARDDX='// 1.47 SCALE FACTOR ACCOUNT FOR 9 LB OF CABLES'      307
C //...                                                                   308
      TFEE_CARDDY=2.900*INCH/2                                            308
      CTFEE_CARDDY='//...'                                                308
C //...                                                                   309
      TFEE_CARDDZ=7.000*INCH/2                                            309
      CTFEE_CARDDZ='//...'                                                309
C //...                                                                   310
      TFEE_PLATEDX=0.110*INCH/2                                           310
      CTFEE_PLATEDX='//...'                                               310
C //...                                                                   311
      TFEE_PLATEDY=1.480*INCH                                             311
      CTFEE_PLATEDY='//...'                                               311
C //...                                                                   312
      TFEE_PLATEDZ=4.650*INCH/2                                           312
      CTFEE_PLATEDZ='//...'                                               312
C //...                                                                   313
      TFEE_ASSEMBLYTHICKNESS=TFEE_CARDDX + TFEE_PLATEDX                   313
      CTFEE_ASSEMBLYTHICKNESS='//...'                                     313
C //...                                                                   314
      TFEE_RIBDX=0.820*INCH/2 - 2* TFEE_CARDDX                            314
      CTFEE_RIBDX='//...'                                                 314
C //...                                                                   315
      TFEE_RIBDY=2.900*INCH/2 - 2* TFEE_CARDDX                            315
      CTFEE_RIBDY='//...'                                                 315
C //...                                                                   316
      TFEE_RIBDZ=TFEE_CARDDX                                              316
      CTFEE_RIBDZ='//...'                                                 316
C // FEE ASSEMBLY BRIK SIZE                                               317
      TFEE_ASS(1)=1.6                                                     317
      TFEE_ASS(2)=4                                                       317
      TFEE_ASS(3)=10                                                      317
      CTFEE_ASS='// FEE ASSEMBLY BRIK SIZE'                               317
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TFEE',    318
     *BPATH_TFEE,IDNUM_TFEE,1, MAP_TFEE,NAMES_TFEE,COMMEN_TFEE,           318
     *BANK_TFEE,LEN_TFEE,LLEN_TFEE,LINK_TFEE,FLAG_TFEE)                   318
C *                                                                       318
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TFEE',    318
     *BPATH_TFEE,IDNUM_TFEE,1, MAP_TFEE,NAMES_TFEE,COMMEN_TFEE,           318
     *BANK_TFEE,LEN_TFEE,LLEN_TFEE,LINK_TFEE,FLAG_TFEE)                   318
C use TECW bank                                                           319
      BPATH_TECW='TECW*'                                                  319
      IDNUM_TECW(1)=0                                                     319
      TECW_SEC=KINNER+1                                                   319
      CALL AGDGETP('TPCEGEO3A','*',-999,'TECW',NTECW_SEC,RE_TECW_SEC,     319
     *BPATH_TECW,IDNUM_TECW,1,MAP_TECW,NAMES_TECW,BANK_TECW,LEN_TECW,     319
     *LLEN_TECW,LINK_TECW,FLAG_TECW)                                      319
C *                                                                       320
      CALL AGSBEGM(MODULE,IPRIN)                                          320
      IF (IPRIN.LT.0) RETURN                                              320
C Filling bank RIBS - // RIB STRUCTURE / INNER DRAWING 24A3925G           320
      BANK_TITLE='// RIB STRUCTURE / INNER DRAWING 24A3925G'              320
      BPATH_RIBS='RIBS*'                                                  320
      IDNUM_RIBS(1)=0                                                     321
C // VERSION NUMBER                                                       321
      RIBS_VERSION=KINNER                                                 321
      CRIBS_VERSION='// VERSION NUMBER'                                   321
C // SELCTOR                                                              322
      RIBS_INOUT=KINNER                                                   322
      CRIBS_INOUT='// SELCTOR'                                            322
C // NUMBER OF ELEMENTS                                                   323
      RIBS_NUM=11                                                         323
      CRIBS_NUM='// NUMBER OF ELEMENTS'                                   323
C // X                                                                    326
      RIBS_X(1)=0.000                                                     326
      RIBS_X(2)=4.261                                                     326
      RIBS_X(3)=10.561                                                    326
      RIBS_X(4)=16.861                                                    326
      RIBS_X(5)=23.161                                                    326
      RIBS_X(6)=25.950                                                    326
      RIBS_X(7)=3.726                                                     326
      RIBS_X(8)=3.726                                                     326
      RIBS_X(9)=10.026                                                    326
      RIBS_X(10)=10.026                                                   326
      RIBS_X(11)=25.480                                                   326
      CRIBS_X='// X'                                                      326
C // Y                                                                    329
      RIBS_Y(1)=0.000                                                     329
      RIBS_Y(2)=0.000                                                     329
      RIBS_Y(3)=0.000                                                     329
      RIBS_Y(4)=0.000                                                     329
      RIBS_Y(5)=0.000                                                     329
      RIBS_Y(6)=0.000                                                     329
      RIBS_Y(7)=-5.645                                                    329
      RIBS_Y(8)=5.645                                                     329
      RIBS_Y(9)=-3.895                                                    329
      RIBS_Y(10)=3.895                                                    329
      RIBS_Y(11)=0.000                                                    329
      CRIBS_Y='// Y'                                                      329
C // DX ... LAST=2*(25.950-(1.050+2.515+21.915))                          340
      RIBS_DX(1)=1.470                                                    340
      RIBS_DX(2)=0.375                                                    340
      RIBS_DX(3)=0.375                                                    340
      RIBS_DX(4)=0.375                                                    340
      RIBS_DX(5)=0.375                                                    340
      RIBS_DX(6)=TECW_HEIGHT/INCH-25.950                                  340
      RIBS_DX(7)=1.070                                                    340
      RIBS_DX(8)=1.070                                                    340
      RIBS_DX(9)=1.070                                                    340
      RIBS_DX(10)=1.070                                                   340
      RIBS_DX(11)=0.940                                                   340
      CRIBS_DX='// DX ... LAST=2*(25.950-(1.050+2.515+21.915))'           340
C // DY                                                                   342
      RIBS_DY(1)=0.                                                       342
      RIBS_DY(2)=0.                                                       342
      RIBS_DY(3)=0.                                                       342
      RIBS_DY(4)=0.                                                       342
      RIBS_DY(5)=0.                                                       342
      RIBS_DY(6)=0.                                                       342
      RIBS_DY(7)=0.750                                                    342
      RIBS_DY(8)=0.750                                                    342
      RIBS_DY(9)=0.750                                                    342
      RIBS_DY(10)=0.750                                                   342
      RIBS_DY(11)=2.0                                                     342
      CRIBS_DY='// DY'                                                    342
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RIBS',    343
     *BPATH_RIBS,IDNUM_RIBS,1, MAP_RIBS,NAMES_RIBS,COMMEN_RIBS,           343
     *BANK_RIBS,LEN_RIBS,LLEN_RIBS,LINK_RIBS,FLAG_RIBS)                   343
C *                                                                       343
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RIBS',    343
     *BPATH_RIBS,IDNUM_RIBS,1, MAP_RIBS,NAMES_RIBS,COMMEN_RIBS,           343
     *BANK_RIBS,LEN_RIBS,LLEN_RIBS,LINK_RIBS,FLAG_RIBS)                   343
C use TECW bank                                                           344
      BPATH_TECW='TECW*'                                                  344
      IDNUM_TECW(1)=0                                                     344
      TECW_SEC=KOUTER+1                                                   344
      CALL AGDGETP('TPCEGEO3A','*',-999,'TECW',NTECW_SEC,RE_TECW_SEC,     344
     *BPATH_TECW,IDNUM_TECW,1,MAP_TECW,NAMES_TECW,BANK_TECW,LEN_TECW,     344
     *LLEN_TECW,LINK_TECW,FLAG_TECW)                                      344
C *                                                                       345
      CALL AGSBEGM(MODULE,IPRIN)                                          345
      IF (IPRIN.LT.0) RETURN                                              345
C Filling bank RIBS - // RIB STRUCTURE / OUTER DRAWING 24A3925G           345
      BANK_TITLE='// RIB STRUCTURE / OUTER DRAWING 24A3925G'              345
      BPATH_RIBS='RIBS*'                                                  345
      IDNUM_RIBS(1)=0                                                     346
C // VERSION NUMBER                                                       346
      RIBS_VERSION=KOUTER                                                 346
      CRIBS_VERSION='// VERSION NUMBER'                                   346
C // SELCTOR                                                              347
      RIBS_INOUT=KOUTER                                                   347
      CRIBS_INOUT='// SELCTOR'                                            347
C // NUMBER OF ELEMENTS                                                   348
      RIBS_NUM=14                                                         348
      CRIBS_NUM='// NUMBER OF ELEMENTS'                                   348
C // X                                                                    353
      RIBS_X(1)=0.280                                                     353
      RIBS_X(2)=4.261                                                     353
      RIBS_X(3)=7.410                                                     353
      RIBS_X(4)=10.560                                                    353
      RIBS_X(5)=13.710                                                    353
      RIBS_X(6)=16.859                                                    353
      RIBS_X(7)=20.009                                                    353
      RIBS_X(8)=23.158                                                    353
      RIBS_X(9)=26.309                                                    353
      RIBS_X(10)=6.023                                                    353
      RIBS_X(11)=6.023                                                    353
      RIBS_X(12)=12.3225                                                  353
      RIBS_X(13)=15.472                                                   353
      RIBS_X(14)=26.309-1.575/2                                           353
      CRIBS_X='// X'                                                      353
C // Y                                                                    357
      RIBS_Y(1)=0                                                         357
      RIBS_Y(2)=0                                                         357
      RIBS_Y(3)=0                                                         357
      RIBS_Y(4)=0                                                         357
      RIBS_Y(5)=0                                                         357
      RIBS_Y(6)=0                                                         357
      RIBS_Y(7)=0                                                         357
      RIBS_Y(8)=0                                                         357
      RIBS_Y(9)=0                                                         357
      RIBS_Y(10)=17.250/2                                                 357
      RIBS_Y(11)=-17.250/2                                                357
      RIBS_Y(12)=0                                                        357
      RIBS_Y(13)=0                                                        357
      RIBS_Y(14)=0                                                        357
      CRIBS_Y='// Y'                                                      357
C // DX                                                                   360
      RIBS_DX(1)=1.268                                                    360
      RIBS_DX(2)=0.375                                                    360
      RIBS_DX(3)=0.375                                                    360
      RIBS_DX(4)=0.375                                                    360
      RIBS_DX(5)=0.375                                                    360
      RIBS_DX(6)=0.375                                                    360
      RIBS_DX(7)=0.375                                                    360
      RIBS_DX(8)=0.375                                                    360
      RIBS_DX(9)=1.566                                                    360
      RIBS_DX(10)=2.774                                                   360
      RIBS_DX(11)=2.774                                                   360
      RIBS_DX(12)=2.775                                                   360
      RIBS_DX(13)=2.774                                                   360
      RIBS_DX(14)=1.575                                                   360
      CRIBS_DX='// DX'                                                    360
C // DY                                                                   363
      RIBS_DY(1)=0                                                        363
      RIBS_DY(2)=0                                                        363
      RIBS_DY(3)=0                                                        363
      RIBS_DY(4)=0                                                        363
      RIBS_DY(5)=0                                                        363
      RIBS_DY(6)=0                                                        363
      RIBS_DY(7)=0                                                        363
      RIBS_DY(8)=0                                                        363
      RIBS_DY(9)=0                                                        363
      RIBS_DY(10)=1.250                                                   363
      RIBS_DY(11)=1.250                                                   363
      RIBS_DY(12)=0.375                                                   363
      RIBS_DY(13)=0.375                                                   363
      RIBS_DY(14)=1.5680                                                  363
      CRIBS_DY='// DY'                                                    363
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RIBS',    364
     *BPATH_RIBS,IDNUM_RIBS,1, MAP_RIBS,NAMES_RIBS,COMMEN_RIBS,           364
     *BANK_RIBS,LEN_RIBS,LLEN_RIBS,LINK_RIBS,FLAG_RIBS)                   364
C *                                                                       364
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RIBS',    364
     *BPATH_RIBS,IDNUM_RIBS,1, MAP_RIBS,NAMES_RIBS,COMMEN_RIBS,           364
     *BANK_RIBS,LEN_RIBS,LLEN_RIBS,LINK_RIBS,FLAG_RIBS)                   364
C *                                                                       365
      CALL AGSBEGM(MODULE,IPRIN)                                          365
      IF (IPRIN.LT.0) RETURN                                              365
C Filling bank HOLZ - // HOLES PER ROW INNER                              365
      BANK_TITLE='// HOLES PER ROW INNER'                                 365
      BPATH_HOLZ='HOLZ*'                                                  365
      IDNUM_HOLZ(1)=0                                                     366
C // SELECTOR                                                             366
      HOLZ_INOUT=KINNER                                                   366
      CHOLZ_INOUT='// SELECTOR'                                           366
C // NUMBER OF ROWS                                                       367
      HOLZ_NUMBEROFROWS=15                                                367
      CHOLZ_NUMBEROFROWS='// NUMBER OF ROWS'                              367
C // N/ROW                                                                369
      HOLZ_NUMBERPERROW(1)=6                                              369
      HOLZ_NUMBERPERROW(2)=5                                              369
      HOLZ_NUMBERPERROW(3)=5                                              369
      HOLZ_NUMBERPERROW(4)=2                                              369
      HOLZ_NUMBERPERROW(5)=5                                              369
      HOLZ_NUMBERPERROW(6)=4                                              369
      HOLZ_NUMBERPERROW(7)=4                                              369
      HOLZ_NUMBERPERROW(8)=4                                              369
      HOLZ_NUMBERPERROW(9)=4                                              369
      HOLZ_NUMBERPERROW(10)=3                                             369
      HOLZ_NUMBERPERROW(11)=3                                             369
      HOLZ_NUMBERPERROW(12)=3                                             369
      HOLZ_NUMBERPERROW(13)=3                                             369
      HOLZ_NUMBERPERROW(14)=2                                             369
      HOLZ_NUMBERPERROW(15)=2                                             369
      HOLZ_NUMBERPERROW(16)=0                                             369
      CHOLZ_NUMBERPERROW='// N/ROW'                                       369
C // Y HOLES                                                              441
      HOLZ_YHOLES(1)=3.180*(-2.5+0)                                       441
      HOLZ_YHOLES(2)=3.180*(-2.5+1)                                       441
      HOLZ_YHOLES(3)=3.180*(-2.5+2)                                       441
      HOLZ_YHOLES(4)=3.180*(-2.5+3)                                       441
      HOLZ_YHOLES(5)=3.180*(-2.5+4)                                       441
      HOLZ_YHOLES(6)=3.180*(-2.5+5)                                       441
      HOLZ_YHOLES(7)=3.180*(-2.5+6)                                       441
      HOLZ_YHOLES(8)=3.180*(-2.5+7)                                       441
      HOLZ_YHOLES(9)=3.180*(-2.5+8)                                       441
      HOLZ_YHOLES(10)=3.764*(-2.0+0)                                      441
      HOLZ_YHOLES(11)=3.764*(-2.0+1)                                      441
      HOLZ_YHOLES(12)=3.764*(-2.0+2)                                      441
      HOLZ_YHOLES(13)=3.764*(-2.0+3)                                      441
      HOLZ_YHOLES(14)=3.764*(-2.0+4)                                      441
      HOLZ_YHOLES(15)=3.764*(-2.0+5)                                      441
      HOLZ_YHOLES(16)=3.764*(-2.0+6)                                      441
      HOLZ_YHOLES(17)=3.764*(-2.0+7)                                      441
      HOLZ_YHOLES(18)=3.764*(-2.0+8)                                      441
      HOLZ_YHOLES(19)=3.553*(-2.0+0)                                      441
      HOLZ_YHOLES(20)=3.553*(-2.0+1)                                      441
      HOLZ_YHOLES(21)=3.553*(-2.0+2)                                      441
      HOLZ_YHOLES(22)=3.553*(-2.0+3)                                      441
      HOLZ_YHOLES(23)=3.553*(-2.0+4)                                      441
      HOLZ_YHOLES(24)=3.553*(-2.0+5)                                      441
      HOLZ_YHOLES(25)=3.553*(-2.0+6)                                      441
      HOLZ_YHOLES(26)=3.553*(-2.0+7)                                      441
      HOLZ_YHOLES(27)=3.553*(-2.0+8)                                      441
      HOLZ_YHOLES(28)=13.368*(-0.5+0)                                     441
      HOLZ_YHOLES(29)=13.368*(-0.5+1)                                     441
      HOLZ_YHOLES(30)=13.368*(-0.5+2)                                     441
      HOLZ_YHOLES(31)=13.368*(-0.5+3)                                     441
      HOLZ_YHOLES(32)=13.368*(-0.5+4)                                     441
      HOLZ_YHOLES(33)=13.368*(-0.5+5)                                     441
      HOLZ_YHOLES(34)=13.368*(-0.5+6)                                     441
      HOLZ_YHOLES(35)=13.368*(-0.5+7)                                     441
      HOLZ_YHOLES(36)=13.368*(-0.5+8)                                     441
      HOLZ_YHOLES(37)=3.131*(-2.0+0)                                      441
      HOLZ_YHOLES(38)=3.131*(-2.0+1)                                      441
      HOLZ_YHOLES(39)=3.131*(-2.0+2)                                      441
      HOLZ_YHOLES(40)=3.131*(-2.0+3)                                      441
      HOLZ_YHOLES(41)=3.131*(-2.0+4)                                      441
      HOLZ_YHOLES(42)=3.131*(-2.0+5)                                      441
      HOLZ_YHOLES(43)=3.131*(-2.0+6)                                      441
      HOLZ_YHOLES(44)=3.131*(-2.0+7)                                      441
      HOLZ_YHOLES(45)=3.131*(-2.0+8)                                      441
      HOLZ_YHOLES(46)=-3.892 - 1.948                                      441
      HOLZ_YHOLES(47)=-1.948                                              441
      HOLZ_YHOLES(48)=+1.948                                              441
      HOLZ_YHOLES(49)=3.892 + 1.948                                       441
      HOLZ_YHOLES(50)=0.000                                               441
      HOLZ_YHOLES(51)=0.000                                               441
      HOLZ_YHOLES(52)=0.000                                               441
      HOLZ_YHOLES(53)=0.000                                               441
      HOLZ_YHOLES(54)=0.000                                               441
      HOLZ_YHOLES(55)=-3.611 - 1.807                                      441
      HOLZ_YHOLES(56)=-1.807                                              441
      HOLZ_YHOLES(57)=+1.807                                              441
      HOLZ_YHOLES(58)=3.611 + 1.807                                       441
      HOLZ_YHOLES(59)=0.000                                               441
      HOLZ_YHOLES(60)=0.000                                               441
      HOLZ_YHOLES(61)=0.000                                               441
      HOLZ_YHOLES(62)=0.000                                               441
      HOLZ_YHOLES(63)=0.000                                               441
      HOLZ_YHOLES(64)=-3.281 - 1.7145                                     441
      HOLZ_YHOLES(65)=-1.7145                                             441
      HOLZ_YHOLES(66)=+1.7145                                             441
      HOLZ_YHOLES(67)=3.281 + 1.7145                                      441
      HOLZ_YHOLES(68)=0.000                                               441
      HOLZ_YHOLES(69)=0.000                                               441
      HOLZ_YHOLES(70)=0.000                                               441
      HOLZ_YHOLES(71)=0.000                                               441
      HOLZ_YHOLES(72)=0.000                                               441
      HOLZ_YHOLES(73)=3.150*(-1.5+0)                                      441
      HOLZ_YHOLES(74)=3.150*(-1.5+1)                                      441
      HOLZ_YHOLES(75)=3.150*(-1.5+2)                                      441
      HOLZ_YHOLES(76)=3.150*(-1.5+3)                                      441
      HOLZ_YHOLES(77)=3.150*(-1.5+4)                                      441
      HOLZ_YHOLES(78)=3.150*(-1.5+5)                                      441
      HOLZ_YHOLES(79)=3.150*(-1.5+6)                                      441
      HOLZ_YHOLES(80)=3.150*(-1.5+7)                                      441
      HOLZ_YHOLES(81)=3.150*(-1.5+8)                                      441
      HOLZ_YHOLES(82)=4.152*(-1.0+0)                                      441
      HOLZ_YHOLES(83)=4.152*(-1.0+1)                                      441
      HOLZ_YHOLES(84)=4.152*(-1.0+2)                                      441
      HOLZ_YHOLES(85)=4.152*(-1.0+3)                                      441
      HOLZ_YHOLES(86)=4.152*(-1.0+4)                                      441
      HOLZ_YHOLES(87)=4.152*(-1.0+5)                                      441
      HOLZ_YHOLES(88)=4.152*(-1.0+6)                                      441
      HOLZ_YHOLES(89)=4.152*(-1.0+7)                                      441
      HOLZ_YHOLES(90)=4.152*(-1.0+8)                                      441
      HOLZ_YHOLES(91)=3.730*(-1.0+0)                                      441
      HOLZ_YHOLES(92)=3.730*(-1.0+1)                                      441
      HOLZ_YHOLES(93)=3.730*(-1.0+2)                                      441
      HOLZ_YHOLES(94)=3.730*(-1.0+3)                                      441
      HOLZ_YHOLES(95)=3.730*(-1.0+4)                                      441
      HOLZ_YHOLES(96)=3.730*(-1.0+5)                                      441
      HOLZ_YHOLES(97)=3.730*(-1.0+6)                                      441
      HOLZ_YHOLES(98)=3.730*(-1.0+7)                                      441
      HOLZ_YHOLES(99)=3.730*(-1.0+8)                                      441
      HOLZ_YHOLES(100)=3.308*(-1.0+0)                                     441
      HOLZ_YHOLES(101)=3.308*(-1.0+1)                                     441
      HOLZ_YHOLES(102)=3.308*(-1.0+2)                                     441
      HOLZ_YHOLES(103)=3.308*(-1.0+3)                                     441
      HOLZ_YHOLES(104)=3.308*(-1.0+4)                                     441
      HOLZ_YHOLES(105)=3.308*(-1.0+5)                                     441
      HOLZ_YHOLES(106)=3.308*(-1.0+6)                                     441
      HOLZ_YHOLES(107)=3.308*(-1.0+7)                                     441
      HOLZ_YHOLES(108)=3.308*(-1.0+8)                                     441
      HOLZ_YHOLES(109)=3.075*(-1.0+0)                                     441
      HOLZ_YHOLES(110)=3.075*(-1.0+1)                                     441
      HOLZ_YHOLES(111)=3.075*(-1.0+2)                                     441
      HOLZ_YHOLES(112)=3.075*(-1.0+3)                                     441
      HOLZ_YHOLES(113)=3.075*(-1.0+4)                                     441
      HOLZ_YHOLES(114)=3.075*(-1.0+5)                                     441
      HOLZ_YHOLES(115)=3.075*(-1.0+6)                                     441
      HOLZ_YHOLES(116)=3.075*(-1.0+7)                                     441
      HOLZ_YHOLES(117)=3.075*(-1.0+8)                                     441
      HOLZ_YHOLES(118)=4.927*(-0.5+0)                                     441
      HOLZ_YHOLES(119)=4.927*(-0.5+1)                                     441
      HOLZ_YHOLES(120)=4.927*(-0.5+2)                                     441
      HOLZ_YHOLES(121)=4.927*(-0.5+3)                                     441
      HOLZ_YHOLES(122)=4.927*(-0.5+4)                                     441
      HOLZ_YHOLES(123)=4.927*(-0.5+5)                                     441
      HOLZ_YHOLES(124)=4.927*(-0.5+6)                                     441
      HOLZ_YHOLES(125)=4.927*(-0.5+7)                                     441
      HOLZ_YHOLES(126)=4.927*(-0.5+8)                                     441
      HOLZ_YHOLES(127)=4.083*(-0.5+0)                                     441
      HOLZ_YHOLES(128)=4.083*(-0.5+1)                                     441
      HOLZ_YHOLES(129)=4.083*(-0.5+2)                                     441
      HOLZ_YHOLES(130)=4.083*(-0.5+3)                                     441
      HOLZ_YHOLES(131)=4.083*(-0.5+4)                                     441
      HOLZ_YHOLES(132)=4.083*(-0.5+5)                                     441
      HOLZ_YHOLES(133)=4.083*(-0.5+6)                                     441
      HOLZ_YHOLES(134)=4.083*(-0.5+7)                                     441
      HOLZ_YHOLES(135)=4.083*(-0.5+8)                                     441
      HOLZ_YHOLES(136)=0.000                                              441
      HOLZ_YHOLES(137)=0.000                                              441
      HOLZ_YHOLES(138)=0.000                                              441
      HOLZ_YHOLES(139)=0.000                                              441
      HOLZ_YHOLES(140)=0.000                                              441
      HOLZ_YHOLES(141)=0.000                                              441
      HOLZ_YHOLES(142)=0.000                                              441
      HOLZ_YHOLES(143)=0.000                                              441
      HOLZ_YHOLES(144)=0.000                                              441
      CHOLZ_YHOLES='// Y HOLES'                                           441
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'HOLZ',    442
     *BPATH_HOLZ,IDNUM_HOLZ,1, MAP_HOLZ,NAMES_HOLZ,COMMEN_HOLZ,           442
     *BANK_HOLZ,LEN_HOLZ,LLEN_HOLZ,LINK_HOLZ,FLAG_HOLZ)                   442
C *                                                                       442
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'HOLZ',    442
     *BPATH_HOLZ,IDNUM_HOLZ,1, MAP_HOLZ,NAMES_HOLZ,COMMEN_HOLZ,           442
     *BANK_HOLZ,LEN_HOLZ,LLEN_HOLZ,LINK_HOLZ,FLAG_HOLZ)                   442
C *                                                                       443
      CALL AGSBEGM(MODULE,IPRIN)                                          443
      IF (IPRIN.LT.0) RETURN                                              443
C Filling bank HOLZ - // HOLES PER ROW OUTER                              443
      BANK_TITLE='// HOLES PER ROW OUTER'                                 443
      BPATH_HOLZ='HOLZ*'                                                  443
      IDNUM_HOLZ(1)=0                                                     444
C // SELECTOR                                                             444
      HOLZ_INOUT=KOUTER                                                   444
      CHOLZ_INOUT='// SELECTOR'                                           444
C // NUMBER OF ROWS                                                       445
      HOLZ_NUMBEROFROWS=16                                                445
      CHOLZ_NUMBEROFROWS='// NUMBER OF ROWS'                              445
C // N/ROW                                                                447
      HOLZ_NUMBERPERROW(1)=9                                              447
      HOLZ_NUMBERPERROW(2)=9                                              447
      HOLZ_NUMBERPERROW(3)=9                                              447
      HOLZ_NUMBERPERROW(4)=9                                              447
      HOLZ_NUMBERPERROW(5)=9                                              447
      HOLZ_NUMBERPERROW(6)=8                                              447
      HOLZ_NUMBERPERROW(7)=8                                              447
      HOLZ_NUMBERPERROW(8)=8                                              447
      HOLZ_NUMBERPERROW(9)=8                                              447
      HOLZ_NUMBERPERROW(10)=8                                             447
      HOLZ_NUMBERPERROW(11)=7                                             447
      HOLZ_NUMBERPERROW(12)=7                                             447
      HOLZ_NUMBERPERROW(13)=7                                             447
      HOLZ_NUMBERPERROW(14)=7                                             447
      HOLZ_NUMBERPERROW(15)=7                                             447
      HOLZ_NUMBERPERROW(16)=6                                             447
      CHOLZ_NUMBERPERROW='// N/ROW'                                       447
C // Y HOLES                                                              483
      HOLZ_YHOLES(1)=-15.025                                              483
      HOLZ_YHOLES(2)=-11.606                                              483
      HOLZ_YHOLES(3)=-8.177                                               483
      HOLZ_YHOLES(4)=-4.220                                               483
      HOLZ_YHOLES(5)=0                                                    483
      HOLZ_YHOLES(6)=4.220                                                483
      HOLZ_YHOLES(7)=8.177                                                483
      HOLZ_YHOLES(8)=11.606                                               483
      HOLZ_YHOLES(9)=15.025                                               483
      HOLZ_YHOLES(10)=-15.025                                             483
      HOLZ_YHOLES(11)=-11.606                                             483
      HOLZ_YHOLES(12)=-8.177                                              483
      HOLZ_YHOLES(13)=-4.220                                              483
      HOLZ_YHOLES(14)=0                                                   483
      HOLZ_YHOLES(15)=4.220                                               483
      HOLZ_YHOLES(16)=8.177                                               483
      HOLZ_YHOLES(17)=11.606                                              483
      HOLZ_YHOLES(18)=15.025                                              483
      HOLZ_YHOLES(19)=-14.243                                             483
      HOLZ_YHOLES(20)=-11.078                                             483
      HOLZ_YHOLES(21)=-6.330                                              483
      HOLZ_YHOLES(22)=-3.165                                              483
      HOLZ_YHOLES(23)=0                                                   483
      HOLZ_YHOLES(24)=3.165                                               483
      HOLZ_YHOLES(25)=6.330                                               483
      HOLZ_YHOLES(26)=11.078                                              483
      HOLZ_YHOLES(27)=14.243                                              483
      HOLZ_YHOLES(28)=-14.243                                             483
      HOLZ_YHOLES(29)=-11.078                                             483
      HOLZ_YHOLES(30)=-6.330                                              483
      HOLZ_YHOLES(31)=-3.165                                              483
      HOLZ_YHOLES(32)=0                                                   483
      HOLZ_YHOLES(33)=3.165                                               483
      HOLZ_YHOLES(34)=6.330                                               483
      HOLZ_YHOLES(35)=11.078                                              483
      HOLZ_YHOLES(36)=14.243                                              483
      HOLZ_YHOLES(37)=-13.452                                             483
      HOLZ_YHOLES(38)=-10.023                                             483
      HOLZ_YHOLES(39)=-6.330                                              483
      HOLZ_YHOLES(40)=-3.165                                              483
      HOLZ_YHOLES(41)=0                                                   483
      HOLZ_YHOLES(42)=3.165                                               483
      HOLZ_YHOLES(43)=6.330                                               483
      HOLZ_YHOLES(44)=10.023                                              483
      HOLZ_YHOLES(45)=13.452                                              483
      HOLZ_YHOLES(46)=-13.452                                             483
      HOLZ_YHOLES(47)=-10.023                                             483
      HOLZ_YHOLES(48)=-6.330                                              483
      HOLZ_YHOLES(49)=-3.165                                              483
      HOLZ_YHOLES(50)=3.165                                               483
      HOLZ_YHOLES(51)=6.330                                               483
      HOLZ_YHOLES(52)=10.023                                              483
      HOLZ_YHOLES(53)=13.452                                              483
      HOLZ_YHOLES(54)=0                                                   483
      HOLZ_YHOLES(55)=-12.660                                             483
      HOLZ_YHOLES(56)=-9.495                                              483
      HOLZ_YHOLES(57)=-6.330                                              483
      HOLZ_YHOLES(58)=-2.110                                              483
      HOLZ_YHOLES(59)=2.110                                               483
      HOLZ_YHOLES(60)=6.330                                               483
      HOLZ_YHOLES(61)=9.495                                               483
      HOLZ_YHOLES(62)=12.660                                              483
      HOLZ_YHOLES(63)=0                                                   483
      HOLZ_YHOLES(64)=-12.660                                             483
      HOLZ_YHOLES(65)=-9.495                                              483
      HOLZ_YHOLES(66)=-6.330                                              483
      HOLZ_YHOLES(67)=-2.110                                              483
      HOLZ_YHOLES(68)=2.110                                               483
      HOLZ_YHOLES(69)=6.330                                               483
      HOLZ_YHOLES(70)=9.495                                               483
      HOLZ_YHOLES(71)=12.660                                              483
      HOLZ_YHOLES(72)=0                                                   483
      HOLZ_YHOLES(73)=-11.870                                             483
      HOLZ_YHOLES(74)=-8.440                                              483
      HOLZ_YHOLES(75)=-5.275                                              483
      HOLZ_YHOLES(76)=-2.110                                              483
      HOLZ_YHOLES(77)=2.110                                               483
      HOLZ_YHOLES(78)=5.275                                               483
      HOLZ_YHOLES(79)=8.440                                               483
      HOLZ_YHOLES(80)=11.870                                              483
      HOLZ_YHOLES(81)=0                                                   483
      HOLZ_YHOLES(82)=-11.870                                             483
      HOLZ_YHOLES(83)=-8.440                                              483
      HOLZ_YHOLES(84)=-5.275                                              483
      HOLZ_YHOLES(85)=-2.110                                              483
      HOLZ_YHOLES(86)=2.110                                               483
      HOLZ_YHOLES(87)=5.275                                               483
      HOLZ_YHOLES(88)=8.440                                               483
      HOLZ_YHOLES(89)=11.870                                              483
      HOLZ_YHOLES(90)=0                                                   483
      HOLZ_YHOLES(91)=-11.078                                             483
      HOLZ_YHOLES(92)=-7.649                                              483
      HOLZ_YHOLES(93)=-4.220                                              483
      HOLZ_YHOLES(94)=0                                                   483
      HOLZ_YHOLES(95)=4.220                                               483
      HOLZ_YHOLES(96)=7.649                                               483
      HOLZ_YHOLES(97)=11.078                                              483
      HOLZ_YHOLES(98)=0                                                   483
      HOLZ_YHOLES(99)=0                                                   483
      HOLZ_YHOLES(100)=-11.078                                            483
      HOLZ_YHOLES(101)=-7.649                                             483
      HOLZ_YHOLES(102)=-4.220                                             483
      HOLZ_YHOLES(103)=0                                                  483
      HOLZ_YHOLES(104)=4.220                                              483
      HOLZ_YHOLES(105)=7.649                                              483
      HOLZ_YHOLES(106)=11.078                                             483
      HOLZ_YHOLES(107)=0                                                  483
      HOLZ_YHOLES(108)=0                                                  483
      HOLZ_YHOLES(109)=-10.022                                            483
      HOLZ_YHOLES(110)=-6.858                                             483
      HOLZ_YHOLES(111)=-3.429                                             483
      HOLZ_YHOLES(112)=0                                                  483
      HOLZ_YHOLES(113)=3.429                                              483
      HOLZ_YHOLES(114)=6.858                                              483
      HOLZ_YHOLES(115)=10.022                                             483
      HOLZ_YHOLES(116)=0                                                  483
      HOLZ_YHOLES(117)=0                                                  483
      HOLZ_YHOLES(118)=-10.022                                            483
      HOLZ_YHOLES(119)=-6.858                                             483
      HOLZ_YHOLES(120)=-3.429                                             483
      HOLZ_YHOLES(121)=0                                                  483
      HOLZ_YHOLES(122)=3.429                                              483
      HOLZ_YHOLES(123)=6.858                                              483
      HOLZ_YHOLES(124)=10.022                                             483
      HOLZ_YHOLES(125)=0                                                  483
      HOLZ_YHOLES(126)=0                                                  483
      HOLZ_YHOLES(127)=-9.495                                             483
      HOLZ_YHOLES(128)=-6.330                                             483
      HOLZ_YHOLES(129)=-3.165                                             483
      HOLZ_YHOLES(130)=0                                                  483
      HOLZ_YHOLES(131)=3.165                                              483
      HOLZ_YHOLES(132)=6.330                                              483
      HOLZ_YHOLES(133)=9.495                                              483
      HOLZ_YHOLES(134)=0                                                  483
      HOLZ_YHOLES(135)=0                                                  483
      HOLZ_YHOLES(136)=-9.495                                             483
      HOLZ_YHOLES(137)=-6.330                                             483
      HOLZ_YHOLES(138)=-3.165                                             483
      HOLZ_YHOLES(139)=3.165                                              483
      HOLZ_YHOLES(140)=6.330                                              483
      HOLZ_YHOLES(141)=9.495                                              483
      HOLZ_YHOLES(142)=0                                                  483
      HOLZ_YHOLES(143)=0                                                  483
      HOLZ_YHOLES(144)=0                                                  483
      CHOLZ_YHOLES='// Y HOLES'                                           483
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'HOLZ',    484
     *BPATH_HOLZ,IDNUM_HOLZ,1, MAP_HOLZ,NAMES_HOLZ,COMMEN_HOLZ,           484
     *BANK_HOLZ,LEN_HOLZ,LLEN_HOLZ,LINK_HOLZ,FLAG_HOLZ)                   484
C *                                                                       484
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'HOLZ',    484
     *BPATH_HOLZ,IDNUM_HOLZ,1, MAP_HOLZ,NAMES_HOLZ,COMMEN_HOLZ,           484
     *BANK_HOLZ,LEN_HOLZ,LLEN_HOLZ,LINK_HOLZ,FLAG_HOLZ)                   484
C *                                                                       485
      CALL AGSBEGM(MODULE,IPRIN)                                          485
      IF (IPRIN.LT.0) RETURN                                              485
C Filling bank COOL - // COOLING TUBE LENGTHS / INNER                     485
      BANK_TITLE='// COOLING TUBE LENGTHS / INNER'                        485
      BPATH_COOL='COOL*'                                                  485
      IDNUM_COOL(1)=0                                                     486
C // INNER VS OUTER                                                       486
      COOL_INOUT=KINNER                                                   486
      CCOOL_INOUT='// INNER VS OUTER'                                     486
C // TUBE LENGTHS                                                         490
      COOL_TUBELENGTH(1)=19.300                                           490
      COOL_TUBELENGTH(2)=18.454                                           490
      COOL_TUBELENGTH(3)=17.610                                           490
      COOL_TUBELENGTH(4)=16.766                                           490
      COOL_TUBELENGTH(5)=15.922                                           490
      COOL_TUBELENGTH(6)=15.078                                           490
      COOL_TUBELENGTH(7)=14.235                                           490
      COOL_TUBELENGTH(8)=13.390                                           490
      COOL_TUBELENGTH(9)=12.546                                           490
      COOL_TUBELENGTH(10)=11.704                                          490
      COOL_TUBELENGTH(11)=10.860                                          490
      COOL_TUBELENGTH(12)=10.016                                          490
      COOL_TUBELENGTH(13)=9.172                                           490
      COOL_TUBELENGTH(14)=8.327                                           490
      COOL_TUBELENGTH(15)=7.483                                           490
      COOL_TUBELENGTH(16)=0.000                                           490
      CCOOL_TUBELENGTH='// TUBE LENGTHS'                                  490
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'COOL',    491
     *BPATH_COOL,IDNUM_COOL,1, MAP_COOL,NAMES_COOL,COMMEN_COOL,           491
     *BANK_COOL,LEN_COOL,LLEN_COOL,LINK_COOL,FLAG_COOL)                   491
C *                                                                       491
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'COOL',    491
     *BPATH_COOL,IDNUM_COOL,1, MAP_COOL,NAMES_COOL,COMMEN_COOL,           491
     *BANK_COOL,LEN_COOL,LLEN_COOL,LINK_COOL,FLAG_COOL)                   491
C *                                                                       492
      CALL AGSBEGM(MODULE,IPRIN)                                          492
      IF (IPRIN.LT.0) RETURN                                              492
C Filling bank COOL - // COOLING TUBE LENGTHS / OUTER                     492
      BANK_TITLE='// COOLING TUBE LENGTHS / OUTER'                        492
      BPATH_COOL='COOL*'                                                  492
      IDNUM_COOL(1)=0                                                     493
C // INNER VS OUTER                                                       493
      COOL_INOUT=KOUTER                                                   493
      CCOOL_INOUT='// INNER VS OUTER'                                     493
C // TUBE LENGTHS                                                         497
      COOL_TUBELENGTH(1)=33.620                                           497
      COOL_TUBELENGTH(2)=33.605                                           497
      COOL_TUBELENGTH(3)=32.761                                           497
      COOL_TUBELENGTH(4)=31.917                                           497
      COOL_TUBELENGTH(5)=31.073                                           497
      COOL_TUBELENGTH(6)=30.229                                           497
      COOL_TUBELENGTH(7)=29.385                                           497
      COOL_TUBELENGTH(8)=28.541                                           497
      COOL_TUBELENGTH(9)=27.697                                           497
      COOL_TUBELENGTH(10)=26.853                                          497
      COOL_TUBELENGTH(11)=26.009                                          497
      COOL_TUBELENGTH(12)=25.165                                          497
      COOL_TUBELENGTH(13)=24.321                                          497
      COOL_TUBELENGTH(14)=23.478                                          497
      COOL_TUBELENGTH(15)=22.634                                          497
      COOL_TUBELENGTH(16)=21.790                                          497
      CCOOL_TUBELENGTH='// TUBE LENGTHS'                                  497
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'COOL',    498
     *BPATH_COOL,IDNUM_COOL,1, MAP_COOL,NAMES_COOL,COMMEN_COOL,           498
     *BANK_COOL,LEN_COOL,LLEN_COOL,LINK_COOL,FLAG_COOL)                   498
C *                                                                       498
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'COOL',    498
     *BPATH_COOL,IDNUM_COOL,1, MAP_COOL,NAMES_COOL,COMMEN_COOL,           498
     *BANK_COOL,LEN_COOL,LLEN_COOL,LINK_COOL,FLAG_COOL)                   498
C use default TFEE bank                                                   499
      BPATH_TFEE='TFEE*'                                                  499
      IDNUM_TFEE(1)=0                                                     499
      CALL AGDGETP('TPCEGEO3A','*',-999,'TFEE',' ',BANK_TFEE,             499
     *BPATH_TFEE,IDNUM_TFEE,1,MAP_TFEE,NAMES_TFEE,BANK_TFEE,LEN_TFEE,     499
     *LLEN_TFEE,LINK_TFEE,FLAG_TFEE)                                      499
C use default TECW bank                                                   500
      BPATH_TECW='TECW*'                                                  500
      IDNUM_TECW(1)=0                                                     500
      CALL AGDGETP('TPCEGEO3A','*',-999,'TECW',' ',BANK_TECW,             500
     *BPATH_TECW,IDNUM_TECW,1,MAP_TECW,NAMES_TECW,BANK_TECW,LEN_TECW,     500
     *LLEN_TECW,LINK_TECW,FLAG_TECW)                                      500
C use default TPCR bank                                                   501
      BPATH_TPCR='TPCR*'                                                  501
      IDNUM_TPCR(1)=0                                                     501
      CALL AGDGETP('TPCEGEO3A','*',-999,'TPCR',' ',BANK_TPCR,             501
     *BPATH_TPCR,IDNUM_TPCR,1,MAP_TPCR,NAMES_TPCR,BANK_TPCR,LEN_TPCR,     501
     *LLEN_TPCR,LINK_TPCR,FLAG_TPCR)                                      501
C use default TPCC bank                                                   502
      BPATH_TPCC='TPCC*'                                                  502
      IDNUM_TPCC(1)=0                                                     502
      CALL AGDGETP('TPCEGEO3A','*',-999,'TPCC',' ',BANK_TPCC,             502
     *BPATH_TPCC,IDNUM_TPCC,1,MAP_TPCC,NAMES_TPCC,BANK_TPCC,LEN_TPCC,     502
     *LLEN_TPCC,LINK_TPCC,FLAG_TPCC)                                      502
C use TPCG bank                                                           503
      BPATH_TPCG='TPCG*'                                                  503
      IDNUM_TPCG(1)=0                                                     503
      TPCG_VERSION=TPCC_VERSION                                           503
      CALL AGDGETP('TPCEGEO3A','*',-999,'TPCG',NTPCG_VERSION,             503
     *RE_TPCG_VERSION,BPATH_TPCG,IDNUM_TPCG,1,MAP_TPCG,NAMES_TPCG,        503
     *BANK_TPCG,LEN_TPCG,LLEN_TPCG,LINK_TPCG,FLAG_TPCG)                   503
C GAS PLUS ENDCAPS                                                        504
      TOFCLENG = TPCG_LENGTH-2*TPCG_WHEELTHK-2*TPCR_RDOVTHK               504
C ACTIVE GAS                                                              505
      TPGVLENG = (TOFCLENG-TPCG_MEMBTHK)/2                                505
C ACTIVE GAS                                                              506
      TPGVLENG = (TPCG_LENGTHV-TPCG_MEMBTHK)/2                            506
      TOCSIR = TPCG_SENGASOR                                              507
      TOCSOR = TOCSIR + TPCG_TOCSDR                                       508
      TOKAIR = TOCSOR                                                     508
      TOKAOR = TOKAIR + TPCG_TOKADR                                       509
      TONXIR = TOKAOR                                                     509
      TONXOR = TONXIR + TPCG_TONXDR                                       510
      TOADIR = TONXOR                                                     510
      TOADOR = TOADIR + TPCG_TOADDR                                       511
      TOIGIR = TOADOR                                                     511
      TOIGOR = TOIGIR + TPCG_TOIGDR                                       512
      TOALIR = TOIGOR                                                     512
      TOALOR = TOALIR + TPCG_TOALDR/2.                                    513
      TOHAIR = TOALOR                                                     513
      TOHAOR = TOHAIR + TPCG_TOHADR                                       514
      TOALIR2= TOHAOR                                                     514
      TOALOR2= TOALIR2+ TPCG_TOALDR/2.                                    515
      TOFCIR = TOCSIR                                                     515
      TOFCOR = TOALOR2                                                    516
      TOFSIR = TOCSIR                                                     516
      TOFSOR = TOADOR                                                     517
      TIFCIR = TPCG_RMINIFC                                               518
      TIALIR = TIFCIR                                                     518
      TIALOR = TIALIR + TPCG_TIALDR/2.                                    519
      TIKAIR = TIALOR                                                     519
      TIKAOR = TIKAIR + TPCG_TIKADR                                       520
      TINXIR = TIKAOR                                                     520
      TINXOR = TINXIR + TPCG_TINXDR                                       521
      TIADIR = TINXOR                                                     521
      TIADOR = TIADIR + TPCG_TIADDR                                       522
      TIFCOR = TIADOR + TPCG_TIALDR/2.                                    523
      TPGVIR = TIFCOR                                                     523
C // TPC GAS INNER RADIUS                                                 524
      DEL = TPCG_RMAX-TOFCOR                                              525
      WRITE(LOUT,5010)TPCG_VERSION                                        525
5010  FORMAT(' TPCE: config=',F9.3)                                       527
      WIDTH1IOF(1)=7.661*INCH                                             528
      WIDTH1IOF(2)=22.287*INCH                                            528
      WIDTH2IOF(1)=20.552*INCH                                            529
      WIDTH2IOF(2)=35.422*INCH                                            529
      HEIGHTIOF(1)=24.222*INCH                                            530
      HEIGHTIOF(2)=24.840*INCH                                            530
      DEPTHIOF(1)=1.800*INCH                                              531
      DEPTHIOF(2)=1.800*INCH                                              531
C Geant COMPONENT : C ===> A=12 Z=6 W=5 <===                              532
      CALL AGSRESET                                                       532
      AG_A = 12                                                           532
      AG_Z = 6                                                            532
      AG_W = 5                                                            532
      AG_TITLE = 'C'                                                      532
      AG_EXNAME = 'COMPONENT'                                             532
      AG_PARLIST = 'A_Z_W'                                                532
      CALL AXCOMPONENT                                                    532
C *                                                                       532
C Geant COMPONENT : H ===> A=1 Z=1 W=8 <===                               532
      CALL AGSRESET                                                       533
      AG_A = 1                                                            533
      AG_Z = 1                                                            533
      AG_W = 8                                                            533
      AG_TITLE = 'H'                                                      533
      AG_EXNAME = 'COMPONENT'                                             533
      AG_PARLIST = 'A_Z_W'                                                533
      CALL AXCOMPONENT                                                    533
C *                                                                       533
C Geant COMPONENT : O ===> A=16 Z=8 W=2 <===                              533
      CALL AGSRESET                                                       534
      AG_A = 16                                                           534
      AG_Z = 8                                                            534
      AG_W = 2                                                            534
      AG_TITLE = 'O'                                                      534
      AG_EXNAME = 'COMPONENT'                                             534
      AG_PARLIST = 'A_Z_W'                                                534
      CALL AXCOMPONENT                                                    534
C *                                                                       534
C Geant MIXTURE : NOMEX ===> DENS=0.064 <===                              534
      CALL AGSRESET                                                       535
      AG_DENS = 0.064                                                     535
      AG_TITLE = 'NOMEX'                                                  535
      AG_EXNAME = 'MIXTURE'                                               535
      AG_PARLIST = 'DENS'                                                 535
      CALL AXMIXTURE                                                      535
C *                                                                       535
C Geant COMPONENT : C ===> A=12 Z=6 W=0.625 <===                          535
      CALL AGSRESET                                                       536
      AG_A = 12                                                           536
      AG_Z = 6                                                            536
      AG_W = 0.625                                                        536
      AG_TITLE = 'C'                                                      536
      AG_EXNAME = 'COMPONENT'                                             536
      AG_PARLIST = 'A_Z_W'                                                536
      CALL AXCOMPONENT                                                    536
C *                                                                       536
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4166667E-01 <===                   536
      CALL AGSRESET                                                       537
      AG_A = 1                                                            537
      AG_Z = 1                                                            537
      AG_W = 0.4166667E-01                                                537
      AG_TITLE = 'H'                                                      537
      AG_EXNAME = 'COMPONENT'                                             537
      AG_PARLIST = 'A_Z_W'                                                537
      CALL AXCOMPONENT                                                    537
C *                                                                       537
C Geant COMPONENT : O ===> A=16 Z=8 W=0.3333333 <===                      537
      CALL AGSRESET                                                       538
      AG_A = 16                                                           538
      AG_Z = 8                                                            538
      AG_W = 0.3333333                                                    538
      AG_TITLE = 'O'                                                      538
      AG_EXNAME = 'COMPONENT'                                             538
      AG_PARLIST = 'A_Z_W'                                                538
      CALL AXCOMPONENT                                                    538
C *                                                                       538
C Geant MIXTURE : ADHESIVE ===> DENS=1.2 <===                             538
      CALL AGSRESET                                                       539
      AG_DENS = 1.2                                                       539
      AG_TITLE = 'ADHESIVE'                                               539
      AG_EXNAME = 'MIXTURE'                                               539
      AG_PARLIST = 'DENS'                                                 539
      CALL AXMIXTURE                                                      539
C *                                                                       539
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.0105 <===                       539
      CALL AGSRESET                                                       540
      AG_A = 27                                                           540
      AG_Z = 13                                                           540
      AG_W = 0.0105                                                       540
      AG_TITLE = 'AL'                                                     540
      AG_EXNAME = 'COMPONENT'                                             540
      AG_PARLIST = 'A_Z_W'                                                540
      CALL AXCOMPONENT                                                    540
C *                                                                       540
C Geant COMPONENT : N ===> A=14 Z=7 W=0.7395 <===                         540
      CALL AGSRESET                                                       541
      AG_A = 14                                                           541
      AG_Z = 7                                                            541
      AG_W = 0.7395                                                       541
      AG_TITLE = 'N'                                                      541
      AG_EXNAME = 'COMPONENT'                                             541
      AG_PARLIST = 'A_Z_W'                                                541
      CALL AXCOMPONENT                                                    541
C *                                                                       541
C Geant COMPONENT : ADHESIVE ===> A=9 Z=4.5 W=0.2500 <===                 541
      CALL AGSRESET                                                       542
      AG_A = 9                                                            542
      AG_Z = 4.5                                                          542
      AG_W = 0.2500                                                       542
      AG_TITLE = 'ADHESIVE'                                               542
      AG_EXNAME = 'COMPONENT'                                             542
      AG_PARLIST = 'A_Z_W'                                                542
      CALL AXCOMPONENT                                                    542
C *                                                                       542
C Geant MIXTURE : AL_HONEYCOMB ===> DENS=0.282 <===                       542
      CALL AGSRESET                                                       543
      AG_DENS = 0.282                                                     543
      AG_TITLE = 'AL_HONEYCOMB'                                           543
      AG_EXNAME = 'MIXTURE'                                               543
      AG_PARLIST = 'DENS'                                                 543
      CALL AXMIXTURE                                                      543
C *                                                                       543
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             543
      CALL AGSRESET                                                       544
      AG_A = 28.08                                                        544
      AG_Z = 14                                                           544
      AG_W = 0.6*1*28./60.                                                544
      AG_TITLE = 'SI'                                                     544
      AG_EXNAME = 'COMPONENT'                                             544
      AG_PARLIST = 'A_Z_W'                                                544
      CALL AXCOMPONENT                                                    544
C *                                                                       544
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  544
C =                                                                       544
      CALL AGSRESET                                                       545
      AG_A = 16                                                           545
      AG_Z = 8                                                            545
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               545
      AG_TITLE = 'O'                                                      545
      AG_EXNAME = 'COMPONENT'                                             545
      AG_PARLIST = 'A_Z_W'                                                545
      CALL AXCOMPONENT                                                    545
C *                                                                       545
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 545
      CALL AGSRESET                                                       546
      AG_A = 12                                                           546
      AG_Z = 6                                                            546
      AG_W = 0.4*8*12./174.                                               546
      AG_TITLE = 'C'                                                      546
      AG_EXNAME = 'COMPONENT'                                             546
      AG_PARLIST = 'A_Z_W'                                                546
      CALL AXCOMPONENT                                                    546
C *                                                                       546
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  546
      CALL AGSRESET                                                       547
      AG_A = 1                                                            547
      AG_Z = 1                                                            547
      AG_W = 0.4*14*1./174.                                               547
      AG_TITLE = 'H'                                                      547
      AG_EXNAME = 'COMPONENT'                                             547
      AG_PARLIST = 'A_Z_W'                                                547
      CALL AXCOMPONENT                                                    547
C *                                                                       547
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  547
      CALL AGSRESET                                                       548
      AG_DENS = 1.7                                                       548
      AG_TITLE = 'G10'                                                    548
      AG_EXNAME = 'MIXTURE'                                               548
      AG_PARLIST = 'DENS'                                                 548
      CALL AXMIXTURE                                                      548
C *                                                                       548
C Geant COMPONENT : H ===> A=1 Z=1 W=1.06546299984001044E-02 <===         548
      CALL AGSRESET                                                       549
      AG_A = 1                                                            549
      AG_Z = 1                                                            549
      AG_W = 1.06546299984001044E-02                                      549
      AG_TITLE = 'H'                                                      549
      AG_EXNAME = 'COMPONENT'                                             549
      AG_PARLIST = 'A_Z_W'                                                549
      CALL AXCOMPONENT                                                    549
C *                                                                       549
C Geant COMPONENT : O ===> A=16 Z=8 W=8.52370399872008355E-02 <===        549
      CALL AGSRESET                                                       550
      AG_A = 16                                                           550
      AG_Z = 8                                                            550
      AG_W = 8.52370399872008355E-02                                      550
      AG_TITLE = 'O'                                                      550
      AG_EXNAME = 'COMPONENT'                                             550
      AG_PARLIST = 'A_Z_W'                                                550
      CALL AXCOMPONENT                                                    550
C *                                                                       550
C Geant COMPONENT : AL ===> A=27 Z=14 W=9.04108330014399053E-01 <===      550
      CALL AGSRESET                                                       551
      AG_A = 27                                                           551
      AG_Z = 14                                                           551
      AG_W = 9.04108330014399053E-01                                      551
      AG_TITLE = 'AL'                                                     551
      AG_EXNAME = 'COMPONENT'                                             551
      AG_PARLIST = 'A_Z_W'                                                551
      CALL AXCOMPONENT                                                    551
C *                                                                       551
C Geant MIXTURE : WATER_PIPE ===> DENS=2.32155 <===                       551
      CALL AGSRESET                                                       552
      AG_DENS = 2.32155                                                   552
      AG_TITLE = 'WATER_PIPE'                                             552
      AG_EXNAME = 'MIXTURE'                                               552
      AG_PARLIST = 'DENS'                                                 552
      CALL AXMIXTURE                                                      552
C *                                                                       552
C Geant COMPONENT : CU ===> A=63.54 Z=29 W=0.586 <===                     552
      CALL AGSRESET                                                       553
      AG_A = 63.54                                                        553
      AG_Z = 29                                                           553
      AG_W = 0.586                                                        553
      AG_TITLE = 'CU'                                                     553
      AG_EXNAME = 'COMPONENT'                                             553
      AG_PARLIST = 'A_Z_W'                                                553
      CALL AXCOMPONENT                                                    553
C *                                                                       553
C Geant COMPONENT : C ===> A=12.01 Z=6 W=0.259 <===                       553
      CALL AGSRESET                                                       554
      AG_A = 12.01                                                        554
      AG_Z = 6                                                            554
      AG_W = 0.259                                                        554
      AG_TITLE = 'C'                                                      554
      AG_EXNAME = 'COMPONENT'                                             554
      AG_PARLIST = 'A_Z_W'                                                554
      CALL AXCOMPONENT                                                    554
C *                                                                       554
C Geant COMPONENT : O ===> A=15.999 Z=8 W=0.138 <===                      554
      CALL AGSRESET                                                       555
      AG_A = 15.999                                                       555
      AG_Z = 8                                                            555
      AG_W = 0.138                                                        555
      AG_TITLE = 'O'                                                      555
      AG_EXNAME = 'COMPONENT'                                             555
      AG_PARLIST = 'A_Z_W'                                                555
      CALL AXCOMPONENT                                                    555
C *                                                                       555
C Geant COMPONENT : H ===> A=1.00794 Z=1 W=0.017 <===                     555
      CALL AGSRESET                                                       556
      AG_A = 1.00794                                                      556
      AG_Z = 1                                                            556
      AG_W = 0.017                                                        556
      AG_TITLE = 'H'                                                      556
      AG_EXNAME = 'COMPONENT'                                             556
      AG_PARLIST = 'A_Z_W'                                                556
      CALL AXCOMPONENT                                                    556
C *                                                                       556
C Geant MIXTURE : CABLES ===> DENS=2.68 <===                              556
      CALL AGSRESET                                                       557
      AG_DENS = 2.68                                                      557
      AG_TITLE = 'CABLES'                                                 557
      AG_EXNAME = 'MIXTURE'                                               557
      AG_PARLIST = 'DENS'                                                 557
      CALL AXMIXTURE                                                      557
C *                                                                       557
C Geant COMPONENT : AR ===> A=40 Z=18 W=9 <===                            557
      CALL AGSRESET                                                       558
      AG_A = 40                                                           558
      AG_Z = 18                                                           558
      AG_W = 9                                                            558
      AG_TITLE = 'AR'                                                     558
      AG_EXNAME = 'COMPONENT'                                             558
      AG_PARLIST = 'A_Z_W'                                                558
      CALL AXCOMPONENT                                                    558
C *                                                                       558
C Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                              558
      CALL AGSRESET                                                       559
      AG_A = 12                                                           559
      AG_Z = 6                                                            559
      AG_W = 1                                                            559
      AG_TITLE = 'C'                                                      559
      AG_EXNAME = 'COMPONENT'                                             559
      AG_PARLIST = 'A_Z_W'                                                559
      CALL AXCOMPONENT                                                    559
C *                                                                       559
C Geant COMPONENT : H ===> A=1 Z=1 W=4 <===                               559
      CALL AGSRESET                                                       560
      AG_A = 1                                                            560
      AG_Z = 1                                                            560
      AG_W = 4                                                            560
      AG_TITLE = 'H'                                                      560
      AG_EXNAME = 'COMPONENT'                                             560
      AG_PARLIST = 'A_Z_W'                                                560
      CALL AXCOMPONENT                                                    560
C *                                                                       560
C Geant MIXTURE : P10 ===> DENS=0.154053E-02 <===                         560
      CALL AGSRESET                                                       561
      AG_DENS = 0.154053E-02                                              561
      AG_TITLE = 'P10'                                                    561
      AG_EXNAME = 'MIXTURE'                                               561
      AG_PARLIST = 'DENS'                                                 561
      CALL AXMIXTURE                                                      561
C *                                                                       561
      CALL AGSBEGM(MODULE,IPRIN)                                          561
C Check IPRIN<0                                                           562
      IF (IPRIN.LT.0) RETURN                                              562
C Create TPCE :                                                           562
C Call To Local Procedure TPCE                                            562
      ASSIGN 5030 TO BLOCK_TPCE                                           562
      GO TO  5020                                                         562
5030  CONTINUE                                                            562
C *                                                                       562
C Geant POSITION : TPCE ===> MOTHER='CAVE' <===                           563
      CALL AGSRESET                                                       563
      AG_MOTHER = 'CAVE'                                                  563
      AG_TITLE = 'TPCE'                                                   563
      AG_EXNAME = 'POSITION'                                              563
      AG_PARLIST = 'MOTHER'                                               563
      CALL AXPOSITION                                                     563
C *                                                                       563
C ----------------------------------------------------------------------  563
C ------------ TPCE                                                       563
      CALL AGSENDM                                                        565
      RETURN                                                              565
C Begin Local Procedure TPCE                                              565
C TPCE IS THE TPC ENVELOPE                                                565
5020  CONTINUE                                                            565
         AG_TITLE='TPCE'                                                  565
         AG_MODULE=MODULE                                                 565
         AG_MARK='V00'                                                    565
         CALL AGSPUSH                                                     565
         AG_MODULE=MODULE                                                 565
C    *                                                                    565
C    Geant MATERIAL : AIR ===> inherited arguments used <===              566
         CALL AGSRESET                                                    566
         AG_TITLE = 'AIR'                                                 566
         AG_EXNAME = 'MATERIAL'                                           566
         AG_PARLIST = 'NONE'                                              566
         CALL AXMATERIAL                                                  566
C    *                                                                    566
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           566
         CALL AGSRESET                                                    567
         AG_TITLE = 'STANDARD'                                            567
         AG_EXNAME = 'MEDIUM'                                             567
         AG_PARLIST = 'NONE'                                              567
         CALL AXMEDIUM                                                    567
C    *                                                                    567
C    Geant ATTRIBUTE : TPCE ===> SEEN=0 COLO=KRED <===                    567
         CALL AGSRESET                                                    568
         AG_SEEN = 0                                                      568
         AG_COLO = KRED                                                   568
         AG_TITLE = 'TPCE'                                                568
         AG_EXNAME = 'ATTRIBUTE'                                          568
         AG_PARLIST = 'SEEN_COLO'                                         568
         CALL AXATTRIBUTE                                                 568
C    *                                                                    568
C    Geant SHAPE : TUBE ===> RMIN=TPCG_RMIN RMAX=TPCG_RMAX, DZ=TPCG_DZEN  568
C    VELOP <===                                                           568
         CALL AGSRESET                                                    570
         AG_RMIN = TPCG_RMIN                                              570
         AG_RMAX = TPCG_RMAX                                              570
         AG_DZ = TPCG_DZENVELOP                                           570
         AG_TITLE = 'TUBE'                                                570
         AG_EXNAME = 'SHAPE'                                              570
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      570
         CALL AXSHAPE                                                     570
C    *                                                                    570
C    Check IPRIN>=1                                                       570
         IF (IPRIN.GE.1) WRITE(LOUT,5040)AG_IVOLUME,AG_VOLUME             570
5040     FORMAT('Creating volume ',I4,2X,A4)                              572
         TPGVZ =(TPCG_MEMBTHK + TPGVLENG)/2                               573
         ZWHEEL1 = TPCG_LENGTHW/2 - TPCG_WHEELTHK                         573
         ZTOFCEND = TPCG_LENGTHW/2 - TPCG_WHEELTHK1                       574
         ZTIFCEND = TPCG_LENGTHW/2 + 0.97                                 575
         ZTIFCFLANGEBEGIN = ZTIFCEND - 1*INCH                             576
C    Create TPGV :                                                        578
C    Call To Local Procedure TPGV                                         578
         ASSIGN 5060 TO BLOCK_TPGV                                        578
         GO TO  5050                                                      578
5060     CONTINUE                                                         578
C    *                                                                    578
C    Geant POSITION : TPGV ===> Z=+TPGVZ THETAX=90 PHIX=75 THETAY=90 PHI  580
C    Y=-15 THETAZ=0 PHIZ=0 KONLY='MANY' <===                              580
         CALL AGSRESET                                                    580
         AG_Z = +TPGVZ                                                    580
         AG_THETAX = 90                                                   580
         AG_PHIX = 75                                                     580
         AG_THETAY = 90                                                   580
         AG_PHIY = -15                                                    580
         AG_THETAZ = 0                                                    580
         AG_PHIZ = 0                                                      580
         AG_KONLY = 'MANY'                                                580
         AG_TITLE = 'TPGV'                                                580
         AG_EXNAME = 'POSITION'                                           580
         AG_PARLIST = 'Z_THETAX_PHIX_THETAY_PHIY_THETAZ_PHIZ_KONLY'       580
         CALL AXPOSITION                                                  580
C    *                                                                    580
C    Geant POSITION : TPGV ===> Z=-TPGVZ THETAX=90 PHIX=105 THETAY=90 PH  580
C    IY=195 THETAZ=180 PHIZ=0 KONLY='MANY' <===                           580
         CALL AGSRESET                                                    582
         AG_Z = -TPGVZ                                                    582
         AG_THETAX = 90                                                   582
         AG_PHIX = 105                                                    582
         AG_THETAY = 90                                                   582
         AG_PHIY = 195                                                    582
         AG_THETAZ = 180                                                  582
         AG_PHIZ = 0                                                      582
         AG_KONLY = 'MANY'                                                582
         AG_TITLE = 'TPGV'                                                582
         AG_EXNAME = 'POSITION'                                           582
         AG_PARLIST = 'Z_THETAX_PHIX_THETAY_PHIY_THETAZ_PHIZ_KONLY'       582
         CALL AXPOSITION                                                  582
C    *                                                                    582
C    Create TPCM :                                                        583
C    Call To Local Procedure TPCM                                         583
         ASSIGN 5080 TO BLOCK_TPCM                                        583
         GO TO  5070                                                      583
5080     CONTINUE                                                         583
C    *                                                                    583
C    Geant POSITION : TPCM ===> inherited arguments used <===             584
         CALL AGSRESET                                                    584
         AG_TITLE = 'TPCM'                                                584
         AG_EXNAME = 'POSITION'                                           584
         AG_PARLIST = 'NONE'                                              584
         CALL AXPOSITION                                                  584
C    *                                                                    584
C    Create TIFC :                                                        585
C    Call To Local Procedure TIFC                                         585
         ASSIGN 5100 TO BLOCK_TIFC                                        585
         GO TO  5090                                                      585
5100     CONTINUE                                                         585
C    *                                                                    585
C    Geant POSITION : TIFC ===> inherited arguments used <===             586
         CALL AGSRESET                                                    586
         AG_TITLE = 'TIFC'                                                586
         AG_EXNAME = 'POSITION'                                           586
         AG_PARLIST = 'NONE'                                              586
         CALL AXPOSITION                                                  586
C    *                                                                    586
C    Create TOFC :                                                        587
C    Call To Local Procedure TOFC                                         587
         ASSIGN 5120 TO BLOCK_TOFC                                        587
         GO TO  5110                                                      587
5120     CONTINUE                                                         587
C    *                                                                    587
C    Geant POSITION : TOFC ===> inherited arguments used <===             588
         CALL AGSRESET                                                    588
         AG_TITLE = 'TOFC'                                                588
         AG_EXNAME = 'POSITION'                                           588
         AG_PARLIST = 'NONE'                                              588
         CALL AXPOSITION                                                  588
C    *                                                                    588
C    *                                                                    588
         ISECANG=0                                                        589
            GOTO 5133                                                     589
5131        ISECANG=ISECANG+(30)                                          589
5133        IF((30)*(ISECANG-(360-30)).GT.0) GO TO 5132                   589
C       Create TSWH :                                                     590
C       Call To Local Procedure TSWH                                      590
            ASSIGN 5150 TO BLOCK_TSWH                                     590
            GO TO  5140                                                   590
5150        CONTINUE                                                      590
C       *                                                                 590
C       Geant POSITION : TSWH ===> ALPHAZ=ISECANG KONLY='MANY' <===       591
            CALL AGSRESET                                                 591
            AG_ALPHAN=AG_ALPHAN+1                                         591
            AG_ALPHAK(AG_ALPHAN)=3                                        591
            AG_ALPHAL(AG_ALPHAN)=ISECANG                                  591
            AG_KONLY = 'MANY'                                             591
            AG_TITLE = 'TSWH'                                             591
            AG_EXNAME = 'POSITION'                                        591
            AG_PARLIST = 'ALPHAZ_KONLY'                                   591
            CALL AXPOSITION                                               591
C       *                                                                 591
C       Create TSWH :                                                     592
C       Call To Local Procedure TSWH                                      592
            ASSIGN 5160 TO BLOCK_TSWH                                     592
            GO TO  5140                                                   592
5160        CONTINUE                                                      592
C       *                                                                 592
C       Geant POSITION : TSWH ===> ORT=XY-Z ALPHAZ=ISECANG KONLY='MANY'   593
C       <===                                                              593
            CALL AGSRESET                                                 593
            AG_ALPHAN=AG_ALPHAN+1                                         593
            AG_ALPHAK(AG_ALPHAN)=IGOR('XY-Z')                             593
            AG_ALPHAN=AG_ALPHAN+1                                         593
            AG_ALPHAK(AG_ALPHAN)=3                                        593
            AG_ALPHAL(AG_ALPHAN)=ISECANG                                  593
            AG_KONLY = 'MANY'                                             593
            AG_TITLE = 'TSWH'                                             593
            AG_EXNAME = 'POSITION'                                        593
            AG_PARLIST = 'ORT_ALPHAZ_KONLY'                               593
            CALL AXPOSITION                                               593
C       *                                                                 593
         GO TO 5131                                                       594
5132     CONTINUE                                                         594
5021  CALL AGSPOPB                                                        595
      GO TO BLOCK_TPCE, (5020 ,5030)                                      595
C End of Local Procedure                                                  595
C ----------------------------------------------------------------------  596
C ------------ TPCM                                                       596
C Begin Local Procedure TPCM                                              597
C TPCM IS THE CENTRAL MEMBRANE PLACED IN TPC                              597
5070  CONTINUE                                                            597
         AG_TITLE='TPCM'                                                  597
         AG_MODULE=MODULE                                                 597
         AG_MARK='V00'                                                    597
         CALL AGSPUSH                                                     597
         AG_MODULE=MODULE                                                 597
C    *                                                                    597
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            598
         CALL AGSRESET                                                    598
         AG_TITLE = 'MYLAR'                                               598
         AG_EXNAME = 'MATERIAL'                                           598
         AG_PARLIST = 'NONE'                                              598
         CALL AXMATERIAL                                                  598
C    *                                                                    598
C    Geant ATTRIBUTE : TPCM ===> SEEN=1 COLO=KBLUE <===                   598
         CALL AGSRESET                                                    599
         AG_SEEN = 1                                                      599
         AG_COLO = KBLUE                                                  599
         AG_TITLE = 'TPCM'                                                599
         AG_EXNAME = 'ATTRIBUTE'                                          599
         AG_PARLIST = 'SEEN_COLO'                                         599
         CALL AXATTRIBUTE                                                 599
C    *                                                                    599
C    Geant SHAPE : TUBE ===> RMIN=TPGVIR RMAX=TPCG_SENGASOR, DZ=TPCG_MEM  599
C    BTHK/2 <===                                                          599
         CALL AGSRESET                                                    601
         AG_RMIN = TPGVIR                                                 601
         AG_RMAX = TPCG_SENGASOR                                          601
         AG_DZ = TPCG_MEMBTHK/2                                           601
         AG_TITLE = 'TUBE'                                                601
         AG_EXNAME = 'SHAPE'                                              601
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      601
         CALL AXSHAPE                                                     601
C    *                                                                    601
5071  CALL AGSPOPB                                                        602
      GO TO BLOCK_TPCM, (5070 ,5080)                                      602
C End of Local Procedure                                                  602
C ----------------------------------------------------------------------  603
C ------------ TIFC                                                       603
C Begin Local Procedure TIFC                                              604
C TIFC DEFINES THE INNER FIELD CAGE PLACED IN TPC                         604
5090  CONTINUE                                                            604
         AG_TITLE='TIFC'                                                  604
         AG_MODULE=MODULE                                                 604
         AG_MARK='V00'                                                    604
         CALL AGSPUSH                                                     604
         AG_MODULE=MODULE                                                 604
C    *                                                                    604
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        605
         CALL AGSRESET                                                    605
         AG_TITLE = 'ALUMINIUM'                                           605
         AG_EXNAME = 'MATERIAL'                                           605
         AG_PARLIST = 'NONE'                                              605
         CALL AXMATERIAL                                                  605
C    *                                                                    605
C    Geant ATTRIBUTE : TIFC ===> SEEN=1 COLO=KBLUE <===                   605
         CALL AGSRESET                                                    606
         AG_SEEN = 1                                                      606
         AG_COLO = KBLUE                                                  606
         AG_TITLE = 'TIFC'                                                606
         AG_EXNAME = 'ATTRIBUTE'                                          606
         AG_PARLIST = 'SEEN_COLO'                                         606
         CALL AXATTRIBUTE                                                 606
C    *                                                                    606
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=6 ZI={-ZTIFCEND, -ZTIFCF  606
C    LANGEBEGIN,-ZTIFCFLANGEBEGIN,ZTIFCFLANGEBEGIN, ZTIFCFLANGEBEGIN,ZTI  606
C    FCEND} RMN={TIFCIR, TIFCIR,TIFCIR,TIFCIR,TIFCIR,TIFCIR} RMX={TPCG_T  606
C    IFCRF, TPCG_TIFCRF,TIFCOR,TIFCOR,TPCG_TIFCRF, TPCG_TIFCRF} <===      606
         CALL AGSRESET                                                    612
         AG_PHI1 = 0                                                      612
         AG_DPHI = 360                                                    612
         AG_NZ = 6                                                        612
         AG_ZI(1)=-ZTIFCEND                                               612
         AG_ZI(2)=-ZTIFCFLANGEBEGIN                                       612
         AG_ZI(3)=-ZTIFCFLANGEBEGIN                                       612
         AG_ZI(4)=ZTIFCFLANGEBEGIN                                        612
         AG_ZI(5)=ZTIFCFLANGEBEGIN                                        612
         AG_ZI(6)=ZTIFCEND                                                612
         AG_RMN(1)=TIFCIR                                                 612
         AG_RMN(2)=TIFCIR                                                 612
         AG_RMN(3)=TIFCIR                                                 612
         AG_RMN(4)=TIFCIR                                                 612
         AG_RMN(5)=TIFCIR                                                 612
         AG_RMN(6)=TIFCIR                                                 612
         AG_RMX(1)=TPCG_TIFCRF                                            612
         AG_RMX(2)=TPCG_TIFCRF                                            612
         AG_RMX(3)=TIFCOR                                                 612
         AG_RMX(4)=TIFCOR                                                 612
         AG_RMX(5)=TPCG_TIFCRF                                            612
         AG_RMX(6)=TPCG_TIFCRF                                            612
         AG_TITLE = 'PCON'                                                612
         AG_EXNAME = 'SHAPE'                                              612
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           612
         CALL AXSHAPE                                                     612
C    *                                                                    612
C    Create TIKA :                                                        613
C    Call To Local Procedure TIKA                                         613
         ASSIGN 5180 TO BLOCK_TIKA                                        613
         GO TO  5170                                                      613
5180     CONTINUE                                                         613
C    *                                                                    613
C    Geant POSITION : TIKA ===> inherited arguments used <===             614
         CALL AGSRESET                                                    614
         AG_TITLE = 'TIKA'                                                614
         AG_EXNAME = 'POSITION'                                           614
         AG_PARLIST = 'NONE'                                              614
         CALL AXPOSITION                                                  614
C    *                                                                    614
C    Create TINX :                                                        615
C    Call To Local Procedure TINX                                         615
         ASSIGN 5200 TO BLOCK_TINX                                        615
         GO TO  5190                                                      615
5200     CONTINUE                                                         615
C    *                                                                    615
C    Geant POSITION : TINX ===> inherited arguments used <===             616
         CALL AGSRESET                                                    616
         AG_TITLE = 'TINX'                                                616
         AG_EXNAME = 'POSITION'                                           616
         AG_PARLIST = 'NONE'                                              616
         CALL AXPOSITION                                                  616
C    *                                                                    616
C    Create TIAD :                                                        617
C    Call To Local Procedure TIAD                                         617
         ASSIGN 5220 TO BLOCK_TIAD                                        617
         GO TO  5210                                                      617
5220     CONTINUE                                                         617
C    *                                                                    617
C    Geant POSITION : TIAD ===> inherited arguments used <===             618
         CALL AGSRESET                                                    618
         AG_TITLE = 'TIAD'                                                618
         AG_EXNAME = 'POSITION'                                           618
         AG_PARLIST = 'NONE'                                              618
         CALL AXPOSITION                                                  618
C    *                                                                    618
5091  CALL AGSPOPB                                                        619
      GO TO BLOCK_TIFC, (5090 ,5100)                                      619
C End of Local Procedure                                                  619
C ----------------------------------------------------------------------  620
C ------------ TIKA                                                       620
C Begin Local Procedure TIKA                                              621
C TIKA IS THE KAPTON FILM OF THE INNER FIELD CAGE                         621
5170  CONTINUE                                                            621
         AG_TITLE='TIKA'                                                  621
         AG_MODULE=MODULE                                                 621
         AG_MARK='V00'                                                    621
         CALL AGSPUSH                                                     621
         AG_MODULE=MODULE                                                 621
C    *                                                                    621
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            622
         CALL AGSRESET                                                    622
         AG_TITLE = 'MYLAR'                                               622
         AG_EXNAME = 'MATERIAL'                                           622
         AG_PARLIST = 'NONE'                                              622
         CALL AXMATERIAL                                                  622
C    *                                                                    622
C    Geant ATTRIBUTE : TIKA ===> SEEN=1 COLO=KVIOLET <===                 622
         CALL AGSRESET                                                    623
         AG_SEEN = 1                                                      623
         AG_COLO = KVIOLET                                                623
         AG_TITLE = 'TIKA'                                                623
         AG_EXNAME = 'ATTRIBUTE'                                          623
         AG_PARLIST = 'SEEN_COLO'                                         623
         CALL AXATTRIBUTE                                                 623
C    *                                                                    623
C    Geant SHAPE : TUBE ===> RMIN=TIKAIR RMAX=TIKAOR DZ=TPCG_DZYF1 <===   623
         CALL AGSRESET                                                    624
         AG_RMIN = TIKAIR                                                 624
         AG_RMAX = TIKAOR                                                 624
         AG_DZ = TPCG_DZYF1                                               624
         AG_TITLE = 'TUBE'                                                624
         AG_EXNAME = 'SHAPE'                                              624
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      624
         CALL AXSHAPE                                                     624
C    *                                                                    624
5171  CALL AGSPOPB                                                        625
      GO TO BLOCK_TIKA, (5170 ,5180)                                      625
C End of Local Procedure                                                  625
C ----------------------------------------------------------------------  626
C ------------ TINX                                                       626
C Begin Local Procedure TINX                                              627
C TINX IS THE INNER NOMEX STRUCTUREURE                                    627
5190  CONTINUE                                                            627
         AG_TITLE='TINX'                                                  627
         AG_MODULE=MODULE                                                 627
         AG_MARK='V00'                                                    627
         CALL AGSPUSH                                                     627
         AG_MODULE=MODULE                                                 627
C    *                                                                    627
C    Geant MATERIAL : NOMEX ===> inherited arguments used <===            628
         CALL AGSRESET                                                    628
         AG_TITLE = 'NOMEX'                                               628
         AG_EXNAME = 'MATERIAL'                                           628
         AG_PARLIST = 'NONE'                                              628
         CALL AXMATERIAL                                                  628
C    *                                                                    628
C    Geant ATTRIBUTE : TINX ===> SEEN=1 COLO=KYELLOW <===                 628
         CALL AGSRESET                                                    629
         AG_SEEN = 1                                                      629
         AG_COLO = KYELLOW                                                629
         AG_TITLE = 'TINX'                                                629
         AG_EXNAME = 'ATTRIBUTE'                                          629
         AG_PARLIST = 'SEEN_COLO'                                         629
         CALL AXATTRIBUTE                                                 629
C    *                                                                    629
C    Geant SHAPE : TUBE ===> RMIN=TINXIR RMAX=TINXOR DZ=TPCG_DZYF1 <===   629
         CALL AGSRESET                                                    630
         AG_RMIN = TINXIR                                                 630
         AG_RMAX = TINXOR                                                 630
         AG_DZ = TPCG_DZYF1                                               630
         AG_TITLE = 'TUBE'                                                630
         AG_EXNAME = 'SHAPE'                                              630
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      630
         CALL AXSHAPE                                                     630
C    *                                                                    630
5191  CALL AGSPOPB                                                        631
      GO TO BLOCK_TINX, (5190 ,5200)                                      631
C End of Local Procedure                                                  631
C ----------------------------------------------------------------------  632
C ------------ TIAD                                                       632
C Begin Local Procedure TIAD                                              633
C TIAD THE INNER ADHESIVE STRUCTUREURE                                    633
5210  CONTINUE                                                            633
         AG_TITLE='TIAD'                                                  633
         AG_MODULE=MODULE                                                 633
         AG_MARK='V00'                                                    633
         CALL AGSPUSH                                                     633
         AG_MODULE=MODULE                                                 633
C    *                                                                    633
C    Geant MATERIAL : ADHESIVE ===> inherited arguments used <===         634
         CALL AGSRESET                                                    634
         AG_TITLE = 'ADHESIVE'                                            634
         AG_EXNAME = 'MATERIAL'                                           634
         AG_PARLIST = 'NONE'                                              634
         CALL AXMATERIAL                                                  634
C    *                                                                    634
C    Geant ATTRIBUTE : TIKA ===> SEEN=1 COLO=KVIOLET <===                 634
         CALL AGSRESET                                                    635
         AG_SEEN = 1                                                      635
         AG_COLO = KVIOLET                                                635
         AG_TITLE = 'TIKA'                                                635
         AG_EXNAME = 'ATTRIBUTE'                                          635
         AG_PARLIST = 'SEEN_COLO'                                         635
         CALL AXATTRIBUTE                                                 635
C    *                                                                    635
C    Geant SHAPE : TUBE ===> RMIN=TIADIR RMAX=TIADOR DZ=TPCG_DZYF1 <===   635
         CALL AGSRESET                                                    636
         AG_RMIN = TIADIR                                                 636
         AG_RMAX = TIADOR                                                 636
         AG_DZ = TPCG_DZYF1                                               636
         AG_TITLE = 'TUBE'                                                636
         AG_EXNAME = 'SHAPE'                                              636
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      636
         CALL AXSHAPE                                                     636
C    *                                                                    636
5211  CALL AGSPOPB                                                        637
      GO TO BLOCK_TIAD, (5210 ,5220)                                      637
C End of Local Procedure                                                  637
C ----------------------------------------------------------------------  638
C ------------ TOFC                                                       638
C Begin Local Procedure TOFC                                              639
C TOFC DEFINES OUTER FIELD CAGE - FILL IT WITH INSULATING GAS ALREADY     639
5110  CONTINUE                                                            639
         AG_TITLE='TOFC'                                                  639
         AG_MODULE=MODULE                                                 639
         AG_MARK='V00'                                                    639
         CALL AGSPUSH                                                     639
         AG_MODULE=MODULE                                                 639
C    *                                                                    639
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        640
         CALL AGSRESET                                                    640
         AG_TITLE = 'ALUMINIUM'                                           640
         AG_EXNAME = 'MATERIAL'                                           640
         AG_PARLIST = 'NONE'                                              640
         CALL AXMATERIAL                                                  640
C    *                                                                    640
C    Geant ATTRIBUTE : TOFC ===> SEEN=1 COLO=KGREEN <===                  640
         CALL AGSRESET                                                    641
         AG_SEEN = 1                                                      641
         AG_COLO = KGREEN                                                 641
         AG_TITLE = 'TOFC'                                                641
         AG_EXNAME = 'ATTRIBUTE'                                          641
         AG_PARLIST = 'SEEN_COLO'                                         641
         CALL AXATTRIBUTE                                                 641
C    *                                                                    641
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=6 ZI={-ZTOFCEND, -ZWHEEL  641
C    1,-ZWHEEL1,ZWHEEL1,ZWHEEL1,ZTOFCEND}, RMN={TPCG_WHEELOR1,TPCG_WHEEL  641
C    OR1,TOFCIR, TOFCIR,TPCG_WHEELOR1,TPCG_WHEELOR1} RMX={TPCG_WHEELOR,   641
C    TPCG_WHEELOR,TOFCOR,TOFCOR,TPCG_WHEELOR, TPCG_WHEELOR} <===          641
         CALL AGSRESET                                                    647
         AG_PHI1 = 0                                                      647
         AG_DPHI = 360                                                    647
         AG_NZ = 6                                                        647
         AG_ZI(1)=-ZTOFCEND                                               647
         AG_ZI(2)=-ZWHEEL1                                                647
         AG_ZI(3)=-ZWHEEL1                                                647
         AG_ZI(4)=ZWHEEL1                                                 647
         AG_ZI(5)=ZWHEEL1                                                 647
         AG_ZI(6)=ZTOFCEND                                                647
         AG_RMN(1)=TPCG_WHEELOR1                                          647
         AG_RMN(2)=TPCG_WHEELOR1                                          647
         AG_RMN(3)=TOFCIR                                                 647
         AG_RMN(4)=TOFCIR                                                 647
         AG_RMN(5)=TPCG_WHEELOR1                                          647
         AG_RMN(6)=TPCG_WHEELOR1                                          647
         AG_RMX(1)=TPCG_WHEELOR                                           647
         AG_RMX(2)=TPCG_WHEELOR                                           647
         AG_RMX(3)=TOFCOR                                                 647
         AG_RMX(4)=TOFCOR                                                 647
         AG_RMX(5)=TPCG_WHEELOR                                           647
         AG_RMX(6)=TPCG_WHEELOR                                           647
         AG_TITLE = 'PCON'                                                647
         AG_EXNAME = 'SHAPE'                                              647
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           647
         CALL AXSHAPE                                                     647
C    *                                                                    647
C    Create TOAD :                                                        648
C    Call To Local Procedure TOAD                                         648
         ASSIGN 5240 TO BLOCK_TOAD                                        648
         GO TO  5230                                                      648
5240     CONTINUE                                                         648
C    *                                                                    648
C    Geant POSITION : TOAD ===> inherited arguments used <===             649
         CALL AGSRESET                                                    649
         AG_TITLE = 'TOAD'                                                649
         AG_EXNAME = 'POSITION'                                           649
         AG_PARLIST = 'NONE'                                              649
         CALL AXPOSITION                                                  649
C    *                                                                    649
C    Create TOFS :                                                        650
C    Call To Local Procedure TOFS                                         650
         ASSIGN 5260 TO BLOCK_TOFS                                        650
         GO TO  5250                                                      650
5260     CONTINUE                                                         650
C    *                                                                    650
C    Geant POSITION : TOFS ===> inherited arguments used <===             651
         CALL AGSRESET                                                    651
         AG_TITLE = 'TOFS'                                                651
         AG_EXNAME = 'POSITION'                                           651
         AG_PARLIST = 'NONE'                                              651
         CALL AXPOSITION                                                  651
C    *                                                                    651
C    Create TOKA :                                                        652
C    Call To Local Procedure TOKA                                         652
         ASSIGN 5280 TO BLOCK_TOKA                                        652
         GO TO  5270                                                      652
5280     CONTINUE                                                         652
C    *                                                                    652
C    Geant POSITION : TOKA ===> inherited arguments used <===             653
         CALL AGSRESET                                                    653
         AG_TITLE = 'TOKA'                                                653
         AG_EXNAME = 'POSITION'                                           653
         AG_PARLIST = 'NONE'                                              653
         CALL AXPOSITION                                                  653
C    *                                                                    653
C    Create TONX :                                                        654
C    Call To Local Procedure TONX                                         654
         ASSIGN 5300 TO BLOCK_TONX                                        654
         GO TO  5290                                                      654
5300     CONTINUE                                                         654
C    *                                                                    654
C    Geant POSITION : TONX ===> inherited arguments used <===             655
         CALL AGSRESET                                                    655
         AG_TITLE = 'TONX'                                                655
         AG_EXNAME = 'POSITION'                                           655
         AG_PARLIST = 'NONE'                                              655
         CALL AXPOSITION                                                  655
C    *                                                                    655
C    Create TOIG :                                                        656
C    Call To Local Procedure TOIG                                         656
         ASSIGN 5320 TO BLOCK_TOIG                                        656
         GO TO  5310                                                      656
5320     CONTINUE                                                         656
C    *                                                                    656
C    Geant POSITION : TOIG ===> inherited arguments used <===             657
         CALL AGSRESET                                                    657
         AG_TITLE = 'TOIG'                                                657
         AG_EXNAME = 'POSITION'                                           657
         AG_PARLIST = 'NONE'                                              657
         CALL AXPOSITION                                                  657
C    *                                                                    657
C    Create TOHA :                                                        658
C    Call To Local Procedure TOHA                                         658
         ASSIGN 5340 TO BLOCK_TOHA                                        658
         GO TO  5330                                                      658
5340     CONTINUE                                                         658
C    *                                                                    658
C    Geant POSITION : TOHA ===> inherited arguments used <===             659
         CALL AGSRESET                                                    659
         AG_TITLE = 'TOHA'                                                659
         AG_EXNAME = 'POSITION'                                           659
         AG_PARLIST = 'NONE'                                              659
         CALL AXPOSITION                                                  659
C    *                                                                    659
5111  CALL AGSPOPB                                                        660
      GO TO BLOCK_TOFC, (5110 ,5120)                                      660
C End of Local Procedure                                                  660
C ----------------------------------------------------------------------  661
C ------------ TOFS                                                       661
C Begin Local Procedure TOFS                                              662
C TOFS IS THE OUTER FIELD CAGE STRUCTUREURE                               662
5250  CONTINUE                                                            662
         AG_TITLE='TOFS'                                                  662
         AG_MODULE=MODULE                                                 662
         AG_MARK='V00'                                                    662
         CALL AGSPUSH                                                     662
         AG_MODULE=MODULE                                                 662
C    *                                                                    662
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           663
         CALL AGSRESET                                                    663
         AG_TITLE = 'COPPER'                                              663
         AG_EXNAME = 'MATERIAL'                                           663
         AG_PARLIST = 'NONE'                                              663
         CALL AXMATERIAL                                                  663
C    *                                                                    663
C    Geant MEDIUM : TOFS_COPPER ===> IFIELD=1 FIELDM=20 TMAXFD=20 EPSIL=  663
C    0.01 STEMAX=10 DEEMAX=0.25, STMIN=0.0281386 ISVOL=0 <===             663
         CALL AGSRESET                                                    665
         AG_IFIELD = 1                                                    665
         AG_FIELDM = 20                                                   665
         AG_TMAXFD = 20                                                   665
         AG_EPSIL = 0.01                                                  665
         AG_STEMAX = 10                                                   665
         AG_DEEMAX = 0.25                                                 665
         AG_STMIN = 0.0281386                                             665
         AG_ISVOL = 0                                                     665
         AG_TITLE = 'TOFS_COPPER'                                         665
         AG_EXNAME = 'MEDIUM'                                             665
         AG_PARLIST = 'IFIELD_FIELDM_TMAXFD_EPSIL_STEMAX_DEEMAX_STMIN_IS  665
     *VOL'                                                                665
         CALL AXMEDIUM                                                    665
C    *                                                                    665
C    Geant ATTRIBUTE : TOFS ===> SEEN=1 COLO=KBLUE <===                   665
         CALL AGSRESET                                                    666
         AG_SEEN = 1                                                      666
         AG_COLO = KBLUE                                                  666
         AG_TITLE = 'TOFS'                                                666
         AG_EXNAME = 'ATTRIBUTE'                                          666
         AG_PARLIST = 'SEEN_COLO'                                         666
         CALL AXATTRIBUTE                                                 666
C    *                                                                    666
C    Geant SHAPE : TUBE ===> RMIN=TOCSIR RMAX=TOCSOR DZ=TPCG_DZYF2 <===   666
         CALL AGSRESET                                                    667
         AG_RMIN = TOCSIR                                                 667
         AG_RMAX = TOCSOR                                                 667
         AG_DZ = TPCG_DZYF2                                               667
         AG_TITLE = 'TUBE'                                                667
         AG_EXNAME = 'SHAPE'                                              667
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      667
         CALL AXSHAPE                                                     667
C    *                                                                    667
5251  CALL AGSPOPB                                                        668
      GO TO BLOCK_TOFS, (5250 ,5260)                                      668
C End of Local Procedure                                                  668
C ----------------------------------------------------------------------  669
C ------------ TOKA                                                       669
C Begin Local Procedure TOKA                                              670
C TOKA IS MYLAR LAYER                                                     670
5270  CONTINUE                                                            670
         AG_TITLE='TOKA'                                                  670
         AG_MODULE=MODULE                                                 670
         AG_MARK='V00'                                                    670
         CALL AGSPUSH                                                     670
         AG_MODULE=MODULE                                                 670
C    *                                                                    670
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            671
         CALL AGSRESET                                                    671
         AG_TITLE = 'MYLAR'                                               671
         AG_EXNAME = 'MATERIAL'                                           671
         AG_PARLIST = 'NONE'                                              671
         CALL AXMATERIAL                                                  671
C    *                                                                    671
C    Geant ATTRIBUTE : TOKA ===> SEEN=1 COLO=KGREEN <===                  671
         CALL AGSRESET                                                    672
         AG_SEEN = 1                                                      672
         AG_COLO = KGREEN                                                 672
         AG_TITLE = 'TOKA'                                                672
         AG_EXNAME = 'ATTRIBUTE'                                          672
         AG_PARLIST = 'SEEN_COLO'                                         672
         CALL AXATTRIBUTE                                                 672
C    *                                                                    672
C    Geant SHAPE : TUBE ===> RMIN=TOKAIR RMAX=TOKAOR DZ=TPCG_DZYF2 <===   672
         CALL AGSRESET                                                    673
         AG_RMIN = TOKAIR                                                 673
         AG_RMAX = TOKAOR                                                 673
         AG_DZ = TPCG_DZYF2                                               673
         AG_TITLE = 'TUBE'                                                673
         AG_EXNAME = 'SHAPE'                                              673
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      673
         CALL AXSHAPE                                                     673
C    *                                                                    673
5271  CALL AGSPOPB                                                        674
      GO TO BLOCK_TOKA, (5270 ,5280)                                      674
C End of Local Procedure                                                  674
C ----------------------------------------------------------------------  675
C ------------ TONX                                                       675
C Begin Local Procedure TONX                                              676
C TONX IS NOMEX SUPPORT                                                   676
5290  CONTINUE                                                            676
         AG_TITLE='TONX'                                                  676
         AG_MODULE=MODULE                                                 676
         AG_MARK='V00'                                                    676
         CALL AGSPUSH                                                     676
         AG_MODULE=MODULE                                                 676
C    *                                                                    676
C    Geant MATERIAL : NOMEX ===> inherited arguments used <===            677
         CALL AGSRESET                                                    677
         AG_TITLE = 'NOMEX'                                               677
         AG_EXNAME = 'MATERIAL'                                           677
         AG_PARLIST = 'NONE'                                              677
         CALL AXMATERIAL                                                  677
C    *                                                                    677
C    Geant ATTRIBUTE : TONX ===> SEEN=1 COLO=KRED <===                    677
         CALL AGSRESET                                                    678
         AG_SEEN = 1                                                      678
         AG_COLO = KRED                                                   678
         AG_TITLE = 'TONX'                                                678
         AG_EXNAME = 'ATTRIBUTE'                                          678
         AG_PARLIST = 'SEEN_COLO'                                         678
         CALL AXATTRIBUTE                                                 678
C    *                                                                    678
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=6 ZI={-TPCG_DZYF2, -TPCG  678
C    _DZYF3,-TPCG_DZYF3,TPCG_DZYF3,TPCG_DZYF3, TPCG_DZYF2} RMN={TONXOR-0  678
C    .2,TONXOR-0.2, TONXIR,TONXIR,TONXOR-0.2,TONXOR-0.2} RMX={TONXOR, TO  678
C    NXOR,TONXOR,TONXOR,TONXOR,TONXOR} <===                               678
         CALL AGSRESET                                                    683
         AG_PHI1 = 0                                                      683
         AG_DPHI = 360                                                    683
         AG_NZ = 6                                                        683
         AG_ZI(1)=-TPCG_DZYF2                                             683
         AG_ZI(2)=-TPCG_DZYF3                                             683
         AG_ZI(3)=-TPCG_DZYF3                                             683
         AG_ZI(4)=TPCG_DZYF3                                              683
         AG_ZI(5)=TPCG_DZYF3                                              683
         AG_ZI(6)=TPCG_DZYF2                                              683
         AG_RMN(1)=TONXOR-0.2                                             683
         AG_RMN(2)=TONXOR-0.2                                             683
         AG_RMN(3)=TONXIR                                                 683
         AG_RMN(4)=TONXIR                                                 683
         AG_RMN(5)=TONXOR-0.2                                             683
         AG_RMN(6)=TONXOR-0.2                                             683
         AG_RMX(1)=TONXOR                                                 683
         AG_RMX(2)=TONXOR                                                 683
         AG_RMX(3)=TONXOR                                                 683
         AG_RMX(4)=TONXOR                                                 683
         AG_RMX(5)=TONXOR                                                 683
         AG_RMX(6)=TONXOR                                                 683
         AG_TITLE = 'PCON'                                                683
         AG_EXNAME = 'SHAPE'                                              683
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           683
         CALL AXSHAPE                                                     683
C    *                                                                    683
5291  CALL AGSPOPB                                                        684
      GO TO BLOCK_TONX, (5290 ,5300)                                      684
C End of Local Procedure                                                  684
C ----------------------------------------------------------------------  685
C ------------ TOAD                                                       685
C Begin Local Procedure TOAD                                              686
C TOAD IS ADHESIVE LAYER                                                  686
5230  CONTINUE                                                            686
         AG_TITLE='TOAD'                                                  686
         AG_MODULE=MODULE                                                 686
         AG_MARK='V00'                                                    686
         CALL AGSPUSH                                                     686
         AG_MODULE=MODULE                                                 686
C    *                                                                    686
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            687
         CALL AGSRESET                                                    687
         AG_TITLE = 'MYLAR'                                               687
         AG_EXNAME = 'MATERIAL'                                           687
         AG_PARLIST = 'NONE'                                              687
         CALL AXMATERIAL                                                  687
C    *                                                                    687
C    Geant ATTRIBUTE : TOAD ===> SEEN=1 COLO=KLIGHTBLUE <===              687
         CALL AGSRESET                                                    688
         AG_SEEN = 1                                                      688
         AG_COLO = KLIGHTBLUE                                             688
         AG_TITLE = 'TOAD'                                                688
         AG_EXNAME = 'ATTRIBUTE'                                          688
         AG_PARLIST = 'SEEN_COLO'                                         688
         CALL AXATTRIBUTE                                                 688
C    *                                                                    688
C    Geant SHAPE : TUBE ===> RMIN=TOADIR RMAX=TOADOR DZ=TPCG_DZYF2 <===   688
         CALL AGSRESET                                                    689
         AG_RMIN = TOADIR                                                 689
         AG_RMAX = TOADOR                                                 689
         AG_DZ = TPCG_DZYF2                                               689
         AG_TITLE = 'TUBE'                                                689
         AG_EXNAME = 'SHAPE'                                              689
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      689
         CALL AXSHAPE                                                     689
C    *                                                                    689
5231  CALL AGSPOPB                                                        690
      GO TO BLOCK_TOAD, (5230 ,5240)                                      690
C End of Local Procedure                                                  690
C ----------------------------------------------------------------------  691
C ------------ TOIG                                                       691
C Begin Local Procedure TOIG                                              692
C TOIG IS INSULATING GAS (NITROGEN)                                       692
5310  CONTINUE                                                            692
         AG_TITLE='TOIG'                                                  692
         AG_MODULE=MODULE                                                 692
         AG_MARK='V00'                                                    692
         CALL AGSPUSH                                                     692
         AG_MODULE=MODULE                                                 692
C    *                                                                    692
C    Geant MATERIAL : NITROGEN_GAS ===> inherited arguments used <===     693
         CALL AGSRESET                                                    693
         AG_TITLE = 'NITROGEN_GAS'                                        693
         AG_EXNAME = 'MATERIAL'                                           693
         AG_PARLIST = 'NONE'                                              693
         CALL AXMATERIAL                                                  693
C    *                                                                    693
C    Geant ATTRIBUTE : TOAD ===> SEEN=1 COLO=KLIGHTBLUE <===              693
         CALL AGSRESET                                                    694
         AG_SEEN = 1                                                      694
         AG_COLO = KLIGHTBLUE                                             694
         AG_TITLE = 'TOAD'                                                694
         AG_EXNAME = 'ATTRIBUTE'                                          694
         AG_PARLIST = 'SEEN_COLO'                                         694
         CALL AXATTRIBUTE                                                 694
C    *                                                                    694
C    Geant SHAPE : TUBE ===> RMIN=TOIGIR RMAX=TOIGOR DZ=TPCG_DZYF2 <===   694
         CALL AGSRESET                                                    695
         AG_RMIN = TOIGIR                                                 695
         AG_RMAX = TOIGOR                                                 695
         AG_DZ = TPCG_DZYF2                                               695
         AG_TITLE = 'TUBE'                                                695
         AG_EXNAME = 'SHAPE'                                              695
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      695
         CALL AXSHAPE                                                     695
C    *                                                                    695
5311  CALL AGSPOPB                                                        696
      GO TO BLOCK_TOIG, (5310 ,5320)                                      696
C End of Local Procedure                                                  696
C ----------------------------------------------------------------------  697
C ------------ TOHA                                                       697
C Begin Local Procedure TOHA                                              698
C TOHA GAS CONTAINMENT VESSEL (AL) + HA                                   698
5330  CONTINUE                                                            698
         AG_TITLE='TOHA'                                                  698
         AG_MODULE=MODULE                                                 698
         AG_MARK='V00'                                                    698
         CALL AGSPUSH                                                     698
         AG_MODULE=MODULE                                                 698
C    *                                                                    698
C    Geant MATERIAL : AL_HONEYCOMB ===> inherited arguments used <===     699
         CALL AGSRESET                                                    699
         AG_TITLE = 'AL_HONEYCOMB'                                        699
         AG_EXNAME = 'MATERIAL'                                           699
         AG_PARLIST = 'NONE'                                              699
         CALL AXMATERIAL                                                  699
C    *                                                                    699
C    Geant ATTRIBUTE : TOHA ===> SEEN=1 COLO=KVIOLET <===                 699
         CALL AGSRESET                                                    700
         AG_SEEN = 1                                                      700
         AG_COLO = KVIOLET                                                700
         AG_TITLE = 'TOHA'                                                700
         AG_EXNAME = 'ATTRIBUTE'                                          700
         AG_PARLIST = 'SEEN_COLO'                                         700
         CALL AXATTRIBUTE                                                 700
C    *                                                                    700
C    Geant SHAPE : TUBE ===> RMIN=TOHAIR RMAX=TOHAOR DZ=TPCG_DZYF3 <===   700
         CALL AGSRESET                                                    701
         AG_RMIN = TOHAIR                                                 701
         AG_RMAX = TOHAOR                                                 701
         AG_DZ = TPCG_DZYF3                                               701
         AG_TITLE = 'TUBE'                                                701
         AG_EXNAME = 'SHAPE'                                              701
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      701
         CALL AXSHAPE                                                     701
C    *                                                                    701
5331  CALL AGSPOPB                                                        702
      GO TO BLOCK_TOHA, (5330 ,5340)                                      702
C End of Local Procedure                                                  702
C ----------------------------------------------------------------------  703
C ------------ TSWH                                                       703
C Begin Local Procedure TSWH                                              704
C TSWH TPCSECTORWHOLE IS SECTOR AS WHOLE                                  704
5140  CONTINUE                                                            704
         AG_TITLE='TSWH'                                                  704
         AG_MODULE=MODULE                                                 704
         AG_MARK='V00'                                                    704
         CALL AGSPUSH                                                     704
         AG_MODULE=MODULE                                                 704
C    *                                                                    704
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           705
         CALL AGSRESET                                                    705
         AG_TITLE = 'STANDARD'                                            705
         AG_EXNAME = 'MEDIUM'                                             705
         AG_PARLIST = 'NONE'                                              705
         CALL AXMEDIUM                                                    705
C    *                                                                    705
C    Geant ATTRIBUTE : TSWH ===> SEEN=1 COLO=KVIOLET <===                 705
         CALL AGSRESET                                                    706
         AG_SEEN = 1                                                      706
         AG_COLO = KVIOLET                                                706
         AG_TITLE = 'TSWH'                                                706
         AG_EXNAME = 'ATTRIBUTE'                                          706
         AG_PARLIST = 'SEEN_COLO'                                         706
         CALL AXATTRIBUTE                                                 706
C    *                                                                    706
C    Geant SHAPE : PCON ===> PHI1=-15 DPHI=30 NZ=8 ZI={TPCG_MEMBTHK/2, Z  706
C    WHEEL1,ZWHEEL1,ZTOFCEND,ZTOFCEND, ZTIFCFLANGEBEGIN,ZTIFCFLANGEBEGIN  706
C    ,TPCG_DZENVELOP}, RMN={TIFCOR,TIFCOR,TIFCOR,TIFCOR,TIFCOR, TPCG_TIF  706
C    CRF+TPCG_TIFCDRT,TPCG_TIFCRF+TPCG_TIFCDRT, TPCG_TIFCRF+TPCG_TIFCDRT  706
C    } RMX={TOFCIR,TOFCIR, TPCG_WHEELOR1,TPCG_WHEELOR1,TPCG_WHEELOR, TPC  706
C    G_WHEELOR,TPCG_WHEELOR,TPCG_WHEELOR} <===                            706
         CALL AGSRESET                                                    714
         AG_PHI1 = -15                                                    714
         AG_DPHI = 30                                                     714
         AG_NZ = 8                                                        714
         AG_ZI(1)=TPCG_MEMBTHK/2                                          714
         AG_ZI(2)=ZWHEEL1                                                 714
         AG_ZI(3)=ZWHEEL1                                                 714
         AG_ZI(4)=ZTOFCEND                                                714
         AG_ZI(5)=ZTOFCEND                                                714
         AG_ZI(6)=ZTIFCFLANGEBEGIN                                        714
         AG_ZI(7)=ZTIFCFLANGEBEGIN                                        714
         AG_ZI(8)=TPCG_DZENVELOP                                          714
         AG_RMN(1)=TIFCOR                                                 714
         AG_RMN(2)=TIFCOR                                                 714
         AG_RMN(3)=TIFCOR                                                 714
         AG_RMN(4)=TIFCOR                                                 714
         AG_RMN(5)=TIFCOR                                                 714
         AG_RMN(6)=TPCG_TIFCRF+TPCG_TIFCDRT                               714
         AG_RMN(7)=TPCG_TIFCRF+TPCG_TIFCDRT                               714
         AG_RMN(8)=TPCG_TIFCRF+TPCG_TIFCDRT                               714
         AG_RMX(1)=TOFCIR                                                 714
         AG_RMX(2)=TOFCIR                                                 714
         AG_RMX(3)=TPCG_WHEELOR1                                          714
         AG_RMX(4)=TPCG_WHEELOR1                                          714
         AG_RMX(5)=TPCG_WHEELOR                                           714
         AG_RMX(6)=TPCG_WHEELOR                                           714
         AG_RMX(7)=TPCG_WHEELOR                                           714
         AG_RMX(8)=TPCG_WHEELOR                                           714
         AG_TITLE = 'PCON'                                                714
         AG_EXNAME = 'SHAPE'                                              714
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           714
         CALL AXSHAPE                                                     714
C    *                                                                    714
C    Create TSAW :                                                        715
C    Call To Local Procedure TSAW                                         715
         ASSIGN 5360 TO BLOCK_TSAW                                        715
         GO TO  5350                                                      715
5360     CONTINUE                                                         715
C    *                                                                    715
C    Geant POSITION : TSAW ===> KONLY='MANY' <===                         716
         CALL AGSRESET                                                    716
         AG_KONLY = 'MANY'                                                716
         AG_TITLE = 'TSAW'                                                716
         AG_EXNAME = 'POSITION'                                           716
         AG_PARLIST = 'KONLY'                                             716
         CALL AXPOSITION                                                  716
C    *                                                                    716
5141  CALL AGSPOPB                                                        717
      GO TO BLOCK_TSWH, (5140 ,5150,5160)                                 717
C End of Local Procedure                                                  717
C ----------------------------------------------------------------------  718
C ------------ TSAW                                                       718
C Begin Local Procedure TSAW                                              719
C TSAW TPCSECTORANDWHEEL                                                  719
5350  CONTINUE                                                            719
         AG_TITLE='TSAW'                                                  719
         AG_MODULE=MODULE                                                 719
         AG_MARK='V00'                                                    719
         CALL AGSPUSH                                                     719
         AG_MODULE=MODULE                                                 719
C    *                                                                    719
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           720
         CALL AGSRESET                                                    720
         AG_TITLE = 'STANDARD'                                            720
         AG_EXNAME = 'MEDIUM'                                             720
         AG_PARLIST = 'NONE'                                              720
         CALL AXMEDIUM                                                    720
C    *                                                                    720
C    Geant ATTRIBUTE : TSAW ===> SEEN=1 COLO=KYELLOW <===                 720
         CALL AGSRESET                                                    721
         AG_SEEN = 1                                                      721
         AG_COLO = KYELLOW                                                721
         AG_TITLE = 'TSAW'                                                721
         AG_EXNAME = 'ATTRIBUTE'                                          721
         AG_PARLIST = 'SEEN_COLO'                                         721
         CALL AXATTRIBUTE                                                 721
C    *                                                                    721
C    Geant SHAPE : PCON ===> PHI1=-15 DPHI=30 NZ=2 ZI={204, TPCG_DZENVEL  721
C    OP-TPCG_MEMBTHK/2} RMN={48,48} RMX={208, 208} <===                   721
         CALL AGSRESET                                                    724
         AG_PHI1 = -15                                                    724
         AG_DPHI = 30                                                     724
         AG_NZ = 2                                                        724
         AG_ZI(1)=204                                                     724
         AG_ZI(2)=TPCG_DZENVELOP-TPCG_MEMBTHK/2                           724
         AG_RMN(1)=48                                                     724
         AG_RMN(2)=48                                                     724
         AG_RMX(1)=208                                                    724
         AG_RMX(2)=208                                                    724
         AG_TITLE = 'PCON'                                                724
         AG_EXNAME = 'SHAPE'                                              724
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           724
         CALL AXSHAPE                                                     724
C    *                                                                    724
         DX2 = 1.470                                                      724
         DX = 0                                                           726
C    use TECW bank                                                        727
         BPATH_TECW='TECW*'                                               727
         IDNUM_TECW(1)=0                                                  727
         TECW_SEC=1                                                       727
         CALL AGDGETP('TPCEGEO3A','*',-999,'TECW',NTECW_SEC,RE_TECW_SEC,  727
     *   BPATH_TECW,IDNUM_TECW,1,MAP_TECW,NAMES_TECW,BANK_TECW,LEN_TECW,  727
     *   LLEN_TECW,LINK_TECW,FLAG_TECW)                                   727
C    *                                                                    728
         INOUT=KINNER                                                     728
            GO TO 5373                                                    728
5371        INOUT=INOUT+1                                                 728
5373        IF(INOUT-(KOUTER).GT.0) GO TO 5372                            728
            MYCOUNT = 0                                                   728
C       use COOL bank                                                     730
            BPATH_COOL='COOL*'                                            730
            IDNUM_COOL(1)=0                                               730
            COOL_INOUT=INOUT                                              730
            CALL AGDGETP('TPCEGEO3A','*',-999,'COOL',NCOOL_INOUT,         730
     *      RE_COOL_INOUT,BPATH_COOL,IDNUM_COOL,1,MAP_COOL,NAMES_COOL,    730
     *      BANK_COOL,LEN_COOL,LLEN_COOL,LINK_COOL,FLAG_COOL)             730
C       use RIBS bank                                                     731
            BPATH_RIBS='RIBS*'                                            731
            IDNUM_RIBS(1)=0                                               731
            RIBS_INOUT=INOUT                                              731
            CALL AGDGETP('TPCEGEO3A','*',-999,'RIBS',NRIBS_INOUT,         731
     *      RE_RIBS_INOUT,BPATH_RIBS,IDNUM_RIBS,1,MAP_RIBS,NAMES_RIBS,    731
     *      BANK_RIBS,LEN_RIBS,LLEN_RIBS,LINK_RIBS,FLAG_RIBS)             731
C       use HOLZ bank                                                     732
            BPATH_HOLZ='HOLZ*'                                            732
            IDNUM_HOLZ(1)=0                                               732
            HOLZ_INOUT=INOUT                                              732
            CALL AGDGETP('TPCEGEO3A','*',-999,'HOLZ',NHOLZ_INOUT,         732
     *      RE_HOLZ_INOUT,BPATH_HOLZ,IDNUM_HOLZ,1,MAP_HOLZ,NAMES_HOLZ,    732
     *      BANK_HOLZ,LEN_HOLZ,LLEN_HOLZ,LINK_HOLZ,FLAG_HOLZ)             732
C       use TECW bank                                                     733
            BPATH_TECW='TECW*'                                            733
            IDNUM_TECW(1)=0                                               733
            TECW_SEC=(INOUT+1)                                            733
            CALL AGDGETP('TPCEGEO3A','*',-999,'TECW',NTECW_SEC,           733
     *      RE_TECW_SEC,BPATH_TECW,IDNUM_TECW,1,MAP_TECW,NAMES_TECW,      733
     *      BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)             733
            Z2 = ZWHEEL1                                                  734
            Z1 = Z2 - TECW_THICK                                          735
            Z = (Z1 + TPCG_MEMBTHK/2)/2                                   736
            DZ = (Z1 - TPCG_MEMBTHK/2)/2                                  737
            X1 = -TECW_HEIGHT/2                                           738
            X2 = TECW_HEIGHT/2                                            739
            Y1 = TECW_INWIDTH/2                                           740
            Y2 = TECW_OUWIDTH/2                                           741
            X0 = (Y1*X2-Y2*X1)/(X2-X1)                                    742
            DYDX = (Y2 - Y1)/(X2 - X1)                                    743
            ALPHA = 180./PI*ATAN(DYDX)                                    744
            R1 = Y1/DYDX                                                  745
            R2 = Y2/DYDX                                                  746
            DRSECTORSHIFT = TECW_RMIN - R1                                747
C       Check INOUT==KINNER                                               748
            IF (INOUT.EQ.KINNER) THEN                                     748
            XHOLES(0) = R2 - 1.988*INCH                                   749
C       *                                                                 750
            I=1                                                           750
               GO TO 5383                                                 750
5381           I=I+1                                                      750
5383           IF(I-(HOLZ_NUMBEROFROWS-1).GT.0) GO TO 5382                750
               XHOLES(I) = XHOLES(I-1) - 1.575*INCH                       751
            GO TO 5381                                                    752
5382        CONTINUE                                                      752
            ELSE                                                          753
C       *                                                                 754
            I=0                                                           754
               GO TO 5393                                                 754
5391           I=I+1                                                      754
5393           IF(I-(HOLZ_NUMBEROFROWS - 1).GT.0) GO TO 5392              754
               XHOLES(I) = R2 - (1.988 + 1.575*I)*INCH                    755
            GO TO 5391                                                    756
5392        CONTINUE                                                      756
            ENDIF                                                         757
C       Create TSAS :                                                     758
C       Call To Local Procedure TSAS                                      758
            ASSIGN 5410 TO BLOCK_TSAS                                     758
            GO TO  5400                                                   758
5410        CONTINUE                                                      758
C       *                                                                 758
C       Geant POSITION : TSAS ===> Z=TSAS_ZOFFSET KONLY='MANY' <===       759
            CALL AGSRESET                                                 759
            AG_Z = TSAS_ZOFFSET                                           759
            AG_KONLY = 'MANY'                                             759
            AG_TITLE = 'TSAS'                                             759
            AG_EXNAME = 'POSITION'                                        759
            AG_PARLIST = 'Z_KONLY'                                        759
            CALL AXPOSITION                                               759
C       *                                                                 759
C       Create TWAS :                                                     760
C       Call To Local Procedure TWAS                                      760
            ASSIGN 5430 TO BLOCK_TWAS                                     760
            GO TO  5420                                                   760
5430        CONTINUE                                                      760
C       *                                                                 760
C       Geant POSITION : TWAS ===> KONLY='MANY' <===                      761
            CALL AGSRESET                                                 761
            AG_KONLY = 'MANY'                                             761
            AG_TITLE = 'TWAS'                                             761
            AG_EXNAME = 'POSITION'                                        761
            AG_PARLIST = 'KONLY'                                          761
            CALL AXPOSITION                                               761
C       *                                                                 761
C       Check INOUT==0                                                    761
            IF (INOUT.EQ.0) THEN                                          762
C       Create TBRW :                                                     763
C       Call To Local Procedure TBRW                                      763
            ASSIGN 5450 TO BLOCK_TBRW                                     763
            GO TO  5440                                                   763
5450        CONTINUE                                                      763
C       *                                                                 763
C       Geant POSITION : TBRW ===> X=TPCG_WHEELR1 Z=ZWHEEL1+TPCG_WHEELTH  764
C       K/2 <===                                                          764
            CALL AGSRESET                                                 764
            AG_X = TPCG_WHEELR1                                           764
            AG_Z = ZWHEEL1+TPCG_WHEELTHK/2                                764
            AG_TITLE = 'TBRW'                                             764
            AG_EXNAME = 'POSITION'                                        764
            AG_PARLIST = 'X_Z'                                            764
            CALL AXPOSITION                                               764
C       *                                                                 764
            ENDIF                                                         764
            X1 = TPCG_WHEELR0*COS15                                       766
            X2 = TPCG_WHEELR2                                             767
            DZ = TPCG_WHEELRIBHEIGHT/2                                    768
            Z = ZWHEEL1 + DZ                                              769
            DX = (X2 - X1)/2                                              770
            DY = TPCG_WHEELTOTALRIBWIDTH/4                                771
            X = (X1 + X2)/2                                               772
            Y = (X *TAN15 - DY)*(2*INOUT-1)                               773
            MYDX=DY                                                       773
            MYDY=DX                                                       773
            MYDZ=DZ                                                       773
            MYALPH=-(1-2*INOUT)*15                                        774
C       Create TWRB :                                                     775
C       Call To Local Procedure TWRB                                      775
            ASSIGN 5470 TO BLOCK_TWRB                                     775
            GO TO  5460                                                   775
5470        CONTINUE                                                      775
C       *                                                                 775
C       Geant POSITION : TWRB ===> X=X Y=Y Z=Z ORT=YX-Z <===              776
            CALL AGSRESET                                                 776
            AG_X = X                                                      776
            AG_Y = Y                                                      776
            AG_Z = Z                                                      776
            AG_ALPHAN=AG_ALPHAN+1                                         776
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             776
            AG_TITLE = 'TWRB'                                             776
            AG_EXNAME = 'POSITION'                                        776
            AG_PARLIST = 'X_Y_Z_ORT'                                      776
            CALL AXPOSITION                                               776
C       *                                                                 776
            DZ = TPCG_WHEELTHK/2                                          776
            Z = ZWHEEL1 + DZ                                              778
            DY = TPCG_WHEELRIBWIDTH/4                                     779
            X = (X1 + X2)/2                                               780
            Y = (X *TAN15 - DY)*(2*INOUT-1)                               781
            MYDX=DY                                                       781
            MYDY=DX                                                       781
            MYDZ=DZ                                                       781
            MYALPH=-(1-2*INOUT)*15                                        782
C       Create TWRB :                                                     783
C       Call To Local Procedure TWRB                                      783
            ASSIGN 5480 TO BLOCK_TWRB                                     783
            GO TO  5460                                                   783
5480        CONTINUE                                                      783
C       *                                                                 783
C       Geant POSITION : TWRB ===> X=X Y=Y Z=Z ORT=YX-Z <===              784
            CALL AGSRESET                                                 784
            AG_X = X                                                      784
            AG_Y = Y                                                      784
            AG_Z = Z                                                      784
            AG_ALPHAN=AG_ALPHAN+1                                         784
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             784
            AG_TITLE = 'TWRB'                                             784
            AG_EXNAME = 'POSITION'                                        784
            AG_PARLIST = 'X_Y_Z_ORT'                                      784
            CALL AXPOSITION                                               784
C       *                                                                 784
         GO TO 5371                                                       785
5372     CONTINUE                                                         785
C    *                                                                    786
         ALPHA=-15                                                        786
            GOTO 5493                                                     786
5491        ALPHA=ALPHA+(30)                                              786
5493        IF((30)*(ALPHA-(15)).GT.0) GO TO 5492                         786
C       *                                                                 787
            J=0                                                           787
               GO TO 5503                                                 787
5501           J=J+1                                                      787
5503           IF(J-(3).GT.0) GO TO 5502                                  787
C          Check (J==0)                                                   788
               IF ((J.EQ.0)) THEN                                         788
               R = TPCG_WHEELR2 - 1.5/8.5*(TPCG_WHEELR2 - TPCG_WHEELR1)   789
               ENDIF                                                      790
C          Check (J==1)                                                   791
               IF ((J.EQ.1)) THEN                                         791
               R = TPCG_WHEELR2 - 6.5/8.5*(TPCG_WHEELR2 - TPCG_WHEELR1)   792
               ENDIF                                                      793
C          Check (J==2)                                                   794
               IF ((J.EQ.2)) THEN                                         794
               R = TPCG_WHEELR1 - 2.0/8.5*(TPCG_WHEELR1 - TPCG_WHEELR0)   795
               ENDIF                                                      796
C          Check (J==3)                                                   797
               IF ((J.EQ.3)) THEN                                         797
               R = TPCG_WHEELR1 - 7.0/8.5*(TPCG_WHEELR1 - TPCG_WHEELR0)   798
               ENDIF                                                      799
               X = R                                                      800
               Y = R *TAND(ALPHA)                                         801
C          Geant POSITION : TBRW ===> X=X Y=Y Z=ZWHEEL1+TPCG_WHEELTHK/2   802
C          ALPHAZ=ALPHA <===                                              802
               CALL AGSRESET                                              802
               AG_X = X                                                   802
               AG_Y = Y                                                   802
               AG_Z = ZWHEEL1+TPCG_WHEELTHK/2                             802
               AG_ALPHAN=AG_ALPHAN+1                                      802
               AG_ALPHAK(AG_ALPHAN)=3                                     802
               AG_ALPHAL(AG_ALPHAN)=ALPHA                                 802
               AG_TITLE = 'TBRW'                                          802
               AG_EXNAME = 'POSITION'                                     802
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                802
               CALL AXPOSITION                                            802
C          *                                                              802
               MYCOUNT=MYCOUNT+1                                          802
            GO TO 5501                                                    804
5502        CONTINUE                                                      804
            QWE(1) = -QWE(1)                                              805
            QWE(2) = -QWE(2)                                              806
         GO TO 5491                                                       807
5492     CONTINUE                                                         807
C    Create TWBT :                                                        808
C    Call To Local Procedure TWBT                                         808
         ASSIGN 5520 TO BLOCK_TWBT                                        808
         GO TO  5510                                                      808
5520     CONTINUE                                                         808
C    *                                                                    808
C    Geant POSITION : TWBT ===> inherited arguments used <===             809
         CALL AGSRESET                                                    809
         AG_TITLE = 'TWBT'                                                809
         AG_EXNAME = 'POSITION'                                           809
         AG_PARLIST = 'NONE'                                              809
         CALL AXPOSITION                                                  809
C    *                                                                    809
C    Create TWRI :                                                        810
C    Call To Local Procedure TWRI                                         810
         ASSIGN 5540 TO BLOCK_TWRI                                        810
         GO TO  5530                                                      810
5540     CONTINUE                                                         810
C    *                                                                    810
C    Geant POSITION : TWRI ===> KONLY='MANY' <===                         811
         CALL AGSRESET                                                    811
         AG_KONLY = 'MANY'                                                811
         AG_TITLE = 'TWRI'                                                811
         AG_EXNAME = 'POSITION'                                           811
         AG_PARLIST = 'KONLY'                                             811
         CALL AXPOSITION                                                  811
C    *                                                                    811
C    Create TWTR :                                                        812
C    Call To Local Procedure TWTR                                         812
         ASSIGN 5560 TO BLOCK_TWTR                                        812
         GO TO  5550                                                      812
5560     CONTINUE                                                         812
C    *                                                                    812
C    Geant POSITION : TWTR ===> inherited arguments used <===             813
         CALL AGSRESET                                                    813
         AG_TITLE = 'TWTR'                                                813
         AG_EXNAME = 'POSITION'                                           813
         AG_PARLIST = 'NONE'                                              813
         CALL AXPOSITION                                                  813
C    *                                                                    813
C    Create TWMR :                                                        814
C    Call To Local Procedure TWMR                                         814
         ASSIGN 5580 TO BLOCK_TWMR                                        814
         GO TO  5570                                                      814
5580     CONTINUE                                                         814
C    *                                                                    814
C    Geant POSITION : TWMR ===> inherited arguments used <===             815
         CALL AGSRESET                                                    815
         AG_TITLE = 'TWMR'                                                815
         AG_EXNAME = 'POSITION'                                           815
         AG_PARLIST = 'NONE'                                              815
         CALL AXPOSITION                                                  815
C    *                                                                    815
C    Create TRDO :                                                        816
C    Call To Local Procedure TRDO                                         816
         ASSIGN 5600 TO BLOCK_TRDO                                        816
         GO TO  5590                                                      816
5600     CONTINUE                                                         816
C    *                                                                    816
C    Geant POSITION : TRDO ===> KONLY='MANY' <===                         817
         CALL AGSRESET                                                    817
         AG_KONLY = 'MANY'                                                817
         AG_TITLE = 'TRDO'                                                817
         AG_EXNAME = 'POSITION'                                           817
         AG_PARLIST = 'KONLY'                                             817
         CALL AXPOSITION                                                  817
C    *                                                                    817
         RCOOLINGTUBE = TPCG_WHEELR2 - TPCG_HEIGTUBE - 2*TPCG_DXRDO       817
C    *                                                                    819
         ICOO=0                                                           819
            GO TO 5613                                                    819
5611        ICOO=ICOO+1                                                   819
5613        IF(ICOO-(7).GT.0) GO TO 5612                                  819
            DZ = TPCG_HEIGTUBE*CM/2                                       820
            DY = TPCG_WIDTUBE *CM/2                                       821
            Z = RCOOLINGTUBE                                              822
            DX1 = (Z-DZ)*TAN15 - 2*TPCG_RDOCOOLINGDY                      823
            DX2 = (Z+DZ)*TAN15 - 2*TPCG_RDOCOOLINGDY                      824
            ISHAPE = KTRD1                                                824
            MYPAR(1)=DX1                                                  826
            MYPAR(2)=DX2                                                  826
            MYPAR(3)=DY                                                   826
            MYPAR(4)=DZ                                                   826
C       Create TCOO :                                                     827
C       Call To Local Procedure TCOO                                      827
            ASSIGN 5630 TO BLOCK_TCOO                                     827
            GO TO  5620                                                   827
5630        CONTINUE                                                      827
C       *                                                                 827
C       Geant POSITION : TCOO ===> X=Z Z=(TPCG_ZRDO-2*DY) ORT=YZX <===    828
            CALL AGSRESET                                                 828
            AG_X = Z                                                      828
            AG_Z = (TPCG_ZRDO-2*DY)                                       828
            AG_ALPHAN=AG_ALPHAN+1                                         828
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              828
            AG_TITLE = 'TCOO'                                             828
            AG_EXNAME = 'POSITION'                                        828
            AG_PARLIST = 'X_Z_ORT'                                        828
            CALL AXPOSITION                                               828
C       *                                                                 828
            RCOOLINGTUBE=RCOOLINGTUBE-DRDOCOOLING(ICOO)                   828
         GO TO 5611                                                       830
5612     CONTINUE                                                         830
         DZ = 4.5                                                         831
         DY = 4.5/2                                                       832
         Z = TPCG_ZRDO + TPCG_DZRDO + DZ                                  833
         X1 = TPCG_WHEELR0                                                834
         X2 = TPCG_WHEELR2                                                835
         DX =(X2 - X1)/2                                                  836
         X =(X1 + X2)/2                                                   837
C    *                                                                    838
         ICAB=0                                                           838
            GO TO 5643                                                    838
5641        ICAB=ICAB+1                                                   838
5643        IF(ICAB-(1).GT.0) GO TO 5642                                  838
            Y = - (X *TAN15 - DY) * (1 - 2*ICAB)                          839
            MYPAR(1)=DY                                                   840
            MYPAR(2)=DX                                                   840
            MYPAR(3)=DZ                                                   840
            MYPAR(4)=-15*(1-2*ICAB)                                       840
            MYPAR(5)=0                                                    840
            MYPAR(6)=0                                                    840
C       Create TCAB :                                                     841
C       Call To Local Procedure TCAB                                      841
            ASSIGN 5660 TO BLOCK_TCAB                                     841
            GO TO  5650                                                   841
5660        CONTINUE                                                      841
C       *                                                                 841
C       Geant POSITION : TCAB ===> X=X Y=Y Z=Z ORT=YX-Z <===              842
            CALL AGSRESET                                                 842
            AG_X = X                                                      842
            AG_Y = Y                                                      842
            AG_Z = Z                                                      842
            AG_ALPHAN=AG_ALPHAN+1                                         842
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             842
            AG_TITLE = 'TCAB'                                             842
            AG_EXNAME = 'POSITION'                                        842
            AG_PARLIST = 'X_Y_Z_ORT'                                      842
            CALL AXPOSITION                                               842
C       *                                                                 842
         GO TO 5641                                                       843
5642     CONTINUE                                                         843
5351  CALL AGSPOPB                                                        844
      GO TO BLOCK_TSAW, (5350 ,5360)                                      844
C End of Local Procedure                                                  844
C ----------------------------------------------------------------------  845
C ------------ TSAS                                                       845
C Begin Local Procedure TSAS                                              846
C TSAS TPCINNERSECTORASSEMBLY & TPCOUTERSECTORASSEMBLY TSAS AND TSA1      846
5400  CONTINUE                                                            846
         AG_TITLE='TSAS'                                                  846
         AG_MODULE=MODULE                                                 846
         AG_MARK='V00'                                                    846
         CALL AGSPUSH                                                     846
         AG_MODULE=MODULE                                                 846
C    *                                                                    846
C    Geant SHAPE : PGON ===> PHI1=-15 DPHI=30 NPDIV=1 NZ=2, ZI={ZZZSA(1)  849
C    ,ZZZSA(2)} RMN={RMNSA(INOUT),RMNSA(INOUT)}, RMX={RMXSA(INOUT),RMXSA  849
C    (INOUT)} <===                                                        849
         CALL AGSRESET                                                    849
         AG_PHI1 = -15                                                    849
         AG_DPHI = 30                                                     849
         AG_NPDIV = 1                                                     849
         AG_NZ = 2                                                        849
         AG_ZI(1)=ZZZSA(1)                                                849
         AG_ZI(2)=ZZZSA(2)                                                849
         AG_RMN(1)=RMNSA(INOUT)                                           849
         AG_RMN(2)=RMNSA(INOUT)                                           849
         AG_RMX(1)=RMXSA(INOUT)                                           849
         AG_RMX(2)=RMXSA(INOUT)                                           849
         AG_TITLE = 'PGON'                                                849
         AG_EXNAME = 'SHAPE'                                              849
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     849
         CALL AXSHAPE                                                     849
C    *                                                                    849
C    Geant ATTRIBUTE : TSAS ===> SEEN=0 <===                              849
         CALL AGSRESET                                                    850
         AG_SEEN = 0                                                      850
         AG_TITLE = 'TSAS'                                                850
         AG_EXNAME = 'ATTRIBUTE'                                          850
         AG_PARLIST = 'SEEN'                                              850
         CALL AXATTRIBUTE                                                 850
C    *                                                                    850
C    Create TALS :                                                        851
C    Call To Local Procedure TALS                                         851
         ASSIGN 5680 TO BLOCK_TALS                                        851
         GO TO  5670                                                      851
5680     CONTINUE                                                         851
C    *                                                                    851
C    Geant POSITION : TALS ===> inherited arguments used <===             852
         CALL AGSRESET                                                    852
         AG_TITLE = 'TALS'                                                852
         AG_EXNAME = 'POSITION'                                           852
         AG_PARLIST = 'NONE'                                              852
         CALL AXPOSITION                                                  852
C    *                                                                    852
C    Create TSGT :                                                        853
C    Call To Local Procedure TSGT                                         853
         ASSIGN 5700 TO BLOCK_TSGT                                        853
         GO TO  5690                                                      853
5700     CONTINUE                                                         853
C    *                                                                    853
C    Geant POSITION : TSGT ===> Z=TSGT_ZOFFSET <===                       854
         CALL AGSRESET                                                    854
         AG_Z = TSGT_ZOFFSET                                              854
         AG_TITLE = 'TSGT'                                                854
         AG_EXNAME = 'POSITION'                                           854
         AG_PARLIST = 'Z'                                                 854
         CALL AXPOSITION                                                  854
C    *                                                                    854
         DY =(R2 - R1)/2                                                  854
         Y =(R2 + R1)/2                                                   856
         DX = TECW_WIDTHRIB/2                                             857
         DZ =(TECW_THICK - TPCG_PADPLANETHICKNESS - TECW_THICKAL)/2       858
         Z = Z2 - DZ                                                      859
         DXW = TPCG_WIREMOUNTWIDTH/COS15/2                                860
         DZW = TPCG_WIREMOUNTHEIGHT/2                                     861
         XW = 0                                                           862
C    *                                                                    863
         IRIB=0                                                           863
            GO TO 5713                                                    863
5711        IRIB=IRIB+1                                                   863
5713        IF(IRIB-(1).GT.0) GO TO 5712                                  863
            ISHAPE = KPARA                                                863
            MYPAR(1)=DX                                                   865
            MYPAR(2)=DY                                                   865
            MYPAR(3)=DZ                                                   865
            MYPAR(4)=-(1-2*IRIB)*ALPHA                                    865
C       Create TRIB :                                                     866
C       Call To Local Procedure TRIB                                      866
            ASSIGN 5730 TO BLOCK_TRIB                                     866
            GO TO  5720                                                   866
5730        CONTINUE                                                      866
C       *                                                                 866
            X = ((Y1 + Y2)/2 - DX)*(1-2*IRIB)                             867
C       Geant POSITION : TRIB ===> X=Y Y=-X Z=Z ORT=YX-Z <===             868
            CALL AGSRESET                                                 868
            AG_X = Y                                                      868
            AG_Y = -X                                                     868
            AG_Z = Z                                                      868
            AG_ALPHAN=AG_ALPHAN+1                                         868
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             868
            AG_TITLE = 'TRIB'                                             868
            AG_EXNAME = 'POSITION'                                        868
            AG_PARLIST = 'X_Y_Z_ORT'                                      868
            CALL AXPOSITION                                               868
C       *                                                                 868
            XW = ((Y1 + Y2)/2 - DXW)*(1-2*IRIB)                           868
            MYPAR(1)=DXW                                                  870
            MYPAR(2)=DY                                                   870
            MYPAR(3)=DZW                                                  870
            MYPAR(4)=-(1-2*IRIB)*ALPHA                                    870
            MYPAR(5)=0                                                    870
            MYPAR(6)=0                                                    870
C       Create TWIR :                                                     871
C       Call To Local Procedure TWIR                                      871
            ASSIGN 5750 TO BLOCK_TWIR                                     871
            GO TO  5740                                                   871
5750        CONTINUE                                                      871
C       *                                                                 871
            ZW = Z1 - DZW/2                                               872
C       Geant POSITION : TWIR ===> X=Y Y=-XW Z=ZW ORT=YX-Z <===           873
            CALL AGSRESET                                                 873
            AG_X = Y                                                      873
            AG_Y = -XW                                                    873
            AG_Z = ZW                                                     873
            AG_ALPHAN=AG_ALPHAN+1                                         873
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             873
            AG_TITLE = 'TWIR'                                             873
            AG_EXNAME = 'POSITION'                                        873
            AG_PARLIST = 'X_Y_Z_ORT'                                      873
            CALL AXPOSITION                                               873
C       *                                                                 873
         GO TO 5711                                                       874
5712     CONTINUE                                                         874
C    *                                                                    875
         IRIB=0                                                           875
            GO TO 5763                                                    875
5761        IRIB=IRIB+1                                                   875
5763        IF(IRIB-(TECW_NORIBS-1).GT.0) GO TO 5762                      875
            R = R2 - RIBS_X (IRIB+1) * INCH                               876
            Y = RIBS_Y (IRIB+1) * INCH                                    877
            DR = RIBS_DX (IRIB+1) * INCH                                  878
            DY = RIBS_DY (IRIB+1) * INCH                                  879
C       Check DY<1.E-7                                                    880
            IF (DY.LT.1.E-7) THEN                                         880
            DX2 = R *DYDX - TECW_WIDTHRIB - 2*DXW                         880
C       TECW_WIDTHLIP                                                     881
            DX1 = (R - DR)*DYDX - TECW_WIDTHRIB - 2*DXW                   881
C       TECW_WIDTHLIP                                                     882
            XC = R - TECW_ZSTEPRIB/2                                      883
            ISHAPE = KTRD1                                                883
            MYPAR(1)=DX1                                                  885
            MYPAR(2)=DX2                                                  885
            MYPAR(3)=DZ                                                   885
            MYPAR(4)=DR/2                                                 885
C       Create TRIB :                                                     886
C       Call To Local Procedure TRIB                                      886
            ASSIGN 5770 TO BLOCK_TRIB                                     886
            GO TO  5720                                                   886
5770        CONTINUE                                                      886
C       *                                                                 886
C       Geant POSITION : TRIB ===> X=R-DR/2 Y=0 Z=Z2-DZ ORT=YZX <===      887
            CALL AGSRESET                                                 887
            AG_X = R-DR/2                                                 887
            AG_Y = 0                                                      887
            AG_Z = Z2-DZ                                                  887
            AG_ALPHAN=AG_ALPHAN+1                                         887
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              887
            AG_TITLE = 'TRIB'                                             887
            AG_EXNAME = 'POSITION'                                        887
            AG_PARLIST = 'X_Y_Z_ORT'                                      887
            CALL AXPOSITION                                               887
C       *                                                                 887
            ELSE                                                          887
            ISHAPE = KBBOX                                                888
            MYPAR(1)=DR/2                                                 890
            MYPAR(2)=DY/2                                                 890
            MYPAR(3)=DZ                                                   890
C       Create TRIB :                                                     891
C       Call To Local Procedure TRIB                                      891
            ASSIGN 5780 TO BLOCK_TRIB                                     891
            GO TO  5720                                                   891
5780        CONTINUE                                                      891
C       *                                                                 891
C       Geant POSITION : TRIB ===> X=R Y=Y Z=Z2-DZ <===                   892
            CALL AGSRESET                                                 892
            AG_X = R                                                      892
            AG_Y = Y                                                      892
            AG_Z = Z2-DZ                                                  892
            AG_TITLE = 'TRIB'                                             892
            AG_EXNAME = 'POSITION'                                        892
            AG_PARLIST = 'X_Y_Z'                                          892
            CALL AXPOSITION                                               892
C       *                                                                 892
            ENDIF                                                         892
         GO TO 5761                                                       894
5762     CONTINUE                                                         894
5401  CALL AGSPOPB                                                        895
      GO TO BLOCK_TSAS, (5400 ,5410)                                      895
C End of Local Procedure                                                  895
C ----------------------------------------------------------------------  896
C ------------ TWAS                                                       896
C Begin Local Procedure TWAS                                              897
C TWAS TPCWHEELINNERASSEMBLY & TPCWHEELOUTERASSEMBLY TWAS AND TWA1        897
5420  CONTINUE                                                            897
         AG_TITLE='TWAS'                                                  897
         AG_MODULE=MODULE                                                 897
         AG_MARK='V00'                                                    897
         CALL AGSPUSH                                                     897
         AG_MODULE=MODULE                                                 897
C    *                                                                    897
C    Geant ATTRIBUTE : TWAS ===> SEEN=0 <===                              898
         CALL AGSRESET                                                    898
         AG_SEEN = 0                                                      898
         AG_TITLE = 'TWAS'                                                898
         AG_EXNAME = 'ATTRIBUTE'                                          898
         AG_PARLIST = 'SEEN'                                              898
         CALL AXATTRIBUTE                                                 898
C    *                                                                    898
C    Geant SHAPE : PGON ===> PHI1=-15 DPHI=30 NPDIV=1 NZ=2, ZI={ZZZWA(1)  898
C    ,ZZZWA(2)} RMN={RMNWA(INOUT),RMNWA(INOUT)}, RMX={RMXWA(INOUT),RMXWA  898
C    (INOUT)} <===                                                        898
         CALL AGSRESET                                                    901
         AG_PHI1 = -15                                                    901
         AG_DPHI = 30                                                     901
         AG_NPDIV = 1                                                     901
         AG_NZ = 2                                                        901
         AG_ZI(1)=ZZZWA(1)                                                901
         AG_ZI(2)=ZZZWA(2)                                                901
         AG_RMN(1)=RMNWA(INOUT)                                           901
         AG_RMN(2)=RMNWA(INOUT)                                           901
         AG_RMX(1)=RMXWA(INOUT)                                           901
         AG_RMX(2)=RMXWA(INOUT)                                           901
         AG_TITLE = 'PGON'                                                901
         AG_EXNAME = 'SHAPE'                                              901
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     901
         CALL AXSHAPE                                                     901
C    *                                                                    901
         DX = TPCG_WIDTUBE*CM/2                                           901
         DZ = DEPTHIOF(INOUT + 1)/2                                       903
         DY = HEIGHTIOF(INOUT + 1)/2                                      904
         Y1 = - DY                                                        905
         Y2 = DY                                                          906
         X1 = WIDTH1IOF(INOUT + 1)/2 - DX                                 907
         X2 = WIDTH2IOF(INOUT + 1)/2 - DX                                 908
         X0 =(Y1*X2 - Y2*X1)/(Y2 - Y1)                                    909
         DYDX =(X2 - X1)/(Y2 - Y1)                                        910
         BETA = 180./PI*ATAN(DYDX)                                        911
C    Check (INOUT==KOUTER)                                                912
         IF ((INOUT.EQ.KOUTER)) THEN                                      912
         DY=DY-0.7                                                        913
         ENDIF                                                            914
         Y = XHOLES(0) - DY                                               915
         X =(X1 + X2)/2                                                   916
C    *                                                                    917
         ICOO=0                                                           917
            GO TO 5793                                                    917
5791        ICOO=ICOO+1                                                   917
5793        IF(ICOO-(1).GT.0) GO TO 5792                                  917
            MYPAR(1)=DX                                                   918
            MYPAR(2)=DY                                                   918
            MYPAR(3)=DZ                                                   918
            MYPAR(4)=-(1-2*ICOO)*BETA                                     918
            MYPAR(5)=0                                                    918
            MYPAR(6)=0                                                    918
            ISHAPE = KPARA                                                918
C       Create TCOO :                                                     920
C       Call To Local Procedure TCOO                                      920
            ASSIGN 5800 TO BLOCK_TCOO                                     920
            GO TO  5620                                                   920
5800        CONTINUE                                                      920
C       *                                                                 920
C       Geant POSITION : TCOO ===> X=Y Y=X*(2*ICOO-1) Z=ZWHEEL1+DZ ORT=Y  921
C       X-Z <===                                                          921
            CALL AGSRESET                                                 921
            AG_X = Y                                                      921
            AG_Y = X*(2*ICOO-1)                                           921
            AG_Z = ZWHEEL1+DZ                                             921
            AG_ALPHAN=AG_ALPHAN+1                                         921
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             921
            AG_TITLE = 'TCOO'                                             921
            AG_EXNAME = 'POSITION'                                        921
            AG_PARLIST = 'X_Y_Z_ORT'                                      921
            CALL AXPOSITION                                               921
C       *                                                                 921
         GO TO 5791                                                       922
5792     CONTINUE                                                         922
C    *                                                                    923
         ICOO=0                                                           923
            GO TO 5813                                                    923
5811        ICOO=ICOO+1                                                   923
5813        IF(ICOO-(HOLZ_NUMBEROFROWS - 1).GT.0) GO TO 5812              923
            X = XHOLES(ICOO)                                              923
C       // CM                                                             924
            DX = TPCG_HEIGTUBE*CM/2                                       925
            DZ = TPCG_WIDTUBE*CM/2                                        926
            DY = COOL_TUBELENGTH( ICOO+1 )*INCH / 2                       927
C       Check INOUT==KINNER                                               928
            IF (INOUT.EQ.KINNER) THEN                                     928
            DY2 = DY + 2*DYDX*DX                                          929
            DY1 = DY                                                      929
            ELSE                                                          931
            DY2 = DY                                                      932
            DY1 = DY - 2*DYDX*DX                                          933
            ENDIF                                                         934
            ISHAPE = KTRD1                                                934
            MYPAR(1)=DY1                                                  936
            MYPAR(2)=DY2                                                  936
            MYPAR(3)=DZ                                                   936
            MYPAR(4)=DX                                                   936
            MYPAR(11) = X-DX-TFEE_ASSEMBLYTHICKNESS/2+DRSECTORSHIFT       937
            MYPAR(13) = ZWHEEL1+DZ                                        938
C       Create TCOO :                                                     939
C       Call To Local Procedure TCOO                                      939
            ASSIGN 5820 TO BLOCK_TCOO                                     939
            GO TO  5620                                                   939
5820        CONTINUE                                                      939
C       *                                                                 939
C       Geant POSITION : TCOO ===> X=MYPAR(11) Z=MYPAR(13) ORT=YZX <===   940
            CALL AGSRESET                                                 940
            AG_X = MYPAR(11)                                              940
            AG_Z = MYPAR(13)                                              940
            AG_ALPHAN=AG_ALPHAN+1                                         940
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              940
            AG_TITLE = 'TCOO'                                             940
            AG_EXNAME = 'POSITION'                                        940
            AG_PARLIST = 'X_Z_ORT'                                        940
            CALL AXPOSITION                                               940
C       *                                                                 940
C       *                                                                 940
            JCOO=0                                                        941
               GO TO 5833                                                 941
5831           JCOO=JCOO+1                                                941
5833           IF(JCOO-(HOLZ_NUMBERPERROW(ICOO+1) - 1).GT.0) GO TO 5832   941
               Y = HOLZ_YHOLES( 9*ICOO + JCOO + 1 )*INCH                  942
C          Create FEEA :                                                  943
C          Call To Local Procedure FEEA                                   943
               ASSIGN 5850 TO BLOCK_FEEA                                  943
               GO TO  5840                                                943
5850           CONTINUE                                                   943
C          *                                                              943
C          Geant POSITION : FEEA ===> X=X+TFEE_ASSEMBLYTHICKNESS/2+DRSEC  945
C          TORSHIFT Y=Y Z=ZWHEEL1+2*DZ+TFEE_RIBDZ KONLY='MANY' <===       945
               CALL AGSRESET                                              945
               AG_X = X+TFEE_ASSEMBLYTHICKNESS/2+DRSECTORSHIFT            945
               AG_Y = Y                                                   945
               AG_Z = ZWHEEL1+2*DZ+TFEE_RIBDZ                             945
               AG_KONLY = 'MANY'                                          945
               AG_TITLE = 'FEEA'                                          945
               AG_EXNAME = 'POSITION'                                     945
               AG_PARLIST = 'X_Y_Z_KONLY'                                 945
               CALL AXPOSITION                                            945
C          *                                                              945
            GO TO 5831                                                    946
5832        CONTINUE                                                      946
         GO TO 5811                                                       947
5812     CONTINUE                                                         947
5421  CALL AGSPOPB                                                        948
      GO TO BLOCK_TWAS, (5420 ,5430)                                      948
C End of Local Procedure                                                  948
C ----------------------------------------------------------------------  949
C ------------ FEEA                                                       949
C Begin Local Procedure FEEA                                              950
C FEEA TGEOVOLUMEASSEMBLY(FEE)                                            950
5840  CONTINUE                                                            950
         AG_TITLE='FEEA'                                                  950
         AG_MODULE=MODULE                                                 950
         AG_MARK='V00'                                                    950
         CALL AGSPUSH                                                     950
         AG_MODULE=MODULE                                                 950
C    *                                                                    950
C    Geant ATTRIBUTE : FEEA ===> SEEN=1 COLO=KGREEN <===                  951
         CALL AGSRESET                                                    951
         AG_SEEN = 1                                                      951
         AG_COLO = KGREEN                                                 951
         AG_TITLE = 'FEEA'                                                951
         AG_EXNAME = 'ATTRIBUTE'                                          951
         AG_PARLIST = 'SEEN_COLO'                                         951
         CALL AXATTRIBUTE                                                 951
C    *                                                                    951
C    Geant SHAPE : BOX ===> DX=TFEE_ASS(1) DY=TFEE_ASS(2), DZ=TFEE_ASS(3  951
C    ) <===                                                               951
         CALL AGSRESET                                                    953
         AG_DX = TFEE_ASS(1)                                              953
         AG_DY = TFEE_ASS(2)                                              953
         AG_DZ = TFEE_ASS(3)                                              953
         AG_TITLE = 'BOX'                                                 953
         AG_EXNAME = 'SHAPE'                                              953
         AG_PARLIST = 'DX_DY_DZ'                                          953
         CALL AXSHAPE                                                     953
C    *                                                                    953
C    Geant ATTRIBUTE : FEEA ===> SEEN=1 COLO=42 <===                      953
         CALL AGSRESET                                                    954
         AG_SEEN = 1                                                      954
         AG_COLO = 42                                                     954
         AG_TITLE = 'FEEA'                                                954
         AG_EXNAME = 'ATTRIBUTE'                                          954
         AG_PARLIST = 'SEEN_COLO'                                         954
         CALL AXATTRIBUTE                                                 954
C    *                                                                    954
C    Create FEEP :                                                        955
C    Call To Local Procedure FEEP                                         955
         ASSIGN 5870 TO BLOCK_FEEP                                        955
         GO TO  5860                                                      955
5870     CONTINUE                                                         955
C    *                                                                    955
C    Geant POSITION : FEEP ===> inherited arguments used <===             956
         CALL AGSRESET                                                    956
         AG_TITLE = 'FEEP'                                                956
         AG_EXNAME = 'POSITION'                                           956
         AG_PARLIST = 'NONE'                                              956
         CALL AXPOSITION                                                  956
C    *                                                                    956
C    Create FEER :                                                        957
C    Call To Local Procedure FEER                                         957
         ASSIGN 5890 TO BLOCK_FEER                                        957
         GO TO  5880                                                      957
5890     CONTINUE                                                         957
C    *                                                                    957
C    Geant POSITION : FEER ===> X=-TFEE_RIBDX-TFEE_CARDDX <===            958
         CALL AGSRESET                                                    958
         AG_X = -TFEE_RIBDX-TFEE_CARDDX                                   958
         AG_TITLE = 'FEER'                                                958
         AG_EXNAME = 'POSITION'                                           958
         AG_PARLIST = 'X'                                                 958
         CALL AXPOSITION                                                  958
C    *                                                                    958
C    Create FEEI :                                                        959
C    Call To Local Procedure FEEI                                         959
         ASSIGN 5910 TO BLOCK_FEEI                                        959
         GO TO  5900                                                      959
5910     CONTINUE                                                         959
C    *                                                                    959
C    Geant POSITION : FEEI ===> X=2*TFEE_CARDDX Z=0.5 <===                960
         CALL AGSRESET                                                    960
         AG_X = 2*TFEE_CARDDX                                             960
         AG_Z = 0.5                                                       960
         AG_TITLE = 'FEEI'                                                960
         AG_EXNAME = 'POSITION'                                           960
         AG_PARLIST = 'X_Z'                                               960
         CALL AXPOSITION                                                  960
C    *                                                                    960
5841  CALL AGSPOPB                                                        961
      GO TO BLOCK_FEEA, (5840 ,5850)                                      961
C End of Local Procedure                                                  961
C ----------------------------------------------------------------------  962
C ------------ FEEP                                                       962
C Begin Local Procedure FEEP                                              963
C FEEP FEEPLATE                                                           963
5860  CONTINUE                                                            963
         AG_TITLE='FEEP'                                                  963
         AG_MODULE=MODULE                                                 963
         AG_MARK='V00'                                                    963
         CALL AGSPUSH                                                     963
         AG_MODULE=MODULE                                                 963
C    *                                                                    963
C    Geant ATTRIBUTE : FEEP ===> SEEN=1 COLO=KRED <===                    964
         CALL AGSRESET                                                    964
         AG_SEEN = 1                                                      964
         AG_COLO = KRED                                                   964
         AG_TITLE = 'FEEP'                                                964
         AG_EXNAME = 'ATTRIBUTE'                                          964
         AG_PARLIST = 'SEEN_COLO'                                         964
         CALL AXATTRIBUTE                                                 964
C    *                                                                    964
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        964
         CALL AGSRESET                                                    965
         AG_TITLE = 'ALUMINIUM'                                           965
         AG_EXNAME = 'MATERIAL'                                           965
         AG_PARLIST = 'NONE'                                              965
         CALL AXMATERIAL                                                  965
C    *                                                                    965
C    Geant SHAPE : BOX ===> DX=TFEE_PLATEDX DY=TFEE_PLATEDY, DZ=TFEE_PLA  965
C    TEDZ <===                                                            965
         CALL AGSRESET                                                    967
         AG_DX = TFEE_PLATEDX                                             967
         AG_DY = TFEE_PLATEDY                                             967
         AG_DZ = TFEE_PLATEDZ                                             967
         AG_TITLE = 'BOX'                                                 967
         AG_EXNAME = 'SHAPE'                                              967
         AG_PARLIST = 'DX_DY_DZ'                                          967
         CALL AXSHAPE                                                     967
C    *                                                                    967
5861  CALL AGSPOPB                                                        968
      GO TO BLOCK_FEEP, (5860 ,5870)                                      968
C End of Local Procedure                                                  968
C ----------------------------------------------------------------------  969
C ------------ FEER                                                       969
C Begin Local Procedure FEER                                              970
C FEER FEERIB                                                             970
5880  CONTINUE                                                            970
         AG_TITLE='FEER'                                                  970
         AG_MODULE=MODULE                                                 970
         AG_MARK='V00'                                                    970
         CALL AGSPUSH                                                     970
         AG_MODULE=MODULE                                                 970
C    *                                                                    970
C    Geant ATTRIBUTE : FEER ===> SEEN=1 COLO=1 <===                       971
         CALL AGSRESET                                                    971
         AG_SEEN = 1                                                      971
         AG_COLO = 1                                                      971
         AG_TITLE = 'FEER'                                                971
         AG_EXNAME = 'ATTRIBUTE'                                          971
         AG_PARLIST = 'SEEN_COLO'                                         971
         CALL AXATTRIBUTE                                                 971
C    *                                                                    971
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        971
         CALL AGSRESET                                                    972
         AG_TITLE = 'ALUMINIUM'                                           972
         AG_EXNAME = 'MATERIAL'                                           972
         AG_PARLIST = 'NONE'                                              972
         CALL AXMATERIAL                                                  972
C    *                                                                    972
C    Geant SHAPE : BOX ===> DX=TFEE_RIBDX DY=TFEE_RIBDY DZ=TFEE_RIBDZ <=  972
C    ==                                                                   972
         CALL AGSRESET                                                    973
         AG_DX = TFEE_RIBDX                                               973
         AG_DY = TFEE_RIBDY                                               973
         AG_DZ = TFEE_RIBDZ                                               973
         AG_TITLE = 'BOX'                                                 973
         AG_EXNAME = 'SHAPE'                                              973
         AG_PARLIST = 'DX_DY_DZ'                                          973
         CALL AXSHAPE                                                     973
C    *                                                                    973
5881  CALL AGSPOPB                                                        974
      GO TO BLOCK_FEER, (5880 ,5890)                                      974
C End of Local Procedure                                                  974
C ----------------------------------------------------------------------  975
C ------------ FEEI                                                       975
C Begin Local Procedure FEEI                                              976
C FEEI FEEITSELF                                                          976
5900  CONTINUE                                                            976
         AG_TITLE='FEEI'                                                  976
         AG_MODULE=MODULE                                                 976
         AG_MARK='V00'                                                    976
         CALL AGSPUSH                                                     976
         AG_MODULE=MODULE                                                 976
C    *                                                                    976
C    Geant ATTRIBUTE : FEEI ===> SEEN=1 COLO=KVIOLET <===                 977
         CALL AGSRESET                                                    977
         AG_SEEN = 1                                                      977
         AG_COLO = KVIOLET                                                977
         AG_TITLE = 'FEEI'                                                977
         AG_EXNAME = 'ATTRIBUTE'                                          977
         AG_PARLIST = 'SEEN_COLO'                                         977
         CALL AXATTRIBUTE                                                 977
C    *                                                                    977
C    Geant MATERIAL : G10 ===> inherited arguments used <===              977
         CALL AGSRESET                                                    978
         AG_TITLE = 'G10'                                                 978
         AG_EXNAME = 'MATERIAL'                                           978
         AG_PARLIST = 'NONE'                                              978
         CALL AXMATERIAL                                                  978
C    *                                                                    978
C    Geant SHAPE : BOX ===> DX=TFEE_CARDDX DY=TFEE_CARDDY, DZ=TFEE_CARDD  978
C    Z <===                                                               978
         CALL AGSRESET                                                    980
         AG_DX = TFEE_CARDDX                                              980
         AG_DY = TFEE_CARDDY                                              980
         AG_DZ = TFEE_CARDDZ                                              980
         AG_TITLE = 'BOX'                                                 980
         AG_EXNAME = 'SHAPE'                                              980
         AG_PARLIST = 'DX_DY_DZ'                                          980
         CALL AXSHAPE                                                     980
C    *                                                                    980
5901  CALL AGSPOPB                                                        981
      GO TO BLOCK_FEEI, (5900 ,5910)                                      981
C End of Local Procedure                                                  981
C ----------------------------------------------------------------------  982
C ------------ TALS                                                       982
C Begin Local Procedure TALS                                              983
C TALS TPCSECTORALSUPPORT THE SECTOR G10 ALLIMINIUM SUPPORT               983
5670  CONTINUE                                                            983
         AG_TITLE='TALS'                                                  983
         AG_MODULE=MODULE                                                 983
         AG_MARK='V00'                                                    983
         CALL AGSPUSH                                                     983
         AG_MODULE=MODULE                                                 983
C    *                                                                    983
C    Geant ATTRIBUTE : TALS ===> SEEN=1 COLO=KYELLOW <===                 984
         CALL AGSRESET                                                    984
         AG_SEEN = 1                                                      984
         AG_COLO = KYELLOW                                                984
         AG_TITLE = 'TALS'                                                984
         AG_EXNAME = 'ATTRIBUTE'                                          984
         AG_PARLIST = 'SEEN_COLO'                                         984
         CALL AXATTRIBUTE                                                 984
C    *                                                                    984
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        984
         CALL AGSRESET                                                    985
         AG_TITLE = 'ALUMINIUM'                                           985
         AG_EXNAME = 'MATERIAL'                                           985
         AG_PARLIST = 'NONE'                                              985
         CALL AXMATERIAL                                                  985
C    *                                                                    985
C    Geant SHAPE : PGON ===> PHI1=-ALPHA DPHI=2*ALPHA NPDIV=1, NZ=2 ZI={  985
C    Z1+TPCG_PADPLANETHICKNESS,Z1+TPCG_PADPLANETHICKNESS+TECW_THICKAL},   985
C    RMN={R1,R1}, RMX={R2,R2} <===                                        985
         CALL AGSRESET                                                    989
         AG_PHI1 = -ALPHA                                                 989
         AG_DPHI = 2*ALPHA                                                989
         AG_NPDIV = 1                                                     989
         AG_NZ = 2                                                        989
         AG_ZI(1)=Z1+TPCG_PADPLANETHICKNESS                               989
         AG_ZI(2)=Z1+TPCG_PADPLANETHICKNESS+TECW_THICKAL                  989
         AG_RMN(1)=R1                                                     989
         AG_RMN(2)=R1                                                     989
         AG_RMX(1)=R2                                                     989
         AG_RMX(2)=R2                                                     989
         AG_TITLE = 'PGON'                                                989
         AG_EXNAME = 'SHAPE'                                              989
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     989
         CALL AXSHAPE                                                     989
C    *                                                                    989
         MYPAR(1)= TECW_HOLEDX                                            989
         MYPAR(2)= TECW_HOLEDY                                            991
         MYPAR(3)= TECW_THICKAL/2                                         992
         MYPAR(13)= ZWHEEL1 - TECW_THICK + TPCG_PADPLANETHICKNESS +       993
     *   TECW_THICKAL/2                                                   993
C    *                                                                    994
         ITALS=0                                                          994
            GO TO 5923                                                    994
5921        ITALS=ITALS+1                                                 994
5923        IF(ITALS-(HOLZ_NUMBEROFROWS - 1).GT.0) GO TO 5922             994
            MYPAR(11) = XHOLES(ITALS)                                     994
C       // CM                                                             995
C       *                                                                 996
            JTALS=0                                                       996
               GO TO 5933                                                 996
5931           JTALS=JTALS+1                                              996
5933           IF(JTALS-(HOLZ_NUMBERPERROW( ITALS+1 ) - 1).GT.0) GO TO    996
     *         5932                                                       996
               MYPAR(12) = HOLZ_YHOLES( 9*ITALS+JTALS+1) *INCH            997
C          Create THOL :                                                  998
C          Call To Local Procedure THOL                                   998
               ASSIGN 5950 TO BLOCK_THOL                                  998
               GO TO  5940                                                998
5950           CONTINUE                                                   998
C          *                                                              998
C          Geant POSITION : THOL ===> X=MYPAR(11) Y=MYPAR(12) Z=MYPAR(13  999
C          ) <===                                                         999
               CALL AGSRESET                                              999
               AG_X = MYPAR(11)                                           999
               AG_Y = MYPAR(12)                                           999
               AG_Z = MYPAR(13)                                           999
               AG_TITLE = 'THOL'                                          999
               AG_EXNAME = 'POSITION'                                     999
               AG_PARLIST = 'X_Y_Z'                                       999
               CALL AXPOSITION                                            999
C          *                                                              999
            GO TO 5931                                                    1000
5932        CONTINUE                                                      1000
         GO TO 5921                                                       1001
5922     CONTINUE                                                         1001
5671  CALL AGSPOPB                                                        1002
      GO TO BLOCK_TALS, (5670 ,5680)                                      1002
C End of Local Procedure                                                  1002
C ----------------------------------------------------------------------  1003
C ------------ TSGT                                                       1003
C Begin Local Procedure TSGT                                              1004
C TSGT TPCSECTORG10 THE SECTOR G10                                        1004
5690  CONTINUE                                                            1004
         AG_TITLE='TSGT'                                                  1004
         AG_MODULE=MODULE                                                 1004
         AG_MARK='V00'                                                    1004
         CALL AGSPUSH                                                     1004
         AG_MODULE=MODULE                                                 1004
C    *                                                                    1004
C    Geant ATTRIBUTE : TSGT ===> SEEN=1 COLO=KBLUE <===                   1005
         CALL AGSRESET                                                    1005
         AG_SEEN = 1                                                      1005
         AG_COLO = KBLUE                                                  1005
         AG_TITLE = 'TSGT'                                                1005
         AG_EXNAME = 'ATTRIBUTE'                                          1005
         AG_PARLIST = 'SEEN_COLO'                                         1005
         CALL AXATTRIBUTE                                                 1005
C    *                                                                    1005
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1005
         CALL AGSRESET                                                    1006
         AG_TITLE = 'G10'                                                 1006
         AG_EXNAME = 'MATERIAL'                                           1006
         AG_PARLIST = 'NONE'                                              1006
         CALL AXMATERIAL                                                  1006
C    *                                                                    1006
C    Geant SHAPE : PGON ===> PHI1=-ALPHA DPHI=2*ALPHA NPDIV=1, NZ=2 ZI={  1006
C    Z1,Z1+TPCG_PADPLANETHICKNESS} RMN={R1, R1} RMX={R2,R2} <===          1006
         CALL AGSRESET                                                    1009
         AG_PHI1 = -ALPHA                                                 1009
         AG_DPHI = 2*ALPHA                                                1009
         AG_NPDIV = 1                                                     1009
         AG_NZ = 2                                                        1009
         AG_ZI(1)=Z1                                                      1009
         AG_ZI(2)=Z1+TPCG_PADPLANETHICKNESS                               1009
         AG_RMN(1)=R1                                                     1009
         AG_RMN(2)=R1                                                     1009
         AG_RMX(1)=R2                                                     1009
         AG_RMX(2)=R2                                                     1009
         AG_TITLE = 'PGON'                                                1009
         AG_EXNAME = 'SHAPE'                                              1009
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1009
         CALL AXSHAPE                                                     1009
C    *                                                                    1009
5691  CALL AGSPOPB                                                        1010
      GO TO BLOCK_TSGT, (5690 ,5700)                                      1010
C End of Local Procedure                                                  1010
C ----------------------------------------------------------------------  1011
C ------------ TWBT                                                       1011
C Begin Local Procedure TWBT                                              1012
C TWBT //WHEELBOTTOM                                                      1012
5510  CONTINUE                                                            1012
         AG_TITLE='TWBT'                                                  1012
         AG_MODULE=MODULE                                                 1012
         AG_MARK='V00'                                                    1012
         CALL AGSPUSH                                                     1012
         AG_MODULE=MODULE                                                 1012
C    *                                                                    1012
C    Geant ATTRIBUTE : TWBT ===> SEEN=1 COLO=KGREEN <===                  1013
         CALL AGSRESET                                                    1013
         AG_SEEN = 1                                                      1013
         AG_COLO = KGREEN                                                 1013
         AG_TITLE = 'TWBT'                                                1013
         AG_EXNAME = 'ATTRIBUTE'                                          1013
         AG_PARLIST = 'SEEN_COLO'                                         1013
         CALL AXATTRIBUTE                                                 1013
C    *                                                                    1013
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1013
         CALL AGSRESET                                                    1014
         AG_TITLE = 'ALUMINIUM'                                           1014
         AG_EXNAME = 'MATERIAL'                                           1014
         AG_PARLIST = 'NONE'                                              1014
         CALL AXMATERIAL                                                  1014
C    *                                                                    1014
C    Geant SHAPE : PCON ===> PHI1=-15 DPHI=30 NZ=4 ZI={ZWHEEL1, ZWHEEL1+  1014
C    3.500*INCH,ZWHEEL1+3.500*INCH,TPCG_LENGTHW/2}, RMN={TPCG_WHEELIR,TP  1014
C    CG_WHEELIR, TPCG_WHEELIR+3.6,TPCG_WHEELIR+3.6} RMX={TPCG_WHEELR0, T  1014
C    PCG_WHEELR0,TPCG_WHEELR0,TPCG_WHEELR0} <===                          1014
         CALL AGSRESET                                                    1019
         AG_PHI1 = -15                                                    1019
         AG_DPHI = 30                                                     1019
         AG_NZ = 4                                                        1019
         AG_ZI(1)=ZWHEEL1                                                 1019
         AG_ZI(2)=ZWHEEL1+3.500*INCH                                      1019
         AG_ZI(3)=ZWHEEL1+3.500*INCH                                      1019
         AG_ZI(4)=TPCG_LENGTHW/2                                          1019
         AG_RMN(1)=TPCG_WHEELIR                                           1019
         AG_RMN(2)=TPCG_WHEELIR                                           1019
         AG_RMN(3)=TPCG_WHEELIR+3.6                                       1019
         AG_RMN(4)=TPCG_WHEELIR+3.6                                       1019
         AG_RMX(1)=TPCG_WHEELR0                                           1019
         AG_RMX(2)=TPCG_WHEELR0                                           1019
         AG_RMX(3)=TPCG_WHEELR0                                           1019
         AG_RMX(4)=TPCG_WHEELR0                                           1019
         AG_TITLE = 'PCON'                                                1019
         AG_EXNAME = 'SHAPE'                                              1019
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1019
         CALL AXSHAPE                                                     1019
C    *                                                                    1019
5511  CALL AGSPOPB                                                        1020
      GO TO BLOCK_TWBT, (5510 ,5520)                                      1020
C End of Local Procedure                                                  1020
C ----------------------------------------------------------------------  1021
C ------------ TWRI                                                       1021
C Begin Local Procedure TWRI                                              1022
C TWRI WHEELOUTERRING                                                     1022
5530  CONTINUE                                                            1022
         AG_TITLE='TWRI'                                                  1022
         AG_MODULE=MODULE                                                 1022
         AG_MARK='V00'                                                    1022
         CALL AGSPUSH                                                     1022
         AG_MODULE=MODULE                                                 1022
C    *                                                                    1022
C    Geant SHAPE : PCON ===> PHI1=-15 DPHI=30 NZ=4 ZI={ZWHEEL1, ZTOFCEND  1027
C    ,ZTOFCEND,TPCG_LENGTHW/2} RMN={TPCG_WHEELR2*COS15, TPCG_WHEELR2*COS  1027
C    15,TPCG_WHEELR2*COS15, TPCG_WHEELR2*COS15} RMX={TPCG_WHEELOR1, TPCG  1027
C    _WHEELOR1,TPCG_WHEELOR,TPCG_WHEELOR} <===                            1027
         CALL AGSRESET                                                    1027
         AG_PHI1 = -15                                                    1027
         AG_DPHI = 30                                                     1027
         AG_NZ = 4                                                        1027
         AG_ZI(1)=ZWHEEL1                                                 1027
         AG_ZI(2)=ZTOFCEND                                                1027
         AG_ZI(3)=ZTOFCEND                                                1027
         AG_ZI(4)=TPCG_LENGTHW/2                                          1027
         AG_RMN(1)=TPCG_WHEELR2*COS15                                     1027
         AG_RMN(2)=TPCG_WHEELR2*COS15                                     1027
         AG_RMN(3)=TPCG_WHEELR2*COS15                                     1027
         AG_RMN(4)=TPCG_WHEELR2*COS15                                     1027
         AG_RMX(1)=TPCG_WHEELOR1                                          1027
         AG_RMX(2)=TPCG_WHEELOR1                                          1027
         AG_RMX(3)=TPCG_WHEELOR                                           1027
         AG_RMX(4)=TPCG_WHEELOR                                           1027
         AG_TITLE = 'PCON'                                                1027
         AG_EXNAME = 'SHAPE'                                              1027
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1027
         CALL AXSHAPE                                                     1027
C    *                                                                    1027
C    Geant MEDIUM : TWRI_SOMETHING ===> DEEMAX=0.248853 STMIN=1.15055 <=  1027
C    ==                                                                   1027
         CALL AGSRESET                                                    1028
         AG_DEEMAX = 0.248853                                             1028
         AG_STMIN = 1.15055                                               1028
         AG_TITLE = 'TWRI_SOMETHING'                                      1028
         AG_EXNAME = 'MEDIUM'                                             1028
         AG_PARLIST = 'DEEMAX_STMIN'                                      1028
         CALL AXMEDIUM                                                    1028
C    *                                                                    1028
C    Geant ATTRIBUTE : TWRI ===> SEEN=0 COLO=KGREEN <===                  1028
         CALL AGSRESET                                                    1029
         AG_SEEN = 0                                                      1029
         AG_COLO = KGREEN                                                 1029
         AG_TITLE = 'TWRI'                                                1029
         AG_EXNAME = 'ATTRIBUTE'                                          1029
         AG_PARLIST = 'SEEN_COLO'                                         1029
         CALL AXATTRIBUTE                                                 1029
C    *                                                                    1029
C    Create TWRC :                                                        1030
C    Call To Local Procedure TWRC                                         1030
         ASSIGN 5970 TO BLOCK_TWRC                                        1030
         GO TO  5960                                                      1030
5970     CONTINUE                                                         1030
C    *                                                                    1030
C    Geant POSITION : TWRC ===> inherited arguments used <===             1031
         CALL AGSRESET                                                    1031
         AG_TITLE = 'TWRC'                                                1031
         AG_EXNAME = 'POSITION'                                           1031
         AG_PARLIST = 'NONE'                                              1031
         CALL AXPOSITION                                                  1031
C    *                                                                    1031
         MYCOS = COS15                                                    1031
         MYRMIN=TPCG_WHEELR2                                              1031
         MYRMAX = TPCG_WHEELOR1                                           1032
         MYG1 = MYRMIN*COS15                                              1033
         NCOUNT = 0                                                       1034
C    *                                                                    1035
C       Loop here                                                         1035
5981        IF(MYG2.GE.MYRMIN) GO TO 5982                                 1035
            NCOUNT=NCOUNT+1                                               1035
            MYG2 = MYRMIN                                                 1037
C       Check (MYG2>MYRMAX*MYCOS)                                         1038
            IF ((MYG2.GT.MYRMAX*MYCOS)) THEN                              1038
            MYG2 = MYRMAX*MYCOS                                           1039
            ENDIF                                                         1040
            ALPHA = ACOS(MYCOS)*RADDEG                                    1040
C       Create TWRG :                                                     1042
C       Call To Local Procedure TWRG                                      1042
            ASSIGN 6000 TO BLOCK_TWRG                                     1042
            GO TO  5990                                                   1042
6000        CONTINUE                                                      1042
C       *                                                                 1042
C       Geant POSITION : TWRG ===> inherited arguments used <===          1043
            CALL AGSRESET                                                 1043
            AG_TITLE = 'TWRG'                                             1043
            AG_EXNAME = 'POSITION'                                        1043
            AG_PARLIST = 'NONE'                                           1043
            CALL AXPOSITION                                               1043
C       *                                                                 1043
            MYCOS = MYG2/MYRMIN                                           1043
            MYG1=MYG2                                                     1044
C       Check (NCOUNT>10)                                                 1045
            IF ((NCOUNT.GT.10)) THEN                                      1045
            GO TO 5982                                                    1046
            ENDIF                                                         1047
         GO TO 5981                                                       1048
5982     CONTINUE                                                         1048
5531  CALL AGSPOPB                                                        1049
      GO TO BLOCK_TWRI, (5530 ,5540)                                      1049
C End of Local Procedure                                                  1049
C ----------------------------------------------------------------------  1050
C ------------ TWRG                                                       1050
C Begin Local Procedure TWRG                                              1051
C TWRG WHEELOUTERRING PGON PART                                           1051
5990  CONTINUE                                                            1051
         AG_TITLE='TWRG'                                                  1051
         AG_MODULE=MODULE                                                 1051
         AG_MARK='V00'                                                    1051
         CALL AGSPUSH                                                     1051
         AG_MODULE=MODULE                                                 1051
C    *                                                                    1051
C    Geant ATTRIBUTE : TWRG ===> SEEN=1 COLO=KGREEN <===                  1052
         CALL AGSRESET                                                    1052
         AG_SEEN = 1                                                      1052
         AG_COLO = KGREEN                                                 1052
         AG_TITLE = 'TWRG'                                                1052
         AG_EXNAME = 'ATTRIBUTE'                                          1052
         AG_PARLIST = 'SEEN_COLO'                                         1052
         CALL AXATTRIBUTE                                                 1052
C    *                                                                    1052
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1052
         CALL AGSRESET                                                    1053
         AG_TITLE = 'ALUMINIUM'                                           1053
         AG_EXNAME = 'MATERIAL'                                           1053
         AG_PARLIST = 'NONE'                                              1053
         CALL AXMATERIAL                                                  1053
C    *                                                                    1053
C    Geant SHAPE : PGON ===> PHI1=-ALPHA DPHI=ALPHA*2 NPDIV=1, NZ=2 ZI={  1053
C    ZWHEEL1,TPCG_LENGTHW/2} RMN={MYG1, MYG1} RMX={MYG2,MYG2} <===        1053
         CALL AGSRESET                                                    1056
         AG_PHI1 = -ALPHA                                                 1056
         AG_DPHI = ALPHA*2                                                1056
         AG_NPDIV = 1                                                     1056
         AG_NZ = 2                                                        1056
         AG_ZI(1)=ZWHEEL1                                                 1056
         AG_ZI(2)=TPCG_LENGTHW/2                                          1056
         AG_RMN(1)=MYG1                                                   1056
         AG_RMN(2)=MYG1                                                   1056
         AG_RMX(1)=MYG2                                                   1056
         AG_RMX(2)=MYG2                                                   1056
         AG_TITLE = 'PGON'                                                1056
         AG_EXNAME = 'SHAPE'                                              1056
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1056
         CALL AXSHAPE                                                     1056
C    *                                                                    1056
5991  CALL AGSPOPB                                                        1057
      GO TO BLOCK_TWRG, (5990 ,6000)                                      1057
C End of Local Procedure                                                  1057
C ----------------------------------------------------------------------  1058
C ------------ TWRC                                                       1058
C Begin Local Procedure TWRC                                              1059
C TWRC WHEELOUTERRING PCON PART                                           1059
5960  CONTINUE                                                            1059
         AG_TITLE='TWRC'                                                  1059
         AG_MODULE=MODULE                                                 1059
         AG_MARK='V00'                                                    1059
         CALL AGSPUSH                                                     1059
         AG_MODULE=MODULE                                                 1059
C    *                                                                    1059
C    Geant ATTRIBUTE : TWRC ===> SEEN=1 COLO=KGREEN <===                  1060
         CALL AGSRESET                                                    1060
         AG_SEEN = 1                                                      1060
         AG_COLO = KGREEN                                                 1060
         AG_TITLE = 'TWRC'                                                1060
         AG_EXNAME = 'ATTRIBUTE'                                          1060
         AG_PARLIST = 'SEEN_COLO'                                         1060
         CALL AXATTRIBUTE                                                 1060
C    *                                                                    1060
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1060
         CALL AGSRESET                                                    1061
         AG_TITLE = 'ALUMINIUM'                                           1061
         AG_EXNAME = 'MATERIAL'                                           1061
         AG_PARLIST = 'NONE'                                              1061
         CALL AXMATERIAL                                                  1061
C    *                                                                    1061
C    Geant SHAPE : PCON ===> PHI1=-15 DPHI=30 NZ=4 ZI={ZWHEEL1, ZTOFCEND  1061
C    ,ZTOFCEND,TPCG_LENGTHW/2} RMN={TPCG_WHEELR2/COS15, TPCG_WHEELR2/COS  1061
C    15,TPCG_WHEELR2/COS15, TPCG_WHEELR2/COS15} RMX={TPCG_WHEELOR1, TPCG  1061
C    _WHEELOR1,TPCG_WHEELOR,TPCG_WHEELOR} <===                            1061
         CALL AGSRESET                                                    1066
         AG_PHI1 = -15                                                    1066
         AG_DPHI = 30                                                     1066
         AG_NZ = 4                                                        1066
         AG_ZI(1)=ZWHEEL1                                                 1066
         AG_ZI(2)=ZTOFCEND                                                1066
         AG_ZI(3)=ZTOFCEND                                                1066
         AG_ZI(4)=TPCG_LENGTHW/2                                          1066
         AG_RMN(1)=TPCG_WHEELR2/COS15                                     1066
         AG_RMN(2)=TPCG_WHEELR2/COS15                                     1066
         AG_RMN(3)=TPCG_WHEELR2/COS15                                     1066
         AG_RMN(4)=TPCG_WHEELR2/COS15                                     1066
         AG_RMX(1)=TPCG_WHEELOR1                                          1066
         AG_RMX(2)=TPCG_WHEELOR1                                          1066
         AG_RMX(3)=TPCG_WHEELOR                                           1066
         AG_RMX(4)=TPCG_WHEELOR                                           1066
         AG_TITLE = 'PCON'                                                1066
         AG_EXNAME = 'SHAPE'                                              1066
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1066
         CALL AXSHAPE                                                     1066
C    *                                                                    1066
5961  CALL AGSPOPB                                                        1067
      GO TO BLOCK_TWRC, (5960 ,5970)                                      1067
C End of Local Procedure                                                  1067
C ----------------------------------------------------------------------  1068
C ------------ TWTR                                                       1068
C Begin Local Procedure TWTR                                              1069
C TWTR TPCWHEELTOPRIB                                                     1069
5550  CONTINUE                                                            1069
         AG_TITLE='TWTR'                                                  1069
         AG_MODULE=MODULE                                                 1069
         AG_MARK='V00'                                                    1069
         CALL AGSPUSH                                                     1069
         AG_MODULE=MODULE                                                 1069
C    *                                                                    1069
C    Geant ATTRIBUTE : TWTR ===> SEEN=1 COLO=KBLUE <===                   1070
         CALL AGSRESET                                                    1070
         AG_SEEN = 1                                                      1070
         AG_COLO = KBLUE                                                  1070
         AG_TITLE = 'TWTR'                                                1070
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
         MYG1=TPCG_WHEELR2 - (TPCG_WHEELTOTALRIBWIDTH -                   1071
     *   TPCG_WHEELRIBWIDTH)/2                                            1071
C    Geant SHAPE : PGON ===> PHI1=-15 DPHI=30 NPDIV=1 NZ=2, ZI={ZWHEEL1,  1075
C    ZWHEEL1+TPCG_WHEELRIBHEIGHT} RMN={MYG1, MYG1} RMX={TPCG_WHEELR2,TPC  1075
C    G_WHEELR2} <===                                                      1075
         CALL AGSRESET                                                    1075
         AG_PHI1 = -15                                                    1075
         AG_DPHI = 30                                                     1075
         AG_NPDIV = 1                                                     1075
         AG_NZ = 2                                                        1075
         AG_ZI(1)=ZWHEEL1                                                 1075
         AG_ZI(2)=ZWHEEL1+TPCG_WHEELRIBHEIGHT                             1075
         AG_RMN(1)=MYG1                                                   1075
         AG_RMN(2)=MYG1                                                   1075
         AG_RMX(1)=TPCG_WHEELR2                                           1075
         AG_RMX(2)=TPCG_WHEELR2                                           1075
         AG_TITLE = 'PGON'                                                1075
         AG_EXNAME = 'SHAPE'                                              1075
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1075
         CALL AXSHAPE                                                     1075
C    *                                                                    1075
5551  CALL AGSPOPB                                                        1076
      GO TO BLOCK_TWTR, (5550 ,5560)                                      1076
C End of Local Procedure                                                  1076
C ----------------------------------------------------------------------  1077
C ------------ TWMR                                                       1077
C Begin Local Procedure TWMR                                              1078
C TWMR TPCWHEELMIDDLERIB                                                  1078
5570  CONTINUE                                                            1078
         AG_TITLE='TWMR'                                                  1078
         AG_MODULE=MODULE                                                 1078
         AG_MARK='V00'                                                    1078
         CALL AGSPUSH                                                     1078
         AG_MODULE=MODULE                                                 1078
C    *                                                                    1078
C    Geant ATTRIBUTE : TWMR ===> SEEN=1 COLO=KYELLOW <===                 1079
         CALL AGSRESET                                                    1079
         AG_SEEN = 1                                                      1079
         AG_COLO = KYELLOW                                                1079
         AG_TITLE = 'TWMR'                                                1079
         AG_EXNAME = 'ATTRIBUTE'                                          1079
         AG_PARLIST = 'SEEN_COLO'                                         1079
         CALL AXATTRIBUTE                                                 1079
C    *                                                                    1079
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1079
         CALL AGSRESET                                                    1080
         AG_TITLE = 'ALUMINIUM'                                           1080
         AG_EXNAME = 'MATERIAL'                                           1080
         AG_PARLIST = 'NONE'                                              1080
         CALL AXMATERIAL                                                  1080
C    *                                                                    1080
         MYG1=TPCG_WHEELR2 - (TPCG_WHEELTOTALRIBWIDTH -                   1080
     *   TPCG_WHEELRIBWIDTH)/2                                            1080
C    Geant SHAPE : PGON ===> PHI1=-15 DPHI=30 NPDIV=1 NZ=4, ZI={ZWHEEL1,  1090
C    ZWHEEL1+TPCG_WHEELRIBHEIGHT,ZWHEEL1+TPCG_WHEELRIBHEIGHT, TPCG_LENGT  1090
C    HW/2} RMN={TPCG_WHEELR1-TPCG_WHEELTOTALRIBWIDTH/2, TPCG_WHEELR1-TPC  1090
C    G_WHEELTOTALRIBWIDTH/2, TPCG_WHEELR1-TPCG_WHEELRIBWIDTH/2, TPCG_WHE  1090
C    ELR1-TPCG_WHEELRIBWIDTH/2}, RMX={TPCG_WHEELR1+TPCG_WHEELTOTALRIBWID  1090
C    TH/2, TPCG_WHEELR1+TPCG_WHEELTOTALRIBWIDTH/2, TPCG_WHEELR1+TPCG_WHE  1090
C    ELRIBWIDTH/2,TPCG_WHEELR1+TPCG_WHEELRIBWIDTH/2} <===                 1090
         CALL AGSRESET                                                    1090
         AG_PHI1 = -15                                                    1090
         AG_DPHI = 30                                                     1090
         AG_NPDIV = 1                                                     1090
         AG_NZ = 4                                                        1090
         AG_ZI(1)=ZWHEEL1                                                 1090
         AG_ZI(2)=ZWHEEL1+TPCG_WHEELRIBHEIGHT                             1090
         AG_ZI(3)=ZWHEEL1+TPCG_WHEELRIBHEIGHT                             1090
         AG_ZI(4)=TPCG_LENGTHW/2                                          1090
         AG_RMN(1)=TPCG_WHEELR1-TPCG_WHEELTOTALRIBWIDTH/2                 1090
         AG_RMN(2)=TPCG_WHEELR1-TPCG_WHEELTOTALRIBWIDTH/2                 1090
         AG_RMN(3)=TPCG_WHEELR1-TPCG_WHEELRIBWIDTH/2                      1090
         AG_RMN(4)=TPCG_WHEELR1-TPCG_WHEELRIBWIDTH/2                      1090
         AG_RMX(1)=TPCG_WHEELR1+TPCG_WHEELTOTALRIBWIDTH/2                 1090
         AG_RMX(2)=TPCG_WHEELR1+TPCG_WHEELTOTALRIBWIDTH/2                 1090
         AG_RMX(3)=TPCG_WHEELR1+TPCG_WHEELRIBWIDTH/2                      1090
         AG_RMX(4)=TPCG_WHEELR1+TPCG_WHEELRIBWIDTH/2                      1090
         AG_TITLE = 'PGON'                                                1090
         AG_EXNAME = 'SHAPE'                                              1090
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1090
         CALL AXSHAPE                                                     1090
C    *                                                                    1090
5571  CALL AGSPOPB                                                        1091
      GO TO BLOCK_TWMR, (5570 ,5580)                                      1091
C End of Local Procedure                                                  1091
C ----------------------------------------------------------------------  1092
C ------------ TRDO                                                       1092
C Begin Local Procedure TRDO                                              1093
C TRDO TPCRDOASSEMBLY                                                     1093
5590  CONTINUE                                                            1093
         AG_TITLE='TRDO'                                                  1093
         AG_MODULE=MODULE                                                 1093
         AG_MARK='V00'                                                    1093
         CALL AGSPUSH                                                     1093
         AG_MODULE=MODULE                                                 1093
C    *                                                                    1093
C    Geant ATTRIBUTE : TRDO ===> SEEN=0 <===                              1094
         CALL AGSRESET                                                    1094
         AG_SEEN = 0                                                      1094
         AG_TITLE = 'TRDO'                                                1094
         AG_EXNAME = 'ATTRIBUTE'                                          1094
         AG_PARLIST = 'SEEN'                                              1094
         CALL AXATTRIBUTE                                                 1094
C    *                                                                    1094
C    Geant ATTRIBUTE : TRDO ===> SEEN=1 COLO=KYELLOW <===                 1094
         CALL AGSRESET                                                    1095
         AG_SEEN = 1                                                      1095
         AG_COLO = KYELLOW                                                1095
         AG_TITLE = 'TRDO'                                                1095
         AG_EXNAME = 'ATTRIBUTE'                                          1095
         AG_PARLIST = 'SEEN_COLO'                                         1095
         CALL AXATTRIBUTE                                                 1095
C    *                                                                    1095
C    Geant SHAPE : PGON ===> PHI1=-15 DPHI=30 NPDIV=1 NZ=2, ZI={ZZZRDO(1  1095
C    ),ZZZRDO(2)} RMN={RMNRDO(1),RMNRDO(2)}, RMX={RMXRDO(1),RMXRDO(2)} <  1095
C    ===                                                                  1095
         CALL AGSRESET                                                    1098
         AG_PHI1 = -15                                                    1098
         AG_DPHI = 30                                                     1098
         AG_NPDIV = 1                                                     1098
         AG_NZ = 2                                                        1098
         AG_ZI(1)=ZZZRDO(1)                                               1098
         AG_ZI(2)=ZZZRDO(2)                                               1098
         AG_RMN(1)=RMNRDO(1)                                              1098
         AG_RMN(2)=RMNRDO(2)                                              1098
         AG_RMX(1)=RMXRDO(1)                                              1098
         AG_RMX(2)=RMXRDO(2)                                              1098
         AG_TITLE = 'PGON'                                                1098
         AG_EXNAME = 'SHAPE'                                              1098
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1098
         CALL AXSHAPE                                                     1098
C    *                                                                    1098
         ISHAPE = KPARA                                                   1098
         MYPAR(1)=TPCG_RDOCOOLINGDY                                       1100
         MYPAR(2)=TPCG_RDOCOOLINGDX                                       1100
         MYPAR(3)=TPCG_RDOCOOLINGDZ                                       1100
         MYPAR(4)=15                                                      1100
         X = TPCG_WHEELR2 - TPCG_RDOCOOLINGDX                             1101
         Y = X*TAN15 - TPCG_RDOCOOLINGDY                                  1102
         Z = TPCG_ZRDO - 2*DY                                             1103
C    Create TCOO :                                                        1104
C    Call To Local Procedure TCOO                                         1104
         ASSIGN 6010 TO BLOCK_TCOO                                        1104
         GO TO  5620                                                      1104
6010     CONTINUE                                                         1104
C    *                                                                    1104
C    Geant POSITION : TCOO ===> X=X Y=Y Z=Z ORT=YX-Z <===                 1105
         CALL AGSRESET                                                    1105
         AG_X = X                                                         1105
         AG_Y = Y                                                         1105
         AG_Z = Z                                                         1105
         AG_ALPHAN=AG_ALPHAN+1                                            1105
         AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                                1105
         AG_TITLE = 'TCOO'                                                1105
         AG_EXNAME = 'POSITION'                                           1105
         AG_PARLIST = 'X_Y_Z_ORT'                                         1105
         CALL AXPOSITION                                                  1105
C    *                                                                    1105
         ISHAPE = KPARA                                                   1105
         MYPAR(1)=TPCG_RDOCOOLINGDY                                       1107
         MYPAR(2)=TPCG_RDOCOOLINGDX                                       1107
         MYPAR(3)=TPCG_RDOCOOLINGDZ                                       1107
         MYPAR(4)=-15                                                     1107
         X = TPCG_WHEELR2 - TPCG_RDOCOOLINGDX                             1108
         Y = X*TAN15 - TPCG_RDOCOOLINGDY                                  1109
         Z = TPCG_ZRDO - 2*DY                                             1110
C    Create TCOO :                                                        1111
C    Call To Local Procedure TCOO                                         1111
         ASSIGN 6020 TO BLOCK_TCOO                                        1111
         GO TO  5620                                                      1111
6020     CONTINUE                                                         1111
C    *                                                                    1111
C    Geant POSITION : TCOO ===> X=X Y=-Y Z=Z ORT=YX-Z <===                1112
         CALL AGSRESET                                                    1112
         AG_X = X                                                         1112
         AG_Y = -Y                                                        1112
         AG_Z = Z                                                         1112
         AG_ALPHAN=AG_ALPHAN+1                                            1112
         AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                                1112
         AG_TITLE = 'TCOO'                                                1112
         AG_EXNAME = 'POSITION'                                           1112
         AG_PARLIST = 'X_Y_Z_ORT'                                         1112
         CALL AXPOSITION                                                  1112
C    *                                                                    1112
         RCOOLINGTUBE = TPCG_WHEELR2 - TPCG_HEIGTUBE - 2*TPCG_DXRDO       1112
C    *                                                                    1114
         ICOO=0                                                           1114
            GO TO 6033                                                    1114
6031        ICOO=ICOO+1                                                   1114
6033        IF(ICOO-(7).GT.0) GO TO 6032                                  1114
            DZ = TPCG_HEIGTUBE*CM/2                                       1115
            DY = TPCG_WIDTUBE*CM/2                                        1116
            Z = RCOOLINGTUBE                                              1117
            DX1 = (Z-DZ)*TAN15 - 2*TPCG_RDOCOOLINGDY                      1118
            DX2 = (Z+DZ)*TAN15 - 2*TPCG_RDOCOOLINGDY                      1119
            ISHAPE = KTRD1                                                1119
            MYPAR(1)=DX1                                                  1121
            MYPAR(2)=DX2                                                  1121
            MYPAR(3)=DY                                                   1121
            MYPAR(4)=DZ                                                   1121
            MYPAR(11) = Z                                                 1121
            MYPAR(13) = TPCG_ZRDO - 2*DY                                  1122
C       Create TCOO :                                                     1123
C       Call To Local Procedure TCOO                                      1123
            ASSIGN 6040 TO BLOCK_TCOO                                     1123
            GO TO  5620                                                   1123
6040        CONTINUE                                                      1123
C       *                                                                 1123
C       Geant POSITION : TCOO ===> X=MYPAR(11) Y=0 Z=MYPAR(13) ORT=YZX <  1124
C       ===                                                               1124
            CALL AGSRESET                                                 1124
            AG_X = MYPAR(11)                                              1124
            AG_Y = 0                                                      1124
            AG_Z = MYPAR(13)                                              1124
            AG_ALPHAN=AG_ALPHAN+1                                         1124
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              1124
            AG_TITLE = 'TCOO'                                             1124
            AG_EXNAME = 'POSITION'                                        1124
            AG_PARLIST = 'X_Y_Z_ORT'                                      1124
            CALL AXPOSITION                                               1124
C       *                                                                 1124
C       Check (ICOO .NE. 0 .AND. ICOO .NE. 6)                             1124
            IF ((ICOO .NE. 0 .AND. ICOO .NE. 6)) THEN                     1125
            MYPAR(11) = Z + DZ + TPCG_DXRDO                               1125
            MYPAR(13) = TPCG_ZRDO-TPCG_DZRDO                              1127
C       Create TRDC :                                                     1128
C       Call To Local Procedure TRDC                                      1128
            ASSIGN 6060 TO BLOCK_TRDC                                     1128
            GO TO  6050                                                   1128
6060        CONTINUE                                                      1128
C       *                                                                 1128
C       Geant POSITION : TRDC ===> X=MYPAR(11) Y=0 Z=MYPAR(13) <===       1129
            CALL AGSRESET                                                 1129
            AG_X = MYPAR(11)                                              1129
            AG_Y = 0                                                      1129
            AG_Z = MYPAR(13)                                              1129
            AG_TITLE = 'TRDC'                                             1129
            AG_EXNAME = 'POSITION'                                        1129
            AG_PARLIST = 'X_Y_Z'                                          1129
            CALL AXPOSITION                                               1129
C       *                                                                 1129
            ENDIF                                                         1129
            RCOOLINGTUBE=RCOOLINGTUBE-DRDOCOOLING(ICOO)                   1131
         GO TO 6031                                                       1132
6032     CONTINUE                                                         1132
5591  CALL AGSPOPB                                                        1133
      GO TO BLOCK_TRDO, (5590 ,5600)                                      1133
C End of Local Procedure                                                  1133
C ----------------------------------------------------------------------  1134
C ------------ TBRW                                                       1134
C Begin Local Procedure TBRW                                              1135
C TBRW WHEELRIBBOX                                                        1135
5440  CONTINUE                                                            1135
         AG_TITLE='TBRW'                                                  1135
         AG_MODULE=MODULE                                                 1135
         AG_MARK='V00'                                                    1135
         CALL AGSPUSH                                                     1135
         AG_MODULE=MODULE                                                 1135
C    *                                                                    1135
C    Geant ATTRIBUTE : TBRW ===> SEEN=1 COLO=KVIOLET <===                 1136
         CALL AGSRESET                                                    1136
         AG_SEEN = 1                                                      1136
         AG_COLO = KVIOLET                                                1136
         AG_TITLE = 'TBRW'                                                1136
         AG_EXNAME = 'ATTRIBUTE'                                          1136
         AG_PARLIST = 'SEEN_COLO'                                         1136
         CALL AXATTRIBUTE                                                 1136
C    *                                                                    1136
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1136
         CALL AGSRESET                                                    1137
         AG_TITLE = 'ALUMINIUM'                                           1137
         AG_EXNAME = 'MATERIAL'                                           1137
         AG_PARLIST = 'NONE'                                              1137
         CALL AXMATERIAL                                                  1137
C    *                                                                    1137
C    Geant SHAPE : BOX ===> DX=TPCG_WHEELBOXDY/2 DY=TPCG_WHEELBOXDX/2, D  1137
C    Z=TPCG_WHEELTHK/2 <===                                               1137
         CALL AGSRESET                                                    1139
         AG_DX = TPCG_WHEELBOXDY/2                                        1139
         AG_DY = TPCG_WHEELBOXDX/2                                        1139
         AG_DZ = TPCG_WHEELTHK/2                                          1139
         AG_TITLE = 'BOX'                                                 1139
         AG_EXNAME = 'SHAPE'                                              1139
         AG_PARLIST = 'DX_DY_DZ'                                          1139
         CALL AXSHAPE                                                     1139
C    *                                                                    1139
5441  CALL AGSPOPB                                                        1140
      GO TO BLOCK_TBRW, (5440 ,5450)                                      1140
C End of Local Procedure                                                  1140
C ----------------------------------------------------------------------  1141
C ------------ TWRB                                                       1141
C Begin Local Procedure TWRB                                              1142
C TWRB TPCWHEELRIB                                                        1142
5460  CONTINUE                                                            1142
         AG_TITLE='TWRB'                                                  1142
         AG_MODULE=MODULE                                                 1142
         AG_MARK='V00'                                                    1142
         CALL AGSPUSH                                                     1142
         AG_MODULE=MODULE                                                 1142
C    *                                                                    1142
C    Geant ATTRIBUTE : TWRB ===> SEEN=1 COLO=KYELLOW <===                 1143
         CALL AGSRESET                                                    1143
         AG_SEEN = 1                                                      1143
         AG_COLO = KYELLOW                                                1143
         AG_TITLE = 'TWRB'                                                1143
         AG_EXNAME = 'ATTRIBUTE'                                          1143
         AG_PARLIST = 'SEEN_COLO'                                         1143
         CALL AXATTRIBUTE                                                 1143
C    *                                                                    1143
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1143
         CALL AGSRESET                                                    1144
         AG_TITLE = 'ALUMINIUM'                                           1144
         AG_EXNAME = 'MATERIAL'                                           1144
         AG_PARLIST = 'NONE'                                              1144
         CALL AXMATERIAL                                                  1144
C    *                                                                    1144
C    Geant SHAPE : PARA ===> DX=MYDX DY=MYDY DZ=MYDZ ALPH=MYALPH, THET=0  1144
C     PHI=0 <===                                                          1144
         CALL AGSRESET                                                    1146
         AG_DX = MYDX                                                     1146
         AG_DY = MYDY                                                     1146
         AG_DZ = MYDZ                                                     1146
         AG_ALPH = MYALPH                                                 1146
         AG_THET = 0                                                      1146
         AG_PHI = 0                                                       1146
         AG_TITLE = 'PARA'                                                1146
         AG_EXNAME = 'SHAPE'                                              1146
         AG_PARLIST = 'DX_DY_DZ_ALPH_THET_PHI'                            1146
         CALL AXSHAPE                                                     1146
C    *                                                                    1146
5461  CALL AGSPOPB                                                        1147
      GO TO BLOCK_TWRB, (5460 ,5470,5480)                                 1147
C End of Local Procedure                                                  1147
C ----------------------------------------------------------------------  1148
C ------------ TCOO                                                       1148
C Begin Local Procedure TCOO                                              1149
C TCOO COOLINGTUBE                                                        1149
5620  CONTINUE                                                            1149
         AG_TITLE='TCOO'                                                  1149
         AG_MODULE=MODULE                                                 1149
         AG_MARK='V00'                                                    1149
         CALL AGSPUSH                                                     1149
         AG_MODULE=MODULE                                                 1149
C    *                                                                    1149
C    Geant ATTRIBUTE : TCOO ===> SEEN=1 COLO=KRED <===                    1150
         CALL AGSRESET                                                    1150
         AG_SEEN = 1                                                      1150
         AG_COLO = KRED                                                   1150
         AG_TITLE = 'TCOO'                                                1150
         AG_EXNAME = 'ATTRIBUTE'                                          1150
         AG_PARLIST = 'SEEN_COLO'                                         1150
         CALL AXATTRIBUTE                                                 1150
C    *                                                                    1150
C    Geant MATERIAL : WATER_PIPE ===> inherited arguments used <===       1150
         CALL AGSRESET                                                    1151
         AG_TITLE = 'WATER_PIPE'                                          1151
         AG_EXNAME = 'MATERIAL'                                           1151
         AG_PARLIST = 'NONE'                                              1151
         CALL AXMATERIAL                                                  1151
C    *                                                                    1151
C    Check ISHAPE==KPARA                                                  1151
         IF (ISHAPE.EQ.KPARA) THEN                                        1152
C    Geant SHAPE : PARA ===> DX=MYPAR(1) DY=MYPAR(2) DZ=MYPAR(3), ALPH=M  1154
C    YPAR(4) THET=0 PHI=0 <===                                            1154
         CALL AGSRESET                                                    1154
         AG_DX = MYPAR(1)                                                 1154
         AG_DY = MYPAR(2)                                                 1154
         AG_DZ = MYPAR(3)                                                 1154
         AG_ALPH = MYPAR(4)                                               1154
         AG_THET = 0                                                      1154
         AG_PHI = 0                                                       1154
         AG_TITLE = 'PARA'                                                1154
         AG_EXNAME = 'SHAPE'                                              1154
         AG_PARLIST = 'DX_DY_DZ_ALPH_THET_PHI'                            1154
         CALL AXSHAPE                                                     1154
C    *                                                                    1154
         ENDIF                                                            1154
C    Check ISHAPE==KTRD1                                                  1156
         IF (ISHAPE.EQ.KTRD1) THEN                                        1156
C    Geant SHAPE : TRD1 ===> DX1=MYPAR(1) DX2=MYPAR(2) DY=MYPAR(3), DZ=M  1158
C    YPAR(4) <===                                                         1158
         CALL AGSRESET                                                    1158
         AG_DX1 = MYPAR(1)                                                1158
         AG_DX2 = MYPAR(2)                                                1158
         AG_DY = MYPAR(3)                                                 1158
         AG_DZ = MYPAR(4)                                                 1158
         AG_TITLE = 'TRD1'                                                1158
         AG_EXNAME = 'SHAPE'                                              1158
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     1158
         CALL AXSHAPE                                                     1158
C    *                                                                    1158
         ENDIF                                                            1158
5621  CALL AGSPOPB                                                        1160
      GO TO BLOCK_TCOO, (5620 ,5630,5800,5820,6010,6020,6040)             1160
C End of Local Procedure                                                  1160
C ----------------------------------------------------------------------  1161
C ------------ TCAB                                                       1161
C Begin Local Procedure TCAB                                              1162
C TCAB CABLES                                                             1162
5650  CONTINUE                                                            1162
         AG_TITLE='TCAB'                                                  1162
         AG_MODULE=MODULE                                                 1162
         AG_MARK='V00'                                                    1162
         CALL AGSPUSH                                                     1162
         AG_MODULE=MODULE                                                 1162
C    *                                                                    1162
C    Geant ATTRIBUTE : TCAB ===> SEEN=1 COLO=KGREEN <===                  1163
         CALL AGSRESET                                                    1163
         AG_SEEN = 1                                                      1163
         AG_COLO = KGREEN                                                 1163
         AG_TITLE = 'TCAB'                                                1163
         AG_EXNAME = 'ATTRIBUTE'                                          1163
         AG_PARLIST = 'SEEN_COLO'                                         1163
         CALL AXATTRIBUTE                                                 1163
C    *                                                                    1163
C    Geant MATERIAL : CABLES ===> inherited arguments used <===           1163
         CALL AGSRESET                                                    1164
         AG_TITLE = 'CABLES'                                              1164
         AG_EXNAME = 'MATERIAL'                                           1164
         AG_PARLIST = 'NONE'                                              1164
         CALL AXMATERIAL                                                  1164
C    *                                                                    1164
C    Geant SHAPE : PARA ===> DX=MYPAR(1) DY=MYPAR(2) DZ=MYPAR(3), ALPH=M  1164
C    YPAR(4) THET=0 PHI=0 <===                                            1164
         CALL AGSRESET                                                    1166
         AG_DX = MYPAR(1)                                                 1166
         AG_DY = MYPAR(2)                                                 1166
         AG_DZ = MYPAR(3)                                                 1166
         AG_ALPH = MYPAR(4)                                               1166
         AG_THET = 0                                                      1166
         AG_PHI = 0                                                       1166
         AG_TITLE = 'PARA'                                                1166
         AG_EXNAME = 'SHAPE'                                              1166
         AG_PARLIST = 'DX_DY_DZ_ALPH_THET_PHI'                            1166
         CALL AXSHAPE                                                     1166
C    *                                                                    1166
5651  CALL AGSPOPB                                                        1167
      GO TO BLOCK_TCAB, (5650 ,5660)                                      1167
C End of Local Procedure                                                  1167
C ----------------------------------------------------------------------  1168
C ------------ TRDC                                                       1168
C Begin Local Procedure TRDC                                              1169
C TRDC RDOCARD                                                            1169
6050  CONTINUE                                                            1169
         AG_TITLE='TRDC'                                                  1169
         AG_MODULE=MODULE                                                 1169
         AG_MARK='V00'                                                    1169
         CALL AGSPUSH                                                     1169
         AG_MODULE=MODULE                                                 1169
C    *                                                                    1169
C    Geant ATTRIBUTE : TRDC ===> SEEN=1 COLO=KBLUE <===                   1170
         CALL AGSRESET                                                    1170
         AG_SEEN = 1                                                      1170
         AG_COLO = KBLUE                                                  1170
         AG_TITLE = 'TRDC'                                                1170
         AG_EXNAME = 'ATTRIBUTE'                                          1170
         AG_PARLIST = 'SEEN_COLO'                                         1170
         CALL AXATTRIBUTE                                                 1170
C    *                                                                    1170
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1170
         CALL AGSRESET                                                    1171
         AG_TITLE = 'G10'                                                 1171
         AG_EXNAME = 'MATERIAL'                                           1171
         AG_PARLIST = 'NONE'                                              1171
         CALL AXMATERIAL                                                  1171
C    *                                                                    1171
C    Geant SHAPE : BOX ===> DX=TPCG_DXRDO DY=TPCG_DYRDO DZ=TPCG_DZRDO <=  1171
C    ==                                                                   1171
         CALL AGSRESET                                                    1172
         AG_DX = TPCG_DXRDO                                               1172
         AG_DY = TPCG_DYRDO                                               1172
         AG_DZ = TPCG_DZRDO                                               1172
         AG_TITLE = 'BOX'                                                 1172
         AG_EXNAME = 'SHAPE'                                              1172
         AG_PARLIST = 'DX_DY_DZ'                                          1172
         CALL AXSHAPE                                                     1172
C    *                                                                    1172
6051  CALL AGSPOPB                                                        1173
      GO TO BLOCK_TRDC, (6050 ,6060)                                      1173
C End of Local Procedure                                                  1173
C ----------------------------------------------------------------------  1174
C ------------ TRIB                                                       1174
C Begin Local Procedure TRIB                                              1175
C TRIB TPC RIBS                                                           1175
5720  CONTINUE                                                            1175
         AG_TITLE='TRIB'                                                  1175
         AG_MODULE=MODULE                                                 1175
         AG_MARK='V00'                                                    1175
         CALL AGSPUSH                                                     1175
         AG_MODULE=MODULE                                                 1175
C    *                                                                    1175
C    Geant ATTRIBUTE : TRIB ===> SEEN=1 COLO=KRED <===                    1176
         CALL AGSRESET                                                    1176
         AG_SEEN = 1                                                      1176
         AG_COLO = KRED                                                   1176
         AG_TITLE = 'TRIB'                                                1176
         AG_EXNAME = 'ATTRIBUTE'                                          1176
         AG_PARLIST = 'SEEN_COLO'                                         1176
         CALL AXATTRIBUTE                                                 1176
C    *                                                                    1176
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1176
         CALL AGSRESET                                                    1177
         AG_TITLE = 'ALUMINIUM'                                           1177
         AG_EXNAME = 'MATERIAL'                                           1177
         AG_PARLIST = 'NONE'                                              1177
         CALL AXMATERIAL                                                  1177
C    *                                                                    1177
C    Check ISHAPE==KPARA                                                  1177
         IF (ISHAPE.EQ.KPARA) THEN                                        1178
C    Geant SHAPE : PARA ===> DX=MYPAR(1) DY=MYPAR(2) DZ=MYPAR(3), ALPH=M  1180
C    YPAR(4) THET=0 PHI=0 <===                                            1180
         CALL AGSRESET                                                    1180
         AG_DX = MYPAR(1)                                                 1180
         AG_DY = MYPAR(2)                                                 1180
         AG_DZ = MYPAR(3)                                                 1180
         AG_ALPH = MYPAR(4)                                               1180
         AG_THET = 0                                                      1180
         AG_PHI = 0                                                       1180
         AG_TITLE = 'PARA'                                                1180
         AG_EXNAME = 'SHAPE'                                              1180
         AG_PARLIST = 'DX_DY_DZ_ALPH_THET_PHI'                            1180
         CALL AXSHAPE                                                     1180
C    *                                                                    1180
         ELSEIF (ISHAPE.EQ.KTRD1) THEN                                    1180
C    Geant SHAPE : TRD1 ===> DX1=MYPAR(1) DX2=MYPAR(2) DY=MYPAR(3), DZ=M  1183
C    YPAR(4) <===                                                         1183
         CALL AGSRESET                                                    1183
         AG_DX1 = MYPAR(1)                                                1183
         AG_DX2 = MYPAR(2)                                                1183
         AG_DY = MYPAR(3)                                                 1183
         AG_DZ = MYPAR(4)                                                 1183
         AG_TITLE = 'TRD1'                                                1183
         AG_EXNAME = 'SHAPE'                                              1183
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     1183
         CALL AXSHAPE                                                     1183
C    *                                                                    1183
         ELSEIF (ISHAPE.EQ.KBBOX) THEN                                    1183
C    Geant SHAPE : BOX ===> DX=MYPAR(1) DY=MYPAR(2) DZ=MYPAR(3) <===      1185
         CALL AGSRESET                                                    1185
         AG_DX = MYPAR(1)                                                 1185
         AG_DY = MYPAR(2)                                                 1185
         AG_DZ = MYPAR(3)                                                 1185
         AG_TITLE = 'BOX'                                                 1185
         AG_EXNAME = 'SHAPE'                                              1185
         AG_PARLIST = 'DX_DY_DZ'                                          1185
         CALL AXSHAPE                                                     1185
C    *                                                                    1185
         ENDIF                                                            1185
5721  CALL AGSPOPB                                                        1187
      GO TO BLOCK_TRIB, (5720 ,5730,5770,5780)                            1187
C End of Local Procedure                                                  1187
C ----------------------------------------------------------------------  1188
C ------------ TWIR                                                       1188
C Begin Local Procedure TWIR                                              1189
C TWIR WIREMOUNT                                                          1189
5740  CONTINUE                                                            1189
         AG_TITLE='TWIR'                                                  1189
         AG_MODULE=MODULE                                                 1189
         AG_MARK='V00'                                                    1189
         CALL AGSPUSH                                                     1189
         AG_MODULE=MODULE                                                 1189
C    *                                                                    1189
C    Geant ATTRIBUTE : TWIR ===> SEEN=1 COLO=KYELLOW <===                 1190
         CALL AGSRESET                                                    1190
         AG_SEEN = 1                                                      1190
         AG_COLO = KYELLOW                                                1190
         AG_TITLE = 'TWIR'                                                1190
         AG_EXNAME = 'ATTRIBUTE'                                          1190
         AG_PARLIST = 'SEEN_COLO'                                         1190
         CALL AXATTRIBUTE                                                 1190
C    *                                                                    1190
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1190
         CALL AGSRESET                                                    1191
         AG_TITLE = 'G10'                                                 1191
         AG_EXNAME = 'MATERIAL'                                           1191
         AG_PARLIST = 'NONE'                                              1191
         CALL AXMATERIAL                                                  1191
C    *                                                                    1191
C    Geant SHAPE : PARA ===> DX=MYPAR(1) DY=MYPAR(2) DZ=MYPAR(3), ALPH=M  1191
C    YPAR(4) THET=0 PHI=0 <===                                            1191
         CALL AGSRESET                                                    1193
         AG_DX = MYPAR(1)                                                 1193
         AG_DY = MYPAR(2)                                                 1193
         AG_DZ = MYPAR(3)                                                 1193
         AG_ALPH = MYPAR(4)                                               1193
         AG_THET = 0                                                      1193
         AG_PHI = 0                                                       1193
         AG_TITLE = 'PARA'                                                1193
         AG_EXNAME = 'SHAPE'                                              1193
         AG_PARLIST = 'DX_DY_DZ_ALPH_THET_PHI'                            1193
         CALL AXSHAPE                                                     1193
C    *                                                                    1193
5741  CALL AGSPOPB                                                        1194
      GO TO BLOCK_TWIR, (5740 ,5750)                                      1194
C End of Local Procedure                                                  1194
C ----------------------------------------------------------------------  1195
C ------------ THOL                                                       1195
C Begin Local Procedure THOL                                              1196
C THOL THE SECTOR HOLES FOR FEE                                           1196
5940  CONTINUE                                                            1196
         AG_TITLE='THOL'                                                  1196
         AG_MODULE=MODULE                                                 1196
         AG_MARK='V00'                                                    1196
         CALL AGSPUSH                                                     1196
         AG_MODULE=MODULE                                                 1196
C    *                                                                    1196
C    Geant ATTRIBUTE : THOL ===> SEEN=1 COLO=KGREEN <===                  1197
         CALL AGSRESET                                                    1197
         AG_SEEN = 1                                                      1197
         AG_COLO = KGREEN                                                 1197
         AG_TITLE = 'THOL'                                                1197
         AG_EXNAME = 'ATTRIBUTE'                                          1197
         AG_PARLIST = 'SEEN_COLO'                                         1197
         CALL AXATTRIBUTE                                                 1197
C    *                                                                    1197
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1197
         CALL AGSRESET                                                    1198
         AG_TITLE = 'AIR'                                                 1198
         AG_EXNAME = 'MATERIAL'                                           1198
         AG_PARLIST = 'NONE'                                              1198
         CALL AXMATERIAL                                                  1198
C    *                                                                    1198
C    Geant SHAPE : BOX ===> DX=MYPAR(1) DY=MYPAR(2) DZ=MYPAR(3) <===      1198
         CALL AGSRESET                                                    1199
         AG_DX = MYPAR(1)                                                 1199
         AG_DY = MYPAR(2)                                                 1199
         AG_DZ = MYPAR(3)                                                 1199
         AG_TITLE = 'BOX'                                                 1199
         AG_EXNAME = 'SHAPE'                                              1199
         AG_PARLIST = 'DX_DY_DZ'                                          1199
         CALL AXSHAPE                                                     1199
C    *                                                                    1199
C    Create FEES :                                                        1200
C    Call To Local Procedure FEES                                         1200
         ASSIGN 6080 TO BLOCK_FEES                                        1200
         GO TO  6070                                                      1200
6080     CONTINUE                                                         1200
C    *                                                                    1200
C    Geant POSITION : FEES ===> inherited arguments used <===             1201
         CALL AGSRESET                                                    1201
         AG_TITLE = 'FEES'                                                1201
         AG_EXNAME = 'POSITION'                                           1201
         AG_PARLIST = 'NONE'                                              1201
         CALL AXPOSITION                                                  1201
C    *                                                                    1201
5941  CALL AGSPOPB                                                        1202
      GO TO BLOCK_THOL, (5940 ,5950)                                      1202
C End of Local Procedure                                                  1202
C ----------------------------------------------------------------------  1203
C ------------ FEES                                                       1203
C Begin Local Procedure FEES                                              1204
C FEES TFEESOCKET                                                         1204
6070  CONTINUE                                                            1204
         AG_TITLE='FEES'                                                  1204
         AG_MODULE=MODULE                                                 1204
         AG_MARK='V00'                                                    1204
         CALL AGSPUSH                                                     1204
         AG_MODULE=MODULE                                                 1204
C    *                                                                    1204
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1205
         CALL AGSRESET                                                    1205
         AG_TITLE = 'G10'                                                 1205
         AG_EXNAME = 'MATERIAL'                                           1205
         AG_PARLIST = 'NONE'                                              1205
         CALL AXMATERIAL                                                  1205
C    *                                                                    1205
C    Geant ATTRIBUTE : FEES ===> SEEN=1 COLO=KBLUE <===                   1205
         CALL AGSRESET                                                    1206
         AG_SEEN = 1                                                      1206
         AG_COLO = KBLUE                                                  1206
         AG_TITLE = 'FEES'                                                1206
         AG_EXNAME = 'ATTRIBUTE'                                          1206
         AG_PARLIST = 'SEEN_COLO'                                         1206
         CALL AXATTRIBUTE                                                 1206
C    *                                                                    1206
C    Geant SHAPE : BOX ===> DX=6.2/20 DY=57./20 DZ=5.8/20 <===            1206
         CALL AGSRESET                                                    1207
         AG_DX = 6.2/20                                                   1207
         AG_DY = 57./20                                                   1207
         AG_DZ = 5.8/20                                                   1207
         AG_TITLE = 'BOX'                                                 1207
         AG_EXNAME = 'SHAPE'                                              1207
         AG_PARLIST = 'DX_DY_DZ'                                          1207
         CALL AXSHAPE                                                     1207
C    *                                                                    1207
6071  CALL AGSPOPB                                                        1208
      GO TO BLOCK_FEES, (6070 ,6080)                                      1208
C End of Local Procedure                                                  1208
C ----------------------------------------------------------------------  1209
C ------------ TPGV                                                       1209
C Begin Local Procedure TPGV                                              1210
C TPGV IS THE GAS VOLUME PLACED IN TPC                                    1210
5050  CONTINUE                                                            1210
         AG_TITLE='TPGV'                                                  1210
         AG_MODULE=MODULE                                                 1210
         AG_MARK='V00'                                                    1210
         CALL AGSPUSH                                                     1210
         AG_MODULE=MODULE                                                 1210
C    *                                                                    1210
C    Geant ATTRIBUTE : TPGV ===> SEEN=1 COLO=KRED <===                    1211
         CALL AGSRESET                                                    1211
         AG_SEEN = 1                                                      1211
         AG_COLO = KRED                                                   1211
         AG_TITLE = 'TPGV'                                                1211
         AG_EXNAME = 'ATTRIBUTE'                                          1211
         AG_PARLIST = 'SEEN_COLO'                                         1211
         CALL AXATTRIBUTE                                                 1211
C    *                                                                    1211
C    Geant MATERIAL : P10 ===> inherited arguments used <===              1211
         CALL AGSRESET                                                    1212
         AG_TITLE = 'P10'                                                 1212
         AG_EXNAME = 'MATERIAL'                                           1212
         AG_PARLIST = 'NONE'                                              1212
         CALL AXMATERIAL                                                  1212
C    *                                                                    1212
C    Geant SHAPE : TUBE ===> RMIN=TPGVIR RMAX=TPCG_SENGASOR, DZ=TPGVLENG  1212
C    /2 <===                                                              1212
         CALL AGSRESET                                                    1214
         AG_RMIN = TPGVIR                                                 1214
         AG_RMAX = TPCG_SENGASOR                                          1214
         AG_DZ = TPGVLENG/2                                               1214
         AG_TITLE = 'TUBE'                                                1214
         AG_EXNAME = 'SHAPE'                                              1214
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1214
         CALL AXSHAPE                                                     1214
C    *                                                                    1214
C    *                                                                    1214
         ISECANG=15                                                       1215
            GOTO 6093                                                     1215
6091        ISECANG=ISECANG+(30)                                          1215
6093        IF((30)*(ISECANG-(360-15)).GT.0) GO TO 6092                   1215
C       Create TPSS :                                                     1216
C       Call To Local Procedure TPSS                                      1216
            ASSIGN 6110 TO BLOCK_TPSS                                     1216
            GO TO  6100                                                   1216
6110        CONTINUE                                                      1216
C       *                                                                 1216
C       Geant POSITION : TPSS ===> ALPHAZ=ISECANG KONLY='MANY' <===       1217
            CALL AGSRESET                                                 1217
            AG_ALPHAN=AG_ALPHAN+1                                         1217
            AG_ALPHAK(AG_ALPHAN)=3                                        1217
            AG_ALPHAL(AG_ALPHAN)=ISECANG                                  1217
            AG_KONLY = 'MANY'                                             1217
            AG_TITLE = 'TPSS'                                             1217
            AG_EXNAME = 'POSITION'                                        1217
            AG_PARLIST = 'ALPHAZ_KONLY'                                   1217
            CALL AXPOSITION                                               1217
C       *                                                                 1217
         GO TO 6091                                                       1218
6092     CONTINUE                                                         1218
5051  CALL AGSPOPB                                                        1219
      GO TO BLOCK_TPGV, (5050 ,5060)                                      1219
C End of Local Procedure                                                  1219
C ----------------------------------------------------------------------  1220
C ------------ TPSS                                                       1220
C Begin Local Procedure TPSS                                              1221
C TPSS IS A DIVISION OF GAS VOLUME CORRESPONDING TO A SUPERSECTORS        1221
6100  CONTINUE                                                            1221
         AG_TITLE='TPSS'                                                  1221
         AG_MODULE=MODULE                                                 1221
         AG_MARK='V00'                                                    1221
         CALL AGSPUSH                                                     1221
         AG_MODULE=MODULE                                                 1221
C    *                                                                    1221
C    Geant ATTRIBUTE : TPSS ===> SEEN=1 COLO=KBLUE <===                   1222
         CALL AGSRESET                                                    1222
         AG_SEEN = 1                                                      1222
         AG_COLO = KBLUE                                                  1222
         AG_TITLE = 'TPSS'                                                1222
         AG_EXNAME = 'ATTRIBUTE'                                          1222
         AG_PARLIST = 'SEEN_COLO'                                         1222
         CALL AXATTRIBUTE                                                 1222
C    *                                                                    1222
C    Geant SHAPE : TUBS ===> PHI1=-15 PHI2=15 <===                        1222
         CALL AGSRESET                                                    1223
         AG_PHI1 = -15                                                    1223
         AG_PHI2 = 15                                                     1223
         AG_TITLE = 'TUBS'                                                1223
         AG_EXNAME = 'SHAPE'                                              1223
         AG_PARLIST = 'PHI1_PHI2'                                         1223
         CALL AXSHAPE                                                     1223
C    *                                                                    1223
C    *                                                                    1223
         KASE=1                                                           1224
            GO TO 6123                                                    1224
6121        KASE=KASE+1                                                   1224
6123        IF(KASE-(2).GT.0) GO TO 6122                                  1224
C       *                                                                 1225
            I_SEC=1                                                       1225
               GO TO 6133                                                 1225
6131           I_SEC=I_SEC+1                                              1225
6133           IF(I_SEC-(2).GT.0) GO TO 6132                              1225
C          use TPRS bank                                                  1226
               BPATH_TPRS='TPRS*'                                         1226
               IDNUM_TPRS(1)=0                                            1226
               TPRS_SEC=I_SEC                                             1226
               CALL AGDGETP('TPCEGEO3A','*',-999,'TPRS',NTPRS_SEC,        1226
     *         RE_TPRS_SEC,BPATH_TPRS,IDNUM_TPRS,1,MAP_TPRS,NAMES_TPRS,   1226
     *         BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)          1226
C          use TECW bank                                                  1227
               BPATH_TECW='TECW*'                                         1227
               IDNUM_TECW(1)=0                                            1227
               TECW_SEC=I_SEC                                             1227
               CALL AGDGETP('TPCEGEO3A','*',-999,'TECW',NTECW_SEC,        1227
     *         RE_TECW_SEC,BPATH_TECW,IDNUM_TECW,1,MAP_TECW,NAMES_TECW,   1227
     *         BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)          1227
               ZWHEEL1 = TPCG_LENGTHW/2 - TPCG_WHEELTHK                   1228
               ZBEG = TPCG_MEMBTHK/2                                      1229
               ZEND = TPCG_ZGROUNDGRID + TPRS_DANODE*2                    1230
               ZDED = TPCG_ZGATINGGRID - TPCG_DEADZONE                    1231
               ZPMT = TPCG_ZGATINGGRID                                    1232
               DX = TPRS_WIDTH/2                                          1233
C          Check KASE==1                                                  1234
               IF (KASE.EQ.1) THEN                                        1234
               DZ = (ZDED - ZBEG)/2                                       1234
               Z = (ZDED + ZBEG)/2 - TPGVZ                                1236
               ENDIF                                                      1237
C          Check KASE==2                                                  1238
               IF (KASE.EQ.2) THEN                                        1238
               DZ = (ZEND - ZPMT)/2                                       1238
               Z = (ZEND + ZPMT)/2 - TPGVZ                                1240
               ENDIF                                                      1241
C          *                                                              1242
               I_ROW=1                                                    1242
                  GO TO 6143                                              1242
6141              I_ROW=I_ROW+1                                           1242
6143              IF(I_ROW-(NINT(TPRS_NROW)).GT.0) GO TO 6142             1242
C             Check (NINT(TPRS_SUPER)==3|I_ROW==1)                        1243
                  IF ((NINT(TPRS_SUPER).EQ.3.OR.I_ROW.EQ.1)) THEN         1243
                  DY=TPRS_NPADS(I_ROW)*TPRS_PITCH/2                       1244
                  X=TPRS_RPADS(I_ROW)-TPRS_WIDTH                          1245
C             Create TPAD :                                               1246
C             Call To Local Procedure TPAD                                1246
                  ASSIGN 6160 TO BLOCK_TPAD                               1246
                  GO TO  6150                                             1246
6160              CONTINUE                                                1246
C             *                                                           1246
C             Geant POSITION : TPAD ===> X=X Z=Z DZ=DZ DX=DX DY=DY <===   1247
                  CALL AGSRESET                                           1247
                  AG_X = X                                                1247
                  AG_Z = Z                                                1247
                  AG_DZ = DZ                                              1247
                  AG_DX = DX                                              1247
                  AG_DY = DY                                              1247
                  AG_TITLE = 'TPAD'                                       1247
                  AG_EXNAME = 'POSITION'                                  1247
                  AG_PARLIST = 'X_Z_DZ_DX_DY'                             1247
                  CALL AXPOSITION                                         1247
C             *                                                           1247
                  ENDIF                                                   1247
                  DY=TPRS_NPADS(I_ROW)*TPRS_PITCH/2                       1249
                  X=TPRS_RPADS(I_ROW)                                     1250
C             Create TPAD :                                               1251
C             Call To Local Procedure TPAD                                1251
                  ASSIGN 6170 TO BLOCK_TPAD                               1251
                  GO TO  6150                                             1251
6170              CONTINUE                                                1251
C             *                                                           1251
C             Geant POSITION : TPAD ===> X=X Z=Z DZ=DZ DX=DX DY=DY <===   1252
                  CALL AGSRESET                                           1252
                  AG_X = X                                                1252
                  AG_Z = Z                                                1252
                  AG_DZ = DZ                                              1252
                  AG_DX = DX                                              1252
                  AG_DY = DY                                              1252
                  AG_TITLE = 'TPAD'                                       1252
                  AG_EXNAME = 'POSITION'                                  1252
                  AG_PARLIST = 'X_Z_DZ_DX_DY'                             1252
                  CALL AXPOSITION                                         1252
C             *                                                           1252
C             Check (NINT(TPRS_SUPER)==3|I_ROW==NINT(TPRS_NROW))          1252
                  IF                                                      1253
     *            ((NINT(TPRS_SUPER).EQ.3.OR.I_ROW.EQ.NINT(TPRS_NROW)))   1253
     *            THEN                                                    1253
                  X=TPRS_RPADS(I_ROW)+TPRS_WIDTH                          1254
                  DY=TPRS_NPADS(I_ROW)*TPRS_PITCH/2                       1255
C             Create TPAD :                                               1256
C             Call To Local Procedure TPAD                                1256
                  ASSIGN 6180 TO BLOCK_TPAD                               1256
                  GO TO  6150                                             1256
6180              CONTINUE                                                1256
C             *                                                           1256
C             Geant POSITION : TPAD ===> X=X Z=Z DZ=DZ DX=DX DY=DY <===   1257
                  CALL AGSRESET                                           1257
                  AG_X = X                                                1257
                  AG_Z = Z                                                1257
                  AG_DZ = DZ                                              1257
                  AG_DX = DX                                              1257
                  AG_DY = DY                                              1257
                  AG_TITLE = 'TPAD'                                       1257
                  AG_EXNAME = 'POSITION'                                  1257
                  AG_PARLIST = 'X_Z_DZ_DX_DY'                             1257
                  CALL AXPOSITION                                         1257
C             *                                                           1257
                  ENDIF                                                   1257
               GO TO 6141                                                 1259
6142           CONTINUE                                                   1259
            GO TO 6131                                                    1260
6132        CONTINUE                                                      1260
         GO TO 6121                                                       1261
6122     CONTINUE                                                         1261
6101  CALL AGSPOPB                                                        1262
      GO TO BLOCK_TPSS, (6100 ,6110)                                      1262
C End of Local Procedure                                                  1262
C ----------------------------------------------------------------------  1263
C ------------ TPAD                                                       1263
C Begin Local Procedure TPAD                                              1264
C TPAD IS A REAL PADROW WITH DIMENSIONS DEFINED AT POSITIONING TIME       1264
6150  CONTINUE                                                            1264
         AG_TITLE='TPAD'                                                  1264
         AG_MODULE=MODULE                                                 1264
         AG_MARK='V00'                                                    1264
         CALL AGSPUSH                                                     1264
         AG_MODULE=MODULE                                                 1264
C    *                                                                    1264
C    *                                                                    1265
C       Check TPRS_WIDTH<1.5                                              1265
            IF (TPRS_WIDTH.LT.1.5) THEN                                   1265
C       Geant ATTRIBUTE : TPAD ===> SEEN=1 COLO=2 SERIAL=1 <===           1266
            CALL AGSRESET                                                 1266
            AG_SEEN = 1                                                   1266
            AG_COLO = 2                                                   1266
            AG_SERIAL = 1                                                 1266
            AG_TITLE = 'TPAD'                                             1266
            AG_EXNAME = 'ATTRIBUTE'                                       1266
            AG_PARLIST = 'SEEN_COLO_SERIAL'                               1266
            CALL AXATTRIBUTE                                              1266
C       *                                                                 1266
         END IF                                                           1267
C    *                                                                    1268
C       Check TPRS_WIDTH>1.5                                              1268
            IF (TPRS_WIDTH.GT.1.5) THEN                                   1268
C       Geant ATTRIBUTE : TPAD ===> SEEN=1 COLO=2 SERIAL=2 <===           1269
            CALL AGSRESET                                                 1269
            AG_SEEN = 1                                                   1269
            AG_COLO = 2                                                   1269
            AG_SERIAL = 2                                                 1269
            AG_TITLE = 'TPAD'                                             1269
            AG_EXNAME = 'ATTRIBUTE'                                       1269
            AG_PARLIST = 'SEEN_COLO_SERIAL'                               1269
            CALL AXATTRIBUTE                                              1269
C       *                                                                 1269
         END IF                                                           1270
C    Geant MATERIAL : P10 ===> inherited arguments used <===              1271
         CALL AGSRESET                                                    1271
         AG_TITLE = 'P10'                                                 1271
         AG_EXNAME = 'MATERIAL'                                           1271
         AG_PARLIST = 'NONE'                                              1271
         CALL AXMATERIAL                                                  1271
C    *                                                                    1271
C    Geant MATERIAL : SENSITIVE_GAS ===> ISVOL=1 STEMAX=2.5*TPRS_WIDTH <  1271
C    ===                                                                  1271
         CALL AGSRESET                                                    1272
         AG_ISVOL = 1                                                     1272
         AG_STEMAX = 2.5*TPRS_WIDTH                                       1272
         AG_TITLE = 'SENSITIVE_GAS'                                       1272
         AG_EXNAME = 'MATERIAL'                                           1272
         AG_PARLIST = 'ISVOL_STEMAX'                                      1272
         CALL AXMATERIAL                                                  1272
C    *                                                                    1272
C    Geant MEDIUM : TPAD_P10 ===> STEMAX=2.5*TPRS_WIDTH DEEMAX=0.0017536  1272
C    7 STMIN=0.0249056 ISVOL=1 <===                                       1272
         CALL AGSRESET                                                    1273
         AG_STEMAX = 2.5*TPRS_WIDTH                                       1273
         AG_DEEMAX = 0.00175367                                           1273
         AG_STMIN = 0.0249056                                             1273
         AG_ISVOL = 1                                                     1273
         AG_TITLE = 'TPAD_P10'                                            1273
         AG_EXNAME = 'MEDIUM'                                             1273
         AG_PARLIST = 'STEMAX_DEEMAX_STMIN_ISVOL'                         1273
         CALL AXMEDIUM                                                    1273
C    *                                                                    1273
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           1273
         CALL AGSRESET                                                    1274
         AG_DX = 0                                                        1274
         AG_DY = 0                                                        1274
         AG_DZ = 0                                                        1274
         AG_TITLE = 'BOX'                                                 1274
         AG_EXNAME = 'SHAPE'                                              1274
         AG_PARLIST = 'DX_DY_DZ'                                          1274
         CALL AXSHAPE                                                     1274
C    *                                                                    1274
         CALL GSTPAR(AG_IMED, 'CUTGAM',1E-4)                              1274
         CALL GSTPAR(AG_IMED, 'CUTELE',1E-4)                              1276
         CALL GSTPAR(AG_IMED, 'DCUTE',1E-4)                               1277
         CALL GSTPAR(AG_IMED, 'DCUTM',1E-4)                               1278
         CALL GSTPAR(AG_IMED, 'STRA', 1.)                                 1279
C    Defining Hits: Z:0.0005:S Y:0.0005: X:0.0005: CX:10: CY:10: CZ:10:   1281
C    LPTOT:18:(-3,2), SLENG:0.1:(0,800) TOF:16:(0,TPCG_TOFMAX) LGAM:16:(  1281
C    -2,+2) STEP:11:(0,10) USER:21:(-0.01,0.01)                           1281
         AG_TITLE = 'TPAD'                                                1281
         AG_EXNAME = 'HITS'                                               1281
         AG_TYPE=AG_MAGIC                                                 1281
         CALL AGSDETV('TPAD')                                             1281
C    *                                                                    1281
         AG_PARLIST='Z'                                                   1281
         AG_TITLE = 'DETH'                                                1281
         AG_NBITS = AG_MAGIC                                              1281
         AG_BIN=0.0005                                                    1281
         AG_OPTION='S'                                                    1281
         AG_FHMIN = AG_MAGIC                                              1281
         AG_FHMAX = AG_MAGIC                                              1281
         CALL AGSDETH (0)                                                 1281
C    *                                                                    1281
         AG_PARLIST='Y'                                                   1281
         AG_TITLE = 'DETH'                                                1281
         AG_NBITS = AG_MAGIC                                              1281
         AG_BIN=0.0005                                                    1281
         AG_FHMIN=AG_MAGIC                                                1281
         AG_FHMAX=AG_MAGIC                                                1281
         AG_OPTION='*'                                                    1281
         CALL AGSDETH (0)                                                 1281
C    *                                                                    1281
         AG_PARLIST='X'                                                   1281
         AG_TITLE = 'DETH'                                                1281
         AG_NBITS = AG_MAGIC                                              1281
         AG_BIN=0.0005                                                    1281
         AG_FHMIN=AG_MAGIC                                                1281
         AG_FHMAX=AG_MAGIC                                                1281
         AG_OPTION='*'                                                    1281
         CALL AGSDETH (0)                                                 1281
C    *                                                                    1281
         AG_PARLIST='CX'                                                  1281
         AG_TITLE = 'DETH'                                                1281
         AG_NBITS = 10                                                    1281
         AG_BIN=AG_MAGIC                                                  1281
         AG_FHMIN=AG_MAGIC                                                1281
         AG_FHMAX=AG_MAGIC                                                1281
         AG_OPTION='*'                                                    1281
         CALL AGSDETH (0)                                                 1281
C    *                                                                    1281
         AG_PARLIST='CY'                                                  1281
         AG_TITLE = 'DETH'                                                1281
         AG_NBITS = 10                                                    1281
         AG_BIN=AG_MAGIC                                                  1281
         AG_FHMIN=AG_MAGIC                                                1281
         AG_FHMAX=AG_MAGIC                                                1281
         AG_OPTION='*'                                                    1281
         CALL AGSDETH (0)                                                 1281
C    *                                                                    1281
         AG_PARLIST='CZ'                                                  1281
         AG_TITLE = 'DETH'                                                1281
         AG_NBITS = 10                                                    1281
         AG_BIN=AG_MAGIC                                                  1281
         AG_FHMIN=AG_MAGIC                                                1281
         AG_FHMAX=AG_MAGIC                                                1281
         AG_OPTION='*'                                                    1281
         CALL AGSDETH (0)                                                 1281
C    *                                                                    1281
         AG_PARLIST='LPTOT'                                               1281
         AG_TITLE = 'DETH'                                                1281
         AG_NBITS = 18                                                    1281
         AG_BIN=AG_MAGIC                                                  1281
         AG_OPTION='*'                                                    1281
         AG_FHMIN = -3                                                    1281
         AG_FHMAX = 2                                                     1281
         CALL AGSDETH (0)                                                 1281
C    *                                                                    1281
         AG_PARLIST='SLENG'                                               1281
         AG_TITLE = 'DETH'                                                1281
         AG_NBITS = AG_MAGIC                                              1281
         AG_BIN=0.1                                                       1281
         AG_OPTION='*'                                                    1281
         AG_FHMIN = 0                                                     1281
         AG_FHMAX = 800                                                   1281
         CALL AGSDETH (0)                                                 1281
C    *                                                                    1281
         AG_PARLIST='TOF'                                                 1281
         AG_TITLE = 'DETH'                                                1281
         AG_NBITS = 16                                                    1281
         AG_BIN=AG_MAGIC                                                  1281
         AG_OPTION='*'                                                    1281
         AG_FHMIN = 0                                                     1281
         AG_FHMAX = TPCG_TOFMAX                                           1281
         CALL AGSDETH (0)                                                 1281
C    *                                                                    1281
         AG_PARLIST='LGAM'                                                1281
         AG_TITLE = 'DETH'                                                1281
         AG_NBITS = 16                                                    1281
         AG_BIN=AG_MAGIC                                                  1281
         AG_OPTION='*'                                                    1281
         AG_FHMIN = -2                                                    1281
         AG_FHMAX = +2                                                    1281
         CALL AGSDETH (0)                                                 1281
C    *                                                                    1281
         AG_PARLIST='STEP'                                                1281
         AG_TITLE = 'DETH'                                                1281
         AG_NBITS = 11                                                    1281
         AG_BIN=AG_MAGIC                                                  1281
         AG_OPTION='*'                                                    1281
         AG_FHMIN = 0                                                     1281
         AG_FHMAX = 10                                                    1281
         CALL AGSDETH (0)                                                 1281
C    *                                                                    1281
         AG_PARLIST='USER'                                                1281
         AG_TITLE = 'DETH'                                                1281
         AG_NBITS = 21                                                    1281
         AG_BIN=AG_MAGIC                                                  1281
         AG_OPTION='*'                                                    1281
         AG_FHMIN = -0.01                                                 1281
         AG_FHMAX = 0.01                                                  1281
         CALL AGSDETH (JUMPAD(TPADSTEP))                                  1281
C    *                                                                    1281
         CALL AGSDETU('TPAD')                                             1281
C    *                                                                    1281
6151  CALL AGSPOPB                                                        1282
      GO TO BLOCK_TPAD, (6150 ,6160,6170,6180)                            1282
C End of Local Procedure                                                  1282
      WRITE(LOUT,6190)'===>>  [End AgML Module tpceGeo3a]  <<==='         1282
6190  FORMAT(A32,/,/)                                                     1282
C MODULE TPCEGEO3A                                                        1284
      END                                                                 1284
