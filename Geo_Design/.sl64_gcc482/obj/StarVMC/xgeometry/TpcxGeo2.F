C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE TPCXGEO2                                                 1
C TPCXGEO2 IS THE UPDATED TPC                                             1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'TPCXGEO2'/, BANK_TITLE/'*'/                            1
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
      REAL TPCG_TPADCONFIG                                                143
      EQUIVALENCE (BANK_TPCG(4),TPCG_TPADCONFIG,RE_TPCG_TPADCONFIG),      143
     *(NAMES_TPCG(2),NTPCG_TPADCONFIG)                                    143
      EQUIVALENCE (COMMEN_TPCG(2),CTPCG_TPADCONFIG),(MAP_TPCG(1,2),       143
     *MTPCG_TPADCONFIG)                                                   143
      CHARACTER CTPCG_TPADCONFIG*40/' '/                                  143
      CHARACTER NTPCG_TPADCONFIG*12/'R.TPADCONFIG'/                       143
      INTEGER MTPCG_TPADCONFIG(2)/1,1/                                    143
      REAL RE_TPCG_TPADCONFIG                                             143
C -                                                                       143
      REAL TPCG_RMIN                                                      143
      EQUIVALENCE (BANK_TPCG(5),TPCG_RMIN,RE_TPCG_RMIN),(NAMES_TPCG(3),   143
     *NTPCG_RMIN)                                                         143
      EQUIVALENCE (COMMEN_TPCG(3),CTPCG_RMIN),(MAP_TPCG(1,3),             143
     *MTPCG_RMIN)                                                         143
      CHARACTER CTPCG_RMIN*40/' '/                                        143
      CHARACTER NTPCG_RMIN*12/'R.RMIN'/                                   143
      INTEGER MTPCG_RMIN(2)/1,1/                                          143
      REAL RE_TPCG_RMIN                                                   143
C -                                                                       143
      REAL TPCG_RMAX                                                      143
      EQUIVALENCE (BANK_TPCG(6),TPCG_RMAX,RE_TPCG_RMAX),(NAMES_TPCG(4),   143
     *NTPCG_RMAX)                                                         143
      EQUIVALENCE (COMMEN_TPCG(4),CTPCG_RMAX),(MAP_TPCG(1,4),             143
     *MTPCG_RMAX)                                                         143
      CHARACTER CTPCG_RMAX*40/' '/                                        143
      CHARACTER NTPCG_RMAX*12/'R.RMAX'/                                   143
      INTEGER MTPCG_RMAX(2)/1,1/                                          143
      REAL RE_TPCG_RMAX                                                   143
C -                                                                       143
      REAL TPCG_RMINIFC                                                   143
      EQUIVALENCE (BANK_TPCG(7),TPCG_RMINIFC,RE_TPCG_RMINIFC),            143
     *(NAMES_TPCG(5),NTPCG_RMINIFC)                                       143
      EQUIVALENCE (COMMEN_TPCG(5),CTPCG_RMINIFC),(MAP_TPCG(1,5),          143
     *MTPCG_RMINIFC)                                                      143
      CHARACTER CTPCG_RMINIFC*40/' '/                                     143
      CHARACTER NTPCG_RMINIFC*12/'R.RMINIFC'/                             143
      INTEGER MTPCG_RMINIFC(2)/1,1/                                       143
      REAL RE_TPCG_RMINIFC                                                143
C -                                                                       143
      REAL TPCG_LENGTHT                                                   143
      EQUIVALENCE (BANK_TPCG(8),TPCG_LENGTHT,RE_TPCG_LENGTHT),            143
     *(NAMES_TPCG(6),NTPCG_LENGTHT)                                       143
      EQUIVALENCE (COMMEN_TPCG(6),CTPCG_LENGTHT),(MAP_TPCG(1,6),          143
     *MTPCG_LENGTHT)                                                      143
      CHARACTER CTPCG_LENGTHT*40/' '/                                     143
      CHARACTER NTPCG_LENGTHT*12/'R.LENGTHT'/                             143
      INTEGER MTPCG_LENGTHT(2)/1,1/                                       143
      REAL RE_TPCG_LENGTHT                                                143
C -                                                                       143
      REAL TPCG_LENGTH                                                    143
      EQUIVALENCE (BANK_TPCG(9),TPCG_LENGTH,RE_TPCG_LENGTH),              143
     *(NAMES_TPCG(7),NTPCG_LENGTH)                                        143
      EQUIVALENCE (COMMEN_TPCG(7),CTPCG_LENGTH),(MAP_TPCG(1,7),           143
     *MTPCG_LENGTH)                                                       143
      CHARACTER CTPCG_LENGTH*40/' '/                                      143
      CHARACTER NTPCG_LENGTH*12/'R.LENGTH'/                               143
      INTEGER MTPCG_LENGTH(2)/1,1/                                        143
      REAL RE_TPCG_LENGTH                                                 143
C -                                                                       143
      REAL TPCG_LENGTHW                                                   143
      EQUIVALENCE (BANK_TPCG(10),TPCG_LENGTHW,RE_TPCG_LENGTHW),           143
     *(NAMES_TPCG(8),NTPCG_LENGTHW)                                       143
      EQUIVALENCE (COMMEN_TPCG(8),CTPCG_LENGTHW),(MAP_TPCG(1,8),          143
     *MTPCG_LENGTHW)                                                      143
      CHARACTER CTPCG_LENGTHW*40/' '/                                     143
      CHARACTER NTPCG_LENGTHW*12/'R.LENGTHW'/                             143
      INTEGER MTPCG_LENGTHW(2)/1,1/                                       143
      REAL RE_TPCG_LENGTHW                                                143
C -                                                                       143
      REAL TPCG_LENGTHV                                                   143
      EQUIVALENCE (BANK_TPCG(11),TPCG_LENGTHV,RE_TPCG_LENGTHV),           143
     *(NAMES_TPCG(9),NTPCG_LENGTHV)                                       143
      EQUIVALENCE (COMMEN_TPCG(9),CTPCG_LENGTHV),(MAP_TPCG(1,9),          143
     *MTPCG_LENGTHV)                                                      143
      CHARACTER CTPCG_LENGTHV*40/' '/                                     143
      CHARACTER NTPCG_LENGTHV*12/'R.LENGTHV'/                             143
      INTEGER MTPCG_LENGTHV(2)/1,1/                                       143
      REAL RE_TPCG_LENGTHV                                                143
C -                                                                       143
      REAL TPCG_DZENVELOP                                                 143
      EQUIVALENCE (BANK_TPCG(12),TPCG_DZENVELOP,RE_TPCG_DZENVELOP),       143
     *(NAMES_TPCG(10),NTPCG_DZENVELOP)                                    143
      EQUIVALENCE (COMMEN_TPCG(10),CTPCG_DZENVELOP),(MAP_TPCG(1,10),      143
     *MTPCG_DZENVELOP)                                                    143
      CHARACTER CTPCG_DZENVELOP*40/' '/                                   143
      CHARACTER NTPCG_DZENVELOP*12/'R.DZENVELOP'/                         143
      INTEGER MTPCG_DZENVELOP(2)/1,1/                                     143
      REAL RE_TPCG_DZENVELOP                                              143
C -                                                                       143
      REAL TPCG_WHEELIR                                                   143
      EQUIVALENCE (BANK_TPCG(13),TPCG_WHEELIR,RE_TPCG_WHEELIR),           143
     *(NAMES_TPCG(11),NTPCG_WHEELIR)                                      143
      EQUIVALENCE (COMMEN_TPCG(11),CTPCG_WHEELIR),(MAP_TPCG(1,11),        143
     *MTPCG_WHEELIR)                                                      143
      CHARACTER CTPCG_WHEELIR*40/' '/                                     143
      CHARACTER NTPCG_WHEELIR*12/'R.WHEELIR'/                             143
      INTEGER MTPCG_WHEELIR(2)/1,1/                                       143
      REAL RE_TPCG_WHEELIR                                                143
C -                                                                       143
      REAL TPCG_WHEELR0                                                   143
      EQUIVALENCE (BANK_TPCG(14),TPCG_WHEELR0,RE_TPCG_WHEELR0),           143
     *(NAMES_TPCG(12),NTPCG_WHEELR0)                                      143
      EQUIVALENCE (COMMEN_TPCG(12),CTPCG_WHEELR0),(MAP_TPCG(1,12),        143
     *MTPCG_WHEELR0)                                                      143
      CHARACTER CTPCG_WHEELR0*40/' '/                                     143
      CHARACTER NTPCG_WHEELR0*12/'R.WHEELR0'/                             143
      INTEGER MTPCG_WHEELR0(2)/1,1/                                       143
      REAL RE_TPCG_WHEELR0                                                143
C -                                                                       143
      REAL TPCG_WHEELR1                                                   143
      EQUIVALENCE (BANK_TPCG(15),TPCG_WHEELR1,RE_TPCG_WHEELR1),           143
     *(NAMES_TPCG(13),NTPCG_WHEELR1)                                      143
      EQUIVALENCE (COMMEN_TPCG(13),CTPCG_WHEELR1),(MAP_TPCG(1,13),        143
     *MTPCG_WHEELR1)                                                      143
      CHARACTER CTPCG_WHEELR1*40/' '/                                     143
      CHARACTER NTPCG_WHEELR1*12/'R.WHEELR1'/                             143
      INTEGER MTPCG_WHEELR1(2)/1,1/                                       143
      REAL RE_TPCG_WHEELR1                                                143
C -                                                                       143
      REAL TPCG_WHEELR2                                                   143
      EQUIVALENCE (BANK_TPCG(16),TPCG_WHEELR2,RE_TPCG_WHEELR2),           143
     *(NAMES_TPCG(14),NTPCG_WHEELR2)                                      143
      EQUIVALENCE (COMMEN_TPCG(14),CTPCG_WHEELR2),(MAP_TPCG(1,14),        143
     *MTPCG_WHEELR2)                                                      143
      CHARACTER CTPCG_WHEELR2*40/' '/                                     143
      CHARACTER NTPCG_WHEELR2*12/'R.WHEELR2'/                             143
      INTEGER MTPCG_WHEELR2(2)/1,1/                                       143
      REAL RE_TPCG_WHEELR2                                                143
C -                                                                       143
      REAL TPCG_WHEELTOTALRIBWIDTH                                        143
      EQUIVALENCE (BANK_TPCG(17),TPCG_WHEELTOTALRIBWIDTH,                 143
     *RE_TPCG_WHEELTOTALRIBWIDTH),(NAMES_TPCG(15),                        143
     *NTPCG_WHEELTOTALRIBWIDTH)                                           143
      EQUIVALENCE (COMMEN_TPCG(15),CTPCG_WHEELTOTALRIBWIDTH),             143
     *(MAP_TPCG(1,15),MTPCG_WHEELTOTALRIBWIDTH)                           143
      CHARACTER CTPCG_WHEELTOTALRIBWIDTH*40/' '/                          143
      CHARACTER NTPCG_WHEELTOTALRIBWIDTH*12/'R.WHEELTOTALRIBWIDTH'/       143
      INTEGER MTPCG_WHEELTOTALRIBWIDTH(2)/1,1/                            143
      REAL RE_TPCG_WHEELTOTALRIBWIDTH                                     143
C -                                                                       143
      REAL TPCG_WHEELRIBWIDTH                                             143
      EQUIVALENCE (BANK_TPCG(18),TPCG_WHEELRIBWIDTH,                      143
     *RE_TPCG_WHEELRIBWIDTH),(NAMES_TPCG(16),NTPCG_WHEELRIBWIDTH)         143
      EQUIVALENCE (COMMEN_TPCG(16),CTPCG_WHEELRIBWIDTH),(MAP_TPCG(1,16),  143
     *MTPCG_WHEELRIBWIDTH)                                                143
      CHARACTER CTPCG_WHEELRIBWIDTH*40/' '/                               143
      CHARACTER NTPCG_WHEELRIBWIDTH*12/'R.WHEELRIBWIDTH'/                 143
      INTEGER MTPCG_WHEELRIBWIDTH(2)/1,1/                                 143
      REAL RE_TPCG_WHEELRIBWIDTH                                          143
C -                                                                       143
      REAL TPCG_WHEELRIBHEIGHT                                            143
      EQUIVALENCE (BANK_TPCG(19),TPCG_WHEELRIBHEIGHT,                     143
     *RE_TPCG_WHEELRIBHEIGHT),(NAMES_TPCG(17),NTPCG_WHEELRIBHEIGHT)       143
      EQUIVALENCE (COMMEN_TPCG(17),CTPCG_WHEELRIBHEIGHT),(MAP_TPCG(1,     143
     *17),MTPCG_WHEELRIBHEIGHT)                                           143
      CHARACTER CTPCG_WHEELRIBHEIGHT*40/' '/                              143
      CHARACTER NTPCG_WHEELRIBHEIGHT*12/'R.WHEELRIBHEIGHT'/               143
      INTEGER MTPCG_WHEELRIBHEIGHT(2)/1,1/                                143
      REAL RE_TPCG_WHEELRIBHEIGHT                                         143
C -                                                                       143
      REAL TPCG_WHEELBOXDX                                                143
      EQUIVALENCE (BANK_TPCG(20),TPCG_WHEELBOXDX,RE_TPCG_WHEELBOXDX),     143
     *(NAMES_TPCG(18),NTPCG_WHEELBOXDX)                                   143
      EQUIVALENCE (COMMEN_TPCG(18),CTPCG_WHEELBOXDX),(MAP_TPCG(1,18),     143
     *MTPCG_WHEELBOXDX)                                                   143
      CHARACTER CTPCG_WHEELBOXDX*40/' '/                                  143
      CHARACTER NTPCG_WHEELBOXDX*12/'R.WHEELBOXDX'/                       143
      INTEGER MTPCG_WHEELBOXDX(2)/1,1/                                    143
      REAL RE_TPCG_WHEELBOXDX                                             143
C -                                                                       143
      REAL TPCG_WHEELBOXDY                                                143
      EQUIVALENCE (BANK_TPCG(21),TPCG_WHEELBOXDY,RE_TPCG_WHEELBOXDY),     143
     *(NAMES_TPCG(19),NTPCG_WHEELBOXDY)                                   143
      EQUIVALENCE (COMMEN_TPCG(19),CTPCG_WHEELBOXDY),(MAP_TPCG(1,19),     143
     *MTPCG_WHEELBOXDY)                                                   143
      CHARACTER CTPCG_WHEELBOXDY*40/' '/                                  143
      CHARACTER NTPCG_WHEELBOXDY*12/'R.WHEELBOXDY'/                       143
      INTEGER MTPCG_WHEELBOXDY(2)/1,1/                                    143
      REAL RE_TPCG_WHEELBOXDY                                             143
C -                                                                       143
      REAL TPCG_WHEELOR1                                                  143
      EQUIVALENCE (BANK_TPCG(22),TPCG_WHEELOR1,RE_TPCG_WHEELOR1),         143
     *(NAMES_TPCG(20),NTPCG_WHEELOR1)                                     143
      EQUIVALENCE (COMMEN_TPCG(20),CTPCG_WHEELOR1),(MAP_TPCG(1,20),       143
     *MTPCG_WHEELOR1)                                                     143
      CHARACTER CTPCG_WHEELOR1*40/' '/                                    143
      CHARACTER NTPCG_WHEELOR1*12/'R.WHEELOR1'/                           143
      INTEGER MTPCG_WHEELOR1(2)/1,1/                                      143
      REAL RE_TPCG_WHEELOR1                                               143
C -                                                                       143
      REAL TPCG_WHEELOR                                                   143
      EQUIVALENCE (BANK_TPCG(23),TPCG_WHEELOR,RE_TPCG_WHEELOR),           143
     *(NAMES_TPCG(21),NTPCG_WHEELOR)                                      143
      EQUIVALENCE (COMMEN_TPCG(21),CTPCG_WHEELOR),(MAP_TPCG(1,21),        143
     *MTPCG_WHEELOR)                                                      143
      CHARACTER CTPCG_WHEELOR*40/' '/                                     143
      CHARACTER NTPCG_WHEELOR*12/'R.WHEELOR'/                             143
      INTEGER MTPCG_WHEELOR(2)/1,1/                                       143
      REAL RE_TPCG_WHEELOR                                                143
C -                                                                       143
      REAL TPCG_WHEELTHK1                                                 143
      EQUIVALENCE (BANK_TPCG(24),TPCG_WHEELTHK1,RE_TPCG_WHEELTHK1),       143
     *(NAMES_TPCG(22),NTPCG_WHEELTHK1)                                    143
      EQUIVALENCE (COMMEN_TPCG(22),CTPCG_WHEELTHK1),(MAP_TPCG(1,22),      143
     *MTPCG_WHEELTHK1)                                                    143
      CHARACTER CTPCG_WHEELTHK1*40/' '/                                   143
      CHARACTER NTPCG_WHEELTHK1*12/'R.WHEELTHK1'/                         143
      INTEGER MTPCG_WHEELTHK1(2)/1,1/                                     143
      REAL RE_TPCG_WHEELTHK1                                              143
C -                                                                       143
      REAL TPCG_WHEELTHK                                                  143
      EQUIVALENCE (BANK_TPCG(25),TPCG_WHEELTHK,RE_TPCG_WHEELTHK),         143
     *(NAMES_TPCG(23),NTPCG_WHEELTHK)                                     143
      EQUIVALENCE (COMMEN_TPCG(23),CTPCG_WHEELTHK),(MAP_TPCG(1,23),       143
     *MTPCG_WHEELTHK)                                                     143
      CHARACTER CTPCG_WHEELTHK*40/' '/                                    143
      CHARACTER NTPCG_WHEELTHK*12/'R.WHEELTHK'/                           143
      INTEGER MTPCG_WHEELTHK(2)/1,1/                                      143
      REAL RE_TPCG_WHEELTHK                                               143
C -                                                                       143
      REAL TPCG_SENGASOR                                                  143
      EQUIVALENCE (BANK_TPCG(26),TPCG_SENGASOR,RE_TPCG_SENGASOR),         143
     *(NAMES_TPCG(24),NTPCG_SENGASOR)                                     143
      EQUIVALENCE (COMMEN_TPCG(24),CTPCG_SENGASOR),(MAP_TPCG(1,24),       143
     *MTPCG_SENGASOR)                                                     143
      CHARACTER CTPCG_SENGASOR*40/' '/                                    143
      CHARACTER NTPCG_SENGASOR*12/'R.SENGASOR'/                           143
      INTEGER MTPCG_SENGASOR(2)/1,1/                                      143
      REAL RE_TPCG_SENGASOR                                               143
C -                                                                       143
      REAL TPCG_MEMBTHK                                                   143
      EQUIVALENCE (BANK_TPCG(27),TPCG_MEMBTHK,RE_TPCG_MEMBTHK),           143
     *(NAMES_TPCG(25),NTPCG_MEMBTHK)                                      143
      EQUIVALENCE (COMMEN_TPCG(25),CTPCG_MEMBTHK),(MAP_TPCG(1,25),        143
     *MTPCG_MEMBTHK)                                                      143
      CHARACTER CTPCG_MEMBTHK*40/' '/                                     143
      CHARACTER NTPCG_MEMBTHK*12/'R.MEMBTHK'/                             143
      INTEGER MTPCG_MEMBTHK(2)/1,1/                                       143
      REAL RE_TPCG_MEMBTHK                                                143
C -                                                                       143
      REAL TPCG_TOCSDR                                                    143
      EQUIVALENCE (BANK_TPCG(28),TPCG_TOCSDR,RE_TPCG_TOCSDR),             143
     *(NAMES_TPCG(26),NTPCG_TOCSDR)                                       143
      EQUIVALENCE (COMMEN_TPCG(26),CTPCG_TOCSDR),(MAP_TPCG(1,26),         143
     *MTPCG_TOCSDR)                                                       143
      CHARACTER CTPCG_TOCSDR*40/' '/                                      143
      CHARACTER NTPCG_TOCSDR*12/'R.TOCSDR'/                               143
      INTEGER MTPCG_TOCSDR(2)/1,1/                                        143
      REAL RE_TPCG_TOCSDR                                                 143
C -                                                                       143
      REAL TPCG_TOKADR                                                    143
      EQUIVALENCE (BANK_TPCG(29),TPCG_TOKADR,RE_TPCG_TOKADR),             143
     *(NAMES_TPCG(27),NTPCG_TOKADR)                                       143
      EQUIVALENCE (COMMEN_TPCG(27),CTPCG_TOKADR),(MAP_TPCG(1,27),         143
     *MTPCG_TOKADR)                                                       143
      CHARACTER CTPCG_TOKADR*40/' '/                                      143
      CHARACTER NTPCG_TOKADR*12/'R.TOKADR'/                               143
      INTEGER MTPCG_TOKADR(2)/1,1/                                        143
      REAL RE_TPCG_TOKADR                                                 143
C -                                                                       143
      REAL TPCG_TONXDR                                                    143
      EQUIVALENCE (BANK_TPCG(30),TPCG_TONXDR,RE_TPCG_TONXDR),             143
     *(NAMES_TPCG(28),NTPCG_TONXDR)                                       143
      EQUIVALENCE (COMMEN_TPCG(28),CTPCG_TONXDR),(MAP_TPCG(1,28),         143
     *MTPCG_TONXDR)                                                       143
      CHARACTER CTPCG_TONXDR*40/' '/                                      143
      CHARACTER NTPCG_TONXDR*12/'R.TONXDR'/                               143
      INTEGER MTPCG_TONXDR(2)/1,1/                                        143
      REAL RE_TPCG_TONXDR                                                 143
C -                                                                       143
      REAL TPCG_TOADDR                                                    143
      EQUIVALENCE (BANK_TPCG(31),TPCG_TOADDR,RE_TPCG_TOADDR),             143
     *(NAMES_TPCG(29),NTPCG_TOADDR)                                       143
      EQUIVALENCE (COMMEN_TPCG(29),CTPCG_TOADDR),(MAP_TPCG(1,29),         143
     *MTPCG_TOADDR)                                                       143
      CHARACTER CTPCG_TOADDR*40/' '/                                      143
      CHARACTER NTPCG_TOADDR*12/'R.TOADDR'/                               143
      INTEGER MTPCG_TOADDR(2)/1,1/                                        143
      REAL RE_TPCG_TOADDR                                                 143
C -                                                                       143
      REAL TPCG_TOIGDR                                                    143
      EQUIVALENCE (BANK_TPCG(32),TPCG_TOIGDR,RE_TPCG_TOIGDR),             143
     *(NAMES_TPCG(30),NTPCG_TOIGDR)                                       143
      EQUIVALENCE (COMMEN_TPCG(30),CTPCG_TOIGDR),(MAP_TPCG(1,30),         143
     *MTPCG_TOIGDR)                                                       143
      CHARACTER CTPCG_TOIGDR*40/' '/                                      143
      CHARACTER NTPCG_TOIGDR*12/'R.TOIGDR'/                               143
      INTEGER MTPCG_TOIGDR(2)/1,1/                                        143
      REAL RE_TPCG_TOIGDR                                                 143
C -                                                                       143
      REAL TPCG_TOALDR                                                    143
      EQUIVALENCE (BANK_TPCG(33),TPCG_TOALDR,RE_TPCG_TOALDR),             143
     *(NAMES_TPCG(31),NTPCG_TOALDR)                                       143
      EQUIVALENCE (COMMEN_TPCG(31),CTPCG_TOALDR),(MAP_TPCG(1,31),         143
     *MTPCG_TOALDR)                                                       143
      CHARACTER CTPCG_TOALDR*40/' '/                                      143
      CHARACTER NTPCG_TOALDR*12/'R.TOALDR'/                               143
      INTEGER MTPCG_TOALDR(2)/1,1/                                        143
      REAL RE_TPCG_TOALDR                                                 143
C -                                                                       143
      REAL TPCG_TOHADR                                                    143
      EQUIVALENCE (BANK_TPCG(34),TPCG_TOHADR,RE_TPCG_TOHADR),             143
     *(NAMES_TPCG(32),NTPCG_TOHADR)                                       143
      EQUIVALENCE (COMMEN_TPCG(32),CTPCG_TOHADR),(MAP_TPCG(1,32),         143
     *MTPCG_TOHADR)                                                       143
      CHARACTER CTPCG_TOHADR*40/' '/                                      143
      CHARACTER NTPCG_TOHADR*12/'R.TOHADR'/                               143
      INTEGER MTPCG_TOHADR(2)/1,1/                                        143
      REAL RE_TPCG_TOHADR                                                 143
C -                                                                       143
      REAL TPCG_TIADDR                                                    143
      EQUIVALENCE (BANK_TPCG(35),TPCG_TIADDR,RE_TPCG_TIADDR),             143
     *(NAMES_TPCG(33),NTPCG_TIADDR)                                       143
      EQUIVALENCE (COMMEN_TPCG(33),CTPCG_TIADDR),(MAP_TPCG(1,33),         143
     *MTPCG_TIADDR)                                                       143
      CHARACTER CTPCG_TIADDR*40/' '/                                      143
      CHARACTER NTPCG_TIADDR*12/'R.TIADDR'/                               143
      INTEGER MTPCG_TIADDR(2)/1,1/                                        143
      REAL RE_TPCG_TIADDR                                                 143
C -                                                                       143
      REAL TPCG_TINXDR                                                    143
      EQUIVALENCE (BANK_TPCG(36),TPCG_TINXDR,RE_TPCG_TINXDR),             143
     *(NAMES_TPCG(34),NTPCG_TINXDR)                                       143
      EQUIVALENCE (COMMEN_TPCG(34),CTPCG_TINXDR),(MAP_TPCG(1,34),         143
     *MTPCG_TINXDR)                                                       143
      CHARACTER CTPCG_TINXDR*40/' '/                                      143
      CHARACTER NTPCG_TINXDR*12/'R.TINXDR'/                               143
      INTEGER MTPCG_TINXDR(2)/1,1/                                        143
      REAL RE_TPCG_TINXDR                                                 143
C -                                                                       143
      REAL TPCG_TIKADR                                                    143
      EQUIVALENCE (BANK_TPCG(37),TPCG_TIKADR,RE_TPCG_TIKADR),             143
     *(NAMES_TPCG(35),NTPCG_TIKADR)                                       143
      EQUIVALENCE (COMMEN_TPCG(35),CTPCG_TIKADR),(MAP_TPCG(1,35),         143
     *MTPCG_TIKADR)                                                       143
      CHARACTER CTPCG_TIKADR*40/' '/                                      143
      CHARACTER NTPCG_TIKADR*12/'R.TIKADR'/                               143
      INTEGER MTPCG_TIKADR(2)/1,1/                                        143
      REAL RE_TPCG_TIKADR                                                 143
C -                                                                       143
      REAL TPCG_TIALDR                                                    143
      EQUIVALENCE (BANK_TPCG(38),TPCG_TIALDR,RE_TPCG_TIALDR),             143
     *(NAMES_TPCG(36),NTPCG_TIALDR)                                       143
      EQUIVALENCE (COMMEN_TPCG(36),CTPCG_TIALDR),(MAP_TPCG(1,36),         143
     *MTPCG_TIALDR)                                                       143
      CHARACTER CTPCG_TIALDR*40/' '/                                      143
      CHARACTER NTPCG_TIALDR*12/'R.TIALDR'/                               143
      INTEGER MTPCG_TIALDR(2)/1,1/                                        143
      REAL RE_TPCG_TIALDR                                                 143
C -                                                                       143
      REAL TPCG_TIFCRF                                                    143
      EQUIVALENCE (BANK_TPCG(39),TPCG_TIFCRF,RE_TPCG_TIFCRF),             143
     *(NAMES_TPCG(37),NTPCG_TIFCRF)                                       143
      EQUIVALENCE (COMMEN_TPCG(37),CTPCG_TIFCRF),(MAP_TPCG(1,37),         143
     *MTPCG_TIFCRF)                                                       143
      CHARACTER CTPCG_TIFCRF*40/' '/                                      143
      CHARACTER NTPCG_TIFCRF*12/'R.TIFCRF'/                               143
      INTEGER MTPCG_TIFCRF(2)/1,1/                                        143
      REAL RE_TPCG_TIFCRF                                                 143
C -                                                                       143
      REAL TPCG_TIFCDRT                                                   143
      EQUIVALENCE (BANK_TPCG(40),TPCG_TIFCDRT,RE_TPCG_TIFCDRT),           143
     *(NAMES_TPCG(38),NTPCG_TIFCDRT)                                      143
      EQUIVALENCE (COMMEN_TPCG(38),CTPCG_TIFCDRT),(MAP_TPCG(1,38),        143
     *MTPCG_TIFCDRT)                                                      143
      CHARACTER CTPCG_TIFCDRT*40/' '/                                     143
      CHARACTER NTPCG_TIFCDRT*12/'R.TIFCDRT'/                             143
      INTEGER MTPCG_TIFCDRT(2)/1,1/                                       143
      REAL RE_TPCG_TIFCDRT                                                143
C -                                                                       143
      REAL TPCG_DZYF1                                                     143
      EQUIVALENCE (BANK_TPCG(41),TPCG_DZYF1,RE_TPCG_DZYF1),               143
     *(NAMES_TPCG(39),NTPCG_DZYF1)                                        143
      EQUIVALENCE (COMMEN_TPCG(39),CTPCG_DZYF1),(MAP_TPCG(1,39),          143
     *MTPCG_DZYF1)                                                        143
      CHARACTER CTPCG_DZYF1*40/' '/                                       143
      CHARACTER NTPCG_DZYF1*12/'R.DZYF1'/                                 143
      INTEGER MTPCG_DZYF1(2)/1,1/                                         143
      REAL RE_TPCG_DZYF1                                                  143
C -                                                                       143
      REAL TPCG_DZYF2                                                     143
      EQUIVALENCE (BANK_TPCG(42),TPCG_DZYF2,RE_TPCG_DZYF2),               143
     *(NAMES_TPCG(40),NTPCG_DZYF2)                                        143
      EQUIVALENCE (COMMEN_TPCG(40),CTPCG_DZYF2),(MAP_TPCG(1,40),          143
     *MTPCG_DZYF2)                                                        143
      CHARACTER CTPCG_DZYF2*40/' '/                                       143
      CHARACTER NTPCG_DZYF2*12/'R.DZYF2'/                                 143
      INTEGER MTPCG_DZYF2(2)/1,1/                                         143
      REAL RE_TPCG_DZYF2                                                  143
C -                                                                       143
      REAL TPCG_DZYF3                                                     143
      EQUIVALENCE (BANK_TPCG(43),TPCG_DZYF3,RE_TPCG_DZYF3),               143
     *(NAMES_TPCG(41),NTPCG_DZYF3)                                        143
      EQUIVALENCE (COMMEN_TPCG(41),CTPCG_DZYF3),(MAP_TPCG(1,41),          143
     *MTPCG_DZYF3)                                                        143
      CHARACTER CTPCG_DZYF3*40/' '/                                       143
      CHARACTER NTPCG_DZYF3*12/'R.DZYF3'/                                 143
      INTEGER MTPCG_DZYF3(2)/1,1/                                         143
      REAL RE_TPCG_DZYF3                                                  143
C -                                                                       143
      REAL TPCG_PADPLANETHICKNESS                                         143
      EQUIVALENCE (BANK_TPCG(44),TPCG_PADPLANETHICKNESS,                  143
     *RE_TPCG_PADPLANETHICKNESS),(NAMES_TPCG(42),                         143
     *NTPCG_PADPLANETHICKNESS)                                            143
      EQUIVALENCE (COMMEN_TPCG(42),CTPCG_PADPLANETHICKNESS),(MAP_TPCG(1,  143
     *42),MTPCG_PADPLANETHICKNESS)                                        143
      CHARACTER CTPCG_PADPLANETHICKNESS*40/' '/                           143
      CHARACTER NTPCG_PADPLANETHICKNESS*12/'R.PADPLANETHICKNESS'/         143
      INTEGER MTPCG_PADPLANETHICKNESS(2)/1,1/                             143
      REAL RE_TPCG_PADPLANETHICKNESS                                      143
C -                                                                       143
      REAL TPCG_DGATEGROUND                                               143
      EQUIVALENCE (BANK_TPCG(45),TPCG_DGATEGROUND,RE_TPCG_DGATEGROUND),   143
     *(NAMES_TPCG(43),NTPCG_DGATEGROUND)                                  143
      EQUIVALENCE (COMMEN_TPCG(43),CTPCG_DGATEGROUND),(MAP_TPCG(1,43),    143
     *MTPCG_DGATEGROUND)                                                  143
      CHARACTER CTPCG_DGATEGROUND*40/' '/                                 143
      CHARACTER NTPCG_DGATEGROUND*12/'R.DGATEGROUND'/                     143
      INTEGER MTPCG_DGATEGROUND(2)/1,1/                                   143
      REAL RE_TPCG_DGATEGROUND                                            143
C -                                                                       143
      REAL TPCG_WIREMOUNTWIDTH                                            143
      EQUIVALENCE (BANK_TPCG(46),TPCG_WIREMOUNTWIDTH,                     143
     *RE_TPCG_WIREMOUNTWIDTH),(NAMES_TPCG(44),NTPCG_WIREMOUNTWIDTH)       143
      EQUIVALENCE (COMMEN_TPCG(44),CTPCG_WIREMOUNTWIDTH),(MAP_TPCG(1,     143
     *44),MTPCG_WIREMOUNTWIDTH)                                           143
      CHARACTER CTPCG_WIREMOUNTWIDTH*40/' '/                              143
      CHARACTER NTPCG_WIREMOUNTWIDTH*12/'R.WIREMOUNTWIDTH'/               143
      INTEGER MTPCG_WIREMOUNTWIDTH(2)/1,1/                                143
      REAL RE_TPCG_WIREMOUNTWIDTH                                         143
C -                                                                       143
      REAL TPCG_WIREMOUNTHEIGHT                                           143
      EQUIVALENCE (BANK_TPCG(47),TPCG_WIREMOUNTHEIGHT,                    143
     *RE_TPCG_WIREMOUNTHEIGHT),(NAMES_TPCG(45),NTPCG_WIREMOUNTHEIGHT)     143
      EQUIVALENCE (COMMEN_TPCG(45),CTPCG_WIREMOUNTHEIGHT),(MAP_TPCG(1,    143
     *45),MTPCG_WIREMOUNTHEIGHT)                                          143
      CHARACTER CTPCG_WIREMOUNTHEIGHT*40/' '/                             143
      CHARACTER NTPCG_WIREMOUNTHEIGHT*12/'R.WIREMOUNTHEIGHT'/             143
      INTEGER MTPCG_WIREMOUNTHEIGHT(2)/1,1/                               143
      REAL RE_TPCG_WIREMOUNTHEIGHT                                        143
C -                                                                       143
      REAL TPCG_DXRDO                                                     143
      EQUIVALENCE (BANK_TPCG(48),TPCG_DXRDO,RE_TPCG_DXRDO),               143
     *(NAMES_TPCG(46),NTPCG_DXRDO)                                        143
      EQUIVALENCE (COMMEN_TPCG(46),CTPCG_DXRDO),(MAP_TPCG(1,46),          143
     *MTPCG_DXRDO)                                                        143
      CHARACTER CTPCG_DXRDO*40/' '/                                       143
      CHARACTER NTPCG_DXRDO*12/'R.DXRDO'/                                 143
      INTEGER MTPCG_DXRDO(2)/1,1/                                         143
      REAL RE_TPCG_DXRDO                                                  143
C -                                                                       143
      REAL TPCG_DYRDO                                                     143
      EQUIVALENCE (BANK_TPCG(49),TPCG_DYRDO,RE_TPCG_DYRDO),               143
     *(NAMES_TPCG(47),NTPCG_DYRDO)                                        143
      EQUIVALENCE (COMMEN_TPCG(47),CTPCG_DYRDO),(MAP_TPCG(1,47),          143
     *MTPCG_DYRDO)                                                        143
      CHARACTER CTPCG_DYRDO*40/' '/                                       143
      CHARACTER NTPCG_DYRDO*12/'R.DYRDO'/                                 143
      INTEGER MTPCG_DYRDO(2)/1,1/                                         143
      REAL RE_TPCG_DYRDO                                                  143
C -                                                                       143
      REAL TPCG_DZRDO                                                     143
      EQUIVALENCE (BANK_TPCG(50),TPCG_DZRDO,RE_TPCG_DZRDO),               143
     *(NAMES_TPCG(48),NTPCG_DZRDO)                                        143
      EQUIVALENCE (COMMEN_TPCG(48),CTPCG_DZRDO),(MAP_TPCG(1,48),          143
     *MTPCG_DZRDO)                                                        143
      CHARACTER CTPCG_DZRDO*40/' '/                                       143
      CHARACTER NTPCG_DZRDO*12/'R.DZRDO'/                                 143
      INTEGER MTPCG_DZRDO(2)/1,1/                                         143
      REAL RE_TPCG_DZRDO                                                  143
C -                                                                       143
      REAL TPCG_ZRDO                                                      143
      EQUIVALENCE (BANK_TPCG(51),TPCG_ZRDO,RE_TPCG_ZRDO),                 143
     *(NAMES_TPCG(49),NTPCG_ZRDO)                                         143
      EQUIVALENCE (COMMEN_TPCG(49),CTPCG_ZRDO),(MAP_TPCG(1,49),           143
     *MTPCG_ZRDO)                                                         143
      CHARACTER CTPCG_ZRDO*40/' '/                                        143
      CHARACTER NTPCG_ZRDO*12/'R.ZRDO'/                                   143
      INTEGER MTPCG_ZRDO(2)/1,1/                                          143
      REAL RE_TPCG_ZRDO                                                   143
C -                                                                       143
      REAL TPCG_HEIGTUBE                                                  143
      EQUIVALENCE (BANK_TPCG(52),TPCG_HEIGTUBE,RE_TPCG_HEIGTUBE),         143
     *(NAMES_TPCG(50),NTPCG_HEIGTUBE)                                     143
      EQUIVALENCE (COMMEN_TPCG(50),CTPCG_HEIGTUBE),(MAP_TPCG(1,50),       143
     *MTPCG_HEIGTUBE)                                                     143
      CHARACTER CTPCG_HEIGTUBE*40/' '/                                    143
      CHARACTER NTPCG_HEIGTUBE*12/'R.HEIGTUBE'/                           143
      INTEGER MTPCG_HEIGTUBE(2)/1,1/                                      143
      REAL RE_TPCG_HEIGTUBE                                               143
C -                                                                       143
      REAL TPCG_WIDTUBE                                                   143
      EQUIVALENCE (BANK_TPCG(53),TPCG_WIDTUBE,RE_TPCG_WIDTUBE),           143
     *(NAMES_TPCG(51),NTPCG_WIDTUBE)                                      143
      EQUIVALENCE (COMMEN_TPCG(51),CTPCG_WIDTUBE),(MAP_TPCG(1,51),        143
     *MTPCG_WIDTUBE)                                                      143
      CHARACTER CTPCG_WIDTUBE*40/' '/                                     143
      CHARACTER NTPCG_WIDTUBE*12/'R.WIDTUBE'/                             143
      INTEGER MTPCG_WIDTUBE(2)/1,1/                                       143
      REAL RE_TPCG_WIDTUBE                                                143
C -                                                                       143
      REAL TPCG_RDOCOOLINGDX                                              143
      EQUIVALENCE (BANK_TPCG(54),TPCG_RDOCOOLINGDX,                       143
     *RE_TPCG_RDOCOOLINGDX),(NAMES_TPCG(52),NTPCG_RDOCOOLINGDX)           143
      EQUIVALENCE (COMMEN_TPCG(52),CTPCG_RDOCOOLINGDX),(MAP_TPCG(1,52),   143
     *MTPCG_RDOCOOLINGDX)                                                 143
      CHARACTER CTPCG_RDOCOOLINGDX*40/' '/                                143
      CHARACTER NTPCG_RDOCOOLINGDX*12/'R.RDOCOOLINGDX'/                   143
      INTEGER MTPCG_RDOCOOLINGDX(2)/1,1/                                  143
      REAL RE_TPCG_RDOCOOLINGDX                                           143
C -                                                                       143
      REAL TPCG_RDOCOOLINGDY                                              143
      EQUIVALENCE (BANK_TPCG(55),TPCG_RDOCOOLINGDY,                       143
     *RE_TPCG_RDOCOOLINGDY),(NAMES_TPCG(53),NTPCG_RDOCOOLINGDY)           143
      EQUIVALENCE (COMMEN_TPCG(53),CTPCG_RDOCOOLINGDY),(MAP_TPCG(1,53),   143
     *MTPCG_RDOCOOLINGDY)                                                 143
      CHARACTER CTPCG_RDOCOOLINGDY*40/' '/                                143
      CHARACTER NTPCG_RDOCOOLINGDY*12/'R.RDOCOOLINGDY'/                   143
      INTEGER MTPCG_RDOCOOLINGDY(2)/1,1/                                  143
      REAL RE_TPCG_RDOCOOLINGDY                                           143
C -                                                                       143
      REAL TPCG_RDOCOOLINGDZ                                              143
      EQUIVALENCE (BANK_TPCG(56),TPCG_RDOCOOLINGDZ,                       143
     *RE_TPCG_RDOCOOLINGDZ),(NAMES_TPCG(54),NTPCG_RDOCOOLINGDZ)           143
      EQUIVALENCE (COMMEN_TPCG(54),CTPCG_RDOCOOLINGDZ),(MAP_TPCG(1,54),   143
     *MTPCG_RDOCOOLINGDZ)                                                 143
      CHARACTER CTPCG_RDOCOOLINGDZ*40/' '/                                143
      CHARACTER NTPCG_RDOCOOLINGDZ*12/'R.RDOCOOLINGDZ'/                   143
      INTEGER MTPCG_RDOCOOLINGDZ(2)/1,1/                                  143
      REAL RE_TPCG_RDOCOOLINGDZ                                           143
C -                                                                       143
      REAL TPCG_ZGATINGGRID                                               143
      EQUIVALENCE (BANK_TPCG(57),TPCG_ZGATINGGRID,RE_TPCG_ZGATINGGRID),   143
     *(NAMES_TPCG(55),NTPCG_ZGATINGGRID)                                  143
      EQUIVALENCE (COMMEN_TPCG(55),CTPCG_ZGATINGGRID),(MAP_TPCG(1,55),    143
     *MTPCG_ZGATINGGRID)                                                  143
      CHARACTER CTPCG_ZGATINGGRID*40/' '/                                 143
      CHARACTER NTPCG_ZGATINGGRID*12/'R.ZGATINGGRID'/                     143
      INTEGER MTPCG_ZGATINGGRID(2)/1,1/                                   143
      REAL RE_TPCG_ZGATINGGRID                                            143
C -                                                                       143
      REAL TPCG_ZGROUNDGRID                                               143
      EQUIVALENCE (BANK_TPCG(58),TPCG_ZGROUNDGRID,RE_TPCG_ZGROUNDGRID),   143
     *(NAMES_TPCG(56),NTPCG_ZGROUNDGRID)                                  143
      EQUIVALENCE (COMMEN_TPCG(56),CTPCG_ZGROUNDGRID),(MAP_TPCG(1,56),    143
     *MTPCG_ZGROUNDGRID)                                                  143
      CHARACTER CTPCG_ZGROUNDGRID*40/' '/                                 143
      CHARACTER NTPCG_ZGROUNDGRID*12/'R.ZGROUNDGRID'/                     143
      INTEGER MTPCG_ZGROUNDGRID(2)/1,1/                                   143
      REAL RE_TPCG_ZGROUNDGRID                                            143
C -                                                                       143
      REAL TPCG_DEADZONE                                                  143
      EQUIVALENCE (BANK_TPCG(59),TPCG_DEADZONE,RE_TPCG_DEADZONE),         143
     *(NAMES_TPCG(57),NTPCG_DEADZONE)                                     143
      EQUIVALENCE (COMMEN_TPCG(57),CTPCG_DEADZONE),(MAP_TPCG(1,57),       143
     *MTPCG_DEADZONE)                                                     143
      CHARACTER CTPCG_DEADZONE*40/' '/                                    143
      CHARACTER NTPCG_DEADZONE*12/'R.DEADZONE'/                           143
      INTEGER MTPCG_DEADZONE(2)/1,1/                                      143
      REAL RE_TPCG_DEADZONE                                               143
C -                                                                       143
      REAL TPCG_DANODE(2)                                                 143
      EQUIVALENCE (BANK_TPCG(60),TPCG_DANODE,RE_TPCG_DANODE),             143
     *(NAMES_TPCG(58),NTPCG_DANODE)                                       143
      EQUIVALENCE (COMMEN_TPCG(58),CTPCG_DANODE),(MAP_TPCG(1,58),         143
     *MTPCG_DANODE)                                                       143
      CHARACTER CTPCG_DANODE*40/' '/                                      143
      CHARACTER NTPCG_DANODE*12/'R.DANODE'/                               143
      INTEGER MTPCG_DANODE(2)/2,1/                                        143
      REAL RE_TPCG_DANODE                                                 143
C -                                                                       143
      REAL TPCG_TOFMAX                                                    143
      EQUIVALENCE (BANK_TPCG(62),TPCG_TOFMAX,RE_TPCG_TOFMAX),             143
     *(NAMES_TPCG(59),NTPCG_TOFMAX)                                       143
      EQUIVALENCE (COMMEN_TPCG(59),CTPCG_TOFMAX),(MAP_TPCG(1,59),         143
     *MTPCG_TOFMAX)                                                       143
      CHARACTER CTPCG_TOFMAX*40/' '/                                      143
      CHARACTER NTPCG_TOFMAX*12/'R.TOFMAX'/                               143
      INTEGER MTPCG_TOFMAX(2)/1,1/                                        143
      REAL RE_TPCG_TOFMAX                                                 143
C -                                                                       143
      INTEGER LEN_TPCG,LLEN_TPCG,LINK_TPCG                                143
      PARAMETER (LEN_TPCG=62)                                             143
      PARAMETER (LLEN_TPCG=59)                                            143
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
      REAL TPRS_CONFIG                                                    144
      EQUIVALENCE (BANK_TPRS(3),TPRS_CONFIG,RE_TPRS_CONFIG),              144
     *(NAMES_TPRS(1),NTPRS_CONFIG)                                        144
      EQUIVALENCE (COMMEN_TPRS(1),CTPRS_CONFIG),(MAP_TPRS(1,1),           144
     *MTPRS_CONFIG)                                                       144
      CHARACTER CTPRS_CONFIG*40/' '/                                      144
      CHARACTER NTPRS_CONFIG*12/'R.CONFIG'/                               144
      INTEGER MTPRS_CONFIG(2)/1,1/                                        144
      REAL RE_TPRS_CONFIG                                                 144
C -                                                                       144
      REAL TPRS_SEC                                                       144
      EQUIVALENCE (BANK_TPRS(4),TPRS_SEC,RE_TPRS_SEC),(NAMES_TPRS(2),     144
     *NTPRS_SEC)                                                          144
      EQUIVALENCE (COMMEN_TPRS(2),CTPRS_SEC),(MAP_TPRS(1,2),MTPRS_SEC)    144
      CHARACTER CTPRS_SEC*40/' '/                                         144
      CHARACTER NTPRS_SEC*12/'R.SEC'/                                     144
      INTEGER MTPRS_SEC(2)/1,1/                                           144
      REAL RE_TPRS_SEC                                                    144
C -                                                                       144
      REAL TPRS_NROW                                                      144
      EQUIVALENCE (BANK_TPRS(5),TPRS_NROW,RE_TPRS_NROW),(NAMES_TPRS(3),   144
     *NTPRS_NROW)                                                         144
      EQUIVALENCE (COMMEN_TPRS(3),CTPRS_NROW),(MAP_TPRS(1,3),             144
     *MTPRS_NROW)                                                         144
      CHARACTER CTPRS_NROW*40/' '/                                        144
      CHARACTER NTPRS_NROW*12/'R.NROW'/                                   144
      INTEGER MTPRS_NROW(2)/1,1/                                          144
      REAL RE_TPRS_NROW                                                   144
C -                                                                       144
      REAL TPRS_PITCH                                                     144
      EQUIVALENCE (BANK_TPRS(6),TPRS_PITCH,RE_TPRS_PITCH),                144
     *(NAMES_TPRS(4),NTPRS_PITCH)                                         144
      EQUIVALENCE (COMMEN_TPRS(4),CTPRS_PITCH),(MAP_TPRS(1,4),            144
     *MTPRS_PITCH)                                                        144
      CHARACTER CTPRS_PITCH*40/' '/                                       144
      CHARACTER NTPRS_PITCH*12/'R.PITCH'/                                 144
      INTEGER MTPRS_PITCH(2)/1,1/                                         144
      REAL RE_TPRS_PITCH                                                  144
C -                                                                       144
      REAL TPRS_WIDTH                                                     144
      EQUIVALENCE (BANK_TPRS(7),TPRS_WIDTH,RE_TPRS_WIDTH),                144
     *(NAMES_TPRS(5),NTPRS_WIDTH)                                         144
      EQUIVALENCE (COMMEN_TPRS(5),CTPRS_WIDTH),(MAP_TPRS(1,5),            144
     *MTPRS_WIDTH)                                                        144
      CHARACTER CTPRS_WIDTH*40/' '/                                       144
      CHARACTER NTPRS_WIDTH*12/'R.WIDTH'/                                 144
      INTEGER MTPRS_WIDTH(2)/1,1/                                         144
      REAL RE_TPRS_WIDTH                                                  144
C -                                                                       144
      REAL TPRS_SUPER                                                     144
      EQUIVALENCE (BANK_TPRS(8),TPRS_SUPER,RE_TPRS_SUPER),                144
     *(NAMES_TPRS(6),NTPRS_SUPER)                                         144
      EQUIVALENCE (COMMEN_TPRS(6),CTPRS_SUPER),(MAP_TPRS(1,6),            144
     *MTPRS_SUPER)                                                        144
      CHARACTER CTPRS_SUPER*40/' '/                                       144
      CHARACTER NTPRS_SUPER*12/'R.SUPER'/                                 144
      INTEGER MTPRS_SUPER(2)/1,1/                                         144
      REAL RE_TPRS_SUPER                                                  144
C -                                                                       144
      REAL TPRS_DANODE                                                    144
      EQUIVALENCE (BANK_TPRS(9),TPRS_DANODE,RE_TPRS_DANODE),              144
     *(NAMES_TPRS(7),NTPRS_DANODE)                                        144
      EQUIVALENCE (COMMEN_TPRS(7),CTPRS_DANODE),(MAP_TPRS(1,7),           144
     *MTPRS_DANODE)                                                       144
      CHARACTER CTPRS_DANODE*40/' '/                                      144
      CHARACTER NTPRS_DANODE*12/'R.DANODE'/                               144
      INTEGER MTPRS_DANODE(2)/1,1/                                        144
      REAL RE_TPRS_DANODE                                                 144
C -                                                                       144
      REAL TPRS_RPADS(40)                                                 144
      EQUIVALENCE (BANK_TPRS(10),TPRS_RPADS,RE_TPRS_RPADS),               144
     *(NAMES_TPRS(8),NTPRS_RPADS)                                         144
      EQUIVALENCE (COMMEN_TPRS(8),CTPRS_RPADS),(MAP_TPRS(1,8),            144
     *MTPRS_RPADS)                                                        144
      CHARACTER CTPRS_RPADS*40/' '/                                       144
      CHARACTER NTPRS_RPADS*12/'R.RPADS'/                                 144
      INTEGER MTPRS_RPADS(2)/40,1/                                        144
      REAL RE_TPRS_RPADS                                                  144
C -                                                                       144
      REAL TPRS_NPADS(40)                                                 144
      EQUIVALENCE (BANK_TPRS(50),TPRS_NPADS,RE_TPRS_NPADS),               144
     *(NAMES_TPRS(9),NTPRS_NPADS)                                         144
      EQUIVALENCE (COMMEN_TPRS(9),CTPRS_NPADS),(MAP_TPRS(1,9),            144
     *MTPRS_NPADS)                                                        144
      CHARACTER CTPRS_NPADS*40/' '/                                       144
      CHARACTER NTPRS_NPADS*12/'R.NPADS'/                                 144
      INTEGER MTPRS_NPADS(2)/40,1/                                        144
      REAL RE_TPRS_NPADS                                                  144
C -                                                                       144
      INTEGER LEN_TPRS,LLEN_TPRS,LINK_TPRS                                144
      PARAMETER (LEN_TPRS=89)                                             144
      PARAMETER (LLEN_TPRS=9)                                             144
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
C DEFAULT TO STARNDARD TPC TPAD CONFIGURATION                             158
      TPCG_TPADCONFIG=6.0                                                 158
      CTPCG_TPADCONFIG='DEFAULT TO STARNDARD TPC TPAD CONFIGURATION'      158
C // RMIN => TPC ENVELOPE INNER RADIUS                                    159
      TPCG_RMIN=46.107                                                    159
      CTPCG_RMIN='// RMIN => TPC ENVELOPE INNER RADIUS'                   159
C // RMAX => TPC ENVELOPE OUTER RADIUS                                    160
      TPCG_RMAX=206.75/COS15                                              160
      CTPCG_RMAX='// RMAX => TPC ENVELOPE OUTER RADIUS'                   160
C // RMINIFC => INNER RADIUS TPC IFC : A.LEBEDEV MEASUREMENT 10/16/08     161
      TPCG_RMINIFC=46.6                                                   161
      CTPCG_RMINIFC='// RMINIFC => INNER RADIUS TPC IFC : A.LEBEDEV MEAS  161
     *UREMENT 10/16/08'                                                   161
C // LENGTHT => TPC FULL LENGTH UP TO FRONT OF EEMC                       162
      TPCG_LENGTHT=2*271.0                                                162
      CTPCG_LENGTHT='// LENGTHT => TPC FULL LENGTH UP TO FRONT OF EEMC'   162
C // LENGTH => TPC FULL LENGTH INCLUDING RDOS                             163
      TPCG_LENGTH=2*259.685                                               163
      CTPCG_LENGTH='// LENGTH => TPC FULL LENGTH INCLUDING RDOS'          163
C // LENGTHW => TPC LENGTH INCLUDING WHEEL                                164
      TPCG_LENGTHW=2*229.71                                               164
      CTPCG_LENGTHW='// LENGTHW => TPC LENGTH INCLUDING WHEEL'            164
C // LENGTHV => TPC GAS VOLUME LENGTH                                     165
      TPCG_LENGTHV=2*210.00                                               165
      CTPCG_LENGTHV='// LENGTHV => TPC GAS VOLUME LENGTH'                 165
C // TPCE ENVELOP DZ                                                      166
      TPCG_DZENVELOP=268.                                                 166
      CTPCG_DZENVELOP='// TPCE ENVELOP DZ'                                166
C // 49.60 WHEELIR => SUPPORT WHEEL INNER RADIUS                          167
      TPCG_WHEELIR=38.620*INCH/2                                          167
      CTPCG_WHEELIR='// 49.60 WHEELIR => SUPPORT WHEEL INNER RADIUS'      167
C // WHEELR0 => DISTANCE FROM IP OF END OF INNER CYLINDRICAL PART         168
      TPCG_WHEELR0=21.500*INCH                                            168
      CTPCG_WHEELR0='// WHEELR0 => DISTANCE FROM IP OF END OF INNER CYLI  168
     *NDRICAL PART'                                                       168
C // WHEELR1 => DISTANCE FROM IP OF MIDDLE RIB                            169
      TPCG_WHEELR1=47.867*INCH                                            169
      CTPCG_WHEELR1='// WHEELR1 => DISTANCE FROM IP OF MIDDLE RIB'        169
C // WHEELR2 => DISTANCE FROM IP OF UPPER RIB                             170
      TPCG_WHEELR2=76.093*INCH                                            170
      CTPCG_WHEELR2='// WHEELR2 => DISTANCE FROM IP OF UPPER RIB'         170
C // WHEELTOTALRIBWIDTH => TOTAL RIB WIDTH (A.LEBEDEV MEASUREMENT)        171
      TPCG_WHEELTOTALRIBWIDTH=7.6                                         171
      CTPCG_WHEELTOTALRIBWIDTH='// WHEELTOTALRIBWIDTH => TOTAL RIB WIDTH  171
     * (A.LEBEDEV MEASUREMENT)'                                           171
C // WHEELRIBWIDTH => RIB PLATE WIDTH (A.LEBEDEV MEASUREMENT)             172
      TPCG_WHEELRIBWIDTH=1.9                                              172
      CTPCG_WHEELRIBWIDTH='// WHEELRIBWIDTH => RIB PLATE WIDTH (A.LEBEDE  172
     *V MEASUREMENT)'                                                     172
C // WHEELRIBHEIGHT => RIB PLATE HEIGHT (A.LEBEDEV MEASUREMENT)           173
      TPCG_WHEELRIBHEIGHT=1.3                                             173
      CTPCG_WHEELRIBHEIGHT='// WHEELRIBHEIGHT => RIB PLATE HEIGHT (A.LEB  173
     *EDEV MEASUREMENT)'                                                  173
C // WHEELBOXDX (A.LEBEDEV MEASUREMENT)                                   174
      TPCG_WHEELBOXDX=4.7                                                 174
      CTPCG_WHEELBOXDX='// WHEELBOXDX (A.LEBEDEV MEASUREMENT)'            174
C // WHEELBOXDY (A.LEBEDEV MEASUREMENT)                                   175
      TPCG_WHEELBOXDY=7.6                                                 175
      CTPCG_WHEELBOXDY='// WHEELBOXDY (A.LEBEDEV MEASUREMENT)'            175
C // WHEELOR1 => @ TOFC                                                   176
      TPCG_WHEELOR1=201.0                                                 176
      CTPCG_WHEELOR1='// WHEELOR1 => @ TOFC'                              176
C // WHEELOR => SUPPORT WHEEL OUTER RADIUS                                177
      TPCG_WHEELOR=206.75                                                 177
      CTPCG_WHEELOR='// WHEELOR => SUPPORT WHEEL OUTER RADIUS'            177
C // WHEELTHK1                                                            178
      TPCG_WHEELTHK1=5.72                                                 178
      CTPCG_WHEELTHK1='// WHEELTHK1'                                      178
C // WHEELTHK => SUPPORT WHEEL LENGTH                                     179
      TPCG_WHEELTHK=11.43                                                 179
      CTPCG_WHEELTHK='// WHEELTHK => SUPPORT WHEEL LENGTH'                179
C // SENGASOR => TPC SENSITIVE GAS OUTER RADIUS                           180
      TPCG_SENGASOR=200                                                   180
      CTPCG_SENGASOR='// SENGASOR => TPC SENSITIVE GAS OUTER RADIUS'      180
C // MEMBTHK => CENTRAL MEMBRANE THICKNESS                                181
      TPCG_MEMBTHK=0.00762                                                181
      CTPCG_MEMBTHK='// MEMBTHK => CENTRAL MEMBRANE THICKNESS'            181
C // TOCSDR => OUTER COPPER THICKNESS: NIM A499 (2003) 659-678 TABLE 2    182
      TPCG_TOCSDR=0.013                                                   182
      CTPCG_TOCSDR='// TOCSDR => OUTER COPPER THICKNESS: NIM A499 (2003)  182
     * 659-678 TABLE 2'                                                   182
C // TOKADR => OUTER KAPTON THICKNESS                                     183
      TPCG_TOKADR=0.015                                                   183
      CTPCG_TOKADR='// TOKADR => OUTER KAPTON THICKNESS'                  183
C // TONXDR => OUTER NOMEX THICKNESS                                      184
      TPCG_TONXDR=0.953                                                   184
      CTPCG_TONXDR='// TONXDR => OUTER NOMEX THICKNESS'                   184
C // TOADDR => OUTER ADHESIVE THICKNESS                                   185
      TPCG_TOADDR=0.05                                                    185
      CTPCG_TOADDR='// TOADDR => OUTER ADHESIVE THICKNESS'                185
C // TOIGDR => OUTER ISOLATING GAS THICKNESS                              186
      TPCG_TOIGDR=5.70                                                    186
      CTPCG_TOIGDR='// TOIGDR => OUTER ISOLATING GAS THICKNESS'           186
C // TOALDR => OUTER ALUMINUM THICKNESS (FOR BOTH LAYERS)                 187
      TPCG_TOALDR=0.40                                                    187
      CTPCG_TOALDR='// TOALDR => OUTER ALUMINUM THICKNESS (FOR BOTH LAYE  187
     *RS)'                                                                187
C // TOHADR => OUTER HONEYCOMB AL THICKNESS                               188
      TPCG_TOHADR=0.60                                                    188
      CTPCG_TOHADR='// TOHADR => OUTER HONEYCOMB AL THICKNESS'            188
C // TIADDR => INNER ADHESIVE LAYER THICKNESS: NIM A499 (2003) 659-678 T  189
C ABLE 2                                                                  189
      TPCG_TIADDR=0.080                                                   189
      CTPCG_TIADDR='// TIADDR => INNER ADHESIVE LAYER THICKNESS: NIM A49  189
     *9 (2003) 659-678 TABLE 2'                                           189
C // TINXDR => INNER NOMEX STRUCTUREURE THICKNESS                         190
      TPCG_TINXDR=1.270                                                   190
      CTPCG_TINXDR='// TINXDR => INNER NOMEX STRUCTUREURE THICKNESS'      190
C // TIKADR => INNER KAPTON LAYER THICKNESS                               191
      TPCG_TIKADR=0.015                                                   191
      CTPCG_TIKADR='// TIKADR => INNER KAPTON LAYER THICKNESS'            191
C // TIALDR => INNER ALUMINUM LAYER THICKNESS                             192
      TPCG_TIALDR=0.004                                                   192
      CTPCG_TIALDR='// TIALDR => INNER ALUMINUM LAYER THICKNESS'          192
C // TIFCRF => OUTER RADIUS OF IFC FLANGE                                 193
      TPCG_TIFCRF=51.7                                                    193
      CTPCG_TIFCRF='// TIFCRF => OUTER RADIUS OF IFC FLANGE'              193
C // TIFCDRT => TOLERANCE BETWEEN FLANGE AND WHEEL                        194
      TPCG_TIFCDRT=0.1                                                    194
      CTPCG_TIFCDRT='// TIFCDRT => TOLERANCE BETWEEN FLANGE AND WHEEL'    194
C // DZ OF YF1                                                            195
      TPCG_DZYF1=221.162                                                  195
      CTPCG_DZYF1='// DZ OF YF1'                                          195
C // DZ OF YF2                                                            196
      TPCG_DZYF2=211.9                                                    196
      CTPCG_DZYF2='// DZ OF YF2'                                          196
C // DZ OF YF3                                                            197
      TPCG_DZYF3=208.02                                                   197
      CTPCG_DZYF3='// DZ OF YF3'                                          197
C // 1.82 MM DRAWING 24A0314 & 24A0304                                    198
      TPCG_PADPLANETHICKNESS=0.182                                        198
      CTPCG_PADPLANETHICKNESS='// 1.82 MM DRAWING 24A0314 & 24A0304'      198
C //                                                                      199
      TPCG_DGATEGROUND=0.6                                                199
      CTPCG_DGATEGROUND='//'                                              199
C //                                                                      200
      TPCG_WIREMOUNTWIDTH=5*((0.130+0.1376)/2)*INCH                       200
      CTPCG_WIREMOUNTWIDTH='//'                                           200
C // DRAWING 24A0434                                                      201
      TPCG_WIREMOUNTHEIGHT=1.340*INCH                                     201
      CTPCG_WIREMOUNTHEIGHT='// DRAWING 24A0434'                          201
C // A.LEBEDEV 1 RDO = 5 LB                                               202
      TPCG_DXRDO=1.75/2                                                   202
      CTPCG_DXRDO='// A.LEBEDEV 1 RDO = 5 LB'                             202
C //                                                                      203
      TPCG_DYRDO=45./2                                                    203
      CTPCG_DYRDO='//'                                                    203
C //                                                                      204
      TPCG_DZRDO=17.0/2                                                   204
      CTPCG_DZRDO='//'                                                    204
C //                                                                      205
      TPCG_ZRDO=TPCG_LENGTHW/2 + 20.0                                     205
      CTPCG_ZRDO='//'                                                     205
C // X COOLING TUBE DRAWING 24A0801B                                      206
      TPCG_HEIGTUBE=0.703*INCH                                            206
      CTPCG_HEIGTUBE='// X COOLING TUBE DRAWING 24A0801B'                 206
C // Z MIXTURE TPCE_WATER_PIPE => RHO = 2.32155 G/CM**3                   207
      TPCG_WIDTUBE=0.500*INCH                                             207
      CTPCG_WIDTUBE='// Z MIXTURE TPCE_WATER_PIPE => RHO = 2.32155 G/CM*  207
     **3'                                                                 207
C //                                                                      208
      TPCG_RDOCOOLINGDX=(38.0 + 9.0 + 58.0)/2                             208
      CTPCG_RDOCOOLINGDX='//'                                             208
C //                                                                      209
      TPCG_RDOCOOLINGDY=1.25                                              209
      CTPCG_RDOCOOLINGDY='//'                                             209
C //                                                                      210
      TPCG_RDOCOOLINGDZ=2.50                                              210
      CTPCG_RDOCOOLINGDZ='//'                                             210
C // INNER/OUTER ANODE WIDTH                                              211
      TPCG_DANODE(1)=0.2                                                  211
      TPCG_DANODE(2)=0.4                                                  211
      CTPCG_DANODE='// INNER/OUTER ANODE WIDTH'                           211
C //                                                                      212
      TPCG_ZGATINGGRID=TPCG_LENGTHV/2                                     212
     *-(TPCG_DGATEGROUND+2*TPCG_DANODE(2))                                212
      CTPCG_ZGATINGGRID='//'                                              212
C //                                                                      213
      TPCG_ZGROUNDGRID=TPCG_ZGATINGGRID+TPCG_DGATEGROUND                  213
      CTPCG_ZGROUNDGRID='//'                                              213
C // DEAD ZONE BEFORE GATINGGRID. NO HITS THERE                           214
      TPCG_DEADZONE=12.                                                   214
      CTPCG_DEADZONE='// DEAD ZONE BEFORE GATINGGRID. NO HITS THERE'      214
C // MAX TIME OF FLIGHT                                                   215
      TPCG_TOFMAX=1.0E-6                                                  215
      CTPCG_TOFMAX='// MAX TIME OF FLIGHT'                                215
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCG',    216
     *BPATH_TPCG,IDNUM_TPCG,1, MAP_TPCG,NAMES_TPCG,COMMEN_TPCG,           216
     *BANK_TPCG,LEN_TPCG,LLEN_TPCG,LINK_TPCG,FLAG_TPCG)                   216
C *                                                                       216
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCG',    216
     *BPATH_TPCG,IDNUM_TPCG,1, MAP_TPCG,NAMES_TPCG,COMMEN_TPCG,           216
     *BANK_TPCG,LEN_TPCG,LLEN_TPCG,LINK_TPCG,FLAG_TPCG)                   216
C *                                                                       217
      CALL AGSBEGM(MODULE,IPRIN)                                          217
      IF (IPRIN.LT.0) RETURN                                              217
C Filling bank TPCG - SUBVERSION 1                                        217
      BANK_TITLE='SUBVERSION 1'                                           217
      BPATH_TPCG='TPCG*'                                                  217
      IDNUM_TPCG(1)=0                                                     218
C SELECTOR                                                                218
      TPCG_VERSION=3.1                                                    218
      CTPCG_VERSION='SELECTOR'                                            218
C MAX ENVELOPE RADIUS                                                     219
      TPCG_RMAX=207.77                                                    219
      CTPCG_RMAX='MAX ENVELOPE RADIUS'                                    219
C EXTEND DEADZONE BEFORE GATING GRID                                      220
      TPCG_DEADZONE=14                                                    220
      CTPCG_DEADZONE='EXTEND DEADZONE BEFORE GATING GRID'                 220
C INCREASE MAXIMUM TOF FOR PARTICLES                                      221
      TPCG_TOFMAX=32.0E-6                                                 221
      CTPCG_TOFMAX='INCREASE MAXIMUM TOF FOR PARTICLES'                   221
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCG',    222
     *BPATH_TPCG,IDNUM_TPCG,1, MAP_TPCG,NAMES_TPCG,COMMEN_TPCG,           222
     *BANK_TPCG,LEN_TPCG,LLEN_TPCG,LINK_TPCG,FLAG_TPCG)                   222
C *                                                                       222
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCG',    222
     *BPATH_TPCG,IDNUM_TPCG,1, MAP_TPCG,NAMES_TPCG,COMMEN_TPCG,           222
     *BANK_TPCG,LEN_TPCG,LLEN_TPCG,LINK_TPCG,FLAG_TPCG)                   222
C use default TPCC bank                                                   223
      BPATH_TPCC='TPCC*'                                                  223
      IDNUM_TPCC(1)=0                                                     223
      CALL AGDGETP('TPCXGEO2','*',-999,'TPCC',' ',BANK_TPCC, BPATH_TPCC,  223
     *IDNUM_TPCC,1,MAP_TPCC,NAMES_TPCC,BANK_TPCC,LEN_TPCC,LLEN_TPCC,      223
     *LINK_TPCC,FLAG_TPCC)                                                223
C use TPCG bank                                                           224
      BPATH_TPCG='TPCG*'                                                  224
      IDNUM_TPCG(1)=0                                                     224
      TPCG_VERSION=TPCC_VERSION                                           224
      CALL AGDGETP('TPCXGEO2','*',-999,'TPCG',NTPCG_VERSION,              224
     *RE_TPCG_VERSION,BPATH_TPCG,IDNUM_TPCG,1,MAP_TPCG,NAMES_TPCG,        224
     *BANK_TPCG,LEN_TPCG,LLEN_TPCG,LINK_TPCG,FLAG_TPCG)                   224
C Check TPCG_TPADCONFIG==0                                                225
      IF (TPCG_TPADCONFIG.EQ.0) THEN                                      225
C *                                                                       226
      CALL AGSBEGM(MODULE,IPRIN)                                          226
      IF (IPRIN.LT.0) RETURN                                              226
C Filling bank TPRS - // SECTOR OF PADROWS                                226
      BANK_TITLE='// SECTOR OF PADROWS'                                   226
      BPATH_TPRS='TPRS*'                                                  226
      IDNUM_TPRS(1)=0                                                     227
C DEFAULT NUMBER OF PADROWS                                               227
      TPRS_CONFIG=0                                                       227
      CTPRS_CONFIG='DEFAULT NUMBER OF PADROWS'                            227
C // SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                              228
      TPRS_SEC=1                                                          228
      CTPRS_SEC='// SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'              228
C // NUMBER OF PADROWS IN THE SECTOR                                      229
      TPRS_NROW=13                                                        229
      CTPRS_NROW='// NUMBER OF PADROWS IN THE SECTOR'                     229
C // TPC PADROW PITCH WIDTH                                               230
      TPRS_PITCH=0.335                                                    230
      CTPRS_PITCH='// TPC PADROW PITCH WIDTH'                             230
C // TPC PADROW THICKNESS                                                 231
      TPRS_WIDTH=1.15                                                     231
      CTPRS_WIDTH='// TPC PADROW THICKNESS'                               231
C // NUMBER OF PADRAWS IN A SUPERPADROW                                   232
      TPRS_SUPER=3                                                        232
      CTPRS_SUPER='// NUMBER OF PADRAWS IN A SUPERPADROW'                 232
C // DISTANCE TO ANODE WIRE FROM PAD PLANE                                233
      TPRS_DANODE=0.2                                                     233
      CTPRS_DANODE='// DISTANCE TO ANODE WIRE FROM PAD PLANE'             233
C // NUMBER OF PADS IN ROW                                                235
      TPRS_NPADS(1)=88                                                    235
      TPRS_NPADS(2)=96                                                    235
      TPRS_NPADS(3)=104                                                   235
      TPRS_NPADS(4)=112                                                   235
      TPRS_NPADS(5)=118                                                   235
      TPRS_NPADS(6)=126                                                   235
      TPRS_NPADS(7)=134                                                   235
      TPRS_NPADS(8)=142                                                   235
      TPRS_NPADS(9)=150                                                   235
      TPRS_NPADS(10)=158                                                  235
      TPRS_NPADS(11)=166                                                  235
      TPRS_NPADS(12)=174                                                  235
      TPRS_NPADS(13)=182                                                  235
      CTPRS_NPADS='// NUMBER OF PADS IN ROW'                              235
C // TPC PADROW RADII                                                     238
      TPRS_RPADS(1)=60.0                                                  238
      TPRS_RPADS(2)=64.8                                                  238
      TPRS_RPADS(3)=69.6                                                  238
      TPRS_RPADS(4)=74.4                                                  238
      TPRS_RPADS(5)=79.2                                                  238
      TPRS_RPADS(6)=84.0                                                  238
      TPRS_RPADS(7)=88.8                                                  238
      TPRS_RPADS(8)=93.6                                                  238
      TPRS_RPADS(9)=98.8                                                  238
      TPRS_RPADS(10)=104.0                                                238
      TPRS_RPADS(11)=109.2                                                238
      TPRS_RPADS(12)=114.4                                                238
      TPRS_RPADS(13)=119.6                                                238
      CTPRS_RPADS='// TPC PADROW RADII'                                   238
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    239
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           239
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   239
C *                                                                       239
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    239
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           239
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   239
      ENDIF                                                               240
C Check TPCG_TPADCONFIG==6                                                241
      IF (TPCG_TPADCONFIG.EQ.6) THEN                                      241
C *                                                                       242
      CALL AGSBEGM(MODULE,IPRIN)                                          242
      IF (IPRIN.LT.0) RETURN                                              242
C Filling bank TPRS - // SECTOR OF PADROWS                                242
      BANK_TITLE='// SECTOR OF PADROWS'                                   242
      BPATH_TPRS='TPRS*'                                                  242
      IDNUM_TPRS(1)=0                                                     243
C DEV TF                                                                  243
      TPRS_CONFIG=6                                                       243
      CTPRS_CONFIG='DEV TF'                                               243
C // SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                              244
      TPRS_SEC=1                                                          244
      CTPRS_SEC='// SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'              244
C // NUMBER OF PADROWS IN THE SECTOR                                      245
      TPRS_NROW=32                                                        245
      CTPRS_NROW='// NUMBER OF PADROWS IN THE SECTOR'                     245
C // TPC PADROW PITCH WIDTH                                               246
      TPRS_PITCH=0.400                                                    246
      CTPRS_PITCH='// TPC PADROW PITCH WIDTH'                             246
C // TPC PADROW THICKNESS                                                 247
      TPRS_WIDTH=1.95                                                     247
      CTPRS_WIDTH='// TPC PADROW THICKNESS'                               247
C // NUMBER OF PADRAWS IN A SUPERPADROW                                   248
      TPRS_SUPER=0                                                        248
      CTPRS_SUPER='// NUMBER OF PADRAWS IN A SUPERPADROW'                 248
C // DISTANCE TO ANODE WIRE FROM PAD PLANE                                249
      TPRS_DANODE=0.2                                                     249
      CTPRS_DANODE='// DISTANCE TO ANODE WIRE FROM PAD PLANE'             249
C NUMBER OF PADS IN ROW                                                   253
      TPRS_NPADS(1)=66                                                    253
      TPRS_NPADS(2)=70                                                    253
      TPRS_NPADS(3)=72                                                    253
      TPRS_NPADS(4)=74                                                    253
      TPRS_NPADS(5)=78                                                    253
      TPRS_NPADS(6)=80                                                    253
      TPRS_NPADS(7)=82                                                    253
      TPRS_NPADS(8)=86                                                    253
      TPRS_NPADS(9)=88                                                    253
      TPRS_NPADS(10)=90                                                   253
      TPRS_NPADS(11)=94                                                   253
      TPRS_NPADS(12)=96                                                   253
      TPRS_NPADS(13)=98                                                   253
      TPRS_NPADS(14)=102                                                  253
      TPRS_NPADS(15)=104                                                  253
      TPRS_NPADS(16)=106                                                  253
      TPRS_NPADS(17)=110                                                  253
      TPRS_NPADS(18)=112                                                  253
      TPRS_NPADS(19)=114                                                  253
      TPRS_NPADS(20)=118                                                  253
      TPRS_NPADS(21)=120                                                  253
      TPRS_NPADS(22)=122                                                  253
      TPRS_NPADS(23)=126                                                  253
      TPRS_NPADS(24)=128                                                  253
      TPRS_NPADS(25)=130                                                  253
      TPRS_NPADS(26)=134                                                  253
      TPRS_NPADS(27)=136                                                  253
      TPRS_NPADS(28)=138                                                  253
      TPRS_NPADS(29)=142                                                  253
      TPRS_NPADS(30)=144                                                  253
      TPRS_NPADS(31)=146                                                  253
      TPRS_NPADS(32)=150                                                  253
      CTPRS_NPADS='NUMBER OF PADS IN ROW'                                 253
C RADIUS OF PADS                                                          259
      TPRS_RPADS(1)=57.2                                                  259
      TPRS_RPADS(2)=59.2                                                  259
      TPRS_RPADS(3)=61.2                                                  259
      TPRS_RPADS(4)=63.2                                                  259
      TPRS_RPADS(5)=65.2                                                  259
      TPRS_RPADS(6)=67.2                                                  259
      TPRS_RPADS(7)=69.2                                                  259
      TPRS_RPADS(8)=71.2                                                  259
      TPRS_RPADS(9)=73.2                                                  259
      TPRS_RPADS(10)=75.2                                                 259
      TPRS_RPADS(11)=77.2                                                 259
      TPRS_RPADS(12)=79.2                                                 259
      TPRS_RPADS(13)=81.2                                                 259
      TPRS_RPADS(14)=83.2                                                 259
      TPRS_RPADS(15)=85.2                                                 259
      TPRS_RPADS(16)=87.2                                                 259
      TPRS_RPADS(17)=89.2                                                 259
      TPRS_RPADS(18)=91.2                                                 259
      TPRS_RPADS(19)=93.2                                                 259
      TPRS_RPADS(20)=95.2                                                 259
      TPRS_RPADS(21)=97.2                                                 259
      TPRS_RPADS(22)=99.2                                                 259
      TPRS_RPADS(23)=101.2                                                259
      TPRS_RPADS(24)=103.2                                                259
      TPRS_RPADS(25)=105.2                                                259
      TPRS_RPADS(26)=107.2                                                259
      TPRS_RPADS(27)=109.2                                                259
      TPRS_RPADS(28)=111.2                                                259
      TPRS_RPADS(29)=113.2                                                259
      TPRS_RPADS(30)=115.2                                                259
      TPRS_RPADS(31)=117.2                                                259
      TPRS_RPADS(32)=119.2                                                259
      CTPRS_RPADS='RADIUS OF PADS'                                        259
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    260
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           260
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   260
C *                                                                       260
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    260
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           260
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   260
      ENDIF                                                               261
C *                                                                       262
      CALL AGSBEGM(MODULE,IPRIN)                                          262
      IF (IPRIN.LT.0) RETURN                                              262
C Filling bank TPRS - // SECTOR OF PADROWS                                262
      BANK_TITLE='// SECTOR OF PADROWS'                                   262
      BPATH_TPRS='TPRS*'                                                  262
      IDNUM_TPRS(1)=0                                                     263
C // SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                              263
      TPRS_SEC=2                                                          263
      CTPRS_SEC='// SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'              263
C // NUMBER OF PADROWS IN OUTER SECTOR                                    264
      TPRS_NROW=32                                                        264
      CTPRS_NROW='// NUMBER OF PADROWS IN OUTER SECTOR'                   264
C // OUTER TPC PADROW PITCH WIDTH                                         265
      TPRS_PITCH=0.67                                                     265
      CTPRS_PITCH='// OUTER TPC PADROW PITCH WIDTH'                       265
C // OUTER TPC PADROW THICKNESS                                           266
      TPRS_WIDTH=1.95                                                     266
      CTPRS_WIDTH='// OUTER TPC PADROW THICKNESS'                         266
C // NUMBER OF PADROWS IN A SUPERPADROW                                   267
      TPRS_SUPER=1                                                        267
      CTPRS_SUPER='// NUMBER OF PADROWS IN A SUPERPADROW'                 267
C // DISTANCE TO ANODE WIRE FROM PAD PLANE                                268
      TPRS_DANODE=0.4                                                     268
      CTPRS_DANODE='// DISTANCE TO ANODE WIRE FROM PAD PLANE'             268
C // NUMBER OF PADS IN ROW                                                272
      TPRS_NPADS(1)=98                                                    272
      TPRS_NPADS(2)=100                                                   272
      TPRS_NPADS(3)=102                                                   272
      TPRS_NPADS(4)=104                                                   272
      TPRS_NPADS(5)=106                                                   272
      TPRS_NPADS(6)=106                                                   272
      TPRS_NPADS(7)=108                                                   272
      TPRS_NPADS(8)=110                                                   272
      TPRS_NPADS(9)=112                                                   272
      TPRS_NPADS(10)=112                                                  272
      TPRS_NPADS(11)=114                                                  272
      TPRS_NPADS(12)=116                                                  272
      TPRS_NPADS(13)=118                                                  272
      TPRS_NPADS(14)=120                                                  272
      TPRS_NPADS(15)=122                                                  272
      TPRS_NPADS(16)=122                                                  272
      TPRS_NPADS(17)=124                                                  272
      TPRS_NPADS(18)=126                                                  272
      TPRS_NPADS(19)=128                                                  272
      TPRS_NPADS(20)=128                                                  272
      TPRS_NPADS(21)=130                                                  272
      TPRS_NPADS(22)=132                                                  272
      TPRS_NPADS(23)=134                                                  272
      TPRS_NPADS(24)=136                                                  272
      TPRS_NPADS(25)=138                                                  272
      TPRS_NPADS(26)=138                                                  272
      TPRS_NPADS(27)=140                                                  272
      TPRS_NPADS(28)=142                                                  272
      TPRS_NPADS(29)=144                                                  272
      TPRS_NPADS(30)=144                                                  272
      TPRS_NPADS(31)=144                                                  272
      TPRS_NPADS(32)=144                                                  272
      CTPRS_NPADS='// NUMBER OF PADS IN ROW'                              272
C // TPC PADROW RADII                                                     280
      TPRS_RPADS(1)=127.195                                               280
      TPRS_RPADS(2)=129.195                                               280
      TPRS_RPADS(3)=131.195                                               280
      TPRS_RPADS(4)=133.195                                               280
      TPRS_RPADS(5)=135.195                                               280
      TPRS_RPADS(6)=137.195                                               280
      TPRS_RPADS(7)=139.195                                               280
      TPRS_RPADS(8)=141.195                                               280
      TPRS_RPADS(9)=143.195                                               280
      TPRS_RPADS(10)=145.195                                              280
      TPRS_RPADS(11)=147.195                                              280
      TPRS_RPADS(12)=149.195                                              280
      TPRS_RPADS(13)=151.195                                              280
      TPRS_RPADS(14)=153.195                                              280
      TPRS_RPADS(15)=155.195                                              280
      TPRS_RPADS(16)=157.195                                              280
      TPRS_RPADS(17)=159.195                                              280
      TPRS_RPADS(18)=161.195                                              280
      TPRS_RPADS(19)=163.195                                              280
      TPRS_RPADS(20)=165.195                                              280
      TPRS_RPADS(21)=167.195                                              280
      TPRS_RPADS(22)=169.195                                              280
      TPRS_RPADS(23)=171.195                                              280
      TPRS_RPADS(24)=173.195                                              280
      TPRS_RPADS(25)=175.195                                              280
      TPRS_RPADS(26)=177.195                                              280
      TPRS_RPADS(27)=179.195                                              280
      TPRS_RPADS(28)=181.195                                              280
      TPRS_RPADS(29)=183.195                                              280
      TPRS_RPADS(30)=185.195                                              280
      TPRS_RPADS(31)=187.195                                              280
      TPRS_RPADS(32)=189.195                                              280
      CTPRS_RPADS='// TPC PADROW RADII'                                   280
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    281
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           281
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   281
C *                                                                       281
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPRS',    281
     *BPATH_TPRS,IDNUM_TPRS,1, MAP_TPRS,NAMES_TPRS,COMMEN_TPRS,           281
     *BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)                   281
C use default TPRS bank                                                   282
      BPATH_TPRS='TPRS*'                                                  282
      IDNUM_TPRS(1)=0                                                     282
      CALL AGDGETP('TPCXGEO2','*',-999,'TPRS',' ',BANK_TPRS, BPATH_TPRS,  282
     *IDNUM_TPRS,1,MAP_TPRS,NAMES_TPRS,BANK_TPRS,LEN_TPRS,LLEN_TPRS,      282
     *LINK_TPRS,FLAG_TPRS)                                                282
C *                                                                       283
      CALL AGSBEGM(MODULE,IPRIN)                                          283
      IF (IPRIN.LT.0) RETURN                                              283
C Filling bank TECW - // EC TRAPEZOID AND SUPPORT WHEEL                   283
      BANK_TITLE='// EC TRAPEZOID AND SUPPORT WHEEL'                      283
      BPATH_TECW='TECW*'                                                  283
      IDNUM_TECW(1)=0                                                     284
C // SEC => SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                       284
      TECW_SEC=1                                                          284
      CTECW_SEC='// SEC => SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'       284
C // GAPWIDI => AIR IN SUPPORT WHEEL - INNER WIDTH                        285
      TECW_GAPWIDI=2.* 10.91                                              285
      CTECW_GAPWIDI='// GAPWIDI => AIR IN SUPPORT WHEEL - INNER WIDTH'    285
C // GAPWIDO => AIR IN SUPPORT WHEEL - OUTER WIDTH                        286
      TECW_GAPWIDO=2.* 27.56                                              286
      CTECW_GAPWIDO='// GAPWIDO => AIR IN SUPPORT WHEEL - OUTER WIDTH'    286
C // GAPHEIT => AIR IN SUPPORT WHEEL - HEIGHT (DR)                        287
      TECW_GAPHEIT=62.15                                                  287
      CTECW_GAPHEIT='// GAPHEIT => AIR IN SUPPORT WHEEL - HEIGHT (DR)'    287
C // GAPRAD => AIR IN SUPPORT WHEEL - CENTER RADIUS                       288
      TECW_GAPRAD=87.0                                                    288
      CTECW_GAPRAD='// GAPRAD => AIR IN SUPPORT WHEEL - CENTER RADIUS'    288
C //2.* 13.75, !// INWIDTH => SECTOR WIDTH AT INNER RADIUS                289
      TECW_INWIDTH=10.829*INCH                                            289
      CTECW_INWIDTH='//2.* 13.75, !// INWIDTH => SECTOR WIDTH AT INNER R  289
     *ADIUS'                                                              289
C //2.* 32.38, !// OUWIDTH => SECTOR WIDTH AT OUTER RADIUS                290
      TECW_OUWIDTH=25.498*INCH                                            290
      CTECW_OUWIDTH='//2.* 32.38, !// OUWIDTH => SECTOR WIDTH AT OUTER R  290
     *ADIUS'                                                              290
C // NORIBS                                                               291
      TECW_NORIBS=11                                                      291
      CTECW_NORIBS='// NORIBS'                                            291
C // ZSTEPRIB                                                             292
      TECW_ZSTEPRIB=3.150*INCH                                            292
      CTECW_ZSTEPRIB='// ZSTEPRIB'                                        292
C // WIDTHRIB => SIDE RIB WIDTH                                           293
      TECW_WIDTHRIB=1.47*COS(15./180*ACOS(-1.))*INCH                      293
      CTECW_WIDTHRIB='// WIDTHRIB => SIDE RIB WIDTH'                      293
C // DRAWING 24A3685B 27.77*INCH FROM DRAWING 24A0325, !// => 69.527 =>   294
C (WIRE PLANE) 69.                                                        294
      TECW_HEIGHT=27.373*INCH                                             294
      CTECW_HEIGHT='// DRAWING 24A3685B 27.77*INCH FROM DRAWING 24A0325,  294
     * !// => 69.527 => (WIRE PLANE) 69.'                                 294
C // 24A3685B 3.219*INCH !// THICK => SECTOR THICKNESS                    295
      TECW_THICK=3.300*INCH                                               295
      CTECW_THICK='// 24A3685B 3.219*INCH !// THICK => SECTOR             295
     *THICKNESS'                                                          295
C //                                                                      296
      TECW_THICKAL=0.375*INCH                                             296
      CTECW_THICKAL='//'                                                  296
C // RMIN => MINIMUM DISTANCE TO BEAM LINE (WIRE PLANE)                   297
      TECW_RMIN=51.905                                                    297
      CTECW_RMIN='// RMIN => MINIMUM DISTANCE TO BEAM LINE (WIRE          297
     *PLANE)'                                                             297
C // RCENTER => SECTOR CENTER RADIUS (SET BY PRECISION HOLES)             298
      TECW_RCENTER=86.669                                                 298
      CTECW_RCENTER='// RCENTER => SECTOR CENTER RADIUS (SET BY PRECISIO  298
     *N HOLES)'                                                           298
C // = 0.9525 CM                                                          299
      TECW_HOLEDX=0.750/2*INCH                                            299
      CTECW_HOLEDX='// = 0.9525 CM'                                       299
C // = 3.33375 CM                                                         300
      TECW_HOLEDY=2.625/2*INCH                                            300
      CTECW_HOLEDY='// = 3.33375 CM'                                      300
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TECW',    301
     *BPATH_TECW,IDNUM_TECW,1, MAP_TECW,NAMES_TECW,COMMEN_TECW,           301
     *BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)                   301
C *                                                                       301
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TECW',    301
     *BPATH_TECW,IDNUM_TECW,1, MAP_TECW,NAMES_TECW,COMMEN_TECW,           301
     *BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)                   301
C *                                                                       302
      CALL AGSBEGM(MODULE,IPRIN)                                          302
      IF (IPRIN.LT.0) RETURN                                              302
C Filling bank TECW - // EC TRAPEZOID AND SUPPORT WHEEL                   302
      BANK_TITLE='// EC TRAPEZOID AND SUPPORT WHEEL'                      302
      BPATH_TECW='TECW*'                                                  302
      IDNUM_TECW(1)=0                                                     303
C // SEC => SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER                       303
      TECW_SEC=2                                                          303
      CTECW_SEC='// SEC => SECTOR NUMBER: 1 FOR INNER, 2 FOR OUTER'       303
C // GAPWIDI => AIR IN SUPPORT WHEEL - INNER WIDTH                        304
      TECW_GAPWIDI=2.* 28.92                                              304
      CTECW_GAPWIDI='// GAPWIDI => AIR IN SUPPORT WHEEL - INNER WIDTH'    304
C // GAPWIDO => AIR IN SUPPORT WHEEL - OUTER WIDTH                        305
      TECW_GAPWIDO=2.* 46.74                                              305
      CTECW_GAPWIDO='// GAPWIDO => AIR IN SUPPORT WHEEL - OUTER WIDTH'    305
C // GAPHEIT => AIR IN SUPPORT WHEEL - HEIGHT (DR)                        306
      TECW_GAPHEIT=65.0                                                   306
      CTECW_GAPHEIT='// GAPHEIT => AIR IN SUPPORT WHEEL - HEIGHT (DR)'    306
C // AGAPRAD => IR IN SUPPORT WHEEL - RADIUS                              307
      TECW_GAPRAD=158.0                                                   307
      CTECW_GAPRAD='// AGAPRAD => IR IN SUPPORT WHEEL - RADIUS'           307
C //64.92, !// INWIDTH => SECTOR WIDTH AT INNER RADIUS                    308
      TECW_INWIDTH=25.561*INCH                                            308
      CTECW_INWIDTH='//64.92, !// INWIDTH => SECTOR WIDTH AT INNER RADIU  308
     *S'                                                                  308
C //103.25, !// OUWIDTH => SECTOR WIDTH AT OUTER RADIUS                   309
      TECW_OUWIDTH=40.649*INCH                                            309
      CTECW_OUWIDTH='//103.25, !// OUWIDTH => SECTOR WIDTH AT OUTER RADI  309
     *US'                                                                 309
C // = 1.242060 WIDTHLIP => WIDTH OF LIP                                  310
      TECW_WIDTHLIP=((25.561-24.583)+(40.649-39.671))/4*INCH              310
      CTECW_WIDTHLIP='// = 1.242060 WIDTHLIP => WIDTH OF LIP'             310
C // NORIBS                                                               311
      TECW_NORIBS=14                                                      311
      CTECW_NORIBS='// NORIBS'                                            311
C // ZSTEPRIB                                                             312
      TECW_ZSTEPRIB=3.150*INCH                                            312
      CTECW_ZSTEPRIB='// ZSTEPRIB'                                        312
C // WIDTHRIB = > SIDE RIB WIDTH                                          313
      TECW_WIDTHRIB=1.47*COS(15./180*ACOS(-1.))*INCH                      313
      CTECW_WIDTHRIB='// WIDTHRIB = > SIDE RIB WIDTH'                     313
C // 71.514 (WIRE PLANE) => 71.51, !// HEIGHT => SECTOR RADIAL HEIGHT     314
      TECW_HEIGHT=28.155*INCH                                             314
      CTECW_HEIGHT='// 71.514 (WIRE PLANE) => 71.51, !// HEIGHT => SECTO  314
     *R RADIAL HEIGHT'                                                    314
C // 24A3925G THICK => SECTOR THICKNESS                                   315
      TECW_THICK=3.140*INCH                                               315
      CTECW_THICK='// 24A3925G THICK => SECTOR THICKNESS'                 315
C //                                                                      316
      TECW_THICKAL=0.375*INCH                                             316
      CTECW_THICKAL='//'                                                  316
C // RMIN => MINIMUM DISTANCE TO BEAM LINE (WIRE PLANE)                   317
      TECW_RMIN=121.732                                                   317
      CTECW_RMIN='// RMIN => MINIMUM DISTANCE TO BEAM LINE (WIRE          317
     *PLANE)'                                                             317
C // RCENTER => SECTOR CENTER RADIUS (SET BY PRECISION HOLES)             318
      TECW_RCENTER=157.488                                                318
      CTECW_RCENTER='// RCENTER => SECTOR CENTER RADIUS (SET BY PRECISIO  318
     *N HOLES)'                                                           318
C //                                                                      319
      TECW_HOLEDX=0.750/2*INCH                                            319
      CTECW_HOLEDX='//'                                                   319
C //                                                                      320
      TECW_HOLEDY=2.500/2*INCH                                            320
      CTECW_HOLEDY='//'                                                   320
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TECW',    321
     *BPATH_TECW,IDNUM_TECW,1, MAP_TECW,NAMES_TECW,COMMEN_TECW,           321
     *BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)                   321
C *                                                                       321
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TECW',    321
     *BPATH_TECW,IDNUM_TECW,1, MAP_TECW,NAMES_TECW,COMMEN_TECW,           321
     *BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)                   321
C *                                                                       322
      CALL AGSBEGM(MODULE,IPRIN)                                          322
      IF (IPRIN.LT.0) RETURN                                              322
C Filling bank TPCR - // VOLUME FOR TPC READOUT BOARDS                    322
      BANK_TITLE='// VOLUME FOR TPC READOUT BOARDS'                       322
      BPATH_TPCR='TPCR*'                                                  322
      IDNUM_TPCR(1)=0                                                     323
C // LENGTH OF RDO VOLUME                                                 323
      TPCR_RDOVTHK=30.                                                    323
      CTPCR_RDOVTHK='// LENGTH OF RDO VOLUME'                             323
C // THICKNESS OF RDO CARD                                                324
      TPCR_RDOTHK=.25                                                     324
      CTPCR_RDOTHK='// THICKNESS OF RDO CARD'                             324
C // CARD LENGTH ALONG BEAM DIRECTION                                     325
      TPCR_RDOLEN=27                                                      325
      CTPCR_RDOLEN='// CARD LENGTH ALONG BEAM DIRECTION'                  325
C // NUMBER OF RDO BOARDS                                                 326
      TPCR_NRDOBRD=9                                                      326
      CTPCR_NRDOBRD='// NUMBER OF RDO BOARDS'                             326
C // RADIAL POS OF RDO BOARDS                                             328
      TPCR_RDOHT(1)=60.0                                                  328
      TPCR_RDOHT(2)=74.0                                                  328
      TPCR_RDOHT(3)=84.0                                                  328
      TPCR_RDOHT(4)=101.0                                                 328
      TPCR_RDOHT(5)=106.0                                                 328
      TPCR_RDOHT(6)=126.0                                                 328
      TPCR_RDOHT(7)=146.0                                                 328
      TPCR_RDOHT(8)=166.0                                                 328
      TPCR_RDOHT(9)=186.0                                                 328
      CTPCR_RDOHT='// RADIAL POS OF RDO BOARDS'                           328
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCR',    329
     *BPATH_TPCR,IDNUM_TPCR,1, MAP_TPCR,NAMES_TPCR,COMMEN_TPCR,           329
     *BANK_TPCR,LEN_TPCR,LLEN_TPCR,LINK_TPCR,FLAG_TPCR)                   329
C *                                                                       329
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TPCR',    329
     *BPATH_TPCR,IDNUM_TPCR,1, MAP_TPCR,NAMES_TPCR,COMMEN_TPCR,           329
     *BANK_TPCR,LEN_TPCR,LLEN_TPCR,LINK_TPCR,FLAG_TPCR)                   329
C *                                                                       330
      CALL AGSBEGM(MODULE,IPRIN)                                          330
      IF (IPRIN.LT.0) RETURN                                              330
C Filling bank TFEE - // FRONTENDELECTRONICS                              330
      BANK_TITLE='// FRONTENDELECTRONICS'                                 330
      BPATH_TFEE='TFEE*'                                                  330
      IDNUM_TFEE(1)=0                                                     331
C // VERSION OF FRONTENDELECTRONICS                                       331
      TFEE_VERS=1                                                         331
      CTFEE_VERS='// VERSION OF FRONTENDELECTRONICS'                      331
C // 1.47 SCALE FACTOR ACCOUNT FOR 9 LB OF CABLES                         332
      TFEE_CARDDX=1.47*0.110*INCH/2                                       332
      CTFEE_CARDDX='// 1.47 SCALE FACTOR ACCOUNT FOR 9 LB OF CABLES'      332
C //...                                                                   333
      TFEE_CARDDY=2.900*INCH/2                                            333
      CTFEE_CARDDY='//...'                                                333
C //...                                                                   334
      TFEE_CARDDZ=7.000*INCH/2                                            334
      CTFEE_CARDDZ='//...'                                                334
C //...                                                                   335
      TFEE_PLATEDX=0.110*INCH/2                                           335
      CTFEE_PLATEDX='//...'                                               335
C //...                                                                   336
      TFEE_PLATEDY=1.480*INCH                                             336
      CTFEE_PLATEDY='//...'                                               336
C //...                                                                   337
      TFEE_PLATEDZ=4.650*INCH/2                                           337
      CTFEE_PLATEDZ='//...'                                               337
C //...                                                                   338
      TFEE_ASSEMBLYTHICKNESS=TFEE_CARDDX + TFEE_PLATEDX                   338
      CTFEE_ASSEMBLYTHICKNESS='//...'                                     338
C //...                                                                   339
      TFEE_RIBDX=0.820*INCH/2 - 2* TFEE_CARDDX                            339
      CTFEE_RIBDX='//...'                                                 339
C //...                                                                   340
      TFEE_RIBDY=2.900*INCH/2 - 2* TFEE_CARDDX                            340
      CTFEE_RIBDY='//...'                                                 340
C //...                                                                   341
      TFEE_RIBDZ=TFEE_CARDDX                                              341
      CTFEE_RIBDZ='//...'                                                 341
C // FEE ASSEMBLY BRIK SIZE                                               342
      TFEE_ASS(1)=1.6                                                     342
      TFEE_ASS(2)=4                                                       342
      TFEE_ASS(3)=10                                                      342
      CTFEE_ASS='// FEE ASSEMBLY BRIK SIZE'                               342
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TFEE',    343
     *BPATH_TFEE,IDNUM_TFEE,1, MAP_TFEE,NAMES_TFEE,COMMEN_TFEE,           343
     *BANK_TFEE,LEN_TFEE,LLEN_TFEE,LINK_TFEE,FLAG_TFEE)                   343
C *                                                                       343
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TFEE',    343
     *BPATH_TFEE,IDNUM_TFEE,1, MAP_TFEE,NAMES_TFEE,COMMEN_TFEE,           343
     *BANK_TFEE,LEN_TFEE,LLEN_TFEE,LINK_TFEE,FLAG_TFEE)                   343
C use TECW bank                                                           344
      BPATH_TECW='TECW*'                                                  344
      IDNUM_TECW(1)=0                                                     344
      TECW_SEC=KINNER+1                                                   344
      CALL AGDGETP('TPCXGEO2','*',-999,'TECW',NTECW_SEC,RE_TECW_SEC,      344
     *BPATH_TECW,IDNUM_TECW,1,MAP_TECW,NAMES_TECW,BANK_TECW,LEN_TECW,     344
     *LLEN_TECW,LINK_TECW,FLAG_TECW)                                      344
C *                                                                       345
      CALL AGSBEGM(MODULE,IPRIN)                                          345
      IF (IPRIN.LT.0) RETURN                                              345
C Filling bank RIBS - // RIB STRUCTURE / INNER DRAWING 24A3925G           345
      BANK_TITLE='// RIB STRUCTURE / INNER DRAWING 24A3925G'              345
      BPATH_RIBS='RIBS*'                                                  345
      IDNUM_RIBS(1)=0                                                     346
C // VERSION NUMBER                                                       346
      RIBS_VERSION=KINNER                                                 346
      CRIBS_VERSION='// VERSION NUMBER'                                   346
C // SELCTOR                                                              347
      RIBS_INOUT=KINNER                                                   347
      CRIBS_INOUT='// SELCTOR'                                            347
C // NUMBER OF ELEMENTS                                                   348
      RIBS_NUM=11                                                         348
      CRIBS_NUM='// NUMBER OF ELEMENTS'                                   348
C // X                                                                    351
      RIBS_X(1)=0.000                                                     351
      RIBS_X(2)=4.261                                                     351
      RIBS_X(3)=10.561                                                    351
      RIBS_X(4)=16.861                                                    351
      RIBS_X(5)=23.161                                                    351
      RIBS_X(6)=25.950                                                    351
      RIBS_X(7)=3.726                                                     351
      RIBS_X(8)=3.726                                                     351
      RIBS_X(9)=10.026                                                    351
      RIBS_X(10)=10.026                                                   351
      RIBS_X(11)=25.480                                                   351
      CRIBS_X='// X'                                                      351
C // Y                                                                    354
      RIBS_Y(1)=0.000                                                     354
      RIBS_Y(2)=0.000                                                     354
      RIBS_Y(3)=0.000                                                     354
      RIBS_Y(4)=0.000                                                     354
      RIBS_Y(5)=0.000                                                     354
      RIBS_Y(6)=0.000                                                     354
      RIBS_Y(7)=-5.645                                                    354
      RIBS_Y(8)=5.645                                                     354
      RIBS_Y(9)=-3.895                                                    354
      RIBS_Y(10)=3.895                                                    354
      RIBS_Y(11)=0.000                                                    354
      CRIBS_Y='// Y'                                                      354
C // DX ... LAST=2*(25.950-(1.050+2.515+21.915))                          365
      RIBS_DX(1)=1.470                                                    365
      RIBS_DX(2)=0.375                                                    365
      RIBS_DX(3)=0.375                                                    365
      RIBS_DX(4)=0.375                                                    365
      RIBS_DX(5)=0.375                                                    365
      RIBS_DX(6)=TECW_HEIGHT/INCH-25.950                                  365
      RIBS_DX(7)=1.070                                                    365
      RIBS_DX(8)=1.070                                                    365
      RIBS_DX(9)=1.070                                                    365
      RIBS_DX(10)=1.070                                                   365
      RIBS_DX(11)=0.940                                                   365
      CRIBS_DX='// DX ... LAST=2*(25.950-(1.050+2.515+21.915))'           365
C // DY                                                                   367
      RIBS_DY(1)=0.                                                       367
      RIBS_DY(2)=0.                                                       367
      RIBS_DY(3)=0.                                                       367
      RIBS_DY(4)=0.                                                       367
      RIBS_DY(5)=0.                                                       367
      RIBS_DY(6)=0.                                                       367
      RIBS_DY(7)=0.750                                                    367
      RIBS_DY(8)=0.750                                                    367
      RIBS_DY(9)=0.750                                                    367
      RIBS_DY(10)=0.750                                                   367
      RIBS_DY(11)=2.0                                                     367
      CRIBS_DY='// DY'                                                    367
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RIBS',    368
     *BPATH_RIBS,IDNUM_RIBS,1, MAP_RIBS,NAMES_RIBS,COMMEN_RIBS,           368
     *BANK_RIBS,LEN_RIBS,LLEN_RIBS,LINK_RIBS,FLAG_RIBS)                   368
C *                                                                       368
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RIBS',    368
     *BPATH_RIBS,IDNUM_RIBS,1, MAP_RIBS,NAMES_RIBS,COMMEN_RIBS,           368
     *BANK_RIBS,LEN_RIBS,LLEN_RIBS,LINK_RIBS,FLAG_RIBS)                   368
C use TECW bank                                                           369
      BPATH_TECW='TECW*'                                                  369
      IDNUM_TECW(1)=0                                                     369
      TECW_SEC=KOUTER+1                                                   369
      CALL AGDGETP('TPCXGEO2','*',-999,'TECW',NTECW_SEC,RE_TECW_SEC,      369
     *BPATH_TECW,IDNUM_TECW,1,MAP_TECW,NAMES_TECW,BANK_TECW,LEN_TECW,     369
     *LLEN_TECW,LINK_TECW,FLAG_TECW)                                      369
C *                                                                       370
      CALL AGSBEGM(MODULE,IPRIN)                                          370
      IF (IPRIN.LT.0) RETURN                                              370
C Filling bank RIBS - // RIB STRUCTURE / OUTER DRAWING 24A3925G           370
      BANK_TITLE='// RIB STRUCTURE / OUTER DRAWING 24A3925G'              370
      BPATH_RIBS='RIBS*'                                                  370
      IDNUM_RIBS(1)=0                                                     371
C // VERSION NUMBER                                                       371
      RIBS_VERSION=KOUTER                                                 371
      CRIBS_VERSION='// VERSION NUMBER'                                   371
C // SELCTOR                                                              372
      RIBS_INOUT=KOUTER                                                   372
      CRIBS_INOUT='// SELCTOR'                                            372
C // NUMBER OF ELEMENTS                                                   373
      RIBS_NUM=14                                                         373
      CRIBS_NUM='// NUMBER OF ELEMENTS'                                   373
C // X                                                                    378
      RIBS_X(1)=0.280                                                     378
      RIBS_X(2)=4.261                                                     378
      RIBS_X(3)=7.410                                                     378
      RIBS_X(4)=10.560                                                    378
      RIBS_X(5)=13.710                                                    378
      RIBS_X(6)=16.859                                                    378
      RIBS_X(7)=20.009                                                    378
      RIBS_X(8)=23.158                                                    378
      RIBS_X(9)=26.309                                                    378
      RIBS_X(10)=6.023                                                    378
      RIBS_X(11)=6.023                                                    378
      RIBS_X(12)=12.3225                                                  378
      RIBS_X(13)=15.472                                                   378
      RIBS_X(14)=26.309-1.575/2                                           378
      CRIBS_X='// X'                                                      378
C // Y                                                                    382
      RIBS_Y(1)=0                                                         382
      RIBS_Y(2)=0                                                         382
      RIBS_Y(3)=0                                                         382
      RIBS_Y(4)=0                                                         382
      RIBS_Y(5)=0                                                         382
      RIBS_Y(6)=0                                                         382
      RIBS_Y(7)=0                                                         382
      RIBS_Y(8)=0                                                         382
      RIBS_Y(9)=0                                                         382
      RIBS_Y(10)=17.250/2                                                 382
      RIBS_Y(11)=-17.250/2                                                382
      RIBS_Y(12)=0                                                        382
      RIBS_Y(13)=0                                                        382
      RIBS_Y(14)=0                                                        382
      CRIBS_Y='// Y'                                                      382
C // DX                                                                   385
      RIBS_DX(1)=1.268                                                    385
      RIBS_DX(2)=0.375                                                    385
      RIBS_DX(3)=0.375                                                    385
      RIBS_DX(4)=0.375                                                    385
      RIBS_DX(5)=0.375                                                    385
      RIBS_DX(6)=0.375                                                    385
      RIBS_DX(7)=0.375                                                    385
      RIBS_DX(8)=0.375                                                    385
      RIBS_DX(9)=1.566                                                    385
      RIBS_DX(10)=2.774                                                   385
      RIBS_DX(11)=2.774                                                   385
      RIBS_DX(12)=2.775                                                   385
      RIBS_DX(13)=2.774                                                   385
      RIBS_DX(14)=1.575                                                   385
      CRIBS_DX='// DX'                                                    385
C // DY                                                                   388
      RIBS_DY(1)=0                                                        388
      RIBS_DY(2)=0                                                        388
      RIBS_DY(3)=0                                                        388
      RIBS_DY(4)=0                                                        388
      RIBS_DY(5)=0                                                        388
      RIBS_DY(6)=0                                                        388
      RIBS_DY(7)=0                                                        388
      RIBS_DY(8)=0                                                        388
      RIBS_DY(9)=0                                                        388
      RIBS_DY(10)=1.250                                                   388
      RIBS_DY(11)=1.250                                                   388
      RIBS_DY(12)=0.375                                                   388
      RIBS_DY(13)=0.375                                                   388
      RIBS_DY(14)=1.5680                                                  388
      CRIBS_DY='// DY'                                                    388
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RIBS',    389
     *BPATH_RIBS,IDNUM_RIBS,1, MAP_RIBS,NAMES_RIBS,COMMEN_RIBS,           389
     *BANK_RIBS,LEN_RIBS,LLEN_RIBS,LINK_RIBS,FLAG_RIBS)                   389
C *                                                                       389
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'RIBS',    389
     *BPATH_RIBS,IDNUM_RIBS,1, MAP_RIBS,NAMES_RIBS,COMMEN_RIBS,           389
     *BANK_RIBS,LEN_RIBS,LLEN_RIBS,LINK_RIBS,FLAG_RIBS)                   389
C *                                                                       390
      CALL AGSBEGM(MODULE,IPRIN)                                          390
      IF (IPRIN.LT.0) RETURN                                              390
C Filling bank HOLZ - // HOLES PER ROW INNER                              390
      BANK_TITLE='// HOLES PER ROW INNER'                                 390
      BPATH_HOLZ='HOLZ*'                                                  390
      IDNUM_HOLZ(1)=0                                                     391
C // SELECTOR                                                             391
      HOLZ_INOUT=KINNER                                                   391
      CHOLZ_INOUT='// SELECTOR'                                           391
C // NUMBER OF ROWS                                                       392
      HOLZ_NUMBEROFROWS=15                                                392
      CHOLZ_NUMBEROFROWS='// NUMBER OF ROWS'                              392
C // N/ROW                                                                394
      HOLZ_NUMBERPERROW(1)=6                                              394
      HOLZ_NUMBERPERROW(2)=5                                              394
      HOLZ_NUMBERPERROW(3)=5                                              394
      HOLZ_NUMBERPERROW(4)=2                                              394
      HOLZ_NUMBERPERROW(5)=5                                              394
      HOLZ_NUMBERPERROW(6)=4                                              394
      HOLZ_NUMBERPERROW(7)=4                                              394
      HOLZ_NUMBERPERROW(8)=4                                              394
      HOLZ_NUMBERPERROW(9)=4                                              394
      HOLZ_NUMBERPERROW(10)=3                                             394
      HOLZ_NUMBERPERROW(11)=3                                             394
      HOLZ_NUMBERPERROW(12)=3                                             394
      HOLZ_NUMBERPERROW(13)=3                                             394
      HOLZ_NUMBERPERROW(14)=2                                             394
      HOLZ_NUMBERPERROW(15)=2                                             394
      HOLZ_NUMBERPERROW(16)=0                                             394
      CHOLZ_NUMBERPERROW='// N/ROW'                                       394
C // Y HOLES                                                              466
      HOLZ_YHOLES(1)=3.180*(-2.5+0)                                       466
      HOLZ_YHOLES(2)=3.180*(-2.5+1)                                       466
      HOLZ_YHOLES(3)=3.180*(-2.5+2)                                       466
      HOLZ_YHOLES(4)=3.180*(-2.5+3)                                       466
      HOLZ_YHOLES(5)=3.180*(-2.5+4)                                       466
      HOLZ_YHOLES(6)=3.180*(-2.5+5)                                       466
      HOLZ_YHOLES(7)=3.180*(-2.5+6)                                       466
      HOLZ_YHOLES(8)=3.180*(-2.5+7)                                       466
      HOLZ_YHOLES(9)=3.180*(-2.5+8)                                       466
      HOLZ_YHOLES(10)=3.764*(-2.0+0)                                      466
      HOLZ_YHOLES(11)=3.764*(-2.0+1)                                      466
      HOLZ_YHOLES(12)=3.764*(-2.0+2)                                      466
      HOLZ_YHOLES(13)=3.764*(-2.0+3)                                      466
      HOLZ_YHOLES(14)=3.764*(-2.0+4)                                      466
      HOLZ_YHOLES(15)=3.764*(-2.0+5)                                      466
      HOLZ_YHOLES(16)=3.764*(-2.0+6)                                      466
      HOLZ_YHOLES(17)=3.764*(-2.0+7)                                      466
      HOLZ_YHOLES(18)=3.764*(-2.0+8)                                      466
      HOLZ_YHOLES(19)=3.553*(-2.0+0)                                      466
      HOLZ_YHOLES(20)=3.553*(-2.0+1)                                      466
      HOLZ_YHOLES(21)=3.553*(-2.0+2)                                      466
      HOLZ_YHOLES(22)=3.553*(-2.0+3)                                      466
      HOLZ_YHOLES(23)=3.553*(-2.0+4)                                      466
      HOLZ_YHOLES(24)=3.553*(-2.0+5)                                      466
      HOLZ_YHOLES(25)=3.553*(-2.0+6)                                      466
      HOLZ_YHOLES(26)=3.553*(-2.0+7)                                      466
      HOLZ_YHOLES(27)=3.553*(-2.0+8)                                      466
      HOLZ_YHOLES(28)=13.368*(-0.5+0)                                     466
      HOLZ_YHOLES(29)=13.368*(-0.5+1)                                     466
      HOLZ_YHOLES(30)=13.368*(-0.5+2)                                     466
      HOLZ_YHOLES(31)=13.368*(-0.5+3)                                     466
      HOLZ_YHOLES(32)=13.368*(-0.5+4)                                     466
      HOLZ_YHOLES(33)=13.368*(-0.5+5)                                     466
      HOLZ_YHOLES(34)=13.368*(-0.5+6)                                     466
      HOLZ_YHOLES(35)=13.368*(-0.5+7)                                     466
      HOLZ_YHOLES(36)=13.368*(-0.5+8)                                     466
      HOLZ_YHOLES(37)=3.131*(-2.0+0)                                      466
      HOLZ_YHOLES(38)=3.131*(-2.0+1)                                      466
      HOLZ_YHOLES(39)=3.131*(-2.0+2)                                      466
      HOLZ_YHOLES(40)=3.131*(-2.0+3)                                      466
      HOLZ_YHOLES(41)=3.131*(-2.0+4)                                      466
      HOLZ_YHOLES(42)=3.131*(-2.0+5)                                      466
      HOLZ_YHOLES(43)=3.131*(-2.0+6)                                      466
      HOLZ_YHOLES(44)=3.131*(-2.0+7)                                      466
      HOLZ_YHOLES(45)=3.131*(-2.0+8)                                      466
      HOLZ_YHOLES(46)=-3.892 - 1.948                                      466
      HOLZ_YHOLES(47)=-1.948                                              466
      HOLZ_YHOLES(48)=+1.948                                              466
      HOLZ_YHOLES(49)=3.892 + 1.948                                       466
      HOLZ_YHOLES(50)=0.000                                               466
      HOLZ_YHOLES(51)=0.000                                               466
      HOLZ_YHOLES(52)=0.000                                               466
      HOLZ_YHOLES(53)=0.000                                               466
      HOLZ_YHOLES(54)=0.000                                               466
      HOLZ_YHOLES(55)=-3.611 - 1.807                                      466
      HOLZ_YHOLES(56)=-1.807                                              466
      HOLZ_YHOLES(57)=+1.807                                              466
      HOLZ_YHOLES(58)=3.611 + 1.807                                       466
      HOLZ_YHOLES(59)=0.000                                               466
      HOLZ_YHOLES(60)=0.000                                               466
      HOLZ_YHOLES(61)=0.000                                               466
      HOLZ_YHOLES(62)=0.000                                               466
      HOLZ_YHOLES(63)=0.000                                               466
      HOLZ_YHOLES(64)=-3.281 - 1.7145                                     466
      HOLZ_YHOLES(65)=-1.7145                                             466
      HOLZ_YHOLES(66)=+1.7145                                             466
      HOLZ_YHOLES(67)=3.281 + 1.7145                                      466
      HOLZ_YHOLES(68)=0.000                                               466
      HOLZ_YHOLES(69)=0.000                                               466
      HOLZ_YHOLES(70)=0.000                                               466
      HOLZ_YHOLES(71)=0.000                                               466
      HOLZ_YHOLES(72)=0.000                                               466
      HOLZ_YHOLES(73)=3.150*(-1.5+0)                                      466
      HOLZ_YHOLES(74)=3.150*(-1.5+1)                                      466
      HOLZ_YHOLES(75)=3.150*(-1.5+2)                                      466
      HOLZ_YHOLES(76)=3.150*(-1.5+3)                                      466
      HOLZ_YHOLES(77)=3.150*(-1.5+4)                                      466
      HOLZ_YHOLES(78)=3.150*(-1.5+5)                                      466
      HOLZ_YHOLES(79)=3.150*(-1.5+6)                                      466
      HOLZ_YHOLES(80)=3.150*(-1.5+7)                                      466
      HOLZ_YHOLES(81)=3.150*(-1.5+8)                                      466
      HOLZ_YHOLES(82)=4.152*(-1.0+0)                                      466
      HOLZ_YHOLES(83)=4.152*(-1.0+1)                                      466
      HOLZ_YHOLES(84)=4.152*(-1.0+2)                                      466
      HOLZ_YHOLES(85)=4.152*(-1.0+3)                                      466
      HOLZ_YHOLES(86)=4.152*(-1.0+4)                                      466
      HOLZ_YHOLES(87)=4.152*(-1.0+5)                                      466
      HOLZ_YHOLES(88)=4.152*(-1.0+6)                                      466
      HOLZ_YHOLES(89)=4.152*(-1.0+7)                                      466
      HOLZ_YHOLES(90)=4.152*(-1.0+8)                                      466
      HOLZ_YHOLES(91)=3.730*(-1.0+0)                                      466
      HOLZ_YHOLES(92)=3.730*(-1.0+1)                                      466
      HOLZ_YHOLES(93)=3.730*(-1.0+2)                                      466
      HOLZ_YHOLES(94)=3.730*(-1.0+3)                                      466
      HOLZ_YHOLES(95)=3.730*(-1.0+4)                                      466
      HOLZ_YHOLES(96)=3.730*(-1.0+5)                                      466
      HOLZ_YHOLES(97)=3.730*(-1.0+6)                                      466
      HOLZ_YHOLES(98)=3.730*(-1.0+7)                                      466
      HOLZ_YHOLES(99)=3.730*(-1.0+8)                                      466
      HOLZ_YHOLES(100)=3.308*(-1.0+0)                                     466
      HOLZ_YHOLES(101)=3.308*(-1.0+1)                                     466
      HOLZ_YHOLES(102)=3.308*(-1.0+2)                                     466
      HOLZ_YHOLES(103)=3.308*(-1.0+3)                                     466
      HOLZ_YHOLES(104)=3.308*(-1.0+4)                                     466
      HOLZ_YHOLES(105)=3.308*(-1.0+5)                                     466
      HOLZ_YHOLES(106)=3.308*(-1.0+6)                                     466
      HOLZ_YHOLES(107)=3.308*(-1.0+7)                                     466
      HOLZ_YHOLES(108)=3.308*(-1.0+8)                                     466
      HOLZ_YHOLES(109)=3.075*(-1.0+0)                                     466
      HOLZ_YHOLES(110)=3.075*(-1.0+1)                                     466
      HOLZ_YHOLES(111)=3.075*(-1.0+2)                                     466
      HOLZ_YHOLES(112)=3.075*(-1.0+3)                                     466
      HOLZ_YHOLES(113)=3.075*(-1.0+4)                                     466
      HOLZ_YHOLES(114)=3.075*(-1.0+5)                                     466
      HOLZ_YHOLES(115)=3.075*(-1.0+6)                                     466
      HOLZ_YHOLES(116)=3.075*(-1.0+7)                                     466
      HOLZ_YHOLES(117)=3.075*(-1.0+8)                                     466
      HOLZ_YHOLES(118)=4.927*(-0.5+0)                                     466
      HOLZ_YHOLES(119)=4.927*(-0.5+1)                                     466
      HOLZ_YHOLES(120)=4.927*(-0.5+2)                                     466
      HOLZ_YHOLES(121)=4.927*(-0.5+3)                                     466
      HOLZ_YHOLES(122)=4.927*(-0.5+4)                                     466
      HOLZ_YHOLES(123)=4.927*(-0.5+5)                                     466
      HOLZ_YHOLES(124)=4.927*(-0.5+6)                                     466
      HOLZ_YHOLES(125)=4.927*(-0.5+7)                                     466
      HOLZ_YHOLES(126)=4.927*(-0.5+8)                                     466
      HOLZ_YHOLES(127)=4.083*(-0.5+0)                                     466
      HOLZ_YHOLES(128)=4.083*(-0.5+1)                                     466
      HOLZ_YHOLES(129)=4.083*(-0.5+2)                                     466
      HOLZ_YHOLES(130)=4.083*(-0.5+3)                                     466
      HOLZ_YHOLES(131)=4.083*(-0.5+4)                                     466
      HOLZ_YHOLES(132)=4.083*(-0.5+5)                                     466
      HOLZ_YHOLES(133)=4.083*(-0.5+6)                                     466
      HOLZ_YHOLES(134)=4.083*(-0.5+7)                                     466
      HOLZ_YHOLES(135)=4.083*(-0.5+8)                                     466
      HOLZ_YHOLES(136)=0.000                                              466
      HOLZ_YHOLES(137)=0.000                                              466
      HOLZ_YHOLES(138)=0.000                                              466
      HOLZ_YHOLES(139)=0.000                                              466
      HOLZ_YHOLES(140)=0.000                                              466
      HOLZ_YHOLES(141)=0.000                                              466
      HOLZ_YHOLES(142)=0.000                                              466
      HOLZ_YHOLES(143)=0.000                                              466
      HOLZ_YHOLES(144)=0.000                                              466
      CHOLZ_YHOLES='// Y HOLES'                                           466
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'HOLZ',    467
     *BPATH_HOLZ,IDNUM_HOLZ,1, MAP_HOLZ,NAMES_HOLZ,COMMEN_HOLZ,           467
     *BANK_HOLZ,LEN_HOLZ,LLEN_HOLZ,LINK_HOLZ,FLAG_HOLZ)                   467
C *                                                                       467
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'HOLZ',    467
     *BPATH_HOLZ,IDNUM_HOLZ,1, MAP_HOLZ,NAMES_HOLZ,COMMEN_HOLZ,           467
     *BANK_HOLZ,LEN_HOLZ,LLEN_HOLZ,LINK_HOLZ,FLAG_HOLZ)                   467
C *                                                                       468
      CALL AGSBEGM(MODULE,IPRIN)                                          468
      IF (IPRIN.LT.0) RETURN                                              468
C Filling bank HOLZ - // HOLES PER ROW OUTER                              468
      BANK_TITLE='// HOLES PER ROW OUTER'                                 468
      BPATH_HOLZ='HOLZ*'                                                  468
      IDNUM_HOLZ(1)=0                                                     469
C // SELECTOR                                                             469
      HOLZ_INOUT=KOUTER                                                   469
      CHOLZ_INOUT='// SELECTOR'                                           469
C // NUMBER OF ROWS                                                       470
      HOLZ_NUMBEROFROWS=16                                                470
      CHOLZ_NUMBEROFROWS='// NUMBER OF ROWS'                              470
C // N/ROW                                                                472
      HOLZ_NUMBERPERROW(1)=9                                              472
      HOLZ_NUMBERPERROW(2)=9                                              472
      HOLZ_NUMBERPERROW(3)=9                                              472
      HOLZ_NUMBERPERROW(4)=9                                              472
      HOLZ_NUMBERPERROW(5)=9                                              472
      HOLZ_NUMBERPERROW(6)=8                                              472
      HOLZ_NUMBERPERROW(7)=8                                              472
      HOLZ_NUMBERPERROW(8)=8                                              472
      HOLZ_NUMBERPERROW(9)=8                                              472
      HOLZ_NUMBERPERROW(10)=8                                             472
      HOLZ_NUMBERPERROW(11)=7                                             472
      HOLZ_NUMBERPERROW(12)=7                                             472
      HOLZ_NUMBERPERROW(13)=7                                             472
      HOLZ_NUMBERPERROW(14)=7                                             472
      HOLZ_NUMBERPERROW(15)=7                                             472
      HOLZ_NUMBERPERROW(16)=6                                             472
      CHOLZ_NUMBERPERROW='// N/ROW'                                       472
C // Y HOLES                                                              508
      HOLZ_YHOLES(1)=-15.025                                              508
      HOLZ_YHOLES(2)=-11.606                                              508
      HOLZ_YHOLES(3)=-8.177                                               508
      HOLZ_YHOLES(4)=-4.220                                               508
      HOLZ_YHOLES(5)=0                                                    508
      HOLZ_YHOLES(6)=4.220                                                508
      HOLZ_YHOLES(7)=8.177                                                508
      HOLZ_YHOLES(8)=11.606                                               508
      HOLZ_YHOLES(9)=15.025                                               508
      HOLZ_YHOLES(10)=-15.025                                             508
      HOLZ_YHOLES(11)=-11.606                                             508
      HOLZ_YHOLES(12)=-8.177                                              508
      HOLZ_YHOLES(13)=-4.220                                              508
      HOLZ_YHOLES(14)=0                                                   508
      HOLZ_YHOLES(15)=4.220                                               508
      HOLZ_YHOLES(16)=8.177                                               508
      HOLZ_YHOLES(17)=11.606                                              508
      HOLZ_YHOLES(18)=15.025                                              508
      HOLZ_YHOLES(19)=-14.243                                             508
      HOLZ_YHOLES(20)=-11.078                                             508
      HOLZ_YHOLES(21)=-6.330                                              508
      HOLZ_YHOLES(22)=-3.165                                              508
      HOLZ_YHOLES(23)=0                                                   508
      HOLZ_YHOLES(24)=3.165                                               508
      HOLZ_YHOLES(25)=6.330                                               508
      HOLZ_YHOLES(26)=11.078                                              508
      HOLZ_YHOLES(27)=14.243                                              508
      HOLZ_YHOLES(28)=-14.243                                             508
      HOLZ_YHOLES(29)=-11.078                                             508
      HOLZ_YHOLES(30)=-6.330                                              508
      HOLZ_YHOLES(31)=-3.165                                              508
      HOLZ_YHOLES(32)=0                                                   508
      HOLZ_YHOLES(33)=3.165                                               508
      HOLZ_YHOLES(34)=6.330                                               508
      HOLZ_YHOLES(35)=11.078                                              508
      HOLZ_YHOLES(36)=14.243                                              508
      HOLZ_YHOLES(37)=-13.452                                             508
      HOLZ_YHOLES(38)=-10.023                                             508
      HOLZ_YHOLES(39)=-6.330                                              508
      HOLZ_YHOLES(40)=-3.165                                              508
      HOLZ_YHOLES(41)=0                                                   508
      HOLZ_YHOLES(42)=3.165                                               508
      HOLZ_YHOLES(43)=6.330                                               508
      HOLZ_YHOLES(44)=10.023                                              508
      HOLZ_YHOLES(45)=13.452                                              508
      HOLZ_YHOLES(46)=-13.452                                             508
      HOLZ_YHOLES(47)=-10.023                                             508
      HOLZ_YHOLES(48)=-6.330                                              508
      HOLZ_YHOLES(49)=-3.165                                              508
      HOLZ_YHOLES(50)=3.165                                               508
      HOLZ_YHOLES(51)=6.330                                               508
      HOLZ_YHOLES(52)=10.023                                              508
      HOLZ_YHOLES(53)=13.452                                              508
      HOLZ_YHOLES(54)=0                                                   508
      HOLZ_YHOLES(55)=-12.660                                             508
      HOLZ_YHOLES(56)=-9.495                                              508
      HOLZ_YHOLES(57)=-6.330                                              508
      HOLZ_YHOLES(58)=-2.110                                              508
      HOLZ_YHOLES(59)=2.110                                               508
      HOLZ_YHOLES(60)=6.330                                               508
      HOLZ_YHOLES(61)=9.495                                               508
      HOLZ_YHOLES(62)=12.660                                              508
      HOLZ_YHOLES(63)=0                                                   508
      HOLZ_YHOLES(64)=-12.660                                             508
      HOLZ_YHOLES(65)=-9.495                                              508
      HOLZ_YHOLES(66)=-6.330                                              508
      HOLZ_YHOLES(67)=-2.110                                              508
      HOLZ_YHOLES(68)=2.110                                               508
      HOLZ_YHOLES(69)=6.330                                               508
      HOLZ_YHOLES(70)=9.495                                               508
      HOLZ_YHOLES(71)=12.660                                              508
      HOLZ_YHOLES(72)=0                                                   508
      HOLZ_YHOLES(73)=-11.870                                             508
      HOLZ_YHOLES(74)=-8.440                                              508
      HOLZ_YHOLES(75)=-5.275                                              508
      HOLZ_YHOLES(76)=-2.110                                              508
      HOLZ_YHOLES(77)=2.110                                               508
      HOLZ_YHOLES(78)=5.275                                               508
      HOLZ_YHOLES(79)=8.440                                               508
      HOLZ_YHOLES(80)=11.870                                              508
      HOLZ_YHOLES(81)=0                                                   508
      HOLZ_YHOLES(82)=-11.870                                             508
      HOLZ_YHOLES(83)=-8.440                                              508
      HOLZ_YHOLES(84)=-5.275                                              508
      HOLZ_YHOLES(85)=-2.110                                              508
      HOLZ_YHOLES(86)=2.110                                               508
      HOLZ_YHOLES(87)=5.275                                               508
      HOLZ_YHOLES(88)=8.440                                               508
      HOLZ_YHOLES(89)=11.870                                              508
      HOLZ_YHOLES(90)=0                                                   508
      HOLZ_YHOLES(91)=-11.078                                             508
      HOLZ_YHOLES(92)=-7.649                                              508
      HOLZ_YHOLES(93)=-4.220                                              508
      HOLZ_YHOLES(94)=0                                                   508
      HOLZ_YHOLES(95)=4.220                                               508
      HOLZ_YHOLES(96)=7.649                                               508
      HOLZ_YHOLES(97)=11.078                                              508
      HOLZ_YHOLES(98)=0                                                   508
      HOLZ_YHOLES(99)=0                                                   508
      HOLZ_YHOLES(100)=-11.078                                            508
      HOLZ_YHOLES(101)=-7.649                                             508
      HOLZ_YHOLES(102)=-4.220                                             508
      HOLZ_YHOLES(103)=0                                                  508
      HOLZ_YHOLES(104)=4.220                                              508
      HOLZ_YHOLES(105)=7.649                                              508
      HOLZ_YHOLES(106)=11.078                                             508
      HOLZ_YHOLES(107)=0                                                  508
      HOLZ_YHOLES(108)=0                                                  508
      HOLZ_YHOLES(109)=-10.022                                            508
      HOLZ_YHOLES(110)=-6.858                                             508
      HOLZ_YHOLES(111)=-3.429                                             508
      HOLZ_YHOLES(112)=0                                                  508
      HOLZ_YHOLES(113)=3.429                                              508
      HOLZ_YHOLES(114)=6.858                                              508
      HOLZ_YHOLES(115)=10.022                                             508
      HOLZ_YHOLES(116)=0                                                  508
      HOLZ_YHOLES(117)=0                                                  508
      HOLZ_YHOLES(118)=-10.022                                            508
      HOLZ_YHOLES(119)=-6.858                                             508
      HOLZ_YHOLES(120)=-3.429                                             508
      HOLZ_YHOLES(121)=0                                                  508
      HOLZ_YHOLES(122)=3.429                                              508
      HOLZ_YHOLES(123)=6.858                                              508
      HOLZ_YHOLES(124)=10.022                                             508
      HOLZ_YHOLES(125)=0                                                  508
      HOLZ_YHOLES(126)=0                                                  508
      HOLZ_YHOLES(127)=-9.495                                             508
      HOLZ_YHOLES(128)=-6.330                                             508
      HOLZ_YHOLES(129)=-3.165                                             508
      HOLZ_YHOLES(130)=0                                                  508
      HOLZ_YHOLES(131)=3.165                                              508
      HOLZ_YHOLES(132)=6.330                                              508
      HOLZ_YHOLES(133)=9.495                                              508
      HOLZ_YHOLES(134)=0                                                  508
      HOLZ_YHOLES(135)=0                                                  508
      HOLZ_YHOLES(136)=-9.495                                             508
      HOLZ_YHOLES(137)=-6.330                                             508
      HOLZ_YHOLES(138)=-3.165                                             508
      HOLZ_YHOLES(139)=3.165                                              508
      HOLZ_YHOLES(140)=6.330                                              508
      HOLZ_YHOLES(141)=9.495                                              508
      HOLZ_YHOLES(142)=0                                                  508
      HOLZ_YHOLES(143)=0                                                  508
      HOLZ_YHOLES(144)=0                                                  508
      CHOLZ_YHOLES='// Y HOLES'                                           508
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'HOLZ',    509
     *BPATH_HOLZ,IDNUM_HOLZ,1, MAP_HOLZ,NAMES_HOLZ,COMMEN_HOLZ,           509
     *BANK_HOLZ,LEN_HOLZ,LLEN_HOLZ,LINK_HOLZ,FLAG_HOLZ)                   509
C *                                                                       509
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'HOLZ',    509
     *BPATH_HOLZ,IDNUM_HOLZ,1, MAP_HOLZ,NAMES_HOLZ,COMMEN_HOLZ,           509
     *BANK_HOLZ,LEN_HOLZ,LLEN_HOLZ,LINK_HOLZ,FLAG_HOLZ)                   509
C *                                                                       510
      CALL AGSBEGM(MODULE,IPRIN)                                          510
      IF (IPRIN.LT.0) RETURN                                              510
C Filling bank COOL - // COOLING TUBE LENGTHS / INNER                     510
      BANK_TITLE='// COOLING TUBE LENGTHS / INNER'                        510
      BPATH_COOL='COOL*'                                                  510
      IDNUM_COOL(1)=0                                                     511
C // INNER VS OUTER                                                       511
      COOL_INOUT=KINNER                                                   511
      CCOOL_INOUT='// INNER VS OUTER'                                     511
C // TUBE LENGTHS                                                         515
      COOL_TUBELENGTH(1)=19.300                                           515
      COOL_TUBELENGTH(2)=18.454                                           515
      COOL_TUBELENGTH(3)=17.610                                           515
      COOL_TUBELENGTH(4)=16.766                                           515
      COOL_TUBELENGTH(5)=15.922                                           515
      COOL_TUBELENGTH(6)=15.078                                           515
      COOL_TUBELENGTH(7)=14.235                                           515
      COOL_TUBELENGTH(8)=13.390                                           515
      COOL_TUBELENGTH(9)=12.546                                           515
      COOL_TUBELENGTH(10)=11.704                                          515
      COOL_TUBELENGTH(11)=10.860                                          515
      COOL_TUBELENGTH(12)=10.016                                          515
      COOL_TUBELENGTH(13)=9.172                                           515
      COOL_TUBELENGTH(14)=8.327                                           515
      COOL_TUBELENGTH(15)=7.483                                           515
      COOL_TUBELENGTH(16)=0.000                                           515
      CCOOL_TUBELENGTH='// TUBE LENGTHS'                                  515
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'COOL',    516
     *BPATH_COOL,IDNUM_COOL,1, MAP_COOL,NAMES_COOL,COMMEN_COOL,           516
     *BANK_COOL,LEN_COOL,LLEN_COOL,LINK_COOL,FLAG_COOL)                   516
C *                                                                       516
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'COOL',    516
     *BPATH_COOL,IDNUM_COOL,1, MAP_COOL,NAMES_COOL,COMMEN_COOL,           516
     *BANK_COOL,LEN_COOL,LLEN_COOL,LINK_COOL,FLAG_COOL)                   516
C *                                                                       517
      CALL AGSBEGM(MODULE,IPRIN)                                          517
      IF (IPRIN.LT.0) RETURN                                              517
C Filling bank COOL - // COOLING TUBE LENGTHS / OUTER                     517
      BANK_TITLE='// COOLING TUBE LENGTHS / OUTER'                        517
      BPATH_COOL='COOL*'                                                  517
      IDNUM_COOL(1)=0                                                     518
C // INNER VS OUTER                                                       518
      COOL_INOUT=KOUTER                                                   518
      CCOOL_INOUT='// INNER VS OUTER'                                     518
C // TUBE LENGTHS                                                         522
      COOL_TUBELENGTH(1)=33.620                                           522
      COOL_TUBELENGTH(2)=33.605                                           522
      COOL_TUBELENGTH(3)=32.761                                           522
      COOL_TUBELENGTH(4)=31.917                                           522
      COOL_TUBELENGTH(5)=31.073                                           522
      COOL_TUBELENGTH(6)=30.229                                           522
      COOL_TUBELENGTH(7)=29.385                                           522
      COOL_TUBELENGTH(8)=28.541                                           522
      COOL_TUBELENGTH(9)=27.697                                           522
      COOL_TUBELENGTH(10)=26.853                                          522
      COOL_TUBELENGTH(11)=26.009                                          522
      COOL_TUBELENGTH(12)=25.165                                          522
      COOL_TUBELENGTH(13)=24.321                                          522
      COOL_TUBELENGTH(14)=23.478                                          522
      COOL_TUBELENGTH(15)=22.634                                          522
      COOL_TUBELENGTH(16)=21.790                                          522
      CCOOL_TUBELENGTH='// TUBE LENGTHS'                                  522
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'COOL',    523
     *BPATH_COOL,IDNUM_COOL,1, MAP_COOL,NAMES_COOL,COMMEN_COOL,           523
     *BANK_COOL,LEN_COOL,LLEN_COOL,LINK_COOL,FLAG_COOL)                   523
C *                                                                       523
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'COOL',    523
     *BPATH_COOL,IDNUM_COOL,1, MAP_COOL,NAMES_COOL,COMMEN_COOL,           523
     *BANK_COOL,LEN_COOL,LLEN_COOL,LINK_COOL,FLAG_COOL)                   523
C use default TFEE bank                                                   524
      BPATH_TFEE='TFEE*'                                                  524
      IDNUM_TFEE(1)=0                                                     524
      CALL AGDGETP('TPCXGEO2','*',-999,'TFEE',' ',BANK_TFEE, BPATH_TFEE,  524
     *IDNUM_TFEE,1,MAP_TFEE,NAMES_TFEE,BANK_TFEE,LEN_TFEE,LLEN_TFEE,      524
     *LINK_TFEE,FLAG_TFEE)                                                524
C use default TECW bank                                                   525
      BPATH_TECW='TECW*'                                                  525
      IDNUM_TECW(1)=0                                                     525
      CALL AGDGETP('TPCXGEO2','*',-999,'TECW',' ',BANK_TECW, BPATH_TECW,  525
     *IDNUM_TECW,1,MAP_TECW,NAMES_TECW,BANK_TECW,LEN_TECW,LLEN_TECW,      525
     *LINK_TECW,FLAG_TECW)                                                525
C use default TPCR bank                                                   526
      BPATH_TPCR='TPCR*'                                                  526
      IDNUM_TPCR(1)=0                                                     526
      CALL AGDGETP('TPCXGEO2','*',-999,'TPCR',' ',BANK_TPCR, BPATH_TPCR,  526
     *IDNUM_TPCR,1,MAP_TPCR,NAMES_TPCR,BANK_TPCR,LEN_TPCR,LLEN_TPCR,      526
     *LINK_TPCR,FLAG_TPCR)                                                526
C use default TPCC bank                                                   527
      BPATH_TPCC='TPCC*'                                                  527
      IDNUM_TPCC(1)=0                                                     527
      CALL AGDGETP('TPCXGEO2','*',-999,'TPCC',' ',BANK_TPCC, BPATH_TPCC,  527
     *IDNUM_TPCC,1,MAP_TPCC,NAMES_TPCC,BANK_TPCC,LEN_TPCC,LLEN_TPCC,      527
     *LINK_TPCC,FLAG_TPCC)                                                527
C use TPCG bank                                                           528
      BPATH_TPCG='TPCG*'                                                  528
      IDNUM_TPCG(1)=0                                                     528
      TPCG_VERSION=TPCC_VERSION                                           528
      CALL AGDGETP('TPCXGEO2','*',-999,'TPCG',NTPCG_VERSION,              528
     *RE_TPCG_VERSION,BPATH_TPCG,IDNUM_TPCG,1,MAP_TPCG,NAMES_TPCG,        528
     *BANK_TPCG,LEN_TPCG,LLEN_TPCG,LINK_TPCG,FLAG_TPCG)                   528
C GAS PLUS ENDCAPS                                                        529
      TOFCLENG = TPCG_LENGTH-2*TPCG_WHEELTHK-2*TPCR_RDOVTHK               529
C ACTIVE GAS                                                              530
      TPGVLENG = (TOFCLENG-TPCG_MEMBTHK)/2                                530
C ACTIVE GAS                                                              531
      TPGVLENG = (TPCG_LENGTHV-TPCG_MEMBTHK)/2                            531
      TOCSIR = TPCG_SENGASOR                                              532
      TOCSOR = TOCSIR + TPCG_TOCSDR                                       533
      TOKAIR = TOCSOR                                                     533
      TOKAOR = TOKAIR + TPCG_TOKADR                                       534
      TONXIR = TOKAOR                                                     534
      TONXOR = TONXIR + TPCG_TONXDR                                       535
      TOADIR = TONXOR                                                     535
      TOADOR = TOADIR + TPCG_TOADDR                                       536
      TOIGIR = TOADOR                                                     536
      TOIGOR = TOIGIR + TPCG_TOIGDR                                       537
      TOALIR = TOIGOR                                                     537
      TOALOR = TOALIR + TPCG_TOALDR/2.                                    538
      TOHAIR = TOALOR                                                     538
      TOHAOR = TOHAIR + TPCG_TOHADR                                       539
      TOALIR2= TOHAOR                                                     539
      TOALOR2= TOALIR2+ TPCG_TOALDR/2.                                    540
      TOFCIR = TOCSIR                                                     540
      TOFCOR = TOALOR2                                                    541
      TOFSIR = TOCSIR                                                     541
      TOFSOR = TOADOR                                                     542
      TIFCIR = TPCG_RMINIFC                                               543
      TIALIR = TIFCIR                                                     543
      TIALOR = TIALIR + TPCG_TIALDR/2.                                    544
      TIKAIR = TIALOR                                                     544
      TIKAOR = TIKAIR + TPCG_TIKADR                                       545
      TINXIR = TIKAOR                                                     545
      TINXOR = TINXIR + TPCG_TINXDR                                       546
      TIADIR = TINXOR                                                     546
      TIADOR = TIADIR + TPCG_TIADDR                                       547
      TIFCOR = TIADOR + TPCG_TIALDR/2.                                    548
      TPGVIR = TIFCOR                                                     548
C // TPC GAS INNER RADIUS                                                 549
      DEL = TPCG_RMAX-TOFCOR                                              550
      WRITE(LOUT,5010)TPCG_VERSION                                        550
5010  FORMAT(' TPCE: config=',F9.3)                                       552
      WIDTH1IOF(1)=7.661*INCH                                             553
      WIDTH1IOF(2)=22.287*INCH                                            553
      WIDTH2IOF(1)=20.552*INCH                                            554
      WIDTH2IOF(2)=35.422*INCH                                            554
      HEIGHTIOF(1)=24.222*INCH                                            555
      HEIGHTIOF(2)=24.840*INCH                                            555
      DEPTHIOF(1)=1.800*INCH                                              556
      DEPTHIOF(2)=1.800*INCH                                              556
C Geant COMPONENT : C ===> A=12 Z=6 W=5 <===                              557
      CALL AGSRESET                                                       557
      AG_A = 12                                                           557
      AG_Z = 6                                                            557
      AG_W = 5                                                            557
      AG_TITLE = 'C'                                                      557
      AG_EXNAME = 'COMPONENT'                                             557
      AG_PARLIST = 'A_Z_W'                                                557
      CALL AXCOMPONENT                                                    557
C *                                                                       557
C Geant COMPONENT : H ===> A=1 Z=1 W=8 <===                               557
      CALL AGSRESET                                                       558
      AG_A = 1                                                            558
      AG_Z = 1                                                            558
      AG_W = 8                                                            558
      AG_TITLE = 'H'                                                      558
      AG_EXNAME = 'COMPONENT'                                             558
      AG_PARLIST = 'A_Z_W'                                                558
      CALL AXCOMPONENT                                                    558
C *                                                                       558
C Geant COMPONENT : O ===> A=16 Z=8 W=2 <===                              558
      CALL AGSRESET                                                       559
      AG_A = 16                                                           559
      AG_Z = 8                                                            559
      AG_W = 2                                                            559
      AG_TITLE = 'O'                                                      559
      AG_EXNAME = 'COMPONENT'                                             559
      AG_PARLIST = 'A_Z_W'                                                559
      CALL AXCOMPONENT                                                    559
C *                                                                       559
C Geant MIXTURE : NOMEX ===> DENS=0.064 <===                              559
      CALL AGSRESET                                                       560
      AG_DENS = 0.064                                                     560
      AG_TITLE = 'NOMEX'                                                  560
      AG_EXNAME = 'MIXTURE'                                               560
      AG_PARLIST = 'DENS'                                                 560
      CALL AXMIXTURE                                                      560
C *                                                                       560
C Geant COMPONENT : C ===> A=12 Z=6 W=0.625 <===                          560
      CALL AGSRESET                                                       561
      AG_A = 12                                                           561
      AG_Z = 6                                                            561
      AG_W = 0.625                                                        561
      AG_TITLE = 'C'                                                      561
      AG_EXNAME = 'COMPONENT'                                             561
      AG_PARLIST = 'A_Z_W'                                                561
      CALL AXCOMPONENT                                                    561
C *                                                                       561
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4166667E-01 <===                   561
      CALL AGSRESET                                                       562
      AG_A = 1                                                            562
      AG_Z = 1                                                            562
      AG_W = 0.4166667E-01                                                562
      AG_TITLE = 'H'                                                      562
      AG_EXNAME = 'COMPONENT'                                             562
      AG_PARLIST = 'A_Z_W'                                                562
      CALL AXCOMPONENT                                                    562
C *                                                                       562
C Geant COMPONENT : O ===> A=16 Z=8 W=0.3333333 <===                      562
      CALL AGSRESET                                                       563
      AG_A = 16                                                           563
      AG_Z = 8                                                            563
      AG_W = 0.3333333                                                    563
      AG_TITLE = 'O'                                                      563
      AG_EXNAME = 'COMPONENT'                                             563
      AG_PARLIST = 'A_Z_W'                                                563
      CALL AXCOMPONENT                                                    563
C *                                                                       563
C Geant MIXTURE : ADHESIVE ===> DENS=1.2 <===                             563
      CALL AGSRESET                                                       564
      AG_DENS = 1.2                                                       564
      AG_TITLE = 'ADHESIVE'                                               564
      AG_EXNAME = 'MIXTURE'                                               564
      AG_PARLIST = 'DENS'                                                 564
      CALL AXMIXTURE                                                      564
C *                                                                       564
C Geant COMPONENT : AL ===> A=27 Z=13 W=0.0105 <===                       564
      CALL AGSRESET                                                       565
      AG_A = 27                                                           565
      AG_Z = 13                                                           565
      AG_W = 0.0105                                                       565
      AG_TITLE = 'AL'                                                     565
      AG_EXNAME = 'COMPONENT'                                             565
      AG_PARLIST = 'A_Z_W'                                                565
      CALL AXCOMPONENT                                                    565
C *                                                                       565
C Geant COMPONENT : N ===> A=14 Z=7 W=0.7395 <===                         565
      CALL AGSRESET                                                       566
      AG_A = 14                                                           566
      AG_Z = 7                                                            566
      AG_W = 0.7395                                                       566
      AG_TITLE = 'N'                                                      566
      AG_EXNAME = 'COMPONENT'                                             566
      AG_PARLIST = 'A_Z_W'                                                566
      CALL AXCOMPONENT                                                    566
C *                                                                       566
C Geant COMPONENT : ADHESIVE ===> A=9 Z=4.5 W=0.2500 <===                 566
      CALL AGSRESET                                                       567
      AG_A = 9                                                            567
      AG_Z = 4.5                                                          567
      AG_W = 0.2500                                                       567
      AG_TITLE = 'ADHESIVE'                                               567
      AG_EXNAME = 'COMPONENT'                                             567
      AG_PARLIST = 'A_Z_W'                                                567
      CALL AXCOMPONENT                                                    567
C *                                                                       567
C Geant MIXTURE : AL_HONEYCOMB ===> DENS=0.282 <===                       567
      CALL AGSRESET                                                       568
      AG_DENS = 0.282                                                     568
      AG_TITLE = 'AL_HONEYCOMB'                                           568
      AG_EXNAME = 'MIXTURE'                                               568
      AG_PARLIST = 'DENS'                                                 568
      CALL AXMIXTURE                                                      568
C *                                                                       568
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             568
      CALL AGSRESET                                                       569
      AG_A = 28.08                                                        569
      AG_Z = 14                                                           569
      AG_W = 0.6*1*28./60.                                                569
      AG_TITLE = 'SI'                                                     569
      AG_EXNAME = 'COMPONENT'                                             569
      AG_PARLIST = 'A_Z_W'                                                569
      CALL AXCOMPONENT                                                    569
C *                                                                       569
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  569
C =                                                                       569
      CALL AGSRESET                                                       570
      AG_A = 16                                                           570
      AG_Z = 8                                                            570
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               570
      AG_TITLE = 'O'                                                      570
      AG_EXNAME = 'COMPONENT'                                             570
      AG_PARLIST = 'A_Z_W'                                                570
      CALL AXCOMPONENT                                                    570
C *                                                                       570
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 570
      CALL AGSRESET                                                       571
      AG_A = 12                                                           571
      AG_Z = 6                                                            571
      AG_W = 0.4*8*12./174.                                               571
      AG_TITLE = 'C'                                                      571
      AG_EXNAME = 'COMPONENT'                                             571
      AG_PARLIST = 'A_Z_W'                                                571
      CALL AXCOMPONENT                                                    571
C *                                                                       571
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  571
      CALL AGSRESET                                                       572
      AG_A = 1                                                            572
      AG_Z = 1                                                            572
      AG_W = 0.4*14*1./174.                                               572
      AG_TITLE = 'H'                                                      572
      AG_EXNAME = 'COMPONENT'                                             572
      AG_PARLIST = 'A_Z_W'                                                572
      CALL AXCOMPONENT                                                    572
C *                                                                       572
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  572
      CALL AGSRESET                                                       573
      AG_DENS = 1.7                                                       573
      AG_TITLE = 'G10'                                                    573
      AG_EXNAME = 'MIXTURE'                                               573
      AG_PARLIST = 'DENS'                                                 573
      CALL AXMIXTURE                                                      573
C *                                                                       573
C Geant COMPONENT : H ===> A=1 Z=1 W=1.06546299984001044E-02 <===         573
      CALL AGSRESET                                                       574
      AG_A = 1                                                            574
      AG_Z = 1                                                            574
      AG_W = 1.06546299984001044E-02                                      574
      AG_TITLE = 'H'                                                      574
      AG_EXNAME = 'COMPONENT'                                             574
      AG_PARLIST = 'A_Z_W'                                                574
      CALL AXCOMPONENT                                                    574
C *                                                                       574
C Geant COMPONENT : O ===> A=16 Z=8 W=8.52370399872008355E-02 <===        574
      CALL AGSRESET                                                       575
      AG_A = 16                                                           575
      AG_Z = 8                                                            575
      AG_W = 8.52370399872008355E-02                                      575
      AG_TITLE = 'O'                                                      575
      AG_EXNAME = 'COMPONENT'                                             575
      AG_PARLIST = 'A_Z_W'                                                575
      CALL AXCOMPONENT                                                    575
C *                                                                       575
C Geant COMPONENT : AL ===> A=27 Z=14 W=9.04108330014399053E-01 <===      575
      CALL AGSRESET                                                       576
      AG_A = 27                                                           576
      AG_Z = 14                                                           576
      AG_W = 9.04108330014399053E-01                                      576
      AG_TITLE = 'AL'                                                     576
      AG_EXNAME = 'COMPONENT'                                             576
      AG_PARLIST = 'A_Z_W'                                                576
      CALL AXCOMPONENT                                                    576
C *                                                                       576
C Geant MIXTURE : WATER_PIPE ===> DENS=2.32155 <===                       576
      CALL AGSRESET                                                       577
      AG_DENS = 2.32155                                                   577
      AG_TITLE = 'WATER_PIPE'                                             577
      AG_EXNAME = 'MIXTURE'                                               577
      AG_PARLIST = 'DENS'                                                 577
      CALL AXMIXTURE                                                      577
C *                                                                       577
C Geant COMPONENT : CU ===> A=63.54 Z=29 W=0.586 <===                     577
      CALL AGSRESET                                                       578
      AG_A = 63.54                                                        578
      AG_Z = 29                                                           578
      AG_W = 0.586                                                        578
      AG_TITLE = 'CU'                                                     578
      AG_EXNAME = 'COMPONENT'                                             578
      AG_PARLIST = 'A_Z_W'                                                578
      CALL AXCOMPONENT                                                    578
C *                                                                       578
C Geant COMPONENT : C ===> A=12.01 Z=6 W=0.259 <===                       578
      CALL AGSRESET                                                       579
      AG_A = 12.01                                                        579
      AG_Z = 6                                                            579
      AG_W = 0.259                                                        579
      AG_TITLE = 'C'                                                      579
      AG_EXNAME = 'COMPONENT'                                             579
      AG_PARLIST = 'A_Z_W'                                                579
      CALL AXCOMPONENT                                                    579
C *                                                                       579
C Geant COMPONENT : O ===> A=15.999 Z=8 W=0.138 <===                      579
      CALL AGSRESET                                                       580
      AG_A = 15.999                                                       580
      AG_Z = 8                                                            580
      AG_W = 0.138                                                        580
      AG_TITLE = 'O'                                                      580
      AG_EXNAME = 'COMPONENT'                                             580
      AG_PARLIST = 'A_Z_W'                                                580
      CALL AXCOMPONENT                                                    580
C *                                                                       580
C Geant COMPONENT : H ===> A=1.00794 Z=1 W=0.017 <===                     580
      CALL AGSRESET                                                       581
      AG_A = 1.00794                                                      581
      AG_Z = 1                                                            581
      AG_W = 0.017                                                        581
      AG_TITLE = 'H'                                                      581
      AG_EXNAME = 'COMPONENT'                                             581
      AG_PARLIST = 'A_Z_W'                                                581
      CALL AXCOMPONENT                                                    581
C *                                                                       581
C Geant MIXTURE : CABLES ===> DENS=2.68 <===                              581
      CALL AGSRESET                                                       582
      AG_DENS = 2.68                                                      582
      AG_TITLE = 'CABLES'                                                 582
      AG_EXNAME = 'MIXTURE'                                               582
      AG_PARLIST = 'DENS'                                                 582
      CALL AXMIXTURE                                                      582
C *                                                                       582
C Geant COMPONENT : AR ===> A=40 Z=18 W=9 <===                            582
      CALL AGSRESET                                                       583
      AG_A = 40                                                           583
      AG_Z = 18                                                           583
      AG_W = 9                                                            583
      AG_TITLE = 'AR'                                                     583
      AG_EXNAME = 'COMPONENT'                                             583
      AG_PARLIST = 'A_Z_W'                                                583
      CALL AXCOMPONENT                                                    583
C *                                                                       583
C Geant COMPONENT : C ===> A=12 Z=6 W=1 <===                              583
      CALL AGSRESET                                                       584
      AG_A = 12                                                           584
      AG_Z = 6                                                            584
      AG_W = 1                                                            584
      AG_TITLE = 'C'                                                      584
      AG_EXNAME = 'COMPONENT'                                             584
      AG_PARLIST = 'A_Z_W'                                                584
      CALL AXCOMPONENT                                                    584
C *                                                                       584
C Geant COMPONENT : H ===> A=1 Z=1 W=4 <===                               584
      CALL AGSRESET                                                       585
      AG_A = 1                                                            585
      AG_Z = 1                                                            585
      AG_W = 4                                                            585
      AG_TITLE = 'H'                                                      585
      AG_EXNAME = 'COMPONENT'                                             585
      AG_PARLIST = 'A_Z_W'                                                585
      CALL AXCOMPONENT                                                    585
C *                                                                       585
C Geant MIXTURE : P10 ===> DENS=0.154053E-02 <===                         585
      CALL AGSRESET                                                       586
      AG_DENS = 0.154053E-02                                              586
      AG_TITLE = 'P10'                                                    586
      AG_EXNAME = 'MIXTURE'                                               586
      AG_PARLIST = 'DENS'                                                 586
      CALL AXMIXTURE                                                      586
C *                                                                       586
      CALL AGSBEGM(MODULE,IPRIN)                                          586
C Check IPRIN<0                                                           587
      IF (IPRIN.LT.0) RETURN                                              587
C Create TPCE :                                                           587
C Call To Local Procedure TPCE                                            587
      ASSIGN 5030 TO BLOCK_TPCE                                           587
      GO TO  5020                                                         587
5030  CONTINUE                                                            587
C *                                                                       587
C Geant POSITION : TPCE ===> MOTHER='CAVE' <===                           588
      CALL AGSRESET                                                       588
      AG_MOTHER = 'CAVE'                                                  588
      AG_TITLE = 'TPCE'                                                   588
      AG_EXNAME = 'POSITION'                                              588
      AG_PARLIST = 'MOTHER'                                               588
      CALL AXPOSITION                                                     588
C *                                                                       588
C ----------------------------------------------------------------------  588
C ------------ TPCE                                                       588
      CALL AGSENDM                                                        590
      RETURN                                                              590
C Begin Local Procedure TPCE                                              590
C TPCE IS THE TPC ENVELOPE                                                590
5020  CONTINUE                                                            590
         AG_TITLE='TPCE'                                                  590
         AG_MODULE=MODULE                                                 590
         AG_MARK='V00'                                                    590
         CALL AGSPUSH                                                     590
         AG_MODULE=MODULE                                                 590
C    *                                                                    590
C    Geant MATERIAL : AIR ===> inherited arguments used <===              591
         CALL AGSRESET                                                    591
         AG_TITLE = 'AIR'                                                 591
         AG_EXNAME = 'MATERIAL'                                           591
         AG_PARLIST = 'NONE'                                              591
         CALL AXMATERIAL                                                  591
C    *                                                                    591
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           591
         CALL AGSRESET                                                    592
         AG_TITLE = 'STANDARD'                                            592
         AG_EXNAME = 'MEDIUM'                                             592
         AG_PARLIST = 'NONE'                                              592
         CALL AXMEDIUM                                                    592
C    *                                                                    592
C    Geant ATTRIBUTE : TPCE ===> SEEN=0 COLO=KRED <===                    592
         CALL AGSRESET                                                    593
         AG_SEEN = 0                                                      593
         AG_COLO = KRED                                                   593
         AG_TITLE = 'TPCE'                                                593
         AG_EXNAME = 'ATTRIBUTE'                                          593
         AG_PARLIST = 'SEEN_COLO'                                         593
         CALL AXATTRIBUTE                                                 593
C    *                                                                    593
C    Geant SHAPE : TUBE ===> RMIN=TPCG_RMIN RMAX=TPCG_RMAX, DZ=TPCG_DZEN  593
C    VELOP <===                                                           593
         CALL AGSRESET                                                    595
         AG_RMIN = TPCG_RMIN                                              595
         AG_RMAX = TPCG_RMAX                                              595
         AG_DZ = TPCG_DZENVELOP                                           595
         AG_TITLE = 'TUBE'                                                595
         AG_EXNAME = 'SHAPE'                                              595
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      595
         CALL AXSHAPE                                                     595
C    *                                                                    595
C    Check IPRIN>=1                                                       595
         IF (IPRIN.GE.1) WRITE(LOUT,5040)AG_IVOLUME,AG_VOLUME             595
5040     FORMAT('Creating volume ',I4,2X,A4)                              597
         TPGVZ =(TPCG_MEMBTHK + TPGVLENG)/2                               598
         ZWHEEL1 = TPCG_LENGTHW/2 - TPCG_WHEELTHK                         598
         ZTOFCEND = TPCG_LENGTHW/2 - TPCG_WHEELTHK1                       599
         ZTIFCEND = TPCG_LENGTHW/2 + 0.97                                 600
         ZTIFCFLANGEBEGIN = ZTIFCEND - 1*INCH                             601
C    Create TPGV :                                                        603
C    Call To Local Procedure TPGV                                         603
         ASSIGN 5060 TO BLOCK_TPGV                                        603
         GO TO  5050                                                      603
5060     CONTINUE                                                         603
C    *                                                                    603
C    Geant POSITION : TPGV ===> Z=+TPGVZ THETAX=90 PHIX=75 THETAY=90 PHI  605
C    Y=-15 THETAZ=0 PHIZ=0 KONLY='MANY' <===                              605
         CALL AGSRESET                                                    605
         AG_Z = +TPGVZ                                                    605
         AG_THETAX = 90                                                   605
         AG_PHIX = 75                                                     605
         AG_THETAY = 90                                                   605
         AG_PHIY = -15                                                    605
         AG_THETAZ = 0                                                    605
         AG_PHIZ = 0                                                      605
         AG_KONLY = 'MANY'                                                605
         AG_TITLE = 'TPGV'                                                605
         AG_EXNAME = 'POSITION'                                           605
         AG_PARLIST = 'Z_THETAX_PHIX_THETAY_PHIY_THETAZ_PHIZ_KONLY'       605
         CALL AXPOSITION                                                  605
C    *                                                                    605
C    Geant POSITION : TPGV ===> Z=-TPGVZ THETAX=90 PHIX=105 THETAY=90 PH  605
C    IY=195 THETAZ=180 PHIZ=0 KONLY='MANY' <===                           605
         CALL AGSRESET                                                    607
         AG_Z = -TPGVZ                                                    607
         AG_THETAX = 90                                                   607
         AG_PHIX = 105                                                    607
         AG_THETAY = 90                                                   607
         AG_PHIY = 195                                                    607
         AG_THETAZ = 180                                                  607
         AG_PHIZ = 0                                                      607
         AG_KONLY = 'MANY'                                                607
         AG_TITLE = 'TPGV'                                                607
         AG_EXNAME = 'POSITION'                                           607
         AG_PARLIST = 'Z_THETAX_PHIX_THETAY_PHIY_THETAZ_PHIZ_KONLY'       607
         CALL AXPOSITION                                                  607
C    *                                                                    607
C    Create TPCM :                                                        608
C    Call To Local Procedure TPCM                                         608
         ASSIGN 5080 TO BLOCK_TPCM                                        608
         GO TO  5070                                                      608
5080     CONTINUE                                                         608
C    *                                                                    608
C    Geant POSITION : TPCM ===> inherited arguments used <===             609
         CALL AGSRESET                                                    609
         AG_TITLE = 'TPCM'                                                609
         AG_EXNAME = 'POSITION'                                           609
         AG_PARLIST = 'NONE'                                              609
         CALL AXPOSITION                                                  609
C    *                                                                    609
C    Create TIFC :                                                        610
C    Call To Local Procedure TIFC                                         610
         ASSIGN 5100 TO BLOCK_TIFC                                        610
         GO TO  5090                                                      610
5100     CONTINUE                                                         610
C    *                                                                    610
C    Geant POSITION : TIFC ===> inherited arguments used <===             611
         CALL AGSRESET                                                    611
         AG_TITLE = 'TIFC'                                                611
         AG_EXNAME = 'POSITION'                                           611
         AG_PARLIST = 'NONE'                                              611
         CALL AXPOSITION                                                  611
C    *                                                                    611
C    Create TOFC :                                                        612
C    Call To Local Procedure TOFC                                         612
         ASSIGN 5120 TO BLOCK_TOFC                                        612
         GO TO  5110                                                      612
5120     CONTINUE                                                         612
C    *                                                                    612
C    Geant POSITION : TOFC ===> inherited arguments used <===             613
         CALL AGSRESET                                                    613
         AG_TITLE = 'TOFC'                                                613
         AG_EXNAME = 'POSITION'                                           613
         AG_PARLIST = 'NONE'                                              613
         CALL AXPOSITION                                                  613
C    *                                                                    613
C    *                                                                    613
         ISECANG=0                                                        614
            GOTO 5133                                                     614
5131        ISECANG=ISECANG+(30)                                          614
5133        IF((30)*(ISECANG-(360-30)).GT.0) GO TO 5132                   614
C       Create TSWH :                                                     615
C       Call To Local Procedure TSWH                                      615
            ASSIGN 5150 TO BLOCK_TSWH                                     615
            GO TO  5140                                                   615
5150        CONTINUE                                                      615
C       *                                                                 615
C       Geant POSITION : TSWH ===> ALPHAZ=ISECANG KONLY='MANY' <===       616
            CALL AGSRESET                                                 616
            AG_ALPHAN=AG_ALPHAN+1                                         616
            AG_ALPHAK(AG_ALPHAN)=3                                        616
            AG_ALPHAL(AG_ALPHAN)=ISECANG                                  616
            AG_KONLY = 'MANY'                                             616
            AG_TITLE = 'TSWH'                                             616
            AG_EXNAME = 'POSITION'                                        616
            AG_PARLIST = 'ALPHAZ_KONLY'                                   616
            CALL AXPOSITION                                               616
C       *                                                                 616
C       Create TSWH :                                                     617
C       Call To Local Procedure TSWH                                      617
            ASSIGN 5160 TO BLOCK_TSWH                                     617
            GO TO  5140                                                   617
5160        CONTINUE                                                      617
C       *                                                                 617
C       Geant POSITION : TSWH ===> ORT=XY-Z ALPHAZ=ISECANG KONLY='MANY'   618
C       <===                                                              618
            CALL AGSRESET                                                 618
            AG_ALPHAN=AG_ALPHAN+1                                         618
            AG_ALPHAK(AG_ALPHAN)=IGOR('XY-Z')                             618
            AG_ALPHAN=AG_ALPHAN+1                                         618
            AG_ALPHAK(AG_ALPHAN)=3                                        618
            AG_ALPHAL(AG_ALPHAN)=ISECANG                                  618
            AG_KONLY = 'MANY'                                             618
            AG_TITLE = 'TSWH'                                             618
            AG_EXNAME = 'POSITION'                                        618
            AG_PARLIST = 'ORT_ALPHAZ_KONLY'                               618
            CALL AXPOSITION                                               618
C       *                                                                 618
         GO TO 5131                                                       619
5132     CONTINUE                                                         619
5021  CALL AGSPOPB                                                        620
      GO TO BLOCK_TPCE, (5020 ,5030)                                      620
C End of Local Procedure                                                  620
C ----------------------------------------------------------------------  621
C ------------ TPCM                                                       621
C Begin Local Procedure TPCM                                              622
C TPCM IS THE CENTRAL MEMBRANE PLACED IN TPC                              622
5070  CONTINUE                                                            622
         AG_TITLE='TPCM'                                                  622
         AG_MODULE=MODULE                                                 622
         AG_MARK='V00'                                                    622
         CALL AGSPUSH                                                     622
         AG_MODULE=MODULE                                                 622
C    *                                                                    622
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            623
         CALL AGSRESET                                                    623
         AG_TITLE = 'MYLAR'                                               623
         AG_EXNAME = 'MATERIAL'                                           623
         AG_PARLIST = 'NONE'                                              623
         CALL AXMATERIAL                                                  623
C    *                                                                    623
C    Geant ATTRIBUTE : TPCM ===> SEEN=1 COLO=KBLUE <===                   623
         CALL AGSRESET                                                    624
         AG_SEEN = 1                                                      624
         AG_COLO = KBLUE                                                  624
         AG_TITLE = 'TPCM'                                                624
         AG_EXNAME = 'ATTRIBUTE'                                          624
         AG_PARLIST = 'SEEN_COLO'                                         624
         CALL AXATTRIBUTE                                                 624
C    *                                                                    624
C    Geant SHAPE : TUBE ===> RMIN=TPGVIR RMAX=TPCG_SENGASOR, DZ=TPCG_MEM  624
C    BTHK/2 <===                                                          624
         CALL AGSRESET                                                    626
         AG_RMIN = TPGVIR                                                 626
         AG_RMAX = TPCG_SENGASOR                                          626
         AG_DZ = TPCG_MEMBTHK/2                                           626
         AG_TITLE = 'TUBE'                                                626
         AG_EXNAME = 'SHAPE'                                              626
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      626
         CALL AXSHAPE                                                     626
C    *                                                                    626
5071  CALL AGSPOPB                                                        627
      GO TO BLOCK_TPCM, (5070 ,5080)                                      627
C End of Local Procedure                                                  627
C ----------------------------------------------------------------------  628
C ------------ TIFC                                                       628
C Begin Local Procedure TIFC                                              629
C TIFC DEFINES THE INNER FIELD CAGE PLACED IN TPC                         629
5090  CONTINUE                                                            629
         AG_TITLE='TIFC'                                                  629
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
C    Geant ATTRIBUTE : TIFC ===> SEEN=1 COLO=KBLUE <===                   630
         CALL AGSRESET                                                    631
         AG_SEEN = 1                                                      631
         AG_COLO = KBLUE                                                  631
         AG_TITLE = 'TIFC'                                                631
         AG_EXNAME = 'ATTRIBUTE'                                          631
         AG_PARLIST = 'SEEN_COLO'                                         631
         CALL AXATTRIBUTE                                                 631
C    *                                                                    631
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=6 ZI={-ZTIFCEND, -ZTIFCF  631
C    LANGEBEGIN,-ZTIFCFLANGEBEGIN,ZTIFCFLANGEBEGIN, ZTIFCFLANGEBEGIN,ZTI  631
C    FCEND} RMN={TIFCIR, TIFCIR,TIFCIR,TIFCIR,TIFCIR,TIFCIR} RMX={TPCG_T  631
C    IFCRF, TPCG_TIFCRF,TIFCOR,TIFCOR,TPCG_TIFCRF, TPCG_TIFCRF} <===      631
         CALL AGSRESET                                                    637
         AG_PHI1 = 0                                                      637
         AG_DPHI = 360                                                    637
         AG_NZ = 6                                                        637
         AG_ZI(1)=-ZTIFCEND                                               637
         AG_ZI(2)=-ZTIFCFLANGEBEGIN                                       637
         AG_ZI(3)=-ZTIFCFLANGEBEGIN                                       637
         AG_ZI(4)=ZTIFCFLANGEBEGIN                                        637
         AG_ZI(5)=ZTIFCFLANGEBEGIN                                        637
         AG_ZI(6)=ZTIFCEND                                                637
         AG_RMN(1)=TIFCIR                                                 637
         AG_RMN(2)=TIFCIR                                                 637
         AG_RMN(3)=TIFCIR                                                 637
         AG_RMN(4)=TIFCIR                                                 637
         AG_RMN(5)=TIFCIR                                                 637
         AG_RMN(6)=TIFCIR                                                 637
         AG_RMX(1)=TPCG_TIFCRF                                            637
         AG_RMX(2)=TPCG_TIFCRF                                            637
         AG_RMX(3)=TIFCOR                                                 637
         AG_RMX(4)=TIFCOR                                                 637
         AG_RMX(5)=TPCG_TIFCRF                                            637
         AG_RMX(6)=TPCG_TIFCRF                                            637
         AG_TITLE = 'PCON'                                                637
         AG_EXNAME = 'SHAPE'                                              637
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           637
         CALL AXSHAPE                                                     637
C    *                                                                    637
C    Create TIKA :                                                        638
C    Call To Local Procedure TIKA                                         638
         ASSIGN 5180 TO BLOCK_TIKA                                        638
         GO TO  5170                                                      638
5180     CONTINUE                                                         638
C    *                                                                    638
C    Geant POSITION : TIKA ===> inherited arguments used <===             639
         CALL AGSRESET                                                    639
         AG_TITLE = 'TIKA'                                                639
         AG_EXNAME = 'POSITION'                                           639
         AG_PARLIST = 'NONE'                                              639
         CALL AXPOSITION                                                  639
C    *                                                                    639
C    Create TINX :                                                        640
C    Call To Local Procedure TINX                                         640
         ASSIGN 5200 TO BLOCK_TINX                                        640
         GO TO  5190                                                      640
5200     CONTINUE                                                         640
C    *                                                                    640
C    Geant POSITION : TINX ===> inherited arguments used <===             641
         CALL AGSRESET                                                    641
         AG_TITLE = 'TINX'                                                641
         AG_EXNAME = 'POSITION'                                           641
         AG_PARLIST = 'NONE'                                              641
         CALL AXPOSITION                                                  641
C    *                                                                    641
C    Create TIAD :                                                        642
C    Call To Local Procedure TIAD                                         642
         ASSIGN 5220 TO BLOCK_TIAD                                        642
         GO TO  5210                                                      642
5220     CONTINUE                                                         642
C    *                                                                    642
C    Geant POSITION : TIAD ===> inherited arguments used <===             643
         CALL AGSRESET                                                    643
         AG_TITLE = 'TIAD'                                                643
         AG_EXNAME = 'POSITION'                                           643
         AG_PARLIST = 'NONE'                                              643
         CALL AXPOSITION                                                  643
C    *                                                                    643
5091  CALL AGSPOPB                                                        644
      GO TO BLOCK_TIFC, (5090 ,5100)                                      644
C End of Local Procedure                                                  644
C ----------------------------------------------------------------------  645
C ------------ TIKA                                                       645
C Begin Local Procedure TIKA                                              646
C TIKA IS THE KAPTON FILM OF THE INNER FIELD CAGE                         646
5170  CONTINUE                                                            646
         AG_TITLE='TIKA'                                                  646
         AG_MODULE=MODULE                                                 646
         AG_MARK='V00'                                                    646
         CALL AGSPUSH                                                     646
         AG_MODULE=MODULE                                                 646
C    *                                                                    646
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            647
         CALL AGSRESET                                                    647
         AG_TITLE = 'MYLAR'                                               647
         AG_EXNAME = 'MATERIAL'                                           647
         AG_PARLIST = 'NONE'                                              647
         CALL AXMATERIAL                                                  647
C    *                                                                    647
C    Geant ATTRIBUTE : TIKA ===> SEEN=1 COLO=KVIOLET <===                 647
         CALL AGSRESET                                                    648
         AG_SEEN = 1                                                      648
         AG_COLO = KVIOLET                                                648
         AG_TITLE = 'TIKA'                                                648
         AG_EXNAME = 'ATTRIBUTE'                                          648
         AG_PARLIST = 'SEEN_COLO'                                         648
         CALL AXATTRIBUTE                                                 648
C    *                                                                    648
C    Geant SHAPE : TUBE ===> RMIN=TIKAIR RMAX=TIKAOR DZ=TPCG_DZYF1 <===   648
         CALL AGSRESET                                                    649
         AG_RMIN = TIKAIR                                                 649
         AG_RMAX = TIKAOR                                                 649
         AG_DZ = TPCG_DZYF1                                               649
         AG_TITLE = 'TUBE'                                                649
         AG_EXNAME = 'SHAPE'                                              649
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      649
         CALL AXSHAPE                                                     649
C    *                                                                    649
5171  CALL AGSPOPB                                                        650
      GO TO BLOCK_TIKA, (5170 ,5180)                                      650
C End of Local Procedure                                                  650
C ----------------------------------------------------------------------  651
C ------------ TINX                                                       651
C Begin Local Procedure TINX                                              652
C TINX IS THE INNER NOMEX STRUCTUREURE                                    652
5190  CONTINUE                                                            652
         AG_TITLE='TINX'                                                  652
         AG_MODULE=MODULE                                                 652
         AG_MARK='V00'                                                    652
         CALL AGSPUSH                                                     652
         AG_MODULE=MODULE                                                 652
C    *                                                                    652
C    Geant MATERIAL : NOMEX ===> inherited arguments used <===            653
         CALL AGSRESET                                                    653
         AG_TITLE = 'NOMEX'                                               653
         AG_EXNAME = 'MATERIAL'                                           653
         AG_PARLIST = 'NONE'                                              653
         CALL AXMATERIAL                                                  653
C    *                                                                    653
C    Geant ATTRIBUTE : TINX ===> SEEN=1 COLO=KYELLOW <===                 653
         CALL AGSRESET                                                    654
         AG_SEEN = 1                                                      654
         AG_COLO = KYELLOW                                                654
         AG_TITLE = 'TINX'                                                654
         AG_EXNAME = 'ATTRIBUTE'                                          654
         AG_PARLIST = 'SEEN_COLO'                                         654
         CALL AXATTRIBUTE                                                 654
C    *                                                                    654
C    Geant SHAPE : TUBE ===> RMIN=TINXIR RMAX=TINXOR DZ=TPCG_DZYF1 <===   654
         CALL AGSRESET                                                    655
         AG_RMIN = TINXIR                                                 655
         AG_RMAX = TINXOR                                                 655
         AG_DZ = TPCG_DZYF1                                               655
         AG_TITLE = 'TUBE'                                                655
         AG_EXNAME = 'SHAPE'                                              655
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      655
         CALL AXSHAPE                                                     655
C    *                                                                    655
5191  CALL AGSPOPB                                                        656
      GO TO BLOCK_TINX, (5190 ,5200)                                      656
C End of Local Procedure                                                  656
C ----------------------------------------------------------------------  657
C ------------ TIAD                                                       657
C Begin Local Procedure TIAD                                              658
C TIAD THE INNER ADHESIVE STRUCTUREURE                                    658
5210  CONTINUE                                                            658
         AG_TITLE='TIAD'                                                  658
         AG_MODULE=MODULE                                                 658
         AG_MARK='V00'                                                    658
         CALL AGSPUSH                                                     658
         AG_MODULE=MODULE                                                 658
C    *                                                                    658
C    Geant MATERIAL : ADHESIVE ===> inherited arguments used <===         659
         CALL AGSRESET                                                    659
         AG_TITLE = 'ADHESIVE'                                            659
         AG_EXNAME = 'MATERIAL'                                           659
         AG_PARLIST = 'NONE'                                              659
         CALL AXMATERIAL                                                  659
C    *                                                                    659
C    Geant ATTRIBUTE : TIKA ===> SEEN=1 COLO=KVIOLET <===                 659
         CALL AGSRESET                                                    660
         AG_SEEN = 1                                                      660
         AG_COLO = KVIOLET                                                660
         AG_TITLE = 'TIKA'                                                660
         AG_EXNAME = 'ATTRIBUTE'                                          660
         AG_PARLIST = 'SEEN_COLO'                                         660
         CALL AXATTRIBUTE                                                 660
C    *                                                                    660
C    Geant SHAPE : TUBE ===> RMIN=TIADIR RMAX=TIADOR DZ=TPCG_DZYF1 <===   660
         CALL AGSRESET                                                    661
         AG_RMIN = TIADIR                                                 661
         AG_RMAX = TIADOR                                                 661
         AG_DZ = TPCG_DZYF1                                               661
         AG_TITLE = 'TUBE'                                                661
         AG_EXNAME = 'SHAPE'                                              661
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      661
         CALL AXSHAPE                                                     661
C    *                                                                    661
5211  CALL AGSPOPB                                                        662
      GO TO BLOCK_TIAD, (5210 ,5220)                                      662
C End of Local Procedure                                                  662
C ----------------------------------------------------------------------  663
C ------------ TOFC                                                       663
C Begin Local Procedure TOFC                                              664
C TOFC DEFINES OUTER FIELD CAGE - FILL IT WITH INSULATING GAS ALREADY     664
5110  CONTINUE                                                            664
         AG_TITLE='TOFC'                                                  664
         AG_MODULE=MODULE                                                 664
         AG_MARK='V00'                                                    664
         CALL AGSPUSH                                                     664
         AG_MODULE=MODULE                                                 664
C    *                                                                    664
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        665
         CALL AGSRESET                                                    665
         AG_TITLE = 'ALUMINIUM'                                           665
         AG_EXNAME = 'MATERIAL'                                           665
         AG_PARLIST = 'NONE'                                              665
         CALL AXMATERIAL                                                  665
C    *                                                                    665
C    Geant ATTRIBUTE : TOFC ===> SEEN=1 COLO=KGREEN <===                  665
         CALL AGSRESET                                                    666
         AG_SEEN = 1                                                      666
         AG_COLO = KGREEN                                                 666
         AG_TITLE = 'TOFC'                                                666
         AG_EXNAME = 'ATTRIBUTE'                                          666
         AG_PARLIST = 'SEEN_COLO'                                         666
         CALL AXATTRIBUTE                                                 666
C    *                                                                    666
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=6 ZI={-ZTOFCEND, -ZWHEEL  666
C    1,-ZWHEEL1,ZWHEEL1,ZWHEEL1,ZTOFCEND}, RMN={TPCG_WHEELOR1,TPCG_WHEEL  666
C    OR1,TOFCIR, TOFCIR,TPCG_WHEELOR1,TPCG_WHEELOR1} RMX={TPCG_WHEELOR,   666
C    TPCG_WHEELOR,TOFCOR,TOFCOR,TPCG_WHEELOR, TPCG_WHEELOR} <===          666
         CALL AGSRESET                                                    672
         AG_PHI1 = 0                                                      672
         AG_DPHI = 360                                                    672
         AG_NZ = 6                                                        672
         AG_ZI(1)=-ZTOFCEND                                               672
         AG_ZI(2)=-ZWHEEL1                                                672
         AG_ZI(3)=-ZWHEEL1                                                672
         AG_ZI(4)=ZWHEEL1                                                 672
         AG_ZI(5)=ZWHEEL1                                                 672
         AG_ZI(6)=ZTOFCEND                                                672
         AG_RMN(1)=TPCG_WHEELOR1                                          672
         AG_RMN(2)=TPCG_WHEELOR1                                          672
         AG_RMN(3)=TOFCIR                                                 672
         AG_RMN(4)=TOFCIR                                                 672
         AG_RMN(5)=TPCG_WHEELOR1                                          672
         AG_RMN(6)=TPCG_WHEELOR1                                          672
         AG_RMX(1)=TPCG_WHEELOR                                           672
         AG_RMX(2)=TPCG_WHEELOR                                           672
         AG_RMX(3)=TOFCOR                                                 672
         AG_RMX(4)=TOFCOR                                                 672
         AG_RMX(5)=TPCG_WHEELOR                                           672
         AG_RMX(6)=TPCG_WHEELOR                                           672
         AG_TITLE = 'PCON'                                                672
         AG_EXNAME = 'SHAPE'                                              672
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           672
         CALL AXSHAPE                                                     672
C    *                                                                    672
C    Create TOAD :                                                        673
C    Call To Local Procedure TOAD                                         673
         ASSIGN 5240 TO BLOCK_TOAD                                        673
         GO TO  5230                                                      673
5240     CONTINUE                                                         673
C    *                                                                    673
C    Geant POSITION : TOAD ===> inherited arguments used <===             674
         CALL AGSRESET                                                    674
         AG_TITLE = 'TOAD'                                                674
         AG_EXNAME = 'POSITION'                                           674
         AG_PARLIST = 'NONE'                                              674
         CALL AXPOSITION                                                  674
C    *                                                                    674
C    Create TOFS :                                                        675
C    Call To Local Procedure TOFS                                         675
         ASSIGN 5260 TO BLOCK_TOFS                                        675
         GO TO  5250                                                      675
5260     CONTINUE                                                         675
C    *                                                                    675
C    Geant POSITION : TOFS ===> inherited arguments used <===             676
         CALL AGSRESET                                                    676
         AG_TITLE = 'TOFS'                                                676
         AG_EXNAME = 'POSITION'                                           676
         AG_PARLIST = 'NONE'                                              676
         CALL AXPOSITION                                                  676
C    *                                                                    676
C    Create TOKA :                                                        677
C    Call To Local Procedure TOKA                                         677
         ASSIGN 5280 TO BLOCK_TOKA                                        677
         GO TO  5270                                                      677
5280     CONTINUE                                                         677
C    *                                                                    677
C    Geant POSITION : TOKA ===> inherited arguments used <===             678
         CALL AGSRESET                                                    678
         AG_TITLE = 'TOKA'                                                678
         AG_EXNAME = 'POSITION'                                           678
         AG_PARLIST = 'NONE'                                              678
         CALL AXPOSITION                                                  678
C    *                                                                    678
C    Create TONX :                                                        679
C    Call To Local Procedure TONX                                         679
         ASSIGN 5300 TO BLOCK_TONX                                        679
         GO TO  5290                                                      679
5300     CONTINUE                                                         679
C    *                                                                    679
C    Geant POSITION : TONX ===> inherited arguments used <===             680
         CALL AGSRESET                                                    680
         AG_TITLE = 'TONX'                                                680
         AG_EXNAME = 'POSITION'                                           680
         AG_PARLIST = 'NONE'                                              680
         CALL AXPOSITION                                                  680
C    *                                                                    680
C    Create TOIG :                                                        681
C    Call To Local Procedure TOIG                                         681
         ASSIGN 5320 TO BLOCK_TOIG                                        681
         GO TO  5310                                                      681
5320     CONTINUE                                                         681
C    *                                                                    681
C    Geant POSITION : TOIG ===> inherited arguments used <===             682
         CALL AGSRESET                                                    682
         AG_TITLE = 'TOIG'                                                682
         AG_EXNAME = 'POSITION'                                           682
         AG_PARLIST = 'NONE'                                              682
         CALL AXPOSITION                                                  682
C    *                                                                    682
C    Create TOHA :                                                        683
C    Call To Local Procedure TOHA                                         683
         ASSIGN 5340 TO BLOCK_TOHA                                        683
         GO TO  5330                                                      683
5340     CONTINUE                                                         683
C    *                                                                    683
C    Geant POSITION : TOHA ===> inherited arguments used <===             684
         CALL AGSRESET                                                    684
         AG_TITLE = 'TOHA'                                                684
         AG_EXNAME = 'POSITION'                                           684
         AG_PARLIST = 'NONE'                                              684
         CALL AXPOSITION                                                  684
C    *                                                                    684
5111  CALL AGSPOPB                                                        685
      GO TO BLOCK_TOFC, (5110 ,5120)                                      685
C End of Local Procedure                                                  685
C ----------------------------------------------------------------------  686
C ------------ TOFS                                                       686
C Begin Local Procedure TOFS                                              687
C TOFS IS THE OUTER FIELD CAGE STRUCTUREURE                               687
5250  CONTINUE                                                            687
         AG_TITLE='TOFS'                                                  687
         AG_MODULE=MODULE                                                 687
         AG_MARK='V00'                                                    687
         CALL AGSPUSH                                                     687
         AG_MODULE=MODULE                                                 687
C    *                                                                    687
C    Geant MATERIAL : COPPER ===> inherited arguments used <===           688
         CALL AGSRESET                                                    688
         AG_TITLE = 'COPPER'                                              688
         AG_EXNAME = 'MATERIAL'                                           688
         AG_PARLIST = 'NONE'                                              688
         CALL AXMATERIAL                                                  688
C    *                                                                    688
C    Geant MEDIUM : TOFS_COPPER ===> IFIELD=1 FIELDM=20 TMAXFD=20 EPSIL=  688
C    0.01 STEMAX=10 DEEMAX=0.25, STMIN=0.0281386 ISVOL=0 <===             688
         CALL AGSRESET                                                    690
         AG_IFIELD = 1                                                    690
         AG_FIELDM = 20                                                   690
         AG_TMAXFD = 20                                                   690
         AG_EPSIL = 0.01                                                  690
         AG_STEMAX = 10                                                   690
         AG_DEEMAX = 0.25                                                 690
         AG_STMIN = 0.0281386                                             690
         AG_ISVOL = 0                                                     690
         AG_TITLE = 'TOFS_COPPER'                                         690
         AG_EXNAME = 'MEDIUM'                                             690
         AG_PARLIST = 'IFIELD_FIELDM_TMAXFD_EPSIL_STEMAX_DEEMAX_STMIN_IS  690
     *VOL'                                                                690
         CALL AXMEDIUM                                                    690
C    *                                                                    690
C    Geant ATTRIBUTE : TOFS ===> SEEN=1 COLO=KBLUE <===                   690
         CALL AGSRESET                                                    691
         AG_SEEN = 1                                                      691
         AG_COLO = KBLUE                                                  691
         AG_TITLE = 'TOFS'                                                691
         AG_EXNAME = 'ATTRIBUTE'                                          691
         AG_PARLIST = 'SEEN_COLO'                                         691
         CALL AXATTRIBUTE                                                 691
C    *                                                                    691
C    Geant SHAPE : TUBE ===> RMIN=TOCSIR RMAX=TOCSOR DZ=TPCG_DZYF2 <===   691
         CALL AGSRESET                                                    692
         AG_RMIN = TOCSIR                                                 692
         AG_RMAX = TOCSOR                                                 692
         AG_DZ = TPCG_DZYF2                                               692
         AG_TITLE = 'TUBE'                                                692
         AG_EXNAME = 'SHAPE'                                              692
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      692
         CALL AXSHAPE                                                     692
C    *                                                                    692
5251  CALL AGSPOPB                                                        693
      GO TO BLOCK_TOFS, (5250 ,5260)                                      693
C End of Local Procedure                                                  693
C ----------------------------------------------------------------------  694
C ------------ TOKA                                                       694
C Begin Local Procedure TOKA                                              695
C TOKA IS MYLAR LAYER                                                     695
5270  CONTINUE                                                            695
         AG_TITLE='TOKA'                                                  695
         AG_MODULE=MODULE                                                 695
         AG_MARK='V00'                                                    695
         CALL AGSPUSH                                                     695
         AG_MODULE=MODULE                                                 695
C    *                                                                    695
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            696
         CALL AGSRESET                                                    696
         AG_TITLE = 'MYLAR'                                               696
         AG_EXNAME = 'MATERIAL'                                           696
         AG_PARLIST = 'NONE'                                              696
         CALL AXMATERIAL                                                  696
C    *                                                                    696
C    Geant ATTRIBUTE : TOKA ===> SEEN=1 COLO=KGREEN <===                  696
         CALL AGSRESET                                                    697
         AG_SEEN = 1                                                      697
         AG_COLO = KGREEN                                                 697
         AG_TITLE = 'TOKA'                                                697
         AG_EXNAME = 'ATTRIBUTE'                                          697
         AG_PARLIST = 'SEEN_COLO'                                         697
         CALL AXATTRIBUTE                                                 697
C    *                                                                    697
C    Geant SHAPE : TUBE ===> RMIN=TOKAIR RMAX=TOKAOR DZ=TPCG_DZYF2 <===   697
         CALL AGSRESET                                                    698
         AG_RMIN = TOKAIR                                                 698
         AG_RMAX = TOKAOR                                                 698
         AG_DZ = TPCG_DZYF2                                               698
         AG_TITLE = 'TUBE'                                                698
         AG_EXNAME = 'SHAPE'                                              698
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      698
         CALL AXSHAPE                                                     698
C    *                                                                    698
5271  CALL AGSPOPB                                                        699
      GO TO BLOCK_TOKA, (5270 ,5280)                                      699
C End of Local Procedure                                                  699
C ----------------------------------------------------------------------  700
C ------------ TONX                                                       700
C Begin Local Procedure TONX                                              701
C TONX IS NOMEX SUPPORT                                                   701
5290  CONTINUE                                                            701
         AG_TITLE='TONX'                                                  701
         AG_MODULE=MODULE                                                 701
         AG_MARK='V00'                                                    701
         CALL AGSPUSH                                                     701
         AG_MODULE=MODULE                                                 701
C    *                                                                    701
C    Geant MATERIAL : NOMEX ===> inherited arguments used <===            702
         CALL AGSRESET                                                    702
         AG_TITLE = 'NOMEX'                                               702
         AG_EXNAME = 'MATERIAL'                                           702
         AG_PARLIST = 'NONE'                                              702
         CALL AXMATERIAL                                                  702
C    *                                                                    702
C    Geant ATTRIBUTE : TONX ===> SEEN=1 COLO=KRED <===                    702
         CALL AGSRESET                                                    703
         AG_SEEN = 1                                                      703
         AG_COLO = KRED                                                   703
         AG_TITLE = 'TONX'                                                703
         AG_EXNAME = 'ATTRIBUTE'                                          703
         AG_PARLIST = 'SEEN_COLO'                                         703
         CALL AXATTRIBUTE                                                 703
C    *                                                                    703
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=6 ZI={-TPCG_DZYF2, -TPCG  703
C    _DZYF3,-TPCG_DZYF3,TPCG_DZYF3,TPCG_DZYF3, TPCG_DZYF2} RMN={TONXOR-0  703
C    .2,TONXOR-0.2, TONXIR,TONXIR,TONXOR-0.2,TONXOR-0.2} RMX={TONXOR, TO  703
C    NXOR,TONXOR,TONXOR,TONXOR,TONXOR} <===                               703
         CALL AGSRESET                                                    708
         AG_PHI1 = 0                                                      708
         AG_DPHI = 360                                                    708
         AG_NZ = 6                                                        708
         AG_ZI(1)=-TPCG_DZYF2                                             708
         AG_ZI(2)=-TPCG_DZYF3                                             708
         AG_ZI(3)=-TPCG_DZYF3                                             708
         AG_ZI(4)=TPCG_DZYF3                                              708
         AG_ZI(5)=TPCG_DZYF3                                              708
         AG_ZI(6)=TPCG_DZYF2                                              708
         AG_RMN(1)=TONXOR-0.2                                             708
         AG_RMN(2)=TONXOR-0.2                                             708
         AG_RMN(3)=TONXIR                                                 708
         AG_RMN(4)=TONXIR                                                 708
         AG_RMN(5)=TONXOR-0.2                                             708
         AG_RMN(6)=TONXOR-0.2                                             708
         AG_RMX(1)=TONXOR                                                 708
         AG_RMX(2)=TONXOR                                                 708
         AG_RMX(3)=TONXOR                                                 708
         AG_RMX(4)=TONXOR                                                 708
         AG_RMX(5)=TONXOR                                                 708
         AG_RMX(6)=TONXOR                                                 708
         AG_TITLE = 'PCON'                                                708
         AG_EXNAME = 'SHAPE'                                              708
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           708
         CALL AXSHAPE                                                     708
C    *                                                                    708
5291  CALL AGSPOPB                                                        709
      GO TO BLOCK_TONX, (5290 ,5300)                                      709
C End of Local Procedure                                                  709
C ----------------------------------------------------------------------  710
C ------------ TOAD                                                       710
C Begin Local Procedure TOAD                                              711
C TOAD IS ADHESIVE LAYER                                                  711
5230  CONTINUE                                                            711
         AG_TITLE='TOAD'                                                  711
         AG_MODULE=MODULE                                                 711
         AG_MARK='V00'                                                    711
         CALL AGSPUSH                                                     711
         AG_MODULE=MODULE                                                 711
C    *                                                                    711
C    Geant MATERIAL : MYLAR ===> inherited arguments used <===            712
         CALL AGSRESET                                                    712
         AG_TITLE = 'MYLAR'                                               712
         AG_EXNAME = 'MATERIAL'                                           712
         AG_PARLIST = 'NONE'                                              712
         CALL AXMATERIAL                                                  712
C    *                                                                    712
C    Geant ATTRIBUTE : TOAD ===> SEEN=1 COLO=KLIGHTBLUE <===              712
         CALL AGSRESET                                                    713
         AG_SEEN = 1                                                      713
         AG_COLO = KLIGHTBLUE                                             713
         AG_TITLE = 'TOAD'                                                713
         AG_EXNAME = 'ATTRIBUTE'                                          713
         AG_PARLIST = 'SEEN_COLO'                                         713
         CALL AXATTRIBUTE                                                 713
C    *                                                                    713
C    Geant SHAPE : TUBE ===> RMIN=TOADIR RMAX=TOADOR DZ=TPCG_DZYF2 <===   713
         CALL AGSRESET                                                    714
         AG_RMIN = TOADIR                                                 714
         AG_RMAX = TOADOR                                                 714
         AG_DZ = TPCG_DZYF2                                               714
         AG_TITLE = 'TUBE'                                                714
         AG_EXNAME = 'SHAPE'                                              714
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      714
         CALL AXSHAPE                                                     714
C    *                                                                    714
5231  CALL AGSPOPB                                                        715
      GO TO BLOCK_TOAD, (5230 ,5240)                                      715
C End of Local Procedure                                                  715
C ----------------------------------------------------------------------  716
C ------------ TOIG                                                       716
C Begin Local Procedure TOIG                                              717
C TOIG IS INSULATING GAS (NITROGEN)                                       717
5310  CONTINUE                                                            717
         AG_TITLE='TOIG'                                                  717
         AG_MODULE=MODULE                                                 717
         AG_MARK='V00'                                                    717
         CALL AGSPUSH                                                     717
         AG_MODULE=MODULE                                                 717
C    *                                                                    717
C    Geant MATERIAL : NITROGEN_GAS ===> inherited arguments used <===     718
         CALL AGSRESET                                                    718
         AG_TITLE = 'NITROGEN_GAS'                                        718
         AG_EXNAME = 'MATERIAL'                                           718
         AG_PARLIST = 'NONE'                                              718
         CALL AXMATERIAL                                                  718
C    *                                                                    718
C    Geant ATTRIBUTE : TOAD ===> SEEN=1 COLO=KLIGHTBLUE <===              718
         CALL AGSRESET                                                    719
         AG_SEEN = 1                                                      719
         AG_COLO = KLIGHTBLUE                                             719
         AG_TITLE = 'TOAD'                                                719
         AG_EXNAME = 'ATTRIBUTE'                                          719
         AG_PARLIST = 'SEEN_COLO'                                         719
         CALL AXATTRIBUTE                                                 719
C    *                                                                    719
C    Geant SHAPE : TUBE ===> RMIN=TOIGIR RMAX=TOIGOR DZ=TPCG_DZYF2 <===   719
         CALL AGSRESET                                                    720
         AG_RMIN = TOIGIR                                                 720
         AG_RMAX = TOIGOR                                                 720
         AG_DZ = TPCG_DZYF2                                               720
         AG_TITLE = 'TUBE'                                                720
         AG_EXNAME = 'SHAPE'                                              720
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      720
         CALL AXSHAPE                                                     720
C    *                                                                    720
5311  CALL AGSPOPB                                                        721
      GO TO BLOCK_TOIG, (5310 ,5320)                                      721
C End of Local Procedure                                                  721
C ----------------------------------------------------------------------  722
C ------------ TOHA                                                       722
C Begin Local Procedure TOHA                                              723
C TOHA GAS CONTAINMENT VESSEL (AL) + HA                                   723
5330  CONTINUE                                                            723
         AG_TITLE='TOHA'                                                  723
         AG_MODULE=MODULE                                                 723
         AG_MARK='V00'                                                    723
         CALL AGSPUSH                                                     723
         AG_MODULE=MODULE                                                 723
C    *                                                                    723
C    Geant MATERIAL : AL_HONEYCOMB ===> inherited arguments used <===     724
         CALL AGSRESET                                                    724
         AG_TITLE = 'AL_HONEYCOMB'                                        724
         AG_EXNAME = 'MATERIAL'                                           724
         AG_PARLIST = 'NONE'                                              724
         CALL AXMATERIAL                                                  724
C    *                                                                    724
C    Geant ATTRIBUTE : TOHA ===> SEEN=1 COLO=KVIOLET <===                 724
         CALL AGSRESET                                                    725
         AG_SEEN = 1                                                      725
         AG_COLO = KVIOLET                                                725
         AG_TITLE = 'TOHA'                                                725
         AG_EXNAME = 'ATTRIBUTE'                                          725
         AG_PARLIST = 'SEEN_COLO'                                         725
         CALL AXATTRIBUTE                                                 725
C    *                                                                    725
C    Geant SHAPE : TUBE ===> RMIN=TOHAIR RMAX=TOHAOR DZ=TPCG_DZYF3 <===   725
         CALL AGSRESET                                                    726
         AG_RMIN = TOHAIR                                                 726
         AG_RMAX = TOHAOR                                                 726
         AG_DZ = TPCG_DZYF3                                               726
         AG_TITLE = 'TUBE'                                                726
         AG_EXNAME = 'SHAPE'                                              726
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      726
         CALL AXSHAPE                                                     726
C    *                                                                    726
5331  CALL AGSPOPB                                                        727
      GO TO BLOCK_TOHA, (5330 ,5340)                                      727
C End of Local Procedure                                                  727
C ----------------------------------------------------------------------  728
C ------------ TSWH                                                       728
C Begin Local Procedure TSWH                                              729
C TSWH TPCSECTORWHOLE IS SECTOR AS WHOLE                                  729
5140  CONTINUE                                                            729
         AG_TITLE='TSWH'                                                  729
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
C    Geant ATTRIBUTE : TSWH ===> SEEN=1 COLO=KVIOLET <===                 730
         CALL AGSRESET                                                    731
         AG_SEEN = 1                                                      731
         AG_COLO = KVIOLET                                                731
         AG_TITLE = 'TSWH'                                                731
         AG_EXNAME = 'ATTRIBUTE'                                          731
         AG_PARLIST = 'SEEN_COLO'                                         731
         CALL AXATTRIBUTE                                                 731
C    *                                                                    731
C    Geant SHAPE : PCON ===> PHI1=-15 DPHI=30 NZ=8 ZI={TPCG_MEMBTHK/2, Z  731
C    WHEEL1,ZWHEEL1,ZTOFCEND,ZTOFCEND, ZTIFCFLANGEBEGIN,ZTIFCFLANGEBEGIN  731
C    ,TPCG_DZENVELOP}, RMN={TIFCOR,TIFCOR,TIFCOR,TIFCOR,TIFCOR, TPCG_TIF  731
C    CRF+TPCG_TIFCDRT,TPCG_TIFCRF+TPCG_TIFCDRT, TPCG_TIFCRF+TPCG_TIFCDRT  731
C    } RMX={TOFCIR,TOFCIR, TPCG_WHEELOR1,TPCG_WHEELOR1,TPCG_WHEELOR, TPC  731
C    G_WHEELOR,TPCG_WHEELOR,TPCG_WHEELOR} <===                            731
         CALL AGSRESET                                                    739
         AG_PHI1 = -15                                                    739
         AG_DPHI = 30                                                     739
         AG_NZ = 8                                                        739
         AG_ZI(1)=TPCG_MEMBTHK/2                                          739
         AG_ZI(2)=ZWHEEL1                                                 739
         AG_ZI(3)=ZWHEEL1                                                 739
         AG_ZI(4)=ZTOFCEND                                                739
         AG_ZI(5)=ZTOFCEND                                                739
         AG_ZI(6)=ZTIFCFLANGEBEGIN                                        739
         AG_ZI(7)=ZTIFCFLANGEBEGIN                                        739
         AG_ZI(8)=TPCG_DZENVELOP                                          739
         AG_RMN(1)=TIFCOR                                                 739
         AG_RMN(2)=TIFCOR                                                 739
         AG_RMN(3)=TIFCOR                                                 739
         AG_RMN(4)=TIFCOR                                                 739
         AG_RMN(5)=TIFCOR                                                 739
         AG_RMN(6)=TPCG_TIFCRF+TPCG_TIFCDRT                               739
         AG_RMN(7)=TPCG_TIFCRF+TPCG_TIFCDRT                               739
         AG_RMN(8)=TPCG_TIFCRF+TPCG_TIFCDRT                               739
         AG_RMX(1)=TOFCIR                                                 739
         AG_RMX(2)=TOFCIR                                                 739
         AG_RMX(3)=TPCG_WHEELOR1                                          739
         AG_RMX(4)=TPCG_WHEELOR1                                          739
         AG_RMX(5)=TPCG_WHEELOR                                           739
         AG_RMX(6)=TPCG_WHEELOR                                           739
         AG_RMX(7)=TPCG_WHEELOR                                           739
         AG_RMX(8)=TPCG_WHEELOR                                           739
         AG_TITLE = 'PCON'                                                739
         AG_EXNAME = 'SHAPE'                                              739
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           739
         CALL AXSHAPE                                                     739
C    *                                                                    739
C    Create TSAW :                                                        740
C    Call To Local Procedure TSAW                                         740
         ASSIGN 5360 TO BLOCK_TSAW                                        740
         GO TO  5350                                                      740
5360     CONTINUE                                                         740
C    *                                                                    740
C    Geant POSITION : TSAW ===> KONLY='MANY' <===                         741
         CALL AGSRESET                                                    741
         AG_KONLY = 'MANY'                                                741
         AG_TITLE = 'TSAW'                                                741
         AG_EXNAME = 'POSITION'                                           741
         AG_PARLIST = 'KONLY'                                             741
         CALL AXPOSITION                                                  741
C    *                                                                    741
5141  CALL AGSPOPB                                                        742
      GO TO BLOCK_TSWH, (5140 ,5150,5160)                                 742
C End of Local Procedure                                                  742
C ----------------------------------------------------------------------  743
C ------------ TSAW                                                       743
C Begin Local Procedure TSAW                                              744
C TSAW TPCSECTORANDWHEEL                                                  744
5350  CONTINUE                                                            744
         AG_TITLE='TSAW'                                                  744
         AG_MODULE=MODULE                                                 744
         AG_MARK='V00'                                                    744
         CALL AGSPUSH                                                     744
         AG_MODULE=MODULE                                                 744
C    *                                                                    744
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           745
         CALL AGSRESET                                                    745
         AG_TITLE = 'STANDARD'                                            745
         AG_EXNAME = 'MEDIUM'                                             745
         AG_PARLIST = 'NONE'                                              745
         CALL AXMEDIUM                                                    745
C    *                                                                    745
C    Geant ATTRIBUTE : TSAW ===> SEEN=1 COLO=KYELLOW <===                 745
         CALL AGSRESET                                                    746
         AG_SEEN = 1                                                      746
         AG_COLO = KYELLOW                                                746
         AG_TITLE = 'TSAW'                                                746
         AG_EXNAME = 'ATTRIBUTE'                                          746
         AG_PARLIST = 'SEEN_COLO'                                         746
         CALL AXATTRIBUTE                                                 746
C    *                                                                    746
C    Geant SHAPE : PCON ===> PHI1=-15 DPHI=30 NZ=2 ZI={204, TPCG_DZENVEL  746
C    OP-TPCG_MEMBTHK/2} RMN={48,48} RMX={208, 208} <===                   746
         CALL AGSRESET                                                    749
         AG_PHI1 = -15                                                    749
         AG_DPHI = 30                                                     749
         AG_NZ = 2                                                        749
         AG_ZI(1)=204                                                     749
         AG_ZI(2)=TPCG_DZENVELOP-TPCG_MEMBTHK/2                           749
         AG_RMN(1)=48                                                     749
         AG_RMN(2)=48                                                     749
         AG_RMX(1)=208                                                    749
         AG_RMX(2)=208                                                    749
         AG_TITLE = 'PCON'                                                749
         AG_EXNAME = 'SHAPE'                                              749
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           749
         CALL AXSHAPE                                                     749
C    *                                                                    749
         DX2 = 1.470                                                      749
         DX = 0                                                           751
C    use TECW bank                                                        752
         BPATH_TECW='TECW*'                                               752
         IDNUM_TECW(1)=0                                                  752
         TECW_SEC=1                                                       752
         CALL AGDGETP('TPCXGEO2','*',-999,'TECW',NTECW_SEC,RE_TECW_SEC,   752
     *   BPATH_TECW,IDNUM_TECW,1,MAP_TECW,NAMES_TECW,BANK_TECW,LEN_TECW,  752
     *   LLEN_TECW,LINK_TECW,FLAG_TECW)                                   752
C    *                                                                    753
         INOUT=KINNER                                                     753
            GO TO 5373                                                    753
5371        INOUT=INOUT+1                                                 753
5373        IF(INOUT-(KOUTER).GT.0) GO TO 5372                            753
            MYCOUNT = 0                                                   753
C       use COOL bank                                                     755
            BPATH_COOL='COOL*'                                            755
            IDNUM_COOL(1)=0                                               755
            COOL_INOUT=INOUT                                              755
            CALL AGDGETP('TPCXGEO2','*',-999,'COOL',NCOOL_INOUT,          755
     *      RE_COOL_INOUT,BPATH_COOL,IDNUM_COOL,1,MAP_COOL,NAMES_COOL,    755
     *      BANK_COOL,LEN_COOL,LLEN_COOL,LINK_COOL,FLAG_COOL)             755
C       use RIBS bank                                                     756
            BPATH_RIBS='RIBS*'                                            756
            IDNUM_RIBS(1)=0                                               756
            RIBS_INOUT=INOUT                                              756
            CALL AGDGETP('TPCXGEO2','*',-999,'RIBS',NRIBS_INOUT,          756
     *      RE_RIBS_INOUT,BPATH_RIBS,IDNUM_RIBS,1,MAP_RIBS,NAMES_RIBS,    756
     *      BANK_RIBS,LEN_RIBS,LLEN_RIBS,LINK_RIBS,FLAG_RIBS)             756
C       use HOLZ bank                                                     757
            BPATH_HOLZ='HOLZ*'                                            757
            IDNUM_HOLZ(1)=0                                               757
            HOLZ_INOUT=INOUT                                              757
            CALL AGDGETP('TPCXGEO2','*',-999,'HOLZ',NHOLZ_INOUT,          757
     *      RE_HOLZ_INOUT,BPATH_HOLZ,IDNUM_HOLZ,1,MAP_HOLZ,NAMES_HOLZ,    757
     *      BANK_HOLZ,LEN_HOLZ,LLEN_HOLZ,LINK_HOLZ,FLAG_HOLZ)             757
C       use TECW bank                                                     758
            BPATH_TECW='TECW*'                                            758
            IDNUM_TECW(1)=0                                               758
            TECW_SEC=(INOUT+1)                                            758
            CALL AGDGETP('TPCXGEO2','*',-999,'TECW',NTECW_SEC,            758
     *      RE_TECW_SEC,BPATH_TECW,IDNUM_TECW,1,MAP_TECW,NAMES_TECW,      758
     *      BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)             758
            Z2 = ZWHEEL1                                                  759
            Z1 = Z2 - TECW_THICK                                          760
            Z = (Z1 + TPCG_MEMBTHK/2)/2                                   761
            DZ = (Z1 - TPCG_MEMBTHK/2)/2                                  762
            X1 = -TECW_HEIGHT/2                                           763
            X2 = TECW_HEIGHT/2                                            764
            Y1 = TECW_INWIDTH/2                                           765
            Y2 = TECW_OUWIDTH/2                                           766
            X0 = (Y1*X2-Y2*X1)/(X2-X1)                                    767
            DYDX = (Y2 - Y1)/(X2 - X1)                                    768
            ALPHA = 180./PI*ATAN(DYDX)                                    769
            R1 = Y1/DYDX                                                  770
            R2 = Y2/DYDX                                                  771
            DRSECTORSHIFT = TECW_RMIN - R1                                772
C       Check INOUT==KINNER                                               773
            IF (INOUT.EQ.KINNER) THEN                                     773
            XHOLES(0) = R2 - 1.988*INCH                                   774
C       *                                                                 775
            I=1                                                           775
               GO TO 5383                                                 775
5381           I=I+1                                                      775
5383           IF(I-(HOLZ_NUMBEROFROWS-1).GT.0) GO TO 5382                775
               XHOLES(I) = XHOLES(I-1) - 1.575*INCH                       776
            GO TO 5381                                                    777
5382        CONTINUE                                                      777
            ELSE                                                          778
C       *                                                                 779
            I=0                                                           779
               GO TO 5393                                                 779
5391           I=I+1                                                      779
5393           IF(I-(HOLZ_NUMBEROFROWS - 1).GT.0) GO TO 5392              779
               XHOLES(I) = R2 - (1.988 + 1.575*I)*INCH                    780
            GO TO 5391                                                    781
5392        CONTINUE                                                      781
            ENDIF                                                         782
C       Create TSAS :                                                     783
C       Call To Local Procedure TSAS                                      783
            ASSIGN 5410 TO BLOCK_TSAS                                     783
            GO TO  5400                                                   783
5410        CONTINUE                                                      783
C       *                                                                 783
C       Geant POSITION : TSAS ===> Z=TSAS_ZOFFSET KONLY='MANY' <===       784
            CALL AGSRESET                                                 784
            AG_Z = TSAS_ZOFFSET                                           784
            AG_KONLY = 'MANY'                                             784
            AG_TITLE = 'TSAS'                                             784
            AG_EXNAME = 'POSITION'                                        784
            AG_PARLIST = 'Z_KONLY'                                        784
            CALL AXPOSITION                                               784
C       *                                                                 784
C       Create TWAS :                                                     785
C       Call To Local Procedure TWAS                                      785
            ASSIGN 5430 TO BLOCK_TWAS                                     785
            GO TO  5420                                                   785
5430        CONTINUE                                                      785
C       *                                                                 785
C       Geant POSITION : TWAS ===> KONLY='MANY' <===                      786
            CALL AGSRESET                                                 786
            AG_KONLY = 'MANY'                                             786
            AG_TITLE = 'TWAS'                                             786
            AG_EXNAME = 'POSITION'                                        786
            AG_PARLIST = 'KONLY'                                          786
            CALL AXPOSITION                                               786
C       *                                                                 786
C       Check INOUT==0                                                    786
            IF (INOUT.EQ.0) THEN                                          787
C       Create TBRW :                                                     788
C       Call To Local Procedure TBRW                                      788
            ASSIGN 5450 TO BLOCK_TBRW                                     788
            GO TO  5440                                                   788
5450        CONTINUE                                                      788
C       *                                                                 788
C       Geant POSITION : TBRW ===> X=TPCG_WHEELR1 Z=ZWHEEL1+TPCG_WHEELTH  789
C       K/2 <===                                                          789
            CALL AGSRESET                                                 789
            AG_X = TPCG_WHEELR1                                           789
            AG_Z = ZWHEEL1+TPCG_WHEELTHK/2                                789
            AG_TITLE = 'TBRW'                                             789
            AG_EXNAME = 'POSITION'                                        789
            AG_PARLIST = 'X_Z'                                            789
            CALL AXPOSITION                                               789
C       *                                                                 789
            ENDIF                                                         789
            X1 = TPCG_WHEELR0*COS15                                       791
            X2 = TPCG_WHEELR2                                             792
            DZ = TPCG_WHEELRIBHEIGHT/2                                    793
            Z = ZWHEEL1 + DZ                                              794
            DX = (X2 - X1)/2                                              795
            DY = TPCG_WHEELTOTALRIBWIDTH/4                                796
            X = (X1 + X2)/2                                               797
            Y = (X *TAN15 - DY)*(2*INOUT-1)                               798
            MYDX=DY                                                       798
            MYDY=DX                                                       798
            MYDZ=DZ                                                       798
            MYALPH=-(1-2*INOUT)*15                                        799
C       Create TWRB :                                                     800
C       Call To Local Procedure TWRB                                      800
            ASSIGN 5470 TO BLOCK_TWRB                                     800
            GO TO  5460                                                   800
5470        CONTINUE                                                      800
C       *                                                                 800
C       Geant POSITION : TWRB ===> X=X Y=Y Z=Z ORT=YX-Z <===              801
            CALL AGSRESET                                                 801
            AG_X = X                                                      801
            AG_Y = Y                                                      801
            AG_Z = Z                                                      801
            AG_ALPHAN=AG_ALPHAN+1                                         801
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             801
            AG_TITLE = 'TWRB'                                             801
            AG_EXNAME = 'POSITION'                                        801
            AG_PARLIST = 'X_Y_Z_ORT'                                      801
            CALL AXPOSITION                                               801
C       *                                                                 801
            DZ = TPCG_WHEELTHK/2                                          801
            Z = ZWHEEL1 + DZ                                              803
            DY = TPCG_WHEELRIBWIDTH/4                                     804
            X = (X1 + X2)/2                                               805
            Y = (X *TAN15 - DY)*(2*INOUT-1)                               806
            MYDX=DY                                                       806
            MYDY=DX                                                       806
            MYDZ=DZ                                                       806
            MYALPH=-(1-2*INOUT)*15                                        807
C       Create TWRB :                                                     808
C       Call To Local Procedure TWRB                                      808
            ASSIGN 5480 TO BLOCK_TWRB                                     808
            GO TO  5460                                                   808
5480        CONTINUE                                                      808
C       *                                                                 808
C       Geant POSITION : TWRB ===> X=X Y=Y Z=Z ORT=YX-Z <===              809
            CALL AGSRESET                                                 809
            AG_X = X                                                      809
            AG_Y = Y                                                      809
            AG_Z = Z                                                      809
            AG_ALPHAN=AG_ALPHAN+1                                         809
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             809
            AG_TITLE = 'TWRB'                                             809
            AG_EXNAME = 'POSITION'                                        809
            AG_PARLIST = 'X_Y_Z_ORT'                                      809
            CALL AXPOSITION                                               809
C       *                                                                 809
         GO TO 5371                                                       810
5372     CONTINUE                                                         810
C    *                                                                    811
         ALPHA=-15                                                        811
            GOTO 5493                                                     811
5491        ALPHA=ALPHA+(30)                                              811
5493        IF((30)*(ALPHA-(15)).GT.0) GO TO 5492                         811
C       *                                                                 812
            J=0                                                           812
               GO TO 5503                                                 812
5501           J=J+1                                                      812
5503           IF(J-(3).GT.0) GO TO 5502                                  812
C          Check (J==0)                                                   813
               IF ((J.EQ.0)) THEN                                         813
               R = TPCG_WHEELR2 - 1.5/8.5*(TPCG_WHEELR2 - TPCG_WHEELR1)   814
               ENDIF                                                      815
C          Check (J==1)                                                   816
               IF ((J.EQ.1)) THEN                                         816
               R = TPCG_WHEELR2 - 6.5/8.5*(TPCG_WHEELR2 - TPCG_WHEELR1)   817
               ENDIF                                                      818
C          Check (J==2)                                                   819
               IF ((J.EQ.2)) THEN                                         819
               R = TPCG_WHEELR1 - 2.0/8.5*(TPCG_WHEELR1 - TPCG_WHEELR0)   820
               ENDIF                                                      821
C          Check (J==3)                                                   822
               IF ((J.EQ.3)) THEN                                         822
               R = TPCG_WHEELR1 - 7.0/8.5*(TPCG_WHEELR1 - TPCG_WHEELR0)   823
               ENDIF                                                      824
               X = R                                                      825
               Y = R *TAND(ALPHA)                                         826
C          Geant POSITION : TBRW ===> X=X Y=Y Z=ZWHEEL1+TPCG_WHEELTHK/2   827
C          ALPHAZ=ALPHA <===                                              827
               CALL AGSRESET                                              827
               AG_X = X                                                   827
               AG_Y = Y                                                   827
               AG_Z = ZWHEEL1+TPCG_WHEELTHK/2                             827
               AG_ALPHAN=AG_ALPHAN+1                                      827
               AG_ALPHAK(AG_ALPHAN)=3                                     827
               AG_ALPHAL(AG_ALPHAN)=ALPHA                                 827
               AG_TITLE = 'TBRW'                                          827
               AG_EXNAME = 'POSITION'                                     827
               AG_PARLIST = 'X_Y_Z_ALPHAZ'                                827
               CALL AXPOSITION                                            827
C          *                                                              827
               MYCOUNT=MYCOUNT+1                                          827
            GO TO 5501                                                    829
5502        CONTINUE                                                      829
            QWE(1) = -QWE(1)                                              830
            QWE(2) = -QWE(2)                                              831
         GO TO 5491                                                       832
5492     CONTINUE                                                         832
C    Create TWBT :                                                        833
C    Call To Local Procedure TWBT                                         833
         ASSIGN 5520 TO BLOCK_TWBT                                        833
         GO TO  5510                                                      833
5520     CONTINUE                                                         833
C    *                                                                    833
C    Geant POSITION : TWBT ===> inherited arguments used <===             834
         CALL AGSRESET                                                    834
         AG_TITLE = 'TWBT'                                                834
         AG_EXNAME = 'POSITION'                                           834
         AG_PARLIST = 'NONE'                                              834
         CALL AXPOSITION                                                  834
C    *                                                                    834
C    Create TWRI :                                                        835
C    Call To Local Procedure TWRI                                         835
         ASSIGN 5540 TO BLOCK_TWRI                                        835
         GO TO  5530                                                      835
5540     CONTINUE                                                         835
C    *                                                                    835
C    Geant POSITION : TWRI ===> KONLY='MANY' <===                         836
         CALL AGSRESET                                                    836
         AG_KONLY = 'MANY'                                                836
         AG_TITLE = 'TWRI'                                                836
         AG_EXNAME = 'POSITION'                                           836
         AG_PARLIST = 'KONLY'                                             836
         CALL AXPOSITION                                                  836
C    *                                                                    836
C    Create TWTR :                                                        837
C    Call To Local Procedure TWTR                                         837
         ASSIGN 5560 TO BLOCK_TWTR                                        837
         GO TO  5550                                                      837
5560     CONTINUE                                                         837
C    *                                                                    837
C    Geant POSITION : TWTR ===> inherited arguments used <===             838
         CALL AGSRESET                                                    838
         AG_TITLE = 'TWTR'                                                838
         AG_EXNAME = 'POSITION'                                           838
         AG_PARLIST = 'NONE'                                              838
         CALL AXPOSITION                                                  838
C    *                                                                    838
C    Create TWMR :                                                        839
C    Call To Local Procedure TWMR                                         839
         ASSIGN 5580 TO BLOCK_TWMR                                        839
         GO TO  5570                                                      839
5580     CONTINUE                                                         839
C    *                                                                    839
C    Geant POSITION : TWMR ===> inherited arguments used <===             840
         CALL AGSRESET                                                    840
         AG_TITLE = 'TWMR'                                                840
         AG_EXNAME = 'POSITION'                                           840
         AG_PARLIST = 'NONE'                                              840
         CALL AXPOSITION                                                  840
C    *                                                                    840
C    Create TRDO :                                                        841
C    Call To Local Procedure TRDO                                         841
         ASSIGN 5600 TO BLOCK_TRDO                                        841
         GO TO  5590                                                      841
5600     CONTINUE                                                         841
C    *                                                                    841
C    Geant POSITION : TRDO ===> KONLY='MANY' <===                         842
         CALL AGSRESET                                                    842
         AG_KONLY = 'MANY'                                                842
         AG_TITLE = 'TRDO'                                                842
         AG_EXNAME = 'POSITION'                                           842
         AG_PARLIST = 'KONLY'                                             842
         CALL AXPOSITION                                                  842
C    *                                                                    842
         RCOOLINGTUBE = TPCG_WHEELR2 - TPCG_HEIGTUBE - 2*TPCG_DXRDO       842
C    *                                                                    844
         ICOO=0                                                           844
            GO TO 5613                                                    844
5611        ICOO=ICOO+1                                                   844
5613        IF(ICOO-(7).GT.0) GO TO 5612                                  844
            DZ = TPCG_HEIGTUBE*CM/2                                       845
            DY = TPCG_WIDTUBE *CM/2                                       846
            Z = RCOOLINGTUBE                                              847
            DX1 = (Z-DZ)*TAN15 - 2*TPCG_RDOCOOLINGDY                      848
            DX2 = (Z+DZ)*TAN15 - 2*TPCG_RDOCOOLINGDY                      849
            ISHAPE = KTRD1                                                849
            MYPAR(1)=DX1                                                  851
            MYPAR(2)=DX2                                                  851
            MYPAR(3)=DY                                                   851
            MYPAR(4)=DZ                                                   851
C       Create TCOO :                                                     852
C       Call To Local Procedure TCOO                                      852
            ASSIGN 5630 TO BLOCK_TCOO                                     852
            GO TO  5620                                                   852
5630        CONTINUE                                                      852
C       *                                                                 852
C       Geant POSITION : TCOO ===> X=Z Z=(TPCG_ZRDO-2*DY) ORT=YZX <===    853
            CALL AGSRESET                                                 853
            AG_X = Z                                                      853
            AG_Z = (TPCG_ZRDO-2*DY)                                       853
            AG_ALPHAN=AG_ALPHAN+1                                         853
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              853
            AG_TITLE = 'TCOO'                                             853
            AG_EXNAME = 'POSITION'                                        853
            AG_PARLIST = 'X_Z_ORT'                                        853
            CALL AXPOSITION                                               853
C       *                                                                 853
            RCOOLINGTUBE=RCOOLINGTUBE-DRDOCOOLING(ICOO)                   853
         GO TO 5611                                                       855
5612     CONTINUE                                                         855
         DZ = 4.5                                                         856
         DY = 4.5/2                                                       857
         Z = TPCG_ZRDO + TPCG_DZRDO + DZ                                  858
         X1 = TPCG_WHEELR0                                                859
         X2 = TPCG_WHEELR2                                                860
         DX =(X2 - X1)/2                                                  861
         X =(X1 + X2)/2                                                   862
C    *                                                                    863
         ICAB=0                                                           863
            GO TO 5643                                                    863
5641        ICAB=ICAB+1                                                   863
5643        IF(ICAB-(1).GT.0) GO TO 5642                                  863
            Y = - (X *TAN15 - DY) * (1 - 2*ICAB)                          864
            MYPAR(1)=DY                                                   865
            MYPAR(2)=DX                                                   865
            MYPAR(3)=DZ                                                   865
            MYPAR(4)=-15*(1-2*ICAB)                                       865
            MYPAR(5)=0                                                    865
            MYPAR(6)=0                                                    865
C       Create TCAB :                                                     866
C       Call To Local Procedure TCAB                                      866
            ASSIGN 5660 TO BLOCK_TCAB                                     866
            GO TO  5650                                                   866
5660        CONTINUE                                                      866
C       *                                                                 866
C       Geant POSITION : TCAB ===> X=X Y=Y Z=Z ORT=YX-Z <===              867
            CALL AGSRESET                                                 867
            AG_X = X                                                      867
            AG_Y = Y                                                      867
            AG_Z = Z                                                      867
            AG_ALPHAN=AG_ALPHAN+1                                         867
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             867
            AG_TITLE = 'TCAB'                                             867
            AG_EXNAME = 'POSITION'                                        867
            AG_PARLIST = 'X_Y_Z_ORT'                                      867
            CALL AXPOSITION                                               867
C       *                                                                 867
         GO TO 5641                                                       868
5642     CONTINUE                                                         868
5351  CALL AGSPOPB                                                        869
      GO TO BLOCK_TSAW, (5350 ,5360)                                      869
C End of Local Procedure                                                  869
C ----------------------------------------------------------------------  870
C ------------ TSAS                                                       870
C Begin Local Procedure TSAS                                              871
C TSAS TPCINNERSECTORASSEMBLY & TPCOUTERSECTORASSEMBLY TSAS AND TSA1      871
5400  CONTINUE                                                            871
         AG_TITLE='TSAS'                                                  871
         AG_MODULE=MODULE                                                 871
         AG_MARK='V00'                                                    871
         CALL AGSPUSH                                                     871
         AG_MODULE=MODULE                                                 871
C    *                                                                    871
C    Geant SHAPE : PGON ===> PHI1=-15 DPHI=30 NPDIV=1 NZ=2, ZI={ZZZSA(1)  874
C    ,ZZZSA(2)} RMN={RMNSA(INOUT),RMNSA(INOUT)}, RMX={RMXSA(INOUT),RMXSA  874
C    (INOUT)} <===                                                        874
         CALL AGSRESET                                                    874
         AG_PHI1 = -15                                                    874
         AG_DPHI = 30                                                     874
         AG_NPDIV = 1                                                     874
         AG_NZ = 2                                                        874
         AG_ZI(1)=ZZZSA(1)                                                874
         AG_ZI(2)=ZZZSA(2)                                                874
         AG_RMN(1)=RMNSA(INOUT)                                           874
         AG_RMN(2)=RMNSA(INOUT)                                           874
         AG_RMX(1)=RMXSA(INOUT)                                           874
         AG_RMX(2)=RMXSA(INOUT)                                           874
         AG_TITLE = 'PGON'                                                874
         AG_EXNAME = 'SHAPE'                                              874
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     874
         CALL AXSHAPE                                                     874
C    *                                                                    874
C    Geant ATTRIBUTE : TSAS ===> SEEN=0 <===                              874
         CALL AGSRESET                                                    875
         AG_SEEN = 0                                                      875
         AG_TITLE = 'TSAS'                                                875
         AG_EXNAME = 'ATTRIBUTE'                                          875
         AG_PARLIST = 'SEEN'                                              875
         CALL AXATTRIBUTE                                                 875
C    *                                                                    875
C    Create TALS :                                                        876
C    Call To Local Procedure TALS                                         876
         ASSIGN 5680 TO BLOCK_TALS                                        876
         GO TO  5670                                                      876
5680     CONTINUE                                                         876
C    *                                                                    876
C    Geant POSITION : TALS ===> inherited arguments used <===             877
         CALL AGSRESET                                                    877
         AG_TITLE = 'TALS'                                                877
         AG_EXNAME = 'POSITION'                                           877
         AG_PARLIST = 'NONE'                                              877
         CALL AXPOSITION                                                  877
C    *                                                                    877
C    Create TSGT :                                                        878
C    Call To Local Procedure TSGT                                         878
         ASSIGN 5700 TO BLOCK_TSGT                                        878
         GO TO  5690                                                      878
5700     CONTINUE                                                         878
C    *                                                                    878
C    Geant POSITION : TSGT ===> Z=TSGT_ZOFFSET <===                       879
         CALL AGSRESET                                                    879
         AG_Z = TSGT_ZOFFSET                                              879
         AG_TITLE = 'TSGT'                                                879
         AG_EXNAME = 'POSITION'                                           879
         AG_PARLIST = 'Z'                                                 879
         CALL AXPOSITION                                                  879
C    *                                                                    879
         DY =(R2 - R1)/2                                                  879
         Y =(R2 + R1)/2                                                   881
         DX = TECW_WIDTHRIB/2                                             882
         DZ =(TECW_THICK - TPCG_PADPLANETHICKNESS - TECW_THICKAL)/2       883
         Z = Z2 - DZ                                                      884
         DXW = TPCG_WIREMOUNTWIDTH/COS15/2                                885
         DZW = TPCG_WIREMOUNTHEIGHT/2                                     886
         XW = 0                                                           887
C    *                                                                    888
         IRIB=0                                                           888
            GO TO 5713                                                    888
5711        IRIB=IRIB+1                                                   888
5713        IF(IRIB-(1).GT.0) GO TO 5712                                  888
            ISHAPE = KPARA                                                888
            MYPAR(1)=DX                                                   890
            MYPAR(2)=DY                                                   890
            MYPAR(3)=DZ                                                   890
            MYPAR(4)=-(1-2*IRIB)*ALPHA                                    890
C       Create TRIB :                                                     891
C       Call To Local Procedure TRIB                                      891
            ASSIGN 5730 TO BLOCK_TRIB                                     891
            GO TO  5720                                                   891
5730        CONTINUE                                                      891
C       *                                                                 891
            X = ((Y1 + Y2)/2 - DX)*(1-2*IRIB)                             892
C       Geant POSITION : TRIB ===> X=Y Y=-X Z=Z ORT=YX-Z <===             893
            CALL AGSRESET                                                 893
            AG_X = Y                                                      893
            AG_Y = -X                                                     893
            AG_Z = Z                                                      893
            AG_ALPHAN=AG_ALPHAN+1                                         893
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             893
            AG_TITLE = 'TRIB'                                             893
            AG_EXNAME = 'POSITION'                                        893
            AG_PARLIST = 'X_Y_Z_ORT'                                      893
            CALL AXPOSITION                                               893
C       *                                                                 893
            XW = ((Y1 + Y2)/2 - DXW)*(1-2*IRIB)                           893
            MYPAR(1)=DXW                                                  895
            MYPAR(2)=DY                                                   895
            MYPAR(3)=DZW                                                  895
            MYPAR(4)=-(1-2*IRIB)*ALPHA                                    895
            MYPAR(5)=0                                                    895
            MYPAR(6)=0                                                    895
C       Create TWIR :                                                     896
C       Call To Local Procedure TWIR                                      896
            ASSIGN 5750 TO BLOCK_TWIR                                     896
            GO TO  5740                                                   896
5750        CONTINUE                                                      896
C       *                                                                 896
            ZW = Z1 - DZW/2                                               897
C       Geant POSITION : TWIR ===> X=Y Y=-XW Z=ZW ORT=YX-Z <===           898
            CALL AGSRESET                                                 898
            AG_X = Y                                                      898
            AG_Y = -XW                                                    898
            AG_Z = ZW                                                     898
            AG_ALPHAN=AG_ALPHAN+1                                         898
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             898
            AG_TITLE = 'TWIR'                                             898
            AG_EXNAME = 'POSITION'                                        898
            AG_PARLIST = 'X_Y_Z_ORT'                                      898
            CALL AXPOSITION                                               898
C       *                                                                 898
         GO TO 5711                                                       899
5712     CONTINUE                                                         899
C    *                                                                    900
         IRIB=0                                                           900
            GO TO 5763                                                    900
5761        IRIB=IRIB+1                                                   900
5763        IF(IRIB-(TECW_NORIBS-1).GT.0) GO TO 5762                      900
            R = R2 - RIBS_X (IRIB+1) * INCH                               901
            Y = RIBS_Y (IRIB+1) * INCH                                    902
            DR = RIBS_DX (IRIB+1) * INCH                                  903
            DY = RIBS_DY (IRIB+1) * INCH                                  904
C       Check DY<1.E-7                                                    905
            IF (DY.LT.1.E-7) THEN                                         905
            DX2 = R *DYDX - TECW_WIDTHRIB - 2*DXW                         905
C       TECW_WIDTHLIP                                                     906
            DX1 = (R - DR)*DYDX - TECW_WIDTHRIB - 2*DXW                   906
C       TECW_WIDTHLIP                                                     907
            XC = R - TECW_ZSTEPRIB/2                                      908
            ISHAPE = KTRD1                                                908
            MYPAR(1)=DX1                                                  910
            MYPAR(2)=DX2                                                  910
            MYPAR(3)=DZ                                                   910
            MYPAR(4)=DR/2                                                 910
C       Create TRIB :                                                     911
C       Call To Local Procedure TRIB                                      911
            ASSIGN 5770 TO BLOCK_TRIB                                     911
            GO TO  5720                                                   911
5770        CONTINUE                                                      911
C       *                                                                 911
C       Geant POSITION : TRIB ===> X=R-DR/2 Y=0 Z=Z2-DZ ORT=YZX <===      912
            CALL AGSRESET                                                 912
            AG_X = R-DR/2                                                 912
            AG_Y = 0                                                      912
            AG_Z = Z2-DZ                                                  912
            AG_ALPHAN=AG_ALPHAN+1                                         912
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              912
            AG_TITLE = 'TRIB'                                             912
            AG_EXNAME = 'POSITION'                                        912
            AG_PARLIST = 'X_Y_Z_ORT'                                      912
            CALL AXPOSITION                                               912
C       *                                                                 912
            ELSE                                                          912
            ISHAPE = KBBOX                                                913
            MYPAR(1)=DR/2                                                 915
            MYPAR(2)=DY/2                                                 915
            MYPAR(3)=DZ                                                   915
C       Create TRIB :                                                     916
C       Call To Local Procedure TRIB                                      916
            ASSIGN 5780 TO BLOCK_TRIB                                     916
            GO TO  5720                                                   916
5780        CONTINUE                                                      916
C       *                                                                 916
C       Geant POSITION : TRIB ===> X=R Y=Y Z=Z2-DZ <===                   917
            CALL AGSRESET                                                 917
            AG_X = R                                                      917
            AG_Y = Y                                                      917
            AG_Z = Z2-DZ                                                  917
            AG_TITLE = 'TRIB'                                             917
            AG_EXNAME = 'POSITION'                                        917
            AG_PARLIST = 'X_Y_Z'                                          917
            CALL AXPOSITION                                               917
C       *                                                                 917
            ENDIF                                                         917
         GO TO 5761                                                       919
5762     CONTINUE                                                         919
5401  CALL AGSPOPB                                                        920
      GO TO BLOCK_TSAS, (5400 ,5410)                                      920
C End of Local Procedure                                                  920
C ----------------------------------------------------------------------  921
C ------------ TWAS                                                       921
C Begin Local Procedure TWAS                                              922
C TWAS TPCWHEELINNERASSEMBLY & TPCWHEELOUTERASSEMBLY TWAS AND TWA1        922
5420  CONTINUE                                                            922
         AG_TITLE='TWAS'                                                  922
         AG_MODULE=MODULE                                                 922
         AG_MARK='V00'                                                    922
         CALL AGSPUSH                                                     922
         AG_MODULE=MODULE                                                 922
C    *                                                                    922
C    Geant ATTRIBUTE : TWAS ===> SEEN=0 <===                              923
         CALL AGSRESET                                                    923
         AG_SEEN = 0                                                      923
         AG_TITLE = 'TWAS'                                                923
         AG_EXNAME = 'ATTRIBUTE'                                          923
         AG_PARLIST = 'SEEN'                                              923
         CALL AXATTRIBUTE                                                 923
C    *                                                                    923
C    Geant SHAPE : PGON ===> PHI1=-15 DPHI=30 NPDIV=1 NZ=2, ZI={ZZZWA(1)  923
C    ,ZZZWA(2)} RMN={RMNWA(INOUT),RMNWA(INOUT)}, RMX={RMXWA(INOUT),RMXWA  923
C    (INOUT)} <===                                                        923
         CALL AGSRESET                                                    926
         AG_PHI1 = -15                                                    926
         AG_DPHI = 30                                                     926
         AG_NPDIV = 1                                                     926
         AG_NZ = 2                                                        926
         AG_ZI(1)=ZZZWA(1)                                                926
         AG_ZI(2)=ZZZWA(2)                                                926
         AG_RMN(1)=RMNWA(INOUT)                                           926
         AG_RMN(2)=RMNWA(INOUT)                                           926
         AG_RMX(1)=RMXWA(INOUT)                                           926
         AG_RMX(2)=RMXWA(INOUT)                                           926
         AG_TITLE = 'PGON'                                                926
         AG_EXNAME = 'SHAPE'                                              926
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     926
         CALL AXSHAPE                                                     926
C    *                                                                    926
         DX = TPCG_WIDTUBE*CM/2                                           926
         DZ = DEPTHIOF(INOUT + 1)/2                                       928
         DY = HEIGHTIOF(INOUT + 1)/2                                      929
         Y1 = - DY                                                        930
         Y2 = DY                                                          931
         X1 = WIDTH1IOF(INOUT + 1)/2 - DX                                 932
         X2 = WIDTH2IOF(INOUT + 1)/2 - DX                                 933
         X0 =(Y1*X2 - Y2*X1)/(Y2 - Y1)                                    934
         DYDX =(X2 - X1)/(Y2 - Y1)                                        935
         BETA = 180./PI*ATAN(DYDX)                                        936
C    Check (INOUT==KOUTER)                                                937
         IF ((INOUT.EQ.KOUTER)) THEN                                      937
         DY=DY-0.7                                                        938
         ENDIF                                                            939
         Y = XHOLES(0) - DY                                               940
         X =(X1 + X2)/2                                                   941
C    *                                                                    942
         ICOO=0                                                           942
            GO TO 5793                                                    942
5791        ICOO=ICOO+1                                                   942
5793        IF(ICOO-(1).GT.0) GO TO 5792                                  942
            MYPAR(1)=DX                                                   943
            MYPAR(2)=DY                                                   943
            MYPAR(3)=DZ                                                   943
            MYPAR(4)=-(1-2*ICOO)*BETA                                     943
            MYPAR(5)=0                                                    943
            MYPAR(6)=0                                                    943
            ISHAPE = KPARA                                                943
C       Create TCOO :                                                     945
C       Call To Local Procedure TCOO                                      945
            ASSIGN 5800 TO BLOCK_TCOO                                     945
            GO TO  5620                                                   945
5800        CONTINUE                                                      945
C       *                                                                 945
C       Geant POSITION : TCOO ===> X=Y Y=X*(2*ICOO-1) Z=ZWHEEL1+DZ ORT=Y  946
C       X-Z <===                                                          946
            CALL AGSRESET                                                 946
            AG_X = Y                                                      946
            AG_Y = X*(2*ICOO-1)                                           946
            AG_Z = ZWHEEL1+DZ                                             946
            AG_ALPHAN=AG_ALPHAN+1                                         946
            AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                             946
            AG_TITLE = 'TCOO'                                             946
            AG_EXNAME = 'POSITION'                                        946
            AG_PARLIST = 'X_Y_Z_ORT'                                      946
            CALL AXPOSITION                                               946
C       *                                                                 946
         GO TO 5791                                                       947
5792     CONTINUE                                                         947
C    *                                                                    948
         ICOO=0                                                           948
            GO TO 5813                                                    948
5811        ICOO=ICOO+1                                                   948
5813        IF(ICOO-(HOLZ_NUMBEROFROWS - 1).GT.0) GO TO 5812              948
            X = XHOLES(ICOO)                                              948
C       // CM                                                             949
            DX = TPCG_HEIGTUBE*CM/2                                       950
            DZ = TPCG_WIDTUBE*CM/2                                        951
            DY = COOL_TUBELENGTH( ICOO+1 )*INCH / 2                       952
C       Check INOUT==KINNER                                               953
            IF (INOUT.EQ.KINNER) THEN                                     953
            DY2 = DY + 2*DYDX*DX                                          954
            DY1 = DY                                                      954
            ELSE                                                          956
            DY2 = DY                                                      957
            DY1 = DY - 2*DYDX*DX                                          958
            ENDIF                                                         959
            ISHAPE = KTRD1                                                959
            MYPAR(1)=DY1                                                  961
            MYPAR(2)=DY2                                                  961
            MYPAR(3)=DZ                                                   961
            MYPAR(4)=DX                                                   961
            MYPAR(11) = X-DX-TFEE_ASSEMBLYTHICKNESS/2+DRSECTORSHIFT       962
            MYPAR(13) = ZWHEEL1+DZ                                        963
C       Create TCOO :                                                     964
C       Call To Local Procedure TCOO                                      964
            ASSIGN 5820 TO BLOCK_TCOO                                     964
            GO TO  5620                                                   964
5820        CONTINUE                                                      964
C       *                                                                 964
C       Geant POSITION : TCOO ===> X=MYPAR(11) Z=MYPAR(13) ORT=YZX <===   965
            CALL AGSRESET                                                 965
            AG_X = MYPAR(11)                                              965
            AG_Z = MYPAR(13)                                              965
            AG_ALPHAN=AG_ALPHAN+1                                         965
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              965
            AG_TITLE = 'TCOO'                                             965
            AG_EXNAME = 'POSITION'                                        965
            AG_PARLIST = 'X_Z_ORT'                                        965
            CALL AXPOSITION                                               965
C       *                                                                 965
C       *                                                                 965
            JCOO=0                                                        966
               GO TO 5833                                                 966
5831           JCOO=JCOO+1                                                966
5833           IF(JCOO-(HOLZ_NUMBERPERROW(ICOO+1) - 1).GT.0) GO TO 5832   966
               Y = HOLZ_YHOLES( 9*ICOO + JCOO + 1 )*INCH                  967
C          Create FEEA :                                                  968
C          Call To Local Procedure FEEA                                   968
               ASSIGN 5850 TO BLOCK_FEEA                                  968
               GO TO  5840                                                968
5850           CONTINUE                                                   968
C          *                                                              968
C          Geant POSITION : FEEA ===> X=X+TFEE_ASSEMBLYTHICKNESS/2+DRSEC  970
C          TORSHIFT Y=Y Z=ZWHEEL1+2*DZ+TFEE_RIBDZ KONLY='MANY' <===       970
               CALL AGSRESET                                              970
               AG_X = X+TFEE_ASSEMBLYTHICKNESS/2+DRSECTORSHIFT            970
               AG_Y = Y                                                   970
               AG_Z = ZWHEEL1+2*DZ+TFEE_RIBDZ                             970
               AG_KONLY = 'MANY'                                          970
               AG_TITLE = 'FEEA'                                          970
               AG_EXNAME = 'POSITION'                                     970
               AG_PARLIST = 'X_Y_Z_KONLY'                                 970
               CALL AXPOSITION                                            970
C          *                                                              970
            GO TO 5831                                                    971
5832        CONTINUE                                                      971
         GO TO 5811                                                       972
5812     CONTINUE                                                         972
5421  CALL AGSPOPB                                                        973
      GO TO BLOCK_TWAS, (5420 ,5430)                                      973
C End of Local Procedure                                                  973
C ----------------------------------------------------------------------  974
C ------------ FEEA                                                       974
C Begin Local Procedure FEEA                                              975
C FEEA TGEOVOLUMEASSEMBLY(FEE)                                            975
5840  CONTINUE                                                            975
         AG_TITLE='FEEA'                                                  975
         AG_MODULE=MODULE                                                 975
         AG_MARK='V00'                                                    975
         CALL AGSPUSH                                                     975
         AG_MODULE=MODULE                                                 975
C    *                                                                    975
C    Geant ATTRIBUTE : FEEA ===> SEEN=1 COLO=KGREEN <===                  976
         CALL AGSRESET                                                    976
         AG_SEEN = 1                                                      976
         AG_COLO = KGREEN                                                 976
         AG_TITLE = 'FEEA'                                                976
         AG_EXNAME = 'ATTRIBUTE'                                          976
         AG_PARLIST = 'SEEN_COLO'                                         976
         CALL AXATTRIBUTE                                                 976
C    *                                                                    976
C    Geant SHAPE : BOX ===> DX=TFEE_ASS(1) DY=TFEE_ASS(2), DZ=TFEE_ASS(3  976
C    ) <===                                                               976
         CALL AGSRESET                                                    978
         AG_DX = TFEE_ASS(1)                                              978
         AG_DY = TFEE_ASS(2)                                              978
         AG_DZ = TFEE_ASS(3)                                              978
         AG_TITLE = 'BOX'                                                 978
         AG_EXNAME = 'SHAPE'                                              978
         AG_PARLIST = 'DX_DY_DZ'                                          978
         CALL AXSHAPE                                                     978
C    *                                                                    978
C    Geant ATTRIBUTE : FEEA ===> SEEN=1 COLO=42 <===                      978
         CALL AGSRESET                                                    979
         AG_SEEN = 1                                                      979
         AG_COLO = 42                                                     979
         AG_TITLE = 'FEEA'                                                979
         AG_EXNAME = 'ATTRIBUTE'                                          979
         AG_PARLIST = 'SEEN_COLO'                                         979
         CALL AXATTRIBUTE                                                 979
C    *                                                                    979
C    Create FEEP :                                                        980
C    Call To Local Procedure FEEP                                         980
         ASSIGN 5870 TO BLOCK_FEEP                                        980
         GO TO  5860                                                      980
5870     CONTINUE                                                         980
C    *                                                                    980
C    Geant POSITION : FEEP ===> inherited arguments used <===             981
         CALL AGSRESET                                                    981
         AG_TITLE = 'FEEP'                                                981
         AG_EXNAME = 'POSITION'                                           981
         AG_PARLIST = 'NONE'                                              981
         CALL AXPOSITION                                                  981
C    *                                                                    981
C    Create FEER :                                                        982
C    Call To Local Procedure FEER                                         982
         ASSIGN 5890 TO BLOCK_FEER                                        982
         GO TO  5880                                                      982
5890     CONTINUE                                                         982
C    *                                                                    982
C    Geant POSITION : FEER ===> X=-TFEE_RIBDX-TFEE_CARDDX <===            983
         CALL AGSRESET                                                    983
         AG_X = -TFEE_RIBDX-TFEE_CARDDX                                   983
         AG_TITLE = 'FEER'                                                983
         AG_EXNAME = 'POSITION'                                           983
         AG_PARLIST = 'X'                                                 983
         CALL AXPOSITION                                                  983
C    *                                                                    983
C    Create FEEI :                                                        984
C    Call To Local Procedure FEEI                                         984
         ASSIGN 5910 TO BLOCK_FEEI                                        984
         GO TO  5900                                                      984
5910     CONTINUE                                                         984
C    *                                                                    984
C    Geant POSITION : FEEI ===> X=2*TFEE_CARDDX Z=0.5 <===                985
         CALL AGSRESET                                                    985
         AG_X = 2*TFEE_CARDDX                                             985
         AG_Z = 0.5                                                       985
         AG_TITLE = 'FEEI'                                                985
         AG_EXNAME = 'POSITION'                                           985
         AG_PARLIST = 'X_Z'                                               985
         CALL AXPOSITION                                                  985
C    *                                                                    985
5841  CALL AGSPOPB                                                        986
      GO TO BLOCK_FEEA, (5840 ,5850)                                      986
C End of Local Procedure                                                  986
C ----------------------------------------------------------------------  987
C ------------ FEEP                                                       987
C Begin Local Procedure FEEP                                              988
C FEEP FEEPLATE                                                           988
5860  CONTINUE                                                            988
         AG_TITLE='FEEP'                                                  988
         AG_MODULE=MODULE                                                 988
         AG_MARK='V00'                                                    988
         CALL AGSPUSH                                                     988
         AG_MODULE=MODULE                                                 988
C    *                                                                    988
C    Geant ATTRIBUTE : FEEP ===> SEEN=1 COLO=KRED <===                    989
         CALL AGSRESET                                                    989
         AG_SEEN = 1                                                      989
         AG_COLO = KRED                                                   989
         AG_TITLE = 'FEEP'                                                989
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
C    Geant SHAPE : BOX ===> DX=TFEE_PLATEDX DY=TFEE_PLATEDY, DZ=TFEE_PLA  990
C    TEDZ <===                                                            990
         CALL AGSRESET                                                    992
         AG_DX = TFEE_PLATEDX                                             992
         AG_DY = TFEE_PLATEDY                                             992
         AG_DZ = TFEE_PLATEDZ                                             992
         AG_TITLE = 'BOX'                                                 992
         AG_EXNAME = 'SHAPE'                                              992
         AG_PARLIST = 'DX_DY_DZ'                                          992
         CALL AXSHAPE                                                     992
C    *                                                                    992
5861  CALL AGSPOPB                                                        993
      GO TO BLOCK_FEEP, (5860 ,5870)                                      993
C End of Local Procedure                                                  993
C ----------------------------------------------------------------------  994
C ------------ FEER                                                       994
C Begin Local Procedure FEER                                              995
C FEER FEERIB                                                             995
5880  CONTINUE                                                            995
         AG_TITLE='FEER'                                                  995
         AG_MODULE=MODULE                                                 995
         AG_MARK='V00'                                                    995
         CALL AGSPUSH                                                     995
         AG_MODULE=MODULE                                                 995
C    *                                                                    995
C    Geant ATTRIBUTE : FEER ===> SEEN=1 COLO=1 <===                       996
         CALL AGSRESET                                                    996
         AG_SEEN = 1                                                      996
         AG_COLO = 1                                                      996
         AG_TITLE = 'FEER'                                                996
         AG_EXNAME = 'ATTRIBUTE'                                          996
         AG_PARLIST = 'SEEN_COLO'                                         996
         CALL AXATTRIBUTE                                                 996
C    *                                                                    996
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        996
         CALL AGSRESET                                                    997
         AG_TITLE = 'ALUMINIUM'                                           997
         AG_EXNAME = 'MATERIAL'                                           997
         AG_PARLIST = 'NONE'                                              997
         CALL AXMATERIAL                                                  997
C    *                                                                    997
C    Geant SHAPE : BOX ===> DX=TFEE_RIBDX DY=TFEE_RIBDY DZ=TFEE_RIBDZ <=  997
C    ==                                                                   997
         CALL AGSRESET                                                    998
         AG_DX = TFEE_RIBDX                                               998
         AG_DY = TFEE_RIBDY                                               998
         AG_DZ = TFEE_RIBDZ                                               998
         AG_TITLE = 'BOX'                                                 998
         AG_EXNAME = 'SHAPE'                                              998
         AG_PARLIST = 'DX_DY_DZ'                                          998
         CALL AXSHAPE                                                     998
C    *                                                                    998
5881  CALL AGSPOPB                                                        999
      GO TO BLOCK_FEER, (5880 ,5890)                                      999
C End of Local Procedure                                                  999
C ----------------------------------------------------------------------  1000
C ------------ FEEI                                                       1000
C Begin Local Procedure FEEI                                              1001
C FEEI FEEITSELF                                                          1001
5900  CONTINUE                                                            1001
         AG_TITLE='FEEI'                                                  1001
         AG_MODULE=MODULE                                                 1001
         AG_MARK='V00'                                                    1001
         CALL AGSPUSH                                                     1001
         AG_MODULE=MODULE                                                 1001
C    *                                                                    1001
C    Geant ATTRIBUTE : FEEI ===> SEEN=1 COLO=KVIOLET <===                 1002
         CALL AGSRESET                                                    1002
         AG_SEEN = 1                                                      1002
         AG_COLO = KVIOLET                                                1002
         AG_TITLE = 'FEEI'                                                1002
         AG_EXNAME = 'ATTRIBUTE'                                          1002
         AG_PARLIST = 'SEEN_COLO'                                         1002
         CALL AXATTRIBUTE                                                 1002
C    *                                                                    1002
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1002
         CALL AGSRESET                                                    1003
         AG_TITLE = 'G10'                                                 1003
         AG_EXNAME = 'MATERIAL'                                           1003
         AG_PARLIST = 'NONE'                                              1003
         CALL AXMATERIAL                                                  1003
C    *                                                                    1003
C    Geant SHAPE : BOX ===> DX=TFEE_CARDDX DY=TFEE_CARDDY, DZ=TFEE_CARDD  1003
C    Z <===                                                               1003
         CALL AGSRESET                                                    1005
         AG_DX = TFEE_CARDDX                                              1005
         AG_DY = TFEE_CARDDY                                              1005
         AG_DZ = TFEE_CARDDZ                                              1005
         AG_TITLE = 'BOX'                                                 1005
         AG_EXNAME = 'SHAPE'                                              1005
         AG_PARLIST = 'DX_DY_DZ'                                          1005
         CALL AXSHAPE                                                     1005
C    *                                                                    1005
5901  CALL AGSPOPB                                                        1006
      GO TO BLOCK_FEEI, (5900 ,5910)                                      1006
C End of Local Procedure                                                  1006
C ----------------------------------------------------------------------  1007
C ------------ TALS                                                       1007
C Begin Local Procedure TALS                                              1008
C TALS TPCSECTORALSUPPORT THE SECTOR G10 ALLIMINIUM SUPPORT               1008
5670  CONTINUE                                                            1008
         AG_TITLE='TALS'                                                  1008
         AG_MODULE=MODULE                                                 1008
         AG_MARK='V00'                                                    1008
         CALL AGSPUSH                                                     1008
         AG_MODULE=MODULE                                                 1008
C    *                                                                    1008
C    Geant ATTRIBUTE : TALS ===> SEEN=1 COLO=KYELLOW <===                 1009
         CALL AGSRESET                                                    1009
         AG_SEEN = 1                                                      1009
         AG_COLO = KYELLOW                                                1009
         AG_TITLE = 'TALS'                                                1009
         AG_EXNAME = 'ATTRIBUTE'                                          1009
         AG_PARLIST = 'SEEN_COLO'                                         1009
         CALL AXATTRIBUTE                                                 1009
C    *                                                                    1009
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1009
         CALL AGSRESET                                                    1010
         AG_TITLE = 'ALUMINIUM'                                           1010
         AG_EXNAME = 'MATERIAL'                                           1010
         AG_PARLIST = 'NONE'                                              1010
         CALL AXMATERIAL                                                  1010
C    *                                                                    1010
C    Geant SHAPE : PGON ===> PHI1=-ALPHA DPHI=2*ALPHA NPDIV=1, NZ=2 ZI={  1010
C    Z1+TPCG_PADPLANETHICKNESS,Z1+TPCG_PADPLANETHICKNESS+TECW_THICKAL},   1010
C    RMN={R1,R1}, RMX={R2,R2} <===                                        1010
         CALL AGSRESET                                                    1014
         AG_PHI1 = -ALPHA                                                 1014
         AG_DPHI = 2*ALPHA                                                1014
         AG_NPDIV = 1                                                     1014
         AG_NZ = 2                                                        1014
         AG_ZI(1)=Z1+TPCG_PADPLANETHICKNESS                               1014
         AG_ZI(2)=Z1+TPCG_PADPLANETHICKNESS+TECW_THICKAL                  1014
         AG_RMN(1)=R1                                                     1014
         AG_RMN(2)=R1                                                     1014
         AG_RMX(1)=R2                                                     1014
         AG_RMX(2)=R2                                                     1014
         AG_TITLE = 'PGON'                                                1014
         AG_EXNAME = 'SHAPE'                                              1014
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1014
         CALL AXSHAPE                                                     1014
C    *                                                                    1014
         MYPAR(1)= TECW_HOLEDX                                            1014
         MYPAR(2)= TECW_HOLEDY                                            1016
         MYPAR(3)= TECW_THICKAL/2                                         1017
         MYPAR(13)= ZWHEEL1 - TECW_THICK + TPCG_PADPLANETHICKNESS +       1018
     *   TECW_THICKAL/2                                                   1018
C    *                                                                    1019
         ITALS=0                                                          1019
            GO TO 5923                                                    1019
5921        ITALS=ITALS+1                                                 1019
5923        IF(ITALS-(HOLZ_NUMBEROFROWS - 1).GT.0) GO TO 5922             1019
            MYPAR(11) = XHOLES(ITALS)                                     1019
C       // CM                                                             1020
C       *                                                                 1021
            JTALS=0                                                       1021
               GO TO 5933                                                 1021
5931           JTALS=JTALS+1                                              1021
5933           IF(JTALS-(HOLZ_NUMBERPERROW( ITALS+1 ) - 1).GT.0) GO TO    1021
     *         5932                                                       1021
               MYPAR(12) = HOLZ_YHOLES( 9*ITALS+JTALS+1) *INCH            1022
C          Create THOL :                                                  1023
C          Call To Local Procedure THOL                                   1023
               ASSIGN 5950 TO BLOCK_THOL                                  1023
               GO TO  5940                                                1023
5950           CONTINUE                                                   1023
C          *                                                              1023
C          Geant POSITION : THOL ===> X=MYPAR(11) Y=MYPAR(12) Z=MYPAR(13  1024
C          ) <===                                                         1024
               CALL AGSRESET                                              1024
               AG_X = MYPAR(11)                                           1024
               AG_Y = MYPAR(12)                                           1024
               AG_Z = MYPAR(13)                                           1024
               AG_TITLE = 'THOL'                                          1024
               AG_EXNAME = 'POSITION'                                     1024
               AG_PARLIST = 'X_Y_Z'                                       1024
               CALL AXPOSITION                                            1024
C          *                                                              1024
            GO TO 5931                                                    1025
5932        CONTINUE                                                      1025
         GO TO 5921                                                       1026
5922     CONTINUE                                                         1026
5671  CALL AGSPOPB                                                        1027
      GO TO BLOCK_TALS, (5670 ,5680)                                      1027
C End of Local Procedure                                                  1027
C ----------------------------------------------------------------------  1028
C ------------ TSGT                                                       1028
C Begin Local Procedure TSGT                                              1029
C TSGT TPCSECTORG10 THE SECTOR G10                                        1029
5690  CONTINUE                                                            1029
         AG_TITLE='TSGT'                                                  1029
         AG_MODULE=MODULE                                                 1029
         AG_MARK='V00'                                                    1029
         CALL AGSPUSH                                                     1029
         AG_MODULE=MODULE                                                 1029
C    *                                                                    1029
C    Geant ATTRIBUTE : TSGT ===> SEEN=1 COLO=KBLUE <===                   1030
         CALL AGSRESET                                                    1030
         AG_SEEN = 1                                                      1030
         AG_COLO = KBLUE                                                  1030
         AG_TITLE = 'TSGT'                                                1030
         AG_EXNAME = 'ATTRIBUTE'                                          1030
         AG_PARLIST = 'SEEN_COLO'                                         1030
         CALL AXATTRIBUTE                                                 1030
C    *                                                                    1030
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1030
         CALL AGSRESET                                                    1031
         AG_TITLE = 'G10'                                                 1031
         AG_EXNAME = 'MATERIAL'                                           1031
         AG_PARLIST = 'NONE'                                              1031
         CALL AXMATERIAL                                                  1031
C    *                                                                    1031
C    Geant SHAPE : PGON ===> PHI1=-ALPHA DPHI=2*ALPHA NPDIV=1, NZ=2 ZI={  1031
C    Z1,Z1+TPCG_PADPLANETHICKNESS} RMN={R1, R1} RMX={R2,R2} <===          1031
         CALL AGSRESET                                                    1034
         AG_PHI1 = -ALPHA                                                 1034
         AG_DPHI = 2*ALPHA                                                1034
         AG_NPDIV = 1                                                     1034
         AG_NZ = 2                                                        1034
         AG_ZI(1)=Z1                                                      1034
         AG_ZI(2)=Z1+TPCG_PADPLANETHICKNESS                               1034
         AG_RMN(1)=R1                                                     1034
         AG_RMN(2)=R1                                                     1034
         AG_RMX(1)=R2                                                     1034
         AG_RMX(2)=R2                                                     1034
         AG_TITLE = 'PGON'                                                1034
         AG_EXNAME = 'SHAPE'                                              1034
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1034
         CALL AXSHAPE                                                     1034
C    *                                                                    1034
5691  CALL AGSPOPB                                                        1035
      GO TO BLOCK_TSGT, (5690 ,5700)                                      1035
C End of Local Procedure                                                  1035
C ----------------------------------------------------------------------  1036
C ------------ TWBT                                                       1036
C Begin Local Procedure TWBT                                              1037
C TWBT //WHEELBOTTOM                                                      1037
5510  CONTINUE                                                            1037
         AG_TITLE='TWBT'                                                  1037
         AG_MODULE=MODULE                                                 1037
         AG_MARK='V00'                                                    1037
         CALL AGSPUSH                                                     1037
         AG_MODULE=MODULE                                                 1037
C    *                                                                    1037
C    Geant ATTRIBUTE : TWBT ===> SEEN=1 COLO=KGREEN <===                  1038
         CALL AGSRESET                                                    1038
         AG_SEEN = 1                                                      1038
         AG_COLO = KGREEN                                                 1038
         AG_TITLE = 'TWBT'                                                1038
         AG_EXNAME = 'ATTRIBUTE'                                          1038
         AG_PARLIST = 'SEEN_COLO'                                         1038
         CALL AXATTRIBUTE                                                 1038
C    *                                                                    1038
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1038
         CALL AGSRESET                                                    1039
         AG_TITLE = 'ALUMINIUM'                                           1039
         AG_EXNAME = 'MATERIAL'                                           1039
         AG_PARLIST = 'NONE'                                              1039
         CALL AXMATERIAL                                                  1039
C    *                                                                    1039
C    Geant SHAPE : PCON ===> PHI1=-15 DPHI=30 NZ=4 ZI={ZWHEEL1, ZWHEEL1+  1039
C    3.500*INCH,ZWHEEL1+3.500*INCH,TPCG_LENGTHW/2}, RMN={TPCG_WHEELIR,TP  1039
C    CG_WHEELIR, TPCG_WHEELIR+3.6,TPCG_WHEELIR+3.6} RMX={TPCG_WHEELR0, T  1039
C    PCG_WHEELR0,TPCG_WHEELR0,TPCG_WHEELR0} <===                          1039
         CALL AGSRESET                                                    1044
         AG_PHI1 = -15                                                    1044
         AG_DPHI = 30                                                     1044
         AG_NZ = 4                                                        1044
         AG_ZI(1)=ZWHEEL1                                                 1044
         AG_ZI(2)=ZWHEEL1+3.500*INCH                                      1044
         AG_ZI(3)=ZWHEEL1+3.500*INCH                                      1044
         AG_ZI(4)=TPCG_LENGTHW/2                                          1044
         AG_RMN(1)=TPCG_WHEELIR                                           1044
         AG_RMN(2)=TPCG_WHEELIR                                           1044
         AG_RMN(3)=TPCG_WHEELIR+3.6                                       1044
         AG_RMN(4)=TPCG_WHEELIR+3.6                                       1044
         AG_RMX(1)=TPCG_WHEELR0                                           1044
         AG_RMX(2)=TPCG_WHEELR0                                           1044
         AG_RMX(3)=TPCG_WHEELR0                                           1044
         AG_RMX(4)=TPCG_WHEELR0                                           1044
         AG_TITLE = 'PCON'                                                1044
         AG_EXNAME = 'SHAPE'                                              1044
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1044
         CALL AXSHAPE                                                     1044
C    *                                                                    1044
5511  CALL AGSPOPB                                                        1045
      GO TO BLOCK_TWBT, (5510 ,5520)                                      1045
C End of Local Procedure                                                  1045
C ----------------------------------------------------------------------  1046
C ------------ TWRI                                                       1046
C Begin Local Procedure TWRI                                              1047
C TWRI WHEELOUTERRING                                                     1047
5530  CONTINUE                                                            1047
         AG_TITLE='TWRI'                                                  1047
         AG_MODULE=MODULE                                                 1047
         AG_MARK='V00'                                                    1047
         CALL AGSPUSH                                                     1047
         AG_MODULE=MODULE                                                 1047
C    *                                                                    1047
C    Geant SHAPE : PCON ===> PHI1=-15 DPHI=30 NZ=4 ZI={ZWHEEL1, ZTOFCEND  1052
C    ,ZTOFCEND,TPCG_LENGTHW/2} RMN={TPCG_WHEELR2*COS15, TPCG_WHEELR2*COS  1052
C    15,TPCG_WHEELR2*COS15, TPCG_WHEELR2*COS15} RMX={TPCG_WHEELOR1, TPCG  1052
C    _WHEELOR1,TPCG_WHEELOR,TPCG_WHEELOR} <===                            1052
         CALL AGSRESET                                                    1052
         AG_PHI1 = -15                                                    1052
         AG_DPHI = 30                                                     1052
         AG_NZ = 4                                                        1052
         AG_ZI(1)=ZWHEEL1                                                 1052
         AG_ZI(2)=ZTOFCEND                                                1052
         AG_ZI(3)=ZTOFCEND                                                1052
         AG_ZI(4)=TPCG_LENGTHW/2                                          1052
         AG_RMN(1)=TPCG_WHEELR2*COS15                                     1052
         AG_RMN(2)=TPCG_WHEELR2*COS15                                     1052
         AG_RMN(3)=TPCG_WHEELR2*COS15                                     1052
         AG_RMN(4)=TPCG_WHEELR2*COS15                                     1052
         AG_RMX(1)=TPCG_WHEELOR1                                          1052
         AG_RMX(2)=TPCG_WHEELOR1                                          1052
         AG_RMX(3)=TPCG_WHEELOR                                           1052
         AG_RMX(4)=TPCG_WHEELOR                                           1052
         AG_TITLE = 'PCON'                                                1052
         AG_EXNAME = 'SHAPE'                                              1052
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1052
         CALL AXSHAPE                                                     1052
C    *                                                                    1052
C    Geant MEDIUM : TWRI_SOMETHING ===> DEEMAX=0.248853 STMIN=1.15055 <=  1052
C    ==                                                                   1052
         CALL AGSRESET                                                    1053
         AG_DEEMAX = 0.248853                                             1053
         AG_STMIN = 1.15055                                               1053
         AG_TITLE = 'TWRI_SOMETHING'                                      1053
         AG_EXNAME = 'MEDIUM'                                             1053
         AG_PARLIST = 'DEEMAX_STMIN'                                      1053
         CALL AXMEDIUM                                                    1053
C    *                                                                    1053
C    Geant ATTRIBUTE : TWRI ===> SEEN=0 COLO=KGREEN <===                  1053
         CALL AGSRESET                                                    1054
         AG_SEEN = 0                                                      1054
         AG_COLO = KGREEN                                                 1054
         AG_TITLE = 'TWRI'                                                1054
         AG_EXNAME = 'ATTRIBUTE'                                          1054
         AG_PARLIST = 'SEEN_COLO'                                         1054
         CALL AXATTRIBUTE                                                 1054
C    *                                                                    1054
C    Create TWRC :                                                        1055
C    Call To Local Procedure TWRC                                         1055
         ASSIGN 5970 TO BLOCK_TWRC                                        1055
         GO TO  5960                                                      1055
5970     CONTINUE                                                         1055
C    *                                                                    1055
C    Geant POSITION : TWRC ===> inherited arguments used <===             1056
         CALL AGSRESET                                                    1056
         AG_TITLE = 'TWRC'                                                1056
         AG_EXNAME = 'POSITION'                                           1056
         AG_PARLIST = 'NONE'                                              1056
         CALL AXPOSITION                                                  1056
C    *                                                                    1056
         MYCOS = COS15                                                    1056
         MYRMIN=TPCG_WHEELR2                                              1056
         MYRMAX = TPCG_WHEELOR1                                           1057
         MYG1 = MYRMIN*COS15                                              1058
         NCOUNT = 0                                                       1059
C    *                                                                    1060
C       Loop here                                                         1060
5981        IF(MYG2.GE.MYRMIN) GO TO 5982                                 1060
            NCOUNT=NCOUNT+1                                               1060
            MYG2 = MYRMIN                                                 1062
C       Check (MYG2>MYRMAX*MYCOS)                                         1063
            IF ((MYG2.GT.MYRMAX*MYCOS)) THEN                              1063
            MYG2 = MYRMAX*MYCOS                                           1064
            ENDIF                                                         1065
            ALPHA = ACOS(MYCOS)*RADDEG                                    1065
C       Create TWRG :                                                     1067
C       Call To Local Procedure TWRG                                      1067
            ASSIGN 6000 TO BLOCK_TWRG                                     1067
            GO TO  5990                                                   1067
6000        CONTINUE                                                      1067
C       *                                                                 1067
C       Geant POSITION : TWRG ===> inherited arguments used <===          1068
            CALL AGSRESET                                                 1068
            AG_TITLE = 'TWRG'                                             1068
            AG_EXNAME = 'POSITION'                                        1068
            AG_PARLIST = 'NONE'                                           1068
            CALL AXPOSITION                                               1068
C       *                                                                 1068
            MYCOS = MYG2/MYRMIN                                           1068
            MYG1=MYG2                                                     1069
C       Check (NCOUNT>10)                                                 1070
            IF ((NCOUNT.GT.10)) THEN                                      1070
            GO TO 5982                                                    1071
            ENDIF                                                         1072
         GO TO 5981                                                       1073
5982     CONTINUE                                                         1073
5531  CALL AGSPOPB                                                        1074
      GO TO BLOCK_TWRI, (5530 ,5540)                                      1074
C End of Local Procedure                                                  1074
C ----------------------------------------------------------------------  1075
C ------------ TWRG                                                       1075
C Begin Local Procedure TWRG                                              1076
C TWRG WHEELOUTERRING PGON PART                                           1076
5990  CONTINUE                                                            1076
         AG_TITLE='TWRG'                                                  1076
         AG_MODULE=MODULE                                                 1076
         AG_MARK='V00'                                                    1076
         CALL AGSPUSH                                                     1076
         AG_MODULE=MODULE                                                 1076
C    *                                                                    1076
C    Geant ATTRIBUTE : TWRG ===> SEEN=1 COLO=KGREEN <===                  1077
         CALL AGSRESET                                                    1077
         AG_SEEN = 1                                                      1077
         AG_COLO = KGREEN                                                 1077
         AG_TITLE = 'TWRG'                                                1077
         AG_EXNAME = 'ATTRIBUTE'                                          1077
         AG_PARLIST = 'SEEN_COLO'                                         1077
         CALL AXATTRIBUTE                                                 1077
C    *                                                                    1077
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1077
         CALL AGSRESET                                                    1078
         AG_TITLE = 'ALUMINIUM'                                           1078
         AG_EXNAME = 'MATERIAL'                                           1078
         AG_PARLIST = 'NONE'                                              1078
         CALL AXMATERIAL                                                  1078
C    *                                                                    1078
C    Geant SHAPE : PGON ===> PHI1=-ALPHA DPHI=ALPHA*2 NPDIV=1, NZ=2 ZI={  1078
C    ZWHEEL1,TPCG_LENGTHW/2} RMN={MYG1, MYG1} RMX={MYG2,MYG2} <===        1078
         CALL AGSRESET                                                    1081
         AG_PHI1 = -ALPHA                                                 1081
         AG_DPHI = ALPHA*2                                                1081
         AG_NPDIV = 1                                                     1081
         AG_NZ = 2                                                        1081
         AG_ZI(1)=ZWHEEL1                                                 1081
         AG_ZI(2)=TPCG_LENGTHW/2                                          1081
         AG_RMN(1)=MYG1                                                   1081
         AG_RMN(2)=MYG1                                                   1081
         AG_RMX(1)=MYG2                                                   1081
         AG_RMX(2)=MYG2                                                   1081
         AG_TITLE = 'PGON'                                                1081
         AG_EXNAME = 'SHAPE'                                              1081
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1081
         CALL AXSHAPE                                                     1081
C    *                                                                    1081
5991  CALL AGSPOPB                                                        1082
      GO TO BLOCK_TWRG, (5990 ,6000)                                      1082
C End of Local Procedure                                                  1082
C ----------------------------------------------------------------------  1083
C ------------ TWRC                                                       1083
C Begin Local Procedure TWRC                                              1084
C TWRC WHEELOUTERRING PCON PART                                           1084
5960  CONTINUE                                                            1084
         AG_TITLE='TWRC'                                                  1084
         AG_MODULE=MODULE                                                 1084
         AG_MARK='V00'                                                    1084
         CALL AGSPUSH                                                     1084
         AG_MODULE=MODULE                                                 1084
C    *                                                                    1084
C    Geant ATTRIBUTE : TWRC ===> SEEN=1 COLO=KGREEN <===                  1085
         CALL AGSRESET                                                    1085
         AG_SEEN = 1                                                      1085
         AG_COLO = KGREEN                                                 1085
         AG_TITLE = 'TWRC'                                                1085
         AG_EXNAME = 'ATTRIBUTE'                                          1085
         AG_PARLIST = 'SEEN_COLO'                                         1085
         CALL AXATTRIBUTE                                                 1085
C    *                                                                    1085
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1085
         CALL AGSRESET                                                    1086
         AG_TITLE = 'ALUMINIUM'                                           1086
         AG_EXNAME = 'MATERIAL'                                           1086
         AG_PARLIST = 'NONE'                                              1086
         CALL AXMATERIAL                                                  1086
C    *                                                                    1086
C    Geant SHAPE : PCON ===> PHI1=-15 DPHI=30 NZ=4 ZI={ZWHEEL1, ZTOFCEND  1086
C    ,ZTOFCEND,TPCG_LENGTHW/2} RMN={TPCG_WHEELR2/COS15, TPCG_WHEELR2/COS  1086
C    15,TPCG_WHEELR2/COS15, TPCG_WHEELR2/COS15} RMX={TPCG_WHEELOR1, TPCG  1086
C    _WHEELOR1,TPCG_WHEELOR,TPCG_WHEELOR} <===                            1086
         CALL AGSRESET                                                    1091
         AG_PHI1 = -15                                                    1091
         AG_DPHI = 30                                                     1091
         AG_NZ = 4                                                        1091
         AG_ZI(1)=ZWHEEL1                                                 1091
         AG_ZI(2)=ZTOFCEND                                                1091
         AG_ZI(3)=ZTOFCEND                                                1091
         AG_ZI(4)=TPCG_LENGTHW/2                                          1091
         AG_RMN(1)=TPCG_WHEELR2/COS15                                     1091
         AG_RMN(2)=TPCG_WHEELR2/COS15                                     1091
         AG_RMN(3)=TPCG_WHEELR2/COS15                                     1091
         AG_RMN(4)=TPCG_WHEELR2/COS15                                     1091
         AG_RMX(1)=TPCG_WHEELOR1                                          1091
         AG_RMX(2)=TPCG_WHEELOR1                                          1091
         AG_RMX(3)=TPCG_WHEELOR                                           1091
         AG_RMX(4)=TPCG_WHEELOR                                           1091
         AG_TITLE = 'PCON'                                                1091
         AG_EXNAME = 'SHAPE'                                              1091
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           1091
         CALL AXSHAPE                                                     1091
C    *                                                                    1091
5961  CALL AGSPOPB                                                        1092
      GO TO BLOCK_TWRC, (5960 ,5970)                                      1092
C End of Local Procedure                                                  1092
C ----------------------------------------------------------------------  1093
C ------------ TWTR                                                       1093
C Begin Local Procedure TWTR                                              1094
C TWTR TPCWHEELTOPRIB                                                     1094
5550  CONTINUE                                                            1094
         AG_TITLE='TWTR'                                                  1094
         AG_MODULE=MODULE                                                 1094
         AG_MARK='V00'                                                    1094
         CALL AGSPUSH                                                     1094
         AG_MODULE=MODULE                                                 1094
C    *                                                                    1094
C    Geant ATTRIBUTE : TWTR ===> SEEN=1 COLO=KBLUE <===                   1095
         CALL AGSRESET                                                    1095
         AG_SEEN = 1                                                      1095
         AG_COLO = KBLUE                                                  1095
         AG_TITLE = 'TWTR'                                                1095
         AG_EXNAME = 'ATTRIBUTE'                                          1095
         AG_PARLIST = 'SEEN_COLO'                                         1095
         CALL AXATTRIBUTE                                                 1095
C    *                                                                    1095
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1095
         CALL AGSRESET                                                    1096
         AG_TITLE = 'ALUMINIUM'                                           1096
         AG_EXNAME = 'MATERIAL'                                           1096
         AG_PARLIST = 'NONE'                                              1096
         CALL AXMATERIAL                                                  1096
C    *                                                                    1096
         MYG1=TPCG_WHEELR2 - (TPCG_WHEELTOTALRIBWIDTH -                   1096
     *   TPCG_WHEELRIBWIDTH)/2                                            1096
C    Geant SHAPE : PGON ===> PHI1=-15 DPHI=30 NPDIV=1 NZ=2, ZI={ZWHEEL1,  1100
C    ZWHEEL1+TPCG_WHEELRIBHEIGHT} RMN={MYG1, MYG1} RMX={TPCG_WHEELR2,TPC  1100
C    G_WHEELR2} <===                                                      1100
         CALL AGSRESET                                                    1100
         AG_PHI1 = -15                                                    1100
         AG_DPHI = 30                                                     1100
         AG_NPDIV = 1                                                     1100
         AG_NZ = 2                                                        1100
         AG_ZI(1)=ZWHEEL1                                                 1100
         AG_ZI(2)=ZWHEEL1+TPCG_WHEELRIBHEIGHT                             1100
         AG_RMN(1)=MYG1                                                   1100
         AG_RMN(2)=MYG1                                                   1100
         AG_RMX(1)=TPCG_WHEELR2                                           1100
         AG_RMX(2)=TPCG_WHEELR2                                           1100
         AG_TITLE = 'PGON'                                                1100
         AG_EXNAME = 'SHAPE'                                              1100
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1100
         CALL AXSHAPE                                                     1100
C    *                                                                    1100
5551  CALL AGSPOPB                                                        1101
      GO TO BLOCK_TWTR, (5550 ,5560)                                      1101
C End of Local Procedure                                                  1101
C ----------------------------------------------------------------------  1102
C ------------ TWMR                                                       1102
C Begin Local Procedure TWMR                                              1103
C TWMR TPCWHEELMIDDLERIB                                                  1103
5570  CONTINUE                                                            1103
         AG_TITLE='TWMR'                                                  1103
         AG_MODULE=MODULE                                                 1103
         AG_MARK='V00'                                                    1103
         CALL AGSPUSH                                                     1103
         AG_MODULE=MODULE                                                 1103
C    *                                                                    1103
C    Geant ATTRIBUTE : TWMR ===> SEEN=1 COLO=KYELLOW <===                 1104
         CALL AGSRESET                                                    1104
         AG_SEEN = 1                                                      1104
         AG_COLO = KYELLOW                                                1104
         AG_TITLE = 'TWMR'                                                1104
         AG_EXNAME = 'ATTRIBUTE'                                          1104
         AG_PARLIST = 'SEEN_COLO'                                         1104
         CALL AXATTRIBUTE                                                 1104
C    *                                                                    1104
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1104
         CALL AGSRESET                                                    1105
         AG_TITLE = 'ALUMINIUM'                                           1105
         AG_EXNAME = 'MATERIAL'                                           1105
         AG_PARLIST = 'NONE'                                              1105
         CALL AXMATERIAL                                                  1105
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
C    Geant SHAPE : PGON ===> PHI1=-15 DPHI=30 NPDIV=1 NZ=2, ZI={ZZZRDO(1  1120
C    ),ZZZRDO(2)} RMN={RMNRDO(1),RMNRDO(2)}, RMX={RMXRDO(1),RMXRDO(2)} <  1120
C    ===                                                                  1120
         CALL AGSRESET                                                    1123
         AG_PHI1 = -15                                                    1123
         AG_DPHI = 30                                                     1123
         AG_NPDIV = 1                                                     1123
         AG_NZ = 2                                                        1123
         AG_ZI(1)=ZZZRDO(1)                                               1123
         AG_ZI(2)=ZZZRDO(2)                                               1123
         AG_RMN(1)=RMNRDO(1)                                              1123
         AG_RMN(2)=RMNRDO(2)                                              1123
         AG_RMX(1)=RMXRDO(1)                                              1123
         AG_RMX(2)=RMXRDO(2)                                              1123
         AG_TITLE = 'PGON'                                                1123
         AG_EXNAME = 'SHAPE'                                              1123
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     1123
         CALL AXSHAPE                                                     1123
C    *                                                                    1123
         ISHAPE = KPARA                                                   1123
         MYPAR(1)=TPCG_RDOCOOLINGDY                                       1125
         MYPAR(2)=TPCG_RDOCOOLINGDX                                       1125
         MYPAR(3)=TPCG_RDOCOOLINGDZ                                       1125
         MYPAR(4)=15                                                      1125
         X = TPCG_WHEELR2 - TPCG_RDOCOOLINGDX                             1126
         Y = X*TAN15 - TPCG_RDOCOOLINGDY                                  1127
         Z = TPCG_ZRDO - 2*DY                                             1128
C    Create TCOO :                                                        1129
C    Call To Local Procedure TCOO                                         1129
         ASSIGN 6010 TO BLOCK_TCOO                                        1129
         GO TO  5620                                                      1129
6010     CONTINUE                                                         1129
C    *                                                                    1129
C    Geant POSITION : TCOO ===> X=X Y=Y Z=Z ORT=YX-Z <===                 1130
         CALL AGSRESET                                                    1130
         AG_X = X                                                         1130
         AG_Y = Y                                                         1130
         AG_Z = Z                                                         1130
         AG_ALPHAN=AG_ALPHAN+1                                            1130
         AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                                1130
         AG_TITLE = 'TCOO'                                                1130
         AG_EXNAME = 'POSITION'                                           1130
         AG_PARLIST = 'X_Y_Z_ORT'                                         1130
         CALL AXPOSITION                                                  1130
C    *                                                                    1130
         ISHAPE = KPARA                                                   1130
         MYPAR(1)=TPCG_RDOCOOLINGDY                                       1132
         MYPAR(2)=TPCG_RDOCOOLINGDX                                       1132
         MYPAR(3)=TPCG_RDOCOOLINGDZ                                       1132
         MYPAR(4)=-15                                                     1132
         X = TPCG_WHEELR2 - TPCG_RDOCOOLINGDX                             1133
         Y = X*TAN15 - TPCG_RDOCOOLINGDY                                  1134
         Z = TPCG_ZRDO - 2*DY                                             1135
C    Create TCOO :                                                        1136
C    Call To Local Procedure TCOO                                         1136
         ASSIGN 6020 TO BLOCK_TCOO                                        1136
         GO TO  5620                                                      1136
6020     CONTINUE                                                         1136
C    *                                                                    1136
C    Geant POSITION : TCOO ===> X=X Y=-Y Z=Z ORT=YX-Z <===                1137
         CALL AGSRESET                                                    1137
         AG_X = X                                                         1137
         AG_Y = -Y                                                        1137
         AG_Z = Z                                                         1137
         AG_ALPHAN=AG_ALPHAN+1                                            1137
         AG_ALPHAK(AG_ALPHAN)=IGOR('YX-Z')                                1137
         AG_TITLE = 'TCOO'                                                1137
         AG_EXNAME = 'POSITION'                                           1137
         AG_PARLIST = 'X_Y_Z_ORT'                                         1137
         CALL AXPOSITION                                                  1137
C    *                                                                    1137
         RCOOLINGTUBE = TPCG_WHEELR2 - TPCG_HEIGTUBE - 2*TPCG_DXRDO       1137
C    *                                                                    1139
         ICOO=0                                                           1139
            GO TO 6033                                                    1139
6031        ICOO=ICOO+1                                                   1139
6033        IF(ICOO-(7).GT.0) GO TO 6032                                  1139
            DZ = TPCG_HEIGTUBE*CM/2                                       1140
            DY = TPCG_WIDTUBE*CM/2                                        1141
            Z = RCOOLINGTUBE                                              1142
            DX1 = (Z-DZ)*TAN15 - 2*TPCG_RDOCOOLINGDY                      1143
            DX2 = (Z+DZ)*TAN15 - 2*TPCG_RDOCOOLINGDY                      1144
            ISHAPE = KTRD1                                                1144
            MYPAR(1)=DX1                                                  1146
            MYPAR(2)=DX2                                                  1146
            MYPAR(3)=DY                                                   1146
            MYPAR(4)=DZ                                                   1146
            MYPAR(11) = Z                                                 1146
            MYPAR(13) = TPCG_ZRDO - 2*DY                                  1147
C       Create TCOO :                                                     1148
C       Call To Local Procedure TCOO                                      1148
            ASSIGN 6040 TO BLOCK_TCOO                                     1148
            GO TO  5620                                                   1148
6040        CONTINUE                                                      1148
C       *                                                                 1148
C       Geant POSITION : TCOO ===> X=MYPAR(11) Y=0 Z=MYPAR(13) ORT=YZX <  1149
C       ===                                                               1149
            CALL AGSRESET                                                 1149
            AG_X = MYPAR(11)                                              1149
            AG_Y = 0                                                      1149
            AG_Z = MYPAR(13)                                              1149
            AG_ALPHAN=AG_ALPHAN+1                                         1149
            AG_ALPHAK(AG_ALPHAN)=IGOR('YZX')                              1149
            AG_TITLE = 'TCOO'                                             1149
            AG_EXNAME = 'POSITION'                                        1149
            AG_PARLIST = 'X_Y_Z_ORT'                                      1149
            CALL AXPOSITION                                               1149
C       *                                                                 1149
C       Check (ICOO .NE. 0 .AND. ICOO .NE. 6)                             1149
            IF ((ICOO .NE. 0 .AND. ICOO .NE. 6)) THEN                     1150
            MYPAR(11) = Z + DZ + TPCG_DXRDO                               1150
            MYPAR(13) = TPCG_ZRDO-TPCG_DZRDO                              1152
C       Create TRDC :                                                     1153
C       Call To Local Procedure TRDC                                      1153
            ASSIGN 6060 TO BLOCK_TRDC                                     1153
            GO TO  6050                                                   1153
6060        CONTINUE                                                      1153
C       *                                                                 1153
C       Geant POSITION : TRDC ===> X=MYPAR(11) Y=0 Z=MYPAR(13) <===       1154
            CALL AGSRESET                                                 1154
            AG_X = MYPAR(11)                                              1154
            AG_Y = 0                                                      1154
            AG_Z = MYPAR(13)                                              1154
            AG_TITLE = 'TRDC'                                             1154
            AG_EXNAME = 'POSITION'                                        1154
            AG_PARLIST = 'X_Y_Z'                                          1154
            CALL AXPOSITION                                               1154
C       *                                                                 1154
            ENDIF                                                         1154
            RCOOLINGTUBE=RCOOLINGTUBE-DRDOCOOLING(ICOO)                   1156
         GO TO 6031                                                       1157
6032     CONTINUE                                                         1157
5591  CALL AGSPOPB                                                        1158
      GO TO BLOCK_TRDO, (5590 ,5600)                                      1158
C End of Local Procedure                                                  1158
C ----------------------------------------------------------------------  1159
C ------------ TBRW                                                       1159
C Begin Local Procedure TBRW                                              1160
C TBRW WHEELRIBBOX                                                        1160
5440  CONTINUE                                                            1160
         AG_TITLE='TBRW'                                                  1160
         AG_MODULE=MODULE                                                 1160
         AG_MARK='V00'                                                    1160
         CALL AGSPUSH                                                     1160
         AG_MODULE=MODULE                                                 1160
C    *                                                                    1160
C    Geant ATTRIBUTE : TBRW ===> SEEN=1 COLO=KVIOLET <===                 1161
         CALL AGSRESET                                                    1161
         AG_SEEN = 1                                                      1161
         AG_COLO = KVIOLET                                                1161
         AG_TITLE = 'TBRW'                                                1161
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
C    Geant SHAPE : BOX ===> DX=TPCG_WHEELBOXDY/2 DY=TPCG_WHEELBOXDX/2, D  1162
C    Z=TPCG_WHEELTHK/2 <===                                               1162
         CALL AGSRESET                                                    1164
         AG_DX = TPCG_WHEELBOXDY/2                                        1164
         AG_DY = TPCG_WHEELBOXDX/2                                        1164
         AG_DZ = TPCG_WHEELTHK/2                                          1164
         AG_TITLE = 'BOX'                                                 1164
         AG_EXNAME = 'SHAPE'                                              1164
         AG_PARLIST = 'DX_DY_DZ'                                          1164
         CALL AXSHAPE                                                     1164
C    *                                                                    1164
5441  CALL AGSPOPB                                                        1165
      GO TO BLOCK_TBRW, (5440 ,5450)                                      1165
C End of Local Procedure                                                  1165
C ----------------------------------------------------------------------  1166
C ------------ TWRB                                                       1166
C Begin Local Procedure TWRB                                              1167
C TWRB TPCWHEELRIB                                                        1167
5460  CONTINUE                                                            1167
         AG_TITLE='TWRB'                                                  1167
         AG_MODULE=MODULE                                                 1167
         AG_MARK='V00'                                                    1167
         CALL AGSPUSH                                                     1167
         AG_MODULE=MODULE                                                 1167
C    *                                                                    1167
C    Geant ATTRIBUTE : TWRB ===> SEEN=1 COLO=KYELLOW <===                 1168
         CALL AGSRESET                                                    1168
         AG_SEEN = 1                                                      1168
         AG_COLO = KYELLOW                                                1168
         AG_TITLE = 'TWRB'                                                1168
         AG_EXNAME = 'ATTRIBUTE'                                          1168
         AG_PARLIST = 'SEEN_COLO'                                         1168
         CALL AXATTRIBUTE                                                 1168
C    *                                                                    1168
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1168
         CALL AGSRESET                                                    1169
         AG_TITLE = 'ALUMINIUM'                                           1169
         AG_EXNAME = 'MATERIAL'                                           1169
         AG_PARLIST = 'NONE'                                              1169
         CALL AXMATERIAL                                                  1169
C    *                                                                    1169
C    Geant SHAPE : PARA ===> DX=MYDX DY=MYDY DZ=MYDZ ALPH=MYALPH, THET=0  1169
C     PHI=0 <===                                                          1169
         CALL AGSRESET                                                    1171
         AG_DX = MYDX                                                     1171
         AG_DY = MYDY                                                     1171
         AG_DZ = MYDZ                                                     1171
         AG_ALPH = MYALPH                                                 1171
         AG_THET = 0                                                      1171
         AG_PHI = 0                                                       1171
         AG_TITLE = 'PARA'                                                1171
         AG_EXNAME = 'SHAPE'                                              1171
         AG_PARLIST = 'DX_DY_DZ_ALPH_THET_PHI'                            1171
         CALL AXSHAPE                                                     1171
C    *                                                                    1171
5461  CALL AGSPOPB                                                        1172
      GO TO BLOCK_TWRB, (5460 ,5470,5480)                                 1172
C End of Local Procedure                                                  1172
C ----------------------------------------------------------------------  1173
C ------------ TCOO                                                       1173
C Begin Local Procedure TCOO                                              1174
C TCOO COOLINGTUBE                                                        1174
5620  CONTINUE                                                            1174
         AG_TITLE='TCOO'                                                  1174
         AG_MODULE=MODULE                                                 1174
         AG_MARK='V00'                                                    1174
         CALL AGSPUSH                                                     1174
         AG_MODULE=MODULE                                                 1174
C    *                                                                    1174
C    Geant ATTRIBUTE : TCOO ===> SEEN=1 COLO=KRED <===                    1175
         CALL AGSRESET                                                    1175
         AG_SEEN = 1                                                      1175
         AG_COLO = KRED                                                   1175
         AG_TITLE = 'TCOO'                                                1175
         AG_EXNAME = 'ATTRIBUTE'                                          1175
         AG_PARLIST = 'SEEN_COLO'                                         1175
         CALL AXATTRIBUTE                                                 1175
C    *                                                                    1175
C    Geant MATERIAL : WATER_PIPE ===> inherited arguments used <===       1175
         CALL AGSRESET                                                    1176
         AG_TITLE = 'WATER_PIPE'                                          1176
         AG_EXNAME = 'MATERIAL'                                           1176
         AG_PARLIST = 'NONE'                                              1176
         CALL AXMATERIAL                                                  1176
C    *                                                                    1176
C    Check ISHAPE==KPARA                                                  1176
         IF (ISHAPE.EQ.KPARA) THEN                                        1177
C    Geant SHAPE : PARA ===> DX=MYPAR(1) DY=MYPAR(2) DZ=MYPAR(3), ALPH=M  1179
C    YPAR(4) THET=0 PHI=0 <===                                            1179
         CALL AGSRESET                                                    1179
         AG_DX = MYPAR(1)                                                 1179
         AG_DY = MYPAR(2)                                                 1179
         AG_DZ = MYPAR(3)                                                 1179
         AG_ALPH = MYPAR(4)                                               1179
         AG_THET = 0                                                      1179
         AG_PHI = 0                                                       1179
         AG_TITLE = 'PARA'                                                1179
         AG_EXNAME = 'SHAPE'                                              1179
         AG_PARLIST = 'DX_DY_DZ_ALPH_THET_PHI'                            1179
         CALL AXSHAPE                                                     1179
C    *                                                                    1179
         ENDIF                                                            1179
C    Check ISHAPE==KTRD1                                                  1181
         IF (ISHAPE.EQ.KTRD1) THEN                                        1181
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
         ENDIF                                                            1183
5621  CALL AGSPOPB                                                        1185
      GO TO BLOCK_TCOO, (5620 ,5630,5800,5820,6010,6020,6040)             1185
C End of Local Procedure                                                  1185
C ----------------------------------------------------------------------  1186
C ------------ TCAB                                                       1186
C Begin Local Procedure TCAB                                              1187
C TCAB CABLES                                                             1187
5650  CONTINUE                                                            1187
         AG_TITLE='TCAB'                                                  1187
         AG_MODULE=MODULE                                                 1187
         AG_MARK='V00'                                                    1187
         CALL AGSPUSH                                                     1187
         AG_MODULE=MODULE                                                 1187
C    *                                                                    1187
C    Geant ATTRIBUTE : TCAB ===> SEEN=1 COLO=KGREEN <===                  1188
         CALL AGSRESET                                                    1188
         AG_SEEN = 1                                                      1188
         AG_COLO = KGREEN                                                 1188
         AG_TITLE = 'TCAB'                                                1188
         AG_EXNAME = 'ATTRIBUTE'                                          1188
         AG_PARLIST = 'SEEN_COLO'                                         1188
         CALL AXATTRIBUTE                                                 1188
C    *                                                                    1188
C    Geant MATERIAL : CABLES ===> inherited arguments used <===           1188
         CALL AGSRESET                                                    1189
         AG_TITLE = 'CABLES'                                              1189
         AG_EXNAME = 'MATERIAL'                                           1189
         AG_PARLIST = 'NONE'                                              1189
         CALL AXMATERIAL                                                  1189
C    *                                                                    1189
C    Geant SHAPE : PARA ===> DX=MYPAR(1) DY=MYPAR(2) DZ=MYPAR(3), ALPH=M  1189
C    YPAR(4) THET=0 PHI=0 <===                                            1189
         CALL AGSRESET                                                    1191
         AG_DX = MYPAR(1)                                                 1191
         AG_DY = MYPAR(2)                                                 1191
         AG_DZ = MYPAR(3)                                                 1191
         AG_ALPH = MYPAR(4)                                               1191
         AG_THET = 0                                                      1191
         AG_PHI = 0                                                       1191
         AG_TITLE = 'PARA'                                                1191
         AG_EXNAME = 'SHAPE'                                              1191
         AG_PARLIST = 'DX_DY_DZ_ALPH_THET_PHI'                            1191
         CALL AXSHAPE                                                     1191
C    *                                                                    1191
5651  CALL AGSPOPB                                                        1192
      GO TO BLOCK_TCAB, (5650 ,5660)                                      1192
C End of Local Procedure                                                  1192
C ----------------------------------------------------------------------  1193
C ------------ TRDC                                                       1193
C Begin Local Procedure TRDC                                              1194
C TRDC RDOCARD                                                            1194
6050  CONTINUE                                                            1194
         AG_TITLE='TRDC'                                                  1194
         AG_MODULE=MODULE                                                 1194
         AG_MARK='V00'                                                    1194
         CALL AGSPUSH                                                     1194
         AG_MODULE=MODULE                                                 1194
C    *                                                                    1194
C    Geant ATTRIBUTE : TRDC ===> SEEN=1 COLO=KBLUE <===                   1195
         CALL AGSRESET                                                    1195
         AG_SEEN = 1                                                      1195
         AG_COLO = KBLUE                                                  1195
         AG_TITLE = 'TRDC'                                                1195
         AG_EXNAME = 'ATTRIBUTE'                                          1195
         AG_PARLIST = 'SEEN_COLO'                                         1195
         CALL AXATTRIBUTE                                                 1195
C    *                                                                    1195
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1195
         CALL AGSRESET                                                    1196
         AG_TITLE = 'G10'                                                 1196
         AG_EXNAME = 'MATERIAL'                                           1196
         AG_PARLIST = 'NONE'                                              1196
         CALL AXMATERIAL                                                  1196
C    *                                                                    1196
C    Geant SHAPE : BOX ===> DX=TPCG_DXRDO DY=TPCG_DYRDO DZ=TPCG_DZRDO <=  1196
C    ==                                                                   1196
         CALL AGSRESET                                                    1197
         AG_DX = TPCG_DXRDO                                               1197
         AG_DY = TPCG_DYRDO                                               1197
         AG_DZ = TPCG_DZRDO                                               1197
         AG_TITLE = 'BOX'                                                 1197
         AG_EXNAME = 'SHAPE'                                              1197
         AG_PARLIST = 'DX_DY_DZ'                                          1197
         CALL AXSHAPE                                                     1197
C    *                                                                    1197
6051  CALL AGSPOPB                                                        1198
      GO TO BLOCK_TRDC, (6050 ,6060)                                      1198
C End of Local Procedure                                                  1198
C ----------------------------------------------------------------------  1199
C ------------ TRIB                                                       1199
C Begin Local Procedure TRIB                                              1200
C TRIB TPC RIBS                                                           1200
5720  CONTINUE                                                            1200
         AG_TITLE='TRIB'                                                  1200
         AG_MODULE=MODULE                                                 1200
         AG_MARK='V00'                                                    1200
         CALL AGSPUSH                                                     1200
         AG_MODULE=MODULE                                                 1200
C    *                                                                    1200
C    Geant ATTRIBUTE : TRIB ===> SEEN=1 COLO=KRED <===                    1201
         CALL AGSRESET                                                    1201
         AG_SEEN = 1                                                      1201
         AG_COLO = KRED                                                   1201
         AG_TITLE = 'TRIB'                                                1201
         AG_EXNAME = 'ATTRIBUTE'                                          1201
         AG_PARLIST = 'SEEN_COLO'                                         1201
         CALL AXATTRIBUTE                                                 1201
C    *                                                                    1201
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        1201
         CALL AGSRESET                                                    1202
         AG_TITLE = 'ALUMINIUM'                                           1202
         AG_EXNAME = 'MATERIAL'                                           1202
         AG_PARLIST = 'NONE'                                              1202
         CALL AXMATERIAL                                                  1202
C    *                                                                    1202
C    Check ISHAPE==KPARA                                                  1202
         IF (ISHAPE.EQ.KPARA) THEN                                        1203
C    Geant SHAPE : PARA ===> DX=MYPAR(1) DY=MYPAR(2) DZ=MYPAR(3), ALPH=M  1205
C    YPAR(4) THET=0 PHI=0 <===                                            1205
         CALL AGSRESET                                                    1205
         AG_DX = MYPAR(1)                                                 1205
         AG_DY = MYPAR(2)                                                 1205
         AG_DZ = MYPAR(3)                                                 1205
         AG_ALPH = MYPAR(4)                                               1205
         AG_THET = 0                                                      1205
         AG_PHI = 0                                                       1205
         AG_TITLE = 'PARA'                                                1205
         AG_EXNAME = 'SHAPE'                                              1205
         AG_PARLIST = 'DX_DY_DZ_ALPH_THET_PHI'                            1205
         CALL AXSHAPE                                                     1205
C    *                                                                    1205
         ELSEIF (ISHAPE.EQ.KTRD1) THEN                                    1205
C    Geant SHAPE : TRD1 ===> DX1=MYPAR(1) DX2=MYPAR(2) DY=MYPAR(3), DZ=M  1208
C    YPAR(4) <===                                                         1208
         CALL AGSRESET                                                    1208
         AG_DX1 = MYPAR(1)                                                1208
         AG_DX2 = MYPAR(2)                                                1208
         AG_DY = MYPAR(3)                                                 1208
         AG_DZ = MYPAR(4)                                                 1208
         AG_TITLE = 'TRD1'                                                1208
         AG_EXNAME = 'SHAPE'                                              1208
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     1208
         CALL AXSHAPE                                                     1208
C    *                                                                    1208
         ELSEIF (ISHAPE.EQ.KBBOX) THEN                                    1208
C    Geant SHAPE : BOX ===> DX=MYPAR(1) DY=MYPAR(2) DZ=MYPAR(3) <===      1210
         CALL AGSRESET                                                    1210
         AG_DX = MYPAR(1)                                                 1210
         AG_DY = MYPAR(2)                                                 1210
         AG_DZ = MYPAR(3)                                                 1210
         AG_TITLE = 'BOX'                                                 1210
         AG_EXNAME = 'SHAPE'                                              1210
         AG_PARLIST = 'DX_DY_DZ'                                          1210
         CALL AXSHAPE                                                     1210
C    *                                                                    1210
         ENDIF                                                            1210
5721  CALL AGSPOPB                                                        1212
      GO TO BLOCK_TRIB, (5720 ,5730,5770,5780)                            1212
C End of Local Procedure                                                  1212
C ----------------------------------------------------------------------  1213
C ------------ TWIR                                                       1213
C Begin Local Procedure TWIR                                              1214
C TWIR WIREMOUNT                                                          1214
5740  CONTINUE                                                            1214
         AG_TITLE='TWIR'                                                  1214
         AG_MODULE=MODULE                                                 1214
         AG_MARK='V00'                                                    1214
         CALL AGSPUSH                                                     1214
         AG_MODULE=MODULE                                                 1214
C    *                                                                    1214
C    Geant ATTRIBUTE : TWIR ===> SEEN=1 COLO=KYELLOW <===                 1215
         CALL AGSRESET                                                    1215
         AG_SEEN = 1                                                      1215
         AG_COLO = KYELLOW                                                1215
         AG_TITLE = 'TWIR'                                                1215
         AG_EXNAME = 'ATTRIBUTE'                                          1215
         AG_PARLIST = 'SEEN_COLO'                                         1215
         CALL AXATTRIBUTE                                                 1215
C    *                                                                    1215
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1215
         CALL AGSRESET                                                    1216
         AG_TITLE = 'G10'                                                 1216
         AG_EXNAME = 'MATERIAL'                                           1216
         AG_PARLIST = 'NONE'                                              1216
         CALL AXMATERIAL                                                  1216
C    *                                                                    1216
C    Geant SHAPE : PARA ===> DX=MYPAR(1) DY=MYPAR(2) DZ=MYPAR(3), ALPH=M  1216
C    YPAR(4) THET=0 PHI=0 <===                                            1216
         CALL AGSRESET                                                    1218
         AG_DX = MYPAR(1)                                                 1218
         AG_DY = MYPAR(2)                                                 1218
         AG_DZ = MYPAR(3)                                                 1218
         AG_ALPH = MYPAR(4)                                               1218
         AG_THET = 0                                                      1218
         AG_PHI = 0                                                       1218
         AG_TITLE = 'PARA'                                                1218
         AG_EXNAME = 'SHAPE'                                              1218
         AG_PARLIST = 'DX_DY_DZ_ALPH_THET_PHI'                            1218
         CALL AXSHAPE                                                     1218
C    *                                                                    1218
5741  CALL AGSPOPB                                                        1219
      GO TO BLOCK_TWIR, (5740 ,5750)                                      1219
C End of Local Procedure                                                  1219
C ----------------------------------------------------------------------  1220
C ------------ THOL                                                       1220
C Begin Local Procedure THOL                                              1221
C THOL THE SECTOR HOLES FOR FEE                                           1221
5940  CONTINUE                                                            1221
         AG_TITLE='THOL'                                                  1221
         AG_MODULE=MODULE                                                 1221
         AG_MARK='V00'                                                    1221
         CALL AGSPUSH                                                     1221
         AG_MODULE=MODULE                                                 1221
C    *                                                                    1221
C    Geant ATTRIBUTE : THOL ===> SEEN=1 COLO=KGREEN <===                  1222
         CALL AGSRESET                                                    1222
         AG_SEEN = 1                                                      1222
         AG_COLO = KGREEN                                                 1222
         AG_TITLE = 'THOL'                                                1222
         AG_EXNAME = 'ATTRIBUTE'                                          1222
         AG_PARLIST = 'SEEN_COLO'                                         1222
         CALL AXATTRIBUTE                                                 1222
C    *                                                                    1222
C    Geant MATERIAL : AIR ===> inherited arguments used <===              1222
         CALL AGSRESET                                                    1223
         AG_TITLE = 'AIR'                                                 1223
         AG_EXNAME = 'MATERIAL'                                           1223
         AG_PARLIST = 'NONE'                                              1223
         CALL AXMATERIAL                                                  1223
C    *                                                                    1223
C    Geant SHAPE : BOX ===> DX=MYPAR(1) DY=MYPAR(2) DZ=MYPAR(3) <===      1223
         CALL AGSRESET                                                    1224
         AG_DX = MYPAR(1)                                                 1224
         AG_DY = MYPAR(2)                                                 1224
         AG_DZ = MYPAR(3)                                                 1224
         AG_TITLE = 'BOX'                                                 1224
         AG_EXNAME = 'SHAPE'                                              1224
         AG_PARLIST = 'DX_DY_DZ'                                          1224
         CALL AXSHAPE                                                     1224
C    *                                                                    1224
C    Create FEES :                                                        1225
C    Call To Local Procedure FEES                                         1225
         ASSIGN 6080 TO BLOCK_FEES                                        1225
         GO TO  6070                                                      1225
6080     CONTINUE                                                         1225
C    *                                                                    1225
C    Geant POSITION : FEES ===> inherited arguments used <===             1226
         CALL AGSRESET                                                    1226
         AG_TITLE = 'FEES'                                                1226
         AG_EXNAME = 'POSITION'                                           1226
         AG_PARLIST = 'NONE'                                              1226
         CALL AXPOSITION                                                  1226
C    *                                                                    1226
5941  CALL AGSPOPB                                                        1227
      GO TO BLOCK_THOL, (5940 ,5950)                                      1227
C End of Local Procedure                                                  1227
C ----------------------------------------------------------------------  1228
C ------------ FEES                                                       1228
C Begin Local Procedure FEES                                              1229
C FEES TFEESOCKET                                                         1229
6070  CONTINUE                                                            1229
         AG_TITLE='FEES'                                                  1229
         AG_MODULE=MODULE                                                 1229
         AG_MARK='V00'                                                    1229
         CALL AGSPUSH                                                     1229
         AG_MODULE=MODULE                                                 1229
C    *                                                                    1229
C    Geant MATERIAL : G10 ===> inherited arguments used <===              1230
         CALL AGSRESET                                                    1230
         AG_TITLE = 'G10'                                                 1230
         AG_EXNAME = 'MATERIAL'                                           1230
         AG_PARLIST = 'NONE'                                              1230
         CALL AXMATERIAL                                                  1230
C    *                                                                    1230
C    Geant ATTRIBUTE : FEES ===> SEEN=1 COLO=KBLUE <===                   1230
         CALL AGSRESET                                                    1231
         AG_SEEN = 1                                                      1231
         AG_COLO = KBLUE                                                  1231
         AG_TITLE = 'FEES'                                                1231
         AG_EXNAME = 'ATTRIBUTE'                                          1231
         AG_PARLIST = 'SEEN_COLO'                                         1231
         CALL AXATTRIBUTE                                                 1231
C    *                                                                    1231
C    Geant SHAPE : BOX ===> DX=6.2/20 DY=57./20 DZ=5.8/20 <===            1231
         CALL AGSRESET                                                    1232
         AG_DX = 6.2/20                                                   1232
         AG_DY = 57./20                                                   1232
         AG_DZ = 5.8/20                                                   1232
         AG_TITLE = 'BOX'                                                 1232
         AG_EXNAME = 'SHAPE'                                              1232
         AG_PARLIST = 'DX_DY_DZ'                                          1232
         CALL AXSHAPE                                                     1232
C    *                                                                    1232
6071  CALL AGSPOPB                                                        1233
      GO TO BLOCK_FEES, (6070 ,6080)                                      1233
C End of Local Procedure                                                  1233
C ----------------------------------------------------------------------  1234
C ------------ TPGV                                                       1234
C Begin Local Procedure TPGV                                              1235
C TPGV IS THE GAS VOLUME PLACED IN TPC                                    1235
5050  CONTINUE                                                            1235
         AG_TITLE='TPGV'                                                  1235
         AG_MODULE=MODULE                                                 1235
         AG_MARK='V00'                                                    1235
         CALL AGSPUSH                                                     1235
         AG_MODULE=MODULE                                                 1235
C    *                                                                    1235
C    Geant ATTRIBUTE : TPGV ===> SEEN=1 COLO=KRED <===                    1236
         CALL AGSRESET                                                    1236
         AG_SEEN = 1                                                      1236
         AG_COLO = KRED                                                   1236
         AG_TITLE = 'TPGV'                                                1236
         AG_EXNAME = 'ATTRIBUTE'                                          1236
         AG_PARLIST = 'SEEN_COLO'                                         1236
         CALL AXATTRIBUTE                                                 1236
C    *                                                                    1236
C    Geant MATERIAL : P10 ===> inherited arguments used <===              1236
         CALL AGSRESET                                                    1237
         AG_TITLE = 'P10'                                                 1237
         AG_EXNAME = 'MATERIAL'                                           1237
         AG_PARLIST = 'NONE'                                              1237
         CALL AXMATERIAL                                                  1237
C    *                                                                    1237
C    Geant SHAPE : TUBE ===> RMIN=TPGVIR RMAX=TPCG_SENGASOR, DZ=TPGVLENG  1237
C    /2 <===                                                              1237
         CALL AGSRESET                                                    1239
         AG_RMIN = TPGVIR                                                 1239
         AG_RMAX = TPCG_SENGASOR                                          1239
         AG_DZ = TPGVLENG/2                                               1239
         AG_TITLE = 'TUBE'                                                1239
         AG_EXNAME = 'SHAPE'                                              1239
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      1239
         CALL AXSHAPE                                                     1239
C    *                                                                    1239
C    *                                                                    1239
         ISECANG=15                                                       1240
            GOTO 6093                                                     1240
6091        ISECANG=ISECANG+(30)                                          1240
6093        IF((30)*(ISECANG-(360-15)).GT.0) GO TO 6092                   1240
C       Create TPSS :                                                     1241
C       Call To Local Procedure TPSS                                      1241
            ASSIGN 6110 TO BLOCK_TPSS                                     1241
            GO TO  6100                                                   1241
6110        CONTINUE                                                      1241
C       *                                                                 1241
C       Geant POSITION : TPSS ===> ALPHAZ=ISECANG KONLY='MANY' <===       1242
            CALL AGSRESET                                                 1242
            AG_ALPHAN=AG_ALPHAN+1                                         1242
            AG_ALPHAK(AG_ALPHAN)=3                                        1242
            AG_ALPHAL(AG_ALPHAN)=ISECANG                                  1242
            AG_KONLY = 'MANY'                                             1242
            AG_TITLE = 'TPSS'                                             1242
            AG_EXNAME = 'POSITION'                                        1242
            AG_PARLIST = 'ALPHAZ_KONLY'                                   1242
            CALL AXPOSITION                                               1242
C       *                                                                 1242
         GO TO 6091                                                       1243
6092     CONTINUE                                                         1243
5051  CALL AGSPOPB                                                        1244
      GO TO BLOCK_TPGV, (5050 ,5060)                                      1244
C End of Local Procedure                                                  1244
C ----------------------------------------------------------------------  1245
C ------------ TPSS                                                       1245
C Begin Local Procedure TPSS                                              1246
C TPSS IS A DIVISION OF GAS VOLUME CORRESPONDING TO A SUPERSECTORS        1246
6100  CONTINUE                                                            1246
         AG_TITLE='TPSS'                                                  1246
         AG_MODULE=MODULE                                                 1246
         AG_MARK='V00'                                                    1246
         CALL AGSPUSH                                                     1246
         AG_MODULE=MODULE                                                 1246
C    *                                                                    1246
C    Geant ATTRIBUTE : TPSS ===> SEEN=1 COLO=KBLUE <===                   1247
         CALL AGSRESET                                                    1247
         AG_SEEN = 1                                                      1247
         AG_COLO = KBLUE                                                  1247
         AG_TITLE = 'TPSS'                                                1247
         AG_EXNAME = 'ATTRIBUTE'                                          1247
         AG_PARLIST = 'SEEN_COLO'                                         1247
         CALL AXATTRIBUTE                                                 1247
C    *                                                                    1247
C    Geant SHAPE : TUBS ===> PHI1=-15 PHI2=15 <===                        1247
         CALL AGSRESET                                                    1248
         AG_PHI1 = -15                                                    1248
         AG_PHI2 = 15                                                     1248
         AG_TITLE = 'TUBS'                                                1248
         AG_EXNAME = 'SHAPE'                                              1248
         AG_PARLIST = 'PHI1_PHI2'                                         1248
         CALL AXSHAPE                                                     1248
C    *                                                                    1248
C    *                                                                    1248
         KASE=1                                                           1249
            GO TO 6123                                                    1249
6121        KASE=KASE+1                                                   1249
6123        IF(KASE-(2).GT.0) GO TO 6122                                  1249
C       *                                                                 1250
            I_SEC=1                                                       1250
               GO TO 6133                                                 1250
6131           I_SEC=I_SEC+1                                              1250
6133           IF(I_SEC-(2).GT.0) GO TO 6132                              1250
C          use TPRS bank                                                  1251
               BPATH_TPRS='TPRS*'                                         1251
               IDNUM_TPRS(1)=0                                            1251
               TPRS_SEC=I_SEC                                             1251
               CALL AGDGETP('TPCXGEO2','*',-999,'TPRS',NTPRS_SEC,         1251
     *         RE_TPRS_SEC,BPATH_TPRS,IDNUM_TPRS,1,MAP_TPRS,NAMES_TPRS,   1251
     *         BANK_TPRS,LEN_TPRS,LLEN_TPRS,LINK_TPRS,FLAG_TPRS)          1251
C          use TECW bank                                                  1252
               BPATH_TECW='TECW*'                                         1252
               IDNUM_TECW(1)=0                                            1252
               TECW_SEC=I_SEC                                             1252
               CALL AGDGETP('TPCXGEO2','*',-999,'TECW',NTECW_SEC,         1252
     *         RE_TECW_SEC,BPATH_TECW,IDNUM_TECW,1,MAP_TECW,NAMES_TECW,   1252
     *         BANK_TECW,LEN_TECW,LLEN_TECW,LINK_TECW,FLAG_TECW)          1252
               ZWHEEL1 = TPCG_LENGTHW/2 - TPCG_WHEELTHK                   1253
               ZBEG = TPCG_MEMBTHK/2                                      1254
               ZEND = TPCG_ZGROUNDGRID + TPRS_DANODE*2                    1255
               ZDED = TPCG_ZGATINGGRID - TPCG_DEADZONE                    1256
               ZPMT = TPCG_ZGATINGGRID                                    1257
               DX = TPRS_WIDTH/2                                          1258
C          Check KASE==1                                                  1259
               IF (KASE.EQ.1) THEN                                        1259
               DZ = (ZDED - ZBEG)/2                                       1259
               Z = (ZDED + ZBEG)/2 - TPGVZ                                1261
               ENDIF                                                      1262
C          Check KASE==2                                                  1263
               IF (KASE.EQ.2) THEN                                        1263
               DZ = (ZEND - ZPMT)/2                                       1263
               Z = (ZEND + ZPMT)/2 - TPGVZ                                1265
               ENDIF                                                      1266
C          *                                                              1267
               I_ROW=1                                                    1267
                  GO TO 6143                                              1267
6141              I_ROW=I_ROW+1                                           1267
6143              IF(I_ROW-(NINT(TPRS_NROW)).GT.0) GO TO 6142             1267
C             Check (NINT(TPRS_SUPER)==3|I_ROW==1)                        1268
                  IF ((NINT(TPRS_SUPER).EQ.3.OR.I_ROW.EQ.1)) THEN         1268
                  DY=TPRS_NPADS(I_ROW)*TPRS_PITCH/2                       1269
                  X=TPRS_RPADS(I_ROW)-TPRS_WIDTH                          1270
C             Create TPAD :                                               1271
C             Call To Local Procedure TPAD                                1271
                  ASSIGN 6160 TO BLOCK_TPAD                               1271
                  GO TO  6150                                             1271
6160              CONTINUE                                                1271
C             *                                                           1271
C             Geant POSITION : TPAD ===> X=X Z=Z DZ=DZ DX=DX DY=DY <===   1272
                  CALL AGSRESET                                           1272
                  AG_X = X                                                1272
                  AG_Z = Z                                                1272
                  AG_DZ = DZ                                              1272
                  AG_DX = DX                                              1272
                  AG_DY = DY                                              1272
                  AG_TITLE = 'TPAD'                                       1272
                  AG_EXNAME = 'POSITION'                                  1272
                  AG_PARLIST = 'X_Z_DZ_DX_DY'                             1272
                  CALL AXPOSITION                                         1272
C             *                                                           1272
                  ENDIF                                                   1272
                  DY=TPRS_NPADS(I_ROW)*TPRS_PITCH/2                       1274
                  X=TPRS_RPADS(I_ROW)                                     1275
C             Create TPAD :                                               1276
C             Call To Local Procedure TPAD                                1276
                  ASSIGN 6170 TO BLOCK_TPAD                               1276
                  GO TO  6150                                             1276
6170              CONTINUE                                                1276
C             *                                                           1276
C             Geant POSITION : TPAD ===> X=X Z=Z DZ=DZ DX=DX DY=DY <===   1277
                  CALL AGSRESET                                           1277
                  AG_X = X                                                1277
                  AG_Z = Z                                                1277
                  AG_DZ = DZ                                              1277
                  AG_DX = DX                                              1277
                  AG_DY = DY                                              1277
                  AG_TITLE = 'TPAD'                                       1277
                  AG_EXNAME = 'POSITION'                                  1277
                  AG_PARLIST = 'X_Z_DZ_DX_DY'                             1277
                  CALL AXPOSITION                                         1277
C             *                                                           1277
C             Check (NINT(TPRS_SUPER)==3|I_ROW==NINT(TPRS_NROW))          1277
                  IF                                                      1278
     *            ((NINT(TPRS_SUPER).EQ.3.OR.I_ROW.EQ.NINT(TPRS_NROW)))   1278
     *            THEN                                                    1278
                  X=TPRS_RPADS(I_ROW)+TPRS_WIDTH                          1279
                  DY=TPRS_NPADS(I_ROW)*TPRS_PITCH/2                       1280
C             Create TPAD :                                               1281
C             Call To Local Procedure TPAD                                1281
                  ASSIGN 6180 TO BLOCK_TPAD                               1281
                  GO TO  6150                                             1281
6180              CONTINUE                                                1281
C             *                                                           1281
C             Geant POSITION : TPAD ===> X=X Z=Z DZ=DZ DX=DX DY=DY <===   1282
                  CALL AGSRESET                                           1282
                  AG_X = X                                                1282
                  AG_Z = Z                                                1282
                  AG_DZ = DZ                                              1282
                  AG_DX = DX                                              1282
                  AG_DY = DY                                              1282
                  AG_TITLE = 'TPAD'                                       1282
                  AG_EXNAME = 'POSITION'                                  1282
                  AG_PARLIST = 'X_Z_DZ_DX_DY'                             1282
                  CALL AXPOSITION                                         1282
C             *                                                           1282
                  ENDIF                                                   1282
               GO TO 6141                                                 1284
6142           CONTINUE                                                   1284
            GO TO 6131                                                    1285
6132        CONTINUE                                                      1285
         GO TO 6121                                                       1286
6122     CONTINUE                                                         1286
6101  CALL AGSPOPB                                                        1287
      GO TO BLOCK_TPSS, (6100 ,6110)                                      1287
C End of Local Procedure                                                  1287
C ----------------------------------------------------------------------  1288
C ------------ TPAD                                                       1288
C Begin Local Procedure TPAD                                              1289
C TPAD IS A REAL PADROW WITH DIMENSIONS DEFINED AT POSITIONING TIME       1289
6150  CONTINUE                                                            1289
         AG_TITLE='TPAD'                                                  1289
         AG_MODULE=MODULE                                                 1289
         AG_MARK='V00'                                                    1289
         CALL AGSPUSH                                                     1289
         AG_MODULE=MODULE                                                 1289
C    *                                                                    1289
C    *                                                                    1290
C       Check TPRS_WIDTH<1.5                                              1290
            IF (TPRS_WIDTH.LT.1.5) THEN                                   1290
C       Geant ATTRIBUTE : TPAD ===> SEEN=1 COLO=2 SERIAL=1 <===           1291
            CALL AGSRESET                                                 1291
            AG_SEEN = 1                                                   1291
            AG_COLO = 2                                                   1291
            AG_SERIAL = 1                                                 1291
            AG_TITLE = 'TPAD'                                             1291
            AG_EXNAME = 'ATTRIBUTE'                                       1291
            AG_PARLIST = 'SEEN_COLO_SERIAL'                               1291
            CALL AXATTRIBUTE                                              1291
C       *                                                                 1291
         END IF                                                           1292
C    *                                                                    1293
C       Check TPRS_WIDTH>1.5                                              1293
            IF (TPRS_WIDTH.GT.1.5) THEN                                   1293
C       Geant ATTRIBUTE : TPAD ===> SEEN=1 COLO=2 SERIAL=2 <===           1294
            CALL AGSRESET                                                 1294
            AG_SEEN = 1                                                   1294
            AG_COLO = 2                                                   1294
            AG_SERIAL = 2                                                 1294
            AG_TITLE = 'TPAD'                                             1294
            AG_EXNAME = 'ATTRIBUTE'                                       1294
            AG_PARLIST = 'SEEN_COLO_SERIAL'                               1294
            CALL AXATTRIBUTE                                              1294
C       *                                                                 1294
         END IF                                                           1295
C    Geant MATERIAL : P10 ===> inherited arguments used <===              1296
         CALL AGSRESET                                                    1296
         AG_TITLE = 'P10'                                                 1296
         AG_EXNAME = 'MATERIAL'                                           1296
         AG_PARLIST = 'NONE'                                              1296
         CALL AXMATERIAL                                                  1296
C    *                                                                    1296
C    Geant MATERIAL : SENSITIVE_GAS ===> ISVOL=1 STEMAX=2.5*TPRS_WIDTH <  1296
C    ===                                                                  1296
         CALL AGSRESET                                                    1297
         AG_ISVOL = 1                                                     1297
         AG_STEMAX = 2.5*TPRS_WIDTH                                       1297
         AG_TITLE = 'SENSITIVE_GAS'                                       1297
         AG_EXNAME = 'MATERIAL'                                           1297
         AG_PARLIST = 'ISVOL_STEMAX'                                      1297
         CALL AXMATERIAL                                                  1297
C    *                                                                    1297
C    Geant MEDIUM : TPAD_P10 ===> STEMAX=2.5*TPRS_WIDTH DEEMAX=0.0017536  1297
C    7 STMIN=0.0249056 ISVOL=1 <===                                       1297
         CALL AGSRESET                                                    1298
         AG_STEMAX = 2.5*TPRS_WIDTH                                       1298
         AG_DEEMAX = 0.00175367                                           1298
         AG_STMIN = 0.0249056                                             1298
         AG_ISVOL = 1                                                     1298
         AG_TITLE = 'TPAD_P10'                                            1298
         AG_EXNAME = 'MEDIUM'                                             1298
         AG_PARLIST = 'STEMAX_DEEMAX_STMIN_ISVOL'                         1298
         CALL AXMEDIUM                                                    1298
C    *                                                                    1298
C    Geant SHAPE : BOX ===> DX=0 DY=0 DZ=0 <===                           1298
         CALL AGSRESET                                                    1299
         AG_DX = 0                                                        1299
         AG_DY = 0                                                        1299
         AG_DZ = 0                                                        1299
         AG_TITLE = 'BOX'                                                 1299
         AG_EXNAME = 'SHAPE'                                              1299
         AG_PARLIST = 'DX_DY_DZ'                                          1299
         CALL AXSHAPE                                                     1299
C    *                                                                    1299
         CALL GSTPAR(AG_IMED, 'CUTGAM',1E-4)                              1299
         CALL GSTPAR(AG_IMED, 'CUTELE',1E-4)                              1301
         CALL GSTPAR(AG_IMED, 'DCUTE',1E-4)                               1302
         CALL GSTPAR(AG_IMED, 'DCUTM',1E-4)                               1303
         CALL GSTPAR(AG_IMED, 'STRA', 1.)                                 1304
C    Defining Hits: Z:0.0005:S Y:0.0005: X:0.0005: CX:10: CY:10: CZ:10:   1306
C    LPTOT:18:(-3,2), SLENG:0.1:(0,800) TOF:16:(0,TPCG_TOFMAX) LGAM:16:(  1306
C    -2,+2) STEP:11:(0,10) USER:21:(-0.01,0.01)                           1306
         AG_TITLE = 'TPAD'                                                1306
         AG_EXNAME = 'HITS'                                               1306
         AG_TYPE=AG_MAGIC                                                 1306
         CALL AGSDETV('TPAD')                                             1306
C    *                                                                    1306
         AG_PARLIST='Z'                                                   1306
         AG_TITLE = 'DETH'                                                1306
         AG_NBITS = AG_MAGIC                                              1306
         AG_BIN=0.0005                                                    1306
         AG_OPTION='S'                                                    1306
         AG_FHMIN = AG_MAGIC                                              1306
         AG_FHMAX = AG_MAGIC                                              1306
         CALL AGSDETH (0)                                                 1306
C    *                                                                    1306
         AG_PARLIST='Y'                                                   1306
         AG_TITLE = 'DETH'                                                1306
         AG_NBITS = AG_MAGIC                                              1306
         AG_BIN=0.0005                                                    1306
         AG_FHMIN=AG_MAGIC                                                1306
         AG_FHMAX=AG_MAGIC                                                1306
         AG_OPTION='*'                                                    1306
         CALL AGSDETH (0)                                                 1306
C    *                                                                    1306
         AG_PARLIST='X'                                                   1306
         AG_TITLE = 'DETH'                                                1306
         AG_NBITS = AG_MAGIC                                              1306
         AG_BIN=0.0005                                                    1306
         AG_FHMIN=AG_MAGIC                                                1306
         AG_FHMAX=AG_MAGIC                                                1306
         AG_OPTION='*'                                                    1306
         CALL AGSDETH (0)                                                 1306
C    *                                                                    1306
         AG_PARLIST='CX'                                                  1306
         AG_TITLE = 'DETH'                                                1306
         AG_NBITS = 10                                                    1306
         AG_BIN=AG_MAGIC                                                  1306
         AG_FHMIN=AG_MAGIC                                                1306
         AG_FHMAX=AG_MAGIC                                                1306
         AG_OPTION='*'                                                    1306
         CALL AGSDETH (0)                                                 1306
C    *                                                                    1306
         AG_PARLIST='CY'                                                  1306
         AG_TITLE = 'DETH'                                                1306
         AG_NBITS = 10                                                    1306
         AG_BIN=AG_MAGIC                                                  1306
         AG_FHMIN=AG_MAGIC                                                1306
         AG_FHMAX=AG_MAGIC                                                1306
         AG_OPTION='*'                                                    1306
         CALL AGSDETH (0)                                                 1306
C    *                                                                    1306
         AG_PARLIST='CZ'                                                  1306
         AG_TITLE = 'DETH'                                                1306
         AG_NBITS = 10                                                    1306
         AG_BIN=AG_MAGIC                                                  1306
         AG_FHMIN=AG_MAGIC                                                1306
         AG_FHMAX=AG_MAGIC                                                1306
         AG_OPTION='*'                                                    1306
         CALL AGSDETH (0)                                                 1306
C    *                                                                    1306
         AG_PARLIST='LPTOT'                                               1306
         AG_TITLE = 'DETH'                                                1306
         AG_NBITS = 18                                                    1306
         AG_BIN=AG_MAGIC                                                  1306
         AG_OPTION='*'                                                    1306
         AG_FHMIN = -3                                                    1306
         AG_FHMAX = 2                                                     1306
         CALL AGSDETH (0)                                                 1306
C    *                                                                    1306
         AG_PARLIST='SLENG'                                               1306
         AG_TITLE = 'DETH'                                                1306
         AG_NBITS = AG_MAGIC                                              1306
         AG_BIN=0.1                                                       1306
         AG_OPTION='*'                                                    1306
         AG_FHMIN = 0                                                     1306
         AG_FHMAX = 800                                                   1306
         CALL AGSDETH (0)                                                 1306
C    *                                                                    1306
         AG_PARLIST='TOF'                                                 1306
         AG_TITLE = 'DETH'                                                1306
         AG_NBITS = 16                                                    1306
         AG_BIN=AG_MAGIC                                                  1306
         AG_OPTION='*'                                                    1306
         AG_FHMIN = 0                                                     1306
         AG_FHMAX = TPCG_TOFMAX                                           1306
         CALL AGSDETH (0)                                                 1306
C    *                                                                    1306
         AG_PARLIST='LGAM'                                                1306
         AG_TITLE = 'DETH'                                                1306
         AG_NBITS = 16                                                    1306
         AG_BIN=AG_MAGIC                                                  1306
         AG_OPTION='*'                                                    1306
         AG_FHMIN = -2                                                    1306
         AG_FHMAX = +2                                                    1306
         CALL AGSDETH (0)                                                 1306
C    *                                                                    1306
         AG_PARLIST='STEP'                                                1306
         AG_TITLE = 'DETH'                                                1306
         AG_NBITS = 11                                                    1306
         AG_BIN=AG_MAGIC                                                  1306
         AG_OPTION='*'                                                    1306
         AG_FHMIN = 0                                                     1306
         AG_FHMAX = 10                                                    1306
         CALL AGSDETH (0)                                                 1306
C    *                                                                    1306
         AG_PARLIST='USER'                                                1306
         AG_TITLE = 'DETH'                                                1306
         AG_NBITS = 21                                                    1306
         AG_BIN=AG_MAGIC                                                  1306
         AG_OPTION='*'                                                    1306
         AG_FHMIN = -0.01                                                 1306
         AG_FHMAX = 0.01                                                  1306
         CALL AGSDETH (JUMPAD(TPADSTEP))                                  1306
C    *                                                                    1306
         CALL AGSDETU('TPAD')                                             1306
C    *                                                                    1306
6151  CALL AGSPOPB                                                        1307
      GO TO BLOCK_TPAD, (6150 ,6160,6170,6180)                            1307
C End of Local Procedure                                                  1307
      WRITE(LOUT,6190)'===>>  [End AgML Module tpcxgeo2]  <<==='          1307
6190  FORMAT(A32,/,/)                                                     1307
C MODULE TPCXGEO2                                                         1309
      END                                                                 1309
