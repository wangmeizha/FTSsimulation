C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE HCALGEO1                                                 1
C HCALGEO1 FORWARD HADRONIC CALORIMETER, PROPOSED FULL GEOMETRY, 2016     1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'HCALGEO1'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'FORWARD HADRONIC CALORIMETER, PROPOSED FULL GEOMETR  1
     *Y, 2016'/                                                           1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C AUTHOR PRASHANTH S (KSU), FPS BY AKIO                                   1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'PRASHANTH S (KSU), FPS BY AKIO'/              2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED 01/12/2014                                                      2
      CHARACTER*40 CREATED /'01/12/2014'/                                 3
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
      EXTERNAL HCALHIT                                                    4
      INTEGER BLOCK_HCMO                                                  20
      INTEGER BLOCK_HTWR                                                  20
      INTEGER BLOCK_HCEL                                                  20
      INTEGER BLOCK_HCES                                                  20
      INTEGER BLOCK_HCLX                                                  20
      INTEGER BLOCK_HCLY                                                  20
      INTEGER BLOCK_HCSX                                                  20
      INTEGER BLOCK_HCSY                                                  20
      INTEGER BLOCK_HFIB                                                  20
      INTEGER BLOCK_HFIC                                                  20
      INTEGER BLOCK_HACL                                                  20
      INTEGER BLOCK_FPRS                                                  20
      INTEGER BLOCK_FPLY                                                  20
      INTEGER BLOCK_FPPB                                                  20
      INTEGER BLOCK_FPSC                                                  20
      INTEGER BLOCK_FPBB                                                  20
      INTEGER BLOCK_FLIG                                                  20
      INTEGER BLOCK_FLBA                                                  20
      INTEGER BLOCK_FLCO                                                  20
      INTEGER BLOCK_FPMT                                                  20
      INTEGER BLOCK_FFEE                                                  20
      INTEGER BLOCK_FLLG                                                  20
      INTEGER BLOCK_FBLL                                                  20
      INTEGER BLOCK_FCLL                                                  20
      INTEGER BLOCK_WPPL                                                  20
      INTEGER BLOCK_WPIS                                                  20
      INTEGER BLOCK_WPUF                                                  20
      INTEGER BLOCK_WPUN                                                  20
      INTEGER BLOCK_WPUS                                                  20
      INTEGER BLOCK_WPAR                                                  20
      INTEGER *4 MMI,MMJ, NNI,NNJ, OOI,OOJ, PPI,PPJ, Q1I,Q1J, Q2I,Q2J,    22
     *Q3I,Q3J, Q4I,Q4J, FLAG,FLAG1,FLAG2, FLAG3                           22
      INTEGER *4 I,J , M,L                                                24
      REAL *4 XTEMP, YTEMP                                                26
      INTEGER K,KK,SERN,LR,TB,COL,KMOD,ROT,ID,LAYR,QUAD,SLAT(4,3),        29
     *DEBUG,NPSTYPE, PSTYPE(20),CPSTYPE(20)                               29
      REAL XX,YY,ZZ,X1,Y1,Z1,ZTOT,RTOT,WID,WIDX,WIDY,BWID,X0,WIDL,DX,DY,  33
     *DZ,DXX,DYY, DZZ,XXX,YYY,DXXX,DYYY,XPMT,YPMT, DX1,DY1,DZ1,DX2,DY2,   33
     *DZ2,X2,Y2,Z2,DZ3,DX1A, DX1B,DZSB,PSDIM(20,2)                        33
      REAL ZTOTSMD,WTOTSMD,ZSMD,ZSMD2,WSMD                                35
      REAL XSMDH,YSMDH,ZSMDH,XSMDV,YSMDV,ZSMDV                            37
      REAL XLCOFFSET,BZOFFSET                                             39
      REAL BASEWIDTH,DISTANCER,XOFFFECC,XOFFFEDC,XSHIFT                   41
      REAL XOFFFENC,YOFFFENC,ZOFFFENC,ZOFFFECC                            43
      REAL TMP(7)                                                         45
      INTEGER N                                                           47
      PARAMETER (N=12)                                                    49
      REAL E(N)                                                           50
      REAL RINDEX_PBG(N)                                                  52
      REAL RINDEX_SIRUB(N)                                                54
      REAL RINDEX_PHCATH(N)                                               56
      REAL RINDEX_ALM(N)                                                  58
      REAL RINDEX_MUMET(N)                                                60
      REAL ABSCO_PBG(N)                                                   62
      REAL ABSCO_SIRUB(N)                                                 64
      REAL ABSCO_PHCATH(N)                                                66
      REAL ABSCO_ALM(N)                                                   68
      REAL ABSCO_MUMET(N)                                                 70
      REAL EFFIC_PHCATH(N)                                                72
      REAL EFFIC_ALL(N)                                                   74
      REAL LPRS1,LPRS2,LPRS3,LPRS4,PWID,X,Y,Z,DLG                         76
      EXTERNAL FFPDSTEP                                                   78
      EXTERNAL FPCTSTEP                                                   79
C -                                                                       80
      REAL MCDT_VERSION                                                   80
      EQUIVALENCE (BANK_MCDT(3),MCDT_VERSION,RE_MCDT_VERSION),            80
     *(NAMES_MCDT(1),NMCDT_VERSION)                                       80
      EQUIVALENCE (COMMEN_MCDT(1),CMCDT_VERSION),(MAP_MCDT(1,1),          80
     *MMCDT_VERSION)                                                      80
      CHARACTER CMCDT_VERSION*40/' '/                                     80
      CHARACTER NMCDT_VERSION*12/'R.VERSION'/                             80
      INTEGER MMCDT_VERSION(2)/1,1/                                       80
      REAL RE_MCDT_VERSION                                                80
C -                                                                       80
      REAL MCDT_X                                                         80
      EQUIVALENCE (BANK_MCDT(4),MCDT_X,RE_MCDT_X),(NAMES_MCDT(2),         80
     *NMCDT_X)                                                            80
      EQUIVALENCE (COMMEN_MCDT(2),CMCDT_X),(MAP_MCDT(1,2),MMCDT_X)        80
      CHARACTER CMCDT_X*40/' '/                                           80
      CHARACTER NMCDT_X*12/'R.X'/                                         80
      INTEGER MMCDT_X(2)/1,1/                                             80
      REAL RE_MCDT_X                                                      80
C -                                                                       80
      REAL MCDT_Y                                                         80
      EQUIVALENCE (BANK_MCDT(5),MCDT_Y,RE_MCDT_Y),(NAMES_MCDT(3),         80
     *NMCDT_Y)                                                            80
      EQUIVALENCE (COMMEN_MCDT(3),CMCDT_Y),(MAP_MCDT(1,3),MMCDT_Y)        80
      CHARACTER CMCDT_Y*40/' '/                                           80
      CHARACTER NMCDT_Y*12/'R.Y'/                                         80
      INTEGER MMCDT_Y(2)/1,1/                                             80
      REAL RE_MCDT_Y                                                      80
C -                                                                       80
      REAL MCDT_Z                                                         80
      EQUIVALENCE (BANK_MCDT(6),MCDT_Z,RE_MCDT_Z),(NAMES_MCDT(4),         80
     *NMCDT_Z)                                                            80
      EQUIVALENCE (COMMEN_MCDT(4),CMCDT_Z),(MAP_MCDT(1,4),MMCDT_Z)        80
      CHARACTER CMCDT_Z*40/' '/                                           80
      CHARACTER NMCDT_Z*12/'R.Z'/                                         80
      INTEGER MMCDT_Z(2)/1,1/                                             80
      REAL RE_MCDT_Z                                                      80
C -                                                                       80
      REAL MCDT_DX                                                        80
      EQUIVALENCE (BANK_MCDT(7),MCDT_DX,RE_MCDT_DX),(NAMES_MCDT(5),       80
     *NMCDT_DX)                                                           80
      EQUIVALENCE (COMMEN_MCDT(5),CMCDT_DX),(MAP_MCDT(1,5),MMCDT_DX)      80
      CHARACTER CMCDT_DX*40/' '/                                          80
      CHARACTER NMCDT_DX*12/'R.DX'/                                       80
      INTEGER MMCDT_DX(2)/1,1/                                            80
      REAL RE_MCDT_DX                                                     80
C -                                                                       80
      REAL MCDT_DY                                                        80
      EQUIVALENCE (BANK_MCDT(8),MCDT_DY,RE_MCDT_DY),(NAMES_MCDT(6),       80
     *NMCDT_DY)                                                           80
      EQUIVALENCE (COMMEN_MCDT(6),CMCDT_DY),(MAP_MCDT(1,6),MMCDT_DY)      80
      CHARACTER CMCDT_DY*40/' '/                                          80
      CHARACTER NMCDT_DY*12/'R.DY'/                                       80
      INTEGER MMCDT_DY(2)/1,1/                                            80
      REAL RE_MCDT_DY                                                     80
C -                                                                       80
      REAL MCDT_DZ                                                        80
      EQUIVALENCE (BANK_MCDT(9),MCDT_DZ,RE_MCDT_DZ),(NAMES_MCDT(7),       80
     *NMCDT_DZ)                                                           80
      EQUIVALENCE (COMMEN_MCDT(7),CMCDT_DZ),(MAP_MCDT(1,7),MMCDT_DZ)      80
      CHARACTER CMCDT_DZ*40/' '/                                          80
      CHARACTER NMCDT_DZ*12/'R.DZ'/                                       80
      INTEGER MMCDT_DZ(2)/1,1/                                            80
      REAL RE_MCDT_DZ                                                     80
C -                                                                       80
      INTEGER LEN_MCDT,LLEN_MCDT,LINK_MCDT                                80
      PARAMETER (LEN_MCDT=9)                                              80
      PARAMETER (LLEN_MCDT=7)                                             80
      INTEGER FLAG_MCDT/0/,MAP_MCDT(2,LLEN_MCDT),IDNUM_MCDT(10)           80
      CHARACTER NAMES_MCDT(LLEN_MCDT)*12,COMMEN_MCDT(LLEN_MCDT)*40        80
      DATA IDNUM_MCDT/10*0/                                               80
      REAL BANK_MCDT(LEN_MCDT)                                            80
      CHARACTER*60 BPATH_MCDT/' '/                                        80
      SAVE FLAG_MCDT,MAP_MCDT,IDNUM_MCDT,NAMES_MCDT,COMMEN_MCDT,          80
     *BPATH_MCDT                                                          80
      DATA LINK_MCDT/0/,BANK_MCDT/LEN_MCDT*0/                             80
C *                                                                       80
C -                                                                       81
      REAL TOWR_VERSION                                                   81
      EQUIVALENCE (BANK_TOWR(3),TOWR_VERSION,RE_TOWR_VERSION),            81
     *(NAMES_TOWR(1),NTOWR_VERSION)                                       81
      EQUIVALENCE (COMMEN_TOWR(1),CTOWR_VERSION),(MAP_TOWR(1,1),          81
     *MTOWR_VERSION)                                                      81
      CHARACTER CTOWR_VERSION*40/' '/                                     81
      CHARACTER NTOWR_VERSION*12/'R.VERSION'/                             81
      INTEGER MTOWR_VERSION(2)/1,1/                                       81
      REAL RE_TOWR_VERSION                                                81
C -                                                                       81
      REAL TOWR_DX                                                        81
      EQUIVALENCE (BANK_TOWR(4),TOWR_DX,RE_TOWR_DX),(NAMES_TOWR(2),       81
     *NTOWR_DX)                                                           81
      EQUIVALENCE (COMMEN_TOWR(2),CTOWR_DX),(MAP_TOWR(1,2),MTOWR_DX)      81
      CHARACTER CTOWR_DX*40/' '/                                          81
      CHARACTER NTOWR_DX*12/'R.DX'/                                       81
      INTEGER MTOWR_DX(2)/1,1/                                            81
      REAL RE_TOWR_DX                                                     81
C -                                                                       81
      REAL TOWR_DY                                                        81
      EQUIVALENCE (BANK_TOWR(5),TOWR_DY,RE_TOWR_DY),(NAMES_TOWR(3),       81
     *NTOWR_DY)                                                           81
      EQUIVALENCE (COMMEN_TOWR(3),CTOWR_DY),(MAP_TOWR(1,3),MTOWR_DY)      81
      CHARACTER CTOWR_DY*40/' '/                                          81
      CHARACTER NTOWR_DY*12/'R.DY'/                                       81
      INTEGER MTOWR_DY(2)/1,1/                                            81
      REAL RE_TOWR_DY                                                     81
C -                                                                       81
      REAL TOWR_DZ                                                        81
      EQUIVALENCE (BANK_TOWR(6),TOWR_DZ,RE_TOWR_DZ),(NAMES_TOWR(4),       81
     *NTOWR_DZ)                                                           81
      EQUIVALENCE (COMMEN_TOWR(4),CTOWR_DZ),(MAP_TOWR(1,4),MTOWR_DZ)      81
      CHARACTER CTOWR_DZ*40/' '/                                          81
      CHARACTER NTOWR_DZ*12/'R.DZ'/                                       81
      INTEGER MTOWR_DZ(2)/1,1/                                            81
      REAL RE_TOWR_DZ                                                     81
C -                                                                       81
      REAL TOWR_NTOWERX                                                   81
      EQUIVALENCE (BANK_TOWR(7),TOWR_NTOWERX,RE_TOWR_NTOWERX),            81
     *(NAMES_TOWR(5),NTOWR_NTOWERX)                                       81
      EQUIVALENCE (COMMEN_TOWR(5),CTOWR_NTOWERX),(MAP_TOWR(1,5),          81
     *MTOWR_NTOWERX)                                                      81
      CHARACTER CTOWR_NTOWERX*40/' '/                                     81
      CHARACTER NTOWR_NTOWERX*12/'R.NTOWERX'/                             81
      INTEGER MTOWR_NTOWERX(2)/1,1/                                       81
      REAL RE_TOWR_NTOWERX                                                81
C -                                                                       81
      REAL TOWR_NTOWERY                                                   81
      EQUIVALENCE (BANK_TOWR(8),TOWR_NTOWERY,RE_TOWR_NTOWERY),            81
     *(NAMES_TOWR(6),NTOWR_NTOWERY)                                       81
      EQUIVALENCE (COMMEN_TOWR(6),CTOWR_NTOWERY),(MAP_TOWR(1,6),          81
     *MTOWR_NTOWERY)                                                      81
      CHARACTER CTOWR_NTOWERY*40/' '/                                     81
      CHARACTER NTOWR_NTOWERY*12/'R.NTOWERY'/                             81
      INTEGER MTOWR_NTOWERY(2)/1,1/                                       81
      REAL RE_TOWR_NTOWERY                                                81
C -                                                                       81
      INTEGER LEN_TOWR,LLEN_TOWR,LINK_TOWR                                81
      PARAMETER (LEN_TOWR=8)                                              81
      PARAMETER (LLEN_TOWR=6)                                             81
      INTEGER FLAG_TOWR/0/,MAP_TOWR(2,LLEN_TOWR),IDNUM_TOWR(10)           81
      CHARACTER NAMES_TOWR(LLEN_TOWR)*12,COMMEN_TOWR(LLEN_TOWR)*40        81
      DATA IDNUM_TOWR/10*0/                                               81
      REAL BANK_TOWR(LEN_TOWR)                                            81
      CHARACTER*60 BPATH_TOWR/' '/                                        81
      SAVE FLAG_TOWR,MAP_TOWR,IDNUM_TOWR,NAMES_TOWR,COMMEN_TOWR,          81
     *BPATH_TOWR                                                          81
      DATA LINK_TOWR/0/,BANK_TOWR/LEN_TOWR*0/                             81
C *                                                                       81
C -                                                                       83
      REAL CELL_VERSION                                                   83
      EQUIVALENCE (BANK_CELL(3),CELL_VERSION,RE_CELL_VERSION),            83
     *(NAMES_CELL(1),NCELL_VERSION)                                       83
      EQUIVALENCE (COMMEN_CELL(1),CCELL_VERSION),(MAP_CELL(1,1),          83
     *MCELL_VERSION)                                                      83
      CHARACTER CCELL_VERSION*40/' '/                                     83
      CHARACTER NCELL_VERSION*12/'R.VERSION'/                             83
      INTEGER MCELL_VERSION(2)/1,1/                                       83
      REAL RE_CELL_VERSION                                                83
C -                                                                       83
      REAL CELL_DX                                                        83
      EQUIVALENCE (BANK_CELL(4),CELL_DX,RE_CELL_DX),(NAMES_CELL(2),       83
     *NCELL_DX)                                                           83
      EQUIVALENCE (COMMEN_CELL(2),CCELL_DX),(MAP_CELL(1,2),MCELL_DX)      83
      CHARACTER CCELL_DX*40/' '/                                          83
      CHARACTER NCELL_DX*12/'R.DX'/                                       83
      INTEGER MCELL_DX(2)/1,1/                                            83
      REAL RE_CELL_DX                                                     83
C -                                                                       83
      REAL CELL_DY                                                        83
      EQUIVALENCE (BANK_CELL(5),CELL_DY,RE_CELL_DY),(NAMES_CELL(3),       83
     *NCELL_DY)                                                           83
      EQUIVALENCE (COMMEN_CELL(3),CCELL_DY),(MAP_CELL(1,3),MCELL_DY)      83
      CHARACTER CCELL_DY*40/' '/                                          83
      CHARACTER NCELL_DY*12/'R.DY'/                                       83
      INTEGER MCELL_DY(2)/1,1/                                            83
      REAL RE_CELL_DY                                                     83
C -                                                                       83
      REAL CELL_DZ                                                        83
      EQUIVALENCE (BANK_CELL(6),CELL_DZ,RE_CELL_DZ),(NAMES_CELL(4),       83
     *NCELL_DZ)                                                           83
      EQUIVALENCE (COMMEN_CELL(4),CCELL_DZ),(MAP_CELL(1,4),MCELL_DZ)      83
      CHARACTER CCELL_DZ*40/' '/                                          83
      CHARACTER NCELL_DZ*12/'R.DZ'/                                       83
      INTEGER MCELL_DZ(2)/1,1/                                            83
      REAL RE_CELL_DZ                                                     83
C -                                                                       83
      REAL CELL_NCELLX                                                    83
      EQUIVALENCE (BANK_CELL(7),CELL_NCELLX,RE_CELL_NCELLX),              83
     *(NAMES_CELL(5),NCELL_NCELLX)                                        83
      EQUIVALENCE (COMMEN_CELL(5),CCELL_NCELLX),(MAP_CELL(1,5),           83
     *MCELL_NCELLX)                                                       83
      CHARACTER CCELL_NCELLX*40/' '/                                      83
      CHARACTER NCELL_NCELLX*12/'R.NCELLX'/                               83
      INTEGER MCELL_NCELLX(2)/1,1/                                        83
      REAL RE_CELL_NCELLX                                                 83
C -                                                                       83
      REAL CELL_NCELLY                                                    83
      EQUIVALENCE (BANK_CELL(8),CELL_NCELLY,RE_CELL_NCELLY),              83
     *(NAMES_CELL(6),NCELL_NCELLY)                                        83
      EQUIVALENCE (COMMEN_CELL(6),CCELL_NCELLY),(MAP_CELL(1,6),           83
     *MCELL_NCELLY)                                                       83
      CHARACTER CCELL_NCELLY*40/' '/                                      83
      CHARACTER NCELL_NCELLY*12/'R.NCELLY'/                               83
      INTEGER MCELL_NCELLY(2)/1,1/                                        83
      REAL RE_CELL_NCELLY                                                 83
C -                                                                       83
      REAL CELL_NFIBERXL                                                  83
      EQUIVALENCE (BANK_CELL(9),CELL_NFIBERXL,RE_CELL_NFIBERXL),          83
     *(NAMES_CELL(7),NCELL_NFIBERXL)                                      83
      EQUIVALENCE (COMMEN_CELL(7),CCELL_NFIBERXL),(MAP_CELL(1,7),         83
     *MCELL_NFIBERXL)                                                     83
      CHARACTER CCELL_NFIBERXL*40/' '/                                    83
      CHARACTER NCELL_NFIBERXL*12/'R.NFIBERXL'/                           83
      INTEGER MCELL_NFIBERXL(2)/1,1/                                      83
      REAL RE_CELL_NFIBERXL                                               83
C -                                                                       83
      REAL CELL_NFIBERYL                                                  83
      EQUIVALENCE (BANK_CELL(10),CELL_NFIBERYL,RE_CELL_NFIBERYL),         83
     *(NAMES_CELL(8),NCELL_NFIBERYL)                                      83
      EQUIVALENCE (COMMEN_CELL(8),CCELL_NFIBERYL),(MAP_CELL(1,8),         83
     *MCELL_NFIBERYL)                                                     83
      CHARACTER CCELL_NFIBERYL*40/' '/                                    83
      CHARACTER NCELL_NFIBERYL*12/'R.NFIBERYL'/                           83
      INTEGER MCELL_NFIBERYL(2)/1,1/                                      83
      REAL RE_CELL_NFIBERYL                                               83
C -                                                                       83
      REAL CELL_NFIBERXS                                                  83
      EQUIVALENCE (BANK_CELL(11),CELL_NFIBERXS,RE_CELL_NFIBERXS),         83
     *(NAMES_CELL(9),NCELL_NFIBERXS)                                      83
      EQUIVALENCE (COMMEN_CELL(9),CCELL_NFIBERXS),(MAP_CELL(1,9),         83
     *MCELL_NFIBERXS)                                                     83
      CHARACTER CCELL_NFIBERXS*40/' '/                                    83
      CHARACTER NCELL_NFIBERXS*12/'R.NFIBERXS'/                           83
      INTEGER MCELL_NFIBERXS(2)/1,1/                                      83
      REAL RE_CELL_NFIBERXS                                               83
C -                                                                       83
      REAL CELL_NFIBERYS                                                  83
      EQUIVALENCE (BANK_CELL(12),CELL_NFIBERYS,RE_CELL_NFIBERYS),         83
     *(NAMES_CELL(10),NCELL_NFIBERYS)                                     83
      EQUIVALENCE (COMMEN_CELL(10),CCELL_NFIBERYS),(MAP_CELL(1,10),       83
     *MCELL_NFIBERYS)                                                     83
      CHARACTER CCELL_NFIBERYS*40/' '/                                    83
      CHARACTER NCELL_NFIBERYS*12/'R.NFIBERYS'/                           83
      INTEGER MCELL_NFIBERYS(2)/1,1/                                      83
      REAL RE_CELL_NFIBERYS                                               83
C -                                                                       83
      INTEGER LEN_CELL,LLEN_CELL,LINK_CELL                                83
      PARAMETER (LEN_CELL=12)                                             83
      PARAMETER (LLEN_CELL=10)                                            83
      INTEGER FLAG_CELL/0/,MAP_CELL(2,LLEN_CELL),IDNUM_CELL(10)           83
      CHARACTER NAMES_CELL(LLEN_CELL)*12,COMMEN_CELL(LLEN_CELL)*40        83
      DATA IDNUM_CELL/10*0/                                               83
      REAL BANK_CELL(LEN_CELL)                                            83
      CHARACTER*60 BPATH_CELL/' '/                                        83
      SAVE FLAG_CELL,MAP_CELL,IDNUM_CELL,NAMES_CELL,COMMEN_CELL,          83
     *BPATH_CELL                                                          83
      DATA LINK_CELL/0/,BANK_CELL/LEN_CELL*0/                             83
C *                                                                       83
C -                                                                       85
      REAL FIBR_VERSION                                                   85
      EQUIVALENCE (BANK_FIBR(3),FIBR_VERSION,RE_FIBR_VERSION),            85
     *(NAMES_FIBR(1),NFIBR_VERSION)                                       85
      EQUIVALENCE (COMMEN_FIBR(1),CFIBR_VERSION),(MAP_FIBR(1,1),          85
     *MFIBR_VERSION)                                                      85
      CHARACTER CFIBR_VERSION*40/' '/                                     85
      CHARACTER NFIBR_VERSION*12/'R.VERSION'/                             85
      INTEGER MFIBR_VERSION(2)/1,1/                                       85
      REAL RE_FIBR_VERSION                                                85
C -                                                                       85
      REAL FIBR_RMIN                                                      85
      EQUIVALENCE (BANK_FIBR(4),FIBR_RMIN,RE_FIBR_RMIN),(NAMES_FIBR(2),   85
     *NFIBR_RMIN)                                                         85
      EQUIVALENCE (COMMEN_FIBR(2),CFIBR_RMIN),(MAP_FIBR(1,2),             85
     *MFIBR_RMIN)                                                         85
      CHARACTER CFIBR_RMIN*40/' '/                                        85
      CHARACTER NFIBR_RMIN*12/'R.RMIN'/                                   85
      INTEGER MFIBR_RMIN(2)/1,1/                                          85
      REAL RE_FIBR_RMIN                                                   85
C -                                                                       85
      REAL FIBR_RMAX                                                      85
      EQUIVALENCE (BANK_FIBR(5),FIBR_RMAX,RE_FIBR_RMAX),(NAMES_FIBR(3),   85
     *NFIBR_RMAX)                                                         85
      EQUIVALENCE (COMMEN_FIBR(3),CFIBR_RMAX),(MAP_FIBR(1,3),             85
     *MFIBR_RMAX)                                                         85
      CHARACTER CFIBR_RMAX*40/' '/                                        85
      CHARACTER NFIBR_RMAX*12/'R.RMAX'/                                   85
      INTEGER MFIBR_RMAX(2)/1,1/                                          85
      REAL RE_FIBR_RMAX                                                   85
C -                                                                       85
      REAL FIBR_DZ                                                        85
      EQUIVALENCE (BANK_FIBR(6),FIBR_DZ,RE_FIBR_DZ),(NAMES_FIBR(4),       85
     *NFIBR_DZ)                                                           85
      EQUIVALENCE (COMMEN_FIBR(4),CFIBR_DZ),(MAP_FIBR(1,4),MFIBR_DZ)      85
      CHARACTER CFIBR_DZ*40/' '/                                          85
      CHARACTER NFIBR_DZ*12/'R.DZ'/                                       85
      INTEGER MFIBR_DZ(2)/1,1/                                            85
      REAL RE_FIBR_DZ                                                     85
C -                                                                       85
      REAL FIBR_STARTX1                                                   85
      EQUIVALENCE (BANK_FIBR(7),FIBR_STARTX1,RE_FIBR_STARTX1),            85
     *(NAMES_FIBR(5),NFIBR_STARTX1)                                       85
      EQUIVALENCE (COMMEN_FIBR(5),CFIBR_STARTX1),(MAP_FIBR(1,5),          85
     *MFIBR_STARTX1)                                                      85
      CHARACTER CFIBR_STARTX1*40/' '/                                     85
      CHARACTER NFIBR_STARTX1*12/'R.STARTX1'/                             85
      INTEGER MFIBR_STARTX1(2)/1,1/                                       85
      REAL RE_FIBR_STARTX1                                                85
C -                                                                       85
      REAL FIBR_STARTY1                                                   85
      EQUIVALENCE (BANK_FIBR(8),FIBR_STARTY1,RE_FIBR_STARTY1),            85
     *(NAMES_FIBR(6),NFIBR_STARTY1)                                       85
      EQUIVALENCE (COMMEN_FIBR(6),CFIBR_STARTY1),(MAP_FIBR(1,6),          85
     *MFIBR_STARTY1)                                                      85
      CHARACTER CFIBR_STARTY1*40/' '/                                     85
      CHARACTER NFIBR_STARTY1*12/'R.STARTY1'/                             85
      INTEGER MFIBR_STARTY1(2)/1,1/                                       85
      REAL RE_FIBR_STARTY1                                                85
C -                                                                       85
      REAL FIBR_STARTX2                                                   85
      EQUIVALENCE (BANK_FIBR(9),FIBR_STARTX2,RE_FIBR_STARTX2),            85
     *(NAMES_FIBR(7),NFIBR_STARTX2)                                       85
      EQUIVALENCE (COMMEN_FIBR(7),CFIBR_STARTX2),(MAP_FIBR(1,7),          85
     *MFIBR_STARTX2)                                                      85
      CHARACTER CFIBR_STARTX2*40/' '/                                     85
      CHARACTER NFIBR_STARTX2*12/'R.STARTX2'/                             85
      INTEGER MFIBR_STARTX2(2)/1,1/                                       85
      REAL RE_FIBR_STARTX2                                                85
C -                                                                       85
      REAL FIBR_STARTY2                                                   85
      EQUIVALENCE (BANK_FIBR(10),FIBR_STARTY2,RE_FIBR_STARTY2),           85
     *(NAMES_FIBR(8),NFIBR_STARTY2)                                       85
      EQUIVALENCE (COMMEN_FIBR(8),CFIBR_STARTY2),(MAP_FIBR(1,8),          85
     *MFIBR_STARTY2)                                                      85
      CHARACTER CFIBR_STARTY2*40/' '/                                     85
      CHARACTER NFIBR_STARTY2*12/'R.STARTY2'/                             85
      INTEGER MFIBR_STARTY2(2)/1,1/                                       85
      REAL RE_FIBR_STARTY2                                                85
C -                                                                       85
      REAL FIBR_STEPX                                                     85
      EQUIVALENCE (BANK_FIBR(11),FIBR_STEPX,RE_FIBR_STEPX),               85
     *(NAMES_FIBR(9),NFIBR_STEPX)                                         85
      EQUIVALENCE (COMMEN_FIBR(9),CFIBR_STEPX),(MAP_FIBR(1,9),            85
     *MFIBR_STEPX)                                                        85
      CHARACTER CFIBR_STEPX*40/' '/                                       85
      CHARACTER NFIBR_STEPX*12/'R.STEPX'/                                 85
      INTEGER MFIBR_STEPX(2)/1,1/                                         85
      REAL RE_FIBR_STEPX                                                  85
C -                                                                       85
      REAL FIBR_STEPY                                                     85
      EQUIVALENCE (BANK_FIBR(12),FIBR_STEPY,RE_FIBR_STEPY),               85
     *(NAMES_FIBR(10),NFIBR_STEPY)                                        85
      EQUIVALENCE (COMMEN_FIBR(10),CFIBR_STEPY),(MAP_FIBR(1,10),          85
     *MFIBR_STEPY)                                                        85
      CHARACTER CFIBR_STEPY*40/' '/                                       85
      CHARACTER NFIBR_STEPY*12/'R.STEPY'/                                 85
      INTEGER MFIBR_STEPY(2)/1,1/                                         85
      REAL RE_FIBR_STEPY                                                  85
C -                                                                       85
      INTEGER LEN_FIBR,LLEN_FIBR,LINK_FIBR                                85
      PARAMETER (LEN_FIBR=12)                                             85
      PARAMETER (LLEN_FIBR=10)                                            85
      INTEGER FLAG_FIBR/0/,MAP_FIBR(2,LLEN_FIBR),IDNUM_FIBR(10)           85
      CHARACTER NAMES_FIBR(LLEN_FIBR)*12,COMMEN_FIBR(LLEN_FIBR)*40        85
      DATA IDNUM_FIBR/10*0/                                               85
      REAL BANK_FIBR(LEN_FIBR)                                            85
      CHARACTER*60 BPATH_FIBR/' '/                                        85
      SAVE FLAG_FIBR,MAP_FIBR,IDNUM_FIBR,NAMES_FIBR,COMMEN_FIBR,          85
     *BPATH_FIBR                                                          85
      DATA LINK_FIBR/0/,BANK_FIBR/LEN_FIBR*0/                             85
C *                                                                       85
C -                                                                       87
      REAL PRSW_DBOX(3)                                                   87
      EQUIVALENCE (BANK_PRSW(3),PRSW_DBOX,RE_PRSW_DBOX),(NAMES_PRSW(1),   87
     *NPRSW_DBOX)                                                         87
      EQUIVALENCE (COMMEN_PRSW(1),CPRSW_DBOX),(MAP_PRSW(1,1),             87
     *MPRSW_DBOX)                                                         87
      CHARACTER CPRSW_DBOX*40/' '/                                        87
      CHARACTER NPRSW_DBOX*12/'R.DBOX'/                                   87
      INTEGER MPRSW_DBOX(2)/3,1/                                          87
      REAL RE_PRSW_DBOX                                                   87
C -                                                                       87
      REAL PRSW_XOFF(4)                                                   87
      EQUIVALENCE (BANK_PRSW(6),PRSW_XOFF,RE_PRSW_XOFF),(NAMES_PRSW(2),   87
     *NPRSW_XOFF)                                                         87
      EQUIVALENCE (COMMEN_PRSW(2),CPRSW_XOFF),(MAP_PRSW(1,2),             87
     *MPRSW_XOFF)                                                         87
      CHARACTER CPRSW_XOFF*40/' '/                                        87
      CHARACTER NPRSW_XOFF*12/'R.XOFF'/                                   87
      INTEGER MPRSW_XOFF(2)/4,1/                                          87
      REAL RE_PRSW_XOFF                                                   87
C -                                                                       87
      REAL PRSW_YOFF(4)                                                   87
      EQUIVALENCE (BANK_PRSW(10),PRSW_YOFF,RE_PRSW_YOFF),(NAMES_PRSW(3),  87
     *NPRSW_YOFF)                                                         87
      EQUIVALENCE (COMMEN_PRSW(3),CPRSW_YOFF),(MAP_PRSW(1,3),             87
     *MPRSW_YOFF)                                                         87
      CHARACTER CPRSW_YOFF*40/' '/                                        87
      CHARACTER NPRSW_YOFF*12/'R.YOFF'/                                   87
      INTEGER MPRSW_YOFF(2)/4,1/                                          87
      REAL RE_PRSW_YOFF                                                   87
C -                                                                       87
      REAL PRSW_ZOFF(4)                                                   87
      EQUIVALENCE (BANK_PRSW(14),PRSW_ZOFF,RE_PRSW_ZOFF),(NAMES_PRSW(4),  87
     *NPRSW_ZOFF)                                                         87
      EQUIVALENCE (COMMEN_PRSW(4),CPRSW_ZOFF),(MAP_PRSW(1,4),             87
     *MPRSW_ZOFF)                                                         87
      CHARACTER CPRSW_ZOFF*40/' '/                                        87
      CHARACTER NPRSW_ZOFF*12/'R.ZOFF'/                                   87
      INTEGER MPRSW_ZOFF(2)/4,1/                                          87
      REAL RE_PRSW_ZOFF                                                   87
C -                                                                       87
      REAL PRSW_DZ(4)                                                     87
      EQUIVALENCE (BANK_PRSW(18),PRSW_DZ,RE_PRSW_DZ),(NAMES_PRSW(5),      87
     *NPRSW_DZ)                                                           87
      EQUIVALENCE (COMMEN_PRSW(5),CPRSW_DZ),(MAP_PRSW(1,5),MPRSW_DZ)      87
      CHARACTER CPRSW_DZ*40/' '/                                          87
      CHARACTER NPRSW_DZ*12/'R.DZ'/                                       87
      INTEGER MPRSW_DZ(2)/4,1/                                            87
      REAL RE_PRSW_DZ                                                     87
C -                                                                       87
      REAL PRSW_DHOLE                                                     87
      EQUIVALENCE (BANK_PRSW(22),PRSW_DHOLE,RE_PRSW_DHOLE),               87
     *(NAMES_PRSW(6),NPRSW_DHOLE)                                         87
      EQUIVALENCE (COMMEN_PRSW(6),CPRSW_DHOLE),(MAP_PRSW(1,6),            87
     *MPRSW_DHOLE)                                                        87
      CHARACTER CPRSW_DHOLE*40/' '/                                       87
      CHARACTER NPRSW_DHOLE*12/'R.DHOLE'/                                 87
      INTEGER MPRSW_DHOLE(2)/1,1/                                         87
      REAL RE_PRSW_DHOLE                                                  87
C -                                                                       87
      REAL PRSW_NTYPE                                                     87
      EQUIVALENCE (BANK_PRSW(23),PRSW_NTYPE,RE_PRSW_NTYPE),               87
     *(NAMES_PRSW(7),NPRSW_NTYPE)                                         87
      EQUIVALENCE (COMMEN_PRSW(7),CPRSW_NTYPE),(MAP_PRSW(1,7),            87
     *MPRSW_NTYPE)                                                        87
      CHARACTER CPRSW_NTYPE*40/' '/                                       87
      CHARACTER NPRSW_NTYPE*12/'R.NTYPE'/                                 87
      INTEGER MPRSW_NTYPE(2)/1,1/                                         87
      REAL RE_PRSW_NTYPE                                                  87
C -                                                                       87
      REAL PRSW_NPRS(2)                                                   87
      EQUIVALENCE (BANK_PRSW(24),PRSW_NPRS,RE_PRSW_NPRS),(NAMES_PRSW(8),  87
     *NPRSW_NPRS)                                                         87
      EQUIVALENCE (COMMEN_PRSW(8),CPRSW_NPRS),(MAP_PRSW(1,8),             87
     *MPRSW_NPRS)                                                         87
      CHARACTER CPRSW_NPRS*40/' '/                                        87
      CHARACTER NPRSW_NPRS*12/'R.NPRS'/                                   87
      INTEGER MPRSW_NPRS(2)/2,1/                                          87
      REAL RE_PRSW_NPRS                                                   87
C -                                                                       87
      REAL PRSW_DPRS(3)                                                   87
      EQUIVALENCE (BANK_PRSW(26),PRSW_DPRS,RE_PRSW_DPRS),(NAMES_PRSW(9),  87
     *NPRSW_DPRS)                                                         87
      EQUIVALENCE (COMMEN_PRSW(9),CPRSW_DPRS),(MAP_PRSW(1,9),             87
     *MPRSW_DPRS)                                                         87
      CHARACTER CPRSW_DPRS*40/' '/                                        87
      CHARACTER NPRSW_DPRS*12/'R.DPRS'/                                   87
      INTEGER MPRSW_DPRS(2)/3,1/                                          87
      REAL RE_PRSW_DPRS                                                   87
C -                                                                       87
      REAL PRSW_NSKIPV                                                    87
      EQUIVALENCE (BANK_PRSW(29),PRSW_NSKIPV,RE_PRSW_NSKIPV),             87
     *(NAMES_PRSW(10),NPRSW_NSKIPV)                                       87
      EQUIVALENCE (COMMEN_PRSW(10),CPRSW_NSKIPV),(MAP_PRSW(1,10),         87
     *MPRSW_NSKIPV)                                                       87
      CHARACTER CPRSW_NSKIPV*40/' '/                                      87
      CHARACTER NPRSW_NSKIPV*12/'R.NSKIPV'/                               87
      INTEGER MPRSW_NSKIPV(2)/1,1/                                        87
      REAL RE_PRSW_NSKIPV                                                 87
C -                                                                       87
      REAL PRSW_NSKIPH                                                    87
      EQUIVALENCE (BANK_PRSW(30),PRSW_NSKIPH,RE_PRSW_NSKIPH),             87
     *(NAMES_PRSW(11),NPRSW_NSKIPH)                                       87
      EQUIVALENCE (COMMEN_PRSW(11),CPRSW_NSKIPH),(MAP_PRSW(1,11),         87
     *MPRSW_NSKIPH)                                                       87
      CHARACTER CPRSW_NSKIPH*40/' '/                                      87
      CHARACTER NPRSW_NSKIPH*12/'R.NSKIPH'/                               87
      INTEGER MPRSW_NSKIPH(2)/1,1/                                        87
      REAL RE_PRSW_NSKIPH                                                 87
C -                                                                       87
      REAL PRSW_DYCUT                                                     87
      EQUIVALENCE (BANK_PRSW(31),PRSW_DYCUT,RE_PRSW_DYCUT),               87
     *(NAMES_PRSW(12),NPRSW_DYCUT)                                        87
      EQUIVALENCE (COMMEN_PRSW(12),CPRSW_DYCUT),(MAP_PRSW(1,12),          87
     *MPRSW_DYCUT)                                                        87
      CHARACTER CPRSW_DYCUT*40/' '/                                       87
      CHARACTER NPRSW_DYCUT*12/'R.DYCUT'/                                 87
      INTEGER MPRSW_DYCUT(2)/1,1/                                         87
      REAL RE_PRSW_DYCUT                                                  87
C -                                                                       87
      REAL PRSW_DLG(3)                                                    87
      EQUIVALENCE (BANK_PRSW(32),PRSW_DLG,RE_PRSW_DLG),(NAMES_PRSW(13),   87
     *NPRSW_DLG)                                                          87
      EQUIVALENCE (COMMEN_PRSW(13),CPRSW_DLG),(MAP_PRSW(1,13),            87
     *MPRSW_DLG)                                                          87
      CHARACTER CPRSW_DLG*40/' '/                                         87
      CHARACTER NPRSW_DLG*12/'R.DLG'/                                     87
      INTEGER MPRSW_DLG(2)/3,1/                                           87
      REAL RE_PRSW_DLG                                                    87
C -                                                                       87
      REAL PRSW_DSIPM(4)                                                  87
      EQUIVALENCE (BANK_PRSW(35),PRSW_DSIPM,RE_PRSW_DSIPM),               87
     *(NAMES_PRSW(14),NPRSW_DSIPM)                                        87
      EQUIVALENCE (COMMEN_PRSW(14),CPRSW_DSIPM),(MAP_PRSW(1,14),          87
     *MPRSW_DSIPM)                                                        87
      CHARACTER CPRSW_DSIPM*40/' '/                                       87
      CHARACTER NPRSW_DSIPM*12/'R.DSIPM'/                                 87
      INTEGER MPRSW_DSIPM(2)/4,1/                                         87
      REAL RE_PRSW_DSIPM                                                  87
C -                                                                       87
      REAL PRSW_DZBBD(4)                                                  87
      EQUIVALENCE (BANK_PRSW(39),PRSW_DZBBD,RE_PRSW_DZBBD),               87
     *(NAMES_PRSW(15),NPRSW_DZBBD)                                        87
      EQUIVALENCE (COMMEN_PRSW(15),CPRSW_DZBBD),(MAP_PRSW(1,15),          87
     *MPRSW_DZBBD)                                                        87
      CHARACTER CPRSW_DZBBD*40/' '/                                       87
      CHARACTER NPRSW_DZBBD*12/'R.DZBBD'/                                 87
      INTEGER MPRSW_DZBBD(2)/4,1/                                         87
      REAL RE_PRSW_DZBBD                                                  87
C -                                                                       87
      REAL PRSW_DXBBD(2)                                                  87
      EQUIVALENCE (BANK_PRSW(43),PRSW_DXBBD,RE_PRSW_DXBBD),               87
     *(NAMES_PRSW(16),NPRSW_DXBBD)                                        87
      EQUIVALENCE (COMMEN_PRSW(16),CPRSW_DXBBD),(MAP_PRSW(1,16),          87
     *MPRSW_DXBBD)                                                        87
      CHARACTER CPRSW_DXBBD*40/' '/                                       87
      CHARACTER NPRSW_DXBBD*12/'R.DXBBD'/                                 87
      INTEGER MPRSW_DXBBD(2)/2,1/                                         87
      REAL RE_PRSW_DXBBD                                                  87
C -                                                                       87
      INTEGER LEN_PRSW,LLEN_PRSW,LINK_PRSW                                87
      PARAMETER (LEN_PRSW=44)                                             87
      PARAMETER (LLEN_PRSW=16)                                            87
      INTEGER FLAG_PRSW/0/,MAP_PRSW(2,LLEN_PRSW),IDNUM_PRSW(10)           87
      CHARACTER NAMES_PRSW(LLEN_PRSW)*12,COMMEN_PRSW(LLEN_PRSW)*40        87
      DATA IDNUM_PRSW/10*0/                                               87
      REAL BANK_PRSW(LEN_PRSW)                                            87
      CHARACTER*60 BPATH_PRSW/' '/                                        87
      SAVE FLAG_PRSW,MAP_PRSW,IDNUM_PRSW,NAMES_PRSW,COMMEN_PRSW,          87
     *BPATH_PRSW                                                          87
      DATA LINK_PRSW/0/,BANK_PRSW/LEN_PRSW*0/                             87
C *                                                                       87
C -                                                                       89
      REAL WPFM_POLED(4)                                                  89
      EQUIVALENCE (BANK_WPFM(3),WPFM_POLED,RE_WPFM_POLED),                89
     *(NAMES_WPFM(1),NWPFM_POLED)                                         89
      EQUIVALENCE (COMMEN_WPFM(1),CWPFM_POLED),(MAP_WPFM(1,1),            89
     *MWPFM_POLED)                                                        89
      CHARACTER CWPFM_POLED*40/' '/                                       89
      CHARACTER NWPFM_POLED*12/'R.POLED'/                                 89
      INTEGER MWPFM_POLED(2)/4,1/                                         89
      REAL RE_WPFM_POLED                                                  89
C -                                                                       89
      REAL WPFM_POLEP(3)                                                  89
      EQUIVALENCE (BANK_WPFM(7),WPFM_POLEP,RE_WPFM_POLEP),                89
     *(NAMES_WPFM(2),NWPFM_POLEP)                                         89
      EQUIVALENCE (COMMEN_WPFM(2),CWPFM_POLEP),(MAP_WPFM(1,2),            89
     *MWPFM_POLEP)                                                        89
      CHARACTER CWPFM_POLEP*40/' '/                                       89
      CHARACTER NWPFM_POLEP*12/'R.POLEP'/                                 89
      INTEGER MWPFM_POLEP(2)/3,1/                                         89
      REAL RE_WPFM_POLEP                                                  89
C -                                                                       89
      REAL WPFM_ISTRD(4)                                                  89
      EQUIVALENCE (BANK_WPFM(10),WPFM_ISTRD,RE_WPFM_ISTRD),               89
     *(NAMES_WPFM(3),NWPFM_ISTRD)                                         89
      EQUIVALENCE (COMMEN_WPFM(3),CWPFM_ISTRD),(MAP_WPFM(1,3),            89
     *MWPFM_ISTRD)                                                        89
      CHARACTER CWPFM_ISTRD*40/' '/                                       89
      CHARACTER NWPFM_ISTRD*12/'R.ISTRD'/                                 89
      INTEGER MWPFM_ISTRD(2)/4,1/                                         89
      REAL RE_WPFM_ISTRD                                                  89
C -                                                                       89
      REAL WPFM_IST1P(3)                                                  89
      EQUIVALENCE (BANK_WPFM(14),WPFM_IST1P,RE_WPFM_IST1P),               89
     *(NAMES_WPFM(4),NWPFM_IST1P)                                         89
      EQUIVALENCE (COMMEN_WPFM(4),CWPFM_IST1P),(MAP_WPFM(1,4),            89
     *MWPFM_IST1P)                                                        89
      CHARACTER CWPFM_IST1P*40/' '/                                       89
      CHARACTER NWPFM_IST1P*12/'R.IST1P'/                                 89
      INTEGER MWPFM_IST1P(2)/3,1/                                         89
      REAL RE_WPFM_IST1P                                                  89
C -                                                                       89
      REAL WPFM_IST2P(3)                                                  89
      EQUIVALENCE (BANK_WPFM(17),WPFM_IST2P,RE_WPFM_IST2P),               89
     *(NAMES_WPFM(5),NWPFM_IST2P)                                         89
      EQUIVALENCE (COMMEN_WPFM(5),CWPFM_IST2P),(MAP_WPFM(1,5),            89
     *MWPFM_IST2P)                                                        89
      CHARACTER CWPFM_IST2P*40/' '/                                       89
      CHARACTER NWPFM_IST2P*12/'R.IST2P'/                                 89
      INTEGER MWPFM_IST2P(2)/3,1/                                         89
      REAL RE_WPFM_IST2P                                                  89
C -                                                                       89
      REAL WPFM_USTFD(4)                                                  89
      EQUIVALENCE (BANK_WPFM(20),WPFM_USTFD,RE_WPFM_USTFD),               89
     *(NAMES_WPFM(6),NWPFM_USTFD)                                         89
      EQUIVALENCE (COMMEN_WPFM(6),CWPFM_USTFD),(MAP_WPFM(1,6),            89
     *MWPFM_USTFD)                                                        89
      CHARACTER CWPFM_USTFD*40/' '/                                       89
      CHARACTER NWPFM_USTFD*12/'R.USTFD'/                                 89
      INTEGER MWPFM_USTFD(2)/4,1/                                         89
      REAL RE_WPFM_USTFD                                                  89
C -                                                                       89
      REAL WPFM_USTFP(3)                                                  89
      EQUIVALENCE (BANK_WPFM(24),WPFM_USTFP,RE_WPFM_USTFP),               89
     *(NAMES_WPFM(7),NWPFM_USTFP)                                         89
      EQUIVALENCE (COMMEN_WPFM(7),CWPFM_USTFP),(MAP_WPFM(1,7),            89
     *MWPFM_USTFP)                                                        89
      CHARACTER CWPFM_USTFP*40/' '/                                       89
      CHARACTER NWPFM_USTFP*12/'R.USTFP'/                                 89
      INTEGER MWPFM_USTFP(2)/3,1/                                         89
      REAL RE_WPFM_USTFP                                                  89
C -                                                                       89
      REAL WPFM_USTND(4)                                                  89
      EQUIVALENCE (BANK_WPFM(27),WPFM_USTND,RE_WPFM_USTND),               89
     *(NAMES_WPFM(8),NWPFM_USTND)                                         89
      EQUIVALENCE (COMMEN_WPFM(8),CWPFM_USTND),(MAP_WPFM(1,8),            89
     *MWPFM_USTND)                                                        89
      CHARACTER CWPFM_USTND*40/' '/                                       89
      CHARACTER NWPFM_USTND*12/'R.USTND'/                                 89
      INTEGER MWPFM_USTND(2)/4,1/                                         89
      REAL RE_WPFM_USTND                                                  89
C -                                                                       89
      REAL WPFM_USTNP(3)                                                  89
      EQUIVALENCE (BANK_WPFM(31),WPFM_USTNP,RE_WPFM_USTNP),               89
     *(NAMES_WPFM(9),NWPFM_USTNP)                                         89
      EQUIVALENCE (COMMEN_WPFM(9),CWPFM_USTNP),(MAP_WPFM(1,9),            89
     *MWPFM_USTNP)                                                        89
      CHARACTER CWPFM_USTNP*40/' '/                                       89
      CHARACTER NWPFM_USTNP*12/'R.USTNP'/                                 89
      INTEGER MWPFM_USTNP(2)/3,1/                                         89
      REAL RE_WPFM_USTNP                                                  89
C -                                                                       89
      REAL WPFM_USTSD(4)                                                  89
      EQUIVALENCE (BANK_WPFM(34),WPFM_USTSD,RE_WPFM_USTSD),               89
     *(NAMES_WPFM(10),NWPFM_USTSD)                                        89
      EQUIVALENCE (COMMEN_WPFM(10),CWPFM_USTSD),(MAP_WPFM(1,10),          89
     *MWPFM_USTSD)                                                        89
      CHARACTER CWPFM_USTSD*40/' '/                                       89
      CHARACTER NWPFM_USTSD*12/'R.USTSD'/                                 89
      INTEGER MWPFM_USTSD(2)/4,1/                                         89
      REAL RE_WPFM_USTSD                                                  89
C -                                                                       89
      REAL WPFM_USTSP(3)                                                  89
      EQUIVALENCE (BANK_WPFM(38),WPFM_USTSP,RE_WPFM_USTSP),               89
     *(NAMES_WPFM(11),NWPFM_USTSP)                                        89
      EQUIVALENCE (COMMEN_WPFM(11),CWPFM_USTSP),(MAP_WPFM(1,11),          89
     *MWPFM_USTSP)                                                        89
      CHARACTER CWPFM_USTSP*40/' '/                                       89
      CHARACTER NWPFM_USTSP*12/'R.USTSP'/                                 89
      INTEGER MWPFM_USTSP(2)/3,1/                                         89
      REAL RE_WPFM_USTSP                                                  89
C -                                                                       89
      INTEGER LEN_WPFM,LLEN_WPFM,LINK_WPFM                                89
      PARAMETER (LEN_WPFM=40)                                             89
      PARAMETER (LLEN_WPFM=11)                                            89
      INTEGER FLAG_WPFM/0/,MAP_WPFM(2,LLEN_WPFM),IDNUM_WPFM(10)           89
      CHARACTER NAMES_WPFM(LLEN_WPFM)*12,COMMEN_WPFM(LLEN_WPFM)*40        89
      DATA IDNUM_WPFM/10*0/                                               89
      REAL BANK_WPFM(LEN_WPFM)                                            89
      CHARACTER*60 BPATH_WPFM/' '/                                        89
      SAVE FLAG_WPFM,MAP_WPFM,IDNUM_WPFM,NAMES_WPFM,COMMEN_WPFM,          89
     *BPATH_WPFM                                                          89
      DATA LINK_WPFM/0/,BANK_WPFM/LEN_WPFM*0/                             89
C *                                                                       89
C *                                                                       90
      CALL AGSBEGM(MODULE,IPRIN)                                          90
      IF (IPRIN.LT.0) RETURN                                              90
C Filling bank MCDT - MOTHER CALORIMETER                                  90
      BANK_TITLE='MOTHER CALORIMETER'                                     90
      BPATH_MCDT='MCDT*'                                                  90
      IDNUM_MCDT(1)=0                                                     91
C FACE CENTER OF CALORIMETER                                              91
      MCDT_VERSION=1.0                                                    91
      CMCDT_VERSION='FACE CENTER OF CALORIMETER'                          91
C X CDT FROM CENTER OF TPC                                                92
      MCDT_X=0.0                                                          92
      CMCDT_X='X CDT FROM CENTER OF TPC'                                  92
C Y CDT FROM CENTER OF TPC                                                93
      MCDT_Y=0.0                                                          93
      CMCDT_Y='Y CDT FROM CENTER OF TPC'                                  93
C Z CDT FROM CENTER OF TPC                                                94
      MCDT_Z=777.4                                                        94
      CMCDT_Z='Z CDT FROM CENTER OF TPC'                                  94
C HCMO WIDTH                                                              95
      MCDT_DX=20*10/2.                                                    95
      CMCDT_DX='HCMO WIDTH'                                               95
C HCMO HEIGHT                                                             96
      MCDT_DY=18*10/2.                                                    96
      CMCDT_DY='HCMO HEIGHT'                                              96
C HCMO LENGTH                                                             97
      MCDT_DZ=117/2.                                                      97
      CMCDT_DZ='HCMO LENGTH'                                              97
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MCDT',    98
     *BPATH_MCDT,IDNUM_MCDT,1, MAP_MCDT,NAMES_MCDT,COMMEN_MCDT,           98
     *BANK_MCDT,LEN_MCDT,LLEN_MCDT,LINK_MCDT,FLAG_MCDT)                   98
C *                                                                       98
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MCDT',    98
     *BPATH_MCDT,IDNUM_MCDT,1, MAP_MCDT,NAMES_MCDT,COMMEN_MCDT,           98
     *BANK_MCDT,LEN_MCDT,LLEN_MCDT,LINK_MCDT,FLAG_MCDT)                   98
C *                                                                       99
      CALL AGSBEGM(MODULE,IPRIN)                                          99
      IF (IPRIN.LT.0) RETURN                                              99
C Filling bank TOWR - STRUCTURE CONTAINING CALORIMETER TOWR DIMENTION     99
      BANK_TITLE='STRUCTURE CONTAINING CALORIMETER TOWR DIMENTION'        99
      BPATH_TOWR='TOWR*'                                                  99
      IDNUM_TOWR(1)=0                                                     100
C TOWR DIMENTION                                                          100
      TOWR_VERSION=1.0                                                    100
      CTOWR_VERSION='TOWR DIMENTION'                                      100
C TOWR WIDTH                                                              101
      TOWR_DX=10.0/2.                                                     101
      CTOWR_DX='TOWR WIDTH'                                               101
C TOWR HEIGHT                                                             102
      TOWR_DY=10.0/2.                                                     102
      CTOWR_DY='TOWR HEIGHT'                                              102
C TOWR LENGTH                                                             103
      TOWR_DZ=117.0/2.                                                    103
      CTOWR_DZ='TOWR LENGTH'                                              103
C NO OF TOWER IN X DIR                                                    104
      TOWR_NTOWERX=20                                                     104
      CTOWR_NTOWERX='NO OF TOWER IN X DIR'                                104
C NO OF TOWER IN Y DIR                                                    105
      TOWR_NTOWERY=18                                                     105
      CTOWR_NTOWERY='NO OF TOWER IN Y DIR'                                105
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TOWR',    106
     *BPATH_TOWR,IDNUM_TOWR,1, MAP_TOWR,NAMES_TOWR,COMMEN_TOWR,           106
     *BANK_TOWR,LEN_TOWR,LLEN_TOWR,LINK_TOWR,FLAG_TOWR)                   106
C *                                                                       106
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TOWR',    106
     *BPATH_TOWR,IDNUM_TOWR,1, MAP_TOWR,NAMES_TOWR,COMMEN_TOWR,           106
     *BANK_TOWR,LEN_TOWR,LLEN_TOWR,LINK_TOWR,FLAG_TOWR)                   106
C *                                                                       107
      CALL AGSBEGM(MODULE,IPRIN)                                          107
      IF (IPRIN.LT.0) RETURN                                              107
C Filling bank CELL - STRUCTURE CONTAINING CALORIMETER PIXELS DIMENTION   107
      BANK_TITLE='STRUCTURE CONTAINING CALORIMETER PIXELS DIMENTION'      107
      BPATH_CELL='CELL*'                                                  107
      IDNUM_CELL(1)=0                                                     108
C PIXELS DIMENTION                                                        108
      CELL_VERSION=1.0                                                    108
      CCELL_VERSION='PIXELS DIMENTION'                                    108
C PIXELS WIDTH                                                            109
      CELL_DX=3.3/2.                                                      109
      CCELL_DX='PIXELS WIDTH'                                             109
C PIXELS HEIGHT                                                           110
      CELL_DY=3.3/2.                                                      110
      CCELL_DY='PIXELS HEIGHT'                                            110
C TOWR LENGTH                                                             111
      CELL_DZ=117.0/2.                                                    111
      CCELL_DZ='TOWR LENGTH'                                              111
C NO OF CELLS IN X DIR                                                    112
      CELL_NCELLX=3                                                       112
      CCELL_NCELLX='NO OF CELLS IN X DIR'                                 112
C NO OF CELLS IN Y DIR                                                    113
      CELL_NCELLY=3                                                       113
      CCELL_NCELLY='NO OF CELLS IN Y DIR'                                 113
C NO FIBERS IN CELL 16 FIBERS                                             114
      CELL_NFIBERXL=16                                                    114
      CCELL_NFIBERXL='NO FIBERS IN CELL 16 FIBERS'                        114
C NO FIBERS IN CELL 16 FIBERS                                             115
      CELL_NFIBERYL=16                                                    115
      CCELL_NFIBERYL='NO FIBERS IN CELL 16 FIBERS'                        115
C NO FIBERS IN CELL 15 FIBERS                                             116
      CELL_NFIBERXS=15                                                    116
      CCELL_NFIBERXS='NO FIBERS IN CELL 15 FIBERS'                        116
C NO FIBERS IN CELL 15 FIBERS                                             117
      CELL_NFIBERYS=15                                                    117
      CCELL_NFIBERYS='NO FIBERS IN CELL 15 FIBERS'                        117
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CELL',    118
     *BPATH_CELL,IDNUM_CELL,1, MAP_CELL,NAMES_CELL,COMMEN_CELL,           118
     *BANK_CELL,LEN_CELL,LLEN_CELL,LINK_CELL,FLAG_CELL)                   118
C *                                                                       118
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CELL',    118
     *BPATH_CELL,IDNUM_CELL,1, MAP_CELL,NAMES_CELL,COMMEN_CELL,           118
     *BANK_CELL,LEN_CELL,LLEN_CELL,LINK_CELL,FLAG_CELL)                   118
C *                                                                       119
      CALL AGSBEGM(MODULE,IPRIN)                                          119
      IF (IPRIN.LT.0) RETURN                                              119
C Filling bank FIBR - SCINTILATING FIBES IN TOWERS/CELLS                  119
      BANK_TITLE='SCINTILATING FIBES IN TOWERS/CELLS'                     119
      BPATH_FIBR='FIBR*'                                                  119
      IDNUM_FIBR(1)=0                                                     120
C FIBER DIMENTION                                                         120
      FIBR_VERSION=1.0                                                    120
      CFIBR_VERSION='FIBER DIMENTION'                                     120
C INNER RADIUS OF FIBER                                                   121
      FIBR_RMIN=0.0                                                       121
      CFIBR_RMIN='INNER RADIUS OF FIBER'                                  121
C OUTER RADIUS OF FIBER                                                   122
      FIBR_RMAX=0.055                                                     122
      CFIBR_RMAX='OUTER RADIUS OF FIBER'                                  122
C TOWR LENGTH                                                             123
      FIBR_DZ=117.0/2.                                                    123
      CFIBR_DZ='TOWR LENGTH'                                              123
C DISTANCE FROM EDGE OR FROM ONE CELL BOUNDRY TO OTHER 1: FOR FIRST CELL  124
      FIBR_STARTX1=0.107                                                  124
      CFIBR_STARTX1='DISTANCE FROM EDGE OR FROM ONE CELL BOUNDRY TO OTHE  124
     *R 1: FOR FIRST CELL'                                                124
C DISTANCE FROM EDGE OR FROM ONE CELL BOUNDRY TO OTHER                    125
      FIBR_STARTY1=0.107                                                  125
      CFIBR_STARTY1='DISTANCE FROM EDGE OR FROM ONE CELL BOUNDRY TO OTHE  125
     *R'                                                                  125
C DISTANCE FROM EDGE OR FROM ONE CELL BOUNDRY TO OTHER 2:FOR 2ND AND 3RD  126
C  CELL                                                                   126
      FIBR_STARTX2=0.1065                                                 126
      CFIBR_STARTX2='DISTANCE FROM EDGE OR FROM ONE CELL BOUNDRY TO OTHE  126
     *R 2:FOR 2ND AND 3RD CELL'                                           126
C DISTANCE FROM EDGE OR FROM ONE CELL BOUNDRY TO OTHER                    127
      FIBR_STARTY2=0.1065                                                 127
      CFIBR_STARTY2='DISTANCE FROM EDGE OR FROM ONE CELL BOUNDRY TO OTHE  127
     *R'                                                                  127
C DISTANCE BETWEEN FIBERS CENTER TO CENTER                                128
      FIBR_STEPX=0.213                                                    128
      CFIBR_STEPX='DISTANCE BETWEEN FIBERS CENTER TO CENTER'              128
C DISTANCE BETWEEN FIBERS CENTER TO CENTER                                129
      FIBR_STEPY=0.213                                                    129
      CFIBR_STEPY='DISTANCE BETWEEN FIBERS CENTER TO CENTER'              129
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FIBR',    130
     *BPATH_FIBR,IDNUM_FIBR,1, MAP_FIBR,NAMES_FIBR,COMMEN_FIBR,           130
     *BANK_FIBR,LEN_FIBR,LLEN_FIBR,LINK_FIBR,FLAG_FIBR)                   130
C *                                                                       130
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FIBR',    130
     *BPATH_FIBR,IDNUM_FIBR,1, MAP_FIBR,NAMES_FIBR,COMMEN_FIBR,           130
     *BANK_FIBR,LEN_FIBR,LLEN_FIBR,LINK_FIBR,FLAG_FIBR)                   130
C *                                                                       131
      CALL AGSBEGM(MODULE,IPRIN)                                          131
      IF (IPRIN.LT.0) RETURN                                              131
C Filling bank PRSW - PRE SHOWER GEOMETRY                                 131
      BANK_TITLE='PRE SHOWER GEOMETRY'                                    131
      BPATH_PRSW='PRSW*'                                                  131
      IDNUM_PRSW(1)=0                                                     132
C NONE                                                                    132
      PRSW_DBOX(1)=225.0                                                  132
      PRSW_DBOX(2)=250                                                    132
      PRSW_DBOX(3)=22.0                                                   132
      CPRSW_DBOX='NONE'                                                   132
C NONE                                                                    133
      PRSW_XOFF(1)=1.0                                                    133
      PRSW_XOFF(2)=1.0                                                    133
      PRSW_XOFF(3)=1.0                                                    133
      PRSW_XOFF(4)=1.0                                                    133
      CPRSW_XOFF='NONE'                                                   133
C NONE                                                                    134
      PRSW_YOFF(1)=0.0                                                    134
      PRSW_YOFF(2)=0.0                                                    134
      PRSW_YOFF(3)=0.0                                                    134
      PRSW_YOFF(4)=0.0                                                    134
      CPRSW_YOFF='NONE'                                                   134
C NONE                                                                    135
      PRSW_ZOFF(1)=675.64                                                 135
      PRSW_ZOFF(2)=678.52                                                 135
      PRSW_ZOFF(3)=690.29                                                 135
      PRSW_ZOFF(4)=692.22                                                 135
      CPRSW_ZOFF='NONE'                                                   135
C NONE                                                                    136
      PRSW_DZ(1)=1.0                                                      136
      PRSW_DZ(2)=1.0                                                      136
      PRSW_DZ(3)=0.635                                                    136
      PRSW_DZ(4)=1.0                                                      136
      CPRSW_DZ='NONE'                                                     136
C NONE                                                                    137
      PRSW_DHOLE=20.25                                                    137
      CPRSW_DHOLE='NONE'                                                  137
C NONE                                                                    138
      PRSW_NTYPE=2                                                        138
      CPRSW_NTYPE='NONE'                                                  138
C NONE                                                                    139
      PRSW_NPRS(1)=12                                                     139
      PRSW_NPRS(2)=9                                                      139
      CPRSW_NPRS='NONE'                                                   139
C NONE                                                                    140
      PRSW_DPRS(1)=4.0                                                    140
      PRSW_DPRS(2)=5.8                                                    140
      PRSW_DPRS(3)=0.05                                                   140
      CPRSW_DPRS='NONE'                                                   140
C NONE                                                                    141
      PRSW_NSKIPV=2                                                       141
      CPRSW_NSKIPV='NONE'                                                 141
C NONE                                                                    142
      PRSW_NSKIPH=2                                                       142
      CPRSW_NSKIPH='NONE'                                                 142
C NONE                                                                    143
      PRSW_DYCUT=3.0+5.8                                                  143
      CPRSW_DYCUT='NONE'                                                  143
C NONE                                                                    144
      PRSW_DLG(1)=0.3                                                     144
      PRSW_DLG(2)=6.0                                                     144
      PRSW_DLG(3)=1.0                                                     144
      CPRSW_DLG='NONE'                                                    144
C NONE                                                                    145
      PRSW_DSIPM(1)=0.5                                                   145
      PRSW_DSIPM(2)=3.5                                                   145
      PRSW_DSIPM(3)=1.0                                                   145
      PRSW_DSIPM(4)=7.0                                                   145
      CPRSW_DSIPM='NONE'                                                  145
C NONE                                                                    147
      PRSW_DZBBD(1)=2.54/8.0                                              147
      PRSW_DZBBD(2)=2.54/4.0                                              147
      PRSW_DZBBD(3)=2*2.54/4.0                                            147
      PRSW_DZBBD(4)=2.54/4.0                                              147
      CPRSW_DZBBD='NONE'                                                  147
C NONE                                                                    148
      PRSW_DXBBD(1)=2.54                                                  148
      PRSW_DXBBD(2)=2.54+7.0                                              148
      CPRSW_DXBBD='NONE'                                                  148
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PRSW',    149
     *BPATH_PRSW,IDNUM_PRSW,1, MAP_PRSW,NAMES_PRSW,COMMEN_PRSW,           149
     *BANK_PRSW,LEN_PRSW,LLEN_PRSW,LINK_PRSW,FLAG_PRSW)                   149
C *                                                                       149
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PRSW',    149
     *BPATH_PRSW,IDNUM_PRSW,1, MAP_PRSW,NAMES_PRSW,COMMEN_PRSW,           149
     *BANK_PRSW,LEN_PRSW,LLEN_PRSW,LINK_PRSW,FLAG_PRSW)                   149
C *                                                                       150
      CALL AGSBEGM(MODULE,IPRIN)                                          150
      IF (IPRIN.LT.0) RETURN                                              150
C Filling bank WPFM - WEST PLATFORM GEOMETRY                              150
      BANK_TITLE='WEST PLATFORM GEOMETRY'                                 150
      BPATH_WPFM='WPFM*'                                                  150
      IDNUM_WPFM(1)=0                                                     152
C NONE                                                                    152
      WPFM_POLED(1)=12.0*2.54                                             152
      WPFM_POLED(2)=155.5 * 2.54                                          152
      WPFM_POLED(3)=12.0*2.54                                             152
      WPFM_POLED(4)=1.0                                                   152
      CWPFM_POLED='NONE'                                                  152
C NONE                                                                    155
      WPFM_POLEP(1)=72.0*2.54                                             155
      WPFM_POLEP(2)=(-44.36-10-155.5/2)*2.54                              155
      WPFM_POLEP(3)=807.72-27*2.54                                        155
      CWPFM_POLEP='NONE'                                                  155
C NONE                                                                    157
      WPFM_ISTRD(1)=231.0*2.54                                            157
      WPFM_ISTRD(2)=8.0*2.54                                              157
      WPFM_ISTRD(3)=8.0*2.54                                              157
      WPFM_ISTRD(4)=1.0                                                   157
      CWPFM_ISTRD='NONE'                                                  157
C NONE                                                                    159
      WPFM_IST1P(1)=-8.0*2.54                                             159
      WPFM_IST1P(2)=(-44.36-5.0)*2.54                                     159
      WPFM_IST1P(3)=807.72-37*2.54                                        159
      CWPFM_IST1P='NONE'                                                  159
C NONE                                                                    161
      WPFM_IST2P(1)=-8.0*2.54                                             161
      WPFM_IST2P(2)=(-44.36-5.0)*2.54                                     161
      WPFM_IST2P(3)=807.72-17*2.54                                        161
      CWPFM_IST2P='NONE'                                                  161
C NONE                                                                    163
      WPFM_USTFD(1)=236.0*2.54                                            163
      WPFM_USTFD(2)=12*2.54                                               163
      WPFM_USTFD(3)=4*2.54                                                163
      WPFM_USTFD(4)=1.0                                                   163
      CWPFM_USTFD='NONE'                                                  163
C NONE                                                                    165
      WPFM_USTFP(1)=-8.0*2.54                                             165
      WPFM_USTFP(2)=-44.36*2.54                                           165
      WPFM_USTFP(3)=807.72-(46-2)*2.54                                    165
      CWPFM_USTFP='NONE'                                                  165
C NONE                                                                    167
      WPFM_USTND(1)=4*2.54                                                167
      WPFM_USTND(2)=12.0*2.54                                             167
      WPFM_USTND(3)=(46-4)*2.54                                           167
      WPFM_USTND(4)=1.0                                                   167
      CWPFM_USTND='NONE'                                                  167
C NONE                                                                    169
      WPFM_USTNP(1)=-(118-2+8)*2.54                                       169
      WPFM_USTNP(2)=-44.36*2.54                                           169
      WPFM_USTNP(3)=807.72-(46-4)*2.54/2                                  169
      CWPFM_USTNP='NONE'                                                  169
C NONE                                                                    171
      WPFM_USTSD(1)=4*2.54                                                171
      WPFM_USTSD(2)=12.0*2.54                                             171
      WPFM_USTSD(3)=(46-4)*2.54                                           171
      WPFM_USTSD(4)=1.0                                                   171
      CWPFM_USTSD='NONE'                                                  171
C NONE                                                                    173
      WPFM_USTSP(1)=+(118-2-8)*2.54                                       173
      WPFM_USTSP(2)=-44.36*2.54                                           173
      WPFM_USTSP(3)=807.72-(46-4)*2.54/2                                  173
      CWPFM_USTSP='NONE'                                                  173
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'WPFM',    174
     *BPATH_WPFM,IDNUM_WPFM,1, MAP_WPFM,NAMES_WPFM,COMMEN_WPFM,           174
     *BANK_WPFM,LEN_WPFM,LLEN_WPFM,LINK_WPFM,FLAG_WPFM)                   174
C *                                                                       174
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'WPFM',    174
     *BPATH_WPFM,IDNUM_WPFM,1, MAP_WPFM,NAMES_WPFM,COMMEN_WPFM,           174
     *BANK_WPFM,LEN_WPFM,LLEN_WPFM,LINK_WPFM,FLAG_WPFM)                   174
C use MCDT bank                                                           175
      BPATH_MCDT='MCDT*'                                                  175
      IDNUM_MCDT(1)=0                                                     175
      MCDT_VERSION=1.0                                                    175
      CALL AGDGETP('HCALGEO1','*',-999,'MCDT',NMCDT_VERSION,              175
     *RE_MCDT_VERSION,BPATH_MCDT,IDNUM_MCDT,1,MAP_MCDT,NAMES_MCDT,        175
     *BANK_MCDT,LEN_MCDT,LLEN_MCDT,LINK_MCDT,FLAG_MCDT)                   175
C use TOWR bank                                                           176
      BPATH_TOWR='TOWR*'                                                  176
      IDNUM_TOWR(1)=0                                                     176
      TOWR_VERSION=1.0                                                    176
      CALL AGDGETP('HCALGEO1','*',-999,'TOWR',NTOWR_VERSION,              176
     *RE_TOWR_VERSION,BPATH_TOWR,IDNUM_TOWR,1,MAP_TOWR,NAMES_TOWR,        176
     *BANK_TOWR,LEN_TOWR,LLEN_TOWR,LINK_TOWR,FLAG_TOWR)                   176
C use CELL bank                                                           177
      BPATH_CELL='CELL*'                                                  177
      IDNUM_CELL(1)=0                                                     177
      CELL_VERSION=1.0                                                    177
      CALL AGDGETP('HCALGEO1','*',-999,'CELL',NCELL_VERSION,              177
     *RE_CELL_VERSION,BPATH_CELL,IDNUM_CELL,1,MAP_CELL,NAMES_CELL,        177
     *BANK_CELL,LEN_CELL,LLEN_CELL,LINK_CELL,FLAG_CELL)                   177
C use FIBR bank                                                           178
      BPATH_FIBR='FIBR*'                                                  178
      IDNUM_FIBR(1)=0                                                     178
      FIBR_VERSION=1.0                                                    178
      CALL AGDGETP('HCALGEO1','*',-999,'FIBR',NFIBR_VERSION,              178
     *RE_FIBR_VERSION,BPATH_FIBR,IDNUM_FIBR,1,MAP_FIBR,NAMES_FIBR,        178
     *BANK_FIBR,LEN_FIBR,LLEN_FIBR,LINK_FIBR,FLAG_FIBR)                   178
      CALL AGSBEGM(MODULE,IPRIN)                                          179
C Check IPRIN<0                                                           179
      IF (IPRIN.LT.0) RETURN                                              179
C Create HCMO :                                                           179
C Call To Local Procedure HCMO                                            179
      ASSIGN 5020 TO BLOCK_HCMO                                           179
      GO TO  5010                                                         179
5020  CONTINUE                                                            179
C *                                                                       179
C Geant POSITION : HCMO ===> MOTHER='CAVE' X=MCDT_X Y=MCDT_Y Z=MCDT_Z <=  180
C ==                                                                      180
      CALL AGSRESET                                                       180
      AG_MOTHER = 'CAVE'                                                  180
      AG_X = MCDT_X                                                       180
      AG_Y = MCDT_Y                                                       180
      AG_Z = MCDT_Z                                                       180
      AG_TITLE = 'HCMO'                                                   180
      AG_EXNAME = 'POSITION'                                              180
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         180
      CALL AXPOSITION                                                     180
C *                                                                       180
      CALL AGSBEGM(MODULE,IPRIN)                                          180
C Check IPRIN<0                                                           181
      IF (IPRIN.LT.0) RETURN                                              181
C Create FPRS :                                                           181
C Call To Local Procedure FPRS                                            181
      ASSIGN 5040 TO BLOCK_FPRS                                           181
      GO TO  5030                                                         181
5040  CONTINUE                                                            181
C *                                                                       181
C Geant POSITION : FPRS ===> MOTHER='CAVE' X=0.0 Y=0.0 Z=PRSW_ZOFF(1)-PR  183
C SW_DSIPM(1)+PRSW_DBOX(3)/2.0 KONLY='MANY' <===                          183
      CALL AGSRESET                                                       183
      AG_MOTHER = 'CAVE'                                                  183
      AG_X = 0.0                                                          183
      AG_Y = 0.0                                                          183
      AG_Z = PRSW_ZOFF(1)-PRSW_DSIPM(1)+PRSW_DBOX(3)/2.0                  183
      AG_KONLY = 'MANY'                                                   183
      AG_TITLE = 'FPRS'                                                   183
      AG_EXNAME = 'POSITION'                                              183
      AG_PARLIST = 'MOTHER_X_Y_Z_KONLY'                                   183
      CALL AXPOSITION                                                     183
C *                                                                       183
      CALL AGSBEGM(MODULE,IPRIN)                                          183
C Check IPRIN<0                                                           184
      IF (IPRIN.LT.0) RETURN                                              184
C Create WPPL :                                                           184
C Call To Local Procedure WPPL                                            184
      ASSIGN 5060 TO BLOCK_WPPL                                           184
      GO TO  5050                                                         184
5060  CONTINUE                                                            184
C *                                                                       184
C Geant POSITION : WPPL ===> MOTHER='CAVE' X=+WPFM_POLEP(1) Y=WPFM_POLEP  185
C (2) Z=WPFM_POLEP(3) <===                                                185
      CALL AGSRESET                                                       185
      AG_MOTHER = 'CAVE'                                                  185
      AG_X = +WPFM_POLEP(1)                                               185
      AG_Y = WPFM_POLEP(2)                                                185
      AG_Z = WPFM_POLEP(3)                                                185
      AG_TITLE = 'WPPL'                                                   185
      AG_EXNAME = 'POSITION'                                              185
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         185
      CALL AXPOSITION                                                     185
C *                                                                       185
C Geant POSITION : WPPL ===> MOTHER='CAVE' X=-WPFM_POLEP(1) Y=WPFM_POLEP  185
C (2) Z=WPFM_POLEP(3) <===                                                185
      CALL AGSRESET                                                       186
      AG_MOTHER = 'CAVE'                                                  186
      AG_X = -WPFM_POLEP(1)                                               186
      AG_Y = WPFM_POLEP(2)                                                186
      AG_Z = WPFM_POLEP(3)                                                186
      AG_TITLE = 'WPPL'                                                   186
      AG_EXNAME = 'POSITION'                                              186
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         186
      CALL AXPOSITION                                                     186
C *                                                                       186
      CALL AGSBEGM(MODULE,IPRIN)                                          186
C Check IPRIN<0                                                           187
      IF (IPRIN.LT.0) RETURN                                              187
C Create WPIS :                                                           187
C Call To Local Procedure WPIS                                            187
      ASSIGN 5080 TO BLOCK_WPIS                                           187
      GO TO  5070                                                         187
5080  CONTINUE                                                            187
C *                                                                       187
C Geant POSITION : WPIS ===> MOTHER='CAVE' X=WPFM_IST1P(1) Y=WPFM_IST1P(  188
C 2) Z=WPFM_IST1P(3) <===                                                 188
      CALL AGSRESET                                                       188
      AG_MOTHER = 'CAVE'                                                  188
      AG_X = WPFM_IST1P(1)                                                188
      AG_Y = WPFM_IST1P(2)                                                188
      AG_Z = WPFM_IST1P(3)                                                188
      AG_TITLE = 'WPIS'                                                   188
      AG_EXNAME = 'POSITION'                                              188
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         188
      CALL AXPOSITION                                                     188
C *                                                                       188
C Geant POSITION : WPIS ===> MOTHER='CAVE' X=WPFM_IST2P(1) Y=WPFM_IST2P(  188
C 2) Z=WPFM_IST2P(3) <===                                                 188
      CALL AGSRESET                                                       189
      AG_MOTHER = 'CAVE'                                                  189
      AG_X = WPFM_IST2P(1)                                                189
      AG_Y = WPFM_IST2P(2)                                                189
      AG_Z = WPFM_IST2P(3)                                                189
      AG_TITLE = 'WPIS'                                                   189
      AG_EXNAME = 'POSITION'                                              189
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         189
      CALL AXPOSITION                                                     189
C *                                                                       189
      CALL AGSBEGM(MODULE,IPRIN)                                          189
C Check IPRIN<0                                                           190
      IF (IPRIN.LT.0) RETURN                                              190
C Create WPUF :                                                           190
C Call To Local Procedure WPUF                                            190
      ASSIGN 5100 TO BLOCK_WPUF                                           190
      GO TO  5090                                                         190
5100  CONTINUE                                                            190
C *                                                                       190
C Geant POSITION : WPUF ===> MOTHER='CAVE' X=+WPFM_USTFP(1) Y=WPFM_USTFP  191
C (2) Z=WPFM_USTFP(3) <===                                                191
      CALL AGSRESET                                                       191
      AG_MOTHER = 'CAVE'                                                  191
      AG_X = +WPFM_USTFP(1)                                               191
      AG_Y = WPFM_USTFP(2)                                                191
      AG_Z = WPFM_USTFP(3)                                                191
      AG_TITLE = 'WPUF'                                                   191
      AG_EXNAME = 'POSITION'                                              191
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         191
      CALL AXPOSITION                                                     191
C *                                                                       191
      CALL AGSBEGM(MODULE,IPRIN)                                          191
C Check IPRIN<0                                                           192
      IF (IPRIN.LT.0) RETURN                                              192
C Create WPUN :                                                           192
C Call To Local Procedure WPUN                                            192
      ASSIGN 5120 TO BLOCK_WPUN                                           192
      GO TO  5110                                                         192
5120  CONTINUE                                                            192
C *                                                                       192
C Geant POSITION : WPUN ===> MOTHER='CAVE' X=+WPFM_USTNP(1) Y=WPFM_USTNP  193
C (2) Z=WPFM_USTNP(3) <===                                                193
      CALL AGSRESET                                                       193
      AG_MOTHER = 'CAVE'                                                  193
      AG_X = +WPFM_USTNP(1)                                               193
      AG_Y = WPFM_USTNP(2)                                                193
      AG_Z = WPFM_USTNP(3)                                                193
      AG_TITLE = 'WPUN'                                                   193
      AG_EXNAME = 'POSITION'                                              193
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         193
      CALL AXPOSITION                                                     193
C *                                                                       193
      CALL AGSBEGM(MODULE,IPRIN)                                          193
C Check IPRIN<0                                                           194
      IF (IPRIN.LT.0) RETURN                                              194
C Create WPUS :                                                           194
C Call To Local Procedure WPUS                                            194
      ASSIGN 5140 TO BLOCK_WPUS                                           194
      GO TO  5130                                                         194
5140  CONTINUE                                                            194
C *                                                                       194
C Geant POSITION : WPUS ===> MOTHER='CAVE' X=+WPFM_USTSP(1) Y=WPFM_USTSP  195
C (2) Z=WPFM_USTSP(3) <===                                                195
      CALL AGSRESET                                                       195
      AG_MOTHER = 'CAVE'                                                  195
      AG_X = +WPFM_USTSP(1)                                               195
      AG_Y = WPFM_USTSP(2)                                                195
      AG_Z = WPFM_USTSP(3)                                                195
      AG_TITLE = 'WPUS'                                                   195
      AG_EXNAME = 'POSITION'                                              195
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         195
      CALL AXPOSITION                                                     195
C *                                                                       195
C ----------------------------------------------------------------------  195
C ------------ HCMO                                                       195
      CALL AGSENDM                                                        197
      RETURN                                                              197
C Begin Local Procedure HCMO                                              197
C HCMO MOTHER CALORIMETER                                                 197
5010  CONTINUE                                                            197
         AG_TITLE='HCMO'                                                  197
         AG_MODULE=MODULE                                                 197
         AG_MARK='V00'                                                    197
         CALL AGSPUSH                                                     197
         AG_MODULE=MODULE                                                 197
C    *                                                                    197
C    Geant MATERIAL : AIR ===> inherited arguments used <===              198
         CALL AGSRESET                                                    198
         AG_TITLE = 'AIR'                                                 198
         AG_EXNAME = 'MATERIAL'                                           198
         AG_PARLIST = 'NONE'                                              198
         CALL AXMATERIAL                                                  198
C    *                                                                    198
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           198
         CALL AGSRESET                                                    199
         AG_TITLE = 'STANDARD'                                            199
         AG_EXNAME = 'MEDIUM'                                             199
         AG_PARLIST = 'NONE'                                              199
         CALL AXMEDIUM                                                    199
C    *                                                                    199
C    Geant ATTRIBUTE : NONE ===> SEEN=0 COLO=3 <===                       199
         CALL AGSRESET                                                    200
         AG_SEEN = 0                                                      200
         AG_COLO = 3                                                      200
         AG_TITLE = 'NONE'                                                200
         AG_EXNAME = 'ATTRIBUTE'                                          200
         AG_PARLIST = 'SEEN_COLO'                                         200
         CALL AXATTRIBUTE                                                 200
C    *                                                                    200
C    Geant SHAPE : BOX ===> DX=MCDT_DX DY=MCDT_DY DZ=MCDT_DZ <===         200
         CALL AGSRESET                                                    201
         AG_DX = MCDT_DX                                                  201
         AG_DY = MCDT_DY                                                  201
         AG_DZ = MCDT_DZ                                                  201
         AG_TITLE = 'BOX'                                                 201
         AG_EXNAME = 'SHAPE'                                              201
         AG_PARLIST = 'DX_DY_DZ'                                          201
         CALL AXSHAPE                                                     201
C    *                                                                    201
         MMI=0                                                            201
         MMJ=4                                                            202
         NNI=0                                                            203
         NNJ=13                                                           204
         OOI=16                                                           205
         OOJ=1                                                            206
         PPI=16                                                           207
         PPJ=16                                                           208
         Q1I=9                                                            209
         Q1J=8                                                            209
         Q2I=9                                                            210
         Q2J=9                                                            210
         Q3I=10                                                           211
         Q3J=8                                                            211
         Q4I=10                                                           212
         Q4J=9                                                            212
C    *                                                                    214
         I=0                                                              214
            GO TO 5153                                                    214
5151        I=I+1                                                         214
5153        IF(I-(TOWR_NTOWERX-1).GT.0) GO TO 5152                        214
C       *                                                                 215
            J=0                                                           215
               GO TO 5163                                                 215
5161           J=J+1                                                      215
5163           IF(J-(TOWR_NTOWERY-1).GT.0) GO TO 5162                     215
               FLAG1 = ( (I.EQ.MMI) .AND. (J.LT.MMJ) ) .OR. (             216
     *         (I.EQ.NNI) .AND. (J.GT.NNJ) ) .OR. ( (I.EQ.OOI) .AND.      216
     *         (J.LT.OOJ) ) .OR. ( (I.EQ.PPI) .AND. (J.GT.PPJ) )          216
               FLAG2 = ( (I.EQ.Q1I ) .AND. ( J.EQ.Q1J) ) .OR. (           219
     *         (I.EQ.Q2I ) .AND. ( J.EQ.Q2J) ) .OR. ( (I.EQ.Q3I ) .AND.   219
     *         ( J.EQ.Q3J) ) .OR. ( (I.EQ.Q4I ) .AND. ( J.EQ.Q4J) )       219
               FLAG = FLAG1 + FLAG2                                       220
               FLAG3 = ( (I.EQ.MMI) .AND. (J.LT.MMJ) ) .OR. (             222
     *         (I.EQ.OOI) .AND. (J.LT.OOJ) )                              222
C          Check (FLAG.EQ.0)                                              224
               IF ((FLAG.EQ.0)) THEN                                      224
C          Create HTWR :                                                  225
C          Call To Local Procedure HTWR                                   225
               ASSIGN 5180 TO BLOCK_HTWR                                  225
               GO TO  5170                                                225
5180           CONTINUE                                                   225
C          *                                                              225
C          Geant POSITION : HTWR ===> X=-(((TOWR_NTOWERX/2)*TOWR_DX*2)-T  227
C          OWR_DX)+(I*TOWR_DX*2) Y=-(((TOWR_NTOWERY/2)*TOWR_DY*2)-TOWR_D  227
C          Y)+(J*TOWR_DY*2) Z=0.0 <===                                    227
               CALL AGSRESET                                              227
               AG_X = -(((TOWR_NTOWERX/                                   227
     *         2)*TOWR_DX*2)-TOWR_DX)+(I*TOWR_DX*2)                       227
               AG_Y = -(((TOWR_NTOWERY/                                   227
     *         2)*TOWR_DY*2)-TOWR_DY)+(J*TOWR_DY*2)                       227
               AG_Z = 0.0                                                 227
               AG_TITLE = 'HTWR'                                          227
               AG_EXNAME = 'POSITION'                                     227
               AG_PARLIST = 'X_Y_Z'                                       227
               CALL AXPOSITION                                            227
C          *                                                              227
               ENDIF                                                      227
C          Check (FLAG3.EQ.1)                                             229
               IF ((FLAG3.EQ.1)) THEN                                     229
C          Create HACL :                                                  230
C          Call To Local Procedure HACL                                   230
               ASSIGN 5200 TO BLOCK_HACL                                  230
               GO TO  5190                                                230
5200           CONTINUE                                                   230
C          *                                                              230
C          Geant POSITION : HACL ===> X=-(((TOWR_NTOWERX/2)*TOWR_DX*2)-T  232
C          OWR_DX)+(I*TOWR_DX*2) Y=-(((TOWR_NTOWERY/2)*TOWR_DY*2)-TOWR_D  232
C          Y)+(J*TOWR_DY*2) Z=0.0 <===                                    232
               CALL AGSRESET                                              232
               AG_X = -(((TOWR_NTOWERX/                                   232
     *         2)*TOWR_DX*2)-TOWR_DX)+(I*TOWR_DX*2)                       232
               AG_Y = -(((TOWR_NTOWERY/                                   232
     *         2)*TOWR_DY*2)-TOWR_DY)+(J*TOWR_DY*2)                       232
               AG_Z = 0.0                                                 232
               AG_TITLE = 'HACL'                                          232
               AG_EXNAME = 'POSITION'                                     232
               AG_PARLIST = 'X_Y_Z'                                       232
               CALL AXPOSITION                                            232
C          *                                                              232
               ENDIF                                                      232
            GO TO 5161                                                    234
5162        CONTINUE                                                      234
C       Check I.EQ.MMI                                                    235
            IF (I.EQ.MMI) THEN                                            235
            MMI=MMI+1                                                     235
            MMJ=MMJ-1                                                     236
            ENDIF                                                         238
C       Check I.EQ.NNI                                                    239
            IF (I.EQ.NNI) THEN                                            239
            NNI=NNI+1                                                     239
            NNJ=NNJ+1                                                     240
            ENDIF                                                         242
C       Check I.EQ.OOI                                                    243
            IF (I.EQ.OOI) THEN                                            243
            OOI=OOI+1                                                     243
            OOJ=OOJ+1                                                     244
            ENDIF                                                         246
C       Check I.EQ.PPI                                                    247
            IF (I.EQ.PPI) THEN                                            247
            PPI=PPI+1                                                     247
            PPJ=PPJ-1                                                     248
            ENDIF                                                         250
         GO TO 5151                                                       251
5152     CONTINUE                                                         251
5011  CALL AGSPOPB                                                        252
      GO TO BLOCK_HCMO, (5010 ,5020)                                      252
C End of Local Procedure                                                  252
C ----------------------------------------------------------------------  253
C ------------ HTWR                                                       253
C Begin Local Procedure HTWR                                              254
C HTWR TOWRS OF CALORIMETER                                               254
5170  CONTINUE                                                            254
         AG_TITLE='HTWR'                                                  254
         AG_MODULE=MODULE                                                 254
         AG_MARK='V00'                                                    254
         CALL AGSPUSH                                                     254
         AG_MODULE=MODULE                                                 254
C    *                                                                    254
C    Geant MATERIAL : LEAD ===> inherited arguments used <===             255
         CALL AGSRESET                                                    255
         AG_TITLE = 'LEAD'                                                255
         AG_EXNAME = 'MATERIAL'                                           255
         AG_PARLIST = 'NONE'                                              255
         CALL AXMATERIAL                                                  255
C    *                                                                    255
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           255
         CALL AGSRESET                                                    256
         AG_TITLE = 'STANDARD'                                            256
         AG_EXNAME = 'MEDIUM'                                             256
         AG_PARLIST = 'NONE'                                              256
         CALL AXMEDIUM                                                    256
C    *                                                                    256
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=3 <===                       256
         CALL AGSRESET                                                    257
         AG_SEEN = 1                                                      257
         AG_COLO = 3                                                      257
         AG_TITLE = 'NONE'                                                257
         AG_EXNAME = 'ATTRIBUTE'                                          257
         AG_PARLIST = 'SEEN_COLO'                                         257
         CALL AXATTRIBUTE                                                 257
C    *                                                                    257
C    Geant SHAPE : BOX ===> DX=TOWR_DX DY=TOWR_DY DZ=TOWR_DZ <===         257
         CALL AGSRESET                                                    258
         AG_DX = TOWR_DX                                                  258
         AG_DY = TOWR_DY                                                  258
         AG_DZ = TOWR_DZ                                                  258
         AG_TITLE = 'BOX'                                                 258
         AG_EXNAME = 'SHAPE'                                              258
         AG_PARLIST = 'DX_DY_DZ'                                          258
         CALL AXSHAPE                                                     258
C    *                                                                    258
         XTEMP=FIBR_STARTX1                                               258
C    *                                                                    260
         M=0                                                              260
            GO TO 5213                                                    260
5211        M=M+1                                                         260
5213        IF(M-(CELL_NCELLX-1).GT.0) GO TO 5212                         260
            YTEMP=FIBR_STARTY1                                            260
C       *                                                                 262
            L=0                                                           262
               GO TO 5223                                                 262
5221           L=L+1                                                      262
5223           IF(L-(CELL_NCELLY-1).GT.0) GO TO 5222                      262
C          Check M.EQ.L                                                   263
               IF (M.EQ.L) THEN                                           263
C          Create HCES :                                                  264
C          Call To Local Procedure HCES                                   264
               ASSIGN 5240 TO BLOCK_HCES                                  264
               GO TO  5230                                                264
5240           CONTINUE                                                   264
C          *                                                              264
C          Geant POSITION : HCES ===> X=-3.33+(3.33*M) Y=-3.33+(L*3.33)   265
C          Z=0.0 <===                                                     265
               CALL AGSRESET                                              265
               AG_X = -3.33+(3.33*M)                                      265
               AG_Y = -3.33+(L*3.33)                                      265
               AG_Z = 0.0                                                 265
               AG_TITLE = 'HCES'                                          265
               AG_EXNAME = 'POSITION'                                     265
               AG_PARLIST = 'X_Y_Z'                                       265
               CALL AXPOSITION                                            265
C          *                                                              265
               ELSE                                                       265
C          Create HCEL :                                                  267
C          Call To Local Procedure HCEL                                   267
               ASSIGN 5260 TO BLOCK_HCEL                                  267
               GO TO  5250                                                267
5260           CONTINUE                                                   267
C          *                                                              267
C          Geant POSITION : HCEL ===> X=-3.33+(3.33*M) Y=-3.33+(L*3.33)   268
C          Z=0.0 <===                                                     268
               CALL AGSRESET                                              268
               AG_X = -3.33+(3.33*M)                                      268
               AG_Y = -3.33+(L*3.33)                                      268
               AG_Z = 0.0                                                 268
               AG_TITLE = 'HCEL'                                          268
               AG_EXNAME = 'POSITION'                                     268
               AG_PARLIST = 'X_Y_Z'                                       268
               CALL AXPOSITION                                            268
C          *                                                              268
               ENDIF                                                      268
               YTEMP=FIBR_STARTY2                                         269
            GO TO 5221                                                    271
5222        CONTINUE                                                      271
            XTEMP=FIBR_STARTX2                                            271
         GO TO 5211                                                       273
5212     CONTINUE                                                         273
5171  CALL AGSPOPB                                                        274
      GO TO BLOCK_HTWR, (5170 ,5180)                                      274
C End of Local Procedure                                                  274
C ----------------------------------------------------------------------  275
C ------------ HCES                                                       275
C Begin Local Procedure HCES                                              276
C HCES 3*3 CELL IN TOWER, HCES HAS 15*15 FIBERS                           276
5230  CONTINUE                                                            276
         AG_TITLE='HCES'                                                  276
         AG_MODULE=MODULE                                                 276
         AG_MARK='V00'                                                    276
         CALL AGSPUSH                                                     276
         AG_MODULE=MODULE                                                 276
C    *                                                                    276
C    Geant MATERIAL : LEAD ===> inherited arguments used <===             277
         CALL AGSRESET                                                    277
         AG_TITLE = 'LEAD'                                                277
         AG_EXNAME = 'MATERIAL'                                           277
         AG_PARLIST = 'NONE'                                              277
         CALL AXMATERIAL                                                  277
C    *                                                                    277
C    Geant MATERIAL : LEAD ===> ISVOL=5 <===                              277
         CALL AGSRESET                                                    278
         AG_ISVOL = 5                                                     278
         AG_TITLE = 'LEAD'                                                278
         AG_EXNAME = 'MATERIAL'                                           278
         AG_PARLIST = 'ISVOL'                                             278
         CALL AXMATERIAL                                                  278
C    *                                                                    278
C    Geant ATTRIBUTE : NONE ===> SEEN=0 COLO=3 <===                       278
         CALL AGSRESET                                                    279
         AG_SEEN = 0                                                      279
         AG_COLO = 3                                                      279
         AG_TITLE = 'NONE'                                                279
         AG_EXNAME = 'ATTRIBUTE'                                          279
         AG_PARLIST = 'SEEN_COLO'                                         279
         CALL AXATTRIBUTE                                                 279
C    *                                                                    279
C    Geant SHAPE : BOX ===> DX=CELL_DX DY=CELL_DY DZ=CELL_DZ <===         279
         CALL AGSRESET                                                    280
         AG_DX = CELL_DX                                                  280
         AG_DY = CELL_DY                                                  280
         AG_DZ = CELL_DZ                                                  280
         AG_TITLE = 'BOX'                                                 280
         AG_EXNAME = 'SHAPE'                                              280
         AG_PARLIST = 'DX_DY_DZ'                                          280
         CALL AXSHAPE                                                     280
C    *                                                                    280
C    Create HCSX :                                                        281
C    Call To Local Procedure HCSX                                         281
         ASSIGN 5280 TO BLOCK_HCSX                                        281
         GO TO  5270                                                      281
5280     CONTINUE                                                         281
C    *                                                                    281
C    Defining Hits: HCAL:0:S(0,100.0) ELOS:0:S(0,100.0) BIRK:0:S(0,100.0  282
C    )                                                                    282
         AG_TITLE = 'HCES'                                                282
         AG_EXNAME = 'HITS'                                               282
         AG_TYPE=AG_MAGIC                                                 282
         CALL AGSDETV('HCES')                                             282
C    *                                                                    282
         AG_PARLIST='HCAL'                                                282
         AG_TITLE = 'DETH'                                                282
         AG_NBITS = 0                                                     282
         AG_BIN=AG_MAGIC                                                  282
         AG_OPTION='S'                                                    282
         AG_FHMIN = 0                                                     282
         AG_FHMAX = 100.0                                                 282
         CALL AGSDETH (0)                                                 282
C    *                                                                    282
         AG_PARLIST='ELOS'                                                282
         AG_TITLE = 'DETH'                                                282
         AG_NBITS = 0                                                     282
         AG_BIN=AG_MAGIC                                                  282
         AG_OPTION='S'                                                    282
         AG_FHMIN = 0                                                     282
         AG_FHMAX = 100.0                                                 282
         CALL AGSDETH (0)                                                 282
C    *                                                                    282
         AG_PARLIST='BIRK'                                                282
         AG_TITLE = 'DETH'                                                282
         AG_NBITS = 0                                                     282
         AG_BIN=AG_MAGIC                                                  282
         AG_OPTION='S'                                                    282
         AG_FHMIN = 0                                                     282
         AG_FHMAX = 100.0                                                 282
         CALL AGSDETH (0)                                                 282
C    *                                                                    282
         CALL AGSDETU('HCES')                                             282
C    *                                                                    282
5231  CALL AGSPOPB                                                        283
      GO TO BLOCK_HCES, (5230 ,5240)                                      283
C End of Local Procedure                                                  283
C ----------------------------------------------------------------------  284
C ------------ HCEL                                                       284
C Begin Local Procedure HCEL                                              285
C HCEL 3*3 CELL IN TOWER, HCES HAS 16*16 FIBERS                           285
5250  CONTINUE                                                            285
         AG_TITLE='HCEL'                                                  285
         AG_MODULE=MODULE                                                 285
         AG_MARK='V00'                                                    285
         CALL AGSPUSH                                                     285
         AG_MODULE=MODULE                                                 285
C    *                                                                    285
C    Geant MATERIAL : LEAD ===> inherited arguments used <===             286
         CALL AGSRESET                                                    286
         AG_TITLE = 'LEAD'                                                286
         AG_EXNAME = 'MATERIAL'                                           286
         AG_PARLIST = 'NONE'                                              286
         CALL AXMATERIAL                                                  286
C    *                                                                    286
C    Geant MATERIAL : LEAD ===> ISVOL=5 <===                              286
         CALL AGSRESET                                                    287
         AG_ISVOL = 5                                                     287
         AG_TITLE = 'LEAD'                                                287
         AG_EXNAME = 'MATERIAL'                                           287
         AG_PARLIST = 'ISVOL'                                             287
         CALL AXMATERIAL                                                  287
C    *                                                                    287
C    Geant ATTRIBUTE : NONE ===> SEEN=0 COLO=3 <===                       287
         CALL AGSRESET                                                    288
         AG_SEEN = 0                                                      288
         AG_COLO = 3                                                      288
         AG_TITLE = 'NONE'                                                288
         AG_EXNAME = 'ATTRIBUTE'                                          288
         AG_PARLIST = 'SEEN_COLO'                                         288
         CALL AXATTRIBUTE                                                 288
C    *                                                                    288
C    Geant SHAPE : BOX ===> DX=CELL_DX DY=CELL_DY DZ=CELL_DZ <===         288
         CALL AGSRESET                                                    289
         AG_DX = CELL_DX                                                  289
         AG_DY = CELL_DY                                                  289
         AG_DZ = CELL_DZ                                                  289
         AG_TITLE = 'BOX'                                                 289
         AG_EXNAME = 'SHAPE'                                              289
         AG_PARLIST = 'DX_DY_DZ'                                          289
         CALL AXSHAPE                                                     289
C    *                                                                    289
C    Create HCLX :                                                        290
C    Call To Local Procedure HCLX                                         290
         ASSIGN 5300 TO BLOCK_HCLX                                        290
         GO TO  5290                                                      290
5300     CONTINUE                                                         290
C    *                                                                    290
C    Defining Hits: HCAL:0:S(0,100.0) ELOS:0:S(0,100.0) BIRK:0:S(0,100.0  291
C    )                                                                    291
         AG_TITLE = 'HCEL'                                                291
         AG_EXNAME = 'HITS'                                               291
         AG_TYPE=AG_MAGIC                                                 291
         CALL AGSDETV('HCEL')                                             291
C    *                                                                    291
         AG_PARLIST='HCAL'                                                291
         AG_TITLE = 'DETH'                                                291
         AG_NBITS = 0                                                     291
         AG_BIN=AG_MAGIC                                                  291
         AG_OPTION='S'                                                    291
         AG_FHMIN = 0                                                     291
         AG_FHMAX = 100.0                                                 291
         CALL AGSDETH (0)                                                 291
C    *                                                                    291
         AG_PARLIST='ELOS'                                                291
         AG_TITLE = 'DETH'                                                291
         AG_NBITS = 0                                                     291
         AG_BIN=AG_MAGIC                                                  291
         AG_OPTION='S'                                                    291
         AG_FHMIN = 0                                                     291
         AG_FHMAX = 100.0                                                 291
         CALL AGSDETH (0)                                                 291
C    *                                                                    291
         AG_PARLIST='BIRK'                                                291
         AG_TITLE = 'DETH'                                                291
         AG_NBITS = 0                                                     291
         AG_BIN=AG_MAGIC                                                  291
         AG_OPTION='S'                                                    291
         AG_FHMIN = 0                                                     291
         AG_FHMAX = 100.0                                                 291
         CALL AGSDETH (0)                                                 291
C    *                                                                    291
         CALL AGSDETU('HCEL')                                             291
C    *                                                                    291
5251  CALL AGSPOPB                                                        292
      GO TO BLOCK_HCEL, (5250 ,5260)                                      292
C End of Local Procedure                                                  292
C ----------------------------------------------------------------------  293
C ------------ HCSX                                                       293
C Begin Local Procedure HCSX                                              294
C HCSX 3*3 CELL IN TOWER X DIVISIONS                                      294
5270  CONTINUE                                                            294
         AG_TITLE='HCSX'                                                  294
         AG_MODULE=MODULE                                                 294
         AG_MARK='V00'                                                    294
         CALL AGSPUSH                                                     294
         AG_MODULE=MODULE                                                 294
C    *                                                                    294
C    Geant MATERIAL : LEAD ===> ISVOL=0 <===                              295
         CALL AGSRESET                                                    295
         AG_ISVOL = 0                                                     295
         AG_TITLE = 'LEAD'                                                295
         AG_EXNAME = 'MATERIAL'                                           295
         AG_PARLIST = 'ISVOL'                                             295
         CALL AXMATERIAL                                                  295
C    *                                                                    295
C    Geant ATTRIBUTE : NONE ===> SEEN=0 COLO=3 <===                       295
         CALL AGSRESET                                                    296
         AG_SEEN = 0                                                      296
         AG_COLO = 3                                                      296
         AG_TITLE = 'NONE'                                                296
         AG_EXNAME = 'ATTRIBUTE'                                          296
         AG_PARLIST = 'SEEN_COLO'                                         296
         CALL AXATTRIBUTE                                                 296
C    *                                                                    296
C    Geant SHAPE : DIVISION ===> NDIV=CELL_NFIBERXS IAXIS=1 <===          296
         CALL AGSRESET                                                    297
         AG_NDIV = CELL_NFIBERXS                                          297
         AG_IAXIS = 1                                                     297
         AG_TITLE = 'DIVISION'                                            297
         AG_EXNAME = 'SHAPE'                                              297
         AG_PARLIST = 'NDIV_IAXIS'                                        297
         CALL AXSHAPE                                                     297
C    *                                                                    297
C    Create HCSY :                                                        298
C    Call To Local Procedure HCSY                                         298
         ASSIGN 5320 TO BLOCK_HCSY                                        298
         GO TO  5310                                                      298
5320     CONTINUE                                                         298
C    *                                                                    298
5271  CALL AGSPOPB                                                        299
      GO TO BLOCK_HCSX, (5270 ,5280)                                      299
C End of Local Procedure                                                  299
C ----------------------------------------------------------------------  300
C ------------ HCSY                                                       300
C Begin Local Procedure HCSY                                              301
C HCSY 3*3 CELL IN TOWER Y DIVISIONS                                      301
5310  CONTINUE                                                            301
         AG_TITLE='HCSY'                                                  301
         AG_MODULE=MODULE                                                 301
         AG_MARK='V00'                                                    301
         CALL AGSPUSH                                                     301
         AG_MODULE=MODULE                                                 301
C    *                                                                    301
C    Geant MATERIAL : LEAD ===> ISVOL=0 <===                              302
         CALL AGSRESET                                                    302
         AG_ISVOL = 0                                                     302
         AG_TITLE = 'LEAD'                                                302
         AG_EXNAME = 'MATERIAL'                                           302
         AG_PARLIST = 'ISVOL'                                             302
         CALL AXMATERIAL                                                  302
C    *                                                                    302
C    Geant ATTRIBUTE : NONE ===> SEEN=0 COLO=3 <===                       302
         CALL AGSRESET                                                    303
         AG_SEEN = 0                                                      303
         AG_COLO = 3                                                      303
         AG_TITLE = 'NONE'                                                303
         AG_EXNAME = 'ATTRIBUTE'                                          303
         AG_PARLIST = 'SEEN_COLO'                                         303
         CALL AXATTRIBUTE                                                 303
C    *                                                                    303
C    Geant SHAPE : DIVISION ===> NDIV=CELL_NFIBERYS IAXIS=2 <===          303
         CALL AGSRESET                                                    304
         AG_NDIV = CELL_NFIBERYS                                          304
         AG_IAXIS = 2                                                     304
         AG_TITLE = 'DIVISION'                                            304
         AG_EXNAME = 'SHAPE'                                              304
         AG_PARLIST = 'NDIV_IAXIS'                                        304
         CALL AXSHAPE                                                     304
C    *                                                                    304
C    Create HFIB :                                                        305
C    Call To Local Procedure HFIB                                         305
         ASSIGN 5340 TO BLOCK_HFIB                                        305
         GO TO  5330                                                      305
5340     CONTINUE                                                         305
C    *                                                                    305
C    Geant POSITION : HFIB ===> X=0 Y=0 Z=0.0 <===                        306
         CALL AGSRESET                                                    306
         AG_X = 0                                                         306
         AG_Y = 0                                                         306
         AG_Z = 0.0                                                       306
         AG_TITLE = 'HFIB'                                                306
         AG_EXNAME = 'POSITION'                                           306
         AG_PARLIST = 'X_Y_Z'                                             306
         CALL AXPOSITION                                                  306
C    *                                                                    306
5311  CALL AGSPOPB                                                        307
      GO TO BLOCK_HCSY, (5310 ,5320)                                      307
C End of Local Procedure                                                  307
C ----------------------------------------------------------------------  308
C ------------ HCLX                                                       308
C Begin Local Procedure HCLX                                              309
C HCLX 3*3 CELL IN TOWER X DIVISIONS                                      309
5290  CONTINUE                                                            309
         AG_TITLE='HCLX'                                                  309
         AG_MODULE=MODULE                                                 309
         AG_MARK='V00'                                                    309
         CALL AGSPUSH                                                     309
         AG_MODULE=MODULE                                                 309
C    *                                                                    309
C    Geant MATERIAL : LEAD ===> ISVOL=0 <===                              310
         CALL AGSRESET                                                    310
         AG_ISVOL = 0                                                     310
         AG_TITLE = 'LEAD'                                                310
         AG_EXNAME = 'MATERIAL'                                           310
         AG_PARLIST = 'ISVOL'                                             310
         CALL AXMATERIAL                                                  310
C    *                                                                    310
C    Geant ATTRIBUTE : NONE ===> SEEN=0 COLO=3 <===                       310
         CALL AGSRESET                                                    311
         AG_SEEN = 0                                                      311
         AG_COLO = 3                                                      311
         AG_TITLE = 'NONE'                                                311
         AG_EXNAME = 'ATTRIBUTE'                                          311
         AG_PARLIST = 'SEEN_COLO'                                         311
         CALL AXATTRIBUTE                                                 311
C    *                                                                    311
C    Geant SHAPE : DIVISION ===> NDIV=CELL_NFIBERXL IAXIS=1 <===          311
         CALL AGSRESET                                                    312
         AG_NDIV = CELL_NFIBERXL                                          312
         AG_IAXIS = 1                                                     312
         AG_TITLE = 'DIVISION'                                            312
         AG_EXNAME = 'SHAPE'                                              312
         AG_PARLIST = 'NDIV_IAXIS'                                        312
         CALL AXSHAPE                                                     312
C    *                                                                    312
C    Create HCLY :                                                        313
C    Call To Local Procedure HCLY                                         313
         ASSIGN 5360 TO BLOCK_HCLY                                        313
         GO TO  5350                                                      313
5360     CONTINUE                                                         313
C    *                                                                    313
5291  CALL AGSPOPB                                                        314
      GO TO BLOCK_HCLX, (5290 ,5300)                                      314
C End of Local Procedure                                                  314
C ----------------------------------------------------------------------  315
C ------------ HCLY                                                       315
C Begin Local Procedure HCLY                                              316
C HCLY 3*3 CELL IN TOWER Y DIVISIONS                                      316
5350  CONTINUE                                                            316
         AG_TITLE='HCLY'                                                  316
         AG_MODULE=MODULE                                                 316
         AG_MARK='V00'                                                    316
         CALL AGSPUSH                                                     316
         AG_MODULE=MODULE                                                 316
C    *                                                                    316
C    Geant MATERIAL : LEAD ===> ISVOL=0 <===                              317
         CALL AGSRESET                                                    317
         AG_ISVOL = 0                                                     317
         AG_TITLE = 'LEAD'                                                317
         AG_EXNAME = 'MATERIAL'                                           317
         AG_PARLIST = 'ISVOL'                                             317
         CALL AXMATERIAL                                                  317
C    *                                                                    317
C    Geant ATTRIBUTE : NONE ===> SEEN=0 COLO=3 <===                       317
         CALL AGSRESET                                                    318
         AG_SEEN = 0                                                      318
         AG_COLO = 3                                                      318
         AG_TITLE = 'NONE'                                                318
         AG_EXNAME = 'ATTRIBUTE'                                          318
         AG_PARLIST = 'SEEN_COLO'                                         318
         CALL AXATTRIBUTE                                                 318
C    *                                                                    318
C    Geant SHAPE : DIVISION ===> NDIV=CELL_NFIBERYL IAXIS=2 <===          318
         CALL AGSRESET                                                    319
         AG_NDIV = CELL_NFIBERYL                                          319
         AG_IAXIS = 2                                                     319
         AG_TITLE = 'DIVISION'                                            319
         AG_EXNAME = 'SHAPE'                                              319
         AG_PARLIST = 'NDIV_IAXIS'                                        319
         CALL AXSHAPE                                                     319
C    *                                                                    319
C    Create HFIC :                                                        320
C    Call To Local Procedure HFIC                                         320
         ASSIGN 5380 TO BLOCK_HFIC                                        320
         GO TO  5370                                                      320
5380     CONTINUE                                                         320
C    *                                                                    320
C    Geant POSITION : HFIC ===> X=0 Y=0 Z=0.0 <===                        321
         CALL AGSRESET                                                    321
         AG_X = 0                                                         321
         AG_Y = 0                                                         321
         AG_Z = 0.0                                                       321
         AG_TITLE = 'HFIC'                                                321
         AG_EXNAME = 'POSITION'                                           321
         AG_PARLIST = 'X_Y_Z'                                             321
         CALL AXPOSITION                                                  321
C    *                                                                    321
5351  CALL AGSPOPB                                                        322
      GO TO BLOCK_HCLY, (5350 ,5360)                                      322
C End of Local Procedure                                                  322
C ----------------------------------------------------------------------  323
C ------------ HFIB                                                       323
C Begin Local Procedure HFIB                                              324
C HFIB FIBER IN CELLS                                                     324
5330  CONTINUE                                                            324
         AG_TITLE='HFIB'                                                  324
         AG_MODULE=MODULE                                                 324
         AG_MARK='V00'                                                    324
         CALL AGSPUSH                                                     324
         AG_MODULE=MODULE                                                 324
C    *                                                                    324
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       325
         CALL AGSRESET                                                    325
         AG_TITLE = 'POLYSTYREN'                                          325
         AG_EXNAME = 'MATERIAL'                                           325
         AG_PARLIST = 'NONE'                                              325
         CALL AXMATERIAL                                                  325
C    *                                                                    325
C    Geant MATERIAL : SENSPOLY ===> ISVOL=1 <===                          325
         CALL AGSRESET                                                    326
         AG_ISVOL = 1                                                     326
         AG_TITLE = 'SENSPOLY'                                            326
         AG_EXNAME = 'MATERIAL'                                           326
         AG_PARLIST = 'ISVOL'                                             326
         CALL AXMATERIAL                                                  326
C    *                                                                    326
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           326
         CALL AGSRESET                                                    327
         AG_TITLE = 'STANDARD'                                            327
         AG_EXNAME = 'MEDIUM'                                             327
         AG_PARLIST = 'NONE'                                              327
         CALL AXMEDIUM                                                    327
C    *                                                                    327
C    Geant ATTRIBUTE : NONE ===> SEEN=0 COLO=3 <===                       327
         CALL AGSRESET                                                    328
         AG_SEEN = 0                                                      328
         AG_COLO = 3                                                      328
         AG_TITLE = 'NONE'                                                328
         AG_EXNAME = 'ATTRIBUTE'                                          328
         AG_PARLIST = 'SEEN_COLO'                                         328
         CALL AXATTRIBUTE                                                 328
C    *                                                                    328
C    Geant SHAPE : TUBE ===> RMIN=FIBR_RMIN RMAX=FIBR_RMAX, DZ=FIBR_DZ <  328
C    ===                                                                  328
         CALL AGSRESET                                                    330
         AG_RMIN = FIBR_RMIN                                              330
         AG_RMAX = FIBR_RMAX                                              330
         AG_DZ = FIBR_DZ                                                  330
         AG_TITLE = 'TUBE'                                                330
         AG_EXNAME = 'SHAPE'                                              330
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      330
         CALL AXSHAPE                                                     330
C    *                                                                    330
5331  CALL AGSPOPB                                                        331
      GO TO BLOCK_HFIB, (5330 ,5340)                                      331
C End of Local Procedure                                                  331
C ----------------------------------------------------------------------  332
C ------------ HFIC                                                       332
C Begin Local Procedure HFIC                                              333
C HFIC FIBER IN CELLS                                                     333
5370  CONTINUE                                                            333
         AG_TITLE='HFIC'                                                  333
         AG_MODULE=MODULE                                                 333
         AG_MARK='V00'                                                    333
         CALL AGSPUSH                                                     333
         AG_MODULE=MODULE                                                 333
C    *                                                                    333
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       334
         CALL AGSRESET                                                    334
         AG_TITLE = 'POLYSTYREN'                                          334
         AG_EXNAME = 'MATERIAL'                                           334
         AG_PARLIST = 'NONE'                                              334
         CALL AXMATERIAL                                                  334
C    *                                                                    334
C    Geant MATERIAL : SENSPOLY ===> ISVOL=1 <===                          334
         CALL AGSRESET                                                    335
         AG_ISVOL = 1                                                     335
         AG_TITLE = 'SENSPOLY'                                            335
         AG_EXNAME = 'MATERIAL'                                           335
         AG_PARLIST = 'ISVOL'                                             335
         CALL AXMATERIAL                                                  335
C    *                                                                    335
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           335
         CALL AGSRESET                                                    336
         AG_TITLE = 'STANDARD'                                            336
         AG_EXNAME = 'MEDIUM'                                             336
         AG_PARLIST = 'NONE'                                              336
         CALL AXMEDIUM                                                    336
C    *                                                                    336
C    Geant ATTRIBUTE : NONE ===> SEEN=0 COLO=3 <===                       336
         CALL AGSRESET                                                    337
         AG_SEEN = 0                                                      337
         AG_COLO = 3                                                      337
         AG_TITLE = 'NONE'                                                337
         AG_EXNAME = 'ATTRIBUTE'                                          337
         AG_PARLIST = 'SEEN_COLO'                                         337
         CALL AXATTRIBUTE                                                 337
C    *                                                                    337
C    Geant SHAPE : TUBE ===> RMIN=FIBR_RMIN RMAX=FIBR_RMAX, DZ=FIBR_DZ <  337
C    ===                                                                  337
         CALL AGSRESET                                                    339
         AG_RMIN = FIBR_RMIN                                              339
         AG_RMAX = FIBR_RMAX                                              339
         AG_DZ = FIBR_DZ                                                  339
         AG_TITLE = 'TUBE'                                                339
         AG_EXNAME = 'SHAPE'                                              339
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      339
         CALL AXSHAPE                                                     339
C    *                                                                    339
5371  CALL AGSPOPB                                                        340
      GO TO BLOCK_HFIC, (5370 ,5380)                                      340
C End of Local Procedure                                                  340
C ----------------------------------------------------------------------  341
C ------------ HACL                                                       341
C Begin Local Procedure HACL                                              342
C HACL ALUMINUM SUPORTING CELLS                                           342
5190  CONTINUE                                                            342
         AG_TITLE='HACL'                                                  342
         AG_MODULE=MODULE                                                 342
         AG_MARK='V00'                                                    342
         CALL AGSPUSH                                                     342
         AG_MODULE=MODULE                                                 342
C    *                                                                    342
C    Geant MATERIAL : ALUMINIUM ===> ISVOL=0 <===                         343
         CALL AGSRESET                                                    343
         AG_ISVOL = 0                                                     343
         AG_TITLE = 'ALUMINIUM'                                           343
         AG_EXNAME = 'MATERIAL'                                           343
         AG_PARLIST = 'ISVOL'                                             343
         CALL AXMATERIAL                                                  343
C    *                                                                    343
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           343
         CALL AGSRESET                                                    344
         AG_TITLE = 'STANDARD'                                            344
         AG_EXNAME = 'MEDIUM'                                             344
         AG_PARLIST = 'NONE'                                              344
         CALL AXMEDIUM                                                    344
C    *                                                                    344
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=2 <===                       344
         CALL AGSRESET                                                    345
         AG_SEEN = 1                                                      345
         AG_COLO = 2                                                      345
         AG_TITLE = 'NONE'                                                345
         AG_EXNAME = 'ATTRIBUTE'                                          345
         AG_PARLIST = 'SEEN_COLO'                                         345
         CALL AXATTRIBUTE                                                 345
C    *                                                                    345
C    Geant SHAPE : BOX ===> DX=TOWR_DX DY=TOWR_DY DZ=TOWR_DZ <===         345
         CALL AGSRESET                                                    346
         AG_DX = TOWR_DX                                                  346
         AG_DY = TOWR_DY                                                  346
         AG_DZ = TOWR_DZ                                                  346
         AG_TITLE = 'BOX'                                                 346
         AG_EXNAME = 'SHAPE'                                              346
         AG_PARLIST = 'DX_DY_DZ'                                          346
         CALL AXSHAPE                                                     346
C    *                                                                    346
5191  CALL AGSPOPB                                                        347
      GO TO BLOCK_HACL, (5190 ,5200)                                      347
C End of Local Procedure                                                  347
C ----------------------------------------------------------------------  348
C ------------ FPRS                                                       348
C Begin Local Procedure FPRS                                              349
C FPRS IS BOX FOR PLASTIC SCHINTI FMS PRE-SHOWER DETECTORS                349
5030  CONTINUE                                                            349
         AG_TITLE='FPRS'                                                  349
         AG_MODULE=MODULE                                                 349
         AG_MARK='V00'                                                    349
         CALL AGSPUSH                                                     349
         AG_MODULE=MODULE                                                 349
C    *                                                                    349
C    Geant MATERIAL : AIR ===> inherited arguments used <===              350
         CALL AGSRESET                                                    350
         AG_TITLE = 'AIR'                                                 350
         AG_EXNAME = 'MATERIAL'                                           350
         AG_PARLIST = 'NONE'                                              350
         CALL AXMATERIAL                                                  350
C    *                                                                    350
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           350
         CALL AGSRESET                                                    351
         AG_TITLE = 'STANDARD'                                            351
         AG_EXNAME = 'MEDIUM'                                             351
         AG_PARLIST = 'NONE'                                              351
         CALL AXMEDIUM                                                    351
C    *                                                                    351
C    Geant ATTRIBUTE : FPRS ===> SEEN=0 COLO=2 <===                       351
         CALL AGSRESET                                                    352
         AG_SEEN = 0                                                      352
         AG_COLO = 2                                                      352
         AG_TITLE = 'FPRS'                                                352
         AG_EXNAME = 'ATTRIBUTE'                                          352
         AG_PARLIST = 'SEEN_COLO'                                         352
         CALL AXATTRIBUTE                                                 352
C    *                                                                    352
C    Geant SHAPE : PGON ===> PHI1=45 DPHI=360 NPDIV=4 NZ=2, ZI={-PRSW_DB  352
C    OX(3)/2.0,PRSW_DBOX(3)/2.0} RMN={PRSW_DHOLE-PRSW_DXBBD(1), PRSW_DHO  352
C    LE-PRSW_DXBBD(1)}, RMX={PRSW_DBOX(1)/2.0,PRSW_DBOX(1)/2.0} <===      352
         CALL AGSRESET                                                    356
         AG_PHI1 = 45                                                     356
         AG_DPHI = 360                                                    356
         AG_NPDIV = 4                                                     356
         AG_NZ = 2                                                        356
         AG_ZI(1)=-PRSW_DBOX(3)/2.0                                       356
         AG_ZI(2)=PRSW_DBOX(3)/2.0                                        356
         AG_RMN(1)=PRSW_DHOLE-PRSW_DXBBD(1)                               356
         AG_RMN(2)=PRSW_DHOLE-PRSW_DXBBD(1)                               356
         AG_RMX(1)=PRSW_DBOX(1)/2.0                                       356
         AG_RMX(2)=PRSW_DBOX(1)/2.0                                       356
         AG_TITLE = 'PGON'                                                356
         AG_EXNAME = 'SHAPE'                                              356
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     356
         CALL AXSHAPE                                                     356
C    *                                                                    356
         DEBUG=0                                                          356
C    Check DEBUG.GE.1                                                     358
         IF (DEBUG.GE.1) THEN                                             358
         NPSTYPE=0                                                        359
         CPSTYPE(1)=1                                                     360
         CPSTYPE(2)=2                                                     361
         CPSTYPE(3)=6                                                     362
         CPSTYPE(4)=3                                                     363
         CPSTYPE(5)=4                                                     364
         CPSTYPE(6)=7                                                     365
         CPSTYPE(7)=5                                                     366
         CPSTYPE(8)=8                                                     367
         OPEN(11,FILE='fpsgeom.txt',STATUS='REPLACE')                     368
         ENDIF                                                            369
         SERN=1                                                           370
         DX=PRSW_DBOX(1)/2.0                                              371
         DY=PRSW_DBOX(2)/2.0                                              372
         DZ=PRSW_DZ(1)/2.0 + PRSW_DZBBD(1)/2.0 + PRSW_DSIPM(1)/2.0        373
         ZZ= - PRSW_DBOX(3)/2.0 + DZ                                      374
C    Create FPLY :                                                        375
C    Call To Local Procedure FPLY                                         375
         ASSIGN 5400 TO BLOCK_FPLY                                        375
         GO TO  5390                                                      375
5400     CONTINUE                                                         375
C    *                                                                    375
C    Geant POSITION : FPLY ===> MOTHER='FPRS' Z=ZZ KONLY='MANY' <===      376
         CALL AGSRESET                                                    376
         AG_MOTHER = 'FPRS'                                               376
         AG_Z = ZZ                                                        376
         AG_KONLY = 'MANY'                                                376
         AG_TITLE = 'FPLY'                                                376
         AG_EXNAME = 'POSITION'                                           376
         AG_PARLIST = 'MOTHER_Z_KONLY'                                    376
         CALL AXPOSITION                                                  376
C    *                                                                    376
         SERN=2                                                           376
         DX=PRSW_DBOX(1)/2.0                                              378
         DY=PRSW_DBOX(2)/2.0                                              379
         DZ=PRSW_DZ(2)/2.0 + PRSW_DZBBD(2)/2.0 + PRSW_DSIPM(1)/2.0        380
         ZZ= - PRSW_DBOX(3)/2.0 + DZ + PRSW_ZOFF(2) - PRSW_ZOFF(1)        381
C    Create FPLY :                                                        382
C    Call To Local Procedure FPLY                                         382
         ASSIGN 5410 TO BLOCK_FPLY                                        382
         GO TO  5390                                                      382
5410     CONTINUE                                                         382
C    *                                                                    382
C    Geant POSITION : FPLY ===> MOTHER='FPRS' Z=ZZ KONLY='MANY' <===      383
         CALL AGSRESET                                                    383
         AG_MOTHER = 'FPRS'                                               383
         AG_Z = ZZ                                                        383
         AG_KONLY = 'MANY'                                                383
         AG_TITLE = 'FPLY'                                                383
         AG_EXNAME = 'POSITION'                                           383
         AG_PARLIST = 'MOTHER_Z_KONLY'                                    383
         CALL AXPOSITION                                                  383
C    *                                                                    383
         SERN=3                                                           383
         DX=PRSW_DBOX(1)/2.0                                              385
         DY=PRSW_DBOX(2)/2.0                                              386
         DZ=PRSW_DZ(3)/2.0 + PRSW_DZBBD(3)/2.0                            387
         ZZ= - PRSW_DBOX(3)/2.0 + PRSW_DSIPM(1) + DZ - PRSW_DZBBD(3)/     389
     *   2.0 + PRSW_ZOFF(3) - PRSW_ZOFF(1)                                389
         DZSB = PRSW_DZ(3)/2.0 + PRSW_DZBBD(3)/4.0                        390
C    Create FPLY :                                                        391
C    Call To Local Procedure FPLY                                         391
         ASSIGN 5420 TO BLOCK_FPLY                                        391
         GO TO  5390                                                      391
5420     CONTINUE                                                         391
C    *                                                                    391
C    Geant POSITION : FPLY ===> MOTHER='FPRS' Z=ZZ KONLY='MANY' <===      392
         CALL AGSRESET                                                    392
         AG_MOTHER = 'FPRS'                                               392
         AG_Z = ZZ                                                        392
         AG_KONLY = 'MANY'                                                392
         AG_TITLE = 'FPLY'                                                392
         AG_EXNAME = 'POSITION'                                           392
         AG_PARLIST = 'MOTHER_Z_KONLY'                                    392
         CALL AXPOSITION                                                  392
C    *                                                                    392
         SERN=4                                                           392
         DX=PRSW_DBOX(1)/2.0                                              394
         DY=PRSW_DBOX(2)/2.0                                              395
         DZ=PRSW_DZ(4)/2.0 + PRSW_DZBBD(4)/2.0 + PRSW_DSIPM(1)/2.0        396
         ZZ= - PRSW_DBOX(3)/2.0 + DZ + PRSW_DSIPM(1) + PRSW_ZOFF(4) -     398
     *   PRSW_ZOFF(1) - PRSW_DZBBD(4)                                     398
C    Create FPLY :                                                        399
C    Call To Local Procedure FPLY                                         399
         ASSIGN 5430 TO BLOCK_FPLY                                        399
         GO TO  5390                                                      399
5430     CONTINUE                                                         399
C    *                                                                    399
C    Geant POSITION : FPLY ===> MOTHER='FPRS' Z=ZZ KONLY='MANY' <===      400
         CALL AGSRESET                                                    400
         AG_MOTHER = 'FPRS'                                               400
         AG_Z = ZZ                                                        400
         AG_KONLY = 'MANY'                                                400
         AG_TITLE = 'FPLY'                                                400
         AG_EXNAME = 'POSITION'                                           400
         AG_PARLIST = 'MOTHER_Z_KONLY'                                    400
         CALL AXPOSITION                                                  400
C    *                                                                    400
5031  CALL AGSPOPB                                                        401
      GO TO BLOCK_FPRS, (5030 ,5040)                                      401
C End of Local Procedure                                                  401
C ----------------------------------------------------------------------  402
C ------------ FPLY                                                       402
C Begin Local Procedure FPLY                                              403
C FPLY IS A LAYER OF FMS PRESHOWER                                        403
5390  CONTINUE                                                            403
         AG_TITLE='FPLY'                                                  403
         AG_MODULE=MODULE                                                 403
         AG_MARK='V00'                                                    403
         CALL AGSPUSH                                                     403
         AG_MODULE=MODULE                                                 403
C    *                                                                    403
C    Geant MATERIAL : AIR ===> inherited arguments used <===              404
         CALL AGSRESET                                                    404
         AG_TITLE = 'AIR'                                                 404
         AG_EXNAME = 'MATERIAL'                                           404
         AG_PARLIST = 'NONE'                                              404
         CALL AXMATERIAL                                                  404
C    *                                                                    404
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           404
         CALL AGSRESET                                                    405
         AG_TITLE = 'STANDARD'                                            405
         AG_EXNAME = 'MEDIUM'                                             405
         AG_PARLIST = 'NONE'                                              405
         CALL AXMEDIUM                                                    405
C    *                                                                    405
C    Geant ATTRIBUTE : FPLY ===> SEEN=0 COLO=3 SERIAL=SERN <===           405
         CALL AGSRESET                                                    406
         AG_SEEN = 0                                                      406
         AG_COLO = 3                                                      406
         AG_SERIAL = SERN                                                 406
         AG_TITLE = 'FPLY'                                                406
         AG_EXNAME = 'ATTRIBUTE'                                          406
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  406
         CALL AXATTRIBUTE                                                 406
C    *                                                                    406
C    Geant SHAPE : PGON ===> PHI1=45 DPHI=360 NPDIV=4 NZ=2, ZI={-DZ,DZ}   406
C    RMN={PRSW_DHOLE-PRSW_DXBBD(1),PRSW_DHOLE-PRSW_DXBBD(1)}, RMX={PRSW_  406
C    DBOX(1)/2.0, PRSW_DBOX(1)/2.0} <===                                  406
         CALL AGSRESET                                                    410
         AG_PHI1 = 45                                                     410
         AG_DPHI = 360                                                    410
         AG_NPDIV = 4                                                     410
         AG_NZ = 2                                                        410
         AG_ZI(1)=-DZ                                                     410
         AG_ZI(2)=DZ                                                      410
         AG_RMN(1)=PRSW_DHOLE-PRSW_DXBBD(1)                               410
         AG_RMN(2)=PRSW_DHOLE-PRSW_DXBBD(1)                               410
         AG_RMX(1)=PRSW_DBOX(1)/2.0                                       410
         AG_RMX(2)=PRSW_DBOX(1)/2.0                                       410
         AG_TITLE = 'PGON'                                                410
         AG_EXNAME = 'SHAPE'                                              410
         AG_PARLIST = 'PHI1_DPHI_NPDIV_NZ_ZI_RMN_RMX'                     410
         CALL AXSHAPE                                                     410
C    *                                                                    410
         LPRS1=PRSW_NPRS(1)*(PRSW_DPRS(1) + PRSW_DPRS(3))                 410
         LPRS2=PRSW_NPRS(2)*(PRSW_DPRS(2) + PRSW_DPRS(3)) + LPRS1         412
         LPRS3=(PRSW_NPRS(2) - PRSW_NSKIPH)*(PRSW_DPRS(2) +               413
     *   PRSW_DPRS(3)) + LPRS1                                            413
         LPRS4=(PRSW_NPRS(1) - PRSW_NSKIPV)*(PRSW_DPRS(1) +               415
     *   PRSW_DPRS(3)) + PRSW_NPRS(2)*(PRSW_DPRS(2) + PRSW_DPRS(3))       415
         DLG=(PRSW_DLG(2) + PRSW_DLG(3) + PRSW_DSIPM(1))/2.0              416
         K = 0                                                            417
         DZZ = PRSW_DZ(SERN)/2.0                                          418
C    Check SERN.EQ.3                                                      419
         IF (SERN.EQ.3) THEN                                              419
C    PB IN MIDDLE, BACK PLATE IN FRONT AND BACK                           420
         Z1=0                                                             421
         Z2=0                                                             422
         ELSEIF (SERN.EQ.4) THEN                                          423
C    BACK BOARD FIRST FOR LAYER3                                          424
C    Z FOR SCHINTI                                                        425
         Z1=-DZ+PRSW_DZBBD(SERN)+DZZ                                      425
C    Z FOR BACK BOARD                                                     426
         Z2=-DZ+PRSW_DZBBD(SERN)/2.0                                      426
         ELSE                                                             427
C    BACK BOARD 2ND FOR LAYER1,2                                          428
C    Z FOR SCHINTI                                                        429
         Z1=-DZ+PRSW_DSIPM(1)+DZZ                                         429
C    Z FOR BACK BOARD                                                     430
         Z2=-DZ+PRSW_DSIPM(1)+DZZ*2+PRSW_DZBBD(SERN)/2.0                  430
         ENDIF                                                            431
         Z = PRSW_ZOFF(SERN)                                              432
C    Create FPSC :                                                        433
C    Call To Local Procedure FPSC                                         433
         ASSIGN 5450 TO BLOCK_FPSC                                        433
         GO TO  5440                                                      433
5450     CONTINUE                                                         433
C    *                                                                    433
C    Create FLIG :                                                        434
C    Call To Local Procedure FLIG                                         434
         ASSIGN 5470 TO BLOCK_FLIG                                        434
         GO TO  5460                                                      434
5470     CONTINUE                                                         434
C    *                                                                    434
C    Create FLLG :                                                        435
C    Call To Local Procedure FLLG                                         435
         ASSIGN 5490 TO BLOCK_FLLG                                        435
         GO TO  5480                                                      435
5490     CONTINUE                                                         435
C    *                                                                    435
C    Create WPAR :                                                        436
C    Call To Local Procedure WPAR                                         436
         ASSIGN 5510 TO BLOCK_WPAR                                        436
         GO TO  5500                                                      436
5510     CONTINUE                                                         436
C    *                                                                    436
C    Create FPBB :                                                        437
C    Call To Local Procedure FPBB                                         437
         ASSIGN 5530 TO BLOCK_FPBB                                        437
         GO TO  5520                                                      437
5530     CONTINUE                                                         437
C    *                                                                    437
C    Create FPPB :                                                        438
C    Call To Local Procedure FPPB                                         438
         ASSIGN 5550 TO BLOCK_FPPB                                        438
         GO TO  5540                                                      438
5550     CONTINUE                                                         438
C    *                                                                    438
C    Check DEBUG.GE.1 .AND. SERN.EQ.1                                     439
         IF (DEBUG.GE.1 .AND. SERN.EQ.1) THEN                             439
         OPEN(1,FILE='prs1map.txt',STATUS='REPLACE')                      440
         ENDIF                                                            441
C    Check DEBUG.GE.1 .AND. SERN.EQ.2                                     442
         IF (DEBUG.GE.1 .AND. SERN.EQ.2) THEN                             442
         OPEN(2,FILE='prs2map.txt',STATUS='REPLACE')                      443
         ENDIF                                                            444
C    Check DEBUG.GE.1 .AND. SERN.EQ.3                                     445
         IF (DEBUG.GE.1 .AND. SERN.EQ.3) THEN                             445
         OPEN(3,FILE='prsCmap.txt',STATUS='REPLACE')                      446
         ENDIF                                                            447
C    Check DEBUG.GE.1 .AND. SERN.EQ.4                                     448
         IF (DEBUG.GE.1 .AND. SERN.EQ.4) THEN                             448
         OPEN(4,FILE='prs3map.txt',STATUS='REPLACE')                      449
         ENDIF                                                            450
C    Check SERN.LE.2                                                      451
         IF (SERN.LE.2) THEN                                              451
         LAYR=SERN                                                        452
         ELSE                                                             453
         LAYR=SERN-1                                                      454
         ENDIF                                                            455
C    Check SERN.EQ.1                                                      456
         IF (SERN.EQ.1) THEN                                              456
C    *                                                                    457
         LR=1                                                             457
            GO TO 5563                                                    457
5561        LR=LR+1                                                       457
5563        IF(LR-(2).GT.0) GO TO 5562                                    457
C       *                                                                 458
            TB=1                                                          458
               GO TO 5573                                                 458
5571           TB=TB+1                                                    458
5573           IF(TB-(2).GT.0) GO TO 5572                                 458
               X=0.0                                                      459
               QUAD=(LR-1)*2+TB                                           460
               SLAT(QUAD,LAYR)=0                                          461
C          *                                                              462
               J=1                                                        462
                  GO TO 5583                                              462
5581              J=J+1                                                   462
5583              IF(J-(2).GT.0) GO TO 5582                               462
C             *                                                           463
                  I=1                                                     463
                     GO TO 5593                                           463
5591                 I=I+1                                                463
5593                 IF(I-(PRSW_NPRS(J)).GT.0) GO TO 5592                 463
                     DXX = PRSW_DPRS(J)/2.0                               464
                     X = X + DXX + PRSW_DPRS(3)/2.0                       465
C                Check TB.EQ.1                                            466
                     IF (TB.EQ.1) THEN                                    466
                     PWID=LPRS2                                           467
                     ELSE                                                 468
                     PWID=LPRS3-PRSW_DYCUT                                469
                     ENDIF                                                470
C                Check X.LE.PRSW_DHOLE                                    471
                     IF (X.LE.PRSW_DHOLE) THEN                            471
                     DYY = (PWID-PRSW_DHOLE)/2.0                          472
                     Y = DYY+PRSW_DHOLE                                   473
                     ELSE                                                 474
                     DYY = PWID/2.0                                       475
                     Y = DYY                                              476
                     ENDIF                                                477
                     YY=Y+DYY+DLG                                         478
C                Check TB.EQ.1                                            479
                     IF (TB.EQ.1) THEN                                    479
                     ROT=0                                                480
                     ENDIF                                                481
C                Check TB.EQ.2                                            482
                     IF (TB.EQ.2) THEN                                    482
                     Y=-Y                                                 482
                     YY=-YY                                               482
                     ROT=180                                              483
                     ENDIF                                                484
C                Check LR.EQ.1                                            485
                     IF (LR.EQ.1) THEN                                    485
                     XX=+X+PRSW_XOFF(SERN)                                486
                     ENDIF                                                487
C                Check LR.EQ.2                                            488
                     IF (LR.EQ.2) THEN                                    488
                     XX=-X-PRSW_XOFF(SERN)                                489
                     ENDIF                                                490
                     Y=Y+PRSW_YOFF(SERN)                                  490
                     YY=YY+PRSW_YOFF(SERN)                                490
C                Check TB.EQ.1 .OR. J.EQ.2 .OR. I.GT.PRSW_NSKIPV          492
                     IF (TB.EQ.1 .OR. J.EQ.2 .OR. I.GT.PRSW_NSKIPV)       492
     *               THEN                                                 492
C                Check DEBUG.GE.1                                         493
                     IF (DEBUG.GE.1) THEN                                 493
                     K = K+1                                              494
                     SLAT(QUAD,LAYR)=SLAT(QUAD,LAYR)+1                    495
                     ID=(QUAD-1)*3*21+(LAYR-1)*21+(SLAT(QUAD,LAYR)-1)     496
                     FLAG=0                                               497
C                *                                                        498
                     KK=1                                                 498
                        GO TO 5603                                        498
5601                    KK=KK+1                                           498
5603                    IF(KK-(NPSTYPE).GT.0) GO TO 5602                  498
C                   Check (PSDIM(KK,1).EQ.DXX.AND.PSDIM(KK,2).EQ.DYY).OR  500
C                   .(PSDIM(KK,1).EQ.DYY.AND.PSDIM(KK, 2).EQ.DXX)         500
                        IF ((PSDIM(KK,1).EQ.DXX.AND.PSDIM(KK,             500
     *                  2).EQ.DYY).OR.(PSDIM(KK,1).EQ.DYY.AND.PSDIM(KK,   500
     *                  2).EQ.DXX)) THEN                                  500
                        FLAG=KK                                           501
                        PSTYPE(KK)=PSTYPE(KK)+1                           502
                        ENDIF                                             503
                     GO TO 5601                                           504
5602                 CONTINUE                                             504
C                Check FLAG.EQ.0                                          505
                     IF (FLAG.EQ.0) THEN                                  505
                     NPSTYPE=NPSTYPE+1                                    506
                     FLAG=NPSTYPE                                         507
                     PSTYPE(FLAG)=1                                       508
C                Check DXX.GT.DYY                                         509
                     IF (DXX.GT.DYY) THEN                                 509
                     PSDIM(NPSTYPE,1)=DYY                                 510
                     PSDIM(NPSTYPE,2)=DXX                                 511
                     ELSE                                                 512
                     PSDIM(NPSTYPE,1)=DXX                                 513
                     PSDIM(NPSTYPE,2)=DYY                                 514
                     ENDIF                                                515
                     ENDIF                                                516
                     ENDIF                                                517
C                Geant POSITION : FPSC ===> X=XX Y=Y Z=Z1 DZ=DZZ DX=DXX   518
C                DY=DYY <===                                              518
                     CALL AGSRESET                                        518
                     AG_X = XX                                            518
                     AG_Y = Y                                             518
                     AG_Z = Z1                                            518
                     AG_DZ = DZZ                                          518
                     AG_DX = DXX                                          518
                     AG_DY = DYY                                          518
                     AG_TITLE = 'FPSC'                                    518
                     AG_EXNAME = 'POSITION'                               518
                     AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                        518
                     CALL AXPOSITION                                      518
C                *                                                        518
C                *                                                        519
C                   Check J.EQ.1                                          519
                        IF (J.EQ.1) THEN                                  519
C                   Geant POSITION : FLIG ===> X=XX Y=YY Z=Z1-PRSW_DSIPM  520
C                   (1)/2.0 ALPHAX=-90 ALPHAZ=ROT <===                    520
                        CALL AGSRESET                                     520
                        AG_X = XX                                         520
                        AG_Y = YY                                         520
                        AG_Z = Z1-PRSW_DSIPM(1)/2.0                       520
                        AG_ALPHAN=AG_ALPHAN+1                             520
                        AG_ALPHAK(AG_ALPHAN)=1                            520
                        AG_ALPHAL(AG_ALPHAN)=-90                          520
                        AG_ALPHAN=AG_ALPHAN+1                             520
                        AG_ALPHAK(AG_ALPHAN)=3                            520
                        AG_ALPHAL(AG_ALPHAN)=ROT                          520
                        AG_TITLE = 'FLIG'                                 520
                        AG_EXNAME = 'POSITION'                            520
                        AG_PARLIST = 'X_Y_Z_ALPHAX_ALPHAZ'                520
                        CALL AXPOSITION                                   520
C                   *                                                     520
                     END IF                                               521
C                *                                                        522
C                   Check J.EQ.2                                          522
                        IF (J.EQ.2) THEN                                  522
C                   Geant POSITION : FLLG ===> X=XX Y=YY Z=Z1-PRSW_DSIPM  523
C                   (1)/2.0 ALPHAX=-90 ALPHAZ=ROT <===                    523
                        CALL AGSRESET                                     523
                        AG_X = XX                                         523
                        AG_Y = YY                                         523
                        AG_Z = Z1-PRSW_DSIPM(1)/2.0                       523
                        AG_ALPHAN=AG_ALPHAN+1                             523
                        AG_ALPHAK(AG_ALPHAN)=1                            523
                        AG_ALPHAL(AG_ALPHAN)=-90                          523
                        AG_ALPHAN=AG_ALPHAN+1                             523
                        AG_ALPHAK(AG_ALPHAN)=3                            523
                        AG_ALPHAL(AG_ALPHAN)=ROT                          523
                        AG_TITLE = 'FLLG'                                 523
                        AG_EXNAME = 'POSITION'                            523
                        AG_PARLIST = 'X_Y_Z_ALPHAX_ALPHAZ'                523
                        CALL AXPOSITION                                   523
C                   *                                                     523
                     END IF                                               524
                     ENDIF                                                525
                     X = X + DXX                                          526
                  GO TO 5591                                              527
5592              CONTINUE                                                527
               GO TO 5581                                                 528
5582           CONTINUE                                                   528
            GO TO 5571                                                    529
5572        CONTINUE                                                      529
         GO TO 5561                                                       530
5562     CONTINUE                                                         530
         ELSEIF (SERN.EQ.2 .OR. SERN.EQ.4) THEN                           531
C    *                                                                    532
         LR=1                                                             532
            GO TO 5613                                                    532
5611        LR=LR+1                                                       532
5613        IF(LR-(2).GT.0) GO TO 5612                                    532
C       *                                                                 533
            TB=1                                                          533
               GO TO 5623                                                 533
5621           TB=TB+1                                                    533
5623           IF(TB-(2).GT.0) GO TO 5622                                 533
               Y=0.0                                                      534
               QUAD=(LR-1)*2+TB                                           535
               SLAT(QUAD,LAYR)=0                                          536
C          *                                                              537
               J=1                                                        537
                  GO TO 5633                                              537
5631              J=J+1                                                   537
5633              IF(J-(2).GT.0) GO TO 5632                               537
C             *                                                           538
                  I=1                                                     538
                     GO TO 5643                                           538
5641                 I=I+1                                                538
5643                 IF(I-(PRSW_NPRS(J)).GT.0) GO TO 5642                 538
                     DYY = PRSW_DPRS(J)/2.0                               539
                     Y = Y + DYY + PRSW_DPRS(3)/2.0                       540
                     PWID=LPRS2                                           541
C                Check Y.LE.PRSW_DHOLE                                    542
                     IF (Y.LE.PRSW_DHOLE) THEN                            542
                     DXX = (PWID-PRSW_DHOLE)/2.0                          543
                     X = DXX+PRSW_DHOLE                                   544
                     ELSE                                                 545
C                Check TB.EQ.1                                            546
                     IF (TB.EQ.1) THEN                                    546
                     DXX = PWID/2.0                                       547
                     X = DXX                                              548
                     ELSE                                                 549
                     DXX = LPRS4/2.0                                      550
                     X = DXX+2*PRSW_DPRS(1)                               551
                     ENDIF                                                552
                     ENDIF                                                553
                     XX=X+DXX+DLG                                         554
C                Check TB.EQ.1                                            555
                     IF (TB.EQ.1) THEN                                    555
                     YY=+Y                                                555
                     ENDIF                                                557
C                Check TB.EQ.2                                            558
                     IF (TB.EQ.2) THEN                                    558
                     YY=-Y                                                558
                     ENDIF                                                560
C                Check LR.EQ.1                                            561
                     IF (LR.EQ.1) THEN                                    561
                     X=+X+PRSW_XOFF(SERN)                                 561
                     XX=+XX+PRSW_XOFF(SERN)                               561
                     ROT=-90                                              562
                     ENDIF                                                563
C                Check LR.EQ.2                                            564
                     IF (LR.EQ.2) THEN                                    564
                     X=-X-PRSW_XOFF(SERN)                                 564
                     XX=-XX-PRSW_XOFF(SERN)                               564
                     ROT=90                                               565
                     ENDIF                                                566
                     YY=YY+PRSW_YOFF(SERN)                                566
C                Check TB.EQ.1 .OR. J.EQ.1 .OR. I.LE.PRSW_NPRS(J)-PRSW_N  568
C                SKIPH                                                    568
                     IF (TB.EQ.1 .OR. J.EQ.1 .OR.                         568
     *               I.LE.PRSW_NPRS(J)-PRSW_NSKIPH) THEN                  568
C                Check DEBUG.GE.1                                         569
                     IF (DEBUG.GE.1) THEN                                 569
                     K = K+1                                              570
                     SLAT(QUAD,LAYR)=SLAT(QUAD,LAYR)+1                    571
                     ID=(QUAD-1)*3*21+(LAYR-1)*21+(SLAT(QUAD,LAYR)-1)     572
                     FLAG=0                                               573
C                *                                                        574
                     KK=1                                                 574
                        GO TO 5653                                        574
5651                    KK=KK+1                                           574
5653                    IF(KK-(NPSTYPE).GT.0) GO TO 5652                  574
C                   Check (PSDIM(KK,1).EQ.DXX.AND.PSDIM(KK,2).EQ.DYY).OR  576
C                   .(PSDIM(KK,1).EQ.DYY.AND.PSDIM(KK, 2).EQ.DXX)         576
                        IF ((PSDIM(KK,1).EQ.DXX.AND.PSDIM(KK,             576
     *                  2).EQ.DYY).OR.(PSDIM(KK,1).EQ.DYY.AND.PSDIM(KK,   576
     *                  2).EQ.DXX)) THEN                                  576
                        FLAG=KK                                           577
                        PSTYPE(KK)=PSTYPE(KK)+1                           578
                        ENDIF                                             579
                     GO TO 5651                                           580
5652                 CONTINUE                                             580
C                Check FLAG.EQ.0                                          581
                     IF (FLAG.EQ.0) THEN                                  581
                     NPSTYPE=NPSTYPE+1                                    582
                     FLAG=NPSTYPE                                         583
                     PSTYPE(FLAG)=1                                       584
C                Check DXX.GT.DYY                                         585
                     IF (DXX.GT.DYY) THEN                                 585
                     PSDIM(NPSTYPE,1)=DYY                                 586
                     PSDIM(NPSTYPE,2)=DXX                                 587
                     ELSE                                                 588
                     PSDIM(NPSTYPE,1)=DXX                                 589
                     PSDIM(NPSTYPE,2)=DYY                                 590
                     ENDIF                                                591
                     ENDIF                                                592
                     ENDIF                                                593
C                Geant POSITION : FPSC ===> X=X Y=YY Z=Z1 DZ=DZZ DX=DXX   594
C                DY=DYY <===                                              594
                     CALL AGSRESET                                        594
                     AG_X = X                                             594
                     AG_Y = YY                                            594
                     AG_Z = Z1                                            594
                     AG_DZ = DZZ                                          594
                     AG_DX = DXX                                          594
                     AG_DY = DYY                                          594
                     AG_TITLE = 'FPSC'                                    594
                     AG_EXNAME = 'POSITION'                               594
                     AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                        594
                     CALL AXPOSITION                                      594
C                *                                                        594
C                *                                                        595
C                   Check J.EQ.1.AND.SERN.EQ.2                            595
                        IF (J.EQ.1.AND.SERN.EQ.2) THEN                    595
C                   Geant POSITION : FLIG ===> X=XX Y=YY Z=Z1-PRSW_DSIPM  596
C                   (1)/2.0 ALPHAX=-90 ALPHAZ=ROT <===                    596
                        CALL AGSRESET                                     596
                        AG_X = XX                                         596
                        AG_Y = YY                                         596
                        AG_Z = Z1-PRSW_DSIPM(1)/2.0                       596
                        AG_ALPHAN=AG_ALPHAN+1                             596
                        AG_ALPHAK(AG_ALPHAN)=1                            596
                        AG_ALPHAL(AG_ALPHAN)=-90                          596
                        AG_ALPHAN=AG_ALPHAN+1                             596
                        AG_ALPHAK(AG_ALPHAN)=3                            596
                        AG_ALPHAL(AG_ALPHAN)=ROT                          596
                        AG_TITLE = 'FLIG'                                 596
                        AG_EXNAME = 'POSITION'                            596
                        AG_PARLIST = 'X_Y_Z_ALPHAX_ALPHAZ'                596
                        CALL AXPOSITION                                   596
C                   *                                                     596
                     END IF                                               597
C                *                                                        598
C                   Check J.EQ.2.AND.SERN.EQ.2                            598
                        IF (J.EQ.2.AND.SERN.EQ.2) THEN                    598
C                   Geant POSITION : FLLG ===> X=XX Y=YY Z=Z1-PRSW_DSIPM  599
C                   (1)/2.0 ALPHAX=-90 ALPHAZ=ROT <===                    599
                        CALL AGSRESET                                     599
                        AG_X = XX                                         599
                        AG_Y = YY                                         599
                        AG_Z = Z1-PRSW_DSIPM(1)/2.0                       599
                        AG_ALPHAN=AG_ALPHAN+1                             599
                        AG_ALPHAK(AG_ALPHAN)=1                            599
                        AG_ALPHAL(AG_ALPHAN)=-90                          599
                        AG_ALPHAN=AG_ALPHAN+1                             599
                        AG_ALPHAK(AG_ALPHAN)=3                            599
                        AG_ALPHAL(AG_ALPHAN)=ROT                          599
                        AG_TITLE = 'FLLG'                                 599
                        AG_EXNAME = 'POSITION'                            599
                        AG_PARLIST = 'X_Y_Z_ALPHAX_ALPHAZ'                599
                        CALL AXPOSITION                                   599
C                   *                                                     599
                     END IF                                               600
C                *                                                        601
C                   Check J.EQ.1.AND.SERN.EQ.4                            601
                        IF (J.EQ.1.AND.SERN.EQ.4) THEN                    601
C                   Geant POSITION : FLIG ===> X=XX Y=YY Z=Z1+PRSW_DSIPM  602
C                   (1)/2.0 ALPHAX=90 ALPHAZ=-ROT <===                    602
                        CALL AGSRESET                                     602
                        AG_X = XX                                         602
                        AG_Y = YY                                         602
                        AG_Z = Z1+PRSW_DSIPM(1)/2.0                       602
                        AG_ALPHAN=AG_ALPHAN+1                             602
                        AG_ALPHAK(AG_ALPHAN)=1                            602
                        AG_ALPHAL(AG_ALPHAN)=90                           602
                        AG_ALPHAN=AG_ALPHAN+1                             602
                        AG_ALPHAK(AG_ALPHAN)=3                            602
                        AG_ALPHAL(AG_ALPHAN)=-ROT                         602
                        AG_TITLE = 'FLIG'                                 602
                        AG_EXNAME = 'POSITION'                            602
                        AG_PARLIST = 'X_Y_Z_ALPHAX_ALPHAZ'                602
                        CALL AXPOSITION                                   602
C                   *                                                     602
                     END IF                                               603
C                *                                                        604
C                   Check J.EQ.2.AND.SERN.EQ.4                            604
                        IF (J.EQ.2.AND.SERN.EQ.4) THEN                    604
C                   Geant POSITION : FLLG ===> X=XX Y=YY Z=Z1+PRSW_DSIPM  605
C                   (1)/2.0 ALPHAX=90 ALPHAZ=-ROT <===                    605
                        CALL AGSRESET                                     605
                        AG_X = XX                                         605
                        AG_Y = YY                                         605
                        AG_Z = Z1+PRSW_DSIPM(1)/2.0                       605
                        AG_ALPHAN=AG_ALPHAN+1                             605
                        AG_ALPHAK(AG_ALPHAN)=1                            605
                        AG_ALPHAL(AG_ALPHAN)=90                           605
                        AG_ALPHAN=AG_ALPHAN+1                             605
                        AG_ALPHAK(AG_ALPHAN)=3                            605
                        AG_ALPHAL(AG_ALPHAN)=-ROT                         605
                        AG_TITLE = 'FLLG'                                 605
                        AG_EXNAME = 'POSITION'                            605
                        AG_PARLIST = 'X_Y_Z_ALPHAX_ALPHAZ'                605
                        CALL AXPOSITION                                   605
C                   *                                                     605
                     END IF                                               606
                     ENDIF                                                607
                     Y = Y + DYY                                          608
                  GO TO 5641                                              609
5642              CONTINUE                                                609
               GO TO 5631                                                 610
5632           CONTINUE                                                   610
            GO TO 5621                                                    611
5622        CONTINUE                                                      611
         GO TO 5611                                                       612
5612     CONTINUE                                                         612
         ENDIF                                                            613
C    Check DEBUG.GE.1                                                     614
         IF (DEBUG.GE.1) THEN                                             614
         CLOSE(SERN)                                                      615
         ENDIF                                                            616
         DZ1=PRSW_DZBBD(SERN)/2.0                                         617
         DZ3=PRSW_DZ(3) + PRSW_DZBBD(3)                                   618
         DX1=(PRSW_DHOLE - PRSW_DXBBD(1))/2.0                             619
         DY1=(LPRS2 - PRSW_DHOLE + PRSW_DXBBD(1) + PRSW_DXBBD(2))/2.0     620
         X2=PRSW_XOFF(SERN) + DX1                                         621
         Y2=PRSW_DHOLE - PRSW_DXBBD(1) + DY1                              622
         DXX=DX1                                                          622
         DYY=DY1                                                          622
C    *                                                                    624
C       Check SERN.NE.3                                                   624
            IF (SERN.NE.3) THEN                                           624
C       Geant POSITION : FPBB ===> X=X2 Y=Y2 Z=Z2 DZ=DZ1 DX=DX1 DY=DY1 <  625
C       ===                                                               625
            CALL AGSRESET                                                 625
            AG_X = X2                                                     625
            AG_Y = Y2                                                     625
            AG_Z = Z2                                                     625
            AG_DZ = DZ1                                                   625
            AG_DX = DX1                                                   625
            AG_DY = DY1                                                   625
            AG_TITLE = 'FPBB'                                             625
            AG_EXNAME = 'POSITION'                                        625
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 625
            CALL AXPOSITION                                               625
C       *                                                                 625
         END IF                                                           626
C    *                                                                    627
C       Check SERN.NE.3                                                   627
            IF (SERN.NE.3) THEN                                           627
C       Geant POSITION : FPBB ===> X=-X2 Y=Y2 Z=Z2 DZ=DZ1 DX=DX1 DY=DY1   628
C       <===                                                              628
            CALL AGSRESET                                                 628
            AG_X = -X2                                                    628
            AG_Y = Y2                                                     628
            AG_Z = Z2                                                     628
            AG_DZ = DZ1                                                   628
            AG_DX = DX1                                                   628
            AG_DY = DY1                                                   628
            AG_TITLE = 'FPBB'                                             628
            AG_EXNAME = 'POSITION'                                        628
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 628
            CALL AXPOSITION                                               628
C       *                                                                 628
         END IF                                                           629
C    *                                                                    630
C       Check SERN.EQ.3                                                   630
            IF (SERN.EQ.3) THEN                                           630
C       Geant POSITION : FPBB ===> X=X2 Y=Y2 Z=Z2-DZSB DZ=PRSW_DZBBD(3)/  631
C       4.0 DX=DX1 DY=DY1 <===                                            631
            CALL AGSRESET                                                 631
            AG_X = X2                                                     631
            AG_Y = Y2                                                     631
            AG_Z = Z2-DZSB                                                631
            AG_DZ = PRSW_DZBBD(3)/4.0                                     631
            AG_DX = DX1                                                   631
            AG_DY = DY1                                                   631
            AG_TITLE = 'FPBB'                                             631
            AG_EXNAME = 'POSITION'                                        631
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 631
            CALL AXPOSITION                                               631
C       *                                                                 631
         END IF                                                           632
C    *                                                                    633
C       Check SERN.EQ.3                                                   633
            IF (SERN.EQ.3) THEN                                           633
C       Geant POSITION : FPPB ===> X=X2 Y=Y2 Z=Z2 DZ=PRSW_DZ(3)/2.0 DX=D  634
C       X1 DY=DY1 <===                                                    634
            CALL AGSRESET                                                 634
            AG_X = X2                                                     634
            AG_Y = Y2                                                     634
            AG_Z = Z2                                                     634
            AG_DZ = PRSW_DZ(3)/2.0                                        634
            AG_DX = DX1                                                   634
            AG_DY = DY1                                                   634
            AG_TITLE = 'FPPB'                                             634
            AG_EXNAME = 'POSITION'                                        634
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 634
            CALL AXPOSITION                                               634
C       *                                                                 634
         END IF                                                           635
C    *                                                                    636
C       Check SERN.EQ.3                                                   636
            IF (SERN.EQ.3) THEN                                           636
C       Geant POSITION : FPBB ===> X=X2 Y=Y2 Z=Z2+DZSB DZ=PRSW_DZBBD(3)/  637
C       4.0 DX=DX1 DY=DY1 <===                                            637
            CALL AGSRESET                                                 637
            AG_X = X2                                                     637
            AG_Y = Y2                                                     637
            AG_Z = Z2+DZSB                                                637
            AG_DZ = PRSW_DZBBD(3)/4.0                                     637
            AG_DX = DX1                                                   637
            AG_DY = DY1                                                   637
            AG_TITLE = 'FPBB'                                             637
            AG_EXNAME = 'POSITION'                                        637
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 637
            CALL AXPOSITION                                               637
C       *                                                                 637
         END IF                                                           638
C    *                                                                    639
C       Check SERN.EQ.3                                                   639
            IF (SERN.EQ.3) THEN                                           639
C       Geant POSITION : FPBB ===> X=-X2 Y=Y2 Z=Z2-DZSB DZ=PRSW_DZBBD(3)  640
C       /4.0 DX=DX1 DY=DY1 <===                                           640
            CALL AGSRESET                                                 640
            AG_X = -X2                                                    640
            AG_Y = Y2                                                     640
            AG_Z = Z2-DZSB                                                640
            AG_DZ = PRSW_DZBBD(3)/4.0                                     640
            AG_DX = DX1                                                   640
            AG_DY = DY1                                                   640
            AG_TITLE = 'FPBB'                                             640
            AG_EXNAME = 'POSITION'                                        640
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 640
            CALL AXPOSITION                                               640
C       *                                                                 640
         END IF                                                           641
C    *                                                                    642
C       Check SERN.EQ.3                                                   642
            IF (SERN.EQ.3) THEN                                           642
C       Geant POSITION : FPPB ===> X=-X2 Y=Y2 Z=Z2 DZ=PRSW_DZ(3)/2.0 DX=  643
C       DX1 DY=DY1 <===                                                   643
            CALL AGSRESET                                                 643
            AG_X = -X2                                                    643
            AG_Y = Y2                                                     643
            AG_Z = Z2                                                     643
            AG_DZ = PRSW_DZ(3)/2.0                                        643
            AG_DX = DX1                                                   643
            AG_DY = DY1                                                   643
            AG_TITLE = 'FPPB'                                             643
            AG_EXNAME = 'POSITION'                                        643
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 643
            CALL AXPOSITION                                               643
C       *                                                                 643
         END IF                                                           644
C    *                                                                    645
C       Check SERN.EQ.3                                                   645
            IF (SERN.EQ.3) THEN                                           645
C       Geant POSITION : FPBB ===> X=-X2 Y=Y2 Z=Z2+DZSB DZ=PRSW_DZBBD(3)  646
C       /4.0 DX=DX1 DY=DY1 <===                                           646
            CALL AGSRESET                                                 646
            AG_X = -X2                                                    646
            AG_Y = Y2                                                     646
            AG_Z = Z2+DZSB                                                646
            AG_DZ = PRSW_DZBBD(3)/4.0                                     646
            AG_DX = DX1                                                   646
            AG_DY = DY1                                                   646
            AG_TITLE = 'FPBB'                                             646
            AG_EXNAME = 'POSITION'                                        646
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 646
            CALL AXPOSITION                                               646
C       *                                                                 646
         END IF                                                           647
         DX2=(LPRS2 - PRSW_DHOLE + PRSW_DXBBD(1) + PRSW_DXBBD(2))/2.0     648
         DY2=(LPRS2 + PRSW_DXBBD(2))/2.0                                  649
         X2=PRSW_XOFF(SERN) + DX1*2 + DX2                                 650
         Y2=DY2                                                           651
         DXX=DX2                                                          651
         DYY=DY2                                                          651
C    *                                                                    653
C       Check SERN.NE.3                                                   653
            IF (SERN.NE.3) THEN                                           653
C       Geant POSITION : FPBB ===> X=X2 Y=Y2 Z=Z2 DZ=DZ1 DX=DX2 DY=DY2 <  654
C       ===                                                               654
            CALL AGSRESET                                                 654
            AG_X = X2                                                     654
            AG_Y = Y2                                                     654
            AG_Z = Z2                                                     654
            AG_DZ = DZ1                                                   654
            AG_DX = DX2                                                   654
            AG_DY = DY2                                                   654
            AG_TITLE = 'FPBB'                                             654
            AG_EXNAME = 'POSITION'                                        654
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 654
            CALL AXPOSITION                                               654
C       *                                                                 654
         END IF                                                           655
C    *                                                                    656
C       Check SERN.NE.3                                                   656
            IF (SERN.NE.3) THEN                                           656
C       Geant POSITION : FPBB ===> X=-X2 Y=Y2 Z=Z2 DZ=DZ1 DX=DX2 DY=DY2   657
C       <===                                                              657
            CALL AGSRESET                                                 657
            AG_X = -X2                                                    657
            AG_Y = Y2                                                     657
            AG_Z = Z2                                                     657
            AG_DZ = DZ1                                                   657
            AG_DX = DX2                                                   657
            AG_DY = DY2                                                   657
            AG_TITLE = 'FPBB'                                             657
            AG_EXNAME = 'POSITION'                                        657
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 657
            CALL AXPOSITION                                               657
C       *                                                                 657
         END IF                                                           658
C    *                                                                    659
C       Check SERN.EQ.3                                                   659
            IF (SERN.EQ.3) THEN                                           659
C       Geant POSITION : FPBB ===> X=X2 Y=Y2 Z=Z2-DZSB DZ=PRSW_DZBBD(3)/  660
C       4.0 DX=DX2 DY=DY2 <===                                            660
            CALL AGSRESET                                                 660
            AG_X = X2                                                     660
            AG_Y = Y2                                                     660
            AG_Z = Z2-DZSB                                                660
            AG_DZ = PRSW_DZBBD(3)/4.0                                     660
            AG_DX = DX2                                                   660
            AG_DY = DY2                                                   660
            AG_TITLE = 'FPBB'                                             660
            AG_EXNAME = 'POSITION'                                        660
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 660
            CALL AXPOSITION                                               660
C       *                                                                 660
         END IF                                                           661
C    *                                                                    662
C       Check SERN.EQ.3                                                   662
            IF (SERN.EQ.3) THEN                                           662
C       Geant POSITION : FPPB ===> X=X2 Y=Y2 Z=Z2 DZ=PRSW_DZ(3)/2.0 DX=D  663
C       X2 DY=DY2 <===                                                    663
            CALL AGSRESET                                                 663
            AG_X = X2                                                     663
            AG_Y = Y2                                                     663
            AG_Z = Z2                                                     663
            AG_DZ = PRSW_DZ(3)/2.0                                        663
            AG_DX = DX2                                                   663
            AG_DY = DY2                                                   663
            AG_TITLE = 'FPPB'                                             663
            AG_EXNAME = 'POSITION'                                        663
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 663
            CALL AXPOSITION                                               663
C       *                                                                 663
         END IF                                                           664
C    *                                                                    665
C       Check SERN.EQ.3                                                   665
            IF (SERN.EQ.3) THEN                                           665
C       Geant POSITION : FPBB ===> X=X2 Y=Y2 Z=Z2+DZSB DZ=PRSW_DZBBD(3)/  666
C       4.0 DX=DX2 DY=DY2 <===                                            666
            CALL AGSRESET                                                 666
            AG_X = X2                                                     666
            AG_Y = Y2                                                     666
            AG_Z = Z2+DZSB                                                666
            AG_DZ = PRSW_DZBBD(3)/4.0                                     666
            AG_DX = DX2                                                   666
            AG_DY = DY2                                                   666
            AG_TITLE = 'FPBB'                                             666
            AG_EXNAME = 'POSITION'                                        666
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 666
            CALL AXPOSITION                                               666
C       *                                                                 666
         END IF                                                           667
C    *                                                                    668
C       Check SERN.EQ.3                                                   668
            IF (SERN.EQ.3) THEN                                           668
C       Geant POSITION : FPBB ===> X=-X2 Y=Y2 Z=Z2-DZSB DZ=PRSW_DZBBD(3)  669
C       /4.0 DX=DX2 DY=DY2 <===                                           669
            CALL AGSRESET                                                 669
            AG_X = -X2                                                    669
            AG_Y = Y2                                                     669
            AG_Z = Z2-DZSB                                                669
            AG_DZ = PRSW_DZBBD(3)/4.0                                     669
            AG_DX = DX2                                                   669
            AG_DY = DY2                                                   669
            AG_TITLE = 'FPBB'                                             669
            AG_EXNAME = 'POSITION'                                        669
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 669
            CALL AXPOSITION                                               669
C       *                                                                 669
         END IF                                                           670
C    *                                                                    671
C       Check SERN.EQ.3                                                   671
            IF (SERN.EQ.3) THEN                                           671
C       Geant POSITION : FPPB ===> X=-X2 Y=Y2 Z=Z2 DZ=PRSW_DZ(3)/2.0 DX=  672
C       DX2 DY=DY2 <===                                                   672
            CALL AGSRESET                                                 672
            AG_X = -X2                                                    672
            AG_Y = Y2                                                     672
            AG_Z = Z2                                                     672
            AG_DZ = PRSW_DZ(3)/2.0                                        672
            AG_DX = DX2                                                   672
            AG_DY = DY2                                                   672
            AG_TITLE = 'FPPB'                                             672
            AG_EXNAME = 'POSITION'                                        672
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 672
            CALL AXPOSITION                                               672
C       *                                                                 672
         END IF                                                           673
C    *                                                                    674
C       Check SERN.EQ.3                                                   674
            IF (SERN.EQ.3) THEN                                           674
C       Geant POSITION : FPBB ===> X=-X2 Y=Y2 Z=Z2+DZSB DZ=PRSW_DZBBD(3)  675
C       /4.0 DX=DX2 DY=DY2 <===                                           675
            CALL AGSRESET                                                 675
            AG_X = -X2                                                    675
            AG_Y = Y2                                                     675
            AG_Z = Z2+DZSB                                                675
            AG_DZ = PRSW_DZBBD(3)/4.0                                     675
            AG_DX = DX2                                                   675
            AG_DY = DY2                                                   675
            AG_TITLE = 'FPBB'                                             675
            AG_EXNAME = 'POSITION'                                        675
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 675
            CALL AXPOSITION                                               675
C       *                                                                 675
         END IF                                                           676
         DX1A=(PRSW_DHOLE - PRSW_DXBBD(1) - PRSW_NSKIPV*PRSW_DPRS(1))/    677
     *   2.0                                                              677
         DX1B=(PRSW_DHOLE - PRSW_DXBBD(1))/2.0                            678
C    Check SERN.LE.4                                                      679
         IF (SERN.LE.4) THEN                                              679
         DX1=DX1A                                                         680
         X2=PRSW_XOFF(SERN)+PRSW_NSKIPV*PRSW_DPRS(1)+DX1                  681
         ELSE                                                             682
         DX1=DX1B                                                         683
         X2=PRSW_XOFF(SERN)+DX1                                           684
         ENDIF                                                            685
         DY1=(LPRS3 - PRSW_DHOLE - PRSW_DYCUT + PRSW_DXBBD(1) +           686
     *   PRSW_DXBBD(2))/2.0                                               686
         Y2=PRSW_DHOLE - PRSW_DXBBD(1) + DY1                              687
         DXX=DX1                                                          687
         DYY=DY1                                                          687
C    *                                                                    689
C       Check SERN.NE.3                                                   689
            IF (SERN.NE.3) THEN                                           689
C       Geant POSITION : FPBB ===> X=X2 Y=-Y2 Z=Z2 DZ=DZ1 DX=DX1 DY=DY1   690
C       <===                                                              690
            CALL AGSRESET                                                 690
            AG_X = X2                                                     690
            AG_Y = -Y2                                                    690
            AG_Z = Z2                                                     690
            AG_DZ = DZ1                                                   690
            AG_DX = DX1                                                   690
            AG_DY = DY1                                                   690
            AG_TITLE = 'FPBB'                                             690
            AG_EXNAME = 'POSITION'                                        690
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 690
            CALL AXPOSITION                                               690
C       *                                                                 690
         END IF                                                           691
C    *                                                                    692
C       Check SERN.NE.3                                                   692
            IF (SERN.NE.3) THEN                                           692
C       Geant POSITION : FPBB ===> X=-X2 Y=-Y2 Z=Z2 DZ=DZ1 DX=DX1 DY=DY1  693
C        <===                                                             693
            CALL AGSRESET                                                 693
            AG_X = -X2                                                    693
            AG_Y = -Y2                                                    693
            AG_Z = Z2                                                     693
            AG_DZ = DZ1                                                   693
            AG_DX = DX1                                                   693
            AG_DY = DY1                                                   693
            AG_TITLE = 'FPBB'                                             693
            AG_EXNAME = 'POSITION'                                        693
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 693
            CALL AXPOSITION                                               693
C       *                                                                 693
         END IF                                                           694
C    *                                                                    695
C       Check SERN.EQ.3                                                   695
            IF (SERN.EQ.3) THEN                                           695
C       Geant POSITION : FPBB ===> X=X2 Y=-Y2 Z=Z2-DZSB DZ=PRSW_DZBBD(3)  696
C       /4.0 DX=DX1 DY=DY1 <===                                           696
            CALL AGSRESET                                                 696
            AG_X = X2                                                     696
            AG_Y = -Y2                                                    696
            AG_Z = Z2-DZSB                                                696
            AG_DZ = PRSW_DZBBD(3)/4.0                                     696
            AG_DX = DX1                                                   696
            AG_DY = DY1                                                   696
            AG_TITLE = 'FPBB'                                             696
            AG_EXNAME = 'POSITION'                                        696
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 696
            CALL AXPOSITION                                               696
C       *                                                                 696
         END IF                                                           697
C    *                                                                    698
C       Check SERN.EQ.3                                                   698
            IF (SERN.EQ.3) THEN                                           698
C       Geant POSITION : FPPB ===> X=X2 Y=-Y2 Z=Z2 DZ=PRSW_DZ(3)/2.0 DX=  699
C       DX1 DY=DY1 <===                                                   699
            CALL AGSRESET                                                 699
            AG_X = X2                                                     699
            AG_Y = -Y2                                                    699
            AG_Z = Z2                                                     699
            AG_DZ = PRSW_DZ(3)/2.0                                        699
            AG_DX = DX1                                                   699
            AG_DY = DY1                                                   699
            AG_TITLE = 'FPPB'                                             699
            AG_EXNAME = 'POSITION'                                        699
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 699
            CALL AXPOSITION                                               699
C       *                                                                 699
         END IF                                                           700
C    *                                                                    701
C       Check SERN.EQ.3                                                   701
            IF (SERN.EQ.3) THEN                                           701
C       Geant POSITION : FPBB ===> X=X2 Y=-Y2 Z=Z2+DZSB DZ=PRSW_DZBBD(3)  702
C       /4.0 DX=DX1 DY=DY1 <===                                           702
            CALL AGSRESET                                                 702
            AG_X = X2                                                     702
            AG_Y = -Y2                                                    702
            AG_Z = Z2+DZSB                                                702
            AG_DZ = PRSW_DZBBD(3)/4.0                                     702
            AG_DX = DX1                                                   702
            AG_DY = DY1                                                   702
            AG_TITLE = 'FPBB'                                             702
            AG_EXNAME = 'POSITION'                                        702
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 702
            CALL AXPOSITION                                               702
C       *                                                                 702
         END IF                                                           703
C    *                                                                    704
C       Check SERN.EQ.3                                                   704
            IF (SERN.EQ.3) THEN                                           704
C       Geant POSITION : FPBB ===> X=-X2 Y=-Y2 Z=Z2-DZSB DZ=PRSW_DZBBD(3  705
C       )/4.0 DX=DX1 DY=DY1 <===                                          705
            CALL AGSRESET                                                 705
            AG_X = -X2                                                    705
            AG_Y = -Y2                                                    705
            AG_Z = Z2-DZSB                                                705
            AG_DZ = PRSW_DZBBD(3)/4.0                                     705
            AG_DX = DX1                                                   705
            AG_DY = DY1                                                   705
            AG_TITLE = 'FPBB'                                             705
            AG_EXNAME = 'POSITION'                                        705
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 705
            CALL AXPOSITION                                               705
C       *                                                                 705
         END IF                                                           706
C    *                                                                    707
C       Check SERN.EQ.3                                                   707
            IF (SERN.EQ.3) THEN                                           707
C       Geant POSITION : FPPB ===> X=-X2 Y=-Y2 Z=Z2 DZ=PRSW_DZ(3)/2.0 DX  708
C       =DX1 DY=DY1 <===                                                  708
            CALL AGSRESET                                                 708
            AG_X = -X2                                                    708
            AG_Y = -Y2                                                    708
            AG_Z = Z2                                                     708
            AG_DZ = PRSW_DZ(3)/2.0                                        708
            AG_DX = DX1                                                   708
            AG_DY = DY1                                                   708
            AG_TITLE = 'FPPB'                                             708
            AG_EXNAME = 'POSITION'                                        708
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 708
            CALL AXPOSITION                                               708
C       *                                                                 708
         END IF                                                           709
C    *                                                                    710
C       Check SERN.EQ.3                                                   710
            IF (SERN.EQ.3) THEN                                           710
C       Geant POSITION : FPBB ===> X=-X2 Y=-Y2 Z=Z2+DZSB DZ=PRSW_DZBBD(3  711
C       )/4.0 DX=DX1 DY=DY1 <===                                          711
            CALL AGSRESET                                                 711
            AG_X = -X2                                                    711
            AG_Y = -Y2                                                    711
            AG_Z = Z2+DZSB                                                711
            AG_DZ = PRSW_DZBBD(3)/4.0                                     711
            AG_DX = DX1                                                   711
            AG_DY = DY1                                                   711
            AG_TITLE = 'FPBB'                                             711
            AG_EXNAME = 'POSITION'                                        711
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 711
            CALL AXPOSITION                                               711
C       *                                                                 711
         END IF                                                           712
C    Check SERN.LE.2                                                      713
         IF (SERN.LE.2) THEN                                              713
         DX2=(LPRS2-PRSW_DHOLE+PRSW_DXBBD(1)+PRSW_DXBBD(2))/2.0           714
         ELSE                                                             715
         DX2=(LPRS2-PRSW_DHOLE+PRSW_DXBBD(1)+PRSW_DXBBD(2))/2.0           716
         ENDIF                                                            717
         DY2=(LPRS3 - PRSW_DYCUT + PRSW_DXBBD(2))/2.0                     718
         X2=PRSW_XOFF(SERN) + DX1B*2 + DX2                                719
         Y2=DY2                                                           720
         DXX=DX2                                                          720
         DYY=DY2                                                          720
C    *                                                                    722
C       Check SERN.NE.3                                                   722
            IF (SERN.NE.3) THEN                                           722
C       Geant POSITION : FPBB ===> X=X2 Y=-Y2 Z=Z2 DZ=DZ1 DX=DX2 DY=DY2   723
C       <===                                                              723
            CALL AGSRESET                                                 723
            AG_X = X2                                                     723
            AG_Y = -Y2                                                    723
            AG_Z = Z2                                                     723
            AG_DZ = DZ1                                                   723
            AG_DX = DX2                                                   723
            AG_DY = DY2                                                   723
            AG_TITLE = 'FPBB'                                             723
            AG_EXNAME = 'POSITION'                                        723
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 723
            CALL AXPOSITION                                               723
C       *                                                                 723
         END IF                                                           724
C    *                                                                    725
C       Check SERN.NE.3                                                   725
            IF (SERN.NE.3) THEN                                           725
C       Geant POSITION : FPBB ===> X=-X2 Y=-Y2 Z=Z2 DZ=DZ1 DX=DX2 DY=DY2  726
C        <===                                                             726
            CALL AGSRESET                                                 726
            AG_X = -X2                                                    726
            AG_Y = -Y2                                                    726
            AG_Z = Z2                                                     726
            AG_DZ = DZ1                                                   726
            AG_DX = DX2                                                   726
            AG_DY = DY2                                                   726
            AG_TITLE = 'FPBB'                                             726
            AG_EXNAME = 'POSITION'                                        726
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 726
            CALL AXPOSITION                                               726
C       *                                                                 726
         END IF                                                           727
C    *                                                                    728
C       Check SERN.EQ.3                                                   728
            IF (SERN.EQ.3) THEN                                           728
C       Geant POSITION : FPBB ===> X=X2 Y=-Y2 Z=Z2-DZSB DZ=PRSW_DZBBD(3)  729
C       /4.0 DX=DX2 DY=DY2 <===                                           729
            CALL AGSRESET                                                 729
            AG_X = X2                                                     729
            AG_Y = -Y2                                                    729
            AG_Z = Z2-DZSB                                                729
            AG_DZ = PRSW_DZBBD(3)/4.0                                     729
            AG_DX = DX2                                                   729
            AG_DY = DY2                                                   729
            AG_TITLE = 'FPBB'                                             729
            AG_EXNAME = 'POSITION'                                        729
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 729
            CALL AXPOSITION                                               729
C       *                                                                 729
         END IF                                                           730
C    *                                                                    731
C       Check SERN.EQ.3                                                   731
            IF (SERN.EQ.3) THEN                                           731
C       Geant POSITION : FPPB ===> X=X2 Y=-Y2 Z=Z2 DZ=PRSW_DZ(3)/2.0 DX=  732
C       DX2 DY=DY2 <===                                                   732
            CALL AGSRESET                                                 732
            AG_X = X2                                                     732
            AG_Y = -Y2                                                    732
            AG_Z = Z2                                                     732
            AG_DZ = PRSW_DZ(3)/2.0                                        732
            AG_DX = DX2                                                   732
            AG_DY = DY2                                                   732
            AG_TITLE = 'FPPB'                                             732
            AG_EXNAME = 'POSITION'                                        732
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 732
            CALL AXPOSITION                                               732
C       *                                                                 732
         END IF                                                           733
C    *                                                                    734
C       Check SERN.EQ.3                                                   734
            IF (SERN.EQ.3) THEN                                           734
C       Geant POSITION : FPBB ===> X=X2 Y=-Y2 Z=Z2+DZSB DZ=PRSW_DZBBD(3)  735
C       /4.0 DX=DX2 DY=DY2 <===                                           735
            CALL AGSRESET                                                 735
            AG_X = X2                                                     735
            AG_Y = -Y2                                                    735
            AG_Z = Z2+DZSB                                                735
            AG_DZ = PRSW_DZBBD(3)/4.0                                     735
            AG_DX = DX2                                                   735
            AG_DY = DY2                                                   735
            AG_TITLE = 'FPBB'                                             735
            AG_EXNAME = 'POSITION'                                        735
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 735
            CALL AXPOSITION                                               735
C       *                                                                 735
         END IF                                                           736
C    *                                                                    737
C       Check SERN.EQ.3                                                   737
            IF (SERN.EQ.3) THEN                                           737
C       Geant POSITION : FPBB ===> X=-X2 Y=-Y2 Z=Z2-DZSB DZ=PRSW_DZBBD(3  738
C       )/4.0 DX=DX2 DY=DY2 <===                                          738
            CALL AGSRESET                                                 738
            AG_X = -X2                                                    738
            AG_Y = -Y2                                                    738
            AG_Z = Z2-DZSB                                                738
            AG_DZ = PRSW_DZBBD(3)/4.0                                     738
            AG_DX = DX2                                                   738
            AG_DY = DY2                                                   738
            AG_TITLE = 'FPBB'                                             738
            AG_EXNAME = 'POSITION'                                        738
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 738
            CALL AXPOSITION                                               738
C       *                                                                 738
         END IF                                                           739
C    *                                                                    740
C       Check SERN.EQ.3                                                   740
            IF (SERN.EQ.3) THEN                                           740
C       Geant POSITION : FPPB ===> X=-X2 Y=-Y2 Z=Z2 DZ=PRSW_DZ(3)/2.0 DX  741
C       =DX2 DY=DY2 <===                                                  741
            CALL AGSRESET                                                 741
            AG_X = -X2                                                    741
            AG_Y = -Y2                                                    741
            AG_Z = Z2                                                     741
            AG_DZ = PRSW_DZ(3)/2.0                                        741
            AG_DX = DX2                                                   741
            AG_DY = DY2                                                   741
            AG_TITLE = 'FPPB'                                             741
            AG_EXNAME = 'POSITION'                                        741
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 741
            CALL AXPOSITION                                               741
C       *                                                                 741
         END IF                                                           742
C    *                                                                    743
C       Check SERN.EQ.3                                                   743
            IF (SERN.EQ.3) THEN                                           743
C       Geant POSITION : FPBB ===> X=-X2 Y=-Y2 Z=Z2+DZSB DZ=PRSW_DZBBD(3  744
C       )/4.0 DX=DX2 DY=DY2 <===                                          744
            CALL AGSRESET                                                 744
            AG_X = -X2                                                    744
            AG_Y = -Y2                                                    744
            AG_Z = Z2+DZSB                                                744
            AG_DZ = PRSW_DZBBD(3)/4.0                                     744
            AG_DX = DX2                                                   744
            AG_DY = DY2                                                   744
            AG_TITLE = 'FPBB'                                             744
            AG_EXNAME = 'POSITION'                                        744
            AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                 744
            CALL AXPOSITION                                               744
C       *                                                                 744
         END IF                                                           745
5391  CALL AGSPOPB                                                        746
      GO TO BLOCK_FPLY, (5390 ,5400,5410,5420,5430)                       746
C End of Local Procedure                                                  746
C ----------------------------------------------------------------------  747
C ------------ FPBB                                                       747
C Begin Local Procedure FPBB                                              748
C FPBB IS THE G10 BACK BOARD                                              748
5520  CONTINUE                                                            748
         AG_TITLE='FPBB'                                                  748
         AG_MODULE=MODULE                                                 748
         AG_MARK='V00'                                                    748
         CALL AGSPUSH                                                     748
         AG_MODULE=MODULE                                                 748
C    *                                                                    748
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===          749
         CALL AGSRESET                                                    749
         AG_A = 28.08                                                     749
         AG_Z = 14                                                        749
         AG_W = 0.6*1*28./60.                                             749
         AG_TITLE = 'SI'                                                  749
         AG_EXNAME = 'COMPONENT'                                          749
         AG_PARLIST = 'A_Z_W'                                             749
         CALL AXCOMPONENT                                                 749
C    *                                                                    749
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. <===               749
         CALL AGSRESET                                                    750
         AG_A = 16                                                        750
         AG_Z = 8                                                         750
         AG_W = 0.6*2*16./60.                                             750
         AG_TITLE = 'O'                                                   750
         AG_EXNAME = 'COMPONENT'                                          750
         AG_PARLIST = 'A_Z_W'                                             750
         CALL AXCOMPONENT                                                 750
C    *                                                                    750
C    Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===              750
         CALL AGSRESET                                                    751
         AG_A = 12                                                        751
         AG_Z = 6                                                         751
         AG_W = 0.4*8*12./174.                                            751
         AG_TITLE = 'C'                                                   751
         AG_EXNAME = 'COMPONENT'                                          751
         AG_PARLIST = 'A_Z_W'                                             751
         CALL AXCOMPONENT                                                 751
C    *                                                                    751
C    Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===               751
         CALL AGSRESET                                                    752
         AG_A = 1                                                         752
         AG_Z = 1                                                         752
         AG_W = 0.4*14*1./174.                                            752
         AG_TITLE = 'H'                                                   752
         AG_EXNAME = 'COMPONENT'                                          752
         AG_PARLIST = 'A_Z_W'                                             752
         CALL AXCOMPONENT                                                 752
C    *                                                                    752
C    Geant COMPONENT : O ===> A=16 Z=8 W=0.4*4*16./174. <===              752
         CALL AGSRESET                                                    753
         AG_A = 16                                                        753
         AG_Z = 8                                                         753
         AG_W = 0.4*4*16./174.                                            753
         AG_TITLE = 'O'                                                   753
         AG_EXNAME = 'COMPONENT'                                          753
         AG_PARLIST = 'A_Z_W'                                             753
         CALL AXCOMPONENT                                                 753
C    *                                                                    753
C    Geant MIXTURE : G10 ===> DENS=1.7 <===                               753
         CALL AGSRESET                                                    754
         AG_DENS = 1.7                                                    754
         AG_TITLE = 'G10'                                                 754
         AG_EXNAME = 'MIXTURE'                                            754
         AG_PARLIST = 'DENS'                                              754
         CALL AXMIXTURE                                                   754
C    *                                                                    754
C    Geant ATTRIBUTE : FPBB ===> SEEN=1 COLO=7 <===                       754
         CALL AGSRESET                                                    755
         AG_SEEN = 1                                                      755
         AG_COLO = 7                                                      755
         AG_TITLE = 'FPBB'                                                755
         AG_EXNAME = 'ATTRIBUTE'                                          755
         AG_PARLIST = 'SEEN_COLO'                                         755
         CALL AXATTRIBUTE                                                 755
C    *                                                                    755
C    Geant SHAPE : BOX ===> DX=0.0 DY=0.0 DZ=0.0 <===                     755
         CALL AGSRESET                                                    756
         AG_DX = 0.0                                                      756
         AG_DY = 0.0                                                      756
         AG_DZ = 0.0                                                      756
         AG_TITLE = 'BOX'                                                 756
         AG_EXNAME = 'SHAPE'                                              756
         AG_PARLIST = 'DX_DY_DZ'                                          756
         CALL AXSHAPE                                                     756
C    *                                                                    756
5521  CALL AGSPOPB                                                        757
      GO TO BLOCK_FPBB, (5520 ,5530)                                      757
C End of Local Procedure                                                  757
C ----------------------------------------------------------------------  758
C ------------ FPPB                                                       758
C Begin Local Procedure FPPB                                              759
C FPPB IS FMS PS PB CONVERTER                                             759
5540  CONTINUE                                                            759
         AG_TITLE='FPPB'                                                  759
         AG_MODULE=MODULE                                                 759
         AG_MARK='V00'                                                    759
         CALL AGSPUSH                                                     759
         AG_MODULE=MODULE                                                 759
C    *                                                                    759
C    Geant MATERIAL : LEAD ===> inherited arguments used <===             760
         CALL AGSRESET                                                    760
         AG_TITLE = 'LEAD'                                                760
         AG_EXNAME = 'MATERIAL'                                           760
         AG_PARLIST = 'NONE'                                              760
         CALL AXMATERIAL                                                  760
C    *                                                                    760
C    Geant ATTRIBUTE : FPPB ===> SEEN=1 COLO=6 <===                       760
         CALL AGSRESET                                                    761
         AG_SEEN = 1                                                      761
         AG_COLO = 6                                                      761
         AG_TITLE = 'FPPB'                                                761
         AG_EXNAME = 'ATTRIBUTE'                                          761
         AG_PARLIST = 'SEEN_COLO'                                         761
         CALL AXATTRIBUTE                                                 761
C    *                                                                    761
C    Geant SHAPE : BOX ===> DX=0.0 DY=0.0 DZ=0.0 <===                     761
         CALL AGSRESET                                                    762
         AG_DX = 0.0                                                      762
         AG_DY = 0.0                                                      762
         AG_DZ = 0.0                                                      762
         AG_TITLE = 'BOX'                                                 762
         AG_EXNAME = 'SHAPE'                                              762
         AG_PARLIST = 'DX_DY_DZ'                                          762
         CALL AXSHAPE                                                     762
C    *                                                                    762
5541  CALL AGSPOPB                                                        763
      GO TO BLOCK_FPPB, (5540 ,5550)                                      763
C End of Local Procedure                                                  763
C ----------------------------------------------------------------------  764
C ------------ FPSC                                                       764
C Begin Local Procedure FPSC                                              765
C FPSC IS A PIECE OF SCINTILLATOR IN FMS PRESHOWER                        765
5440  CONTINUE                                                            765
         AG_TITLE='FPSC'                                                  765
         AG_MODULE=MODULE                                                 765
         AG_MARK='V00'                                                    765
         CALL AGSPUSH                                                     765
         AG_MODULE=MODULE                                                 765
C    *                                                                    765
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       766
         CALL AGSRESET                                                    766
         AG_TITLE = 'POLYSTYREN'                                          766
         AG_EXNAME = 'MATERIAL'                                           766
         AG_PARLIST = 'NONE'                                              766
         CALL AXMATERIAL                                                  766
C    *                                                                    766
C    Geant MATERIAL : FMSPS_SCINT ===> ISVOL=1 <===                       766
         CALL AGSRESET                                                    767
         AG_ISVOL = 1                                                     767
         AG_TITLE = 'FMSPS_SCINT'                                         767
         AG_EXNAME = 'MATERIAL'                                           767
         AG_PARLIST = 'ISVOL'                                             767
         CALL AXMATERIAL                                                  767
C    *                                                                    767
C    Geant ATTRIBUTE : FPSC ===> SEEN=1 COLO=4 <===                       767
         CALL AGSRESET                                                    768
         AG_SEEN = 1                                                      768
         AG_COLO = 4                                                      768
         AG_TITLE = 'FPSC'                                                768
         AG_EXNAME = 'ATTRIBUTE'                                          768
         AG_PARLIST = 'SEEN_COLO'                                         768
         CALL AXATTRIBUTE                                                 768
C    *                                                                    768
C    Geant SHAPE : BOX ===> DX=0.0 DY=0.0 DZ=0.0 <===                     768
         CALL AGSRESET                                                    769
         AG_DX = 0.0                                                      769
         AG_DY = 0.0                                                      769
         AG_DZ = 0.0                                                      769
         AG_TITLE = 'BOX'                                                 769
         AG_EXNAME = 'SHAPE'                                              769
         AG_PARLIST = 'DX_DY_DZ'                                          769
         CALL AXSHAPE                                                     769
C    *                                                                    769
         CALL GSTPAR(AG_IMED, 'CUTGAM', 0.00008)                          769
         CALL GSTPAR(AG_IMED, 'CUTELE', 0.001)                            771
         CALL GSTPAR(AG_IMED, 'BCUTE', 0.0001)                            772
         CALL GSTPAR(AG_IMED, 'BIRK1', 1.)                                773
         CALL GSTPAR(AG_IMED, 'BIRK2', 0.013)                             774
         CALL GSTPAR(AG_IMED, 'BIRK3', 9.6E-6)                            775
C    Defining Hits: BIRK:0:(0,10)                                         776
         AG_TITLE = 'FPSC'                                                776
         AG_EXNAME = 'HITS'                                               776
         AG_TYPE=AG_MAGIC                                                 776
         CALL AGSDETV('FPSC')                                             776
C    *                                                                    776
         AG_PARLIST='BIRK'                                                776
         AG_TITLE = 'DETH'                                                776
         AG_NBITS = 0                                                     776
         AG_BIN=AG_MAGIC                                                  776
         AG_OPTION='*'                                                    776
         AG_FHMIN = 0                                                     776
         AG_FHMAX = 10                                                    776
         CALL AGSDETH (0)                                                 776
C    *                                                                    776
         CALL AGSDETU('FPSC')                                             776
C    *                                                                    776
5441  CALL AGSPOPB                                                        777
      GO TO BLOCK_FPSC, (5440 ,5450)                                      777
C End of Local Procedure                                                  777
C ----------------------------------------------------------------------  778
C ------------ FLIG                                                       778
C Begin Local Procedure FLIG                                              779
C FLIG IS A LIGHT GUIDE + SIPM BOARD FOR PRESHOWER                        779
5460  CONTINUE                                                            779
         AG_TITLE='FLIG'                                                  779
         AG_MODULE=MODULE                                                 779
         AG_MARK='V00'                                                    779
         CALL AGSPUSH                                                     779
         AG_MODULE=MODULE                                                 779
C    *                                                                    779
C    Geant MATERIAL : AIR ===> inherited arguments used <===              780
         CALL AGSRESET                                                    780
         AG_TITLE = 'AIR'                                                 780
         AG_EXNAME = 'MATERIAL'                                           780
         AG_PARLIST = 'NONE'                                              780
         CALL AXMATERIAL                                                  780
C    *                                                                    780
C    Geant ATTRIBUTE : FLIG ===> SEEN=1 COLO=6 SERIAL=1 <===              780
         CALL AGSRESET                                                    781
         AG_SEEN = 1                                                      781
         AG_COLO = 6                                                      781
         AG_SERIAL = 1                                                    781
         AG_TITLE = 'FLIG'                                                781
         AG_EXNAME = 'ATTRIBUTE'                                          781
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  781
         CALL AXATTRIBUTE                                                 781
C    *                                                                    781
C    Geant SHAPE : BOX ===> DX=PRSW_DPRS(1)/2.0 DY=PRSW_DZ(1)/2.0+PRSW_D  781
C    SIPM(1)/2.0, DZ=DLG <===                                             781
         CALL AGSRESET                                                    783
         AG_DX = PRSW_DPRS(1)/2.0                                         783
         AG_DY = PRSW_DZ(1)/2.0+PRSW_DSIPM(1)/2.0                         783
         AG_DZ = DLG                                                      783
         AG_TITLE = 'BOX'                                                 783
         AG_EXNAME = 'SHAPE'                                              783
         AG_PARLIST = 'DX_DY_DZ'                                          783
         CALL AXSHAPE                                                     783
C    *                                                                    783
C    Create FLBA :                                                        784
C    Call To Local Procedure FLBA                                         784
         ASSIGN 5670 TO BLOCK_FLBA                                        784
         GO TO  5660                                                      784
5670     CONTINUE                                                         784
C    *                                                                    784
C    Create FLCO :                                                        785
C    Call To Local Procedure FLCO                                         785
         ASSIGN 5690 TO BLOCK_FLCO                                        785
         GO TO  5680                                                      785
5690     CONTINUE                                                         785
C    *                                                                    785
C    Create FPMT :                                                        786
C    Call To Local Procedure FPMT                                         786
         ASSIGN 5710 TO BLOCK_FPMT                                        786
         GO TO  5700                                                      786
5710     CONTINUE                                                         786
C    *                                                                    786
C    Create FFEE :                                                        787
C    Call To Local Procedure FFEE                                         787
         ASSIGN 5730 TO BLOCK_FFEE                                        787
         GO TO  5720                                                      787
5730     CONTINUE                                                         787
C    *                                                                    787
C    Geant POSITION : FLBA ===> X=0 Y=-PRSW_DSIPM(1)/2.0 Z=PRSW_DLG(3)/2  788
C    .0-DLG <===                                                          788
         CALL AGSRESET                                                    788
         AG_X = 0                                                         788
         AG_Y = -PRSW_DSIPM(1)/2.0                                        788
         AG_Z = PRSW_DLG(3)/2.0-DLG                                       788
         AG_TITLE = 'FLBA'                                                788
         AG_EXNAME = 'POSITION'                                           788
         AG_PARLIST = 'X_Y_Z'                                             788
         CALL AXPOSITION                                                  788
C    *                                                                    788
C    Geant POSITION : FLCO ===> X=-PRSW_DPRS(1)/4.0 Y=-PRSW_DSIPM(1)/2.0  788
C     Z=PRSW_DLG(3)+PRSW_DLG(2)/2.0-DLG <===                              788
         CALL AGSRESET                                                    790
         AG_X = -PRSW_DPRS(1)/4.0                                         790
         AG_Y = -PRSW_DSIPM(1)/2.0                                        790
         AG_Z = PRSW_DLG(3)+PRSW_DLG(2)/2.0-DLG                           790
         AG_TITLE = 'FLCO'                                                790
         AG_EXNAME = 'POSITION'                                           790
         AG_PARLIST = 'X_Y_Z'                                             790
         CALL AXPOSITION                                                  790
C    *                                                                    790
C    Geant POSITION : FLCO ===> X=+PRSW_DPRS(1)/4.0 Y=-PRSW_DSIPM(1)/2.0  790
C     Z=PRSW_DLG(3)+PRSW_DLG(2)/2.0-DLG <===                              790
         CALL AGSRESET                                                    792
         AG_X = +PRSW_DPRS(1)/4.0                                         792
         AG_Y = -PRSW_DSIPM(1)/2.0                                        792
         AG_Z = PRSW_DLG(3)+PRSW_DLG(2)/2.0-DLG                           792
         AG_TITLE = 'FLCO'                                                792
         AG_EXNAME = 'POSITION'                                           792
         AG_PARLIST = 'X_Y_Z'                                             792
         CALL AXPOSITION                                                  792
C    *                                                                    792
C    Geant POSITION : FPMT ===> X=0 Y=-PRSW_DSIPM(1)/2.0 Z=DLG-PRSW_DSIP  792
C    M(1)/2.0 <===                                                        792
         CALL AGSRESET                                                    793
         AG_X = 0                                                         793
         AG_Y = -PRSW_DSIPM(1)/2.0                                        793
         AG_Z = DLG-PRSW_DSIPM(1)/2.0                                     793
         AG_TITLE = 'FPMT'                                                793
         AG_EXNAME = 'POSITION'                                           793
         AG_PARLIST = 'X_Y_Z'                                             793
         CALL AXPOSITION                                                  793
C    *                                                                    793
C    Geant POSITION : FFEE ===> X=0 Y=PRSW_DZ(1)/2.0 Z=DLG-PRSW_DSIPM(4)  793
C    /2.0 <===                                                            793
         CALL AGSRESET                                                    794
         AG_X = 0                                                         794
         AG_Y = PRSW_DZ(1)/2.0                                            794
         AG_Z = DLG-PRSW_DSIPM(4)/2.0                                     794
         AG_TITLE = 'FFEE'                                                794
         AG_EXNAME = 'POSITION'                                           794
         AG_PARLIST = 'X_Y_Z'                                             794
         CALL AXPOSITION                                                  794
C    *                                                                    794
5461  CALL AGSPOPB                                                        795
      GO TO BLOCK_FLIG, (5460 ,5470)                                      795
C End of Local Procedure                                                  795
C ----------------------------------------------------------------------  796
C ------------ FLLG                                                       796
C Begin Local Procedure FLLG                                              797
C FLLG IS A LIGHT GUIDE + SIPM BOARD FOR PRESHOWER FOR LARGE              797
5480  CONTINUE                                                            797
         AG_TITLE='FLLG'                                                  797
         AG_MODULE=MODULE                                                 797
         AG_MARK='V00'                                                    797
         CALL AGSPUSH                                                     797
         AG_MODULE=MODULE                                                 797
C    *                                                                    797
C    Geant MATERIAL : AIR ===> inherited arguments used <===              798
         CALL AGSRESET                                                    798
         AG_TITLE = 'AIR'                                                 798
         AG_EXNAME = 'MATERIAL'                                           798
         AG_PARLIST = 'NONE'                                              798
         CALL AXMATERIAL                                                  798
C    *                                                                    798
C    Geant ATTRIBUTE : FLLG ===> SEEN=1 COLO=6 SERIAL=1 <===              798
         CALL AGSRESET                                                    799
         AG_SEEN = 1                                                      799
         AG_COLO = 6                                                      799
         AG_SERIAL = 1                                                    799
         AG_TITLE = 'FLLG'                                                799
         AG_EXNAME = 'ATTRIBUTE'                                          799
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  799
         CALL AXATTRIBUTE                                                 799
C    *                                                                    799
C    Geant SHAPE : BOX ===> DX=PRSW_DPRS(2)/2.0 DY=PRSW_DZ(1)/2.0+PRSW_D  799
C    SIPM(1)/2.0, DZ=DLG <===                                             799
         CALL AGSRESET                                                    801
         AG_DX = PRSW_DPRS(2)/2.0                                         801
         AG_DY = PRSW_DZ(1)/2.0+PRSW_DSIPM(1)/2.0                         801
         AG_DZ = DLG                                                      801
         AG_TITLE = 'BOX'                                                 801
         AG_EXNAME = 'SHAPE'                                              801
         AG_PARLIST = 'DX_DY_DZ'                                          801
         CALL AXSHAPE                                                     801
C    *                                                                    801
C    Create FBLL :                                                        802
C    Call To Local Procedure FBLL                                         802
         ASSIGN 5750 TO BLOCK_FBLL                                        802
         GO TO  5740                                                      802
5750     CONTINUE                                                         802
C    *                                                                    802
C    Create FCLL :                                                        803
C    Call To Local Procedure FCLL                                         803
         ASSIGN 5770 TO BLOCK_FCLL                                        803
         GO TO  5760                                                      803
5770     CONTINUE                                                         803
C    *                                                                    803
C    Create FPMT :                                                        804
C    Call To Local Procedure FPMT                                         804
         ASSIGN 5780 TO BLOCK_FPMT                                        804
         GO TO  5700                                                      804
5780     CONTINUE                                                         804
C    *                                                                    804
C    Create FFEE :                                                        805
C    Call To Local Procedure FFEE                                         805
         ASSIGN 5790 TO BLOCK_FFEE                                        805
         GO TO  5720                                                      805
5790     CONTINUE                                                         805
C    *                                                                    805
C    Geant POSITION : FBLL ===> X=0 Y=-PRSW_DSIPM(1)/2.0 Z=PRSW_DLG(3)/2  806
C    .0-DLG <===                                                          806
         CALL AGSRESET                                                    806
         AG_X = 0                                                         806
         AG_Y = -PRSW_DSIPM(1)/2.0                                        806
         AG_Z = PRSW_DLG(3)/2.0-DLG                                       806
         AG_TITLE = 'FBLL'                                                806
         AG_EXNAME = 'POSITION'                                           806
         AG_PARLIST = 'X_Y_Z'                                             806
         CALL AXPOSITION                                                  806
C    *                                                                    806
C    Geant POSITION : FCLL ===> X=-PRSW_DPRS(2)/4.0 Y=-PRSW_DSIPM(1)/2.0  806
C     Z=PRSW_DLG(3)+PRSW_DLG(2)/2-DLG <===                                806
         CALL AGSRESET                                                    808
         AG_X = -PRSW_DPRS(2)/4.0                                         808
         AG_Y = -PRSW_DSIPM(1)/2.0                                        808
         AG_Z = PRSW_DLG(3)+PRSW_DLG(2)/2-DLG                             808
         AG_TITLE = 'FCLL'                                                808
         AG_EXNAME = 'POSITION'                                           808
         AG_PARLIST = 'X_Y_Z'                                             808
         CALL AXPOSITION                                                  808
C    *                                                                    808
C    Geant POSITION : FCLL ===> X=PRSW_DPRS(2)/4.0 Y=-PRSW_DSIPM(1)/2.0   808
C    Z=PRSW_DLG(3)+PRSW_DLG(2)/2-DLG <===                                 808
         CALL AGSRESET                                                    810
         AG_X = PRSW_DPRS(2)/4.0                                          810
         AG_Y = -PRSW_DSIPM(1)/2.0                                        810
         AG_Z = PRSW_DLG(3)+PRSW_DLG(2)/2-DLG                             810
         AG_TITLE = 'FCLL'                                                810
         AG_EXNAME = 'POSITION'                                           810
         AG_PARLIST = 'X_Y_Z'                                             810
         CALL AXPOSITION                                                  810
C    *                                                                    810
C    Geant POSITION : FPMT ===> X=0 Y=-PRSW_DSIPM(1)/2.0 Z=DLG-PRSW_DSIP  810
C    M(1)/2.0 <===                                                        810
         CALL AGSRESET                                                    811
         AG_X = 0                                                         811
         AG_Y = -PRSW_DSIPM(1)/2.0                                        811
         AG_Z = DLG-PRSW_DSIPM(1)/2.0                                     811
         AG_TITLE = 'FPMT'                                                811
         AG_EXNAME = 'POSITION'                                           811
         AG_PARLIST = 'X_Y_Z'                                             811
         CALL AXPOSITION                                                  811
C    *                                                                    811
C    Geant POSITION : FFEE ===> X=0 Y=PRSW_DZ(1)/2.0 Z=DLG-PRSW_DSIPM(4)  811
C    /2.0 <===                                                            811
         CALL AGSRESET                                                    812
         AG_X = 0                                                         812
         AG_Y = PRSW_DZ(1)/2.0                                            812
         AG_Z = DLG-PRSW_DSIPM(4)/2.0                                     812
         AG_TITLE = 'FFEE'                                                812
         AG_EXNAME = 'POSITION'                                           812
         AG_PARLIST = 'X_Y_Z'                                             812
         CALL AXPOSITION                                                  812
C    *                                                                    812
5481  CALL AGSPOPB                                                        813
      GO TO BLOCK_FLLG, (5480 ,5490)                                      813
C End of Local Procedure                                                  813
C ----------------------------------------------------------------------  814
C ------------ FLBA                                                       814
C Begin Local Procedure FLBA                                              815
C FLBA IS A LIGHT GUIDE BASE FOR PRESHOWER                                815
5660  CONTINUE                                                            815
         AG_TITLE='FLBA'                                                  815
         AG_MODULE=MODULE                                                 815
         AG_MARK='V00'                                                    815
         CALL AGSPUSH                                                     815
         AG_MODULE=MODULE                                                 815
C    *                                                                    815
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       816
         CALL AGSRESET                                                    816
         AG_TITLE = 'POLYSTYREN'                                          816
         AG_EXNAME = 'MATERIAL'                                           816
         AG_PARLIST = 'NONE'                                              816
         CALL AXMATERIAL                                                  816
C    *                                                                    816
C    Geant ATTRIBUTE : FLBA ===> SEEN=1 COLO=4 SERIAL=1 <===              816
         CALL AGSRESET                                                    817
         AG_SEEN = 1                                                      817
         AG_COLO = 4                                                      817
         AG_SERIAL = 1                                                    817
         AG_TITLE = 'FLBA'                                                817
         AG_EXNAME = 'ATTRIBUTE'                                          817
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  817
         CALL AXATTRIBUTE                                                 817
C    *                                                                    817
C    Geant SHAPE : BOX ===> DX=PRSW_DPRS(1)/2.0 DY=PRSW_DZ(SERN)/2.0, DZ  817
C    =PRSW_DLG(3)/2.0 <===                                                817
         CALL AGSRESET                                                    819
         AG_DX = PRSW_DPRS(1)/2.0                                         819
         AG_DY = PRSW_DZ(SERN)/2.0                                        819
         AG_DZ = PRSW_DLG(3)/2.0                                          819
         AG_TITLE = 'BOX'                                                 819
         AG_EXNAME = 'SHAPE'                                              819
         AG_PARLIST = 'DX_DY_DZ'                                          819
         CALL AXSHAPE                                                     819
C    *                                                                    819
5661  CALL AGSPOPB                                                        820
      GO TO BLOCK_FLBA, (5660 ,5670)                                      820
C End of Local Procedure                                                  820
C ----------------------------------------------------------------------  821
C ------------ FBLL                                                       821
C Begin Local Procedure FBLL                                              822
C FBLL IS A LIGHT GUIDE BASE FOR PRESHOWER LARGE                          822
5740  CONTINUE                                                            822
         AG_TITLE='FBLL'                                                  822
         AG_MODULE=MODULE                                                 822
         AG_MARK='V00'                                                    822
         CALL AGSPUSH                                                     822
         AG_MODULE=MODULE                                                 822
C    *                                                                    822
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       823
         CALL AGSRESET                                                    823
         AG_TITLE = 'POLYSTYREN'                                          823
         AG_EXNAME = 'MATERIAL'                                           823
         AG_PARLIST = 'NONE'                                              823
         CALL AXMATERIAL                                                  823
C    *                                                                    823
C    Geant ATTRIBUTE : FBLL ===> SEEN=1 COLO=4 SERIAL=1 <===              823
         CALL AGSRESET                                                    824
         AG_SEEN = 1                                                      824
         AG_COLO = 4                                                      824
         AG_SERIAL = 1                                                    824
         AG_TITLE = 'FBLL'                                                824
         AG_EXNAME = 'ATTRIBUTE'                                          824
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  824
         CALL AXATTRIBUTE                                                 824
C    *                                                                    824
C    Geant SHAPE : BOX ===> DX=PRSW_DPRS(2)/2.0 DY=PRSW_DZ(SERN)/2.0, DZ  824
C    =PRSW_DLG(3)/2.0 <===                                                824
         CALL AGSRESET                                                    826
         AG_DX = PRSW_DPRS(2)/2.0                                         826
         AG_DY = PRSW_DZ(SERN)/2.0                                        826
         AG_DZ = PRSW_DLG(3)/2.0                                          826
         AG_TITLE = 'BOX'                                                 826
         AG_EXNAME = 'SHAPE'                                              826
         AG_PARLIST = 'DX_DY_DZ'                                          826
         CALL AXSHAPE                                                     826
C    *                                                                    826
5741  CALL AGSPOPB                                                        827
      GO TO BLOCK_FBLL, (5740 ,5750)                                      827
C End of Local Procedure                                                  827
C ----------------------------------------------------------------------  828
C ------------ FLCO                                                       828
C Begin Local Procedure FLCO                                              829
C FLCO IS A LIGHT GUIDE CONE FOR PRESHOWER                                829
5680  CONTINUE                                                            829
         AG_TITLE='FLCO'                                                  829
         AG_MODULE=MODULE                                                 829
         AG_MARK='V00'                                                    829
         CALL AGSPUSH                                                     829
         AG_MODULE=MODULE                                                 829
C    *                                                                    829
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       830
         CALL AGSRESET                                                    830
         AG_TITLE = 'POLYSTYREN'                                          830
         AG_EXNAME = 'MATERIAL'                                           830
         AG_PARLIST = 'NONE'                                              830
         CALL AXMATERIAL                                                  830
C    *                                                                    830
C    Geant ATTRIBUTE : FLCO ===> SEEN=1 COLO=4 SERIAL=1 <===              830
         CALL AGSRESET                                                    831
         AG_SEEN = 1                                                      831
         AG_COLO = 4                                                      831
         AG_SERIAL = 1                                                    831
         AG_TITLE = 'FLCO'                                                831
         AG_EXNAME = 'ATTRIBUTE'                                          831
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  831
         CALL AXATTRIBUTE                                                 831
C    *                                                                    831
C    Geant SHAPE : TRD2 ===> DX1=PRSW_DPRS(1)/4.0 DX2=PRSW_DLG(1)/2.0, D  831
C    Y1=PRSW_DZ(SERN)/2.0 DY2=PRSW_DLG(1)/2.0, DZ=PRSW_DLG(2)/2.0 <===    831
         CALL AGSRESET                                                    834
         AG_DX1 = PRSW_DPRS(1)/4.0                                        834
         AG_DX2 = PRSW_DLG(1)/2.0                                         834
         AG_DY1 = PRSW_DZ(SERN)/2.0                                       834
         AG_DY2 = PRSW_DLG(1)/2.0                                         834
         AG_DZ = PRSW_DLG(2)/2.0                                          834
         AG_TITLE = 'TRD2'                                                834
         AG_EXNAME = 'SHAPE'                                              834
         AG_PARLIST = 'DX1_DX2_DY1_DY2_DZ'                                834
         CALL AXSHAPE                                                     834
C    *                                                                    834
5681  CALL AGSPOPB                                                        835
      GO TO BLOCK_FLCO, (5680 ,5690)                                      835
C End of Local Procedure                                                  835
C ----------------------------------------------------------------------  836
C ------------ FCLL                                                       836
C Begin Local Procedure FCLL                                              837
C FCLL IS A LIGHT GUIDE CONE FOR PRESHOWER LARGE                          837
5760  CONTINUE                                                            837
         AG_TITLE='FCLL'                                                  837
         AG_MODULE=MODULE                                                 837
         AG_MARK='V00'                                                    837
         CALL AGSPUSH                                                     837
         AG_MODULE=MODULE                                                 837
C    *                                                                    837
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       838
         CALL AGSRESET                                                    838
         AG_TITLE = 'POLYSTYREN'                                          838
         AG_EXNAME = 'MATERIAL'                                           838
         AG_PARLIST = 'NONE'                                              838
         CALL AXMATERIAL                                                  838
C    *                                                                    838
C    Geant ATTRIBUTE : FCLL ===> SEEN=1 COLO=4 SERIAL=1 <===              838
         CALL AGSRESET                                                    839
         AG_SEEN = 1                                                      839
         AG_COLO = 4                                                      839
         AG_SERIAL = 1                                                    839
         AG_TITLE = 'FCLL'                                                839
         AG_EXNAME = 'ATTRIBUTE'                                          839
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  839
         CALL AXATTRIBUTE                                                 839
C    *                                                                    839
C    Geant SHAPE : TRD2 ===> DX1=PRSW_DPRS(2)/4.0 DX2=PRSW_DLG(1)/2.0, D  839
C    Y1=PRSW_DZ(SERN)/2.0 DY2=PRSW_DLG(1)/2.0, DZ=PRSW_DLG(2)/2.0 <===    839
         CALL AGSRESET                                                    842
         AG_DX1 = PRSW_DPRS(2)/4.0                                        842
         AG_DX2 = PRSW_DLG(1)/2.0                                         842
         AG_DY1 = PRSW_DZ(SERN)/2.0                                       842
         AG_DY2 = PRSW_DLG(1)/2.0                                         842
         AG_DZ = PRSW_DLG(2)/2.0                                          842
         AG_TITLE = 'TRD2'                                                842
         AG_EXNAME = 'SHAPE'                                              842
         AG_PARLIST = 'DX1_DX2_DY1_DY2_DZ'                                842
         CALL AXSHAPE                                                     842
C    *                                                                    842
5761  CALL AGSPOPB                                                        843
      GO TO BLOCK_FCLL, (5760 ,5770)                                      843
C End of Local Procedure                                                  843
C ----------------------------------------------------------------------  844
C ------------ FPMT                                                       844
C Begin Local Procedure FPMT                                              845
C FPMT IS A SIPM BOARD FOR PRESHOWER                                      845
5700  CONTINUE                                                            845
         AG_TITLE='FPMT'                                                  845
         AG_MODULE=MODULE                                                 845
         AG_MARK='V00'                                                    845
         CALL AGSPUSH                                                     845
         AG_MODULE=MODULE                                                 845
C    *                                                                    845
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       846
         CALL AGSRESET                                                    846
         AG_TITLE = 'POLYSTYREN'                                          846
         AG_EXNAME = 'MATERIAL'                                           846
         AG_PARLIST = 'NONE'                                              846
         CALL AXMATERIAL                                                  846
C    *                                                                    846
C    Geant ATTRIBUTE : FPMT ===> SEEN=1 COLO=3 SERIAL=1 <===              846
         CALL AGSRESET                                                    847
         AG_SEEN = 1                                                      847
         AG_COLO = 3                                                      847
         AG_SERIAL = 1                                                    847
         AG_TITLE = 'FPMT'                                                847
         AG_EXNAME = 'ATTRIBUTE'                                          847
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  847
         CALL AXATTRIBUTE                                                 847
C    *                                                                    847
C    Geant SHAPE : BOX ===> DX=PRSW_DSIPM(2)/2.0 DY=PRSW_DSIPM(3)/2.0, D  847
C    Z=PRSW_DSIPM(1)/2.0 <===                                             847
         CALL AGSRESET                                                    849
         AG_DX = PRSW_DSIPM(2)/2.0                                        849
         AG_DY = PRSW_DSIPM(3)/2.0                                        849
         AG_DZ = PRSW_DSIPM(1)/2.0                                        849
         AG_TITLE = 'BOX'                                                 849
         AG_EXNAME = 'SHAPE'                                              849
         AG_PARLIST = 'DX_DY_DZ'                                          849
         CALL AXSHAPE                                                     849
C    *                                                                    849
5701  CALL AGSPOPB                                                        850
      GO TO BLOCK_FPMT, (5700 ,5710,5780)                                 850
C End of Local Procedure                                                  850
C ----------------------------------------------------------------------  851
C ------------ FFEE                                                       851
C Begin Local Procedure FFEE                                              852
C FFEE IS A FEE BOARD FOR PRESHOWER                                       852
5720  CONTINUE                                                            852
         AG_TITLE='FFEE'                                                  852
         AG_MODULE=MODULE                                                 852
         AG_MARK='V00'                                                    852
         CALL AGSPUSH                                                     852
         AG_MODULE=MODULE                                                 852
C    *                                                                    852
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       853
         CALL AGSRESET                                                    853
         AG_TITLE = 'POLYSTYREN'                                          853
         AG_EXNAME = 'MATERIAL'                                           853
         AG_PARLIST = 'NONE'                                              853
         CALL AXMATERIAL                                                  853
C    *                                                                    853
C    Geant ATTRIBUTE : FEEE ===> SEEN=1 COLO=3 SERIAL=1 <===              853
         CALL AGSRESET                                                    854
         AG_SEEN = 1                                                      854
         AG_COLO = 3                                                      854
         AG_SERIAL = 1                                                    854
         AG_TITLE = 'FEEE'                                                854
         AG_EXNAME = 'ATTRIBUTE'                                          854
         AG_PARLIST = 'SEEN_COLO_SERIAL'                                  854
         CALL AXATTRIBUTE                                                 854
C    *                                                                    854
C    Geant SHAPE : BOX ===> DX=PRSW_DSIPM(2)/2.0 DY=PRSW_DSIPM(1)/2.0, D  854
C    Z=PRSW_DSIPM(4)/2.0 <===                                             854
         CALL AGSRESET                                                    856
         AG_DX = PRSW_DSIPM(2)/2.0                                        856
         AG_DY = PRSW_DSIPM(1)/2.0                                        856
         AG_DZ = PRSW_DSIPM(4)/2.0                                        856
         AG_TITLE = 'BOX'                                                 856
         AG_EXNAME = 'SHAPE'                                              856
         AG_PARLIST = 'DX_DY_DZ'                                          856
         CALL AXSHAPE                                                     856
C    *                                                                    856
5721  CALL AGSPOPB                                                        857
      GO TO BLOCK_FFEE, (5720 ,5730,5790)                                 857
C End of Local Procedure                                                  857
C ----------------------------------------------------------------------  858
C ------------ WPPL                                                       858
C Begin Local Procedure WPPL                                              859
C WPPL IS A POLE FOR WEST PLATFORM                                        859
5050  CONTINUE                                                            859
         AG_TITLE='WPPL'                                                  859
         AG_MODULE=MODULE                                                 859
         AG_MARK='V00'                                                    859
         CALL AGSPUSH                                                     859
         AG_MODULE=MODULE                                                 859
C    *                                                                    859
C    Geant MATERIAL : IRON ===> inherited arguments used <===             860
         CALL AGSRESET                                                    860
         AG_TITLE = 'IRON'                                                860
         AG_EXNAME = 'MATERIAL'                                           860
         AG_PARLIST = 'NONE'                                              860
         CALL AXMATERIAL                                                  860
C    *                                                                    860
C    Geant ATTRIBUTE : WPPL ===> SEEN=1 COLO=4 <===                       860
         CALL AGSRESET                                                    861
         AG_SEEN = 1                                                      861
         AG_COLO = 4                                                      861
         AG_TITLE = 'WPPL'                                                861
         AG_EXNAME = 'ATTRIBUTE'                                          861
         AG_PARLIST = 'SEEN_COLO'                                         861
         CALL AXATTRIBUTE                                                 861
C    *                                                                    861
C    Geant SHAPE : BOX ===> DX=WPFM_POLED(1)/2.0 DY=WPFM_POLED(2)/2.0, D  861
C    Z=WPFM_POLED(3)/2.0 <===                                             861
         CALL AGSRESET                                                    863
         AG_DX = WPFM_POLED(1)/2.0                                        863
         AG_DY = WPFM_POLED(2)/2.0                                        863
         AG_DZ = WPFM_POLED(3)/2.0                                        863
         AG_TITLE = 'BOX'                                                 863
         AG_EXNAME = 'SHAPE'                                              863
         AG_PARLIST = 'DX_DY_DZ'                                          863
         CALL AXSHAPE                                                     863
C    *                                                                    863
C    Geant POSITION : WPAR ===> X=0.0 Y=0.0 Z=0.0 DZ=WPFM_POLED(3)/2.0-W  863
C    PFM_POLED(4) DX=WPFM_POLED(1)/2.0-WPFM_POLED(4) DY=WPFM_POLED(2)/2.  863
C    0 <===                                                               863
         CALL AGSRESET                                                    865
         AG_X = 0.0                                                       865
         AG_Y = 0.0                                                       865
         AG_Z = 0.0                                                       865
         AG_DZ = WPFM_POLED(3)/2.0-WPFM_POLED(4)                          865
         AG_DX = WPFM_POLED(1)/2.0-WPFM_POLED(4)                          865
         AG_DY = WPFM_POLED(2)/2.0                                        865
         AG_TITLE = 'WPAR'                                                865
         AG_EXNAME = 'POSITION'                                           865
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    865
         CALL AXPOSITION                                                  865
C    *                                                                    865
5051  CALL AGSPOPB                                                        866
      GO TO BLOCK_WPPL, (5050 ,5060)                                      866
C End of Local Procedure                                                  866
C ----------------------------------------------------------------------  867
C ------------ WPIS                                                       867
C Begin Local Procedure WPIS                                              868
C WPIS IS A I-STRUCT FOR WEST PLATFORM                                    868
5070  CONTINUE                                                            868
         AG_TITLE='WPIS'                                                  868
         AG_MODULE=MODULE                                                 868
         AG_MARK='V00'                                                    868
         CALL AGSPUSH                                                     868
         AG_MODULE=MODULE                                                 868
C    *                                                                    868
C    Geant MATERIAL : IRON ===> inherited arguments used <===             869
         CALL AGSRESET                                                    869
         AG_TITLE = 'IRON'                                                869
         AG_EXNAME = 'MATERIAL'                                           869
         AG_PARLIST = 'NONE'                                              869
         CALL AXMATERIAL                                                  869
C    *                                                                    869
C    Geant ATTRIBUTE : WPIS ===> SEEN=1 COLO=4 <===                       869
         CALL AGSRESET                                                    870
         AG_SEEN = 1                                                      870
         AG_COLO = 4                                                      870
         AG_TITLE = 'WPIS'                                                870
         AG_EXNAME = 'ATTRIBUTE'                                          870
         AG_PARLIST = 'SEEN_COLO'                                         870
         CALL AXATTRIBUTE                                                 870
C    *                                                                    870
C    Geant SHAPE : BOX ===> DX=WPFM_ISTRD(1)/2.0 DY=WPFM_ISTRD(2)/2.0, D  870
C    Z=WPFM_ISTRD(3)/2.0 <===                                             870
         CALL AGSRESET                                                    872
         AG_DX = WPFM_ISTRD(1)/2.0                                        872
         AG_DY = WPFM_ISTRD(2)/2.0                                        872
         AG_DZ = WPFM_ISTRD(3)/2.0                                        872
         AG_TITLE = 'BOX'                                                 872
         AG_EXNAME = 'SHAPE'                                              872
         AG_PARLIST = 'DX_DY_DZ'                                          872
         CALL AXSHAPE                                                     872
C    *                                                                    872
C    Geant POSITION : WPAR ===> X=0.0 Y=0.0 Z=+(WPFM_ISTRD(3)/4.0-WPFM_I  872
C    STRD(4)/2.0) DZ=(WPFM_ISTRD(3)/2.0-WPFM_ISTRD(4)/2.0)/2.0 DX=WPFM_I  872
C    STRD(1)/2.0 DY=WPFM_ISTRD(2)/2.0-WPFM_ISTRD(4) <===                  872
         CALL AGSRESET                                                    875
         AG_X = 0.0                                                       875
         AG_Y = 0.0                                                       875
         AG_Z = +(WPFM_ISTRD(3)/4.0-WPFM_ISTRD(4)/2.0)                    875
         AG_DZ = (WPFM_ISTRD(3)/2.0-WPFM_ISTRD(4)/2.0)/2.0                875
         AG_DX = WPFM_ISTRD(1)/2.0                                        875
         AG_DY = WPFM_ISTRD(2)/2.0-WPFM_ISTRD(4)                          875
         AG_TITLE = 'WPAR'                                                875
         AG_EXNAME = 'POSITION'                                           875
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    875
         CALL AXPOSITION                                                  875
C    *                                                                    875
C    Geant POSITION : WPAR ===> X=0.0 Y=0.0 Z=-(WPFM_ISTRD(3)/4.0-WPFM_I  875
C    STRD(4)/2.0) DZ=(WPFM_ISTRD(3)/2.0-WPFM_ISTRD(4)/2.0)/2.0 DX=WPFM_I  875
C    STRD(1)/2.0 DY=WPFM_ISTRD(2)/2.0-WPFM_ISTRD(4) <===                  875
         CALL AGSRESET                                                    878
         AG_X = 0.0                                                       878
         AG_Y = 0.0                                                       878
         AG_Z = -(WPFM_ISTRD(3)/4.0-WPFM_ISTRD(4)/2.0)                    878
         AG_DZ = (WPFM_ISTRD(3)/2.0-WPFM_ISTRD(4)/2.0)/2.0                878
         AG_DX = WPFM_ISTRD(1)/2.0                                        878
         AG_DY = WPFM_ISTRD(2)/2.0-WPFM_ISTRD(4)                          878
         AG_TITLE = 'WPAR'                                                878
         AG_EXNAME = 'POSITION'                                           878
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    878
         CALL AXPOSITION                                                  878
C    *                                                                    878
5071  CALL AGSPOPB                                                        879
      GO TO BLOCK_WPIS, (5070 ,5080)                                      879
C End of Local Procedure                                                  879
C ----------------------------------------------------------------------  880
C ------------ WPUF                                                       880
C Begin Local Procedure WPUF                                              881
C WPUF IS A FRONT U-STRUCT FOR WEST PLATFORM                              881
5090  CONTINUE                                                            881
         AG_TITLE='WPUF'                                                  881
         AG_MODULE=MODULE                                                 881
         AG_MARK='V00'                                                    881
         CALL AGSPUSH                                                     881
         AG_MODULE=MODULE                                                 881
C    *                                                                    881
C    Geant MATERIAL : IRON ===> inherited arguments used <===             882
         CALL AGSRESET                                                    882
         AG_TITLE = 'IRON'                                                882
         AG_EXNAME = 'MATERIAL'                                           882
         AG_PARLIST = 'NONE'                                              882
         CALL AXMATERIAL                                                  882
C    *                                                                    882
C    Geant ATTRIBUTE : WPUF ===> SEEN=1 COLO=4 <===                       882
         CALL AGSRESET                                                    883
         AG_SEEN = 1                                                      883
         AG_COLO = 4                                                      883
         AG_TITLE = 'WPUF'                                                883
         AG_EXNAME = 'ATTRIBUTE'                                          883
         AG_PARLIST = 'SEEN_COLO'                                         883
         CALL AXATTRIBUTE                                                 883
C    *                                                                    883
C    Geant SHAPE : BOX ===> DX=WPFM_USTFD(1)/2.0 DY=WPFM_USTFD(2)/2.0, D  883
C    Z=WPFM_USTFD(3)/2.0 <===                                             883
         CALL AGSRESET                                                    885
         AG_DX = WPFM_USTFD(1)/2.0                                        885
         AG_DY = WPFM_USTFD(2)/2.0                                        885
         AG_DZ = WPFM_USTFD(3)/2.0                                        885
         AG_TITLE = 'BOX'                                                 885
         AG_EXNAME = 'SHAPE'                                              885
         AG_PARLIST = 'DX_DY_DZ'                                          885
         CALL AXSHAPE                                                     885
C    *                                                                    885
C    Geant POSITION : WPAR ===> X=0.0 Y=0.0 Z=WPFM_USTFD(4)/2.0 DZ=WPFM_  885
C    USTFD(3)/2.0-WPFM_USTFD(4)/2.0 DX=WPFM_USTFD(1)/2.0 DY=WPFM_USTFD(2  885
C    )/2.0-WPFM_USTFD(4) <===                                             885
         CALL AGSRESET                                                    888
         AG_X = 0.0                                                       888
         AG_Y = 0.0                                                       888
         AG_Z = WPFM_USTFD(4)/2.0                                         888
         AG_DZ = WPFM_USTFD(3)/2.0-WPFM_USTFD(4)/2.0                      888
         AG_DX = WPFM_USTFD(1)/2.0                                        888
         AG_DY = WPFM_USTFD(2)/2.0-WPFM_USTFD(4)                          888
         AG_TITLE = 'WPAR'                                                888
         AG_EXNAME = 'POSITION'                                           888
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    888
         CALL AXPOSITION                                                  888
C    *                                                                    888
5091  CALL AGSPOPB                                                        889
      GO TO BLOCK_WPUF, (5090 ,5100)                                      889
C End of Local Procedure                                                  889
C ----------------------------------------------------------------------  890
C ------------ WPUN                                                       890
C Begin Local Procedure WPUN                                              891
C WPUN IS A NORTH U-STRUCT FOR WEST PLATFORM                              891
5110  CONTINUE                                                            891
         AG_TITLE='WPUN'                                                  891
         AG_MODULE=MODULE                                                 891
         AG_MARK='V00'                                                    891
         CALL AGSPUSH                                                     891
         AG_MODULE=MODULE                                                 891
C    *                                                                    891
C    Geant MATERIAL : IRON ===> inherited arguments used <===             892
         CALL AGSRESET                                                    892
         AG_TITLE = 'IRON'                                                892
         AG_EXNAME = 'MATERIAL'                                           892
         AG_PARLIST = 'NONE'                                              892
         CALL AXMATERIAL                                                  892
C    *                                                                    892
C    Geant ATTRIBUTE : WPUN ===> SEEN=1 COLO=4 <===                       892
         CALL AGSRESET                                                    893
         AG_SEEN = 1                                                      893
         AG_COLO = 4                                                      893
         AG_TITLE = 'WPUN'                                                893
         AG_EXNAME = 'ATTRIBUTE'                                          893
         AG_PARLIST = 'SEEN_COLO'                                         893
         CALL AXATTRIBUTE                                                 893
C    *                                                                    893
C    Geant SHAPE : BOX ===> DX=WPFM_USTND(1)/2.0 DY=WPFM_USTND(2)/2.0, D  893
C    Z=WPFM_USTND(3)/2.0 <===                                             893
         CALL AGSRESET                                                    895
         AG_DX = WPFM_USTND(1)/2.0                                        895
         AG_DY = WPFM_USTND(2)/2.0                                        895
         AG_DZ = WPFM_USTND(3)/2.0                                        895
         AG_TITLE = 'BOX'                                                 895
         AG_EXNAME = 'SHAPE'                                              895
         AG_PARLIST = 'DX_DY_DZ'                                          895
         CALL AXSHAPE                                                     895
C    *                                                                    895
C    Geant POSITION : WPAR ===> X=+WPFM_USTND(4)/2.0 Y=0.0 Z=0.0 DZ=WPFM  895
C    _USTND(3)/2.0 DX=WPFM_USTND(1)/2.0-WPFM_USTND(4)/2.0 DY=WPFM_USTND(  895
C    2)/2.0-WPFM_USTND(4) <===                                            895
         CALL AGSRESET                                                    897
         AG_X = +WPFM_USTND(4)/2.0                                        897
         AG_Y = 0.0                                                       897
         AG_Z = 0.0                                                       897
         AG_DZ = WPFM_USTND(3)/2.0                                        897
         AG_DX = WPFM_USTND(1)/2.0-WPFM_USTND(4)/2.0                      897
         AG_DY = WPFM_USTND(2)/2.0-WPFM_USTND(4)                          897
         AG_TITLE = 'WPAR'                                                897
         AG_EXNAME = 'POSITION'                                           897
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    897
         CALL AXPOSITION                                                  897
C    *                                                                    897
5111  CALL AGSPOPB                                                        898
      GO TO BLOCK_WPUN, (5110 ,5120)                                      898
C End of Local Procedure                                                  898
C ----------------------------------------------------------------------  899
C ------------ WPUS                                                       899
C Begin Local Procedure WPUS                                              900
C WPUS IS A SOUTH U-STRUCT FOR WEST PLATFORM                              900
5130  CONTINUE                                                            900
         AG_TITLE='WPUS'                                                  900
         AG_MODULE=MODULE                                                 900
         AG_MARK='V00'                                                    900
         CALL AGSPUSH                                                     900
         AG_MODULE=MODULE                                                 900
C    *                                                                    900
C    Geant MATERIAL : IRON ===> inherited arguments used <===             901
         CALL AGSRESET                                                    901
         AG_TITLE = 'IRON'                                                901
         AG_EXNAME = 'MATERIAL'                                           901
         AG_PARLIST = 'NONE'                                              901
         CALL AXMATERIAL                                                  901
C    *                                                                    901
C    Geant ATTRIBUTE : WPUS ===> SEEN=1 COLO=4 <===                       901
         CALL AGSRESET                                                    902
         AG_SEEN = 1                                                      902
         AG_COLO = 4                                                      902
         AG_TITLE = 'WPUS'                                                902
         AG_EXNAME = 'ATTRIBUTE'                                          902
         AG_PARLIST = 'SEEN_COLO'                                         902
         CALL AXATTRIBUTE                                                 902
C    *                                                                    902
C    Geant SHAPE : BOX ===> DX=WPFM_USTSD(1)/2.0 DY=WPFM_USTSD(2)/2.0, D  902
C    Z=WPFM_USTSD(3)/2.0 <===                                             902
         CALL AGSRESET                                                    904
         AG_DX = WPFM_USTSD(1)/2.0                                        904
         AG_DY = WPFM_USTSD(2)/2.0                                        904
         AG_DZ = WPFM_USTSD(3)/2.0                                        904
         AG_TITLE = 'BOX'                                                 904
         AG_EXNAME = 'SHAPE'                                              904
         AG_PARLIST = 'DX_DY_DZ'                                          904
         CALL AXSHAPE                                                     904
C    *                                                                    904
C    Geant POSITION : WPAR ===> X=-WPFM_USTSD(4)/2.0 Y=0.0 Z=0.0 DZ=WPFM  904
C    _USTSD(3)/2.0 DX=WPFM_USTSD(1)/2.0-WPFM_USTSD(4)/2.0 DY=WPFM_USTSD(  904
C    2)/2.0-WPFM_USTSD(4) <===                                            904
         CALL AGSRESET                                                    906
         AG_X = -WPFM_USTSD(4)/2.0                                        906
         AG_Y = 0.0                                                       906
         AG_Z = 0.0                                                       906
         AG_DZ = WPFM_USTSD(3)/2.0                                        906
         AG_DX = WPFM_USTSD(1)/2.0-WPFM_USTSD(4)/2.0                      906
         AG_DY = WPFM_USTSD(2)/2.0-WPFM_USTSD(4)                          906
         AG_TITLE = 'WPAR'                                                906
         AG_EXNAME = 'POSITION'                                           906
         AG_PARLIST = 'X_Y_Z_DZ_DX_DY'                                    906
         CALL AXPOSITION                                                  906
C    *                                                                    906
5131  CALL AGSPOPB                                                        907
      GO TO BLOCK_WPUS, (5130 ,5140)                                      907
C End of Local Procedure                                                  907
C ----------------------------------------------------------------------  908
C ------------ WPAR                                                       908
C Begin Local Procedure WPAR                                              909
C WPAR IS A PLATFORM STRUCTS AIR                                          909
5500  CONTINUE                                                            909
         AG_TITLE='WPAR'                                                  909
         AG_MODULE=MODULE                                                 909
         AG_MARK='V00'                                                    909
         CALL AGSPUSH                                                     909
         AG_MODULE=MODULE                                                 909
C    *                                                                    909
C    Geant MATERIAL : AIR ===> inherited arguments used <===              910
         CALL AGSRESET                                                    910
         AG_TITLE = 'AIR'                                                 910
         AG_EXNAME = 'MATERIAL'                                           910
         AG_PARLIST = 'NONE'                                              910
         CALL AXMATERIAL                                                  910
C    *                                                                    910
C    Geant ATTRIBUTE : WPAR ===> SEEN=1 COLO=0 <===                       910
         CALL AGSRESET                                                    911
         AG_SEEN = 1                                                      911
         AG_COLO = 0                                                      911
         AG_TITLE = 'WPAR'                                                911
         AG_EXNAME = 'ATTRIBUTE'                                          911
         AG_PARLIST = 'SEEN_COLO'                                         911
         CALL AXATTRIBUTE                                                 911
C    *                                                                    911
C    Geant SHAPE : BOX ===> DX=0.0 DY=0.0 DZ=0.0 <===                     911
         CALL AGSRESET                                                    912
         AG_DX = 0.0                                                      912
         AG_DY = 0.0                                                      912
         AG_DZ = 0.0                                                      912
         AG_TITLE = 'BOX'                                                 912
         AG_EXNAME = 'SHAPE'                                              912
         AG_PARLIST = 'DX_DY_DZ'                                          912
         CALL AXSHAPE                                                     912
C    *                                                                    912
5501  CALL AGSPOPB                                                        913
      GO TO BLOCK_WPAR, (5500 ,5510)                                      913
C End of Local Procedure                                                  913
      WRITE(LOUT,5800)'===>>  [End AgML Module HcalGeo1]  <<==='          913
5800  FORMAT(A32,/,/)                                                     913
C MODULE HCALGEO1                                                         915
      END                                                                 915
