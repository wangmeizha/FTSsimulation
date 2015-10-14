C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE HCALGEO                                                  1
C HCALGEO FORWARD HADRONIC CALORIMETER, TEST RUN, 2014                    1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'HCALGEO'/, BANK_TITLE/'*'/                             1
      DATA MCOMMENT/'FORWARD HADRONIC CALORIMETER, TEST RUN, 2014'/       1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C AUTHOR PRASHANTH S (KSU)                                                1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 AUTHOR /'PRASHANTH S (KSU)'/                           2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C CREATED 12/30/2013                                                      2
      CHARACTER*40 CREATED /'12/30/2013'/                                 3
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
      INTEGER BLOCK_HCMO                                                  19
      INTEGER BLOCK_HTWR                                                  19
      INTEGER BLOCK_HCEL                                                  19
      INTEGER BLOCK_HCES                                                  19
      INTEGER BLOCK_HCLX                                                  19
      INTEGER BLOCK_HCLY                                                  19
      INTEGER BLOCK_HCSX                                                  19
      INTEGER BLOCK_HCSY                                                  19
      INTEGER BLOCK_HFIB                                                  19
      INTEGER BLOCK_HFIC                                                  19
      INTEGER BLOCK_HSLD                                                  19
      INTEGER BLOCK_HSWR                                                  19
      INTEGER BLOCK_HSTP                                                  19
      INTEGER BLOCK_HSFE                                                  19
      INTEGER *4 I,J , M,N , P,Q                                          20
      REAL *4 XTEMP, YTEMP                                                22
C -                                                                       24
      REAL MCDT_VERSION                                                   24
      EQUIVALENCE (BANK_MCDT(3),MCDT_VERSION,RE_MCDT_VERSION),            24
     *(NAMES_MCDT(1),NMCDT_VERSION)                                       24
      EQUIVALENCE (COMMEN_MCDT(1),CMCDT_VERSION),(MAP_MCDT(1,1),          24
     *MMCDT_VERSION)                                                      24
      CHARACTER CMCDT_VERSION*40/' '/                                     24
      CHARACTER NMCDT_VERSION*12/'R.VERSION'/                             24
      INTEGER MMCDT_VERSION(2)/1,1/                                       24
      REAL RE_MCDT_VERSION                                                24
C -                                                                       24
      REAL MCDT_X                                                         24
      EQUIVALENCE (BANK_MCDT(4),MCDT_X,RE_MCDT_X),(NAMES_MCDT(2),         24
     *NMCDT_X)                                                            24
      EQUIVALENCE (COMMEN_MCDT(2),CMCDT_X),(MAP_MCDT(1,2),MMCDT_X)        24
      CHARACTER CMCDT_X*40/' '/                                           24
      CHARACTER NMCDT_X*12/'R.X'/                                         24
      INTEGER MMCDT_X(2)/1,1/                                             24
      REAL RE_MCDT_X                                                      24
C -                                                                       24
      REAL MCDT_Y                                                         24
      EQUIVALENCE (BANK_MCDT(5),MCDT_Y,RE_MCDT_Y),(NAMES_MCDT(3),         24
     *NMCDT_Y)                                                            24
      EQUIVALENCE (COMMEN_MCDT(3),CMCDT_Y),(MAP_MCDT(1,3),MMCDT_Y)        24
      CHARACTER CMCDT_Y*40/' '/                                           24
      CHARACTER NMCDT_Y*12/'R.Y'/                                         24
      INTEGER MMCDT_Y(2)/1,1/                                             24
      REAL RE_MCDT_Y                                                      24
C -                                                                       24
      REAL MCDT_Z                                                         24
      EQUIVALENCE (BANK_MCDT(6),MCDT_Z,RE_MCDT_Z),(NAMES_MCDT(4),         24
     *NMCDT_Z)                                                            24
      EQUIVALENCE (COMMEN_MCDT(4),CMCDT_Z),(MAP_MCDT(1,4),MMCDT_Z)        24
      CHARACTER CMCDT_Z*40/' '/                                           24
      CHARACTER NMCDT_Z*12/'R.Z'/                                         24
      INTEGER MMCDT_Z(2)/1,1/                                             24
      REAL RE_MCDT_Z                                                      24
C -                                                                       24
      REAL MCDT_DX                                                        24
      EQUIVALENCE (BANK_MCDT(7),MCDT_DX,RE_MCDT_DX),(NAMES_MCDT(5),       24
     *NMCDT_DX)                                                           24
      EQUIVALENCE (COMMEN_MCDT(5),CMCDT_DX),(MAP_MCDT(1,5),MMCDT_DX)      24
      CHARACTER CMCDT_DX*40/' '/                                          24
      CHARACTER NMCDT_DX*12/'R.DX'/                                       24
      INTEGER MMCDT_DX(2)/1,1/                                            24
      REAL RE_MCDT_DX                                                     24
C -                                                                       24
      REAL MCDT_DY                                                        24
      EQUIVALENCE (BANK_MCDT(8),MCDT_DY,RE_MCDT_DY),(NAMES_MCDT(6),       24
     *NMCDT_DY)                                                           24
      EQUIVALENCE (COMMEN_MCDT(6),CMCDT_DY),(MAP_MCDT(1,6),MMCDT_DY)      24
      CHARACTER CMCDT_DY*40/' '/                                          24
      CHARACTER NMCDT_DY*12/'R.DY'/                                       24
      INTEGER MMCDT_DY(2)/1,1/                                            24
      REAL RE_MCDT_DY                                                     24
C -                                                                       24
      REAL MCDT_DZ                                                        24
      EQUIVALENCE (BANK_MCDT(9),MCDT_DZ,RE_MCDT_DZ),(NAMES_MCDT(7),       24
     *NMCDT_DZ)                                                           24
      EQUIVALENCE (COMMEN_MCDT(7),CMCDT_DZ),(MAP_MCDT(1,7),MMCDT_DZ)      24
      CHARACTER CMCDT_DZ*40/' '/                                          24
      CHARACTER NMCDT_DZ*12/'R.DZ'/                                       24
      INTEGER MMCDT_DZ(2)/1,1/                                            24
      REAL RE_MCDT_DZ                                                     24
C -                                                                       24
      INTEGER LEN_MCDT,LLEN_MCDT,LINK_MCDT                                24
      PARAMETER (LEN_MCDT=9)                                              24
      PARAMETER (LLEN_MCDT=7)                                             24
      INTEGER FLAG_MCDT/0/,MAP_MCDT(2,LLEN_MCDT),IDNUM_MCDT(10)           24
      CHARACTER NAMES_MCDT(LLEN_MCDT)*12,COMMEN_MCDT(LLEN_MCDT)*40        24
      DATA IDNUM_MCDT/10*0/                                               24
      REAL BANK_MCDT(LEN_MCDT)                                            24
      CHARACTER*60 BPATH_MCDT/' '/                                        24
      SAVE FLAG_MCDT,MAP_MCDT,IDNUM_MCDT,NAMES_MCDT,COMMEN_MCDT,          24
     *BPATH_MCDT                                                          24
      DATA LINK_MCDT/0/,BANK_MCDT/LEN_MCDT*0/                             24
C *                                                                       24
C -                                                                       25
      REAL TOWR_VERSION                                                   25
      EQUIVALENCE (BANK_TOWR(3),TOWR_VERSION,RE_TOWR_VERSION),            25
     *(NAMES_TOWR(1),NTOWR_VERSION)                                       25
      EQUIVALENCE (COMMEN_TOWR(1),CTOWR_VERSION),(MAP_TOWR(1,1),          25
     *MTOWR_VERSION)                                                      25
      CHARACTER CTOWR_VERSION*40/' '/                                     25
      CHARACTER NTOWR_VERSION*12/'R.VERSION'/                             25
      INTEGER MTOWR_VERSION(2)/1,1/                                       25
      REAL RE_TOWR_VERSION                                                25
C -                                                                       25
      REAL TOWR_DX                                                        25
      EQUIVALENCE (BANK_TOWR(4),TOWR_DX,RE_TOWR_DX),(NAMES_TOWR(2),       25
     *NTOWR_DX)                                                           25
      EQUIVALENCE (COMMEN_TOWR(2),CTOWR_DX),(MAP_TOWR(1,2),MTOWR_DX)      25
      CHARACTER CTOWR_DX*40/' '/                                          25
      CHARACTER NTOWR_DX*12/'R.DX'/                                       25
      INTEGER MTOWR_DX(2)/1,1/                                            25
      REAL RE_TOWR_DX                                                     25
C -                                                                       25
      REAL TOWR_DY                                                        25
      EQUIVALENCE (BANK_TOWR(5),TOWR_DY,RE_TOWR_DY),(NAMES_TOWR(3),       25
     *NTOWR_DY)                                                           25
      EQUIVALENCE (COMMEN_TOWR(3),CTOWR_DY),(MAP_TOWR(1,3),MTOWR_DY)      25
      CHARACTER CTOWR_DY*40/' '/                                          25
      CHARACTER NTOWR_DY*12/'R.DY'/                                       25
      INTEGER MTOWR_DY(2)/1,1/                                            25
      REAL RE_TOWR_DY                                                     25
C -                                                                       25
      REAL TOWR_DZ                                                        25
      EQUIVALENCE (BANK_TOWR(6),TOWR_DZ,RE_TOWR_DZ),(NAMES_TOWR(4),       25
     *NTOWR_DZ)                                                           25
      EQUIVALENCE (COMMEN_TOWR(4),CTOWR_DZ),(MAP_TOWR(1,4),MTOWR_DZ)      25
      CHARACTER CTOWR_DZ*40/' '/                                          25
      CHARACTER NTOWR_DZ*12/'R.DZ'/                                       25
      INTEGER MTOWR_DZ(2)/1,1/                                            25
      REAL RE_TOWR_DZ                                                     25
C -                                                                       25
      REAL TOWR_NTOWERX                                                   25
      EQUIVALENCE (BANK_TOWR(7),TOWR_NTOWERX,RE_TOWR_NTOWERX),            25
     *(NAMES_TOWR(5),NTOWR_NTOWERX)                                       25
      EQUIVALENCE (COMMEN_TOWR(5),CTOWR_NTOWERX),(MAP_TOWR(1,5),          25
     *MTOWR_NTOWERX)                                                      25
      CHARACTER CTOWR_NTOWERX*40/' '/                                     25
      CHARACTER NTOWR_NTOWERX*12/'R.NTOWERX'/                             25
      INTEGER MTOWR_NTOWERX(2)/1,1/                                       25
      REAL RE_TOWR_NTOWERX                                                25
C -                                                                       25
      REAL TOWR_NTOWERY                                                   25
      EQUIVALENCE (BANK_TOWR(8),TOWR_NTOWERY,RE_TOWR_NTOWERY),            25
     *(NAMES_TOWR(6),NTOWR_NTOWERY)                                       25
      EQUIVALENCE (COMMEN_TOWR(6),CTOWR_NTOWERY),(MAP_TOWR(1,6),          25
     *MTOWR_NTOWERY)                                                      25
      CHARACTER CTOWR_NTOWERY*40/' '/                                     25
      CHARACTER NTOWR_NTOWERY*12/'R.NTOWERY'/                             25
      INTEGER MTOWR_NTOWERY(2)/1,1/                                       25
      REAL RE_TOWR_NTOWERY                                                25
C -                                                                       25
      INTEGER LEN_TOWR,LLEN_TOWR,LINK_TOWR                                25
      PARAMETER (LEN_TOWR=8)                                              25
      PARAMETER (LLEN_TOWR=6)                                             25
      INTEGER FLAG_TOWR/0/,MAP_TOWR(2,LLEN_TOWR),IDNUM_TOWR(10)           25
      CHARACTER NAMES_TOWR(LLEN_TOWR)*12,COMMEN_TOWR(LLEN_TOWR)*40        25
      DATA IDNUM_TOWR/10*0/                                               25
      REAL BANK_TOWR(LEN_TOWR)                                            25
      CHARACTER*60 BPATH_TOWR/' '/                                        25
      SAVE FLAG_TOWR,MAP_TOWR,IDNUM_TOWR,NAMES_TOWR,COMMEN_TOWR,          25
     *BPATH_TOWR                                                          25
      DATA LINK_TOWR/0/,BANK_TOWR/LEN_TOWR*0/                             25
C *                                                                       25
C -                                                                       27
      REAL CELL_VERSION                                                   27
      EQUIVALENCE (BANK_CELL(3),CELL_VERSION,RE_CELL_VERSION),            27
     *(NAMES_CELL(1),NCELL_VERSION)                                       27
      EQUIVALENCE (COMMEN_CELL(1),CCELL_VERSION),(MAP_CELL(1,1),          27
     *MCELL_VERSION)                                                      27
      CHARACTER CCELL_VERSION*40/' '/                                     27
      CHARACTER NCELL_VERSION*12/'R.VERSION'/                             27
      INTEGER MCELL_VERSION(2)/1,1/                                       27
      REAL RE_CELL_VERSION                                                27
C -                                                                       27
      REAL CELL_DX                                                        27
      EQUIVALENCE (BANK_CELL(4),CELL_DX,RE_CELL_DX),(NAMES_CELL(2),       27
     *NCELL_DX)                                                           27
      EQUIVALENCE (COMMEN_CELL(2),CCELL_DX),(MAP_CELL(1,2),MCELL_DX)      27
      CHARACTER CCELL_DX*40/' '/                                          27
      CHARACTER NCELL_DX*12/'R.DX'/                                       27
      INTEGER MCELL_DX(2)/1,1/                                            27
      REAL RE_CELL_DX                                                     27
C -                                                                       27
      REAL CELL_DY                                                        27
      EQUIVALENCE (BANK_CELL(5),CELL_DY,RE_CELL_DY),(NAMES_CELL(3),       27
     *NCELL_DY)                                                           27
      EQUIVALENCE (COMMEN_CELL(3),CCELL_DY),(MAP_CELL(1,3),MCELL_DY)      27
      CHARACTER CCELL_DY*40/' '/                                          27
      CHARACTER NCELL_DY*12/'R.DY'/                                       27
      INTEGER MCELL_DY(2)/1,1/                                            27
      REAL RE_CELL_DY                                                     27
C -                                                                       27
      REAL CELL_DZ                                                        27
      EQUIVALENCE (BANK_CELL(6),CELL_DZ,RE_CELL_DZ),(NAMES_CELL(4),       27
     *NCELL_DZ)                                                           27
      EQUIVALENCE (COMMEN_CELL(4),CCELL_DZ),(MAP_CELL(1,4),MCELL_DZ)      27
      CHARACTER CCELL_DZ*40/' '/                                          27
      CHARACTER NCELL_DZ*12/'R.DZ'/                                       27
      INTEGER MCELL_DZ(2)/1,1/                                            27
      REAL RE_CELL_DZ                                                     27
C -                                                                       27
      REAL CELL_NCELLX                                                    27
      EQUIVALENCE (BANK_CELL(7),CELL_NCELLX,RE_CELL_NCELLX),              27
     *(NAMES_CELL(5),NCELL_NCELLX)                                        27
      EQUIVALENCE (COMMEN_CELL(5),CCELL_NCELLX),(MAP_CELL(1,5),           27
     *MCELL_NCELLX)                                                       27
      CHARACTER CCELL_NCELLX*40/' '/                                      27
      CHARACTER NCELL_NCELLX*12/'R.NCELLX'/                               27
      INTEGER MCELL_NCELLX(2)/1,1/                                        27
      REAL RE_CELL_NCELLX                                                 27
C -                                                                       27
      REAL CELL_NCELLY                                                    27
      EQUIVALENCE (BANK_CELL(8),CELL_NCELLY,RE_CELL_NCELLY),              27
     *(NAMES_CELL(6),NCELL_NCELLY)                                        27
      EQUIVALENCE (COMMEN_CELL(6),CCELL_NCELLY),(MAP_CELL(1,6),           27
     *MCELL_NCELLY)                                                       27
      CHARACTER CCELL_NCELLY*40/' '/                                      27
      CHARACTER NCELL_NCELLY*12/'R.NCELLY'/                               27
      INTEGER MCELL_NCELLY(2)/1,1/                                        27
      REAL RE_CELL_NCELLY                                                 27
C -                                                                       27
      REAL CELL_NFIBERXL                                                  27
      EQUIVALENCE (BANK_CELL(9),CELL_NFIBERXL,RE_CELL_NFIBERXL),          27
     *(NAMES_CELL(7),NCELL_NFIBERXL)                                      27
      EQUIVALENCE (COMMEN_CELL(7),CCELL_NFIBERXL),(MAP_CELL(1,7),         27
     *MCELL_NFIBERXL)                                                     27
      CHARACTER CCELL_NFIBERXL*40/' '/                                    27
      CHARACTER NCELL_NFIBERXL*12/'R.NFIBERXL'/                           27
      INTEGER MCELL_NFIBERXL(2)/1,1/                                      27
      REAL RE_CELL_NFIBERXL                                               27
C -                                                                       27
      REAL CELL_NFIBERYL                                                  27
      EQUIVALENCE (BANK_CELL(10),CELL_NFIBERYL,RE_CELL_NFIBERYL),         27
     *(NAMES_CELL(8),NCELL_NFIBERYL)                                      27
      EQUIVALENCE (COMMEN_CELL(8),CCELL_NFIBERYL),(MAP_CELL(1,8),         27
     *MCELL_NFIBERYL)                                                     27
      CHARACTER CCELL_NFIBERYL*40/' '/                                    27
      CHARACTER NCELL_NFIBERYL*12/'R.NFIBERYL'/                           27
      INTEGER MCELL_NFIBERYL(2)/1,1/                                      27
      REAL RE_CELL_NFIBERYL                                               27
C -                                                                       27
      REAL CELL_NFIBERXS                                                  27
      EQUIVALENCE (BANK_CELL(11),CELL_NFIBERXS,RE_CELL_NFIBERXS),         27
     *(NAMES_CELL(9),NCELL_NFIBERXS)                                      27
      EQUIVALENCE (COMMEN_CELL(9),CCELL_NFIBERXS),(MAP_CELL(1,9),         27
     *MCELL_NFIBERXS)                                                     27
      CHARACTER CCELL_NFIBERXS*40/' '/                                    27
      CHARACTER NCELL_NFIBERXS*12/'R.NFIBERXS'/                           27
      INTEGER MCELL_NFIBERXS(2)/1,1/                                      27
      REAL RE_CELL_NFIBERXS                                               27
C -                                                                       27
      REAL CELL_NFIBERYS                                                  27
      EQUIVALENCE (BANK_CELL(12),CELL_NFIBERYS,RE_CELL_NFIBERYS),         27
     *(NAMES_CELL(10),NCELL_NFIBERYS)                                     27
      EQUIVALENCE (COMMEN_CELL(10),CCELL_NFIBERYS),(MAP_CELL(1,10),       27
     *MCELL_NFIBERYS)                                                     27
      CHARACTER CCELL_NFIBERYS*40/' '/                                    27
      CHARACTER NCELL_NFIBERYS*12/'R.NFIBERYS'/                           27
      INTEGER MCELL_NFIBERYS(2)/1,1/                                      27
      REAL RE_CELL_NFIBERYS                                               27
C -                                                                       27
      INTEGER LEN_CELL,LLEN_CELL,LINK_CELL                                27
      PARAMETER (LEN_CELL=12)                                             27
      PARAMETER (LLEN_CELL=10)                                            27
      INTEGER FLAG_CELL/0/,MAP_CELL(2,LLEN_CELL),IDNUM_CELL(10)           27
      CHARACTER NAMES_CELL(LLEN_CELL)*12,COMMEN_CELL(LLEN_CELL)*40        27
      DATA IDNUM_CELL/10*0/                                               27
      REAL BANK_CELL(LEN_CELL)                                            27
      CHARACTER*60 BPATH_CELL/' '/                                        27
      SAVE FLAG_CELL,MAP_CELL,IDNUM_CELL,NAMES_CELL,COMMEN_CELL,          27
     *BPATH_CELL                                                          27
      DATA LINK_CELL/0/,BANK_CELL/LEN_CELL*0/                             27
C *                                                                       27
C -                                                                       29
      REAL FIBR_VERSION                                                   29
      EQUIVALENCE (BANK_FIBR(3),FIBR_VERSION,RE_FIBR_VERSION),            29
     *(NAMES_FIBR(1),NFIBR_VERSION)                                       29
      EQUIVALENCE (COMMEN_FIBR(1),CFIBR_VERSION),(MAP_FIBR(1,1),          29
     *MFIBR_VERSION)                                                      29
      CHARACTER CFIBR_VERSION*40/' '/                                     29
      CHARACTER NFIBR_VERSION*12/'R.VERSION'/                             29
      INTEGER MFIBR_VERSION(2)/1,1/                                       29
      REAL RE_FIBR_VERSION                                                29
C -                                                                       29
      REAL FIBR_RMIN                                                      29
      EQUIVALENCE (BANK_FIBR(4),FIBR_RMIN,RE_FIBR_RMIN),(NAMES_FIBR(2),   29
     *NFIBR_RMIN)                                                         29
      EQUIVALENCE (COMMEN_FIBR(2),CFIBR_RMIN),(MAP_FIBR(1,2),             29
     *MFIBR_RMIN)                                                         29
      CHARACTER CFIBR_RMIN*40/' '/                                        29
      CHARACTER NFIBR_RMIN*12/'R.RMIN'/                                   29
      INTEGER MFIBR_RMIN(2)/1,1/                                          29
      REAL RE_FIBR_RMIN                                                   29
C -                                                                       29
      REAL FIBR_RMAX                                                      29
      EQUIVALENCE (BANK_FIBR(5),FIBR_RMAX,RE_FIBR_RMAX),(NAMES_FIBR(3),   29
     *NFIBR_RMAX)                                                         29
      EQUIVALENCE (COMMEN_FIBR(3),CFIBR_RMAX),(MAP_FIBR(1,3),             29
     *MFIBR_RMAX)                                                         29
      CHARACTER CFIBR_RMAX*40/' '/                                        29
      CHARACTER NFIBR_RMAX*12/'R.RMAX'/                                   29
      INTEGER MFIBR_RMAX(2)/1,1/                                          29
      REAL RE_FIBR_RMAX                                                   29
C -                                                                       29
      REAL FIBR_DZ                                                        29
      EQUIVALENCE (BANK_FIBR(6),FIBR_DZ,RE_FIBR_DZ),(NAMES_FIBR(4),       29
     *NFIBR_DZ)                                                           29
      EQUIVALENCE (COMMEN_FIBR(4),CFIBR_DZ),(MAP_FIBR(1,4),MFIBR_DZ)      29
      CHARACTER CFIBR_DZ*40/' '/                                          29
      CHARACTER NFIBR_DZ*12/'R.DZ'/                                       29
      INTEGER MFIBR_DZ(2)/1,1/                                            29
      REAL RE_FIBR_DZ                                                     29
C -                                                                       29
      REAL FIBR_STARTX1                                                   29
      EQUIVALENCE (BANK_FIBR(7),FIBR_STARTX1,RE_FIBR_STARTX1),            29
     *(NAMES_FIBR(5),NFIBR_STARTX1)                                       29
      EQUIVALENCE (COMMEN_FIBR(5),CFIBR_STARTX1),(MAP_FIBR(1,5),          29
     *MFIBR_STARTX1)                                                      29
      CHARACTER CFIBR_STARTX1*40/' '/                                     29
      CHARACTER NFIBR_STARTX1*12/'R.STARTX1'/                             29
      INTEGER MFIBR_STARTX1(2)/1,1/                                       29
      REAL RE_FIBR_STARTX1                                                29
C -                                                                       29
      REAL FIBR_STARTY1                                                   29
      EQUIVALENCE (BANK_FIBR(8),FIBR_STARTY1,RE_FIBR_STARTY1),            29
     *(NAMES_FIBR(6),NFIBR_STARTY1)                                       29
      EQUIVALENCE (COMMEN_FIBR(6),CFIBR_STARTY1),(MAP_FIBR(1,6),          29
     *MFIBR_STARTY1)                                                      29
      CHARACTER CFIBR_STARTY1*40/' '/                                     29
      CHARACTER NFIBR_STARTY1*12/'R.STARTY1'/                             29
      INTEGER MFIBR_STARTY1(2)/1,1/                                       29
      REAL RE_FIBR_STARTY1                                                29
C -                                                                       29
      REAL FIBR_STARTX2                                                   29
      EQUIVALENCE (BANK_FIBR(9),FIBR_STARTX2,RE_FIBR_STARTX2),            29
     *(NAMES_FIBR(7),NFIBR_STARTX2)                                       29
      EQUIVALENCE (COMMEN_FIBR(7),CFIBR_STARTX2),(MAP_FIBR(1,7),          29
     *MFIBR_STARTX2)                                                      29
      CHARACTER CFIBR_STARTX2*40/' '/                                     29
      CHARACTER NFIBR_STARTX2*12/'R.STARTX2'/                             29
      INTEGER MFIBR_STARTX2(2)/1,1/                                       29
      REAL RE_FIBR_STARTX2                                                29
C -                                                                       29
      REAL FIBR_STARTY2                                                   29
      EQUIVALENCE (BANK_FIBR(10),FIBR_STARTY2,RE_FIBR_STARTY2),           29
     *(NAMES_FIBR(8),NFIBR_STARTY2)                                       29
      EQUIVALENCE (COMMEN_FIBR(8),CFIBR_STARTY2),(MAP_FIBR(1,8),          29
     *MFIBR_STARTY2)                                                      29
      CHARACTER CFIBR_STARTY2*40/' '/                                     29
      CHARACTER NFIBR_STARTY2*12/'R.STARTY2'/                             29
      INTEGER MFIBR_STARTY2(2)/1,1/                                       29
      REAL RE_FIBR_STARTY2                                                29
C -                                                                       29
      REAL FIBR_STEPX                                                     29
      EQUIVALENCE (BANK_FIBR(11),FIBR_STEPX,RE_FIBR_STEPX),               29
     *(NAMES_FIBR(9),NFIBR_STEPX)                                         29
      EQUIVALENCE (COMMEN_FIBR(9),CFIBR_STEPX),(MAP_FIBR(1,9),            29
     *MFIBR_STEPX)                                                        29
      CHARACTER CFIBR_STEPX*40/' '/                                       29
      CHARACTER NFIBR_STEPX*12/'R.STEPX'/                                 29
      INTEGER MFIBR_STEPX(2)/1,1/                                         29
      REAL RE_FIBR_STEPX                                                  29
C -                                                                       29
      REAL FIBR_STEPY                                                     29
      EQUIVALENCE (BANK_FIBR(12),FIBR_STEPY,RE_FIBR_STEPY),               29
     *(NAMES_FIBR(10),NFIBR_STEPY)                                        29
      EQUIVALENCE (COMMEN_FIBR(10),CFIBR_STEPY),(MAP_FIBR(1,10),          29
     *MFIBR_STEPY)                                                        29
      CHARACTER CFIBR_STEPY*40/' '/                                       29
      CHARACTER NFIBR_STEPY*12/'R.STEPY'/                                 29
      INTEGER MFIBR_STEPY(2)/1,1/                                         29
      REAL RE_FIBR_STEPY                                                  29
C -                                                                       29
      INTEGER LEN_FIBR,LLEN_FIBR,LINK_FIBR                                29
      PARAMETER (LEN_FIBR=12)                                             29
      PARAMETER (LLEN_FIBR=10)                                            29
      INTEGER FLAG_FIBR/0/,MAP_FIBR(2,LLEN_FIBR),IDNUM_FIBR(10)           29
      CHARACTER NAMES_FIBR(LLEN_FIBR)*12,COMMEN_FIBR(LLEN_FIBR)*40        29
      DATA IDNUM_FIBR/10*0/                                               29
      REAL BANK_FIBR(LEN_FIBR)                                            29
      CHARACTER*60 BPATH_FIBR/' '/                                        29
      SAVE FLAG_FIBR,MAP_FIBR,IDNUM_FIBR,NAMES_FIBR,COMMEN_FIBR,          29
     *BPATH_FIBR                                                          29
      DATA LINK_FIBR/0/,BANK_FIBR/LEN_FIBR*0/                             29
C *                                                                       29
C -                                                                       30
      REAL SHLD_VERSION                                                   30
      EQUIVALENCE (BANK_SHLD(3),SHLD_VERSION,RE_SHLD_VERSION),            30
     *(NAMES_SHLD(1),NSHLD_VERSION)                                       30
      EQUIVALENCE (COMMEN_SHLD(1),CSHLD_VERSION),(MAP_SHLD(1,1),          30
     *MSHLD_VERSION)                                                      30
      CHARACTER CSHLD_VERSION*40/' '/                                     30
      CHARACTER NSHLD_VERSION*12/'R.VERSION'/                             30
      INTEGER MSHLD_VERSION(2)/1,1/                                       30
      REAL RE_SHLD_VERSION                                                30
C -                                                                       30
      REAL SHLD_XD                                                        30
      EQUIVALENCE (BANK_SHLD(4),SHLD_XD,RE_SHLD_XD),(NAMES_SHLD(2),       30
     *NSHLD_XD)                                                           30
      EQUIVALENCE (COMMEN_SHLD(2),CSHLD_XD),(MAP_SHLD(1,2),MSHLD_XD)      30
      CHARACTER CSHLD_XD*40/' '/                                          30
      CHARACTER NSHLD_XD*12/'R.XD'/                                       30
      INTEGER MSHLD_XD(2)/1,1/                                            30
      REAL RE_SHLD_XD                                                     30
C -                                                                       30
      REAL SHLD_YD                                                        30
      EQUIVALENCE (BANK_SHLD(5),SHLD_YD,RE_SHLD_YD),(NAMES_SHLD(3),       30
     *NSHLD_YD)                                                           30
      EQUIVALENCE (COMMEN_SHLD(3),CSHLD_YD),(MAP_SHLD(1,3),MSHLD_YD)      30
      CHARACTER CSHLD_YD*40/' '/                                          30
      CHARACTER NSHLD_YD*12/'R.YD'/                                       30
      INTEGER MSHLD_YD(2)/1,1/                                            30
      REAL RE_SHLD_YD                                                     30
C -                                                                       30
      REAL SHLD_ZD                                                        30
      EQUIVALENCE (BANK_SHLD(6),SHLD_ZD,RE_SHLD_ZD),(NAMES_SHLD(4),       30
     *NSHLD_ZD)                                                           30
      EQUIVALENCE (COMMEN_SHLD(4),CSHLD_ZD),(MAP_SHLD(1,4),MSHLD_ZD)      30
      CHARACTER CSHLD_ZD*40/' '/                                          30
      CHARACTER NSHLD_ZD*12/'R.ZD'/                                       30
      INTEGER MSHLD_ZD(2)/1,1/                                            30
      REAL RE_SHLD_ZD                                                     30
C -                                                                       30
      REAL SHLD_DXD                                                       30
      EQUIVALENCE (BANK_SHLD(7),SHLD_DXD,RE_SHLD_DXD),(NAMES_SHLD(5),     30
     *NSHLD_DXD)                                                          30
      EQUIVALENCE (COMMEN_SHLD(5),CSHLD_DXD),(MAP_SHLD(1,5),MSHLD_DXD)    30
      CHARACTER CSHLD_DXD*40/' '/                                         30
      CHARACTER NSHLD_DXD*12/'R.DXD'/                                     30
      INTEGER MSHLD_DXD(2)/1,1/                                           30
      REAL RE_SHLD_DXD                                                    30
C -                                                                       30
      REAL SHLD_DYD                                                       30
      EQUIVALENCE (BANK_SHLD(8),SHLD_DYD,RE_SHLD_DYD),(NAMES_SHLD(6),     30
     *NSHLD_DYD)                                                          30
      EQUIVALENCE (COMMEN_SHLD(6),CSHLD_DYD),(MAP_SHLD(1,6),MSHLD_DYD)    30
      CHARACTER CSHLD_DYD*40/' '/                                         30
      CHARACTER NSHLD_DYD*12/'R.DYD'/                                     30
      INTEGER MSHLD_DYD(2)/1,1/                                           30
      REAL RE_SHLD_DYD                                                    30
C -                                                                       30
      REAL SHLD_DZD                                                       30
      EQUIVALENCE (BANK_SHLD(9),SHLD_DZD,RE_SHLD_DZD),(NAMES_SHLD(7),     30
     *NSHLD_DZD)                                                          30
      EQUIVALENCE (COMMEN_SHLD(7),CSHLD_DZD),(MAP_SHLD(1,7),MSHLD_DZD)    30
      CHARACTER CSHLD_DZD*40/' '/                                         30
      CHARACTER NSHLD_DZD*12/'R.DZD'/                                     30
      INTEGER MSHLD_DZD(2)/1,1/                                           30
      REAL RE_SHLD_DZD                                                    30
C -                                                                       30
      INTEGER LEN_SHLD,LLEN_SHLD,LINK_SHLD                                30
      PARAMETER (LEN_SHLD=9)                                              30
      PARAMETER (LLEN_SHLD=7)                                             30
      INTEGER FLAG_SHLD/0/,MAP_SHLD(2,LLEN_SHLD),IDNUM_SHLD(10)           30
      CHARACTER NAMES_SHLD(LLEN_SHLD)*12,COMMEN_SHLD(LLEN_SHLD)*40        30
      DATA IDNUM_SHLD/10*0/                                               30
      REAL BANK_SHLD(LEN_SHLD)                                            30
      CHARACTER*60 BPATH_SHLD/' '/                                        30
      SAVE FLAG_SHLD,MAP_SHLD,IDNUM_SHLD,NAMES_SHLD,COMMEN_SHLD,          30
     *BPATH_SHLD                                                          30
      DATA LINK_SHLD/0/,BANK_SHLD/LEN_SHLD*0/                             30
C *                                                                       30
C -                                                                       32
      REAL PRES_VERSION                                                   32
      EQUIVALENCE (BANK_PRES(3),PRES_VERSION,RE_PRES_VERSION),            32
     *(NAMES_PRES(1),NPRES_VERSION)                                       32
      EQUIVALENCE (COMMEN_PRES(1),CPRES_VERSION),(MAP_PRES(1,1),          32
     *MPRES_VERSION)                                                      32
      CHARACTER CPRES_VERSION*40/' '/                                     32
      CHARACTER NPRES_VERSION*12/'R.VERSION'/                             32
      INTEGER MPRES_VERSION(2)/1,1/                                       32
      REAL RE_PRES_VERSION                                                32
C -                                                                       32
      REAL PRES_XP1                                                       32
      EQUIVALENCE (BANK_PRES(4),PRES_XP1,RE_PRES_XP1),(NAMES_PRES(2),     32
     *NPRES_XP1)                                                          32
      EQUIVALENCE (COMMEN_PRES(2),CPRES_XP1),(MAP_PRES(1,2),MPRES_XP1)    32
      CHARACTER CPRES_XP1*40/' '/                                         32
      CHARACTER NPRES_XP1*12/'R.XP1'/                                     32
      INTEGER MPRES_XP1(2)/1,1/                                           32
      REAL RE_PRES_XP1                                                    32
C -                                                                       32
      REAL PRES_YP1                                                       32
      EQUIVALENCE (BANK_PRES(5),PRES_YP1,RE_PRES_YP1),(NAMES_PRES(3),     32
     *NPRES_YP1)                                                          32
      EQUIVALENCE (COMMEN_PRES(3),CPRES_YP1),(MAP_PRES(1,3),MPRES_YP1)    32
      CHARACTER CPRES_YP1*40/' '/                                         32
      CHARACTER NPRES_YP1*12/'R.YP1'/                                     32
      INTEGER MPRES_YP1(2)/1,1/                                           32
      REAL RE_PRES_YP1                                                    32
C -                                                                       32
      REAL PRES_ZP1                                                       32
      EQUIVALENCE (BANK_PRES(6),PRES_ZP1,RE_PRES_ZP1),(NAMES_PRES(4),     32
     *NPRES_ZP1)                                                          32
      EQUIVALENCE (COMMEN_PRES(4),CPRES_ZP1),(MAP_PRES(1,4),MPRES_ZP1)    32
      CHARACTER CPRES_ZP1*40/' '/                                         32
      CHARACTER NPRES_ZP1*12/'R.ZP1'/                                     32
      INTEGER MPRES_ZP1(2)/1,1/                                           32
      REAL RE_PRES_ZP1                                                    32
C -                                                                       32
      REAL PRES_RP1                                                       32
      EQUIVALENCE (BANK_PRES(7),PRES_RP1,RE_PRES_RP1),(NAMES_PRES(5),     32
     *NPRES_RP1)                                                          32
      EQUIVALENCE (COMMEN_PRES(5),CPRES_RP1),(MAP_PRES(1,5),MPRES_RP1)    32
      CHARACTER CPRES_RP1*40/' '/                                         32
      CHARACTER NPRES_RP1*12/'R.RP1'/                                     32
      INTEGER MPRES_RP1(2)/1,1/                                           32
      REAL RE_PRES_RP1                                                    32
C -                                                                       32
      REAL PRES_XP2                                                       32
      EQUIVALENCE (BANK_PRES(8),PRES_XP2,RE_PRES_XP2),(NAMES_PRES(6),     32
     *NPRES_XP2)                                                          32
      EQUIVALENCE (COMMEN_PRES(6),CPRES_XP2),(MAP_PRES(1,6),MPRES_XP2)    32
      CHARACTER CPRES_XP2*40/' '/                                         32
      CHARACTER NPRES_XP2*12/'R.XP2'/                                     32
      INTEGER MPRES_XP2(2)/1,1/                                           32
      REAL RE_PRES_XP2                                                    32
C -                                                                       32
      REAL PRES_YP2                                                       32
      EQUIVALENCE (BANK_PRES(9),PRES_YP2,RE_PRES_YP2),(NAMES_PRES(7),     32
     *NPRES_YP2)                                                          32
      EQUIVALENCE (COMMEN_PRES(7),CPRES_YP2),(MAP_PRES(1,7),MPRES_YP2)    32
      CHARACTER CPRES_YP2*40/' '/                                         32
      CHARACTER NPRES_YP2*12/'R.YP2'/                                     32
      INTEGER MPRES_YP2(2)/1,1/                                           32
      REAL RE_PRES_YP2                                                    32
C -                                                                       32
      REAL PRES_ZP2                                                       32
      EQUIVALENCE (BANK_PRES(10),PRES_ZP2,RE_PRES_ZP2),(NAMES_PRES(8),    32
     *NPRES_ZP2)                                                          32
      EQUIVALENCE (COMMEN_PRES(8),CPRES_ZP2),(MAP_PRES(1,8),MPRES_ZP2)    32
      CHARACTER CPRES_ZP2*40/' '/                                         32
      CHARACTER NPRES_ZP2*12/'R.ZP2'/                                     32
      INTEGER MPRES_ZP2(2)/1,1/                                           32
      REAL RE_PRES_ZP2                                                    32
C -                                                                       32
      REAL PRES_RP2                                                       32
      EQUIVALENCE (BANK_PRES(11),PRES_RP2,RE_PRES_RP2),(NAMES_PRES(9),    32
     *NPRES_RP2)                                                          32
      EQUIVALENCE (COMMEN_PRES(9),CPRES_RP2),(MAP_PRES(1,9),MPRES_RP2)    32
      CHARACTER CPRES_RP2*40/' '/                                         32
      CHARACTER NPRES_RP2*12/'R.RP2'/                                     32
      INTEGER MPRES_RP2(2)/1,1/                                           32
      REAL RE_PRES_RP2                                                    32
C -                                                                       32
      REAL PRES_XFE                                                       32
      EQUIVALENCE (BANK_PRES(12),PRES_XFE,RE_PRES_XFE),(NAMES_PRES(10),   32
     *NPRES_XFE)                                                          32
      EQUIVALENCE (COMMEN_PRES(10),CPRES_XFE),(MAP_PRES(1,10),            32
     *MPRES_XFE)                                                          32
      CHARACTER CPRES_XFE*40/' '/                                         32
      CHARACTER NPRES_XFE*12/'R.XFE'/                                     32
      INTEGER MPRES_XFE(2)/1,1/                                           32
      REAL RE_PRES_XFE                                                    32
C -                                                                       32
      REAL PRES_YFE                                                       32
      EQUIVALENCE (BANK_PRES(13),PRES_YFE,RE_PRES_YFE),(NAMES_PRES(11),   32
     *NPRES_YFE)                                                          32
      EQUIVALENCE (COMMEN_PRES(11),CPRES_YFE),(MAP_PRES(1,11),            32
     *MPRES_YFE)                                                          32
      CHARACTER CPRES_YFE*40/' '/                                         32
      CHARACTER NPRES_YFE*12/'R.YFE'/                                     32
      INTEGER MPRES_YFE(2)/1,1/                                           32
      REAL RE_PRES_YFE                                                    32
C -                                                                       32
      REAL PRES_ZFE                                                       32
      EQUIVALENCE (BANK_PRES(14),PRES_ZFE,RE_PRES_ZFE),(NAMES_PRES(12),   32
     *NPRES_ZFE)                                                          32
      EQUIVALENCE (COMMEN_PRES(12),CPRES_ZFE),(MAP_PRES(1,12),            32
     *MPRES_ZFE)                                                          32
      CHARACTER CPRES_ZFE*40/' '/                                         32
      CHARACTER NPRES_ZFE*12/'R.ZFE'/                                     32
      INTEGER MPRES_ZFE(2)/1,1/                                           32
      REAL RE_PRES_ZFE                                                    32
C -                                                                       32
      REAL PRES_RFE                                                       32
      EQUIVALENCE (BANK_PRES(15),PRES_RFE,RE_PRES_RFE),(NAMES_PRES(13),   32
     *NPRES_RFE)                                                          32
      EQUIVALENCE (COMMEN_PRES(13),CPRES_RFE),(MAP_PRES(1,13),            32
     *MPRES_RFE)                                                          32
      CHARACTER CPRES_RFE*40/' '/                                         32
      CHARACTER NPRES_RFE*12/'R.RFE'/                                     32
      INTEGER MPRES_RFE(2)/1,1/                                           32
      REAL RE_PRES_RFE                                                    32
C -                                                                       32
      REAL PRES_STRPXD                                                    32
      EQUIVALENCE (BANK_PRES(16),PRES_STRPXD,RE_PRES_STRPXD),             32
     *(NAMES_PRES(14),NPRES_STRPXD)                                       32
      EQUIVALENCE (COMMEN_PRES(14),CPRES_STRPXD),(MAP_PRES(1,14),         32
     *MPRES_STRPXD)                                                       32
      CHARACTER CPRES_STRPXD*40/' '/                                      32
      CHARACTER NPRES_STRPXD*12/'R.STRPXD'/                               32
      INTEGER MPRES_STRPXD(2)/1,1/                                        32
      REAL RE_PRES_STRPXD                                                 32
C -                                                                       32
      REAL PRES_STRPYD                                                    32
      EQUIVALENCE (BANK_PRES(17),PRES_STRPYD,RE_PRES_STRPYD),             32
     *(NAMES_PRES(15),NPRES_STRPYD)                                       32
      EQUIVALENCE (COMMEN_PRES(15),CPRES_STRPYD),(MAP_PRES(1,15),         32
     *MPRES_STRPYD)                                                       32
      CHARACTER CPRES_STRPYD*40/' '/                                      32
      CHARACTER NPRES_STRPYD*12/'R.STRPYD'/                               32
      INTEGER MPRES_STRPYD(2)/1,1/                                        32
      REAL RE_PRES_STRPYD                                                 32
C -                                                                       32
      REAL PRES_STRPZD                                                    32
      EQUIVALENCE (BANK_PRES(18),PRES_STRPZD,RE_PRES_STRPZD),             32
     *(NAMES_PRES(16),NPRES_STRPZD)                                       32
      EQUIVALENCE (COMMEN_PRES(16),CPRES_STRPZD),(MAP_PRES(1,16),         32
     *MPRES_STRPZD)                                                       32
      CHARACTER CPRES_STRPZD*40/' '/                                      32
      CHARACTER NPRES_STRPZD*12/'R.STRPZD'/                               32
      INTEGER MPRES_STRPZD(2)/1,1/                                        32
      REAL RE_PRES_STRPZD                                                 32
C -                                                                       32
      REAL PRES_FEXD                                                      32
      EQUIVALENCE (BANK_PRES(19),PRES_FEXD,RE_PRES_FEXD),                 32
     *(NAMES_PRES(17),NPRES_FEXD)                                         32
      EQUIVALENCE (COMMEN_PRES(17),CPRES_FEXD),(MAP_PRES(1,17),           32
     *MPRES_FEXD)                                                         32
      CHARACTER CPRES_FEXD*40/' '/                                        32
      CHARACTER NPRES_FEXD*12/'R.FEXD'/                                   32
      INTEGER MPRES_FEXD(2)/1,1/                                          32
      REAL RE_PRES_FEXD                                                   32
C -                                                                       32
      REAL PRES_FEYD                                                      32
      EQUIVALENCE (BANK_PRES(20),PRES_FEYD,RE_PRES_FEYD),                 32
     *(NAMES_PRES(18),NPRES_FEYD)                                         32
      EQUIVALENCE (COMMEN_PRES(18),CPRES_FEYD),(MAP_PRES(1,18),           32
     *MPRES_FEYD)                                                         32
      CHARACTER CPRES_FEYD*40/' '/                                        32
      CHARACTER NPRES_FEYD*12/'R.FEYD'/                                   32
      INTEGER MPRES_FEYD(2)/1,1/                                          32
      REAL RE_PRES_FEYD                                                   32
C -                                                                       32
      REAL PRES_FEZD                                                      32
      EQUIVALENCE (BANK_PRES(21),PRES_FEZD,RE_PRES_FEZD),                 32
     *(NAMES_PRES(19),NPRES_FEZD)                                         32
      EQUIVALENCE (COMMEN_PRES(19),CPRES_FEZD),(MAP_PRES(1,19),           32
     *MPRES_FEZD)                                                         32
      CHARACTER CPRES_FEZD*40/' '/                                        32
      CHARACTER NPRES_FEZD*12/'R.FEZD'/                                   32
      INTEGER MPRES_FEZD(2)/1,1/                                          32
      REAL RE_PRES_FEZD                                                   32
C -                                                                       32
      REAL PRES_STRIPS                                                    32
      EQUIVALENCE (BANK_PRES(22),PRES_STRIPS,RE_PRES_STRIPS),             32
     *(NAMES_PRES(20),NPRES_STRIPS)                                       32
      EQUIVALENCE (COMMEN_PRES(20),CPRES_STRIPS),(MAP_PRES(1,20),         32
     *MPRES_STRIPS)                                                       32
      CHARACTER CPRES_STRIPS*40/' '/                                      32
      CHARACTER NPRES_STRIPS*12/'R.STRIPS'/                               32
      INTEGER MPRES_STRIPS(2)/1,1/                                        32
      REAL RE_PRES_STRIPS                                                 32
C -                                                                       32
      INTEGER LEN_PRES,LLEN_PRES,LINK_PRES                                32
      PARAMETER (LEN_PRES=22)                                             32
      PARAMETER (LLEN_PRES=20)                                            32
      INTEGER FLAG_PRES/0/,MAP_PRES(2,LLEN_PRES),IDNUM_PRES(10)           32
      CHARACTER NAMES_PRES(LLEN_PRES)*12,COMMEN_PRES(LLEN_PRES)*40        32
      DATA IDNUM_PRES/10*0/                                               32
      REAL BANK_PRES(LEN_PRES)                                            32
      CHARACTER*60 BPATH_PRES/' '/                                        32
      SAVE FLAG_PRES,MAP_PRES,IDNUM_PRES,NAMES_PRES,COMMEN_PRES,          32
     *BPATH_PRES                                                          32
      DATA LINK_PRES/0/,BANK_PRES/LEN_PRES*0/                             32
C *                                                                       32
C *                                                                       33
      CALL AGSBEGM(MODULE,IPRIN)                                          33
      IF (IPRIN.LT.0) RETURN                                              33
C Filling bank MCDT - MOTHER CALORIMETER                                  33
      BANK_TITLE='MOTHER CALORIMETER'                                     33
      BPATH_MCDT='MCDT*'                                                  33
      IDNUM_MCDT(1)=0                                                     34
C FACE CENTER OF CALORIMETER                                              34
      MCDT_VERSION=1.0                                                    34
      CMCDT_VERSION='FACE CENTER OF CALORIMETER'                          34
C X CDT FROM CENTER OF TPC                                                35
      MCDT_X=-69.8                                                        35
      CMCDT_X='X CDT FROM CENTER OF TPC'                                  35
C Y CDT FROM CENTER OF TPC                                                36
      MCDT_Y=4.9                                                          36
      CMCDT_Y='Y CDT FROM CENTER OF TPC'                                  36
C Z CDT FROM CENTER OF TPC                                                37
      MCDT_Z=777.4                                                        37
      CMCDT_Z='Z CDT FROM CENTER OF TPC'                                  37
C HCMO WIDTH                                                              38
      MCDT_DX=60/2.                                                       38
      CMCDT_DX='HCMO WIDTH'                                               38
C HCMO HEIGHT + SHILD HIGHT                                               39
      MCDT_DY=(60+5)/2.                                                   39
      CMCDT_DY='HCMO HEIGHT + SHILD HIGHT'                                39
C HCMO LENGTH                                                             40
      MCDT_DZ=117/2.                                                      40
      CMCDT_DZ='HCMO LENGTH'                                              40
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MCDT',    41
     *BPATH_MCDT,IDNUM_MCDT,1, MAP_MCDT,NAMES_MCDT,COMMEN_MCDT,           41
     *BANK_MCDT,LEN_MCDT,LLEN_MCDT,LINK_MCDT,FLAG_MCDT)                   41
C *                                                                       41
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MCDT',    41
     *BPATH_MCDT,IDNUM_MCDT,1, MAP_MCDT,NAMES_MCDT,COMMEN_MCDT,           41
     *BANK_MCDT,LEN_MCDT,LLEN_MCDT,LINK_MCDT,FLAG_MCDT)                   41
C *                                                                       42
      CALL AGSBEGM(MODULE,IPRIN)                                          42
      IF (IPRIN.LT.0) RETURN                                              42
C Filling bank TOWR - STRUCTURE CONTAINING CALORIMETER TOWR DIMENTION     42
      BANK_TITLE='STRUCTURE CONTAINING CALORIMETER TOWR DIMENTION'        42
      BPATH_TOWR='TOWR*'                                                  42
      IDNUM_TOWR(1)=0                                                     43
C TOWR DIMENTION                                                          43
      TOWR_VERSION=1.0                                                    43
      CTOWR_VERSION='TOWR DIMENTION'                                      43
C TOWR WIDTH                                                              44
      TOWR_DX=10.0/2.                                                     44
      CTOWR_DX='TOWR WIDTH'                                               44
C TOWR HEIGHT                                                             45
      TOWR_DY=10.0/2.                                                     45
      CTOWR_DY='TOWR HEIGHT'                                              45
C TOWR LENGTH                                                             46
      TOWR_DZ=117.0/2.                                                    46
      CTOWR_DZ='TOWR LENGTH'                                              46
C NO OF TOWER IN X DIR                                                    47
      TOWR_NTOWERX=6                                                      47
      CTOWR_NTOWERX='NO OF TOWER IN X DIR'                                47
C NO OF TOWER IN Y DIR                                                    48
      TOWR_NTOWERY=6                                                      48
      CTOWR_NTOWERY='NO OF TOWER IN Y DIR'                                48
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TOWR',    49
     *BPATH_TOWR,IDNUM_TOWR,1, MAP_TOWR,NAMES_TOWR,COMMEN_TOWR,           49
     *BANK_TOWR,LEN_TOWR,LLEN_TOWR,LINK_TOWR,FLAG_TOWR)                   49
C *                                                                       49
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TOWR',    49
     *BPATH_TOWR,IDNUM_TOWR,1, MAP_TOWR,NAMES_TOWR,COMMEN_TOWR,           49
     *BANK_TOWR,LEN_TOWR,LLEN_TOWR,LINK_TOWR,FLAG_TOWR)                   49
C *                                                                       50
      CALL AGSBEGM(MODULE,IPRIN)                                          50
      IF (IPRIN.LT.0) RETURN                                              50
C Filling bank CELL - STRUCTURE CONTAINING CALORIMETER PIXELS DIMENTION   50
      BANK_TITLE='STRUCTURE CONTAINING CALORIMETER PIXELS DIMENTION'      50
      BPATH_CELL='CELL*'                                                  50
      IDNUM_CELL(1)=0                                                     51
C PIXELS DIMENTION                                                        51
      CELL_VERSION=1.0                                                    51
      CCELL_VERSION='PIXELS DIMENTION'                                    51
C PIXELS WIDTH                                                            52
      CELL_DX=3.3/2.                                                      52
      CCELL_DX='PIXELS WIDTH'                                             52
C PIXELS HEIGHT                                                           53
      CELL_DY=3.3/2.                                                      53
      CCELL_DY='PIXELS HEIGHT'                                            53
C TOWR LENGTH                                                             54
      CELL_DZ=117.0/2.                                                    54
      CCELL_DZ='TOWR LENGTH'                                              54
C NO OF CELLS IN X DIR                                                    55
      CELL_NCELLX=3                                                       55
      CCELL_NCELLX='NO OF CELLS IN X DIR'                                 55
C NO OF CELLS IN Y DIR                                                    56
      CELL_NCELLY=3                                                       56
      CCELL_NCELLY='NO OF CELLS IN Y DIR'                                 56
C NO FIBERS IN CELL 16 FIBERS                                             57
      CELL_NFIBERXL=16                                                    57
      CCELL_NFIBERXL='NO FIBERS IN CELL 16 FIBERS'                        57
C NO FIBERS IN CELL 16 FIBERS                                             58
      CELL_NFIBERYL=16                                                    58
      CCELL_NFIBERYL='NO FIBERS IN CELL 16 FIBERS'                        58
C NO FIBERS IN CELL 15 FIBERS                                             59
      CELL_NFIBERXS=15                                                    59
      CCELL_NFIBERXS='NO FIBERS IN CELL 15 FIBERS'                        59
C NO FIBERS IN CELL 15 FIBERS                                             60
      CELL_NFIBERYS=15                                                    60
      CCELL_NFIBERYS='NO FIBERS IN CELL 15 FIBERS'                        60
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CELL',    61
     *BPATH_CELL,IDNUM_CELL,1, MAP_CELL,NAMES_CELL,COMMEN_CELL,           61
     *BANK_CELL,LEN_CELL,LLEN_CELL,LINK_CELL,FLAG_CELL)                   61
C *                                                                       61
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CELL',    61
     *BPATH_CELL,IDNUM_CELL,1, MAP_CELL,NAMES_CELL,COMMEN_CELL,           61
     *BANK_CELL,LEN_CELL,LLEN_CELL,LINK_CELL,FLAG_CELL)                   61
C *                                                                       62
      CALL AGSBEGM(MODULE,IPRIN)                                          62
      IF (IPRIN.LT.0) RETURN                                              62
C Filling bank FIBR - SCINTILATING FIBES IN TOWERS/CELLS                  62
      BANK_TITLE='SCINTILATING FIBES IN TOWERS/CELLS'                     62
      BPATH_FIBR='FIBR*'                                                  62
      IDNUM_FIBR(1)=0                                                     63
C FIBER DIMENTION                                                         63
      FIBR_VERSION=1.0                                                    63
      CFIBR_VERSION='FIBER DIMENTION'                                     63
C INNER RADIUS OF FIBER                                                   64
      FIBR_RMIN=0.0                                                       64
      CFIBR_RMIN='INNER RADIUS OF FIBER'                                  64
C OUTER RADIUS OF FIBER                                                   65
      FIBR_RMAX=0.055                                                     65
      CFIBR_RMAX='OUTER RADIUS OF FIBER'                                  65
C TOWR LENGTH                                                             66
      FIBR_DZ=117.0/2.                                                    66
      CFIBR_DZ='TOWR LENGTH'                                              66
C DISTANCE FROM EDGE OR FROM ONE CELL BOUNDRY TO OTHER 1: FOR FIRST CELL  67
      FIBR_STARTX1=0.107                                                  67
      CFIBR_STARTX1='DISTANCE FROM EDGE OR FROM ONE CELL BOUNDRY TO OTHE  67
     *R 1: FOR FIRST CELL'                                                67
C DISTANCE FROM EDGE OR FROM ONE CELL BOUNDRY TO OTHER                    68
      FIBR_STARTY1=0.107                                                  68
      CFIBR_STARTY1='DISTANCE FROM EDGE OR FROM ONE CELL BOUNDRY TO OTHE  68
     *R'                                                                  68
C DISTANCE FROM EDGE OR FROM ONE CELL BOUNDRY TO OTHER 2:FOR 2ND AND 3RD  69
C  CELL                                                                   69
      FIBR_STARTX2=0.1065                                                 69
      CFIBR_STARTX2='DISTANCE FROM EDGE OR FROM ONE CELL BOUNDRY TO OTHE  69
     *R 2:FOR 2ND AND 3RD CELL'                                           69
C DISTANCE FROM EDGE OR FROM ONE CELL BOUNDRY TO OTHER                    70
      FIBR_STARTY2=0.1065                                                 70
      CFIBR_STARTY2='DISTANCE FROM EDGE OR FROM ONE CELL BOUNDRY TO OTHE  70
     *R'                                                                  70
C DISTANCE BETWEEN FIBERS CENTER TO CENTER                                71
      FIBR_STEPX=0.213                                                    71
      CFIBR_STEPX='DISTANCE BETWEEN FIBERS CENTER TO CENTER'              71
C DISTANCE BETWEEN FIBERS CENTER TO CENTER                                72
      FIBR_STEPY=0.213                                                    72
      CFIBR_STEPY='DISTANCE BETWEEN FIBERS CENTER TO CENTER'              72
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FIBR',    73
     *BPATH_FIBR,IDNUM_FIBR,1, MAP_FIBR,NAMES_FIBR,COMMEN_FIBR,           73
     *BANK_FIBR,LEN_FIBR,LLEN_FIBR,LINK_FIBR,FLAG_FIBR)                   73
C *                                                                       73
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FIBR',    73
     *BPATH_FIBR,IDNUM_FIBR,1, MAP_FIBR,NAMES_FIBR,COMMEN_FIBR,           73
     *BANK_FIBR,LEN_FIBR,LLEN_FIBR,LINK_FIBR,FLAG_FIBR)                   73
C *                                                                       74
      CALL AGSBEGM(MODULE,IPRIN)                                          74
      IF (IPRIN.LT.0) RETURN                                              74
C Filling bank SHLD - SHILD AROUND CALORIMETER                            74
      BANK_TITLE='SHILD AROUND CALORIMETER'                               74
      BPATH_SHLD='SHLD*'                                                  74
      IDNUM_SHLD(1)=0                                                     75
C NONE                                                                    75
      SHLD_VERSION=1.0                                                    75
      CSHLD_VERSION='NONE'                                                75
C NONE                                                                    76
      SHLD_XD=0.0                                                         76
      CSHLD_XD='NONE'                                                     76
C NONE                                                                    77
      SHLD_YD=-32.5                                                       77
      CSHLD_YD='NONE'                                                     77
C NONE                                                                    78
      SHLD_ZD=0.0                                                         78
      CSHLD_ZD='NONE'                                                     78
C NONE                                                                    79
      SHLD_DXD=35.0                                                       79
      CSHLD_DXD='NONE'                                                    79
C NONE                                                                    80
      SHLD_DYD=5.0/2                                                      80
      CSHLD_DYD='NONE'                                                    80
C NONE                                                                    81
      SHLD_DZD=127.0/2                                                    81
      CSHLD_DZD='NONE'                                                    81
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SHLD',    82
     *BPATH_SHLD,IDNUM_SHLD,1, MAP_SHLD,NAMES_SHLD,COMMEN_SHLD,           82
     *BANK_SHLD,LEN_SHLD,LLEN_SHLD,LINK_SHLD,FLAG_SHLD)                   82
C *                                                                       82
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SHLD',    82
     *BPATH_SHLD,IDNUM_SHLD,1, MAP_SHLD,NAMES_SHLD,COMMEN_SHLD,           82
     *BANK_SHLD,LEN_SHLD,LLEN_SHLD,LINK_SHLD,FLAG_SHLD)                   82
C *                                                                       83
      CALL AGSBEGM(MODULE,IPRIN)                                          83
      IF (IPRIN.LT.0) RETURN                                              83
C Filling bank PRES - PRE_SHOWER                                          83
      BANK_TITLE='PRE_SHOWER'                                             83
      BPATH_PRES='PRES*'                                                  83
      IDNUM_PRES(1)=0                                                     84
C NONE                                                                    84
      PRES_VERSION=1.0                                                    84
      CPRES_VERSION='NONE'                                                84
C NONE                                                                    85
      PRES_XP1=0.0                                                        85
      CPRES_XP1='NONE'                                                    85
C NONE                                                                    86
      PRES_YP1=0.0                                                        86
      CPRES_YP1='NONE'                                                    86
C NONE                                                                    87
      PRES_ZP1=707.2                                                      87
      CPRES_ZP1='NONE'                                                    87
C NONE                                                                    88
      PRES_RP1=45.0                                                       88
      CPRES_RP1='NONE'                                                    88
C NONE                                                                    89
      PRES_XP2=0.0                                                        89
      CPRES_XP2='NONE'                                                    89
C NONE                                                                    90
      PRES_YP2=0.0                                                        90
      CPRES_YP2='NONE'                                                    90
C NONE                                                                    91
      PRES_ZP2=711.2                                                      91
      CPRES_ZP2='NONE'                                                    91
C NONE                                                                    92
      PRES_RP2=-45.0                                                      92
      CPRES_RP2='NONE'                                                    92
C NONE                                                                    93
      PRES_XFE=0.0                                                        93
      CPRES_XFE='NONE'                                                    93
C NONE                                                                    94
      PRES_YFE=0.0                                                        94
      CPRES_YFE='NONE'                                                    94
C NONE                                                                    95
      PRES_ZFE=709.2                                                      95
      CPRES_ZFE='NONE'                                                    95
C NONE                                                                    96
      PRES_RFE=45.0                                                       96
      CPRES_RFE='NONE'                                                    96
C NONE                                                                    97
      PRES_STRPXD=1.25                                                    97
      CPRES_STRPXD='NONE'                                                 97
C NONE                                                                    98
      PRES_STRPYD=45.0                                                    98
      CPRES_STRPYD='NONE'                                                 98
C NONE                                                                    99
      PRES_STRPZD=0.50                                                    99
      CPRES_STRPZD='NONE'                                                 99
C NONE                                                                    100
      PRES_FEXD=24.1                                                      100
      CPRES_FEXD='NONE'                                                   100
C NONE                                                                    101
      PRES_FEYD=24.1                                                      101
      CPRES_FEYD='NONE'                                                   101
C NONE                                                                    102
      PRES_FEZD=0.31                                                      102
      CPRES_FEZD='NONE'                                                   102
C NONE                                                                    103
      PRES_STRIPS=20                                                      103
      CPRES_STRIPS='NONE'                                                 103
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PRES',    104
     *BPATH_PRES,IDNUM_PRES,1, MAP_PRES,NAMES_PRES,COMMEN_PRES,           104
     *BANK_PRES,LEN_PRES,LLEN_PRES,LINK_PRES,FLAG_PRES)                   104
C *                                                                       104
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PRES',    104
     *BPATH_PRES,IDNUM_PRES,1, MAP_PRES,NAMES_PRES,COMMEN_PRES,           104
     *BANK_PRES,LEN_PRES,LLEN_PRES,LINK_PRES,FLAG_PRES)                   104
C use MCDT bank                                                           105
      BPATH_MCDT='MCDT*'                                                  105
      IDNUM_MCDT(1)=0                                                     105
      MCDT_VERSION=1.0                                                    105
      CALL AGDGETP('HCALGEO','*',-999,'MCDT',NMCDT_VERSION,               105
     *RE_MCDT_VERSION,BPATH_MCDT,IDNUM_MCDT,1,MAP_MCDT,NAMES_MCDT,        105
     *BANK_MCDT,LEN_MCDT,LLEN_MCDT,LINK_MCDT,FLAG_MCDT)                   105
C use TOWR bank                                                           106
      BPATH_TOWR='TOWR*'                                                  106
      IDNUM_TOWR(1)=0                                                     106
      TOWR_VERSION=1.0                                                    106
      CALL AGDGETP('HCALGEO','*',-999,'TOWR',NTOWR_VERSION,               106
     *RE_TOWR_VERSION,BPATH_TOWR,IDNUM_TOWR,1,MAP_TOWR,NAMES_TOWR,        106
     *BANK_TOWR,LEN_TOWR,LLEN_TOWR,LINK_TOWR,FLAG_TOWR)                   106
C use CELL bank                                                           107
      BPATH_CELL='CELL*'                                                  107
      IDNUM_CELL(1)=0                                                     107
      CELL_VERSION=1.0                                                    107
      CALL AGDGETP('HCALGEO','*',-999,'CELL',NCELL_VERSION,               107
     *RE_CELL_VERSION,BPATH_CELL,IDNUM_CELL,1,MAP_CELL,NAMES_CELL,        107
     *BANK_CELL,LEN_CELL,LLEN_CELL,LINK_CELL,FLAG_CELL)                   107
C use FIBR bank                                                           108
      BPATH_FIBR='FIBR*'                                                  108
      IDNUM_FIBR(1)=0                                                     108
      FIBR_VERSION=1.0                                                    108
      CALL AGDGETP('HCALGEO','*',-999,'FIBR',NFIBR_VERSION,               108
     *RE_FIBR_VERSION,BPATH_FIBR,IDNUM_FIBR,1,MAP_FIBR,NAMES_FIBR,        108
     *BANK_FIBR,LEN_FIBR,LLEN_FIBR,LINK_FIBR,FLAG_FIBR)                   108
C use SHLD bank                                                           109
      BPATH_SHLD='SHLD*'                                                  109
      IDNUM_SHLD(1)=0                                                     109
      SHLD_VERSION=1.0                                                    109
      CALL AGDGETP('HCALGEO','*',-999,'SHLD',NSHLD_VERSION,               109
     *RE_SHLD_VERSION,BPATH_SHLD,IDNUM_SHLD,1,MAP_SHLD,NAMES_SHLD,        109
     *BANK_SHLD,LEN_SHLD,LLEN_SHLD,LINK_SHLD,FLAG_SHLD)                   109
C use PRES bank                                                           110
      BPATH_PRES='PRES*'                                                  110
      IDNUM_PRES(1)=0                                                     110
      PRES_VERSION=1.0                                                    110
      CALL AGDGETP('HCALGEO','*',-999,'PRES',NPRES_VERSION,               110
     *RE_PRES_VERSION,BPATH_PRES,IDNUM_PRES,1,MAP_PRES,NAMES_PRES,        110
     *BANK_PRES,LEN_PRES,LLEN_PRES,LINK_PRES,FLAG_PRES)                   110
      CALL AGSBEGM(MODULE,IPRIN)                                          111
C Check IPRIN<0                                                           111
      IF (IPRIN.LT.0) RETURN                                              111
C Create HCMO :                                                           111
C Call To Local Procedure HCMO                                            111
      ASSIGN 5020 TO BLOCK_HCMO                                           111
      GO TO  5010                                                         111
5020  CONTINUE                                                            111
C *                                                                       111
C Geant POSITION : HCMO ===> MOTHER='CAVE' X=MCDT_X Y=MCDT_Y Z=MCDT_Z <=  112
C ==                                                                      112
      CALL AGSRESET                                                       112
      AG_MOTHER = 'CAVE'                                                  112
      AG_X = MCDT_X                                                       112
      AG_Y = MCDT_Y                                                       112
      AG_Z = MCDT_Z                                                       112
      AG_TITLE = 'HCMO'                                                   112
      AG_EXNAME = 'POSITION'                                              112
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         112
      CALL AXPOSITION                                                     112
C *                                                                       112
      CALL AGSBEGM(MODULE,IPRIN)                                          112
C Check IPRIN<0                                                           113
      IF (IPRIN.LT.0) RETURN                                              113
C Create HSWR :                                                           113
C Call To Local Procedure HSWR                                            113
      ASSIGN 5040 TO BLOCK_HSWR                                           113
      GO TO  5030                                                         113
5040  CONTINUE                                                            113
C *                                                                       113
C Geant POSITION : HSWR ===> MOTHER='CAVE' X=PRES_XP2 Y=PRES_YP2 Z=PRES_  114
C ZP2 ALPHAZ=PRES_RP2 <===                                                114
      CALL AGSRESET                                                       114
      AG_MOTHER = 'CAVE'                                                  114
      AG_X = PRES_XP2                                                     114
      AG_Y = PRES_YP2                                                     114
      AG_Z = PRES_ZP2                                                     114
      AG_ALPHAN=AG_ALPHAN+1                                               114
      AG_ALPHAK(AG_ALPHAN)=3                                              114
      AG_ALPHAL(AG_ALPHAN)=PRES_RP2                                       114
      AG_TITLE = 'HSWR'                                                   114
      AG_EXNAME = 'POSITION'                                              114
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAZ'                                  114
      CALL AXPOSITION                                                     114
C *                                                                       114
      CALL AGSBEGM(MODULE,IPRIN)                                          114
C Check IPRIN<0                                                           115
      IF (IPRIN.LT.0) RETURN                                              115
C Create HSFE :                                                           115
C Call To Local Procedure HSFE                                            115
      ASSIGN 5060 TO BLOCK_HSFE                                           115
      GO TO  5050                                                         115
5060  CONTINUE                                                            115
C *                                                                       115
C Geant POSITION : HSFE ===> MOTHER='CAVE' X=PRES_XFE Y=PRES_YFE Z=PRES_  116
C ZFE ALPHAZ=PRES_RFE <===                                                116
      CALL AGSRESET                                                       116
      AG_MOTHER = 'CAVE'                                                  116
      AG_X = PRES_XFE                                                     116
      AG_Y = PRES_YFE                                                     116
      AG_Z = PRES_ZFE                                                     116
      AG_ALPHAN=AG_ALPHAN+1                                               116
      AG_ALPHAK(AG_ALPHAN)=3                                              116
      AG_ALPHAL(AG_ALPHAN)=PRES_RFE                                       116
      AG_TITLE = 'HSFE'                                                   116
      AG_EXNAME = 'POSITION'                                              116
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAZ'                                  116
      CALL AXPOSITION                                                     116
C *                                                                       116
      CALL AGSBEGM(MODULE,IPRIN)                                          116
C Check IPRIN<0                                                           117
      IF (IPRIN.LT.0) RETURN                                              117
C Create HSWR :                                                           117
C Call To Local Procedure HSWR                                            117
      ASSIGN 5070 TO BLOCK_HSWR                                           117
      GO TO  5030                                                         117
5070  CONTINUE                                                            117
C *                                                                       117
C Geant POSITION : HSWR ===> MOTHER='CAVE' X=PRES_XP1 Y=PRES_YP1 Z=PRES_  118
C ZP1 ALPHAZ=PRES_RP1 <===                                                118
      CALL AGSRESET                                                       118
      AG_MOTHER = 'CAVE'                                                  118
      AG_X = PRES_XP1                                                     118
      AG_Y = PRES_YP1                                                     118
      AG_Z = PRES_ZP1                                                     118
      AG_ALPHAN=AG_ALPHAN+1                                               118
      AG_ALPHAK(AG_ALPHAN)=3                                              118
      AG_ALPHAL(AG_ALPHAN)=PRES_RP1                                       118
      AG_TITLE = 'HSWR'                                                   118
      AG_EXNAME = 'POSITION'                                              118
      AG_PARLIST = 'MOTHER_X_Y_Z_ALPHAZ'                                  118
      CALL AXPOSITION                                                     118
C *                                                                       118
C ----------------------------------------------------------------------  118
C ------------ HCMO                                                       118
      CALL AGSENDM                                                        120
      RETURN                                                              120
C Begin Local Procedure HCMO                                              120
C HCMO MOTHER CALORIMETER                                                 120
5010  CONTINUE                                                            120
         AG_TITLE='HCMO'                                                  120
         AG_MODULE=MODULE                                                 120
         AG_MARK='V00'                                                    120
         CALL AGSPUSH                                                     120
         AG_MODULE=MODULE                                                 120
C    *                                                                    120
C    Geant MATERIAL : AIR ===> inherited arguments used <===              121
         CALL AGSRESET                                                    121
         AG_TITLE = 'AIR'                                                 121
         AG_EXNAME = 'MATERIAL'                                           121
         AG_PARLIST = 'NONE'                                              121
         CALL AXMATERIAL                                                  121
C    *                                                                    121
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           121
         CALL AGSRESET                                                    122
         AG_TITLE = 'STANDARD'                                            122
         AG_EXNAME = 'MEDIUM'                                             122
         AG_PARLIST = 'NONE'                                              122
         CALL AXMEDIUM                                                    122
C    *                                                                    122
C    Geant ATTRIBUTE : NONE ===> SEEN=0 COLO=2 <===                       122
         CALL AGSRESET                                                    123
         AG_SEEN = 0                                                      123
         AG_COLO = 2                                                      123
         AG_TITLE = 'NONE'                                                123
         AG_EXNAME = 'ATTRIBUTE'                                          123
         AG_PARLIST = 'SEEN_COLO'                                         123
         CALL AXATTRIBUTE                                                 123
C    *                                                                    123
C    Geant SHAPE : BOX ===> DX=MCDT_DX DY=MCDT_DY DZ=MCDT_DZ <===         123
         CALL AGSRESET                                                    124
         AG_DX = MCDT_DX                                                  124
         AG_DY = MCDT_DY                                                  124
         AG_DZ = MCDT_DZ                                                  124
         AG_TITLE = 'BOX'                                                 124
         AG_EXNAME = 'SHAPE'                                              124
         AG_PARLIST = 'DX_DY_DZ'                                          124
         CALL AXSHAPE                                                     124
C    *                                                                    124
C    *                                                                    124
         I=0                                                              125
            GO TO 5083                                                    125
5081        I=I+1                                                         125
5083        IF(I-(TOWR_NTOWERX-1).GT.0) GO TO 5082                        125
C       *                                                                 126
            J=0                                                           126
               GO TO 5093                                                 126
5091           J=J+1                                                      126
5093           IF(J-(TOWR_NTOWERY-1).GT.0) GO TO 5092                     126
C          Create HTWR :                                                  127
C          Call To Local Procedure HTWR                                   127
               ASSIGN 5110 TO BLOCK_HTWR                                  127
               GO TO  5100                                                127
5110           CONTINUE                                                   127
C          *                                                              127
C          Geant POSITION : HTWR ===> X=-((TOWR_NTOWERX-1)*TOWR_DX)+(I*T  129
C          OWR_DX*2) Y=-((TOWR_NTOWERY-1)*TOWR_DY)+(J*TOWR_DY*2) Z=0.0 <  129
C          ===                                                            129
               CALL AGSRESET                                              129
               AG_X = -((TOWR_NTOWERX-1)*TOWR_DX)+(I*TOWR_DX*2)           129
               AG_Y = -((TOWR_NTOWERY-1)*TOWR_DY)+(J*TOWR_DY*2)           129
               AG_Z = 0.0                                                 129
               AG_TITLE = 'HTWR'                                          129
               AG_EXNAME = 'POSITION'                                     129
               AG_PARLIST = 'X_Y_Z'                                       129
               CALL AXPOSITION                                            129
C          *                                                              129
            GO TO 5091                                                    130
5092        CONTINUE                                                      130
         GO TO 5081                                                       131
5082     CONTINUE                                                         131
C    Create HSLD :                                                        132
C    Call To Local Procedure HSLD                                         132
         ASSIGN 5130 TO BLOCK_HSLD                                        132
         GO TO  5120                                                      132
5130     CONTINUE                                                         132
C    *                                                                    132
C    Geant POSITION : HSLD ===> X=SHLD_XD Y=SHLD_YD Z=SHLD_ZD <===        133
         CALL AGSRESET                                                    133
         AG_X = SHLD_XD                                                   133
         AG_Y = SHLD_YD                                                   133
         AG_Z = SHLD_ZD                                                   133
         AG_TITLE = 'HSLD'                                                133
         AG_EXNAME = 'POSITION'                                           133
         AG_PARLIST = 'X_Y_Z'                                             133
         CALL AXPOSITION                                                  133
C    *                                                                    133
5011  CALL AGSPOPB                                                        134
      GO TO BLOCK_HCMO, (5010 ,5020)                                      134
C End of Local Procedure                                                  134
C ----------------------------------------------------------------------  135
C ------------ HTWR                                                       135
C Begin Local Procedure HTWR                                              136
C HTWR TOWRS OF CALORIMETER                                               136
5100  CONTINUE                                                            136
         AG_TITLE='HTWR'                                                  136
         AG_MODULE=MODULE                                                 136
         AG_MARK='V00'                                                    136
         CALL AGSPUSH                                                     136
         AG_MODULE=MODULE                                                 136
C    *                                                                    136
C    Geant MATERIAL : LEAD ===> inherited arguments used <===             137
         CALL AGSRESET                                                    137
         AG_TITLE = 'LEAD'                                                137
         AG_EXNAME = 'MATERIAL'                                           137
         AG_PARLIST = 'NONE'                                              137
         CALL AXMATERIAL                                                  137
C    *                                                                    137
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           137
         CALL AGSRESET                                                    138
         AG_TITLE = 'STANDARD'                                            138
         AG_EXNAME = 'MEDIUM'                                             138
         AG_PARLIST = 'NONE'                                              138
         CALL AXMEDIUM                                                    138
C    *                                                                    138
C    Geant ATTRIBUTE : NONE ===> SEEN=0 COLO=4 <===                       138
         CALL AGSRESET                                                    139
         AG_SEEN = 0                                                      139
         AG_COLO = 4                                                      139
         AG_TITLE = 'NONE'                                                139
         AG_EXNAME = 'ATTRIBUTE'                                          139
         AG_PARLIST = 'SEEN_COLO'                                         139
         CALL AXATTRIBUTE                                                 139
C    *                                                                    139
C    Geant SHAPE : BOX ===> DX=TOWR_DX DY=TOWR_DY DZ=TOWR_DZ <===         139
         CALL AGSRESET                                                    140
         AG_DX = TOWR_DX                                                  140
         AG_DY = TOWR_DY                                                  140
         AG_DZ = TOWR_DZ                                                  140
         AG_TITLE = 'BOX'                                                 140
         AG_EXNAME = 'SHAPE'                                              140
         AG_PARLIST = 'DX_DY_DZ'                                          140
         CALL AXSHAPE                                                     140
C    *                                                                    140
         XTEMP=FIBR_STARTX1                                               140
C    *                                                                    142
         M=0                                                              142
            GO TO 5143                                                    142
5141        M=M+1                                                         142
5143        IF(M-(CELL_NCELLX-1).GT.0) GO TO 5142                         142
            YTEMP=FIBR_STARTY1                                            142
C       *                                                                 144
            N=0                                                           144
               GO TO 5153                                                 144
5151           N=N+1                                                      144
5153           IF(N-(CELL_NCELLY-1).GT.0) GO TO 5152                      144
C          Check M.EQ.N                                                   145
               IF (M.EQ.N) THEN                                           145
C          Create HCES :                                                  146
C          Call To Local Procedure HCES                                   146
               ASSIGN 5170 TO BLOCK_HCES                                  146
               GO TO  5160                                                146
5170           CONTINUE                                                   146
C          *                                                              146
C          Geant POSITION : HCES ===> X=-3.33+(3.33*M) Y=-3.33+(N*3.33)   147
C          Z=0.0 <===                                                     147
               CALL AGSRESET                                              147
               AG_X = -3.33+(3.33*M)                                      147
               AG_Y = -3.33+(N*3.33)                                      147
               AG_Z = 0.0                                                 147
               AG_TITLE = 'HCES'                                          147
               AG_EXNAME = 'POSITION'                                     147
               AG_PARLIST = 'X_Y_Z'                                       147
               CALL AXPOSITION                                            147
C          *                                                              147
               ELSE                                                       147
C          Create HCEL :                                                  149
C          Call To Local Procedure HCEL                                   149
               ASSIGN 5190 TO BLOCK_HCEL                                  149
               GO TO  5180                                                149
5190           CONTINUE                                                   149
C          *                                                              149
C          Geant POSITION : HCEL ===> X=-3.33+(3.33*M) Y=-3.33+(N*3.33)   150
C          Z=0.0 <===                                                     150
               CALL AGSRESET                                              150
               AG_X = -3.33+(3.33*M)                                      150
               AG_Y = -3.33+(N*3.33)                                      150
               AG_Z = 0.0                                                 150
               AG_TITLE = 'HCEL'                                          150
               AG_EXNAME = 'POSITION'                                     150
               AG_PARLIST = 'X_Y_Z'                                       150
               CALL AXPOSITION                                            150
C          *                                                              150
               ENDIF                                                      150
               YTEMP=FIBR_STARTY2                                         151
            GO TO 5151                                                    153
5152        CONTINUE                                                      153
            XTEMP=FIBR_STARTX2                                            153
         GO TO 5141                                                       155
5142     CONTINUE                                                         155
5101  CALL AGSPOPB                                                        156
      GO TO BLOCK_HTWR, (5100 ,5110)                                      156
C End of Local Procedure                                                  156
C ----------------------------------------------------------------------  157
C ------------ HCES                                                       157
C Begin Local Procedure HCES                                              158
C HCES 3*3 CELL IN TOWER, HCES HAS 15*15 FIBERS                           158
5160  CONTINUE                                                            158
         AG_TITLE='HCES'                                                  158
         AG_MODULE=MODULE                                                 158
         AG_MARK='V00'                                                    158
         CALL AGSPUSH                                                     158
         AG_MODULE=MODULE                                                 158
C    *                                                                    158
C    Geant MATERIAL : LEAD ===> inherited arguments used <===             159
         CALL AGSRESET                                                    159
         AG_TITLE = 'LEAD'                                                159
         AG_EXNAME = 'MATERIAL'                                           159
         AG_PARLIST = 'NONE'                                              159
         CALL AXMATERIAL                                                  159
C    *                                                                    159
C    Geant MATERIAL : LEAD ===> ISVOL=1 <===                              159
         CALL AGSRESET                                                    160
         AG_ISVOL = 1                                                     160
         AG_TITLE = 'LEAD'                                                160
         AG_EXNAME = 'MATERIAL'                                           160
         AG_PARLIST = 'ISVOL'                                             160
         CALL AXMATERIAL                                                  160
C    *                                                                    160
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=4 <===                       160
         CALL AGSRESET                                                    161
         AG_SEEN = 1                                                      161
         AG_COLO = 4                                                      161
         AG_TITLE = 'NONE'                                                161
         AG_EXNAME = 'ATTRIBUTE'                                          161
         AG_PARLIST = 'SEEN_COLO'                                         161
         CALL AXATTRIBUTE                                                 161
C    *                                                                    161
C    Geant SHAPE : BOX ===> DX=CELL_DX DY=CELL_DY DZ=CELL_DZ <===         161
         CALL AGSRESET                                                    162
         AG_DX = CELL_DX                                                  162
         AG_DY = CELL_DY                                                  162
         AG_DZ = CELL_DZ                                                  162
         AG_TITLE = 'BOX'                                                 162
         AG_EXNAME = 'SHAPE'                                              162
         AG_PARLIST = 'DX_DY_DZ'                                          162
         CALL AXSHAPE                                                     162
C    *                                                                    162
C    Create HCSX :                                                        163
C    Call To Local Procedure HCSX                                         163
         ASSIGN 5210 TO BLOCK_HCSX                                        163
         GO TO  5200                                                      163
5210     CONTINUE                                                         163
C    *                                                                    163
C    Defining Hits: HCAL:0:S(0,100.0) ELOS:0:S(0,100.0) BIRK:0:S(0,100.0  164
C    )                                                                    164
         AG_TITLE = 'HCES'                                                164
         AG_EXNAME = 'HITS'                                               164
         AG_TYPE=AG_MAGIC                                                 164
         CALL AGSDETV('HCES')                                             164
C    *                                                                    164
         AG_PARLIST='HCAL'                                                164
         AG_TITLE = 'DETH'                                                164
         AG_NBITS = 0                                                     164
         AG_BIN=AG_MAGIC                                                  164
         AG_OPTION='S'                                                    164
         AG_FHMIN = 0                                                     164
         AG_FHMAX = 100.0                                                 164
         CALL AGSDETH (0)                                                 164
C    *                                                                    164
         AG_PARLIST='ELOS'                                                164
         AG_TITLE = 'DETH'                                                164
         AG_NBITS = 0                                                     164
         AG_BIN=AG_MAGIC                                                  164
         AG_OPTION='S'                                                    164
         AG_FHMIN = 0                                                     164
         AG_FHMAX = 100.0                                                 164
         CALL AGSDETH (0)                                                 164
C    *                                                                    164
         AG_PARLIST='BIRK'                                                164
         AG_TITLE = 'DETH'                                                164
         AG_NBITS = 0                                                     164
         AG_BIN=AG_MAGIC                                                  164
         AG_OPTION='S'                                                    164
         AG_FHMIN = 0                                                     164
         AG_FHMAX = 100.0                                                 164
         CALL AGSDETH (0)                                                 164
C    *                                                                    164
         CALL AGSDETU('HCES')                                             164
C    *                                                                    164
5161  CALL AGSPOPB                                                        165
      GO TO BLOCK_HCES, (5160 ,5170)                                      165
C End of Local Procedure                                                  165
C ----------------------------------------------------------------------  166
C ------------ HCEL                                                       166
C Begin Local Procedure HCEL                                              167
C HCEL 3*3 CELL IN TOWER, HCES HAS 16*16 FIBERS                           167
5180  CONTINUE                                                            167
         AG_TITLE='HCEL'                                                  167
         AG_MODULE=MODULE                                                 167
         AG_MARK='V00'                                                    167
         CALL AGSPUSH                                                     167
         AG_MODULE=MODULE                                                 167
C    *                                                                    167
C    Geant MATERIAL : LEAD ===> inherited arguments used <===             168
         CALL AGSRESET                                                    168
         AG_TITLE = 'LEAD'                                                168
         AG_EXNAME = 'MATERIAL'                                           168
         AG_PARLIST = 'NONE'                                              168
         CALL AXMATERIAL                                                  168
C    *                                                                    168
C    Geant MATERIAL : LEAD ===> ISVOL=1 <===                              168
         CALL AGSRESET                                                    169
         AG_ISVOL = 1                                                     169
         AG_TITLE = 'LEAD'                                                169
         AG_EXNAME = 'MATERIAL'                                           169
         AG_PARLIST = 'ISVOL'                                             169
         CALL AXMATERIAL                                                  169
C    *                                                                    169
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=4 <===                       169
         CALL AGSRESET                                                    170
         AG_SEEN = 1                                                      170
         AG_COLO = 4                                                      170
         AG_TITLE = 'NONE'                                                170
         AG_EXNAME = 'ATTRIBUTE'                                          170
         AG_PARLIST = 'SEEN_COLO'                                         170
         CALL AXATTRIBUTE                                                 170
C    *                                                                    170
C    Geant SHAPE : BOX ===> DX=CELL_DX DY=CELL_DY DZ=CELL_DZ <===         170
         CALL AGSRESET                                                    171
         AG_DX = CELL_DX                                                  171
         AG_DY = CELL_DY                                                  171
         AG_DZ = CELL_DZ                                                  171
         AG_TITLE = 'BOX'                                                 171
         AG_EXNAME = 'SHAPE'                                              171
         AG_PARLIST = 'DX_DY_DZ'                                          171
         CALL AXSHAPE                                                     171
C    *                                                                    171
C    Create HCLX :                                                        172
C    Call To Local Procedure HCLX                                         172
         ASSIGN 5230 TO BLOCK_HCLX                                        172
         GO TO  5220                                                      172
5230     CONTINUE                                                         172
C    *                                                                    172
C    Defining Hits: HCAL:0:S(0,100.0) ELOS:0:S(0,100.0) BIRK:0:S(0,100.0  173
C    )                                                                    173
         AG_TITLE = 'HCEL'                                                173
         AG_EXNAME = 'HITS'                                               173
         AG_TYPE=AG_MAGIC                                                 173
         CALL AGSDETV('HCEL')                                             173
C    *                                                                    173
         AG_PARLIST='HCAL'                                                173
         AG_TITLE = 'DETH'                                                173
         AG_NBITS = 0                                                     173
         AG_BIN=AG_MAGIC                                                  173
         AG_OPTION='S'                                                    173
         AG_FHMIN = 0                                                     173
         AG_FHMAX = 100.0                                                 173
         CALL AGSDETH (0)                                                 173
C    *                                                                    173
         AG_PARLIST='ELOS'                                                173
         AG_TITLE = 'DETH'                                                173
         AG_NBITS = 0                                                     173
         AG_BIN=AG_MAGIC                                                  173
         AG_OPTION='S'                                                    173
         AG_FHMIN = 0                                                     173
         AG_FHMAX = 100.0                                                 173
         CALL AGSDETH (0)                                                 173
C    *                                                                    173
         AG_PARLIST='BIRK'                                                173
         AG_TITLE = 'DETH'                                                173
         AG_NBITS = 0                                                     173
         AG_BIN=AG_MAGIC                                                  173
         AG_OPTION='S'                                                    173
         AG_FHMIN = 0                                                     173
         AG_FHMAX = 100.0                                                 173
         CALL AGSDETH (0)                                                 173
C    *                                                                    173
         CALL AGSDETU('HCEL')                                             173
C    *                                                                    173
5181  CALL AGSPOPB                                                        174
      GO TO BLOCK_HCEL, (5180 ,5190)                                      174
C End of Local Procedure                                                  174
C ----------------------------------------------------------------------  175
C ------------ HCSX                                                       175
C Begin Local Procedure HCSX                                              176
C HCSX 3*3 CELL IN TOWER X DIVISIONS                                      176
5200  CONTINUE                                                            176
         AG_TITLE='HCSX'                                                  176
         AG_MODULE=MODULE                                                 176
         AG_MARK='V00'                                                    176
         CALL AGSPUSH                                                     176
         AG_MODULE=MODULE                                                 176
C    *                                                                    176
C    Geant MATERIAL : LEAD ===> ISVOL=0 <===                              177
         CALL AGSRESET                                                    177
         AG_ISVOL = 0                                                     177
         AG_TITLE = 'LEAD'                                                177
         AG_EXNAME = 'MATERIAL'                                           177
         AG_PARLIST = 'ISVOL'                                             177
         CALL AXMATERIAL                                                  177
C    *                                                                    177
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=4 <===                       177
         CALL AGSRESET                                                    178
         AG_SEEN = 1                                                      178
         AG_COLO = 4                                                      178
         AG_TITLE = 'NONE'                                                178
         AG_EXNAME = 'ATTRIBUTE'                                          178
         AG_PARLIST = 'SEEN_COLO'                                         178
         CALL AXATTRIBUTE                                                 178
C    *                                                                    178
C    Geant SHAPE : DIVISION ===> NDIV=CELL_NFIBERXS IAXIS=1 <===          178
         CALL AGSRESET                                                    179
         AG_NDIV = CELL_NFIBERXS                                          179
         AG_IAXIS = 1                                                     179
         AG_TITLE = 'DIVISION'                                            179
         AG_EXNAME = 'SHAPE'                                              179
         AG_PARLIST = 'NDIV_IAXIS'                                        179
         CALL AXSHAPE                                                     179
C    *                                                                    179
C    Create HCSY :                                                        180
C    Call To Local Procedure HCSY                                         180
         ASSIGN 5250 TO BLOCK_HCSY                                        180
         GO TO  5240                                                      180
5250     CONTINUE                                                         180
C    *                                                                    180
5201  CALL AGSPOPB                                                        181
      GO TO BLOCK_HCSX, (5200 ,5210)                                      181
C End of Local Procedure                                                  181
C ----------------------------------------------------------------------  182
C ------------ HCSY                                                       182
C Begin Local Procedure HCSY                                              183
C HCSY 3*3 CELL IN TOWER Y DIVISIONS                                      183
5240  CONTINUE                                                            183
         AG_TITLE='HCSY'                                                  183
         AG_MODULE=MODULE                                                 183
         AG_MARK='V00'                                                    183
         CALL AGSPUSH                                                     183
         AG_MODULE=MODULE                                                 183
C    *                                                                    183
C    Geant MATERIAL : LEAD ===> ISVOL=0 <===                              184
         CALL AGSRESET                                                    184
         AG_ISVOL = 0                                                     184
         AG_TITLE = 'LEAD'                                                184
         AG_EXNAME = 'MATERIAL'                                           184
         AG_PARLIST = 'ISVOL'                                             184
         CALL AXMATERIAL                                                  184
C    *                                                                    184
C    Geant ATTRIBUTE : NONE ===> SEEN=0 COLO=4 <===                       184
         CALL AGSRESET                                                    185
         AG_SEEN = 0                                                      185
         AG_COLO = 4                                                      185
         AG_TITLE = 'NONE'                                                185
         AG_EXNAME = 'ATTRIBUTE'                                          185
         AG_PARLIST = 'SEEN_COLO'                                         185
         CALL AXATTRIBUTE                                                 185
C    *                                                                    185
C    Geant SHAPE : DIVISION ===> NDIV=CELL_NFIBERYS IAXIS=2 <===          185
         CALL AGSRESET                                                    186
         AG_NDIV = CELL_NFIBERYS                                          186
         AG_IAXIS = 2                                                     186
         AG_TITLE = 'DIVISION'                                            186
         AG_EXNAME = 'SHAPE'                                              186
         AG_PARLIST = 'NDIV_IAXIS'                                        186
         CALL AXSHAPE                                                     186
C    *                                                                    186
C    Create HFIB :                                                        187
C    Call To Local Procedure HFIB                                         187
         ASSIGN 5270 TO BLOCK_HFIB                                        187
         GO TO  5260                                                      187
5270     CONTINUE                                                         187
C    *                                                                    187
C    Geant POSITION : HFIB ===> X=0 Y=0 Z=0.0 <===                        188
         CALL AGSRESET                                                    188
         AG_X = 0                                                         188
         AG_Y = 0                                                         188
         AG_Z = 0.0                                                       188
         AG_TITLE = 'HFIB'                                                188
         AG_EXNAME = 'POSITION'                                           188
         AG_PARLIST = 'X_Y_Z'                                             188
         CALL AXPOSITION                                                  188
C    *                                                                    188
5241  CALL AGSPOPB                                                        189
      GO TO BLOCK_HCSY, (5240 ,5250)                                      189
C End of Local Procedure                                                  189
C ----------------------------------------------------------------------  190
C ------------ HCLX                                                       190
C Begin Local Procedure HCLX                                              191
C HCLX 3*3 CELL IN TOWER X DIVISIONS                                      191
5220  CONTINUE                                                            191
         AG_TITLE='HCLX'                                                  191
         AG_MODULE=MODULE                                                 191
         AG_MARK='V00'                                                    191
         CALL AGSPUSH                                                     191
         AG_MODULE=MODULE                                                 191
C    *                                                                    191
C    Geant MATERIAL : LEAD ===> ISVOL=0 <===                              192
         CALL AGSRESET                                                    192
         AG_ISVOL = 0                                                     192
         AG_TITLE = 'LEAD'                                                192
         AG_EXNAME = 'MATERIAL'                                           192
         AG_PARLIST = 'ISVOL'                                             192
         CALL AXMATERIAL                                                  192
C    *                                                                    192
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=4 <===                       192
         CALL AGSRESET                                                    193
         AG_SEEN = 1                                                      193
         AG_COLO = 4                                                      193
         AG_TITLE = 'NONE'                                                193
         AG_EXNAME = 'ATTRIBUTE'                                          193
         AG_PARLIST = 'SEEN_COLO'                                         193
         CALL AXATTRIBUTE                                                 193
C    *                                                                    193
C    Geant SHAPE : DIVISION ===> NDIV=CELL_NFIBERXL IAXIS=1 <===          193
         CALL AGSRESET                                                    194
         AG_NDIV = CELL_NFIBERXL                                          194
         AG_IAXIS = 1                                                     194
         AG_TITLE = 'DIVISION'                                            194
         AG_EXNAME = 'SHAPE'                                              194
         AG_PARLIST = 'NDIV_IAXIS'                                        194
         CALL AXSHAPE                                                     194
C    *                                                                    194
C    Create HCLY :                                                        195
C    Call To Local Procedure HCLY                                         195
         ASSIGN 5290 TO BLOCK_HCLY                                        195
         GO TO  5280                                                      195
5290     CONTINUE                                                         195
C    *                                                                    195
5221  CALL AGSPOPB                                                        196
      GO TO BLOCK_HCLX, (5220 ,5230)                                      196
C End of Local Procedure                                                  196
C ----------------------------------------------------------------------  197
C ------------ HCLY                                                       197
C Begin Local Procedure HCLY                                              198
C HCLY 3*3 CELL IN TOWER Y DIVISIONS                                      198
5280  CONTINUE                                                            198
         AG_TITLE='HCLY'                                                  198
         AG_MODULE=MODULE                                                 198
         AG_MARK='V00'                                                    198
         CALL AGSPUSH                                                     198
         AG_MODULE=MODULE                                                 198
C    *                                                                    198
C    Geant MATERIAL : LEAD ===> ISVOL=0 <===                              199
         CALL AGSRESET                                                    199
         AG_ISVOL = 0                                                     199
         AG_TITLE = 'LEAD'                                                199
         AG_EXNAME = 'MATERIAL'                                           199
         AG_PARLIST = 'ISVOL'                                             199
         CALL AXMATERIAL                                                  199
C    *                                                                    199
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=4 <===                       199
         CALL AGSRESET                                                    200
         AG_SEEN = 1                                                      200
         AG_COLO = 4                                                      200
         AG_TITLE = 'NONE'                                                200
         AG_EXNAME = 'ATTRIBUTE'                                          200
         AG_PARLIST = 'SEEN_COLO'                                         200
         CALL AXATTRIBUTE                                                 200
C    *                                                                    200
C    Geant SHAPE : DIVISION ===> NDIV=CELL_NFIBERYL IAXIS=2 <===          200
         CALL AGSRESET                                                    201
         AG_NDIV = CELL_NFIBERYL                                          201
         AG_IAXIS = 2                                                     201
         AG_TITLE = 'DIVISION'                                            201
         AG_EXNAME = 'SHAPE'                                              201
         AG_PARLIST = 'NDIV_IAXIS'                                        201
         CALL AXSHAPE                                                     201
C    *                                                                    201
C    Create HFIC :                                                        202
C    Call To Local Procedure HFIC                                         202
         ASSIGN 5310 TO BLOCK_HFIC                                        202
         GO TO  5300                                                      202
5310     CONTINUE                                                         202
C    *                                                                    202
C    Geant POSITION : HFIC ===> X=0 Y=0 Z=0.0 <===                        203
         CALL AGSRESET                                                    203
         AG_X = 0                                                         203
         AG_Y = 0                                                         203
         AG_Z = 0.0                                                       203
         AG_TITLE = 'HFIC'                                                203
         AG_EXNAME = 'POSITION'                                           203
         AG_PARLIST = 'X_Y_Z'                                             203
         CALL AXPOSITION                                                  203
C    *                                                                    203
5281  CALL AGSPOPB                                                        204
      GO TO BLOCK_HCLY, (5280 ,5290)                                      204
C End of Local Procedure                                                  204
C ----------------------------------------------------------------------  205
C ------------ HFIB                                                       205
C Begin Local Procedure HFIB                                              206
C HFIB FIBER IN CELLS                                                     206
5260  CONTINUE                                                            206
         AG_TITLE='HFIB'                                                  206
         AG_MODULE=MODULE                                                 206
         AG_MARK='V00'                                                    206
         CALL AGSPUSH                                                     206
         AG_MODULE=MODULE                                                 206
C    *                                                                    206
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       207
         CALL AGSRESET                                                    207
         AG_TITLE = 'POLYSTYREN'                                          207
         AG_EXNAME = 'MATERIAL'                                           207
         AG_PARLIST = 'NONE'                                              207
         CALL AXMATERIAL                                                  207
C    *                                                                    207
C    Geant MATERIAL : SENSPOLY ===> ISVOL=1 <===                          207
         CALL AGSRESET                                                    208
         AG_ISVOL = 1                                                     208
         AG_TITLE = 'SENSPOLY'                                            208
         AG_EXNAME = 'MATERIAL'                                           208
         AG_PARLIST = 'ISVOL'                                             208
         CALL AXMATERIAL                                                  208
C    *                                                                    208
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           208
         CALL AGSRESET                                                    209
         AG_TITLE = 'STANDARD'                                            209
         AG_EXNAME = 'MEDIUM'                                             209
         AG_PARLIST = 'NONE'                                              209
         CALL AXMEDIUM                                                    209
C    *                                                                    209
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=7 <===                       209
         CALL AGSRESET                                                    210
         AG_SEEN = 1                                                      210
         AG_COLO = 7                                                      210
         AG_TITLE = 'NONE'                                                210
         AG_EXNAME = 'ATTRIBUTE'                                          210
         AG_PARLIST = 'SEEN_COLO'                                         210
         CALL AXATTRIBUTE                                                 210
C    *                                                                    210
C    Geant SHAPE : TUBE ===> RMIN=FIBR_RMIN RMAX=FIBR_RMAX, DZ=FIBR_DZ <  210
C    ===                                                                  210
         CALL AGSRESET                                                    212
         AG_RMIN = FIBR_RMIN                                              212
         AG_RMAX = FIBR_RMAX                                              212
         AG_DZ = FIBR_DZ                                                  212
         AG_TITLE = 'TUBE'                                                212
         AG_EXNAME = 'SHAPE'                                              212
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      212
         CALL AXSHAPE                                                     212
C    *                                                                    212
5261  CALL AGSPOPB                                                        213
      GO TO BLOCK_HFIB, (5260 ,5270)                                      213
C End of Local Procedure                                                  213
C ----------------------------------------------------------------------  214
C ------------ HFIC                                                       214
C Begin Local Procedure HFIC                                              215
C HFIC FIBER IN CELLS                                                     215
5300  CONTINUE                                                            215
         AG_TITLE='HFIC'                                                  215
         AG_MODULE=MODULE                                                 215
         AG_MARK='V00'                                                    215
         CALL AGSPUSH                                                     215
         AG_MODULE=MODULE                                                 215
C    *                                                                    215
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       216
         CALL AGSRESET                                                    216
         AG_TITLE = 'POLYSTYREN'                                          216
         AG_EXNAME = 'MATERIAL'                                           216
         AG_PARLIST = 'NONE'                                              216
         CALL AXMATERIAL                                                  216
C    *                                                                    216
C    Geant MATERIAL : SENSPOLY ===> ISVOL=1 <===                          216
         CALL AGSRESET                                                    217
         AG_ISVOL = 1                                                     217
         AG_TITLE = 'SENSPOLY'                                            217
         AG_EXNAME = 'MATERIAL'                                           217
         AG_PARLIST = 'ISVOL'                                             217
         CALL AXMATERIAL                                                  217
C    *                                                                    217
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           217
         CALL AGSRESET                                                    218
         AG_TITLE = 'STANDARD'                                            218
         AG_EXNAME = 'MEDIUM'                                             218
         AG_PARLIST = 'NONE'                                              218
         CALL AXMEDIUM                                                    218
C    *                                                                    218
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=7 <===                       218
         CALL AGSRESET                                                    219
         AG_SEEN = 1                                                      219
         AG_COLO = 7                                                      219
         AG_TITLE = 'NONE'                                                219
         AG_EXNAME = 'ATTRIBUTE'                                          219
         AG_PARLIST = 'SEEN_COLO'                                         219
         CALL AXATTRIBUTE                                                 219
C    *                                                                    219
C    Geant SHAPE : TUBE ===> RMIN=FIBR_RMIN RMAX=FIBR_RMAX, DZ=FIBR_DZ <  219
C    ===                                                                  219
         CALL AGSRESET                                                    221
         AG_RMIN = FIBR_RMIN                                              221
         AG_RMAX = FIBR_RMAX                                              221
         AG_DZ = FIBR_DZ                                                  221
         AG_TITLE = 'TUBE'                                                221
         AG_EXNAME = 'SHAPE'                                              221
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      221
         CALL AXSHAPE                                                     221
C    *                                                                    221
5301  CALL AGSPOPB                                                        222
      GO TO BLOCK_HFIC, (5300 ,5310)                                      222
C End of Local Procedure                                                  222
C ----------------------------------------------------------------------  223
C ------------ HSLD                                                       223
C Begin Local Procedure HSLD                                              224
C HSLD BASE IRON PLATE                                                    224
5120  CONTINUE                                                            224
         AG_TITLE='HSLD'                                                  224
         AG_MODULE=MODULE                                                 224
         AG_MARK='V00'                                                    224
         CALL AGSPUSH                                                     224
         AG_MODULE=MODULE                                                 224
C    *                                                                    224
C    Geant MATERIAL : IRON ===> ISVOL=0 <===                              225
         CALL AGSRESET                                                    225
         AG_ISVOL = 0                                                     225
         AG_TITLE = 'IRON'                                                225
         AG_EXNAME = 'MATERIAL'                                           225
         AG_PARLIST = 'ISVOL'                                             225
         CALL AXMATERIAL                                                  225
C    *                                                                    225
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           225
         CALL AGSRESET                                                    226
         AG_TITLE = 'STANDARD'                                            226
         AG_EXNAME = 'MEDIUM'                                             226
         AG_PARLIST = 'NONE'                                              226
         CALL AXMEDIUM                                                    226
C    *                                                                    226
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=2 <===                       226
         CALL AGSRESET                                                    227
         AG_SEEN = 1                                                      227
         AG_COLO = 2                                                      227
         AG_TITLE = 'NONE'                                                227
         AG_EXNAME = 'ATTRIBUTE'                                          227
         AG_PARLIST = 'SEEN_COLO'                                         227
         CALL AXATTRIBUTE                                                 227
C    *                                                                    227
C    Geant SHAPE : BOX ===> DX=SHLD_DXD DY=SHLD_DYD DZ=SHLD_DZD <===      227
         CALL AGSRESET                                                    228
         AG_DX = SHLD_DXD                                                 228
         AG_DY = SHLD_DYD                                                 228
         AG_DZ = SHLD_DZD                                                 228
         AG_TITLE = 'BOX'                                                 228
         AG_EXNAME = 'SHAPE'                                              228
         AG_PARLIST = 'DX_DY_DZ'                                          228
         CALL AXSHAPE                                                     228
C    *                                                                    228
5121  CALL AGSPOPB                                                        229
      GO TO BLOCK_HSLD, (5120 ,5130)                                      229
C End of Local Procedure                                                  229
C ----------------------------------------------------------------------  230
C ------------ HSWR                                                       230
C Begin Local Procedure HSWR                                              231
C HSWR MOTHER OF PRE SHOWER                                               231
5030  CONTINUE                                                            231
         AG_TITLE='HSWR'                                                  231
         AG_MODULE=MODULE                                                 231
         AG_MARK='V00'                                                    231
         CALL AGSPUSH                                                     231
         AG_MODULE=MODULE                                                 231
C    *                                                                    231
C    Geant MATERIAL : AIR ===> inherited arguments used <===              232
         CALL AGSRESET                                                    232
         AG_TITLE = 'AIR'                                                 232
         AG_EXNAME = 'MATERIAL'                                           232
         AG_PARLIST = 'NONE'                                              232
         CALL AXMATERIAL                                                  232
C    *                                                                    232
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           232
         CALL AGSRESET                                                    233
         AG_TITLE = 'STANDARD'                                            233
         AG_EXNAME = 'MEDIUM'                                             233
         AG_PARLIST = 'NONE'                                              233
         CALL AXMEDIUM                                                    233
C    *                                                                    233
C    Geant ATTRIBUTE : NONE ===> SEEN=0 COLO=2 <===                       233
         CALL AGSRESET                                                    234
         AG_SEEN = 0                                                      234
         AG_COLO = 2                                                      234
         AG_TITLE = 'NONE'                                                234
         AG_EXNAME = 'ATTRIBUTE'                                          234
         AG_PARLIST = 'SEEN_COLO'                                         234
         CALL AXATTRIBUTE                                                 234
C    *                                                                    234
C    Geant SHAPE : BOX ===> DX=25.0 DY=50.0 DZ=8.0 <===                   234
         CALL AGSRESET                                                    235
         AG_DX = 25.0                                                     235
         AG_DY = 50.0                                                     235
         AG_DZ = 8.0                                                      235
         AG_TITLE = 'BOX'                                                 235
         AG_EXNAME = 'SHAPE'                                              235
         AG_PARLIST = 'DX_DY_DZ'                                          235
         CALL AXSHAPE                                                     235
C    *                                                                    235
C    *                                                                    235
         I=0                                                              236
            GO TO 5323                                                    236
5321        I=I+1                                                         236
5323        IF(I-(PRES_STRIPS-1).GT.0) GO TO 5322                         236
C       Create HSTP :                                                     237
C       Call To Local Procedure HSTP                                      237
            ASSIGN 5340 TO BLOCK_HSTP                                     237
            GO TO  5330                                                   237
5340        CONTINUE                                                      237
C       *                                                                 237
C       Geant POSITION : HSTP ===> X=-23.75+(I*2*PRES_STRPXD) Y=0.0 Z=0.  238
C       0 <===                                                            238
            CALL AGSRESET                                                 238
            AG_X = -23.75+(I*2*PRES_STRPXD)                               238
            AG_Y = 0.0                                                    238
            AG_Z = 0.0                                                    238
            AG_TITLE = 'HSTP'                                             238
            AG_EXNAME = 'POSITION'                                        238
            AG_PARLIST = 'X_Y_Z'                                          238
            CALL AXPOSITION                                               238
C       *                                                                 238
         GO TO 5321                                                       239
5322     CONTINUE                                                         239
5031  CALL AGSPOPB                                                        240
      GO TO BLOCK_HSWR, (5030 ,5040,5070)                                 240
C End of Local Procedure                                                  240
C ----------------------------------------------------------------------  241
C ------------ HSTP                                                       241
C Begin Local Procedure HSTP                                              242
C HSTP SINGLE STRIPS IN PRE SHOWER                                        242
5330  CONTINUE                                                            242
         AG_TITLE='HSTP'                                                  242
         AG_MODULE=MODULE                                                 242
         AG_MARK='V00'                                                    242
         CALL AGSPUSH                                                     242
         AG_MODULE=MODULE                                                 242
C    *                                                                    242
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       243
         CALL AGSRESET                                                    243
         AG_TITLE = 'POLYSTYREN'                                          243
         AG_EXNAME = 'MATERIAL'                                           243
         AG_PARLIST = 'NONE'                                              243
         CALL AXMATERIAL                                                  243
C    *                                                                    243
C    Geant MATERIAL : SENSPOLY ===> ISVOL=1 <===                          243
         CALL AGSRESET                                                    244
         AG_ISVOL = 1                                                     244
         AG_TITLE = 'SENSPOLY'                                            244
         AG_EXNAME = 'MATERIAL'                                           244
         AG_PARLIST = 'ISVOL'                                             244
         CALL AXMATERIAL                                                  244
C    *                                                                    244
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           244
         CALL AGSRESET                                                    245
         AG_TITLE = 'STANDARD'                                            245
         AG_EXNAME = 'MEDIUM'                                             245
         AG_PARLIST = 'NONE'                                              245
         CALL AXMEDIUM                                                    245
C    *                                                                    245
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=7 <===                       245
         CALL AGSRESET                                                    246
         AG_SEEN = 1                                                      246
         AG_COLO = 7                                                      246
         AG_TITLE = 'NONE'                                                246
         AG_EXNAME = 'ATTRIBUTE'                                          246
         AG_PARLIST = 'SEEN_COLO'                                         246
         CALL AXATTRIBUTE                                                 246
C    *                                                                    246
C    Geant SHAPE : BOX ===> DX=PRES_STRPXD DY=PRES_STRPYD, DZ=PRES_STRPZ  246
C    D <===                                                               246
         CALL AGSRESET                                                    248
         AG_DX = PRES_STRPXD                                              248
         AG_DY = PRES_STRPYD                                              248
         AG_DZ = PRES_STRPZD                                              248
         AG_TITLE = 'BOX'                                                 248
         AG_EXNAME = 'SHAPE'                                              248
         AG_PARLIST = 'DX_DY_DZ'                                          248
         CALL AXSHAPE                                                     248
C    *                                                                    248
C    Defining Hits: BIRK:0:C(0,250.0)                                     248
         AG_TITLE = 'HSTP'                                                249
         AG_EXNAME = 'HITS'                                               249
         AG_TYPE=AG_MAGIC                                                 249
         CALL AGSDETV('HSTP')                                             249
C    *                                                                    249
         AG_PARLIST='BIRK'                                                249
         AG_TITLE = 'DETH'                                                249
         AG_NBITS = 0                                                     249
         AG_BIN=AG_MAGIC                                                  249
         AG_OPTION='C'                                                    249
         AG_FHMIN = 0                                                     249
         AG_FHMAX = 250.0                                                 249
         CALL AGSDETH (0)                                                 249
C    *                                                                    249
         CALL AGSDETU('HSTP')                                             249
C    *                                                                    249
5331  CALL AGSPOPB                                                        250
      GO TO BLOCK_HSTP, (5330 ,5340)                                      250
C End of Local Procedure                                                  250
C ----------------------------------------------------------------------  251
C ------------ HSFE                                                       251
C Begin Local Procedure HSFE                                              252
C HSFE FE IN BETWEEN TWO PRE SHOWER                                       252
5050  CONTINUE                                                            252
         AG_TITLE='HSFE'                                                  252
         AG_MODULE=MODULE                                                 252
         AG_MARK='V00'                                                    252
         CALL AGSPUSH                                                     252
         AG_MODULE=MODULE                                                 252
C    *                                                                    252
C    Geant MATERIAL : IRON ===> inherited arguments used <===             253
         CALL AGSRESET                                                    253
         AG_TITLE = 'IRON'                                                253
         AG_EXNAME = 'MATERIAL'                                           253
         AG_PARLIST = 'NONE'                                              253
         CALL AXMATERIAL                                                  253
C    *                                                                    253
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           253
         CALL AGSRESET                                                    254
         AG_TITLE = 'STANDARD'                                            254
         AG_EXNAME = 'MEDIUM'                                             254
         AG_PARLIST = 'NONE'                                              254
         CALL AXMEDIUM                                                    254
C    *                                                                    254
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=2 <===                       254
         CALL AGSRESET                                                    255
         AG_SEEN = 1                                                      255
         AG_COLO = 2                                                      255
         AG_TITLE = 'NONE'                                                255
         AG_EXNAME = 'ATTRIBUTE'                                          255
         AG_PARLIST = 'SEEN_COLO'                                         255
         CALL AXATTRIBUTE                                                 255
C    *                                                                    255
C    Geant SHAPE : BOX ===> DX=PRES_FEXD DY=PRES_FEYD DZ=PRES_FEZD <===   255
         CALL AGSRESET                                                    256
         AG_DX = PRES_FEXD                                                256
         AG_DY = PRES_FEYD                                                256
         AG_DZ = PRES_FEZD                                                256
         AG_TITLE = 'BOX'                                                 256
         AG_EXNAME = 'SHAPE'                                              256
         AG_PARLIST = 'DX_DY_DZ'                                          256
         CALL AXSHAPE                                                     256
C    *                                                                    256
5051  CALL AGSPOPB                                                        257
      GO TO BLOCK_HSFE, (5050 ,5060)                                      257
C End of Local Procedure                                                  257
      WRITE(LOUT,5350)'===>>  [End AgML Module HcalGeo]  <<==='           257
5350  FORMAT(A32,/,/)                                                     257
C MODULE HCALGEO                                                          259
      END                                                                 259
      SUBROUTINE HCALHIT(JJ,HIT)                                          260
      IMPLICIT NONE                                                       261
C - Dynamic core for the GEANT data (v3.15)                               261
      INTEGER KWBANK,KWWORK,NZEBRA,IXSTOR,IXDIV,IXCONS,LMAIN,LR1,JDIGI,   261
     *JDRAW, JHEAD,JHITS,JKINE,JMATE,JPART,JROTM,JRUNG,JSET,JSTAK,        261
     *JGSTAT,JTMED, JTRACK,JVERTX,JVOLUM,JXYZ,JGPAR,JGPAR2,JSKLT,IQ,IWS,  261
     *LQ,JCG                                                              261
      REAL ZVERSN,GVERSN,FENDQ,WS,Q                                       261
      PARAMETER (KWBANK=69000)                                            261
      PARAMETER (KWWORK=5200)                                             261
      COMMON /GCBANK/ NZEBRA,GVERSN,ZVERSN,IXSTOR,IXDIV,IXCONS,           261
     *FENDQ(16),LMAIN,LR1,WS(KWBANK)                                      261
      COMMON /GCLINK/ JDIGI,JDRAW, JHEAD,JHITS ,JKINE, JMATE ,JPART,      261
     *JROTM,JRUNG, JSET, JSTAK,JGSTAT,JTMED,JTRACK,JVERTX,JVOLUM,JXYZ,    261
     *JGPAR,JGPAR2,JSKLT                                                  261
      DIMENSION IQ(2),Q(2),LQ(8000),IWS(2)                                261
      EQUIVALENCE (Q(1),IQ(1),LQ(9)),(LQ(1),LMAIN),(IWS(1),WS(1)),(JCG,   261
     *JGSTAT)                                                             261
C - Natural constants (or considered as such)                             261
      REAL PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS,EMMU,PMASS,AVO   261
      COMMON/GCONST/ PI,TWOPI,PIBY2,DEGRAD,RADDEG,CLIGHT,BIG,EMASS        261
      COMMON/GCONSX/ EMMU,PMASS,AVO                                       261
C - GEANT description of logical units                                    261
      COMMON/GCUNIT/ LIN,LOUT,NUNITS,LUNITS(5)                            261
      INTEGER LIN,LOUT,NUNITS,LUNITS                                      261
      COMMON/GCMAIL/ CHMAIL                                               261
      CHARACTER*132 CHMAIL                                                261
C - parameters of current tracking medium                                 261
      INTEGER NUMED,NATMED,ISVOL,IFIELD,IUPD,ISTPAR,NUMOLD                261
      REAL FIELDM,TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,CFIELD,PREC            261
      COMMON/GCTMED/ NUMED,NATMED(5),ISVOL,IFIELD,FIELDM,TMAXFD,STEMAX,   261
     *DEEMAX,EPSIL,STMIN,CFIELD,PREC,IUPD,ISTPAR,NUMOLD                   261
      REAL THRIND,PMIN,DP,DNDL                                            261
      INTEGER JMIN,ITCKOV,IMCKOV,NPCKOV                                   261
      COMMON/GCTLIT/ THRIND,PMIN,DP,DNDL,JMIN,ITCKOV,IMCKOV,NPCKOV        261
C - track parameters at current point                                     261
      INTEGER NMEC,LMEC,NAMEC,NSTEP,MAXNST,IGNEXT,INWVOL,ISTOP,IGAUTO,    261
     *IEKBIN,ILOSL,IMULL,INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY               261
      REAL VECT,GETOT,GEKIN,VOUT,DESTEP,DESTEL,SAFETY, SLENG,STEP,SNEXT,  261
     *SFIELD,TOFG,GEKRAT,UPWGHT                                           261
      COMMON/GCTRAK/ VECT(7),GETOT,GEKIN,VOUT(7),NMEC,LMEC(30),           261
     *NAMEC(30),NSTEP, MAXNST,DESTEP,DESTEL,SAFETY,SLENG,STEP,SNEXT,      261
     *SFIELD,TOFG, GEKRAT,UPWGHT,IGNEXT,INWVOL,ISTOP,IGAUTO,IEKBIN,       261
     *ILOSL,IMULL, INGOTO,NLDOWN,NLEVIN,NLVSAV,ISTORY                     261
C - Kinematics of current track                                           261
      INTEGER IKINE,ITRA,ISTAK,IVERT,IPART,ITRTYP,NAPART,IPAOLD           261
      REAL PKINE,AMASS,CHARGE,TLIFE,VERT,PVERT                            261
      COMMON/GCKINE/ IKINE,PKINE(10),ITRA,ISTAK,IVERT,IPART,ITRTYP,       261
     *NAPART(5), AMASS,CHARGE,TLIFE,VERT(3),PVERT(4),IPAOLD               261
C - Identification of the current sensitive detector                      261
      INTEGER IHSET,IHDET,ISET,IDET,IDTYPE,NVNAME,NUMBV                   261
      COMMON /GCSETS/ IHSET,IHDET,ISET,IDET,IDTYPE,NVNAME,NUMBV(20)       261
C Connection between AgGSTEP, AgGHIT and a user step routine              261
      REAL VECT0, VLOC0, VLOC, XLOC ,ASTEP,ADESTEP,HITS                   261
      INTEGER IHAVEMORE                                                   261
      COMMON /AGCSTEP/ VECT0(7),VLOC0(7),VLOC(7),XLOC(7),ASTEP,ADESTEP    261
      COMMON /AGCSTEPH/ HITS(15)                                          261
      COMMON /AGCSTEPI/ IHAVEMORE                                         261
C - Multi-level current volume description                                261
      INTEGER NLEVEL,NAMES,NUMBER,LVOLUM,LINDEX,INFROM,NLEVMX,NLDEV,      261
     *LINMX                                                               261
      REAL GTRAN,GRMAT,GONLY,GLX                                          261
      COMMON /GCVOLU/ NLEVEL,NAMES(15),NUMBER(15),LVOLUM(15),LINDEX(15),  261
     *INFROM, NLEVMX,NLDEV(15),LINMX(15),GTRAN(3,15),GRMAT(10,15),        261
     *GONLY(15),GLX(3)                                                    261
      INTEGER JJ                                                          261
      REAL HIT,ATT,ZHCAL                                                  263
      CHARACTER*4 NAME                                                    264
      CALL UHTOC(NAMES(NLEVEL),4,NAME,4)                                  265
C WRITE (*,*) NAME, NLEVEL                                                266
C ACTIVATE FOLLOWING FOR FIBER ONLY                                       267
C IF(NLEVEL.EQ.8) THEN ! NLEVEL == 8 IS FIBER                             268
C ALSO... HARDCODED POSITIONS...                                          269
      ZHCAL=718.9+117-VECT(3)                                             269
      ATT=EXP(-ZHCAL/220)                                                 270
C WRITE (*,*) NAME, NLEVEL,' z=',ZHCAL,' deStep = ', DESTEP, ADESTEP, AT  271
C T                                                                       271
      HIT = DESTEP * ATT                                                  272
C ENDIF                                                                   273
C DESTEP = DESTEP * ATT ! HUH??                                           274
C WRITE (*,*) DESTEP                                                      275
      RETURN                                                              276
      END                                                                 277
