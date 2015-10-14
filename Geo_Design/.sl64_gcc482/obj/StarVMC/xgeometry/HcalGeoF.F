C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE HCALGEOF                                                 1
C HCALGEOF HCAL AT FERMITBF                                               1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'HCALGEOF'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'HCAL AT FERMITBF'/                                   1
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
      INTEGER BLOCK_HCMO                                                  18
      INTEGER BLOCK_HTWR                                                  18
      INTEGER BLOCK_HCEL                                                  18
      INTEGER BLOCK_HCES                                                  18
      INTEGER BLOCK_HCLX                                                  18
      INTEGER BLOCK_HCLY                                                  18
      INTEGER BLOCK_HCSX                                                  18
      INTEGER BLOCK_HCSY                                                  18
      INTEGER BLOCK_HFIB                                                  18
      INTEGER BLOCK_HFIC                                                  18
      INTEGER BLOCK_BBCF                                                  18
      INTEGER BLOCK_BBCB                                                  18
      INTEGER BLOCK_LEDG                                                  18
      INTEGER *4 I,J , M,N , P,Q                                          19
      REAL *4 XTEMP, YTEMP                                                21
C -                                                                       23
      REAL MCDT_VERSION                                                   23
      EQUIVALENCE (BANK_MCDT(3),MCDT_VERSION,RE_MCDT_VERSION),            23
     *(NAMES_MCDT(1),NMCDT_VERSION)                                       23
      EQUIVALENCE (COMMEN_MCDT(1),CMCDT_VERSION),(MAP_MCDT(1,1),          23
     *MMCDT_VERSION)                                                      23
      CHARACTER CMCDT_VERSION*40/' '/                                     23
      CHARACTER NMCDT_VERSION*12/'R.VERSION'/                             23
      INTEGER MMCDT_VERSION(2)/1,1/                                       23
      REAL RE_MCDT_VERSION                                                23
C -                                                                       23
      REAL MCDT_X                                                         23
      EQUIVALENCE (BANK_MCDT(4),MCDT_X,RE_MCDT_X),(NAMES_MCDT(2),         23
     *NMCDT_X)                                                            23
      EQUIVALENCE (COMMEN_MCDT(2),CMCDT_X),(MAP_MCDT(1,2),MMCDT_X)        23
      CHARACTER CMCDT_X*40/' '/                                           23
      CHARACTER NMCDT_X*12/'R.X'/                                         23
      INTEGER MMCDT_X(2)/1,1/                                             23
      REAL RE_MCDT_X                                                      23
C -                                                                       23
      REAL MCDT_Y                                                         23
      EQUIVALENCE (BANK_MCDT(5),MCDT_Y,RE_MCDT_Y),(NAMES_MCDT(3),         23
     *NMCDT_Y)                                                            23
      EQUIVALENCE (COMMEN_MCDT(3),CMCDT_Y),(MAP_MCDT(1,3),MMCDT_Y)        23
      CHARACTER CMCDT_Y*40/' '/                                           23
      CHARACTER NMCDT_Y*12/'R.Y'/                                         23
      INTEGER MMCDT_Y(2)/1,1/                                             23
      REAL RE_MCDT_Y                                                      23
C -                                                                       23
      REAL MCDT_Z                                                         23
      EQUIVALENCE (BANK_MCDT(6),MCDT_Z,RE_MCDT_Z),(NAMES_MCDT(4),         23
     *NMCDT_Z)                                                            23
      EQUIVALENCE (COMMEN_MCDT(4),CMCDT_Z),(MAP_MCDT(1,4),MMCDT_Z)        23
      CHARACTER CMCDT_Z*40/' '/                                           23
      CHARACTER NMCDT_Z*12/'R.Z'/                                         23
      INTEGER MMCDT_Z(2)/1,1/                                             23
      REAL RE_MCDT_Z                                                      23
C -                                                                       23
      REAL MCDT_DX                                                        23
      EQUIVALENCE (BANK_MCDT(7),MCDT_DX,RE_MCDT_DX),(NAMES_MCDT(5),       23
     *NMCDT_DX)                                                           23
      EQUIVALENCE (COMMEN_MCDT(5),CMCDT_DX),(MAP_MCDT(1,5),MMCDT_DX)      23
      CHARACTER CMCDT_DX*40/' '/                                          23
      CHARACTER NMCDT_DX*12/'R.DX'/                                       23
      INTEGER MMCDT_DX(2)/1,1/                                            23
      REAL RE_MCDT_DX                                                     23
C -                                                                       23
      REAL MCDT_DY                                                        23
      EQUIVALENCE (BANK_MCDT(8),MCDT_DY,RE_MCDT_DY),(NAMES_MCDT(6),       23
     *NMCDT_DY)                                                           23
      EQUIVALENCE (COMMEN_MCDT(6),CMCDT_DY),(MAP_MCDT(1,6),MMCDT_DY)      23
      CHARACTER CMCDT_DY*40/' '/                                          23
      CHARACTER NMCDT_DY*12/'R.DY'/                                       23
      INTEGER MMCDT_DY(2)/1,1/                                            23
      REAL RE_MCDT_DY                                                     23
C -                                                                       23
      REAL MCDT_DZ                                                        23
      EQUIVALENCE (BANK_MCDT(9),MCDT_DZ,RE_MCDT_DZ),(NAMES_MCDT(7),       23
     *NMCDT_DZ)                                                           23
      EQUIVALENCE (COMMEN_MCDT(7),CMCDT_DZ),(MAP_MCDT(1,7),MMCDT_DZ)      23
      CHARACTER CMCDT_DZ*40/' '/                                          23
      CHARACTER NMCDT_DZ*12/'R.DZ'/                                       23
      INTEGER MMCDT_DZ(2)/1,1/                                            23
      REAL RE_MCDT_DZ                                                     23
C -                                                                       23
      INTEGER LEN_MCDT,LLEN_MCDT,LINK_MCDT                                23
      PARAMETER (LEN_MCDT=9)                                              23
      PARAMETER (LLEN_MCDT=7)                                             23
      INTEGER FLAG_MCDT/0/,MAP_MCDT(2,LLEN_MCDT),IDNUM_MCDT(10)           23
      CHARACTER NAMES_MCDT(LLEN_MCDT)*12,COMMEN_MCDT(LLEN_MCDT)*40        23
      DATA IDNUM_MCDT/10*0/                                               23
      REAL BANK_MCDT(LEN_MCDT)                                            23
      CHARACTER*60 BPATH_MCDT/' '/                                        23
      SAVE FLAG_MCDT,MAP_MCDT,IDNUM_MCDT,NAMES_MCDT,COMMEN_MCDT,          23
     *BPATH_MCDT                                                          23
      DATA LINK_MCDT/0/,BANK_MCDT/LEN_MCDT*0/                             23
C *                                                                       23
C -                                                                       24
      REAL TOWR_VERSION                                                   24
      EQUIVALENCE (BANK_TOWR(3),TOWR_VERSION,RE_TOWR_VERSION),            24
     *(NAMES_TOWR(1),NTOWR_VERSION)                                       24
      EQUIVALENCE (COMMEN_TOWR(1),CTOWR_VERSION),(MAP_TOWR(1,1),          24
     *MTOWR_VERSION)                                                      24
      CHARACTER CTOWR_VERSION*40/' '/                                     24
      CHARACTER NTOWR_VERSION*12/'R.VERSION'/                             24
      INTEGER MTOWR_VERSION(2)/1,1/                                       24
      REAL RE_TOWR_VERSION                                                24
C -                                                                       24
      REAL TOWR_DX                                                        24
      EQUIVALENCE (BANK_TOWR(4),TOWR_DX,RE_TOWR_DX),(NAMES_TOWR(2),       24
     *NTOWR_DX)                                                           24
      EQUIVALENCE (COMMEN_TOWR(2),CTOWR_DX),(MAP_TOWR(1,2),MTOWR_DX)      24
      CHARACTER CTOWR_DX*40/' '/                                          24
      CHARACTER NTOWR_DX*12/'R.DX'/                                       24
      INTEGER MTOWR_DX(2)/1,1/                                            24
      REAL RE_TOWR_DX                                                     24
C -                                                                       24
      REAL TOWR_DY                                                        24
      EQUIVALENCE (BANK_TOWR(5),TOWR_DY,RE_TOWR_DY),(NAMES_TOWR(3),       24
     *NTOWR_DY)                                                           24
      EQUIVALENCE (COMMEN_TOWR(3),CTOWR_DY),(MAP_TOWR(1,3),MTOWR_DY)      24
      CHARACTER CTOWR_DY*40/' '/                                          24
      CHARACTER NTOWR_DY*12/'R.DY'/                                       24
      INTEGER MTOWR_DY(2)/1,1/                                            24
      REAL RE_TOWR_DY                                                     24
C -                                                                       24
      REAL TOWR_DZ                                                        24
      EQUIVALENCE (BANK_TOWR(6),TOWR_DZ,RE_TOWR_DZ),(NAMES_TOWR(4),       24
     *NTOWR_DZ)                                                           24
      EQUIVALENCE (COMMEN_TOWR(4),CTOWR_DZ),(MAP_TOWR(1,4),MTOWR_DZ)      24
      CHARACTER CTOWR_DZ*40/' '/                                          24
      CHARACTER NTOWR_DZ*12/'R.DZ'/                                       24
      INTEGER MTOWR_DZ(2)/1,1/                                            24
      REAL RE_TOWR_DZ                                                     24
C -                                                                       24
      REAL TOWR_NTOWERX                                                   24
      EQUIVALENCE (BANK_TOWR(7),TOWR_NTOWERX,RE_TOWR_NTOWERX),            24
     *(NAMES_TOWR(5),NTOWR_NTOWERX)                                       24
      EQUIVALENCE (COMMEN_TOWR(5),CTOWR_NTOWERX),(MAP_TOWR(1,5),          24
     *MTOWR_NTOWERX)                                                      24
      CHARACTER CTOWR_NTOWERX*40/' '/                                     24
      CHARACTER NTOWR_NTOWERX*12/'R.NTOWERX'/                             24
      INTEGER MTOWR_NTOWERX(2)/1,1/                                       24
      REAL RE_TOWR_NTOWERX                                                24
C -                                                                       24
      REAL TOWR_NTOWERY                                                   24
      EQUIVALENCE (BANK_TOWR(8),TOWR_NTOWERY,RE_TOWR_NTOWERY),            24
     *(NAMES_TOWR(6),NTOWR_NTOWERY)                                       24
      EQUIVALENCE (COMMEN_TOWR(6),CTOWR_NTOWERY),(MAP_TOWR(1,6),          24
     *MTOWR_NTOWERY)                                                      24
      CHARACTER CTOWR_NTOWERY*40/' '/                                     24
      CHARACTER NTOWR_NTOWERY*12/'R.NTOWERY'/                             24
      INTEGER MTOWR_NTOWERY(2)/1,1/                                       24
      REAL RE_TOWR_NTOWERY                                                24
C -                                                                       24
      INTEGER LEN_TOWR,LLEN_TOWR,LINK_TOWR                                24
      PARAMETER (LEN_TOWR=8)                                              24
      PARAMETER (LLEN_TOWR=6)                                             24
      INTEGER FLAG_TOWR/0/,MAP_TOWR(2,LLEN_TOWR),IDNUM_TOWR(10)           24
      CHARACTER NAMES_TOWR(LLEN_TOWR)*12,COMMEN_TOWR(LLEN_TOWR)*40        24
      DATA IDNUM_TOWR/10*0/                                               24
      REAL BANK_TOWR(LEN_TOWR)                                            24
      CHARACTER*60 BPATH_TOWR/' '/                                        24
      SAVE FLAG_TOWR,MAP_TOWR,IDNUM_TOWR,NAMES_TOWR,COMMEN_TOWR,          24
     *BPATH_TOWR                                                          24
      DATA LINK_TOWR/0/,BANK_TOWR/LEN_TOWR*0/                             24
C *                                                                       24
C -                                                                       26
      REAL CELL_VERSION                                                   26
      EQUIVALENCE (BANK_CELL(3),CELL_VERSION,RE_CELL_VERSION),            26
     *(NAMES_CELL(1),NCELL_VERSION)                                       26
      EQUIVALENCE (COMMEN_CELL(1),CCELL_VERSION),(MAP_CELL(1,1),          26
     *MCELL_VERSION)                                                      26
      CHARACTER CCELL_VERSION*40/' '/                                     26
      CHARACTER NCELL_VERSION*12/'R.VERSION'/                             26
      INTEGER MCELL_VERSION(2)/1,1/                                       26
      REAL RE_CELL_VERSION                                                26
C -                                                                       26
      REAL CELL_DX                                                        26
      EQUIVALENCE (BANK_CELL(4),CELL_DX,RE_CELL_DX),(NAMES_CELL(2),       26
     *NCELL_DX)                                                           26
      EQUIVALENCE (COMMEN_CELL(2),CCELL_DX),(MAP_CELL(1,2),MCELL_DX)      26
      CHARACTER CCELL_DX*40/' '/                                          26
      CHARACTER NCELL_DX*12/'R.DX'/                                       26
      INTEGER MCELL_DX(2)/1,1/                                            26
      REAL RE_CELL_DX                                                     26
C -                                                                       26
      REAL CELL_DY                                                        26
      EQUIVALENCE (BANK_CELL(5),CELL_DY,RE_CELL_DY),(NAMES_CELL(3),       26
     *NCELL_DY)                                                           26
      EQUIVALENCE (COMMEN_CELL(3),CCELL_DY),(MAP_CELL(1,3),MCELL_DY)      26
      CHARACTER CCELL_DY*40/' '/                                          26
      CHARACTER NCELL_DY*12/'R.DY'/                                       26
      INTEGER MCELL_DY(2)/1,1/                                            26
      REAL RE_CELL_DY                                                     26
C -                                                                       26
      REAL CELL_DZ                                                        26
      EQUIVALENCE (BANK_CELL(6),CELL_DZ,RE_CELL_DZ),(NAMES_CELL(4),       26
     *NCELL_DZ)                                                           26
      EQUIVALENCE (COMMEN_CELL(4),CCELL_DZ),(MAP_CELL(1,4),MCELL_DZ)      26
      CHARACTER CCELL_DZ*40/' '/                                          26
      CHARACTER NCELL_DZ*12/'R.DZ'/                                       26
      INTEGER MCELL_DZ(2)/1,1/                                            26
      REAL RE_CELL_DZ                                                     26
C -                                                                       26
      REAL CELL_NCELLX                                                    26
      EQUIVALENCE (BANK_CELL(7),CELL_NCELLX,RE_CELL_NCELLX),              26
     *(NAMES_CELL(5),NCELL_NCELLX)                                        26
      EQUIVALENCE (COMMEN_CELL(5),CCELL_NCELLX),(MAP_CELL(1,5),           26
     *MCELL_NCELLX)                                                       26
      CHARACTER CCELL_NCELLX*40/' '/                                      26
      CHARACTER NCELL_NCELLX*12/'R.NCELLX'/                               26
      INTEGER MCELL_NCELLX(2)/1,1/                                        26
      REAL RE_CELL_NCELLX                                                 26
C -                                                                       26
      REAL CELL_NCELLY                                                    26
      EQUIVALENCE (BANK_CELL(8),CELL_NCELLY,RE_CELL_NCELLY),              26
     *(NAMES_CELL(6),NCELL_NCELLY)                                        26
      EQUIVALENCE (COMMEN_CELL(6),CCELL_NCELLY),(MAP_CELL(1,6),           26
     *MCELL_NCELLY)                                                       26
      CHARACTER CCELL_NCELLY*40/' '/                                      26
      CHARACTER NCELL_NCELLY*12/'R.NCELLY'/                               26
      INTEGER MCELL_NCELLY(2)/1,1/                                        26
      REAL RE_CELL_NCELLY                                                 26
C -                                                                       26
      REAL CELL_NFIBERXL                                                  26
      EQUIVALENCE (BANK_CELL(9),CELL_NFIBERXL,RE_CELL_NFIBERXL),          26
     *(NAMES_CELL(7),NCELL_NFIBERXL)                                      26
      EQUIVALENCE (COMMEN_CELL(7),CCELL_NFIBERXL),(MAP_CELL(1,7),         26
     *MCELL_NFIBERXL)                                                     26
      CHARACTER CCELL_NFIBERXL*40/' '/                                    26
      CHARACTER NCELL_NFIBERXL*12/'R.NFIBERXL'/                           26
      INTEGER MCELL_NFIBERXL(2)/1,1/                                      26
      REAL RE_CELL_NFIBERXL                                               26
C -                                                                       26
      REAL CELL_NFIBERYL                                                  26
      EQUIVALENCE (BANK_CELL(10),CELL_NFIBERYL,RE_CELL_NFIBERYL),         26
     *(NAMES_CELL(8),NCELL_NFIBERYL)                                      26
      EQUIVALENCE (COMMEN_CELL(8),CCELL_NFIBERYL),(MAP_CELL(1,8),         26
     *MCELL_NFIBERYL)                                                     26
      CHARACTER CCELL_NFIBERYL*40/' '/                                    26
      CHARACTER NCELL_NFIBERYL*12/'R.NFIBERYL'/                           26
      INTEGER MCELL_NFIBERYL(2)/1,1/                                      26
      REAL RE_CELL_NFIBERYL                                               26
C -                                                                       26
      REAL CELL_NFIBERXS                                                  26
      EQUIVALENCE (BANK_CELL(11),CELL_NFIBERXS,RE_CELL_NFIBERXS),         26
     *(NAMES_CELL(9),NCELL_NFIBERXS)                                      26
      EQUIVALENCE (COMMEN_CELL(9),CCELL_NFIBERXS),(MAP_CELL(1,9),         26
     *MCELL_NFIBERXS)                                                     26
      CHARACTER CCELL_NFIBERXS*40/' '/                                    26
      CHARACTER NCELL_NFIBERXS*12/'R.NFIBERXS'/                           26
      INTEGER MCELL_NFIBERXS(2)/1,1/                                      26
      REAL RE_CELL_NFIBERXS                                               26
C -                                                                       26
      REAL CELL_NFIBERYS                                                  26
      EQUIVALENCE (BANK_CELL(12),CELL_NFIBERYS,RE_CELL_NFIBERYS),         26
     *(NAMES_CELL(10),NCELL_NFIBERYS)                                     26
      EQUIVALENCE (COMMEN_CELL(10),CCELL_NFIBERYS),(MAP_CELL(1,10),       26
     *MCELL_NFIBERYS)                                                     26
      CHARACTER CCELL_NFIBERYS*40/' '/                                    26
      CHARACTER NCELL_NFIBERYS*12/'R.NFIBERYS'/                           26
      INTEGER MCELL_NFIBERYS(2)/1,1/                                      26
      REAL RE_CELL_NFIBERYS                                               26
C -                                                                       26
      INTEGER LEN_CELL,LLEN_CELL,LINK_CELL                                26
      PARAMETER (LEN_CELL=12)                                             26
      PARAMETER (LLEN_CELL=10)                                            26
      INTEGER FLAG_CELL/0/,MAP_CELL(2,LLEN_CELL),IDNUM_CELL(10)           26
      CHARACTER NAMES_CELL(LLEN_CELL)*12,COMMEN_CELL(LLEN_CELL)*40        26
      DATA IDNUM_CELL/10*0/                                               26
      REAL BANK_CELL(LEN_CELL)                                            26
      CHARACTER*60 BPATH_CELL/' '/                                        26
      SAVE FLAG_CELL,MAP_CELL,IDNUM_CELL,NAMES_CELL,COMMEN_CELL,          26
     *BPATH_CELL                                                          26
      DATA LINK_CELL/0/,BANK_CELL/LEN_CELL*0/                             26
C *                                                                       26
C -                                                                       28
      REAL FIBR_VERSION                                                   28
      EQUIVALENCE (BANK_FIBR(3),FIBR_VERSION,RE_FIBR_VERSION),            28
     *(NAMES_FIBR(1),NFIBR_VERSION)                                       28
      EQUIVALENCE (COMMEN_FIBR(1),CFIBR_VERSION),(MAP_FIBR(1,1),          28
     *MFIBR_VERSION)                                                      28
      CHARACTER CFIBR_VERSION*40/' '/                                     28
      CHARACTER NFIBR_VERSION*12/'R.VERSION'/                             28
      INTEGER MFIBR_VERSION(2)/1,1/                                       28
      REAL RE_FIBR_VERSION                                                28
C -                                                                       28
      REAL FIBR_RMIN                                                      28
      EQUIVALENCE (BANK_FIBR(4),FIBR_RMIN,RE_FIBR_RMIN),(NAMES_FIBR(2),   28
     *NFIBR_RMIN)                                                         28
      EQUIVALENCE (COMMEN_FIBR(2),CFIBR_RMIN),(MAP_FIBR(1,2),             28
     *MFIBR_RMIN)                                                         28
      CHARACTER CFIBR_RMIN*40/' '/                                        28
      CHARACTER NFIBR_RMIN*12/'R.RMIN'/                                   28
      INTEGER MFIBR_RMIN(2)/1,1/                                          28
      REAL RE_FIBR_RMIN                                                   28
C -                                                                       28
      REAL FIBR_RMAX                                                      28
      EQUIVALENCE (BANK_FIBR(5),FIBR_RMAX,RE_FIBR_RMAX),(NAMES_FIBR(3),   28
     *NFIBR_RMAX)                                                         28
      EQUIVALENCE (COMMEN_FIBR(3),CFIBR_RMAX),(MAP_FIBR(1,3),             28
     *MFIBR_RMAX)                                                         28
      CHARACTER CFIBR_RMAX*40/' '/                                        28
      CHARACTER NFIBR_RMAX*12/'R.RMAX'/                                   28
      INTEGER MFIBR_RMAX(2)/1,1/                                          28
      REAL RE_FIBR_RMAX                                                   28
C -                                                                       28
      REAL FIBR_DZ                                                        28
      EQUIVALENCE (BANK_FIBR(6),FIBR_DZ,RE_FIBR_DZ),(NAMES_FIBR(4),       28
     *NFIBR_DZ)                                                           28
      EQUIVALENCE (COMMEN_FIBR(4),CFIBR_DZ),(MAP_FIBR(1,4),MFIBR_DZ)      28
      CHARACTER CFIBR_DZ*40/' '/                                          28
      CHARACTER NFIBR_DZ*12/'R.DZ'/                                       28
      INTEGER MFIBR_DZ(2)/1,1/                                            28
      REAL RE_FIBR_DZ                                                     28
C -                                                                       28
      REAL FIBR_STARTX1                                                   28
      EQUIVALENCE (BANK_FIBR(7),FIBR_STARTX1,RE_FIBR_STARTX1),            28
     *(NAMES_FIBR(5),NFIBR_STARTX1)                                       28
      EQUIVALENCE (COMMEN_FIBR(5),CFIBR_STARTX1),(MAP_FIBR(1,5),          28
     *MFIBR_STARTX1)                                                      28
      CHARACTER CFIBR_STARTX1*40/' '/                                     28
      CHARACTER NFIBR_STARTX1*12/'R.STARTX1'/                             28
      INTEGER MFIBR_STARTX1(2)/1,1/                                       28
      REAL RE_FIBR_STARTX1                                                28
C -                                                                       28
      REAL FIBR_STARTY1                                                   28
      EQUIVALENCE (BANK_FIBR(8),FIBR_STARTY1,RE_FIBR_STARTY1),            28
     *(NAMES_FIBR(6),NFIBR_STARTY1)                                       28
      EQUIVALENCE (COMMEN_FIBR(6),CFIBR_STARTY1),(MAP_FIBR(1,6),          28
     *MFIBR_STARTY1)                                                      28
      CHARACTER CFIBR_STARTY1*40/' '/                                     28
      CHARACTER NFIBR_STARTY1*12/'R.STARTY1'/                             28
      INTEGER MFIBR_STARTY1(2)/1,1/                                       28
      REAL RE_FIBR_STARTY1                                                28
C -                                                                       28
      REAL FIBR_STARTX2                                                   28
      EQUIVALENCE (BANK_FIBR(9),FIBR_STARTX2,RE_FIBR_STARTX2),            28
     *(NAMES_FIBR(7),NFIBR_STARTX2)                                       28
      EQUIVALENCE (COMMEN_FIBR(7),CFIBR_STARTX2),(MAP_FIBR(1,7),          28
     *MFIBR_STARTX2)                                                      28
      CHARACTER CFIBR_STARTX2*40/' '/                                     28
      CHARACTER NFIBR_STARTX2*12/'R.STARTX2'/                             28
      INTEGER MFIBR_STARTX2(2)/1,1/                                       28
      REAL RE_FIBR_STARTX2                                                28
C -                                                                       28
      REAL FIBR_STARTY2                                                   28
      EQUIVALENCE (BANK_FIBR(10),FIBR_STARTY2,RE_FIBR_STARTY2),           28
     *(NAMES_FIBR(8),NFIBR_STARTY2)                                       28
      EQUIVALENCE (COMMEN_FIBR(8),CFIBR_STARTY2),(MAP_FIBR(1,8),          28
     *MFIBR_STARTY2)                                                      28
      CHARACTER CFIBR_STARTY2*40/' '/                                     28
      CHARACTER NFIBR_STARTY2*12/'R.STARTY2'/                             28
      INTEGER MFIBR_STARTY2(2)/1,1/                                       28
      REAL RE_FIBR_STARTY2                                                28
C -                                                                       28
      REAL FIBR_STEPX                                                     28
      EQUIVALENCE (BANK_FIBR(11),FIBR_STEPX,RE_FIBR_STEPX),               28
     *(NAMES_FIBR(9),NFIBR_STEPX)                                         28
      EQUIVALENCE (COMMEN_FIBR(9),CFIBR_STEPX),(MAP_FIBR(1,9),            28
     *MFIBR_STEPX)                                                        28
      CHARACTER CFIBR_STEPX*40/' '/                                       28
      CHARACTER NFIBR_STEPX*12/'R.STEPX'/                                 28
      INTEGER MFIBR_STEPX(2)/1,1/                                         28
      REAL RE_FIBR_STEPX                                                  28
C -                                                                       28
      REAL FIBR_STEPY                                                     28
      EQUIVALENCE (BANK_FIBR(12),FIBR_STEPY,RE_FIBR_STEPY),               28
     *(NAMES_FIBR(10),NFIBR_STEPY)                                        28
      EQUIVALENCE (COMMEN_FIBR(10),CFIBR_STEPY),(MAP_FIBR(1,10),          28
     *MFIBR_STEPY)                                                        28
      CHARACTER CFIBR_STEPY*40/' '/                                       28
      CHARACTER NFIBR_STEPY*12/'R.STEPY'/                                 28
      INTEGER MFIBR_STEPY(2)/1,1/                                         28
      REAL RE_FIBR_STEPY                                                  28
C -                                                                       28
      INTEGER LEN_FIBR,LLEN_FIBR,LINK_FIBR                                28
      PARAMETER (LEN_FIBR=12)                                             28
      PARAMETER (LLEN_FIBR=10)                                            28
      INTEGER FLAG_FIBR/0/,MAP_FIBR(2,LLEN_FIBR),IDNUM_FIBR(10)           28
      CHARACTER NAMES_FIBR(LLEN_FIBR)*12,COMMEN_FIBR(LLEN_FIBR)*40        28
      DATA IDNUM_FIBR/10*0/                                               28
      REAL BANK_FIBR(LEN_FIBR)                                            28
      CHARACTER*60 BPATH_FIBR/' '/                                        28
      SAVE FLAG_FIBR,MAP_FIBR,IDNUM_FIBR,NAMES_FIBR,COMMEN_FIBR,          28
     *BPATH_FIBR                                                          28
      DATA LINK_FIBR/0/,BANK_FIBR/LEN_FIBR*0/                             28
C *                                                                       28
C -                                                                       29
      REAL LGLA_VERSION                                                   29
      EQUIVALENCE (BANK_LGLA(3),LGLA_VERSION,RE_LGLA_VERSION),            29
     *(NAMES_LGLA(1),NLGLA_VERSION)                                       29
      EQUIVALENCE (COMMEN_LGLA(1),CLGLA_VERSION),(MAP_LGLA(1,1),          29
     *MLGLA_VERSION)                                                      29
      CHARACTER CLGLA_VERSION*40/' '/                                     29
      CHARACTER NLGLA_VERSION*12/'R.VERSION'/                             29
      INTEGER MLGLA_VERSION(2)/1,1/                                       29
      REAL RE_LGLA_VERSION                                                29
C -                                                                       29
      REAL LGLA_X                                                         29
      EQUIVALENCE (BANK_LGLA(4),LGLA_X,RE_LGLA_X),(NAMES_LGLA(2),         29
     *NLGLA_X)                                                            29
      EQUIVALENCE (COMMEN_LGLA(2),CLGLA_X),(MAP_LGLA(1,2),MLGLA_X)        29
      CHARACTER CLGLA_X*40/' '/                                           29
      CHARACTER NLGLA_X*12/'R.X'/                                         29
      INTEGER MLGLA_X(2)/1,1/                                             29
      REAL RE_LGLA_X                                                      29
C -                                                                       29
      REAL LGLA_Y                                                         29
      EQUIVALENCE (BANK_LGLA(5),LGLA_Y,RE_LGLA_Y),(NAMES_LGLA(3),         29
     *NLGLA_Y)                                                            29
      EQUIVALENCE (COMMEN_LGLA(3),CLGLA_Y),(MAP_LGLA(1,3),MLGLA_Y)        29
      CHARACTER CLGLA_Y*40/' '/                                           29
      CHARACTER NLGLA_Y*12/'R.Y'/                                         29
      INTEGER MLGLA_Y(2)/1,1/                                             29
      REAL RE_LGLA_Y                                                      29
C -                                                                       29
      REAL LGLA_Z                                                         29
      EQUIVALENCE (BANK_LGLA(6),LGLA_Z,RE_LGLA_Z),(NAMES_LGLA(4),         29
     *NLGLA_Z)                                                            29
      EQUIVALENCE (COMMEN_LGLA(4),CLGLA_Z),(MAP_LGLA(1,4),MLGLA_Z)        29
      CHARACTER CLGLA_Z*40/' '/                                           29
      CHARACTER NLGLA_Z*12/'R.Z'/                                         29
      INTEGER MLGLA_Z(2)/1,1/                                             29
      REAL RE_LGLA_Z                                                      29
C -                                                                       29
      REAL LGLA_DX                                                        29
      EQUIVALENCE (BANK_LGLA(7),LGLA_DX,RE_LGLA_DX),(NAMES_LGLA(5),       29
     *NLGLA_DX)                                                           29
      EQUIVALENCE (COMMEN_LGLA(5),CLGLA_DX),(MAP_LGLA(1,5),MLGLA_DX)      29
      CHARACTER CLGLA_DX*40/' '/                                          29
      CHARACTER NLGLA_DX*12/'R.DX'/                                       29
      INTEGER MLGLA_DX(2)/1,1/                                            29
      REAL RE_LGLA_DX                                                     29
C -                                                                       29
      REAL LGLA_DY                                                        29
      EQUIVALENCE (BANK_LGLA(8),LGLA_DY,RE_LGLA_DY),(NAMES_LGLA(6),       29
     *NLGLA_DY)                                                           29
      EQUIVALENCE (COMMEN_LGLA(6),CLGLA_DY),(MAP_LGLA(1,6),MLGLA_DY)      29
      CHARACTER CLGLA_DY*40/' '/                                          29
      CHARACTER NLGLA_DY*12/'R.DY'/                                       29
      INTEGER MLGLA_DY(2)/1,1/                                            29
      REAL RE_LGLA_DY                                                     29
C -                                                                       29
      REAL LGLA_DZ                                                        29
      EQUIVALENCE (BANK_LGLA(9),LGLA_DZ,RE_LGLA_DZ),(NAMES_LGLA(7),       29
     *NLGLA_DZ)                                                           29
      EQUIVALENCE (COMMEN_LGLA(7),CLGLA_DZ),(MAP_LGLA(1,7),MLGLA_DZ)      29
      CHARACTER CLGLA_DZ*40/' '/                                          29
      CHARACTER NLGLA_DZ*12/'R.DZ'/                                       29
      INTEGER MLGLA_DZ(2)/1,1/                                            29
      REAL RE_LGLA_DZ                                                     29
C -                                                                       29
      REAL LGLA_DENSITY                                                   29
      EQUIVALENCE (BANK_LGLA(10),LGLA_DENSITY,RE_LGLA_DENSITY),           29
     *(NAMES_LGLA(8),NLGLA_DENSITY)                                       29
      EQUIVALENCE (COMMEN_LGLA(8),CLGLA_DENSITY),(MAP_LGLA(1,8),          29
     *MLGLA_DENSITY)                                                      29
      CHARACTER CLGLA_DENSITY*40/' '/                                     29
      CHARACTER NLGLA_DENSITY*12/'R.DENSITY'/                             29
      INTEGER MLGLA_DENSITY(2)/1,1/                                       29
      REAL RE_LGLA_DENSITY                                                29
C -                                                                       29
      REAL LGLA_RADLEN                                                    29
      EQUIVALENCE (BANK_LGLA(11),LGLA_RADLEN,RE_LGLA_RADLEN),             29
     *(NAMES_LGLA(9),NLGLA_RADLEN)                                        29
      EQUIVALENCE (COMMEN_LGLA(9),CLGLA_RADLEN),(MAP_LGLA(1,9),           29
     *MLGLA_RADLEN)                                                       29
      CHARACTER CLGLA_RADLEN*40/' '/                                      29
      CHARACTER NLGLA_RADLEN*12/'R.RADLEN'/                               29
      INTEGER MLGLA_RADLEN(2)/1,1/                                        29
      REAL RE_LGLA_RADLEN                                                 29
C -                                                                       29
      REAL LGLA_PBCONT                                                    29
      EQUIVALENCE (BANK_LGLA(12),LGLA_PBCONT,RE_LGLA_PBCONT),             29
     *(NAMES_LGLA(10),NLGLA_PBCONT)                                       29
      EQUIVALENCE (COMMEN_LGLA(10),CLGLA_PBCONT),(MAP_LGLA(1,10),         29
     *MLGLA_PBCONT)                                                       29
      CHARACTER CLGLA_PBCONT*40/' '/                                      29
      CHARACTER NLGLA_PBCONT*12/'R.PBCONT'/                               29
      INTEGER MLGLA_PBCONT(2)/1,1/                                        29
      REAL RE_LGLA_PBCONT                                                 29
C -                                                                       29
      REAL LGLA_CRITENE                                                   29
      EQUIVALENCE (BANK_LGLA(13),LGLA_CRITENE,RE_LGLA_CRITENE),           29
     *(NAMES_LGLA(11),NLGLA_CRITENE)                                      29
      EQUIVALENCE (COMMEN_LGLA(11),CLGLA_CRITENE),(MAP_LGLA(1,11),        29
     *MLGLA_CRITENE)                                                      29
      CHARACTER CLGLA_CRITENE*40/' '/                                     29
      CHARACTER NLGLA_CRITENE*12/'R.CRITENE'/                             29
      INTEGER MLGLA_CRITENE(2)/1,1/                                       29
      REAL RE_LGLA_CRITENE                                                29
C -                                                                       29
      REAL LGLA_MOLIERER                                                  29
      EQUIVALENCE (BANK_LGLA(14),LGLA_MOLIERER,RE_LGLA_MOLIERER),         29
     *(NAMES_LGLA(12),NLGLA_MOLIERER)                                     29
      EQUIVALENCE (COMMEN_LGLA(12),CLGLA_MOLIERER),(MAP_LGLA(1,12),       29
     *MLGLA_MOLIERER)                                                     29
      CHARACTER CLGLA_MOLIERER*40/' '/                                    29
      CHARACTER NLGLA_MOLIERER*12/'R.MOLIERER'/                           29
      INTEGER MLGLA_MOLIERER(2)/1,1/                                      29
      REAL RE_LGLA_MOLIERER                                               29
C -                                                                       29
      INTEGER LEN_LGLA,LLEN_LGLA,LINK_LGLA                                29
      PARAMETER (LEN_LGLA=14)                                             29
      PARAMETER (LLEN_LGLA=12)                                            29
      INTEGER FLAG_LGLA/0/,MAP_LGLA(2,LLEN_LGLA),IDNUM_LGLA(10)           29
      CHARACTER NAMES_LGLA(LLEN_LGLA)*12,COMMEN_LGLA(LLEN_LGLA)*40        29
      DATA IDNUM_LGLA/10*0/                                               29
      REAL BANK_LGLA(LEN_LGLA)                                            29
      CHARACTER*60 BPATH_LGLA/' '/                                        29
      SAVE FLAG_LGLA,MAP_LGLA,IDNUM_LGLA,NAMES_LGLA,COMMEN_LGLA,          29
     *BPATH_LGLA                                                          29
      DATA LINK_LGLA/0/,BANK_LGLA/LEN_LGLA*0/                             29
C *                                                                       29
C -                                                                       30
      REAL TRAP_VERSION                                                   30
      EQUIVALENCE (BANK_TRAP(3),TRAP_VERSION,RE_TRAP_VERSION),            30
     *(NAMES_TRAP(1),NTRAP_VERSION)                                       30
      EQUIVALENCE (COMMEN_TRAP(1),CTRAP_VERSION),(MAP_TRAP(1,1),          30
     *MTRAP_VERSION)                                                      30
      CHARACTER CTRAP_VERSION*40/' '/                                     30
      CHARACTER NTRAP_VERSION*12/'R.VERSION'/                             30
      INTEGER MTRAP_VERSION(2)/1,1/                                       30
      REAL RE_TRAP_VERSION                                                30
C -                                                                       30
      REAL TRAP_XF                                                        30
      EQUIVALENCE (BANK_TRAP(4),TRAP_XF,RE_TRAP_XF),(NAMES_TRAP(2),       30
     *NTRAP_XF)                                                           30
      EQUIVALENCE (COMMEN_TRAP(2),CTRAP_XF),(MAP_TRAP(1,2),MTRAP_XF)      30
      CHARACTER CTRAP_XF*40/' '/                                          30
      CHARACTER NTRAP_XF*12/'R.XF'/                                       30
      INTEGER MTRAP_XF(2)/1,1/                                            30
      REAL RE_TRAP_XF                                                     30
C -                                                                       30
      REAL TRAP_YF                                                        30
      EQUIVALENCE (BANK_TRAP(5),TRAP_YF,RE_TRAP_YF),(NAMES_TRAP(3),       30
     *NTRAP_YF)                                                           30
      EQUIVALENCE (COMMEN_TRAP(3),CTRAP_YF),(MAP_TRAP(1,3),MTRAP_YF)      30
      CHARACTER CTRAP_YF*40/' '/                                          30
      CHARACTER NTRAP_YF*12/'R.YF'/                                       30
      INTEGER MTRAP_YF(2)/1,1/                                            30
      REAL RE_TRAP_YF                                                     30
C -                                                                       30
      REAL TRAP_ZF                                                        30
      EQUIVALENCE (BANK_TRAP(6),TRAP_ZF,RE_TRAP_ZF),(NAMES_TRAP(4),       30
     *NTRAP_ZF)                                                           30
      EQUIVALENCE (COMMEN_TRAP(4),CTRAP_ZF),(MAP_TRAP(1,4),MTRAP_ZF)      30
      CHARACTER CTRAP_ZF*40/' '/                                          30
      CHARACTER NTRAP_ZF*12/'R.ZF'/                                       30
      INTEGER MTRAP_ZF(2)/1,1/                                            30
      REAL RE_TRAP_ZF                                                     30
C -                                                                       30
      REAL TRAP_XB                                                        30
      EQUIVALENCE (BANK_TRAP(7),TRAP_XB,RE_TRAP_XB),(NAMES_TRAP(5),       30
     *NTRAP_XB)                                                           30
      EQUIVALENCE (COMMEN_TRAP(5),CTRAP_XB),(MAP_TRAP(1,5),MTRAP_XB)      30
      CHARACTER CTRAP_XB*40/' '/                                          30
      CHARACTER NTRAP_XB*12/'R.XB'/                                       30
      INTEGER MTRAP_XB(2)/1,1/                                            30
      REAL RE_TRAP_XB                                                     30
C -                                                                       30
      REAL TRAP_YB                                                        30
      EQUIVALENCE (BANK_TRAP(8),TRAP_YB,RE_TRAP_YB),(NAMES_TRAP(6),       30
     *NTRAP_YB)                                                           30
      EQUIVALENCE (COMMEN_TRAP(6),CTRAP_YB),(MAP_TRAP(1,6),MTRAP_YB)      30
      CHARACTER CTRAP_YB*40/' '/                                          30
      CHARACTER NTRAP_YB*12/'R.YB'/                                       30
      INTEGER MTRAP_YB(2)/1,1/                                            30
      REAL RE_TRAP_YB                                                     30
C -                                                                       30
      REAL TRAP_ZB                                                        30
      EQUIVALENCE (BANK_TRAP(9),TRAP_ZB,RE_TRAP_ZB),(NAMES_TRAP(7),       30
     *NTRAP_ZB)                                                           30
      EQUIVALENCE (COMMEN_TRAP(7),CTRAP_ZB),(MAP_TRAP(1,7),MTRAP_ZB)      30
      CHARACTER CTRAP_ZB*40/' '/                                          30
      CHARACTER NTRAP_ZB*12/'R.ZB'/                                       30
      INTEGER MTRAP_ZB(2)/1,1/                                            30
      REAL RE_TRAP_ZB                                                     30
C -                                                                       30
      REAL TRAP_DX1                                                       30
      EQUIVALENCE (BANK_TRAP(10),TRAP_DX1,RE_TRAP_DX1),(NAMES_TRAP(8),    30
     *NTRAP_DX1)                                                          30
      EQUIVALENCE (COMMEN_TRAP(8),CTRAP_DX1),(MAP_TRAP(1,8),MTRAP_DX1)    30
      CHARACTER CTRAP_DX1*40/' '/                                         30
      CHARACTER NTRAP_DX1*12/'R.DX1'/                                     30
      INTEGER MTRAP_DX1(2)/1,1/                                           30
      REAL RE_TRAP_DX1                                                    30
C -                                                                       30
      REAL TRAP_DX2                                                       30
      EQUIVALENCE (BANK_TRAP(11),TRAP_DX2,RE_TRAP_DX2),(NAMES_TRAP(9),    30
     *NTRAP_DX2)                                                          30
      EQUIVALENCE (COMMEN_TRAP(9),CTRAP_DX2),(MAP_TRAP(1,9),MTRAP_DX2)    30
      CHARACTER CTRAP_DX2*40/' '/                                         30
      CHARACTER NTRAP_DX2*12/'R.DX2'/                                     30
      INTEGER MTRAP_DX2(2)/1,1/                                           30
      REAL RE_TRAP_DX2                                                    30
C -                                                                       30
      REAL TRAP_DY                                                        30
      EQUIVALENCE (BANK_TRAP(12),TRAP_DY,RE_TRAP_DY),(NAMES_TRAP(10),     30
     *NTRAP_DY)                                                           30
      EQUIVALENCE (COMMEN_TRAP(10),CTRAP_DY),(MAP_TRAP(1,10),MTRAP_DY)    30
      CHARACTER CTRAP_DY*40/' '/                                          30
      CHARACTER NTRAP_DY*12/'R.DY'/                                       30
      INTEGER MTRAP_DY(2)/1,1/                                            30
      REAL RE_TRAP_DY                                                     30
C -                                                                       30
      REAL TRAP_DZ                                                        30
      EQUIVALENCE (BANK_TRAP(13),TRAP_DZ,RE_TRAP_DZ),(NAMES_TRAP(11),     30
     *NTRAP_DZ)                                                           30
      EQUIVALENCE (COMMEN_TRAP(11),CTRAP_DZ),(MAP_TRAP(1,11),MTRAP_DZ)    30
      CHARACTER CTRAP_DZ*40/' '/                                          30
      CHARACTER NTRAP_DZ*12/'R.DZ'/                                       30
      INTEGER MTRAP_DZ(2)/1,1/                                            30
      REAL RE_TRAP_DZ                                                     30
C -                                                                       30
      INTEGER LEN_TRAP,LLEN_TRAP,LINK_TRAP                                30
      PARAMETER (LEN_TRAP=13)                                             30
      PARAMETER (LLEN_TRAP=11)                                            30
      INTEGER FLAG_TRAP/0/,MAP_TRAP(2,LLEN_TRAP),IDNUM_TRAP(10)           30
      CHARACTER NAMES_TRAP(LLEN_TRAP)*12,COMMEN_TRAP(LLEN_TRAP)*40        30
      DATA IDNUM_TRAP/10*0/                                               30
      REAL BANK_TRAP(LEN_TRAP)                                            30
      CHARACTER*60 BPATH_TRAP/' '/                                        30
      SAVE FLAG_TRAP,MAP_TRAP,IDNUM_TRAP,NAMES_TRAP,COMMEN_TRAP,          30
     *BPATH_TRAP                                                          30
      DATA LINK_TRAP/0/,BANK_TRAP/LEN_TRAP*0/                             30
C *                                                                       30
C *                                                                       31
      CALL AGSBEGM(MODULE,IPRIN)                                          31
      IF (IPRIN.LT.0) RETURN                                              31
C Filling bank MCDT - MOTHER CALORIMETER                                  31
      BANK_TITLE='MOTHER CALORIMETER'                                     31
      BPATH_MCDT='MCDT*'                                                  31
      IDNUM_MCDT(1)=0                                                     32
C FACE CENTER OF CALORIMETER                                              32
      MCDT_VERSION=1.0                                                    32
      CMCDT_VERSION='FACE CENTER OF CALORIMETER'                          32
C X CDT FROM CENTER OF TPC                                                33
      MCDT_X=0.0                                                          33
      CMCDT_X='X CDT FROM CENTER OF TPC'                                  33
C Y CDT FROM CENTER OF TPC                                                34
      MCDT_Y=0.0                                                          34
      CMCDT_Y='Y CDT FROM CENTER OF TPC'                                  34
C Z CDT FROM CENTER OF TPC                                                35
      MCDT_Z=750.0                                                        35
      CMCDT_Z='Z CDT FROM CENTER OF TPC'                                  35
C HCMO WIDTH                                                              36
      MCDT_DX=30/2.                                                       36
      CMCDT_DX='HCMO WIDTH'                                               36
C HCMO HEIGHT + SHILD HIGHT                                               37
      MCDT_DY=(30)/2.                                                     37
      CMCDT_DY='HCMO HEIGHT + SHILD HIGHT'                                37
C HCMO LENGTH                                                             38
      MCDT_DZ=117/2.                                                      38
      CMCDT_DZ='HCMO LENGTH'                                              38
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MCDT',    39
     *BPATH_MCDT,IDNUM_MCDT,1, MAP_MCDT,NAMES_MCDT,COMMEN_MCDT,           39
     *BANK_MCDT,LEN_MCDT,LLEN_MCDT,LINK_MCDT,FLAG_MCDT)                   39
C *                                                                       39
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MCDT',    39
     *BPATH_MCDT,IDNUM_MCDT,1, MAP_MCDT,NAMES_MCDT,COMMEN_MCDT,           39
     *BANK_MCDT,LEN_MCDT,LLEN_MCDT,LINK_MCDT,FLAG_MCDT)                   39
C *                                                                       40
      CALL AGSBEGM(MODULE,IPRIN)                                          40
      IF (IPRIN.LT.0) RETURN                                              40
C Filling bank TOWR - STRUCTURE CONTAINING CALORIMETER TOWR DIMENTION     40
      BANK_TITLE='STRUCTURE CONTAINING CALORIMETER TOWR DIMENTION'        40
      BPATH_TOWR='TOWR*'                                                  40
      IDNUM_TOWR(1)=0                                                     41
C TOWR DIMENTION                                                          41
      TOWR_VERSION=1.0                                                    41
      CTOWR_VERSION='TOWR DIMENTION'                                      41
C TOWR WIDTH                                                              42
      TOWR_DX=10.0/2.                                                     42
      CTOWR_DX='TOWR WIDTH'                                               42
C TOWR HEIGHT                                                             43
      TOWR_DY=10.0/2.                                                     43
      CTOWR_DY='TOWR HEIGHT'                                              43
C TOWR LENGTH                                                             44
      TOWR_DZ=117.0/2.                                                    44
      CTOWR_DZ='TOWR LENGTH'                                              44
C NO OF TOWER IN X DIR                                                    45
      TOWR_NTOWERX=3                                                      45
      CTOWR_NTOWERX='NO OF TOWER IN X DIR'                                45
C NO OF TOWER IN Y DIR                                                    46
      TOWR_NTOWERY=3                                                      46
      CTOWR_NTOWERY='NO OF TOWER IN Y DIR'                                46
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TOWR',    47
     *BPATH_TOWR,IDNUM_TOWR,1, MAP_TOWR,NAMES_TOWR,COMMEN_TOWR,           47
     *BANK_TOWR,LEN_TOWR,LLEN_TOWR,LINK_TOWR,FLAG_TOWR)                   47
C *                                                                       47
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TOWR',    47
     *BPATH_TOWR,IDNUM_TOWR,1, MAP_TOWR,NAMES_TOWR,COMMEN_TOWR,           47
     *BANK_TOWR,LEN_TOWR,LLEN_TOWR,LINK_TOWR,FLAG_TOWR)                   47
C *                                                                       48
      CALL AGSBEGM(MODULE,IPRIN)                                          48
      IF (IPRIN.LT.0) RETURN                                              48
C Filling bank CELL - STRUCTURE CONTAINING CALORIMETER PIXELS DIMENTION   48
      BANK_TITLE='STRUCTURE CONTAINING CALORIMETER PIXELS DIMENTION'      48
      BPATH_CELL='CELL*'                                                  48
      IDNUM_CELL(1)=0                                                     49
C PIXELS DIMENTION                                                        49
      CELL_VERSION=1.0                                                    49
      CCELL_VERSION='PIXELS DIMENTION'                                    49
C PIXELS WIDTH                                                            50
      CELL_DX=3.3/2.                                                      50
      CCELL_DX='PIXELS WIDTH'                                             50
C PIXELS HEIGHT                                                           51
      CELL_DY=3.3/2.                                                      51
      CCELL_DY='PIXELS HEIGHT'                                            51
C TOWR LENGTH                                                             52
      CELL_DZ=117.0/2.                                                    52
      CCELL_DZ='TOWR LENGTH'                                              52
C NO OF CELLS IN X DIR                                                    53
      CELL_NCELLX=3                                                       53
      CCELL_NCELLX='NO OF CELLS IN X DIR'                                 53
C NO OF CELLS IN Y DIR                                                    54
      CELL_NCELLY=3                                                       54
      CCELL_NCELLY='NO OF CELLS IN Y DIR'                                 54
C NO FIBERS IN CELL 16 FIBERS                                             55
      CELL_NFIBERXL=16                                                    55
      CCELL_NFIBERXL='NO FIBERS IN CELL 16 FIBERS'                        55
C NO FIBERS IN CELL 16 FIBERS                                             56
      CELL_NFIBERYL=16                                                    56
      CCELL_NFIBERYL='NO FIBERS IN CELL 16 FIBERS'                        56
C NO FIBERS IN CELL 15 FIBERS                                             57
      CELL_NFIBERXS=15                                                    57
      CCELL_NFIBERXS='NO FIBERS IN CELL 15 FIBERS'                        57
C NO FIBERS IN CELL 15 FIBERS                                             58
      CELL_NFIBERYS=15                                                    58
      CCELL_NFIBERYS='NO FIBERS IN CELL 15 FIBERS'                        58
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CELL',    59
     *BPATH_CELL,IDNUM_CELL,1, MAP_CELL,NAMES_CELL,COMMEN_CELL,           59
     *BANK_CELL,LEN_CELL,LLEN_CELL,LINK_CELL,FLAG_CELL)                   59
C *                                                                       59
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'CELL',    59
     *BPATH_CELL,IDNUM_CELL,1, MAP_CELL,NAMES_CELL,COMMEN_CELL,           59
     *BANK_CELL,LEN_CELL,LLEN_CELL,LINK_CELL,FLAG_CELL)                   59
C *                                                                       60
      CALL AGSBEGM(MODULE,IPRIN)                                          60
      IF (IPRIN.LT.0) RETURN                                              60
C Filling bank FIBR - SCINTILATING FIBES IN TOWERS/CELLS                  60
      BANK_TITLE='SCINTILATING FIBES IN TOWERS/CELLS'                     60
      BPATH_FIBR='FIBR*'                                                  60
      IDNUM_FIBR(1)=0                                                     61
C FIBER DIMENTION                                                         61
      FIBR_VERSION=1.0                                                    61
      CFIBR_VERSION='FIBER DIMENTION'                                     61
C INNER RADIUS OF FIBER                                                   62
      FIBR_RMIN=0.0                                                       62
      CFIBR_RMIN='INNER RADIUS OF FIBER'                                  62
C OUTER RADIUS OF FIBER                                                   63
      FIBR_RMAX=0.055                                                     63
      CFIBR_RMAX='OUTER RADIUS OF FIBER'                                  63
C TOWR LENGTH                                                             64
      FIBR_DZ=117.0/2.                                                    64
      CFIBR_DZ='TOWR LENGTH'                                              64
C DISTANCE FROM EDGE OR FROM ONE CELL BOUNDRY TO OTHER 1: FOR FIRST CELL  65
      FIBR_STARTX1=0.107                                                  65
      CFIBR_STARTX1='DISTANCE FROM EDGE OR FROM ONE CELL BOUNDRY TO OTHE  65
     *R 1: FOR FIRST CELL'                                                65
C DISTANCE FROM EDGE OR FROM ONE CELL BOUNDRY TO OTHER                    66
      FIBR_STARTY1=0.107                                                  66
      CFIBR_STARTY1='DISTANCE FROM EDGE OR FROM ONE CELL BOUNDRY TO OTHE  66
     *R'                                                                  66
C DISTANCE FROM EDGE OR FROM ONE CELL BOUNDRY TO OTHER 2:FOR 2ND AND 3RD  67
C  CELL                                                                   67
      FIBR_STARTX2=0.1065                                                 67
      CFIBR_STARTX2='DISTANCE FROM EDGE OR FROM ONE CELL BOUNDRY TO OTHE  67
     *R 2:FOR 2ND AND 3RD CELL'                                           67
C DISTANCE FROM EDGE OR FROM ONE CELL BOUNDRY TO OTHER                    68
      FIBR_STARTY2=0.1065                                                 68
      CFIBR_STARTY2='DISTANCE FROM EDGE OR FROM ONE CELL BOUNDRY TO OTHE  68
     *R'                                                                  68
C DISTANCE BETWEEN FIBERS CENTER TO CENTER                                69
      FIBR_STEPX=0.213                                                    69
      CFIBR_STEPX='DISTANCE BETWEEN FIBERS CENTER TO CENTER'              69
C DISTANCE BETWEEN FIBERS CENTER TO CENTER                                70
      FIBR_STEPY=0.213                                                    70
      CFIBR_STEPY='DISTANCE BETWEEN FIBERS CENTER TO CENTER'              70
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FIBR',    71
     *BPATH_FIBR,IDNUM_FIBR,1, MAP_FIBR,NAMES_FIBR,COMMEN_FIBR,           71
     *BANK_FIBR,LEN_FIBR,LLEN_FIBR,LINK_FIBR,FLAG_FIBR)                   71
C *                                                                       71
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FIBR',    71
     *BPATH_FIBR,IDNUM_FIBR,1, MAP_FIBR,NAMES_FIBR,COMMEN_FIBR,           71
     *BANK_FIBR,LEN_FIBR,LLEN_FIBR,LINK_FIBR,FLAG_FIBR)                   71
C *                                                                       72
      CALL AGSBEGM(MODULE,IPRIN)                                          72
      IF (IPRIN.LT.0) RETURN                                              72
C Filling bank LGLA - LEAD GLASS                                          72
      BANK_TITLE='LEAD GLASS'                                             72
      BPATH_LGLA='LGLA*'                                                  72
      IDNUM_LGLA(1)=0                                                     73
C FACE CENTER OF CALORIMETER                                              73
      LGLA_VERSION=1.0                                                    73
      CLGLA_VERSION='FACE CENTER OF CALORIMETER'                          73
C X CDT FROM CENTER OF TPC                                                74
      LGLA_X=0.0                                                          74
      CLGLA_X='X CDT FROM CENTER OF TPC'                                  74
C Y CDT FROM CENTER OF TPC                                                75
      LGLA_Y=0.0                                                          75
      CLGLA_Y='Y CDT FROM CENTER OF TPC'                                  75
C Z CDT FROM CENTER OF TPC                                                76
      LGLA_Z=-600.0                                                       76
      CLGLA_Z='Z CDT FROM CENTER OF TPC'                                  76
C HCMO WIDTH                                                              77
      LGLA_DX=14.5/2.0                                                    77
      CLGLA_DX='HCMO WIDTH'                                               77
C HCMO HEIGHT + SHILD HIGHT                                               78
      LGLA_DY=14.5/2.0                                                    78
      CLGLA_DY='HCMO HEIGHT + SHILD HIGHT'                                78
C HCMO LENGTH                                                             79
      LGLA_DZ=35.0/2.0                                                    79
      CLGLA_DZ='HCMO LENGTH'                                              79
C GDENSITY [/CM^3]                                                        80
      LGLA_DENSITY=3.86                                                   80
      CLGLA_DENSITY='GDENSITY [/CM^3]'                                    80
C RADIATION LENGTH [CM]                                                   81
      LGLA_RADLEN=2.5                                                     81
      CLGLA_RADLEN='RADIATION LENGTH [CM]'                                81
C PBO CONTENT [%]                                                         82
      LGLA_PBCONT=65.4                                                    82
      CLGLA_PBCONT='PBO CONTENT [%]'                                      82
C CRITICAL ENERGY [GEV]                                                   83
      LGLA_CRITENE=0.0158                                                 83
      CLGLA_CRITENE='CRITICAL ENERGY [GEV]'                               83
C MOLIERE RADIUS [CM]                                                     84
      LGLA_MOLIERER=3.32                                                  84
      CLGLA_MOLIERER='MOLIERE RADIUS [CM]'                                84
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'LGLA',    85
     *BPATH_LGLA,IDNUM_LGLA,1, MAP_LGLA,NAMES_LGLA,COMMEN_LGLA,           85
     *BANK_LGLA,LEN_LGLA,LLEN_LGLA,LINK_LGLA,FLAG_LGLA)                   85
C *                                                                       85
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'LGLA',    85
     *BPATH_LGLA,IDNUM_LGLA,1, MAP_LGLA,NAMES_LGLA,COMMEN_LGLA,           85
     *BANK_LGLA,LEN_LGLA,LLEN_LGLA,LINK_LGLA,FLAG_LGLA)                   85
C *                                                                       86
      CALL AGSBEGM(MODULE,IPRIN)                                          86
      IF (IPRIN.LT.0) RETURN                                              86
C Filling bank TRAP - BEAM CONTENT                                        86
      BANK_TITLE='BEAM CONTENT'                                           86
      BPATH_TRAP='TRAP*'                                                  86
      IDNUM_TRAP(1)=0                                                     87
C PIXELS DIMENTION                                                        87
      TRAP_VERSION=1.0                                                    87
      CTRAP_VERSION='PIXELS DIMENTION'                                    87
C X CDT FROM CENTER OF TPC                                                88
      TRAP_XF=-8.35                                                       88
      CTRAP_XF='X CDT FROM CENTER OF TPC'                                 88
C Y CDT FROM CENTER OF TPC                                                89
      TRAP_YF=0.0                                                         89
      CTRAP_YF='Y CDT FROM CENTER OF TPC'                                 89
C Z CDT FROM CENTER OF TPC                                                90
      TRAP_ZF=-86.5                                                       90
      CTRAP_ZF='Z CDT FROM CENTER OF TPC'                                 90
C X CDT FROM CENTER OF TPC                                                91
      TRAP_XB=0.0                                                         91
      CTRAP_XB='X CDT FROM CENTER OF TPC'                                 91
C Y CDT FROM CENTER OF TPC                                                92
      TRAP_YB=-8.35                                                       92
      CTRAP_YB='Y CDT FROM CENTER OF TPC'                                 92
C Z CDT FROM CENTER OF TPC                                                93
      TRAP_ZB=-83.5                                                       93
      CTRAP_ZB='Z CDT FROM CENTER OF TPC'                                 93
C PIXELS WIDTH                                                            94
      TRAP_DX1=9.8/2.0                                                    94
      CTRAP_DX1='PIXELS WIDTH'                                            94
C PIXELS WIDTH                                                            95
      TRAP_DX2=1.8/2.0                                                    95
      CTRAP_DX2='PIXELS WIDTH'                                            95
C PIXELS HEIGHT                                                           96
      TRAP_DY=1.0/2.0                                                     96
      CTRAP_DY='PIXELS HEIGHT'                                            96
C TOWR LENGTH                                                             97
      TRAP_DZ=20.0/2.0                                                    97
      CTRAP_DZ='TOWR LENGTH'                                              97
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TRAP',    98
     *BPATH_TRAP,IDNUM_TRAP,1, MAP_TRAP,NAMES_TRAP,COMMEN_TRAP,           98
     *BANK_TRAP,LEN_TRAP,LLEN_TRAP,LINK_TRAP,FLAG_TRAP)                   98
C *                                                                       98
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'TRAP',    98
     *BPATH_TRAP,IDNUM_TRAP,1, MAP_TRAP,NAMES_TRAP,COMMEN_TRAP,           98
     *BANK_TRAP,LEN_TRAP,LLEN_TRAP,LINK_TRAP,FLAG_TRAP)                   98
C use MCDT bank                                                           99
      BPATH_MCDT='MCDT*'                                                  99
      IDNUM_MCDT(1)=0                                                     99
      MCDT_VERSION=1.0                                                    99
      CALL AGDGETP('HCALGEOF','*',-999,'MCDT',NMCDT_VERSION,              99
     *RE_MCDT_VERSION,BPATH_MCDT,IDNUM_MCDT,1,MAP_MCDT,NAMES_MCDT,        99
     *BANK_MCDT,LEN_MCDT,LLEN_MCDT,LINK_MCDT,FLAG_MCDT)                   99
C use TOWR bank                                                           100
      BPATH_TOWR='TOWR*'                                                  100
      IDNUM_TOWR(1)=0                                                     100
      TOWR_VERSION=1.0                                                    100
      CALL AGDGETP('HCALGEOF','*',-999,'TOWR',NTOWR_VERSION,              100
     *RE_TOWR_VERSION,BPATH_TOWR,IDNUM_TOWR,1,MAP_TOWR,NAMES_TOWR,        100
     *BANK_TOWR,LEN_TOWR,LLEN_TOWR,LINK_TOWR,FLAG_TOWR)                   100
C use CELL bank                                                           101
      BPATH_CELL='CELL*'                                                  101
      IDNUM_CELL(1)=0                                                     101
      CELL_VERSION=1.0                                                    101
      CALL AGDGETP('HCALGEOF','*',-999,'CELL',NCELL_VERSION,              101
     *RE_CELL_VERSION,BPATH_CELL,IDNUM_CELL,1,MAP_CELL,NAMES_CELL,        101
     *BANK_CELL,LEN_CELL,LLEN_CELL,LINK_CELL,FLAG_CELL)                   101
C use FIBR bank                                                           102
      BPATH_FIBR='FIBR*'                                                  102
      IDNUM_FIBR(1)=0                                                     102
      FIBR_VERSION=1.0                                                    102
      CALL AGDGETP('HCALGEOF','*',-999,'FIBR',NFIBR_VERSION,              102
     *RE_FIBR_VERSION,BPATH_FIBR,IDNUM_FIBR,1,MAP_FIBR,NAMES_FIBR,        102
     *BANK_FIBR,LEN_FIBR,LLEN_FIBR,LINK_FIBR,FLAG_FIBR)                   102
C use LGLA bank                                                           103
      BPATH_LGLA='LGLA*'                                                  103
      IDNUM_LGLA(1)=0                                                     103
      LGLA_VERSION=1.0                                                    103
      CALL AGDGETP('HCALGEOF','*',-999,'LGLA',NLGLA_VERSION,              103
     *RE_LGLA_VERSION,BPATH_LGLA,IDNUM_LGLA,1,MAP_LGLA,NAMES_LGLA,        103
     *BANK_LGLA,LEN_LGLA,LLEN_LGLA,LINK_LGLA,FLAG_LGLA)                   103
C use TRAP bank                                                           104
      BPATH_TRAP='TRAP*'                                                  104
      IDNUM_TRAP(1)=0                                                     104
      TRAP_VERSION=1.0                                                    104
      CALL AGDGETP('HCALGEOF','*',-999,'TRAP',NTRAP_VERSION,              104
     *RE_TRAP_VERSION,BPATH_TRAP,IDNUM_TRAP,1,MAP_TRAP,NAMES_TRAP,        104
     *BANK_TRAP,LEN_TRAP,LLEN_TRAP,LINK_TRAP,FLAG_TRAP)                   104
      CALL AGSBEGM(MODULE,IPRIN)                                          105
C Check IPRIN<0                                                           105
      IF (IPRIN.LT.0) RETURN                                              105
C Create HCMO :                                                           105
C Call To Local Procedure HCMO                                            105
      ASSIGN 5020 TO BLOCK_HCMO                                           105
      GO TO  5010                                                         105
5020  CONTINUE                                                            105
C *                                                                       105
C Geant POSITION : HCMO ===> MOTHER='CAVE' X=MCDT_X Y=MCDT_Y Z=MCDT_Z <=  106
C ==                                                                      106
      CALL AGSRESET                                                       106
      AG_MOTHER = 'CAVE'                                                  106
      AG_X = MCDT_X                                                       106
      AG_Y = MCDT_Y                                                       106
      AG_Z = MCDT_Z                                                       106
      AG_TITLE = 'HCMO'                                                   106
      AG_EXNAME = 'POSITION'                                              106
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         106
      CALL AXPOSITION                                                     106
C *                                                                       106
C ----------------------------------------------------------------------  106
C ------------ HCMO                                                       106
      CALL AGSENDM                                                        108
      RETURN                                                              108
C Begin Local Procedure HCMO                                              108
C HCMO MOTHER CALORIMETER                                                 108
5010  CONTINUE                                                            108
         AG_TITLE='HCMO'                                                  108
         AG_MODULE=MODULE                                                 108
         AG_MARK='V00'                                                    108
         CALL AGSPUSH                                                     108
         AG_MODULE=MODULE                                                 108
C    *                                                                    108
C    Geant MATERIAL : AIR ===> inherited arguments used <===              109
         CALL AGSRESET                                                    109
         AG_TITLE = 'AIR'                                                 109
         AG_EXNAME = 'MATERIAL'                                           109
         AG_PARLIST = 'NONE'                                              109
         CALL AXMATERIAL                                                  109
C    *                                                                    109
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           109
         CALL AGSRESET                                                    110
         AG_TITLE = 'STANDARD'                                            110
         AG_EXNAME = 'MEDIUM'                                             110
         AG_PARLIST = 'NONE'                                              110
         CALL AXMEDIUM                                                    110
C    *                                                                    110
C    Geant ATTRIBUTE : HCMO ===> SEEN=0 COLO=2 <===                       110
         CALL AGSRESET                                                    111
         AG_SEEN = 0                                                      111
         AG_COLO = 2                                                      111
         AG_TITLE = 'HCMO'                                                111
         AG_EXNAME = 'ATTRIBUTE'                                          111
         AG_PARLIST = 'SEEN_COLO'                                         111
         CALL AXATTRIBUTE                                                 111
C    *                                                                    111
C    Geant SHAPE : BOX ===> DX=MCDT_DX DY=MCDT_DY DZ=MCDT_DZ <===         111
         CALL AGSRESET                                                    112
         AG_DX = MCDT_DX                                                  112
         AG_DY = MCDT_DY                                                  112
         AG_DZ = MCDT_DZ                                                  112
         AG_TITLE = 'BOX'                                                 112
         AG_EXNAME = 'SHAPE'                                              112
         AG_PARLIST = 'DX_DY_DZ'                                          112
         CALL AXSHAPE                                                     112
C    *                                                                    112
C    *                                                                    112
         I=0                                                              113
            GO TO 5033                                                    113
5031        I=I+1                                                         113
5033        IF(I-(TOWR_NTOWERX-1).GT.0) GO TO 5032                        113
C       *                                                                 114
            J=0                                                           114
               GO TO 5043                                                 114
5041           J=J+1                                                      114
5043           IF(J-(TOWR_NTOWERY-1).GT.0) GO TO 5042                     114
C          Create HTWR :                                                  115
C          Call To Local Procedure HTWR                                   115
               ASSIGN 5060 TO BLOCK_HTWR                                  115
               GO TO  5050                                                115
5060           CONTINUE                                                   115
C          *                                                              115
C          Geant POSITION : HTWR ===> X=-((TOWR_NTOWERX-1)*TOWR_DX)+(I*T  117
C          OWR_DX*2) Y=-((TOWR_NTOWERY-1)*TOWR_DY)+(J*TOWR_DY*2) Z=0.0 <  117
C          ===                                                            117
               CALL AGSRESET                                              117
               AG_X = -((TOWR_NTOWERX-1)*TOWR_DX)+(I*TOWR_DX*2)           117
               AG_Y = -((TOWR_NTOWERY-1)*TOWR_DY)+(J*TOWR_DY*2)           117
               AG_Z = 0.0                                                 117
               AG_TITLE = 'HTWR'                                          117
               AG_EXNAME = 'POSITION'                                     117
               AG_PARLIST = 'X_Y_Z'                                       117
               CALL AXPOSITION                                            117
C          *                                                              117
            GO TO 5041                                                    118
5042        CONTINUE                                                      118
         GO TO 5031                                                       119
5032     CONTINUE                                                         119
C    Create LEDG :                                                        120
C    Call To Local Procedure LEDG                                         120
         ASSIGN 5080 TO BLOCK_LEDG                                        120
         GO TO  5070                                                      120
5080     CONTINUE                                                         120
C    *                                                                    120
C    Geant POSITION : LEDG ===> X=LGLA_X Y=LGLA_Y Z=LGLA_Z <===           121
         CALL AGSRESET                                                    121
         AG_X = LGLA_X                                                    121
         AG_Y = LGLA_Y                                                    121
         AG_Z = LGLA_Z                                                    121
         AG_TITLE = 'LEDG'                                                121
         AG_EXNAME = 'POSITION'                                           121
         AG_PARLIST = 'X_Y_Z'                                             121
         CALL AXPOSITION                                                  121
C    *                                                                    121
C    Create BBCF :                                                        122
C    Call To Local Procedure BBCF                                         122
         ASSIGN 5100 TO BLOCK_BBCF                                        122
         GO TO  5090                                                      122
5100     CONTINUE                                                         122
C    *                                                                    122
C    Geant POSITION : BBCF ===> X=TRAP_XF Y=TRAP_YF Z=TRAP_ZF ALPHAX=-90  123
C     ALPHAZ=-90 <===                                                     123
         CALL AGSRESET                                                    123
         AG_X = TRAP_XF                                                   123
         AG_Y = TRAP_YF                                                   123
         AG_Z = TRAP_ZF                                                   123
         AG_ALPHAN=AG_ALPHAN+1                                            123
         AG_ALPHAK(AG_ALPHAN)=1                                           123
         AG_ALPHAL(AG_ALPHAN)=-90                                         123
         AG_ALPHAN=AG_ALPHAN+1                                            123
         AG_ALPHAK(AG_ALPHAN)=3                                           123
         AG_ALPHAL(AG_ALPHAN)=-90                                         123
         AG_TITLE = 'BBCF'                                                123
         AG_EXNAME = 'POSITION'                                           123
         AG_PARLIST = 'X_Y_Z_ALPHAX_ALPHAZ'                               123
         CALL AXPOSITION                                                  123
C    *                                                                    123
C    Create BBCB :                                                        124
C    Call To Local Procedure BBCB                                         124
         ASSIGN 5120 TO BLOCK_BBCB                                        124
         GO TO  5110                                                      124
5120     CONTINUE                                                         124
C    *                                                                    124
C    Geant POSITION : BBCB ===> X=TRAP_XB Y=TRAP_YB Z=TRAP_ZB ALPHAX=-90  125
C     <===                                                                125
         CALL AGSRESET                                                    125
         AG_X = TRAP_XB                                                   125
         AG_Y = TRAP_YB                                                   125
         AG_Z = TRAP_ZB                                                   125
         AG_ALPHAN=AG_ALPHAN+1                                            125
         AG_ALPHAK(AG_ALPHAN)=1                                           125
         AG_ALPHAL(AG_ALPHAN)=-90                                         125
         AG_TITLE = 'BBCB'                                                125
         AG_EXNAME = 'POSITION'                                           125
         AG_PARLIST = 'X_Y_Z_ALPHAX'                                      125
         CALL AXPOSITION                                                  125
C    *                                                                    125
5011  CALL AGSPOPB                                                        126
      GO TO BLOCK_HCMO, (5010 ,5020)                                      126
C End of Local Procedure                                                  126
C ----------------------------------------------------------------------  127
C ------------ HTWR                                                       127
C Begin Local Procedure HTWR                                              128
C HTWR TOWRS OF CALORIMETER                                               128
5050  CONTINUE                                                            128
         AG_TITLE='HTWR'                                                  128
         AG_MODULE=MODULE                                                 128
         AG_MARK='V00'                                                    128
         CALL AGSPUSH                                                     128
         AG_MODULE=MODULE                                                 128
C    *                                                                    128
C    Geant MATERIAL : LEAD ===> inherited arguments used <===             129
         CALL AGSRESET                                                    129
         AG_TITLE = 'LEAD'                                                129
         AG_EXNAME = 'MATERIAL'                                           129
         AG_PARLIST = 'NONE'                                              129
         CALL AXMATERIAL                                                  129
C    *                                                                    129
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           129
         CALL AGSRESET                                                    130
         AG_TITLE = 'STANDARD'                                            130
         AG_EXNAME = 'MEDIUM'                                             130
         AG_PARLIST = 'NONE'                                              130
         CALL AXMEDIUM                                                    130
C    *                                                                    130
C    Geant ATTRIBUTE : HTWR ===> SEEN=0 COLO=4 <===                       130
         CALL AGSRESET                                                    131
         AG_SEEN = 0                                                      131
         AG_COLO = 4                                                      131
         AG_TITLE = 'HTWR'                                                131
         AG_EXNAME = 'ATTRIBUTE'                                          131
         AG_PARLIST = 'SEEN_COLO'                                         131
         CALL AXATTRIBUTE                                                 131
C    *                                                                    131
C    Geant SHAPE : BOX ===> DX=TOWR_DX DY=TOWR_DY DZ=TOWR_DZ <===         131
         CALL AGSRESET                                                    132
         AG_DX = TOWR_DX                                                  132
         AG_DY = TOWR_DY                                                  132
         AG_DZ = TOWR_DZ                                                  132
         AG_TITLE = 'BOX'                                                 132
         AG_EXNAME = 'SHAPE'                                              132
         AG_PARLIST = 'DX_DY_DZ'                                          132
         CALL AXSHAPE                                                     132
C    *                                                                    132
         XTEMP=FIBR_STARTX1                                               132
C    *                                                                    134
         M=0                                                              134
            GO TO 5133                                                    134
5131        M=M+1                                                         134
5133        IF(M-(CELL_NCELLX-1).GT.0) GO TO 5132                         134
            YTEMP=FIBR_STARTY1                                            134
C       *                                                                 136
            N=0                                                           136
               GO TO 5143                                                 136
5141           N=N+1                                                      136
5143           IF(N-(CELL_NCELLY-1).GT.0) GO TO 5142                      136
C          Check M.EQ.N                                                   137
               IF (M.EQ.N) THEN                                           137
C          Create HCES :                                                  138
C          Call To Local Procedure HCES                                   138
               ASSIGN 5160 TO BLOCK_HCES                                  138
               GO TO  5150                                                138
5160           CONTINUE                                                   138
C          *                                                              138
C          Geant POSITION : HCES ===> X=-3.33+(3.33*M) Y=-3.33+(N*3.33)   139
C          Z=0.0 <===                                                     139
               CALL AGSRESET                                              139
               AG_X = -3.33+(3.33*M)                                      139
               AG_Y = -3.33+(N*3.33)                                      139
               AG_Z = 0.0                                                 139
               AG_TITLE = 'HCES'                                          139
               AG_EXNAME = 'POSITION'                                     139
               AG_PARLIST = 'X_Y_Z'                                       139
               CALL AXPOSITION                                            139
C          *                                                              139
               ELSE                                                       139
C          Create HCEL :                                                  141
C          Call To Local Procedure HCEL                                   141
               ASSIGN 5180 TO BLOCK_HCEL                                  141
               GO TO  5170                                                141
5180           CONTINUE                                                   141
C          *                                                              141
C          Geant POSITION : HCEL ===> X=-3.33+(3.33*M) Y=-3.33+(N*3.33)   142
C          Z=0.0 <===                                                     142
               CALL AGSRESET                                              142
               AG_X = -3.33+(3.33*M)                                      142
               AG_Y = -3.33+(N*3.33)                                      142
               AG_Z = 0.0                                                 142
               AG_TITLE = 'HCEL'                                          142
               AG_EXNAME = 'POSITION'                                     142
               AG_PARLIST = 'X_Y_Z'                                       142
               CALL AXPOSITION                                            142
C          *                                                              142
               ENDIF                                                      142
               YTEMP=FIBR_STARTY2                                         143
            GO TO 5141                                                    145
5142        CONTINUE                                                      145
            XTEMP=FIBR_STARTX2                                            145
         GO TO 5131                                                       147
5132     CONTINUE                                                         147
5051  CALL AGSPOPB                                                        148
      GO TO BLOCK_HTWR, (5050 ,5060)                                      148
C End of Local Procedure                                                  148
C ----------------------------------------------------------------------  149
C ------------ HCES                                                       149
C Begin Local Procedure HCES                                              150
C HCES 3*3 CELL IN TOWER, HCES HAS 15*15 FIBERS                           150
5150  CONTINUE                                                            150
         AG_TITLE='HCES'                                                  150
         AG_MODULE=MODULE                                                 150
         AG_MARK='V00'                                                    150
         CALL AGSPUSH                                                     150
         AG_MODULE=MODULE                                                 150
C    *                                                                    150
C    Geant MATERIAL : LEAD ===> inherited arguments used <===             151
         CALL AGSRESET                                                    151
         AG_TITLE = 'LEAD'                                                151
         AG_EXNAME = 'MATERIAL'                                           151
         AG_PARLIST = 'NONE'                                              151
         CALL AXMATERIAL                                                  151
C    *                                                                    151
C    Geant MATERIAL : LEAD ===> ISVOL=1 <===                              151
         CALL AGSRESET                                                    152
         AG_ISVOL = 1                                                     152
         AG_TITLE = 'LEAD'                                                152
         AG_EXNAME = 'MATERIAL'                                           152
         AG_PARLIST = 'ISVOL'                                             152
         CALL AXMATERIAL                                                  152
C    *                                                                    152
C    Geant ATTRIBUTE : HCES ===> SEEN=1 COLO=4 <===                       152
         CALL AGSRESET                                                    153
         AG_SEEN = 1                                                      153
         AG_COLO = 4                                                      153
         AG_TITLE = 'HCES'                                                153
         AG_EXNAME = 'ATTRIBUTE'                                          153
         AG_PARLIST = 'SEEN_COLO'                                         153
         CALL AXATTRIBUTE                                                 153
C    *                                                                    153
C    Geant SHAPE : BOX ===> DX=CELL_DX DY=CELL_DY DZ=CELL_DZ <===         153
         CALL AGSRESET                                                    154
         AG_DX = CELL_DX                                                  154
         AG_DY = CELL_DY                                                  154
         AG_DZ = CELL_DZ                                                  154
         AG_TITLE = 'BOX'                                                 154
         AG_EXNAME = 'SHAPE'                                              154
         AG_PARLIST = 'DX_DY_DZ'                                          154
         CALL AXSHAPE                                                     154
C    *                                                                    154
C    Create HCSX :                                                        155
C    Call To Local Procedure HCSX                                         155
         ASSIGN 5200 TO BLOCK_HCSX                                        155
         GO TO  5190                                                      155
5200     CONTINUE                                                         155
C    *                                                                    155
C    Defining Hits: HCAL:0:S(0,100.0) ELOS:0:S(0,100.0) BIRK:0:S(0,100.0  156
C    )                                                                    156
         AG_TITLE = 'HCES'                                                156
         AG_EXNAME = 'HITS'                                               156
         AG_TYPE=AG_MAGIC                                                 156
         CALL AGSDETV('HCES')                                             156
C    *                                                                    156
         AG_PARLIST='HCAL'                                                156
         AG_TITLE = 'DETH'                                                156
         AG_NBITS = 0                                                     156
         AG_BIN=AG_MAGIC                                                  156
         AG_OPTION='S'                                                    156
         AG_FHMIN = 0                                                     156
         AG_FHMAX = 100.0                                                 156
         CALL AGSDETH (0)                                                 156
C    *                                                                    156
         AG_PARLIST='ELOS'                                                156
         AG_TITLE = 'DETH'                                                156
         AG_NBITS = 0                                                     156
         AG_BIN=AG_MAGIC                                                  156
         AG_OPTION='S'                                                    156
         AG_FHMIN = 0                                                     156
         AG_FHMAX = 100.0                                                 156
         CALL AGSDETH (0)                                                 156
C    *                                                                    156
         AG_PARLIST='BIRK'                                                156
         AG_TITLE = 'DETH'                                                156
         AG_NBITS = 0                                                     156
         AG_BIN=AG_MAGIC                                                  156
         AG_OPTION='S'                                                    156
         AG_FHMIN = 0                                                     156
         AG_FHMAX = 100.0                                                 156
         CALL AGSDETH (0)                                                 156
C    *                                                                    156
         CALL AGSDETU('HCES')                                             156
C    *                                                                    156
5151  CALL AGSPOPB                                                        157
      GO TO BLOCK_HCES, (5150 ,5160)                                      157
C End of Local Procedure                                                  157
C ----------------------------------------------------------------------  158
C ------------ HCEL                                                       158
C Begin Local Procedure HCEL                                              159
C HCEL 3*3 CELL IN TOWER, HCES HAS 16*16 FIBERS                           159
5170  CONTINUE                                                            159
         AG_TITLE='HCEL'                                                  159
         AG_MODULE=MODULE                                                 159
         AG_MARK='V00'                                                    159
         CALL AGSPUSH                                                     159
         AG_MODULE=MODULE                                                 159
C    *                                                                    159
C    Geant MATERIAL : LEAD ===> inherited arguments used <===             160
         CALL AGSRESET                                                    160
         AG_TITLE = 'LEAD'                                                160
         AG_EXNAME = 'MATERIAL'                                           160
         AG_PARLIST = 'NONE'                                              160
         CALL AXMATERIAL                                                  160
C    *                                                                    160
C    Geant MATERIAL : LEAD ===> ISVOL=1 <===                              160
         CALL AGSRESET                                                    161
         AG_ISVOL = 1                                                     161
         AG_TITLE = 'LEAD'                                                161
         AG_EXNAME = 'MATERIAL'                                           161
         AG_PARLIST = 'ISVOL'                                             161
         CALL AXMATERIAL                                                  161
C    *                                                                    161
C    Geant ATTRIBUTE : HCEL ===> SEEN=1 COLO=4 <===                       161
         CALL AGSRESET                                                    162
         AG_SEEN = 1                                                      162
         AG_COLO = 4                                                      162
         AG_TITLE = 'HCEL'                                                162
         AG_EXNAME = 'ATTRIBUTE'                                          162
         AG_PARLIST = 'SEEN_COLO'                                         162
         CALL AXATTRIBUTE                                                 162
C    *                                                                    162
C    Geant SHAPE : BOX ===> DX=CELL_DX DY=CELL_DY DZ=CELL_DZ <===         162
         CALL AGSRESET                                                    163
         AG_DX = CELL_DX                                                  163
         AG_DY = CELL_DY                                                  163
         AG_DZ = CELL_DZ                                                  163
         AG_TITLE = 'BOX'                                                 163
         AG_EXNAME = 'SHAPE'                                              163
         AG_PARLIST = 'DX_DY_DZ'                                          163
         CALL AXSHAPE                                                     163
C    *                                                                    163
C    Create HCLX :                                                        164
C    Call To Local Procedure HCLX                                         164
         ASSIGN 5220 TO BLOCK_HCLX                                        164
         GO TO  5210                                                      164
5220     CONTINUE                                                         164
C    *                                                                    164
C    Defining Hits: HCAL:0:S(0,100.0) ELOS:0:S(0,100.0) BIRK:0:S(0,100.0  165
C    )                                                                    165
         AG_TITLE = 'HCEL'                                                165
         AG_EXNAME = 'HITS'                                               165
         AG_TYPE=AG_MAGIC                                                 165
         CALL AGSDETV('HCEL')                                             165
C    *                                                                    165
         AG_PARLIST='HCAL'                                                165
         AG_TITLE = 'DETH'                                                165
         AG_NBITS = 0                                                     165
         AG_BIN=AG_MAGIC                                                  165
         AG_OPTION='S'                                                    165
         AG_FHMIN = 0                                                     165
         AG_FHMAX = 100.0                                                 165
         CALL AGSDETH (0)                                                 165
C    *                                                                    165
         AG_PARLIST='ELOS'                                                165
         AG_TITLE = 'DETH'                                                165
         AG_NBITS = 0                                                     165
         AG_BIN=AG_MAGIC                                                  165
         AG_OPTION='S'                                                    165
         AG_FHMIN = 0                                                     165
         AG_FHMAX = 100.0                                                 165
         CALL AGSDETH (0)                                                 165
C    *                                                                    165
         AG_PARLIST='BIRK'                                                165
         AG_TITLE = 'DETH'                                                165
         AG_NBITS = 0                                                     165
         AG_BIN=AG_MAGIC                                                  165
         AG_OPTION='S'                                                    165
         AG_FHMIN = 0                                                     165
         AG_FHMAX = 100.0                                                 165
         CALL AGSDETH (0)                                                 165
C    *                                                                    165
         CALL AGSDETU('HCEL')                                             165
C    *                                                                    165
5171  CALL AGSPOPB                                                        166
      GO TO BLOCK_HCEL, (5170 ,5180)                                      166
C End of Local Procedure                                                  166
C ----------------------------------------------------------------------  167
C ------------ HCSX                                                       167
C Begin Local Procedure HCSX                                              168
C HCSX 3*3 CELL IN TOWER X DIVISIONS                                      168
5190  CONTINUE                                                            168
         AG_TITLE='HCSX'                                                  168
         AG_MODULE=MODULE                                                 168
         AG_MARK='V00'                                                    168
         CALL AGSPUSH                                                     168
         AG_MODULE=MODULE                                                 168
C    *                                                                    168
C    Geant MATERIAL : LEAD ===> ISVOL=0 <===                              169
         CALL AGSRESET                                                    169
         AG_ISVOL = 0                                                     169
         AG_TITLE = 'LEAD'                                                169
         AG_EXNAME = 'MATERIAL'                                           169
         AG_PARLIST = 'ISVOL'                                             169
         CALL AXMATERIAL                                                  169
C    *                                                                    169
C    Geant ATTRIBUTE : HCSX ===> SEEN=0 COLO=4 <===                       169
         CALL AGSRESET                                                    170
         AG_SEEN = 0                                                      170
         AG_COLO = 4                                                      170
         AG_TITLE = 'HCSX'                                                170
         AG_EXNAME = 'ATTRIBUTE'                                          170
         AG_PARLIST = 'SEEN_COLO'                                         170
         CALL AXATTRIBUTE                                                 170
C    *                                                                    170
C    Geant SHAPE : DIVISION ===> NDIV=CELL_NFIBERXS IAXIS=1 <===          170
         CALL AGSRESET                                                    171
         AG_NDIV = CELL_NFIBERXS                                          171
         AG_IAXIS = 1                                                     171
         AG_TITLE = 'DIVISION'                                            171
         AG_EXNAME = 'SHAPE'                                              171
         AG_PARLIST = 'NDIV_IAXIS'                                        171
         CALL AXSHAPE                                                     171
C    *                                                                    171
C    Create HCSY :                                                        172
C    Call To Local Procedure HCSY                                         172
         ASSIGN 5240 TO BLOCK_HCSY                                        172
         GO TO  5230                                                      172
5240     CONTINUE                                                         172
C    *                                                                    172
5191  CALL AGSPOPB                                                        173
      GO TO BLOCK_HCSX, (5190 ,5200)                                      173
C End of Local Procedure                                                  173
C ----------------------------------------------------------------------  174
C ------------ HCSY                                                       174
C Begin Local Procedure HCSY                                              175
C HCSY 3*3 CELL IN TOWER Y DIVISIONS                                      175
5230  CONTINUE                                                            175
         AG_TITLE='HCSY'                                                  175
         AG_MODULE=MODULE                                                 175
         AG_MARK='V00'                                                    175
         CALL AGSPUSH                                                     175
         AG_MODULE=MODULE                                                 175
C    *                                                                    175
C    Geant MATERIAL : LEAD ===> ISVOL=0 <===                              176
         CALL AGSRESET                                                    176
         AG_ISVOL = 0                                                     176
         AG_TITLE = 'LEAD'                                                176
         AG_EXNAME = 'MATERIAL'                                           176
         AG_PARLIST = 'ISVOL'                                             176
         CALL AXMATERIAL                                                  176
C    *                                                                    176
C    Geant ATTRIBUTE : HCSY ===> SEEN=0 COLO=4 <===                       176
         CALL AGSRESET                                                    177
         AG_SEEN = 0                                                      177
         AG_COLO = 4                                                      177
         AG_TITLE = 'HCSY'                                                177
         AG_EXNAME = 'ATTRIBUTE'                                          177
         AG_PARLIST = 'SEEN_COLO'                                         177
         CALL AXATTRIBUTE                                                 177
C    *                                                                    177
C    Geant SHAPE : DIVISION ===> NDIV=CELL_NFIBERYS IAXIS=2 <===          177
         CALL AGSRESET                                                    178
         AG_NDIV = CELL_NFIBERYS                                          178
         AG_IAXIS = 2                                                     178
         AG_TITLE = 'DIVISION'                                            178
         AG_EXNAME = 'SHAPE'                                              178
         AG_PARLIST = 'NDIV_IAXIS'                                        178
         CALL AXSHAPE                                                     178
C    *                                                                    178
C    Create HFIB :                                                        179
C    Call To Local Procedure HFIB                                         179
         ASSIGN 5260 TO BLOCK_HFIB                                        179
         GO TO  5250                                                      179
5260     CONTINUE                                                         179
C    *                                                                    179
C    Geant POSITION : HFIB ===> X=0 Y=0 Z=0.0 <===                        180
         CALL AGSRESET                                                    180
         AG_X = 0                                                         180
         AG_Y = 0                                                         180
         AG_Z = 0.0                                                       180
         AG_TITLE = 'HFIB'                                                180
         AG_EXNAME = 'POSITION'                                           180
         AG_PARLIST = 'X_Y_Z'                                             180
         CALL AXPOSITION                                                  180
C    *                                                                    180
5231  CALL AGSPOPB                                                        181
      GO TO BLOCK_HCSY, (5230 ,5240)                                      181
C End of Local Procedure                                                  181
C ----------------------------------------------------------------------  182
C ------------ HCLX                                                       182
C Begin Local Procedure HCLX                                              183
C HCLX 3*3 CELL IN TOWER X DIVISIONS                                      183
5210  CONTINUE                                                            183
         AG_TITLE='HCLX'                                                  183
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
C    Geant ATTRIBUTE : HCLX ===> SEEN=0 COLO=4 <===                       184
         CALL AGSRESET                                                    185
         AG_SEEN = 0                                                      185
         AG_COLO = 4                                                      185
         AG_TITLE = 'HCLX'                                                185
         AG_EXNAME = 'ATTRIBUTE'                                          185
         AG_PARLIST = 'SEEN_COLO'                                         185
         CALL AXATTRIBUTE                                                 185
C    *                                                                    185
C    Geant SHAPE : DIVISION ===> NDIV=CELL_NFIBERXL IAXIS=1 <===          185
         CALL AGSRESET                                                    186
         AG_NDIV = CELL_NFIBERXL                                          186
         AG_IAXIS = 1                                                     186
         AG_TITLE = 'DIVISION'                                            186
         AG_EXNAME = 'SHAPE'                                              186
         AG_PARLIST = 'NDIV_IAXIS'                                        186
         CALL AXSHAPE                                                     186
C    *                                                                    186
C    Create HCLY :                                                        187
C    Call To Local Procedure HCLY                                         187
         ASSIGN 5280 TO BLOCK_HCLY                                        187
         GO TO  5270                                                      187
5280     CONTINUE                                                         187
C    *                                                                    187
5211  CALL AGSPOPB                                                        188
      GO TO BLOCK_HCLX, (5210 ,5220)                                      188
C End of Local Procedure                                                  188
C ----------------------------------------------------------------------  189
C ------------ HCLY                                                       189
C Begin Local Procedure HCLY                                              190
C HCLY 3*3 CELL IN TOWER Y DIVISIONS                                      190
5270  CONTINUE                                                            190
         AG_TITLE='HCLY'                                                  190
         AG_MODULE=MODULE                                                 190
         AG_MARK='V00'                                                    190
         CALL AGSPUSH                                                     190
         AG_MODULE=MODULE                                                 190
C    *                                                                    190
C    Geant MATERIAL : LEAD ===> ISVOL=0 <===                              191
         CALL AGSRESET                                                    191
         AG_ISVOL = 0                                                     191
         AG_TITLE = 'LEAD'                                                191
         AG_EXNAME = 'MATERIAL'                                           191
         AG_PARLIST = 'ISVOL'                                             191
         CALL AXMATERIAL                                                  191
C    *                                                                    191
C    Geant ATTRIBUTE : HCLY ===> SEEN=1 COLO=4 <===                       191
         CALL AGSRESET                                                    192
         AG_SEEN = 1                                                      192
         AG_COLO = 4                                                      192
         AG_TITLE = 'HCLY'                                                192
         AG_EXNAME = 'ATTRIBUTE'                                          192
         AG_PARLIST = 'SEEN_COLO'                                         192
         CALL AXATTRIBUTE                                                 192
C    *                                                                    192
C    Geant SHAPE : DIVISION ===> NDIV=CELL_NFIBERYL IAXIS=2 <===          192
         CALL AGSRESET                                                    193
         AG_NDIV = CELL_NFIBERYL                                          193
         AG_IAXIS = 2                                                     193
         AG_TITLE = 'DIVISION'                                            193
         AG_EXNAME = 'SHAPE'                                              193
         AG_PARLIST = 'NDIV_IAXIS'                                        193
         CALL AXSHAPE                                                     193
C    *                                                                    193
C    Create HFIC :                                                        194
C    Call To Local Procedure HFIC                                         194
         ASSIGN 5300 TO BLOCK_HFIC                                        194
         GO TO  5290                                                      194
5300     CONTINUE                                                         194
C    *                                                                    194
C    Geant POSITION : HFIC ===> X=0 Y=0 Z=0.0 <===                        195
         CALL AGSRESET                                                    195
         AG_X = 0                                                         195
         AG_Y = 0                                                         195
         AG_Z = 0.0                                                       195
         AG_TITLE = 'HFIC'                                                195
         AG_EXNAME = 'POSITION'                                           195
         AG_PARLIST = 'X_Y_Z'                                             195
         CALL AXPOSITION                                                  195
C    *                                                                    195
5271  CALL AGSPOPB                                                        196
      GO TO BLOCK_HCLY, (5270 ,5280)                                      196
C End of Local Procedure                                                  196
C ----------------------------------------------------------------------  197
C ------------ HFIB                                                       197
C Begin Local Procedure HFIB                                              198
C HFIB FIBER IN CELLS                                                     198
5250  CONTINUE                                                            198
         AG_TITLE='HFIB'                                                  198
         AG_MODULE=MODULE                                                 198
         AG_MARK='V00'                                                    198
         CALL AGSPUSH                                                     198
         AG_MODULE=MODULE                                                 198
C    *                                                                    198
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       199
         CALL AGSRESET                                                    199
         AG_TITLE = 'POLYSTYREN'                                          199
         AG_EXNAME = 'MATERIAL'                                           199
         AG_PARLIST = 'NONE'                                              199
         CALL AXMATERIAL                                                  199
C    *                                                                    199
C    Geant MATERIAL : SENSPOLY ===> ISVOL=1 <===                          199
         CALL AGSRESET                                                    200
         AG_ISVOL = 1                                                     200
         AG_TITLE = 'SENSPOLY'                                            200
         AG_EXNAME = 'MATERIAL'                                           200
         AG_PARLIST = 'ISVOL'                                             200
         CALL AXMATERIAL                                                  200
C    *                                                                    200
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           200
         CALL AGSRESET                                                    201
         AG_TITLE = 'STANDARD'                                            201
         AG_EXNAME = 'MEDIUM'                                             201
         AG_PARLIST = 'NONE'                                              201
         CALL AXMEDIUM                                                    201
C    *                                                                    201
C    Geant ATTRIBUTE : HFIB ===> SEEN=0 COLO=7 <===                       201
         CALL AGSRESET                                                    202
         AG_SEEN = 0                                                      202
         AG_COLO = 7                                                      202
         AG_TITLE = 'HFIB'                                                202
         AG_EXNAME = 'ATTRIBUTE'                                          202
         AG_PARLIST = 'SEEN_COLO'                                         202
         CALL AXATTRIBUTE                                                 202
C    *                                                                    202
C    Geant SHAPE : TUBE ===> RMIN=FIBR_RMIN RMAX=FIBR_RMAX, DZ=FIBR_DZ <  202
C    ===                                                                  202
         CALL AGSRESET                                                    204
         AG_RMIN = FIBR_RMIN                                              204
         AG_RMAX = FIBR_RMAX                                              204
         AG_DZ = FIBR_DZ                                                  204
         AG_TITLE = 'TUBE'                                                204
         AG_EXNAME = 'SHAPE'                                              204
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      204
         CALL AXSHAPE                                                     204
C    *                                                                    204
5251  CALL AGSPOPB                                                        205
      GO TO BLOCK_HFIB, (5250 ,5260)                                      205
C End of Local Procedure                                                  205
C ----------------------------------------------------------------------  206
C ------------ HFIC                                                       206
C Begin Local Procedure HFIC                                              207
C HFIC FIBER IN CELLS                                                     207
5290  CONTINUE                                                            207
         AG_TITLE='HFIC'                                                  207
         AG_MODULE=MODULE                                                 207
         AG_MARK='V00'                                                    207
         CALL AGSPUSH                                                     207
         AG_MODULE=MODULE                                                 207
C    *                                                                    207
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       208
         CALL AGSRESET                                                    208
         AG_TITLE = 'POLYSTYREN'                                          208
         AG_EXNAME = 'MATERIAL'                                           208
         AG_PARLIST = 'NONE'                                              208
         CALL AXMATERIAL                                                  208
C    *                                                                    208
C    Geant MATERIAL : SENSPOLY ===> ISVOL=1 <===                          208
         CALL AGSRESET                                                    209
         AG_ISVOL = 1                                                     209
         AG_TITLE = 'SENSPOLY'                                            209
         AG_EXNAME = 'MATERIAL'                                           209
         AG_PARLIST = 'ISVOL'                                             209
         CALL AXMATERIAL                                                  209
C    *                                                                    209
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           209
         CALL AGSRESET                                                    210
         AG_TITLE = 'STANDARD'                                            210
         AG_EXNAME = 'MEDIUM'                                             210
         AG_PARLIST = 'NONE'                                              210
         CALL AXMEDIUM                                                    210
C    *                                                                    210
C    Geant ATTRIBUTE : HFIC ===> SEEN=0 COLO=7 <===                       210
         CALL AGSRESET                                                    211
         AG_SEEN = 0                                                      211
         AG_COLO = 7                                                      211
         AG_TITLE = 'HFIC'                                                211
         AG_EXNAME = 'ATTRIBUTE'                                          211
         AG_PARLIST = 'SEEN_COLO'                                         211
         CALL AXATTRIBUTE                                                 211
C    *                                                                    211
C    Geant SHAPE : TUBE ===> RMIN=FIBR_RMIN RMAX=FIBR_RMAX, DZ=FIBR_DZ <  211
C    ===                                                                  211
         CALL AGSRESET                                                    213
         AG_RMIN = FIBR_RMIN                                              213
         AG_RMAX = FIBR_RMAX                                              213
         AG_DZ = FIBR_DZ                                                  213
         AG_TITLE = 'TUBE'                                                213
         AG_EXNAME = 'SHAPE'                                              213
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      213
         CALL AXSHAPE                                                     213
C    *                                                                    213
5291  CALL AGSPOPB                                                        214
      GO TO BLOCK_HFIC, (5290 ,5300)                                      214
C End of Local Procedure                                                  214
C ----------------------------------------------------------------------  215
C ------------ LEDG                                                       215
C Begin Local Procedure LEDG                                              216
C LEDG LEAD GLASS                                                         216
5070  CONTINUE                                                            216
         AG_TITLE='LEDG'                                                  216
         AG_MODULE=MODULE                                                 216
         AG_MARK='V00'                                                    216
         CALL AGSPUSH                                                     216
         AG_MODULE=MODULE                                                 216
C    *                                                                    216
C    Geant COMPONENT : PB ===> A=207.19 Z=82 W=.41774 <===                217
         CALL AGSRESET                                                    217
         AG_A = 207.19                                                    217
         AG_Z = 82                                                        217
         AG_W = .41774                                                    217
         AG_TITLE = 'PB'                                                  217
         AG_EXNAME = 'COMPONENT'                                          217
         AG_PARLIST = 'A_Z_W'                                             217
         CALL AXCOMPONENT                                                 217
C    *                                                                    217
C    Geant COMPONENT : K ===> A=39.102 Z=19 W=.04151 <===                 217
         CALL AGSRESET                                                    218
         AG_A = 39.102                                                    218
         AG_Z = 19                                                        218
         AG_W = .04151                                                    218
         AG_TITLE = 'K'                                                   218
         AG_EXNAME = 'COMPONENT'                                          218
         AG_PARLIST = 'A_Z_W'                                             218
         CALL AXCOMPONENT                                                 218
C    *                                                                    218
C    Geant COMPONENT : SI ===> A=28.088 Z=14 W=.21047 <===                218
         CALL AGSRESET                                                    219
         AG_A = 28.088                                                    219
         AG_Z = 14                                                        219
         AG_W = .21047                                                    219
         AG_TITLE = 'SI'                                                  219
         AG_EXNAME = 'COMPONENT'                                          219
         AG_PARLIST = 'A_Z_W'                                             219
         CALL AXCOMPONENT                                                 219
C    *                                                                    219
C    Geant COMPONENT : O ===> A=15.999 Z=8 W=.29330 <===                  219
         CALL AGSRESET                                                    220
         AG_A = 15.999                                                    220
         AG_Z = 8                                                         220
         AG_W = .29330                                                    220
         AG_TITLE = 'O'                                                   220
         AG_EXNAME = 'COMPONENT'                                          220
         AG_PARLIST = 'A_Z_W'                                             220
         CALL AXCOMPONENT                                                 220
C    *                                                                    220
C    Geant COMPONENT : NA ===> A=22.990 Z=11 W=.03710 <===                220
         CALL AGSRESET                                                    221
         AG_A = 22.990                                                    221
         AG_Z = 11                                                        221
         AG_W = .03710                                                    221
         AG_TITLE = 'NA'                                                  221
         AG_EXNAME = 'COMPONENT'                                          221
         AG_PARLIST = 'A_Z_W'                                             221
         CALL AXCOMPONENT                                                 221
C    *                                                                    221
C    Geant MIXTURE : F2 ===> DENS=LGLA_DENSITY <===                       221
         CALL AGSRESET                                                    222
         AG_DENS = LGLA_DENSITY                                           222
         AG_TITLE = 'F2'                                                  222
         AG_EXNAME = 'MIXTURE'                                            222
         AG_PARLIST = 'DENS'                                              222
         CALL AXMIXTURE                                                   222
C    *                                                                    222
C    Geant MATERIAL : F2 ===> ISVOL=1 <===                                222
         CALL AGSRESET                                                    223
         AG_ISVOL = 1                                                     223
         AG_TITLE = 'F2'                                                  223
         AG_EXNAME = 'MATERIAL'                                           223
         AG_PARLIST = 'ISVOL'                                             223
         CALL AXMATERIAL                                                  223
C    *                                                                    223
C    Geant MEDIUM : LEADGLASS ===> ISVOL=1 <===                           223
         CALL AGSRESET                                                    224
         AG_ISVOL = 1                                                     224
         AG_TITLE = 'LEADGLASS'                                           224
         AG_EXNAME = 'MEDIUM'                                             224
         AG_PARLIST = 'ISVOL'                                             224
         CALL AXMEDIUM                                                    224
C    *                                                                    224
C    Geant ATTRIBUTE : LEDG ===> SEEN=1 COLO=4 <===                       224
         CALL AGSRESET                                                    225
         AG_SEEN = 1                                                      225
         AG_COLO = 4                                                      225
         AG_TITLE = 'LEDG'                                                225
         AG_EXNAME = 'ATTRIBUTE'                                          225
         AG_PARLIST = 'SEEN_COLO'                                         225
         CALL AXATTRIBUTE                                                 225
C    *                                                                    225
C    Geant SHAPE : BOX ===> DX=LGLA_DX DY=LGLA_DY DZ=LGLA_DZ <===         225
         CALL AGSRESET                                                    226
         AG_DX = LGLA_DX                                                  226
         AG_DY = LGLA_DY                                                  226
         AG_DZ = LGLA_DZ                                                  226
         AG_TITLE = 'BOX'                                                 226
         AG_EXNAME = 'SHAPE'                                              226
         AG_PARLIST = 'DX_DY_DZ'                                          226
         CALL AXSHAPE                                                     226
C    *                                                                    226
C    Defining Hits: ELOS:0:C(0,250)                                       226
         AG_TITLE = 'LEDG'                                                227
         AG_EXNAME = 'HITS'                                               227
         AG_TYPE=AG_MAGIC                                                 227
         CALL AGSDETV('LEDG')                                             227
C    *                                                                    227
         AG_PARLIST='ELOS'                                                227
         AG_TITLE = 'DETH'                                                227
         AG_NBITS = 0                                                     227
         AG_BIN=AG_MAGIC                                                  227
         AG_OPTION='C'                                                    227
         AG_FHMIN = 0                                                     227
         AG_FHMAX = 250                                                   227
         CALL AGSDETH (0)                                                 227
C    *                                                                    227
         CALL AGSDETU('LEDG')                                             227
C    *                                                                    227
5071  CALL AGSPOPB                                                        228
      GO TO BLOCK_LEDG, (5070 ,5080)                                      228
C End of Local Procedure                                                  228
C ----------------------------------------------------------------------  229
C ------------ BBCF                                                       229
C Begin Local Procedure BBCF                                              230
C BBCF BEAM COUNTER FRONT                                                 230
5090  CONTINUE                                                            230
         AG_TITLE='BBCF'                                                  230
         AG_MODULE=MODULE                                                 230
         AG_MARK='V00'                                                    230
         CALL AGSPUSH                                                     230
         AG_MODULE=MODULE                                                 230
C    *                                                                    230
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       231
         CALL AGSRESET                                                    231
         AG_TITLE = 'POLYSTYREN'                                          231
         AG_EXNAME = 'MATERIAL'                                           231
         AG_PARLIST = 'NONE'                                              231
         CALL AXMATERIAL                                                  231
C    *                                                                    231
C    Geant MATERIAL : POLYSTYREN ===> ISVOL=1 <===                        231
         CALL AGSRESET                                                    232
         AG_ISVOL = 1                                                     232
         AG_TITLE = 'POLYSTYREN'                                          232
         AG_EXNAME = 'MATERIAL'                                           232
         AG_PARLIST = 'ISVOL'                                             232
         CALL AXMATERIAL                                                  232
C    *                                                                    232
C    Geant ATTRIBUTE : BBCF ===> SEEN=1 COLO=4 <===                       232
         CALL AGSRESET                                                    233
         AG_SEEN = 1                                                      233
         AG_COLO = 4                                                      233
         AG_TITLE = 'BBCF'                                                233
         AG_EXNAME = 'ATTRIBUTE'                                          233
         AG_PARLIST = 'SEEN_COLO'                                         233
         CALL AXATTRIBUTE                                                 233
C    *                                                                    233
C    Geant SHAPE : TRD1 ===> DX1=TRAP_DX1 DX2=TRAP_DX2 DY=TRAP_DY, DZ=TR  233
C    AP_DZ <===                                                           233
         CALL AGSRESET                                                    235
         AG_DX1 = TRAP_DX1                                                235
         AG_DX2 = TRAP_DX2                                                235
         AG_DY = TRAP_DY                                                  235
         AG_DZ = TRAP_DZ                                                  235
         AG_TITLE = 'TRD1'                                                235
         AG_EXNAME = 'SHAPE'                                              235
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     235
         CALL AXSHAPE                                                     235
C    *                                                                    235
C    Defining Hits: BIRK:0:C(0,250) TOF:16:C(0,1.0E-6)                    235
         AG_TITLE = 'BBCF'                                                236
         AG_EXNAME = 'HITS'                                               236
         AG_TYPE=AG_MAGIC                                                 236
         CALL AGSDETV('BBCF')                                             236
C    *                                                                    236
         AG_PARLIST='BIRK'                                                236
         AG_TITLE = 'DETH'                                                236
         AG_NBITS = 0                                                     236
         AG_BIN=AG_MAGIC                                                  236
         AG_OPTION='C'                                                    236
         AG_FHMIN = 0                                                     236
         AG_FHMAX = 250                                                   236
         CALL AGSDETH (0)                                                 236
C    *                                                                    236
         AG_PARLIST='TOF'                                                 236
         AG_TITLE = 'DETH'                                                236
         AG_NBITS = 16                                                    236
         AG_BIN=AG_MAGIC                                                  236
         AG_OPTION='C'                                                    236
         AG_FHMIN = 0                                                     236
         AG_FHMAX = 1.0E-6                                                236
         CALL AGSDETH (0)                                                 236
C    *                                                                    236
         CALL AGSDETU('BBCF')                                             236
C    *                                                                    236
5091  CALL AGSPOPB                                                        237
      GO TO BLOCK_BBCF, (5090 ,5100)                                      237
C End of Local Procedure                                                  237
C ----------------------------------------------------------------------  238
C ------------ BBCB                                                       238
C Begin Local Procedure BBCB                                              239
C BBCB BEAM COUNTER FRONT                                                 239
5110  CONTINUE                                                            239
         AG_TITLE='BBCB'                                                  239
         AG_MODULE=MODULE                                                 239
         AG_MARK='V00'                                                    239
         CALL AGSPUSH                                                     239
         AG_MODULE=MODULE                                                 239
C    *                                                                    239
C    Geant MATERIAL : POLYSTYREN ===> inherited arguments used <===       240
         CALL AGSRESET                                                    240
         AG_TITLE = 'POLYSTYREN'                                          240
         AG_EXNAME = 'MATERIAL'                                           240
         AG_PARLIST = 'NONE'                                              240
         CALL AXMATERIAL                                                  240
C    *                                                                    240
C    Geant MATERIAL : POLYSTYREN ===> ISVOL=1 <===                        240
         CALL AGSRESET                                                    241
         AG_ISVOL = 1                                                     241
         AG_TITLE = 'POLYSTYREN'                                          241
         AG_EXNAME = 'MATERIAL'                                           241
         AG_PARLIST = 'ISVOL'                                             241
         CALL AXMATERIAL                                                  241
C    *                                                                    241
C    Geant ATTRIBUTE : BBCB ===> SEEN=1 COLO=4 <===                       241
         CALL AGSRESET                                                    242
         AG_SEEN = 1                                                      242
         AG_COLO = 4                                                      242
         AG_TITLE = 'BBCB'                                                242
         AG_EXNAME = 'ATTRIBUTE'                                          242
         AG_PARLIST = 'SEEN_COLO'                                         242
         CALL AXATTRIBUTE                                                 242
C    *                                                                    242
C    Geant SHAPE : TRD1 ===> DX1=TRAP_DX1 DX2=TRAP_DX2 DY=TRAP_DY, DZ=TR  242
C    AP_DZ <===                                                           242
         CALL AGSRESET                                                    244
         AG_DX1 = TRAP_DX1                                                244
         AG_DX2 = TRAP_DX2                                                244
         AG_DY = TRAP_DY                                                  244
         AG_DZ = TRAP_DZ                                                  244
         AG_TITLE = 'TRD1'                                                244
         AG_EXNAME = 'SHAPE'                                              244
         AG_PARLIST = 'DX1_DX2_DY_DZ'                                     244
         CALL AXSHAPE                                                     244
C    *                                                                    244
C    Defining Hits: BIRK:0:C(0,250) TOF:16:C(0,1.0E-6)                    244
         AG_TITLE = 'BBCB'                                                245
         AG_EXNAME = 'HITS'                                               245
         AG_TYPE=AG_MAGIC                                                 245
         CALL AGSDETV('BBCB')                                             245
C    *                                                                    245
         AG_PARLIST='BIRK'                                                245
         AG_TITLE = 'DETH'                                                245
         AG_NBITS = 0                                                     245
         AG_BIN=AG_MAGIC                                                  245
         AG_OPTION='C'                                                    245
         AG_FHMIN = 0                                                     245
         AG_FHMAX = 250                                                   245
         CALL AGSDETH (0)                                                 245
C    *                                                                    245
         AG_PARLIST='TOF'                                                 245
         AG_TITLE = 'DETH'                                                245
         AG_NBITS = 16                                                    245
         AG_BIN=AG_MAGIC                                                  245
         AG_OPTION='C'                                                    245
         AG_FHMIN = 0                                                     245
         AG_FHMAX = 1.0E-6                                                245
         CALL AGSDETH (0)                                                 245
C    *                                                                    245
         CALL AGSDETU('BBCB')                                             245
C    *                                                                    245
5111  CALL AGSPOPB                                                        246
      GO TO BLOCK_BBCB, (5110 ,5120)                                      246
C End of Local Procedure                                                  246
      WRITE(LOUT,5310)'===>>  [End AgML Module HcalGeoF]  <<==='          246
5310  FORMAT(A32,/,/)                                                     246
C MODULE HCALGEOF                                                         248
      END                                                                 248
