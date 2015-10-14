C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE SHLDGEO                                                  1
C SHLDGEO IS THE SHIELDING                                                1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'SHLDGEO'/, BANK_TITLE/'*'/                             1
      DATA MCOMMENT/'IS THE SHIELDING'/                                   1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C CREATED 15-AUG-2005                                                     1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 CREATED /'15-AUG-2005'/                                2
C * . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .*  2
C AUTHOR MAXIM POTEKHIN                                                   2
      CHARACTER*40 AUTHOR /'MAXIM POTEKHIN'/                              3
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
      INTEGER BLOCK_SHLD                                                  5
      INTEGER BLOCK_SHBS                                                  6
      INTEGER BLOCK_SHLS                                                  6
      INTEGER BLOCK_SHBI                                                  6
      INTEGER BLOCK_SHFI                                                  6
      INTEGER BLOCK_SHOL                                                  6
      INTEGER BLOCK_SFLR                                                  6
C -                                                                       8
      REAL SHLG_VERSION                                                   8
      EQUIVALENCE (BANK_SHLG(3),SHLG_VERSION,RE_SHLG_VERSION),            8
     *(NAMES_SHLG(1),NSHLG_VERSION)                                       8
      EQUIVALENCE (COMMEN_SHLG(1),CSHLG_VERSION),(MAP_SHLG(1,1),          8
     *MSHLG_VERSION)                                                      8
      CHARACTER CSHLG_VERSION*40/' '/                                     8
      CHARACTER NSHLG_VERSION*12/'R.VERSION'/                             8
      INTEGER MSHLG_VERSION(2)/1,1/                                       8
      REAL RE_SHLG_VERSION                                                8
C -                                                                       8
      REAL SHLG_Z                                                         8
      EQUIVALENCE (BANK_SHLG(4),SHLG_Z,RE_SHLG_Z),(NAMES_SHLG(2),         8
     *NSHLG_Z)                                                            8
      EQUIVALENCE (COMMEN_SHLG(2),CSHLG_Z),(MAP_SHLG(1,2),MSHLG_Z)        8
      CHARACTER CSHLG_Z*40/' '/                                           8
      CHARACTER NSHLG_Z*12/'R.Z'/                                         8
      INTEGER MSHLG_Z(2)/1,1/                                             8
      REAL RE_SHLG_Z                                                      8
C -                                                                       8
      REAL SHLG_DX                                                        8
      EQUIVALENCE (BANK_SHLG(5),SHLG_DX,RE_SHLG_DX),(NAMES_SHLG(3),       8
     *NSHLG_DX)                                                           8
      EQUIVALENCE (COMMEN_SHLG(3),CSHLG_DX),(MAP_SHLG(1,3),MSHLG_DX)      8
      CHARACTER CSHLG_DX*40/' '/                                          8
      CHARACTER NSHLG_DX*12/'R.DX'/                                       8
      INTEGER MSHLG_DX(2)/1,1/                                            8
      REAL RE_SHLG_DX                                                     8
C -                                                                       8
      REAL SHLG_DY                                                        8
      EQUIVALENCE (BANK_SHLG(6),SHLG_DY,RE_SHLG_DY),(NAMES_SHLG(4),       8
     *NSHLG_DY)                                                           8
      EQUIVALENCE (COMMEN_SHLG(4),CSHLG_DY),(MAP_SHLG(1,4),MSHLG_DY)      8
      CHARACTER CSHLG_DY*40/' '/                                          8
      CHARACTER NSHLG_DY*12/'R.DY'/                                       8
      INTEGER MSHLG_DY(2)/1,1/                                            8
      REAL RE_SHLG_DY                                                     8
C -                                                                       8
      REAL SHLG_DZ                                                        8
      EQUIVALENCE (BANK_SHLG(7),SHLG_DZ,RE_SHLG_DZ),(NAMES_SHLG(5),       8
     *NSHLG_DZ)                                                           8
      EQUIVALENCE (COMMEN_SHLG(5),CSHLG_DZ),(MAP_SHLG(1,5),MSHLG_DZ)      8
      CHARACTER CSHLG_DZ*40/' '/                                          8
      CHARACTER NSHLG_DZ*12/'R.DZ'/                                       8
      INTEGER MSHLG_DZ(2)/1,1/                                            8
      REAL RE_SHLG_DZ                                                     8
C -                                                                       8
      REAL SHLG_BASELEVEL                                                 8
      EQUIVALENCE (BANK_SHLG(8),SHLG_BASELEVEL,RE_SHLG_BASELEVEL),        8
     *(NAMES_SHLG(6),NSHLG_BASELEVEL)                                     8
      EQUIVALENCE (COMMEN_SHLG(6),CSHLG_BASELEVEL),(MAP_SHLG(1,6),        8
     *MSHLG_BASELEVEL)                                                    8
      CHARACTER CSHLG_BASELEVEL*40/' '/                                   8
      CHARACTER NSHLG_BASELEVEL*12/'R.BASELEVEL'/                         8
      INTEGER MSHLG_BASELEVEL(2)/1,1/                                     8
      REAL RE_SHLG_BASELEVEL                                              8
C -                                                                       8
      REAL SHLG_BASEZ                                                     8
      EQUIVALENCE (BANK_SHLG(9),SHLG_BASEZ,RE_SHLG_BASEZ),                8
     *(NAMES_SHLG(7),NSHLG_BASEZ)                                         8
      EQUIVALENCE (COMMEN_SHLG(7),CSHLG_BASEZ),(MAP_SHLG(1,7),            8
     *MSHLG_BASEZ)                                                        8
      CHARACTER CSHLG_BASEZ*40/' '/                                       8
      CHARACTER NSHLG_BASEZ*12/'R.BASEZ'/                                 8
      INTEGER MSHLG_BASEZ(2)/1,1/                                         8
      REAL RE_SHLG_BASEZ                                                  8
C -                                                                       8
      REAL SHLG_BASEDX                                                    8
      EQUIVALENCE (BANK_SHLG(10),SHLG_BASEDX,RE_SHLG_BASEDX),             8
     *(NAMES_SHLG(8),NSHLG_BASEDX)                                        8
      EQUIVALENCE (COMMEN_SHLG(8),CSHLG_BASEDX),(MAP_SHLG(1,8),           8
     *MSHLG_BASEDX)                                                       8
      CHARACTER CSHLG_BASEDX*40/' '/                                      8
      CHARACTER NSHLG_BASEDX*12/'R.BASEDX'/                               8
      INTEGER MSHLG_BASEDX(2)/1,1/                                        8
      REAL RE_SHLG_BASEDX                                                 8
C -                                                                       8
      REAL SHLG_BASEDY                                                    8
      EQUIVALENCE (BANK_SHLG(11),SHLG_BASEDY,RE_SHLG_BASEDY),             8
     *(NAMES_SHLG(9),NSHLG_BASEDY)                                        8
      EQUIVALENCE (COMMEN_SHLG(9),CSHLG_BASEDY),(MAP_SHLG(1,9),           8
     *MSHLG_BASEDY)                                                       8
      CHARACTER CSHLG_BASEDY*40/' '/                                      8
      CHARACTER NSHLG_BASEDY*12/'R.BASEDY'/                               8
      INTEGER MSHLG_BASEDY(2)/1,1/                                        8
      REAL RE_SHLG_BASEDY                                                 8
C -                                                                       8
      REAL SHLG_SLABX                                                     8
      EQUIVALENCE (BANK_SHLG(12),SHLG_SLABX,RE_SHLG_SLABX),               8
     *(NAMES_SHLG(10),NSHLG_SLABX)                                        8
      EQUIVALENCE (COMMEN_SHLG(10),CSHLG_SLABX),(MAP_SHLG(1,10),          8
     *MSHLG_SLABX)                                                        8
      CHARACTER CSHLG_SLABX*40/' '/                                       8
      CHARACTER NSHLG_SLABX*12/'R.SLABX'/                                 8
      INTEGER MSHLG_SLABX(2)/1,1/                                         8
      REAL RE_SHLG_SLABX                                                  8
C -                                                                       8
      REAL SHLG_SLABZ                                                     8
      EQUIVALENCE (BANK_SHLG(13),SHLG_SLABZ,RE_SHLG_SLABZ),               8
     *(NAMES_SHLG(11),NSHLG_SLABZ)                                        8
      EQUIVALENCE (COMMEN_SHLG(11),CSHLG_SLABZ),(MAP_SHLG(1,11),          8
     *MSHLG_SLABZ)                                                        8
      CHARACTER CSHLG_SLABZ*40/' '/                                       8
      CHARACTER NSHLG_SLABZ*12/'R.SLABZ'/                                 8
      INTEGER MSHLG_SLABZ(2)/1,1/                                         8
      REAL RE_SHLG_SLABZ                                                  8
C -                                                                       8
      REAL SHLG_SLABDY                                                    8
      EQUIVALENCE (BANK_SHLG(14),SHLG_SLABDY,RE_SHLG_SLABDY),             8
     *(NAMES_SHLG(12),NSHLG_SLABDY)                                       8
      EQUIVALENCE (COMMEN_SHLG(12),CSHLG_SLABDY),(MAP_SHLG(1,12),         8
     *MSHLG_SLABDY)                                                       8
      CHARACTER CSHLG_SLABDY*40/' '/                                      8
      CHARACTER NSHLG_SLABDY*12/'R.SLABDY'/                               8
      INTEGER MSHLG_SLABDY(2)/1,1/                                        8
      REAL RE_SHLG_SLABDY                                                 8
C -                                                                       8
      REAL SHLG_SLABDZ                                                    8
      EQUIVALENCE (BANK_SHLG(15),SHLG_SLABDZ,RE_SHLG_SLABDZ),             8
     *(NAMES_SHLG(13),NSHLG_SLABDZ)                                       8
      EQUIVALENCE (COMMEN_SHLG(13),CSHLG_SLABDZ),(MAP_SHLG(1,13),         8
     *MSHLG_SLABDZ)                                                       8
      CHARACTER CSHLG_SLABDZ*40/' '/                                      8
      CHARACTER NSHLG_SLABDZ*12/'R.SLABDZ'/                               8
      INTEGER MSHLG_SLABDZ(2)/1,1/                                        8
      REAL RE_SHLG_SLABDZ                                                 8
C -                                                                       8
      REAL SHLG_FIDZ                                                      8
      EQUIVALENCE (BANK_SHLG(16),SHLG_FIDZ,RE_SHLG_FIDZ),                 8
     *(NAMES_SHLG(14),NSHLG_FIDZ)                                         8
      EQUIVALENCE (COMMEN_SHLG(14),CSHLG_FIDZ),(MAP_SHLG(1,14),           8
     *MSHLG_FIDZ)                                                         8
      CHARACTER CSHLG_FIDZ*40/' '/                                        8
      CHARACTER NSHLG_FIDZ*12/'R.FIDZ'/                                   8
      INTEGER MSHLG_FIDZ(2)/1,1/                                          8
      REAL RE_SHLG_FIDZ                                                   8
C -                                                                       8
      REAL SHLG_FIDY                                                      8
      EQUIVALENCE (BANK_SHLG(17),SHLG_FIDY,RE_SHLG_FIDY),                 8
     *(NAMES_SHLG(15),NSHLG_FIDY)                                         8
      EQUIVALENCE (COMMEN_SHLG(15),CSHLG_FIDY),(MAP_SHLG(1,15),           8
     *MSHLG_FIDY)                                                         8
      CHARACTER CSHLG_FIDY*40/' '/                                        8
      CHARACTER NSHLG_FIDY*12/'R.FIDY'/                                   8
      INTEGER MSHLG_FIDY(2)/1,1/                                          8
      REAL RE_SHLG_FIDY                                                   8
C -                                                                       8
      REAL SHLG_HOLEX                                                     8
      EQUIVALENCE (BANK_SHLG(18),SHLG_HOLEX,RE_SHLG_HOLEX),               8
     *(NAMES_SHLG(16),NSHLG_HOLEX)                                        8
      EQUIVALENCE (COMMEN_SHLG(16),CSHLG_HOLEX),(MAP_SHLG(1,16),          8
     *MSHLG_HOLEX)                                                        8
      CHARACTER CSHLG_HOLEX*40/' '/                                       8
      CHARACTER NSHLG_HOLEX*12/'R.HOLEX'/                                 8
      INTEGER MSHLG_HOLEX(2)/1,1/                                         8
      REAL RE_SHLG_HOLEX                                                  8
C -                                                                       8
      REAL SHLG_HOLEY                                                     8
      EQUIVALENCE (BANK_SHLG(19),SHLG_HOLEY,RE_SHLG_HOLEY),               8
     *(NAMES_SHLG(17),NSHLG_HOLEY)                                        8
      EQUIVALENCE (COMMEN_SHLG(17),CSHLG_HOLEY),(MAP_SHLG(1,17),          8
     *MSHLG_HOLEY)                                                        8
      CHARACTER CSHLG_HOLEY*40/' '/                                       8
      CHARACTER NSHLG_HOLEY*12/'R.HOLEY'/                                 8
      INTEGER MSHLG_HOLEY(2)/1,1/                                         8
      REAL RE_SHLG_HOLEY                                                  8
C -                                                                       8
      REAL SHLG_FLOORTHK                                                  8
      EQUIVALENCE (BANK_SHLG(20),SHLG_FLOORTHK,RE_SHLG_FLOORTHK),         8
     *(NAMES_SHLG(18),NSHLG_FLOORTHK)                                     8
      EQUIVALENCE (COMMEN_SHLG(18),CSHLG_FLOORTHK),(MAP_SHLG(1,18),       8
     *MSHLG_FLOORTHK)                                                     8
      CHARACTER CSHLG_FLOORTHK*40/' '/                                    8
      CHARACTER NSHLG_FLOORTHK*12/'R.FLOORTHK'/                           8
      INTEGER MSHLG_FLOORTHK(2)/1,1/                                      8
      REAL RE_SHLG_FLOORTHK                                               8
C -                                                                       8
      REAL SHLG_FLOORLEN                                                  8
      EQUIVALENCE (BANK_SHLG(21),SHLG_FLOORLEN,RE_SHLG_FLOORLEN),         8
     *(NAMES_SHLG(19),NSHLG_FLOORLEN)                                     8
      EQUIVALENCE (COMMEN_SHLG(19),CSHLG_FLOORLEN),(MAP_SHLG(1,19),       8
     *MSHLG_FLOORLEN)                                                     8
      CHARACTER CSHLG_FLOORLEN*40/' '/                                    8
      CHARACTER NSHLG_FLOORLEN*12/'R.FLOORLEN'/                           8
      INTEGER MSHLG_FLOORLEN(2)/1,1/                                      8
      REAL RE_SHLG_FLOORLEN                                               8
C -                                                                       8
      REAL SHLG_FLOORWIDTH                                                8
      EQUIVALENCE (BANK_SHLG(22),SHLG_FLOORWIDTH,RE_SHLG_FLOORWIDTH),     8
     *(NAMES_SHLG(20),NSHLG_FLOORWIDTH)                                   8
      EQUIVALENCE (COMMEN_SHLG(20),CSHLG_FLOORWIDTH),(MAP_SHLG(1,20),     8
     *MSHLG_FLOORWIDTH)                                                   8
      CHARACTER CSHLG_FLOORWIDTH*40/' '/                                  8
      CHARACTER NSHLG_FLOORWIDTH*12/'R.FLOORWIDTH'/                       8
      INTEGER MSHLG_FLOORWIDTH(2)/1,1/                                    8
      REAL RE_SHLG_FLOORWIDTH                                             8
C -                                                                       8
      REAL SHLG_FLOORPOS                                                  8
      EQUIVALENCE (BANK_SHLG(23),SHLG_FLOORPOS,RE_SHLG_FLOORPOS),         8
     *(NAMES_SHLG(21),NSHLG_FLOORPOS)                                     8
      EQUIVALENCE (COMMEN_SHLG(21),CSHLG_FLOORPOS),(MAP_SHLG(1,21),       8
     *MSHLG_FLOORPOS)                                                     8
      CHARACTER CSHLG_FLOORPOS*40/' '/                                    8
      CHARACTER NSHLG_FLOORPOS*12/'R.FLOORPOS'/                           8
      INTEGER MSHLG_FLOORPOS(2)/1,1/                                      8
      REAL RE_SHLG_FLOORPOS                                               8
C -                                                                       8
      INTEGER LEN_SHLG,LLEN_SHLG,LINK_SHLG                                8
      PARAMETER (LEN_SHLG=23)                                             8
      PARAMETER (LLEN_SHLG=21)                                            8
      INTEGER FLAG_SHLG/0/,MAP_SHLG(2,LLEN_SHLG),IDNUM_SHLG(10)           8
      CHARACTER NAMES_SHLG(LLEN_SHLG)*12,COMMEN_SHLG(LLEN_SHLG)*40        8
      DATA IDNUM_SHLG/10*0/                                               8
      REAL BANK_SHLG(LEN_SHLG)                                            8
      CHARACTER*60 BPATH_SHLG/' '/                                        8
      SAVE FLAG_SHLG,MAP_SHLG,IDNUM_SHLG,NAMES_SHLG,COMMEN_SHLG,          8
     *BPATH_SHLG                                                          8
      DATA LINK_SHLG/0/,BANK_SHLG/LEN_SHLG*0/                             8
C *                                                                       8
      REAL YSLAB,SHIELDHALFHEIGHT,YFI                                     9
C *                                                                       11
      CALL AGSBEGM(MODULE,IPRIN)                                          11
      IF (IPRIN.LT.0) RETURN                                              11
C Filling bank SHLG - SHIELDING GEOMETRY DATA                             11
      BANK_TITLE='SHIELDING GEOMETRY DATA'                                11
      BPATH_SHLG='SHLG*'                                                  11
      IDNUM_SHLG(1)=0                                                     12
C GEOMETRY VERSION                                                        12
      SHLG_VERSION=1                                                      12
      CSHLG_VERSION='GEOMETRY VERSION'                                    12
C POSITION OF THE SHIELDING                                               13
      SHLG_Z=1750                                                         13
      CSHLG_Z='POSITION OF THE SHIELDING'                                 13
C HALF-X DIMENSION                                                        14
      SHLG_DX=170                                                         14
      CSHLG_DX='HALF-X DIMENSION'                                         14
C HALF-Y DIMENSION                                                        15
      SHLG_DY=150                                                         15
      CSHLG_DY='HALF-Y DIMENSION'                                         15
C HALF-Z DIMENSION                                                        16
      SHLG_DZ=100                                                         16
      CSHLG_DZ='HALF-Z DIMENSION'                                         16
C BASE POSITION ON THE FLOOR                                              17
      SHLG_BASELEVEL=-125                                                 17
      CSHLG_BASELEVEL='BASE POSITION ON THE FLOOR'                        17
C BASE POSITION                                                           18
      SHLG_BASEZ=0                                                        18
      CSHLG_BASEZ='BASE POSITION'                                         18
C BASE HALF-X DIMENSION                                                   19
      SHLG_BASEDX=60                                                      19
      CSHLG_BASEDX='BASE HALF-X DIMENSION'                                19
C BASE HALF-Y DIMENSION                                                   20
      SHLG_BASEDY=30                                                      20
      CSHLG_BASEDY='BASE HALF-Y DIMENSION'                                20
C SLAB POSITION                                                           21
      SHLG_SLABX=0                                                        21
      CSHLG_SLABX='SLAB POSITION'                                         21
C SLAB POSITION                                                           22
      SHLG_SLABZ=-30                                                      22
      CSHLG_SLABZ='SLAB POSITION'                                         22
C SLAB HALF-Y DIMENSION                                                   23
      SHLG_SLABDY=30                                                      23
      CSHLG_SLABDY='SLAB HALF-Y DIMENSION'                                23
C SLAB HALF-Z DIMENSION                                                   24
      SHLG_SLABDZ=30                                                      24
      CSHLG_SLABDZ='SLAB HALF-Z DIMENSION'                                24
C FORWARD IRON SLAB HALF-THICKNESS                                        25
      SHLG_FIDZ=47                                                        25
      CSHLG_FIDZ='FORWARD IRON SLAB HALF-THICKNESS'                       25
C HALF-HEIGHT                                                             26
      SHLG_FIDY=55                                                        26
      CSHLG_FIDY='HALF-HEIGHT'                                            26
C BEAM HOLE HALF-SIZE IN X                                                27
      SHLG_HOLEX=20                                                       27
      CSHLG_HOLEX='BEAM HOLE HALF-SIZE IN X'                              27
C BEAM HOLE HALF-SIZE IN Y                                                28
      SHLG_HOLEY=10                                                       28
      CSHLG_HOLEY='BEAM HOLE HALF-SIZE IN Y'                              28
C CONCRETE FLOOR THICKNESS                                                29
      SHLG_FLOORTHK=70                                                    29
      CSHLG_FLOORTHK='CONCRETE FLOOR THICKNESS'                           29
C CONCRETE FLOOR LENGTH                                                   30
      SHLG_FLOORLEN=3900                                                  30
      CSHLG_FLOORLEN='CONCRETE FLOOR LENGTH'                              30
C CONCRETE FLOOR WIDTH                                                    31
      SHLG_FLOORWIDTH=340                                                 31
      CSHLG_FLOORWIDTH='CONCRETE FLOOR WIDTH'                             31
C CONCRETE FLOOR Z-POSITION                                               32
      SHLG_FLOORPOS=2800                                                  32
      CSHLG_FLOORPOS='CONCRETE FLOOR Z-POSITION'                          32
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SHLG',    33
     *BPATH_SHLG,IDNUM_SHLG,1, MAP_SHLG,NAMES_SHLG,COMMEN_SHLG,           33
     *BANK_SHLG,LEN_SHLG,LLEN_SHLG,LINK_SHLG,FLAG_SHLG)                   33
C *                                                                       33
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SHLG',    33
     *BPATH_SHLG,IDNUM_SHLG,1, MAP_SHLG,NAMES_SHLG,COMMEN_SHLG,           33
     *BANK_SHLG,LEN_SHLG,LLEN_SHLG,LINK_SHLG,FLAG_SHLG)                   33
C use default SHLG bank                                                   34
      BPATH_SHLG='SHLG*'                                                  34
      IDNUM_SHLG(1)=0                                                     34
      CALL AGDGETP('SHLDGEO','*',-999,'SHLG',' ',BANK_SHLG, BPATH_SHLG,   34
     *IDNUM_SHLG,1,MAP_SHLG,NAMES_SHLG,BANK_SHLG,LEN_SHLG,LLEN_SHLG,      34
     *LINK_SHLG,FLAG_SHLG)                                                34
      SHIELDHALFHEIGHT=(SHLG_BASEDY+SHLG_SLABDY+SHLG_FIDY)                35
      CALL AGSBEGM(MODULE,IPRIN)                                          36
C Check IPRIN<0                                                           36
      IF (IPRIN.LT.0) RETURN                                              36
C Create SHLD :                                                           36
C Call To Local Procedure SHLD                                            36
      ASSIGN 5020 TO BLOCK_SHLD                                           36
      GO TO  5010                                                         36
5020  CONTINUE                                                            36
C *                                                                       36
C Geant POSITION : SHLD ===> MOTHER='CAVE' X=0.0 Y=SHLG_BASELEVEL+SHIELD  37
C HALFHEIGHT Z=+SHLG_Z <===                                               37
      CALL AGSRESET                                                       37
      AG_MOTHER = 'CAVE'                                                  37
      AG_X = 0.0                                                          37
      AG_Y = SHLG_BASELEVEL+SHIELDHALFHEIGHT                              37
      AG_Z = +SHLG_Z                                                      37
      AG_TITLE = 'SHLD'                                                   37
      AG_EXNAME = 'POSITION'                                              37
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         37
      CALL AXPOSITION                                                     37
C *                                                                       37
C Geant POSITION : SHLD ===> MOTHER='CAVE' X=0.0 Y=SHLG_BASELEVEL+SHIELD  37
C HALFHEIGHT Z=-SHLG_Z THETAZ=180 <===                                    37
      CALL AGSRESET                                                       39
      AG_MOTHER = 'CAVE'                                                  39
      AG_X = 0.0                                                          39
      AG_Y = SHLG_BASELEVEL+SHIELDHALFHEIGHT                              39
      AG_Z = -SHLG_Z                                                      39
      AG_THETAZ = 180                                                     39
      AG_TITLE = 'SHLD'                                                   39
      AG_EXNAME = 'POSITION'                                              39
      AG_PARLIST = 'MOTHER_X_Y_Z_THETAZ'                                  39
      CALL AXPOSITION                                                     39
C *                                                                       39
      CALL AGSBEGM(MODULE,IPRIN)                                          39
C Check IPRIN<0                                                           40
      IF (IPRIN.LT.0) RETURN                                              40
C Create SFLR :                                                           40
C Call To Local Procedure SFLR                                            40
      ASSIGN 5040 TO BLOCK_SFLR                                           40
      GO TO  5030                                                         40
5040  CONTINUE                                                            40
C *                                                                       40
C Geant POSITION : SFLR ===> MOTHER='CAVE' X=0 Y=SHLG_BASELEVEL-0.5*SHLG  41
C _FLOORTHK Z=+SHLG_FLOORPOS <===                                         41
      CALL AGSRESET                                                       41
      AG_MOTHER = 'CAVE'                                                  41
      AG_X = 0                                                            41
      AG_Y = SHLG_BASELEVEL-0.5*SHLG_FLOORTHK                             41
      AG_Z = +SHLG_FLOORPOS                                               41
      AG_TITLE = 'SFLR'                                                   41
      AG_EXNAME = 'POSITION'                                              41
      AG_PARLIST = 'MOTHER_X_Y_Z'                                         41
      CALL AXPOSITION                                                     41
C *                                                                       41
C Geant POSITION : SFLR ===> MOTHER='CAVE' X=0 Y=SHLG_BASELEVEL-0.5*SHLG  41
C _FLOORTHK Z=-SHLG_FLOORPOS THETAZ=180 <===                              41
      CALL AGSRESET                                                       43
      AG_MOTHER = 'CAVE'                                                  43
      AG_X = 0                                                            43
      AG_Y = SHLG_BASELEVEL-0.5*SHLG_FLOORTHK                             43
      AG_Z = -SHLG_FLOORPOS                                               43
      AG_THETAZ = 180                                                     43
      AG_TITLE = 'SFLR'                                                   43
      AG_EXNAME = 'POSITION'                                              43
      AG_PARLIST = 'MOTHER_X_Y_Z_THETAZ'                                  43
      CALL AXPOSITION                                                     43
C *                                                                       43
C ----------------------------------------------------------------------  43
C ------------ SHLD                                                       43
      CALL AGSENDM                                                        45
      RETURN                                                              45
C Begin Local Procedure SHLD                                              45
C SHLD IS THE SHIELD MOTHER VOLUME IN THE STAR CAVE                       45
5010  CONTINUE                                                            45
         AG_TITLE='SHLD'                                                  45
         AG_MODULE=MODULE                                                 45
         AG_MARK='V00'                                                    45
         CALL AGSPUSH                                                     45
         AG_MODULE=MODULE                                                 45
C    *                                                                    45
C    Geant MATERIAL : AIR ===> inherited arguments used <===              46
         CALL AGSRESET                                                    46
         AG_TITLE = 'AIR'                                                 46
         AG_EXNAME = 'MATERIAL'                                           46
         AG_PARLIST = 'NONE'                                              46
         CALL AXMATERIAL                                                  46
C    *                                                                    46
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           46
         CALL AGSRESET                                                    47
         AG_TITLE = 'STANDARD'                                            47
         AG_EXNAME = 'MEDIUM'                                             47
         AG_PARLIST = 'NONE'                                              47
         CALL AXMEDIUM                                                    47
C    *                                                                    47
C    Geant ATTRIBUTE : SHLD ===> SEEN=1 COLO=2 <===                       47
         CALL AGSRESET                                                    48
         AG_SEEN = 1                                                      48
         AG_COLO = 2                                                      48
         AG_TITLE = 'SHLD'                                                48
         AG_EXNAME = 'ATTRIBUTE'                                          48
         AG_PARLIST = 'SEEN_COLO'                                         48
         CALL AXATTRIBUTE                                                 48
C    *                                                                    48
C    Geant SHAPE : BOX ===> DX=SHLG_DX DY=SHIELDHALFHEIGHT, DZ=SHLG_DZ <  48
C    ===                                                                  48
         CALL AGSRESET                                                    50
         AG_DX = SHLG_DX                                                  50
         AG_DY = SHIELDHALFHEIGHT                                         50
         AG_DZ = SHLG_DZ                                                  50
         AG_TITLE = 'BOX'                                                 50
         AG_EXNAME = 'SHAPE'                                              50
         AG_PARLIST = 'DX_DY_DZ'                                          50
         CALL AXSHAPE                                                     50
C    *                                                                    50
C    Create SHBS :                                                        51
C    Call To Local Procedure SHBS                                         51
         ASSIGN 5060 TO BLOCK_SHBS                                        51
         GO TO  5050                                                      51
5060     CONTINUE                                                         51
C    *                                                                    51
C    Geant POSITION : SHBS ===> X=+SHLG_DX-SHLG_BASEDX Y=-SHIELDHALFHEIG  52
C    HT+SHLG_BASEDY Z=0 <===                                              52
         CALL AGSRESET                                                    52
         AG_X = +SHLG_DX-SHLG_BASEDX                                      52
         AG_Y = -SHIELDHALFHEIGHT+SHLG_BASEDY                             52
         AG_Z = 0                                                         52
         AG_TITLE = 'SHBS'                                                52
         AG_EXNAME = 'POSITION'                                           52
         AG_PARLIST = 'X_Y_Z'                                             52
         CALL AXPOSITION                                                  52
C    *                                                                    52
C    Geant POSITION : SHBS ===> X=-SHLG_DX+SHLG_BASEDX Y=-SHIELDHALFHEIG  52
C    HT+SHLG_BASEDY Z=0 <===                                              52
         CALL AGSRESET                                                    53
         AG_X = -SHLG_DX+SHLG_BASEDX                                      53
         AG_Y = -SHIELDHALFHEIGHT+SHLG_BASEDY                             53
         AG_Z = 0                                                         53
         AG_TITLE = 'SHBS'                                                53
         AG_EXNAME = 'POSITION'                                           53
         AG_PARLIST = 'X_Y_Z'                                             53
         CALL AXPOSITION                                                  53
C    *                                                                    53
         YSLAB = - SHIELDHALFHEIGHT + 2.0*SHLG_BASEDY + SHLG_SLABDY       53
C    Create SHLS :                                                        55
C    Call To Local Procedure SHLS                                         55
         ASSIGN 5080 TO BLOCK_SHLS                                        55
         GO TO  5070                                                      55
5080     CONTINUE                                                         55
C    *                                                                    55
C    Geant POSITION : SHLS ===> X=SHLG_SLABX Y=YSLAB Z=-SHLG_DZ+SHLG_SLA  56
C    BDZ <===                                                             56
         CALL AGSRESET                                                    56
         AG_X = SHLG_SLABX                                                56
         AG_Y = YSLAB                                                     56
         AG_Z = -SHLG_DZ+SHLG_SLABDZ                                      56
         AG_TITLE = 'SHLS'                                                56
         AG_EXNAME = 'POSITION'                                           56
         AG_PARLIST = 'X_Y_Z'                                             56
         CALL AXPOSITION                                                  56
C    *                                                                    56
C    Create SHBI :                                                        57
C    Call To Local Procedure SHBI                                         57
         ASSIGN 5100 TO BLOCK_SHBI                                        57
         GO TO  5090                                                      57
5100     CONTINUE                                                         57
C    *                                                                    57
C    Geant POSITION : SHBI ===> X=0.0 Y=YSLAB Z=SHLG_SLABDZ <===          58
         CALL AGSRESET                                                    58
         AG_X = 0.0                                                       58
         AG_Y = YSLAB                                                     58
         AG_Z = SHLG_SLABDZ                                               58
         AG_TITLE = 'SHBI'                                                58
         AG_EXNAME = 'POSITION'                                           58
         AG_PARLIST = 'X_Y_Z'                                             58
         CALL AXPOSITION                                                  58
C    *                                                                    58
         YFI = - SHIELDHALFHEIGHT + 2.0*(SHLG_BASEDY + SHLG_SLABDY) +     58
     *   SHLG_FIDY                                                        58
C    Create SHFI :                                                        60
C    Call To Local Procedure SHFI                                         60
         ASSIGN 5120 TO BLOCK_SHFI                                        60
         GO TO  5110                                                      60
5120     CONTINUE                                                         60
C    *                                                                    60
C    Geant POSITION : SHFI ===> X=0.0 Y=YFI Z=-SHLG_DZ+SHLG_FIDZ <===     61
         CALL AGSRESET                                                    61
         AG_X = 0.0                                                       61
         AG_Y = YFI                                                       61
         AG_Z = -SHLG_DZ+SHLG_FIDZ                                        61
         AG_TITLE = 'SHFI'                                                61
         AG_EXNAME = 'POSITION'                                           61
         AG_PARLIST = 'X_Y_Z'                                             61
         CALL AXPOSITION                                                  61
C    *                                                                    61
5011  CALL AGSPOPB                                                        62
      GO TO BLOCK_SHLD, (5010 ,5020)                                      62
C End of Local Procedure                                                  62
C ----------------------------------------------------------------------  63
C ------------ SFLR                                                       63
C Begin Local Procedure SFLR                                              64
C SFLR IS THE FLOOR                                                       64
5030  CONTINUE                                                            64
         AG_TITLE='SFLR'                                                  64
         AG_MODULE=MODULE                                                 64
         AG_MARK='V00'                                                    64
         CALL AGSPUSH                                                     64
         AG_MODULE=MODULE                                                 64
C    *                                                                    64
C    Geant ATTRIBUTE : SFLR ===> SEEN=1 COLO=3 <===                       65
         CALL AGSRESET                                                    65
         AG_SEEN = 1                                                      65
         AG_COLO = 3                                                      65
         AG_TITLE = 'SFLR'                                                65
         AG_EXNAME = 'ATTRIBUTE'                                          65
         AG_PARLIST = 'SEEN_COLO'                                         65
         CALL AXATTRIBUTE                                                 65
C    *                                                                    65
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=1 <===                      65
         CALL AGSRESET                                                    66
         AG_A = 28.08                                                     66
         AG_Z = 14                                                        66
         AG_W = 1                                                         66
         AG_TITLE = 'SI'                                                  66
         AG_EXNAME = 'COMPONENT'                                          66
         AG_PARLIST = 'A_Z_W'                                             66
         CALL AXCOMPONENT                                                 66
C    *                                                                    66
C    Geant COMPONENT : O2 ===> A=16 Z=8 W=2 <===                          66
         CALL AGSRESET                                                    67
         AG_A = 16                                                        67
         AG_Z = 8                                                         67
         AG_W = 2                                                         67
         AG_TITLE = 'O2'                                                  67
         AG_EXNAME = 'COMPONENT'                                          67
         AG_PARLIST = 'A_Z_W'                                             67
         CALL AXCOMPONENT                                                 67
C    *                                                                    67
C    Geant MIXTURE : SHIELDCONC ===> DENS=2.5 <===                        67
         CALL AGSRESET                                                    68
         AG_DENS = 2.5                                                    68
         AG_TITLE = 'SHIELDCONC'                                          68
         AG_EXNAME = 'MIXTURE'                                            68
         AG_PARLIST = 'DENS'                                              68
         CALL AXMIXTURE                                                   68
C    *                                                                    68
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           68
         CALL AGSRESET                                                    69
         AG_TITLE = 'STANDARD'                                            69
         AG_EXNAME = 'MEDIUM'                                             69
         AG_PARLIST = 'NONE'                                              69
         CALL AXMEDIUM                                                    69
C    *                                                                    69
C    Geant SHAPE : BOX ===> DX=SHLG_FLOORWIDTH/2.0 DY=SHLG_FLOORTHK/2.0,  69
C     DZ=SHLG_FLOORLEN/2.0 <===                                           69
         CALL AGSRESET                                                    71
         AG_DX = SHLG_FLOORWIDTH/2.0                                      71
         AG_DY = SHLG_FLOORTHK/2.0                                        71
         AG_DZ = SHLG_FLOORLEN/2.0                                        71
         AG_TITLE = 'BOX'                                                 71
         AG_EXNAME = 'SHAPE'                                              71
         AG_PARLIST = 'DX_DY_DZ'                                          71
         CALL AXSHAPE                                                     71
C    *                                                                    71
5031  CALL AGSPOPB                                                        72
      GO TO BLOCK_SFLR, (5030 ,5040)                                      72
C End of Local Procedure                                                  72
C ----------------------------------------------------------------------  73
C ------------ SHBS                                                       73
C Begin Local Procedure SHBS                                              74
C SHBS IS THE SHIELD BASE                                                 74
5050  CONTINUE                                                            74
         AG_TITLE='SHBS'                                                  74
         AG_MODULE=MODULE                                                 74
         AG_MARK='V00'                                                    74
         CALL AGSPUSH                                                     74
         AG_MODULE=MODULE                                                 74
C    *                                                                    74
C    Geant ATTRIBUTE : SHBS ===> SEEN=1 COLO=3 <===                       75
         CALL AGSRESET                                                    75
         AG_SEEN = 1                                                      75
         AG_COLO = 3                                                      75
         AG_TITLE = 'SHBS'                                                75
         AG_EXNAME = 'ATTRIBUTE'                                          75
         AG_PARLIST = 'SEEN_COLO'                                         75
         CALL AXATTRIBUTE                                                 75
C    *                                                                    75
C    Geant COMPONENT : SI ===> A=28.08 Z=14 W=1 <===                      75
         CALL AGSRESET                                                    76
         AG_A = 28.08                                                     76
         AG_Z = 14                                                        76
         AG_W = 1                                                         76
         AG_TITLE = 'SI'                                                  76
         AG_EXNAME = 'COMPONENT'                                          76
         AG_PARLIST = 'A_Z_W'                                             76
         CALL AXCOMPONENT                                                 76
C    *                                                                    76
C    Geant COMPONENT : O2 ===> A=16 Z=8 W=2 <===                          76
         CALL AGSRESET                                                    77
         AG_A = 16                                                        77
         AG_Z = 8                                                         77
         AG_W = 2                                                         77
         AG_TITLE = 'O2'                                                  77
         AG_EXNAME = 'COMPONENT'                                          77
         AG_PARLIST = 'A_Z_W'                                             77
         CALL AXCOMPONENT                                                 77
C    *                                                                    77
C    Geant MIXTURE : BASECONC ===> DENS=2.5 <===                          77
         CALL AGSRESET                                                    78
         AG_DENS = 2.5                                                    78
         AG_TITLE = 'BASECONC'                                            78
         AG_EXNAME = 'MIXTURE'                                            78
         AG_PARLIST = 'DENS'                                              78
         CALL AXMIXTURE                                                   78
C    *                                                                    78
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           78
         CALL AGSRESET                                                    79
         AG_TITLE = 'STANDARD'                                            79
         AG_EXNAME = 'MEDIUM'                                             79
         AG_PARLIST = 'NONE'                                              79
         CALL AXMEDIUM                                                    79
C    *                                                                    79
C    Geant SHAPE : BOX ===> DX=SHLG_BASEDX DY=SHLG_BASEDY, DZ=SHLG_DZ <=  79
C    ==                                                                   79
         CALL AGSRESET                                                    81
         AG_DX = SHLG_BASEDX                                              81
         AG_DY = SHLG_BASEDY                                              81
         AG_DZ = SHLG_DZ                                                  81
         AG_TITLE = 'BOX'                                                 81
         AG_EXNAME = 'SHAPE'                                              81
         AG_PARLIST = 'DX_DY_DZ'                                          81
         CALL AXSHAPE                                                     81
C    *                                                                    81
5051  CALL AGSPOPB                                                        82
      GO TO BLOCK_SHBS, (5050 ,5060)                                      82
C End of Local Procedure                                                  82
C ----------------------------------------------------------------------  83
C ------------ SHLS                                                       83
C Begin Local Procedure SHLS                                              84
C SHLS IS THE LATERAL SLAB                                                84
5070  CONTINUE                                                            84
         AG_TITLE='SHLS'                                                  84
         AG_MODULE=MODULE                                                 84
         AG_MARK='V00'                                                    84
         CALL AGSPUSH                                                     84
         AG_MODULE=MODULE                                                 84
C    *                                                                    84
C    Geant MATERIAL : BASECONC ===> inherited arguments used <===         85
         CALL AGSRESET                                                    85
         AG_TITLE = 'BASECONC'                                            85
         AG_EXNAME = 'MATERIAL'                                           85
         AG_PARLIST = 'NONE'                                              85
         CALL AXMATERIAL                                                  85
C    *                                                                    85
C    Geant ATTRIBUTE : SHBS ===> SEEN=1 COLO=3 <===                       85
         CALL AGSRESET                                                    86
         AG_SEEN = 1                                                      86
         AG_COLO = 3                                                      86
         AG_TITLE = 'SHBS'                                                86
         AG_EXNAME = 'ATTRIBUTE'                                          86
         AG_PARLIST = 'SEEN_COLO'                                         86
         CALL AXATTRIBUTE                                                 86
C    *                                                                    86
C    Geant SHAPE : BOX ===> DX=SHLG_DX DY=SHLG_SLABDY DZ=SHLG_SLABDZ <==  86
C    =                                                                    86
         CALL AGSRESET                                                    87
         AG_DX = SHLG_DX                                                  87
         AG_DY = SHLG_SLABDY                                              87
         AG_DZ = SHLG_SLABDZ                                              87
         AG_TITLE = 'BOX'                                                 87
         AG_EXNAME = 'SHAPE'                                              87
         AG_PARLIST = 'DX_DY_DZ'                                          87
         CALL AXSHAPE                                                     87
C    *                                                                    87
5071  CALL AGSPOPB                                                        88
      GO TO BLOCK_SHLS, (5070 ,5080)                                      88
C End of Local Procedure                                                  88
C ----------------------------------------------------------------------  89
C ------------ SHBI                                                       89
C Begin Local Procedure SHBI                                              90
C SHBI IS THE BACK IRON SLAB                                              90
5090  CONTINUE                                                            90
         AG_TITLE='SHBI'                                                  90
         AG_MODULE=MODULE                                                 90
         AG_MARK='V00'                                                    90
         CALL AGSPUSH                                                     90
         AG_MODULE=MODULE                                                 90
C    *                                                                    90
C    Geant MATERIAL : IRON ===> inherited arguments used <===             91
         CALL AGSRESET                                                    91
         AG_TITLE = 'IRON'                                                91
         AG_EXNAME = 'MATERIAL'                                           91
         AG_PARLIST = 'NONE'                                              91
         CALL AXMATERIAL                                                  91
C    *                                                                    91
C    Geant ATTRIBUTE : SHBI ===> SEEN=1 COLO=1 <===                       91
         CALL AGSRESET                                                    92
         AG_SEEN = 1                                                      92
         AG_COLO = 1                                                      92
         AG_TITLE = 'SHBI'                                                92
         AG_EXNAME = 'ATTRIBUTE'                                          92
         AG_PARLIST = 'SEEN_COLO'                                         92
         CALL AXATTRIBUTE                                                 92
C    *                                                                    92
C    Geant SHAPE : BOX ===> DX=SHLG_DX DY=SHLG_SLABDY DZ=SHLG_DZ-SHLG_SL  92
C    ABDZ <===                                                            92
         CALL AGSRESET                                                    93
         AG_DX = SHLG_DX                                                  93
         AG_DY = SHLG_SLABDY                                              93
         AG_DZ = SHLG_DZ-SHLG_SLABDZ                                      93
         AG_TITLE = 'BOX'                                                 93
         AG_EXNAME = 'SHAPE'                                              93
         AG_PARLIST = 'DX_DY_DZ'                                          93
         CALL AXSHAPE                                                     93
C    *                                                                    93
5091  CALL AGSPOPB                                                        94
      GO TO BLOCK_SHBI, (5090 ,5100)                                      94
C End of Local Procedure                                                  94
C ----------------------------------------------------------------------  95
C ------------ SHFI                                                       95
C Begin Local Procedure SHFI                                              96
C SHFI IS THE FORWARD IRON SLAB                                           96
5110  CONTINUE                                                            96
         AG_TITLE='SHFI'                                                  96
         AG_MODULE=MODULE                                                 96
         AG_MARK='V00'                                                    96
         CALL AGSPUSH                                                     96
         AG_MODULE=MODULE                                                 96
C    *                                                                    96
C    Geant MATERIAL : IRON ===> inherited arguments used <===             97
         CALL AGSRESET                                                    97
         AG_TITLE = 'IRON'                                                97
         AG_EXNAME = 'MATERIAL'                                           97
         AG_PARLIST = 'NONE'                                              97
         CALL AXMATERIAL                                                  97
C    *                                                                    97
C    Geant ATTRIBUTE : SHFI ===> SEEN=1 COLO=1 <===                       97
         CALL AGSRESET                                                    98
         AG_SEEN = 1                                                      98
         AG_COLO = 1                                                      98
         AG_TITLE = 'SHFI'                                                98
         AG_EXNAME = 'ATTRIBUTE'                                          98
         AG_PARLIST = 'SEEN_COLO'                                         98
         CALL AXATTRIBUTE                                                 98
C    *                                                                    98
C    Geant SHAPE : BOX ===> DX=SHLG_DX DY=SHLG_FIDY DZ=SHLG_FIDZ <===     98
         CALL AGSRESET                                                    99
         AG_DX = SHLG_DX                                                  99
         AG_DY = SHLG_FIDY                                                99
         AG_DZ = SHLG_FIDZ                                                99
         AG_TITLE = 'BOX'                                                 99
         AG_EXNAME = 'SHAPE'                                              99
         AG_PARLIST = 'DX_DY_DZ'                                          99
         CALL AXSHAPE                                                     99
C    *                                                                    99
C    Create SHOL :                                                        100
C    Call To Local Procedure SHOL                                         100
         ASSIGN 5140 TO BLOCK_SHOL                                        100
         GO TO  5130                                                      100
5140     CONTINUE                                                         100
C    *                                                                    100
C    Geant POSITION : SHOL ===> Y=-SHLG_FIDY+SHLG_HOLEY <===              101
         CALL AGSRESET                                                    101
         AG_Y = -SHLG_FIDY+SHLG_HOLEY                                     101
         AG_TITLE = 'SHOL'                                                101
         AG_EXNAME = 'POSITION'                                           101
         AG_PARLIST = 'Y'                                                 101
         CALL AXPOSITION                                                  101
C    *                                                                    101
5111  CALL AGSPOPB                                                        102
      GO TO BLOCK_SHFI, (5110 ,5120)                                      102
C End of Local Procedure                                                  102
C ----------------------------------------------------------------------  103
C ------------ SHOL                                                       103
C Begin Local Procedure SHOL                                              104
C SHOL IS THE HOLE IN THE FORWARD IRON SLAB                               104
5130  CONTINUE                                                            104
         AG_TITLE='SHOL'                                                  104
         AG_MODULE=MODULE                                                 104
         AG_MARK='V00'                                                    104
         CALL AGSPUSH                                                     104
         AG_MODULE=MODULE                                                 104
C    *                                                                    104
C    Geant MATERIAL : AIR ===> inherited arguments used <===              105
         CALL AGSRESET                                                    105
         AG_TITLE = 'AIR'                                                 105
         AG_EXNAME = 'MATERIAL'                                           105
         AG_PARLIST = 'NONE'                                              105
         CALL AXMATERIAL                                                  105
C    *                                                                    105
C    Geant ATTRIBUTE : SHOL ===> SEEN=1 COLO=6 <===                       105
         CALL AGSRESET                                                    106
         AG_SEEN = 1                                                      106
         AG_COLO = 6                                                      106
         AG_TITLE = 'SHOL'                                                106
         AG_EXNAME = 'ATTRIBUTE'                                          106
         AG_PARLIST = 'SEEN_COLO'                                         106
         CALL AXATTRIBUTE                                                 106
C    *                                                                    106
C    Geant SHAPE : BOX ===> DX=SHLG_HOLEX DY=SHLG_HOLEY DZ=SHLG_FIDZ <==  106
C    =                                                                    106
         CALL AGSRESET                                                    107
         AG_DX = SHLG_HOLEX                                               107
         AG_DY = SHLG_HOLEY                                               107
         AG_DZ = SHLG_FIDZ                                                107
         AG_TITLE = 'BOX'                                                 107
         AG_EXNAME = 'SHAPE'                                              107
         AG_PARLIST = 'DX_DY_DZ'                                          107
         CALL AXSHAPE                                                     107
C    *                                                                    107
5131  CALL AGSPOPB                                                        108
      GO TO BLOCK_SHOL, (5130 ,5140)                                      108
C End of Local Procedure                                                  108
      WRITE(LOUT,5150)'===>>  [End AgML Module SHLDGEO]  <<==='           108
5150  FORMAT(A32,/,/)                                                     108
C MODULE SHLDGEO                                                          110
      END                                                                 110
