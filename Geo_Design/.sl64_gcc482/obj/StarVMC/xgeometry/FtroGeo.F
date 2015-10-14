C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE FTROGEO                                                  1
C FTROGEO IS THE GEOMETRY OF THE READOUT STRUCTURE OF THE FTPC            1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'FTROGEO'/, BANK_TITLE/'*'/                             1
      DATA MCOMMENT/'IS THE GEOMETRY OF THE READOUT STRUCTURE OF THE FTP  1
     *C'/                                                                 1
      LOGICAL FIRST                                                       1
      DATA FIRST/.TRUE./                                                  1
      SAVE FIRST                                                          1
      INTEGER JUMPAD                                                      1
      EXTERNAL JUMPAD                                                     1
      INTEGER OK                                                          1
      PARAMETER (OK=0)                                                    1
      INTEGER AG_I,AG_J                                                   1
      SAVE AG_I,AG_J                                                      1
C CREATED 07/22/04                                                        1
      DATA AG_I/0/,AG_J/0/                                                2
      CHARACTER*40 CREATED /'07/22/04'/                                   2
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
      REAL ZPOS,MYRADDEG,HOLEANGLE,STRUTANGLE,ANGLEPOS,ANGLE              5
      REAL STRUTRAD,PLANKRAD,STRUTHOLESPACING,CENTERSTRUT,INFLANGEZ       8
      REAL FTPOANGLEPOS,FTPIANGLEPOS,A,REFF                               10
      INTEGER NHOLE,NSTRUT                                                12
      INTEGER BLOCK_FTMO                                                  15
      INTEGER BLOCK_FTOF                                                  15
      INTEGER BLOCK_FTOH                                                  15
      INTEGER BLOCK_FSMO                                                  15
      INTEGER BLOCK_FTCM                                                  15
      INTEGER BLOCK_FTCD                                                  15
      INTEGER BLOCK_FSTL                                                  15
      INTEGER BLOCK_FSTC                                                  15
      INTEGER BLOCK_FSHL                                                  15
      INTEGER BLOCK_FTIF                                                  15
      INTEGER BLOCK_FTPO                                                  15
      INTEGER BLOCK_FTPI                                                  15
      INTEGER BLOCK_FTSH                                                  15
      INTEGER BLOCK_FTRM                                                  15
      INTEGER BLOCK_FTPL                                                  15
C -                                                                       19
      REAL FTRG_VERSION                                                   19
      EQUIVALENCE (BANK_FTRG(3),FTRG_VERSION,RE_FTRG_VERSION),            19
     *(NAMES_FTRG(1),NFTRG_VERSION)                                       19
      EQUIVALENCE (COMMEN_FTRG(1),CFTRG_VERSION),(MAP_FTRG(1,1),          19
     *MFTRG_VERSION)                                                      19
      CHARACTER CFTRG_VERSION*40/' '/                                     19
      CHARACTER NFTRG_VERSION*12/'R.VERSION'/                             19
      INTEGER MFTRG_VERSION(2)/1,1/                                       19
      REAL RE_FTRG_VERSION                                                19
C -                                                                       19
      REAL FTRG_N                                                         19
      EQUIVALENCE (BANK_FTRG(4),FTRG_N,RE_FTRG_N),(NAMES_FTRG(2),         19
     *NFTRG_N)                                                            19
      EQUIVALENCE (COMMEN_FTRG(2),CFTRG_N),(MAP_FTRG(1,2),MFTRG_N)        19
      CHARACTER CFTRG_N*40/' '/                                           19
      CHARACTER NFTRG_N*12/'R.N'/                                         19
      INTEGER MFTRG_N(2)/1,1/                                             19
      REAL RE_FTRG_N                                                      19
C -                                                                       19
      REAL FTRG_FTPCZ                                                     19
      EQUIVALENCE (BANK_FTRG(5),FTRG_FTPCZ,RE_FTRG_FTPCZ),                19
     *(NAMES_FTRG(3),NFTRG_FTPCZ)                                         19
      EQUIVALENCE (COMMEN_FTRG(3),CFTRG_FTPCZ),(MAP_FTRG(1,3),            19
     *MFTRG_FTPCZ)                                                        19
      CHARACTER CFTRG_FTPCZ*40/' '/                                       19
      CHARACTER NFTRG_FTPCZ*12/'R.FTPCZ'/                                 19
      INTEGER MFTRG_FTPCZ(2)/1,1/                                         19
      REAL RE_FTRG_FTPCZ                                                  19
C -                                                                       19
      REAL FTRG_FTPCLEN                                                   19
      EQUIVALENCE (BANK_FTRG(6),FTRG_FTPCLEN,RE_FTRG_FTPCLEN),            19
     *(NAMES_FTRG(4),NFTRG_FTPCLEN)                                       19
      EQUIVALENCE (COMMEN_FTRG(4),CFTRG_FTPCLEN),(MAP_FTRG(1,4),          19
     *MFTRG_FTPCLEN)                                                      19
      CHARACTER CFTRG_FTPCLEN*40/' '/                                     19
      CHARACTER NFTRG_FTPCLEN*12/'R.FTPCLEN'/                             19
      INTEGER MFTRG_FTPCLEN(2)/1,1/                                       19
      REAL RE_FTRG_FTPCLEN                                                19
C -                                                                       19
      REAL FTRG_LENGTH                                                    19
      EQUIVALENCE (BANK_FTRG(7),FTRG_LENGTH,RE_FTRG_LENGTH),              19
     *(NAMES_FTRG(5),NFTRG_LENGTH)                                        19
      EQUIVALENCE (COMMEN_FTRG(5),CFTRG_LENGTH),(MAP_FTRG(1,5),           19
     *MFTRG_LENGTH)                                                       19
      CHARACTER CFTRG_LENGTH*40/' '/                                      19
      CHARACTER NFTRG_LENGTH*12/'R.LENGTH'/                               19
      INTEGER MFTRG_LENGTH(2)/1,1/                                        19
      REAL RE_FTRG_LENGTH                                                 19
C -                                                                       19
      REAL FTRG_ANGOFFSET                                                 19
      EQUIVALENCE (BANK_FTRG(8),FTRG_ANGOFFSET,RE_FTRG_ANGOFFSET),        19
     *(NAMES_FTRG(6),NFTRG_ANGOFFSET)                                     19
      EQUIVALENCE (COMMEN_FTRG(6),CFTRG_ANGOFFSET),(MAP_FTRG(1,6),        19
     *MFTRG_ANGOFFSET)                                                    19
      CHARACTER CFTRG_ANGOFFSET*40/' '/                                   19
      CHARACTER NFTRG_ANGOFFSET*12/'R.ANGOFFSET'/                         19
      INTEGER MFTRG_ANGOFFSET(2)/1,1/                                     19
      REAL RE_FTRG_ANGOFFSET                                              19
C -                                                                       19
      REAL FTRG_RIN                                                       19
      EQUIVALENCE (BANK_FTRG(9),FTRG_RIN,RE_FTRG_RIN),(NAMES_FTRG(7),     19
     *NFTRG_RIN)                                                          19
      EQUIVALENCE (COMMEN_FTRG(7),CFTRG_RIN),(MAP_FTRG(1,7),MFTRG_RIN)    19
      CHARACTER CFTRG_RIN*40/' '/                                         19
      CHARACTER NFTRG_RIN*12/'R.RIN'/                                     19
      INTEGER MFTRG_RIN(2)/1,1/                                           19
      REAL RE_FTRG_RIN                                                    19
C -                                                                       19
      REAL FTRG_ROUT                                                      19
      EQUIVALENCE (BANK_FTRG(10),FTRG_ROUT,RE_FTRG_ROUT),(NAMES_FTRG(8),  19
     *NFTRG_ROUT)                                                         19
      EQUIVALENCE (COMMEN_FTRG(8),CFTRG_ROUT),(MAP_FTRG(1,8),             19
     *MFTRG_ROUT)                                                         19
      CHARACTER CFTRG_ROUT*40/' '/                                        19
      CHARACTER NFTRG_ROUT*12/'R.ROUT'/                                   19
      INTEGER MFTRG_ROUT(2)/1,1/                                          19
      REAL RE_FTRG_ROUT                                                   19
C -                                                                       19
      REAL FTRG_INRIN                                                     19
      EQUIVALENCE (BANK_FTRG(11),FTRG_INRIN,RE_FTRG_INRIN),               19
     *(NAMES_FTRG(9),NFTRG_INRIN)                                         19
      EQUIVALENCE (COMMEN_FTRG(9),CFTRG_INRIN),(MAP_FTRG(1,9),            19
     *MFTRG_INRIN)                                                        19
      CHARACTER CFTRG_INRIN*40/' '/                                       19
      CHARACTER NFTRG_INRIN*12/'R.INRIN'/                                 19
      INTEGER MFTRG_INRIN(2)/1,1/                                         19
      REAL RE_FTRG_INRIN                                                  19
C -                                                                       19
      REAL FTRG_INROUT                                                    19
      EQUIVALENCE (BANK_FTRG(12),FTRG_INROUT,RE_FTRG_INROUT),             19
     *(NAMES_FTRG(10),NFTRG_INROUT)                                       19
      EQUIVALENCE (COMMEN_FTRG(10),CFTRG_INROUT),(MAP_FTRG(1,10),         19
     *MFTRG_INROUT)                                                       19
      CHARACTER CFTRG_INROUT*40/' '/                                      19
      CHARACTER NFTRG_INROUT*12/'R.INROUT'/                               19
      INTEGER MFTRG_INROUT(2)/1,1/                                        19
      REAL RE_FTRG_INROUT                                                 19
C -                                                                       19
      REAL FTRG_INRTHK                                                    19
      EQUIVALENCE (BANK_FTRG(13),FTRG_INRTHK,RE_FTRG_INRTHK),             19
     *(NAMES_FTRG(11),NFTRG_INRTHK)                                       19
      EQUIVALENCE (COMMEN_FTRG(11),CFTRG_INRTHK),(MAP_FTRG(1,11),         19
     *MFTRG_INRTHK)                                                       19
      CHARACTER CFTRG_INRTHK*40/' '/                                      19
      CHARACTER NFTRG_INRTHK*12/'R.INRTHK'/                               19
      INTEGER MFTRG_INRTHK(2)/1,1/                                        19
      REAL RE_FTRG_INRTHK                                                 19
C -                                                                       19
      REAL FTRG_OFRIN                                                     19
      EQUIVALENCE (BANK_FTRG(14),FTRG_OFRIN,RE_FTRG_OFRIN),               19
     *(NAMES_FTRG(12),NFTRG_OFRIN)                                        19
      EQUIVALENCE (COMMEN_FTRG(12),CFTRG_OFRIN),(MAP_FTRG(1,12),          19
     *MFTRG_OFRIN)                                                        19
      CHARACTER CFTRG_OFRIN*40/' '/                                       19
      CHARACTER NFTRG_OFRIN*12/'R.OFRIN'/                                 19
      INTEGER MFTRG_OFRIN(2)/1,1/                                         19
      REAL RE_FTRG_OFRIN                                                  19
C -                                                                       19
      REAL FTRG_OFROUT                                                    19
      EQUIVALENCE (BANK_FTRG(15),FTRG_OFROUT,RE_FTRG_OFROUT),             19
     *(NAMES_FTRG(13),NFTRG_OFROUT)                                       19
      EQUIVALENCE (COMMEN_FTRG(13),CFTRG_OFROUT),(MAP_FTRG(1,13),         19
     *MFTRG_OFROUT)                                                       19
      CHARACTER CFTRG_OFROUT*40/' '/                                      19
      CHARACTER NFTRG_OFROUT*12/'R.OFROUT'/                               19
      INTEGER MFTRG_OFROUT(2)/1,1/                                        19
      REAL RE_FTRG_OFROUT                                                 19
C -                                                                       19
      REAL FTRG_OFRTHK                                                    19
      EQUIVALENCE (BANK_FTRG(16),FTRG_OFRTHK,RE_FTRG_OFRTHK),             19
     *(NAMES_FTRG(14),NFTRG_OFRTHK)                                       19
      EQUIVALENCE (COMMEN_FTRG(14),CFTRG_OFRTHK),(MAP_FTRG(1,14),         19
     *MFTRG_OFRTHK)                                                       19
      CHARACTER CFTRG_OFRTHK*40/' '/                                      19
      CHARACTER NFTRG_OFRTHK*12/'R.OFRTHK'/                               19
      INTEGER MFTRG_OFRTHK(2)/1,1/                                        19
      REAL RE_FTRG_OFRTHK                                                 19
C -                                                                       19
      REAL FTRG_OFZ                                                       19
      EQUIVALENCE (BANK_FTRG(17),FTRG_OFZ,RE_FTRG_OFZ),(NAMES_FTRG(15),   19
     *NFTRG_OFZ)                                                          19
      EQUIVALENCE (COMMEN_FTRG(15),CFTRG_OFZ),(MAP_FTRG(1,15),            19
     *MFTRG_OFZ)                                                          19
      CHARACTER CFTRG_OFZ*40/' '/                                         19
      CHARACTER NFTRG_OFZ*12/'R.OFZ'/                                     19
      INTEGER MFTRG_OFZ(2)/1,1/                                           19
      REAL RE_FTRG_OFZ                                                    19
C -                                                                       19
      REAL FTRG_OFNHOLES                                                  19
      EQUIVALENCE (BANK_FTRG(18),FTRG_OFNHOLES,RE_FTRG_OFNHOLES),         19
     *(NAMES_FTRG(16),NFTRG_OFNHOLES)                                     19
      EQUIVALENCE (COMMEN_FTRG(16),CFTRG_OFNHOLES),(MAP_FTRG(1,16),       19
     *MFTRG_OFNHOLES)                                                     19
      CHARACTER CFTRG_OFNHOLES*40/' '/                                    19
      CHARACTER NFTRG_OFNHOLES*12/'R.OFNHOLES'/                           19
      INTEGER MFTRG_OFNHOLES(2)/1,1/                                      19
      REAL RE_FTRG_OFNHOLES                                               19
C -                                                                       19
      REAL FTRG_OFHOLERAD                                                 19
      EQUIVALENCE (BANK_FTRG(19),FTRG_OFHOLERAD,RE_FTRG_OFHOLERAD),       19
     *(NAMES_FTRG(17),NFTRG_OFHOLERAD)                                    19
      EQUIVALENCE (COMMEN_FTRG(17),CFTRG_OFHOLERAD),(MAP_FTRG(1,17),      19
     *MFTRG_OFHOLERAD)                                                    19
      CHARACTER CFTRG_OFHOLERAD*40/' '/                                   19
      CHARACTER NFTRG_OFHOLERAD*12/'R.OFHOLERAD'/                         19
      INTEGER MFTRG_OFHOLERAD(2)/1,1/                                     19
      REAL RE_FTRG_OFHOLERAD                                              19
C -                                                                       19
      REAL FTRG_OFHOLER                                                   19
      EQUIVALENCE (BANK_FTRG(20),FTRG_OFHOLER,RE_FTRG_OFHOLER),           19
     *(NAMES_FTRG(18),NFTRG_OFHOLER)                                      19
      EQUIVALENCE (COMMEN_FTRG(18),CFTRG_OFHOLER),(MAP_FTRG(1,18),        19
     *MFTRG_OFHOLER)                                                      19
      CHARACTER CFTRG_OFHOLER*40/' '/                                     19
      CHARACTER NFTRG_OFHOLER*12/'R.OFHOLER'/                             19
      INTEGER MFTRG_OFHOLER(2)/1,1/                                       19
      REAL RE_FTRG_OFHOLER                                                19
C -                                                                       19
      REAL FTRG_STRUTLEN                                                  19
      EQUIVALENCE (BANK_FTRG(21),FTRG_STRUTLEN,RE_FTRG_STRUTLEN),         19
     *(NAMES_FTRG(19),NFTRG_STRUTLEN)                                     19
      EQUIVALENCE (COMMEN_FTRG(19),CFTRG_STRUTLEN),(MAP_FTRG(1,19),       19
     *MFTRG_STRUTLEN)                                                     19
      CHARACTER CFTRG_STRUTLEN*40/' '/                                    19
      CHARACTER NFTRG_STRUTLEN*12/'R.STRUTLEN'/                           19
      INTEGER MFTRG_STRUTLEN(2)/1,1/                                      19
      REAL RE_FTRG_STRUTLEN                                               19
C -                                                                       19
      REAL FTRG_STRUTHGT                                                  19
      EQUIVALENCE (BANK_FTRG(22),FTRG_STRUTHGT,RE_FTRG_STRUTHGT),         19
     *(NAMES_FTRG(20),NFTRG_STRUTHGT)                                     19
      EQUIVALENCE (COMMEN_FTRG(20),CFTRG_STRUTHGT),(MAP_FTRG(1,20),       19
     *MFTRG_STRUTHGT)                                                     19
      CHARACTER CFTRG_STRUTHGT*40/' '/                                    19
      CHARACTER NFTRG_STRUTHGT*12/'R.STRUTHGT'/                           19
      INTEGER MFTRG_STRUTHGT(2)/1,1/                                      19
      REAL RE_FTRG_STRUTHGT                                               19
C -                                                                       19
      REAL FTRG_STRUTWTH                                                  19
      EQUIVALENCE (BANK_FTRG(23),FTRG_STRUTWTH,RE_FTRG_STRUTWTH),         19
     *(NAMES_FTRG(21),NFTRG_STRUTWTH)                                     19
      EQUIVALENCE (COMMEN_FTRG(21),CFTRG_STRUTWTH),(MAP_FTRG(1,21),       19
     *MFTRG_STRUTWTH)                                                     19
      CHARACTER CFTRG_STRUTWTH*40/' '/                                    19
      CHARACTER NFTRG_STRUTWTH*12/'R.STRUTWTH'/                           19
      INTEGER MFTRG_STRUTWTH(2)/1,1/                                      19
      REAL RE_FTRG_STRUTWTH                                               19
C -                                                                       19
      REAL FTRG_STRUTTHK                                                  19
      EQUIVALENCE (BANK_FTRG(24),FTRG_STRUTTHK,RE_FTRG_STRUTTHK),         19
     *(NAMES_FTRG(22),NFTRG_STRUTTHK)                                     19
      EQUIVALENCE (COMMEN_FTRG(22),CFTRG_STRUTTHK),(MAP_FTRG(1,22),       19
     *MFTRG_STRUTTHK)                                                     19
      CHARACTER CFTRG_STRUTTHK*40/' '/                                    19
      CHARACTER NFTRG_STRUTTHK*12/'R.STRUTTHK'/                           19
      INTEGER MFTRG_STRUTTHK(2)/1,1/                                      19
      REAL RE_FTRG_STRUTTHK                                               19
C -                                                                       19
      REAL FTRG_STRUTHOLER                                                19
      EQUIVALENCE (BANK_FTRG(25),FTRG_STRUTHOLER,RE_FTRG_STRUTHOLER),     19
     *(NAMES_FTRG(23),NFTRG_STRUTHOLER)                                   19
      EQUIVALENCE (COMMEN_FTRG(23),CFTRG_STRUTHOLER),(MAP_FTRG(1,23),     19
     *MFTRG_STRUTHOLER)                                                   19
      CHARACTER CFTRG_STRUTHOLER*40/' '/                                  19
      CHARACTER NFTRG_STRUTHOLER*12/'R.STRUTHOLER'/                       19
      INTEGER MFTRG_STRUTHOLER(2)/1,1/                                    19
      REAL RE_FTRG_STRUTHOLER                                             19
C -                                                                       19
      REAL FTRG_STRUTNHOLES                                               19
      EQUIVALENCE (BANK_FTRG(26),FTRG_STRUTNHOLES,RE_FTRG_STRUTNHOLES),   19
     *(NAMES_FTRG(24),NFTRG_STRUTNHOLES)                                  19
      EQUIVALENCE (COMMEN_FTRG(24),CFTRG_STRUTNHOLES),(MAP_FTRG(1,24),    19
     *MFTRG_STRUTNHOLES)                                                  19
      CHARACTER CFTRG_STRUTNHOLES*40/' '/                                 19
      CHARACTER NFTRG_STRUTNHOLES*12/'R.STRUTNHOLES'/                     19
      INTEGER MFTRG_STRUTNHOLES(2)/1,1/                                   19
      REAL RE_FTRG_STRUTNHOLES                                            19
C -                                                                       19
      REAL FTRG_FTPOWTH                                                   19
      EQUIVALENCE (BANK_FTRG(27),FTRG_FTPOWTH,RE_FTRG_FTPOWTH),           19
     *(NAMES_FTRG(25),NFTRG_FTPOWTH)                                      19
      EQUIVALENCE (COMMEN_FTRG(25),CFTRG_FTPOWTH),(MAP_FTRG(1,25),        19
     *MFTRG_FTPOWTH)                                                      19
      CHARACTER CFTRG_FTPOWTH*40/' '/                                     19
      CHARACTER NFTRG_FTPOWTH*12/'R.FTPOWTH'/                             19
      INTEGER MFTRG_FTPOWTH(2)/1,1/                                       19
      REAL RE_FTRG_FTPOWTH                                                19
C -                                                                       19
      REAL FTRG_FTPOTHK                                                   19
      EQUIVALENCE (BANK_FTRG(28),FTRG_FTPOTHK,RE_FTRG_FTPOTHK),           19
     *(NAMES_FTRG(26),NFTRG_FTPOTHK)                                      19
      EQUIVALENCE (COMMEN_FTRG(26),CFTRG_FTPOTHK),(MAP_FTRG(1,26),        19
     *MFTRG_FTPOTHK)                                                      19
      CHARACTER CFTRG_FTPOTHK*40/' '/                                     19
      CHARACTER NFTRG_FTPOTHK*12/'R.FTPOTHK'/                             19
      INTEGER MFTRG_FTPOTHK(2)/1,1/                                       19
      REAL RE_FTRG_FTPOTHK                                                19
C -                                                                       19
      REAL FTRG_FTPOR                                                     19
      EQUIVALENCE (BANK_FTRG(29),FTRG_FTPOR,RE_FTRG_FTPOR),               19
     *(NAMES_FTRG(27),NFTRG_FTPOR)                                        19
      EQUIVALENCE (COMMEN_FTRG(27),CFTRG_FTPOR),(MAP_FTRG(1,27),          19
     *MFTRG_FTPOR)                                                        19
      CHARACTER CFTRG_FTPOR*40/' '/                                       19
      CHARACTER NFTRG_FTPOR*12/'R.FTPOR'/                                 19
      INTEGER MFTRG_FTPOR(2)/1,1/                                         19
      REAL RE_FTRG_FTPOR                                                  19
C -                                                                       19
      REAL FTRG_FTPIWTH                                                   19
      EQUIVALENCE (BANK_FTRG(30),FTRG_FTPIWTH,RE_FTRG_FTPIWTH),           19
     *(NAMES_FTRG(28),NFTRG_FTPIWTH)                                      19
      EQUIVALENCE (COMMEN_FTRG(28),CFTRG_FTPIWTH),(MAP_FTRG(1,28),        19
     *MFTRG_FTPIWTH)                                                      19
      CHARACTER CFTRG_FTPIWTH*40/' '/                                     19
      CHARACTER NFTRG_FTPIWTH*12/'R.FTPIWTH'/                             19
      INTEGER MFTRG_FTPIWTH(2)/1,1/                                       19
      REAL RE_FTRG_FTPIWTH                                                19
C -                                                                       19
      REAL FTRG_FTPITHK                                                   19
      EQUIVALENCE (BANK_FTRG(31),FTRG_FTPITHK,RE_FTRG_FTPITHK),           19
     *(NAMES_FTRG(29),NFTRG_FTPITHK)                                      19
      EQUIVALENCE (COMMEN_FTRG(29),CFTRG_FTPITHK),(MAP_FTRG(1,29),        19
     *MFTRG_FTPITHK)                                                      19
      CHARACTER CFTRG_FTPITHK*40/' '/                                     19
      CHARACTER NFTRG_FTPITHK*12/'R.FTPITHK'/                             19
      INTEGER MFTRG_FTPITHK(2)/1,1/                                       19
      REAL RE_FTRG_FTPITHK                                                19
C -                                                                       19
      REAL FTRG_FTPIR                                                     19
      EQUIVALENCE (BANK_FTRG(32),FTRG_FTPIR,RE_FTRG_FTPIR),               19
     *(NAMES_FTRG(30),NFTRG_FTPIR)                                        19
      EQUIVALENCE (COMMEN_FTRG(30),CFTRG_FTPIR),(MAP_FTRG(1,30),          19
     *MFTRG_FTPIR)                                                        19
      CHARACTER CFTRG_FTPIR*40/' '/                                       19
      CHARACTER NFTRG_FTPIR*12/'R.FTPIR'/                                 19
      INTEGER MFTRG_FTPIR(2)/1,1/                                         19
      REAL RE_FTRG_FTPIR                                                  19
C -                                                                       19
      REAL FTRG_SHELLTHK                                                  19
      EQUIVALENCE (BANK_FTRG(33),FTRG_SHELLTHK,RE_FTRG_SHELLTHK),         19
     *(NAMES_FTRG(31),NFTRG_SHELLTHK)                                     19
      EQUIVALENCE (COMMEN_FTRG(31),CFTRG_SHELLTHK),(MAP_FTRG(1,31),       19
     *MFTRG_SHELLTHK)                                                     19
      CHARACTER CFTRG_SHELLTHK*40/' '/                                    19
      CHARACTER NFTRG_SHELLTHK*12/'R.SHELLTHK'/                           19
      INTEGER MFTRG_SHELLTHK(2)/1,1/                                      19
      REAL RE_FTRG_SHELLTHK                                               19
C -                                                                       19
      REAL FTRG_RIMTHK                                                    19
      EQUIVALENCE (BANK_FTRG(34),FTRG_RIMTHK,RE_FTRG_RIMTHK),             19
     *(NAMES_FTRG(32),NFTRG_RIMTHK)                                       19
      EQUIVALENCE (COMMEN_FTRG(32),CFTRG_RIMTHK),(MAP_FTRG(1,32),         19
     *MFTRG_RIMTHK)                                                       19
      CHARACTER CFTRG_RIMTHK*40/' '/                                      19
      CHARACTER NFTRG_RIMTHK*12/'R.RIMTHK'/                               19
      INTEGER MFTRG_RIMTHK(2)/1,1/                                        19
      REAL RE_FTRG_RIMTHK                                                 19
C -                                                                       19
      REAL FTRG_RIMWTH                                                    19
      EQUIVALENCE (BANK_FTRG(35),FTRG_RIMWTH,RE_FTRG_RIMWTH),             19
     *(NAMES_FTRG(33),NFTRG_RIMWTH)                                       19
      EQUIVALENCE (COMMEN_FTRG(33),CFTRG_RIMWTH),(MAP_FTRG(1,33),         19
     *MFTRG_RIMWTH)                                                       19
      CHARACTER CFTRG_RIMWTH*40/' '/                                      19
      CHARACTER NFTRG_RIMWTH*12/'R.RIMWTH'/                               19
      INTEGER MFTRG_RIMWTH(2)/1,1/                                        19
      REAL RE_FTRG_RIMWTH                                                 19
C -                                                                       19
      REAL FTRG_PLANKWTH                                                  19
      EQUIVALENCE (BANK_FTRG(36),FTRG_PLANKWTH,RE_FTRG_PLANKWTH),         19
     *(NAMES_FTRG(34),NFTRG_PLANKWTH)                                     19
      EQUIVALENCE (COMMEN_FTRG(34),CFTRG_PLANKWTH),(MAP_FTRG(1,34),       19
     *MFTRG_PLANKWTH)                                                     19
      CHARACTER CFTRG_PLANKWTH*40/' '/                                    19
      CHARACTER NFTRG_PLANKWTH*12/'R.PLANKWTH'/                           19
      INTEGER MFTRG_PLANKWTH(2)/1,1/                                      19
      REAL RE_FTRG_PLANKWTH                                               19
C -                                                                       19
      REAL FTRG_PLANKTHK                                                  19
      EQUIVALENCE (BANK_FTRG(37),FTRG_PLANKTHK,RE_FTRG_PLANKTHK),         19
     *(NAMES_FTRG(35),NFTRG_PLANKTHK)                                     19
      EQUIVALENCE (COMMEN_FTRG(35),CFTRG_PLANKTHK),(MAP_FTRG(1,35),       19
     *MFTRG_PLANKTHK)                                                     19
      CHARACTER CFTRG_PLANKTHK*40/' '/                                    19
      CHARACTER NFTRG_PLANKTHK*12/'R.PLANKTHK'/                           19
      INTEGER MFTRG_PLANKTHK(2)/1,1/                                      19
      REAL RE_FTRG_PLANKTHK                                               19
C -                                                                       19
      INTEGER LEN_FTRG,LLEN_FTRG,LINK_FTRG                                19
      PARAMETER (LEN_FTRG=37)                                             19
      PARAMETER (LLEN_FTRG=35)                                            19
      INTEGER FLAG_FTRG/0/,MAP_FTRG(2,LLEN_FTRG),IDNUM_FTRG(10)           19
      CHARACTER NAMES_FTRG(LLEN_FTRG)*12,COMMEN_FTRG(LLEN_FTRG)*40        19
      DATA IDNUM_FTRG/10*0/                                               19
      REAL BANK_FTRG(LEN_FTRG)                                            19
      CHARACTER*60 BPATH_FTRG/' '/                                        19
      SAVE FLAG_FTRG,MAP_FTRG,IDNUM_FTRG,NAMES_FTRG,COMMEN_FTRG,          19
     *BPATH_FTRG                                                          19
      DATA LINK_FTRG/0/,BANK_FTRG/LEN_FTRG*0/                             19
C *                                                                       19
C *                                                                       20
      CALL AGSBEGM(MODULE,IPRIN)                                          20
      IF (IPRIN.LT.0) RETURN                                              20
C Filling bank FTRG - FTPC READOUT DATA                                   20
      BANK_TITLE='FTPC READOUT DATA'                                      20
      BPATH_FTRG='FTRG*'                                                  20
      IDNUM_FTRG(1)=0                                                     21
C VERSION                                                                 21
      FTRG_VERSION=1                                                      21
      CFTRG_VERSION='VERSION'                                             21
C NUMBER OF SECTORS                                                       22
      FTRG_N=5                                                            22
      CFTRG_N='NUMBER OF SECTORS'                                         22
C FTPC FACE FROM THE INTERACTION POINT                                    23
      FTRG_FTPCZ=150.0                                                    23
      CFTRG_FTPCZ='FTPC FACE FROM THE INTERACTION POINT'                  23
C FTPC OVERALL LENGTH                                                     24
      FTRG_FTPCLEN=119.0                                                  24
      CFTRG_FTPCLEN='FTPC OVERALL LENGTH'                                 24
C RO BARREL LENGTH                                                        25
      FTRG_LENGTH=80.0                                                    25
      CFTRG_LENGTH='RO BARREL LENGTH'                                     25
C THETA ANGLE OFFSET FOR THE WHLE STRUCTURE                               26
      FTRG_ANGOFFSET=0.0                                                  26
      CFTRG_ANGOFFSET='THETA ANGLE OFFSET FOR THE WHLE STRUCTURE'         26
C INNERMOST RADIUS OF ENVELOPE                                            27
      FTRG_RIN=15.0                                                       27
      CFTRG_RIN='INNERMOST RADIUS OF ENVELOPE'                            27
C OUTERMOST RADIUS OF ENVELOPE                                            28
      FTRG_ROUT=30.0                                                      28
      CFTRG_ROUT='OUTERMOST RADIUS OF ENVELOPE'                           28
C INNERMOST RADIUS OF THE INNER FLANGE                                    29
      FTRG_INRIN=22.0                                                     29
      CFTRG_INRIN='INNERMOST RADIUS OF THE INNER FLANGE'                  29
C OUTERMOST RADIUS OF THE INNER FLANGE                                    30
      FTRG_INROUT=30.0                                                    30
      CFTRG_INROUT='OUTERMOST RADIUS OF THE INNER FLANGE'                 30
C THICKNESS OF THE INNER FLANGE                                           31
      FTRG_INRTHK=0.15                                                    31
      CFTRG_INRTHK='THICKNESS OF THE INNER FLANGE'                        31
C INNERMOST RADIUS OF THE OUTER FLANGE                                    32
      FTRG_OFRIN=20.0                                                     32
      CFTRG_OFRIN='INNERMOST RADIUS OF THE OUTER FLANGE'                  32
C OUTERMOST RADIUS OF THE OUTER FLANGE                                    33
      FTRG_OFROUT=30.0                                                    33
      CFTRG_OFROUT='OUTERMOST RADIUS OF THE OUTER FLANGE'                 33
C THICKNESS OF THE OUTER FLANGE                                           34
      FTRG_OFRTHK=0.15                                                    34
      CFTRG_OFRTHK='THICKNESS OF THE OUTER FLANGE'                        34
C Z OF THE OUTER FLANGE INNER FACE                                        35
      FTRG_OFZ=79.0                                                       35
      CFTRG_OFZ='Z OF THE OUTER FLANGE INNER FACE'                        35
C NUMBER OF HOLES THE OUTER FLANGE                                        36
      FTRG_OFNHOLES=15.0                                                  36
      CFTRG_OFNHOLES='NUMBER OF HOLES THE OUTER FLANGE'                   36
C RADIUS AT WHICH THE HOLES THE OUTER FLANGE ARE LOCATED                  37
      FTRG_OFHOLERAD=26.0                                                 37
      CFTRG_OFHOLERAD='RADIUS AT WHICH THE HOLES THE OUTER FLANGE ARE LO  37
     *CATED'                                                              37
C RADIUS OF THE HOLES THE OUTER FLANGE                                    38
      FTRG_OFHOLER=2.5                                                    38
      CFTRG_OFHOLER='RADIUS OF THE HOLES THE OUTER FLANGE'                38
C LENGTH OF THE STRUT                                                     39
      FTRG_STRUTLEN=50.0                                                  39
      CFTRG_STRUTLEN='LENGTH OF THE STRUT'                                39
C LENGTH OF THE STRUT                                                     40
      FTRG_STRUTHGT=6.5                                                   40
      CFTRG_STRUTHGT='LENGTH OF THE STRUT'                                40
C WIDTH OF THE STRUT                                                      41
      FTRG_STRUTWTH=2.0                                                   41
      CFTRG_STRUTWTH='WIDTH OF THE STRUT'                                 41
C THICKNESS OF THE STRUT MATERIAL                                         42
      FTRG_STRUTTHK=0.25                                                  42
      CFTRG_STRUTTHK='THICKNESS OF THE STRUT MATERIAL'                    42
C STRUT HOLE RADIUS                                                       43
      FTRG_STRUTHOLER=1.0                                                 43
      CFTRG_STRUTHOLER='STRUT HOLE RADIUS'                                43
C NUMBER OF HOLES IN THE STRUT                                            44
      FTRG_STRUTNHOLES=5                                                  44
      CFTRG_STRUTNHOLES='NUMBER OF HOLES IN THE STRUT'                    44
C OUTER HALF-PCB WIDTH                                                    45
      FTRG_FTPOWTH=13.0                                                   45
      CFTRG_FTPOWTH='OUTER HALF-PCB WIDTH'                                45
C OUTER HALF-PCB THICKNESS                                                46
      FTRG_FTPOTHK=0.3                                                    46
      CFTRG_FTPOTHK='OUTER HALF-PCB THICKNESS'                            46
C OUTER HALF-PCB RADIAL POSITION (TO SURFACE)                             47
      FTRG_FTPOR=22.0                                                     47
      CFTRG_FTPOR='OUTER HALF-PCB RADIAL POSITION (TO SURFACE)'           47
C INNER HALF-PCB WIDTH                                                    48
      FTRG_FTPIWTH=12.0                                                   48
      CFTRG_FTPIWTH='INNER HALF-PCB WIDTH'                                48
C INNER HALF-PCB THICKNESS                                                49
      FTRG_FTPITHK=0.3                                                    49
      CFTRG_FTPITHK='INNER HALF-PCB THICKNESS'                            49
C INNER HALF-PCB RADIAL POSITION (TO SURFACE)                             50
      FTRG_FTPIR=20.0                                                     50
      CFTRG_FTPIR='INNER HALF-PCB RADIAL POSITION (TO SURFACE)'           50
C OUTER PROTECTIVE SHELL THICKNESS                                        51
      FTRG_SHELLTHK=0.1                                                   51
      CFTRG_SHELLTHK='OUTER PROTECTIVE SHELL THICKNESS'                   51
C CYLINDRICAL TIM THICKNESS                                               52
      FTRG_RIMTHK=0.1                                                     52
      CFTRG_RIMTHK='CYLINDRICAL TIM THICKNESS'                            52
C CYLINDRICAL TIM WIDTH                                                   53
      FTRG_RIMWTH=5.0                                                     53
      CFTRG_RIMWTH='CYLINDRICAL TIM WIDTH'                                53
C THE WIDTH OF THE PLANK COVERING THE STRUT                               54
      FTRG_PLANKWTH=8.0                                                   54
      CFTRG_PLANKWTH='THE WIDTH OF THE PLANK COVERING THE STRUT'          54
C THE THICKNESS OF THE PLANK COVERING THE STRUT                           55
      FTRG_PLANKTHK=0.15                                                  55
      CFTRG_PLANKTHK='THE THICKNESS OF THE PLANK COVERING THE STRUT'      55
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FTRG',    56
     *BPATH_FTRG,IDNUM_FTRG,1, MAP_FTRG,NAMES_FTRG,COMMEN_FTRG,           56
     *BANK_FTRG,LEN_FTRG,LLEN_FTRG,LINK_FTRG,FLAG_FTRG)                   56
C *                                                                       56
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'FTRG',    56
     *BPATH_FTRG,IDNUM_FTRG,1, MAP_FTRG,NAMES_FTRG,COMMEN_FTRG,           56
     *BANK_FTRG,LEN_FTRG,LLEN_FTRG,LINK_FTRG,FLAG_FTRG)                   56
C use default FTRG bank                                                   57
      BPATH_FTRG='FTRG*'                                                  57
      IDNUM_FTRG(1)=0                                                     57
      CALL AGDGETP('FTROGEO','*',-999,'FTRG',' ',BANK_FTRG, BPATH_FTRG,   57
     *IDNUM_FTRG,1,MAP_FTRG,NAMES_FTRG,BANK_FTRG,LEN_FTRG,LLEN_FTRG,      57
     *LINK_FTRG,FLAG_FTRG)                                                57
C Geant COMPONENT : SI ===> A=28.08 Z=14 W=0.6*1*28./60. <===             58
      CALL AGSRESET                                                       58
      AG_A = 28.08                                                        58
      AG_Z = 14                                                           58
      AG_W = 0.6*1*28./60.                                                58
      AG_TITLE = 'SI'                                                     58
      AG_EXNAME = 'COMPONENT'                                             58
      AG_PARLIST = 'A_Z_W'                                                58
      CALL AXCOMPONENT                                                    58
C *                                                                       58
C Geant COMPONENT : O ===> A=16 Z=8 W=0.6*2*16./60. + 0.4*4*16./174. <==  58
C =                                                                       58
      CALL AGSRESET                                                       59
      AG_A = 16                                                           59
      AG_Z = 8                                                            59
      AG_W = 0.6*2*16./60. + 0.4*4*16./174.                               59
      AG_TITLE = 'O'                                                      59
      AG_EXNAME = 'COMPONENT'                                             59
      AG_PARLIST = 'A_Z_W'                                                59
      CALL AXCOMPONENT                                                    59
C *                                                                       59
C Geant COMPONENT : C ===> A=12 Z=6 W=0.4*8*12./174. <===                 59
      CALL AGSRESET                                                       60
      AG_A = 12                                                           60
      AG_Z = 6                                                            60
      AG_W = 0.4*8*12./174.                                               60
      AG_TITLE = 'C'                                                      60
      AG_EXNAME = 'COMPONENT'                                             60
      AG_PARLIST = 'A_Z_W'                                                60
      CALL AXCOMPONENT                                                    60
C *                                                                       60
C Geant COMPONENT : H ===> A=1 Z=1 W=0.4*14*1./174. <===                  60
      CALL AGSRESET                                                       61
      AG_A = 1                                                            61
      AG_Z = 1                                                            61
      AG_W = 0.4*14*1./174.                                               61
      AG_TITLE = 'H'                                                      61
      AG_EXNAME = 'COMPONENT'                                             61
      AG_PARLIST = 'A_Z_W'                                                61
      CALL AXCOMPONENT                                                    61
C *                                                                       61
C Geant MIXTURE : G10 ===> DENS=1.7 <===                                  61
      CALL AGSRESET                                                       62
      AG_DENS = 1.7                                                       62
      AG_TITLE = 'G10'                                                    62
      AG_EXNAME = 'MIXTURE'                                               62
      AG_PARLIST = 'DENS'                                                 62
      CALL AXMIXTURE                                                      62
C *                                                                       62
      MYRADDEG = 3.14159265/180.0                                         62
      HOLEANGLE = 360.0/FTRG_OFNHOLES                                     64
      STRUTANGLE = 360.0/FTRG_N                                           65
      STRUTRAD = FTRG_OFRIN+FTRG_STRUTHGT/2.0                             66
      PLANKRAD = STRUTRAD+FTRG_STRUTHGT/2.0+FTRG_PLANKTHK/2.0             67
      STRUTHOLESPACING = FTRG_STRUTLEN/(FTRG_STRUTNHOLES+1)               68
      ZPOS = FTRG_FTPCZ+FTRG_FTPCLEN+FTRG_LENGTH/2.0                      69
      CENTERSTRUT= FTRG_OFZ-FTRG_LENGTH/2.0-FTRG_STRUTLEN/2.0             70
      INFLANGEZ = CENTERSTRUT-FTRG_STRUTLEN/2.0                           71
      FTPOANGLEPOS=ATAN(FTRG_FTPOWTH/((FTRG_FTPOR+FTRG_FTPOTHK/           72
     *2.0)*2.0))                                                          72
      FTPIANGLEPOS=ATAN(FTRG_FTPIWTH/((FTRG_FTPIR+FTRG_FTPITHK/           73
     *2.0)*2.0))                                                          73
      CALL AGSBEGM(MODULE,IPRIN)                                          74
C Check IPRIN<0                                                           74
      IF (IPRIN.LT.0) RETURN                                              74
C Create FTMO :                                                           74
C Call To Local Procedure FTMO                                            74
      ASSIGN 5020 TO BLOCK_FTMO                                           74
      GO TO  5010                                                         74
5020  CONTINUE                                                            74
C *                                                                       74
C Geant POSITION : FTMO ===> MOTHER='CAVE' Z=ZPOS <===                    75
      CALL AGSRESET                                                       75
      AG_MOTHER = 'CAVE'                                                  75
      AG_Z = ZPOS                                                         75
      AG_TITLE = 'FTMO'                                                   75
      AG_EXNAME = 'POSITION'                                              75
      AG_PARLIST = 'MOTHER_Z'                                             75
      CALL AXPOSITION                                                     75
C *                                                                       75
C Geant POSITION : FTMO ===> MOTHER='CAVE' Z=-ZPOS ALPHAZ=180. <===       75
      CALL AGSRESET                                                       76
      AG_MOTHER = 'CAVE'                                                  76
      AG_Z = -ZPOS                                                        76
      AG_ALPHAN=AG_ALPHAN+1                                               76
      AG_ALPHAK(AG_ALPHAN)=3                                              76
      AG_ALPHAL(AG_ALPHAN)=180.                                           76
      AG_TITLE = 'FTMO'                                                   76
      AG_EXNAME = 'POSITION'                                              76
      AG_PARLIST = 'MOTHER_Z_ALPHAZ'                                      76
      CALL AXPOSITION                                                     76
C *                                                                       76
C ----------------------------------------------------------------------  76
C ------------ FTMO                                                       76
      CALL AGSENDM                                                        78
      RETURN                                                              78
C Begin Local Procedure FTMO                                              78
C FTMO IS THE MOTHER OF THE SINGLE FTPC RO BARREL                         78
5010  CONTINUE                                                            78
         AG_TITLE='FTMO'                                                  78
         AG_MODULE=MODULE                                                 78
         AG_MARK='V00'                                                    78
         CALL AGSPUSH                                                     78
         AG_MODULE=MODULE                                                 78
C    *                                                                    78
C    Geant MATERIAL : AIR ===> inherited arguments used <===              79
         CALL AGSRESET                                                    79
         AG_TITLE = 'AIR'                                                 79
         AG_EXNAME = 'MATERIAL'                                           79
         AG_PARLIST = 'NONE'                                              79
         CALL AXMATERIAL                                                  79
C    *                                                                    79
C    Geant ATTRIBUTE : FTMO ===> SEEN=1 COLO=6 <===                       79
         CALL AGSRESET                                                    80
         AG_SEEN = 1                                                      80
         AG_COLO = 6                                                      80
         AG_TITLE = 'FTMO'                                                80
         AG_EXNAME = 'ATTRIBUTE'                                          80
         AG_PARLIST = 'SEEN_COLO'                                         80
         CALL AXATTRIBUTE                                                 80
C    *                                                                    80
C    Geant SHAPE : TUBE ===> RMIN=FTRG_RIN RMAX=FTRG_ROUT, DZ=FTRG_LENGT  80
C    H/2.0 <===                                                           80
         CALL AGSRESET                                                    82
         AG_RMIN = FTRG_RIN                                               82
         AG_RMAX = FTRG_ROUT                                              82
         AG_DZ = FTRG_LENGTH/2.0                                          82
         AG_TITLE = 'TUBE'                                                82
         AG_EXNAME = 'SHAPE'                                              82
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      82
         CALL AXSHAPE                                                     82
C    *                                                                    82
C    Create FTCM :                                                        83
C    Call To Local Procedure FTCM                                         83
         ASSIGN 5040 TO BLOCK_FTCM                                        83
         GO TO  5030                                                      83
5040     CONTINUE                                                         83
C    *                                                                    83
C    Geant POSITION : FTCM ===> Z=CENTERSTRUT <===                        84
         CALL AGSRESET                                                    84
         AG_Z = CENTERSTRUT                                               84
         AG_TITLE = 'FTCM'                                                84
         AG_EXNAME = 'POSITION'                                           84
         AG_PARLIST = 'Z'                                                 84
         CALL AXPOSITION                                                  84
C    *                                                                    84
C    Create FTOF :                                                        85
C    Call To Local Procedure FTOF                                         85
         ASSIGN 5060 TO BLOCK_FTOF                                        85
         GO TO  5050                                                      85
5060     CONTINUE                                                         85
C    *                                                                    85
C    Geant POSITION : FTOF ===> X=0.0 Y=0.0 Z=FTRG_OFZ-FTRG_LENGTH/2.0+F  86
C    TRG_OFRTHK <===                                                      86
         CALL AGSRESET                                                    86
         AG_X = 0.0                                                       86
         AG_Y = 0.0                                                       86
         AG_Z = FTRG_OFZ-FTRG_LENGTH/2.0+FTRG_OFRTHK                      86
         AG_TITLE = 'FTOF'                                                86
         AG_EXNAME = 'POSITION'                                           86
         AG_PARLIST = 'X_Y_Z'                                             86
         CALL AXPOSITION                                                  86
C    *                                                                    86
C    Create FTIF :                                                        87
C    Call To Local Procedure FTIF                                         87
         ASSIGN 5080 TO BLOCK_FTIF                                        87
         GO TO  5070                                                      87
5080     CONTINUE                                                         87
C    *                                                                    87
C    Geant POSITION : FTIF ===> X=0.0 Y=0.0 Z=INFLANGEZ-FTRG_INRTHK/2.0   88
C    <===                                                                 88
         CALL AGSRESET                                                    88
         AG_X = 0.0                                                       88
         AG_Y = 0.0                                                       88
         AG_Z = INFLANGEZ-FTRG_INRTHK/2.0                                 88
         AG_TITLE = 'FTIF'                                                88
         AG_EXNAME = 'POSITION'                                           88
         AG_PARLIST = 'X_Y_Z'                                             88
         CALL AXPOSITION                                                  88
C    *                                                                    88
C    Create FTRM :                                                        89
C    Call To Local Procedure FTRM                                         89
         ASSIGN 5100 TO BLOCK_FTRM                                        89
         GO TO  5090                                                      89
5100     CONTINUE                                                         89
C    *                                                                    89
C    Geant POSITION : FTRM ===> X=0.0 Y=0.0 Z=INFLANGEZ-FTRG_INRTHK-FTRG  90
C    _RIMWTH/2.0 <===                                                     90
         CALL AGSRESET                                                    90
         AG_X = 0.0                                                       90
         AG_Y = 0.0                                                       90
         AG_Z = INFLANGEZ-FTRG_INRTHK-FTRG_RIMWTH/2.0                     90
         AG_TITLE = 'FTRM'                                                90
         AG_EXNAME = 'POSITION'                                           90
         AG_PARLIST = 'X_Y_Z'                                             90
         CALL AXPOSITION                                                  90
C    *                                                                    90
5011  CALL AGSPOPB                                                        91
      GO TO BLOCK_FTMO, (5010 ,5020)                                      91
C End of Local Procedure                                                  91
C ----------------------------------------------------------------------  92
C ------------ FTCM                                                       92
C Begin Local Procedure FTCM                                              93
C FTCM IS THE MOTHER OF THE CORE STRUTS AND PCBS                          93
5030  CONTINUE                                                            93
         AG_TITLE='FTCM'                                                  93
         AG_MODULE=MODULE                                                 93
         AG_MARK='V00'                                                    93
         CALL AGSPUSH                                                     93
         AG_MODULE=MODULE                                                 93
C    *                                                                    93
C    Geant ATTRIBUTE : FTCM ===> SEEN=0 COLO=0 <===                       94
         CALL AGSRESET                                                    94
         AG_SEEN = 0                                                      94
         AG_COLO = 0                                                      94
         AG_TITLE = 'FTCM'                                                94
         AG_EXNAME = 'ATTRIBUTE'                                          94
         AG_PARLIST = 'SEEN_COLO'                                         94
         CALL AXATTRIBUTE                                                 94
C    *                                                                    94
C    Geant SHAPE : TUBE ===> RMIN=FTRG_RIN RMAX=FTRG_ROUT, DZ=FTRG_STRUT  94
C    LEN/2.0 <===                                                         94
         CALL AGSRESET                                                    96
         AG_RMIN = FTRG_RIN                                               96
         AG_RMAX = FTRG_ROUT                                              96
         AG_DZ = FTRG_STRUTLEN/2.0                                        96
         AG_TITLE = 'TUBE'                                                96
         AG_EXNAME = 'SHAPE'                                              96
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      96
         CALL AXSHAPE                                                     96
C    *                                                                    96
C    Create FTCD :                                                        97
C    Call To Local Procedure FTCD                                         97
         ASSIGN 5120 TO BLOCK_FTCD                                        97
         GO TO  5110                                                      97
5120     CONTINUE                                                         97
C    *                                                                    97
5031  CALL AGSPOPB                                                        98
      GO TO BLOCK_FTCM, (5030 ,5040)                                      98
C End of Local Procedure                                                  98
C ----------------------------------------------------------------------  99
C ------------ FTCD                                                       99
C Begin Local Procedure FTCD                                              100
C FTCD IS THE DIVISION OF THE FTCM                                        100
5110  CONTINUE                                                            100
         AG_TITLE='FTCD'                                                  100
         AG_MODULE=MODULE                                                 100
         AG_MARK='V00'                                                    100
         CALL AGSPUSH                                                     100
         AG_MODULE=MODULE                                                 100
C    *                                                                    100
C    Geant SHAPE : DIVISION ===> NDIV=FTRG_N IAXIS=2 <===                 101
         CALL AGSRESET                                                    101
         AG_NDIV = FTRG_N                                                 101
         AG_IAXIS = 2                                                     101
         AG_TITLE = 'DIVISION'                                            101
         AG_EXNAME = 'SHAPE'                                              101
         AG_PARLIST = 'NDIV_IAXIS'                                        101
         CALL AXSHAPE                                                     101
C    *                                                                    101
C    Create FSMO :                                                        102
C    Call To Local Procedure FSMO                                         102
         ASSIGN 5140 TO BLOCK_FSMO                                        102
         GO TO  5130                                                      102
5140     CONTINUE                                                         102
C    *                                                                    102
C    Geant POSITION : FSMO ===> X=STRUTRAD*COS(0.0) Y=STRUTRAD*SIN(0.0)   103
C    Z=0.0 ALPHAZ=90 <===                                                 103
         CALL AGSRESET                                                    103
         AG_X = STRUTRAD*COS(0.0)                                         103
         AG_Y = STRUTRAD*SIN(0.0)                                         103
         AG_Z = 0.0                                                       103
         AG_ALPHAN=AG_ALPHAN+1                                            103
         AG_ALPHAK(AG_ALPHAN)=3                                           103
         AG_ALPHAL(AG_ALPHAN)=90                                          103
         AG_TITLE = 'FSMO'                                                103
         AG_EXNAME = 'POSITION'                                           103
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      103
         CALL AXPOSITION                                                  103
C    *                                                                    103
C    Create FTPL :                                                        104
C    Call To Local Procedure FTPL                                         104
         ASSIGN 5160 TO BLOCK_FTPL                                        104
         GO TO  5150                                                      104
5160     CONTINUE                                                         104
C    *                                                                    104
C    Geant POSITION : FTPL ===> X=PLANKRAD*COS(0.0) Y=PLANKRAD*SIN(0.0)   105
C    Z=0.0 ALPHAZ=90 <===                                                 105
         CALL AGSRESET                                                    105
         AG_X = PLANKRAD*COS(0.0)                                         105
         AG_Y = PLANKRAD*SIN(0.0)                                         105
         AG_Z = 0.0                                                       105
         AG_ALPHAN=AG_ALPHAN+1                                            105
         AG_ALPHAK(AG_ALPHAN)=3                                           105
         AG_ALPHAL(AG_ALPHAN)=90                                          105
         AG_TITLE = 'FTPL'                                                105
         AG_EXNAME = 'POSITION'                                           105
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      105
         CALL AXPOSITION                                                  105
C    *                                                                    105
         A=MYRADDEG*STRUTANGLE/2.0 - FTPOANGLEPOS                         105
         REFF=(FTRG_FTPOR + FTRG_FTPOTHK/2.0)/COS(FTPOANGLEPOS)           107
C    Create FTPO :                                                        108
C    Call To Local Procedure FTPO                                         108
         ASSIGN 5180 TO BLOCK_FTPO                                        108
         GO TO  5170                                                      108
5180     CONTINUE                                                         108
C    *                                                                    108
C    Geant POSITION : FTPO ===> X=REFF*COS(A) Y=REFF*SIN(A) Z=0.0 ALPHAZ  109
C    =90+STRUTANGLE/2.0 <===                                              109
         CALL AGSRESET                                                    109
         AG_X = REFF*COS(A)                                               109
         AG_Y = REFF*SIN(A)                                               109
         AG_Z = 0.0                                                       109
         AG_ALPHAN=AG_ALPHAN+1                                            109
         AG_ALPHAK(AG_ALPHAN)=3                                           109
         AG_ALPHAL(AG_ALPHAN)=90+STRUTANGLE/2.0                           109
         AG_TITLE = 'FTPO'                                                109
         AG_EXNAME = 'POSITION'                                           109
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      109
         CALL AXPOSITION                                                  109
C    *                                                                    109
C    Geant POSITION : FTPO ===> X=REFF*COS(-A) Y=REFF*SIN(-A) Z=0.0 ALPH  109
C    AZ=-90-STRUTANGLE/2.0 <===                                           109
         CALL AGSRESET                                                    110
         AG_X = REFF*COS(-A)                                              110
         AG_Y = REFF*SIN(-A)                                              110
         AG_Z = 0.0                                                       110
         AG_ALPHAN=AG_ALPHAN+1                                            110
         AG_ALPHAK(AG_ALPHAN)=3                                           110
         AG_ALPHAL(AG_ALPHAN)=-90-STRUTANGLE/2.0                          110
         AG_TITLE = 'FTPO'                                                110
         AG_EXNAME = 'POSITION'                                           110
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      110
         CALL AXPOSITION                                                  110
C    *                                                                    110
         A=MYRADDEG*STRUTANGLE/2.0 - FTPIANGLEPOS                         110
         REFF=(FTRG_FTPIR + FTRG_FTPITHK/2.0)/COS(FTPIANGLEPOS)           112
C    Create FTPI :                                                        113
C    Call To Local Procedure FTPI                                         113
         ASSIGN 5200 TO BLOCK_FTPI                                        113
         GO TO  5190                                                      113
5200     CONTINUE                                                         113
C    *                                                                    113
C    Geant POSITION : FTPI ===> X=REFF*COS(A) Y=REFF*SIN(A) Z=0.0 ALPHAZ  114
C    =90+STRUTANGLE/2.0 <===                                              114
         CALL AGSRESET                                                    114
         AG_X = REFF*COS(A)                                               114
         AG_Y = REFF*SIN(A)                                               114
         AG_Z = 0.0                                                       114
         AG_ALPHAN=AG_ALPHAN+1                                            114
         AG_ALPHAK(AG_ALPHAN)=3                                           114
         AG_ALPHAL(AG_ALPHAN)=90+STRUTANGLE/2.0                           114
         AG_TITLE = 'FTPI'                                                114
         AG_EXNAME = 'POSITION'                                           114
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      114
         CALL AXPOSITION                                                  114
C    *                                                                    114
C    Geant POSITION : FTPI ===> X=REFF*COS(-A) Y=REFF*SIN(-A) Z=0.0 ALPH  114
C    AZ=-90-STRUTANGLE/2.0 <===                                           114
         CALL AGSRESET                                                    115
         AG_X = REFF*COS(-A)                                              115
         AG_Y = REFF*SIN(-A)                                              115
         AG_Z = 0.0                                                       115
         AG_ALPHAN=AG_ALPHAN+1                                            115
         AG_ALPHAK(AG_ALPHAN)=3                                           115
         AG_ALPHAL(AG_ALPHAN)=-90-STRUTANGLE/2.0                          115
         AG_TITLE = 'FTPI'                                                115
         AG_EXNAME = 'POSITION'                                           115
         AG_PARLIST = 'X_Y_Z_ALPHAZ'                                      115
         CALL AXPOSITION                                                  115
C    *                                                                    115
C    Create FTSH :                                                        116
C    Call To Local Procedure FTSH                                         116
         ASSIGN 5220 TO BLOCK_FTSH                                        116
         GO TO  5210                                                      116
5220     CONTINUE                                                         116
C    *                                                                    116
C    Geant POSITION : FTSH ===> inherited arguments used <===             117
         CALL AGSRESET                                                    117
         AG_TITLE = 'FTSH'                                                117
         AG_EXNAME = 'POSITION'                                           117
         AG_PARLIST = 'NONE'                                              117
         CALL AXPOSITION                                                  117
C    *                                                                    117
5111  CALL AGSPOPB                                                        118
      GO TO BLOCK_FTCD, (5110 ,5120)                                      118
C End of Local Procedure                                                  118
C ----------------------------------------------------------------------  119
C ------------ FTOF                                                       119
C Begin Local Procedure FTOF                                              120
C FTOF IS THE OUTER FLANGE                                                120
5050  CONTINUE                                                            120
         AG_TITLE='FTOF'                                                  120
         AG_MODULE=MODULE                                                 120
         AG_MARK='V00'                                                    120
         CALL AGSPUSH                                                     120
         AG_MODULE=MODULE                                                 120
C    *                                                                    120
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        121
         CALL AGSRESET                                                    121
         AG_TITLE = 'ALUMINIUM'                                           121
         AG_EXNAME = 'MATERIAL'                                           121
         AG_PARLIST = 'NONE'                                              121
         CALL AXMATERIAL                                                  121
C    *                                                                    121
C    Geant ATTRIBUTE : FTOF ===> SEEN=1 COLO=2 <===                       121
         CALL AGSRESET                                                    122
         AG_SEEN = 1                                                      122
         AG_COLO = 2                                                      122
         AG_TITLE = 'FTOF'                                                122
         AG_EXNAME = 'ATTRIBUTE'                                          122
         AG_PARLIST = 'SEEN_COLO'                                         122
         CALL AXATTRIBUTE                                                 122
C    *                                                                    122
C    Geant SHAPE : TUBE ===> RMIN=FTRG_OFRIN RMAX=FTRG_OFROUT, DZ=FTRG_O  122
C    FRTHK/2.0 <===                                                       122
         CALL AGSRESET                                                    124
         AG_RMIN = FTRG_OFRIN                                             124
         AG_RMAX = FTRG_OFROUT                                            124
         AG_DZ = FTRG_OFRTHK/2.0                                          124
         AG_TITLE = 'TUBE'                                                124
         AG_EXNAME = 'SHAPE'                                              124
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      124
         CALL AXSHAPE                                                     124
C    *                                                                    124
C    Create FTOH :                                                        125
C    Call To Local Procedure FTOH                                         125
         ASSIGN 5240 TO BLOCK_FTOH                                        125
         GO TO  5230                                                      125
5240     CONTINUE                                                         125
C    *                                                                    125
C    *                                                                    126
         NHOLE=1                                                          126
            GO TO 5253                                                    126
5251        NHOLE=NHOLE+1                                                 126
5253        IF(NHOLE-(FTRG_OFNHOLES).GT.0) GO TO 5252                     126
            ANGLE=(NHOLE-1)*HOLEANGLE                                     127
            ANGLEPOS = ANGLE*MYRADDEG                                     128
C       Geant POSITION : FTOH ===> X=FTRG_OFHOLERAD*COS(ANGLEPOS) Y=FTRG  129
C       _OFHOLERAD*SIN(ANGLEPOS) <===                                     129
            CALL AGSRESET                                                 129
            AG_X = FTRG_OFHOLERAD*COS(ANGLEPOS)                           129
            AG_Y = FTRG_OFHOLERAD*SIN(ANGLEPOS)                           129
            AG_TITLE = 'FTOH'                                             129
            AG_EXNAME = 'POSITION'                                        129
            AG_PARLIST = 'X_Y'                                            129
            CALL AXPOSITION                                               129
C       *                                                                 129
         GO TO 5251                                                       130
5252     CONTINUE                                                         130
5051  CALL AGSPOPB                                                        131
      GO TO BLOCK_FTOF, (5050 ,5060)                                      131
C End of Local Procedure                                                  131
C ----------------------------------------------------------------------  132
C ------------ FTIF                                                       132
C Begin Local Procedure FTIF                                              133
C FTIF IS THE INNER FLANGE                                                133
5070  CONTINUE                                                            133
         AG_TITLE='FTIF'                                                  133
         AG_MODULE=MODULE                                                 133
         AG_MARK='V00'                                                    133
         CALL AGSPUSH                                                     133
         AG_MODULE=MODULE                                                 133
C    *                                                                    133
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        134
         CALL AGSRESET                                                    134
         AG_TITLE = 'ALUMINIUM'                                           134
         AG_EXNAME = 'MATERIAL'                                           134
         AG_PARLIST = 'NONE'                                              134
         CALL AXMATERIAL                                                  134
C    *                                                                    134
C    Geant ATTRIBUTE : FTIF ===> SEEN=1 COLO=2 <===                       134
         CALL AGSRESET                                                    135
         AG_SEEN = 1                                                      135
         AG_COLO = 2                                                      135
         AG_TITLE = 'FTIF'                                                135
         AG_EXNAME = 'ATTRIBUTE'                                          135
         AG_PARLIST = 'SEEN_COLO'                                         135
         CALL AXATTRIBUTE                                                 135
C    *                                                                    135
C    Geant SHAPE : TUBE ===> RMIN=FTRG_INRIN RMAX=FTRG_INROUT, DZ=FTRG_I  135
C    NRTHK/2.0 <===                                                       135
         CALL AGSRESET                                                    137
         AG_RMIN = FTRG_INRIN                                             137
         AG_RMAX = FTRG_INROUT                                            137
         AG_DZ = FTRG_INRTHK/2.0                                          137
         AG_TITLE = 'TUBE'                                                137
         AG_EXNAME = 'SHAPE'                                              137
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      137
         CALL AXSHAPE                                                     137
C    *                                                                    137
5071  CALL AGSPOPB                                                        138
      GO TO BLOCK_FTIF, (5070 ,5080)                                      138
C End of Local Procedure                                                  138
C ----------------------------------------------------------------------  139
C ------------ FTRM                                                       139
C Begin Local Procedure FTRM                                              140
C FTRM IS THE RIM CONNECTED TO THE INNER FLANGE                           140
5090  CONTINUE                                                            140
         AG_TITLE='FTRM'                                                  140
         AG_MODULE=MODULE                                                 140
         AG_MARK='V00'                                                    140
         CALL AGSPUSH                                                     140
         AG_MODULE=MODULE                                                 140
C    *                                                                    140
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        141
         CALL AGSRESET                                                    141
         AG_TITLE = 'ALUMINIUM'                                           141
         AG_EXNAME = 'MATERIAL'                                           141
         AG_PARLIST = 'NONE'                                              141
         CALL AXMATERIAL                                                  141
C    *                                                                    141
C    Geant ATTRIBUTE : FTRM ===> SEEN=1 COLO=2 <===                       141
         CALL AGSRESET                                                    142
         AG_SEEN = 1                                                      142
         AG_COLO = 2                                                      142
         AG_TITLE = 'FTRM'                                                142
         AG_EXNAME = 'ATTRIBUTE'                                          142
         AG_PARLIST = 'SEEN_COLO'                                         142
         CALL AXATTRIBUTE                                                 142
C    *                                                                    142
C    Geant SHAPE : TUBE ===> RMIN=FTRG_INRIN RMAX=FTRG_INRIN+FTRG_RIMTHK  142
C    , DZ=FTRG_RIMWTH/2.0 <===                                            142
         CALL AGSRESET                                                    144
         AG_RMIN = FTRG_INRIN                                             144
         AG_RMAX = FTRG_INRIN+FTRG_RIMTHK                                 144
         AG_DZ = FTRG_RIMWTH/2.0                                          144
         AG_TITLE = 'TUBE'                                                144
         AG_EXNAME = 'SHAPE'                                              144
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      144
         CALL AXSHAPE                                                     144
C    *                                                                    144
5091  CALL AGSPOPB                                                        145
      GO TO BLOCK_FTRM, (5090 ,5100)                                      145
C End of Local Procedure                                                  145
C ----------------------------------------------------------------------  146
C ------------ FTOH                                                       146
C Begin Local Procedure FTOH                                              147
C FTOH IS A HOLE THE OUTER FLANGE                                         147
5230  CONTINUE                                                            147
         AG_TITLE='FTOH'                                                  147
         AG_MODULE=MODULE                                                 147
         AG_MARK='V00'                                                    147
         CALL AGSPUSH                                                     147
         AG_MODULE=MODULE                                                 147
C    *                                                                    147
C    Geant MATERIAL : AIR ===> inherited arguments used <===              148
         CALL AGSRESET                                                    148
         AG_TITLE = 'AIR'                                                 148
         AG_EXNAME = 'MATERIAL'                                           148
         AG_PARLIST = 'NONE'                                              148
         CALL AXMATERIAL                                                  148
C    *                                                                    148
C    Geant ATTRIBUTE : FTOF ===> SEEN=1 COLO=2 <===                       148
         CALL AGSRESET                                                    149
         AG_SEEN = 1                                                      149
         AG_COLO = 2                                                      149
         AG_TITLE = 'FTOF'                                                149
         AG_EXNAME = 'ATTRIBUTE'                                          149
         AG_PARLIST = 'SEEN_COLO'                                         149
         CALL AXATTRIBUTE                                                 149
C    *                                                                    149
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=FTRG_OFHOLER DZ=FTRG_OFRTHK/2  149
C    .0 <===                                                              149
         CALL AGSRESET                                                    150
         AG_RMIN = 0.0                                                    150
         AG_RMAX = FTRG_OFHOLER                                           150
         AG_DZ = FTRG_OFRTHK/2.0                                          150
         AG_TITLE = 'TUBE'                                                150
         AG_EXNAME = 'SHAPE'                                              150
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      150
         CALL AXSHAPE                                                     150
C    *                                                                    150
5231  CALL AGSPOPB                                                        151
      GO TO BLOCK_FTOH, (5230 ,5240)                                      151
C End of Local Procedure                                                  151
C ----------------------------------------------------------------------  152
C ------------ FSMO                                                       152
C Begin Local Procedure FSMO                                              153
C FSMO IS THE MOTHER OF THE STRUT                                         153
5130  CONTINUE                                                            153
         AG_TITLE='FSMO'                                                  153
         AG_MODULE=MODULE                                                 153
         AG_MARK='V00'                                                    153
         CALL AGSPUSH                                                     153
         AG_MODULE=MODULE                                                 153
C    *                                                                    153
C    Geant MATERIAL : AIR ===> inherited arguments used <===              154
         CALL AGSRESET                                                    154
         AG_TITLE = 'AIR'                                                 154
         AG_EXNAME = 'MATERIAL'                                           154
         AG_PARLIST = 'NONE'                                              154
         CALL AXMATERIAL                                                  154
C    *                                                                    154
C    Geant ATTRIBUTE : FSMO ===> SEEN=0 COLO=0 <===                       154
         CALL AGSRESET                                                    155
         AG_SEEN = 0                                                      155
         AG_COLO = 0                                                      155
         AG_TITLE = 'FSMO'                                                155
         AG_EXNAME = 'ATTRIBUTE'                                          155
         AG_PARLIST = 'SEEN_COLO'                                         155
         CALL AXATTRIBUTE                                                 155
C    *                                                                    155
C    Geant SHAPE : BOX ===> DX=FTRG_STRUTWTH/2.0 DY=FTRG_STRUTHGT/2.0, D  155
C    Z=FTRG_STRUTLEN/2.0 <===                                             155
         CALL AGSRESET                                                    157
         AG_DX = FTRG_STRUTWTH/2.0                                        157
         AG_DY = FTRG_STRUTHGT/2.0                                        157
         AG_DZ = FTRG_STRUTLEN/2.0                                        157
         AG_TITLE = 'BOX'                                                 157
         AG_EXNAME = 'SHAPE'                                              157
         AG_PARLIST = 'DX_DY_DZ'                                          157
         CALL AXSHAPE                                                     157
C    *                                                                    157
C    Create FSTL :                                                        158
C    Call To Local Procedure FSTL                                         158
         ASSIGN 5270 TO BLOCK_FSTL                                        158
         GO TO  5260                                                      158
5270     CONTINUE                                                         158
C    *                                                                    158
C    Geant POSITION : FSTL ===> X=0.0 Y=-FTRG_STRUTHGT/2.0+FTRG_STRUTTHK  159
C    /2.0 Z=0.0 <===                                                      159
         CALL AGSRESET                                                    159
         AG_X = 0.0                                                       159
         AG_Y = -FTRG_STRUTHGT/2.0+FTRG_STRUTTHK/2.0                      159
         AG_Z = 0.0                                                       159
         AG_TITLE = 'FSTL'                                                159
         AG_EXNAME = 'POSITION'                                           159
         AG_PARLIST = 'X_Y_Z'                                             159
         CALL AXPOSITION                                                  159
C    *                                                                    159
C    Geant POSITION : FSTL ===> X=0.0 Y=+FTRG_STRUTHGT/2.0-FTRG_STRUTTHK  159
C    /2.0 Z=0.0 <===                                                      159
         CALL AGSRESET                                                    160
         AG_X = 0.0                                                       160
         AG_Y = +FTRG_STRUTHGT/2.0-FTRG_STRUTTHK/2.0                      160
         AG_Z = 0.0                                                       160
         AG_TITLE = 'FSTL'                                                160
         AG_EXNAME = 'POSITION'                                           160
         AG_PARLIST = 'X_Y_Z'                                             160
         CALL AXPOSITION                                                  160
C    *                                                                    160
C    Create FSTC :                                                        161
C    Call To Local Procedure FSTC                                         161
         ASSIGN 5290 TO BLOCK_FSTC                                        161
         GO TO  5280                                                      161
5290     CONTINUE                                                         161
C    *                                                                    161
C    Geant POSITION : FSTC ===> X=0.0 Y=0.0 Z=0.0 <===                    162
         CALL AGSRESET                                                    162
         AG_X = 0.0                                                       162
         AG_Y = 0.0                                                       162
         AG_Z = 0.0                                                       162
         AG_TITLE = 'FSTC'                                                162
         AG_EXNAME = 'POSITION'                                           162
         AG_PARLIST = 'X_Y_Z'                                             162
         CALL AXPOSITION                                                  162
C    *                                                                    162
5131  CALL AGSPOPB                                                        163
      GO TO BLOCK_FSMO, (5130 ,5140)                                      163
C End of Local Procedure                                                  163
C ----------------------------------------------------------------------  164
C ------------ FTPL                                                       164
C Begin Local Procedure FTPL                                              165
C FTPL IS THE PLANK COVERING THE STRUT                                    165
5150  CONTINUE                                                            165
         AG_TITLE='FTPL'                                                  165
         AG_MODULE=MODULE                                                 165
         AG_MARK='V00'                                                    165
         CALL AGSPUSH                                                     165
         AG_MODULE=MODULE                                                 165
C    *                                                                    165
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        166
         CALL AGSRESET                                                    166
         AG_TITLE = 'ALUMINIUM'                                           166
         AG_EXNAME = 'MATERIAL'                                           166
         AG_PARLIST = 'NONE'                                              166
         CALL AXMATERIAL                                                  166
C    *                                                                    166
C    Geant ATTRIBUTE : FTPL ===> SEEN=1 COLO=1 <===                       166
         CALL AGSRESET                                                    167
         AG_SEEN = 1                                                      167
         AG_COLO = 1                                                      167
         AG_TITLE = 'FTPL'                                                167
         AG_EXNAME = 'ATTRIBUTE'                                          167
         AG_PARLIST = 'SEEN_COLO'                                         167
         CALL AXATTRIBUTE                                                 167
C    *                                                                    167
C    Geant SHAPE : BOX ===> DX=FTRG_PLANKWTH/2.0 DY=FTRG_PLANKTHK/2.0, D  167
C    Z=FTRG_STRUTLEN/2.0 <===                                             167
         CALL AGSRESET                                                    169
         AG_DX = FTRG_PLANKWTH/2.0                                        169
         AG_DY = FTRG_PLANKTHK/2.0                                        169
         AG_DZ = FTRG_STRUTLEN/2.0                                        169
         AG_TITLE = 'BOX'                                                 169
         AG_EXNAME = 'SHAPE'                                              169
         AG_PARLIST = 'DX_DY_DZ'                                          169
         CALL AXSHAPE                                                     169
C    *                                                                    169
5151  CALL AGSPOPB                                                        170
      GO TO BLOCK_FTPL, (5150 ,5160)                                      170
C End of Local Procedure                                                  170
C ----------------------------------------------------------------------  171
C ------------ FSTL                                                       171
C Begin Local Procedure FSTL                                              172
C FSTL IS THE FLAT PART OF THE STRUT                                      172
5260  CONTINUE                                                            172
         AG_TITLE='FSTL'                                                  172
         AG_MODULE=MODULE                                                 172
         AG_MARK='V00'                                                    172
         CALL AGSPUSH                                                     172
         AG_MODULE=MODULE                                                 172
C    *                                                                    172
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        173
         CALL AGSRESET                                                    173
         AG_TITLE = 'ALUMINIUM'                                           173
         AG_EXNAME = 'MATERIAL'                                           173
         AG_PARLIST = 'NONE'                                              173
         CALL AXMATERIAL                                                  173
C    *                                                                    173
C    Geant ATTRIBUTE : FSTL ===> SEEN=1 COLO=2 <===                       173
         CALL AGSRESET                                                    174
         AG_SEEN = 1                                                      174
         AG_COLO = 2                                                      174
         AG_TITLE = 'FSTL'                                                174
         AG_EXNAME = 'ATTRIBUTE'                                          174
         AG_PARLIST = 'SEEN_COLO'                                         174
         CALL AXATTRIBUTE                                                 174
C    *                                                                    174
C    Geant SHAPE : BOX ===> DX=FTRG_STRUTWTH/2.0 DY=FTRG_STRUTTHK/2.0, D  174
C    Z=FTRG_STRUTLEN/2.0 <===                                             174
         CALL AGSRESET                                                    176
         AG_DX = FTRG_STRUTWTH/2.0                                        176
         AG_DY = FTRG_STRUTTHK/2.0                                        176
         AG_DZ = FTRG_STRUTLEN/2.0                                        176
         AG_TITLE = 'BOX'                                                 176
         AG_EXNAME = 'SHAPE'                                              176
         AG_PARLIST = 'DX_DY_DZ'                                          176
         CALL AXSHAPE                                                     176
C    *                                                                    176
5261  CALL AGSPOPB                                                        177
      GO TO BLOCK_FSTL, (5260 ,5270)                                      177
C End of Local Procedure                                                  177
C ----------------------------------------------------------------------  178
C ------------ FSTC                                                       178
C Begin Local Procedure FSTC                                              179
C FSTC IS THE CENTRAL BEAM OF THE STRUT                                   179
5280  CONTINUE                                                            179
         AG_TITLE='FSTC'                                                  179
         AG_MODULE=MODULE                                                 179
         AG_MARK='V00'                                                    179
         CALL AGSPUSH                                                     179
         AG_MODULE=MODULE                                                 179
C    *                                                                    179
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        180
         CALL AGSRESET                                                    180
         AG_TITLE = 'ALUMINIUM'                                           180
         AG_EXNAME = 'MATERIAL'                                           180
         AG_PARLIST = 'NONE'                                              180
         CALL AXMATERIAL                                                  180
C    *                                                                    180
C    Geant ATTRIBUTE : FSTC ===> SEEN=1 COLO=2 <===                       180
         CALL AGSRESET                                                    181
         AG_SEEN = 1                                                      181
         AG_COLO = 2                                                      181
         AG_TITLE = 'FSTC'                                                181
         AG_EXNAME = 'ATTRIBUTE'                                          181
         AG_PARLIST = 'SEEN_COLO'                                         181
         CALL AXATTRIBUTE                                                 181
C    *                                                                    181
C    Geant SHAPE : BOX ===> DX=FTRG_STRUTTHK/2.0 DY=FTRG_STRUTHGT/2.0-FT  181
C    RG_STRUTTHK, DZ=FTRG_STRUTLEN/2.0 <===                               181
         CALL AGSRESET                                                    183
         AG_DX = FTRG_STRUTTHK/2.0                                        183
         AG_DY = FTRG_STRUTHGT/2.0-FTRG_STRUTTHK                          183
         AG_DZ = FTRG_STRUTLEN/2.0                                        183
         AG_TITLE = 'BOX'                                                 183
         AG_EXNAME = 'SHAPE'                                              183
         AG_PARLIST = 'DX_DY_DZ'                                          183
         CALL AXSHAPE                                                     183
C    *                                                                    183
C    Create FSHL :                                                        184
C    Call To Local Procedure FSHL                                         184
         ASSIGN 5310 TO BLOCK_FSHL                                        184
         GO TO  5300                                                      184
5310     CONTINUE                                                         184
C    *                                                                    184
C    *                                                                    185
         NHOLE=1                                                          185
            GO TO 5323                                                    185
5321        NHOLE=NHOLE+1                                                 185
5323        IF(NHOLE-(FTRG_STRUTNHOLES).GT.0) GO TO 5322                  185
C       Geant POSITION : FSHL ===> X=0.0 Y=0.0 Z=-FTRG_STRUTLEN/2.0+NHOL  186
C       E*STRUTHOLESPACING ALPHAY=90 <===                                 186
            CALL AGSRESET                                                 186
            AG_X = 0.0                                                    186
            AG_Y = 0.0                                                    186
            AG_Z = -FTRG_STRUTLEN/2.0+NHOLE*STRUTHOLESPACING              186
            AG_ALPHAN=AG_ALPHAN+1                                         186
            AG_ALPHAK(AG_ALPHAN)=2                                        186
            AG_ALPHAL(AG_ALPHAN)=90                                       186
            AG_TITLE = 'FSHL'                                             186
            AG_EXNAME = 'POSITION'                                        186
            AG_PARLIST = 'X_Y_Z_ALPHAY'                                   186
            CALL AXPOSITION                                               186
C       *                                                                 186
         GO TO 5321                                                       187
5322     CONTINUE                                                         187
5281  CALL AGSPOPB                                                        188
      GO TO BLOCK_FSTC, (5280 ,5290)                                      188
C End of Local Procedure                                                  188
C ----------------------------------------------------------------------  189
C ------------ FSHL                                                       189
C Begin Local Procedure FSHL                                              190
C FSHL IS A HOLE THE BEAM OF THE STRUT                                    190
5300  CONTINUE                                                            190
         AG_TITLE='FSHL'                                                  190
         AG_MODULE=MODULE                                                 190
         AG_MARK='V00'                                                    190
         CALL AGSPUSH                                                     190
         AG_MODULE=MODULE                                                 190
C    *                                                                    190
C    Geant MATERIAL : AIR ===> inherited arguments used <===              191
         CALL AGSRESET                                                    191
         AG_TITLE = 'AIR'                                                 191
         AG_EXNAME = 'MATERIAL'                                           191
         AG_PARLIST = 'NONE'                                              191
         CALL AXMATERIAL                                                  191
C    *                                                                    191
C    Geant ATTRIBUTE : FSHL ===> SEEN=1 COLO=2 <===                       191
         CALL AGSRESET                                                    192
         AG_SEEN = 1                                                      192
         AG_COLO = 2                                                      192
         AG_TITLE = 'FSHL'                                                192
         AG_EXNAME = 'ATTRIBUTE'                                          192
         AG_PARLIST = 'SEEN_COLO'                                         192
         CALL AXATTRIBUTE                                                 192
C    *                                                                    192
C    Geant SHAPE : TUBE ===> RMIN=0.0 RMAX=FTRG_STRUTHOLER, DZ=FTRG_STRU  192
C    TTHK/2.0 <===                                                        192
         CALL AGSRESET                                                    194
         AG_RMIN = 0.0                                                    194
         AG_RMAX = FTRG_STRUTHOLER                                        194
         AG_DZ = FTRG_STRUTTHK/2.0                                        194
         AG_TITLE = 'TUBE'                                                194
         AG_EXNAME = 'SHAPE'                                              194
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      194
         CALL AXSHAPE                                                     194
C    *                                                                    194
5301  CALL AGSPOPB                                                        195
      GO TO BLOCK_FSHL, (5300 ,5310)                                      195
C End of Local Procedure                                                  195
C ----------------------------------------------------------------------  196
C ------------ FTPO                                                       196
C Begin Local Procedure FTPO                                              197
C FTPO IS THE OUTER PCB                                                   197
5170  CONTINUE                                                            197
         AG_TITLE='FTPO'                                                  197
         AG_MODULE=MODULE                                                 197
         AG_MARK='V00'                                                    197
         CALL AGSPUSH                                                     197
         AG_MODULE=MODULE                                                 197
C    *                                                                    197
C    Geant MATERIAL : G10 ===> inherited arguments used <===              198
         CALL AGSRESET                                                    198
         AG_TITLE = 'G10'                                                 198
         AG_EXNAME = 'MATERIAL'                                           198
         AG_PARLIST = 'NONE'                                              198
         CALL AXMATERIAL                                                  198
C    *                                                                    198
C    Geant ATTRIBUTE : FTPO ===> SEEN=1 COLO=3 <===                       198
         CALL AGSRESET                                                    199
         AG_SEEN = 1                                                      199
         AG_COLO = 3                                                      199
         AG_TITLE = 'FTPO'                                                199
         AG_EXNAME = 'ATTRIBUTE'                                          199
         AG_PARLIST = 'SEEN_COLO'                                         199
         CALL AXATTRIBUTE                                                 199
C    *                                                                    199
C    Geant SHAPE : BOX ===> DX=FTRG_FTPOWTH/2.0 DY=FTRG_FTPOTHK/2.0, DZ=  199
C    FTRG_STRUTLEN/2.0 <===                                               199
         CALL AGSRESET                                                    201
         AG_DX = FTRG_FTPOWTH/2.0                                         201
         AG_DY = FTRG_FTPOTHK/2.0                                         201
         AG_DZ = FTRG_STRUTLEN/2.0                                        201
         AG_TITLE = 'BOX'                                                 201
         AG_EXNAME = 'SHAPE'                                              201
         AG_PARLIST = 'DX_DY_DZ'                                          201
         CALL AXSHAPE                                                     201
C    *                                                                    201
5171  CALL AGSPOPB                                                        202
      GO TO BLOCK_FTPO, (5170 ,5180)                                      202
C End of Local Procedure                                                  202
C ----------------------------------------------------------------------  203
C ------------ FTPI                                                       203
C Begin Local Procedure FTPI                                              204
C FTPI IS THE INNER PCB                                                   204
5190  CONTINUE                                                            204
         AG_TITLE='FTPI'                                                  204
         AG_MODULE=MODULE                                                 204
         AG_MARK='V00'                                                    204
         CALL AGSPUSH                                                     204
         AG_MODULE=MODULE                                                 204
C    *                                                                    204
C    Geant MATERIAL : G10 ===> inherited arguments used <===              205
         CALL AGSRESET                                                    205
         AG_TITLE = 'G10'                                                 205
         AG_EXNAME = 'MATERIAL'                                           205
         AG_PARLIST = 'NONE'                                              205
         CALL AXMATERIAL                                                  205
C    *                                                                    205
C    Geant ATTRIBUTE : FTPI ===> SEEN=1 COLO=3 <===                       205
         CALL AGSRESET                                                    206
         AG_SEEN = 1                                                      206
         AG_COLO = 3                                                      206
         AG_TITLE = 'FTPI'                                                206
         AG_EXNAME = 'ATTRIBUTE'                                          206
         AG_PARLIST = 'SEEN_COLO'                                         206
         CALL AXATTRIBUTE                                                 206
C    *                                                                    206
C    Geant SHAPE : BOX ===> DX=FTRG_FTPIWTH/2.0 DY=FTRG_FTPITHK/2.0, DZ=  206
C    FTRG_STRUTLEN/2.0 <===                                               206
         CALL AGSRESET                                                    208
         AG_DX = FTRG_FTPIWTH/2.0                                         208
         AG_DY = FTRG_FTPITHK/2.0                                         208
         AG_DZ = FTRG_STRUTLEN/2.0                                        208
         AG_TITLE = 'BOX'                                                 208
         AG_EXNAME = 'SHAPE'                                              208
         AG_PARLIST = 'DX_DY_DZ'                                          208
         CALL AXSHAPE                                                     208
C    *                                                                    208
5191  CALL AGSPOPB                                                        209
      GO TO BLOCK_FTPI, (5190 ,5200)                                      209
C End of Local Procedure                                                  209
C ----------------------------------------------------------------------  210
C ------------ FTSH                                                       210
C Begin Local Procedure FTSH                                              211
C FTSH IS THE PROTECTIVE SHELL                                            211
5210  CONTINUE                                                            211
         AG_TITLE='FTSH'                                                  211
         AG_MODULE=MODULE                                                 211
         AG_MARK='V00'                                                    211
         CALL AGSPUSH                                                     211
         AG_MODULE=MODULE                                                 211
C    *                                                                    211
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        212
         CALL AGSRESET                                                    212
         AG_TITLE = 'ALUMINIUM'                                           212
         AG_EXNAME = 'MATERIAL'                                           212
         AG_PARLIST = 'NONE'                                              212
         CALL AXMATERIAL                                                  212
C    *                                                                    212
C    Geant ATTRIBUTE : FTSH ===> SEEN=1 COLO=2 <===                       212
         CALL AGSRESET                                                    213
         AG_SEEN = 1                                                      213
         AG_COLO = 2                                                      213
         AG_TITLE = 'FTSH'                                                213
         AG_EXNAME = 'ATTRIBUTE'                                          213
         AG_PARLIST = 'SEEN_COLO'                                         213
         CALL AXATTRIBUTE                                                 213
C    *                                                                    213
C    Geant SHAPE : TUBS ===> RMIN=FTRG_OFROUT-FTRG_SHELLTHK, RMAX=FTRG_O  213
C    FROUT PHI1=-STRUTANGLE/2.0 PHI2=STRUTANGLE/2.0, DZ=FTRG_STRUTLEN/2.  213
C    0 <===                                                               213
         CALL AGSRESET                                                    216
         AG_RMIN = FTRG_OFROUT-FTRG_SHELLTHK                              216
         AG_RMAX = FTRG_OFROUT                                            216
         AG_PHI1 = -STRUTANGLE/2.0                                        216
         AG_PHI2 = STRUTANGLE/2.0                                         216
         AG_DZ = FTRG_STRUTLEN/2.0                                        216
         AG_TITLE = 'TUBS'                                                216
         AG_EXNAME = 'SHAPE'                                              216
         AG_PARLIST = 'RMIN_RMAX_PHI1_PHI2_DZ'                            216
         CALL AXSHAPE                                                     216
C    *                                                                    216
5211  CALL AGSPOPB                                                        217
      GO TO BLOCK_FTSH, (5210 ,5220)                                      217
C End of Local Procedure                                                  217
      WRITE(LOUT,5330)'===>>  [End AgML Module FTROGEO]  <<==='           217
5330  FORMAT(A32,/,/)                                                     217
C MODULE FTROGEO                                                          219
      END                                                                 219
