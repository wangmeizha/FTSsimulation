C: definitions from /afs/rhic.bnl.gov/star/packages/DEV/.sl64_gcc482/bin/../../.sl64_gcc482/obj/asps/Simulation/agetof/../../../../../asps/Simulation/agetof/agetof.def
      SUBROUTINE PXSTGEO1                                                 1
C PXSTGEO1 GEOMETRY FOR PIXEL SUPPORT TUBE ACTUALLY ALL OF MSC            1
      IMPLICIT NONE                                                       1
      CHARACTER MODULE*20,MCOMMENT*80,BANK_TITLE*60                       1
      DATA MODULE/'PXSTGEO1'/, BANK_TITLE/'*'/                            1
      DATA MCOMMENT/'GEOMETRY FOR PIXEL SUPPORT TUBE ACTUALLY ALL OF MSC  1
     *'/                                                                  1
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
C CREATED FEV, 28TH, 2012                                                 2
      CHARACTER*40 CREATED /'FEV, 28TH, 2012'/                            3
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
      INTEGER BLOCK_PSTM                                                  4
      INTEGER BLOCK_APTS                                                  34
      INTEGER BLOCK_LFBA                                                  34
      INTEGER BLOCK_LFBB                                                  34
      INTEGER BLOCK_LFBK                                                  34
      INTEGER BLOCK_RFBA                                                  34
      INTEGER BLOCK_RFBB                                                  34
      INTEGER BLOCK_RFKK                                                  34
      INTEGER BLOCK_BPPC                                                  34
      INTEGER BLOCK_MTPA                                                  34
      INTEGER BLOCK_MTPB                                                  34
      INTEGER BLOCK_MTPC                                                  34
      INTEGER BLOCK_MTPD                                                  34
      INTEGER BLOCK_MTPE                                                  34
      INTEGER BLOCK_MTPF                                                  34
      INTEGER BLOCK_MTPG                                                  34
      INTEGER BLOCK_MTPH                                                  34
      INTEGER BLOCK_BPRS                                                  34
      INTEGER BLOCK_MOSC                                                  34
      INTEGER BLOCK_PITN                                                  34
      INTEGER BLOCK_APFA                                                  34
      INTEGER BLOCK_APFB                                                  34
      INTEGER BLOCK_APFC                                                  34
      INTEGER BLOCK_APFD                                                  34
      INTEGER BLOCK_APFE                                                  34
      INTEGER BLOCK_APFF                                                  34
      INTEGER BLOCK_APFG                                                  34
      INTEGER BLOCK_APFH                                                  34
      INTEGER BLOCK_APFI                                                  34
      INTEGER BLOCK_PITR                                                  34
      REAL *4 MAXRADPSTM/45.5/                                            35
      REAL *4 MINRADPSTM/2.27/                                            37
      REAL *4 MAXZPSTM/500/                                               39
      REAL *4 WIDTHRINGTP/.6/                                             41
      REAL *4 WIDTHINNERRINGTP/2.6/                                       43
      REAL *4 LENGTHINSIDERING/0.80/                                      45
      REAL *4 WIDTHLOWERRINGTP/3.0/                                       47
      REAL *4 TP/0.95/                                                    49
      REAL *4 TR/1.7873/                                                  51
      REAL *4 RADOUTERMAXTP/45.34/                                        53
      REAL *4 RADOUTERMINTP/42.15/                                        55
      REAL *4 RADINTERATP/43.41/                                          57
      REAL *4 RADINNERMAXTP/37.84/                                        59
      REAL *4 RADINNERMINTP/35.74/                                        61
      REAL *4 TEMPO/.2694/                                                63
      REAL *4 RADOUTERFBACKER/25.9/                                       65
      REAL *4 RADOUTERFBASE/23.9/                                         67
      REAL *4 RADOUTERSHELL/23.68/                                        69
      REAL *4 RADINNERSHELL/23.5/                                         71
      REAL *4 LENGTHFBACKER/.6/                                           73
      REAL *4 LENGTHFBACKERRING/.3/                                       75
      REAL *4 LENGTHFBASE/2.5/                                            77
      REAL *4 SMALLLENGTHFBASE/0.1/                                       79
      REAL *4 LENGTHSHELL/110.0/                                          81
      REAL *4 WIDTHSHELL/.1/                                              83
      REAL *4 GRD/23.52/                                                  85
      REAL *4 RADMAXBFC/7.6/                                              87
      REAL *4 RADMINBFC/6.44/                                             89
      REAL *4 RADINNERBFC/4.541/                                          91
      REAL *4 LENGTHBFC/4.8/                                              93
      REAL *4 WIDTHBFC/0.1/                                               95
      REAL *4 RINGDIAMETERMAX/5.6/                                        97
      REAL *4 RINGDIAMETERMIN/4.54/                                       99
      REAL *4 RINGTHK/.8/                                                 101
      REAL *4 RINGDIST/3.4/                                               103
      REAL *4 RINGEDGE/.3/                                                105
      REAL *4 PITRINNER/43.02/                                            107
      REAL *4 PITROUTER/43.2/                                             109
      REAL *4 PITLENGTH/215.8/                                            111
      REAL *4 PITFR3/43.44/                                               113
      REAL *4 PITFR4/45.34/                                               115
      REAL *4 PITLENGTHA/2.5/                                             117
      REAL *4 PITLENGTHC/.6/                                              119
      REAL *4 PITLENGTHB/.1/                                              121
      REAL *4 PITFACTOR/.760426/                                          123
      REAL *4 RADRAIL/18.338/                                             125
      REAL *4 TEMPOX, TEMPOY , ANGLE                                      127
C -                                                                       129
      REAL PSTS_VERSION                                                   129
      EQUIVALENCE (BANK_PSTS(3),PSTS_VERSION,RE_PSTS_VERSION),            129
     *(NAMES_PSTS(1),NPSTS_VERSION)                                       129
      EQUIVALENCE (COMMEN_PSTS(1),CPSTS_VERSION),(MAP_PSTS(1,1),          129
     *MPSTS_VERSION)                                                      129
      CHARACTER CPSTS_VERSION*40/' '/                                     129
      CHARACTER NPSTS_VERSION*12/'R.VERSION'/                             129
      INTEGER MPSTS_VERSION(2)/1,1/                                       129
      REAL RE_PSTS_VERSION                                                129
C -                                                                       129
      REAL PSTS_RMIN                                                      129
      EQUIVALENCE (BANK_PSTS(4),PSTS_RMIN,RE_PSTS_RMIN),(NAMES_PSTS(2),   129
     *NPSTS_RMIN)                                                         129
      EQUIVALENCE (COMMEN_PSTS(2),CPSTS_RMIN),(MAP_PSTS(1,2),             129
     *MPSTS_RMIN)                                                         129
      CHARACTER CPSTS_RMIN*40/' '/                                        129
      CHARACTER NPSTS_RMIN*12/'R.RMIN'/                                   129
      INTEGER MPSTS_RMIN(2)/1,1/                                          129
      REAL RE_PSTS_RMIN                                                   129
C -                                                                       129
      REAL PSTS_RMAX                                                      129
      EQUIVALENCE (BANK_PSTS(5),PSTS_RMAX,RE_PSTS_RMAX),(NAMES_PSTS(3),   129
     *NPSTS_RMAX)                                                         129
      EQUIVALENCE (COMMEN_PSTS(3),CPSTS_RMAX),(MAP_PSTS(1,3),             129
     *MPSTS_RMAX)                                                         129
      CHARACTER CPSTS_RMAX*40/' '/                                        129
      CHARACTER NPSTS_RMAX*12/'R.RMAX'/                                   129
      INTEGER MPSTS_RMAX(2)/1,1/                                          129
      REAL RE_PSTS_RMAX                                                   129
C -                                                                       129
      REAL PSTS_DZ                                                        129
      EQUIVALENCE (BANK_PSTS(6),PSTS_DZ,RE_PSTS_DZ),(NAMES_PSTS(4),       129
     *NPSTS_DZ)                                                           129
      EQUIVALENCE (COMMEN_PSTS(4),CPSTS_DZ),(MAP_PSTS(1,4),MPSTS_DZ)      129
      CHARACTER CPSTS_DZ*40/' '/                                          129
      CHARACTER NPSTS_DZ*12/'R.DZ'/                                       129
      INTEGER MPSTS_DZ(2)/1,1/                                            129
      REAL RE_PSTS_DZ                                                     129
C -                                                                       129
      INTEGER LEN_PSTS,LLEN_PSTS,LINK_PSTS                                129
      PARAMETER (LEN_PSTS=6)                                              129
      PARAMETER (LLEN_PSTS=4)                                             129
      INTEGER FLAG_PSTS/0/,MAP_PSTS(2,LLEN_PSTS),IDNUM_PSTS(10)           129
      CHARACTER NAMES_PSTS(LLEN_PSTS)*12,COMMEN_PSTS(LLEN_PSTS)*40        129
      DATA IDNUM_PSTS/10*0/                                               129
      REAL BANK_PSTS(LEN_PSTS)                                            129
      CHARACTER*60 BPATH_PSTS/' '/                                        129
      SAVE FLAG_PSTS,MAP_PSTS,IDNUM_PSTS,NAMES_PSTS,COMMEN_PSTS,          129
     *BPATH_PSTS                                                          129
      DATA LINK_PSTS/0/,BANK_PSTS/LEN_PSTS*0/                             129
C *                                                                       129
C -                                                                       130
      REAL SFBA_VERSION                                                   130
      EQUIVALENCE (BANK_SFBA(3),SFBA_VERSION,RE_SFBA_VERSION),            130
     *(NAMES_SFBA(1),NSFBA_VERSION)                                       130
      EQUIVALENCE (COMMEN_SFBA(1),CSFBA_VERSION),(MAP_SFBA(1,1),          130
     *MSFBA_VERSION)                                                      130
      CHARACTER CSFBA_VERSION*40/' '/                                     130
      CHARACTER NSFBA_VERSION*12/'R.VERSION'/                             130
      INTEGER MSFBA_VERSION(2)/1,1/                                       130
      REAL RE_SFBA_VERSION                                                130
C -                                                                       130
      REAL SFBA_RMIN                                                      130
      EQUIVALENCE (BANK_SFBA(4),SFBA_RMIN,RE_SFBA_RMIN),(NAMES_SFBA(2),   130
     *NSFBA_RMIN)                                                         130
      EQUIVALENCE (COMMEN_SFBA(2),CSFBA_RMIN),(MAP_SFBA(1,2),             130
     *MSFBA_RMIN)                                                         130
      CHARACTER CSFBA_RMIN*40/' '/                                        130
      CHARACTER NSFBA_RMIN*12/'R.RMIN'/                                   130
      INTEGER MSFBA_RMIN(2)/1,1/                                          130
      REAL RE_SFBA_RMIN                                                   130
C -                                                                       130
      REAL SFBA_RMAX                                                      130
      EQUIVALENCE (BANK_SFBA(5),SFBA_RMAX,RE_SFBA_RMAX),(NAMES_SFBA(3),   130
     *NSFBA_RMAX)                                                         130
      EQUIVALENCE (COMMEN_SFBA(3),CSFBA_RMAX),(MAP_SFBA(1,3),             130
     *MSFBA_RMAX)                                                         130
      CHARACTER CSFBA_RMAX*40/' '/                                        130
      CHARACTER NSFBA_RMAX*12/'R.RMAX'/                                   130
      INTEGER MSFBA_RMAX(2)/1,1/                                          130
      REAL RE_SFBA_RMAX                                                   130
C -                                                                       130
      REAL SFBA_DZ                                                        130
      EQUIVALENCE (BANK_SFBA(6),SFBA_DZ,RE_SFBA_DZ),(NAMES_SFBA(4),       130
     *NSFBA_DZ)                                                           130
      EQUIVALENCE (COMMEN_SFBA(4),CSFBA_DZ),(MAP_SFBA(1,4),MSFBA_DZ)      130
      CHARACTER CSFBA_DZ*40/' '/                                          130
      CHARACTER NSFBA_DZ*12/'R.DZ'/                                       130
      INTEGER MSFBA_DZ(2)/1,1/                                            130
      REAL RE_SFBA_DZ                                                     130
C -                                                                       130
      INTEGER LEN_SFBA,LLEN_SFBA,LINK_SFBA                                130
      PARAMETER (LEN_SFBA=6)                                              130
      PARAMETER (LLEN_SFBA=4)                                             130
      INTEGER FLAG_SFBA/0/,MAP_SFBA(2,LLEN_SFBA),IDNUM_SFBA(10)           130
      CHARACTER NAMES_SFBA(LLEN_SFBA)*12,COMMEN_SFBA(LLEN_SFBA)*40        130
      DATA IDNUM_SFBA/10*0/                                               130
      REAL BANK_SFBA(LEN_SFBA)                                            130
      CHARACTER*60 BPATH_SFBA/' '/                                        130
      SAVE FLAG_SFBA,MAP_SFBA,IDNUM_SFBA,NAMES_SFBA,COMMEN_SFBA,          130
     *BPATH_SFBA                                                          130
      DATA LINK_SFBA/0/,BANK_SFBA/LEN_SFBA*0/                             130
C *                                                                       130
C -                                                                       131
      REAL SFBB_VERSION                                                   131
      EQUIVALENCE (BANK_SFBB(3),SFBB_VERSION,RE_SFBB_VERSION),            131
     *(NAMES_SFBB(1),NSFBB_VERSION)                                       131
      EQUIVALENCE (COMMEN_SFBB(1),CSFBB_VERSION),(MAP_SFBB(1,1),          131
     *MSFBB_VERSION)                                                      131
      CHARACTER CSFBB_VERSION*40/' '/                                     131
      CHARACTER NSFBB_VERSION*12/'R.VERSION'/                             131
      INTEGER MSFBB_VERSION(2)/1,1/                                       131
      REAL RE_SFBB_VERSION                                                131
C -                                                                       131
      REAL SFBB_RMIN                                                      131
      EQUIVALENCE (BANK_SFBB(4),SFBB_RMIN,RE_SFBB_RMIN),(NAMES_SFBB(2),   131
     *NSFBB_RMIN)                                                         131
      EQUIVALENCE (COMMEN_SFBB(2),CSFBB_RMIN),(MAP_SFBB(1,2),             131
     *MSFBB_RMIN)                                                         131
      CHARACTER CSFBB_RMIN*40/' '/                                        131
      CHARACTER NSFBB_RMIN*12/'R.RMIN'/                                   131
      INTEGER MSFBB_RMIN(2)/1,1/                                          131
      REAL RE_SFBB_RMIN                                                   131
C -                                                                       131
      REAL SFBB_RMAX                                                      131
      EQUIVALENCE (BANK_SFBB(5),SFBB_RMAX,RE_SFBB_RMAX),(NAMES_SFBB(3),   131
     *NSFBB_RMAX)                                                         131
      EQUIVALENCE (COMMEN_SFBB(3),CSFBB_RMAX),(MAP_SFBB(1,3),             131
     *MSFBB_RMAX)                                                         131
      CHARACTER CSFBB_RMAX*40/' '/                                        131
      CHARACTER NSFBB_RMAX*12/'R.RMAX'/                                   131
      INTEGER MSFBB_RMAX(2)/1,1/                                          131
      REAL RE_SFBB_RMAX                                                   131
C -                                                                       131
      REAL SFBB_DZ                                                        131
      EQUIVALENCE (BANK_SFBB(6),SFBB_DZ,RE_SFBB_DZ),(NAMES_SFBB(4),       131
     *NSFBB_DZ)                                                           131
      EQUIVALENCE (COMMEN_SFBB(4),CSFBB_DZ),(MAP_SFBB(1,4),MSFBB_DZ)      131
      CHARACTER CSFBB_DZ*40/' '/                                          131
      CHARACTER NSFBB_DZ*12/'R.DZ'/                                       131
      INTEGER MSFBB_DZ(2)/1,1/                                            131
      REAL RE_SFBB_DZ                                                     131
C -                                                                       131
      INTEGER LEN_SFBB,LLEN_SFBB,LINK_SFBB                                131
      PARAMETER (LEN_SFBB=6)                                              131
      PARAMETER (LLEN_SFBB=4)                                             131
      INTEGER FLAG_SFBB/0/,MAP_SFBB(2,LLEN_SFBB),IDNUM_SFBB(10)           131
      CHARACTER NAMES_SFBB(LLEN_SFBB)*12,COMMEN_SFBB(LLEN_SFBB)*40        131
      DATA IDNUM_SFBB/10*0/                                               131
      REAL BANK_SFBB(LEN_SFBB)                                            131
      CHARACTER*60 BPATH_SFBB/' '/                                        131
      SAVE FLAG_SFBB,MAP_SFBB,IDNUM_SFBB,NAMES_SFBB,COMMEN_SFBB,          131
     *BPATH_SFBB                                                          131
      DATA LINK_SFBB/0/,BANK_SFBB/LEN_SFBB*0/                             131
C *                                                                       131
C -                                                                       132
      REAL SFBK_VERSION                                                   132
      EQUIVALENCE (BANK_SFBK(3),SFBK_VERSION,RE_SFBK_VERSION),            132
     *(NAMES_SFBK(1),NSFBK_VERSION)                                       132
      EQUIVALENCE (COMMEN_SFBK(1),CSFBK_VERSION),(MAP_SFBK(1,1),          132
     *MSFBK_VERSION)                                                      132
      CHARACTER CSFBK_VERSION*40/' '/                                     132
      CHARACTER NSFBK_VERSION*12/'R.VERSION'/                             132
      INTEGER MSFBK_VERSION(2)/1,1/                                       132
      REAL RE_SFBK_VERSION                                                132
C -                                                                       132
      REAL SFBK_RMIN                                                      132
      EQUIVALENCE (BANK_SFBK(4),SFBK_RMIN,RE_SFBK_RMIN),(NAMES_SFBK(2),   132
     *NSFBK_RMIN)                                                         132
      EQUIVALENCE (COMMEN_SFBK(2),CSFBK_RMIN),(MAP_SFBK(1,2),             132
     *MSFBK_RMIN)                                                         132
      CHARACTER CSFBK_RMIN*40/' '/                                        132
      CHARACTER NSFBK_RMIN*12/'R.RMIN'/                                   132
      INTEGER MSFBK_RMIN(2)/1,1/                                          132
      REAL RE_SFBK_RMIN                                                   132
C -                                                                       132
      REAL SFBK_RMAX                                                      132
      EQUIVALENCE (BANK_SFBK(5),SFBK_RMAX,RE_SFBK_RMAX),(NAMES_SFBK(3),   132
     *NSFBK_RMAX)                                                         132
      EQUIVALENCE (COMMEN_SFBK(3),CSFBK_RMAX),(MAP_SFBK(1,3),             132
     *MSFBK_RMAX)                                                         132
      CHARACTER CSFBK_RMAX*40/' '/                                        132
      CHARACTER NSFBK_RMAX*12/'R.RMAX'/                                   132
      INTEGER MSFBK_RMAX(2)/1,1/                                          132
      REAL RE_SFBK_RMAX                                                   132
C -                                                                       132
      REAL SFBK_DZ                                                        132
      EQUIVALENCE (BANK_SFBK(6),SFBK_DZ,RE_SFBK_DZ),(NAMES_SFBK(4),       132
     *NSFBK_DZ)                                                           132
      EQUIVALENCE (COMMEN_SFBK(4),CSFBK_DZ),(MAP_SFBK(1,4),MSFBK_DZ)      132
      CHARACTER CSFBK_DZ*40/' '/                                          132
      CHARACTER NSFBK_DZ*12/'R.DZ'/                                       132
      INTEGER MSFBK_DZ(2)/1,1/                                            132
      REAL RE_SFBK_DZ                                                     132
C -                                                                       132
      INTEGER LEN_SFBK,LLEN_SFBK,LINK_SFBK                                132
      PARAMETER (LEN_SFBK=6)                                              132
      PARAMETER (LLEN_SFBK=4)                                             132
      INTEGER FLAG_SFBK/0/,MAP_SFBK(2,LLEN_SFBK),IDNUM_SFBK(10)           132
      CHARACTER NAMES_SFBK(LLEN_SFBK)*12,COMMEN_SFBK(LLEN_SFBK)*40        132
      DATA IDNUM_SFBK/10*0/                                               132
      REAL BANK_SFBK(LEN_SFBK)                                            132
      CHARACTER*60 BPATH_SFBK/' '/                                        132
      SAVE FLAG_SFBK,MAP_SFBK,IDNUM_SFBK,NAMES_SFBK,COMMEN_SFBK,          132
     *BPATH_SFBK                                                          132
      DATA LINK_SFBK/0/,BANK_SFBK/LEN_SFBK*0/                             132
C *                                                                       132
C -                                                                       133
      REAL BPPC_VERSION                                                   133
      EQUIVALENCE (BANK_BPPC(3),BPPC_VERSION,RE_BPPC_VERSION),            133
     *(NAMES_BPPC(1),NBPPC_VERSION)                                       133
      EQUIVALENCE (COMMEN_BPPC(1),CBPPC_VERSION),(MAP_BPPC(1,1),          133
     *MBPPC_VERSION)                                                      133
      CHARACTER CBPPC_VERSION*40/' '/                                     133
      CHARACTER NBPPC_VERSION*12/'R.VERSION'/                             133
      INTEGER MBPPC_VERSION(2)/1,1/                                       133
      REAL RE_BPPC_VERSION                                                133
C -                                                                       133
      REAL BPPC_RMIN                                                      133
      EQUIVALENCE (BANK_BPPC(4),BPPC_RMIN,RE_BPPC_RMIN),(NAMES_BPPC(2),   133
     *NBPPC_RMIN)                                                         133
      EQUIVALENCE (COMMEN_BPPC(2),CBPPC_RMIN),(MAP_BPPC(1,2),             133
     *MBPPC_RMIN)                                                         133
      CHARACTER CBPPC_RMIN*40/' '/                                        133
      CHARACTER NBPPC_RMIN*12/'R.RMIN'/                                   133
      INTEGER MBPPC_RMIN(2)/1,1/                                          133
      REAL RE_BPPC_RMIN                                                   133
C -                                                                       133
      REAL BPPC_RMAX                                                      133
      EQUIVALENCE (BANK_BPPC(5),BPPC_RMAX,RE_BPPC_RMAX),(NAMES_BPPC(3),   133
     *NBPPC_RMAX)                                                         133
      EQUIVALENCE (COMMEN_BPPC(3),CBPPC_RMAX),(MAP_BPPC(1,3),             133
     *MBPPC_RMAX)                                                         133
      CHARACTER CBPPC_RMAX*40/' '/                                        133
      CHARACTER NBPPC_RMAX*12/'R.RMAX'/                                   133
      INTEGER MBPPC_RMAX(2)/1,1/                                          133
      REAL RE_BPPC_RMAX                                                   133
C -                                                                       133
      REAL BPPC_DZ                                                        133
      EQUIVALENCE (BANK_BPPC(6),BPPC_DZ,RE_BPPC_DZ),(NAMES_BPPC(4),       133
     *NBPPC_DZ)                                                           133
      EQUIVALENCE (COMMEN_BPPC(4),CBPPC_DZ),(MAP_BPPC(1,4),MBPPC_DZ)      133
      CHARACTER CBPPC_DZ*40/' '/                                          133
      CHARACTER NBPPC_DZ*12/'R.DZ'/                                       133
      INTEGER MBPPC_DZ(2)/1,1/                                            133
      REAL RE_BPPC_DZ                                                     133
C -                                                                       133
      INTEGER LEN_BPPC,LLEN_BPPC,LINK_BPPC                                133
      PARAMETER (LEN_BPPC=6)                                              133
      PARAMETER (LLEN_BPPC=4)                                             133
      INTEGER FLAG_BPPC/0/,MAP_BPPC(2,LLEN_BPPC),IDNUM_BPPC(10)           133
      CHARACTER NAMES_BPPC(LLEN_BPPC)*12,COMMEN_BPPC(LLEN_BPPC)*40        133
      DATA IDNUM_BPPC/10*0/                                               133
      REAL BANK_BPPC(LEN_BPPC)                                            133
      CHARACTER*60 BPATH_BPPC/' '/                                        133
      SAVE FLAG_BPPC,MAP_BPPC,IDNUM_BPPC,NAMES_BPPC,COMMEN_BPPC,          133
     *BPATH_BPPC                                                          133
      DATA LINK_BPPC/0/,BANK_BPPC/LEN_BPPC*0/                             133
C *                                                                       133
C -                                                                       134
      REAL MOSC_VERSION                                                   134
      EQUIVALENCE (BANK_MOSC(3),MOSC_VERSION,RE_MOSC_VERSION),            134
     *(NAMES_MOSC(1),NMOSC_VERSION)                                       134
      EQUIVALENCE (COMMEN_MOSC(1),CMOSC_VERSION),(MAP_MOSC(1,1),          134
     *MMOSC_VERSION)                                                      134
      CHARACTER CMOSC_VERSION*40/' '/                                     134
      CHARACTER NMOSC_VERSION*12/'R.VERSION'/                             134
      INTEGER MMOSC_VERSION(2)/1,1/                                       134
      REAL RE_MOSC_VERSION                                                134
C -                                                                       134
      REAL MOSC_RMIN                                                      134
      EQUIVALENCE (BANK_MOSC(4),MOSC_RMIN,RE_MOSC_RMIN),(NAMES_MOSC(2),   134
     *NMOSC_RMIN)                                                         134
      EQUIVALENCE (COMMEN_MOSC(2),CMOSC_RMIN),(MAP_MOSC(1,2),             134
     *MMOSC_RMIN)                                                         134
      CHARACTER CMOSC_RMIN*40/' '/                                        134
      CHARACTER NMOSC_RMIN*12/'R.RMIN'/                                   134
      INTEGER MMOSC_RMIN(2)/1,1/                                          134
      REAL RE_MOSC_RMIN                                                   134
C -                                                                       134
      REAL MOSC_RMAX                                                      134
      EQUIVALENCE (BANK_MOSC(5),MOSC_RMAX,RE_MOSC_RMAX),(NAMES_MOSC(3),   134
     *NMOSC_RMAX)                                                         134
      EQUIVALENCE (COMMEN_MOSC(3),CMOSC_RMAX),(MAP_MOSC(1,3),             134
     *MMOSC_RMAX)                                                         134
      CHARACTER CMOSC_RMAX*40/' '/                                        134
      CHARACTER NMOSC_RMAX*12/'R.RMAX'/                                   134
      INTEGER MMOSC_RMAX(2)/1,1/                                          134
      REAL RE_MOSC_RMAX                                                   134
C -                                                                       134
      REAL MOSC_DZ                                                        134
      EQUIVALENCE (BANK_MOSC(6),MOSC_DZ,RE_MOSC_DZ),(NAMES_MOSC(4),       134
     *NMOSC_DZ)                                                           134
      EQUIVALENCE (COMMEN_MOSC(4),CMOSC_DZ),(MAP_MOSC(1,4),MMOSC_DZ)      134
      CHARACTER CMOSC_DZ*40/' '/                                          134
      CHARACTER NMOSC_DZ*12/'R.DZ'/                                       134
      INTEGER MMOSC_DZ(2)/1,1/                                            134
      REAL RE_MOSC_DZ                                                     134
C -                                                                       134
      INTEGER LEN_MOSC,LLEN_MOSC,LINK_MOSC                                134
      PARAMETER (LEN_MOSC=6)                                              134
      PARAMETER (LLEN_MOSC=4)                                             134
      INTEGER FLAG_MOSC/0/,MAP_MOSC(2,LLEN_MOSC),IDNUM_MOSC(10)           134
      CHARACTER NAMES_MOSC(LLEN_MOSC)*12,COMMEN_MOSC(LLEN_MOSC)*40        134
      DATA IDNUM_MOSC/10*0/                                               134
      REAL BANK_MOSC(LEN_MOSC)                                            134
      CHARACTER*60 BPATH_MOSC/' '/                                        134
      SAVE FLAG_MOSC,MAP_MOSC,IDNUM_MOSC,NAMES_MOSC,COMMEN_MOSC,          134
     *BPATH_MOSC                                                          134
      DATA LINK_MOSC/0/,BANK_MOSC/LEN_MOSC*0/                             134
C *                                                                       134
C -                                                                       135
      REAL PITA_VERSION                                                   135
      EQUIVALENCE (BANK_PITA(3),PITA_VERSION,RE_PITA_VERSION),            135
     *(NAMES_PITA(1),NPITA_VERSION)                                       135
      EQUIVALENCE (COMMEN_PITA(1),CPITA_VERSION),(MAP_PITA(1,1),          135
     *MPITA_VERSION)                                                      135
      CHARACTER CPITA_VERSION*40/' '/                                     135
      CHARACTER NPITA_VERSION*12/'R.VERSION'/                             135
      INTEGER MPITA_VERSION(2)/1,1/                                       135
      REAL RE_PITA_VERSION                                                135
C -                                                                       135
      REAL PITA_RMIN                                                      135
      EQUIVALENCE (BANK_PITA(4),PITA_RMIN,RE_PITA_RMIN),(NAMES_PITA(2),   135
     *NPITA_RMIN)                                                         135
      EQUIVALENCE (COMMEN_PITA(2),CPITA_RMIN),(MAP_PITA(1,2),             135
     *MPITA_RMIN)                                                         135
      CHARACTER CPITA_RMIN*40/' '/                                        135
      CHARACTER NPITA_RMIN*12/'R.RMIN'/                                   135
      INTEGER MPITA_RMIN(2)/1,1/                                          135
      REAL RE_PITA_RMIN                                                   135
C -                                                                       135
      REAL PITA_RMAX                                                      135
      EQUIVALENCE (BANK_PITA(5),PITA_RMAX,RE_PITA_RMAX),(NAMES_PITA(3),   135
     *NPITA_RMAX)                                                         135
      EQUIVALENCE (COMMEN_PITA(3),CPITA_RMAX),(MAP_PITA(1,3),             135
     *MPITA_RMAX)                                                         135
      CHARACTER CPITA_RMAX*40/' '/                                        135
      CHARACTER NPITA_RMAX*12/'R.RMAX'/                                   135
      INTEGER MPITA_RMAX(2)/1,1/                                          135
      REAL RE_PITA_RMAX                                                   135
C -                                                                       135
      REAL PITA_DZ                                                        135
      EQUIVALENCE (BANK_PITA(6),PITA_DZ,RE_PITA_DZ),(NAMES_PITA(4),       135
     *NPITA_DZ)                                                           135
      EQUIVALENCE (COMMEN_PITA(4),CPITA_DZ),(MAP_PITA(1,4),MPITA_DZ)      135
      CHARACTER CPITA_DZ*40/' '/                                          135
      CHARACTER NPITA_DZ*12/'R.DZ'/                                       135
      INTEGER MPITA_DZ(2)/1,1/                                            135
      REAL RE_PITA_DZ                                                     135
C -                                                                       135
      INTEGER LEN_PITA,LLEN_PITA,LINK_PITA                                135
      PARAMETER (LEN_PITA=6)                                              135
      PARAMETER (LLEN_PITA=4)                                             135
      INTEGER FLAG_PITA/0/,MAP_PITA(2,LLEN_PITA),IDNUM_PITA(10)           135
      CHARACTER NAMES_PITA(LLEN_PITA)*12,COMMEN_PITA(LLEN_PITA)*40        135
      DATA IDNUM_PITA/10*0/                                               135
      REAL BANK_PITA(LEN_PITA)                                            135
      CHARACTER*60 BPATH_PITA/' '/                                        135
      SAVE FLAG_PITA,MAP_PITA,IDNUM_PITA,NAMES_PITA,COMMEN_PITA,          135
     *BPATH_PITA                                                          135
      DATA LINK_PITA/0/,BANK_PITA/LEN_PITA*0/                             135
C *                                                                       135
C -                                                                       136
      REAL PITB_VERSION                                                   136
      EQUIVALENCE (BANK_PITB(3),PITB_VERSION,RE_PITB_VERSION),            136
     *(NAMES_PITB(1),NPITB_VERSION)                                       136
      EQUIVALENCE (COMMEN_PITB(1),CPITB_VERSION),(MAP_PITB(1,1),          136
     *MPITB_VERSION)                                                      136
      CHARACTER CPITB_VERSION*40/' '/                                     136
      CHARACTER NPITB_VERSION*12/'R.VERSION'/                             136
      INTEGER MPITB_VERSION(2)/1,1/                                       136
      REAL RE_PITB_VERSION                                                136
C -                                                                       136
      REAL PITB_RMIN                                                      136
      EQUIVALENCE (BANK_PITB(4),PITB_RMIN,RE_PITB_RMIN),(NAMES_PITB(2),   136
     *NPITB_RMIN)                                                         136
      EQUIVALENCE (COMMEN_PITB(2),CPITB_RMIN),(MAP_PITB(1,2),             136
     *MPITB_RMIN)                                                         136
      CHARACTER CPITB_RMIN*40/' '/                                        136
      CHARACTER NPITB_RMIN*12/'R.RMIN'/                                   136
      INTEGER MPITB_RMIN(2)/1,1/                                          136
      REAL RE_PITB_RMIN                                                   136
C -                                                                       136
      REAL PITB_RMAX                                                      136
      EQUIVALENCE (BANK_PITB(5),PITB_RMAX,RE_PITB_RMAX),(NAMES_PITB(3),   136
     *NPITB_RMAX)                                                         136
      EQUIVALENCE (COMMEN_PITB(3),CPITB_RMAX),(MAP_PITB(1,3),             136
     *MPITB_RMAX)                                                         136
      CHARACTER CPITB_RMAX*40/' '/                                        136
      CHARACTER NPITB_RMAX*12/'R.RMAX'/                                   136
      INTEGER MPITB_RMAX(2)/1,1/                                          136
      REAL RE_PITB_RMAX                                                   136
C -                                                                       136
      REAL PITB_DZ                                                        136
      EQUIVALENCE (BANK_PITB(6),PITB_DZ,RE_PITB_DZ),(NAMES_PITB(4),       136
     *NPITB_DZ)                                                           136
      EQUIVALENCE (COMMEN_PITB(4),CPITB_DZ),(MAP_PITB(1,4),MPITB_DZ)      136
      CHARACTER CPITB_DZ*40/' '/                                          136
      CHARACTER NPITB_DZ*12/'R.DZ'/                                       136
      INTEGER MPITB_DZ(2)/1,1/                                            136
      REAL RE_PITB_DZ                                                     136
C -                                                                       136
      INTEGER LEN_PITB,LLEN_PITB,LINK_PITB                                136
      PARAMETER (LEN_PITB=6)                                              136
      PARAMETER (LLEN_PITB=4)                                             136
      INTEGER FLAG_PITB/0/,MAP_PITB(2,LLEN_PITB),IDNUM_PITB(10)           136
      CHARACTER NAMES_PITB(LLEN_PITB)*12,COMMEN_PITB(LLEN_PITB)*40        136
      DATA IDNUM_PITB/10*0/                                               136
      REAL BANK_PITB(LEN_PITB)                                            136
      CHARACTER*60 BPATH_PITB/' '/                                        136
      SAVE FLAG_PITB,MAP_PITB,IDNUM_PITB,NAMES_PITB,COMMEN_PITB,          136
     *BPATH_PITB                                                          136
      DATA LINK_PITB/0/,BANK_PITB/LEN_PITB*0/                             136
C *                                                                       136
C -                                                                       137
      REAL PITC_VERSION                                                   137
      EQUIVALENCE (BANK_PITC(3),PITC_VERSION,RE_PITC_VERSION),            137
     *(NAMES_PITC(1),NPITC_VERSION)                                       137
      EQUIVALENCE (COMMEN_PITC(1),CPITC_VERSION),(MAP_PITC(1,1),          137
     *MPITC_VERSION)                                                      137
      CHARACTER CPITC_VERSION*40/' '/                                     137
      CHARACTER NPITC_VERSION*12/'R.VERSION'/                             137
      INTEGER MPITC_VERSION(2)/1,1/                                       137
      REAL RE_PITC_VERSION                                                137
C -                                                                       137
      REAL PITC_RMIN                                                      137
      EQUIVALENCE (BANK_PITC(4),PITC_RMIN,RE_PITC_RMIN),(NAMES_PITC(2),   137
     *NPITC_RMIN)                                                         137
      EQUIVALENCE (COMMEN_PITC(2),CPITC_RMIN),(MAP_PITC(1,2),             137
     *MPITC_RMIN)                                                         137
      CHARACTER CPITC_RMIN*40/' '/                                        137
      CHARACTER NPITC_RMIN*12/'R.RMIN'/                                   137
      INTEGER MPITC_RMIN(2)/1,1/                                          137
      REAL RE_PITC_RMIN                                                   137
C -                                                                       137
      REAL PITC_RMAX                                                      137
      EQUIVALENCE (BANK_PITC(5),PITC_RMAX,RE_PITC_RMAX),(NAMES_PITC(3),   137
     *NPITC_RMAX)                                                         137
      EQUIVALENCE (COMMEN_PITC(3),CPITC_RMAX),(MAP_PITC(1,3),             137
     *MPITC_RMAX)                                                         137
      CHARACTER CPITC_RMAX*40/' '/                                        137
      CHARACTER NPITC_RMAX*12/'R.RMAX'/                                   137
      INTEGER MPITC_RMAX(2)/1,1/                                          137
      REAL RE_PITC_RMAX                                                   137
C -                                                                       137
      REAL PITC_DZ                                                        137
      EQUIVALENCE (BANK_PITC(6),PITC_DZ,RE_PITC_DZ),(NAMES_PITC(4),       137
     *NPITC_DZ)                                                           137
      EQUIVALENCE (COMMEN_PITC(4),CPITC_DZ),(MAP_PITC(1,4),MPITC_DZ)      137
      CHARACTER CPITC_DZ*40/' '/                                          137
      CHARACTER NPITC_DZ*12/'R.DZ'/                                       137
      INTEGER MPITC_DZ(2)/1,1/                                            137
      REAL RE_PITC_DZ                                                     137
C -                                                                       137
      INTEGER LEN_PITC,LLEN_PITC,LINK_PITC                                137
      PARAMETER (LEN_PITC=6)                                              137
      PARAMETER (LLEN_PITC=4)                                             137
      INTEGER FLAG_PITC/0/,MAP_PITC(2,LLEN_PITC),IDNUM_PITC(10)           137
      CHARACTER NAMES_PITC(LLEN_PITC)*12,COMMEN_PITC(LLEN_PITC)*40        137
      DATA IDNUM_PITC/10*0/                                               137
      REAL BANK_PITC(LEN_PITC)                                            137
      CHARACTER*60 BPATH_PITC/' '/                                        137
      SAVE FLAG_PITC,MAP_PITC,IDNUM_PITC,NAMES_PITC,COMMEN_PITC,          137
     *BPATH_PITC                                                          137
      DATA LINK_PITC/0/,BANK_PITC/LEN_PITC*0/                             137
C *                                                                       137
C -                                                                       138
      REAL PITR_VERSION                                                   138
      EQUIVALENCE (BANK_PITR(3),PITR_VERSION,RE_PITR_VERSION),            138
     *(NAMES_PITR(1),NPITR_VERSION)                                       138
      EQUIVALENCE (COMMEN_PITR(1),CPITR_VERSION),(MAP_PITR(1,1),          138
     *MPITR_VERSION)                                                      138
      CHARACTER CPITR_VERSION*40/' '/                                     138
      CHARACTER NPITR_VERSION*12/'R.VERSION'/                             138
      INTEGER MPITR_VERSION(2)/1,1/                                       138
      REAL RE_PITR_VERSION                                                138
C -                                                                       138
      REAL PITR_DMIN                                                      138
      EQUIVALENCE (BANK_PITR(4),PITR_DMIN,RE_PITR_DMIN),(NAMES_PITR(2),   138
     *NPITR_DMIN)                                                         138
      EQUIVALENCE (COMMEN_PITR(2),CPITR_DMIN),(MAP_PITR(1,2),             138
     *MPITR_DMIN)                                                         138
      CHARACTER CPITR_DMIN*40/' '/                                        138
      CHARACTER NPITR_DMIN*12/'R.DMIN'/                                   138
      INTEGER MPITR_DMIN(2)/1,1/                                          138
      REAL RE_PITR_DMIN                                                   138
C -                                                                       138
      REAL PITR_DMAX                                                      138
      EQUIVALENCE (BANK_PITR(5),PITR_DMAX,RE_PITR_DMAX),(NAMES_PITR(3),   138
     *NPITR_DMAX)                                                         138
      EQUIVALENCE (COMMEN_PITR(3),CPITR_DMAX),(MAP_PITR(1,3),             138
     *MPITR_DMAX)                                                         138
      CHARACTER CPITR_DMAX*40/' '/                                        138
      CHARACTER NPITR_DMAX*12/'R.DMAX'/                                   138
      INTEGER MPITR_DMAX(2)/1,1/                                          138
      REAL RE_PITR_DMAX                                                   138
C -                                                                       138
      REAL PITR_DZ                                                        138
      EQUIVALENCE (BANK_PITR(6),PITR_DZ,RE_PITR_DZ),(NAMES_PITR(4),       138
     *NPITR_DZ)                                                           138
      EQUIVALENCE (COMMEN_PITR(4),CPITR_DZ),(MAP_PITR(1,4),MPITR_DZ)      138
      CHARACTER CPITR_DZ*40/' '/                                          138
      CHARACTER NPITR_DZ*12/'R.DZ'/                                       138
      INTEGER MPITR_DZ(2)/1,1/                                            138
      REAL RE_PITR_DZ                                                     138
C -                                                                       138
      INTEGER LEN_PITR,LLEN_PITR,LINK_PITR                                138
      PARAMETER (LEN_PITR=6)                                              138
      PARAMETER (LLEN_PITR=4)                                             138
      INTEGER FLAG_PITR/0/,MAP_PITR(2,LLEN_PITR),IDNUM_PITR(10)           138
      CHARACTER NAMES_PITR(LLEN_PITR)*12,COMMEN_PITR(LLEN_PITR)*40        138
      DATA IDNUM_PITR/10*0/                                               138
      REAL BANK_PITR(LEN_PITR)                                            138
      CHARACTER*60 BPATH_PITR/' '/                                        138
      SAVE FLAG_PITR,MAP_PITR,IDNUM_PITR,NAMES_PITR,COMMEN_PITR,          138
     *BPATH_PITR                                                          138
      DATA LINK_PITR/0/,BANK_PITR/LEN_PITR*0/                             138
C *                                                                       138
C *                                                                       139
      CALL AGSBEGM(MODULE,IPRIN)                                          139
      IF (IPRIN.LT.0) RETURN                                              139
C Filling bank PSTS - NONE                                                139
      BANK_TITLE='NONE'                                                   139
      BPATH_PSTS='PSTS*'                                                  139
      IDNUM_PSTS(1)=0                                                     140
C NONE                                                                    140
      PSTS_VERSION=1.0                                                    140
      CPSTS_VERSION='NONE'                                                140
C NONE                                                                    141
      PSTS_RMIN=RADINNERSHELL                                             141
      CPSTS_RMIN='NONE'                                                   141
C NONE                                                                    142
      PSTS_RMAX=RADOUTERSHELL                                             142
      CPSTS_RMAX='NONE'                                                   142
C NONE                                                                    143
      PSTS_DZ=LENGTHSHELL                                                 143
      CPSTS_DZ='NONE'                                                     143
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PSTS',    144
     *BPATH_PSTS,IDNUM_PSTS,1, MAP_PSTS,NAMES_PSTS,COMMEN_PSTS,           144
     *BANK_PSTS,LEN_PSTS,LLEN_PSTS,LINK_PSTS,FLAG_PSTS)                   144
C *                                                                       144
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PSTS',    144
     *BPATH_PSTS,IDNUM_PSTS,1, MAP_PSTS,NAMES_PSTS,COMMEN_PSTS,           144
     *BANK_PSTS,LEN_PSTS,LLEN_PSTS,LINK_PSTS,FLAG_PSTS)                   144
C *                                                                       145
      CALL AGSBEGM(MODULE,IPRIN)                                          145
      IF (IPRIN.LT.0) RETURN                                              145
C Filling bank SFBA - NONE                                                145
      BANK_TITLE='NONE'                                                   145
      BPATH_SFBA='SFBA*'                                                  145
      IDNUM_SFBA(1)=0                                                     146
C NONE                                                                    146
      SFBA_VERSION=1.0                                                    146
      CSFBA_VERSION='NONE'                                                146
C NONE                                                                    147
      SFBA_RMIN=RADOUTERSHELL                                             147
      CSFBA_RMIN='NONE'                                                   147
C NONE                                                                    148
      SFBA_RMAX=RADOUTERFBASE                                             148
      CSFBA_RMAX='NONE'                                                   148
C NONE                                                                    149
      SFBA_DZ=LENGTHFBASE                                                 149
      CSFBA_DZ='NONE'                                                     149
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFBA',    150
     *BPATH_SFBA,IDNUM_SFBA,1, MAP_SFBA,NAMES_SFBA,COMMEN_SFBA,           150
     *BANK_SFBA,LEN_SFBA,LLEN_SFBA,LINK_SFBA,FLAG_SFBA)                   150
C *                                                                       150
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFBA',    150
     *BPATH_SFBA,IDNUM_SFBA,1, MAP_SFBA,NAMES_SFBA,COMMEN_SFBA,           150
     *BANK_SFBA,LEN_SFBA,LLEN_SFBA,LINK_SFBA,FLAG_SFBA)                   150
C *                                                                       151
      CALL AGSBEGM(MODULE,IPRIN)                                          151
      IF (IPRIN.LT.0) RETURN                                              151
C Filling bank SFBB - NONE                                                151
      BANK_TITLE='NONE'                                                   151
      BPATH_SFBB='SFBB*'                                                  151
      IDNUM_SFBB(1)=0                                                     152
C NONE                                                                    152
      SFBB_VERSION=1.0                                                    152
      CSFBB_VERSION='NONE'                                                152
C NONE                                                                    153
      SFBB_RMIN=RADOUTERSHELL                                             153
      CSFBB_RMIN='NONE'                                                   153
C NONE                                                                    154
      SFBB_RMAX=RADOUTERFBACKER                                           154
      CSFBB_RMAX='NONE'                                                   154
C NONE                                                                    155
      SFBB_DZ=SMALLLENGTHFBASE                                            155
      CSFBB_DZ='NONE'                                                     155
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFBB',    156
     *BPATH_SFBB,IDNUM_SFBB,1, MAP_SFBB,NAMES_SFBB,COMMEN_SFBB,           156
     *BANK_SFBB,LEN_SFBB,LLEN_SFBB,LINK_SFBB,FLAG_SFBB)                   156
C *                                                                       156
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFBB',    156
     *BPATH_SFBB,IDNUM_SFBB,1, MAP_SFBB,NAMES_SFBB,COMMEN_SFBB,           156
     *BANK_SFBB,LEN_SFBB,LLEN_SFBB,LINK_SFBB,FLAG_SFBB)                   156
C *                                                                       157
      CALL AGSBEGM(MODULE,IPRIN)                                          157
      IF (IPRIN.LT.0) RETURN                                              157
C Filling bank SFBK - NONE                                                157
      BANK_TITLE='NONE'                                                   157
      BPATH_SFBK='SFBK*'                                                  157
      IDNUM_SFBK(1)=0                                                     158
C NONE                                                                    158
      SFBK_VERSION=1.0                                                    158
      CSFBK_VERSION='NONE'                                                158
C NONE                                                                    159
      SFBK_RMIN=RADOUTERFBASE                                             159
      CSFBK_RMIN='NONE'                                                   159
C NONE                                                                    160
      SFBK_RMAX=RADOUTERFBACKER                                           160
      CSFBK_RMAX='NONE'                                                   160
C NONE                                                                    161
      SFBK_DZ=LENGTHFBACKER                                               161
      CSFBK_DZ='NONE'                                                     161
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFBK',    162
     *BPATH_SFBK,IDNUM_SFBK,1, MAP_SFBK,NAMES_SFBK,COMMEN_SFBK,           162
     *BANK_SFBK,LEN_SFBK,LLEN_SFBK,LINK_SFBK,FLAG_SFBK)                   162
C *                                                                       162
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'SFBK',    162
     *BPATH_SFBK,IDNUM_SFBK,1, MAP_SFBK,NAMES_SFBK,COMMEN_SFBK,           162
     *BANK_SFBK,LEN_SFBK,LLEN_SFBK,LINK_SFBK,FLAG_SFBK)                   162
C *                                                                       163
      CALL AGSBEGM(MODULE,IPRIN)                                          163
      IF (IPRIN.LT.0) RETURN                                              163
C Filling bank BPPC - BEAM PIPE SUPPORT RING                              163
      BANK_TITLE='BEAM PIPE SUPPORT RING'                                 163
      BPATH_BPPC='BPPC*'                                                  163
      IDNUM_BPPC(1)=0                                                     164
C NONE                                                                    164
      BPPC_VERSION=1.0                                                    164
      CBPPC_VERSION='NONE'                                                164
C NONE                                                                    165
      BPPC_RMIN=2.8                                                       165
      CBPPC_RMIN='NONE'                                                   165
C NONE                                                                    166
      BPPC_RMAX=13.2                                                      166
      CBPPC_RMAX='NONE'                                                   166
C NONE                                                                    167
      BPPC_DZ=0.1                                                         167
      CBPPC_DZ='NONE'                                                     167
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'BPPC',    168
     *BPATH_BPPC,IDNUM_BPPC,1, MAP_BPPC,NAMES_BPPC,COMMEN_BPPC,           168
     *BANK_BPPC,LEN_BPPC,LLEN_BPPC,LINK_BPPC,FLAG_BPPC)                   168
C *                                                                       168
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'BPPC',    168
     *BPATH_BPPC,IDNUM_BPPC,1, MAP_BPPC,NAMES_BPPC,COMMEN_BPPC,           168
     *BANK_BPPC,LEN_BPPC,LLEN_BPPC,LINK_BPPC,FLAG_BPPC)                   168
C *                                                                       169
      CALL AGSBEGM(MODULE,IPRIN)                                          169
      IF (IPRIN.LT.0) RETURN                                              169
C Filling bank MOSC - OSC MSC SEAL                                        169
      BANK_TITLE='OSC MSC SEAL'                                           169
      BPATH_MOSC='MOSC*'                                                  169
      IDNUM_MOSC(1)=0                                                     170
C NONE                                                                    170
      MOSC_VERSION=1.0                                                    170
      CMOSC_VERSION='NONE'                                                170
C NONE                                                                    171
      MOSC_RMIN=25.9                                                      171
      CMOSC_RMIN='NONE'                                                   171
C NONE                                                                    172
      MOSC_RMAX=42.0                                                      172
      CMOSC_RMAX='NONE'                                                   172
C NONE                                                                    173
      MOSC_DZ=0.1                                                         173
      CMOSC_DZ='NONE'                                                     173
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOSC',    174
     *BPATH_MOSC,IDNUM_MOSC,1, MAP_MOSC,NAMES_MOSC,COMMEN_MOSC,           174
     *BANK_MOSC,LEN_MOSC,LLEN_MOSC,LINK_MOSC,FLAG_MOSC)                   174
C *                                                                       174
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'MOSC',    174
     *BPATH_MOSC,IDNUM_MOSC,1, MAP_MOSC,NAMES_MOSC,COMMEN_MOSC,           174
     *BANK_MOSC,LEN_MOSC,LLEN_MOSC,LINK_MOSC,FLAG_MOSC)                   174
C *                                                                       175
      CALL AGSBEGM(MODULE,IPRIN)                                          175
      IF (IPRIN.LT.0) RETURN                                              175
C Filling bank PITA - NONE                                                175
      BANK_TITLE='NONE'                                                   175
      BPATH_PITA='PITA*'                                                  175
      IDNUM_PITA(1)=0                                                     176
C NONE                                                                    176
      PITA_VERSION=1.0                                                    176
      CPITA_VERSION='NONE'                                                176
C NONE                                                                    177
      PITA_RMIN=PITROUTER                                                 177
      CPITA_RMIN='NONE'                                                   177
C NONE                                                                    178
      PITA_RMAX=PITFR3                                                    178
      CPITA_RMAX='NONE'                                                   178
C NONE                                                                    179
      PITA_DZ=PITLENGTHA                                                  179
      CPITA_DZ='NONE'                                                     179
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PITA',    180
     *BPATH_PITA,IDNUM_PITA,1, MAP_PITA,NAMES_PITA,COMMEN_PITA,           180
     *BANK_PITA,LEN_PITA,LLEN_PITA,LINK_PITA,FLAG_PITA)                   180
C *                                                                       180
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PITA',    180
     *BPATH_PITA,IDNUM_PITA,1, MAP_PITA,NAMES_PITA,COMMEN_PITA,           180
     *BANK_PITA,LEN_PITA,LLEN_PITA,LINK_PITA,FLAG_PITA)                   180
C *                                                                       181
      CALL AGSBEGM(MODULE,IPRIN)                                          181
      IF (IPRIN.LT.0) RETURN                                              181
C Filling bank PITB - NONE                                                181
      BANK_TITLE='NONE'                                                   181
      BPATH_PITB='PITB*'                                                  181
      IDNUM_PITB(1)=0                                                     182
C NONE                                                                    182
      PITB_VERSION=1.0                                                    182
      CPITB_VERSION='NONE'                                                182
C NONE                                                                    183
      PITB_RMIN=PITROUTER                                                 183
      CPITB_RMIN='NONE'                                                   183
C NONE                                                                    184
      PITB_RMAX=PITFR4                                                    184
      CPITB_RMAX='NONE'                                                   184
C NONE                                                                    185
      PITB_DZ=PITLENGTHB                                                  185
      CPITB_DZ='NONE'                                                     185
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PITB',    186
     *BPATH_PITB,IDNUM_PITB,1, MAP_PITB,NAMES_PITB,COMMEN_PITB,           186
     *BANK_PITB,LEN_PITB,LLEN_PITB,LINK_PITB,FLAG_PITB)                   186
C *                                                                       186
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PITB',    186
     *BPATH_PITB,IDNUM_PITB,1, MAP_PITB,NAMES_PITB,COMMEN_PITB,           186
     *BANK_PITB,LEN_PITB,LLEN_PITB,LINK_PITB,FLAG_PITB)                   186
C *                                                                       187
      CALL AGSBEGM(MODULE,IPRIN)                                          187
      IF (IPRIN.LT.0) RETURN                                              187
C Filling bank PITC - NONE                                                187
      BANK_TITLE='NONE'                                                   187
      BPATH_PITC='PITC*'                                                  187
      IDNUM_PITC(1)=0                                                     188
C NONE                                                                    188
      PITC_VERSION=1.0                                                    188
      CPITC_VERSION='NONE'                                                188
C NONE                                                                    189
      PITC_RMIN=PITFR3                                                    189
      CPITC_RMIN='NONE'                                                   189
C NONE                                                                    190
      PITC_RMAX=PITFR4                                                    190
      CPITC_RMAX='NONE'                                                   190
C NONE                                                                    191
      PITC_DZ=PITLENGTHC                                                  191
      CPITC_DZ='NONE'                                                     191
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PITC',    192
     *BPATH_PITC,IDNUM_PITC,1, MAP_PITC,NAMES_PITC,COMMEN_PITC,           192
     *BANK_PITC,LEN_PITC,LLEN_PITC,LINK_PITC,FLAG_PITC)                   192
C *                                                                       192
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PITC',    192
     *BPATH_PITC,IDNUM_PITC,1, MAP_PITC,NAMES_PITC,COMMEN_PITC,           192
     *BANK_PITC,LEN_PITC,LLEN_PITC,LINK_PITC,FLAG_PITC)                   192
C *                                                                       193
      CALL AGSBEGM(MODULE,IPRIN)                                          193
      IF (IPRIN.LT.0) RETURN                                              193
C Filling bank PITR - NONE                                                193
      BANK_TITLE='NONE'                                                   193
      BPATH_PITR='PITR*'                                                  193
      IDNUM_PITR(1)=0                                                     194
C NONE                                                                    194
      PITR_VERSION=1.0                                                    194
      CPITR_VERSION='NONE'                                                194
C TUBE PARAMETERS FOR RAIL DIAMETER                                       195
      PITR_DMIN=1.8                                                       195
      CPITR_DMIN='TUBE PARAMETERS FOR RAIL DIAMETER'                      195
C NONE                                                                    196
      PITR_DMAX=2.0                                                       196
      CPITR_DMAX='NONE'                                                   196
C NONE                                                                    197
      PITR_DZ=211.4                                                       197
      CPITR_DZ='NONE'                                                     197
      CALL AGDOCUM (MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PITR',    198
     *BPATH_PITR,IDNUM_PITR,1, MAP_PITR,NAMES_PITR,COMMEN_PITR,           198
     *BANK_PITR,LEN_PITR,LLEN_PITR,LINK_PITR,FLAG_PITR)                   198
C *                                                                       198
      CALL AGDOCUME(MODULE, MCOMMENT,AUTHOR,CREATED,BANK_TITLE,'PITR',    198
     *BPATH_PITR,IDNUM_PITR,1, MAP_PITR,NAMES_PITR,COMMEN_PITR,           198
     *BANK_PITR,LEN_PITR,LLEN_PITR,LINK_PITR,FLAG_PITR)                   198
C Geant MATERIAL : CFRPMIX ===> A=12 Z=6 DENS=1.3 RADL=23.9 <===          199
      CALL AGSRESET                                                       199
      AG_A = 12                                                           199
      AG_Z = 6                                                            199
      AG_DENS = 1.3                                                       199
      AG_RADL = 23.9                                                      199
      AG_TITLE = 'CFRPMIX'                                                199
      AG_EXNAME = 'MATERIAL'                                              199
      AG_PARLIST = 'A_Z_DENS_RADL'                                        199
      CALL AXMATERIAL                                                     199
C *                                                                       199
C Geant COMPONENT : O ===> A=16 Z=8 W=0.209235 <===                       199
      CALL AGSRESET                                                       200
      AG_A = 16                                                           200
      AG_Z = 8                                                            200
      AG_W = 0.209235                                                     200
      AG_TITLE = 'O'                                                      200
      AG_EXNAME = 'COMPONENT'                                             200
      AG_PARLIST = 'A_Z_W'                                                200
      CALL AXCOMPONENT                                                    200
C *                                                                       200
C Geant COMPONENT : N ===> A=15 Z=7 W=0.073270 <===                       200
      CALL AGSRESET                                                       201
      AG_A = 15                                                           201
      AG_Z = 7                                                            201
      AG_W = 0.073270                                                     201
      AG_TITLE = 'N'                                                      201
      AG_EXNAME = 'COMPONENT'                                             201
      AG_PARLIST = 'A_Z_W'                                                201
      CALL AXCOMPONENT                                                    201
C *                                                                       201
C Geant COMPONENT : C ===> A=12 Z=6 W=0.691133 <===                       201
      CALL AGSRESET                                                       202
      AG_A = 12                                                           202
      AG_Z = 6                                                            202
      AG_W = 0.691133                                                     202
      AG_TITLE = 'C'                                                      202
      AG_EXNAME = 'COMPONENT'                                             202
      AG_PARLIST = 'A_Z_W'                                                202
      CALL AXCOMPONENT                                                    202
C *                                                                       202
C Geant COMPONENT : H ===> A=1 Z=1 W=0.026362 <===                        202
      CALL AGSRESET                                                       203
      AG_A = 1                                                            203
      AG_Z = 1                                                            203
      AG_W = 0.026362                                                     203
      AG_TITLE = 'H'                                                      203
      AG_EXNAME = 'COMPONENT'                                             203
      AG_PARLIST = 'A_Z_W'                                                203
      CALL AXCOMPONENT                                                    203
C *                                                                       203
C Geant MIXTURE : POLYIMIDE ===> DENS=1.41 <===                           203
      CALL AGSRESET                                                       204
      AG_DENS = 1.41                                                      204
      AG_TITLE = 'POLYIMIDE'                                              204
      AG_EXNAME = 'MIXTURE'                                               204
      AG_PARLIST = 'DENS'                                                 204
      CALL AXMIXTURE                                                      204
C *                                                                       204
      CALL AGSBEGM(MODULE,IPRIN)                                          204
C Check IPRIN<0                                                           205
      IF (IPRIN.LT.0) RETURN                                              205
C Create PSTM :                                                           205
C Call To Local Procedure PSTM                                            205
      ASSIGN 5020 TO BLOCK_PSTM                                           205
      GO TO  5010                                                         205
5020  CONTINUE                                                            205
C *                                                                       205
C Geant POSITION : PSTM ===> MOTHER='IDSM' Z=0 KONLY='MANY' <===          206
      CALL AGSRESET                                                       206
      AG_MOTHER = 'IDSM'                                                  206
      AG_Z = 0                                                            206
      AG_KONLY = 'MANY'                                                   206
      AG_TITLE = 'PSTM'                                                   206
      AG_EXNAME = 'POSITION'                                              206
      AG_PARLIST = 'MOTHER_Z_KONLY'                                       206
      CALL AXPOSITION                                                     206
C *                                                                       206
C ----------------------------------------------------------------------  206
C ------------ PSTM                                                       206
      CALL AGSENDM                                                        208
      RETURN                                                              208
C Begin Local Procedure PSTM                                              208
C PSTM MAIN VOLUME IN THE AGML TUTORIAL GEOMETRY                          208
5010  CONTINUE                                                            208
         AG_TITLE='PSTM'                                                  208
         AG_MODULE=MODULE                                                 208
         AG_MARK='V00'                                                    208
         CALL AGSPUSH                                                     208
         AG_MODULE=MODULE                                                 208
C    *                                                                    208
C    Geant MATERIAL : AIR ===> inherited arguments used <===              209
         CALL AGSRESET                                                    209
         AG_TITLE = 'AIR'                                                 209
         AG_EXNAME = 'MATERIAL'                                           209
         AG_PARLIST = 'NONE'                                              209
         CALL AXMATERIAL                                                  209
C    *                                                                    209
C    Geant MEDIUM : STANDARD ===> inherited arguments used <===           209
         CALL AGSRESET                                                    210
         AG_TITLE = 'STANDARD'                                            210
         AG_EXNAME = 'MEDIUM'                                             210
         AG_PARLIST = 'NONE'                                              210
         CALL AXMEDIUM                                                    210
C    *                                                                    210
C    Geant ATTRIBUTE : NONE ===> SEEN=0 <===                              210
         CALL AGSRESET                                                    211
         AG_SEEN = 0                                                      211
         AG_TITLE = 'NONE'                                                211
         AG_EXNAME = 'ATTRIBUTE'                                          211
         AG_PARLIST = 'SEEN'                                              211
         CALL AXATTRIBUTE                                                 211
C    *                                                                    211
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=2 ZI={-359, 60} RMN={MIN  211
C    RADPSTM,MINRADPSTM} RMX={MAXRADPSTM/2, MAXRADPSTM/2} <===            211
         CALL AGSRESET                                                    214
         AG_PHI1 = 0                                                      214
         AG_DPHI = 360                                                    214
         AG_NZ = 2                                                        214
         AG_ZI(1)=-359                                                    214
         AG_ZI(2)=60                                                      214
         AG_RMN(1)=MINRADPSTM                                             214
         AG_RMN(2)=MINRADPSTM                                             214
         AG_RMX(1)=MAXRADPSTM/2                                           214
         AG_RMX(2)=MAXRADPSTM/2                                           214
         AG_TITLE = 'PCON'                                                214
         AG_EXNAME = 'SHAPE'                                              214
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           214
         CALL AXSHAPE                                                     214
C    *                                                                    214
C    use PSTS bank                                                        214
         BPATH_PSTS='PSTS*'                                               215
         IDNUM_PSTS(1)=0                                                  215
         PSTS_VERSION=1.0                                                 215
         CALL AGDGETP('PXSTGEO1','*',-999,'PSTS',NPSTS_VERSION,           215
     *   RE_PSTS_VERSION,BPATH_PSTS,IDNUM_PSTS,1,MAP_PSTS,NAMES_PSTS,     215
     *   BANK_PSTS,LEN_PSTS,LLEN_PSTS,LINK_PSTS,FLAG_PSTS)                215
C    Create APTS :                                                        216
C    Call To Local Procedure APTS                                         216
         ASSIGN 5040 TO BLOCK_APTS                                        216
         GO TO  5030                                                      216
5040     CONTINUE                                                         216
C    *                                                                    216
C    Geant POSITION : APTS ===> X=0 Y=0 Z=0 <===                          217
         CALL AGSRESET                                                    217
         AG_X = 0                                                         217
         AG_Y = 0                                                         217
         AG_Z = 0                                                         217
         AG_TITLE = 'APTS'                                                217
         AG_EXNAME = 'POSITION'                                           217
         AG_PARLIST = 'X_Y_Z'                                             217
         CALL AXPOSITION                                                  217
C    *                                                                    217
C    use SFBA bank                                                        217
         BPATH_SFBA='SFBA*'                                               218
         IDNUM_SFBA(1)=0                                                  218
         SFBA_VERSION=1.0                                                 218
         CALL AGDGETP('PXSTGEO1','*',-999,'SFBA',NSFBA_VERSION,           218
     *   RE_SFBA_VERSION,BPATH_SFBA,IDNUM_SFBA,1,MAP_SFBA,NAMES_SFBA,     218
     *   BANK_SFBA,LEN_SFBA,LLEN_SFBA,LINK_SFBA,FLAG_SFBA)                218
C    Create LFBA :                                                        219
C    Call To Local Procedure LFBA                                         219
         ASSIGN 5060 TO BLOCK_LFBA                                        219
         GO TO  5050                                                      219
5060     CONTINUE                                                         219
C    *                                                                    219
C    Geant POSITION : LFBA ===> X=0 Y=0 Z=-PSTS_DZ/2. + SFBB_DZ + SFBA_D  220
C    Z/2. <===                                                            220
         CALL AGSRESET                                                    220
         AG_X = 0                                                         220
         AG_Y = 0                                                         220
         AG_Z = -PSTS_DZ/2. + SFBB_DZ + SFBA_DZ/2.                        220
         AG_TITLE = 'LFBA'                                                220
         AG_EXNAME = 'POSITION'                                           220
         AG_PARLIST = 'X_Y_Z'                                             220
         CALL AXPOSITION                                                  220
C    *                                                                    220
C    use SFBB bank                                                        220
         BPATH_SFBB='SFBB*'                                               221
         IDNUM_SFBB(1)=0                                                  221
         SFBB_VERSION=1.0                                                 221
         CALL AGDGETP('PXSTGEO1','*',-999,'SFBB',NSFBB_VERSION,           221
     *   RE_SFBB_VERSION,BPATH_SFBB,IDNUM_SFBB,1,MAP_SFBB,NAMES_SFBB,     221
     *   BANK_SFBB,LEN_SFBB,LLEN_SFBB,LINK_SFBB,FLAG_SFBB)                221
C    Create LFBB :                                                        222
C    Call To Local Procedure LFBB                                         222
         ASSIGN 5080 TO BLOCK_LFBB                                        222
         GO TO  5070                                                      222
5080     CONTINUE                                                         222
C    *                                                                    222
C    Geant POSITION : LFBB ===> X=0 Y=0 Z=-PSTS_DZ/2. + SFBB_DZ/2. <===   223
         CALL AGSRESET                                                    223
         AG_X = 0                                                         223
         AG_Y = 0                                                         223
         AG_Z = -PSTS_DZ/2. + SFBB_DZ/2.                                  223
         AG_TITLE = 'LFBB'                                                223
         AG_EXNAME = 'POSITION'                                           223
         AG_PARLIST = 'X_Y_Z'                                             223
         CALL AXPOSITION                                                  223
C    *                                                                    223
C    use SFBK bank                                                        223
         BPATH_SFBK='SFBK*'                                               224
         IDNUM_SFBK(1)=0                                                  224
         SFBK_VERSION=1.0                                                 224
         CALL AGDGETP('PXSTGEO1','*',-999,'SFBK',NSFBK_VERSION,           224
     *   RE_SFBK_VERSION,BPATH_SFBK,IDNUM_SFBK,1,MAP_SFBK,NAMES_SFBK,     224
     *   BANK_SFBK,LEN_SFBK,LLEN_SFBK,LINK_SFBK,FLAG_SFBK)                224
C    Create LFBK :                                                        225
C    Call To Local Procedure LFBK                                         225
         ASSIGN 5100 TO BLOCK_LFBK                                        225
         GO TO  5090                                                      225
5100     CONTINUE                                                         225
C    *                                                                    225
C    Geant POSITION : LFBK ===> X=0 Y=0 Z=-PSTS_DZ/2. + SFBB_DZ + SFBK_D  226
C    Z/2. <===                                                            226
         CALL AGSRESET                                                    226
         AG_X = 0                                                         226
         AG_Y = 0                                                         226
         AG_Z = -PSTS_DZ/2. + SFBB_DZ + SFBK_DZ/2.                        226
         AG_TITLE = 'LFBK'                                                226
         AG_EXNAME = 'POSITION'                                           226
         AG_PARLIST = 'X_Y_Z'                                             226
         CALL AXPOSITION                                                  226
C    *                                                                    226
C    use SFBA bank                                                        226
         BPATH_SFBA='SFBA*'                                               227
         IDNUM_SFBA(1)=0                                                  227
         SFBA_VERSION=1.0                                                 227
         CALL AGDGETP('PXSTGEO1','*',-999,'SFBA',NSFBA_VERSION,           227
     *   RE_SFBA_VERSION,BPATH_SFBA,IDNUM_SFBA,1,MAP_SFBA,NAMES_SFBA,     227
     *   BANK_SFBA,LEN_SFBA,LLEN_SFBA,LINK_SFBA,FLAG_SFBA)                227
C    Create RFBA :                                                        228
C    Call To Local Procedure RFBA                                         228
         ASSIGN 5120 TO BLOCK_RFBA                                        228
         GO TO  5110                                                      228
5120     CONTINUE                                                         228
C    *                                                                    228
C    Geant POSITION : RFBA ===> X=0 Y=0 Z=PSTS_DZ/2. - SFBB_DZ - SFBA_DZ  229
C    /2. <===                                                             229
         CALL AGSRESET                                                    229
         AG_X = 0                                                         229
         AG_Y = 0                                                         229
         AG_Z = PSTS_DZ/2. - SFBB_DZ - SFBA_DZ/2.                         229
         AG_TITLE = 'RFBA'                                                229
         AG_EXNAME = 'POSITION'                                           229
         AG_PARLIST = 'X_Y_Z'                                             229
         CALL AXPOSITION                                                  229
C    *                                                                    229
C    use SFBB bank                                                        229
         BPATH_SFBB='SFBB*'                                               230
         IDNUM_SFBB(1)=0                                                  230
         SFBB_VERSION=1.0                                                 230
         CALL AGDGETP('PXSTGEO1','*',-999,'SFBB',NSFBB_VERSION,           230
     *   RE_SFBB_VERSION,BPATH_SFBB,IDNUM_SFBB,1,MAP_SFBB,NAMES_SFBB,     230
     *   BANK_SFBB,LEN_SFBB,LLEN_SFBB,LINK_SFBB,FLAG_SFBB)                230
C    Create RFBB :                                                        231
C    Call To Local Procedure RFBB                                         231
         ASSIGN 5140 TO BLOCK_RFBB                                        231
         GO TO  5130                                                      231
5140     CONTINUE                                                         231
C    *                                                                    231
C    Geant POSITION : RFBB ===> X=0 Y=0 Z=PSTS_DZ/2. - SFBB_DZ/2. <===    232
         CALL AGSRESET                                                    232
         AG_X = 0                                                         232
         AG_Y = 0                                                         232
         AG_Z = PSTS_DZ/2. - SFBB_DZ/2.                                   232
         AG_TITLE = 'RFBB'                                                232
         AG_EXNAME = 'POSITION'                                           232
         AG_PARLIST = 'X_Y_Z'                                             232
         CALL AXPOSITION                                                  232
C    *                                                                    232
C    use SFBK bank                                                        232
         BPATH_SFBK='SFBK*'                                               233
         IDNUM_SFBK(1)=0                                                  233
         SFBK_VERSION=1.0                                                 233
         CALL AGDGETP('PXSTGEO1','*',-999,'SFBK',NSFBK_VERSION,           233
     *   RE_SFBK_VERSION,BPATH_SFBK,IDNUM_SFBK,1,MAP_SFBK,NAMES_SFBK,     233
     *   BANK_SFBK,LEN_SFBK,LLEN_SFBK,LINK_SFBK,FLAG_SFBK)                233
C    Create RFKK :                                                        234
C    Call To Local Procedure RFKK                                         234
         ASSIGN 5160 TO BLOCK_RFKK                                        234
         GO TO  5150                                                      234
5160     CONTINUE                                                         234
C    *                                                                    234
C    Geant POSITION : RFKK ===> X=0 Y=0 Z=PSTS_DZ/2. - SFBB_DZ - SFBK_DZ  235
C    /2. <===                                                             235
         CALL AGSRESET                                                    235
         AG_X = 0                                                         235
         AG_Y = 0                                                         235
         AG_Z = PSTS_DZ/2. - SFBB_DZ - SFBK_DZ/2.                         235
         AG_TITLE = 'RFKK'                                                235
         AG_EXNAME = 'POSITION'                                           235
         AG_PARLIST = 'X_Y_Z'                                             235
         CALL AXPOSITION                                                  235
C    *                                                                    235
C    use BPPC bank                                                        235
         BPATH_BPPC='BPPC*'                                               236
         IDNUM_BPPC(1)=0                                                  236
         BPPC_VERSION=1.0                                                 236
         CALL AGDGETP('PXSTGEO1','*',-999,'BPPC',NBPPC_VERSION,           236
     *   RE_BPPC_VERSION,BPATH_BPPC,IDNUM_BPPC,1,MAP_BPPC,NAMES_BPPC,     236
     *   BANK_BPPC,LEN_BPPC,LLEN_BPPC,LINK_BPPC,FLAG_BPPC)                236
C    Create BPPC :                                                        237
C    Call To Local Procedure BPPC                                         237
         ASSIGN 5180 TO BLOCK_BPPC                                        237
         GO TO  5170                                                      237
5180     CONTINUE                                                         237
C    *                                                                    237
C    Geant POSITION : BPPC ===> X=0 Y=0 Z=PSTS_DZ/2.0 + BPPC_DZ/2.0 <===  238
         CALL AGSRESET                                                    238
         AG_X = 0                                                         238
         AG_Y = 0                                                         238
         AG_Z = PSTS_DZ/2.0 + BPPC_DZ/2.0                                 238
         AG_TITLE = 'BPPC'                                                238
         AG_EXNAME = 'POSITION'                                           238
         AG_PARLIST = 'X_Y_Z'                                             238
         CALL AXPOSITION                                                  238
C    *                                                                    238
C    Create MOSC :                                                        239
C    Call To Local Procedure MOSC                                         239
         ASSIGN 5200 TO BLOCK_MOSC                                        239
         GO TO  5190                                                      239
5200     CONTINUE                                                         239
C    *                                                                    239
C    Geant POSITION : MOSC ===> X=0 Y=0 Z=PSTS_DZ/2.0 + BPPC_DZ +MOSC_DZ  240
C    /2.0 <===                                                            240
         CALL AGSRESET                                                    240
         AG_X = 0                                                         240
         AG_Y = 0                                                         240
         AG_Z = PSTS_DZ/2.0 + BPPC_DZ +MOSC_DZ/2.0                        240
         AG_TITLE = 'MOSC'                                                240
         AG_EXNAME = 'POSITION'                                           240
         AG_PARLIST = 'X_Y_Z'                                             240
         CALL AXPOSITION                                                  240
C    *                                                                    240
C    Create MTPA :                                                        241
C    Call To Local Procedure MTPA                                         241
         ASSIGN 5220 TO BLOCK_MTPA                                        241
         GO TO  5210                                                      241
5220     CONTINUE                                                         241
C    *                                                                    241
C    Geant POSITION : MTPA ===> X=0 Y=0 Z=0 <===                          242
         CALL AGSRESET                                                    242
         AG_X = 0                                                         242
         AG_Y = 0                                                         242
         AG_Z = 0                                                         242
         AG_TITLE = 'MTPA'                                                242
         AG_EXNAME = 'POSITION'                                           242
         AG_PARLIST = 'X_Y_Z'                                             242
         CALL AXPOSITION                                                  242
C    *                                                                    242
C    Create MTPB :                                                        243
C    Call To Local Procedure MTPB                                         243
         ASSIGN 5240 TO BLOCK_MTPB                                        243
         GO TO  5230                                                      243
5240     CONTINUE                                                         243
C    *                                                                    243
C    Geant POSITION : MTPB ===> X=0 Y=0 Z=0 <===                          244
         CALL AGSRESET                                                    244
         AG_X = 0                                                         244
         AG_Y = 0                                                         244
         AG_Z = 0                                                         244
         AG_TITLE = 'MTPB'                                                244
         AG_EXNAME = 'POSITION'                                           244
         AG_PARLIST = 'X_Y_Z'                                             244
         CALL AXPOSITION                                                  244
C    *                                                                    244
C    Create MTPC :                                                        245
C    Call To Local Procedure MTPC                                         245
         ASSIGN 5260 TO BLOCK_MTPC                                        245
         GO TO  5250                                                      245
5260     CONTINUE                                                         245
C    *                                                                    245
C    Geant POSITION : MTPC ===> X=0 Y=0 Z=-PSTS_DZ/2. - WIDTHLOWERRINGTP  246
C     - WIDTHRINGTP/2. <===                                               246
         CALL AGSRESET                                                    246
         AG_X = 0                                                         246
         AG_Y = 0                                                         246
         AG_Z = -PSTS_DZ/2. - WIDTHLOWERRINGTP - WIDTHRINGTP/2.           246
         AG_TITLE = 'MTPC'                                                246
         AG_EXNAME = 'POSITION'                                           246
         AG_PARLIST = 'X_Y_Z'                                             246
         CALL AXPOSITION                                                  246
C    *                                                                    246
C    Create MTPD :                                                        247
C    Call To Local Procedure MTPD                                         247
         ASSIGN 5280 TO BLOCK_MTPD                                        247
         GO TO  5270                                                      247
5280     CONTINUE                                                         247
C    *                                                                    247
C    Geant POSITION : MTPD ===> X=0 Y=0 Z=0 <===                          248
         CALL AGSRESET                                                    248
         AG_X = 0                                                         248
         AG_Y = 0                                                         248
         AG_Z = 0                                                         248
         AG_TITLE = 'MTPD'                                                248
         AG_EXNAME = 'POSITION'                                           248
         AG_PARLIST = 'X_Y_Z'                                             248
         CALL AXPOSITION                                                  248
C    *                                                                    248
C    Create MTPE :                                                        249
C    Call To Local Procedure MTPE                                         249
         ASSIGN 5300 TO BLOCK_MTPE                                        249
         GO TO  5290                                                      249
5300     CONTINUE                                                         249
C    *                                                                    249
C    Geant POSITION : MTPE ===> X=0 Y=0 Z=-PSTS_DZ/2. - WIDTHRINGTP/2. <  250
C    ===                                                                  250
         CALL AGSRESET                                                    250
         AG_X = 0                                                         250
         AG_Y = 0                                                         250
         AG_Z = -PSTS_DZ/2. - WIDTHRINGTP/2.                              250
         AG_TITLE = 'MTPE'                                                250
         AG_EXNAME = 'POSITION'                                           250
         AG_PARLIST = 'X_Y_Z'                                             250
         CALL AXPOSITION                                                  250
C    *                                                                    250
C    Create BPRS :                                                        251
C    Call To Local Procedure BPRS                                         251
         ASSIGN 5320 TO BLOCK_BPRS                                        251
         GO TO  5310                                                      251
5320     CONTINUE                                                         251
C    *                                                                    251
C    Geant POSITION : BPRS ===> X=0 Y=0 Z=-1*(PSTS_DZ/2. + WIDTHINNERRIN  253
C    GTP + WIDTHLOWERRINGTP ) + .1 <===                                   253
         CALL AGSRESET                                                    253
         AG_X = 0                                                         253
         AG_Y = 0                                                         253
         AG_Z = -1*(PSTS_DZ/2. + WIDTHINNERRINGTP + WIDTHLOWERRINGTP )    253
     *   + .1                                                             253
         AG_TITLE = 'BPRS'                                                253
         AG_EXNAME = 'POSITION'                                           253
         AG_PARLIST = 'X_Y_Z'                                             253
         CALL AXPOSITION                                                  253
C    *                                                                    253
C    Create BPRS :                                                        254
C    Call To Local Procedure BPRS                                         254
         ASSIGN 5330 TO BLOCK_BPRS                                        254
         GO TO  5310                                                      254
5330     CONTINUE                                                         254
C    *                                                                    254
C    Geant POSITION : BPRS ===> X=0 Y=0 Z=-1*(PSTS_DZ/2. + WIDTHINNERRIN  256
C    GTP + WIDTHLOWERRINGTP ) + .1 + RINGTHK + RINGDIST + RINGTHK/2. <==  256
C    =                                                                    256
         CALL AGSRESET                                                    256
         AG_X = 0                                                         256
         AG_Y = 0                                                         256
         AG_Z = -1*(PSTS_DZ/2. + WIDTHINNERRINGTP + WIDTHLOWERRINGTP )    256
     *   + .1 + RINGTHK + RINGDIST + RINGTHK/2.                           256
         AG_TITLE = 'BPRS'                                                256
         AG_EXNAME = 'POSITION'                                           256
         AG_PARLIST = 'X_Y_Z'                                             256
         CALL AXPOSITION                                                  256
C    *                                                                    256
C    Create BPRS :                                                        257
C    Call To Local Procedure BPRS                                         257
         ASSIGN 5340 TO BLOCK_BPRS                                        257
         GO TO  5310                                                      257
5340     CONTINUE                                                         257
C    *                                                                    257
C    Geant POSITION : BPRS ===> X=0 Y=0 Z=PSTS_DZ/2. + BPPC_DZ/2.0 <===   258
         CALL AGSRESET                                                    258
         AG_X = 0                                                         258
         AG_Y = 0                                                         258
         AG_Z = PSTS_DZ/2. + BPPC_DZ/2.0                                  258
         AG_TITLE = 'BPRS'                                                258
         AG_EXNAME = 'POSITION'                                           258
         AG_PARLIST = 'X_Y_Z'                                             258
         CALL AXPOSITION                                                  258
C    *                                                                    258
C    Create MTPF :                                                        259
C    Call To Local Procedure MTPF                                         259
         ASSIGN 5360 TO BLOCK_MTPF                                        259
         GO TO  5350                                                      259
5360     CONTINUE                                                         259
C    *                                                                    259
C    Geant POSITION : MTPF ===> X=0 Y=((RINGDIAMETERMAX/2) + 0.5*(RADOUT  262
C    ERMINTP/2 - RINGDIAMETERMAX/2)) Z=-1*(PSTS_DZ/2. + WIDTHINNERRINGTP  262
C     + WIDTHLOWERRINGTP + WIDTHRINGTP/2.) <===                           262
         CALL AGSRESET                                                    262
         AG_X = 0                                                         262
         AG_Y = ((RINGDIAMETERMAX/2) + 0.5*(RADOUTERMINTP/2 -             262
     *   RINGDIAMETERMAX/2))                                              262
         AG_Z = -1*(PSTS_DZ/2. + WIDTHINNERRINGTP + WIDTHLOWERRINGTP +    262
     *   WIDTHRINGTP/2.)                                                  262
         AG_TITLE = 'MTPF'                                                262
         AG_EXNAME = 'POSITION'                                           262
         AG_PARLIST = 'X_Y_Z'                                             262
         CALL AXPOSITION                                                  262
C    *                                                                    262
C    Create MTPF :                                                        263
C    Call To Local Procedure MTPF                                         263
         ASSIGN 5370 TO BLOCK_MTPF                                        263
         GO TO  5350                                                      263
5370     CONTINUE                                                         263
C    *                                                                    263
C    Geant POSITION : MTPF ===> X=0 Y=-1*((RINGDIAMETERMAX/2) + 0.5*(RAD  266
C    OUTERMINTP/2 - RINGDIAMETERMAX/2)) Z=-1*(PSTS_DZ/2. + WIDTHINNERRIN  266
C    GTP + WIDTHLOWERRINGTP + WIDTHRINGTP/2.) <===                        266
         CALL AGSRESET                                                    266
         AG_X = 0                                                         266
         AG_Y = -1*((RINGDIAMETERMAX/2) + 0.5*(RADOUTERMINTP/2 -          266
     *   RINGDIAMETERMAX/2))                                              266
         AG_Z = -1*(PSTS_DZ/2. + WIDTHINNERRINGTP + WIDTHLOWERRINGTP +    266
     *   WIDTHRINGTP/2.)                                                  266
         AG_TITLE = 'MTPF'                                                266
         AG_EXNAME = 'POSITION'                                           266
         AG_PARLIST = 'X_Y_Z'                                             266
         CALL AXPOSITION                                                  266
C    *                                                                    266
C    Create MTPG :                                                        267
C    Call To Local Procedure MTPG                                         267
         ASSIGN 5390 TO BLOCK_MTPG                                        267
         GO TO  5380                                                      267
5390     CONTINUE                                                         267
C    *                                                                    267
C    Geant POSITION : MTPG ===> X=0 Y=((RINGDIAMETERMAX/2) + 0.5*(RADINN  270
C    ERSHELL/2. - RINGDIAMETERMAX/2.)) Z=-1*(PSTS_DZ/2.) - 1*(WIDTHINNER  270
C    RINGTP + WIDTHLOWERRINGTP)/2. <===                                   270
         CALL AGSRESET                                                    270
         AG_X = 0                                                         270
         AG_Y = ((RINGDIAMETERMAX/2) + 0.5*(RADINNERSHELL/2. -            270
     *   RINGDIAMETERMAX/2.))                                             270
         AG_Z = -1*(PSTS_DZ/2.) - 1*(WIDTHINNERRINGTP +                   270
     *   WIDTHLOWERRINGTP)/2.                                             270
         AG_TITLE = 'MTPG'                                                270
         AG_EXNAME = 'POSITION'                                           270
         AG_PARLIST = 'X_Y_Z'                                             270
         CALL AXPOSITION                                                  270
C    *                                                                    270
C    Create MTPG :                                                        271
C    Call To Local Procedure MTPG                                         271
         ASSIGN 5400 TO BLOCK_MTPG                                        271
         GO TO  5380                                                      271
5400     CONTINUE                                                         271
C    *                                                                    271
C    Geant POSITION : MTPG ===> X=0 Y=-1*((RINGDIAMETERMAX/2) + 0.5*(RAD  274
C    INNERSHELL/2. - RINGDIAMETERMAX/2.)) Z=-1*(PSTS_DZ/2.) - 1*(WIDTHIN  274
C    NERRINGTP + WIDTHLOWERRINGTP)/2. <===                                274
         CALL AGSRESET                                                    274
         AG_X = 0                                                         274
         AG_Y = -1*((RINGDIAMETERMAX/2) + 0.5*(RADINNERSHELL/2. -         274
     *   RINGDIAMETERMAX/2.))                                             274
         AG_Z = -1*(PSTS_DZ/2.) - 1*(WIDTHINNERRINGTP +                   274
     *   WIDTHLOWERRINGTP)/2.                                             274
         AG_TITLE = 'MTPG'                                                274
         AG_EXNAME = 'POSITION'                                           274
         AG_PARLIST = 'X_Y_Z'                                             274
         CALL AXPOSITION                                                  274
C    *                                                                    274
C    Create MTPH :                                                        275
C    Call To Local Procedure MTPH                                         275
         ASSIGN 5420 TO BLOCK_MTPH                                        275
         GO TO  5410                                                      275
5420     CONTINUE                                                         275
C    *                                                                    275
C    Geant POSITION : MTPH ===> X=0 Y=0 Z=-PSTS_DZ/2 - WIDTHLOWERRINGTP   276
C    + 0.85/2 +0.5 <===                                                   276
         CALL AGSRESET                                                    276
         AG_X = 0                                                         276
         AG_Y = 0                                                         276
         AG_Z = -PSTS_DZ/2 - WIDTHLOWERRINGTP + 0.85/2 +0.5               276
         AG_TITLE = 'MTPH'                                                276
         AG_EXNAME = 'POSITION'                                           276
         AG_PARLIST = 'X_Y_Z'                                             276
         CALL AXPOSITION                                                  276
C    *                                                                    276
C    Create PITN :                                                        277
C    Call To Local Procedure PITN                                         277
         ASSIGN 5440 TO BLOCK_PITN                                        277
         GO TO  5430                                                      277
5440     CONTINUE                                                         277
C    *                                                                    277
C    Geant POSITION : PITN ===> X=0 Y=0 Z= -1*(PSTS_DZ/2. + WIDTHRINGTP   279
C    + WIDTHINNERRINGTP + WIDTHLOWERRINGTP) - PITLENGTH/2. <===           279
         CALL AGSRESET                                                    279
         AG_X = 0                                                         279
         AG_Y = 0                                                         279
         AG_Z = -1*(PSTS_DZ/2. + WIDTHRINGTP + WIDTHINNERRINGTP +         279
     *   WIDTHLOWERRINGTP) - PITLENGTH/2.                                 279
         AG_TITLE = 'PITN'                                                279
         AG_EXNAME = 'POSITION'                                           279
         AG_PARLIST = 'X_Y_Z'                                             279
         CALL AXPOSITION                                                  279
C    *                                                                    279
C    use PITA bank                                                        279
         BPATH_PITA='PITA*'                                               280
         IDNUM_PITA(1)=0                                                  280
         PITA_VERSION=1.0                                                 280
         CALL AGDGETP('PXSTGEO1','*',-999,'PITA',NPITA_VERSION,           280
     *   RE_PITA_VERSION,BPATH_PITA,IDNUM_PITA,1,MAP_PITA,NAMES_PITA,     280
     *   BANK_PITA,LEN_PITA,LLEN_PITA,LINK_PITA,FLAG_PITA)                280
C    Create APFA :                                                        281
C    Call To Local Procedure APFA                                         281
         ASSIGN 5460 TO BLOCK_APFA                                        281
         GO TO  5450                                                      281
5460     CONTINUE                                                         281
C    *                                                                    281
C    Geant POSITION : APFA ===> X=0 Y=0 Z=-1*(PSTS_DZ/2. + WIDTHRINGTP +  283
C     WIDTHINNERRINGTP + WIDTHLOWERRINGTP) - PITB_DZ - PITA_DZ/2. <===    283
         CALL AGSRESET                                                    283
         AG_X = 0                                                         283
         AG_Y = 0                                                         283
         AG_Z = -1*(PSTS_DZ/2. + WIDTHRINGTP + WIDTHINNERRINGTP +         283
     *   WIDTHLOWERRINGTP) - PITB_DZ - PITA_DZ/2.                         283
         AG_TITLE = 'APFA'                                                283
         AG_EXNAME = 'POSITION'                                           283
         AG_PARLIST = 'X_Y_Z'                                             283
         CALL AXPOSITION                                                  283
C    *                                                                    283
C    use PITB bank                                                        283
         BPATH_PITB='PITB*'                                               284
         IDNUM_PITB(1)=0                                                  284
         PITB_VERSION=1.0                                                 284
         CALL AGDGETP('PXSTGEO1','*',-999,'PITB',NPITB_VERSION,           284
     *   RE_PITB_VERSION,BPATH_PITB,IDNUM_PITB,1,MAP_PITB,NAMES_PITB,     284
     *   BANK_PITB,LEN_PITB,LLEN_PITB,LINK_PITB,FLAG_PITB)                284
C    Create APFB :                                                        285
C    Call To Local Procedure APFB                                         285
         ASSIGN 5480 TO BLOCK_APFB                                        285
         GO TO  5470                                                      285
5480     CONTINUE                                                         285
C    *                                                                    285
C    Geant POSITION : APFB ===> X=0 Y=0 Z=-1*(PSTS_DZ/2. + WIDTHRINGTP +  287
C     WIDTHINNERRINGTP + WIDTHLOWERRINGTP) - PITB_DZ/2. <===              287
         CALL AGSRESET                                                    287
         AG_X = 0                                                         287
         AG_Y = 0                                                         287
         AG_Z = -1*(PSTS_DZ/2. + WIDTHRINGTP + WIDTHINNERRINGTP +         287
     *   WIDTHLOWERRINGTP) - PITB_DZ/2.                                   287
         AG_TITLE = 'APFB'                                                287
         AG_EXNAME = 'POSITION'                                           287
         AG_PARLIST = 'X_Y_Z'                                             287
         CALL AXPOSITION                                                  287
C    *                                                                    287
C    use PITC bank                                                        287
         BPATH_PITC='PITC*'                                               288
         IDNUM_PITC(1)=0                                                  288
         PITC_VERSION=1.0                                                 288
         CALL AGDGETP('PXSTGEO1','*',-999,'PITC',NPITC_VERSION,           288
     *   RE_PITC_VERSION,BPATH_PITC,IDNUM_PITC,1,MAP_PITC,NAMES_PITC,     288
     *   BANK_PITC,LEN_PITC,LLEN_PITC,LINK_PITC,FLAG_PITC)                288
C    Create APFC :                                                        289
C    Call To Local Procedure APFC                                         289
         ASSIGN 5500 TO BLOCK_APFC                                        289
         GO TO  5490                                                      289
5500     CONTINUE                                                         289
C    *                                                                    289
C    Geant POSITION : APFC ===> X=0 Y=0 Z=-1*(PSTS_DZ/2. + WIDTHRINGTP +  291
C     WIDTHINNERRINGTP + WIDTHLOWERRINGTP) - PITB_DZ - PITC_DZ/2. <===    291
         CALL AGSRESET                                                    291
         AG_X = 0                                                         291
         AG_Y = 0                                                         291
         AG_Z = -1*(PSTS_DZ/2. + WIDTHRINGTP + WIDTHINNERRINGTP +         291
     *   WIDTHLOWERRINGTP) - PITB_DZ - PITC_DZ/2.                         291
         AG_TITLE = 'APFC'                                                291
         AG_EXNAME = 'POSITION'                                           291
         AG_PARLIST = 'X_Y_Z'                                             291
         CALL AXPOSITION                                                  291
C    *                                                                    291
C    use PITA bank                                                        291
         BPATH_PITA='PITA*'                                               292
         IDNUM_PITA(1)=0                                                  292
         PITA_VERSION=1.0                                                 292
         CALL AGDGETP('PXSTGEO1','*',-999,'PITA',NPITA_VERSION,           292
     *   RE_PITA_VERSION,BPATH_PITA,IDNUM_PITA,1,MAP_PITA,NAMES_PITA,     292
     *   BANK_PITA,LEN_PITA,LLEN_PITA,LINK_PITA,FLAG_PITA)                292
C    Create APFD :                                                        293
C    Call To Local Procedure APFD                                         293
         ASSIGN 5520 TO BLOCK_APFD                                        293
         GO TO  5510                                                      293
5520     CONTINUE                                                         293
C    *                                                                    293
C    Geant POSITION : APFD ===> X=0 Y=0 Z=-1*(PSTS_DZ/2. + WIDTHRINGTP +  295
C     WIDTHINNERRINGTP + WIDTHLOWERRINGTP + PITLENGTH) + PITB_DZ + PITA_  295
C    DZ/2. <===                                                           295
         CALL AGSRESET                                                    295
         AG_X = 0                                                         295
         AG_Y = 0                                                         295
         AG_Z = -1*(PSTS_DZ/2. + WIDTHRINGTP + WIDTHINNERRINGTP +         295
     *   WIDTHLOWERRINGTP + PITLENGTH) + PITB_DZ + PITA_DZ/2.             295
         AG_TITLE = 'APFD'                                                295
         AG_EXNAME = 'POSITION'                                           295
         AG_PARLIST = 'X_Y_Z'                                             295
         CALL AXPOSITION                                                  295
C    *                                                                    295
C    use PITB bank                                                        295
         BPATH_PITB='PITB*'                                               296
         IDNUM_PITB(1)=0                                                  296
         PITB_VERSION=1.0                                                 296
         CALL AGDGETP('PXSTGEO1','*',-999,'PITB',NPITB_VERSION,           296
     *   RE_PITB_VERSION,BPATH_PITB,IDNUM_PITB,1,MAP_PITB,NAMES_PITB,     296
     *   BANK_PITB,LEN_PITB,LLEN_PITB,LINK_PITB,FLAG_PITB)                296
C    Create APFE :                                                        297
C    Call To Local Procedure APFE                                         297
         ASSIGN 5540 TO BLOCK_APFE                                        297
         GO TO  5530                                                      297
5540     CONTINUE                                                         297
C    *                                                                    297
C    Geant POSITION : APFE ===> X=0 Y=0 Z=-1*(PSTS_DZ/2. + WIDTHRINGTP +  299
C     WIDTHINNERRINGTP + WIDTHLOWERRINGTP + PITLENGTH ) + PITB_DZ/2. <==  299
C    =                                                                    299
         CALL AGSRESET                                                    299
         AG_X = 0                                                         299
         AG_Y = 0                                                         299
         AG_Z = -1*(PSTS_DZ/2. + WIDTHRINGTP + WIDTHINNERRINGTP +         299
     *   WIDTHLOWERRINGTP + PITLENGTH ) + PITB_DZ/2.                      299
         AG_TITLE = 'APFE'                                                299
         AG_EXNAME = 'POSITION'                                           299
         AG_PARLIST = 'X_Y_Z'                                             299
         CALL AXPOSITION                                                  299
C    *                                                                    299
C    Create APFF :                                                        300
C    Call To Local Procedure APFF                                         300
         ASSIGN 5560 TO BLOCK_APFF                                        300
         GO TO  5550                                                      300
5560     CONTINUE                                                         300
C    *                                                                    300
C    Geant POSITION : APFF ===> X=0 Y=0 Z=-1*(PSTS_DZ/2. + WIDTHRINGTP +  302
C     WIDTHINNERRINGTP + WIDTHLOWERRINGTP + PITLENGTH) + PITB_DZ + PITC_  302
C    DZ/2. <===                                                           302
         CALL AGSRESET                                                    302
         AG_X = 0                                                         302
         AG_Y = 0                                                         302
         AG_Z = -1*(PSTS_DZ/2. + WIDTHRINGTP + WIDTHINNERRINGTP +         302
     *   WIDTHLOWERRINGTP + PITLENGTH) + PITB_DZ + PITC_DZ/2.             302
         AG_TITLE = 'APFF'                                                302
         AG_EXNAME = 'POSITION'                                           302
         AG_PARLIST = 'X_Y_Z'                                             302
         CALL AXPOSITION                                                  302
C    *                                                                    302
C    use PITA bank                                                        302
         BPATH_PITA='PITA*'                                               303
         IDNUM_PITA(1)=0                                                  303
         PITA_VERSION=1.0                                                 303
         CALL AGDGETP('PXSTGEO1','*',-999,'PITA',NPITA_VERSION,           303
     *   RE_PITA_VERSION,BPATH_PITA,IDNUM_PITA,1,MAP_PITA,NAMES_PITA,     303
     *   BANK_PITA,LEN_PITA,LLEN_PITA,LINK_PITA,FLAG_PITA)                303
C    Create APFG :                                                        304
C    Call To Local Procedure APFG                                         304
         ASSIGN 5580 TO BLOCK_APFG                                        304
         GO TO  5570                                                      304
5580     CONTINUE                                                         304
C    *                                                                    304
C    Geant POSITION : APFG ===> X=0 Y=0 Z=-1*(PSTS_DZ/2. + WIDTHRINGTP +  306
C     WIDTHINNERRINGTP + WIDTHLOWERRINGTP + PITLENGTH*PITFACTOR) + PITB_  306
C    DZ + PITA_DZ/2. <===                                                 306
         CALL AGSRESET                                                    306
         AG_X = 0                                                         306
         AG_Y = 0                                                         306
         AG_Z = -1*(PSTS_DZ/2. + WIDTHRINGTP + WIDTHINNERRINGTP +         306
     *   WIDTHLOWERRINGTP + PITLENGTH*PITFACTOR) + PITB_DZ + PITA_DZ/2.   306
         AG_TITLE = 'APFG'                                                306
         AG_EXNAME = 'POSITION'                                           306
         AG_PARLIST = 'X_Y_Z'                                             306
         CALL AXPOSITION                                                  306
C    *                                                                    306
C    use PITB bank                                                        306
         BPATH_PITB='PITB*'                                               307
         IDNUM_PITB(1)=0                                                  307
         PITB_VERSION=1.0                                                 307
         CALL AGDGETP('PXSTGEO1','*',-999,'PITB',NPITB_VERSION,           307
     *   RE_PITB_VERSION,BPATH_PITB,IDNUM_PITB,1,MAP_PITB,NAMES_PITB,     307
     *   BANK_PITB,LEN_PITB,LLEN_PITB,LINK_PITB,FLAG_PITB)                307
C    Create APFH :                                                        308
C    Call To Local Procedure APFH                                         308
         ASSIGN 5600 TO BLOCK_APFH                                        308
         GO TO  5590                                                      308
5600     CONTINUE                                                         308
C    *                                                                    308
C    Geant POSITION : APFH ===> X=0 Y=0 Z=-1*(PSTS_DZ/2. + WIDTHRINGTP +  310
C     WIDTHINNERRINGTP + WIDTHLOWERRINGTP + PITLENGTH*PITFACTOR ) + PITB  310
C    _DZ/2. <===                                                          310
         CALL AGSRESET                                                    310
         AG_X = 0                                                         310
         AG_Y = 0                                                         310
         AG_Z = -1*(PSTS_DZ/2. + WIDTHRINGTP + WIDTHINNERRINGTP +         310
     *   WIDTHLOWERRINGTP + PITLENGTH*PITFACTOR ) + PITB_DZ/2.            310
         AG_TITLE = 'APFH'                                                310
         AG_EXNAME = 'POSITION'                                           310
         AG_PARLIST = 'X_Y_Z'                                             310
         CALL AXPOSITION                                                  310
C    *                                                                    310
C    use PITC bank                                                        310
         BPATH_PITC='PITC*'                                               311
         IDNUM_PITC(1)=0                                                  311
         PITC_VERSION=1.0                                                 311
         CALL AGDGETP('PXSTGEO1','*',-999,'PITC',NPITC_VERSION,           311
     *   RE_PITC_VERSION,BPATH_PITC,IDNUM_PITC,1,MAP_PITC,NAMES_PITC,     311
     *   BANK_PITC,LEN_PITC,LLEN_PITC,LINK_PITC,FLAG_PITC)                311
C    Create APFI :                                                        312
C    Call To Local Procedure APFI                                         312
         ASSIGN 5620 TO BLOCK_APFI                                        312
         GO TO  5610                                                      312
5620     CONTINUE                                                         312
C    *                                                                    312
C    Geant POSITION : APFI ===> X=0 Y=0 Z=-1*(PSTS_DZ/2. + WIDTHRINGTP +  314
C     WIDTHINNERRINGTP + WIDTHLOWERRINGTP + PITLENGTH*PITFACTOR) + PITB_  314
C    DZ + PITC_DZ/2. <===                                                 314
         CALL AGSRESET                                                    314
         AG_X = 0                                                         314
         AG_Y = 0                                                         314
         AG_Z = -1*(PSTS_DZ/2. + WIDTHRINGTP + WIDTHINNERRINGTP +         314
     *   WIDTHLOWERRINGTP + PITLENGTH*PITFACTOR) + PITB_DZ + PITC_DZ/2.   314
         AG_TITLE = 'APFI'                                                314
         AG_EXNAME = 'POSITION'                                           314
         AG_PARLIST = 'X_Y_Z'                                             314
         CALL AXPOSITION                                                  314
C    *                                                                    314
C    use PITR bank                                                        314
         BPATH_PITR='PITR*'                                               315
         IDNUM_PITR(1)=0                                                  315
         PITR_VERSION=1.0                                                 315
         CALL AGDGETP('PXSTGEO1','*',-999,'PITR',NPITR_VERSION,           315
     *   RE_PITR_VERSION,BPATH_PITR,IDNUM_PITR,1,MAP_PITR,NAMES_PITR,     315
     *   BANK_PITR,LEN_PITR,LLEN_PITR,LINK_PITR,FLAG_PITR)                315
         ANGLE = 29.25*(PI/180.0)                                         316
         TEMPOX =(RADRAIL)*COS(ANGLE)                                     317
         TEMPOY =(RADRAIL)*SIN(ANGLE)                                     318
C    Create PITR :                                                        319
C    Call To Local Procedure PITR                                         319
         ASSIGN 5640 TO BLOCK_PITR                                        319
         GO TO  5630                                                      319
5640     CONTINUE                                                         319
C    *                                                                    319
C    Geant POSITION : PITR ===> X=TEMPOX Y=TEMPOY Z=-1*(PSTS_DZ/2. + WID  321
C    THRINGTP + WIDTHINNERRINGTP + WIDTHLOWERRINGTP + PITLENGTH/2.) <===  321
         CALL AGSRESET                                                    321
         AG_X = TEMPOX                                                    321
         AG_Y = TEMPOY                                                    321
         AG_Z = -1*(PSTS_DZ/2. + WIDTHRINGTP + WIDTHINNERRINGTP +         321
     *   WIDTHLOWERRINGTP + PITLENGTH/2.)                                 321
         AG_TITLE = 'PITR'                                                321
         AG_EXNAME = 'POSITION'                                           321
         AG_PARLIST = 'X_Y_Z'                                             321
         CALL AXPOSITION                                                  321
C    *                                                                    321
C    Create PITR :                                                        322
C    Call To Local Procedure PITR                                         322
         ASSIGN 5650 TO BLOCK_PITR                                        322
         GO TO  5630                                                      322
5650     CONTINUE                                                         322
C    *                                                                    322
         TEMPOX =(RADRAIL)*COS( - ANGLE)                                  323
         TEMPOY =(RADRAIL)*SIN( - ANGLE)                                  324
C    Geant POSITION : PITR ===> X=TEMPOX Y=TEMPOY Z=-1*(PSTS_DZ/2. + WID  326
C    THRINGTP + WIDTHINNERRINGTP + WIDTHLOWERRINGTP + PITLENGTH/2.) <===  326
         CALL AGSRESET                                                    326
         AG_X = TEMPOX                                                    326
         AG_Y = TEMPOY                                                    326
         AG_Z = -1*(PSTS_DZ/2. + WIDTHRINGTP + WIDTHINNERRINGTP +         326
     *   WIDTHLOWERRINGTP + PITLENGTH/2.)                                 326
         AG_TITLE = 'PITR'                                                326
         AG_EXNAME = 'POSITION'                                           326
         AG_PARLIST = 'X_Y_Z'                                             326
         CALL AXPOSITION                                                  326
C    *                                                                    326
C    Create PITR :                                                        327
C    Call To Local Procedure PITR                                         327
         ASSIGN 5660 TO BLOCK_PITR                                        327
         GO TO  5630                                                      327
5660     CONTINUE                                                         327
C    *                                                                    327
         TEMPOX =( - 1*RADRAIL)*COS( - ANGLE)                             328
         TEMPOY =( - 1*RADRAIL)*SIN( - ANGLE)                             329
C    Geant POSITION : PITR ===> X=TEMPOX Y=TEMPOY Z=-1*(PSTS_DZ/2. + WID  331
C    THRINGTP + WIDTHINNERRINGTP + WIDTHLOWERRINGTP + PITLENGTH/2.) <===  331
         CALL AGSRESET                                                    331
         AG_X = TEMPOX                                                    331
         AG_Y = TEMPOY                                                    331
         AG_Z = -1*(PSTS_DZ/2. + WIDTHRINGTP + WIDTHINNERRINGTP +         331
     *   WIDTHLOWERRINGTP + PITLENGTH/2.)                                 331
         AG_TITLE = 'PITR'                                                331
         AG_EXNAME = 'POSITION'                                           331
         AG_PARLIST = 'X_Y_Z'                                             331
         CALL AXPOSITION                                                  331
C    *                                                                    331
C    Create PITR :                                                        332
C    Call To Local Procedure PITR                                         332
         ASSIGN 5670 TO BLOCK_PITR                                        332
         GO TO  5630                                                      332
5670     CONTINUE                                                         332
C    *                                                                    332
         TEMPOX =( - 1*RADRAIL)*COS(ANGLE)                                333
         TEMPOY =( - 1*RADRAIL)*SIN(ANGLE)                                334
C    Geant POSITION : PITR ===> X=TEMPOX Y=TEMPOY Z=-1*(PSTS_DZ/2. + WID  336
C    THRINGTP + WIDTHINNERRINGTP + WIDTHLOWERRINGTP + PITLENGTH/2.) <===  336
         CALL AGSRESET                                                    336
         AG_X = TEMPOX                                                    336
         AG_Y = TEMPOY                                                    336
         AG_Z = -1*(PSTS_DZ/2. + WIDTHRINGTP + WIDTHINNERRINGTP +         336
     *   WIDTHLOWERRINGTP + PITLENGTH/2.)                                 336
         AG_TITLE = 'PITR'                                                336
         AG_EXNAME = 'POSITION'                                           336
         AG_PARLIST = 'X_Y_Z'                                             336
         CALL AXPOSITION                                                  336
C    *                                                                    336
5011  CALL AGSPOPB                                                        337
      GO TO BLOCK_PSTM, (5010 ,5020)                                      337
C End of Local Procedure                                                  337
C ----------------------------------------------------------------------  338
C ------------ APTS                                                       338
C Begin Local Procedure APTS                                              339
C APTS A PIXEL TUBE SHELL                                                 339
5030  CONTINUE                                                            339
         AG_TITLE='APTS'                                                  339
         AG_MODULE=MODULE                                                 339
         AG_MARK='V00'                                                    339
         CALL AGSPUSH                                                     339
         AG_MODULE=MODULE                                                 339
C    *                                                                    339
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          340
         CALL AGSRESET                                                    340
         AG_TITLE = 'CFRPMIX'                                             340
         AG_EXNAME = 'MATERIAL'                                           340
         AG_PARLIST = 'NONE'                                              340
         CALL AXMATERIAL                                                  340
C    *                                                                    340
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=2 <===                       340
         CALL AGSRESET                                                    341
         AG_SEEN = 1                                                      341
         AG_COLO = 2                                                      341
         AG_TITLE = 'NONE'                                                341
         AG_EXNAME = 'ATTRIBUTE'                                          341
         AG_PARLIST = 'SEEN_COLO'                                         341
         CALL AXATTRIBUTE                                                 341
C    *                                                                    341
C    Geant SHAPE : TUBE ===> RMIN=PSTS_RMIN/2. RMAX=PSTS_RMAX/2., DZ=PST  341
C    S_DZ/2. <===                                                         341
         CALL AGSRESET                                                    343
         AG_RMIN = PSTS_RMIN/2.                                           343
         AG_RMAX = PSTS_RMAX/2.                                           343
         AG_DZ = PSTS_DZ/2.                                               343
         AG_TITLE = 'TUBE'                                                343
         AG_EXNAME = 'SHAPE'                                              343
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      343
         CALL AXSHAPE                                                     343
C    *                                                                    343
5031  CALL AGSPOPB                                                        344
      GO TO BLOCK_APTS, (5030 ,5040)                                      344
C End of Local Procedure                                                  344
C ----------------------------------------------------------------------  345
C ------------ LFBA                                                       345
C Begin Local Procedure LFBA                                              346
C LFBA A FLANGE BASE                                                      346
5050  CONTINUE                                                            346
         AG_TITLE='LFBA'                                                  346
         AG_MODULE=MODULE                                                 346
         AG_MARK='V00'                                                    346
         CALL AGSPUSH                                                     346
         AG_MODULE=MODULE                                                 346
C    *                                                                    346
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        347
         CALL AGSRESET                                                    347
         AG_TITLE = 'ALUMINIUM'                                           347
         AG_EXNAME = 'MATERIAL'                                           347
         AG_PARLIST = 'NONE'                                              347
         CALL AXMATERIAL                                                  347
C    *                                                                    347
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=3 <===                       347
         CALL AGSRESET                                                    348
         AG_SEEN = 1                                                      348
         AG_COLO = 3                                                      348
         AG_TITLE = 'NONE'                                                348
         AG_EXNAME = 'ATTRIBUTE'                                          348
         AG_PARLIST = 'SEEN_COLO'                                         348
         CALL AXATTRIBUTE                                                 348
C    *                                                                    348
C    Geant SHAPE : TUBE ===> RMIN=SFBA_RMIN/2. RMAX=SFBA_RMAX/2., DZ=SFB  348
C    A_DZ/2. <===                                                         348
         CALL AGSRESET                                                    350
         AG_RMIN = SFBA_RMIN/2.                                           350
         AG_RMAX = SFBA_RMAX/2.                                           350
         AG_DZ = SFBA_DZ/2.                                               350
         AG_TITLE = 'TUBE'                                                350
         AG_EXNAME = 'SHAPE'                                              350
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      350
         CALL AXSHAPE                                                     350
C    *                                                                    350
5051  CALL AGSPOPB                                                        351
      GO TO BLOCK_LFBA, (5050 ,5060)                                      351
C End of Local Procedure                                                  351
C ----------------------------------------------------------------------  352
C ------------ LFBB                                                       352
C Begin Local Procedure LFBB                                              353
C LFBB A FLANGE BASE B                                                    353
5070  CONTINUE                                                            353
         AG_TITLE='LFBB'                                                  353
         AG_MODULE=MODULE                                                 353
         AG_MARK='V00'                                                    353
         CALL AGSPUSH                                                     353
         AG_MODULE=MODULE                                                 353
C    *                                                                    353
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          354
         CALL AGSRESET                                                    354
         AG_TITLE = 'CFRPMIX'                                             354
         AG_EXNAME = 'MATERIAL'                                           354
         AG_PARLIST = 'NONE'                                              354
         CALL AXMATERIAL                                                  354
C    *                                                                    354
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=3 <===                       354
         CALL AGSRESET                                                    355
         AG_SEEN = 1                                                      355
         AG_COLO = 3                                                      355
         AG_TITLE = 'NONE'                                                355
         AG_EXNAME = 'ATTRIBUTE'                                          355
         AG_PARLIST = 'SEEN_COLO'                                         355
         CALL AXATTRIBUTE                                                 355
C    *                                                                    355
C    Geant SHAPE : TUBE ===> RMIN=SFBB_RMIN/2. RMAX=SFBB_RMAX/2., DZ=SFB  355
C    B_DZ/2. <===                                                         355
         CALL AGSRESET                                                    357
         AG_RMIN = SFBB_RMIN/2.                                           357
         AG_RMAX = SFBB_RMAX/2.                                           357
         AG_DZ = SFBB_DZ/2.                                               357
         AG_TITLE = 'TUBE'                                                357
         AG_EXNAME = 'SHAPE'                                              357
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      357
         CALL AXSHAPE                                                     357
C    *                                                                    357
5071  CALL AGSPOPB                                                        358
      GO TO BLOCK_LFBB, (5070 ,5080)                                      358
C End of Local Procedure                                                  358
C ----------------------------------------------------------------------  359
C ------------ LFBK                                                       359
C Begin Local Procedure LFBK                                              360
C LFBK A FLANGE BACKER                                                    360
5090  CONTINUE                                                            360
         AG_TITLE='LFBK'                                                  360
         AG_MODULE=MODULE                                                 360
         AG_MARK='V00'                                                    360
         CALL AGSPUSH                                                     360
         AG_MODULE=MODULE                                                 360
C    *                                                                    360
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        361
         CALL AGSRESET                                                    361
         AG_TITLE = 'ALUMINIUM'                                           361
         AG_EXNAME = 'MATERIAL'                                           361
         AG_PARLIST = 'NONE'                                              361
         CALL AXMATERIAL                                                  361
C    *                                                                    361
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=5 <===                       361
         CALL AGSRESET                                                    362
         AG_SEEN = 1                                                      362
         AG_COLO = 5                                                      362
         AG_TITLE = 'NONE'                                                362
         AG_EXNAME = 'ATTRIBUTE'                                          362
         AG_PARLIST = 'SEEN_COLO'                                         362
         CALL AXATTRIBUTE                                                 362
C    *                                                                    362
C    Geant SHAPE : TUBE ===> RMIN=SFBK_RMIN/2. RMAX=SFBK_RMAX/2., DZ=SFB  362
C    K_DZ/2. <===                                                         362
         CALL AGSRESET                                                    364
         AG_RMIN = SFBK_RMIN/2.                                           364
         AG_RMAX = SFBK_RMAX/2.                                           364
         AG_DZ = SFBK_DZ/2.                                               364
         AG_TITLE = 'TUBE'                                                364
         AG_EXNAME = 'SHAPE'                                              364
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      364
         CALL AXSHAPE                                                     364
C    *                                                                    364
5091  CALL AGSPOPB                                                        365
      GO TO BLOCK_LFBK, (5090 ,5100)                                      365
C End of Local Procedure                                                  365
C ----------------------------------------------------------------------  366
C ------------ RFBA                                                       366
C Begin Local Procedure RFBA                                              367
C RFBA A FLANGE BASE                                                      367
5110  CONTINUE                                                            367
         AG_TITLE='RFBA'                                                  367
         AG_MODULE=MODULE                                                 367
         AG_MARK='V00'                                                    367
         CALL AGSPUSH                                                     367
         AG_MODULE=MODULE                                                 367
C    *                                                                    367
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          368
         CALL AGSRESET                                                    368
         AG_TITLE = 'CFRPMIX'                                             368
         AG_EXNAME = 'MATERIAL'                                           368
         AG_PARLIST = 'NONE'                                              368
         CALL AXMATERIAL                                                  368
C    *                                                                    368
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=3 <===                       368
         CALL AGSRESET                                                    369
         AG_SEEN = 1                                                      369
         AG_COLO = 3                                                      369
         AG_TITLE = 'NONE'                                                369
         AG_EXNAME = 'ATTRIBUTE'                                          369
         AG_PARLIST = 'SEEN_COLO'                                         369
         CALL AXATTRIBUTE                                                 369
C    *                                                                    369
C    Geant SHAPE : TUBE ===> RMIN=SFBA_RMIN/2. RMAX=SFBA_RMAX/2., DZ=SFB  369
C    A_DZ/2. <===                                                         369
         CALL AGSRESET                                                    371
         AG_RMIN = SFBA_RMIN/2.                                           371
         AG_RMAX = SFBA_RMAX/2.                                           371
         AG_DZ = SFBA_DZ/2.                                               371
         AG_TITLE = 'TUBE'                                                371
         AG_EXNAME = 'SHAPE'                                              371
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      371
         CALL AXSHAPE                                                     371
C    *                                                                    371
5111  CALL AGSPOPB                                                        372
      GO TO BLOCK_RFBA, (5110 ,5120)                                      372
C End of Local Procedure                                                  372
C ----------------------------------------------------------------------  373
C ------------ RFBB                                                       373
C Begin Local Procedure RFBB                                              374
C RFBB A FLANGE BASE B                                                    374
5130  CONTINUE                                                            374
         AG_TITLE='RFBB'                                                  374
         AG_MODULE=MODULE                                                 374
         AG_MARK='V00'                                                    374
         CALL AGSPUSH                                                     374
         AG_MODULE=MODULE                                                 374
C    *                                                                    374
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          375
         CALL AGSRESET                                                    375
         AG_TITLE = 'CFRPMIX'                                             375
         AG_EXNAME = 'MATERIAL'                                           375
         AG_PARLIST = 'NONE'                                              375
         CALL AXMATERIAL                                                  375
C    *                                                                    375
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=3 <===                       375
         CALL AGSRESET                                                    376
         AG_SEEN = 1                                                      376
         AG_COLO = 3                                                      376
         AG_TITLE = 'NONE'                                                376
         AG_EXNAME = 'ATTRIBUTE'                                          376
         AG_PARLIST = 'SEEN_COLO'                                         376
         CALL AXATTRIBUTE                                                 376
C    *                                                                    376
C    Geant SHAPE : TUBE ===> RMIN=SFBB_RMIN/2. RMAX=SFBB_RMAX/2., DZ=SFB  376
C    B_DZ/2. <===                                                         376
         CALL AGSRESET                                                    378
         AG_RMIN = SFBB_RMIN/2.                                           378
         AG_RMAX = SFBB_RMAX/2.                                           378
         AG_DZ = SFBB_DZ/2.                                               378
         AG_TITLE = 'TUBE'                                                378
         AG_EXNAME = 'SHAPE'                                              378
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      378
         CALL AXSHAPE                                                     378
C    *                                                                    378
5131  CALL AGSPOPB                                                        379
      GO TO BLOCK_RFBB, (5130 ,5140)                                      379
C End of Local Procedure                                                  379
C ----------------------------------------------------------------------  380
C ------------ RFKK                                                       380
C Begin Local Procedure RFKK                                              381
C RFKK A FLANGE BACKER                                                    381
5150  CONTINUE                                                            381
         AG_TITLE='RFKK'                                                  381
         AG_MODULE=MODULE                                                 381
         AG_MARK='V00'                                                    381
         CALL AGSPUSH                                                     381
         AG_MODULE=MODULE                                                 381
C    *                                                                    381
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          382
         CALL AGSRESET                                                    382
         AG_TITLE = 'CFRPMIX'                                             382
         AG_EXNAME = 'MATERIAL'                                           382
         AG_PARLIST = 'NONE'                                              382
         CALL AXMATERIAL                                                  382
C    *                                                                    382
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=5 <===                       382
         CALL AGSRESET                                                    383
         AG_SEEN = 1                                                      383
         AG_COLO = 5                                                      383
         AG_TITLE = 'NONE'                                                383
         AG_EXNAME = 'ATTRIBUTE'                                          383
         AG_PARLIST = 'SEEN_COLO'                                         383
         CALL AXATTRIBUTE                                                 383
C    *                                                                    383
C    Geant SHAPE : TUBE ===> RMIN=SFBK_RMIN/2. RMAX=SFBK_RMAX/2., DZ=SFB  383
C    K_DZ/2. <===                                                         383
         CALL AGSRESET                                                    385
         AG_RMIN = SFBK_RMIN/2.                                           385
         AG_RMAX = SFBK_RMAX/2.                                           385
         AG_DZ = SFBK_DZ/2.                                               385
         AG_TITLE = 'TUBE'                                                385
         AG_EXNAME = 'SHAPE'                                              385
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      385
         CALL AXSHAPE                                                     385
C    *                                                                    385
5151  CALL AGSPOPB                                                        386
      GO TO BLOCK_RFKK, (5150 ,5160)                                      386
C End of Local Procedure                                                  386
C ----------------------------------------------------------------------  387
C ------------ MOSC                                                       387
C Begin Local Procedure MOSC                                              388
C MOSC A BEAM PIPE RING                                                   388
5190  CONTINUE                                                            388
         AG_TITLE='MOSC'                                                  388
         AG_MODULE=MODULE                                                 388
         AG_MARK='V00'                                                    388
         CALL AGSPUSH                                                     388
         AG_MODULE=MODULE                                                 388
C    *                                                                    388
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          389
         CALL AGSRESET                                                    389
         AG_TITLE = 'CFRPMIX'                                             389
         AG_EXNAME = 'MATERIAL'                                           389
         AG_PARLIST = 'NONE'                                              389
         CALL AXMATERIAL                                                  389
C    *                                                                    389
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=2 <===                       389
         CALL AGSRESET                                                    390
         AG_SEEN = 1                                                      390
         AG_COLO = 2                                                      390
         AG_TITLE = 'NONE'                                                390
         AG_EXNAME = 'ATTRIBUTE'                                          390
         AG_PARLIST = 'SEEN_COLO'                                         390
         CALL AXATTRIBUTE                                                 390
C    *                                                                    390
C    Geant SHAPE : TUBE ===> RMIN=MOSC_RMIN/2. RMAX=MOSC_RMAX/2., DZ=MOS  390
C    C_DZ/2. <===                                                         390
         CALL AGSRESET                                                    392
         AG_RMIN = MOSC_RMIN/2.                                           392
         AG_RMAX = MOSC_RMAX/2.                                           392
         AG_DZ = MOSC_DZ/2.                                               392
         AG_TITLE = 'TUBE'                                                392
         AG_EXNAME = 'SHAPE'                                              392
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      392
         CALL AXSHAPE                                                     392
C    *                                                                    392
5191  CALL AGSPOPB                                                        393
      GO TO BLOCK_MOSC, (5190 ,5200)                                      393
C End of Local Procedure                                                  393
C ----------------------------------------------------------------------  394
C ------------ BPPC                                                       394
C Begin Local Procedure BPPC                                              395
C BPPC BEAM PIPE SUPPORT                                                  395
5170  CONTINUE                                                            395
         AG_TITLE='BPPC'                                                  395
         AG_MODULE=MODULE                                                 395
         AG_MARK='V00'                                                    395
         CALL AGSPUSH                                                     395
         AG_MODULE=MODULE                                                 395
C    *                                                                    395
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          396
         CALL AGSRESET                                                    396
         AG_TITLE = 'CFRPMIX'                                             396
         AG_EXNAME = 'MATERIAL'                                           396
         AG_PARLIST = 'NONE'                                              396
         CALL AXMATERIAL                                                  396
C    *                                                                    396
C    Geant ATTRIBUTE : BPPC ===> SEEN=1 COLO=4 <===                       396
         CALL AGSRESET                                                    397
         AG_SEEN = 1                                                      397
         AG_COLO = 4                                                      397
         AG_TITLE = 'BPPC'                                                397
         AG_EXNAME = 'ATTRIBUTE'                                          397
         AG_PARLIST = 'SEEN_COLO'                                         397
         CALL AXATTRIBUTE                                                 397
C    *                                                                    397
C    Geant SHAPE : TUBE ===> RMIN=BPPC_RMIN RMAX=BPPC_RMAX, DZ=BPPC_DZ/2  397
C    .0 <===                                                              397
         CALL AGSRESET                                                    399
         AG_RMIN = BPPC_RMIN                                              399
         AG_RMAX = BPPC_RMAX                                              399
         AG_DZ = BPPC_DZ/2.0                                              399
         AG_TITLE = 'TUBE'                                                399
         AG_EXNAME = 'SHAPE'                                              399
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      399
         CALL AXSHAPE                                                     399
C    *                                                                    399
5171  CALL AGSPOPB                                                        400
      GO TO BLOCK_BPPC, (5170 ,5180)                                      400
C End of Local Procedure                                                  400
C ----------------------------------------------------------------------  401
C ------------ MTPA                                                       401
C Begin Local Procedure MTPA                                              402
C MTPA MSC TRANSITION PLATE PART A                                        402
5210  CONTINUE                                                            402
         AG_TITLE='MTPA'                                                  402
         AG_MODULE=MODULE                                                 402
         AG_MARK='V00'                                                    402
         CALL AGSPUSH                                                     402
         AG_MODULE=MODULE                                                 402
C    *                                                                    402
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        403
         CALL AGSRESET                                                    403
         AG_TITLE = 'ALUMINIUM'                                           403
         AG_EXNAME = 'MATERIAL'                                           403
         AG_PARLIST = 'NONE'                                              403
         CALL AXMATERIAL                                                  403
C    *                                                                    403
C    Geant ATTRIBUTE : MTPA ===> SEEN=1 COLO=4 <===                       403
         CALL AGSRESET                                                    404
         AG_SEEN = 1                                                      404
         AG_COLO = 4                                                      404
         AG_TITLE = 'MTPA'                                                404
         AG_EXNAME = 'ATTRIBUTE'                                          404
         AG_PARLIST = 'SEEN_COLO'                                         404
         CALL AXATTRIBUTE                                                 404
C    *                                                                    404
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=2 ZI={-1*(PSTS_DZ/2.+WID  404
C    THRINGTP+WIDTHINNERRINGTP+WIDTHLOWERRINGTP), -1*(PSTS_DZ/2.+WIDTHIN  404
C    NERRINGTP+WIDTHLOWERRINGTP)}, RMN={RADINTERATP/2.,RADINTERATP/2.},   404
C    RMX={RADOUTERMAXTP/2.,RADOUTERMAXTP/2.} <===                         404
         CALL AGSRESET                                                    408
         AG_PHI1 = 0                                                      408
         AG_DPHI = 360                                                    408
         AG_NZ = 2                                                        408
         AG_ZI(1)=-1*(PSTS_DZ/                                            408
     *   2.+WIDTHRINGTP+WIDTHINNERRINGTP+WIDTHLOWERRINGTP)                408
         AG_ZI(2)=-1*(PSTS_DZ/2.+WIDTHINNERRINGTP+WIDTHLOWERRINGTP)       408
         AG_RMN(1)=RADINTERATP/2.                                         408
         AG_RMN(2)=RADINTERATP/2.                                         408
         AG_RMX(1)=RADOUTERMAXTP/2.                                       408
         AG_RMX(2)=RADOUTERMAXTP/2.                                       408
         AG_TITLE = 'PCON'                                                408
         AG_EXNAME = 'SHAPE'                                              408
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           408
         CALL AXSHAPE                                                     408
C    *                                                                    408
5211  CALL AGSPOPB                                                        409
      GO TO BLOCK_MTPA, (5210 ,5220)                                      409
C End of Local Procedure                                                  409
C ----------------------------------------------------------------------  410
C ------------ MTPB                                                       410
C Begin Local Procedure MTPB                                              411
C MTPB MSC TRANSITION PLATE PART B                                        411
5230  CONTINUE                                                            411
         AG_TITLE='MTPB'                                                  411
         AG_MODULE=MODULE                                                 411
         AG_MARK='V00'                                                    411
         CALL AGSPUSH                                                     411
         AG_MODULE=MODULE                                                 411
C    *                                                                    411
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        412
         CALL AGSRESET                                                    412
         AG_TITLE = 'ALUMINIUM'                                           412
         AG_EXNAME = 'MATERIAL'                                           412
         AG_PARLIST = 'NONE'                                              412
         CALL AXMATERIAL                                                  412
C    *                                                                    412
C    Geant ATTRIBUTE : MTPB ===> SEEN=1 COLO=2 <===                       412
         CALL AGSRESET                                                    413
         AG_SEEN = 1                                                      413
         AG_COLO = 2                                                      413
         AG_TITLE = 'MTPB'                                                413
         AG_EXNAME = 'ATTRIBUTE'                                          413
         AG_PARLIST = 'SEEN_COLO'                                         413
         CALL AXATTRIBUTE                                                 413
C    *                                                                    413
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={-1*(PSTS_DZ/2.+WID  413
C    THRINGTP+WIDTHINNERRINGTP+WIDTHLOWERRINGTP), -1*(PSTS_DZ/2.+WIDTHIN  413
C    NERRINGTP+WIDTHLOWERRINGTP), -1*(PSTS_DZ/2.+WIDTHLOWERRINGTP)}, RMN  413
C    ={RADOUTERMINTP/2.,RADOUTERMINTP/2., RADINNERMAXTP/2.-TP} RMX={RADI  413
C    NTERATP/2.,RADINTERATP/2., RADINNERMAXTP/2.} <===                    413
         CALL AGSRESET                                                    419
         AG_PHI1 = 0                                                      419
         AG_DPHI = 360                                                    419
         AG_NZ = 3                                                        419
         AG_ZI(1)=-1*(PSTS_DZ/                                            419
     *   2.+WIDTHRINGTP+WIDTHINNERRINGTP+WIDTHLOWERRINGTP)                419
         AG_ZI(2)=-1*(PSTS_DZ/2.+WIDTHINNERRINGTP+WIDTHLOWERRINGTP)       419
         AG_ZI(3)=-1*(PSTS_DZ/2.+WIDTHLOWERRINGTP)                        419
         AG_RMN(1)=RADOUTERMINTP/2.                                       419
         AG_RMN(2)=RADOUTERMINTP/2.                                       419
         AG_RMN(3)=RADINNERMAXTP/2.-TP                                    419
         AG_RMX(1)=RADINTERATP/2.                                         419
         AG_RMX(2)=RADINTERATP/2.                                         419
         AG_RMX(3)=RADINNERMAXTP/2.                                       419
         AG_TITLE = 'PCON'                                                419
         AG_EXNAME = 'SHAPE'                                              419
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           419
         CALL AXSHAPE                                                     419
C    *                                                                    419
5231  CALL AGSPOPB                                                        420
      GO TO BLOCK_MTPB, (5230 ,5240)                                      420
C End of Local Procedure                                                  420
C ----------------------------------------------------------------------  421
C ------------ MTPC                                                       421
C Begin Local Procedure MTPC                                              422
C MTPC MSC TRANSITION PLATE PART C                                        422
5250  CONTINUE                                                            422
         AG_TITLE='MTPC'                                                  422
         AG_MODULE=MODULE                                                 422
         AG_MARK='V00'                                                    422
         CALL AGSPUSH                                                     422
         AG_MODULE=MODULE                                                 422
C    *                                                                    422
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        423
         CALL AGSRESET                                                    423
         AG_TITLE = 'ALUMINIUM'                                           423
         AG_EXNAME = 'MATERIAL'                                           423
         AG_PARLIST = 'NONE'                                              423
         CALL AXMATERIAL                                                  423
C    *                                                                    423
C    Geant ATTRIBUTE : MTPC ===> SEEN=1 COLO=7 <===                       423
         CALL AGSRESET                                                    424
         AG_SEEN = 1                                                      424
         AG_COLO = 7                                                      424
         AG_TITLE = 'MTPC'                                                424
         AG_EXNAME = 'ATTRIBUTE'                                          424
         AG_PARLIST = 'SEEN_COLO'                                         424
         CALL AXATTRIBUTE                                                 424
C    *                                                                    424
C    Geant SHAPE : CONE ===> DZ=WIDTHRINGTP/2. RMN1=RADINNERMINTP/2., RM  424
C    X1=RADINNERMINTP/2.+LENGTHINSIDERING, RMN2=RADINNERMINTP/2. RMX2=RA  424
C    DINNERMAXTP/2.-TP <===                                               424
         CALL AGSRESET                                                    427
         AG_DZ = WIDTHRINGTP/2.                                           427
         AG_RMN1 = RADINNERMINTP/2.                                       427
         AG_RMX1 = RADINNERMINTP/2.+LENGTHINSIDERING                      427
         AG_RMN2 = RADINNERMINTP/2.                                       427
         AG_RMX2 = RADINNERMAXTP/2.-TP                                    427
         AG_TITLE = 'CONE'                                                427
         AG_EXNAME = 'SHAPE'                                              427
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            427
         CALL AXSHAPE                                                     427
C    *                                                                    427
5251  CALL AGSPOPB                                                        428
      GO TO BLOCK_MTPC, (5250 ,5260)                                      428
C End of Local Procedure                                                  428
C ----------------------------------------------------------------------  429
C ------------ MTPD                                                       429
C Begin Local Procedure MTPD                                              430
C MTPD MSC TRANSITION PLATE PART D                                        430
5270  CONTINUE                                                            430
         AG_TITLE='MTPD'                                                  430
         AG_MODULE=MODULE                                                 430
         AG_MARK='V00'                                                    430
         CALL AGSPUSH                                                     430
         AG_MODULE=MODULE                                                 430
C    *                                                                    430
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        431
         CALL AGSRESET                                                    431
         AG_TITLE = 'ALUMINIUM'                                           431
         AG_EXNAME = 'MATERIAL'                                           431
         AG_PARLIST = 'NONE'                                              431
         CALL AXMATERIAL                                                  431
C    *                                                                    431
C    Geant ATTRIBUTE : MTPD ===> SEEN=1 COLO=2 <===                       431
         CALL AGSRESET                                                    432
         AG_SEEN = 1                                                      432
         AG_COLO = 2                                                      432
         AG_TITLE = 'MTPD'                                                432
         AG_EXNAME = 'ATTRIBUTE'                                          432
         AG_PARLIST = 'SEEN_COLO'                                         432
         CALL AXATTRIBUTE                                                 432
C    *                                                                    432
C    Geant SHAPE : PCON ===> PHI1=0 DPHI=360 NZ=3 ZI={-PSTS_DZ/2.-WIDTHL  432
C    OWERRINGTP-WIDTHRINGTP, -PSTS_DZ/2.-WIDTHLOWERRINGTP, -PSTS_DZ/2.}   432
C    RMN={RADINNERMINTP/2., RADINNERMINTP/2.-TP,RADOUTERFBACKER/2-TP}, R  432
C    MX={RADINNERMINTP/2.,RADINNERMINTP/2., RADOUTERFBACKER/2.} <===      432
         CALL AGSRESET                                                    438
         AG_PHI1 = 0                                                      438
         AG_DPHI = 360                                                    438
         AG_NZ = 3                                                        438
         AG_ZI(1)=-PSTS_DZ/2.-WIDTHLOWERRINGTP-WIDTHRINGTP                438
         AG_ZI(2)=-PSTS_DZ/2.-WIDTHLOWERRINGTP                            438
         AG_ZI(3)=-PSTS_DZ/2.                                             438
         AG_RMN(1)=RADINNERMINTP/2.                                       438
         AG_RMN(2)=RADINNERMINTP/2.-TP                                    438
         AG_RMN(3)=RADOUTERFBACKER/2-TP                                   438
         AG_RMX(1)=RADINNERMINTP/2.                                       438
         AG_RMX(2)=RADINNERMINTP/2.                                       438
         AG_RMX(3)=RADOUTERFBACKER/2.                                     438
         AG_TITLE = 'PCON'                                                438
         AG_EXNAME = 'SHAPE'                                              438
         AG_PARLIST = 'PHI1_DPHI_NZ_ZI_RMN_RMX'                           438
         CALL AXSHAPE                                                     438
C    *                                                                    438
5271  CALL AGSPOPB                                                        439
      GO TO BLOCK_MTPD, (5270 ,5280)                                      439
C End of Local Procedure                                                  439
C ----------------------------------------------------------------------  440
C ------------ MTPE                                                       440
C Begin Local Procedure MTPE                                              441
C MTPE MSC TRANSITION PLATE PART E                                        441
5290  CONTINUE                                                            441
         AG_TITLE='MTPE'                                                  441
         AG_MODULE=MODULE                                                 441
         AG_MARK='V00'                                                    441
         CALL AGSPUSH                                                     441
         AG_MODULE=MODULE                                                 441
C    *                                                                    441
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        442
         CALL AGSRESET                                                    442
         AG_TITLE = 'ALUMINIUM'                                           442
         AG_EXNAME = 'MATERIAL'                                           442
         AG_PARLIST = 'NONE'                                              442
         CALL AXMATERIAL                                                  442
C    *                                                                    442
C    Geant ATTRIBUTE : MTPE ===> SEEN=1 COLO=3 <===                       442
         CALL AGSRESET                                                    443
         AG_SEEN = 1                                                      443
         AG_COLO = 3                                                      443
         AG_TITLE = 'MTPE'                                                443
         AG_EXNAME = 'ATTRIBUTE'                                          443
         AG_PARLIST = 'SEEN_COLO'                                         443
         CALL AXATTRIBUTE                                                 443
C    *                                                                    443
C    Geant SHAPE : CONE ===> DZ=WIDTHRINGTP/2. RMN1=RADINNERSHELL/2., RM  443
C    X1=RADOUTERFBACKER/2. RMN2=RADINNERSHELL/2., RMX2=RADOUTERFBACKER/2  443
C    .-TP <===                                                            443
         CALL AGSRESET                                                    446
         AG_DZ = WIDTHRINGTP/2.                                           446
         AG_RMN1 = RADINNERSHELL/2.                                       446
         AG_RMX1 = RADOUTERFBACKER/2.                                     446
         AG_RMN2 = RADINNERSHELL/2.                                       446
         AG_RMX2 = RADOUTERFBACKER/2.-TP                                  446
         AG_TITLE = 'CONE'                                                446
         AG_EXNAME = 'SHAPE'                                              446
         AG_PARLIST = 'DZ_RMN1_RMX1_RMN2_RMX2'                            446
         CALL AXSHAPE                                                     446
C    *                                                                    446
5291  CALL AGSPOPB                                                        447
      GO TO BLOCK_MTPE, (5290 ,5300)                                      447
C End of Local Procedure                                                  447
C ----------------------------------------------------------------------  448
C ------------ BPRS                                                       448
C Begin Local Procedure BPRS                                              449
C BPRS THE RING                                                           449
5310  CONTINUE                                                            449
         AG_TITLE='BPRS'                                                  449
         AG_MODULE=MODULE                                                 449
         AG_MARK='V00'                                                    449
         CALL AGSPUSH                                                     449
         AG_MODULE=MODULE                                                 449
C    *                                                                    449
C    Geant MATERIAL : POLYIMIDE ===> inherited arguments used <===        450
         CALL AGSRESET                                                    450
         AG_TITLE = 'POLYIMIDE'                                           450
         AG_EXNAME = 'MATERIAL'                                           450
         AG_PARLIST = 'NONE'                                              450
         CALL AXMATERIAL                                                  450
C    *                                                                    450
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=6 <===                       450
         CALL AGSRESET                                                    451
         AG_SEEN = 1                                                      451
         AG_COLO = 6                                                      451
         AG_TITLE = 'NONE'                                                451
         AG_EXNAME = 'ATTRIBUTE'                                          451
         AG_PARLIST = 'SEEN_COLO'                                         451
         CALL AXATTRIBUTE                                                 451
C    *                                                                    451
C    Geant SHAPE : TUBE ===> RMIN=RINGDIAMETERMIN/2. RMAX=RINGDIAMETERMA  451
C    X/2., DZ=RINGTHK/2. <===                                             451
         CALL AGSRESET                                                    453
         AG_RMIN = RINGDIAMETERMIN/2.                                     453
         AG_RMAX = RINGDIAMETERMAX/2.                                     453
         AG_DZ = RINGTHK/2.                                               453
         AG_TITLE = 'TUBE'                                                453
         AG_EXNAME = 'SHAPE'                                              453
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      453
         CALL AXSHAPE                                                     453
C    *                                                                    453
5311  CALL AGSPOPB                                                        454
      GO TO BLOCK_BPRS, (5310 ,5320,5330,5340)                            454
C End of Local Procedure                                                  454
C ----------------------------------------------------------------------  455
C ------------ MTPF                                                       455
C Begin Local Procedure MTPF                                              456
C MTPF VERTICAL SUPPORT OF THE INNER RINGS                                456
5350  CONTINUE                                                            456
         AG_TITLE='MTPF'                                                  456
         AG_MODULE=MODULE                                                 456
         AG_MARK='V00'                                                    456
         CALL AGSPUSH                                                     456
         AG_MODULE=MODULE                                                 456
C    *                                                                    456
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          457
         CALL AGSRESET                                                    457
         AG_TITLE = 'CFRPMIX'                                             457
         AG_EXNAME = 'MATERIAL'                                           457
         AG_PARLIST = 'NONE'                                              457
         CALL AXMATERIAL                                                  457
C    *                                                                    457
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=4 <===                       457
         CALL AGSRESET                                                    458
         AG_SEEN = 1                                                      458
         AG_COLO = 4                                                      458
         AG_TITLE = 'NONE'                                                458
         AG_EXNAME = 'ATTRIBUTE'                                          458
         AG_PARLIST = 'SEEN_COLO'                                         458
         CALL AXATTRIBUTE                                                 458
C    *                                                                    458
C    Geant SHAPE : BOX ===> DX=.2 DY=(RADOUTERMINTP/2.-RINGDIAMETERMAX/2  458
C    .)/2., DZ=WIDTHRINGTP/2. <===                                        458
         CALL AGSRESET                                                    460
         AG_DX = .2                                                       460
         AG_DY = (RADOUTERMINTP/2.-RINGDIAMETERMAX/2.)/2.                 460
         AG_DZ = WIDTHRINGTP/2.                                           460
         AG_TITLE = 'BOX'                                                 460
         AG_EXNAME = 'SHAPE'                                              460
         AG_PARLIST = 'DX_DY_DZ'                                          460
         CALL AXSHAPE                                                     460
C    *                                                                    460
5351  CALL AGSPOPB                                                        461
      GO TO BLOCK_MTPF, (5350 ,5360,5370)                                 461
C End of Local Procedure                                                  461
C ----------------------------------------------------------------------  462
C ------------ MTPG                                                       462
C Begin Local Procedure MTPG                                              463
C MTPG VERTICAL SUPPORT OF THE INNER RINGS                                463
5380  CONTINUE                                                            463
         AG_TITLE='MTPG'                                                  463
         AG_MODULE=MODULE                                                 463
         AG_MARK='V00'                                                    463
         CALL AGSPUSH                                                     463
         AG_MODULE=MODULE                                                 463
C    *                                                                    463
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          464
         CALL AGSRESET                                                    464
         AG_TITLE = 'CFRPMIX'                                             464
         AG_EXNAME = 'MATERIAL'                                           464
         AG_PARLIST = 'NONE'                                              464
         CALL AXMATERIAL                                                  464
C    *                                                                    464
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=4 <===                       464
         CALL AGSRESET                                                    465
         AG_SEEN = 1                                                      465
         AG_COLO = 4                                                      465
         AG_TITLE = 'NONE'                                                465
         AG_EXNAME = 'ATTRIBUTE'                                          465
         AG_PARLIST = 'SEEN_COLO'                                         465
         CALL AXATTRIBUTE                                                 465
C    *                                                                    465
C    Geant SHAPE : BOX ===> DX=.2 DY=(RADINNERSHELL/2.-RINGDIAMETERMAX/2  465
C    .)/2., DZ=(WIDTHINNERRINGTP+WIDTHLOWERRINGTP)/2. <===                465
         CALL AGSRESET                                                    467
         AG_DX = .2                                                       467
         AG_DY = (RADINNERSHELL/2.-RINGDIAMETERMAX/2.)/2.                 467
         AG_DZ = (WIDTHINNERRINGTP+WIDTHLOWERRINGTP)/2.                   467
         AG_TITLE = 'BOX'                                                 467
         AG_EXNAME = 'SHAPE'                                              467
         AG_PARLIST = 'DX_DY_DZ'                                          467
         CALL AXSHAPE                                                     467
C    *                                                                    467
5381  CALL AGSPOPB                                                        468
      GO TO BLOCK_MTPG, (5380 ,5390,5400)                                 468
C End of Local Procedure                                                  468
C ----------------------------------------------------------------------  469
C ------------ MTPH                                                       469
C Begin Local Procedure MTPH                                              470
C MTPH OSC - MSC SUPPORT RING                                             470
5410  CONTINUE                                                            470
         AG_TITLE='MTPH'                                                  470
         AG_MODULE=MODULE                                                 470
         AG_MARK='V00'                                                    470
         CALL AGSPUSH                                                     470
         AG_MODULE=MODULE                                                 470
C    *                                                                    470
C    Geant MATERIAL : ALUMINIUM ===> inherited arguments used <===        471
         CALL AGSRESET                                                    471
         AG_TITLE = 'ALUMINIUM'                                           471
         AG_EXNAME = 'MATERIAL'                                           471
         AG_PARLIST = 'NONE'                                              471
         CALL AXMATERIAL                                                  471
C    *                                                                    471
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=4 <===                       471
         CALL AGSRESET                                                    472
         AG_SEEN = 1                                                      472
         AG_COLO = 4                                                      472
         AG_TITLE = 'NONE'                                                472
         AG_EXNAME = 'ATTRIBUTE'                                          472
         AG_PARLIST = 'SEEN_COLO'                                         472
         CALL AXATTRIBUTE                                                 472
C    *                                                                    472
C    Geant SHAPE : TUBE ===> RMIN=20.3 RMAX=22.70 DZ=0.85/2.0 <===        472
         CALL AGSRESET                                                    473
         AG_RMIN = 20.3                                                   473
         AG_RMAX = 22.70                                                  473
         AG_DZ = 0.85/2.0                                                 473
         AG_TITLE = 'TUBE'                                                473
         AG_EXNAME = 'SHAPE'                                              473
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      473
         CALL AXSHAPE                                                     473
C    *                                                                    473
5411  CALL AGSPOPB                                                        474
      GO TO BLOCK_MTPH, (5410 ,5420)                                      474
C End of Local Procedure                                                  474
C ----------------------------------------------------------------------  475
C ------------ PITN                                                       475
C Begin Local Procedure PITN                                              476
C PITN PIXEL INSERTION TUBE                                               476
5430  CONTINUE                                                            476
         AG_TITLE='PITN'                                                  476
         AG_MODULE=MODULE                                                 476
         AG_MARK='V00'                                                    476
         CALL AGSPUSH                                                     476
         AG_MODULE=MODULE                                                 476
C    *                                                                    476
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          477
         CALL AGSRESET                                                    477
         AG_TITLE = 'CFRPMIX'                                             477
         AG_EXNAME = 'MATERIAL'                                           477
         AG_PARLIST = 'NONE'                                              477
         CALL AXMATERIAL                                                  477
C    *                                                                    477
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=6 <===                       477
         CALL AGSRESET                                                    478
         AG_SEEN = 1                                                      478
         AG_COLO = 6                                                      478
         AG_TITLE = 'NONE'                                                478
         AG_EXNAME = 'ATTRIBUTE'                                          478
         AG_PARLIST = 'SEEN_COLO'                                         478
         CALL AXATTRIBUTE                                                 478
C    *                                                                    478
C    Geant SHAPE : TUBE ===> RMIN=PITRINNER/2. RMAX=PITROUTER/2., DZ=PIT  478
C    LENGTH/2. <===                                                       478
         CALL AGSRESET                                                    480
         AG_RMIN = PITRINNER/2.                                           480
         AG_RMAX = PITROUTER/2.                                           480
         AG_DZ = PITLENGTH/2.                                             480
         AG_TITLE = 'TUBE'                                                480
         AG_EXNAME = 'SHAPE'                                              480
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      480
         CALL AXSHAPE                                                     480
C    *                                                                    480
5431  CALL AGSPOPB                                                        481
      GO TO BLOCK_PITN, (5430 ,5440)                                      481
C End of Local Procedure                                                  481
C ----------------------------------------------------------------------  482
C ------------ APFA                                                       482
C Begin Local Procedure APFA                                              483
C APFA A PIT FLANGE BASE A                                                483
5450  CONTINUE                                                            483
         AG_TITLE='APFA'                                                  483
         AG_MODULE=MODULE                                                 483
         AG_MARK='V00'                                                    483
         CALL AGSPUSH                                                     483
         AG_MODULE=MODULE                                                 483
C    *                                                                    483
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          484
         CALL AGSRESET                                                    484
         AG_TITLE = 'CFRPMIX'                                             484
         AG_EXNAME = 'MATERIAL'                                           484
         AG_PARLIST = 'NONE'                                              484
         CALL AXMATERIAL                                                  484
C    *                                                                    484
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=3 <===                       484
         CALL AGSRESET                                                    485
         AG_SEEN = 1                                                      485
         AG_COLO = 3                                                      485
         AG_TITLE = 'NONE'                                                485
         AG_EXNAME = 'ATTRIBUTE'                                          485
         AG_PARLIST = 'SEEN_COLO'                                         485
         CALL AXATTRIBUTE                                                 485
C    *                                                                    485
C    Geant SHAPE : TUBE ===> RMIN=PITA_RMIN/2. RMAX=PITA_RMAX/2., DZ=PIT  485
C    A_DZ/2. <===                                                         485
         CALL AGSRESET                                                    487
         AG_RMIN = PITA_RMIN/2.                                           487
         AG_RMAX = PITA_RMAX/2.                                           487
         AG_DZ = PITA_DZ/2.                                               487
         AG_TITLE = 'TUBE'                                                487
         AG_EXNAME = 'SHAPE'                                              487
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      487
         CALL AXSHAPE                                                     487
C    *                                                                    487
5451  CALL AGSPOPB                                                        488
      GO TO BLOCK_APFA, (5450 ,5460)                                      488
C End of Local Procedure                                                  488
C ----------------------------------------------------------------------  489
C ------------ APFB                                                       489
C Begin Local Procedure APFB                                              490
C APFB A PIT FLANGE BASE B                                                490
5470  CONTINUE                                                            490
         AG_TITLE='APFB'                                                  490
         AG_MODULE=MODULE                                                 490
         AG_MARK='V00'                                                    490
         CALL AGSPUSH                                                     490
         AG_MODULE=MODULE                                                 490
C    *                                                                    490
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          491
         CALL AGSRESET                                                    491
         AG_TITLE = 'CFRPMIX'                                             491
         AG_EXNAME = 'MATERIAL'                                           491
         AG_PARLIST = 'NONE'                                              491
         CALL AXMATERIAL                                                  491
C    *                                                                    491
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=4 <===                       491
         CALL AGSRESET                                                    492
         AG_SEEN = 1                                                      492
         AG_COLO = 4                                                      492
         AG_TITLE = 'NONE'                                                492
         AG_EXNAME = 'ATTRIBUTE'                                          492
         AG_PARLIST = 'SEEN_COLO'                                         492
         CALL AXATTRIBUTE                                                 492
C    *                                                                    492
C    Geant SHAPE : TUBE ===> RMIN=PITB_RMIN/2. RMAX=PITB_RMAX/2., DZ=PIT  492
C    B_DZ/2. <===                                                         492
         CALL AGSRESET                                                    494
         AG_RMIN = PITB_RMIN/2.                                           494
         AG_RMAX = PITB_RMAX/2.                                           494
         AG_DZ = PITB_DZ/2.                                               494
         AG_TITLE = 'TUBE'                                                494
         AG_EXNAME = 'SHAPE'                                              494
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      494
         CALL AXSHAPE                                                     494
C    *                                                                    494
5471  CALL AGSPOPB                                                        495
      GO TO BLOCK_APFB, (5470 ,5480)                                      495
C End of Local Procedure                                                  495
C ----------------------------------------------------------------------  496
C ------------ APFC                                                       496
C Begin Local Procedure APFC                                              497
C APFC A PIT FLANGE BACKER C                                              497
5490  CONTINUE                                                            497
         AG_TITLE='APFC'                                                  497
         AG_MODULE=MODULE                                                 497
         AG_MARK='V00'                                                    497
         CALL AGSPUSH                                                     497
         AG_MODULE=MODULE                                                 497
C    *                                                                    497
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          498
         CALL AGSRESET                                                    498
         AG_TITLE = 'CFRPMIX'                                             498
         AG_EXNAME = 'MATERIAL'                                           498
         AG_PARLIST = 'NONE'                                              498
         CALL AXMATERIAL                                                  498
C    *                                                                    498
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=5 <===                       498
         CALL AGSRESET                                                    499
         AG_SEEN = 1                                                      499
         AG_COLO = 5                                                      499
         AG_TITLE = 'NONE'                                                499
         AG_EXNAME = 'ATTRIBUTE'                                          499
         AG_PARLIST = 'SEEN_COLO'                                         499
         CALL AXATTRIBUTE                                                 499
C    *                                                                    499
C    Geant SHAPE : TUBE ===> RMIN=PITC_RMIN/2. RMAX=PITC_RMAX/2., DZ=PIT  499
C    C_DZ/2. <===                                                         499
         CALL AGSRESET                                                    501
         AG_RMIN = PITC_RMIN/2.                                           501
         AG_RMAX = PITC_RMAX/2.                                           501
         AG_DZ = PITC_DZ/2.                                               501
         AG_TITLE = 'TUBE'                                                501
         AG_EXNAME = 'SHAPE'                                              501
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      501
         CALL AXSHAPE                                                     501
C    *                                                                    501
5491  CALL AGSPOPB                                                        502
      GO TO BLOCK_APFC, (5490 ,5500)                                      502
C End of Local Procedure                                                  502
C ----------------------------------------------------------------------  503
C ------------ APFD                                                       503
C Begin Local Procedure APFD                                              504
C APFD A PIT FLANGE BASE D                                                504
5510  CONTINUE                                                            504
         AG_TITLE='APFD'                                                  504
         AG_MODULE=MODULE                                                 504
         AG_MARK='V00'                                                    504
         CALL AGSPUSH                                                     504
         AG_MODULE=MODULE                                                 504
C    *                                                                    504
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          505
         CALL AGSRESET                                                    505
         AG_TITLE = 'CFRPMIX'                                             505
         AG_EXNAME = 'MATERIAL'                                           505
         AG_PARLIST = 'NONE'                                              505
         CALL AXMATERIAL                                                  505
C    *                                                                    505
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=3 <===                       505
         CALL AGSRESET                                                    506
         AG_SEEN = 1                                                      506
         AG_COLO = 3                                                      506
         AG_TITLE = 'NONE'                                                506
         AG_EXNAME = 'ATTRIBUTE'                                          506
         AG_PARLIST = 'SEEN_COLO'                                         506
         CALL AXATTRIBUTE                                                 506
C    *                                                                    506
C    Geant SHAPE : TUBE ===> RMIN=PITA_RMIN/2. RMAX=PITA_RMAX/2., DZ=PIT  506
C    A_DZ/2. <===                                                         506
         CALL AGSRESET                                                    508
         AG_RMIN = PITA_RMIN/2.                                           508
         AG_RMAX = PITA_RMAX/2.                                           508
         AG_DZ = PITA_DZ/2.                                               508
         AG_TITLE = 'TUBE'                                                508
         AG_EXNAME = 'SHAPE'                                              508
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      508
         CALL AXSHAPE                                                     508
C    *                                                                    508
5511  CALL AGSPOPB                                                        509
      GO TO BLOCK_APFD, (5510 ,5520)                                      509
C End of Local Procedure                                                  509
C ----------------------------------------------------------------------  510
C ------------ APFE                                                       510
C Begin Local Procedure APFE                                              511
C APFE A PIT FLANGE BASE E                                                511
5530  CONTINUE                                                            511
         AG_TITLE='APFE'                                                  511
         AG_MODULE=MODULE                                                 511
         AG_MARK='V00'                                                    511
         CALL AGSPUSH                                                     511
         AG_MODULE=MODULE                                                 511
C    *                                                                    511
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          512
         CALL AGSRESET                                                    512
         AG_TITLE = 'CFRPMIX'                                             512
         AG_EXNAME = 'MATERIAL'                                           512
         AG_PARLIST = 'NONE'                                              512
         CALL AXMATERIAL                                                  512
C    *                                                                    512
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=4 <===                       512
         CALL AGSRESET                                                    513
         AG_SEEN = 1                                                      513
         AG_COLO = 4                                                      513
         AG_TITLE = 'NONE'                                                513
         AG_EXNAME = 'ATTRIBUTE'                                          513
         AG_PARLIST = 'SEEN_COLO'                                         513
         CALL AXATTRIBUTE                                                 513
C    *                                                                    513
C    Geant SHAPE : TUBE ===> RMIN=PITB_RMIN/2. RMAX=PITB_RMAX/2., DZ=PIT  513
C    B_DZ/2. <===                                                         513
         CALL AGSRESET                                                    515
         AG_RMIN = PITB_RMIN/2.                                           515
         AG_RMAX = PITB_RMAX/2.                                           515
         AG_DZ = PITB_DZ/2.                                               515
         AG_TITLE = 'TUBE'                                                515
         AG_EXNAME = 'SHAPE'                                              515
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      515
         CALL AXSHAPE                                                     515
C    *                                                                    515
5531  CALL AGSPOPB                                                        516
      GO TO BLOCK_APFE, (5530 ,5540)                                      516
C End of Local Procedure                                                  516
C ----------------------------------------------------------------------  517
C ------------ APFF                                                       517
C Begin Local Procedure APFF                                              518
C APFF A PIT FLANGE BACKER F                                              518
5550  CONTINUE                                                            518
         AG_TITLE='APFF'                                                  518
         AG_MODULE=MODULE                                                 518
         AG_MARK='V00'                                                    518
         CALL AGSPUSH                                                     518
         AG_MODULE=MODULE                                                 518
C    *                                                                    518
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          519
         CALL AGSRESET                                                    519
         AG_TITLE = 'CFRPMIX'                                             519
         AG_EXNAME = 'MATERIAL'                                           519
         AG_PARLIST = 'NONE'                                              519
         CALL AXMATERIAL                                                  519
C    *                                                                    519
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=5 <===                       519
         CALL AGSRESET                                                    520
         AG_SEEN = 1                                                      520
         AG_COLO = 5                                                      520
         AG_TITLE = 'NONE'                                                520
         AG_EXNAME = 'ATTRIBUTE'                                          520
         AG_PARLIST = 'SEEN_COLO'                                         520
         CALL AXATTRIBUTE                                                 520
C    *                                                                    520
C    Geant SHAPE : TUBE ===> RMIN=PITC_RMIN/2. RMAX=PITC_RMAX/2., DZ=PIT  520
C    C_DZ/2. <===                                                         520
         CALL AGSRESET                                                    522
         AG_RMIN = PITC_RMIN/2.                                           522
         AG_RMAX = PITC_RMAX/2.                                           522
         AG_DZ = PITC_DZ/2.                                               522
         AG_TITLE = 'TUBE'                                                522
         AG_EXNAME = 'SHAPE'                                              522
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      522
         CALL AXSHAPE                                                     522
C    *                                                                    522
5551  CALL AGSPOPB                                                        523
      GO TO BLOCK_APFF, (5550 ,5560)                                      523
C End of Local Procedure                                                  523
C ----------------------------------------------------------------------  524
C ------------ APFG                                                       524
C Begin Local Procedure APFG                                              525
C APFG A PIT FLANGE BASE G                                                525
5570  CONTINUE                                                            525
         AG_TITLE='APFG'                                                  525
         AG_MODULE=MODULE                                                 525
         AG_MARK='V00'                                                    525
         CALL AGSPUSH                                                     525
         AG_MODULE=MODULE                                                 525
C    *                                                                    525
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          526
         CALL AGSRESET                                                    526
         AG_TITLE = 'CFRPMIX'                                             526
         AG_EXNAME = 'MATERIAL'                                           526
         AG_PARLIST = 'NONE'                                              526
         CALL AXMATERIAL                                                  526
C    *                                                                    526
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=3 <===                       526
         CALL AGSRESET                                                    527
         AG_SEEN = 1                                                      527
         AG_COLO = 3                                                      527
         AG_TITLE = 'NONE'                                                527
         AG_EXNAME = 'ATTRIBUTE'                                          527
         AG_PARLIST = 'SEEN_COLO'                                         527
         CALL AXATTRIBUTE                                                 527
C    *                                                                    527
C    Geant SHAPE : TUBE ===> RMIN=PITA_RMIN/2. RMAX=PITA_RMAX/2., DZ=PIT  527
C    A_DZ/2. <===                                                         527
         CALL AGSRESET                                                    529
         AG_RMIN = PITA_RMIN/2.                                           529
         AG_RMAX = PITA_RMAX/2.                                           529
         AG_DZ = PITA_DZ/2.                                               529
         AG_TITLE = 'TUBE'                                                529
         AG_EXNAME = 'SHAPE'                                              529
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      529
         CALL AXSHAPE                                                     529
C    *                                                                    529
5571  CALL AGSPOPB                                                        530
      GO TO BLOCK_APFG, (5570 ,5580)                                      530
C End of Local Procedure                                                  530
C ----------------------------------------------------------------------  531
C ------------ APFH                                                       531
C Begin Local Procedure APFH                                              532
C APFH A PIT FLANGE BASE H                                                532
5590  CONTINUE                                                            532
         AG_TITLE='APFH'                                                  532
         AG_MODULE=MODULE                                                 532
         AG_MARK='V00'                                                    532
         CALL AGSPUSH                                                     532
         AG_MODULE=MODULE                                                 532
C    *                                                                    532
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          533
         CALL AGSRESET                                                    533
         AG_TITLE = 'CFRPMIX'                                             533
         AG_EXNAME = 'MATERIAL'                                           533
         AG_PARLIST = 'NONE'                                              533
         CALL AXMATERIAL                                                  533
C    *                                                                    533
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=4 <===                       533
         CALL AGSRESET                                                    534
         AG_SEEN = 1                                                      534
         AG_COLO = 4                                                      534
         AG_TITLE = 'NONE'                                                534
         AG_EXNAME = 'ATTRIBUTE'                                          534
         AG_PARLIST = 'SEEN_COLO'                                         534
         CALL AXATTRIBUTE                                                 534
C    *                                                                    534
C    Geant SHAPE : TUBE ===> RMIN=PITB_RMIN/2. RMAX=PITB_RMAX/2., DZ=PIT  534
C    B_DZ/2. <===                                                         534
         CALL AGSRESET                                                    536
         AG_RMIN = PITB_RMIN/2.                                           536
         AG_RMAX = PITB_RMAX/2.                                           536
         AG_DZ = PITB_DZ/2.                                               536
         AG_TITLE = 'TUBE'                                                536
         AG_EXNAME = 'SHAPE'                                              536
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      536
         CALL AXSHAPE                                                     536
C    *                                                                    536
5591  CALL AGSPOPB                                                        537
      GO TO BLOCK_APFH, (5590 ,5600)                                      537
C End of Local Procedure                                                  537
C ----------------------------------------------------------------------  538
C ------------ APFI                                                       538
C Begin Local Procedure APFI                                              539
C APFI A PIT FLANGE BACKER I                                              539
5610  CONTINUE                                                            539
         AG_TITLE='APFI'                                                  539
         AG_MODULE=MODULE                                                 539
         AG_MARK='V00'                                                    539
         CALL AGSPUSH                                                     539
         AG_MODULE=MODULE                                                 539
C    *                                                                    539
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          540
         CALL AGSRESET                                                    540
         AG_TITLE = 'CFRPMIX'                                             540
         AG_EXNAME = 'MATERIAL'                                           540
         AG_PARLIST = 'NONE'                                              540
         CALL AXMATERIAL                                                  540
C    *                                                                    540
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=5 <===                       540
         CALL AGSRESET                                                    541
         AG_SEEN = 1                                                      541
         AG_COLO = 5                                                      541
         AG_TITLE = 'NONE'                                                541
         AG_EXNAME = 'ATTRIBUTE'                                          541
         AG_PARLIST = 'SEEN_COLO'                                         541
         CALL AXATTRIBUTE                                                 541
C    *                                                                    541
C    Geant SHAPE : TUBE ===> RMIN=PITC_RMIN/2. RMAX=PITC_RMAX/2., DZ=PIT  541
C    C_DZ/2. <===                                                         541
         CALL AGSRESET                                                    543
         AG_RMIN = PITC_RMIN/2.                                           543
         AG_RMAX = PITC_RMAX/2.                                           543
         AG_DZ = PITC_DZ/2.                                               543
         AG_TITLE = 'TUBE'                                                543
         AG_EXNAME = 'SHAPE'                                              543
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      543
         CALL AXSHAPE                                                     543
C    *                                                                    543
5611  CALL AGSPOPB                                                        544
      GO TO BLOCK_APFI, (5610 ,5620)                                      544
C End of Local Procedure                                                  544
C ----------------------------------------------------------------------  545
C ------------ PITR                                                       545
C Begin Local Procedure PITR                                              546
C PITR A PIT RAIL                                                         546
5630  CONTINUE                                                            546
         AG_TITLE='PITR'                                                  546
         AG_MODULE=MODULE                                                 546
         AG_MARK='V00'                                                    546
         CALL AGSPUSH                                                     546
         AG_MODULE=MODULE                                                 546
C    *                                                                    546
C    Geant MATERIAL : CFRPMIX ===> inherited arguments used <===          547
         CALL AGSRESET                                                    547
         AG_TITLE = 'CFRPMIX'                                             547
         AG_EXNAME = 'MATERIAL'                                           547
         AG_PARLIST = 'NONE'                                              547
         CALL AXMATERIAL                                                  547
C    *                                                                    547
C    Geant ATTRIBUTE : NONE ===> SEEN=1 COLO=2 <===                       547
         CALL AGSRESET                                                    548
         AG_SEEN = 1                                                      548
         AG_COLO = 2                                                      548
         AG_TITLE = 'NONE'                                                548
         AG_EXNAME = 'ATTRIBUTE'                                          548
         AG_PARLIST = 'SEEN_COLO'                                         548
         CALL AXATTRIBUTE                                                 548
C    *                                                                    548
C    Geant SHAPE : TUBE ===> RMIN=PITR_DMIN/2. RMAX=PITR_DMAX/2., DZ=PIT  548
C    R_DZ/2. <===                                                         548
         CALL AGSRESET                                                    550
         AG_RMIN = PITR_DMIN/2.                                           550
         AG_RMAX = PITR_DMAX/2.                                           550
         AG_DZ = PITR_DZ/2.                                               550
         AG_TITLE = 'TUBE'                                                550
         AG_EXNAME = 'SHAPE'                                              550
         AG_PARLIST = 'RMIN_RMAX_DZ'                                      550
         CALL AXSHAPE                                                     550
C    *                                                                    550
5631  CALL AGSPOPB                                                        551
      GO TO BLOCK_PITR, (5630 ,5640,5650,5660,5670)                       551
C End of Local Procedure                                                  551
      WRITE(LOUT,5680)'===>>  [End AgML Module PxstGeo1]  <<==='          551
5680  FORMAT(A32,/,/)                                                     551
C MODULE PXSTGEO1                                                         553
      END                                                                 553
