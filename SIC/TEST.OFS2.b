*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE TEST.OFS2
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.FUNDS.TRANSFER
    APP.NAME='FUNDS.TRANSFER'
    OFFUNCT='I'
    PROCESS='PROCESS'
    OFSVERSION='FUNDS.TRANSFER,'
    GTSMODE=''
    NO.OF.AUTH=''
    TRANSACTION.ID=''
    RECORD=''
    OFSRECORD=''
    RECORD<FT.TRANSACTION.TYPE>='AC'
    RECORD<FT.DEBIT.ACCT.NO>=14378
    RECORD<FT.DEBIT.CURRENCY>='CHF'
    RECORD<FT.DEBIT.AMOUNT>=300
    RECORD<FT.CREDIT.ACCT.NO>=22608
    RECORD<FT.CREDIT.CURRENCY>='CHF'
    CALL OFS.BUILD.RECORD(APP.NAME,OFSFUNCT,PROCESS,OFSVERSION,GTSMODE,NO.OF.AUTH,TRANSACTION.ID,RECORD,OFSRECORD)
    OFS.SRC = 'GENERIC.OFS.PROCESS'
    OFS.MSG.ID = ''
    OPTIONS = ''
    CALL OFS.POST.MESSAGE(OFSRECORD,OFS.MSG.ID,OFS.SRC,OPTIONS)
    CALL JOURNAL.UPDATE("")
    RETURN
END

