*-----------------------------------------------------------------------------
* <Rating>0</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE TEST.OFS10
    $INSERT I_COMMON
    $INSERT I_EQUATE
    OFS.STR = 'SECTOR,/I/PROCESS///,BCARDENAS01/WSXZAQ/GB0010001/////,19,DESCRIPTION=\NULL,SHORT.NAME=HOLA MUNDO'
    OFS.SRC = 'GENERIC.OFS.PROCESS'
    OFS.MSG.ID = ''
    OPTIONS = ''
    CALL OFS.POST.MESSAGE(OFS.STR,OFS.MSG.ID,OFS.SRC,OPTIONS)
    CALL JOURNAL.UPDATE("")
    RETURN
END

