*-----------------------------------------------------------------------------
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.COUNTER.RESET.LOAD

*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Description           : This routine is to have the necessary variable and the OPF statements to be initialised
*
* Developed By          : Dinesh Kumar.R
*
* Development Reference : LIOF DEVELOPMENT
*
* Attached To           : BATCH>SLV.COUNTER.RESET
*
* Attached As           : Batch Routine 
*-----------------------------------------------------------------------------------------------------------------
* Input Parameter: 
* ---------------*
* Argument#1 : NA
*
* Output Parameter:
* ----------------*
* Argument#4 : NA
* 
*-----------------------------------------------------------------------------------------------------------------
*  M O D I F I C A T I O N S
* ***************************
*-----------------------------------------------------------------------------------------------------------------
* Defect Reference     Modified By              Date of Change        Change Details
* (RTC/TUT/PACS)                                 (YYYY-MM-DD)
*-----------------------------------------------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------------------------------------------
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
    $INSERT I_F.SLV.F.CONTADOR.LIOF
    $INSERT I_BATCH.FILES
$INSERT I_SLV.COUNTER.RESET.COMMON
*-----------------------------------------------------------------------------
    GOSUB INIT
    RETURN

*****
INIT:
*****
    FN.COUNTER = 'F.SLV.F.CONTADOR.LIOF'
    F.COUNTER = ''
    CALL OPF(FN.COUNTER, F.COUNTER)
    RETURN
    END
