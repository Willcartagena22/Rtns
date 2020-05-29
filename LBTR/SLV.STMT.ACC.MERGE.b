*-----------------------------------------------------------------------------
* <Rating>-24</Rating>
*-----------------------------------------------------------------------------
    SUBROUTINE SLV.STMT.ACC.MERGE(ARR.1,ARR.2,Y.OUT)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*@date 20161115
*@autor eurias
*@util rutina para funcionar 2 array para la cadena de los estados de cuenta en tfs
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE
*-----------------------------------------------------------------------------

    GOSUB INIT ;*
    GOSUB PROCESS ;*

*-----------------------------------------------------------------------------

*** <region name= INIT>
INIT:
*** <desc> </desc>
    LINEA = ''
    RETURN
*** </region>
*-----------------------------------------------------------------------------
*** <region name= PROCESS>
PROCESS:
*** <desc> </desc>
  Y.MONTO.1 = FIELD(ARR.1,'*',6)
    Y.MONTO.2 = FIELD(ARR.2,'*',6)
    ITER = DCOUNT(ARR.2,'*')
    FOR I = 1 TO ITER
        IF I EQ 6 THEN
            Y.MONTO = Y.MONTO.1 + Y.MONTO.2
            LINEA := Y.MONTO:'*'
        END ELSE
            LINEA := FIELD(ARR.2,'*',I):'*'
        END
    NEXT I
	Y.OUT = LINEA
    END
    RETURN
*** </region>
  


