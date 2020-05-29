*-----------------------------------------------------------------------------
* <Rating>-10</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.I.LISTAR.OFS
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
*-----------------------------------------------------------------------------
*GOSUB CONSTRUCTOR
GOSUB OPENFILE
*GOSUB ESCRIBIR.ARCHIVO



*CONSTRUCTOR:
*RETURN

OPENFILE:
varnom='MASIVOS'
varn='ofs1.txt'
 OPENSEQ varnom, varn TO path THEN
          NULL
       END ELSE
          CRT 'ERROR OPENING FILE'
          STOP
       END
       READSEQ V.LINE FROM path ELSE
          CRT 'ERROR READING FILE'
          STOP
       END
       Y.OPTIONS='SLVOFSPS'
        CALL OFS.CALL.BULK.MANAGER(Y.OPTIONS, V.LINE, Y.RESPONSE,TXN.RESULT)
        	Y.OUT = TXN.RESULT:FM:Y.RESPONSE
        	CALL JOURNAL.UPDATE("")

RETURN



END
