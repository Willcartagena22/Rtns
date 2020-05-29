*-----------------------------------------------------------------------------
* <Rating>-42</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.V.DESCONEXION.LIOF.OFS
*-----------------------------------------------------------------------------
*
* Nombre: SLV.V.DESCONEXION.LIOF.OFS
* Descripción: Rutina para enviar OFS masivo cambio de estado a no cobrar LIOF 
*
*-----------------------------------------------------------------------------
* Modification History :
*-----------------------------------------------------------------------------
* Autor		 	  Fecha		    	Comentario
*-----------------------------------------------------------------------------
*Jhenriquez     24.09.2018		   Initial Code
*-----------------------------------------------------------------------------
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_F.CUSTOMER
*-----------------------------------------------------------------------------

GOSUB INI
GOSUB OPENFILE
GOSUB PROCESS
RETURN

INI:
 FN.CUS = 'F.CUSTOMER'
 F.CUS  = ''
 
 EQU OFS.SOURCE TO 'SLVOFSPS' 
RETURN

OPENFILE:
 CALL OPF(FN.CUS, F.CUS)
RETURN


PROCESS:
    STMT.CUSTOMER = "SELECT ":FN.CUS
    CALL EB.READLIST (STMT.CUSTOMER, KEY.LIST.CUSTOMER, '', NO.REGS.CUSTOMER, ERROR.CUSTOMER)
    
    FOR I = 1 TO NO.REGS.CUSTOMER
        V.ID.CUSTOMER = KEY.LIST.CUSTOMER<I>
        STRING.OFS.ADD.PRD = "CUSTOMER,/I/PROCESS//0/,//SV0010001/////,":V.ID.CUSTOMER:",LF.CTE.EXE:1:1=SI,"
        
        TEXTO.ARCHIVO = STRING.OFS.ADD.PRD
		GOSUB ESCRIBIR.ARCHIVO
        
        ;*Envio de OFS  
		CALL OFS.POST.MESSAGE(STRING.OFS.ADD.PRD,'', OFS.SOURCE,'')
    	;*Para aplicar cambios.
		CALL JOURNAL.UPDATE ('')
    NEXT I
    
RETURN


ESCRIBIR.ARCHIVO:
	    DIR.NAME= 'LogOFSCustomerDescLIOF'
	    R.ID   = 'OFS_CUSTOMER_DESC_LIOF-':TODAY:'.txt'
	    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
	    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
	    END
	    CLOSESEQ SEQ.PTR
    RETURN
END
