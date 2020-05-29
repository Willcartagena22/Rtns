*-----------------------------------------------------------------------------
* <Rating>-42</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.B.JOB.RESERVA
*-----------------------------------------------------------------------------
* Ejecuta las ritunas para la extraccion de datos
*-----------------------------------------------------------------------------
* Modification History :
* Autor    	Fecha     		Version
* GAlfaro  	27.11.2014 		Initial Code
*-----------------------------------------------------------------------------
    $INSERT I_COMMON
    $INSERT I_EQUATE 
*-----------------------------------------------------------------------------
*
    GOSUB INIT
    GOSUB PROCESS

    RETURN
INIT:
**   Variables de trabajo
    ERR		=''
    chNd	=' '
    inNd	= 0
    edLine	='|'
*	DIR.NAME='SIF.OUT'
    DIR.NAME='C:\EXTRACT-CRISK\'
    NAME.FILE='_logdata.txt'
    AR.REG=''
    J=0
    
    ;*eliminando archivo existente     
	DELETESEQ DIR.NAME,NAME.FILE THEN 
	END
    
    ;* Abriendo archivo para escritura
    OPENSEQ DIR.NAME, NAME.FILE TO SEQ.PTR THEN
		WEOFSEQ NAME.FILE
    END       
 	RETURN
    
 PROCESS:  
*		;* Extrae todos los creditos para calculo de reserva
*		CALL SLV.SIF.CRISK.LOAN
*		J=1
*		DATEPROCESS=TODAY
*		FILENAME='_loan'
*		GOSUB ADDLINE
		
	;* Extrae todos los clientes para relaciones con los creditos
	CRT 'Entrando a SLV.SIF.CUSTOMER'
	CALL SLV.SIF.CUSTOMER
	J=2
	DATEPROCESS=TODAY
	FILENAME='_Customer'
	GOSUB ADDLINE
		
*		;* Extrae los documentos de los cuentes relacionados 
*		CALL SLV.SIF.CUSTOMERXDOCUMENTS
*		J=3
*		DATEPROCESS=TODAY
*		FILENAME='_CustomerXdocuemnts'
*		GOSUB ADDLINE
*		
*		;* Extrae las garantias relacionadas a creditos
*		CALL SLV.B.CRISK.LOAN.GUARANTEE
*		J=4
*		DATEPROCESS=TODAY
*		FILENAME='_loanByGuarantee'
*		GOSUB ADDLINE
*		
*		;* Extrae el detalle de las garantias
*		CALL SLV.B.SIF.GUARANTEE.DETAIL
*		J=5
*		DATEPROCESS=TODAY
*		FILENAME='_GuaranteeDetail'
*		GOSUB ADDLINE
				
	;* Generación de csv array para escritura de en csv
*-----------------------------------------------------------------------------
    FOR L=1 TO J
        IF L<J THEN
            WRITESEQ AR.REG<L> APPEND TO SEQ.PTR THEN ;*con retorno de carro
        	END
    	END
	    IF L=J THEN
	        WRITEBLK AR.REG<L> ON SEQ.PTR THEN ;*sin retorno de carro
	    	END
	 	END
    NEXT L
       
*-----------------------------------------------------------------------------
    CLOSESEQ SEQ.PTR
	RETURN
*--------------------------------------------------------------------------------
ADDLINE:
    ;***Valores para cadena
    R.LINE=DATEPROCESS:";":FILENAME:edLine            	
    AR.REG<J>=R.LINE       
	RETURN

END
