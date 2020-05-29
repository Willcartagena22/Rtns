*-----------------------------------------------------------------------------
* <Rating>221</Rating>
*-----------------------------------------------------------------------------
SUBROUTINE SLV.T2.E.NOF.COLLECTOR(OUT.ARRAY)
*-----------------------------------------------------------------------------
*
*-----------------------------------------------------------------------------
* Modification History :  
*-----------------------------------------------------------------------------
* Program Description: Rutina para listar los colectores para TCIB y TCE
*-----------------------------------------------------------------------------
* Version 1.0 2017.04.5 	Rcortes 		Initial
* Version 1.1 2017.06.14	Rcortes			Se cambia el codigo de institucion por el nombre de la categoria, y se puede filtrar por codigo de colector			   
*
$INSERT I_COMMON
$INSERT I_EQUATE
$INSERT I_ENQUIRY.COMMON
$INSERT I_F.EB.SLV.GLOBAL.PARAM
$INSERT I_F.EB.SLV.PARAM.CLT.BANCA


*-----------------------------------------------------------------------------
*-----------------------------------------------------------------------------  
	GOSUB INIT
    GOSUB OPENFILES
    GOSUB PROCESS
   	CRT OUT.ARRAY
   	RETURN

INIT:
	OUT.ARRAY 		= ''
	ID.GLB.PRM 		= 'SLV.TCIB.COL.PARAM.ID' 
	
	FN.PARAM.CLT	= 'F.EB.SLV.PARAM.CLT.BANCA'
    F.PARAM.CLT   	= ''
    
	FN.GLOBAL.PARAM = 'F.EB.SLV.GLOBAL.PARAM'
    F.GLOBAL.PARAM 	= ''
RETURN

OPENFILES:
  CALL OPF(FN.PARAM.CLT, F.PARAM.CLT)
  CALL OPF(FN.GLOBAL.PARAM, F.GLOBAL.PARAM)
  
RETURN

PROCESS:

	;* Obteniendo los parametros enviados desde banca, T2 = TCIB | TCE
	LOCATE 'CATEGORIA' IN D.FIELDS<1> SETTING CAT.POS THEN  ;* es para obtener los parametros de invocacion
    	Y.CAT = D.RANGE.AND.VALUE<CAT.POS>
    	Y.CAT = CHANGE(Y.CAT,FM,'')
    	Y.CAT = CHANGE(Y.CAT,VM,'')
    	Y.CAT = CHANGE(Y.CAT,SM,'')   
    	Y.CAT = CHANGE(Y.CAT,' ','')
    END
    LOCATE 'T2' IN D.FIELDS<1> SETTING T2.POS THEN  ;* es para obtener los parametros de invocacion
    	Y.T2 = D.RANGE.AND.VALUE<T2.POS>
    END
    LOCATE 'COLECTOR' IN D.FIELDS<1> SETTING COL.POS THEN  ;* es para obtener los parametros de invocacion
    	Y.COL = D.RANGE.AND.VALUE<COL.POS>
    END
    							;*TEXTO.ARCHIVO = 'CATEGORIA >> ' : Y.CAT : ' | T2 >> ' :  Y.T2 : ' | COLECTOR >> ' :  Y.COL
							    ;*GOSUB ESCRIBIR.ARCHIVO
    ;*********** <DEBUG> ***********
    ;* Y.CAT 	= 'Obligaciones':FM:'Patronales'
    ;* Y.T2 	= 'TCIB'
    ;* Y.COL 	= '02'
    ;*********** </DEBUG> ***********
    
    CALL F.READ(FN.GLOBAL.PARAM, ID.GLB.PRM , A.GLB, F.GLOBAL.PARAM, ERR.GLB)
    IF A.GLB THEN
    	;* Obteniendo el listado de colectores configurados para TCIB y/o TCE
    	ID.PRM.COLL = A.GLB<EB.SLV39.VALOR.PARAM>
    	CALL F.READ(FN.PARAM.CLT , ID.PRM.COLL, A.COLL, F.PARAM.CLT , ERR.COLL)	
    	
    							;*TEXTO.ARCHIVO = 'A.COLL >> ' : A.COLL : ' | A.GLB >> ' : A.GLB 
							    ;*GOSUB ESCRIBIR.ARCHIVO
    	;*CRT 'A.COLL ' : A.COLL
    	IF A.COLL THEN		
			FOR I = 1 TO DCOUNT(A.COLL<EB.SLV60.CATEGORIA.CLTR>, VM)
				IF Y.COL NE '' THEN 
					IF A.COLL<EB.SLV60.ID.COLECTOR, I> EQ Y.COL THEN
						GOSUB RECORD.T2
					END
				END
				ELSE
					Y.CAT.COL = A.COLL<EB.SLV60.CATEGORIA.CLTR, I>
				    Y.CAT.COL = CHANGE(Y.CAT.COL,FM,'')
			    	Y.CAT.COL = CHANGE(Y.CAT.COL,VM,'')
			    	Y.CAT.COL = CHANGE(Y.CAT.COL,SM,'')   
			    	Y.CAT.COL = CHANGE(Y.CAT.COL,' ','')
    	
					BEGIN CASE
				        CASE Y.T2 = 'TCIB'
				        	IF A.COLL<EB.SLV60.USA.TCIB.CLTR, I> EQ 'SI' AND Y.CAT.COL EQ Y.CAT THEN
				        	
				        						;*TEXTO.ARCHIVO = 'REGISTRO >> ' : TRIM(A.COLL<EB.SLV60.CATEGORIA.CLTR, I>, ' ', 'D') : ' | VARIABLE >> ' : TRIM(Y.CAT, ' ', 'D')
							    				;*GOSUB ESCRIBIR.ARCHIVO
				        		GOSUB RECORD.T2
				        	END        	
												
				        CASE Y.T2 = 'TCE'
				        	IF A.COLL<EB.SLV60.USA.TCE.CLTR, I> EQ 'SI' AND Y.CAT.COL EQ Y.CAT THEN
				        		GOSUB RECORD.T2
				        	END 
				    END CASE
			    END
		    NEXT I
*		    GOSUB RECORD.END
		     
		END		
    END
    
RETURN   

RECORD.T2:
	;* orden de los parametros enviados 
	;* Codigo Colector * Categoria del Colector * Nombre del Colector * Aplica LIOF * Aplica Pagos Durante COB * Longitud NPE * Aplica Token * Aplica Reserva de fondos * Tipo de Input * Colector es en linea o no * Nombre del servicio
	OUT.ARRAY<-1> = A.COLL<EB.SLV60.ID.COLECTOR, I> : '*' : A.COLL<EB.SLV60.CATEGORIA.CLTR, I> : '*' : A.COLL<EB.SLV60.CMP.RESERVA.5, I> : '*' : A.COLL<EB.SLV60.USA.LIOF.CLTR, I> : '*' : A.COLL<EB.SLV60.USA.PGS.COB, I> : '*' : A.COLL<EB.SLV60.LONGITUD.NPE, I> : '*' : A.COLL<EB.SLV60.USA.VAL.TOKEN, I> : '*' : A.COLL<EB.SLV60.USA.RESRV..FND, I> : '*' : A.COLL<EB.SLV60.CMP.RESERVA.3, I> : '*' : A.COLL<EB.SLV60.CMP.RESERVA.2, I> : '*' : A.COLL<EB.SLV60.CMP.RESERVA.1, I>
												;*TEXTO.ARCHIVO = 'OUT.ARRAY >> ' : OUT.ARRAY 
							    				;*GOSUB ESCRIBIR.ARCHIVO
RETURN 	

RECORD.END:
	MULTI.ACT.CONT = DCOUNT(Y.COL.ID, @FM)
    FOR I = 1 TO MULTI.ACT.CONT 
    	OUT.ARRAY<-1> = FIELD(Y.COL.ID , @VM, I) : '*' : FIELD(Y.COL.NOMBRE , @VM, I) : '*' : FIELD(Y.USA.LIOF , @VM, I) : '*' : FIELD(Y.PAGOS.COB , @VM, I) : '*' : FIELD(Y.LONG.NPE , @VM, I) : '*' : FIELD(Y.USA.TOKEN , @VM, I) : '*' : FIELD(Y.USA.RESRV , @VM, I)
    NEXT I  
RETURN 

ESCRIBIR.ARCHIVO:
    DIR.NAME= 'COLECTORES'
    R.ID   = 'ENQ.T2.ENOF':TODAY:'.txt'
;* hacer que escriba un archivo

    OPENSEQ DIR.NAME,R.ID TO SEQ.PTR
    WRITESEQ TEXTO.ARCHIVO APPEND TO SEQ.PTR THEN
    END
    CLOSESEQ SEQ.PTR
RETURN

END
