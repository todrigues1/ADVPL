#include 'protheus.ch'
#include 'parmtype.ch'
#include 'TopConn.ch'

#Define _ENTER	Chr(13)+Chr(10)
#Define _TAB	CHR(9)

/*/
בבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבב
בבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבב
בבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבֽ»בב
בבבPrograma  ב FIMPPRD    ב Autor ב Vitor Santos P2P    ב Data ב  10/08/22בבב
בבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבֽ¹בב
בבבDescricao ב IMPORTAֳַO DE LISTAS DE PRODUTO        		              בבב
בבב          ב                                                            בבב
בבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבֽ¹בב
בבבUso       ב AP6 IDE                                                    בבב
בבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבֽ¼בב
בבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבב
בבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבבב
/*/  

user function FIMPPRD()

	Private cArq	:= Space(50)
	Private lRet	:= .F.

	cArq := cGetFile('*.csv|*.csv','Selecione o Arquivo',0,'C:\',.T.,GETF_LOCALHARD+GETF_NETWORKDRIVE,.T.)

	MsgRun("Aguarde... Lendo o Arquivo..."     ,, {||CursorWait(), lRet := fLeCp(), CursorArrow()})

	If !lRet
		MsgAlert("Ocorreram erros na importaחדo...!!")
	Endif

Return(lRet)

/*
|	EFETUAR LEITURA DOS DADOS
*/
Static Function fLeCp()

	Local nHandle 		:= FT_FUse(cArq)
	Local nConta  		:= 0
	Local cMsgCod   	:= ''
	Local lRet      	:= .T.
	local aDados  		:= {}

	if nHandle = -1
		return
	endif

	FT_FGoTop()
	DbSelectArea('SB1')

	/* ################## ORDEM COLUNAS EXCEL #########################

		GRUPO									aRet[1]
		SUB GRUPO 								aRet[2]
		CODIGO  								aRet[3]
		DESCRICAO								aRet[4]
		TIPO     								aRet[5]
		UNIDADE  								aRet[6]
		ARMAZEM  								aRet[7]
		POS.IPI/NCM  							aRet[8]

	*/

	aDados := {}
	While !FT_FEOF()

		nConta++
		cLine := FT_FReadLn()



		aRet := StrTokArr2( cLine, ";" , .T. )
        
		if nConta < 2
			if (nConta == 1 .AND. UPPER(AllTrim(aRet[1])) == 'GRUPO')
				FT_FSKIP()
				LOOP
			else
				MsgAlert('Layout da Planilha fora do padrדo. Verifique.')
				return(.f.)
			endIf		
		endIf
        
		If len(aRet) > 1
        

			AADD( aDados, { AllTrim(aRet[1]),;//GRUPO
							AllTrim(aRet[2]),;//SUB GRUPO 
							AllTrim(aRet[3]),;//CODIGO
							AllTrim(aRet[4]),;//DESCRICAO
							AllTrim(aRet[5]),;//TIPO
							AllTrim(aRet[6]),; //UNIDADE
							AllTrim(aRet[7]),;//ARMAZEM
							AllTrim(aRet[8])} )//POS.IPI/NCM 
		EndIf

		FT_FSKIP()
	End

	FT_FUSE()

	If Len(aDados) > 0
		If !EMPTY(AllTrim(cMsgCod))
			MsgAlert(cMsgCod)
			lRet := .F.
		Else
			MsgRun("Aguarde... Incluindo a Lista"     ,, {|| CursorWait(), lRet := U_GrvNewB1(aDados), CursorArrow()})
		EndIf
	Endif

Return(lRet)

User function GrvNewB1(aInfo)
    Local aProduto := {}
    Local nOpc := 3 // inclusao
	Local nCont
    Private lMsHelpAuto := .t. 
    Private lMsErroAuto := .f. 

    Begin Transaction

        for nCont := 1 to Len(aInfo)

            aProduto:= {{'B1_GRUPO' ,STRZERO(val(aRet[1]), 3) ,Nil},;//GRUPO
            {'B1_SUBGRUP' ,STRZERO(val(aRet[2]), 3),Nil},;//SUB GRUPO
            {'B1_COD' ,Transform(STRZERO(val(U_TiraGraf(AllTrim(aRet[3]))), 9),"@R 999.999.999") ,Nil},;//CODIGO
            {'B1_DESC' ,AllTrim(aRet[4]) ,Nil},;//DESCRICAO
            {'B1_TIPO' ,AllTrim(aRet[5]) ,Nil},;//TIPO
            {'B1_UM' ,AllTrim(aRet[6]) ,Nil},;//UNIDADE
            {'B1_LOCPAD' ,STRZERO(val(aRet[2]), 2) ,Nil},;//ARMAZEM
            {'B1_POSIPI' ,U_TiraGraf(AllTrim(aRet[8])) ,Nil}}//POS.IPI/NCM

            MSExecAuto({|x,y| mata010(x,y)},aProduto,nOpc)

            If lMsErroAuto
                DisarmTransaction()
                break
            EndIf
        Next nCont
    End Transaction
        If lMsErroAuto

            Mostraerro()
            Return .f.
        EndIf

Return(lRet)
