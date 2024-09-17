Screen 12
DECLARE SUB TelaInicial ()
DECLARE SUB AtualizaStatus (acao$, dificuldade%)
DECLARE SUB ExibeStatus ()
DECLARE SUB VerificaEventosAleatorios ()
DECLARE SUB DesenhaGraficoColorido ()
DECLARE SUB TocarMusica()
DECLARE SUB PararMusica()

Dim Shared energia, sede, moral, temperatura, dia As Integer
Dim Shared dificuldade As Integer
Dim Shared status_energia(20), status_sede(20), status_moral(20), status_temperatura(20) As Integer

' Inicializacao
energia = 100
sede = 0
moral = 100
temperatura = 28
dia = 1

' Tela Inicial
TelaInicial

' Escolha do nivel de dificuldade
Cls
Color 14
Print ""
Print "Voce eh um viajante atravessando o deserto em busca de um oasis. Boa sorte!"
Print ""
Print "Para vencer, eh necessario estar com pelo menos esses requisitos ao final"
Print "Energia maior que 40, sede menor que 50, moral maior que 50 e temperatura entre 25 e 38"
Print ""
Print "A jornada dura 20 dias. Sobreviva para vencer!"
Print ""
Print "Escolha o nivel de dificuldade:"
Print "1. Facil"
Print "2. Medio"
Print "3. Dificil"

Do
    Input dificuldade
    If dificuldade = 1 Or dificuldade = 2 Or dificuldade = 3 Then
        Exit Do
    Else
        Print "Digite apenas um dos numeros disponiveis nas opcoes"
    End If
Loop

If dificuldade = 1 Then
    dificuldade = -2
ElseIf dificuldade = 2 Then
    dificuldade = -3
Else
    dificuldade = -4
End If

Do While dia <= 20 And energia > 0
    Cls ' Limpa a tela a cada novo turno
    Print "Dia "; dia
    ExibeStatus

    Print "Escolha uma acao:"
    Print "1. Andar"
    Print "2. Descansar"
    Print "3. Refrescar-se"
    If dificuldade <> -4 Then
        Print "4. Beber Agua"
    End If

    Do
        Input escolha$
        If escolha$ = "1" Or escolha$ = "2" Or escolha$ = "3" Or (dificuldade <> -4 And escolha$ = "4") Then
            Exit Do
        Else
            Print "Digite apenas um dos numeros disponiveis nas opcoes"
        End If
    Loop

    ' Processa a escolha do jogador e exibe mensagens
    Select Case escolha$
        Case "1"
            AtualizaStatus "andar", dificuldade
            Print "Voce andou um arduo dia no deserto, sua energia e moral diminuiram, e sua sede e temperatura aumentaram."
        Case "2"
            AtualizaStatus "descansar", dificuldade
            Print "Voce recuperou energia para sua jornada, mas ficar parado tambem aumenta sua temperatura."
        Case "3"
            AtualizaStatus "refrescar", dificuldade
            Print "Voce encontrou uma pequena fonte d'gua, sua sede e temperatura diminuiram, mas sua energia e moral tambem."
        Case "4"
            If dificuldade <> -4 Then
                AtualizaStatus "beber agua", dificuldade
                Print "Voce bebeu agua e diminuiu a sua sede."
            Else
                Print "Beber agua nao esta disponivel neste nivel de dificuldade."
            End If
    End Select

    ' Registro dos status
    status_energia(dia) = energia
    status_sede(dia) = sede
    status_moral(dia) = moral
    status_temperatura(dia) = temperatura

    dia = dia + 1

    ' Verificar eventos aleat�rios
    VerificaEventosAleatorios

    ' Esperar o jogador pressionar Enter antes de continuar
    Print "Pressione Enter para continuar..."
    While InKey$ <> Chr$(13) ' 13 � o c�digo ASCII para Enter
    Wend
    Cls ' Limpa a tela antes de continuar para o pr�ximo turno
Loop

' Verifica��o final de vit�ria ou derrota ap�s os 20 dias
Cls ' Limpa a tela antes de exibir a verifica��o final
If energia > 40 And sede < 50 And moral >= 50 And temperatura >= 25 And temperatura <= 39 Then
    Print "        Parabens! Voce sobreviveu a travessia e chegou ao oasis!"
    Print ""
    Print "                @@@@@@"
    Print "    @@@@@@      @@--@@"
    Print "    mm@@++@@MM@@----@@"
    Print "      @@----@@@@----@@@@@@@@               "
    Print "      @@@@----@@::@@@@....@@@@             "
    Print "      @@@@@@@@@@@@......mm@@               "
    Print "  @@@@mm----++@@@@@@@@@@@@"
    Print "::@@--------@@@@@@..mm@@"
    Print "  @@@@@@@@@@@@++@@......@@"
    Print "          mm@@++@@@@mm..MM@@"
    Print "          @@++++@@  @@@@@@@@"
    Print "          @@++++@@"
    Print "          @@++++@@"
    Print "          @@++++@@                      ++mm++.."
    Print "    @@@@@@@@mm++@@@@@@@@@@      @@@@@@@@MM++MM@@@@@@@@"
    Print "@@@@......mm@@++@@......MM@@@@@@++..................::@@"
    Print "@@@@........@@++@@..........++@@@@....................@@"
    Print "@@@@........@@++##MM............++@@@@................@@"
    Print "@@@@........@@++++@@..............  ++@@@@............@@"
    Print "@@@@@@@@@@@@@@@@@@@@..MM@@@@@@@@@@@@@@@@@@@@@@@@@@MM..@@"
    Print "@@@@............  @@@@@@                          MM@@@@"
    Print "@@@@............      @@@@@@                          @@"
    Print "@@@@..............        ::@@@@@@mm              --@@@@"
    Print "@@@@................              @@@@@@@@@@@@@@@@@@..@@"
    Print "@@@@................                                  @@"
    Print "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"

    TocarMusicaVitoria ' Toca a m�sica de vit�ria
Else
    Print "Devido as mas condicoes, voce delirou."
    Print "Achava que estava no oasis, mas acabou morrendo"
    Print ""
    Print "                  ################"
    Print "              ########################"
    Print "            ############################"
    Print "          ################################"
    Print "          ################################"
    Print "        ####################################"
    Print "        ######################################"
    Print "      ########################################"
    Print "      ########      ++##########      ########"
    Print "      @@####            @@##..          @@####"
    Print "      --@@##            ######          MM####"
    Print "        ####          ########          ####"
    Print "        ####################################"
    Print "        ################    ################"
    Print "      --##############        ################"
    Print "      @@##############        ################"
    Print "        ##############  ::MM  ##############"
    Print "                ####################"
    Print "                @@##################"
    Print "                ::##  ####++##  ####"

    TocarMusicaDerrota ' Toca a m�sica de derrota
End If

' Esperar o jogador pressionar Enter antes de mostrar o gr�fico
Print "Pressione Enter para ver o grafico de status..."
While InKey$ <> Chr$(13) ' 13 � o c�digo ASCII para Enter
Wend
Cls ' Limpa a tela antes de mostrar o gr�fico

' Parar m�sica antes de exibir o gr�fico
PararMusicaFinal

' Desenhar gr�fico
DesenhaGraficoColorido

' Parar m�sica
PararMusicaFinal
End

' M�sica de vit�ria
Sub TocarMusicaVitoria ()
    ' Uma melodia simples para a vit�ria
    Sound 784, 10 ' Sol
    Sound 880, 10 ' L�
    Sound 988, 10 ' Si
    Sound 1047, 10 ' D�
    Sound 1175, 10 ' R�
    Sound 1319, 10 ' Mi
    Sound 1480, 10 ' F�
    Sound 1568, 10 ' Sol
    Sound 0, 10
End Sub

' M�sica de derrota
Sub TocarMusicaDerrota ()
    ' Uma melodia simples para a derrota
    Sound 262, 15 ' D�
    Sound 233, 15 ' Si bemol
    Sound 196, 15 ' Sol
    Sound 174, 15 ' F�
    Sound 196, 15 ' Sol
    Sound 233, 15 ' Si bemol
    Sound 262, 15 ' D�
    Sound 0, 15
End Sub

' Parar m�sica final
Sub PararMusicaFinal ()
    ' Finaliza a m�sica
    Sound 0, 0
End Sub

' Definicao das SUBs

Sub TelaInicial ()
    Cls
    ' Definir o texto da tela inicial
    TEXT$ = "O Oasis Perdido: A Travessia do Deserto"
    ' Configurar o texto em amarelo
    Color 14 ' Amarelo

    ' Calcular a largura da tela para centralizar o texto
    TEXTWIDTH = 80
    TEXTLENGTH = Len(TEXT$)

    ' Calcular a posi��o horizontal para centralizar
    X = (TEXTWIDTH - TEXTLENGTH) \ 2

    ' Desenhar o texto centralizado
    Locate 5, X + 1
    Print TEXT$

    ' Restaurar a cor do texto
    Color 7 ' Branco

    Print
    Print "Pressione qualquer tecla para comecar..."

    ' Tocar m�sica
    TocarMusica

    ' Esperar a tecla ser pressionada para continuar
    While InKey$ = "": Wend

    ' Parar a m�sica
    PararMusica
End Sub

' Verifica e Atualiza os Status com a l�gica adequada
Sub AtualizaStatus (acao$, dificuldade%)
    Color 14
    Select Case acao$
        Case "andar"
            energia = energia + dificuldade%
            sede = sede + Abs(dificuldade%)
            moral = moral + dificuldade% \ 2
            temperatura = temperatura + 1
        Case "descansar"
            energia = energia + (10 + dificuldade%)
            sede = sede + 2
            temperatura = temperatura + 2 ' Aumenta a temperatura ao descansar
        Case "refrescar"
            If dificuldade = -2 Then
                temperatura = temperatura - 2
                sede = sede - 5
                energia = energia - 5
                moral = moral - 5
            ElseIf dificuldade = -3 Then
                temperatura = temperatura - 1.5
                sede = sede - 4
                energia = energia - 6
                moral = moral - 6
            ElseIf dificuldade = -4 Then
                temperatura = temperatura - 1
                sede = sede - 2
                energia = energia - 8
                moral = moral - 8
            End If
        Case "beber agua"
            If dificuldade = -2 Then
                sede = sede - 15
            ElseIf dificuldade = -3 Then
                sede = sede - 10
            Else
                sede = sede - 5
            End If
            If sede < 0 Then sede = 0
    End Select

    ' Garante que os status estejam dentro dos limites aceit�veis
    If energia < 0 Then energia = 0
    If sede < 0 Then sede = 0
    If moral < 0 Then moral = 0
    If temperatura > 100 Then temperatura = 100
End Sub

Sub ExibeStatus ()
    Color 14
    Print "Energia: "; energia
    Print "Sede: "; sede
    Print "Moral: "; moral
    Print "Temperatura Corporal: "; temperatura
End Sub

Sub VerificaEventosAleatorios ()
    Randomize Timer
    evento = Int(Rnd * 10) + 1 ' Probabilidade de 1/10 para cada evento

    Select Case evento
        Case 1 ' Tempestade de areia
            Print ""
            Print "Uma tempestade de areia apareceu!"
            Print "        ```````       _________"
            Print "  `````  ```        /           \\```          `` "
            Print "    ````` ``      //              \\``      ` `` "
            Print "       ```      //                 \\``       `  ` ``   "
            Print " `````   `     //                    \\```     ` ` "
            Print "          `   //                      \\     `` ` ` `"
            Print "  ``````  `   \\                       \\```  `` "
            Print "     `     `   \\                       \\`` `   ` ` ` ` "
            Print "                \\                       \\ ``     `"
            Print "  ``` `     `    \\                       \\ ```   `"
            Print "              `   \\                      \\ ``         ``"
            Print "  ````` `  `       \\                      /  ``` "
            Print "            `       \\                   /```   ` `     `` "
            Print "             ` `     \\                /`     ```   ```    "
            Print "   ````       `       \\_____________/ ``    ` ` ` `     ```"


            energia = energia - 6
            sede = sede + 5
            temperatura = temperatura + 2
            TocarSomVentoForte

        Case 2 ' Fonte de �gua
            Print ""
            Print "Voce encontrou uma fonte de agua!"
            Print "                                        ........"
            Print "                                    ................"
            Print "                                  ...................."
            Print "                                ........................"
            Print "                               ........::..::::..::::......"
            Print "                             ............::::..::::..::...."
            Print "         ..........      .................................."
            Print "       ...................................................."
            Print "     ......................................................"
            Print "    ...................................................."
            Print "    ......::..::::..::::................................"
            Print "    ........::....::....::............................"
            Print "    ................................................"
            Print "      ........::::::::::::::............"
            Print "      ................................"
            Print "        ............................"
            Print "          ........................"
            Print "                  .........."

            sede = sede - 10
            temperatura = temperatura - 3
            'Som de bebendo agua
            TocarSomBebendoAgua

    End Select

    ' Garante que a sede fique entre 0 e 100
    If sede < 0 Then sede = 0
    If sede > 100 Then sede = 100
End Sub

Sub DesenhaGraficoColorido ()
    Cls
    Print "Grafico de Status ao longo dos 20 dias:"

    ' Desenho do gr�fico
    For dia = 1 To 20
        ' Cor para Energia (Verde)
        Color 2
        Print "Dia "; dia; " - Energia: "; String$(status_energia(dia) \ 2, Chr$(219))

        ' Cor para Sede (Azul)
        Color 1
        Print "Dia "; dia; " - Sede: "; String$(status_sede(dia) \ 2, Chr$(219))

        ' Cor para Moral (Amarelo)
        Color 14
        Print "Dia "; dia; " - Moral: "; String$(status_moral(dia) \ 2, Chr$(219))

        ' Cor para Temperatura (Vermelho)
        Color 4
        Print "Dia "; dia; " - Temperatura: "; String$(status_temperatura(dia) \ 2, Chr$(219))

        ' Separar os dias no gr�fico
        Print

        ' A cada 4 dias, pausar a exibi��o para evitar corte
        If dia Mod 4 = 0 Then
            Print "Pressione qualquer tecla para continuar..."
            While InKey$ = "": Wend ' Esperar o usu�rio pressionar uma tecla
            Cls ' Limpa a tela antes de exibir os pr�ximos dias
        End If
    Next dia

    ' Restaurar cor padr�o
    Color 7
End Sub

Sub TocarMusica ()
    ' Melodia simples usando a instrucao SOUND em loop
    Do
        Sound 262, 8 ' Do
        Sound 294, 8 ' Re
        Sound 330, 8 ' Mi
        Sound 349, 8 ' Fa
        Sound 392, 8 ' Sol
        Sound 440, 8 ' La
        Sound 494, 8 ' Si
        Sound 523, 8 ' Do
        Sound 587, 8 ' Re
        Sound 659, 8 ' Mi
        Sound 698, 8 ' Fa
        Sound 784, 8 ' Sol
        Sound 880, 8 ' La
        Sound 988, 8 ' Si
        Sound 1047, 8 ' Do
    Loop Until InKey$ <> ""
End Sub

Sub TocarSomVentoForte ()
    ' Simula um som de vento forte
    Sound 392, 15 ' Sol
    Sound 349, 15 ' Fa
    Sound 330, 15 ' Mi
    Sound 311, 15 ' Mi bemol
    Sound 294, 15 ' Re
    Sound 262, 15 ' Do
    Sound 233, 15 ' Si bemol
    Sound 196, 15 ' Sol
    Sound 0, 10 ' Pausa
End Sub

Sub TocarSomBebendoAgua ()
    ' Simula um som de �gua sendo bebida (borbulhando)
    Sound 330, 5 ' Mi
    Sound 392, 5 ' Sol
    Sound 523, 5 ' D�
    Sound 330, 5 ' Mi
    Sound 392, 5 ' Sol
    Sound 262, 5 ' D�
    Sound 0, 10 ' Pausa
End Sub


Sub PararMusica ()
    ' Finaliza a musica (se necessario)
    Sound 0, 0
End Sub

