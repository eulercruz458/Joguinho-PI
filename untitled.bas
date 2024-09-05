DECLARE SUB TelaInicial ()
DECLARE SUB AtualizaStatus (acao$, dificuldade%)
DECLARE SUB ExibeStatus ()
DECLARE SUB VerificaEventosAleatorios ()
DECLARE SUB DesenhaGrafico ()
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
Print "Escolha o nivel de dificuldade:"
Print "1. Facil"
Print "2. Medio"
Print "3. Dificil"

Do
    Input dificuldade
    If dificuldade = 1 Or dificuldade = 2 Or dificuldade = 3 Then
        Exit Do
    Else
        Print "Digite apenas um das dificulades disponiveis nas opcoes"
    End If
Loop

If dificuldade = 1 Then
    dificuldade = -2
ElseIf dificuldade = 2 Then
    dificuldade = -3
Else
    dificuldade = -4
End If

' Comecar a jornada
Cls
Print "Voce e um viajante atravessando o deserto em busca de um oasis. Boa sorte!"
Print "A jornada dura 20 dias. Sobreviva para vencer!"

Do While dia <= 20 And energia > 0
    Print "Dia "; dia
    ExibeStatus

    Print "Escolha uma acao:"
    Print "1. Andar"
    Print "2. Descansar"
    If dificuldade <> -4 Then
        Print "3. Refrescar-se"
    End If
    If dificuldade <> -4 Then
        Print "4. Beber Agua"
    End If

    Do
        Input escolha$
        If escolha$ = "1" Or escolha$ = "2" Or (dificuldade <> -4 And escolha$ = "3") Or (dificuldade <> -4 And escolha$ = "4") Then
            Exit Do
        Else
            Print "Digite apenas um dos numeros disponiveis nas opcoes"
        End If
    Loop

    Select Case escolha$
        Case "1"
            AtualizaStatus "andar", dificuldade
        Case "2"
            AtualizaStatus "descansar", dificuldade
        Case "3"
            If dificuldade <> -4 Then
                AtualizaStatus "refrescar", dificuldade
            Else
                Print "A opcao de refrescar-se nao esta disponivel neste nivel de dificuldade."
            End If
        Case "4"
            If dificuldade <> -4 Then
                VerificaEventosAleatorios
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

    ' Verificar condicoes de derrota
    If energia <= 0 Then
        Print "Voce desmaiou de exaustao!"
    End If
Loop

' Verificacao final de vitoria
If energia > 40 And sede < 50 And moral >= 50 And temperatura >= 29 And temperatura <= 38 Then
    Print "Parabens! Voce sobreviveu a travessia e chegou ao oasis!"
Else
    Print "Voce chegou ao oasis, mas em condicoes ruins. A jornada foi falha."
End If

' Desenhar grafico
DesenhaGrafico

' Parar musica
PararMusica

End

Sub TelaInicial ()
    Cls
    ' Definir o texto da tela inicial
    TEXT$ = "O Oasis Perdido: A Travessia do Deserto"

    ' Configurar o texto em amarelo
    Color 14 ' Amarelo

    ' Calcular a largura da tela para centralizar o texto
    TEXTWIDTH = 80
    TEXTLENGTH = Len(TEXT$)

    ' Calcular a posição horizontal para centralizar
    X = (TEXTWIDTH - TEXTLENGTH) \ 2

    ' Desenhar o texto centralizado
    Locate 5, X + 1
    Print TEXT$

    ' Restaurar a cor do texto
    Color 7 ' Branco

    Print
    Print "Pressione qualquer tecla para comecar..."

    ' Tocar música
    TocarMusica

    ' Esperar a tecla ser pressionada para continuar
    While InKey$ = "": Wend

    ' Parar a música
    PararMusica
End Sub


Sub AtualizaStatus (acao$, dificuldade%)
    Select Case acao$
        Case "andar"
            energia = energia + dificuldade%
            sede = sede + Abs(dificuldade%)
            moral = moral + dificuldade% \ 2
            temperatura = temperatura + 1
            Print "Voce andou. Atravessar o deserto e exaustivo."
        Case "descansar"
            energia = energia + (10 + dificuldade%)
            sede = sede + 2
            temperatura = temperatura + 0.5 ' Aumenta a temperatura ao descansar
            Print "Voce descansou, recuperou energia, mas o calor aumentou."
        Case "refrescar"
            If dificuldade = -2 Then
                temperatura = temperatura - 5
                sede = sede - 5
                energia = energia - 10
                moral = moral - 10
            ElseIf dificuldade = -3 Then
                temperatura = temperatura - 3
                sede = sede - 3
                energia = energia - 5
                moral = moral - 5
            End If
            If temperatura < 28 Then temperatura = 28 ' Temperatura mínima
            If sede < 0 Then sede = 0
            If energia < 0 Then energia = 0
            If moral < 0 Then moral = 0
            Print "Voce encontrou uma sombra e se refrescou. A temperatura e a sede foram reduzidas, mas sua energia e moral diminuiram."
        Case "beber agua"
            If dificuldade = -2 Then
                sede = sede - 15
            ElseIf dificuldade = -3 Then
                sede = sede - 10
            Else
                sede = sede - 5
            End If
            If sede < 0 Then sede = 0
            Print "Voce bebeu agua. A sede foi reduzida."
    End Select
End Sub

Sub ExibeStatus ()
    Print "Energia: "; energia
    Print "Sede: "; sede
    Print "Moral: "; moral
    Print "Temperatura Corporal: "; temperatura
End Sub

Sub VerificaEventosAleatorios ()
    Randomize Timer
    evento = Int(Rnd * 2) + 1
    If evento = 1 Then
        Print "Voce encontrou uma fonte de agua!"
        If dificuldade = -2 Then
            sede = sede - 15
        ElseIf dificuldade = -3 Then
            sede = sede - 10
        Else
            sede = sede - 5
        End If
        If sede < 0 Then sede = 0
    ElseIf evento = 2 Then
        Print "Uma tempestade de areia apareceu!"
        moral = moral - 10
        sede = sede + 10
        If dificuldade = -4 Then ' Somente no modo difícil
            temperatura = temperatura - 3 ' Reduz a temperatura
            If temperatura < 28 Then temperatura = 28 ' Temperatura mínima
        End If
    End If
End Sub

Sub DesenhaGrafico ()
    Cls
    Print "Grafico de Status ao longo dos 20 dias:"
    For dia = 1 To 20
        Print "Dia "; dia
        Print "Energia: "; status_energia(dia)
        Print "Sede: "; status_sede(dia)
        Print "Moral: "; status_moral(dia)
        Print "Temperatura: "; status_temperatura(dia)
        Print
    Next dia
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

Sub PararMusica ()
    ' Finaliza a musica (se necessario)
    Sound 0, 0
End Sub

