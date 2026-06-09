' ============================================================
' PWM Frequency Dithering Signal Generator for CST Studio Suite
' 抖频 PWM 信号生成宏
' ------------------------------------------------------------
' Algorithm (spread-spectrum / jitter):
'   Each PWM cycle, the modulation wave sets instantaneous frequency:
'     f_inst = pwm_freq * (1 + mod_depth * triangle(t))
'     T_inst = 1 / f_inst
'   Duty cycle stays FIXED at base_duty.
'   High phase ends at: t_cur + base_duty * T_inst
'   Cycle    ends at: t_cur + T_inst
' ============================================================
' Usage:
'   1. Open CST Studio Suite
'   2. Macros > Edit/Run Macros > New or Open
'   3. Paste this script, run Sub GeneratePWMDither
'   4. A .sig file is saved next to the project
'   5. Import: Simulation > Excitation Signals > Import Signal
' ============================================================

Option Explicit

' ------------------------------------------------------------
' Main entry point
' ------------------------------------------------------------
Sub GeneratePWMDither()

    ' ========================================================
    ' Parameters  —— modify here as needed
    ' ========================================================
    Dim pwm_freq    As Double
    Dim base_duty   As Double
    Dim v_high      As Double
    Dim v_low       As Double
    Dim rise_time   As Double
    Dim fall_time   As Double
    Dim mod_freq    As Double
    Dim mod_depth   As Double
    Dim duration    As Double
    Dim dt          As Double

    pwm_freq  = 20000.0          ' PWM center frequency       (Hz)
    base_duty = 0.5              ' Fixed duty cycle           (0 ~ 1)
    v_high    = 0.0              ' High level voltage         (V)
    v_low     = 10.0             ' Low level voltage          (V)
    rise_time = 100.0 / 1.0E9   ' Rise time                  (s)  = 100 ns
    fall_time = 100.0 / 1.0E9   ' Fall time                  (s)  = 100 ns
    mod_freq  = 1000.0           ' Modulation (triangle) freq (Hz) = 1 kHz
    mod_depth = 0.10             ' Frequency deviation        (+/-10%)
    duration  = 0.001            ' Export duration            (s)  = 1 ms
    dt        = 1.0 / (pwm_freq * 80.0)   ' Time step (80 pts/cycle)

    ' ========================================================
    ' Allocate signal array
    ' ========================================================
    Dim n_pts As Long
    n_pts = CLng(Int(duration / dt)) + 1

    Dim t_arr()   As Double
    Dim sig_arr() As Double
    ReDim t_arr(0 To n_pts - 1)
    ReDim sig_arr(0 To n_pts - 1)

    Dim i As Long
    For i = 0 To n_pts - 1
        t_arr(i)   = CDbl(i) * dt
        sig_arr(i) = v_low        ' default: low level
    Next i

    ' ========================================================
    ' Frequency-dithered PWM  —— cycle-by-cycle generation
    ' ========================================================
    Dim t_cur   As Double : t_cur = 0.0
    Dim phase   As Double
    Dim mod_val As Double
    Dim f_inst  As Double
    Dim T_inst  As Double
    Dim t_he    As Double   ' end of high phase
    Dim t_ce    As Double   ' end of cycle
    Dim i_s     As Long     ' cycle start index
    Dim i_re    As Long     ' rise  end  index
    Dim i_he    As Long     ' high  end  index
    Dim i_fe    As Long     ' fall  end  index
    Dim i_ce    As Long     ' cycle end  index
    Dim j       As Long

    Do While t_cur < duration

        ' -- Triangle wave: output range [-1, +1] --
        ' phase = fractional part of (t * mod_freq)
        phase   = (t_cur * mod_freq) - Int(t_cur * mod_freq)
        mod_val = 2.0 * Abs(2.0 * phase - 1.0) - 1.0

        ' -- Instantaneous frequency & period --
        f_inst = pwm_freq * (1.0 + mod_depth * mod_val)
        If f_inst < 1.0 Then f_inst = 1.0   ' safety floor
        T_inst = 1.0 / f_inst

        t_he = t_cur + base_duty * T_inst   ' high phase end
        t_ce = t_cur + T_inst               ' cycle end

        ' -- Convert time boundaries to array indices --
        i_s  = TIdx(t_cur,              dt, n_pts - 1)
        i_re = TIdx(t_cur + rise_time,  dt, n_pts - 1)
        i_he = TIdx(t_he,               dt, n_pts - 1)
        i_fe = TIdx(t_he  + fall_time,  dt, n_pts - 1)
        i_ce = TIdx(t_ce,               dt, n_pts - 1)

        ' Clamp ramp ends so they don't overshoot the phase boundaries
        If i_re > i_he Then i_re = i_he
        If i_fe > i_ce Then i_fe = i_ce

        ' -- Rise ramp:  v_low → v_high --
        If i_re > i_s Then
            For j = i_s To i_re - 1
                sig_arr(j) = v_low + (v_high - v_low) * _
                             CDbl(j - i_s) / CDbl(i_re - i_s)
            Next j
        End If

        ' -- High level --
        For j = i_re To i_he - 1
            sig_arr(j) = v_high
        Next j

        ' -- Fall ramp:  v_high → v_low --
        If i_fe > i_he Then
            For j = i_he To i_fe - 1
                sig_arr(j) = v_high + (v_low - v_high) * _
                             CDbl(j - i_he) / CDbl(i_fe - i_he)
            Next j
        End If

        t_cur = t_ce   ' advance to next cycle
    Loop

    ' ========================================================
    ' Write CST-compatible .sig file
    ' ========================================================
    Dim sPath As String
    sPath = GetSaveFolder() & "\pwm_dither.sig"

    Dim fNum As Integer
    fNum = FreeFile()
    Open sPath For Output As #fNum

    ' Header comments
    Print #fNum, "; CST Studio Suite - Signal Import File"
    Print #fNum, "; PWM Frequency Dithering (Spread-Spectrum)"
    Print #fNum, ";"
    Print #fNum, "; pwm_freq   = " & Format(pwm_freq / 1000.0, "0.000") & " kHz"
    Print #fNum, "; duty_cycle = " & Format(base_duty * 100.0, "0.0") & " %  (fixed)"
    Print #fNum, "; freq_range = " & _
                 Format(pwm_freq * (1.0 - mod_depth) / 1000.0, "0.000") & " kHz  ~  " & _
                 Format(pwm_freq * (1.0 + mod_depth) / 1000.0, "0.000") & " kHz"
    Print #fNum, "; v_high     = " & Format(v_high, "0.000") & " V"
    Print #fNum, "; v_low      = " & Format(v_low,  "0.000") & " V"
    Print #fNum, "; rise_time  = " & Format(rise_time * 1.0E9, "0.0") & " ns"
    Print #fNum, "; fall_time  = " & Format(fall_time * 1.0E9, "0.0") & " ns"
    Print #fNum, "; mod_freq   = " & Format(mod_freq  / 1000.0, "0.000") & " kHz"
    Print #fNum, "; mod_depth  = +/- " & Format(mod_depth * 100.0, "0.0") & " %"
    Print #fNum, "; duration   = " & Format(duration  * 1000.0, "0.000") & " ms"
    Print #fNum, "; dt         = " & Format(dt * 1.0E9, "0.000") & " ns"
    Print #fNum, "; samples    = " & n_pts
    Print #fNum, ";"
    Print #fNum, "; Import: Simulation > Excitation Signals > Import Signal"
    Print #fNum, ";   Signal type: Voltage   Time unit: s"
    Print #fNum, "; Column 1 = time [s]   Column 2 = amplitude [V]"
    Print #fNum, ";"

    ' Data rows
    For i = 0 To n_pts - 1
        Print #fNum, Format(t_arr(i), "0.000000000E+00") & "  " & _
                     Format(sig_arr(i), "0.000000")
    Next i

    Close #fNum

    ' Done message
    MsgBox "Signal saved!" & vbCrLf & vbCrLf & _
           "Path:    " & sPath & vbCrLf & _
           "Samples: " & n_pts & vbCrLf & _
           "dt:      " & Format(dt * 1.0E9, "0.1") & " ns" & vbCrLf & _
           "Freq:    " & Format(pwm_freq * (1.0 - mod_depth) / 1000.0, "0.1") & _
           " ~ " & Format(pwm_freq * (1.0 + mod_depth) / 1000.0, "0.1") & " kHz" & vbCrLf & vbCrLf & _
           "Next step:" & vbCrLf & _
           "  Simulation > Excitation Signals > Import Signal", _
           vbInformation, "PWM Dither Export"

End Sub


' ------------------------------------------------------------
' Helper: convert time (s) to array index, clamped to [0, n_max]
' ------------------------------------------------------------
Function TIdx(t As Double, dt As Double, n_max As Long) As Long
    Dim idx As Long
    idx = CLng(Int(t / dt))
    If idx < 0     Then idx = 0
    If idx > n_max Then idx = n_max
    TIdx = idx
End Function


' ------------------------------------------------------------
' Helper: return the project root folder path
'   GetProjectPath("Result") → "C:\...\MyProject\Result"
'   We strip the last component to get "C:\...\MyProject"
' ------------------------------------------------------------
Function GetSaveFolder() As String
    Dim sResult As String
    sResult = GetProjectPath("Result")
    Dim pos As Long
    pos = InStrRev(sResult, "\")
    If pos > 1 Then
        GetSaveFolder = Left(sResult, pos - 1)
    Else
        GetSaveFolder = sResult
    End If
End Function
