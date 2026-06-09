' ============================================================
' PWM Frequency Dithering — CST Studio Suite VBA Macro
' 直接将抖频 PWM 信号写入 CST 项目（无需外部文件）
' ------------------------------------------------------------
' Algorithm:
'   f_inst = pwm_freq * (1 + mod_depth * triangle(t_cycle))
'   T_inst = 1 / f_inst
'   占空比固定 base_duty，每周期周期长度变化
' ------------------------------------------------------------
' Usage:
'   Macros > Edit/Run Macros > 粘贴 > 运行 GeneratePWMDither
'   信号名 "PWM_Dither" 出现在 Signals 树，可直接分配给端口
' ============================================================

Option Explicit

Sub GeneratePWMDither()

    ' ========================================================
    ' Parameters  —— 按需修改
    ' ========================================================
    Dim pwm_freq  As Double : pwm_freq  = 20000.0         ' PWM 中心频率   (Hz)
    Dim base_duty As Double : base_duty = 0.5             ' 固定占空比     (0~1)
    Dim v_high    As Double : v_high    = 0.0             ' 高电平         (V)
    Dim v_low     As Double : v_low     = 10.0            ' 低电平         (V)
    Dim rise_time As Double : rise_time = 100.0 / 1.0E9  ' 上升时间       (s) = 100 ns
    Dim fall_time As Double : fall_time = 100.0 / 1.0E9  ' 下降时间       (s) = 100 ns
    Dim mod_freq  As Double : mod_freq  = 1000.0          ' 调制频率       (Hz) = 1 kHz
    Dim mod_depth As Double : mod_depth = 0.10            ' 频率偏移幅度   (±10%)
    Dim duration  As Double : duration  = 0.001           ' 导入时长       (s)  = 1 ms
    Dim dt        As Double : dt        = 1.0 / (pwm_freq * 80.0)  ' 时间步长 (80 pts/cycle)

    Dim sigName As String   : sigName = "PWM_Dither"      ' CST 中的信号名称

    ' ========================================================
    ' 分配信号数组
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
        sig_arr(i) = v_low
    Next i

    ' ========================================================
    ' 抖频 PWM 生成（逐周期累计时间）
    ' ========================================================
    Dim t_cur   As Double : t_cur = 0.0
    Dim phase   As Double
    Dim mod_val As Double
    Dim f_inst  As Double
    Dim T_inst  As Double
    Dim t_he    As Double
    Dim t_ce    As Double
    Dim i_s     As Long
    Dim i_re    As Long
    Dim i_he    As Long
    Dim i_fe    As Long
    Dim i_ce    As Long
    Dim j       As Long

    Do While t_cur < duration

        ' 三角波调制值 [-1, +1]
        phase   = (t_cur * mod_freq) - Int(t_cur * mod_freq)
        mod_val = 2.0 * Abs(2.0 * phase - 1.0) - 1.0

        ' 瞬时频率 & 周期
        f_inst = pwm_freq * (1.0 + mod_depth * mod_val)
        If f_inst < 1.0 Then f_inst = 1.0
        T_inst = 1.0 / f_inst

        t_he = t_cur + base_duty * T_inst
        t_ce = t_cur + T_inst

        i_s  = TIdx(t_cur,              dt, n_pts - 1)
        i_re = TIdx(t_cur + rise_time,  dt, n_pts - 1)
        i_he = TIdx(t_he,               dt, n_pts - 1)
        i_fe = TIdx(t_he  + fall_time,  dt, n_pts - 1)
        i_ce = TIdx(t_ce,               dt, n_pts - 1)

        If i_re > i_he Then i_re = i_he
        If i_fe > i_ce Then i_fe = i_ce

        ' 上升沿
        If i_re > i_s Then
            For j = i_s To i_re - 1
                sig_arr(j) = v_low + (v_high - v_low) * _
                             CDbl(j - i_s) / CDbl(i_re - i_s)
            Next j
        End If

        ' 高电平
        For j = i_re To i_he - 1
            sig_arr(j) = v_high
        Next j

        ' 下降沿
        If i_fe > i_he Then
            For j = i_he To i_fe - 1
                sig_arr(j) = v_high + (v_low - v_high) * _
                             CDbl(j - i_he) / CDbl(i_fe - i_he)
            Next j
        End If

        t_cur = t_ce
    Loop

    ' ========================================================
    ' 直接写入 CST 项目信号树（无需外部文件）
    ' ========================================================

    ' 若同名信号已存在则先删除
    On Error Resume Next
    Signals.Delete sigName
    On Error GoTo 0

    ' 逐点写入 CST Signals 对象
    With Signals
        .Reset
        .Name sigName
        .Periodic "False"
        For i = 0 To n_pts - 1
            .AddSample t_arr(i), sig_arr(i)
        Next i
        .Create
    End With

    ' ========================================================
    ' 完成提示
    ' ========================================================
    MsgBox "Signal '" & sigName & "' created in project." & vbCrLf & vbCrLf & _
           "Samples : " & n_pts & vbCrLf & _
           "dt      : " & Format(dt * 1.0E9, "0.1") & " ns" & vbCrLf & _
           "Duration: " & Format(duration * 1000.0, "0.000") & " ms" & vbCrLf & _
           "Freq    : " & Format(pwm_freq * (1.0 - mod_depth) / 1000.0, "0.1") & _
           " ~ " & Format(pwm_freq * (1.0 + mod_depth) / 1000.0, "0.1") & " kHz" & vbCrLf & vbCrLf & _
           "Assign to port: Simulation > Excitation > Port Signals", _
           vbInformation, "PWM Dither"

End Sub


' ------------------------------------------------------------
' 辅助：时间 → 数组下标，夹紧到 [0, n_max]
' ------------------------------------------------------------
Function TIdx(t As Double, dt As Double, n_max As Long) As Long
    Dim idx As Long
    idx = CLng(Int(t / dt))
    If idx < 0     Then idx = 0
    If idx > n_max Then idx = n_max
    TIdx = idx
End Function
