#!/usr/bin/env python3
"""
PWM Signal with Selectable Modulation Waveform -- Interactive Viewer

Features
--------
- PWM: 20 kHz center, 50% duty, High=0V / Low=10V, 100ns rise/fall
- Modulation waveforms: Triangle / Sine / Square / Sawtooth-up / Sawtooth-down
- Window auto-sized to fit screen (<=90% width, <=88% height)
- Export: CST Studio Suite format (;-comments, space-separated, time[s] & voltage[V])
"""

import datetime
from pathlib import Path
from typing import Optional, Tuple

import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
from matplotlib.widgets import Slider, Button, RadioButtons


# ---------------------------------------------------------------------------
# Modulation waveforms  (all return values in [-1, +1])
# ---------------------------------------------------------------------------

MOD_WAVES = {
    "Triangle":     lambda t, f: 2.0 * np.abs(2.0 * ((t * f) % 1.0) - 1.0) - 1.0,
    "Sine":         lambda t, f: np.sin(2.0 * np.pi * f * t),
    "Square":       lambda t, f: np.where((t * f) % 1.0 < 0.5, 1.0, -1.0),
    "Sawtooth Up":  lambda t, f: 2.0 * ((t * f) % 1.0) - 1.0,
    "Sawtooth Dn":  lambda t, f: 1.0 - 2.0 * ((t * f) % 1.0),
}
MOD_NAMES = list(MOD_WAVES.keys())


def mod_wave(name: str, t: np.ndarray, freq: float) -> np.ndarray:
    return MOD_WAVES[name](t, freq)


# ---------------------------------------------------------------------------
# PWM generation
# ---------------------------------------------------------------------------

def generate_pwm(
    pwm_freq: float,
    base_duty: float,
    v_high: float,
    v_low: float,
    rise_time: float,
    fall_time: float,
    mod_freq: float,
    mod_depth: float,
    mod_name: str,
    duration: float,
    dt: float,
) -> Tuple[np.ndarray, np.ndarray, np.ndarray]:
    """
    Returns (t, signal, mod_wave_arr).
    Duty cycle for each PWM cycle is set by the modulation wave at the cycle midpoint.
    """
    n = max(2, int(duration / dt))
    t = np.arange(n) * dt
    signal = np.full(n, v_low, dtype=float)

    pwm_period = 1.0 / pwm_freq
    n_cycles = int(np.ceil(duration / pwm_period)) + 1
    fn = MOD_WAVES[mod_name]

    for ci in range(n_cycles):
        t0 = ci * pwm_period
        if t0 >= duration:
            break

        mod_val = float(fn(np.array([t0 + pwm_period * 0.5]), mod_freq)[0])
        dc = float(np.clip(base_duty + mod_depth * mod_val, 0.01, 0.99))

        t_he = t0 + dc * pwm_period
        t_ce = t0 + pwm_period

        def idx(tv: float) -> int:
            return min(max(int(tv / dt), 0), n)

        i_s  = idx(t0)
        i_re = min(idx(t0 + rise_time), idx(t_he))
        i_he = idx(t_he)
        i_fe = min(idx(t_he + fall_time), idx(t_ce))
        i_ce = idx(t_ce)

        if i_re > i_s:  signal[i_s:i_re]  = np.linspace(v_low, v_high, i_re - i_s)
        if i_he > i_re: signal[i_re:i_he] = v_high
        if i_fe > i_he: signal[i_he:i_fe] = np.linspace(v_high, v_low, i_fe - i_he)

    return t, signal, fn(t, mod_freq)


# ---------------------------------------------------------------------------
# Export (CST Studio Suite format)
# ---------------------------------------------------------------------------

_SCRIPT_DIR = Path(__file__).parent


def export_cst_format(
    t: np.ndarray,
    signal: np.ndarray,
    params: dict,
    suffix: str = ".csv",
) -> Path:
    """
    Save time-domain signal in CST Studio Suite compatible format.

    Format
    ------
    - Comment lines prefixed with ';'
    - Two space-separated columns: time [s]  amplitude [V]
    - No column header row

    Import in CST
    -------------
    Simulation > Excitation Signals > Import Signal
    Signal type: Voltage  |  Time unit: s  |  Amplitude unit: V
    """
    stamp = datetime.datetime.now().strftime("%Y%m%d_%H%M%S")
    path = _SCRIPT_DIR / f"pwm_signal_{stamp}{suffix}"

    now = datetime.datetime.now().isoformat(timespec="seconds")
    header = [
        "; CST Studio Suite -- Signal Import File",
        f"; Generated  : {now}",
        f"; Mod wave   : {params.get('mod_name', 'Triangle')}",
        f"; PWM freq   : {params['pwm_freq']/1e3:.4f} kHz",
        f"; Duty cycle : {params['base_duty']*100:.1f} %",
        f"; V High     : {params['v_high']:.3f} V",
        f"; V Low      : {params['v_low']:.3f} V",
        f"; Rise time  : {params['rise_time']*1e9:.1f} ns",
        f"; Fall time  : {params['fall_time']*1e9:.1f} ns",
        f"; Mod freq   : {params['mod_freq']/1e3:.4f} kHz",
        f"; Mod depth  : +/-{params['mod_depth']*100:.1f} %",
        f"; Samples    : {len(t)}",
        f"; dt         : {(t[1]-t[0])*1e9:.3f} ns",
        ";",
        "; Import: Simulation > Excitation Signals > Import Signal",
        "; Col 1 = time [s]   Col 2 = amplitude [V]",
        ";",
    ]
    with open(path, "w", encoding="utf-8") as f:
        f.write("\n".join(header) + "\n")
        for ti, vi in zip(t, signal):
            f.write(f"{ti:.9e}  {vi:.6f}\n")

    return path


# ---------------------------------------------------------------------------
# Screen-aware figure size
# ---------------------------------------------------------------------------

def _screen_inches(max_w_frac: float = 0.90, max_h_frac: float = 0.88):
    """Return (width_in, height_in) capped to a fraction of the screen."""
    try:
        import tkinter as tk
        root = tk.Tk()
        root.withdraw()
        sw = root.winfo_screenwidth()
        sh = root.winfo_screenheight()
        root.update()   # flush events before handing control to matplotlib
        # do NOT destroy — let matplotlib's Tk backend reuse the interpreter
        dpi = matplotlib.rcParams["figure.dpi"]
        return (sw * max_w_frac / dpi, sh * max_h_frac / dpi)
    except Exception:
        return (14.0, 10.0)


# ---------------------------------------------------------------------------
# Interactive viewer
# ---------------------------------------------------------------------------

class PWMViewer:
    DEFAULTS = dict(
        pwm_freq=20e3,
        base_duty=0.50,
        v_high=0.0,
        v_low=10.0,
        rise_time=100e-9,
        fall_time=100e-9,
        mod_freq=1e3,
        mod_depth=0.10,
    )

    # (attr, label, unit, v_min, v_max, scale, row, col)
    SLIDER_DEFS = [
        ("pwm_freq",  "PWM Freq",   "kHz",  1.0, 100.0, 1e3,  0, 0),
        ("base_duty", "Duty Cycle", "%",   10.0,  90.0, 0.01, 1, 0),
        ("v_high",    "V High",     "V",   -5.0,   5.0, 1.0,  2, 0),
        ("v_low",     "V Low",      "V",    5.0,  20.0, 1.0,  3, 0),
        ("rise_time", "Rise Time",  "ns",  10.0,1000.0, 1e-9, 0, 1),
        ("fall_time", "Fall Time",  "ns",  10.0,1000.0, 1e-9, 1, 1),
        ("mod_freq",  "Mod Freq",   "kHz",  0.1,  10.0, 1e3,  2, 1),
        ("mod_depth", "Mod Depth",  "%",    1.0,  49.0, 0.01, 3, 1),
    ]

    BG_DARK  = "#0d1117"
    BG_PANEL = "#161b22"
    C_GRID   = "#21262d"
    C_BORDER = "#30363d"
    C_TEXT   = "#c9d1d9"
    C_MUTED  = "#8b949e"
    C_BLUE   = "#58a6ff"
    C_GREEN  = "#3fb950"
    C_ORANGE = "#f0883e"
    C_ACCENT = "#388bfd"
    C_PURPLE = "#bc8cff"

    def __init__(self):
        self._mod_name: str = "Triangle"
        self._inverted: bool = False
        self._last_ov_data:  Optional[tuple] = None
        self._last_det_data: Optional[tuple] = None
        self._last_params:   Optional[dict]  = None

        self._build_figure()
        self._build_sliders()
        self._build_mod_selector()
        self._build_export_panel()
        self._build_info_box()
        self._update_plots(self.DEFAULTS)
        self._connect()

    # ------------------------------------------------------------------
    # Figure
    # ------------------------------------------------------------------

    def _build_figure(self):
        w, h = _screen_inches()
        self.fig = plt.figure(figsize=(w, h))
        self.fig.patch.set_facecolor(self.BG_DARK)
        self.fig.suptitle(
            "PWM Modulation -- Interactive Viewer",
            color=self.C_TEXT, fontsize=12, fontweight="bold", y=0.99,
        )
        gs = gridspec.GridSpec(3, 1, figure=self.fig,
                               top=0.96, bottom=0.33, hspace=0.52)
        self.ax_duty = self.fig.add_subplot(gs[0])
        self.ax_ov   = self.fig.add_subplot(gs[1])
        self.ax_det  = self.fig.add_subplot(gs[2])

    def _style_ax(self, ax):
        ax.set_facecolor(self.BG_PANEL)
        ax.tick_params(colors=self.C_MUTED, labelsize=8)
        for sp in ax.spines.values():
            sp.set_color(self.C_BORDER)
        ax.xaxis.label.set_color(self.C_MUTED)
        ax.yaxis.label.set_color(self.C_MUTED)
        ax.title.set_color(self.C_TEXT)
        ax.grid(True, color=self.C_GRID, linestyle="--", alpha=0.7, lw=0.6)

    # ------------------------------------------------------------------
    # Sliders
    # ------------------------------------------------------------------

    def _build_sliders(self):
        self.sliders: dict = {}
        col_x = [0.05, 0.52]
        row_y = [0.278, 0.236, 0.194, 0.152]

        for attr, label, unit, vmin, vmax, scale, row, col in self.SLIDER_DEFS:
            ax_s = self.fig.add_axes([col_x[col], row_y[row], 0.36, 0.026])
            ax_s.set_facecolor(self.BG_PANEL)
            init = self.DEFAULTS[attr] / scale
            s = Slider(ax_s, f"{label} ({unit})", vmin, vmax,
                       valinit=init, color=self.C_ACCENT, track_color=self.C_GRID)
            s.label.set_color(self.C_TEXT);   s.label.set_fontsize(8)
            s.valtext.set_color(self.C_BLUE); s.valtext.set_fontsize(8)
            self.sliders[attr] = (s, scale)

        # dt (time step) slider — full width, below the two columns
        ax_dt = self.fig.add_axes([0.05, 0.112, 0.84, 0.026])
        ax_dt.set_facecolor(self.BG_PANEL)
        dt_default_ns = 1.0 / (self.DEFAULTS["pwm_freq"] * 80) * 1e9  # ~625 ns
        self._sl_dt = Slider(ax_dt, "Time Step dt  (ns)", 1.0, 2000.0,
                             valinit=round(dt_default_ns, 1),
                             color=self.C_ACCENT, track_color=self.C_GRID)
        self._sl_dt.label.set_color(self.C_TEXT);   self._sl_dt.label.set_fontsize(8)
        self._sl_dt.valtext.set_color(self.C_BLUE); self._sl_dt.valtext.set_fontsize(8)

        # Reset button
        ax_rst = self.fig.add_axes([0.46, 0.055, 0.08, 0.036])
        self.btn_reset = Button(ax_rst, "Reset", color=self.BG_PANEL, hovercolor=self.C_BORDER)
        self.btn_reset.label.set_color(self.C_TEXT); self.btn_reset.label.set_fontsize(8)

        # Invert button
        ax_inv = self.fig.add_axes([0.56, 0.055, 0.10, 0.036])
        self.btn_invert = Button(ax_inv, "Invert OFF", color=self.BG_PANEL, hovercolor="#3d2200")
        self.btn_invert.label.set_color(self.C_MUTED); self.btn_invert.label.set_fontsize(8)

    # ------------------------------------------------------------------
    # Modulation waveform selector
    # ------------------------------------------------------------------

    def _build_mod_selector(self):
        """RadioButtons for selecting the modulation waveform type."""
        lbl_ax = self.fig.add_axes([0.895, 0.315, 0.10, 0.02])
        lbl_ax.axis("off")
        lbl_ax.text(0.5, 0.5, "Mod Wave", color=self.C_MUTED,
                    fontsize=8, ha="center", va="center")

        ax_mod = self.fig.add_axes([0.895, 0.112, 0.095, 0.20])
        ax_mod.set_facecolor(self.BG_PANEL)
        for sp in ax_mod.spines.values():
            sp.set_color(self.C_BORDER)

        self._mod_radio = RadioButtons(
            ax_mod, MOD_NAMES, active=0, activecolor=self.C_BLUE
        )
        n = len(MOD_NAMES)
        self._mod_radio.set_label_props({"color": [self.C_TEXT] * n, "fontsize": [8] * n})
        # Only set edgecolor — let activecolor fill the selected circle
        self._mod_radio.set_radio_props({"edgecolor": [self.C_BLUE] * n, "linewidth": [1.5] * n})

    # ------------------------------------------------------------------
    # Export panel
    # ------------------------------------------------------------------

    def _build_export_panel(self):
        # Export modulated PWM (overview, 3 mod cycles)
        ax_exp = self.fig.add_axes([0.05, 0.055, 0.18, 0.036])
        self.btn_export = Button(ax_exp, "Export Modulated PWM",
                                 color="#1a2a1a", hovercolor="#2ea043")
        self.btn_export.label.set_color("#aff3b0"); self.btn_export.label.set_fontsize(8)

        # Export unmodulated (pure) PWM
        ax_raw = self.fig.add_axes([0.25, 0.055, 0.18, 0.036])
        self.btn_export_raw = Button(ax_raw, "Export Pure PWM",
                                     color="#1a1a3a", hovercolor="#4040a0")
        self.btn_export_raw.label.set_color(self.C_PURPLE); self.btn_export_raw.label.set_fontsize(8)

        # Status bar
        ax_st = self.fig.add_axes([0.05, 0.012, 0.88, 0.018])
        ax_st.axis("off")
        ax_st.set_facecolor(self.BG_DARK)
        self._status_text = ax_st.text(
            0, 0.5, "Ready -- export saves to script directory.",
            color=self.C_MUTED, fontsize=7.5, va="center", fontfamily="monospace",
        )

    # ------------------------------------------------------------------
    # Info box
    # ------------------------------------------------------------------

    def _build_info_box(self):
        ax = self.fig.add_axes([0.47, 0.042, 0.42, 0.068])
        ax.set_facecolor(self.BG_PANEL)
        ax.axis("off")
        for sp in ax.spines.values():
            sp.set_color(self.C_BORDER)
        self._info_text = ax.text(
            0.03, 0.5, "",
            transform=ax.transAxes,
            color=self.C_MUTED, fontsize=8, va="center",
            fontfamily="monospace", linespacing=1.7,
        )

    # ------------------------------------------------------------------
    # Adaptive dt
    # ------------------------------------------------------------------

    def _get_dt(self) -> float:
        """Return the user-selected dt in seconds."""
        return self._sl_dt.val * 1e-9

    # ------------------------------------------------------------------
    # Plot update
    # ------------------------------------------------------------------

    def _read_params(self) -> dict:
        p = {attr: s.val * sc for attr, (s, sc) in self.sliders.items()}
        p["mod_name"] = self._mod_name
        return p

    def _update_plots(self, p: dict):
        pwm_freq  = p["pwm_freq"]
        mod_freq  = p["mod_freq"]
        base_duty = p["base_duty"]
        mod_depth = p["mod_depth"]
        v_high    = p["v_high"]
        v_low     = p["v_low"]
        mname     = p.get("mod_name", "Triangle")

        ov_dur  = 3.0 / mod_freq
        det_dur = 6.0 / pwm_freq

        args = (pwm_freq, base_duty, v_high, v_low,
                p["rise_time"], p["fall_time"], mod_freq, mod_depth, mname)

        dt = self._get_dt()
        t_ov,  sig_ov,  mod_ov  = generate_pwm(*args, ov_dur,  dt)
        t_det, sig_det, mod_det = generate_pwm(*args, det_dur, dt)

        self._last_ov_data  = (t_ov,  sig_ov,  mod_ov)
        self._last_det_data = (t_det, sig_det, mod_det)
        self._last_params   = dict(p)

        v_lo = min(v_high, v_low)
        v_hi = max(v_high, v_low)
        v_margin = (v_hi - v_lo) * 0.12 + 0.3

        # ---- modulation / duty cycle plot ----
        t_d = np.linspace(0, ov_dur, 3000)
        raw_mod  = MOD_WAVES[mname](t_d, mod_freq)
        duty_pct = np.clip(base_duty + mod_depth * raw_mod, 0, 1) * 100
        d_range  = mod_depth * 100

        ax = self.ax_duty
        ax.cla(); self._style_ax(ax)
        ax.plot(t_d * 1e3, duty_pct, color=self.C_BLUE, lw=1.6, label="Duty cycle")
        ax.axhline(base_duty * 100, color=self.C_ORANGE,
                   linestyle="--", lw=1.0, alpha=0.85,
                   label=f"Centre {base_duty*100:.1f}%")
        ax.fill_between(t_d * 1e3,
                        (base_duty - mod_depth) * 100,
                        (base_duty + mod_depth) * 100,
                        alpha=0.11, color=self.C_BLUE)
        margin_y = d_range * 0.4 + 2.0
        ax.set_ylim(base_duty*100 - d_range - margin_y,
                    base_duty*100 + d_range + margin_y)
        ax.set_xlim(0, ov_dur * 1e3)
        ax.set_xlabel("Time (ms)", fontsize=8)
        ax.set_ylabel("Duty Cycle (%)", fontsize=8)
        ax.set_title(
            f"Modulated Duty Cycle [{mname}]  --  "
            f"{base_duty*100:.0f}% +/- {d_range:.0f}%  @  {mod_freq/1e3:.2f} kHz",
            fontsize=9,
        )
        ax.legend(fontsize=7.5, facecolor=self.BG_PANEL,
                  labelcolor=self.C_TEXT, edgecolor=self.C_BORDER, loc="upper right")

        # ---- overview ----
        ax = self.ax_ov
        ax.cla(); self._style_ax(ax)
        ax.plot(t_ov * 1e3, sig_ov, color=self.C_GREEN, lw=0.7)
        ax.set_ylim(v_lo - v_margin, v_hi + v_margin)
        ax.set_xlim(0, ov_dur * 1e3)
        ax.set_xlabel("Time (ms)", fontsize=8)
        ax.set_ylabel("Voltage (V)", fontsize=8)
        ax.set_title(
            f"PWM Overview -- {ov_dur*1e3:.1f} ms  ({int(ov_dur*pwm_freq)} cycles)",
            fontsize=9,
        )

        # ---- detail ----
        ax = self.ax_det
        ax.cla(); self._style_ax(ax)
        ax.plot(t_det * 1e6, sig_det, color=self.C_ORANGE, lw=1.6)
        ax.set_ylim(v_lo - v_margin, v_hi + v_margin)
        ax.set_xlim(0, det_dur * 1e6)
        ax.set_xlabel("Time (us)", fontsize=8)
        ax.set_ylabel("Voltage (V)", fontsize=8)
        ax.set_title(
            f"PWM Detail -- 6 cycles @ {pwm_freq/1e3:.1f} kHz  "
            f"(rise {p['rise_time']*1e9:.0f}ns / fall {p['fall_time']*1e9:.0f}ns)",
            fontsize=9,
        )

        # ---- info box ----
        dc_min = (base_duty - mod_depth) * 100
        dc_max = (base_duty + mod_depth) * 100
        self._info_text.set_text(
            f"PWM period : {1e6/pwm_freq:.2f} us    Mod period : {1e3/mod_freq:.2f} ms\n"
            f"Duty range : {dc_min:.1f}% -- {dc_max:.1f}%    "
            f"H={v_high:.1f}V / L={v_low:.1f}V\n"
            f"Rise:{p['rise_time']*1e9:.0f}ns  Fall:{p['fall_time']*1e9:.0f}ns  "
            f"dt:{self._sl_dt.val:.0f}ns  Pts(ov/det):{len(t_ov):,}/{len(t_det):,}"
        )
        self.fig.canvas.draw_idle()

    # ------------------------------------------------------------------
    # Export
    # ------------------------------------------------------------------

    def _on_export(self, _event):
        if self._last_params is None:
            return
        t, sig, _ = self._last_ov_data
        path = export_cst_format(t, sig, self._last_params, suffix=".csv")
        size_kb = path.stat().st_size / 1024
        self._status_text.set_text(
            f"[Modulated] Saved: {path.name}  ({size_kb:.1f} KB, {len(t):,} pts)"
        )
        self.fig.canvas.draw_idle()

    def _on_export_raw(self, _event):
        """Export unmodulated PWM (mod_depth=0, same freq/duty/levels/edges)."""
        if self._last_params is None:
            return
        p = dict(self._last_params)
        ov_dur = 3.0 / p["mod_freq"]
        dt = self._get_dt()
        t, sig, _ = generate_pwm(
            p["pwm_freq"], p["base_duty"], p["v_high"], p["v_low"],
            p["rise_time"], p["fall_time"],
            p["mod_freq"], 0.0,          # mod_depth = 0 → no modulation
            "Triangle", ov_dur, dt,
        )
        raw_params = dict(p, mod_depth=0.0, mod_name="(none -- pure PWM)")
        path = export_cst_format(t, sig, raw_params, suffix=".csv")
        size_kb = path.stat().st_size / 1024
        self._status_text.set_text(
            f"[Pure PWM] Saved: {path.name}  ({size_kb:.1f} KB, {len(t):,} pts)"
        )
        self.fig.canvas.draw_idle()

    # ------------------------------------------------------------------
    # Events
    # ------------------------------------------------------------------

    def _on_mod_change(self, name: str):
        self._mod_name = name
        self._update_plots(self._read_params())

    def _on_invert(self, _event):
        s_hi, _ = self.sliders["v_high"]
        s_lo, _ = self.sliders["v_low"]
        hi, lo = s_hi.val, s_lo.val
        for _, (s, _) in self.sliders.items():
            s.eventson = False
        s_hi.set_val(lo); s_lo.set_val(hi)
        for _, (s, _) in self.sliders.items():
            s.eventson = True
        self._inverted = not self._inverted
        if self._inverted:
            self.btn_invert.label.set_text("Invert ON")
            self.btn_invert.label.set_color(self.C_ORANGE)
            self.btn_invert.ax.set_facecolor("#3d1a00")
        else:
            self.btn_invert.label.set_text("Invert OFF")
            self.btn_invert.label.set_color(self.C_MUTED)
            self.btn_invert.ax.set_facecolor(self.BG_PANEL)
        self._update_plots(self._read_params())

    def _on_slider_change(self, _val):
        self._update_plots(self._read_params())

    def _on_reset(self, _event):
        for attr, (s, scale) in self.sliders.items():
            s.eventson = False
            s.set_val(self.DEFAULTS[attr] / scale)
            s.eventson = True
        dt_default_ns = 1.0 / (self.DEFAULTS["pwm_freq"] * 80) * 1e9
        self._sl_dt.set_val(round(dt_default_ns, 1))
        self._mod_name = "Triangle"
        self._mod_radio.set_active(0)
        self._update_plots(self.DEFAULTS)

    def _connect(self):
        for _, (s, _) in self.sliders.items():
            s.on_changed(self._on_slider_change)
        self._sl_dt.on_changed(self._on_slider_change)
        self.btn_reset.on_clicked(self._on_reset)
        self.btn_invert.on_clicked(self._on_invert)
        self.btn_export.on_clicked(self._on_export)
        self.btn_export_raw.on_clicked(self._on_export_raw)
        self._mod_radio.on_clicked(self._on_mod_change)

    def show(self):
        plt.show()


# ---------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------

if __name__ == "__main__":
    viewer = PWMViewer()
    viewer.show()
