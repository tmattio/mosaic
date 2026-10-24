# `15-synthesizer`

A waveform synthesizer demo showcasing the matrix charts library. Generate and
play different waveform types while visualizing their shape, harmonic content,
and amplitude envelope in real-time.

```bash
dune exec ./matrix/examples/15-synthesizer/main.exe
```

## Controls

- `A` / `Z` &mdash; increase / decrease frequency.
- `Tab` &mdash; cycle through waveform types (Sine, Square, Sawtooth, Triangle).
- `S` / `X` &mdash; increase / decrease volume.
- `D` / `C` &mdash; increase / decrease duration.
- `Space` &mdash; play the current waveform.
- `Q` &mdash; quit.

## Highlights

- Four waveform types with mathematically accurate synthesis.
- High-resolution waveform display using Braille character rendering.
- Harmonic spectrum analyzer showing frequency content per waveform type.
- Amplitude envelope sparkline showing the ADSR shape over time.
- Musical note display converting frequency to note name (e.g., 440 Hz = A4).
