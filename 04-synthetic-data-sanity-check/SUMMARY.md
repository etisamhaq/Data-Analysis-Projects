# Executive summary: is this data real?

**Decision: don't base findings on the Kaggle "Cyber Security Attacks" or "Air Quality and Health Impact" datasets, and run `datasanity` on any unfamiliar dataset before analysing it.**

## Why

- **Both datasets are randomly generated.** In the cybersecurity data, every city appears in 24–28 of India's 28 states, none of 40,000 attacker IPs ever repeats, and payloads are lorem-ipsum placeholder text. In the air-quality data, AQI is unrelated to PM2.5 (correlation 0.006), although AQI is calculated from it.
- **So their "findings" are noise**, including ones this repository once reported ("DDoS is the most common attack", "AQI strongly drives respiratory cases").
- **The check is now a tool.** `datasanity` runs eight kinds of check on any CSV or Excel file in seconds. It flagged both synthetic datasets (8 of 8 and 3 of 4 applicable checks triggered) and raised no false alarms on three real datasets (Netflix, NBA, NSE stock prices).

## Actions

1. Add `datasanity --fail-on-synthetic` as a gate in data pipelines that take in third-party or vendor files.
2. Use the two synthetic datasets only for practising tools, never for insight.

## Confidence

- **High** for these two datasets: several independent kinds of evidence, each checkable by hand.
- The tool catches *naive* random generation. Synthetic data built to mimic real distributions can pass it, and its thresholds are calibrated on only five datasets, so a single triggered check means "investigate", not "fake".

**Details:** [README](README.md) · [Methodology](docs/methodology.md)
