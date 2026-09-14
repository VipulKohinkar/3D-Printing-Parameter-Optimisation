# 3D Printing Parameter Optimisation: Impact of K-Value on Carbon-Reinforced PETG

## MSc Mechanical Engineering with Management

**Author:** Vipul Kohinkar  
**Project:** MEE7012-SUM  
**Date:** 27 August 2026

---

## Project Overview

This project investigates the influence of flow-rate multiplier (K-value) and build orientation on the tensile behaviour of PETG and carbon-fibre-reinforced PETG (CF-PETG) manufactured using fused deposition modelling (FDM).

The study compares an unreinforced thermoplastic, PETG, with a carbon-fibre-reinforced PETG composite to investigate whether the two materials respond differently to changes in material flow and build orientation.

---

## Aim

The aim of this study was to determine how K-value and build orientation influence the tensile properties of FDM-printed PETG and carbon-fibre-reinforced PETG (CF-PETG), and whether the response differs between the unreinforced polymer and fibre-reinforced composite.

---

## Experimental Design

A full factorial experimental design was used.

### Materials

- PETG
- 20% carbon-fibre-reinforced PETG (CF-PETG)

### Flow-Rate Multiplier

Four K-values were investigated:

- 80%
- 90%
- 100%
- 110%

### Build Orientations

Two build orientations were investigated:

- Flat
- On-edge

This resulted in:

**2 materials × 4 K-values × 2 orientations = 16 experimental conditions**

---

## Printing Parameters

The specimens were manufactured using Ultimaker S5 FDM printers and prepared using Ultimaker Cura.

| Parameter | Value |
|---|---|
| Layer height | 0.2 mm |
| Infill density | 100% |
| Print speed | 40 mm/s |
| Infill pattern | Lines |
| Nominal wall thickness | 2.4 mm |
| PETG nozzle | 0.4 mm |
| CF-PETG nozzle | 0.6 mm |
| PETG nozzle temperature | 245°C |
| CF-PETG nozzle temperature | 280°C |
| Bed temperature | 80°C |
| Fan speed | 10% |

The K-value was varied through the Cura flow setting while the remaining principal printing parameters were kept constant within the experimental programme.

---

## Specimen Design

Tensile specimens were designed according to ISO 527-2:2025 using the Type 5A geometry.

The CAD model and engineering drawing are provided in:

`1_CAD_Files/`

---

## Tensile Testing

Tensile testing was performed using a Lloyd Instruments LS5 universal testing machine with a 5 kN load cell.

The following mechanical properties were evaluated:

- Young's modulus
- Ultimate tensile strength (UTS)
- Elongation at break
- Toughness

Young's modulus was determined using extensometer-based strain measurements, while the pull-to-break stage was used to determine UTS, elongation at break and toughness.

---

## Optical Microscopy

Optical microscopy was used to examine the printed specimens before testing and the fracture surfaces after testing.

The microscopy data are separated into:

- Original microscopy images
- Images selected and processed for use in the final report

---

## Data Analysis

The experimental data were analysed to investigate the effects of:

- Material
- K-value
- Build orientation
- Interactions between the experimental factors

Three-way ANOVA was used for Young's modulus and UTS. Elongation at break and toughness were log-transformed before statistical analysis because of their skewed distributions. Tukey HSD post-hoc comparisons were used where statistically significant effects were identified.

---

## Key Findings

CF-PETG showed substantially greater stiffness than PETG across the investigated conditions.

The response to K-value was material dependent. CF-PETG showed the clearest response to changes in K-value, particularly for ultimate tensile strength.

For CF-PETG, **K = 110% produced significantly higher UTS than K = 80%, 90% and 100%**. Young's modulus showed comparatively little variation across the investigated K-values.

PETG showed comparatively stable UTS and Young's modulus but substantially greater specimen-to-specimen variation in elongation at break and toughness.

Build orientation produced some differences in the measured responses, although its influence depended on the material and mechanical property being considered.

Overall, the results indicate that increasing flow rate does not provide a universal improvement across all materials and mechanical properties. K-value selection should therefore consider the material and the specific mechanical performance required.

---

## Repository Structure

```text
3D-Printing-Parameter-Optimisation/
│
├── 1_CAD_Files/
│   └── ISO527_5A_Tensile_Specimens/
│
├── 2_Printing/
│   ├── Flat_Orientation/
│   └── On_Edge_Orientation/
│
├── 3_Experimental_data/
│
├── 4_Analysis/
│   └── Graphs/
│
├── 5_Microscope_images/
│   ├── Original_Images/
│   └── Report_Images/
│
├── 6_Project_Management/
│
├── 7_Report/
│   └── Final_Project_Report.pdf
│
└── 8_Presentations/
