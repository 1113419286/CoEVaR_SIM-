# CoEVaR_SIM-
This repository provides the code and data associated with the paper:

> Zhang, F., Xu, Y., & Chen, Y. (2026).  
> **A study of nonlinear tail risk measurement and spillover effects in equity markets under extreme events.**  
> *Systems Engineering — Theory & Practice*, 46(2), 618–637.  
> DOI: 10.12011/SETP2023-2712

## Overview

This repository implements the nonlinear conditional expectile value-at-risk
(**CoEVaR-SIM**) framework proposed in the paper.

The method combines the **single-index expectile model (SIEM)** with
**conditional expectile value-at-risk (CoEVaR)** to capture nonlinear tail-risk
spillovers across financial markets.

Based on CoEVaR-SIM, we construct dynamic tail-risk spillover networks and
analyze the magnitude, direction, and structure of risk transmission across
global equity markets.

## Data

The empirical analysis uses weekly returns of 30 major global stock market
indices from June 2009 to June 2023.

The markets are grouped into three regions:

- Americas
- Europe
- Asia

The analysis also incorporates macro-financial variables, including exchange
rates, the VIX index, commodity futures, and energy prices.

The main data files are stored in the `data/` directory.

## Repository Structure

```text
CoEVaR-SIM/
│
├── README.md
│
├── code/
│   ├── CoEVaR/
│   ├── CoEVaR_SIM/
│   ├── CoEVaR_SIM_and_Linear/
│   ├── CoEVaR_mst_SIM/
│   ├── CoEVaR_network_SIMplot/
│   ├── CoEVaR_Robust_SIM/
│   ├── CoEVaR_SIFIs/
│   └── CoEVaR_SIFIs_SIM/
│
├── data/
│
└── results/
